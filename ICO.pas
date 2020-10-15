(*
    Greenfish Icon Editor Pro
    Copyright (c) 2012-13 B. Szalkai

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)
unit ICO;

interface

uses
  LclIntf, LclType, SysUtils, Classes, BitmapEx, Layers, DocClass;

const
  // These constants denote in an ICO/CUR file whether the file is an
  // icon or a cursor file
  ID_ICO = 1;
  ID_CUR = 2;

type
  PICONDIR = ^ICONDIR;
  ICONDIR = packed record
    idReserved: word;
    idType: word;
    idCount: word;
  end;

  PICONDIRENTRY = ^ICONDIRENTRY;
  ICONDIRENTRY = packed record
    bWidth: byte;
    bHeight: byte;
    bColorCount: byte;
    bReserved: byte;
    wPlanes_XHotSpot: word; // [icon]_[cursor]
    wBitCount_YHotSpot: word; // [icon]_[cursor]
    dwBytesInRes: Cardinal;
    dwImageOffset: Cardinal;
  end;

function icoDetect(Data: Pointer; Size: integer; IsCursor: boolean): boolean;

function icoLoadFromStream(Doc: TIconDoc; s: TStream): boolean;
procedure icoSaveToStream(Doc: TIconDoc; s: TStream;
  AsCursor: boolean; PNGLimit: integer);

function icoLoadFromFile(Doc: TIconDoc; const fn: string): boolean;
procedure icoSaveToFile(Doc: TIconDoc; const fn: string;
  AsCursor: boolean; PNGLimit: integer);

implementation

uses
  Math, PixelFormats, BMP, PNG;

// Leaves the stream position unchanged
function icoLoadPageFromStream(bm: TBitmap32; var HotSpot: TPoint; s: TStream;
  StreamZeroPoint: integer; const Entry: ICONDIRENTRY; AsCursor: boolean): boolean;
const
  PNG_Signature = $474e50; // 'PNG'

var
  i, oldPos: integer;
  dpi: double;

begin
  Result := False;
  try
    // initialize
    oldPos := s.Position;

    // load information from directory entry
    with Entry do
    begin
      if AsCursor then
        HotSpot := Point(wPlanes_XHotSpot, wBitCount_YHotSpot) else
        HotSpot := Point(0, 0);

      // jump to data block
      s.Position := StreamZeroPoint + Integer(dwImageOffset);
    end;

    // does it contain raw PNG data?
    i := Integer(s.ReadDWord);
    s.Seek(-4, soFromCurrent);
    if i shr 8 = PNG_Signature then
      Result := pngLoadFromStream(bm, s, dpi) else
      Result := dibRead(bm, s, -1, True) and (bm.Width <> 0) and (bm.Height <> 0);

    // restore stream position
    s.Position := oldPos;
  except
  end;
end;

// Does not reset the stream position
procedure icoSavePageToStream(bm: TBitmap32; const HotSpot: TPoint; s: TStream;
  StreamZeroPoint: integer; var Entry: ICONDIRENTRY;
  PNGCompress, AsCursor: boolean);
var
  Palette: TiePalette;
  PixelFormat: TPixelFormat32;
  nBits: integer;

begin
  Palette := TiePalette.Create;
  try
    // get palette
    PixelFormat := GetPixelFormat32(bm, palBW, palWin16, nil, Palette);
    nBits := pf32ToBitCount[PixelFormat];

    with Entry do
    begin
      bWidth := bm.Width;
      bHeight := bm.Height;
      if nBits < 8 then bColorCount := 1 shl nBits else bColorCount := 0;
      bReserved := 0;

      if AsCursor then
      begin
        wPlanes_XHotSpot := HotSpot.X;
        wBitCount_YHotSpot := HotSpot.Y;
      end else
      begin
        wPlanes_XHotSpot := 1;
        wBitCount_YHotSpot := nBits;
      end;

      dwImageOffset := s.Position - StreamZeroPoint;
    end;

    // write data
    if PNGCompress then
    // write as PNG
      pngSaveToStream(bm, s, PNG_COMPRESSION_HIGH, 0.0) else
      dibWrite(bm, s, True, PixelFormat, Palette, nil);
  finally
    Palette.Free;
  end;

  with Entry do dwBytesInRes := s.Position - StreamZeroPoint - dwImageOffset;
end;

function icoDetect;
begin
  Result := (Size >= 6) and (PICONDIR(Data).idReserved = 0) and
    (PICONDIR(Data).idType = IfThen(IsCursor, ID_CUR, ID_ICO)) and
    (PICONDIR(Data).idCount <> 0);
end;

function icoLoadFromStream;
var
  i, StreamZeroPoint: integer;
  dir: ICONDIR;
  Entry: ICONDIRENTRY;
  bm: TBitmap32;
  Page: TDocPage;
  HotSpot: TPoint;

begin
  Result := False;
  try
    // set the zero point
    StreamZeroPoint := s.Position;

    // load directory
    s.ReadBuffer(dir, SizeOf(dir));

    // verify data
    if dir.idCount = 0 then Exit;

    // load pages
    Doc.Clear;
    bm := TBitmap32.Create;
    try
      for i := 0 to dir.idCount - 1 do
      begin
        s.ReadBuffer(Entry, SizeOf(Entry));

        if icoLoadPageFromStream(bm, HotSpot, s,
          StreamZeroPoint, Entry, dir.idType = ID_CUR) then
        begin
          Page := Doc.NewPage;
          Page.HotSpot := HotSpot;
          Page.Layers.Assign(bm);
        end;
      end;
    finally
      bm.Free;
    end;

    Result := (Doc.PageCount <> 0);
  except
  end;
end;

procedure icoSaveToStream;
var
  i: integer;
  StreamZeroPoint, oldPos: integer;
  dir: ICONDIR;
  Entry: ICONDIRENTRY;
  bm: TBitmap32;
  r: TRect;

begin
  // set the zero point
  StreamZeroPoint := s.Position;

  // write directory
  dir.idReserved := 0;
  dir.idType := IfThen(AsCursor, ID_CUR, ID_ICO);
  dir.idCount := Doc.PageCount;
  s.WriteBuffer(dir, SizeOf(dir));

  // calculate size of directory entries
  s.Position := StreamZeroPoint + SizeOf(ICONDIR) +
    SizeOf(ICONDIRENTRY) * Doc.PageCount;

  // write all pages
  bm := TBitmap32.Create;
  try
    for i := 0 to Doc.PageCount - 1 do
    begin
      with Doc.Pages[i] do
      begin
        r := Rect(0, 0, Min(256, Layers.Width), Min(256, Layers.Height));
        Layers.Render(bm, r, lssAll, True);

        icoSavePageToStream(bm, HotSpot, s, StreamZeroPoint, Entry,
          PNGCompressIcon(bm.Width, bm.Height, PNGLimit), AsCursor);
      end;

      oldPos := s.Position;
        s.Position := StreamZeroPoint + SizeOf(ICONDIR) + SizeOf(ICONDIRENTRY) * i;
        s.WriteBuffer(Entry, SizeOf(Entry));
      s.Position := oldPos;
    end;
  finally
    bm.Free;
  end;
end;

function icoLoadFromFile;
var
  s: TStream;

begin
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := icoLoadFromStream(Doc, s);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure icoSaveToFile;
var
  s: TStream;

begin
  s := TFileStream.Create(fn, fmCreate);
  try
    icoSaveToStream(Doc, s, AsCursor, PNGLimit);
  finally
    s.Free;
  end;
end;

end.

