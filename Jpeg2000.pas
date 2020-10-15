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
unit Jpeg2000;

interface

uses
  LclIntf, LclType, CIntf, SysUtils, Classes, Graphics, BitmapEx,
  DocClass;

const
  FCC_JFIF = $4649464a;
  FCC_Exif = $66697845;

function jp2Detect(Data: Pointer; Size: integer): boolean;
function jp2LoadFromStream(bm: TBitmap32; s: TStream): boolean;
// Quality is the Peak Signal to Noise Ratio. If Quality=0, then
// image is saved as lossless JPEG 2000
procedure jp2SaveToStream(bm: TBitmap32; s: TStream; Quality: single);

function jpegDetect(Data: Pointer; Size: integer): boolean;
function jpegLoadDocFromStream(Doc: TIconDoc; s: TStream): boolean;
function jpegLoadBitmapFromStream(bm: TBitmap32; s: TStream): boolean;
procedure jpegSaveDocToStream(Doc: TIconDoc; s: TStream; Quality: integer);
procedure jpegSaveBitmapToStream(bm: TBitmap32; s: TStream; Quality: integer);

implementation

function jp2Detect;
begin
  Result := (Size >= 8) and
    (PInteger(Data)^ = $0c000000) and (PIntegerArray(Data)[1] = $2020506a);
end;

function jp2LoadFromStream;
var
  TotalSize, w, h: integer;
  Data: array of byte;
  jp2bm: Pointer;

begin
  Result := False;
  try
    TotalSize := s.Size - s.Position;
    SetLength(Data, TotalSize);
    s.ReadBuffer(Data[0], TotalSize);

    // try to convert
    if not jp2Load(jp2bm, w, h, @Data[0], TotalSize) then Exit;
    try
      bm.Resize(w, h);
      Move(jp2bm^, bm.Data^, w*h*4);
    finally
      jp2Free(jp2bm);
    end;

    // success
    Result := True;
  except
  end;
end;

procedure jp2SaveToStream;
var
  p: Pointer;
  DataSize: integer;

begin
  if jp2Save(bm.Data, bm.Width, bm.Height, Quality, p, DataSize) then
  try
    s.WriteBuffer(p^, DataSize);
  finally
    jp2Free(p);
  end;
end;

function jpegDetect;
begin
  Result := (Size >= 4) and (PInteger(Data)^ and $00ffffff = $00ffd8ff);
end;

function jpegLoadDocFromStream(Doc: TIconDoc; s: TStream): boolean;
var
  bm: TBitmap;
  bm32: TBitmap32;
  jpeg: TJpegImage;

begin
  Result := False;
  try
    bm32 := TBitmap32.Create;
    try
      bm := TBitmap.Create;
      try
        jpeg := TJpegImage.Create;
        try
          jpeg.LoadFromStream(s);
          bm.Assign(jpeg);
        finally
          jpeg.Free;
        end;

        bm32.Assign(bm);
      finally
        bm.Free;
      end;

      Doc.Clear;
      Doc.NewPage.Layers.Assign(bm32);

      // success
      Result := True;
    finally
      bm32.Free;
    end;
  except
  end;
end;

function jpegLoadBitmapFromStream(bm: TBitmap32; s: TStream): boolean;
var
  Doc: TIconDoc;

begin
  Doc := TIconDoc.Create;
  try
    Result := jpegLoadDocFromStream(Doc, s);
    if Result then
      bm.Assign(Doc.Pages[0].Layers);
  finally
    Doc.Free;
  end;
end;

procedure jpegSaveDocToStream(Doc: TIconDoc; s: TStream; Quality: integer);
var
  bm32: TBitmap32;
  bm: TBitmap;
  jpeg: TJpegImage;

begin
  // save on a white background
  bm32 := TBitmap32.Create;
  try
    bm32.Assign(Doc.Pages[0].Layers);
    bm := TBitmap.Create;
    try
      bm32.ToBitmap(bm, clWhite);

      jpeg := TJpegImage.Create;
      try
        jpeg.CompressionQuality := Quality;
        jpeg.Assign(bm);
        jpeg.SaveToStream(s);
      finally
        jpeg.Free;
      end;
    finally
      bm.Free;
    end;
  finally
    bm32.Free;
  end;
end;

procedure jpegSaveBitmapToStream(bm: TBitmap32; s: TStream; Quality: integer);
var
  Doc: TIconDoc;

begin
  Doc := TIconDoc.Create;
  try
    Doc.NewPage.Layers.Assign(bm);
    jpegSaveDocToStream(Doc, s, Quality);
  finally
    Doc.Free;
  end;
end;

end.

