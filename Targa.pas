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
// Based on fpreadtga.pp and fpwritetga.pp of the Free Pascal RTL
unit Targa;

interface

uses BitmapEx, bmExUtils, classes, sysutils, targacmn, fpimage;

type
  TTargaReader = class
  Private
    Procedure FreeBuffers;       // Free (and nil) buffers.
  protected
    Header         : TTargaHeader;
    AlphaBits      : Byte;
    Identification : ShortString;
    Compressed,
    BottomUp       : Boolean;
    BytesPerPixel  : Byte;
    FPalette       : PFPColor;
    FScanLine      : PByte;
    FLineSize      : Integer;
    FPaletteSize   : Integer;
    FBlockCount    : Integer;
    FPixelCount    : Integer;
    FLastPixel     : Packed Array[0..3] of byte;
  public
    // AnalyzeHeader will allocate the needed buffers.
    Procedure AnalyzeHeader(Img : TBitmap32);
    procedure CreateGrayPalette;
    Procedure ReadPalette(Stream : TStream);
    procedure ReadScanLine(Row : Integer; Stream : TStream);
    procedure WriteScanLine(Row : Integer; Img : TBitmap32);
    procedure InternalRead  (Stream:TStream; Img:TBitmap32);
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TTargaWriter = class
  protected
    function  SaveHeader(Stream:TStream; Img: TBitmap32):boolean;
    procedure InternalWrite (Stream:TStream; Img: TBitmap32);
  end;

function tgaDetect(Data: Pointer; Size: integer): boolean;
function tgaLoadFromStream(bm: TBitmap32; s: TStream): boolean;
procedure tgaSaveToStream(bm: TBitmap32; s: TStream);
function tgaLoadFromFile(bm: TBitmap32; const fn: string): boolean;
procedure tgaSaveToFile(bm: TBitmap32; const fn: string);

Implementation

uses dlgDebug;

const
  TARGA_EMPTY_IMAGE = 0;
  TARGA_INDEXED_IMAGE = 1;
  TARGA_TRUECOLOR_IMAGE = 2;
  TARGA_GRAY_IMAGE = 3;

type PTargaHeader = ^TTargaHeader;

function tgaDetect(Data: Pointer; Size: integer): boolean;
var
  h: PTargaHeader;
  alphabits, w, hh: integer;

begin
  // TGA has no signature unfortunately.
  if Size < SizeOf(TTargaHeader)+1 then Exit(false);
  h := Data;
  if h.MapType >= 2 then Exit(false);
  if not (h.ImgType in [1, 2, 3, 9, 10, 11]) or
    not (h.PixelSize in [8, 16, 24, 32]) then Exit(false);
  w := ToWord(h.Width);
  hh := ToWord(h.Height);
//  x := ToWord(h.OriginX);
//  y := ToWord(h.OriginY);
  if (w = 0) or (hh = 0) then Exit(false);
  if (w > 30000) or (hh > 30000) then Exit(false);
  AlphaBits := h.Flags and $0F;
  if (alphabits <> 0) and (alphabits <> 1) and (alphabits <> 8) then Exit(False);
  Result := true;
end;

function tgaLoadFromStream(bm: TBitmap32; s: TStream): boolean;
var
  r: TTargaReader;
begin
  Result := false;
  try
    r := TTargaReader.Create;
    try
      r.InternalRead(s, bm);
    finally
      r.Free;
    end;
    Result := true;
  except
  end;
end;

procedure tgaSaveToStream(bm: TBitmap32; s: TStream);
var
  r: TTargaWriter;
begin
  r := TTargaWriter.Create;
  try
    r.InternalWrite(s, bm);
  finally
    r.Free;
  end;
end;

function tgaLoadFromFile(bm: TBitmap32; const fn: string): boolean;
var
  s: TStream;

begin
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := tgaLoadFromStream(bm, s);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure tgaSaveToFile(bm: TBitmap32; const fn: string);
var
  s: TStream;

begin
  s := TFileStream.Create(fn, fmCreate);
  try
    tgaSaveToStream(bm, s);
  finally
    s.Free;
  end;
end;

Constructor TTargaReader.Create;
begin
end;

Destructor TTargaReader.Destroy;

begin
  FreeBuffers;
  Inherited;
end;

Procedure TTargaReader.FreeBuffers;

begin
  If (FScanLine<>Nil) then
    begin
    FreeMem(FScanLine);
    FScanLine:=Nil;
    end;
  If (FPalette<>Nil) then
    begin
    FreeMem(FPalette);
    FScanLine:=Nil;
    end;
end;

Procedure TTargaReader.AnalyzeHeader(Img : TBitmap32);

begin
  With Header do
    begin
    if not (ImgType in [1, 2, 3, 9, 10, 11]) or
       not (PixelSize in [8, 16, 24, 32]) then
    begin
      Raise Exception.Create('Unknown/Unsupported Targa image type');
    end;
    BottomUp:=(Flags and $20) <>0;
    AlphaBits := Flags and $0F;
    BytesPerPixel:=PixelSize;
    Compressed:=ImgType>=8;
    If Compressed then
      ImgType:=ImgType-8;
    FLineSize:=(BytesPerPixel div 8)*ToWord(Width);
    GetMem(FScanLine,FLineSize);

    if ImgType = TARGA_GRAY_IMAGE then
      FPaletteSize:=SizeOf(TFPColor)*255
    else
      FPaletteSize:=SizeOf(TFPColor)*ToWord(MapLength);
    GetMem(FPalette,FPaletteSize);
    Img.Resize(ToWord(Width), ToWord(Height));
    end;
end;

Procedure TTargaReader.CreateGrayPalette;

Var
  I : Integer;

Begin
  For I:=0 To 255 Do
  Begin
    With FPalette[I] do
      begin
      Red:=I*255;
      Green:=I*255;
      Blue:=I*255;
      Alpha:=AlphaOpaque;
      end;
  end;
End;

Procedure TTargaReader.ReadPalette(Stream : TStream);

Var
  BGREntry : TBGREntry;
  BGRAEntry : TBGRAEntry;
  w: word;
  I : Integer;

begin
  Case Header.MapEntrySize Of
     16:
       For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          w := 0;
          Stream.ReadBuffer(w, 2);
          With FPalette[I] do
            begin
            Red:=((W)shr 10) shl 11;
            Green:=((w)shr 5) shl 11;
            Blue:=((w)) shl 11;
            Alpha:=alphaOpaque;
            end;
        end;
     24:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGREntry, SizeOf(BGREntry));
          With FPalette[I] do
            begin
            Red:=BGREntry.Red shl 8;
            Green:=BGREntry.Green shl 8;
            Blue:=BGREntry.Blue shl 8;
            Alpha:=alphaOpaque;
            end;
        end;
     32:
        For I:=0 to ToWord(Header.MapLength)-1 do
        begin
          Stream.ReadBuffer(BGRAEntry,SizeOf(BGRAEntry));
          With FPalette[I] do
            begin
            Red:=BGRAEntry.Red shl 8;
            Green:=BGRAEntry.Green shl 8;
            Blue:=BGRAEntry.Blue shl 8;
            alpha := BGRAEntry.Alpha shl 8;
            end;
        end;
    end;
end;


Procedure TTargaReader.InternalRead  (Stream:TStream; Img:TBitmap32);

var
  H,Row : Integer;
  pc: PColor32;
  pb: PByte;
  ok: boolean;
  alpha: byte;
  i: integer;

begin
  Stream.Read(Header,SizeOf(Header));
  AnalyzeHeader(Img);
  If Header.IdLen>0 then
    begin
    SetLength(Identification,Header.IDLen);
    Stream.Read(Identification[1],Header.Idlen);
    //If Length(Identification)<>0 then
    //  Img.Extra[KeyIdentification]:=Identification;
    end;

  If Header.MapType<>0 then
    ReadPalette(Stream);
  if Header.ImgType = TARGA_GRAY_IMAGE then
    CreateGrayPalette;

  H:=Img.height;
  If BottomUp then
    For Row:=0 to H-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end
  else
    For Row:=H-1 downto 0 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
  if AlphaBits = 8 then
  begin
    // check images with alpha=0 but where alpha should be 255
    pc := img.Data;
    ok := False;
    for i := 0 to img.Width*img.Height-1 do
    begin
      alpha := PByteArray(pc)[3];
      if alpha > 0 then
      begin
        ok := false;
        break;
      end;
      if (alpha = 0) and (pc^ <> cl32Transparent) then
        ok := True;
    end;
    if ok then // set alpha to 255
    begin
      pb := img.Data;
      inc(pb, 3);
      for i := 0 to img.Width*img.Height-1 do
      begin
        pb^ := 255;
        inc(pb, 4);
      end;
    end;
  end;
end;

Procedure TTargaReader.ReadScanLine(Row : Integer; Stream : TStream);

Var
  P : PByte;
  B : Byte;
  I,J : Integer;

begin
  If Not Compressed then
    Stream.ReadBuffer(FScanLine^,FLineSize)
  else
    begin
    P:=FScanLine;
    For I:=0 to ToWord(Header.Width)-1 do
      begin
      If (FPixelCount>0) then
        Dec(FPixelCount)
      else
        begin
        Dec(FBlockCount);
        If (FBlockCount<0) then
          begin
          Stream.ReadBuffer(B,1);
          If (B and $80)<>0 then
            begin
            FPixelCount:=B and $7F;
            FblockCount:=0;
            end
          else
            FBlockCount:=B and $7F
          end;
        Stream.ReadBuffer(FlastPixel,BytesPerPixel shr 3);
        end;
      For J:=0 to (BytesPerPixel shr 3)-1 do
        begin
        P[0]:=FLastPixel[j];
        Inc(P);
        end;
      end;
    end;
end;

Procedure TTargaReader.WriteScanLine(Row : Integer; Img : TBitmap32);

Var
  Col : Integer;
  C: TFPColor;
  W   : Word;
  P   : PByte;

begin
  C.Alpha:=AlphaOpaque;
  P:=FScanLine;
  Case Header.ImgType of
    TARGA_INDEXED_IMAGE
      : for Col:=0 to Img.width-1 do
         Img.PixelAddr(Col,Row)^:=FPColorTo32(FPalette[P[Col]]);
    TARGA_TRUECOLOR_IMAGE
      : for Col:=0 to Img.Width-1 do
          begin
          // Fill C depending on number of pixels.
          case BytesPerPixel of
          8,16 : begin
                 W:=P[0];
                 inc(P);
                 W:=W or (P[0] shl 8);
                   with C do
                   begin
                   Red:=((W)shr 10) shl 11;
                   Green:=((w)shr 5) shl 11;
                   Blue:=((w)) shl 11;
                   end;
                end;
          24,32 : with C do
                  begin
                  Blue:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Green:=P[0] or (P[0] shl 8);
                  Inc(P);
                  Red:=P[0] or (P[0] shl 8);
                  Alpha:=AlphaOpaque;
                  If bytesPerPixel=32 then
                    begin
                      Inc(P);
                      alpha:=P[0] or (P[0] shl 8);
                    end;
                  end;
          end; // Case BytesPerPixel;
          Img.PixelAddr(Col,Row)^:=FPColorTo32(c);
          Inc(P);
          end;
    TARGA_GRAY_IMAGE
      :  case BytesPerPixel of
           8 : for Col:=0 to Img.width-1 do
                 Img.PixelAddr(Col,Row)^:=FPColorTo32(FPalette[P[Col]]);
          16 : for Col:=0 to Img.width-1 do
               begin
                 with C do
                 begin
                   Blue:=FPalette[P^].blue;
                   Green:=FPalette[P^].green;
                   Red:=FPalette[P^].red;
                   Inc(P);
                   alpha := p[0] or (p[0] shl 8);
                   Inc(P);
                 end;
               Img.PixelAddr(Col,Row)^:=FPColorTo32(c);
               end;
         end;
  end;
end;

function TTargaWriter.SaveHeader(Stream:TStream; Img : TBitmap32):boolean;

var
  Header : TTargaHeader;

begin
  Result:=False;
  FillChar(Header,SizeOf(Header),0);
  With Header do
    begin
    IDLen:=0;
    MapType:=0; // No colormap. Uncompressed RGB Only.
    ImgType:=2; // Uncompressed RGB
    MapStart:=FromWord(0); // No data
    MapLength:=FromWord(0); // No colormap yet.
    MapEntrySize:=0; // No colormap yet.
    OriginX:= FromWord(0);
    OriginY:=FromWord(0);
    Width:=FromWord(Img.Width);
    Height:=FromWord(Img.Height);
    PixelSize:=32; // BGRA data.
    Flags:=$28; // Top-down, non interlaced, 8 alpha bits.
  end;
  Stream.WriteBuffer(Header,SizeOf(Header));
  Result:=true;
end;

procedure TTargaWriter.InternalWrite (Stream:TStream; Img:TBitmap32);

var
  Row,Col,WriteSize:Integer;
  Aline,P: PByte;
  C : TFPColor;

begin
  SaveHeader(Stream,Img);
  WriteSize:=Img.Width*4;
  GetMem(aLine,WriteSize);
  Try
    for Row:=0 to Img.Height-1 do
      begin
      P:=ALine;
      For Col:=0 to Img.width-1 do
        begin
        C:=FPColorFrom32(Img.PixelAddr(Col,Row)^);
        P^:=C.Blue shr 8;
        Inc(P);
        P^:=C.Green shr 8;
        Inc(P);
        P^:=C.Red shr 8;
        Inc(P);
        P^:=C.Alpha shr 8;
        Inc(P);
        end;
      Stream.Write(aLine[0],WriteSize);
      end;
  Finally
    FreeMem(aLine);
  end;
end;

initialization
end.

