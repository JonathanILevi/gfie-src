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
// Based on fpreadpnm.pp and fpwritepnm.pp of the Free Pascal RTL
{   * PBM (P1,P4) : Portable BitMap format : 1 bit per pixel
    * PGM (P2,P5) : Portable GrayMap format : 8 bits per pixel
    * PPM (P5,P6) : Portable PixelMap foramt : 24 bits per pixel}
unit PortableAnyMap;

interface

uses BitmapEx, bmExUtils, classes, sysutils, fpimage, graphics, streamex;

type
  TPnmReader = class
    private
      FBitMapType : Integer;
      FWidth      : Integer;
      FHeight     : Integer;
    protected
      FMaxVal     : Cardinal;
      FBitPP        : Byte;
      FScanLineSize : Integer;
      FScanLine   : PByte;
      procedure ReadHeader(Stream : TBufferedReader);
      procedure InternalRead(Stream:TStream;Img:TBitmap32);
      procedure ReadScanLine(Row : Integer; Stream:TBufferedReader);
      procedure WriteScanLine(Row : Integer; Img : TBitmap32);
  end;

type
  TPNMColorDepth = (pcdAuto,pcdBlackWhite, pcdGrayscale, pcdRGB);

  { TPnmWriter }

  TPnmWriter = class
    protected
      procedure InternalWrite(Stream:TStream;Img:TBitmap32);
    public
      ColorDepth: TPNMColorDepth;
      BinaryFormat: boolean;
      function GuessColorDepthOfImage(Img: TBitmap32): TPNMColorDepth;
      function GetColorDepthOfExtension(AExtension: string): TPNMColorDepth;
      function GetFileExtension(AColorDepth: TPNMColorDepth): string;
      constructor Create; virtual;
  end;

function pbmDetect(Data: Pointer; Size: integer): boolean;
function pgmDetect(Data: Pointer; Size: integer): boolean;
function pnmDetect(Data: Pointer; Size: integer): boolean;
function ppmDetect(Data: Pointer; Size: integer): boolean;

function pbmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
function pgmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
function pnmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
function ppmLoadFromStream(bm: TBitmap32; s: TStream): boolean;

procedure pbmSaveToStream(bm: TBitmap32; s: TStream);
procedure pgmSaveToStream(bm: TBitmap32; s: TStream);
procedure pnmSaveToStream(bm: TBitmap32; s: TStream; ColorDepth: TPNMColorDepth = pcdAuto);
procedure ppmSaveToStream(bm: TBitmap32; s: TStream);

implementation

function pbmDetect(Data: Pointer; Size: integer): boolean;
begin
  result := (PChar(Data)[0] = 'P') and ((PChar(Data)[1] = '1') or (PChar(Data)[1] = '4'));
end;

function pgmDetect(Data: Pointer; Size: integer): boolean;
begin
  result := (PChar(Data)[0] = 'P') and ((PChar(Data)[1] = '2') or (PChar(Data)[1] = '5'));
end;

function pnmDetect(Data: Pointer; Size: integer): boolean;
begin
  result := (PChar(Data)[0] = 'P') and (PChar(Data)[1] >= '1') and (PChar(Data)[1] <= '6');
end;

function ppmDetect(Data: Pointer; Size: integer): boolean;
begin
  result := (PChar(Data)[0] = 'P') and ((PChar(Data)[1] = '3') or (PChar(Data)[1] = '6'));
end;

function pbmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
begin
  result := pnmLoadFromStream(bm, s);
end;

function pgmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
begin
  result := pnmLoadFromStream(bm, s);
end;

function pnmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
var
  r: TPnmReader;
begin
  Result := false;
  try
    r := TPnmReader.Create;
    try
      r.InternalRead(s, bm);
    finally
      r.Free;
    end;
    Result := true;
  except
  end;
end;

function ppmLoadFromStream(bm: TBitmap32; s: TStream): boolean;
begin
  result := pnmLoadFromStream(bm, s);
end;

procedure pbmSaveToStream(bm: TBitmap32; s: TStream);
begin
  pnmSaveToStream(bm, s, pcdBlackWhite);
end;

procedure pgmSaveToStream(bm: TBitmap32; s: TStream);
begin
  pnmSaveToStream(bm, s, pcdGrayscale);
end;

procedure pnmSaveToStream(bm: TBitmap32; s: TStream; ColorDepth: TPNMColorDepth = pcdAuto);
var
  r: TPnmWriter;
begin
  r := TPnmWriter.Create;
  try
    r.BinaryFormat:=true;
    r.ColorDepth:=ColorDepth;
    r.InternalWrite(s, bm);
  finally
    r.Free;
  end;
end;

procedure ppmSaveToStream(bm: TBitmap32; s: TStream);
begin
  pnmSaveToStream(bm, s, pcdRGB);
end;

const
  WhiteSpaces=[#9,#10,#13,#32]; {Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}

function DropWhiteSpaces(Stream: TBufferedReader) :Char;

begin
  with Stream do
    begin
    repeat
      Read(@result,1);
      if result='#' then
      repeat
        Read(@result,1);
      until result=#10;
    until not(result in WhiteSpaces);
    end;
end;

function ReadInteger(Stream : TBufferedReader) :Integer;

var
  s:String[7];

begin
  s:='';
  s[1]:=DropWhiteSpaces(Stream);
  with Stream do
    repeat
      Inc(s[0]);
      Read(@s[Length(s)+1],1)
    until (s[0]=#7) or (s[Length(s)+1] in WhiteSpaces);
  Result:=StrToInt(s);
end;

procedure TPnmReader.ReadHeader(Stream : TBufferedReader);

Var
  C : Char;

begin
  Stream.Read(@C,1);
  If (C<>'P') then
    Raise Exception.Create('Not a valid PNM image.');
  Stream.Read(@C,1);
  FBitmapType:=Ord(C)-Ord('0');
  If Not (FBitmapType in [1..6]) then
    Raise Exception.CreateFmt('Unknown PNM subtype : %s',[C]);
  FWidth:=ReadInteger(Stream);
  FHeight:=ReadInteger(Stream);
  if FBitMapType in [1,4]
  then
    FMaxVal:=1
  else
    FMaxVal:=ReadInteger(Stream);
  If (FWidth<=0) or (FHeight<=0) or (FMaxVal<=0) then
    Raise Exception.Create('Invalid PNM header data');
  case FBitMapType of
    1: FBitPP := 1;                  // 1bit PP (text)
    2: FBitPP := 8 * SizeOf(Word);   // Grayscale (text)
    3: FBitPP := 8 * SizeOf(Word)*3; // RGB (text)
    4: FBitPP := 1;            // 1bit PP (raw)
    5: If (FMaxval>255) then   // Grayscale (raw);
         FBitPP:= 8 * 2
       else
         FBitPP:= 8;
    6: if (FMaxVal>255) then    // RGB (raw)
         FBitPP:= 8 * 6
       else
         FBitPP:= 8 * 3
  end;
//  Writeln(FWidth,'x',Fheight,' Maxval: ',FMaxVal,' BitPP: ',FBitPP);
end;

procedure TPnmReader.InternalRead(Stream:TStream;Img:TBitmap32);

var
  Row:Integer;
  br: TBufferedReader;

begin
  br := TBufferedReader.Create(stream);
  try
    ReadHeader(br);
    Img.Resize(FWidth,FHeight);
    FScanLineSize:=(FBitPP*FWidth + 7) shr 3;
    GetMem(FScanLine,FScanLineSize);
    try
      for Row:=0 to img.Height-1 do
        begin
        ReadScanLine(Row,br);
        WriteScanLine(Row,Img);
        end;
    finally
      FreeMem(FScanLine);
    end;
  finally
    br.Free;
  end;
end;

procedure TPnmReader.ReadScanLine(Row : Integer; Stream:TBufferedReader);

Var
  P : PWord;
  I,j,bitsLeft : Integer;
  PB: PByte;

begin
  Case FBitmapType of
    1 : begin
        PB:=FScanLine;
        For I:=0 to ((FWidth+7)shr 3)-1 do
          begin
            PB^:=0;
            bitsLeft := FWidth-(I shl 3)-1;
            if bitsLeft > 7 then bitsLeft := 7;
            for j:=0 to bitsLeft do
              PB^:=PB^ or (ReadInteger(Stream) shl (7-j));
            Inc(PB);
          end;
        end;
    2 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream);
          Inc(P);
          end;
        end;
    3 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream); // Red
          Inc(P);
          P^:=ReadInteger(Stream); // Green
          Inc(P);
          P^:=ReadInteger(Stream); // Blue;
          Inc(P)
          end;
        end;
    4,5,6 : Stream.Read(FScanLine,FScanLineSize);
    end;
end;


procedure TPnmReader.WriteScanLine(Row : Integer; Img : TBitmap32);

Var
  C : TFPColor;
  L : Cardinal;
  Scale: Cardinal;

  function ScaleByte(B: Byte):Word; inline;
  begin
    if FMaxVal = 255 then
      Result := (B shl 8) or B { As used for reading .BMP files }
    else { Mimic the above with multiplications }
      Result := (B*(FMaxVal+2)) * 65535 div Scale;
  end;

  function ScaleWord(W: Word):Word; inline;
  begin
    if FMaxVal = 65535 then
      Result := W
    else { Mimic the above with multiplications }
      Result := Int64(W*(FMaxVal+2)) * 65535 div Scale;
  end;

  Procedure ByteBnWScanLine;

  Var
    P : PByte;
    I,j,x,bitsLeft : Integer;
    pc: PColor32;

  begin
    P:=PByte(FScanLine);
    For I:=0 to ((FWidth+7)shr 3)-1 do
      begin
      L:=P^;
      x := I shl 3;
      pc := Img.PixelAddr(x,Row);
      bitsLeft := FWidth-x-1;
      if bitsLeft > 7 then bitsLeft := 7;
      for j:=0 to bitsLeft do
        begin
          if L and $80 <> 0 then
            pc^:=cl32Black
          else
            pc^:=cl32White;
          L:=L shl 1;
          inc(x);
          inc(pc);
        end;
      Inc(P);
      end;
  end;

  Procedure WordGrayScanLine;

  Var
    P : PWord;
    I : Integer;
    pc: PColor32;

  begin
    P:=PWord(FScanLine);
    C.Alpha := AlphaOpaque;
    pc := img.pixeladdr(0, row);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleWord(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      pc^:=FPColorTo32(C);
      inc(pc);
      Inc(P);
      end;
  end;

  Procedure WordRGBScanLine;

  Var
    P : PWord;
    I : Integer;
    pc: PColor32;

  begin
    P:=PWord(FScanLine);
    C.Alpha := AlphaOpaque;
    pc := img.pixeladdr(0, row);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleWord(P^);
      Inc(P);
      C.Green:=ScaleWord(P^);
      Inc(P);
      C.Blue:=ScaleWord(P^);
      pc^:=FPColorTo32(C);
      inc(pc);
      Inc(P);
      end;
  end;

  Procedure ByteGrayScanLine;

  Var
    P : PByte;
    I : Integer;
    pc: PColor32;

  begin
    P:=PByte(FScanLine);
    C.Alpha := AlphaOpaque;
    pc := img.pixeladdr(0,row);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleByte(P^);
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      pc^:=FPColorTo32(C);
      inc(pc);
      Inc(P);
      end;
  end;

  Procedure ByteRGBScanLine;

  Var
    P : PByte;
    I : Integer;
    pc: PColor32;

  begin
    P:=PByte(FScanLine);
    C.Alpha := AlphaOpaque;
    pc := img.pixeladdr(0,row);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=ScaleByte(P^);
      Inc(P);
      C.Green:=ScaleByte(P^);
      Inc(P);
      C.Blue:=ScaleByte(P^);
      pc^:=FPColorTo32(C);
      inc(pc);
      Inc(P);
      end;
  end;

begin
  C.Alpha:=AlphaOpaque;
  Scale := FMaxVal*(FMaxVal+2);
  Case FBitmapType of
    1 : ByteBnWScanLine;
    2 : WordGrayScanline;
    3 : WordRGBScanline;
    4 : ByteBnWScanLine;
    5 : If FBitPP=8 then
          ByteGrayScanLine
        else
          WordGrayScanLine;
    6 : If FBitPP=24 then
          ByteRGBScanLine
        else
          WordRGBScanLine;
    end;
end;

{ TPnmWriter }

constructor TPnmWriter.Create;
begin
  inherited Create;
  ColorDepth := pcdAuto;
  BinaryFormat := True;
end;

procedure TPnmWriter.InternalWrite(Stream:TStream;Img:TBitmap32);
var useBitMapType: integer;
  bw: TBufferedWriter;

  function SaveHeader:boolean;
    const
      MagicWords:Array[1..6]OF String[2]=('P1','P2','P3','P4','P5','P6');
    var
      PNMInfo:String;
      strWidth,StrHeight:String[15];
    begin
      SaveHeader:=false;
      Str(Img.Width,StrWidth);
      Str(Img.Height,StrHeight);
      PNMInfo:=Concat(MagicWords[useBitMapType],#10,StrWidth,#32,StrHeight,#10);
      if useBitMapType in [2,3,5,6]
      then
        PNMInfo:=Concat(PNMInfo,'255'#10);
      bw.Write(@PNMInfo[1],Length(PNMInfo));
      SaveHeader := true;
    end;
  var
    Row,Coulumn,nBpLine,i:Integer;
    aColor:TFPColor;
    aLine:PByte;
    strCol:String[3];
    LinuxEndOfLine: char;
    UseColorDepth: TPNMColorDepth;
    cl32: TColor32;
    cl24: TColor;

  begin
    bw := TBufferedWriter.Create(stream);
    try
      LinuxEndOfLine := #10;

      //determine color depth
      if ColorDepth = pcdAuto then
        UseColorDepth := GuessColorDepthOfImage(Img) else
        UseColorDepth := ColorDepth;

      //determine file format number (1-6)
      case UseColorDepth of
        pcdBlackWhite: useBitMapType := 1;
        pcdGrayscale: useBitMapType := 2;
        pcdRGB: useBitMapType := 3;
      end;
      if BinaryFormat then inc(useBitMapType,3);

      SaveHeader;
      case useBitMapType of
        1:nBpLine:=Img.Width*2;{p p p}
        2:nBpLine:=Img.Width*4;{lll lll lll}
        3:nBpLine:=Img.Width*3*4;{rrr ggg bbb rrr ggg bbb}
        4:nBpLine:=(Img.Width+7) SHR 3;
        5:nBpLine:=Img.Width;
        6:nBpLine:=Img.Width*3;
      end;
      GetMem(aLine,nBpLine);//3 extra byte for BMP 4Bytes alignement.
      for Row:=0 to img.Height-1 do
        begin
          FillChar(aLine^,nBpLine,0);
          for Coulumn:=0 to img.Width-1 do
            begin
              cl24 := Put32to24(clWhite, img.PixelAddr(Coulumn,Row)^);
              cl32 := TColor32(cl24) or cl32Opaque;
              aColor:=FPColorFrom32(cl32);
              with aColor do
                case useBitMapType of
                  1:begin
                      if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                      then
                        aLine[2*Coulumn]:=Ord('1')
                      else
                        aLine[2*Coulumn]:=Ord('0');
                      aLine[2*Coulumn+1]:=32;
                    end;
                  2:begin
                      Str(Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114))),strCol);
                      for i:=0 to Length(StrCol)-1 do
                        aLine[4*Coulumn+i]:=Ord(StrCol[i+1]);
                      for i:=Length(StrCol) to 4 do
                        aLine[4*Coulumn+i]:=32;
                    end;
                  3:begin
                      Str(Hi(Red),strCol);
                      for i:=0 to Length(StrCol)-1 do
                        aLine[4*(3*Coulumn)+i]:=Ord(StrCol[i+1]);
                      for i:=Length(StrCol) to 4 do
                        aLine[4*(3*Coulumn)+i]:=32;
                      Str(Hi(Green),strCol);
                      for i:=0 to Length(StrCol)-1 do
                        aLine[4*(3*Coulumn+1)+i]:=Ord(StrCol[i+1]);
                      for i:=Length(StrCol) to 4 do
                        aLine[4*(3*Coulumn+1)+i]:=32;
                      Str(Hi(Blue),strCol);
                      for i:=0 to Length(StrCol)-1 do
                        aLine[4*(3*Coulumn+2)+i]:=Ord(StrCol[i+1]);
                      for i:=Length(StrCol) to 4 do
                        aLine[4*(3*Coulumn+2)+i]:=32;
                    end;
                  4:if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                    then
                      aLine[Coulumn shr 3]:=aLine[Coulumn shr 3] or ($80 shr (Coulumn and $07));
                  5:aLine[Coulumn]:=Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114)));
                  6:begin
                      aLine[3*Coulumn]:=Hi(Red);
                      aLine[3*Coulumn+1]:=Hi(Green);
                      aLine[3*Coulumn+2]:=Hi(Blue);
                    end;
              end;
            end;
          bw.Write(aLine,nBpLine);
          if useBitMapType in[1..3] then bw.Write(@LinuxEndOfLine,1);
        end;
      FreeMem(aLine,nBpLine);
    finally
      bw.Free;
    end;
  end;

function TPnmWriter.GetColorDepthOfExtension(AExtension: string
  ): TPNMColorDepth;
begin
  if (length(AExtension) > 0) and (AExtension[1]='.') then
    delete(AExtension,1,1);
  AExtension := LowerCase(AExtension);
  if AExtension='pbm' then result := pcdBlackWhite else
  if AExtension='pgm' then result := pcdGrayscale else
  if AExtension='ppm' then result := pcdRGB else
    result := pcdAuto;
end;

function TPnmWriter.GuessColorDepthOfImage(Img: TBitmap32): TPNMColorDepth;
var Row, Col: integer;
    aColor: TFPColor;
    cl32: TColor32;
begin
   result := pcdBlackWhite;
   for Row:=0 to img.Height-1 do
     for Col:=0 to img.Width-1 do
     begin
       cl32 := TColor32(Put32to24(clWhite, img.PixelAddr(Col,Row)^)) or cl32Opaque;
       aColor:=FPColorFrom32(cl32);
       if (AColor.red >= 256) and (AColor.green >= 256) and (AColor.blue >= 256) and
          (AColor.red < $FF00) and (AColor.green < $FF00) and (AColor.blue < $FF00) then
       begin
          if (AColor.red shr 8 <> AColor.Green shr 8) or
             (AColor.blue shr 8 <> AColor.Green shr 8) or
             (AColor.red shr 8 <> AColor.blue shr 8) then
          begin
             result := pcdRGB;
             exit;
          end else
            result := pcdGrayscale;
       end;
     end;
end;

function TPnmWriter.GetFileExtension(AColorDepth: TPNMColorDepth): string;
begin
  case AColorDepth of
    pcdBlackWhite: result := 'pbm';
    pcdGrayscale: result := 'pgm';
    pcdRGB: result := 'ppm';
  else
    result := 'pnm';
  end;
end;

end.
