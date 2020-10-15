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
unit GIF;

{$mode delphi}

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Graphics, BitmapEx, bmExUtils, DocClass,
  PixelFormats, ColQuant, Math, StreamEx, CIntf, dlgdebug, MapClass, GenericList;

// THE FOLLOWING FUNCTIONS RETURN THE CALCULATED PALETTE, IF Palette <> nil
// Verifies if a bitmap has at most 256 colors, including the transparent color
// Palette and Transparent valid if Result = True
function gifGet256Colors(bm: TBitmap32; Palette: TiePalette;
  out Transparent: boolean): boolean;
// Makes the bitmap 255-colors, if has transparency, and
// 256-colors, if it does not have that
procedure gifSet256Colors(bm: TBitmap32; Palette: TiePalette;
  out Transparent: boolean);

function gifDetect(Data: Pointer; Size: integer): boolean;
function gifLoadFromStream(Doc: TIconDoc; s: TStream; MaxWidth, MaxHeight: integer): boolean;
procedure gifSaveToStream(Doc: TIconDoc; s: TStream);

implementation

function gifGet256Colors;
var
  i: integer;
  palNeedDestroy: boolean;
  p: PColor32;

begin
  palNeedDestroy := not Assigned(Palette);
  if Assigned(Palette) then Palette.Clear else Palette := TiePalette.Create;

  try
    Result := False;
    Transparent := False;
    p := bm.Data;
    for i := 1 to bm.Width * bm.Height do
    begin
      case PByteArray(p)[3] of
        0: (*transparent*) begin
          Transparent := True;
          if Palette.Count + IfThen(Transparent, 1, 0) > 256 then Exit;
        end;

        $ff: (*solid*) begin
          Palette.Add(p^ and $ffffff, False);
          if Palette.Count + IfThen(Transparent, 1, 0) > 256 then Exit;
        end;

        else Exit; // partially transparent images are not supported
      end; // case Alpha Channel

      // iterate
      inc(p);
    end;

    // it has at most 256 colors, including the transparent one
    Result := True;
  finally
    if palNeedDestroy then Palette.Free;
  end;
end;

procedure gifSet256Colors;
var
  i, nColors: integer;
  p: PByteArray;
  palNeedDestroy: boolean;
  cq: TColorQuantizer;

begin
  // has already <= 256 colors?
  if gifGet256Colors(bm, Palette, Transparent) then Exit;

  // look for transparency
  Transparent := False;
  p := bm.Data;
  for i := 1 to bm.Width * bm.Height do
  begin
    if p[3] < $80 then begin
      Transparent := True;
      Break;
    end;

    inc(PColor32(p));
  end;
  // we have to reserve one place on the palette for transparency, if present
  nColors := 256 - IfThen(Transparent, 1, 0);

  // Create the Palette object, if needed
  palNeedDestroy := not Assigned(Palette);
  if Assigned(Palette) then Palette.Clear else Palette := TiePalette.Create;

  try
    // Create an adaptive palette
    cq := TOctreeQuantizer.Create(nColors);
    try
      p := bm.Data;
      for i := 1 to bm.Width * bm.Height do
      begin
        if p[3] >= $80 then cq.Process(PInteger(p)^ and $ffffff);
        inc(PColor32(p));
      end;

      // create palette
      cq.GetPalette(Palette);
    finally
      cq.Free;
    end; // try cq

    // apply the created palette
    Palette.ApplyToBitmap(bm, True);
  finally
    if palNeedDestroy then Palette.Free;
  end;
end;

const
  GIF_LOOP_INFINITY = 0;

type
  TLogicalScreenDescriptor = packed record
    Width, Height: word;
    PackedFields: byte;
    BackgroundIndex: byte;
    AspectRatio: byte;
  end;

  TApplicationExtension = packed record
    Magic1: byte; // 0x21
    Magic2: byte; // 0xff
    BlockSize: byte; // 11
    AppId: packed array[0..10] of byte; // app name and "auth code"

    // AAppId should be exactly 11 characters long
    procedure Init(const AAppId: string);
  end;

  procedure TApplicationExtension.Init(const AAppId: string);
  begin
    Magic1 := $21;
    Magic2 := $ff;
    BlockSize := 11;
    Move(AAppId[1], AppId[0], 11);
  end;

type
  TLoopCountData = packed record
    Length: byte;
    One: byte;
    LoopCount: word;
    Terminator: byte;

    procedure Init(const ALoopCount: word);
  end;

  procedure TLoopCountData.Init(const ALoopCount: word);
  begin
    Length := 3;
    One := 1;
    LoopCount := ALoopCount;
    Terminator := 0;
  end;

type
  TGraphicControlExtension = packed record
    Magic1: byte; // 0x21
    Magic2: byte; // 0xf9
    BlockSize: byte; // 4
    PackedFields: byte;
    DelayTime: word; // 1/100 s
    TransparentIndex: byte;
    Terminator: byte; // 0

    procedure Init(const ADelayTime: word; const ATransparentIndex: integer);
  end;

  procedure TGraphicControlExtension.Init;
  begin
    Magic1 := $21;
    Magic2 := $f9;
    BlockSize := 4;
    PackedFields := IfThen(ATransparentIndex >= 0, 9, 8);
    DelayTime := ADelayTime;
    TransparentIndex := IfThen(ATransparentIndex >= 0, ATransparentIndex, 0);
    Terminator := 0;
  end;

type
  TImageDescriptor = packed record
    Magic: byte; // 0x2c
    Left, Top, Width, Height: word;
    PackedFields: byte;
  end;

  TRGB = packed record
    r, g, b: byte;
  end;

type
  TGifDocument = class
  public
    LoopCount: integer;
    Frames: TList; // list of TBitmap32
    Delays: array of integer;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function Load(st: TStream): boolean;
    procedure Save(st: TStream);
  end;

constructor TGifDocument.Create;
begin
  Frames := TList.Create;
  Clear;
end;

destructor TGifDocument.Destroy;
begin
  Clear;
  Frames.Free;
end;

procedure TGifDocument.Clear;
var
  i: integer;

begin
  LoopCount := GIF_LOOP_INFINITY;

  for i := 0 to Frames.Count - 1 do TBitmap32(Frames[i]).Free;
  Frames.Clear;
  SetLength(Delays, 0);
end;

function gifReadSubBlocks(const st: TStream): string;
var
  s: string;

begin
  Result := '';
  while True do
  begin
    s := ReadPascalString(st);
    if s = '' then Exit;
    Result += s;
  end;
end;

type
  TlzwEntry = class
  public
    Parent: integer; // index to parent lzw entry, or -1
    Length: integer; // length of sequence
    FirstData: byte; // first data byte
    LastData: byte; // last data byte of the lzw sequence

    constructor Create(const AParent, ALength: integer;
      const AFirstData, ALastData: byte);
  end;

constructor TlzwEntry.Create(const AParent, ALength: integer;
  const AFirstData, ALastData: byte);
begin
  Parent := AParent;
  Length := ALength;
  FirstData := AFirstData;
  LastData := ALastData;
end;

function gifDecompress(st: TStream; var Data: TArrayOfByte;
  const MaxDataSize: integer): boolean;
var
  i, InputPos, DataSize, Code: integer;
  MinCodeSize, BitCount, ResetCode, EndCode, FirstSequenceCode: integer;
  InputByte: byte;
  InputBit: integer;
  lzwMap: TGenericList<TlzwEntry>;
  Input: string;
  Entry: TlzwEntry;

  procedure lzwReset;
  var
    i: integer;

  begin
    for i := 0 to lzwMap.Count - 1 do
      lzwMap[i].Free;
    lzwMap.Clear;

    BitCount := MinCodeSize + 1;

    // initialize LZW alphabet
    for i := 0 to (1 shl MinCodeSize) - 1 do
      lzwMap.Add(TlzwEntry.Create(-1, 1, i, i));
    lzwMap.Add(nil); // ResetCode
    lzwMap.Add(nil); // EndCode
  end;

  procedure SetDataSize(const Value: integer);
  begin
    DataSize := Value;
    if DataSize > Length(Data) then SetLength(Data, DataSize + 5000);
  end;

begin
  Result := False;

  try
    // read min code size
    MinCodeSize := 0;
    st.ReadBuffer(MinCodeSize, 1);
    if MinCodeSize > 12 then Exit;

    BitCount := MinCodeSize + 1;
    ResetCode := 1 shl MinCodeSize;
    EndCode := ResetCode + 1;
    FirstSequenceCode := ResetCode + 2;

    SetLength(Data, 0);
    DataSize := 0;

    // read LZW-compressed data
    Input := gifReadSubBlocks(st);
    InputPos := 0;
    lzwMap := TGenericList<TlzwEntry>.Create;
    try
      // we can expect a reset code, but just in case...
      lzwReset;

      while (InputPos < 8*Length(Input)) and (DataSize < MaxDataSize) do
      begin
        // read bits
        Code := 0;
        for i := 0 to BitCount - 1 do
        begin
          if InputPos >= 8*Length(Input) then Break;

          InputByte := Byte(Input[1 + (InputPos shr 3)]);
          InputBit := 1 shl (InputPos and 7);
          if InputByte and InputBit <> 0 then Code += 1 shl i;

          inc(InputPos);
        end;

        // process this code
        if Code = ResetCode then lzwReset else
        if Code = EndCode then Break else
        if Code < lzwMap.Count then
        begin
          Entry := lzwMap[Code];

          // now we know the last data byte of the previously added seq
          if lzwMap.Count > FirstSequenceCode then
            lzwMap[lzwMap.Count - 1].LastData := Entry.FirstData;

          // augment the dictionary
          if lzwMap.Count < 4096 then
          begin
            lzwMap.Add(TlzwEntry.Create(Code, Entry.Length + 1, Entry.FirstData, 0));
            // modify bit count, if needed
            if lzwMap.Count > 1 shl BitCount then
              inc(BitCount);
          end;

          // output the decoded sequence
          SetDataSize(DataSize + Entry.Length);
          for i := 1 to Entry.Length do
          begin
            Data[DataSize - i] := Entry.LastData;
            Code := Entry.Parent;
            if Code >= 0 then Entry := lzwMap[Code];
          end;
        end; // if real lzw code encountered
      end; // while read input
    finally
      for i := 0 to lzwMap.Count - 1 do
        lzwMap[i].Free;
      lzwMap.Free;
    end;

    // success
    SetLength(Data, DataSize);
    Result := True;
  except
  end;
end;

function TGifDocument.Load;
const
  gctPresent_Bit = $80;
  gctSize_Bit = 7;
  localTransparentIndexPresent_Bit = 1;
  localColorTablePresent_Bit = $80;
  interlaced_Bit = $40;
  lctSize_Bit = 7;

  dmMin = 1;
  dmMax = 3;
  dmLeaveInPlace = 1;
  dmRestoreBackground = 2;
  dmRestorePrevious = 3;

var
  i, Index: integer;
  ImageData: TArrayOfByte;
  BlockMagic, BlockMagic2, PackedFields: byte;
  gifHeader, BlockData: string;
  lsd: TLogicalScreenDescriptor;
  DisposalMethod, PrevDisposalMethod, DelayTime, TransparentIndex: integer;
  ColorTableSize: integer;
  GlobalColorTable, LocalColorTable, ColorTable: packed array of TRGB;
  imd: TImageDescriptor;
  Interlaced: boolean;
  bm, bmFrame: TBitmap32;
  pc32: PColor32;

  procedure ClearGraphicControlExtension;
  begin
    PrevDisposalMethod := DisposalMethod;
    DisposalMethod := dmRestoreBackground;
    DelayTime := 0;
    TransparentIndex := -1;
  end;

begin
  Result := False;
  Clear;

  try
    // read GIF header
    SetLength(gifHeader, 6);
    st.ReadBuffer(gifHeader[1], Length(gifHeader));
    if Copy(gifHeader, 1, 3) <> 'GIF' then Exit;

    // read logical screen descriptor
    st.ReadBuffer(lsd, SizeOf(lsd));

    // read global color table, if present
    if lsd.PackedFields and gctPresent_Bit <> 0 then
    begin
      ColorTableSize := 1 shl ((lsd.PackedFields and gctSize_Bit) + 1);
      SetLength(GlobalColorTable, ColorTableSize);
      st.ReadBuffer(GlobalColorTable[0], 3*ColorTableSize);
    end;

    // even though the GIF may be damaged, we will return true from now on
    Result := True;

    // read blocks
    ClearGraphicControlExtension;
    PrevDisposalMethod := dmRestoreBackground;
    while st.Position < st.Size do
    begin
      BlockMagic := st.ReadByte;
      case BlockMagic of
        $21: begin // extension block
          BlockMagic2 := st.ReadByte;
          BlockData := gifReadSubBlocks(st);

          case BlockMagic2 of
            $01: begin // text
              // text extensions are not supported

              // end of GCE scope
              ClearGraphicControlExtension;
            end; // if text

            $f9: begin // GCE
              while Length(BlockData) < 4 do BlockData += #0;

              // parse packed fields
              PackedFields := Byte(BlockData[1]);
              DisposalMethod := (PackedFields shr 2) and 7;
              // parse delay time
              DelayTime := PWord(@BlockData[2])^;
              // parse local transparent index
              if PackedFields and localTransparentIndexPresent_Bit <> 0 then
                TransparentIndex := Byte(BlockData[4]) else
                TransparentIndex := -1;
            end; // if GCE

            $fe: begin // comment
              // no action, dispose the comment
            end; // if comment

            $ff: begin // app
              // loop count?
              if (Length(BlockData) >= 14) and
                (Copy(BlockData, 1, 11) = 'NETSCAPE2.0') and
                (BlockData[12] = #1) then
              LoopCount := PWord(@BlockData[13])^;
            end; // if app
          end; // case extension block label
        end; // if extension block

        $2c: begin // image
          st.Seek(-1, soFromCurrent);

          // read image descriptor
          st.ReadBuffer(imd, SizeOf(imd));
          if imd.PackedFields and localColorTablePresent_Bit <> 0 then
            ColorTableSize := 1 shl ((imd.PackedFields and lctSize_Bit) + 1) else
            ColorTableSize := 0;
          Interlaced := (imd.PackedFields and interlaced_Bit) <> 0;
          if Interlaced then Exit(False); // interlaced GIFs are not supported

          // read local color table
          SetLength(LocalColorTable, ColorTableSize);
          st.ReadBuffer(LocalColorTable[0], 3*ColorTableSize);
          if ColorTableSize = 0 then
          begin
            ColorTable := GlobalColorTable;
          end else
          begin
            ColorTable := LocalColorTable;
          end;

          // read and decode image
          if not gifDecompress(st, ImageData, imd.Width*imd.Height) then Break;
          bm := TBitmap32.Create;
          try
            bm.Resize(imd.Width, imd.Height);
            bm.FillTransparent;
            pc32 := bm.Data;
            for i := 0 to Min(bm.Width*bm.Height, Length(ImageData)) - 1 do
            begin
              Index := ImageData[i];
              if (Index >= 0) and (Index < Length(ColorTable)) and
                (Index <> TransparentIndex) then
              begin
                pc32^ := cl32Opaque;
                Move3(@ColorTable[Index], pc32);
              end;

              inc(pc32);
            end;

            // perform disposal method
            bmFrame := TBitmap32.Create;
            try
              if (PrevDisposalMethod = dmLeaveInPlace) and (Frames.Count >= 1) then
                bmFrame.Assign(Frames[Frames.Count - 1]) else
              if (PrevDisposalMethod = dmRestorePrevious) and (Frames.Count >= 2) then
                bmFrame.Assign(Frames[Frames.Count - 2]) else
                bmFrame.Resize(lsd.Width, lsd.Height);
              bmFrame.Draw(imd.Left, imd.Top, bm);
            except
              bmFrame.Free;
            end;

            Frames.Add(bmFrame);
            SetLength(Delays, Length(Delays)+1);
            Delays[Length(Delays) - 1] := DelayTime;
          finally
            bm.Free;
          end;

          // end of GCE scope
          ClearGraphicControlExtension;
        end;

        $3b: Break; // trailer
      end; // case block magic
    end; // while read from stream
  except
  end;

  // do not accept GIF files without any frames
  if Frames.Count = 0 then Result := False;
end;

procedure gifCompress(st: TStream; Data: TArrayOfByte;
  const LogColorTableSize: integer);
const
  NullHash = Int64(1234567);
  HashPrime = Int64(1000003);
  CodeCount = 4096;

var
  i: integer;
  MinCodeSize, b: byte;
  lzwMap: TMap<Int64, integer>;
  Output: TArrayOfByte;
  Position, OutputPos, OutputSize: integer;
  ResetCode, EndCode, FirstFreeCode: integer;
  BitCount: integer;
  Hash: Int64;
  Code: integer;
  AddToDictionary: boolean;

  procedure Print(Code: integer);
  var
    i: integer;

  begin
    for i := 0 to BitCount - 1 do
    begin
      Output[OutputPos shr 3] += (Code and 1) shl (OutputPos and 7);

      inc(OutputPos);
      Code := Code div 2;
    end;
  end;

  procedure lzwReset;
  var
    i: integer;

  begin
    Print(ResetCode);

    lzwMap.Clear;
    FirstFreeCode := ResetCode+2;
    BitCount := MinCodeSize + 1;

    // initialize LZW alphabet
    for i := 0 to (1 shl LogColorTableSize) - 1 do
      lzwMap[(NullHash*HashPrime) xor i] := i;
  end;

begin
  // write LZW minimum code size
  MinCodeSize := Max(2, LogColorTableSize);
  st.WriteBuffer(MinCodeSize, 1);
  BitCount := MinCodeSize + 1;
  ResetCode := 1 shl MinCodeSize;
  EndCode := ResetCode+1;

  // LZW-compress
  SetLength(Output, Length(Data) + 20);
  FillChar(Output[0], Length(Output), 0);
  OutputPos := 0;
  lzwMap := TMap<Int64, integer>.Create;
  try
    // do a reset at the beginning
    lzwReset;

    // process the image
    Position := 0;
    while Position < Length(Data) do
    begin
      Hash := NullHash;
      Code := 0;
      AddToDictionary := False;
      while Position < Length(Data) do
      begin
        Hash := (Hash * HashPrime) xor Data[Position];
        if not lzwMap.ContainsKey(Hash) then
        begin
          AddToDictionary := True;
          Break;
        end;
        Code := lzwMap[Hash];
        inc(Position);
      end;
      Print(Code);

      if AddToDictionary then
      begin
        lzwMap[Hash] := FirstFreeCode;
        inc(FirstFreeCode);

        if FirstFreeCode >= CodeCount then lzwReset else
        // modify bit count, if needed
        if FirstFreeCode > 1 shl BitCount then
          inc(BitCount);
      end;
    end; // process all bytes

    // end of stream
    Print(EndCode);
  finally
    lzwMap.Free;
  end;
  OutputSize := (OutputPos + 7) shr 3;

  // output image as data sub-blocks
  b := 255;
  for i := 0 to OutputSize div 255 - 1 do
  begin
    st.WriteBuffer(b, 1);
    st.WriteBuffer(Output[255*i], 255);
  end;
  b := OutputSize mod 255;
  if b <> 0 then
  begin
    st.WriteBuffer(b, 1);
    st.WriteBuffer(Output[OutputSize - b], b);
  end;
  // output block terminator
  b := 0;
  st.WriteBuffer(b, 1);
end;

procedure TGifDocument.Save;
const
  gifHeader = 'GIF89a';

var
  b: byte;
  i, j, w, h, Index: integer;
  c: TColor;
  pc32: PColor32;
  bm: TBitmap32;
  lsd: TLogicalScreenDescriptor;
  ae: TApplicationExtension;
  lcd: TLoopCountData;
  gce: TGraphicControlExtension;
  Palette: TiePalette;
  IsTransparent: boolean;
  TransparentIndex, ColorCount, ColorTableSize, LogColorTableSize: integer;
  imd: TImageDescriptor;
  LocalColorTable: packed array of TRGB;
  IndexedImage: TArrayOfByte;

begin
  if Frames.Count <> 0 then
  begin
    w := TBitmap32(Frames[0]).Width;
    h := TBitmap32(Frames[0]).Height;
  end else
  begin
    w := 0;
    h := 0;
  end;

  // write header
  st.WriteBuffer(gifHeader[1], Length(gifHeader));

  // write logical screen descriptor
  lsd.Width := w;
  lsd.Height := h;
  lsd.PackedFields := (7 shl 4); // no GCT
  lsd.BackgroundIndex := 0;
  lsd.AspectRatio := 0;
  st.WriteBuffer(lsd, SizeOf(lsd));

  // write frames
  Palette := TiePalette.Create;
  try
    for i := 0 to Frames.Count - 1 do
    begin
      bm := Frames[i];
      gifSet256Colors(bm, Palette, IsTransparent);
      TransparentIndex := IfThen(IsTransparent, Palette.Count, -1);

      ColorCount := Palette.Count + IfThen(IsTransparent, 1, 0);
      ColorTableSize := 2;
      LogColorTableSize := 1;
      while ColorTableSize < ColorCount do
      begin
        ColorTableSize *= 2;
        inc(LogColorTableSize);
      end;

      // write GCE
      gce.Init(Delays[i], TransparentIndex);
      st.WriteBuffer(gce, SizeOf(gce));

      // write Image Descriptor
      imd.Magic := $2c;
      imd.Left := 0;
      imd.Top := 0;
      imd.Width := bm.Width;
      imd.Height := bm.Height;
      imd.PackedFields := $80 or (LogColorTableSize - 1);
      st.WriteBuffer(imd, SizeOf(imd));

      // write local color table
      SetLength(LocalColorTable, ColorTableSize);
      FillChar(LocalColorTable[0], 3*ColorTableSize, 0);
      for j := 0 to Palette.Count - 1 do
      begin
        c := Palette[j];
        Move3(@c, @LocalColorTable[j]);
      end;
      st.WriteBuffer(LocalColorTable[0], 3*ColorTableSize);

      // convert image to indexed
      SetLength(IndexedImage, bm.Width*bm.Height);
      pc32 := bm.Data;
      for j := 0 to Length(IndexedImage)-1 do
      begin
        if pc32^ = cl32Transparent then
          Index := TransparentIndex else
          Index := Palette.IndexOf(pc32^ and not cl32Opaque);
        IndexedImage[j] := Index;

        inc(pc32);
      end;

      // write compressed LZW image
      gifCompress(st, IndexedImage, LogColorTableSize);
    end;
  finally
    Palette.Free;
  end;

  // write loop count
  if not ((Frames.Count = 1) and (LoopCount = GIF_LOOP_INFINITY)) then
  begin
    ae.Init('NETSCAPE2.0');
    st.WriteBuffer(ae, SizeOf(ae));
    lcd.Init(LoopCount);
    st.WriteBuffer(lcd, SizeOf(lcd));
  end;

  // write trailer
  b := $3b;
  st.WriteBuffer(b, 1);
end;

function gifDetect;
const
  FCC_GIF = $00464947;

begin
  Result := (Size >= 4) and ((PCardinal(Data)^ and $ffffff) = FCC_GIF);
end;

function gifLoadFromStream;
var
  i: integer;
  bm: TBitmap32;
  Page: TDocPage;
  gd: TGifDocument;

begin
  Result := False;

  try
    gd := TGifDocument.Create;
    try
      if not gd.Load(s) then Exit;

      Doc.Clear;
      Doc.Metadata.LoopCount := gd.LoopCount;

      for i := 0 to gd.Frames.Count - 1 do
      begin
        bm := gd.Frames[i];
        Page := Doc.NewPage;

        Page.FrameRate := 10 * gd.Delays[i];
        bm.Resize(Min(MaxWidth, bm.Width), Min(MaxHeight, bm.Height));
        Page.Layers.Assign(bm);
      end;
    finally
      gd.Free;
    end;

    Result := True;
  except
  end;
end;

procedure gifSaveToStream;
var
  i: integer;
  bm: TBitmap32;
  gd: TGifDocument;

begin
  gd := TGifDocument.Create;
  try
    gd.LoopCount := Doc.Metadata.LoopCount;
    SetLength(gd.Delays, Doc.PageCount);

    for i := 0 to Doc.PageCount - 1 do
    begin
      bm := TBitmap32.Create;
      gd.Frames.Add(bm);
      bm.Assign(Doc.Pages[i].Layers);
      gd.Delays[i] := (Doc.Pages[i].FrameRate + 5) div 10;
    end;

    gd.Save(s);
  finally
    gd.Free;
  end;
end;

end.

