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
unit BMP;

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Graphics, Math, BitmapEx,
  PixelFormats;

const
  SIG_BMP = $4d42;

// Reads BITMAPINFOHEADER and DIB bits
// ICOFormat should be true iff a 1-bit transparency mask is also present
// Offset specifies image offset relative to curr. stream pos., can be -1 (=default)
function dibRead(bm: TBitmap32; s: TStream; Offset: integer;
  ICOFormat: boolean): boolean;
procedure dibWrite(bm: TBitmap32; s: TStream;
  ICOFormat: boolean; PixelFormat: TPixelFormat32; Palette: TiePalette;
  BitsOffset: PInteger);

function bmpDetect(Data: Pointer; Size: integer): boolean;

function bmpLoadFromStream(bm: TBitmap32; s: TStream): boolean;
procedure bmpSaveToStream(bm: TBitmap32; s: TStream);

function bmpLoadFromFile(bm: TBitmap32; const fn: string): boolean;
procedure bmpSaveToFile(bm: TBitmap32; const fn: string);

implementation

uses
  bmExUtils, StreamEx, dlgDebug;

function dibRead;
var
  // buffered I/O -> faster
  sr: TBufferedReader;
  HeaderSize, BytesRead, x, y, i, Padding, nBits, PalCount, PalEntrySize, PixPerByte, PixelMask: integer;
  bmih: BITMAPINFOHEADER;
  bmch: BITMAPCOREHEADER;
  Palette: array[0..255] of TColor;
  c: TColor;
  Color32: TColor32;
  pc32: PColor32;
  Buffer: byte;
  dummy: Cardinal;
  HasNonZeroAlpha, HasInvalidPixel: boolean;

begin
  Result := False;
  try
    // initialize reader
    // (we will use no more special stream instructions)
    sr := TBufferedReader.Create(s);
    try
      // peek the header size
      sr.Read(@HeaderSize, 4);
      sr.Position := sr.Position - 4;
      if HeaderSize > 1000 then
        Exit;

      BytesRead := 0;

      if HeaderSize = SizeOf(BITMAPCOREHEADER) then
      begin
        sr.Read(@bmch, SizeOf(bmch));
        if (bmch.bcPlanes <> 1) or (bmch.bcBitCount > 64) then
          Exit;
        bm.Resize(bmch.bcWidth, IfThen(ICOFormat, bmch.bcHeight div 2, bmch.bcHeight));
        nBits := bmch.bcBitCount;
        PalCount := 1 shl nBits;
        PalEntrySize := 3;
      end else
      begin
        // read BITMAPINFOHEADER
        if HeaderSize < SizeOf(bmih) then
          Exit;
        sr.Read(@bmih, SizeOf(bmih));
        sr.Position := sr.Position + HeaderSize-SizeOf(bmih);

        if (bmih.biWidth < 0) or (bmih.biPlanes <> 1) or (bmih.biBitCount > 64) then
          Exit;
        bm.Resize(bmih.biWidth,
          IfThen(ICOFormat, bmih.biHeight div 2, bmih.biHeight));
        nBits := bmih.biBitCount;
        PalCount := bmih.biClrUsed;
        PalEntrySize := 4;
      end;
      inc(BytesRead, HeaderSize);
      {$IFDEF GFIEDEBUG}
      Log(Format('Loading DIB: %dx%d @ %d-bit, HeaderSize=%d, PalCount=%d, PalEntrySize=%d',
        [bm.Width, bm.Height, nBits, HeaderSize, PalCount, PalEntrySize]));
      {$ENDIF}

      // read bitmap
      if nBits = 32 then
      begin
        if Offset >= 0 then sr.Read(nil, Offset - BytesRead); // skip bytes

        // No padding is required
        for y := bm.Height - 1 downto 0 do
        for x := 0 to bm.Width - 1 do
        begin
          sr.Read(@Color32, 4);
          bm.PixelAddr(x, y)^ := FlipColor32(Color32);
        end;

        // No AND mask
      end else
      begin
        // --------- read XOR mask
        if nBits = 24 then
          // RGB
        begin
          if Offset >= 0 then sr.Read(nil, Offset - BytesRead); // skip bytes

          Padding := PadTo4(bm.Width * 3);
          for y := bm.Height - 1 downto 0 do
          begin
            for x := 0 to bm.Width - 1 do
            begin
              sr.Read(@Color32, 3);
              bm.PixelAddr(x, y)^ := FlipColor32(Color32) or cl32Opaque;
            end;

            sr.Read(@dummy, Padding);
          end;
        end else
          // indexed
        begin
          // read palette - RGBQuads
          if PalCount = 0 then PalCount := 1 shl nBits;

          for i := 0 to PalCount - 1 do
          begin
            sr.Read(@c, PalEntrySize);
            c := c and clWhite;
            Palette[i] := FlipColor(c);
          end;
          inc(BytesRead, PalEntrySize*PalCount);

          if Offset >= 0 then sr.Read(nil, Offset - BytesRead); // skip bytes

          // read DIB bits
          PixPerByte := 8 div nBits;
          PixelMask := (1 shl nBits) - 1;
          Padding := PadTo4((bm.Width*nBits + 7) div 8);

          for y := bm.Height - 1 downto 0 do
          begin
            for i := 0 to (bm.Width*nBits + 7) div 8 - 1 do
            begin
              sr.Read(@Buffer, 1);

              for x := (i+1)*PixPerByte - 1 downto i*PixPerByte do
              begin
                if x < bm.Width then bm.PixelAddr(x, y)^ :=
                  Cardinal(Palette[Buffer and PixelMask]) or cl32Opaque;
                Buffer := Buffer shr nBits;
              end;
            end;

            sr.Read(@dummy, Padding);
          end; // for y
        end; // indexed

        // --------- read AND mask
        if ICOFormat then
        begin
          // ALIGN = 4
          Padding := PadTo4((bm.Width + 7) div 8);

          // read as a monochrome (1-bit) bitmap
          for y := bm.Height - 1 downto 0 do
          begin
            for i := 0 to (bm.Width + 7) div 8 - 1 do
            begin
              sr.Read(@Buffer, 1);

              for x := i*8 + 7 downto i*8 do
              begin
                if x < bm.Width then
                begin
                  if Buffer and 1 <> 0 then
                  begin
                    PByteArray(bm.PixelAddr(x, y))[3] := 0;
                    color32 := bm.PixelAddr(x, y)^;
                  end;
                end;
                Buffer := Buffer shr 1;
              end;
            end;

            sr.Read(@dummy, Padding);
          end; // for y
        end; // if ICOFormat
      end; // not 32-bit

      // Check AND mask
      // Sometimes the AND mask is written as if the whole image were transparent
      // but in fact it isn't, and instead the first color in the palette is transparent.
      // In addition, sometimes 24-bit bitmaps are written as 32-bit bitmaps with alpha=0.
      HasInvalidPixel := False;
      HasNonZeroAlpha := False;
      pc32 := bm.Data;
      for i := 1 to bm.Width*bm.Height do
      begin
        color32 := pc32^;
        if color32 and cl32Opaque = 0 then
        begin
          // inverted pixels are invalid if no AND mask is present
          if (color32 <> cl32Transparent) and
              ((color32 <> cl32Inverted) or not ICOFormat) then
            HasInvalidPixel := True;
        end else
          HasNonZeroAlpha:=true;
        if HasInvalidPixel and HasNonZeroAlpha then
          break;
        inc(pc32);
      end;
      if HasInvalidPixel then
      begin
        pc32 := bm.Data;
        if HasNonZeroAlpha then
        begin
          // replace all 0-alpha pixels with cl32Transparent
          for i := 1 to bm.Width*bm.Height do
          begin
            if pc32^ and cl32Opaque = 0 then
              pc32^ := cl32Transparent;
            inc(pc32);
          end;
        end else
        begin
          // all alpha zero, but has invalid pixel -> probably alpha channel is not needed
          for i := 1 to bm.Width*bm.Height do
          begin
            pc32^ := pc32^ or cl32Opaque;
            inc(pc32);
          end;
        end;
      end;
    finally
      sr.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

procedure dibWrite;
var
  // buffered I/O -> faster
  sw: TBufferedWriter;
  i, x, y, nBits, PixPerByte, Padding, Index: integer;
  p: PColor32;
  Color32: TColor32;
  c: TColor;
  Buffer: byte;
  // for writing the padding bytes
  zero: Cardinal;
  bmih: BITMAPINFOHEADER;

begin
  zero := 0;
  nBits := pf32ToBitCount[PixelFormat];

  sw := TBufferedWriter.Create(s);
  try
    // write the bitmap info header
    FillChar(bmih, SizeOf(bmih), 0);
    with bmih do
    begin
      biSize := SizeOf(bmih);
      biWidth := bm.Width;
      biHeight := bm.Height;
      if ICOFormat then biHeight := biHeight * 2;
      biPlanes := 1;
      biBitCount := nBits;
      biCompression := BI_RGB;
    end;
    sw.Write(@bmih, SizeOf(bmih));

    // update bits offset
    if BitsOffset <> nil then
    begin
      // header
      BitsOffset^ := SizeOf(bmih);
      // palette
      if PixelFormat < pf32_24bit then inc(BitsOffset^, 1 shl (2 + nBits));
    end;

    // write bitmap
    // --------- write XOR mask
    case PixelFormat of
      pf32_32bit: begin
        // No padding is required, bytes/row is always divisible by 4
        for y := bm.Height - 1 downto 0 do
        for x := 0 to bm.Width - 1 do
        begin
          Color32 := FlipColor32(bm.PixelAddr(x, y)^);
          // prevent inverted color from appearing
          if Color32 and cl32Opaque = 0 then Color32 := cl32Transparent;
          sw.Write(@Color32, 4);
        end;
      end; // 32-bit

      pf32_24bit: begin
        Padding := PadTo4(bm.Width * 3);
        for y := bm.Height - 1 downto 0 do
        begin
          for x := 0 to bm.Width - 1 do
          begin
            p := bm.PixelAddr(x, y);

            // there may be 0-alpha non-transparent/inverted pixels
            if (PByteArray(p)[3] = 0) and (p^ <> cl32Inverted) then
              c := clBlack else c := FlipColor(p^ and $ffffff);

            sw.Write(@c, 3);
          end; // for x

          sw.Write(@zero, Padding);
        end; // for y
      end;

      else begin // indexed
        // write palette - RGBQuads
        // We always have to write 1 shl nBits entries, because many
        // programs do not take biClrUsed into account
        // DELPHI BUG: 1 shl nBits cannot be written, internal error...
        for i := 0 to Round(Exp(Ln(2) * nBits)) - 1 do
        begin
          if i < Palette.Count then
            c := FlipColor(Palette[i]) else
            c := 0;
          sw.Write(@c, 4);
        end;

        // write DIB
        PixPerByte := 8 div nBits;
        Padding := PadTo4((bm.Width*nBits + 7) div 8);

        for y := bm.Height - 1 downto 0 do
        begin
          for i := 0 to (bm.Width*nBits + 7) div 8 - 1 do
          begin
            Buffer := 0;

            for x := i*PixPerByte to (i+1)*PixPerByte - 1 do
            begin
              Buffer := Buffer shl nBits;
              if x >= bm.Width then Continue;

              p := bm.PixelAddr(x, y);

              // determine color
              if (PByteArray(p)[3] = 0) and (p^ <> cl32Inverted) then
                c := clBlack else c := p^ and $ffffff;

              // determine index
              Index := Palette.IndexOf(c);
              if Index = -1 then Index := 0;

              // write data
              Buffer := Buffer or Index;
            end; // for x

            sw.Write(@Buffer, 1);
          end; // for i

          sw.Write(@zero, Padding);
        end; // for y
      end; // indexed
    end; // case

    // --------- write AND mask
    if ICOFormat then
    begin
      // pad to 4 bytes
      Padding := PadTo4((bm.Width + 7) div 8);
      // write as a monochrome (1-bit) bitmap
      for y := bm.Height - 1 downto 0 do
      begin
        for i := 0 to (bm.Width + 7) div 8 - 1 do
        begin
          Buffer := 0;

          for x := i*8 to i*8 + 7 do
          begin
            Buffer := Buffer shl 1;
            if (x < bm.Width) and
              (PByteArray(bm.PixelAddr(x, y))[3] = 0) then
              // transparent or inverted -> 1
              Buffer := Buffer or 1;
          end;

          sw.Write(@Buffer, 1);
        end; // for i

        sw.Write(@zero, Padding);
      end; // for y
    end; // if ICOFormat
  finally
    sw.Free;
  end;
end;

function bmpDetect;
begin
  Result := (Size >= 2) and (PWord(Data)^ = SIG_BMP);
end;

function bmpLoadFromStream;
var
  bmfh: BITMAPFILEHEADER;

begin
  Result := False;
  try
    // read file header
    s.ReadBuffer(bmfh, SizeOf(bmfh));

    // check
    if bmfh.bfType <> SIG_BMP then Exit;

    // read dib bits
    Result := dibRead(bm, s, bmfh.bfOffBits - SizeOf(bmfh), False);
  except
  end;
end;

function GetBitmapSize(bm: TBitmap32; pf: TPixelFormat32): integer;
begin
  // bitmap data
  Result := (bm.Width*bm.Height*pf32ToBitCount[pf] + 7) div 8;
  // palette
  if pf < pf32_24bit then inc(Result, 4 shl pf32ToBitCount[pf]);
end;

procedure bmpSaveToStream;
var
  oldPos, newPos, BitsOffset: integer;
  bmfh: BITMAPFILEHEADER;
  pf: TPixelFormat32;
  Palette: TiePalette;

begin
  oldPos := s.Position;

  // leave place for file header
  s.Seek(SizeOf(bmfh), soFromCurrent);

  Palette := nil;
  try
    // optimize pixel format
    if bm.HasTransparency then pf := pf32_32bit else
    begin
      Palette := TiePalette.Create;
      pf := GetPixelFormat32(bm, nil, nil, nil, Palette);

      // sometimes bitmaps get smaller if they use no palette!
      if GetBitmapSize(bm, pf) > GetBitmapSize(bm, pf32_24bit) then
        pf := pf32_24bit;
    end;

    // write dib bits
    dibWrite(bm, s, False, pf, Palette, @BitsOffset);
  finally
    if Palette <> nil then Palette.Free;
  end;

  // write file header
  newPos := s.Position;
  s.Position := oldPos;
    FillChar(bmfh, SizeOf(bmfh), 0);
      bmfh.bfType := SIG_BMP;
      bmfh.bfSize := newPos - oldPos;
      bmfh.bfOffBits := SizeOf(bmfh) + BitsOffset;
    s.WriteBuffer(bmfh, SizeOf(bmfh));
  s.Position := newPos;
end;

function bmpLoadFromFile;
var
  s: TStream;

begin
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      Result := bmpLoadFromStream(bm, s);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure bmpSaveToFile;
var
  s: TStream;

begin
  s := TFileStream.Create(fn, fmCreate);
  try
    bmpSaveToStream(bm, s);
  finally
    s.Free;
  end;
end;

end.
