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
unit PCX;

interface

uses
  LclIntf, LclType, SysUtils, Classes, Graphics, StreamEx, BitmapEx, bmExUtils,
  PixelFormats;

function pcxDetect(Data: Pointer; Size: integer): boolean;
function pcxLoadFromStream(bm: TBitmap32; out hdpi, vdpi: word; s: TStream): boolean;
function pcxSaveToStream(bm: TBitmap32; hdpi, vdpi: word; s: TStream): boolean;

implementation

const
  PCX_MANUFACTURER = 10;
  PCX_VERSION_3x = 5;
  PCX_ENCODING_RLE = 1;
  PCX_PAL_COLOR_BW = 1;
  PCX_PALETTE256_SIGNATURE = 12;

type
  PPCXHeader = ^TPCXHeader;
  TPCXHeader = packed record
    Manufacturer: byte;               // PCX_MANUFACTURER
    Version: byte;
    Encoding: byte;                   // PCX_ENCODING_RLE
    BitsPerPixel: byte;               // 1, 2, 4, 8
    xMin, yMin, xMax, yMax: word;
    hDPI, vDPI: word;
    Palette16: array[0..15, 0..2] of byte;
    Reserved: byte;                   // 0
    nPlanes: byte;                    // number of color planes
    BytesLinePlane: word;             // bytes/scanline plane, must be even
    PaletteInfo: word;                // 1: Color/BW, 2: Grayscale
    hScreenSize, vScreenSize: word;
    Filler: array[0..53] of byte;     // zeros
  end;

  // Possible interpretations of PCX scanline bytes
  TPCXPixelFormat = (pcx1bit, pcx4bit, pcx8bit, pcx24bit);

function pcxDetect;
var
  h: PPCXHeader;
begin
  if Size <= SizeOf(TPCXHeader) then
    Exit(false);
  h := Data;
  if h.Manufacturer <> PCX_MANUFACTURER then
    exit(false);
  if h.Encoding <> PCX_ENCODING_RLE then
    exit(false);
  if h.Reserved <> 0 then
    exit(false);
  if (h.nPlanes <> 1) and (h.nPlanes <> 3) and (h.nPlanes <> 4) then
    exit(false);
  if (h.PaletteInfo <> 1) and (h.PaletteInfo <> 2) then
    exit(false);
  exit(true);
end;

// PCX RLE encoder class

const
  PCX_MAX_RUNLENGTH = 63;

type
  TPCXCompressionStream = class
  private
    bw: TBufferedWriter;
    Prev, Count: integer;

    procedure Flush;
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;

    procedure Write(p: PByte; Size: integer);
  end;

procedure TPCXCompressionStream.Flush;
begin
  if Count <> 0 then
  begin
    if (Prev and $c0 = $c0) or (Count > 1) then
    begin
      Count := Count or $c0;
      bw.Write(@Count, 1);
    end;
    bw.Write(@Prev, 1);

    // clear
    Count := 0;
  end;

  Prev := -1;
end;

constructor TPCXCompressionStream.Create;
begin
  bw := TBufferedWriter.Create(AStream);

  Prev := -1;
  Count := 0;
end;

destructor TPCXCompressionStream.Destroy;
begin
  Flush;
  bw.Free;

  inherited;
end;

procedure TPCXCompressionStream.Write;
var
  i: integer;

begin
  for i := 0 to Size - 1 do
  begin
    if (p^ <> Prev) or (Count >= PCX_MAX_RUNLENGTH) then
    begin
      Flush;
      Prev := p^;
    end;

    // increase counter
    inc(Count);

    // iterate thru array
    inc(p);
  end;
end;

// *******************************************************

procedure pcxReadLine(r: TBufferedReader; p: PByteArray; Size: integer);
var
  i: integer;
  Count, Value: byte;

begin
  i := 0;
  while i < Size do
  begin
    r.Read(@Value, 1);
    if Value and $c0 = $c0 then
    begin
      // count specifier
      Count := Value and not $c0;
      r.Read(@Value, 1);

      FillChar(p[i], Count, Value);
      inc(i, Count);
    end else
    begin
      p[i] := Value;
      inc(i);
    end; // single value
  end; // while i
end;

function pcxLoadFromStream;
var
  i, k, x, y, LineSize, Mask, Index: integer;
  b: byte;
  h: TPCXHeader;
  pf: TPCXPixelFormat;
  Reader: TBufferedReader;
  pcxLine: array of byte;
  p: PColor32;
  Palette256: array[0..255] of TColor32;

begin
  Result := False;
  hdpi := 0;
  vdpi := 0;
  try
    // read and verify header
    s.ReadBuffer(h, SizeOf(h));
    if (h.Manufacturer <> PCX_MANUFACTURER) or
      (h.Encoding <> PCX_ENCODING_RLE) then Exit;

    hdpi := h.hDPI;
    vdpi := h.vDPI;

    // get image size
    bm.Resize(h.xMax - h.xMin + 1, h.yMax - h.yMin + 1);

    // get pixel format
    case h.BitsPerPixel of
      1: case h.nPlanes of
           1: pf := pcx1bit;
           4: pf := pcx4bit;
           else Exit;
         end;

      8: case h.nPlanes of
           1: pf := pcx8bit;
           3: pf := pcx24bit;
           else Exit;
         end;

      else Exit;
    end;

    // get size of scanline data
    LineSize := h.BytesLinePlane * h.nPlanes;
    SetLength(pcxLine, LineSize);

    // read and decode image data
    Reader := TBufferedReader.Create(s);
    try
      // decode image
      for y := 0 to bm.Height - 1 do
      begin
        // read a scanline
        pcxReadLine(Reader, @pcxLine[0], LineSize);

        // interpret
        p := bm.PixelAddr(0, y);
        case pf of
          pcx1bit: for x := 0 to bm.Width - 1 do
          begin
            if pcxLine[x shr 3] and ($80 shr (x and 7)) <> 0 then
              p^ := cl32White else
              p^ := cl32Black;

            inc(p);
          end; // for i, 1-bit

          pcx4bit: for x := 0 to bm.Width - 1 do
          begin
            Mask := $80 shr (x and 7);

            // get color index
            Index := 0;
            for k := 0 to 3 do
              if pcxLine[h.BytesLinePlane*k + x shr 3] and Mask <> 0 then
            Index := Index or (1 shl k);

            // lookup color from index
            Move3(@h.Palette16[Index], p);
            PByteArray(p)[3] := $ff;

            inc(p);
          end; // for i, 4-bit

          pcx8bit: for x := 0 to bm.Width - 1 do
          begin
            // store only the color index
            // we will do the lookup after loading the pixels
            p^ := pcxLine[x];

            inc(p);
          end; // for i, 8-bit

          pcx24bit: for x := 0 to bm.Width - 1 do
          begin
            p^ := pcxLine[x] or (pcxLine[h.BytesLinePlane + x] shl 8) or
              (pcxLine[2*h.BytesLinePlane + x] shl 16) or cl32Opaque;

            inc(p);
          end; // for i, 24-bit
        end; // case pf
      end; // for y

      // read and apply palette
      if pf = pcx8bit then
      begin
        FillChar(Palette256, SizeOf(Palette256), $ff);

        // read indicator byte
        try
          Reader.Read(@b, 1);
        except
          b := 0;
        end;
        if b = PCX_PALETTE256_SIGNATURE then // there is a palette
          for i := 0 to 255 do Reader.Read(@Palette256[i], 3) else
          for i := 0 to 255 do Palette256[i] := TColor32(i * $10101) or cl32Opaque;

        // apply the palette
        p := bm.Data;
        for i := 1 to bm.Width * bm.Height do
        begin
          p^ := Palette256[PByte(p)^];

          inc(p);
        end;
      end; // 8-bit palette
    finally
      Reader.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

function pcxSaveToStream;
var
  bmNeedDestroy: boolean;
  bmToWrite: TBitmap32;
  i, x, y: integer;
  b, Mask: byte;
  pf: TPixelFormat32;
  Palette: TiePalette;
  h: TPCXHeader;
  sl: array of byte;
  c: TColor;
  p: PColor32;
  pRed, pGreen, pBlue, pInten: PByte;
  sPCX: TPCXCompressionStream;
  Palette256: array[0..256*3 - 1] of byte;

begin
  Result := False;
  try
    // PCX does not support any kind of transparency
    bmNeedDestroy := bm.HasTransparency;
    if bmNeedDestroy then
    begin
      bmToWrite := TBitmap32.Create;
      bmToWrite.Assign(bm);
      bmToWrite.FillBehind(cl32White);
    end else
      bmToWrite := bm;

    try
      // determine pixel format and palette
      Palette := TiePalette.Create;
      try
        pf := GetPixelFormat32(bmToWrite, palBW, nil, nil, Palette);

        // create and write PCX header
        FillChar(h, SizeOf(h), 0);
          h.Manufacturer := PCX_MANUFACTURER;
          h.Version := PCX_VERSION_3x;
          h.Encoding := PCX_ENCODING_RLE;

          case pf of
            pf32_1bit, pf32_4bit: h.BitsPerPixel := 1;
            pf32_8bit, pf32_24bit: h.BitsPerPixel := 8;
          end;

          h.xMax := bmToWrite.Width - 1;
          h.yMax := bmToWrite.Height - 1;
          h.hDPI := hdpi;
          h.vDPI := vdpi;

          if pf = pf32_4bit then for i := 0 to Palette.Count - 1 do
          begin
            c := Palette[i];
            Move3(@c, @h.Palette16[i, 0]);
          end;

          case pf of
            pf32_1bit, pf32_8bit: h.nPlanes := 1;
            pf32_4bit: h.nPlanes := 4;
            pf32_24bit: h.nPlanes := 3;
          end;

          h.BytesLinePlane := (bmToWrite.Width * h.BitsPerPixel + 7) div 8;
          if Odd(h.BytesLinePlane) then inc(h.BytesLinePlane);
          h.PaletteInfo := PCX_PAL_COLOR_BW;
        s.WriteBuffer(h, SizeOf(h));

        // write PCX encoded data
        sPCX := TPCXCompressionStream.Create(s);
        try
          // produce scanlines
          SetLength(sl, h.nPlanes * h.BytesLinePlane);

          for y := 0 to bmToWrite.Height - 1 do
          begin
            FillChar(sl[0], Length(sl), 0);
            p := bmToWrite.PixelAddr(0, y);

            case pf of
              pf32_1bit: for x := 0 to bmToWrite.Width - 1 do
              begin
                if p^ = cl32White then
                  sl[x shr 3] := sl[x shr 3] or ($80 shr (x and 7));
                inc(p);
              end;

              pf32_4bit: begin
                pRed := @sl[0];
                pGreen := @sl[h.BytesLinePlane];
                pBlue := @sl[2*h.BytesLinePlane];
                pInten := @sl[3*h.BytesLinePlane];

                for x := 0 to bmToWrite.Width - 1 do
                begin
                  Mask := $80 shr (x and 7);

                  b := Palette.IndexOf(p^ and $ffffff);
                  if b and 1 <> 0 then pRed^ := pRed^ or Mask;
                  if b and 2 <> 0 then pGreen^ := pGreen^ or Mask;
                  if b and 4 <> 0 then pBlue^ := pBlue^ or Mask;
                  if b and 8 <> 0 then pInten^ := pInten^ or Mask;

                  // next byte
                  if Mask = 1 then
                  begin
                    inc(pRed);
                    inc(pGreen);
                    inc(pBlue);
                    inc(pInten);
                  end;

                  inc(p);
                end; // for i
              end;

              pf32_8bit: for x := 0 to bmToWrite.Width - 1 do
              begin
                sl[x] := Palette.IndexOf(p^ and $ffffff);
                inc(p);
              end;

              pf32_24bit: begin
                pRed := @sl[0];
                pGreen := @sl[h.BytesLinePlane];
                pBlue := @sl[2*h.BytesLinePlane];

                for x := 0 to bmToWrite.Width - 1 do
                begin
                  // distribute R, G, B values
                  pRed^ := PByte(p)^; inc(pRed); inc(PByte(p));
                  pGreen^ := PByte(p)^; inc(pGreen); inc(PByte(p));
                  pBlue^ := PByte(p)^; inc(pBlue); inc(PByte(p));

                  // skip alpha
                  inc(PByte(p));
                end;
              end;
            end;

            // encode
            sPCX.Write(@sl[0], Length(sl));
            // output a "line break"
            sPCX.Flush;
          end;
        finally
          sPCX.Free;
        end;

        // write 256-color palette, if needed
        if pf = pf32_8bit then
        begin
          // write indicator
          b := PCX_PALETTE256_SIGNATURE;
          s.WriteBuffer(b, 1);

          // write the palette itself
          FillChar(Palette256, SizeOf(Palette256), 0);
          for i := 0 to Palette.Count - 1 do
          begin
            c := Palette[i];
            Move3(@c, @Palette256[i*3]);
          end;
          s.WriteBuffer(Palette256, SizeOf(Palette256));
        end;
      finally
        Palette.Free;
      end;
    finally
      if bmNeedDestroy then bmToWrite.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

end.

