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
// Determining pixel format for a 32-bit image,
// converting 32-bit bitmaps to black and white/16 colors/256 colors/..., etc.
unit PixelFormats;

interface

{$mode delphi}

uses
  LclIntf, LclType, SysUtils, Graphics, Math, BitmapEx, bmExUtils;

type
  TColorIndexPair = record
    Color: TColor;
    Index: integer;
  end;

  // Color palette
  // Size occupied: O(maximum of indices)
  TiePalette = class
  private
    FCount: integer;
    FIndexToColor: array of TColor;
    // Sorted by colors
    FColorToIndex: array of TColorIndexPair;

    // Searches in FColorToIndex
    // Returns whether found color.
    // If Found=false, Result is where c should be added.
    function Lookup(c: TColor; out Found: boolean): integer;
    function GetColor(Index: integer): TColor; inline;
  public
    constructor Create;
    constructor CreateFromArray(Data: PIntegerArray; DataCount: integer);
    procedure Assign(Src: TiePalette);
    procedure Clear;

    // Returns index where color was added (index is auto-incremented)
    function Add(c: TColor; AllowDuplicates: boolean): integer; // O(n)
    // Returns -1 if not found
    function IndexOf(c: TColor): integer; // O(log n)
    // Is the palette subset of another one?
    function IsSubsetOf(p: TiePalette): boolean;
    // Finds the closest palette color to a specific color
    function FindClosestColor(c: TColor): TColor;

    procedure ApplyToBitmap(bm: TBitmap32; Dither: boolean);

    property Count: integer read FCount;
    property Colors[Index: integer]: TColor read GetColor; default; // O(1)
  end;

var
  // Palette "constants"
  palBW, palWin16, palMac16, palMac256: TiePalette;
  
type
  TPixelFormat32 = (pf32_1bit, pf32_4bit, pf32_8bit, pf32_24bit, pf32_32bit);

const
  pf32ToBitCount: array[TPixelFormat32] of integer = (1, 4, 8, 24, 32);

// Returns pixel format used by the bitmap and the palette if needed
// If the reference palettes are not nil, then
// only allows colors contained by the reference palette
function GetPixelFormat32(bm: TBitmap32;
  RefPal1, RefPal4, RefPal8, PaletteOut: TiePalette): TPixelFormat32;

type
  TColorReduction = (crBW, crWin16, crMac16, crAdaptive256, crMac256, cr24, cr32);

// Sets pixelformat = reduces color depth and returns palette of resulting bitmap,
// if applicable
procedure ReduceColors(bm: TBitmap32; cr: TColorReduction;
  PaletteOut: TiePalette);

// Input: 24-bit bitmap, output: a bitmap which occupies the least space
// Only works under Windows
procedure OptimizeBitmapPF(bm: TBitmap);

implementation

uses
  ColQuant;

// TiePalette

function TiePalette.Lookup;
var
  Lo, Hi, Mid: integer;
  cMid: TColor;

begin
  Found := False;

  if FCount = 0 then Exit(0);

  Lo := 0;
  // color is smaller than smallest?
  if c < FColorToIndex[Lo].Color then Exit(0);
  // color is the smallest?
  if c = FColorToIndex[Lo].Color then
  begin
    Found := True;
    Exit(Lo);
  end;

  Hi := FCount-1;
  // color is larger than largest?
  if c > FColorToIndex[Hi].Color then Exit(Hi+1);
  // color is the largest?
  if c = FColorToIndex[Hi].Color then
  begin
    Found := True;
    Exit(Hi);
  end;

  // Now FColorToIndex[Lo] < c < FColorToIndex[Hi]
  // This property will be invariant
  while Hi - Lo > 1 do
  begin
    Mid := (Lo + Hi) div 2;
    cMid := FColorToIndex[Mid].Color;
    if cMid < c then Lo := Mid else
    if cMid > c then Hi := Mid else
    begin
      Found := True;
      Exit(Mid);
    end;
  end; // while Hi - Lo > 1

  // not found -> insert at Hi
  Exit(Hi);
end;

function TiePalette.GetColor;
begin
  Result := FIndexToColor[Index];
end;

constructor TiePalette.Create;
begin
  FCount := 0;
end;

constructor TiePalette.CreateFromArray(Data: PIntegerArray; DataCount: integer);
var
  i: integer;

begin
  FCount := 0;
  for i := 0 to DataCount - 1 do
    Add(Data[i], True);
end;

procedure TiePalette.Assign;
begin
  FCount := Src.FCount;
  FIndexToColor := Copy(Src.FIndexToColor);
  FColorToIndex := Copy(Src.FColorToIndex);
end;

procedure TiePalette.Clear;
begin
  FCount := 0;
  SetLength(FIndexToColor, 0);
  SetLength(FColorToIndex, 0);
end;

function TiePalette.Add;
var
  i, InsertHere: integer;
  Found: boolean;

begin
  // already exists?
  InsertHere := Lookup(c, Found);
  if Found and not AllowDuplicates then Exit(FColorToIndex[InsertHere].Index);

  // reserve space for new color
  inc(FCount);
  if FCount > Length(FIndexToColor) then
  begin
    SetLength(FIndexToColor, 1 + (FCount or $ff));
    SetLength(FColorToIndex, 1 + (FCount or $ff));
  end;

  // this is the new index of the color
  Result := FCount-1;
  // map index to color
  FIndexToColor[FCount-1] := c;
  // map color to index
  for i := FCount-1 downto InsertHere+1 do
    FColorToIndex[i] := FColorToIndex[i-1];
  with FColorToIndex[InsertHere] do
  begin
    Color := c;
    Index := Result;
  end;
end;

function TiePalette.IndexOf;
var
  i: integer;
  Found: boolean;

begin
  i := Lookup(c, Found);
  if not Found then Exit(-1);
  Exit(FColorToIndex[i].Index);
end;

function TiePalette.IsSubsetOf;
var
  i: integer;

begin
  for i := 0 to Count - 1 do
    if p.IndexOf(Colors[i]) < 0 then Exit(False);
  Result := True;
end;

function TiePalette.FindClosestColor;
var
  j, d, dNew, Index, Red, Green, Blue: integer;
  p2: PByteArray;

begin
  if Count = 0 then Exit(clBlack);

  Red := c and $ff;
  c := c shr 8;
  Green := c and $ff;
  c := c shr 8;
  Blue := c and $ff;

  d := MaxInt;
  Index := 0;
  for j := 0 to FCount - 1 do
  begin
    p2 := @FIndexToColor[j];
    dNew := Abs(Red - p2[0]) + Abs(Green - p2[1]) + Abs(Blue - p2[2]);
    if dNew < d then
    begin
      d := dNew;
      Index := j;
    end;
  end; // for j (palette)

  Result := Colors[Index];
end;

procedure DitherFloydSteinberg(bm: TBitmap32; Palette: TiePalette);
var
  x, y: integer;
  RedError, GreenError, BlueError: integer;
  RedError_Remaining, GreenError_Remaining, BlueError_Remaining: integer;
  p: PColor32;
  cOld, cNew: TColor32;

  procedure AddError(Condition: boolean; pDest: PByteArray;
    r, g, b: integer);
  begin
    if Condition then
    begin
      pDest[0] := Max(0, Min($ff, Integer(pDest[0]) + r));
      pDest[1] := Max(0, Min($ff, Integer(pDest[1]) + g));
      pDest[2] := Max(0, Min($ff, Integer(pDest[2]) + b));
    end;

    RedError_Remaining -= r;
    GreenError_Remaining -= g;
    BlueError_Remaining -= b;
  end;

begin
  p := bm.Data;
  for y := 0 to bm.Height - 1 do
  for x := 0 to bm.Width - 1 do
  begin
    if p^ shr 24 < $80 then p^ := cl32Transparent else
    begin
      cOld := p^;
      cNew := Palette.FindClosestColor(cOld and clWhite) or cl32Opaque;
      p^ := cNew;

      // calc error
      RedError := Integer(cOld and $ff) - (cNew and $ff);
      GreenError := Integer((cOld shr 8) and $ff) - ((cNew shr 8) and $ff);
      BlueError := Integer((cOld shr 16) and $ff) - ((cNew shr 16) and $ff);
      RedError_Remaining := RedError;
      GreenError_Remaining := GreenError;
      BlueError_Remaining := BlueError;

      // add error to neighboring pixels
      AddError(x < bm.Width-1, @PColor32Array(p)[1],
        RedError*7 div 16, GreenError*7 div 16, BlueError*7 div 16);
      AddError((x > 0) and (y < bm.Height-1), @PColor32Array(p)[bm.Width-1],
        RedError*3 div 16, GreenError*3 div 16, BlueError*3 div 16);
      AddError(y < bm.Height-1, @PColor32Array(p)[bm.Width],
        RedError*5 div 16, GreenError*5 div 16, BlueError*5 div 16);
      AddError((x < bm.Width-1) and (y < bm.Height-1),
        @PColor32Array(p)[bm.Width+1],
        RedError_Remaining, GreenError_Remaining, BlueError_Remaining);
    end;

    inc(p);
  end; // for x, y
end;

procedure TiePalette.ApplyToBitmap;
var
  i: integer;
  p: PColor32;
  pa: PByteArray absolute p;

begin
  if FCount <= 0 then Exit;

  if Dither then
    DitherFloydSteinberg(bm, Self) else
  begin
    p := bm.Data;
    for i := 1 to bm.Width * bm.Height do
    begin
      if pa[3] >= $80 then
      begin
        p^ := FindClosestColor(p^ and clWhite) or cl32Opaque;
      end else
        if p^ <> cl32Inverted then p^ := cl32Transparent;

      // iterate
      inc(p);
    end; // for i
  end;
end;

/////////////////////////

function GetPixelFormat32;
var
  i, j: integer;
  p: PByteArray;
  palNeedDestroy: boolean;
  c: TColor;

label
  _Continue;

begin
  palNeedDestroy := not Assigned(PaletteOut);
  if Assigned(PaletteOut) then PaletteOut.Clear else PaletteOut := TiePalette.Create;

  try
    Result := pf32_1bit;

    p := bm.Data;
    for i := 1 to bm.Width * bm.Height do
    begin
      // partially transparent -> has alpha channel
      if (p[3] <> 0) and (p[3] <> $ff) then
      begin
        Result := pf32_32bit;
        Exit;
      end;

      // fully opaque or fully transparent

      // 24-bit: no surprise at all
      if Result = pf32_24bit then goto _Continue;

      // look up the color
      if p[3] = 0 then
        c := IfThen(PColor32(p)^ = cl32Inverted, clWhite, clBlack) else
        c := PColor32(p)^ and $ffffff;
      j := PaletteOut.IndexOf(c);

      if j < 0 then
      // more than 256 colors -> 24 bit
      if PaletteOut.Count >= 256 then Result := pf32_24bit else
      begin
        PaletteOut.Add(c, False);

        // grow pixel format, if needed
        // don't use 'case' -- Result is changed between if clauses
        if Result = pf32_1bit then
        begin
          if (PaletteOut.Count > 2) or
            ((RefPal1 <> nil) and (RefPal1.IndexOf(c) < 0)) then
          inc(Result);
        end;
        if Result = pf32_4bit then
        begin
          if (PaletteOut.Count > 16) or
            ((RefPal4 <> nil) and (RefPal4.IndexOf(c) < 0)) then
          inc(Result);
        end;
        if Result = pf32_8bit then
        begin
          if (RefPal8 <> nil) and (RefPal8.IndexOf(c) < 0) then
            inc(Result);
        end;
      end; // add color to PaletteOut

    _Continue:
      inc(PColor32(p));
    end;
  finally
    if palNeedDestroy then PaletteOut.Free;
  end;
end;

procedure ReduceColors;
var
  i: integer;
  p: PByteArray;
  palNeedDestroy: boolean;
  cq: TColorQuantizer;

begin
  if cr = cr32 then Exit;

  bm.ThresholdAlpha;
  if cr = cr24 then Exit;

  // indexed mode -> create palette
  palNeedDestroy := not Assigned(PaletteOut);
  if Assigned(PaletteOut) then PaletteOut.Clear else PaletteOut := TiePalette.Create;

  try
    case cr of
      crBW: PaletteOut.Assign(palBW);
      crWin16: PaletteOut.Assign(palWin16);
      crMac16: PaletteOut.Assign(palMac16);
      crAdaptive256: begin
        // Create an adaptive palette
        // reserve place for black and white -> 256 - 2 = 254 colors max.
        cq := TOctreeQuantizer.Create(254);
        try
          p := bm.Data;
          for i := 1 to bm.Width * bm.Height do
          begin
            if p[3] >= $80 then cq.Process(PColor32(p)^ and $ffffff);
            inc(PColor32(p));
          end;

          // create palette
          PaletteOut.Add(clBlack, False);
          PaletteOut.Add(clWhite, False);
          cq.GetPalette(PaletteOut);
        finally
          cq.Free;
        end; // try cq
      end; // pf32_8bit
      crMac256: PaletteOut.Assign(palMac256);
    end; // case Value

    // apply the created palette
    PaletteOut.ApplyToBitmap(bm, True);
  finally
    if palNeedDestroy then PaletteOut.Free;
  end;
end;

procedure OptimizeBitmapPF;
{$ifdef windows}
var
  Palette: TiePalette;
  pf: TPixelFormat;
  i, x, y: integer;
  p: Pointer;
  c: TColor;
  logPal: PLOGPALETTE;
  hPalette: THandle;
  bm2: TBitmap;
  sl: TScanLines;
{$endif}
begin
{$ifdef windows}
  Palette := TiePalette.Create;
  try
    // calculate palette and pixel format
    pf := pf1bit;
    c := 0;

    sl := TScanLines.Create(bm);
    try
      for y := 0 to bm.Height - 1 do
      begin
        p := sl[y];

        for x := 0 to bm.Width - 1 do
        begin
          // get the color
          Move3(p, @c);
          c := FlipColor(c);

          if Palette.IndexOf(c) < 0 then
          if Palette.Count >= 256 then
          begin
            pf := pf24bit;
            Break;
          end else
            Palette.Add(c, False);

          // iterate
          inc(PByte(p), sl.BytesPerPixel);
        end; // for x
      end; // for y
    finally
      sl.Free;
    end;

    if pf <> pf24bit then
      if Palette.Count <= 2 then pf := pf1bit else
      if Palette.Count <= 16 then pf := pf4bit else
        pf := pf8bit;

    // create the windows-compatible palette
    logPal := GetMem(SizeOf(LOGPALETTE) + SizeOf(PALETTEENTRY) * (Palette.Count - 1));
    try
      logPal.palVersion := $300;
      logPal.palNumEntries := Palette.Count;
      for i := 0 to Palette.Count - 1 do with logPal.palPalEntry[i] do
      begin
        c := Palette[i];
        Move3(@c, @peRed);
        peFlags := 0;
      end;

      hPalette := CreatePalette(logPal^);
    finally
      FreeAndNilMem(logPal);
    end;
  finally
    Palette.Free;
  end;

  // apply the palette and pixel format to the bitmap
  bm2 := TBitmap.Create;
  try
    bm2.Assign(bm);

    bm.PixelFormat := pf;
    bm.Palette := hPalette;
    bm.Canvas.Draw(0, 0, bm2);
  finally
    bm2.Free;
  end;
{$endif}
end;

// include palette data
{$i Palette.inc}

initialization
  palBW := TiePalette.Create;
  palBW.Add(clBlack, True);
  palBW.Add(clWhite, True);
  palWin16 := TiePalette.CreateFromArray(@palWin16_Data, 16);
  palMac16 := TiePalette.CreateFromArray(@palMac16_Data, 16);
  palMac256 := TiePalette.CreateFromArray(@palMac256_Data, 256);
finalization
  palBW.Free;
  palWin16.Free;
  palMac16.Free;
  palMac256.Free;
end.
