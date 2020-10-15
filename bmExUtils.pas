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
unit bmExUtils;

interface

uses
  LclIntf, LclType, SysUtils, Types, Graphics, GraphType, FastDiv, fpimage;

{$MACRO ON}

{$IFDEF CPUX86_64}
  {$DEFINE CPUX86_32_OR_64}
{$ENDIF}
{$IFDEF CPU386}
  {$DEFINE CPUX86_32_OR_64}
{$ENDIF}

{$define UseAsm}

// ***** ABI *****
// Win 32-bit: eax, edx, ecx
// Win 64-bit: rcx, rdx, r8, r9
// Linux 32-bit: eax, edx, ecx (?)
// Linux 64-bit (System V AMD64 ABI): rdi, rsi, rdx, rcx

type
  // an RGBA color
  PColor32 = ^TColor32;
  TColor32 = Cardinal;
  PColor32Array = ^TColor32Array;
  TColor32Array = array[0..100000000] of TColor32;

  // 32-bit RGBA color with premultiplied 0..256 alpha. Stored on 64 bits.
  // Does not have inverted color!
  PPremulColor32 = ^TPremulColor32;
  TPremulColor32 = UInt64;
  PWord4 = ^TWord4;
  TWord4 = packed array[0..3] of UInt16;

  PPointArray = ^TPointArray;
  TPointArray = array[0..1000000] of TPoint;
  TDynPointArray = array of TPoint;

  TColor2 = array[0..1] of TColor;
  THatchDesc = record
    Color: TColor2;
  end;

  TArrayOfByte = array of byte;
  TArrayOfInteger = array of integer;
  TArrayOfDouble = array of double;

  TIntegerRange = record
    Min: integer;
    Max: integer;

    procedure Clear; inline;
    function Empty: boolean; inline;
    procedure Include(x: integer); inline;
    procedure IncludeRange(x1, x2: integer); inline;
  end;
  TArrayOfIntegerRange = array of TIntegerRange;

  TInteger3 = array[0..2] of integer;
  TDouble3 = array[0..2] of double;
  TDouble4 = array[0..3] of double;

const
  cl32Opaque = $ff000000;
  cl32Transparent = $00000000;
  // A special value (white with 0 alpha) means Inverted
  cl32Inverted = $00ffffff;

  cl32Black = $ff000000;
  cl32Navy = $ff800000;
  cl32Green = $ff008000;
  cl32Teal = $ff808000;
  cl32Maroon = $ff000080;
  cl32Purple = $ff800080;
  cl32Olive = $ff008080;
  cl32Gray = $ff808080;

  cl32Silver = $ffc0c0c0;
  cl32Blue = $ffff0000;
  cl32Lime = $ff00ff00;
  cl32Aqua = $ffffff00;
  cl32Red = $ff0000ff;
  cl32Fuchsia = $ffff00ff;
  cl32Yellow = $ff00ffff;
  cl32White = $ffffffff;

  GRAY_COEFF_RED = 19661;
  GRAY_COEFF_GREEN = 38666;
  GRAY_COEFF_BLUE = 7209;

type
  // A structure which is able to calc. the average of several colors
  TColor32Average<T> = class
  private
    FCount: integer;
    Sum: array[0..2] of T;
    AlphaSum: T;
  public
    constructor Create;
    
    procedure Clear; inline;
    procedure AddWeighted(c: TColor32; Factor: integer); inline;
    procedure Add(c: TColor32); inline;
    procedure AddAlpha256(c: TColor32); inline;
    procedure Sub(c: TColor32); inline;
    function GetAverage: TColor32; inline;
    // Use AddAlpha256 if using GetPremulAverage.
    procedure GetPremulAverage(var c: TPremulColor32); inline;

    property Count: integer read FCount;
  end;
  // Can average up to 2^15 pixels
  TSmallColor32Average = TColor32Average<integer>;
  // Can average up to 2^31 pixels
  TBigColor32Average = TColor32Average<Int64>;

  // A structure which is able to calc. the average _alpha value_ of several colors
  // Always returns a black color, with of course has varying alpha!
  TAlphaAverage<T> = class
  private
    FCount: integer;
    AlphaSum: T;
  public
    constructor Create;

    procedure Clear; inline;
    procedure Add(c: TColor32); inline;
    procedure Sub(c: TColor32); inline;
    function GetAverage: TColor32; inline;

    property Count: integer read FCount;
  end;
  // Can average up to 2^23 pixels
  TSmallAlphaAverage = TAlphaAverage<integer>;
  // Can average up to 2^31 pixels
  TBigAlphaAverage = TAlphaAverage<Int64>;

  TScanLines = class
  private
    bm: TRasterImage;
    FWidth, FHeight: integer;
    FDepth: integer;
    FBytesPerPixel: integer;
    ScanLine0: PByteArray;
    FLineSize: integer;
    function GetLine(Index: integer): PByteArray; inline;
  public
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    property Depth: integer read FDepth; // in bytes
    property BytesPerPixel: integer read FBytesPerPixel; // can be >Depth
    property LineSize: integer read FLineSize; // in bytes
    property Lines[Index: integer]: PByteArray read GetLine; default;
    constructor Create(abm: TRasterImage); virtual;
    destructor Destroy; override;
  end;

// Converts from 0..255 to 0..256 alpha values
function Alpha255to256(x: integer): integer; inline;
// Converts from 0..256 to 0..255 alpha values
function Alpha256to255(x: integer): integer; inline;
// Grade: 0..$ff
function BlendColors(Src, Dest: TColor32; Grade: integer): TColor32;
// There should be an intrinsic function for this
function BSwapS(i: integer): integer; inline;
function BSwapU(i: Cardinal): Cardinal; inline;
// Returns if the colors are closer to each other than Tolerance
function CanFloodFill(c1, c2: TColor32; Tolerance: integer): boolean; inline;
// Converts a 24-bit bitmap to a 32-bit bitmap
procedure ConvertBGRtoRGBA(Src, Dest: PByte; Width: integer);
// if left > right, swaps them, and similarly for top and bottom
function DefaultRectOrientation(const r: TRect): TRect;
// Similar to LineDDA
procedure EnumLinePoints(x1, y1, x2, y2: integer; var pt: TDynPointArray);
procedure FastZeroMemory(p: Pointer; size: integer); inline;
procedure FreeAndNilMem(var p);
// Swaps red and blue components
function FlipColor(c: TColor): TColor; inline;
function FlipColor32(c: TColor32): TColor32; inline;
// Returns true if c has maximum alpha and c is not the inverted color.
function IsOpaque(c: TColor32): boolean; inline;
// Returns whether c represents a transparent color
// This means that c.alpha = 0 and c <> inverted
function IsTransparent(c: TColor32): boolean; inline;
// Checks if a memory chunk is full of zeroes
function IsZeroMemory(Mem: PByteArray; Size: integer): boolean;
// Draws a hatch background on a bitmap
procedure MakeTransparentHatch(bm: TBitmap; r: TRect; xHatchOrigin, yHatchOrigin: integer; Hatch: THatchDesc);
// Moves 3 bytes from Src to Dest
procedure Move3(const Src, Dest: Pointer); inline;
// Calculates the number of additional bytes needed to make x divisible by 4
function PadTo4(x: integer): integer; inline;

// Combines a 32-bit (foreground) and a 24-bit (background) color
function Put32to24(Dest: TColor; Src: TColor32): TColor;
{$ifdef UseAsm}
inline;
{$endif}

// Combines a 32-bit (foreground) color with premultiplied alpha, and a 24-bit (background) color
function PutPremul32to24(Dest: TColor; Src: PPremulColor32): TColor;
{$ifdef UseAsm}
inline;
{$endif}

// Combines two scanlines
procedure Put32to24_BGRScanLine(Src, Dest: Pointer; Width: integer); // Src: [RGBA], Dest: [BGR]
procedure Put32to24_BGRXScanLine(Src, Dest: Pointer; Width: integer); // Src: [RGBA], Dest: [BGRX]
// Projects a color onto another
function PutPixel32(Src, Dest: TColor32): TColor32; inline;

// Warning: this can produce inverted color, or any invalid color with alpha=0!
function FPColorTo32(const c: TFPColor): TColor32;
function FPColorFrom32(c: TColor32): TFPColor;

implementation

uses
  dlgDebug;

// TIntegerRange

procedure TIntegerRange.Clear;
begin
  Min := High(Integer);
  Max := Low(Integer);
end;

function TIntegerRange.Empty: boolean;
begin
  Result := (Min > Max);
end;

procedure TIntegerRange.Include(x: integer);
begin
  if x < Min then
    Min := x;
  if x > Max then
    Max := x;
end;

procedure TIntegerRange.IncludeRange(x1, x2: integer);
begin
  if x1 <= x2 then
  begin
    Include(x1);
    Include(x2);
  end;
end;

// TColor32Average

constructor TColor32Average<T>.Create;
begin
  Clear;
end;

procedure TColor32Average<T>.Clear;
begin
  FCount := 0;
  AlphaSum := 0;
  Sum[0] := 0;
  Sum[1] := 0;
  Sum[2] := 0;
end;

procedure TColor32Average<T>.AddWeighted(c: TColor32; Factor: integer);
var
  Alpha: integer;

begin
  inc(FCount, Factor);
  if c and cl32Opaque <> 0 then
  begin
    Alpha := Factor * Integer(c shr 24);
    AlphaSum += Alpha;
    Sum[0] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[1] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[2] += Alpha * Integer(c and $ff);
  end;
end;

procedure TColor32Average<T>.Add(c: TColor32);
var
  Alpha: integer;

begin
  inc(FCount);
  if c and cl32Opaque <> 0 then
  begin
    Alpha := Integer(c shr 24);
    AlphaSum += Alpha;
    Sum[0] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[1] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[2] += Alpha * Integer(c and $ff);
  end;
end;

procedure TColor32Average<T>.AddAlpha256(c: TColor32);
var
  Alpha: integer;

begin
  inc(FCount);
  if c and cl32Opaque <> 0 then
  begin
    Alpha := Alpha255to256(Integer(c shr 24));
    AlphaSum += Alpha;
    Sum[0] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[1] += Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[2] += Alpha * Integer(c and $ff);
  end;
end;

procedure TColor32Average<T>.Sub(c: TColor32);
var
  Alpha: integer;

begin
  dec(FCount);
  if c and cl32Opaque <> 0 then
  begin
    Alpha := Integer(c shr 24);
    AlphaSum -= Alpha;
    Sum[0] -= Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[1] -= Alpha * Integer(c and $ff);
    c := c shr 8;
    Sum[2] -= Alpha * Integer(c and $ff);
  end;
end;

function TColor32Average<T>.GetAverage;
var
  CountDiv2, i, a: integer;

begin
  CountDiv2 := Count div 2;
  if (AlphaSum = 0) or (AlphaSum + CountDiv2 < Count) then
    Result := cl32Transparent
  else begin
    a := (AlphaSum + CountDiv2) div Count;
    i := AlphaSum div 2;
    Result := ((Sum[0] + i) div AlphaSum) or
      ( ((Sum[1] + i) div AlphaSum) shl 8 ) or
      ( ((Sum[2] + i) div AlphaSum) shl 16 ) or
      ( a shl 24 );
  end;
end;

procedure TColor32Average<T>.GetPremulAverage(var c: TPremulColor32);
var
  comp: TWord4 absolute c;
  DivByCount: TDivFunc;
  CountDiv2: integer;

begin
  CountDiv2 := Count div 2;
  if (AlphaSum = 0) or (AlphaSum + CountDiv2 < Count) then
    c := 0
  else if Count < Length(DivFunc) then
  begin
    DivByCount := DivFunc[Count];
    comp[0] := DivByCount(Sum[0] + CountDiv2);
    comp[1] := DivByCount(Sum[1] + CountDiv2);
    comp[2] := DivByCount(Sum[2] + CountDiv2);
    comp[3] := DivByCount(AlphaSum + CountDiv2);
  end else
  begin
    comp[0] := (Sum[0] + CountDiv2) div Count;
    comp[1] := (Sum[1] + CountDiv2) div Count;
    comp[2] := (Sum[2] + CountDiv2) div Count;
    comp[3] := (AlphaSum + CountDiv2) div Count;
  end;
end;

// TAlphaAverage

constructor TAlphaAverage<T>.Create;
begin
  Clear;
end;

procedure TAlphaAverage<T>.Clear;
begin
  FCount := 0;
  AlphaSum := 0;
end;

procedure TAlphaAverage<T>.Add(c: TColor32);
begin
  inc(FCount);
  AlphaSum += Integer(c shr 24);
end;

procedure TAlphaAverage<T>.Sub(c: TColor32);
begin
  dec(FCount);
  AlphaSum -= Integer(c shr 24);
end;

function TAlphaAverage<T>.GetAverage;
begin
  if AlphaSum = 0 then
    Result := cl32Transparent else
    Result := ((AlphaSum + Count div 2) div Count) shl 24;
end;

// TScanLines

function TScanLines.GetLine;
begin
  Result := @ScanLine0[LineSize*Index];
end;

constructor TScanLines.Create;
var
  ri: TRawImage;

begin
  bm := abm;
  FWidth := bm.Width;
  FHeight := bm.Height;

  bm.BeginUpdate(False);
  ri := bm.RawImage;
//  Log(ri.Description.AsString);
  FDepth := ri.Description.Depth shr 3;
  FBytesPerPixel := ri.Description.BitsPerPixel shr 3;
  case ri.Description.LineOrder of
    riloBottomToTop: begin
      FLineSize := -Integer(ri.Description.BytesPerLine);
      ScanLine0 := @PByteArray(ri.Data)[-(bm.Height-1)*LineSize];
    end;
    riloTopToBottom: begin
      FLineSize := Integer(ri.Description.BytesPerLine);
      ScanLine0 := PByteArray(ri.Data);
    end;
  end;
//  Assert((BytesPerPixel = 3) or (BytesPerPixel = 4), IntToStr(BytesPerPixel));
end;

destructor TScanLines.Destroy;
begin
  bm.EndUpdate(False);
end;

////////////

function Alpha255to256;
begin
  Result := x+(x shr 7);
end;
(*
  cmp ax, $80
  sbb ax, -1
*)

function Alpha256to255;
begin
  Result := x-Integer(x >= $80);
end;
(*
  cmp eax, $80
  adc eax, -1
*)

function BlendColors;
var
  i, j, d: integer;
  cS: array[0..3] of byte absolute Src;
  cD: array[0..3] of byte absolute Dest;
  cR: array[0..3] of byte absolute Result;

begin
  if Grade = 0 then Result := Src else
  if Grade = $ff then Result := Dest else
  begin
    Grade := Alpha255to256(Grade);
    d := (Integer(cS[3]) shl 8) + Grade * (Integer(cD[3]) - cS[3]);
    if d < $100 then Result := cl32Transparent else
    begin
      cR[3] := d shr 8;
      j := ($100 - Grade) * cS[3];
      for i := 0 to 2 do cR[i] := (j * cS[i] + (d - j) * cD[i]) div d;
    end;
  end;

  if Result = cl32Inverted then Result := cl32Transparent;
end;

function BSwapS(i: integer): integer;
begin
  Result := Integer(BSwapU(Cardinal(i)));
end;

function BSwapU(i: Cardinal): Cardinal;
begin
  Result := RolDWord(i and $ff00ff00, 8) or RorDWord(i and $00ff00ff, 8);
end;

function CanFloodFill;
begin
  // When doing a floodfill, CanFloodFill will evaluate to true
  // approx. A times, and evalute to false approx. P times
  // where A is the area and P is the perimeter of the filled region
  // In most cases, A is much greater than P
  // so we must optimize the function in a way that the True branch will be fast
  // A good start is comparing the two colors for equality first
  // -- this will evalute to true in most of the cases
  if c1 = c2 then Exit(True);
  if Tolerance = 0 then Exit(False);

  if Abs(c1 and $ff - c2 and $ff) > Tolerance then Exit(False);
  c1 := c1 shr 8;
  c2 := c2 shr 8;
  if Abs(c1 and $ff - c2 and $ff) > Tolerance then Exit(False);
  c1 := c1 shr 8;
  c2 := c2 shr 8;
  if Abs(c1 and $ff - c2 and $ff) > Tolerance then Exit(False);
  c1 := c1 shr 8;
  c2 := c2 shr 8;
  if Abs(c1 and $ff - c2 and $ff) > Tolerance then Exit(False);

  Result := True;
end;

procedure ConvertBGRtoRGBA;
var
  i: integer;

begin
  for i := 1 to Width do
  begin
    Dest^ := PByteArray(Src)[2]; inc(Dest);
    Dest^ := PByteArray(Src)[1]; inc(Dest);
    Dest^ := Src^; inc(Dest);
    Dest^ := $ff; inc(Dest);
    inc(Src, 3);
  end;
end;

function DefaultRectOrientation;
begin
  if r.Left < r.Right then
    begin Result.Left := r.Left; Result.Right := r.Right; end else
    begin Result.Left := r.Right; Result.Right := r.Left; end;

  if r.Top < r.Bottom then
    begin Result.Top := r.Top; Result.Bottom := r.Bottom; end else
    begin Result.Top := r.Bottom; Result.Bottom := r.Top; end;
end;

procedure EnumLinePoints;
var
  i, j, xSize, ySize: integer;

  procedure SwapPoints;
  var
    t: integer;

  begin
    t := x1; x1 := x2; x2 := t;
    t := y1; y1 := y2; y2 := t;
  end;

begin
  if (x1 = x2) and (y1 = y2) then
  // zero length
  begin
    SetLength(pt, 1);
    pt[0].X := x1;
    pt[0].Y := y1;
    Exit;
  end;

  // nonzero length
  xSize := Abs(x1 - x2) + 1;
  ySize := Abs(y1 - y2) + 1;

  if xSize > ySize then
  begin
    SetLength(pt, xSize);
    if x1 > x2 then SwapPoints;

    j := 0;
    for i := x1 to x2 do
    begin
      pt[j].X := i;
      pt[j].Y := y1 + Round((y2 - y1) * (i - x1) / (x2 - x1));
      inc(j);
    end; // for i
  end else
  begin
    SetLength(pt, ySize);
    if y1 > y2 then SwapPoints;

    j := 0;
    for i := y1 to y2 do
    begin
      pt[j].X := x1 + Round((x2 - x1) * (i - y1) / (y2 - y1));
      pt[j].Y := i;
      inc(j);
    end; // for i
  end; // if xSize > ySize
end;

procedure FastZeroMemory(p: Pointer; size: integer);
begin
  FillChar(p^, Size, #0);
end;

procedure FreeAndNilMem(var p);
var
  ptr: Pointer;
begin
  ptr := Pointer(p);
  Pointer(p) := nil;
  if ptr <> nil then
    FreeMem(ptr);
end;

function FlipColor(c: TColor): TColor;
begin
  Result := TColor(FlipColor32(TColor32(c)));
end;

// return ror8(bswap(x))
function FlipColor32(c: TColor32): TColor32;
begin
  Result := (c and $ff00ff00) or RorDWord(c and $00ff00ff, 16);
end;

function IsOpaque(c: TColor32): boolean;
begin
  Result := (c shr 24 = $ff);
end;

function IsTransparent;
begin
  Result := ((c and cl32Opaque) = 0) and (c <> cl32Inverted);
end;

function IsZeroMemory;
var
  i: integer;

begin
  for i := 0 to Size - 1 do if Mem[i] <> 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure MakeTransparentHatch;
var
  x, y: integer;
  p: Pointer;
  sl: TScanLines;

begin
  Hatch.Color[0] := FlipColor(Hatch.Color[0]);
  Hatch.Color[1] := FlipColor(Hatch.Color[1]);

  if r.Left < 0 then r.Left := 0;
  if r.Right > bm.Width then r.Right := bm.Width;
  if r.Top < 0 then r.Top := 0;
  if r.Bottom > bm.Height then r.Bottom := bm.Height;

  bm.PixelFormat := pf24bit;
  sl := TScanLines.Create(bm);
  try
    for y := r.Top to r.Bottom - 1 do
    begin
      p := sl[y];
      Inc(PByte(p), r.Left*sl.BytesPerPixel);

      for x := r.Left to r.Right - 1 do
      begin
        Move3(@Hatch.Color[( ((y-yHatchOrigin) xor (x-xHatchOrigin)) shr 2 ) and 1], p);
        Inc(PByte(p), sl.BytesPerPixel);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure Move3;
begin
  PWord(Dest)^ := PWord(Src)^;
  PByteArray(Dest)[2] := PByteArray(Src)[2];
end;

function PadTo4;
begin
  Result := x and 3;
  if Result <> 0 then Result := 4 - Result;
end;

{$ifdef UseAsm}
function Put32to24(Dest: TColor; Src: TColor32): TColor; assembler; nostackframe;
{$IFDEF CPUX86_32_OR_64}
asm
{$IFDEF CPU64}
{$IFDEF WINDOWS}
  xchg ecx, eax
{$ELSE}
{$IFDEF LINUX}
  mov rax, rdi
  mov rdx, rsi
{$ELSE}
{$ERROR No asm Put32to24 on this platform}
{$ENDIF}
{$ENDIF}
{$ENDIF}

  test edx, cl32Opaque
  jz @ZeroAlpha

  // move Alpha to cl
  mov ecx, edx
  shr ecx, 24
  // convert source from 32-bit to 24-bit
  and edx, $ffffff

  cmp cl, $ff
  jz @FullyOpaque

  // General case: semi-transparent pixel
  // convert alpha from 0..255 to 0..256
  cmp cx, $80
  sbb cx, -1

  // four vectors: dest, source, alpha
  pxor mm0, mm0
  movd mm1, eax // dest
  movd mm2, edx // source
  movd mm3, ecx // alpha

  // fill mm3 with Alpha and convert RGBA bytes to words
  punpcklbw mm1, mm0
  punpcklwd mm3, mm3
  punpcklbw mm2, mm0
  punpcklwd mm3, mm3

  // calculate dest + (source - dest) * alpha div $100 =
  // [(source - dest) * alpha + (dest shl 8)] shr 8
  psubw mm2, mm1 // source - dest
  pmullw mm2, mm3 // (source - dest) * alpha
  psllw mm1, 8
  paddw mm2, mm1 // ... + (dest shl 8)
  psrlw mm2, 8 // ... shr 8

  // move the result to eax
  packuswb mm2, mm0
  movd eax, mm2

  emms
  ret

@FullyOpaque:
  mov eax, edx
  ret

@ZeroAlpha:
  cmp edx, cl32Inverted
  jnz @Exit
  xor eax, clWhite
@Exit:
{$ELSE}
begin
  if Src = cl32Inverted then
    Result := Dest xor clWhite
  else Result := TColor(PutPixel32(Src, TColor32(Dest) or cl32Opaque) and not cl32Opaque);
{$ENDIF}
{$else}
function Put32to24(Dest: TColor; Src: TColor32): TColor;
var
  alphaS: integer;
  cS: array[0..3] of byte absolute Src;
  cD: array[0..3] of byte absolute Dest;
  cR: array[0..3] of byte absolute Result;

begin
  // 0..$ff source alpha -> alphaS
  alphaS := Src shr 24;

  // source is solid
  if alphaS = $ff then Result := TColor(Src) and clWhite else
  // source is clear
  if alphaS = 0 then
  begin
    if Src = cl32Inverted then
      Result := Dest xor clWhite else
      Result := Dest;
  end else
  // alpha blending
  begin
    // 0..$100 source alpha -> alphaS
    alphaS := Alpha255to256(alphaS);

    Result := 0;
    cR[0] := ((cD[0] shl 8) + alphaS * (Integer(cS[0]) - cD[0]) + $80) shr 8;
    cR[1] := ((cD[1] shl 8) + alphaS * (Integer(cS[1]) - cD[1]) + $80) shr 8;
    cR[2] := ((cD[2] shl 8) + alphaS * (Integer(cS[2]) - cD[2]) + $80) shr 8;
  end; // alpha blending
{$endif}
end;

{$ifdef UseAsm}
function PutPremul32to24(Dest: TColor; Src: PPremulColor32): TColor; assembler; nostackframe;
{$IFDEF CPUX86_32_OR_64}
asm
{$IFDEF CPU64}
{$IFDEF WINDOWS}
  xchg ecx, eax
{$ELSE}
{$IFDEF LINUX}
  mov rax, rdi
  mov rdx, rsi
{$ELSE}
{$ERROR No asm PutPremul32to24 on this platform}
{$ENDIF}
{$ENDIF}
{$ENDIF}

  // eax: 24-bit background
  // [e/r]dx: pointer to uint16[4], foreground

  // cx := alpha, test if 0
  mov cx, word ptr [Src+3*2]
  test cx, cx
  jz @ZeroAlpha

  cmp cx, $100
  jz @FullyOpaque
  // now we know that alpha <= 255, so actually cl contains the alpha

  // four vectors: dest, source, alpha
  pxor mm0, mm0
  movd mm1, eax // dest
  movq mm2, qword ptr [Src] // source
  movd mm3, ecx // alpha

  // fill mm3 with Alpha and convert RGBA bytes to words
  punpcklwd mm3, mm3
  punpcklbw mm1, mm0
  punpcklwd mm3, mm3

  // calculate [source - dest * alpha + (dest shl 8)] shr 8
  // because source is premultiplied!
  pmullw mm3, mm1 // mm3 := dest * alpha
  psllw mm1, 8 // mm1 := dest shl 8
  psubw mm2, mm3 // mm2 := source - dest * alpha
  paddw mm1, mm2 // [...]
  psrlw mm1, 8 // ... shr 8

  // move the result to eax
  // alpha channel of eax will be automatically zero
  packuswb mm1, mm0
  movd eax, mm1

  emms
  ret

@FullyOpaque:
  // premultiplied, so each channel is multiplied by $100
  movzx eax, byte ptr [Src + 5] // blue
  shl eax, 8
  mov al, byte ptr [Src + 3] // green
  shl eax, 8
  mov al, byte ptr [Src + 1] // red
  ret

@ZeroAlpha:
@Exit:
{$ENDIF}
{$else}
function PutPremul32to24(Dest: TColor; Src: PPremulColor32): TColor;
var
  alphaS, x: integer;
  cS: PWord4;
  cD: array[0..3] of byte absolute Dest;
  cR: array[0..3] of byte absolute Result;

begin
  cS := PWord4(Src);
  // 0..$100 source alpha -> alphaS
  alphaS := cS[3];
  Result := 0;

  // source is solid
  if alphaS = $100 then
  begin
    cR[0] := cS[0] shr 8;
    cR[1] := cS[1] shr 8;
    cR[2] := cS[2] shr 8;
  end else
  // source is clear
  if alphaS = 0 then
  begin
    Result := Dest;
  end else
  // alpha blending
  begin
    x := $100 - alphaS;
    cR[0] := (cD[0] * x + cS[0] + $80) shr 8;
    cR[1] := (cD[1] * x + cS[1] + $80) shr 8;
    cR[2] := (cD[2] * x + cS[2] + $80) shr 8;
  end; // alpha blending
{$endif}
end;

procedure Put32to24_BGRScanLine;
var
  i: integer;
  srcColor: TColor32;
  destColor: TColor;

begin
  destColor := 0;
  for i := 1 to Width do
  begin
    srcColor := FlipColor32(PColor32(Src)^);
    inc(PColor32(Src));
    Move3(Dest, @destColor);
    destColor := Put32to24(destColor, srcColor);
    Move3(@destColor, Dest);
    inc(PByte(Dest), 3);
  end;
end;

procedure Put32to24_BGRXScanLine;
var
  i: integer;
  srcColor: TColor32;
  pcDest: PColor;

begin
  pcDest := Dest;
  for i := 1 to Width do
  begin
    srcColor := FlipColor32(PColor32(Src)^);
    inc(PColor32(Src));
    pcDest^ := Put32to24(pcDest^ and clWhite, srcColor);
    inc(pcDest);
  end;
end;

function PutPixel32;
var
  alphaS, x, alpha, a2: integer;
  cS: array[0..3] of byte absolute Src;
  cD: array[0..3] of byte absolute Dest;
  cR: array[0..3] of byte absolute Result;
  DivByAlpha: TDivFunc;

begin
  // 0..$ff source alpha -> alphaS
  alphaS := Src shr 24;

  // source is solid
  if alphaS = $ff then Result := Src else
  // source is clear
  if alphaS = 0 then
  begin
    if Src = cl32Inverted then
      Result := cl32Inverted else
      Result := Dest;
  end else
  // alpha blending
  begin
    // 0..$ff dest alpha -> x
    x := Dest shr 24;
    // projecting onto a transparent pixel?
    if x = 0 then Result := Src else
    begin
      // 0..$100 source alpha -> alphaS
      alphaS := Alpha255to256(alphaS);
      // alphaD * (1 - alphaS) -> 0..$100 value
      x := (Alpha255to256(x) * ($100 - alphaS)) shr 8;
      // 0..$100 result alpha
      alpha := alphaS + x;
      DivByAlpha := DivFunc[alpha];
      a2 := alpha div 2;

      cR[0] := DivByAlpha(x * cD[0] + alphaS * cS[0] + a2);
      cR[1] := DivByAlpha(x * cD[1] + alphaS * cS[1] + a2);
      cR[2] := DivByAlpha(x * cD[2] + alphaS * cS[2] + a2);

      // 0..$ff result alpha
      cR[3] := Alpha256to255(alpha);
    end; // result has nonzero alpha
  end; // alpha blending
end;

function FPColorTo32(const c: TFPColor): TColor32;
begin
  Result := (c.Red shr 8) + (c.Green and $ff00) + (TColor32(c.Blue shr 8) shl 16) + (TColor32(c.Alpha shr 8) shl 24);
end;

function FPColorFrom32(c: TColor32): TFPColor;
begin
  Result.Alpha := (c shr 24)*$101;
  if Result.Alpha = 0 then c := cl32Transparent; // remove inverted color
  Result.Red := (c and $ff)*$101;
  Result.Green := ((c shr 8) and $ff)*$101;
  Result.Blue := ((c shr 16) and $ff)*$101;
end;

end.

