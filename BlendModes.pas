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
unit BlendModes;

interface

uses
  LclIntf, LclType, SysUtils, Math, BitmapEx, bmExUtils,
  gfMath, Types, ColSpaces;

type
  TBlendMode = (bmNormal, bmMask, bmBehind, bmDissolve,
    bmHue, bmHueShift, bmSaturation,
    bmDarken, bmMultiply, bmColorBurn, bmLinearBurn, bmDarkerColor,
    bmLighten, bmScreen, bmColorDodge, bmLinearDodge, bmLighterColor,
    bmOverlay, bmSoftLight, bmHardLight,
    bmVividLight, bmLinearLight, bmPinLight, bmHardMix,
    bmDifference, bmExclusion);

  TBlendFunc = procedure(Src, Dest: PByteArray);

procedure bfNormal(Src, Dest: PByteArray);
procedure bfMask(Src, Dest: PByteArray);
procedure bfBehind(Src, Dest: PByteArray);
// Dissolve is a special case, because the blended value depends on X and Y
// procedure bfDissolve;
procedure bfHue(Src, Dest: PByteArray);
procedure bfHueShift(Src, Dest: PByteArray);
procedure bfSaturation(Src, Dest: PByteArray);
procedure bfDarken(Src, Dest: PByteArray);
procedure bfMultiply(Src, Dest: PByteArray);
procedure bfColorBurn(Src, Dest: PByteArray);
procedure bfLinearBurn(Src, Dest: PByteArray);
procedure bfDarkerColor(Src, Dest: PByteArray);
procedure bfLighten(Src, Dest: PByteArray);
procedure bfScreen(Src, Dest: PByteArray);
procedure bfColorDodge(Src, Dest: PByteArray);
procedure bfLinearDodge(Src, Dest: PByteArray);
procedure bfLighterColor(Src, Dest: PByteArray);
procedure bfOverlay(Src, Dest: PByteArray);
procedure bfSoftLight(Src, Dest: PByteArray);
procedure bfHardLight(Src, Dest: PByteArray);
procedure bfVividLight(Src, Dest: PByteArray);
procedure bfLinearLight(Src, Dest: PByteArray);
procedure bfPinLight(Src, Dest: PByteArray);
procedure bfHardMix(Src, Dest: PByteArray);
procedure bfDifference(Src, Dest: PByteArray);
procedure bfExclusion(Src, Dest: PByteArray);

const
  BlendFunctions: array[TBlendMode] of TBlendFunc =
    (bfNormal, bfMask, bfBehind, nil,
     bfHue, bfHueShift, bfSaturation,
     bfDarken, bfMultiply, bfColorBurn, bfLinearBurn, bfDarkerColor,
     bfLighten, bfScreen, bfColorDodge, bfLinearDodge, bfLighterColor,
     bfOverlay, bfSoftLight, bfHardLight,
     bfVividLight, bfLinearLight, bfPinLight, bfHardMix,
     bfDifference, bfExclusion);

// Accepts 0..255 Opacity values
// Clips Src by rClip, the draws the clipped bitmap to Dest,
// but at position (0, 0)!
procedure DrawBlended(Dest: TBitmap32; rClip: TRect;
  Src: TBitmap32; Opacity: integer;
  BlendMode: TBlendMode; DestIsClear: PBoolean);
// Accepts 0..255 Opacity values
// Clips Src by r, then erases Dest according to the opacity of the pixels
// in the resulting bitmap, but does not move the eraser bitmap to (0, 0) for Dest
// (as DrawBlended does)!
procedure EraseWith(Dest, Src: TBitmap32; const r: TRect; Opacity: integer);

implementation

procedure bfNormal;
begin
  PColor32(Dest)^ := PutPixel32(PColor32(Src)^, PColor32(Dest)^);
end;

procedure bfMask;
begin
  if PColor32(Dest)^ = cl32Inverted then
  begin
    if Src[3] < $80 then PColor32(Dest)^ := cl32Transparent;
  end else
  begin
    // multiply alphas
    Dest[3] := (Alpha255to256(Src[3]) * Dest[3]) shr 8;

    // avoid getting inverted
    if Dest[3] = 0 then PColor32(Dest)^ := cl32Transparent;
  end;
end;

procedure bfBehind;
begin
  PColor32(Dest)^ := PutPixel32(PColor32(Dest)^, PColor32(Src)^);
end;

// Accepts: bmHue, bmHueShift, bmSaturation
procedure bfHSBRelated(Src, Dest: PByteArray; Mode: TBlendMode); inline;
var
  bc: packed array[0..3] of byte;
  a1, a2: TInt3;

begin
  // TODO: maybe we should get and set only the affected component --
  // that would be faster

  // gray -> domain error for Hue
  if (Mode <> bmSaturation) and (Src[0] = Src[1]) and (Src[0] = Src[2]) then Exit;

  // convert both to HSB
  a1 := RGBtoHSB(Src[0], Src[1], Src[2]);
  a2 := RGBtoHSB(Dest[0], Dest[1], Dest[2]);
  // modify
  case Mode of
    bmHue: a2[0] := a1[0];
    bmHueShift: begin
      inc(a2[0], a1[0]);
      if (a2[0] >= HSBMAX) then dec(a2[0], HSBMAX);
    end;
    bmSaturation: a2[1] := a1[1];
  end;
  // convert the modified color back to RGB
  a1 := HSBtoRGB(a2[0], a2[1], a2[2]);

  // this is the resulting color
  bc[0] := a1[0];
  bc[1] := a1[1];
  bc[2] := a1[2];
  bc[3] := Dest[3];

  // produce blending color and do blending
  PColor32(Dest)^ := BlendColors(PColor32(Dest)^, TColor32(bc), Src[3]);
end;

procedure bfHue;
begin
  bfHSBRelated(Src, Dest, bmHue);
end;

procedure bfHueShift;
begin
  bfHSBRelated(Src, Dest, bmHueShift);
end;

procedure bfSaturation;
begin
  bfHSBRelated(Src, Dest, bmSaturation);
end;

procedure bfDarken;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := Min(Src[0], Dest[0]);
  bc[1] := Min(Src[1], Dest[1]);
  bc[2] := Min(Src[2], Dest[2]);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfMultiply;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := (Alpha255to256(Src[0]) * Dest[0]) shr 8;
  bc[1] := (Alpha255to256(Src[1]) * Dest[1]) shr 8;
  bc[2] := (Alpha255to256(Src[2]) * Dest[2]) shr 8;
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfColorBurn;
var
  bc: packed array[0..3] of byte;

begin
  if $ff xor Dest[0] >= Src[0] then bc[0] := 0 else
    bc[0] := $ff xor ((Integer($ff xor Dest[0]) shl 8) div Alpha255to256(Src[0]));
  if $ff xor Dest[1] >= Src[1] then bc[1] := 0 else
    bc[1] := $ff xor ((Integer($ff xor Dest[1]) shl 8) div Alpha255to256(Src[1]));
  if $ff xor Dest[2] >= Src[2] then bc[2] := 0 else
    bc[2] := $ff xor ((Integer($ff xor Dest[2]) shl 8) div Alpha255to256(Src[2]));
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfLinearBurn;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := Max(0, Integer(Src[0]) + Dest[0] - $ff);
  bc[1] := Max(0, Integer(Src[1]) + Dest[1] - $ff);
  bc[2] := Max(0, Integer(Src[2]) + Dest[2] - $ff);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfDarkerColor;
begin
  if GRAY_COEFF_RED * (Src[0] - Dest[0]) + GRAY_COEFF_GREEN * (Src[1] - Dest[1]) +
    GRAY_COEFF_BLUE * (Src[2] - Dest[2]) < 0 then
    bfNormal(Src, Dest) else bfBehind(Src, Dest);
end;

procedure bfLighten;
var
  bc: packed array[0..3] of byte;

begin
  // arithmetics
  bc[0] := Max(Src[0], Dest[0]);
  bc[1] := Max(Src[1], Dest[1]);
  bc[2] := Max(Src[2], Dest[2]);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfScreen;
var
  bc: packed array[0..3] of byte;

begin
  // arithmetics
  bc[0] := $ff xor ((Alpha255to256($ff xor Src[0]) * ($ff xor Dest[0])) shr 8);
  bc[1] := $ff xor ((Alpha255to256($ff xor Src[1]) * ($ff xor Dest[1])) shr 8);
  bc[2] := $ff xor ((Alpha255to256($ff xor Src[2]) * ($ff xor Dest[2])) shr 8);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;
           
procedure bfColorDodge;
var
  bc: packed array[0..3] of byte;

begin
  if Dest[0] >= $ff xor Src[0] then bc[0] := $ff else
    bc[0] := (Integer(Dest[0]) shl 8) div Alpha255to256($ff xor Src[0]);
  if Dest[1] >= $ff xor Src[1] then bc[1] := $ff else
    bc[1] := (Integer(Dest[1]) shl 8) div Alpha255to256($ff xor Src[1]);
  if Dest[2] >= $ff xor Src[2] then bc[2] := $ff else
    bc[2] := (Integer(Dest[2]) shl 8) div Alpha255to256($ff xor Src[2]);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfLinearDodge;
var
  bc: packed array[0..3] of byte;

begin
  // arithmetics
  bc[0] := Min($ff, Integer(Src[0]) + Dest[0]);
  bc[1] := Min($ff, Integer(Src[1]) + Dest[1]);
  bc[2] := Min($ff, Integer(Src[2]) + Dest[2]);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfLighterColor;
begin
  if GRAY_COEFF_RED * (Src[0] - Dest[0]) + GRAY_COEFF_GREEN * (Src[1] - Dest[1]) +
    GRAY_COEFF_BLUE * (Src[2] - Dest[2]) > 0 then
    bfNormal(Src, Dest) else bfBehind(Src, Dest);
end;

procedure bfOverlay;
var
  bc: packed array[0..3] of byte;

begin
  if Dest[0] < $80 then bc[0] := Min($ff, (Alpha255to256(Dest[0]) * Src[0]) shr 7) else
    bc[0] := Max(0, $ff - (Alpha255to256($ff xor Dest[0]) * ($ff xor Src[0])) shr 7);
  if Dest[1] < $80 then bc[1] := Min($ff, (Alpha255to256(Dest[1]) * Src[1]) shr 7) else
    bc[1] := Max(0, $ff - (Alpha255to256($ff xor Dest[1]) * ($ff xor Src[1])) shr 7);
  if Dest[2] < $80 then bc[2] := Min($ff, (Alpha255to256(Dest[2]) * Src[2]) shr 7) else
    bc[2] := Max(0, $ff - (Alpha255to256($ff xor Dest[2]) * ($ff xor Src[2])) shr 7);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

var
  SoftLight_DFunction: array[0..255] of word; // 0..256 values

procedure bfSoftLight;
var
  i: integer;
  bc: packed array[0..3] of byte;

begin
  for i := 0 to 2 do
  if Src[i] < $80 then
    bc[i] := Dest[i] - (Alpha255to256($ff xor (Src[i] shl 1)) *
      Alpha255to256($ff xor Dest[i]) * Dest[i]) shr 16 else
    bc[i] := Dest[i] + ( ((Integer(Src[i]) shl 1) - $ff) *
      SoftLight_DFunction[Dest[i]] ) shr 8;

  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfHardLight;
var
  bc: packed array[0..3] of byte;

begin
  if Src[0] < $80 then bc[0] := Min($ff, (Alpha255to256(Src[0]) * Dest[0]) shr 7) else
    bc[0] := Max(0, $ff - (Alpha255to256($ff xor Src[0]) * ($ff xor Dest[0])) shr 7);
  if Src[1] < $80 then bc[1] := Min($ff, (Alpha255to256(Src[1]) * Dest[1]) shr 7) else
    bc[1] := Max(0, $ff - (Alpha255to256($ff xor Src[1]) * ($ff xor Dest[1])) shr 7);
  if Src[2] < $80 then bc[2] := Min($ff, (Alpha255to256(Src[2]) * Dest[2]) shr 7) else
    bc[2] := Max(0, $ff - (Alpha255to256($ff xor Src[2]) * ($ff xor Dest[2])) shr 7);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfVividLight;
var
  i: integer;
  bc: packed array[0..3] of byte;

begin
  for i := 0 to 2 do
  if Src[i] < $80 then
  begin
    // use some color burn
    if $ff xor Dest[i] >= Integer(Src[i]) shl 1 then
      bc[i] := 0 else
      bc[i] := $ff xor (( (Integer($ff xor Dest[i]) shl 8) div Alpha255to256(Src[i]) ) shr 1);
  end else
  begin
    // use some color dodge
    if Dest[i] >= Integer($ff xor Src[i]) shl 1 then
      bc[i] := $ff else
      bc[i] := ( (Integer(Dest[i]) shl 8) div Alpha255to256($ff xor Src[i]) ) shr 1;
  end;

  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfLinearLight;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := Max(0, Min($ff, 2*Integer(Src[0]) + Dest[0] - $ff));
  bc[1] := Max(0, Min($ff, 2*Integer(Src[1]) + Dest[1] - $ff));
  bc[2] := Max(0, Min($ff, 2*Integer(Src[2]) + Dest[2] - $ff));
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfPinLight;
var
  bc: packed array[0..3] of byte;

begin
  if Src[0] < $80 then bc[0] := Min(Dest[0], Src[0] shl 1) else
    bc[0] := Max(Dest[0], Byte(Src[0] shl 1));
  if Src[1] < $80 then bc[1] := Min(Dest[1], Src[1] shl 1) else
    bc[1] := Max(Dest[1], Byte(Src[1] shl 1));
  if Src[2] < $80 then bc[2] := Min(Dest[2], Src[2] shl 1) else
    bc[2] := Max(Dest[2], Byte(Src[2] shl 1));
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfHardMix;
var
  bc: packed array[0..3] of byte;

begin
  if Src[0] > $ff xor Dest[0] then bc[0] := $ff else bc[0] := 0;
  if Src[1] > $ff xor Dest[1] then bc[1] := $ff else bc[1] := 0;
  if Src[2] > $ff xor Dest[2] then bc[2] := $ff else bc[2] := 0;
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

procedure bfDifference;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := Abs(Integer(Src[0]) - Dest[0]);
  bc[1] := Abs(Integer(Src[1]) - Dest[1]);
  bc[2] := Abs(Integer(Src[2]) - Dest[2]);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;
 
procedure bfExclusion;
var
  bc: packed array[0..3] of byte;

begin
  bc[0] := Min($ff, Integer(Src[0]) + Dest[0] - (Integer(Src[0]) * Dest[0]) shr 7);
  bc[1] := Min($ff, Integer(Src[1]) + Dest[1] - (Integer(Src[1]) * Dest[1]) shr 7);
  bc[2] := Min($ff, Integer(Src[2]) + Dest[2] - (Integer(Src[2]) * Dest[2]) shr 7);
  bc[3] := Src[3];

  // produce blending color and do blending
  PColor32(Dest)^ := PutPixel32(BlendColors(PColor32(Src)^, TColor32(bc), Dest[3]), PColor32(Dest)^);
end;

var
  DissolveTable: array[byte] of byte; // random values

procedure DrawBlended(Dest: TBitmap32; rClip: TRect;
  Src: TBitmap32; Opacity: integer;
  BlendMode: TBlendMode; DestIsClear: PBoolean);
var
  x, y: integer;
  c: packed array[0..3] of byte;
  pSrc, pDest: PColor32;
  BlendFunc: TBlendFunc;

begin
  if Opacity = 0 then
  begin
    if BlendMode <> bmMask then Exit;

    // mask with 0 opacity -> clear image
    if Assigned(DestIsClear) and DestIsClear^ then Exit;
    Dest.Rectangle(Rect(0, 0, rClip.Width, rClip.Height), cl32Transparent, True, 1);
    if Assigned(DestIsClear) then DestIsClear^ := True;
    Exit;
  end;

  rClip.Left := Max(0, rClip.Left);
  rClip.Top := Max(0, rClip.Top);
  rClip.Right := Min3i(rClip.Left + Dest.Width, Src.Width, rClip.Right);
  rClip.Bottom := Min3i(rClip.Top + Dest.Height, Src.Height, rClip.Bottom);

  if (rClip.Left >= rClip.Right) or (rClip.Top >= rClip.Bottom) then Exit;

  // dest is clear?
  if Assigned(DestIsClear) and DestIsClear^ then
  begin
    // these blend modes do not do anything here
    if BlendMode in [bmMask, bmHue, bmHueShift, bmSaturation] then Exit;

    // not a mask -> draw
    DestIsClear^ := False;
    if (Opacity = 255) and (BlendMode <> bmDissolve) then
    begin
      Dest.CopyRectOverwrite(0, 0, Src, rClip);
      Exit;
    end;
  end;

  if (Opacity = 255) and (BlendMode = bmNormal) then
  begin
    Dest.CopyRect(0, 0, Src, rClip);
    Exit;
  end;

  Opacity := Alpha255to256(Opacity);
  BlendFunc := BlendFunctions[BlendMode];

  for y := rClip.Top to rClip.Bottom - 1 do
  begin
    pSrc := Src.PixelAddr(rClip.Left, y);
    pDest := Dest.PixelAddr(0, y - rClip.Top);

    for x := rClip.Left to rClip.Right - 1 do
    begin
      // use Opacity
      TColor32(c) := pSrc^;
      if TColor32(c) = cl32Inverted then
      begin
        if Opacity < $80 then TColor32(c) := cl32Transparent;
      end else
      begin
        c[3] := (Opacity * c[3]) shr 8;
        if c[3] = 0 then TColor32(c) := cl32Transparent;
      end;

      // blend
      if BlendMode = bmDissolve then
      begin
        if (c[3] = $ff) then pDest^ := TColor32(c) else
        // compare pixel value to a coordinate-dependent random value
        if (c[3] <> 0) and (DissolveTable[DissolveTable[Byte(x)] xor Byte(y)] < c[3]) then
          pDest^ := TColor32(c) or cl32Opaque;
      end else
        BlendFunc(@c, PByteArray(pDest));

      // iterate
      inc(pSrc);
      inc(pDest);
    end; // for x
  end; // for y
end;

procedure EraseWith;
var
  x, y, xMin, xMax, a: integer;
  pSrc, pDest: PByteArray;

begin
  Opacity := Alpha255to256(Opacity);

  xMin := Max(0, r.Left); xMax := Min3i(Src.Width, Dest.Width, r.Right) - 1;
  for y := Max(0, r.Top) to Min3i(Src.Height, Dest.Height, r.Bottom) - 1 do
  begin
    pSrc := PByteArray(Src.PixelAddr(xMin, y));
    pDest := PByteArray(Dest.PixelAddr(xMin, y));

    for x := xMin to xMax do
    begin
      a := $ff xor ((pSrc[3] * Opacity) shr 8);
      if PColor32(pDest)^ = cl32Inverted then
      begin
        if a < $80 then PColor32(pDest)^ := cl32Transparent;
      end else
      begin
        pDest[3] := (pDest[3] * Alpha255to256(a)) shr 8;
        if pDest[3] = 0 then PColor32(pDest)^ := cl32Transparent;
      end;

      // iterate
      inc(PColor32(pSrc));
      inc(PColor32(pDest));
    end;
  end;
end;

///////////////////////////////////////////////
procedure InitBlendModes;
var
  i: integer;
  x: double;

begin
  // Dissolve blend mode hash table
  for i := 0 to $ff do DissolveTable[i] := Random($100);

  // Calculate D(x) - x for all channel values
  // This is used by the soft light blend mode
  for i := 0 to $ff do
  begin
    x := i / $ff;
    if x <= 0.25 then
      SoftLight_DFunction[i] := Round( ((16*x - 12)*x + 3)*x * $100 ) else
      SoftLight_DFunction[i] := Round( (Sqrt(x) - x) * $100 );
  end;
end;

initialization
  InitBlendModes;
end.
