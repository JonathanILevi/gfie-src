(*
	Greenfish Controls - Color space conversion
	Copyright © 2013 Balázs Szalkai
	This file is released under the zlib license:

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	   1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgment in the product documentation would be
	   appreciated but is not required.

	   2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.

	   3. This notice may not be removed or altered from any source
	   distribution.
*)
unit ColSpaces;

interface

uses
  {$IFNDEF LCL} Windows, Messages, {$ELSE} LclIntf, LclType, {$ENDIF}
  Math, Graphics, FastDiv;

// The code assumes at several places that HSBMAX = RGBMAX*2
const
  // Change the DivByRGBMAX procedure if this const is changed
  RGBMAX = 255;
  // HSBMAX must be divisible by 6
  // Change the DivByHSBMAX procedure if this const is changed
  HSBMAX = 510;
  XYZMAX = 500;

type
  TInt3 = array[0..2] of integer;

// Type conversion
function Int3toColor(const i: TInt3): TColor;

// Color space: HSB (Hue-Saturation-Brightness)
function RGBtoHSB(r, g, b: integer): TInt3;
function HSBtoRGB(Hue, Sat, Bri: integer): TInt3;

// Color space: XYZ (CIE 1931, Observer = 2°, Illuminant = D65 (daylight))
function RGBtoXYZ(r, g, b: integer): TInt3;
function XYZtoRGB(x, y, z: integer): TInt3;

implementation

function Int3toColor;
begin
  Result := i[0] + (i[1] shl 8) + (i[2] shl 16);
end;

// This function must be changed if HSBMAX changes
// Note: does not work for negative numbers
function DivByHSBMAX(x: integer): integer; inline;
begin
  Result := Integer(DivBy510(Cardinal(x)));
end;

// This function must be changed if RGBMAX changes
// Note: does not work for negative numbers
function DivBy2RGBMAX(x: integer): integer; inline;
begin
  Result := Integer(DivBy510(Cardinal(x)));
end;

// This function must be changed if HSBMAX changes
// Note: does not work for negative numbers
function DivByHSBMAXDiv6(x: integer): integer; inline;
begin
  Result := Integer(DivBy85(Cardinal(x)));
end;

function RGBtoHSB;
var
  cMax, cMin, Rdelta, Gdelta, Bdelta: integer;
  i: integer;
  DivBy_cMax_cMin: TDivFunc;

begin
  cMax := Max(Max(r, g), b);
  cMin := Min(Min(r, g), b);
  Result[2] := DivBy2RGBMAX((cMax+cMin)*HSBMAX + RGBMAX);

  if cMin = cMax then
  begin
    Result[1] := 0;
    Result[0] := 0;
  end else
  begin
    if Result[2] <= HSBMAX div 2 then
      Result[1] := ((cMax-cMin)*HSBMAX + (cMax+cMin) div 2) div (cMax+cMin)
    else begin
      i := 2*RGBMAX-cMax-cMin;
      Result[1] := ((cMax-cMin)*HSBMAX + i div 2) div i;
    end;

    // cMax-cMin is in the interval 1..255, so we can rely on FastDiv
    DivBy_cMax_cMin := DivFunc[cMax-cMin];
    Rdelta := DivBy_cMax_cMin((cMax-R)*(HSBMAX div 6) + (cMax-cMin) div 2);
    Gdelta := DivBy_cMax_cMin((cMax-G)*(HSBMAX div 6) + (cMax-cMin) div 2);
    Bdelta := DivBy_cMax_cMin((cMax-B)*(HSBMAX div 6) + (cMax-cMin) div 2);

    if R = cMax then Result[0] := Bdelta - Gdelta else
    if G = cMax then Result[0] := HSBMAX div 3 + Rdelta - Bdelta else
    // B == cMax
      Result[0] := 2*HSBMAX div 3 + Gdelta - Rdelta;

    if Result[0] < 0 then inc(Result[0], HSBMAX) else
    if Result[0] > HSBMAX then dec(Result[0], HSBMAX);
  end;
end;

function HueToRGB(n1, n2, hue: integer): integer;
begin
  if hue < 0 then inc(hue, HSBMAX) else
  if hue > HSBMAX then dec(hue, HSBMAX);

  if hue < HSBMAX div 6 then
    Result := n1 + DivByHSBMAXDiv6((n2-n1)*hue + HSBMAX div 12) else
  if hue < HSBMAX div 2 then
    Result := n2 else
  if hue < 2*HSBMAX div 3 then
    Result := n1 + DivByHSBMAXDiv6((n2-n1)*(2*HSBMAX div 3-hue) + HSBMAX div 12) else
    Result := n1;
end;

function HSBtoRGB;
var
  n1, n2: integer;

begin
  if Sat = 0 then
  begin
    Result[0] := Bri div 2;
    Result[1] := Result[0];
    Result[2] := Result[0];
  end else
  begin
    if Bri <= HSBMAX div 2 then
       n2 := Bri + DivByHSBMAX(Bri*Sat + HSBMAX div 2) else
       n2 := Bri + Sat - DivByHSBMAX(Bri*Sat + HSBMAX div 2);

    n1 := 2*Bri - n2;

    Result[0] := (HueToRGB(n1, n2, Hue + HSBMAX div 3) + 1) shr 1;
    Result[1] := (HueToRGB(n1, n2, Hue) + 1) shr 1;
    Result[2] := (HueToRGB(n1, n2, Hue - HSBMAX div 3) + 1) shr 1;
  end;
end;

function RGBtoXYZ;
const
  OneDivRGBMAX = 1 / RGBMAX;
var
  i: integer;
  q: array[0..2] of double;

begin
  q[0] := r; q[1] := g; q[2] := b;

  for i := 0 to 2 do
  begin
    q[i] := q[i] * OneDivRGBMAX;

    if q[i] > 0.04045 then q[i] := Exp(Ln((q[i] + 0.055) * 0.94787) * 2.4) else
      q[i] := q[i] * 0.077399;
  end;

  Result[0] := Round((q[0] * 0.4124 + q[1] * 0.3576 + q[2] * 0.1805) * XYZMAX);
  Result[1] := Round((q[0] * 0.2126 + q[1] * 0.7152 + q[2] * 0.0722) * XYZMAX);
  Result[2] := Round((q[0] * 0.0193 + q[1] * 0.1192 + q[2] * 0.9505) * XYZMAX);
end;

function XYZtoRGB;
const
  OneDivXYZMAX = 1 / XYZMAX;
var
  i: integer;
  q: array[0..2] of double;

begin
  q[0] := (x * 3.2406 + y * -1.5372 + z * -0.4986) * OneDivXYZMAX;
  q[1] := (x * -0.9689 + y * 1.8758 + z * 0.0415) * OneDivXYZMAX;
  q[2] := (x * 0.0557 + y * -0.2040 + z * 1.0570) * OneDivXYZMAX;

  for i := 0 to 2 do
  begin
    if q[i] > 0.0031308 then
      q[i] := 1.055 * Exp(Ln(q[i]) * 0.4166667) - 0.055 else
      q[i] := 12.92 * q[i];

    Result[i] := Max(0, Min(RGBMAX, Round(q[i] * RGBMAX)));
  end;
end;

end.
