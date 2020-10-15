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
unit Filters;

interface

uses
  LclIntf, LclType,
  SysUtils, Types, Graphics, Math, BitmapEx, bmExUtils, ColSpaces, gfMath,
  FloatFormula;

type
  TRetouchMode = (rtmBlur, rtmSharpen, rtmDodge, rtmBurn, rtmHue, rtmSponge);
  TfltSimpleFilter = (fsfGrayscale, fsfInvert, fsfSolarize);

// Returns bounding box
function fltRetouch(bm: TBitmap32; Mask: TBitmap1; x1, y1, x2, y2, BrushSize: integer;
  BrushShape: TBrushShape; RetouchMode: TRetouchMode): TRect;
procedure fltRecolor(bm, Mask: TBitmap32; const r: TRect; cSrc, cDest: TColor32;
  Tolerance: integer);

// Mask can be nil
procedure fltSimple(bm: TBitmap32; Mask: TBitmap1; Filter: TfltSimpleFilter);
procedure fltRGBChannels(bm: TBitmap32; Mask: TBitmap1; Red, Green, Blue: double);
function fltChannelFormula(bm: TBitmap32; Mask: TBitmap1;
  const fRed, fGreen, fBlue, fAlpha: string;
  const rangeGray, rangeScale: double): boolean;
procedure fltHueSaturation(bm: TBitmap32; Mask: TBitmap1; rClip: TRect;
  HueShift: integer; Sat: double);
procedure fltExposure(bm: TBitmap32; Mask: TBitmap1; rClip: TRect;
  Gamma, Brightness, Contrast: double);

procedure fltAverage(bm: TBitmap32; Mask: TBitmap1);
procedure fltGaussianBlur(bm: TBitmap32; Mask: TBitmap1;
  Radius: double; Toric: boolean; OnlyAlpha: boolean);
// Shift is used when doing blur with even radius
procedure fltBoxBlur(bm: TBitmap32; Mask: TBitmap1; Size: integer;
  Shift, Toric: boolean; OnlyAlpha: boolean);

// if abs(normal-blurred) >= threshold then
// inc(normal, (blurred-normal)*Amount div 256
procedure fltUnsharpMask(bm: TBitmap32; Mask: TBitmap1; Amount: integer;
  Radius: double; Threshold: integer; Toric: boolean);

procedure fltRemoveMatte(bm: TBitmap32; Mask: TBitmap1; Matte: TColor);
// Alpha is scaled by 256
procedure fltOpacity(bm: TBitmap32; Mask: TBitmap1; Alpha: integer);

procedure fltPaintContour(bm: TBitmap32; Mask: TBitmap1; Threshold: integer;
  BrushSize: integer; BrushShape: TBrushShape; Color: TColor32; Toric: boolean);
procedure fltShadow(bm: TBitmap32; Mask: TBitmap1; dx, dy: integer;
  Blur: double; Color: TColor; Alpha: integer; Toric: boolean);
// Strength will be divided by 256
procedure fltGlow(bm: TBitmap32; Mask: TBitmap1; InnerGlow, OuterGlow: boolean;
  Radius: double; Color: TColor; Strength: integer; Toric: boolean);
procedure fltBevel(bm: TBitmap32; Mask: TBitmap1; dx, dy: integer;
  Blur: double; Alpha: integer; Toric: boolean);

implementation

uses
  Dialogs;

function fltRetouch;
var
  pt: TDynPointArray;
  BrushMask: TBitmap1;

begin
  // create the brush mask
  BrushMask := TBitmap1.Create;
  try
    BrushMask.Resize(bm.Width, bm.Height);

    EnumLinePoints(x1, y1, x2, y2, pt);
    Result := BrushMask.CreateBrushMask(Length(pt), @pt[0], BrushSize, BrushShape);
    if Assigned(Mask) then
      BrushMask.Combine(Mask, bcmAnd);

    case RetouchMode of
      rtmBlur: fltGaussianBlur(bm, BrushMask, 0.5, False, False);
      rtmSharpen: fltUnsharpMask(bm, BrushMask, 100, 0.5, 2, False);
      rtmDodge: fltExposure(bm, BrushMask, Result, 1.06, 0, 1);
      rtmBurn: fltExposure(bm, BrushMask, Result, 1 / 1.06, 0, 1);
      rtmHue: fltHueSaturation(bm, BrushMask, Result, Round(HSBMAX * 0.02), 0);
      rtmSponge: fltHueSaturation(bm, BrushMask, Result, 0, -0.1);
    end;
  finally
    BrushMask.Free;
  end;
end;

procedure fltRecolor;
var
  x, y, xMin, xMax: integer;
  p: PColor32;
  pMask: PByte;

begin
  xMin := Max(0, r.Left);
  xMax := Min(bm.Width, r.Right) - 1;
  for y := Max(0, r.Top) to Min(bm.Height, r.Bottom) - 1 do
  begin
    p := bm.PixelAddr(xMin, y);
    pMask := @PByteArray(Mask.PixelAddr(xMin, y))[3];

    for x := xMin to xMax do
    begin
      if (pMask^ <> 0) and CanFloodFill(p^, cSrc, Tolerance) then
        p^ := BlendColors(p^, cDest, pMask^);

      Inc(p);
      Inc(pMask, 4);
    end;
  end;
end;

procedure fltSimple;
var
  x, y: integer;
  Gray: byte;
  p: PByteArray;

begin
  p := bm.Data;
  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
    begin
      if (not Assigned(Mask) or Mask[x, y]) and (PColor32(p)^ and cl32Opaque <> 0) then
        // unmasked pixel
        case Filter of
          fsfGrayscale:
          begin
            Gray := byte((GRAY_COEFF_RED * integer(p[0]) + GRAY_COEFF_GREEN *
              integer(p[1]) + GRAY_COEFF_BLUE * integer(p[2])) shr 16);
            p[0] := Gray;
            p[1] := Gray;
            p[2] := Gray;
          end;

          fsfInvert: PColor32(p)^ := PColor32(p)^ xor $ffffff;

          fsfSolarize: begin
            if p[0] and $80 <> 0 then
              p[0] := p[0] xor $ff;
            if p[1] and $80 <> 0 then
              p[1] := p[1] xor $ff;
            if p[2] and $80 <> 0 then
              p[2] := p[2] xor $ff;
          end;
        end; // case Filter

      // iterate
      Inc(PColor32(p));
    end;
end;

procedure fltRGBChannels;
var
  i, j, x, y: integer;
  q: double;
  Mapping: array[0..2, 0..$ff] of byte;
  p: PByteArray;

begin
  for j := 0 to 2 do
  begin
    case j of
      0: q := Red;
      1: q := Green;
      else
        q := Blue;
    end;

    for i := 0 to $ff do
      if q < 0 then
        Mapping[j, i] := Round(i * (q + 1))
      else
        Mapping[j, i] := $ff - Round(($ff - i) * (1 - q));
  end;

  p := bm.Data;
  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
    begin
      if (not Assigned(Mask) or Mask[x, y]) and (p[3] <> 0) then
      begin
        p[0] := Mapping[0, p[0]];
        p[1] := Mapping[1, p[1]];
        p[2] := Mapping[2, p[2]];
      end;

      Inc(PColor32(p));
    end; // for y, x
end;

function fltChannelFormula;
const
  OneDivFF = 1 / $ff;
  StackSize = 32;

var
  i, x, y: integer;
  cwOld: word;
  ChangeAlpha: boolean;
  p: PByteArray;
  sa: TffStrArray;
  ff: array[0..3] of TFloatFormula;
  q: array[0..3] of double;
  error: array[0..3] of TffError;
  Stack: array[0..StackSize-1] of double;
  ScaleForward, OffsetForward, ScaleBackward, OffsetBackward: double;

begin
  Result := False;

  for i := 0 to 3 do
    ff[i] := TFloatFormula.Create;
  try
    // compile formulae
    SetLength(sa, 4);
    sa[0] := 'r';
    sa[1] := 'g';
    sa[2] := 'b';
    sa[3] := 'a';
    ChangeAlpha := fAlpha <> 'a';
    error[0] := ff[0].MakeFromString(fRed, sa, StackSize);
    error[1] := ff[1].MakeFromString(fGreen, sa, StackSize);
    error[2] := ff[2].MakeFromString(fBlue, sa, StackSize);
    error[3] := ff[3].MakeFromString(fAlpha, sa, StackSize);
    if (error[0] <> ffeNone) or (error[1] <> ffeNone) or (error[2] <> ffeNone) or (error[3] <> ffeNone) then
    begin
      Exit;
    end;

    // only syntax check?
    if bm = nil then
    begin
      Result := True;
      Exit;
    end;

    ScaleForward := rangeScale;
    OffsetForward := rangeGray - 128.0*rangeScale;
    ScaleBackward := 1.0 / rangeScale;
    OffsetBackward := 128.0 - rangeGray / rangeScale;

    // avoid exceptions
    cwOld := Get8087CW;
    Set8087CW($133f);
    try
      p := bm.Data;
      for y := 0 to bm.Height - 1 do
        for x := 0 to bm.Width - 1 do
        begin
          if (not Assigned(Mask) or Mask[x, y]) and ((p[3] <> 0) or ChangeAlpha) then
          begin
            for i := 0 to 3 do
              q[i] := p[i] * ScaleForward + OffsetForward;
            for i := 0 to 3 do
            begin
              ff[i].Invoke(q, Stack);
              p[i] := Round(Max(0, Min($ff, Stack[0] * ScaleBackward + OffsetBackward)));
            end;
          end;

          Inc(PColor32(p));
        end; // for y, x
    finally
      Set8087CW(cwOld);
    end;
  finally
    for i := 0 to 3 do
      ff[i].Free;
  end;
end;

procedure fltHueSaturation;
var
  i, x, y: integer;
  SatMapping: array[0..HSBMAX] of integer;
  p: PByteArray;
  int3: TInt3;

begin
  if not IntersectRect(rClip, rClip, bm.ClientRect) then
    Exit;

  SatMapping[0] := 0;
  for i := 1 to HSBMAX do
    SatMapping[i] := Min(HSBMAX, Round(i * (Sat + 1)));

  for y := rClip.Top to rClip.Bottom - 1 do
  begin
    PColor32(p) := bm.PixelAddr(rClip.Left, y);
    for x := rClip.Left to rClip.Right - 1 do
    begin
      if (not Assigned(Mask) or Mask[x, y]) and (p[3] <> 0) then
      begin
        int3 := RGBtoHSB(p[0], p[1], p[2]);
        int3 := HSBtoRGB(int3[0] + HueShift, SatMapping[int3[1]], int3[2]);
        for i := 0 to 2 do
          p[i] := int3[i];
      end;

      Inc(PColor32(p));
    end; // for x
  end; // for y
end;

procedure fltExposure;
var
  i, x, y: integer;
  Mapping: array[0..$ff] of byte;
  p: PByteArray;

begin
  if not IntersectRect(rClip, rClip, bm.ClientRect) then
    Exit;

  if Gamma = 0 then
  begin
    for i := 0 to $ff do
      Mapping[i] := 0;
  end
  else
  begin
    Gamma := 1 / Gamma;
    for i := 0 to $ff do
    begin
      Mapping[i] := i;

      if i = 0 then
        Mapping[i] := 0
      else
        Mapping[i] := Round(Exp(Ln(Mapping[i] / $ff) * Gamma) * $ff);

      Mapping[i] := Max(0, Min($ff, Round(Contrast * (integer(Mapping[i]) - $80) + $80)));

      if Brightness < 0 then
        Mapping[i] := Round(Mapping[i] * (Brightness + 1))
      else
        Mapping[i] := $ff - Round(($ff - Mapping[i]) * (1 - Brightness));
    end; // for i
  end; // if Gamma

  for y := rClip.Top to rClip.Bottom - 1 do
  begin
    PColor32(p) := bm.PixelAddr(rClip.Left, y);
    for x := rClip.Left to rClip.Right - 1 do
    begin
      if (not Assigned(Mask) or Mask[x, y]) and (p[3] <> 0) then
      begin
        p[0] := Mapping[p[0]];
        p[1] := Mapping[p[1]];
        p[2] := Mapping[p[2]];
      end;

      Inc(PColor32(p));
    end; // for x
  end; // for y
end;

procedure fltAverage;
var
  x, y: integer;
  p: PColor32;
  c: TColor32;
  avg: TBigColor32Average;

begin
  avg := TBigColor32Average.Create;
  try
    p := bm.Data;
    for y := 0 to bm.Height - 1 do
      for x := 0 to bm.Width - 1 do
      begin
        if not Assigned(Mask) or Mask[x, y] then
          avg.Add(p^);

        // iterate
        Inc(p);
      end;

    c := avg.GetAverage;
  finally
    avg.Free;
  end;

  // fill the area with the average color
  p := bm.Data;
  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
    begin
      if not Assigned(Mask) or Mask[x, y] then
        p^ := c;

      // iterate
      Inc(p);
    end; // for x
end;

procedure fltExactGaussianBlur(bm: TBitmap32; Mask: TBitmap1;
  Radius: double; Toric: boolean; OnlyAlpha: boolean);
var
  bmOrig: TBitmap32;
  i, x, y, kLen: integer;
  HorizontalBlur: boolean;
  Kernel: array of double;
  WeightSum, q: double;
  sum: array[0..3] of double;
  pDest: PByteArray;

  procedure AddPixel(x, y, r: integer);
  var
    p: PByteArray;
    w: double;

  begin
    w := Kernel[r];
    WeightSum := WeightSum + w;
    p := PByteArray(bmOrig.PixelAddr(x, y));
    // cl32Inverted does not need to be examined separately
    if p[3] <> 0 then
    begin
      if not OnlyAlpha then
      begin
        sum[0] += p[0] * p[3] * w;
        sum[1] += p[1] * p[3] * w;
        sum[2] += p[2] * p[3] * w;
      end;
      sum[3] += p[3] * w;
    end;
  end;

begin
  if (bm.Width = 0) or (bm.Height = 0) or (Radius < 1e-2) then
    Exit;

  bmOrig := TBitmap32.Create;
  try
    // create blur kernel
    kLen := 1 + Ceil(Radius * 3);
    SetLength(Kernel, kLen);
    for i := 0 to kLen - 1 do
      Kernel[i] :=
        Exp(-Sqr(i) / (2 * Sqr(Radius))) / (2 * pi * Sqr(Radius));

    // Do horizontal and vertical blur
    // Only horizontal stripes of pixels are averaged first
    for HorizontalBlur := False to True do
    begin
      // save original bitmap
      bmOrig.Assign(bm);
      for y := 0 to bm.Height - 1 do
        for x := 0 to bm.Width - 1 do
        begin
          for i := 0 to 3 do
            sum[i] := 0;
          WeightSum := 0;

          // average horizontal / vertical pixels
          if Toric then
          begin
            if HorizontalBlur then
              for i := x - kLen + 1 to x + kLen - 1 do
                AddPixel(SignedMod(i, bm.Width), y, Abs(i - x))
            else
              for i := y - kLen + 1 to y + kLen - 1 do
                AddPixel(x, SignedMod(i, bm.Width), Abs(i - y));
          end
          else
          begin
            if HorizontalBlur then
              for i := Max(0, x - kLen + 1) to Min(x + kLen, bm.Width) - 1 do
                AddPixel(i, y, Abs(i - x))
            else
              for i := Max(0, y - kLen + 1) to Min(y + kLen, bm.Height) - 1 do
                AddPixel(x, i, Abs(i - y));
          end;

          // replace pixel with average
          pDest := PByteArray(bm.PixelAddr(x, y));

          if (PColor32(pDest)^ <> cl32Inverted) and
            (not Assigned(Mask) or Mask[x, y]) then
            if sum[3] = 0 then
              PColor32(pDest)^ := cl32Transparent
            else
            if OnlyAlpha then
              PColor32(pDest)^ := Round(sum[3] / WeightSum) shl 24
            else
            begin
              pDest[3] := Round(sum[3] / WeightSum);
              // avoid getting cl32Inverted
              if pDest[3] = 0 then
                PColor32(pDest)^ := cl32Transparent
              else
              begin
                // calculate RGB values
                q := 1 / sum[3];
                pDest[0] := Round(sum[0] * q);
                pDest[1] := Round(sum[1] * q);
                pDest[2] := Round(sum[2] * q);
              end;
            end; // can replace pixel

        end; // for y
    end; // for b
  finally
    bmOrig.Free;
  end;
end;

type
  PBoxBlurStep = ^TBoxBlurStep;
  TBoxBlurStep = record
    Size: integer;
    Shift, Toric: boolean;
  end;

// Define a box blur function
{$define boxBlurFunc:=fltBoxBlurInternal_Color}
{$define TBoxBlurColorAverage:=TSmallColor32Average}
{$i BoxBlurInternal.inc}

// Define a box blur function which does not take Alpha into account
{$define boxBlurFunc:=fltBoxBlurInternal_OnlyAlpha}
{$define TBoxBlurColorAverage:=TSmallAlphaAverage}
{$i BoxBlurInternal.inc}

// At large radiuses, gaussian blur is approximated using box blurs
// to improve performance
procedure fltApproxGaussianBlur(bm: TBitmap32; Mask: TBitmap1;
  Radius: double; _Toric, OnlyAlpha: boolean);
var
  d: integer;
  Steps: array[0..2] of TBoxBlurStep;

begin
  d := Round(Radius * 1.88);

  with Steps[0] do begin Size := d; Shift := False; Toric := _Toric; end;
  with Steps[1] do begin Size := d; Shift := True; Toric := _Toric; end;
  if not Odd(d) then Inc(d);
  with Steps[2] do begin Size := d; Shift := False; Toric := _Toric; end;

  if OnlyAlpha then
    fltBoxBlurInternal_OnlyAlpha(bm, Mask, 3, @Steps[0]) else
    fltBoxBlurInternal_Color(bm, Mask, 3, @Steps[0]);
end;

procedure fltGaussianBlur;
begin
  if Radius <= 0 then
    Exit;

  if Radius < 3 then
    fltExactGaussianBlur(bm, Mask, Radius, Toric, OnlyAlpha)
  else
    fltApproxGaussianBlur(bm, Mask, Radius, Toric, OnlyAlpha);
end;

procedure fltBoxBlur;
var
  Step: TBoxBlurStep;

begin
  Step.Size := Size;
  Step.Shift := Shift;
  Step.Toric := Toric;

  if OnlyAlpha then
    fltBoxBlurInternal_OnlyAlpha(bm, Mask, 1, @Step) else
    fltBoxBlurInternal_Color(bm, Mask, 1, @Step);
end;

procedure fltUnsharpMask;
var
  i, j: integer;
  bmBlur: TBitmap32;
  p, pBlur: PByteArray;

begin
  bmBlur := TBitmap32.Create;
  try
    // render the blurred image
    bmBlur.Assign(bm);
    fltGaussianBlur(bmBlur, Mask, Radius, Toric, False);

    p := bm.Data;
    pBlur := bmBlur.Data;
    for i := 1 to bm.Width * bm.Height do
    begin
      if p[3] <> 0 then
        for j := 0 to 2 do
          if Abs(p[j] - pBlur[j]) >= Threshold then
            p[j] := Max(0, Min($ff, p[j] +
              (Amount * (p[j] - pBlur[j])) div 256));

      // iterate
      Inc(PColor32(p));
      Inc(PColor32(pBlur));
    end;
  finally
    bmBlur.Free;
  end;
end;

procedure fltRemoveMatte;
var
  i, x, y: integer;
  Alpha: double;
  p: PByteArray;
  m: array[0..2] of byte absolute Matte;

begin
  p := bm.Data;
  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
    begin
      // leave transparent pixels untouched
      if (not Assigned(Mask) or Mask[x, y]) and (p[3] <> 0) then
      begin
        Alpha := 0;
        for i := 0 to 2 do
          if p[i] <> m[i] then
            if p[i] > m[i] then
              Alpha := Max(Alpha, (p[i] - m[i]) / (m[i] xor $ff))
            else
              Alpha := Max(Alpha, (m[i] - p[i]) / m[i]);

        if Alpha = 0 then
          PColor32(p)^ := cl32Transparent
        else
        begin
          p[3] := Round(p[3] * Alpha);
          for i := 0 to 2 do
            p[i] := Round((p[i] - m[i]) / Alpha) + m[i];
        end;
      end; // non-transparent pixel

      Inc(PColor32(p));
    end; // for y, x
end;

procedure fltOpacity;
var
  i, x, y: integer;
  AlphaMap: array[0..255] of byte;
  p: PByteArray;

begin
  p := bm.Data;

  for i := 0 to 255 do
    AlphaMap[i] := Min($ff, (i * Alpha) shr 8);

  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
    begin
      if (not Assigned(Mask) or Mask[x, y]) and (p[3] <> 0) then
      begin
        p[3] := AlphaMap[p[3]];
        if p[3] = 0 then
          PColor32(p)^ := cl32Transparent;
      end;

      Inc(PColor32(p));
    end; // for y, x
end;

procedure fltPaintContour;
var
  x, y, Count: integer;
  Points: array of TPoint;
  IsContour: boolean;

  function PixelAlpha(x, y: integer): byte;
  begin
    Result := PByteArray(bm.Data)[(y * bm.Width + x) * 4 + 3];
  end;

begin
  // determine which pixels belong to the contour
  Count := 0;
  SetLength(Points, 0);
  for y := 0 to bm.Height - 1 do
    for x := 0 to bm.Width - 1 do
      if PixelAlpha(x, y) >= Threshold then
      begin
        if Toric then
          IsContour := (PixelAlpha(SignedMod(x - 1, bm.Width), y) < Threshold) or
            (PixelAlpha(SignedMod(x + 1, bm.Width), y) < Threshold) or
            (PixelAlpha(x, SignedMod(y - 1, bm.Height)) < Threshold) or
            (PixelAlpha(x, SignedMod(y + 1, bm.Height)) < Threshold)
        else
          IsContour := (x = 0) or (PixelAlpha(x - 1, y) < Threshold) or
            (x = bm.Width - 1) or (PixelAlpha(x + 1, y) < Threshold) or
            (y = 0) or (PixelAlpha(x, y - 1) < Threshold) or (y = bm.Height - 1) or
            (PixelAlpha(x, y + 1) < Threshold);

        if not IsContour then
          Continue;

        Inc(Count);
        if Length(Points) < Count then
          SetLength(Points, Count or $ff);
        Points[Count - 1] := Point(x, y);
      end;

  bm.StrokeBrush(Count, @Points[0], BrushSize, BrushShape,
    Color, Mask);
end;

// Returns margin
function fltAuxGenerateBlurred(bm: TBitmap32; var bmShade: TBitmap32; Radius: double;
  Toric: boolean): integer;
begin
  bmShade := TBitmap32.Create;

  if Toric then
  begin
    // no margin needed
    Result := 0;
    bmShade.Assign(bm);
  end
  else
  begin
    // we have to leave a margin
    Result := Ceil(Radius * 3);

    bmShade.Resize(bm.Width + 2 * Result, bm.Height + 2 * Result);
    bmShade.CopyRectOverwrite(Result, Result, bm, bm.ClientRect);
  end;

  // we need only the alpha channel
  fltGaussianBlur(bmShade, nil, Radius, Toric, True);
end;

procedure fltShadow;
var
  bmShade: TBitmap32;
  x, y, m, xMin, xMax, yMin, yMax: integer;
  pDest, pSrc: PColor32;

begin
  if Alpha = 0 then
    Exit;
  Alpha := Alpha255to256(Alpha);

  // create a blurred shadow and colorize it
  m := fltAuxGenerateBlurred(bm, bmShade, Blur, Toric);
  try
    bmShade.Colorize(Color);
    fltOpacity(bmShade, nil, Alpha);

    // paint the shadow
    if Toric then
    begin
      xMin := 0;
      xMax := bm.Width - 1;
      yMin := 0;
      yMax := bm.Height - 1;
    end
    else
    begin
      xMin := Max(0, dx - m);
      xMax := Min(bm.Width, bmShade.Width + dx - m) - 1;
      yMin := Max(0, dy - m);
      yMax := Min(bm.Height, bmShade.Height + dy - m) - 1;
    end;

    for y := yMin to yMax do
      for x := xMin to xMax do
        if not Assigned(Mask) or Mask[x, y] then
        begin
          if Toric then
            pSrc := bmShade.PixelAddr(SignedMod(x - dx, bm.Width),
              SignedMod(y - dy, bm.Height))
          else
            pSrc := bmShade.PixelAddr(m + x - dx, m + y - dy);
          pDest := bm.PixelAddr(x, y);

          pDest^ := PutPixel32(pDest^, pSrc^);
        end; // for x, y
  finally
    bmShade.Free;
  end;
end;

procedure fltGlow;
var
  i, x, y, m: integer;
  bmShade: TBitmap32;
  pSrc, pDest: PByteArray;

begin
  if Strength = 0 then
    Exit;

  m := fltAuxGenerateBlurred(bm, bmShade, Radius, Toric);
  try
    for y := 0 to bm.Height - 1 do
    begin
      pDest := PByteArray(bm.PixelAddr(0, y));
      pSrc := PByteArray(bmShade.PixelAddr(m, m + y));

      for x := 0 to bm.Width - 1 do
      begin
        if (not Assigned(Mask) or Mask[x, y]) and
          (PColor32(pDest)^ <> cl32Inverted) and
          (((pSrc[3] < pDest[3]) and InnerGlow) or
          ((pSrc[3] > pDest[3]) and OuterGlow)) then
        begin
          // the strength of glow depends on the
          // alpha difference of dest and src
          i := Min($ff, Strength * Abs(pDest[3] - pSrc[3]) shr 8);
          if i > 0 then
            PColor32(pDest)^ :=
              PutPixel32(cardinal(Color) or (byte(i) shl 24), PColor32(pDest)^);
        end;

        // iterate
        Inc(PColor32(pDest));
        Inc(PColor32(pSrc));
      end;
    end;
  finally
    bmShade.Free;
  end;
end;

procedure fltBevel;
var
  bmShade: TBitmap32;
  i, x, y, m, aCanvas, aHL, aShadow: integer;
  pDest: PColor32;
  c: TColor32;

begin
  if Alpha = 0 then
    Exit;
  Alpha := Alpha255to256(Alpha);

  m := fltAuxGenerateBlurred(bm, bmShade, Blur, Toric);
  try
    for y := 0 to bm.Height - 1 do
      for x := 0 to bm.Width - 1 do
      begin
        pDest := bm.PixelAddr(x, y);
        if (not Assigned(Mask) or Mask[x, y]) and (PByteArray(pDest)[3] <> 0) then
        begin
          aCanvas := PByteArray(pDest)[3];

          // calculate white (highlight) and black (shadow) components
          if Toric then
          begin
            aHL := Max(0, aCanvas - integer(
              bmShade.PixelAddr(SignedMod(x - dx, bm.Width),
              SignedMod(y - dy, bm.Height))^ shr 24));
            aShadow := Max(0, aCanvas - integer(
              bmShade.PixelAddr(SignedMod(x + dx, bm.Width),
              SignedMod(y + dy, bm.Height))^ shr 24));
          end
          else
          begin
            aHL := Max(0, aCanvas - integer(
              bmShade.SafeGetPixel(m + x - dx, m + y - dy) shr 24));
            aShadow := Max(0, aCanvas - integer(
              bmShade.SafeGetPixel(m + x + dx, m + y + dy) shr 24));
          end;

          i := ((aHL + aShadow + 1) div 2 * Alpha) shr 8;
          if i <> 0 then
          begin
            c := ((aHL * $ff) div (aHL + aShadow)) * $10101 or (i shl 24);
            pDest^ := PutPixel32(c, pDest^);
          end;
        end;

        Inc(pDest);
      end;
  finally
    bmShade.Free;
  end;
end;

end.

