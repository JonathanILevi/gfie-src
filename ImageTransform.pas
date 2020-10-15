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
unit ImageTransform;

interface

uses
  LclIntf, LclType, SysUtils, Types, Classes, Graphics, bmExUtils, BitmapEx, Math, gfMath, dialogs;

type
  TSimpleFlipRotate = (frFlipH, frFlipV, frRot90Left, frRot90Right, frRot180);

procedure SimpleFlipRotate(bm: TBitmapEx; Mode: TSimpleFlipRotate);
// Translates the rect by (tx, ty) and then rotates it by its center by Angle
function RotateRectToQuad(const r: TRect; tx, ty, Angle: double): TQuad;
// Returns the bounding box of RotateRectToQuad
function RotateRect(const r: TRect; tx, ty, Angle: double): TRect;
procedure StretchDraw(bmSrc, bmDest: TBitmapEx; const rDest: TRect; Antialias: boolean);
// Takes the center of bmSrc as origin
// Rotates bmSrc by Angle, then translates it by the vector (tx, ty)
// and paints it to bmDest
procedure RotateDraw(bmSrc, bmDest: TBitmapEx; tx, ty, Angle: double; Antialias: boolean);

implementation

(*
2d transformation matrix data type
x2 := x1*mat[0, 0] + y1*mat[0, 1] + mat[0, 2]
y2 := x1*mat[1, 0] + y1*mat[1, 1] + mat[1, 2]
*)

type
  TsfrMatrix = array[0..1, 0..2] of integer;

function sfrMatrix(d00, d01, d02, d10, d11, d12: integer): TsfrMatrix; inline;
begin
  Result[0, 0] := d00; Result[0, 1] := d01; Result[0, 2] := d02;
  Result[1, 0] := d10; Result[1, 1] := d11; Result[1, 2] := d12;
end;

procedure sfrCreateXform(w, h: integer; Mode: TSimpleFlipRotate;
  out wNew, hNew: integer; out mat: TsfrMatrix);
begin
  // size changes when rotated
  if Mode in [frFlipH, frFlipV, frRot180] then
    begin wNew := w; hNew := h; end else
    begin wNew := h; hNew := w; end;

  // create INVERSE transformation matrix of the specified xform
  case Mode of
    frFlipH: mat := sfrMatrix(-1, 0, w-1,  0, 1, 0);
    frFlipV: mat := sfrMatrix(1, 0, 0,  0, -1, h-1);
    frRot90Left: mat := sfrMatrix(0, -1, w-1,  1, 0, 0);
    frRot90Right: mat := sfrMatrix(0, 1, 0,  -1, 0, h-1);
    frRot180: mat := sfrMatrix(-1, 0, w-1,  0, -1, h-1);
  end;
end;

procedure SimpleFlipRotate;
var
  bmOrig: TBitmap32;
  w, h, x, y, xSrc, ySrc: integer;
  mat: TsfrMatrix;

begin
  bmOrig := TBitmap32.Create;
  try
    // save original bitmap
    bmOrig.Assign(bm);
    // create inverse transformation
    sfrCreateXform(bm.Width, bm.Height, Mode, w, h, mat);
    // set new size
    bm.Resize(w, h);

    // copy pixels
    for y := 0 to h - 1 do
    begin
      xSrc := mat[0, 1]*y + mat[0, 2];
      ySrc := mat[1, 1]*y + mat[1, 2];

      for x := 0 to w - 1 do
      begin
        bm.SetPixel(x, y, bmOrig.FastGetPixel(xSrc, ySrc));

        inc(xSrc, mat[0, 0]);
        inc(ySrc, mat[1, 0]);
      end; // for x
    end; // for y
  finally
    bmOrig.Free;
  end;
end;

procedure StretchDraw;
var
  i, x, y, xSrcInt, ySrcInt, xMin, xMax, yMin, yMax: integer;
  q: double;
  axSrcInt, axSrcFrac, aySrcInt, aySrcFrac: TArrayOfInteger;
  ca: TBigColor32Average;

  // Fractional parts must be 0..$ff values
  // x1<=x2 and y1<=y2 must apply
  function GetAntialiasedPixel(x1Int, x1Frac, y1Int, y1Frac,
    x2Int, x2Frac, y2Int, y2Frac: integer): TColor32;
  var
    i, j, Factor: integer;

  begin
    Result := cl32Transparent;

    // bounds
    if x1Int < 0 then begin x1Int := 0; x1Frac := 0; end;
    if y1Int < 0 then begin y1Int := 0; y1Frac := 0; end;
    if x2Int >= bmSrc.Width then begin x2Int := bmSrc.Width; x2Frac := 0; end;
    if y2Int >= bmSrc.Height then begin y2Int := bmSrc.Height; y2Frac := 0; end;

    if (x1Int > x2Int) or ((x1Int = x2Int) and (x1Frac > x2Frac)) then Exit;
    if (y1Int > y2Int) or ((y1Int = y2Int) and (y1Frac > y2Frac)) then Exit;

    // make sure that integral parts differ
    if x1Int = x2Int then begin x1Frac := 0; inc(x2Int); x2Frac := 0; end;
    if y1Int = y2Int then begin y1Frac := 0; inc(y2Int); y2Frac := 0; end;

    ca.Clear;

    // CORNERS
    // top-left
    Factor := (($100 - x1Frac) * ($100 - y1Frac)) shr 8;
    if Factor <> 0 then ca.AddWeighted(bmSrc.GetPixel(x1Int, y1Int), Factor);
    // top-right
    Factor := (x2Frac * ($100 - y1Frac)) shr 8;
    if Factor <> 0 then ca.AddWeighted(bmSrc.GetPixel(x2Int, y1Int), Factor);
    // bottom-left
    Factor := (($100 - x1Frac) * y2Frac) shr 8;
    if Factor <> 0 then ca.AddWeighted(bmSrc.GetPixel(x1Int, y2Int), Factor);
    // bottom-right
    Factor := (x2Frac * y2Frac) shr 8;
    if Factor <> 0 then ca.AddWeighted(bmSrc.GetPixel(x2Int, y2Int), Factor);

    // EDGES
    // left
    Factor := $100 - x1Frac;
    if Factor <> 0 then
      for i := y1Int + 1 to y2Int - 1 do ca.AddWeighted(bmSrc.GetPixel(x1Int, i), Factor);
    // top
    Factor := $100 - y1Frac;
    if Factor <> 0 then
      for i := x1Int + 1 to x2Int - 1 do ca.AddWeighted(bmSrc.GetPixel(i, y1Int), Factor);
    // right
    Factor := x2Frac;
    if Factor <> 0 then
      for i := y1Int + 1 to y2Int - 1 do ca.AddWeighted(bmSrc.GetPixel(x2Int, i), Factor);
    // bottom
    Factor := y2Frac;
    if Factor <> 0 then
      for i := x1Int + 1 to x2Int - 1 do ca.AddWeighted(bmSrc.GetPixel(i, y2Int), Factor);

    // SHEET
    for i := x1Int + 1 to x2Int - 1 do
    for j := y1Int + 1 to y2Int - 1 do
      ca.AddWeighted(bmSrc.GetPixel(i, j), $100);

    // Evaluate
    if ca.Count = 0 then
      Result := bmSrc.GetPixel(x1Int, y1Int) else
      Result := ca.GetAverage;
  end;

begin
  if (rDest.Right <= rDest.Left) or (rDest.Bottom <= rDest.Top) then Exit;

  // would a simple draw be enough?
  if (rDest.Width = bmSrc.Width) and
    (rDest.Height = bmSrc.Height) then
  begin
    bmDest.Draw(rDest.Left, rDest.Top, bmSrc);
    Exit;
  end;

  if Antialias then ca := TBigColor32Average.Create;
  try
    // set up coordinate conversion tables
    // X
    xMin := Max(0, rDest.Left);
    xMax := Min(bmDest.Width, rDest.Right);
    i := bmDest.Width + 1;

    SetLength(axSrcInt, i); SetLength(axSrcFrac, i);
    for x := xMin to xMax do
    begin
      q := (x - rDest.Left) / rDest.Width * bmSrc.Width;
      axSrcInt[x] := Floor(q);
      axSrcFrac[x] := Floor(Frac(q) * $100);
    end;
    //////

    // Y
    yMin := Max(0, rDest.Top);
    yMax := Min(bmDest.Height, rDest.Bottom);
    i := bmDest.Height + 1;

    SetLength(aySrcInt, i); SetLength(aySrcFrac, i);
    for y := yMin to yMax do
    begin
      q := (y - rDest.Top) / rDest.Height * bmSrc.Height;
      aySrcInt[y] := Floor(q);
      aySrcFrac[y] := Floor(Frac(q) * $100);
    end;
    //////

    if Antialias then
    begin
      for x := xMin to xMax - 1 do
      for y := yMin to yMax - 1 do
        bmDest.PutPixel(x, y, GetAntialiasedPixel(
          axSrcInt[x], axSrcFrac[x], aySrcInt[y], aySrcFrac[y],
          axSrcInt[x+1], axSrcFrac[x+1], aySrcInt[y+1], aySrcFrac[y+1]));
    end else
    // not antialias
    for y := yMin to yMax - 1 do
    begin
      ySrcInt := aySrcInt[y];
      if (ySrcInt < 0) or (ySrcInt >= bmSrc.Height) then Continue;

      for x := xMin to xMax - 1 do
      begin
        xSrcInt := axSrcInt[x];

        if (xSrcInt >= 0) and (xSrcInt < bmSrc.Width) then
          bmDest.PutPixel(x, y, bmSrc.GetPixel(xSrcInt, ySrcInt));
      end; // for x
    end; // for y
  finally
    if Antialias then ca.Free;
  end;
end;

function RotateRectToQuad;
var
  i, j, Index: integer;
  co, si, cx, cy, qx, qy: double;

begin
  cx := r.xCenter;
  cy := r.yCenter;
  co := Cos(Angle);
  si := Sin(Angle);
  for i := 0 to 1 do
  for j := 0 to 1 do
  begin
    Index := GetQuadIndex(i, j);
    qx := IfThen(i = 0, r.Left, r.Right) - cx;
    qy := IfThen(j = 0, r.Top, r.Bottom) - cy;
    Result[Index].x := co*qx - si*qy + tx;
    Result[Index].y := si*qx + co*qy + ty;
  end;
end;

function RotateRect;
var
  i: integer;
  q: TQuad;

begin
  Result := Rect(High(Integer), High(Integer), Low(Integer), Low(Integer));
  q := RotateRectToQuad(r, tx, ty, Angle);
  for i := 0 to 3 do
  begin
    Result.Left := Min(Result.Left, Floor(q[i].x));
    Result.Top := Min(Result.Top, Floor(q[i].y));
    Result.Right := Max(Result.Right, Ceil(q[i].x));
    Result.Bottom := Max(Result.Bottom, Ceil(q[i].y));
  end;
end;

procedure RotateDraw;
const
  PixelDiv = 32; // Must be a power of two
  EPS = 1e-5;

var
  i, j, x, y: integer;
  xSrc, ySrc, FixedPoint1, FixedPoint_co, FixedPoint_si: Int64;
  xSrc_int, xSrc_frac, ySrc_int, ySrc_frac: integer;
  co, si, q, qx, qy, qx2, qy2, Inv_PixelDiv: double;
  rDest: TRect;
  // The inverse image of a pixel square will overlap with at most 9 pixels
  // of the source image. This array will tell us how much will this overlap be
  // for the 9 source pixels. (Note that we only need to know the fractional
  // part of the coords of the inverse image!)
  // The sum of Weights[x, y, ..., ...] is the same for every (x, y).
  Weights: array[0..PixelDiv-1, 0..PixelDiv-1, -1..1, -1..1] of word;
  ca: TSmallColor32Average;

begin
  // would a simple draw be enough?
  if (Abs(Angle) < EPS) and IsInteger(tx-bmSrc.Width*0.5, EPS) and
    IsInteger(ty-bmSrc.Height*0.5, EPS) then
  begin
    bmDest.Draw(Round(tx-bmSrc.Width*0.5), Round(ty-bmSrc.Height*0.5), bmSrc);
    Exit;
  end;

  // calc dest rectangle
  rDest := RotateRect(bmSrc.ClientRect, tx, ty, Angle);
  if not IntersectRect(rDest, rDest, bmDest.ClientRect) then Exit;

  co := Cos(Angle);
  si := Sin(Angle);

  // initialize the Weights array
  if Antialias then
  begin
    Inv_PixelDiv := 1 / PixelDiv;
    FillChar(Weights, SizeOf(Weights), 0);
    // divide a pixel into many parts
    // Caution: we cannot use more parts than 2^15
    // because we will use TSmallColor32Average
    for x := 0 to 15 do
    for y := 0 to 15 do
    begin
      qx := (2*x - 15)*0.03125;
      qy := (2*y - 15)*0.03125;
      qx2 := co*qx + si*qy + Inv_PixelDiv*0.5;
      qy2 := -si*qx + co*qy + Inv_PixelDiv*0.5;
      for i := 0 to PixelDiv-1 do
      begin
        q := qy2;
        for j := 0 to PixelDiv-1 do
        begin
          inc(Weights[i, j, Floor(qx2), Floor(q)]);
          q += Inv_PixelDiv;
        end;
        qx2 += Inv_PixelDiv;
      end;
    end;
  end; // if antialias

  // calculate pixel values
  FixedPoint1 := $100000000*PixelDiv;
  FixedPoint_co := Round(FixedPoint1*co);
  FixedPoint_si := Round(FixedPoint1*si);
  if Antialias then ca := TSmallColor32Average.Create;
  try
    for y := rDest.Top to rDest.Bottom - 1 do
    begin
      // inverse transform
      xSrc := Round(FixedPoint1*(co*(rDest.Left+0.5-tx) + si*(y+0.5-ty) + bmSrc.Width*0.5));
      ySrc := Round(FixedPoint1*(-si*(rDest.Left+0.5-tx) + co*(y+0.5-ty) + bmSrc.Height*0.5));
      for x := rDest.Left to rDest.Right - 1 do
      begin
        // get integral and fractional part of inverse position
        // fractional part is only needed with 1/PixelDiv precision
        xSrc_int := PIntegerArray(@xSrc)[1];
        if (xSrc_int >= -PixelDiv) and (xSrc_int < PixelDiv*(bmSrc.Width+1)) then
        begin
          xSrc_frac := (xSrc_int+PixelDiv) and (PixelDiv-1); // mod PixelDiv
          xSrc_int := (xSrc_int+PixelDiv) div PixelDiv - 1; // avoid rounding to zero

          ySrc_int := PIntegerArray(@ySrc)[1];
          if (ySrc_int >= -PixelDiv) and (ySrc_int < PixelDiv*(bmSrc.Height+1)) then
          begin
            ySrc_frac := (ySrc_int+PixelDiv) and (PixelDiv-1); // mod PixelDiv
            ySrc_int := (ySrc_int+PixelDiv) div PixelDiv - 1;

            if Antialias then
            begin
              ca.Clear;
              for i := -1 to 1 do for j := -1 to 1 do
                ca.AddWeighted(bmSrc.SafeGetPixel(xSrc_int+i, ySrc_int+j),
                  Weights[xSrc_frac, ySrc_frac, i, j]);
              bmDest.PutPixel(x, y, ca.GetAverage);
            end else
              bmDest.PutPixel(x, y, bmSrc.SafeGetPixel(xSrc_int, ySrc_int));
          end; // ySrc is OK
        end; // xSrc is OK

        // update inverse pixel value
        xSrc += FixedPoint_co;
        ySrc -= FixedPoint_si;
      end; // for x
    end; // for y
  finally
    if Antialias then ca.Free;
  end;
end;

end.

