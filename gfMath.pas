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
// Extension of the Math unit
unit gfMath;

interface

// Undefine this temporarily if Lazarus complains about various things
{$define RectExtensions}

uses
  LclIntf, LclType, SysUtils, Types, Math, GraphMath;

function Min3i(a, b, c: integer): integer; inline;
function Min4i(a, b, c, d: integer): integer; inline;
function Min4d(a, b, c, d: double): double; inline;
function Max3i(a, b, c: integer): integer; inline;
function Max4i(a, b, c, d: integer): integer; inline;
function Max4d(a, b, c, d: double): double; inline;

// Returns polar angle of the specified point in interval [0, 2*pi)
function GetPhi(x, y: double): double;
function IsInteger(x, Epsilon: double): boolean; inline;
// Floating-point remainder operation
function FloatMod(a, b: double): double; inline;
// Works properly if i < 0, in contrary to "mod"
function SignedMod(i, j: integer): integer; inline;

type
  {$ifdef RectExtensions}
  // Extension for TRect
  TRectHelper = record helper for TRect
    function Width: integer; inline;
    function Height: integer; inline;
    function xCenter: double; inline;
    function yCenter: double; inline;
    function Inflate(dx, dy: integer): TRect;
    procedure Include(x, y: integer);
    function IsZero: boolean;
    procedure AddMargin(const r: TRect);
    function Equals(const r: TRect): boolean;
  end;
  {$endif}

  (*
    Schema of indexes counter-clockwise (in base 2):
    00 ------- 11
    |           |
    |           |
    01 ------- 10
  *)
  TDoublePoint = record
    x: double;
    y: double;
  end;
  TQuad = array[0..3] of TDoublePoint;

// Index of vertex at position (x, y) (where x:0..1, y:0..1): 3*x xor y
function GetQuadIndex(x, y: integer): integer; inline;
procedure GetGravityCenter(const q: TQuad; out x, y: double);
// Where the x1,y1,x2,y2 line intersects the horizontal line?
function IsIntersection(x1, y1, x2, y2: double; y: integer;
  var x: integer): boolean;
// Signed distance (can be negative!)
function PointLineDist(x, y, x1, y1, x2, y2: double): double;
function PointSegmentDist(x, y, x1, y1, x2, y2: double): double;

implementation

function Min3i(a, b, c: integer): integer;
begin
  Result := Min(Min(a, b), c);
end;

function Min4i(a, b, c, d: integer): integer;
begin
  Result := Min(Min(a, b), Min(c, d));
end;

function Min4d(a, b, c, d: double): double;
begin
  Result := Min(Min(a, b), Min(c, d));
end;

function Max3i(a, b, c: integer): integer;
begin
  Result := Max(Max(a, b), c);
end;

function Max4i(a, b, c, d: integer): integer;
begin
  Result := Max(Max(a, b), Max(c, d));
end;

function Max4d(a, b, c, d: double): double;
begin
  Result := Max(Max(a, b), Max(c, d));
end;

function GetPhi;
begin
  if x = 0 then
    if y < 0 then Result := 1.5*pi else
    Result := 0.5*pi else
  if x < 0 then
    Result := pi + ArcTan(y / x) else
  begin
    Result := ArcTan(y / x);
    if Result < 0 then Result := Result + 2*pi;
  end;
end;

function IsInteger;
begin
  Result := Abs(Round(x) - x) < Epsilon;
end;

function FloatMod;
begin
  Result := a - Floor(a / b)*b;
end;

function SignedMod;
begin
  if i >= 0 then Result := i mod j else
  begin
    Result := (-i) mod j;
    if Result <> 0 then Result := j - Result;
  end;
end;

{$ifdef RectExtensions}
function TRectHelper.Width; begin Result := Right-Left; end;
function TRectHelper.Height; begin Result := Bottom-Top; end;
function TRectHelper.xCenter; begin Result := 0.5*(Left+Right); end;
function TRectHelper.yCenter; begin Result := 0.5*(Top+Bottom); end;

function TRectHelper.Inflate;
begin
  Result := Rect(Left - dx, Top - dy, Right + dx, Bottom + dy);
end;

procedure TRectHelper.Include(x, y: integer);
begin
  if x < Left then
    Left := x;
  if y < Top then
    Top := y;
  if x >= Right then
    Right := x+1;
  if y >= Bottom then
    Bottom := y+1;
end;

function TRectHelper.IsZero: boolean;
begin
  Result := ((Left or Top or Right or Bottom) = 0);
end;

procedure TRectHelper.AddMargin(const r: TRect);
begin
  Left += r.Left;
  Top += r.Top;
  Right += r.Right;
  Bottom += r.Bottom;
end;

function TRectHelper.Equals(const r: TRect): boolean;
begin
  Result := (Left = r.Left) and (Top = r.Top) and (Right = r.Right) and (Bottom = r.Bottom);
end;
{$endif}

function GetQuadIndex;
begin
  Result := 3*x xor y;
end;

procedure GetGravityCenter;
begin
  x := (q[0].x + q[1].x + q[2].x + q[3].x)*0.25;
  y := (q[0].y + q[1].y + q[2].y + q[3].y)*0.25;
end;

function IsIntersection;
begin
  Result := (y1 <> y2) and ( ((y >= y1) and (y <= y2)) or
    ((y >= y2) and (y <= y1)) );
  if Result then x := Floor(x1 + (x2 - x1) * (y - y1) / (y2 - y1));
end;

function PointLineDist;
begin
  Result := ((x-x1)*(y2-y1) - (y-y1)*(x2-x1)) / Sqrt(Sqr(x2-x1)+Sqr(y2-y1));
end;

function PointSegmentDist;
var
  Length, InvLength, Pos: double;

begin
  Length := Sqrt(Sqr(x2-x1) + Sqr(y2-y1));
  InvLength := 1 / Length;
  Pos := ((x-x1)*(x2-x1) + (y-y1)*(y2-y1)) * InvLength;
  if (Pos > 0) and (Pos < Length) then
    // distance from line
    Result := Abs((x-x1)*(y2-y1) - (y-y1)*(x2-x1)) * InvLength else
    // distance from endpoints
    Result := Min(Sqrt(Sqr(x-x1) + Sqr(y-y1)), Sqrt(Sqr(x-x2) + Sqr(y-y2)));
end;

end.

