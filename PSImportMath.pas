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
// Duplicates functions of the Math unit
// so the PascalScript autoimporter can import them.
unit PSImportMath;

{$mode delphi}

interface

procedure Randomize;
function Random: extended;
function RandomRange(l, h: int64): int64;
function Round(x: extended): int64;
function Floor(x: extended): integer;
function Ceil(x: extended): integer;
function Min(x, y: int64): int64;
function Max(x, y: int64): int64;
function Min2(x, y: extended): extended;
function Max2(x, y: extended): extended;
function Exp(x: extended): extended;
function Ln(x: extended): extended;
function ArcCos(x: extended): extended;
function ArcSin(x: extended): extended;
function ArcTan2(y, x: extended): extended;
function Tan(x: extended): extended;
function Power(x, y: extended): extended;
function IsInfinite(x: double): boolean;
function IsNaN(x: extended): boolean;

implementation

uses
  SysUtils, Math;

procedure Randomize;
begin
  System.Randomize;
end;

function Random: extended;
begin
  Result := System.Random;
end;

function RandomRange(l, h: int64): int64;
begin
  Result := Math.RandomRange(l, h);
end;

function Round(x: extended): int64;
begin
  Result := System.Round(x);
end;

function Floor(x: extended): integer;
begin
  Result := Math.floor(x);
end;

function Ceil(x: extended): integer;
begin
  Result := Math.ceil(x);
end;

function Min(x, y: int64): int64;
begin
  Result := Math.Min(x, y);
end;

function Max(x, y: int64): int64;
begin
  Result := Math.Max(x, y);
end;

function Min2(x, y: extended): extended;
begin
  Result := Math.Min(x, y);
end;

function Max2(x, y: extended): extended;
begin
  Result := Math.Max(x, y);
end;

function Exp(x: extended): extended;
begin
  Result := System.Exp(x);
end;

function Ln(x: extended): extended;
begin
  Result := System.Ln(x);
end;

function ArcCos(x: extended): extended;
begin
  Result := Math.arccos(x);
end;

function ArcSin(x: extended): extended;
begin
  Result := Math.arcsin(x);
end;

function ArcTan2(y, x: extended): extended;
begin
  Result := Math.arctan2(y, x);
end;

function Tan(x: extended): extended;
begin
  Result := Math.tan(x);
end;

function Power(x, y: extended): extended;
begin
  Result := Math.power(x, y);
end;

function IsInfinite(x: double): boolean;
begin
  Result := Math.IsInfinite(x);
end;

function IsNaN(x: extended): boolean;
begin
  Result := Math.IsNan(x);
end;

end.

