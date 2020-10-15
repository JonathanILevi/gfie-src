(*
	Greenfish Generic List
	Copyright © 2016 Balázs Szalkai
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
{$MODE DELPHI}
unit GenericList;

interface

uses
  SysUtils;

type
  TGenericListCompare<T> = function(const a, b: T): integer;

  { TGenericList }

  TGenericList<T> = class
  private
    type
      PT = ^T;
    var
      FCount: integer;
      FArray: array of T;
    function GetItems(Index: integer): T;
    function GetCapacity: integer;
    procedure SetCount(AValue: integer);
    procedure SetItems(Index: integer; const AValue: T);
  public
    type

      { TEnumerator }

      TEnumerator = class
      private
        Parent: TGenericList<T>;
        Index: integer;

        function GetCurrent: T;
      public
        constructor Create(AParent: TGenericList<T>); virtual;
        function MoveNext: Boolean;
        property Current: T read GetCurrent;
      end;

    constructor Create;
    procedure Clear;
    procedure Add(const value: T);
    procedure Delete(Index: integer);
    function GetItemPtr(Index: integer): PT;
    function IndexOf(const value: T): integer; // uses CompareMem
    function GetEnumerator: TEnumerator;
    procedure Sort(Compare: TGenericListCompare<T>);

    property Count: integer read FCount write SetCount;
    property Capacity: integer read GetCapacity;
    property Items[Index: integer]: T read GetItems write SetItems; default;
  end;

implementation

{ TGenericList<T> }

function TGenericList<T>.GetCapacity: integer;
begin
  Result := Length(FArray);
end;

procedure TGenericList<T>.SetCount(AValue: integer);
begin
  if FCount=AValue then Exit;
  FCount:=AValue;
  if (FCount > Length(FArray)) or (FCount*4 < Length(FArray)) then
    SetLength(FArray, FCount*2);
end;

function TGenericList<T>.GetItems(Index: integer): T;
begin
  Result := FArray[Index];
end;

procedure TGenericList<T>.SetItems(Index: integer; const AValue: T);
begin
  FArray[Index] := AValue;
end;

constructor TGenericList<T>.Create;
begin
  FCount := 0;
end;

procedure TGenericList<T>.Clear;
begin
  SetCount(0);
end;

procedure TGenericList<T>.Add(const value: T);
begin
  Count := Count+1;
  Items[Count-1] := value;
end;

procedure TGenericList<T>.Delete(Index: integer);
var
  i: integer;
begin
  for i := Index to Count - 2 do
    FArray[i] := FArray[i+1];
  Count := Count-1;
end;

function TGenericList<T>.GetItemPtr(Index: integer): PT;
begin
  Result := @FArray[Index];
end;

function TGenericList<T>.IndexOf(const value: T): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if CompareMem(GetItemPtr(i), @value, SizeOf(T)) then
      begin
        Result := i;
        break;
      end;
end;

function TGenericList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

procedure TGenericList<T>.Sort(Compare: TGenericListCompare<T>);
var
  i, j, m: integer;
  tmp: T;

begin
  for i := 0 to Count - 2 do
  begin
    m := i;
    for j := i+1 to Count - 1 do
      if Compare(Items[j], Items[m]) < 0 then
        m := j;
    if m <> i then
    begin
      tmp := Items[i];
      Items[i] := Items[m];
      Items[m] := tmp;
    end;
  end;
end;

{ TGenericList<T>.TEnumerator }

function TGenericList<T>.TEnumerator.GetCurrent: T;
begin
  Result := Parent[Index];
end;

constructor TGenericList<T>.TEnumerator.Create(AParent: TGenericList<T>);
begin
  Parent := AParent;
  Index := -1;
end;

function TGenericList<T>.TEnumerator.MoveNext: Boolean;
begin
  inc(Index);
  Result := Index < Parent.Count;
end;

end.
