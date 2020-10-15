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
unit MapClass;

{$mode delphi}

interface

uses
  SysUtils, Contnrs;

type
  TMap<TKey, TValue> = class
  public type
    TNode = class
    private type
      TColor = (clRed, clBlack);
    private
      Color: TColor;
      Parent, Left, Right: TNode;
      FKey: TKey;
    public
      Value: TValue;

      property Key: TKey read FKey;
      constructor Create(const AKey: TKey);
      destructor Destroy; override;
    end;

    TEnumerator = class
    private
      Root: TNode;
      Stack: TStack;

      function GetCurrent: TNode;
    public
      constructor Create(const ARoot: TNode); virtual;
      destructor Destroy; override;

      function MoveNext: Boolean;
      property Current: TNode read GetCurrent;
    end;
  protected
    Root: TNode;
    FCount: integer;

    function Find(const Key: TKey; out Parent: TNode): TNode;
    function AddNode(const Key: TKey): TNode;
    procedure RotateLeft(const Node: TNode);
    procedure RotateRight(const Node: TNode);

    function GetValue(const Key: TKey): TValue;
    procedure SetValue(const Key: TKey; const Value: TValue);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;

    function ContainsKey(const Key: TKey): boolean;
    function TryGet(const Key: TKey): TValue;
    function GetEnumerator: TEnumerator;

    property Count: integer read FCount;
    // Causes access violation on read if key not found
    property Map[Key: TKey]: TValue read GetValue write SetValue; default;
  end;

implementation

// TMap.TNode

constructor TMap<TKey, TValue>.TNode.Create;
begin
  Color := clBlack;
  Parent := nil;
  Left := nil;
  Right := nil;
  FKey := AKey;
  Value := Default(TValue);
end;

destructor TMap<TKey, TValue>.TNode.Destroy;
begin
  if Left <> nil then Left.Free;
  if Right <> nil then Right.Free;

  inherited;
end;

// TMap

function TMap<TKey, TValue>.Find;
begin
  Result := Root;
  Parent := nil;

  while Result <> nil do
  begin
    if Key = Result.Key then Break;
    
    Parent := Result;
    if Key < Result.Key then Result := Result.Left else Result := Result.Right;
  end;
end;

function TMap<TKey, TValue>.AddNode;
var
  Node, Parent, Node2: TNode;

begin
  Node := Find(Key, Parent);
  if Node <> nil then Exit(Node);

  Node := TNode.Create(Key);
  Result := Node;
  inc(FCount);

  if Parent <> nil then
  begin
    if Key < Parent.Key then Parent.Left := Node else Parent.Right := Node;
    Node.Parent := Parent;
  end else
    Root := Node;

  while (Node.Parent <> nil) and (Node.Parent.Color = clRed) do
  if Node.Parent = Node.Parent.Parent.Left then
  begin
    Node2 := Node.Parent.Parent.Right;

    if (Node2 <> nil) and (Node2.Color = clRed) then
    begin
      Node.Parent.Color := clBlack;
      Node := Node.Parent.Parent;
      Node.Color := clRed;
      Node2.Color := clBlack;
    end else
    begin
      if Node = Node.Parent.Right then
      begin
        Node := Node.Parent;
        RotateLeft(Node);
      end;

      Node.Parent.Color := clBlack;
      Node.Parent.Parent.Color := clRed;
      RotateRight(Node.Parent.Parent);
    end;
  end else
  begin
    Node2 := Node.Parent.Parent.Left;

    if (Node2 <> nil) and (Node2.Color = clRed) then
    begin
      Node.Parent.Color := clBlack;
      Node := Node.Parent.Parent;
      Node.Color := clRed;
      Node2.Color := clBlack;
    end else
    begin
      if Node = Node.Parent.Left then
      begin
        Node := Node.Parent;
        RotateRight(Node);
      end;

      Node.Parent.Color := clBlack;
      Node.Parent.Parent.Color := clRed;
      RotateLeft(Node.Parent.Parent);
    end;
  end;

  Root.Color := clBlack;
end;

procedure TMap<TKey, TValue>.RotateLeft;
var
  Node2: TNode;

begin
  Node2 := Node.Right;

  Node.Right := Node2.Left;
  if Node2.Left <> nil then Node2.Left.Parent := Node;
  
  Node2.Parent := Node.Parent;

  if Node.Parent = nil then Root := Node2 else
    if Node = Node.Parent.Left then
      Node.Parent.Left := Node2 else
      Node.Parent.Right := Node2;

  Node2.Left := Node;
  Node.Parent := Node2;
end;

procedure TMap<TKey, TValue>.RotateRight;
var
  Node2: TNode;

begin
  Node2 := Node.Left;

  Node.Left := Node2.Right;
  if Node2.Right <> nil then Node2.Right.Parent := Node;

  Node2.Parent := Node.Parent;

  if Node.Parent = nil then Root := Node2 else
    if Node = Node.Parent.Left then
      Node.Parent.Left := Node2 else
      Node.Parent.Right := Node2;

  Node2.Right := Node;
  Node.Parent := Node2;
end;

function TMap<TKey, TValue>.GetValue;
var
  Dummy: TNode;

begin
  Result := Find(Key, Dummy).Value;
end;

procedure TMap<TKey, TValue>.SetValue;
begin
  AddNode(Key).Value := Value;
end;

constructor TMap<TKey, TValue>.Create;
begin
  Root := nil;
  FCount := 0;
end;

destructor TMap<TKey, TValue>.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMap<TKey, TValue>.Clear;
begin
  FreeAndNil(Root);
  FCount := 0;
end;

function TMap<TKey, TValue>.ContainsKey;
var
  Dummy: TNode;

begin
  Result := (Find(Key, Dummy) <> nil);
end;

function TMap<TKey, TValue>.TryGet;
var
  Node, Parent: TNode;

begin
  Node := Find(Key, Parent);
  if Node = nil then
    Result := Default(TValue)
    else Result := Node.Value;
end;

function TMap<TKey, TValue>.GetEnumerator;
begin
  Result := TEnumerator.Create(Root);
end;

// TMap.TEnumerator

constructor TMap<TKey, TValue>.TEnumerator.Create;
begin
  Root := ARoot;
  Stack := TStack.Create;
end;

destructor TMap<TKey, TValue>.TEnumerator.Destroy;
begin
  Stack.Free;
end;

function TMap<TKey, TValue>.TEnumerator.GetCurrent;
begin
  if Stack.AtLeast(1) then Exit(Stack.Peek);
  Exit(nil); // should never happen
end;

function TMap<TKey, TValue>.TEnumerator.MoveNext;
var
  Node: TNode;

begin
  if not Stack.AtLeast(1) then
  begin
    if Root = nil then Exit(False);

    Stack.Push(Root);
    Exit(True);
  end;

  Node := Stack.Peek;
  if Node.Left <> nil then
  begin
    Stack.Push(Node.Left);
    Exit(True);
  end;
  if Node.Right <> nil then
  begin
    Stack.Push(Node.Right);
    Exit(True);
  end;

  while Stack.AtLeast(1) do
  begin
    Node := Stack.Pop;
    if (Node.Parent <> nil) and
      (Node = Node.Parent.Left) and (Node.Parent.Right <> nil) then
    begin
      Stack.Push(Node.Parent.Right);
      Exit(True);
    end;
  end;

  Exit(False);
end;

end.

