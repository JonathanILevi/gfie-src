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
unit ColQuant;

interface

uses
  LclIntf, LclType, SysUtils, Graphics, PixelFormats;

type
  TColorQuantizer = class
  public
    procedure GetPalette(Palette: TiePalette); virtual; abstract;
    function ColorCount: integer; virtual; abstract;
    procedure Process(Color: TColor); virtual; abstract;
  end;

  POctreeNode = ^TOctreeNode;
  TOctreeNode = class;
  TOctreeNode8 = array[0..7] of TOctreeNode;

  TInteger3 = array[0..2] of integer;
  TOctreeNode = class(TObject)
    IsLeaf: boolean;
    PixelCount: integer;
    Sum: TInteger3;
    Next: TOctreeNode;
    Child: TOctreeNode8;

    constructor Create(Level, ColorBits: integer; var LeafCount: integer;
      var Nodes: TOctreeNode8);
    destructor Destroy; override;
  end;

  TOctreeQuantizer = class(TColorQuantizer)
  private
    FTree: TOctreeNode;
    FLeafCount: integer;
    FNodes: TOctreeNode8;
    FMaxColors: integer;
    FColorBits: integer;
  protected
    procedure AddColor(Node: POctreeNode; r, g, b: byte;
      var Nodes: TOctreeNode8);
    procedure DeleteTree(var Node: TOctreeNode);
    procedure GetPaletteColors(Node: TOctreeNode; Palette: TiePalette);
    procedure ReduceTree(ColorBits: integer; var LeafCount: integer;
      var Nodes: TOctreeNode8);
  public
    constructor Create(MaxColors: integer);
    destructor Destroy; override;

    procedure GetPalette(Palette: TiePalette); override;
    function ColorCount: integer; override;
    procedure Process(Color: TColor); override;
  end;

  TColorQuantizer_Impl = TOctreeQuantizer; // for pascal script

implementation

// TOctreeNode

constructor TOctreeNode.Create;
var
  i: integer;

begin
  PixelCount := 0;
  for i := 0 to 2 do Sum[i] := 0;
  for i := Low(Child) to High(Child) do Child[i] := nil;

  IsLeaf := Level = ColorBits;
  if IsLeaf then
  begin
    Next := nil;
    inc(LeafCount);
  end else
  begin
    Next := Nodes[Level];
    Nodes[Level] := Self;
  end;
end;

destructor TOctreeNode.Destroy;
var
  n: TOctreeNode;

begin
  for n in Child do if Assigned(n) then n.Free;
  inherited;
end;

// TOctreeQuantizer

procedure TOctreeQuantizer.AddColor;
var
  Level, Shift: integer;

begin
  Level := 0;

  while True do
  begin
    if not Assigned(Node^) then Node^ := TOctreeNode.Create
      (Level, FColorBits, FLeafCount, Nodes);

    if Node^.IsLeaf then
    begin
      inc(Node^.PixelCount);
      inc(Node^.Sum[0], r);
      inc(Node^.Sum[1], g);
      inc(Node^.Sum[2], b);
      Exit;
    end else
    begin
      Shift := Level xor 7;

      Node := @Node.Child[ ( ((r shr Shift) and 1) shl 2 ) or
                          ( ((g shr Shift) and 1) shl 1 ) or
                          ((b shr Shift) and 1) ];

      inc(Level);
    end;
  end;
end;

procedure TOctreeQuantizer.DeleteTree;
var
  i: integer;

begin
  for i := Low(Node.Child) to High(Node.Child) do
    if Assigned(Node.Child[i]) then DeleteTree(Node.Child[i]);

  FreeAndNil(Node);
end;

procedure TOctreeQuantizer.GetPaletteColors;
var
  n: TOctreeNode;

begin
  if Node.IsLeaf then
    if Node.PixelCount = 0 then Palette.Add(clBlack, False) else
      Palette.Add(((Node.Sum[0] + Node.PixelCount div 2) div Node.PixelCount) or
        (((Node.Sum[1] + Node.PixelCount div 2) div Node.PixelCount) shl 8) or
        (((Node.Sum[2] + Node.PixelCount div 2) div Node.PixelCount) shl 16), False) else
  begin
    // enumerate children
    for n in Node.Child do
      if Assigned(n) then GetPaletteColors(n, Palette);
  end;
end;

procedure TOctreeQuantizer.ReduceTree;
var
  Sum: array[0..2] of integer;
  i, j: integer;
  Node: TOctreeNode;

begin
  i := ColorBits - 1;
  while (i > 0) and not Assigned(Nodes[i]) do dec(i);

  Node := Nodes[i];
  Nodes[i] := Node.Next;

  for i := 0 to 2 do Sum[i] := 0;

  for i := Low(Nodes) to High(Nodes) do
  begin
    if Assigned(Node.Child[i]) then
    begin
      for j := 0 to 2 do inc(Sum[j], Node.Child[i].Sum[j]);
      inc(Node.PixelCount, Node.Child[i].PixelCount);
      FreeAndNil(Node.Child[i]);
      dec(LeafCount);
    end;
  end;

  Node.IsLeaf := True;
  for i := 0 to 2 do Node.Sum[i] := Sum[i];
  inc(LeafCount);
end;

constructor TOctreeQuantizer.Create;
var
  i: integer;

begin
  FTree := nil;
  FLeafCount := 0;

  for i := Low(FNodes) to High(FNodes) do
    FNodes[i] := nil;

  FMaxColors := MaxColors;
  FColorBits := 8;
end;

destructor TOctreeQuantizer.Destroy;
begin
  if Assigned(FTree) then DeleteTree(FTree);
end;

procedure TOctreeQuantizer.GetPalette;
begin
  if Assigned(FTree) then GetPaletteColors(FTree, Palette);
end;

function TOctreeQuantizer.ColorCount;
begin
  Result := FLeafCount;
end;

procedure TOctreeQuantizer.Process;
begin
  AddColor(@FTree, Color and $ff, (Color shr 8) and $ff,
    (Color shr 16) and $ff, FNodes);

  while FLeafCount > FMaxColors do
    ReduceTree(FColorBits, FLeafCount, FNodes);
end;

end.
