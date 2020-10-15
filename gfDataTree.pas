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

// Managing, loading and saving Greenfish Treefiles
// These files describe an XML-like tree structure, where all nodes have a string ID (name)
// and may contain a block of data and other nodes, while
// * No two sibling nodes may share the same ID
// * All ID's must be between 0 and 255 characters
unit gfDataTree;

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Math, bmExUtils;

type
  TgfDataNode = class(TPersistent)
  protected
    FDataSize: integer;
    FData: Pointer;
    FChildren: TList;

    function GetAsBoolean: boolean;
    function GetAsInt: integer;
    function GetAsInt64: Int64;
    function GetAsDouble: double;
    function GetAsString: string;
    function GetChild(Index: integer): TgfDataNode;

    procedure SetAsBoolean(const Value: boolean);
    procedure SetDataSize(const Value: integer);
    procedure SetAsInt(const Value: integer);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsDouble(const Value: double);
    procedure SetAsString(const Value: string);
  public
    Parent: TgfDataNode;
    ID: string;

    constructor Create(AParent: TgfDataNode); virtual;
    destructor Destroy; override;

    procedure Clear;

    property DataSize: integer read FDataSize write SetDataSize;
    property Data: Pointer read FData;
    
    // Wrappers
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsInt: integer read GetAsInt write SetAsInt;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsDouble: double read GetAsDouble write SetAsDouble;
    property AsString: string read GetAsString write SetAsString;

    function ChildCount: integer;
    property Children[Index: integer]: TgfDataNode read GetChild;
    function NewChild: TgfDataNode;
    procedure DeleteChild(Index: integer);
  end;

  TgfDataTree = class(TPersistent)
  protected
    FRoot, FNode: TgfDataNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property Node: TgfDataNode read FNode;

    function LookupChild(const ID: string): TgfDataNode;
    function DeleteChild(const ID: string): boolean;

    // Returns whether child with ID existed
    function Descend(const ID: string): boolean;
    // Similar to Descend, but creates a child if does not exist
    procedure NewChild(const ID: string);

    function Ascend: boolean;
    procedure AscendToRoot;

    function LoadFromStream(s: TStream): boolean;
    procedure SaveToStream(s: TStream);
    function LoadFromFile(const FileName: string): boolean;
    procedure SaveToFile(const FileName: string);
  end;

const
  GF_DATA_TREE_SIG = $74646667; // 'gfdt'
  GFDT_BLOCK_BEGIN = 60; // <
  GFDT_BLOCK_END = 62; // >

implementation

// TgfDataNode

function TgfDataNode.GetAsBoolean;
begin
  if DataSize >= 1 then Result := PBoolean(Data)^ else Result := False;
end;

function TgfDataNode.GetAsInt;
begin
  Result := 0;
  Move(Data^, Result, Min(SizeOf(Integer), DataSize));
end;

function TgfDataNode.GetAsInt64;
begin
  Result := 0;
  Move(Data^, Result, Min(SizeOf(Int64), DataSize));
end;

function TgfDataNode.GetAsDouble;
begin
  Result := 0;
  Move(Data^, Result, Min(SizeOf(double), DataSize));
end;

function TgfDataNode.GetAsString;
begin
  SetLength(Result, DataSize);
  Move(Data^, Result[1], DataSize);
end;

function TgfDataNode.GetChild;
begin
  Result := FChildren[Index];
end;
   
procedure TgfDataNode.SetDataSize;
begin
  if FDataSize <> Value then
  begin
    FDataSize := Value;
    FreeAndNilMem(FData);
    if FDataSize > 0 then
      FData := GetMem(FDataSize);
  end;
end;

procedure TgfDataNode.SetAsBoolean;
begin
  DataSize := SizeOf(Boolean);
  PBoolean(Data)^ := Value;
end;

procedure TgfDataNode.SetAsInt;
begin
  DataSize := SizeOf(Integer);
  PInteger(Data)^ := Value;
end;

procedure TgfDataNode.SetAsInt64;
begin
  DataSize := SizeOf(Int64);
  PInt64(Data)^ := Value;
end;

procedure TgfDataNode.SetAsDouble;
begin
  DataSize := SizeOf(double);
  PDouble(Data)^ := Value;
end;

procedure TgfDataNode.SetAsString;
begin
  DataSize := Length(Value);
  Move(Value[1], Data^, DataSize);
end;

constructor TgfDataNode.Create;
begin
  FDataSize := 0;
  FData := nil;
  FChildren := TList.Create;
  Parent := AParent;
  ID := '';
end;

destructor TgfDataNode.Destroy;
begin
  Clear;
  FChildren.Free;

  inherited;
end;

procedure TgfDataNode.Clear;
var
  i: integer;
  
begin
  DataSize := 0;
  for i := 0 to ChildCount - 1 do Children[i].Free;
  FChildren.Clear;
end;

function TgfDataNode.ChildCount;
begin
  Result := FChildren.Count;
end;

function TgfDataNode.NewChild;
begin
  Result := TgfDataNode.Create(Self);
  FChildren.Add(Result);
end;

procedure TgfDataNode.DeleteChild;
begin
  Children[Index].Free;
  FChildren.Delete(Index);
end;

// TgfDataTree

constructor TgfDataTree.Create;
begin
  FRoot := TgfDataNode.Create(nil);
  FRoot.ID := '\';
  FNode := FRoot;
end;

destructor TgfDataTree.Destroy;
begin
  FRoot.Free;
end;

procedure TgfDataTree.Clear;
begin
  FNode := FRoot;
  FRoot.Clear;
end;

function TgfDataTree.LookupChild;
var
  i: integer;

begin
  Result := nil;
  for i := 0 to Node.ChildCount - 1 do if Node.Children[i].ID = ID then
  begin
    Result := Node.Children[i];
    Exit;
  end;
end;

function TgfDataTree.DeleteChild;
var
  i: integer;

begin
  Result := False;
  for i := 0 to Node.ChildCount - 1 do if Node.Children[i].ID = ID then
  begin
    Node.DeleteChild(i);
    Result := True;
    Exit;
  end;
end;

function TgfDataTree.Descend;
var
  n: TgfDataNode;

begin
  n := LookupChild(ID);
  Result := Assigned(n);
  if Result then FNode := n;
end;

procedure TgfDataTree.NewChild;
var
  n: TgfDataNode;

begin
  n := LookupChild(ID);
  if Assigned(n) then FNode := n else
  begin
    FNode := Node.NewChild;
    Node.ID := ID;
  end;
end;

function TgfDataTree.Ascend;
begin
  Result := Assigned(Node.Parent);
  if Result then FNode := Node.Parent;
end;

procedure TgfDataTree.AscendToRoot;
begin
  FNode := FRoot;
end;

function TgfDataTree.LoadFromStream;
var
  DataSize: integer;
  b, BlockType: Byte;
  sig: Cardinal;
  RootWasRead: boolean;
  ID: string;

begin
  Result := False;

  try
    // read signature
    sig := s.ReadDWord;
    if sig <> GF_DATA_TREE_SIG then Exit;

    Clear;
    RootWasRead := False;

    // read blocks
    while s.Position < s.Size do
    begin
      // read block type
      BlockType := s.ReadByte;

      case BlockType of
        GFDT_BLOCK_BEGIN: begin
          // read ID
          b := s.ReadByte;
          SetLength(ID, b);
          s.ReadBuffer(ID[1], b);

          if RootWasRead then NewChild(ID) else
          begin
            Node.ID := ID;
            RootWasRead := True;
          end;

          // read data size
          DataSize := Integer(s.ReadDWord);
          Node.DataSize := DataSize;

          // read data
          s.ReadBuffer(Node.Data^, Node.DataSize);
        end;

        GFDT_BLOCK_END: if not Ascend then Break;

        else Exit; // invalid token
      end; // case BlockType
    end; // while loop

    // not enough closing brackets?
    if Node <> FRoot then Exit;
      
    // success
    Result := True;
  except
  end;
end;

procedure TgfDataTree.SaveToStream;
var
  sig: Cardinal;

  procedure WriteNode(n: TgfDataNode);
  var
    i: integer;
    b: byte;
    DataSize: Cardinal;

  begin
    // <
    b := GFDT_BLOCK_BEGIN;
    s.WriteBuffer(b, SizeOf(b));

    // write ID
    b := Length(n.ID);
    s.WriteBuffer(b, SizeOf(b));
    s.WriteBuffer(n.ID[1], b);

    // write data size
    DataSize := n.DataSize;
    s.WriteBuffer(DataSize, SizeOf(DataSize));

    // write data
    s.WriteBuffer(n.Data^, n.DataSize);

    // write children
    for i := 0 to n.ChildCount - 1 do WriteNode(n.Children[i]);

    // >
    b := GFDT_BLOCK_END;
    s.WriteBuffer(b, SizeOf(b));
  end;

begin
  // write signature
  sig := GF_DATA_TREE_SIG;
  s.WriteBuffer(sig, SizeOf(sig));

  // write tree
  WriteNode(FRoot);
end;

function TgfDataTree.LoadFromFile;
var
  s: TStream;
  
begin
  try
    s := TFileStream.Create(FileName, fmOpenRead);
    try
      Result := LoadFromStream(s);
    finally
      s.Free;
    end;
  except
    Result := False;
  end;
end;

procedure TgfDataTree.SaveToFile;
var
  s: TStream;
  
begin
  s := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(s);
  finally
    s.Free;
  end;
end;

end.
