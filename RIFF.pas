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
unit RIFF;
      
interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Math;

type
  TRIFFLocatedChunk = record
    Name: string;
    // start of data and data size
    Start: integer;
	Size: integer;
  end;

  // RIFF file handling
  TRIFFReader = class
  protected
    Stream: TStream;
    Map: array of TRIFFLocatedChunk;
  public
    // position of first byte after
    EndOfFile: integer;

    constructor Create(AStream: TStream); virtual;
    // ChunkName format: e.g. RIFF(ACON).LIST(fram).icon
    // Returns the Index'th occurrence (Index >= 1)
    function Seek(const ChunkName: string; Index: integer; Size: PInteger): boolean;
  end;

  TRIFFWriter = class
  protected
    Stream: TStream;
    // stack, Descend adds the starting position of chunk (with fourcc),
    // Ascend pops it add updates chunk length field
    ChunkStart: array of integer;
  public
    constructor Create(s: TStream); virtual;
    destructor Destroy; override;

    procedure WriteString(const s: string);
    procedure Descend(const FourCC: string);
    procedure Ascend;
  end;

implementation

// TRIFFReader

constructor TRIFFReader.Create;
type
  // RIFF and LIST chunks are folder chunks because they can contain others
  TFolderChunk = record
    Name: string;
    EndPos: integer;
  end;

var
  i, ChunkSize, EndOfChunk: integer;
  FourCC, FourCC2, Path, s: string;
  Folders: array of TFolderChunk;
  IsFolder: boolean;

begin
  Stream := AStream;

  // map the entire RIFF file
  SetLength(Folders, 0);
  SetLength(FourCC, 4);
  SetLength(FourCC2, 4);
  repeat
    // read four-character-code and chunk size
    Stream.ReadBuffer(FourCC[1], 4);
      IsFolder := (FourCC = 'RIFF') or (FourCC = 'LIST');

    Stream.ReadBuffer(ChunkSize, 4);
      EndOfChunk := Stream.Position + ChunkSize;
      if Odd(ChunkSize) then inc(EndOfChunk);

    // produce path string
    Path := '';
    for i := 0 to Length(Folders) - 1 do Path := Path + Folders[i].Name + '.';

    // register the chunk
    SetLength(Map, Length(Map) + 1);
    with Map[Length(Map) - 1] do
    begin
      Start := Stream.Position;
      Size := ChunkSize;

      // check whether it is a folder chunk or not
      if IsFolder then
      begin
        Stream.ReadBuffer(FourCC2[1], 4);
        s := FourCC + '(' + FourCC2 + ')';

        // add to list of folder chunks
        SetLength(Folders, Length(Folders) + 1);
        with Folders[Length(Folders) - 1] do
        begin
          Name := s;
          EndPos := EndOfChunk;
        end;
      end else
      begin
        Stream.Position := Min(Stream.Size, EndOfChunk);
        s := FourCC;
      end;
                         
      Name := Path + s;
    end; // register chunk

    // check whether we have reached the end of some folder (e.g. LIST) chunks
    while (Length(Folders) <> 0) and
      (Stream.Position >= Folders[Length(Folders) - 1].EndPos) do
        SetLength(Folders, Length(Folders) - 1);
  until (Length(Folders) = 0) or (Stream.Position >= Stream.Size);

  EndOfFile := Stream.Position;
end;

function TRIFFReader.Seek;
var
  i, NumFound: integer;
  
begin
  Result := False;
  If Index < 1 then Exit;

  // find the chunk in the list
  NumFound := 0;
  for i := 0 to Length(Map) - 1 do if Map[i].Name = ChunkName then
  begin
    inc(NumFound);
    if NumFound >= Index then
    begin
      Stream.Position := Map[i].Start;
      if Assigned(Size) then Size^ := Map[i].Size;

      Result := True;
      Exit;
    end; // found
  end; // for i
end;

// TRIFFWriter

constructor TRIFFWriter.Create;
begin
  Stream := s;
end;

destructor TRIFFWriter.Destroy;
begin
  while Length(ChunkStart) <> 0 do Ascend;

  inherited;
end;

procedure TRIFFWriter.WriteString;
begin
  Stream.WriteBuffer(s[1], Length(s));
end;

procedure TRIFFWriter.Descend;
begin
  // save starting position of chunk
  SetLength(ChunkStart, Length(ChunkStart) + 1);
  ChunkStart[Length(ChunkStart) - 1] := Stream.Position;

  // write four-character-code and reserve space for chunk length
  Stream.WriteBuffer(FourCC[1], 4);
  Stream.WriteBuffer(FourCC[1], 4);
end;

procedure TRIFFWriter.Ascend;
var
  pStart, pEnd, Size, dummy: integer;

begin
  if Length(ChunkStart) <> 0 then
  begin
    // update chunk length
    pStart := ChunkStart[Length(ChunkStart) - 1];
    pEnd := Stream.Position;
    Size := pEnd - pStart - 8;

    Stream.Position := pStart + 4;
      Stream.WriteBuffer(Size, 4);
    Stream.Position := pEnd;

    // pop from stack
    SetLength(ChunkStart, Length(ChunkStart) - 1);

    // pad if chunk size is odd
    dummy := 0;
    if Odd(Size) then Stream.WriteBuffer(dummy, 1);
  end;
end;

end.
