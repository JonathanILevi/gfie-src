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
unit ResList;

// Object encapsulating a list of icon and cursor resources
// WARNING: Currently, saving to NE format is destructive!!

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF} LclIntf, LclType, LResources,
  SysUtils, Classes, DocClass, StreamEx;

type
  TResIconType2 = (ritPage, ritGroup);
const
  // First index: is it an icon(true) or cursor(false)?
  resIconType: array[boolean, TResIconType2] of string =
    (('1', '12'), ('3', '14'));

type
  TResFileType = (rftNone, rftExe, rftRes);
  TArrayOfInteger = array of integer;

const
  // Default extension for saving -- that's why .icl is default instead of .exe!
  rftDefaultExt: array[TResFileType] of string = ('', '.icl', '.res');
  AnyLanguage = $feed;

type
  // Valid resource names: the set of strings returned by resValidName
  TResEntry = class(TPersistent)
  private
    FData: Pointer;
    FDataSize: integer;

    procedure SetDataSize(const Value: integer);
  public
    _Type: string;
    Name: string;
    Language: word;

    property Data: Pointer read FData;
    property DataSize: integer read FDataSize write SetDataSize;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
    procedure Clear;
  end;

  TChangeIconOp = (ciDelete, ciNewLanguage);
  TExeFormat = (exeNone, exeNE, exePE);

  { TResList }

  TResList = class(TPersistent)
  private
    FItems: TList; // list of TResEntry;

    function GetEntryCount: integer;
    function GetEntry(Index: integer): TResEntry;

    // Load/save Windows resource files
    function LoadFromRes(const FileName: string): boolean;
    function SaveToRes(const FileName: string): boolean;
    // 16-bit executable files
    function LoadFromNE(const FileName: string): boolean;
    function SaveToNE(const FileName: string): boolean; // always destructive
    // 32-bit executable files
    function LoadFromPE(const FileName: string): boolean;
    function SaveToPE(const FileName: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property EntryCount: integer read GetEntryCount;
    property Entries[Index: integer]: TResEntry read GetEntry; default;

    // Language can be AnyLanguage
    function IndexOf(const AType, AName: string; ALanguage: word;
      CanCreate: boolean): integer;
    function NewEntry: TResEntry;
    procedure DeleteEntry(Index: integer);
    procedure Append(rl: TResList);

    // Looks for a free interval of resource ID's of the given Type and Language
    function GetFreeInterval(const AType: string; ALanguage: word;
      Count: integer): integer;
    // Get indexes of all the resource entries for individual resource pages
    // where e is the group resource entry
    function GetPages(e: TResEntry; out Index: TArrayOfInteger): boolean;
    // Get an icon or cursor
    function GetIconToFile(IsIcon: boolean; const AName: string; ALanguage: word;
      const fn: string): boolean;
    function GetIconToStream(IsIcon: boolean; const AName: string; ALanguage: word;
      s: TStream): boolean;
    function GetIconToDoc(IsIcon: boolean; const AName: string; ALanguage: word;
      Doc: TIconDoc): boolean;

    procedure SetResourceFromMemory(const AType: string; const AName: string;
      ALanguage: word; AData: Pointer; ADataSize: integer);
    // Set an icon or cursor
    procedure SetIconFromStream(IsIcon: boolean; const AName: string; ALanguage: word;
      s: TStream);
    procedure SetIconFromFile(IsIcon: boolean; const AName: string; ALanguage: word;
      const fn: string);

    // Delete icon/Set language
    function ChangeIcon(IsIcon: boolean; const AName: string; ALanguage: word;
      Op: TChangeIconOp; Param: integer): boolean;

    // Load/save
    function LoadFromFile(const FileName: string): TResFileType;
    // if ExeFormat is given, then forces saving in the given format
    // else, detects format of file (PE or NE)
    function SaveToFile(const FileName: string; FileType: TResFileType;
      ExeFormat: TExeFormat = exeNone): boolean;
  end;

// Detects file type from stream contents
function DetectResFileType(s: TStream): TResFileType;
// Detects file type from extension of filename
function DetectResFileTypeFromName(const fn: string): TResFileType;

function exeDetectFormat(const FileName: string): TExeFormat;
function CanHandlePE: boolean;
function resValidName(const s: string): string;

implementation

uses
  Math, Dialogs, bmExUtils, ICO, dlgDebug;

type
  IMAGE_DOS_HEADER = packed record
    e_magic: word;
    e_cblp: word;
    e_cp: word;
    e_crlc: word;
    e_cparhdr: word;
    e_minalloc: word;
    e_maxalloc: word;
    e_ss: word;
    e_sp: word;
    e_csum: word;
    e_ip: word;
    e_cs: word;
    e_lfarlc: word;
    e_ovno: word;
    e_res: array[0..3] of word;
    e_oemid: word;
    e_oeminfo: word;
    e_res2: array[0..9] of word;
    e_lfanew: longint;
  end;

  IMAGE_OS2_HEADER = packed record
    ne_magic: word;
    ne_ver: char;
    ne_rev: char;
    ne_enttab: word;
    ne_cbenttab: word;
    ne_crc: longint;
    ne_flags: word;
    ne_autodata: word;
    ne_heap: word;
    ne_stack: word;
    ne_csip: longint;
    ne_sssp: longint;
    ne_cseg: word;
    ne_cmod: word;
    ne_cbnrestab: word;
    ne_segtab: word;
    ne_rsrctab: word;
    ne_restab: word;
    ne_modtab: word;
    ne_imptab: word;
    ne_nrestab: word;
    ne_cmovent: word;
    ne_align: word;
    ne_cres: word;
    ne_exetyp: word;
    ne_flagsothers: word;
    ne_pretthunks: word;
    ne_psegrefbytes: word;
    ne_swaparea: word;
    ne_expver: word;
  end;

  TNAMEINFO = packed record
    Offset, Len, Flags, ID, Handle, Usage: word;
  end;

  // When stored as a resource, cursor hot spot is written to
  // the RT_CURSOR resource
//  PRES_ICONDIRENTRY = ^RES_ICONDIRENTRY;
  RES_ICONDIRENTRY = packed record
    d: packed record
      case Integer of
        0: (bWidth, bHeight, bColorCount, bReserved: byte;);
        1: (wWidth, wHeight: word;);
    end;
    wPlanes, wBitCount: word;
    dwBytesInRes: Cardinal;
    nID: word;
  end;

  PRES_ICONHEADER = ^RES_ICONHEADER;
  RES_ICONHEADER = packed record
    id: ICONDIR;
    ide: array[0..0] of RES_ICONDIRENTRY;
  end;

function exeDetect(Data: Pointer; Size: integer): boolean;
begin
  Result := (Size >= 2) and (PChar(Data)[0] = 'M') and (PChar(Data)[1] = 'Z');
end;

function resDetect(Data: Pointer; Size: integer): boolean;
begin
  Result := (Size >= 16) and
    (PIntegerArray(Data)[2] = $0000ffff) and
    (PIntegerArray(Data)[3] = $0000ffff);
end;

function DetectResFileType;
const
  MAX_HEADER_SIZE = 100;

var
  oldPos, HeaderSize: integer;
  Header: array[0..MAX_HEADER_SIZE-1] of byte;

begin
  Result := rftNone;
  try
    oldPos := s.Position;
    HeaderSize := s.Read(Header, MAX_HEADER_SIZE);
    s.Position := oldPos;

    if exeDetect(@Header, HeaderSize) then Result := rftExe else
    if resDetect(@Header, HeaderSize) then Result := rftRes;
  except
  end;
end;

function DetectResFileTypeFromName;
var
  Ext: string;

begin
  Ext := UpperCase(ExtractFileExt(fn));

  if (Ext = '.EXE') or (Ext = '.DLL') or (Ext = '.SCR') or (Ext = '.CPL') or
    (Ext = '.OCX') or (Ext = '.VBX') or (Ext = '.BPL') or (Ext = '.ICL') or
    (Ext = '.IL') then Result := rftExe else
  if Ext = '.RES' then Result := rftRes else
    Result := rftNone;
end;

function exeDetectFormat;
var
  s: TStream;
  dosh: IMAGE_DOS_HEADER;
  magic: string;

begin
  Result := exeNone;
  try
    s := TFileStream.Create(FileName, fmOpenRead);
    try
      s.ReadBuffer(dosh, SizeOf(dosh));
      s.Position := dosh.e_lfanew;
      SetLength(magic, 2);
      s.ReadBuffer(magic[1], 2);
    finally
      s.Free;
    end;

    if magic = 'NE' then Exit(exeNE);
    if magic = 'PE' then Exit(exePE);
  except
  end;
end;

function CanHandlePE: boolean;
begin
{$ifdef WINDOWS}
  Result := true;
{$else}
  Result := false;
{$endif}
end;

function resValidName;
var
  i: integer;
  
begin
  Result := UpperCase(s);
  for i := 1 to Length(Result) do
    if not (Result[i] in ['A'..'Z', '0'..'9', '_']) then Result[i] := '_';
  if Result = '0' then Result := '_0';
end;

// Misc

function ResNameToStr(sz: PChar): string;
begin
  if PtrUInt(sz) < $10000 then
    Result := IntToStr(PtrUInt(sz)) else
    Result := String(sz);
end;

function ResNameIsNumber(const s: string): boolean;
var
  i: integer;

begin
  Result := TryStrToInt(s, i) and (i >= 0) and (i < $10000);
end;

function StrToResNameW(const s: string): PWideChar;
begin
  if ResNameIsNumber(s) then
    PtrInt(Result) := StrToInt(s) else
    Result := PWideChar(WideString(s));
end;

// TResEntry

procedure TResEntry.SetDataSize;
begin
  if FDataSize <> Value then
  begin
    FDataSize := Value;
    FreeAndNilMem(FData);
    if DataSize > 0 then
      FData := GetMem(DataSize);
  end;
end;

constructor TResEntry.Create;
begin
  inherited;

  _Type := '';
  Name := '';
  Language := 0;

  FData := nil;
  FDataSize := 0;
  
  Clear;
end;

destructor TResEntry.Destroy;
begin
  DataSize := 0;
  
  inherited;
end;

procedure TResEntry.Assign;
var
  x: TResEntry;
  
begin
  if Src = Self then Exit;

  if Src is TResEntry then
  begin
    x := Src as TResEntry;

    _Type := x._Type;
    Name := x.Name;
    Language := x.Language;
    DataSize := x.DataSize;
    Move(x.Data^, Data^, DataSize);
  end else
    inherited;
end;

procedure TResEntry.Clear;
begin
  _Type := '';
  Name := '';
  Language := 0;
  DataSize := 0;
end;

// For sorting a list of TResEntry
function TResEntry_Compare(Item1, Item2: Pointer): Integer;
var
  a: TResEntry absolute Item1;
  b: TResEntry absolute Item2;

begin
  if a._Type < b._Type then Exit(-1);
  if a._Type > b._Type then Exit(1);
  if a.Name < b.Name then Exit(-1);
  if a.Name > b.Name then Exit(1);
  if a.Language < b.Language then Exit(-1);
  if a.Language > b.Language then Exit(1);
  Result := 0;
end;

// TResList

function TResList.GetEntryCount: integer;
begin
  Result := FItems.Count;
end;

function TResList.GetEntry(Index: integer): TResEntry;
begin
  Result := FItems[Index];
end;

constructor TResList.Create;
begin
  inherited;
  
  FItems := TList.Create;
end;

destructor TResList.Destroy;
begin
  Clear;
  FItems.Free;

  inherited;
end;

procedure TResList.Clear;
var
  i: integer;

begin
  for i := 0 to FItems.Count - 1 do Entries[i].Free;
  FItems.Clear;
end;

function TResList.IndexOf(const AType, AName: string; ALanguage: word;
  CanCreate: boolean): integer;
begin
  for Result := 0 to EntryCount - 1 do with Entries[Result] do
    if (_Type = AType) and (Name = AName) and
      ((Language = ALanguage) or (ALanguage = AnyLanguage)) then
  Exit;

  if CanCreate then
  begin
    with NewEntry do
    begin
      _Type := AType;
      Name := AName;
      Language := ALanguage;
    end;
    Result := EntryCount - 1;
  end else
    Result := -1;
end;

function TResList.NewEntry: TResEntry;
begin
  Result := TResEntry.Create;
  FItems.Add(Result);
end;

procedure TResList.DeleteEntry(Index: integer);
begin
  Entries[Index].Free;
  FItems.Delete(Index);
end;

procedure TResList.Append(rl: TResList);
var
  i: integer;
  
begin
  for i := 0 to rl.EntryCount - 1 do NewEntry.Assign(rl[i]);
end;

function TResList.GetFreeInterval(const AType: string; ALanguage: word;
  Count: integer): integer;
var
  i: integer;

begin
  // initialize start of interval
  Result := -1;

  for i := 1 to $ffff do
    if IndexOf(AType, IntToStr(i), ALanguage, False) < 0 then
  begin
    // new free place
    if Result < 0 then Result := i;
    // found enough free places?
    if i - Result + 1 >= Count then Exit;
  end else
    // occupied place
    Result := -1;

  // If we haven't exited sofar, there is no free interval
  Result := -1;
end;

function TResList.GetPages(e: TResEntry; out Index: TArrayOfInteger): boolean;
var
  rih: PRES_ICONHEADER;
  IsIcon: boolean;
  i: integer;
  id: string;

begin
  Result := False;

  // heuristic: idCount must not be too big
  rih := e.Data;
  if rih.id.idCount > 256 then Exit;

  IsIcon := (e._Type = resIconType[True, ritGroup]);

  SetLength(Index, rih.id.idCount);
  for i := 0 to rih.id.idCount - 1 do
  begin
    id := IntToStr(rih.ide[i].nID);

    // first attempt: use same language as icon directory
    Index[i] := IndexOf(resIconType[IsIcon, ritPage],
      id, e.Language, False);

    if Index[i] < 0 then
    // second attempt: use any language
    begin
      Index[i] := IndexOf(resIconType[IsIcon, ritPage],
        id, AnyLanguage, False);
      // failed?
      if Index[i] < 0 then Exit;
    end;
  end; // for i

  Result := True;
end;

function TResList.GetIconToDoc(IsIcon: boolean; const AName: string; ALanguage: word;
  Doc: TIconDoc): boolean;
var
  s: TMemoryStream;

begin
  Result := False;
  s := TMemoryStream.Create;
  try
    if not GetIconToStream(IsIcon, AName, ALanguage, s) then
      Exit;

    s.Position := 0;
    icoLoadFromStream(Doc, s);
  finally
    s.Free;
  end;

  // success
  Result := True;
end;

function TResList.GetIconToFile(IsIcon: boolean; const AName: string;
  ALanguage: word; const fn: string): boolean;
var
  s: TFileStream;

begin
  Result := False;
  s := TFileStream.Create(fn, fmCreate);
  try
    Result := GetIconToStream(IsIcon, AName, ALanguage, s);
  finally
    s.Free;
  end;
end;

function TResList.GetIconToStream(IsIcon: boolean; const AName: string; ALanguage: word;
  s: TStream): boolean;
var
  i, j, iDir, Size, MaxSize: integer;
  Offset: Cardinal;
  e: TResEntry;
  p: Pointer;
  rih: PRES_ICONHEADER;
  ide: ICONDIRENTRY;
  SubIndex, IconPageBytes: TArrayOfInteger;

begin
  Result := False;

  // search for main icon directory resource
  iDir := IndexOf(resIconType[IsIcon, ritGroup], AName, ALanguage, False);
  if iDir < 0 then Exit;
  e := Entries[iDir];
  {$IFDEF GFIEDEBUG}
  Log('Loading resource icon/cursor '+e._Type+' '+e.Name+', size '+IntToStr(e.DataSize));
  {$ENDIF}

  // get subindexes for page resources
  if not GetPages(e, SubIndex) then Exit;
  {$IFDEF GFIEDEBUG}
  Log(IntToStr(Length(SubIndex))+' pages');
  {$ENDIF}

  rih := e.Data;

  // convert resource directory
  s.WriteBuffer(rih.id, SizeOf(rih.id));

  Offset := SizeOf(ICONDIR) + Length(SubIndex) * SizeOf(ICONDIRENTRY);
  SetLength(IconPageBytes, Length(SubIndex));
  for i := 0 to Length(SubIndex) - 1 do
  with rih.ide[i] do
  begin
    FillChar(ide, SizeOf(ide), 0);
    ide.dwBytesInRes := dwBytesInRes;

    if IsIcon then
    begin
      ide.bWidth := d.bWidth;
      ide.bHeight := d.bHeight;
      ide.bColorCount := d.bColorCount;
    end else
    begin
      ide.bWidth := d.wWidth;
      ide.bHeight := d.wHeight div 2;
      // - 4, because hot spot (2 words) is stored along bitmaps in cursor resources
      dec(ide.dwBytesInRes, 4);

      // we will load the cursor hot spot coordinates later
    end;

    // common
    ide.dwImageOffset := Offset;
    IconPageBytes[i] := ide.dwBytesInRes;
    inc(Offset, ide.dwBytesInRes);

    // write
    s.WriteBuffer(ide, SizeOf(ide));
  end; // for i := pages

  // copy pages
  for i := 0 to Length(SubIndex) - 1 do
  begin
    e := Entries[SubIndex[i]];
    p := e.Data;
    Size := IconPageBytes[i];
    MaxSize := e.DataSize;

    // if cursor -> load hot spot from Data
    if not IsIcon then
    begin
      j := s.Position;
      s.Position := SizeOf(ICONDIR) + SizeOf(ICONDIRENTRY)*i + 4;
        // hot spot X and Y are stored in Data in words -> 4 bytes total
        s.WriteBuffer(p^, 4);
        inc(PByte(p), 4);
        dec(MaxSize, 4);
      s.Position := j;
    end;

    // write data bytes
    s.WriteBuffer(p^, Min(Size, MaxSize));
  end; // for i

  // success
  Result := True;
end;

procedure TResList.SetResourceFromMemory(const AType: string; const AName: string;
      ALanguage: word; AData: Pointer; ADataSize: integer);
var
  e: TResEntry;
begin
  e := NewEntry;
  e._Type := AType;
  e.Name := AName;
  e.Language := ALanguage;
  e.DataSize := ADataSize;
  Move(AData^, e.Data^, ADataSize);
end;

procedure TResList.SetIconFromStream(IsIcon: boolean; const AName: string;
  ALanguage: word; s: TStream);
var
  i, SubIndex: integer;
  p: Pointer;
  ZeroPoint: Cardinal;
  eMain, e: TResEntry;
  rih: PRES_ICONHEADER;
  id: ICONDIR;
  ide: array of ICONDIRENTRY;

begin
  ZeroPoint := s.Position;
  s.ReadBuffer(id, SizeOf(id));
  if id.idCount = 0 then
    Exit;

  // delete existing icon
  ChangeIcon(IsIcon, AName, ALanguage, ciDelete, 0);

  SetLength(ide, id.idCount);
  s.ReadBuffer(ide[0], id.idCount * SizeOf(ICONDIRENTRY));

  // create resource icon header
  eMain := NewEntry;
    eMain._Type := resIconType[IsIcon, ritGroup];
    eMain.Name := AName;
    eMain.Language := ALanguage;
    eMain.DataSize := SizeOf(ICONDIR) + SizeOf(RES_ICONDIRENTRY) * id.idCount;
  FillChar(eMain.Data^, eMain.DataSize, 0);
  rih := eMain.Data;

  with rih.id do
  begin
    idReserved := 0; idType := IfThen(IsIcon, ID_ICO, ID_CUR);
    idCount := id.idCount;
  end;

  // allocate space for RT_ICON/RT_CURSOR resources
  SubIndex := GetFreeInterval(resIconType[IsIcon, ritPage], ALanguage, id.idCount);

  // read subresources
  for i := 0 to id.idCount - 1 do
  begin
    e := NewEntry;
      e._Type := resIconType[IsIcon, ritPage];
      e.Name := IntToStr(SubIndex + i);
      e.Language := ALanguage;
      e.DataSize := Byte(IfThen(IsIcon, 0, 4)) + ide[i].dwBytesInRes;
    p := e.Data;

    if not IsIcon then
    // save cursor hot spot to subresource
    begin
      PWord(p)^ := ide[i].wPlanes_XHotSpot; inc(PWord(p));
      PWord(p)^ := ide[i].wBitCount_YHotSpot; inc(PWord(p));
    end;

    // save image bytes to memory stream
    s.Position := Int64(ZeroPoint) + ide[i].dwImageOffset;
    s.ReadBuffer(p^, ide[i].dwBytesInRes);

    // update RES_ICONDIRENTRY structure in main (header) resource
    with rih.ide[i] do
    begin
      dwBytesInRes := e.DataSize;
      nID := SubIndex + i;

      if IsIcon then
      begin
        d.bWidth := ide[i].bWidth;
        d.bHeight := ide[i].bHeight;
        d.bColorCount := ide[i].bColorCount;
      end else
      begin
        d.wWidth := ide[i].bWidth;
        d.wHeight := ide[i].bHeight;
      end;
    end; // with RES_ICONDIRENTRY
  end; // for i := Pages
end;

procedure TResList.SetIconFromFile(IsIcon: boolean; const AName: string;
  ALanguage: word; const fn: string);
var
  s: TFileStream;

begin
  s := TFileStream.Create(fn, fmOpenRead);
  try
    SetIconFromStream(IsIcon, AName, ALanguage, s);
  finally
    s.Free;
  end;
end;

function TResList.ChangeIcon(IsIcon: boolean; const AName: string;
  ALanguage: word; Op: TChangeIconOp; Param: integer): boolean;
var
  i: integer;
  e: TResEntry;
  rih: PRES_ICONHEADER;

  procedure OperateOn(Index: integer);
  begin
    if Index >= 0 then
    case Op of
      ciDelete: DeleteEntry(Index);
      ciNewLanguage: Entries[Index].Language := Param;
    end; // case Op
  end;

begin
  Result := False;

  // search for main icon directory resource
  i := IndexOf(resIconType[IsIcon, ritGroup], AName, ALanguage, False);
  if i < 0 then Exit;
  e := Entries[i];

  // verify header
  rih := e.Data;
  if (rih.id.idReserved <> 0) or (rih.id.idCount > 256) then Exit;

  // operate on subresources
  for i := 0 to rih.id.idCount - 1 do with rih.ide[i] do
    OperateOn(IndexOf(resIconType[IsIcon, ritPage], IntToStr(nID), ALanguage, False));
  // operate on main resource
  OperateOn(IndexOf(resIconType[IsIcon, ritGroup], AName, ALanguage, False));

  // success
  Result := True;
end;

//****************************************
// Loading/Saving resources from/to RES files

// read a type or name string from a resource file
function resReadString(s: TStream): string;
var
  w: word;

begin
  s.ReadBuffer(w, 2);

  if w = $ffff then
  // integral identifier
  begin
    s.ReadBuffer(w, 2);
    Result := IntToStr(w);
  end else
  // text ID
  begin
    Result := '';
    while w <> 0 do
    begin
      Result := Result + Char(w);
      s.ReadBuffer(w, 2);
    end;
    
    // padding
    if not Odd(Length(Result)) then s.Seek(2, soFromCurrent);
  end;
end;

// write a type or name string to a resource file
procedure resWriteString(st: TStream; const s: string);
var
  i: integer;
  w: word;

begin
  if ResNameIsNumber(s) then
  begin
    w := $ffff; st.WriteBuffer(w, 2);
    w := StrToInt(s); st.WriteBuffer(w, 2);
  end else
  begin
    for i := 1 to Length(s) do
    begin
      w := Word(s[i]); st.WriteBuffer(w, 2);
    end;
    // trailing zero and padding
    st.Seek(2 + PadTo4((Length(s) + 1) * 2), soFromCurrent);
  end;
end;

type
  TResInfoTail = packed record
    DataVersion: Cardinal;
    MemoryFlags, Language: word;
    Version, Characteristics: Cardinal;
  end;

procedure resReadResource(s: TStream; e: TResEntry);
var
  i: integer;
  rit: TResInfoTail;

begin
  // read header
  s.ReadBuffer(i, 4);
  e.DataSize := i;
  s.Seek(4, soFromCurrent); // HeaderSize is not important
  e._Type := resReadString(s);
  e.Name := resReadString(s);
  s.ReadBuffer(rit, SizeOf(rit)); // additional header fields
  e.Language := rit.Language;

  // read data
  s.ReadBuffer(e.Data^, e.DataSize);
  s.Seek(PadTo4(e.DataSize), soFromCurrent);
end;

procedure resWriteResource(s: TStream; const _Type, Name: string;
  Language: word; Data: Pointer; Size: integer);

var
  pHeader, pData: integer;
  dw: Cardinal;
  rit: TResInfoTail;

begin
  FillChar(rit, SizeOf(rit), 0);
  rit.MemoryFlags := $1050; // MOVEABLE, PRELOAD, DISCARDABLE
  rit.Language := Language;

  // header
  pHeader := s.Position;
    dw := Size; s.WriteBuffer(dw, 4); // DataSize
    s.Seek(4, soFromCurrent); // HeaderSize written later
    resWriteString(s, _Type);
    resWriteString(s, Name);
    s.WriteBuffer(rit, SizeOf(rit)); // additional header fields
  pData := s.Position;
    s.Position := pHeader + 4;
    dw := pData - pHeader; s.WriteBuffer(dw, 4);
  s.Position := pData;

  // data, write padding as well
  s.WriteBuffer(Data^, Size);
  s.Seek(PadTo4(Size), soFromCurrent);
end;

function TResList.LoadFromRes(const FileName: string): boolean;
var
  b: boolean;
  s: TStream;
  e: TResEntry;

begin
  Result := False;
  try
    s := TFileStream.Create(FileName, fmOpenRead);
    try
      e := TResEntry.Create;
      try
        resReadResource(s, e);
        b := (e._Type = '0') and (e.Name = '0');
      finally
        e.Free;
      end;
      if not b then Exit;

      Clear;

      while s.Position < s.Size do
      begin
        e := TResEntry.Create;
        resReadResource(s, e);

        if e.DataSize <> 0 then FItems.Add(e) else
        begin
          // Heuristics: last resource? padding zeroes?
          b := (e._Type = '') and (e.Name = '') and (e.Language = 0);
          e.Free;

          if b then Break;
        end;
      end; // while not eof
    finally
      s.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

function TResList.SaveToRes(const FileName: string): boolean;
var
  i: integer;
  s: TStream;

begin
  s := TFileStream.Create(FileName, fmCreate);
  try
    // the null resource serves as a header here
    resWriteResource(s, '0', '0', 0, nil, 0);

    for i := 0 to EntryCount - 1 do with Entries[i] do
      resWriteResource(s, _Type, Name, Language, Data, DataSize);
  finally
    s.Free;
  end;

  Result := True;
end;

//****************************************
// Loading resources from NE files

function TResList.LoadFromNE(const FileName: string): boolean;
var
  s: TStream;
  dosh: IMAGE_DOS_HEADER;
  neh: IMAGE_OS2_HEADER;
  ni: TNAMEINFO;
  i, oldPos, startOffset, rsrcOffset, alignShift, rtReserved: integer;
  rtTypeID, rtResourceCount: word;
  Entry: TResEntry;
  ResourceType, ResourceName: string;

begin
  Result := False;
  try
    Clear;

    s := TFileStream.Create(FileName, fmOpenRead);
    try
      // Read DOS header
      s.ReadBuffer(dosh, SizeOf(dosh));
      startOffset := dosh.e_lfanew;
      s.Position := startOffset;
      // Read NE header
      s.ReadBuffer(neh, SizeOf(neh));
      rsrcOffset := startOffset + neh.ne_rsrctab;
      s.Position := rsrcOffset;
      // Read resource table
      alignShift := 0;
      s.ReadBuffer(alignShift, 2);
      if (alignShift < 0) or (alignShift > 31) then
        Exit;
      alignShift := 1 shl alignShift;

      // For all resource types
      while True do
      begin
      	// Read resource type
        s.ReadBuffer(rtTypeID, SizeOf(rtTypeID));

      	// End of table?
      	if rtTypeID = 0 then Break;
        // Parse resource type as string
      	if rtTypeID >= $8000 then
          ResourceType := IntToStr(rtTypeID - $8000) else
        begin
          oldPos := s.Position;
            s.Position := rsrcOffset + Integer(rtTypeID);
            ResourceType := ReadPascalString(s);
          s.Position := oldPos;
        end;

      	// Read number of resources
      	s.ReadBuffer(rtResourceCount, SizeOf(rtResourceCount));
      	s.ReadBuffer(rtReserved, SizeOf(rtReserved));

        for i := 1 to rtResourceCount do
        begin
          // Read nameinfo struct
          s.ReadBuffer(ni, SizeOf(ni));

          // Parse resource name as string
          if ni.ID >= $8000 then
            ResourceName := IntToStr(ni.ID - $8000) else
          begin
            oldPos := s.Position;
              s.Position := rsrcOffset + Integer(ni.ID);
              ResourceName := ReadPascalString(s);
            s.Position := oldPos;
          end;

          // duplicate entry?
          if IndexOf(ResourceType, ResourceName, 0, false) >= 0 then
            Continue; // drop

          // Create entry
          Entry := NewEntry;
          Entry._Type := ResourceType;
          Entry.Name := ResourceName;

          // Read resource contents
          oldPos := s.Position;
            s.Position := alignShift * Integer(ni.Offset);
            Entry.DataSize := Min(alignShift * Integer(ni.Len), s.Size - s.Position);
            s.ReadBuffer(Entry.Data^, Entry.DataSize);
          s.Position := oldPos;
        end; // for i (read individual resources)
      end; // while read resource types
    finally
      s.Free;
    end;

    // success
    Result := True;
  except
  end;
end;

function TResList.SaveToNE(const FileName: string): boolean;
const
  alignShift = 10;
  BlockSize = 1 shl alignShift;

var
  i, p, Start, _End: integer;
  w, rtTypeID, rtResourceCount: word;
  Stub: string;
  s, sEntries, sNames, sData: TStream;
  rsrcTableOffset, sEntries_Offset, sNames_Offset, sData_Offset: integer;
  Zero: packed array[0..1023] of byte;
  ni: TNAMEINFO;
  Entry: TResEntry;
  lRes: TList;
  Relocate_sNames, Relocate_sData: TList;

begin
  Result := False;
  try
    FillChar(Zero, SizeOf(Zero), 0);

    s := TFileStream.Create(FileName, fmCreate);
    try
      // write 16-bit stub:
      // IMAGE_DOS_HEADER, stub program and IMAGE_OS2_HEADER
      Stub := LazarusResources.Find('iclstub16').Value;
      s.WriteBuffer(Stub[1], Length(Stub));

      // sort resources into a list
      lRes := TList.Create;
      try
        lRes.Assign(FItems);
        lRes.Sort(@TResEntry_Compare);

        // write resource table
        rsrcTableOffset := s.Position;
        // write align shift
        w := alignShift;
        s.WriteBuffer(w, SizeOf(w));

        // we will use three streams:
        // - resource type and name entries
        // - resource type and name strings
        // - resource data
        sEntries := TMemoryStream.Create;
        sNames := TMemoryStream.Create;
        Relocate_sNames := TList.Create;
        sData := TMemoryStream.Create;
        Relocate_sData := TList.Create;
        try
          // write entry for all resources
          Start := 0;
          for _End := 0 to lRes.Count do
            if (_End = lRes.Count) or ((_End > 0) and
              (TResEntry(lRes[_End])._Type <> TResEntry(lRes[_End-1])._Type)) then
          begin
            Entry := lRes[Start];

            // Make resource type ID from string name
            if ResNameIsNumber(Entry._Type) then
              rtTypeID := $8000 + StrToInt(Entry._Type) else
            begin
              Relocate_sNames.Add(Pointer(sEntries.Position));
              rtTypeID := sNames.Position;
              WritePascalString(sNames, Entry._Type);
            end;

            // write resource type
            sEntries.WriteBuffer(rtTypeID, SizeOf(rtTypeID));

            // write number of resources
            rtResourceCount := _End - Start;
            sEntries.WriteBuffer(rtResourceCount, SizeOf(rtResourceCount));
            sEntries.WriteBuffer(Zero, 4);

            // write headers and data for individual resources
            for i := Start to _End - 1 do
            begin
              Entry := lRes[i];

              // prepare NAMEINFO struct
              Relocate_sData.Add(Pointer(sEntries.Position));
              ni.Offset := sData.Position div BlockSize;
              ni.Len := (Entry.DataSize + BlockSize - 1) div BlockSize;
              ni.Flags := $70;
              // Make resource name ID from string name
              if ResNameIsNumber(Entry.Name) then
                ni.ID := $8000 + StrToInt(Entry.Name) else
              begin
                Relocate_sNames.Add(Pointer(sEntries.Position + 6));
                ni.ID := sNames.Position;
                WritePascalString(sNames, Entry.Name);
              end;
              ni.Handle := 0;
              ni.Usage := 0;

              // write nameinfo struct
              sEntries.WriteBuffer(ni, SizeOf(ni));

              // Write resource contents to sData
              sData.WriteBuffer(Entry.Data^, Entry.DataSize);
              // Write padding
              if sData.Position mod BlockSize <> 0 then
                sData.WriteBuffer(Zero,
                  BlockSize - sData.Position mod BlockSize);
            end; // for i (write individual resources)

            // start a new block of resources of the same type
            Start := _End;
          end; // for resources

          // write End Of Table signs
          sEntries.WriteBuffer(Zero, SizeOf(rtTypeID));
          sNames.WriteBuffer(Zero, 0);

          // write memory stream contents to output stream
          sEntries_Offset := s.Position;
          sEntries.Position := 0;
          s.CopyFrom(sEntries, sEntries.Size);
          sNames_Offset := s.Position;
          sNames.Position := 0;
          s.CopyFrom(sNames, sNames.Size);
          // write padding before resource data
          if s.Position mod BlockSize <> 0 then
            s.WriteBuffer(Zero, BlockSize - s.Position mod BlockSize);
          sData_Offset := s.Position;
          sData.Position := 0;
          s.CopyFrom(sData, sData.Size);

          // relocate offsets
          for i := 0 to Relocate_sNames.Count - 1 do
          begin
            p := sEntries_Offset + PtrInt(Relocate_sNames[i]);
            s.Position := p;
            s.ReadBuffer(w, 2);
            s.Position := p;
            w += sNames_Offset - rsrcTableOffset;
            s.WriteBuffer(w, 2);
          end;
          for i := 0 to Relocate_sData.Count - 1 do
          begin
            p := sEntries_Offset + PtrInt(Relocate_sData[i]);
            s.Position := p;
            s.ReadBuffer(w, 2);
            s.Position := p;
            w += sData_Offset div BlockSize;
            s.WriteBuffer(w, 2);
          end;
        finally
          sEntries.Free;
          sNames.Free;
          Relocate_sNames.Free;
          sData.Free;
          Relocate_sData.Free;
        end;
      finally
        lRes.Free;
      end;
    finally
      s.Free;
    end;

    Result := True;
  except
  end;
end;

//****************************************
// Loading resources from PE files

{$ifdef windows}
function EasyLoadResource(hModule: THandle; szType, szName: PChar;
  Language: word; out Data: Pointer; out Size: integer): boolean;
var
  hInfo, hData: THandle;

begin
  Result := False;
  Data := nil;
  Size := 0;

  hInfo := FindResourceEx(hModule, szType, szName, Language);
  if hInfo = 0 then Exit;
  Size := SizeofResource(hModule, hInfo);
  if Size = 0 then Exit;
  hData := LoadResource(hModule, hInfo);
  if hData = 0 then Exit;
  Data := LockResource(hData);
  if Data = nil then Exit;

  // success
  Result := True;
end;

function peEnumLanguages(hModule: THandle; szType, szName: PChar;
  Language: word; lParam: LPARAM): boolean; stdcall;
var
  Data: Pointer;
  Size: integer;
  e: TResEntry;

begin
  if EasyLoadResource(hModule, szType, szName, Language, Data, Size) then
  begin
    e := TResList(lParam).NewEntry;

    e._Type := ResNameToStr(szType);
    e.Name := ResNameToStr(szName);
    e.Language := Language;
    e.DataSize := Size;
    Move(Data^, e.Data^, Size);
  end;

  Result := True;
end;

function peEnumNames(hModule: THandle; szType, szName: PChar;
  lParam: LPARAM): boolean; stdcall;
begin
  EnumResourceLanguages(hModule, szType, szName, @peEnumLanguages, lParam);
  Result := True;
end;

function peEnumTypes(hModule: THandle; szType: PChar;
  lParam: LPARAM): boolean; stdcall;
begin
  EnumResourceNames(hModule, szType, @peEnumNames, lParam);
  Result := True;
end;
{$endif} // if windows

function TResList.LoadFromPE(const FileName: string): boolean;
{$ifdef windows}
var
  hModule: THandle;
{$endif}
begin
  Result := False;
{$ifdef windows}
  try
    hModule := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
    if hModule = 0 then Exit;

    try
      Clear;
      // enumerate all resources found in the executable file
      EnumResourceTypes(hModule, @peEnumTypes, LONG_PTR(Self));
    finally
      FreeLibrary(hModule);
    end;

    // success
    Result := True;
  except
  end;
{$endif}
end;

//****************************************

{$ifdef windows}
function LastErrorString: string;
begin
  setlength(result,200);
        setlength(result, FormatMessage(
     FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS,
     nil,
     getlasterror,
     0,
     @result[1],
     200,
     nil));
end;
{$endif}

function TResList.SaveToPE(const FileName: string): boolean;
{$ifdef windows}
var
  i: integer;
  hUpdate: THandle;
  Stub: string;
  s: TFileStream;
  EndUpdateSuccess: boolean;
{$endif}
begin
  Result := False;
{$ifdef windows}
  try
    if not FileExists(FileName) then
    begin
      s := TFileStream.Create(FileName, fmCreate);
      try
        Stub := LazarusResources.Find('iclstub32').Value;
        s.WriteBuffer(Stub[1], Length(Stub));
      finally
        s.Free;
      end;
    end;

    // TODO throw an error; warn the user if total resource size is too big
    // (BeginUpdateResourceW does not like to write too many resources >:( )
    hUpdate := BeginUpdateResourceW(PWideChar(WideString(FileName)), True);
    if hUpdate = 0 then Exit;
    try
      for i := 0 to EntryCount - 1 do
      with Entries[i] do
        if not UpdateResourceW(hUpdate, StrToResNameW(_Type), StrToResNameW(Name),
          Language, Data, DataSize) then Exit;
    finally
      EndUpdateSuccess := EndUpdateResourceW(hUpdate, False);
    end;

    // success
    Result := EndUpdateSuccess;
  except
  end;
{$endif}
end;

//****************************************
// Loading resources from files

function TResList.LoadFromFile(const FileName: string): TResFileType;
begin
  // Trick: we do not have to use DetectResFileType!
  Result := rftNone;
  try
    case exeDetectFormat(FileName) of
      exeNE: if LoadFromNE(FileName) then Result := rftExe;
      exePE: if LoadFromPE(FileName) then Result := rftExe;
      exeNone: if LoadFromRes(FileName) then Result := rftRes;
    end;
  except
  end;
end;

function TResList.SaveToFile(const FileName: string; FileType: TResFileType;
  ExeFormat: TExeFormat): boolean;
begin
  Result := False;
  try
    case FileType of
      rftExe: begin
        if ExeFormat = exeNone then
        begin
          ExeFormat := exeDetectFormat(FileName);
          if ExeFormat = exeNone then Exit;
        end;

        case ExeFormat of
          exeNE: Result := SaveToNE(FileName);
          exePE: Result := SaveToPE(FileName);
        end;
      end;

      rftRes: Result := SaveToRes(FileName);
    end;
  except
  end;
end;

end.

