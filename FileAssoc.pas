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
// Handling file associations in registry
unit FileAssoc;

{$IFNDEF WINDOWS}
{$WARNING FileAssoc: unit is specific to Windows}
interface
implementation
{$ELSE}
interface

uses
  LclIntf, LclType, SysUtils, Registry;

type
  TFileAssoc = record
    TypeId: string; // type identifier, e.g. "MyAPP.icon"
    TypeDesc: string; // type description
    DefaultIcon: string; // e.g. %1
    cmdOpen: string; // e.g. "dummy.exe "%1""

    ddeUse: boolean;
    ddeMacro: string;
	ddeServerName: string;
	ddeTopic: string;
  end;

// This returns false if the program is not run in elevated mode.
function CanSetFileAssoc: boolean;

function GetFileAssoc(const Ext: string; out r: TFileAssoc): boolean;
procedure SetFileAssoc(const Ext: string; const r: TFileAssoc);
procedure UndoFileAssoc(const Ext: string);

implementation

function CanSetFileAssoc: boolean;
var
  reg: TRegistry;
begin
  Result := true;
  try
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;
      reg.OpenKey('\.gfietmp123', True);
      reg.WriteString('abcdef', '12345');
      reg.DeleteKey('\.gfietmp123');
    finally
      reg.Free;
    end;
  except
    Result := False;
  end;
end;

function GetFileAssoc;
var
  reg: TRegistry;
  s: string;

  function ReadDefStr(const Key: string; out Value: string): boolean;
  begin
    Result := reg.OpenKey(Key, False);
    if Result then Value := reg.ReadString('') else Value := '';
  end;

begin
  Result := False;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    // find registry entry for this extension
    if not ReadDefStr('\' + Ext, r.TypeId) or
      not ReadDefStr('\' + r.TypeId, r.TypeDesc) then Exit;

    // jump to description entry
    ReadDefStr('\' + r.TypeId + '\DefaultIcon', r.DefaultIcon);
    ReadDefStr('\' + r.TypeId + '\Shell\Open\Command', r.cmdOpen);

    s := '\' + r.TypeId + '\Shell\Open\DDEExec';
    r.ddeUse := reg.KeyExists(s);
    if r.ddeUse then
    begin
      ReadDefStr(s, r.ddeMacro);
      ReadDefStr(s + '\Application', r.ddeServerName);
      ReadDefStr(s + '\Topic', r.ddeTopic);
    end;

    // success
    Result := True;
  finally
    reg.Free;
  end;
end;

procedure SetFileAssoc;
var
  reg: TRegistry;
  Exists: boolean;
  rOld: TFileAssoc;
  s: string;
  
  procedure WriteDefStr(const Key, Value: string);
  begin
    reg.OpenKey(Key, True);
    reg.WriteString('', Value);
  end;

begin
  Exists := GetFileAssoc(Ext, rOld);
  if not (Exists and CompareMem(@r, @rOld, SizeOf(r))) then
  begin
    reg := TRegistry.Create;
    try
      reg.RootKey := HKEY_CLASSES_ROOT;

      // create an undo if able
      if r.TypeId <> rOld.TypeId then
        WriteDefStr('\' + Ext + '\RestorePoint', rOld.TypeId) else
        reg.DeleteKey('\' + Ext + '\RestorePoint');

      // write information
      WriteDefStr('\' + Ext, r.TypeId);
      WriteDefStr('\' + r.TypeId, r.TypeDesc);
      WriteDefStr('\' + r.TypeId + '\DefaultIcon', r.DefaultIcon);
      WriteDefStr('\' + r.TypeId + '\Shell\Open\Command', r.cmdOpen);

      s := '\' + r.TypeId + '\Shell\Open\DDEExec';
      if not r.ddeUse then reg.DeleteKey(s) else
      begin
        WriteDefStr(s, r.ddeMacro);
        WriteDefStr(s + '\Application', r.ddeServerName);
        WriteDefStr(s + '\Topic', r.ddeTopic);
      end;
    finally
      reg.Free;
    end;
  end; // if rOld <> r
end;

procedure UndoFileAssoc;
var
  reg: TRegistry;
  TypeId: string;

begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;

    // does a restore point exist?
    if reg.OpenKey('\' + Ext + '\RestorePoint', False) then
    begin
      TypeId := reg.ReadString('');
      if reg.OpenKey('\' + Ext, False) then reg.WriteString('', TypeId);
      reg.DeleteKey('\' + Ext + '\RestorePoint');
    end else
      reg.DeleteKey('\' + Ext);
  finally
    reg.Free;
  end;
end;
{$ENDIF}
end.
