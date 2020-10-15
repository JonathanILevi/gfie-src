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
unit DdeServer;

{$IFNDEF WINDOWS}
{$WARNING DdeServer: unit is specific to Windows}
interface
implementation
{$ELSE}
interface

uses
  Windows, Messages, LclIntf, LclType, SysUtils, ieShared, DocClass, Main;

const
  sDDEServerName = 'GFIEPro';
  sDDETopic = 'System';

var
  ddeInited: boolean = False;
  ddeInst: Cardinal;
  ddeServerName: THandle;
  ddeTopic: THandle;

implementation

procedure ddeParseString(Message: PChar; var cmd, param: string);
var
  p1, p2, p3, p4: PChar;
  
begin
  // text between [ and ( : cmd
  p1 := FindInPChar(Message, '[');
  inc(p1);
  p2 := FindInPChar(p1, '(');

  // text between " and " : param
  p3 := FindInPChar(p2, '"');
  inc(p3);
  p4 := FindInPChar(p3, '"');

  SetLength(cmd, p2-p1);
  Move(p1^, cmd[1], Length(cmd));
  cmd := UpperCase(cmd);
  
  SetLength(param, p4-p3);
  Move(p3^, param[1], Length(param));
end;

function ddeCallback(uType, uFmt: UINT; hConv, hsz1, hsz2, hData: THandle;
  dwData1, dwData2: DWORD): THandle; stdcall;
var
  szExecute: array of char;
  cmd, param: string;

begin
  Result := 0;
  
  case uType of
    XTYP_CONNECT: Result := Ord((hsz1 = ddeTopic) and (hsz2 = ddeServerName));
    XTYP_EXECUTE: begin
      SetLength(szExecute, DdeGetData(hData, nil, 0, 0) + 10);
      FillChar(szExecute[0], Length(szExecute), #0);
      DdeGetData(hData, @szExecute[0], Length(szExecute), 0);
      ddeParseString(@szExecute[0], cmd, param);
      if cmd = 'OPEN' then frmMain.DoOpen(param);
    end;
  end;
end;

const
  APPCMD_FILTERINITS = $20;

initialization
  if DdeInitialize(@ddeInst, @ddeCallback, APPCMD_FILTERINITS, 0) = 0 then
  begin
    ddeServerName := DdeCreateStringHandle(ddeInst, sDDEServerName, CP_WINANSI);
    ddeTopic := DdeCreateStringHandle(ddeInst, sDDETopic, CP_WINANSI);

    // register server name
    DdeNameService(ddeInst, ddeServerName, 0, DNS_REGISTER);

    // success
    ddeInited := True;
  end;
finalization
  if ddeInited then
  begin
    ddeInited := False;

    DdeFreeStringHandle(ddeInst, ddeServerName);
    DdeFreeStringHandle(ddeInst, ddeTopic);
    DdeUninitialize(ddeInst);
  end;
{$ENDIF}
end.
