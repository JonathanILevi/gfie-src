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
unit ShellEx;

interface

uses
  {$IFDEF WINDOWS} Windows, Messages, {$ENDIF}
  FileUtil, Forms, LclIntf, LclType, SysUtils, Dialogs;

const
  // Open/Save dialog view modes (WPARAM)
  DVM_ICONS = $7029;
  DVM_LIST = $702B;
  DVM_DETAILS = $702C;
  DVM_THUMBS = $702D;
  DVM_TILES = $702E;

  dvm: array[0..4] of word = (DVM_ICONS, DVM_LIST, DVM_DETAILS,
    DVM_THUMBS, DVM_TILES);

// Displays a 'Browse for folder' dialog box
// Returns '' if the dialog was cancelled.
function BrowseForFolder(const Title, InitFolder: string): string;
// Sets the view mode of the listview in the specified Open/Save dialog
procedure DlgSetViewMode(Dialog: THandle; Value: Cardinal);

implementation

function BrowseForFolder;
var
  sdd: TSelectDirectoryDialog;

begin
  sdd := TSelectDirectoryDialog.Create(Application);
  try
    sdd.Title := Title;
    sdd.FileName := SysToUTF8(InitFolder);
    if sdd.Execute then Result := UTF8ToSys(sdd.FileName) else Result := '';
  finally
    sdd.Free;
  end;
end;

procedure DlgSetViewMode;
begin
{$IFDEF WINDOWS}
  SendMessage(GetDlgItem(GetParent(Dialog), $0461), WM_COMMAND, Value, 0);
{$ENDIF}
end;

end.

