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
unit dlgExeFormat;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmExeFormat }

  TfrmExeFormat = class(TForm)
    bOK: TButton;
    rg: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    procedure ApplyLanguagePack;
  end; 

var
  frmExeFormat: TfrmExeFormat;

implementation

{$R *.lfm}

uses LangPack;

{ TfrmExeFormat }

procedure TfrmExeFormat.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmExeFormat.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmExeFormat.ApplyLanguagePack;
begin
  Caption := lpGet('IF_TITLE');
  bOK.Caption := lpGet('B_OK');
end;

end.

