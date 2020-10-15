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
unit dlgResProp;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, Locales, dlgDebug;

const
  ID_CUSTOM_LANGUAGE = 1;

type

  { TfrmResProp }

  TfrmResProp = class(TForm)
    cbType: TComboBox;
    lName: TLabel;
    lLanguage: TLabel;
    eName: TEdit;
    lType: TLabel;
    neLanguage: TNumberEdit;
    cbLanguage: TComboBox;
    bOK: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure neLanguageChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    LangIDUpdating: integer;
    procedure ApplyLanguagePack;
  end;

var
  frmResProp: TfrmResProp;

implementation

{$R *.lfm}

uses LangPack, dlgLib;

procedure TfrmResProp.FormCreate(Sender: TObject);
var
  i: integer;

begin
  LangIDUpdating := 0;

  // load locales to cb
  cbLanguage.Sorted := True;
  for i := 0 to Length(WindowsLocales) - 1 do
    cbLanguage.Items.AddObject(WindowsLocales[i].Name,
    	TObject(WindowsLocales[i].ID));
  cbLanguage.Sorted := False;

  // add some basic locales: 'Custom' and 'Neutral'
  cbLanguage.Items.InsertObject(0, '', TObject(ID_CUSTOM_LANGUAGE));
  cbLanguage.Items.InsertObject(1, '', TObject(0));

  ApplyLanguagePack;
  if VerboseMode then Log('TfrmResProp created');
end;

procedure TfrmResProp.cbLanguageChange(Sender: TObject);
var
  i: integer;

begin
  i := PtrInt(cbLanguage.Items.Objects[cbLanguage.ItemIndex]);
  if i <> ID_CUSTOM_LANGUAGE then
  begin
    inc(LangIDUpdating);
      neLanguage.Value := i;
    dec(LangIDUpdating);
  end;
end;

procedure TfrmResProp.neLanguageChange(Sender: TObject);
var
  i: integer;
  
begin
  if LangIDUpdating = 0 then
  begin
    i := cbLanguage.Items.IndexOfObject(TObject(Round(neLanguage.Value)));
    if i < 0 then i := 0; // custom ID

    cbLanguage.ItemIndex := i;
  end;
end;

procedure TfrmResProp.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  // update combobox
  neLanguageChange(Self);
end;

procedure TfrmResProp.ApplyLanguagePack;
var
  drit: TDlgResItemType;
begin
  Caption := lpGet('MI_LIB_PROP');
  lType.Caption := lpGet('RP_TYPE')+':';
  cbType.Items.Clear;
  for drit in TDlgResItemType do
    cbType.Items.Add(lpGet(GetDlgResItemTypeLP(drit)));
  lName.Caption := lpGet('RP_NAME')+':';
  lLanguage.Caption := lpGet('RP_LANGUAGE')+':';
  cbLanguage.Items[0] := '('+lpGet('RP_LANG_CUSTOM')+')';
  cbLanguage.Items[1] := '('+lpGet('RP_LANG_NEUTRAL')+')';
  neLanguage.Hint := lpGet('RP_LANGUAGE');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
