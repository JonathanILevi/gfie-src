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
unit dlgLanguage;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ieShared, LangPack, PNG, Math;

type
  TlpFile = record
    Name: string;
    Flag: TBitmap;
  end;

  { TfrmLanguage }

  TfrmLanguage = class(TForm)
    lLangPack: TLabel;
    bOK: TButton;
    bCancel: TButton;
    lb: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure lbDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure lbDblClick(Sender: TObject);
  private
  protected
    lpFiles: array of TlpFile;
  public
    procedure ApplyLanguagePack;
    procedure lpFiles_Clear;
    procedure Execute;
  end;

var
  frmLanguage: TfrmLanguage;

implementation

{$R *.lfm}

procedure TfrmLanguage.lpFiles_Clear;
var
  i: integer;

begin
  for i := 0 to Length(lpFiles) - 1 do lpFiles[i].Flag.Free;
  SetLength(lpFiles, 0);
end;

procedure TfrmLanguage.Execute;
var
  i, j, m: integer;
  s: string;
  t: TlpFile;
  sr: TSearchRecs;

begin
  // load available language pack files
  FindAll(LanguageDir + '*.txt', faAnyFile, sr);
  SetLength(lpFiles, Length(sr));
  for i := 0 to Length(lpFiles) - 1 do
  with lpFiles[i] do
  begin
    Name := WithoutExt(sr[i].Name);
    Flag := TBitmap.Create;
    s := LanguageDir + Name + '.png';
    if FileExists(s) then pngLoadFromFileBM(Flag, s, clWhite);
  end;

  // minimumsort by name
  for i := 0 to Length(lpFiles) - 2 do
  begin
    m := i;
    for j := i+1 to Length(lpFiles) - 1 do
      if lpFiles[j].Name < lpFiles[m].Name then m := j;
    if m <> i then
    begin
      t := lpFiles[i];
      lpFiles[i] := lpFiles[m];
      lpFiles[m] := t;
    end;
  end;

  // select current language file
  lb.Items.Clear;
  for i := 1 to Length(lpFiles) do lb.Items.Add('*');
  lb.ItemIndex := 0;
  for i := 0 to Length(lpFiles) - 1 do
    if AnsiUpperCase(lpFiles[i].Name + '.txt') =
      AnsiUpperCase(Pref_LanguagePack) then
  begin
    lb.ItemIndex := i;
    Break;
  end;

  if (ShowModal = mrOk) and (lb.ItemIndex >= 0) then
  begin
    s := lpFiles[lb.ItemIndex].Name + '.txt';
    if Pref_LanguagePack <> s then
    begin
      // a new language pack was selected
      Pref_LanguagePack := s;
      lpLoad(LanguageDir + Pref_LanguagePack);
      lpApplyToUI;
    end;
  end;

  // clean up
  lpFiles_Clear;
end;

procedure TfrmLanguage.lbDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with lb.Canvas, lpFiles[Index] do
  begin
    Brush.Color := IfThen(odSelected in State, clHighlight, clWindow);
    FillRect(Rect);

    Draw(Rect.Left + 2, (Rect.Top + Rect.Bottom - Flag.Height) div 2, Flag);
    TextOut(Rect.Left + 4 + Flag.Width,
      (Rect.Top + Rect.Bottom - TextHeight('Mg')) div 2, Name);
  end;
end;

procedure TfrmLanguage.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmLanguage.lbDblClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TfrmLanguage.ApplyLanguagePack;
begin
  Caption := lpGet('MI_SET_LANGUAGE');
  lLangPack.Caption := lpGet('LABEL_LANG_PACK')+':';
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
