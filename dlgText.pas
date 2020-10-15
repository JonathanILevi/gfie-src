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
unit dlgText;

interface

uses
{$ifdef WINDOWS}
  Windows,
{$endif}
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdjustControl, NumberEdit, Buttons, Math, ExtCtrls, dlgDoc,
  ieShared, types;

type

  { TfrmText }

  TfrmText = class(TForm)
    mText: TMemo;
    pTop: TPanel;
    sbBold: TSpeedButton;
    sbItalic: TSpeedButton;
    sbUnderline: TSpeedButton;
    cbFace: TComboBox;
    neSize: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    procedure cbFaceDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure cbFaceDropDown(Sender: TObject);
    procedure cbFaceUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure ControlChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    //procedure cbFaceDrawItem(Control: TWinControl; Index: Integer;
    //  Rect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    cbLastKeyPressTime: integer;
    cbSearchString: UnicodeString;
    cbSearchOrigin: integer;
  public
    procedure ApplyLanguagePack;
    procedure GetFont(f: TFont);
    procedure SetFont(f: TFont);
  end;

var
  frmText: TfrmText;

implementation

uses
  Main, dlgColor, dlgToolSet, LangPack, dlgDebug, StrUtils;

{$R *.lfm}

procedure TfrmText.GetFont(f: TFont);
begin
  f.Name := cbFace.Text;
  if Screen.Fonts.IndexOf(f.Name) < 0 then f.Name := Font.Name;
  f.Size := Round(neSize.Value);
  f.Style := [];
  if sbBold.Down then f.Style := f.Style + [fsBold];
  if sbItalic.Down then f.Style := f.Style + [fsItalic];
  if sbUnderline.Down then f.Style := f.Style + [fsUnderline];
end;

procedure TfrmText.SetFont(f: TFont);
begin
  cbFace.Text := f.Name;
  neSize.Value := f.Size;
  sbBold.Down := fsBold in f.Style;
  sbItalic.Down := fsItalic in f.Style;
  sbUnderline.Down := fsUnderline in f.Style;
end;

procedure TfrmText.ControlChange(Sender: TObject);
begin
  GetFont(mText.Font);
end;

procedure TfrmText.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmText.cbFaceDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  x: integer;

begin
  with cbFace.Canvas do
  begin
    Brush.Color := IfThen(odSelected in State, clHighlight, clWhite);
    FillRect(ARect);

    Brush.Style := bsClear;

    x := ARect.Left + 1;
    if not (odComboBoxEdit in State) then
    begin
      Font.Size := 10;
      Font.Name := cbFace.Font.Name;
      TextOut(x, (ARect.Top + ARect.Bottom - TextHeight(cbFace.Items[Index])) div 2,
        cbFace.Items[Index]);
      x += TextWidth(cbFace.Items[Index]);
      x += TextWidth('M')*3 + (40 - x mod 40); // tabulate
    end;
    Font.Size := 12;
    Font.Name := cbFace.Items[Index];
    TextOut(x, (ARect.Top + ARect.Bottom - TextHeight(cbFace.Items[Index])) div 2,
      cbFace.Items[Index]);

    Brush.Style := bsSolid;
  end;
end;

procedure TfrmText.cbFaceDropDown(Sender: TObject);
begin
{$ifdef WINDOWS}
  SendMessage(cbFace.Handle, CB_SETDROPPEDWIDTH, Round(cbFace.Width*1.5), 0);
{$endif}
end;

procedure TfrmText.cbFaceUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
var
  i, ii: integer;
  t: integer;
  item: UnicodeString;
  cb: TComboBox;

begin
  cb := Sender as TComboBox;
  t := GetTickCount;
  if t - cbLastKeyPressTime >= 1000 then
  begin
    cbSearchString := '';
    cbSearchOrigin := cb.ItemIndex + 1;
  end;
  cbSearchString += UnicodeLowerCase(UTF8Decode(UTF8Key));
  cbLastKeyPressTime := t;
  UTF8Key := '';

  cb.DroppedDown:=false;
  for i := 0 to cb.Items.Count - 1 do
  begin
    ii := (cbSearchOrigin + i) mod cb.Items.Count;
    item := UnicodeLowerCase(UTF8Decode(cb.Items[ii]));
    if Pos(cbSearchString, item) = 1 then
    begin
      cb.ItemIndex := ii;
      Break;
    end;
  end;
end;

procedure TfrmText.FormShow(Sender: TObject);
var
  s: string;

begin
  // move form to screen center only the first time
  Position := poDesigned;

  // we should always load fonts when showing the form
  // because new fonts may be installed meanwhile
  with cbFace do
  begin
    if (ItemIndex >= 0) and (ItemIndex < Items.Count) then
      s := Items[ItemIndex]
    else
      s := '';

    Items.Assign(Screen.Fonts);
    ItemIndex := Items.IndexOf(s);
    if ItemIndex < 0 then ItemIndex := Items.IndexOf('Arial');
    if ItemIndex < 0 then ItemIndex := Items.IndexOf('Sans');
    if ItemIndex < 0 then ItemIndex := Items.IndexOf('Liberation Sans');
    if ItemIndex < 0 then ItemIndex := 0;
  end;

  ControlChange(nil);
  ActiveControl := mText;
end;

procedure TfrmText.bCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmText.bOKClick(Sender: TObject);
var
  frm: TGraphicFrame;
  
begin
  frm := frmMain.frmGraphicActive;
  if frm <> nil then frm.DoInsertText(mText.Lines.Text, mText.Font,
    frmMain.frmColor.SelColor[0],
    frmMain.frmToolSet.CommonToolSettings[dtText].Antialias);
  
  Close;
end;

procedure TfrmText.ApplyLanguagePack;
begin
  Caption := lpGet('CAPTION_INSERT_TEXT');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');

  cbFace.Hint := lpGet('HINT_TEXT_FACE');
  neSize.Hint := lpGet('LABEL_SIZE');
  sbBold.Hint := lpGet('HINT_TEXT_BOLD');
  sbItalic.Hint := lpGet('HINT_TEXT_ITALIC');
  sbUnderline.Hint := lpGet('HINT_TEXT_UNDERLINE');
end;

end.
