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
unit dlgCreateIcon;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dlgDoc, Layers, DocClass, PixelFormats, LangPack, BitmapEx, dlgDebug;

const
  fmtSizeCount = 6;
  fmtColorCount = 5;

  fmtSizes: array[0..fmtSizeCount - 1] of integer = (16, 24, 32, 48, 64, 256);

type
  TCreateIcon = array[0..fmtSizeCount - 1, 0..fmtColorCount - 1] of boolean;

  { TfrmCreateIcon }

  TfrmCreateIcon = class(TForm)
    lInfo: TLabel;
    bReset: TButton;
    bNone: TButton;
    bAll: TButton;
    l16Colors: TLabel;
    l256Colors: TLabel;
    l24Bit: TLabel;
    l32Bit: TLabel;
    lSize16: TLabel;
    lSize32: TLabel;
    lSize48: TLabel;
    lSize64: TLabel;
    lSize256: TLabel;
    bOK: TButton;
    bCancel: TButton;
    lBW: TLabel;
    lSize24: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure bAllNoneClick(Sender: TObject);
    procedure bResetClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    cbFormat: array[0..fmtSizeCount - 1, 0..fmtColorCount - 1] of TCheckBox;
  public
    frmDoc: TGraphicFrame;

    procedure ApplyLanguagePack;
    function IsSelected: boolean;
    procedure UpdateForm(const ci: TCreateIcon);
    procedure UpdateObject(var ci: TCreateIcon);
    function Execute(frmSrc: TGraphicFrame): TGraphicFrame;
  end;

var
  frmCreateIcon: TfrmCreateIcon;

procedure DoCreateIcon(Src: TBitmap32; Dest: TIconDoc; const ci: TCreateIcon);

implementation

uses Main;

{$R *.lfm}

procedure DoCreateIcon;
const
  cr: array[TPixelFormat32] of TColorReduction =
    (crBW, crWin16, crAdaptive256, cr24, cr32);

var
  i, j: integer;
  Page: TDocPage;
  l: TLayer;
  
begin
  for i := 0 to fmtSizeCount - 1 do
  for j := 0 to fmtColorCount - 1 do
    if ci[i, j] then
  begin
    Page := Dest.NewPage;
    Page.Layers.Resize(fmtSizes[i], fmtSizes[i]);

    l := Page.Layers.NewLayerAtDepth(0);
    with l do
    begin
      Name := lpGet('LY_BACKGROUND');
      Selected := True;
      Image.TransformDraw(Image.ClientRect, 0, Src, True);
    end;
    ReduceColors(l.Image, cr[TPixelFormat32(j)], nil);
  end;
end;

function TfrmCreateIcon.IsSelected: boolean;
var
  i, j: integer;
  
begin
  Result := False;

  for i := 0 to fmtSizeCount - 1 do for j := 0 to fmtColorCount - 1 do
    if cbFormat[i, j].Checked then
  begin
    Result := True;
    Exit;
  end;
end;

procedure TfrmCreateIcon.UpdateForm(const ci: TCreateIcon);
var
  i, j: integer;

begin
  for i := 0 to fmtSizeCount - 1 do
  for j := 0 to fmtColorCount - 1 do
    cbFormat[i, j].Checked := ci[i, j];
end;

procedure TfrmCreateIcon.UpdateObject(var ci: TCreateIcon);
var
  i, j: integer;

begin
  for i := 0 to fmtSizeCount - 1 do
  for j := 0 to fmtColorCount - 1 do
    ci[i, j] := cbFormat[i, j].Checked;
end;

function TfrmCreateIcon.Execute(frmSrc: TGraphicFrame): TGraphicFrame;
var
  bm: TBitmap32;
  ci: TCreateIcon;

begin
  if ShowModal = mrOk then
  begin
    UpdateObject(ci);

    Result := TGraphicFrame(frmMain.NewDocument(TGraphicFrame));
    with Result do
    begin
      Doc.Clear;

      bm := TBitmap32.Create;
      try
        bm.Assign(frmSrc.Doc.Pages[frmSrc.ImageIndex].Layers);
        DoCreateIcon(bm, Doc, ci);
      finally
        bm.Free;
      end;

      Modified := true;
      PageCountChanged;
      PageSizeChanged;
    end;
  end else
    Result := nil;
end;

procedure TfrmCreateIcon.FormCreate(Sender: TObject);
var
  i, j: integer;

begin
  ApplyLanguagePack;
  for i := 0 to fmtSizeCount - 1 do
    for j := 0 to fmtColorCount - 1 do
  begin
    cbFormat[i, j] := TCheckBox.Create(Self);

    with cbFormat[i, j] do
    begin
      Parent := Self;
      Left := lSize16.Left + 24 * i;
      Top := lBW.Top + 16 * j;
      Width := 20;
    end;
  end;

  bReset.Click;

  if VerboseMode then Log('TfrmCreateIcon created');
end;

procedure TfrmCreateIcon.bAllNoneClick(Sender: TObject);
var
  i, j: integer;

begin
  for i := 0 to fmtSizeCount - 1 do
    for j := 0 to fmtColorCount - 1 do
  cbFormat[i, j].Checked := Boolean((Sender as TControl).Tag);
end;

procedure TfrmCreateIcon.bResetClick(Sender: TObject);
var
  i: integer;
  
begin
  bNone.Click;

  for i := 0 to 3 do if i <> 1 then
  begin
    cbFormat[i, 1].Checked := True;
    cbFormat[i, 2].Checked := True;
    cbFormat[i, 4].Checked := True;
  end;
  cbFormat[5, 4].Checked := True;
end;

procedure TfrmCreateIcon.bOKClick(Sender: TObject);
begin
  if IsSelected then ModalResult := mrOk else
    ShowMessage(lpGet('MSG_SELECT_PAGES'));
end;

procedure TfrmCreateIcon.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmCreateIcon.ApplyLanguagePack;
begin
  Caption := lpGet('MI_ICON_CREATE_WIN');
  lInfo.Caption := lpGet('CI_SELECT_FORMATS')+':';
  lBW.Caption := pf32ToStr[pf32_1bit];
  l16Colors.Caption := pf32ToStr[pf32_4bit];
  l256Colors.Caption := pf32ToStr[pf32_8bit];
  l24Bit.Caption := pf32ToStr[pf32_24Bit];
  l32Bit.Caption := pf32ToStr[pf32_32Bit];

  bReset.Caption := lpGet('B_RESET');
  bNone.Caption := lpGet('B_NONE');
  bAll.Caption := lpGet('B_ALL');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
