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
unit dlgFormulae;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DocClass, BitmapEx, Filters, ieShared,
  Math, FilterDialog, dlgDebug;

type

  { TfrmFormulae }

  TfrmFormulae = class(TFilterDialog)
    bHelp: TButton;
    cbRange: TComboBox;
    eAlpha: TEdit;
    lAlpha: TLabel;
    lRed: TLabel;
    lGreen: TLabel;
    lBlue: TLabel;
    eRed: TEdit;
    eGreen: TEdit;
    eBlue: TEdit;
    cbPreview: TCheckBox;
    bOK: TButton;
    bCancel: TButton;
    bReset: TButton;
    procedure bHelpClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure cbPreviewClick(Sender: TObject);
    procedure bResetClick(Sender: TObject);
    procedure eChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    function DoFilterFn(bm: TBitmap32; Mask: TBitmap1): boolean;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
    procedure PreviewChanged;
  end;

const
  RANGE_COUNT = 4;

var
  frmFormulae: TfrmFormulae;
  rangeGray: array[0..RANGE_COUNT-1] of double = (0.5, 0, 128, 50);
  rangeScale: array[0..RANGE_COUNT-1] of double = (1.0/256.0, 2.0/256.0, 1, 100.0/256.0);

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmFormulae.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_FORMULAE';
end;

function TfrmFormulae.DoFilterFn(bm: TBitmap32; Mask: TBitmap1): boolean;
begin
  if cbRange.ItemIndex >= 0 then
    Result := fltChannelFormula(bm, Mask, eRed.Text, eGreen.Text, eBlue.Text, eAlpha.Text,
      rangeGray[cbRange.ItemIndex], rangeScale[cbRange.ItemIndex])
  else
    Result := false;
end;

procedure TfrmFormulae.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  DoFilterFn(bm, Mask);
end;

procedure TfrmFormulae.eChange(Sender: TObject);
var
  c: TColor;
  
begin
  if frmUpdating = 0 then PreviewChanged;

  // mark syntax errors
  c := IfThen(DoFilterFn(nil, nil), clWindow, $8080ff);
  eRed.Color := c;
  eGreen.Color := c;
  eBlue.Color := c;
  eAlpha.Color := c;
end;

procedure TfrmFormulae.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_FORMULAE');
  cbPreview.Caption := lpGet('B_PREVIEW');
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
  bHelp.Caption := lpGet('B_HELP');
end;

procedure TfrmFormulae.PreviewChanged;
begin
  if cbPreview.Checked then InvokeEffect;
end;

procedure TfrmFormulae.cbPreviewClick(Sender: TObject);
begin
  if (frmUpdating <> 0) or (frmDoc = nil) then Exit;
  if cbPreview.Checked then PreviewChanged else
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.RedrawPaintBox;
  end;
end;

procedure TfrmFormulae.bResetClick(Sender: TObject);
begin
  inc(frmUpdating);
    cbRange.ItemIndex := 0;
    eRed.Text := 'r';
    eGreen.Text := 'g';
    eBlue.Text := 'b';
    eAlpha.Text := 'a';
  dec(frmUpdating);

  PreviewChanged;
end;

procedure TfrmFormulae.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
  bReset.Click;
  if VerboseMode then Log('TfrmFormulae created');
end;

procedure TfrmFormulae.bHelpClick(Sender: TObject);
begin
  ShowMessage(StringReplace(StringReplace(StringReplace(lpGet('HELP_FORMULAE'),
    '${vars}', 'r g b a pi e', [rfReplaceAll, rfIgnoreCase]),
    '${ops}', '+ - * / abs max min pow sqrt exp ln sin cos tan', [rfReplaceAll, rfIgnoreCase]),
    '${example}', 'pow(r - (r+g+b)/3, 2) + max(g, 0.5*(r+b))', [rfReplaceAll, rfIgnoreCase]));
end;

procedure TfrmFormulae.FormHide(Sender: TObject);
begin
  Pref_FilterPreview := cbPreview.Checked;
end;

procedure TfrmFormulae.FormShow(Sender: TObject);
begin
  inc(frmUpdating);
    cbPreview.Checked := Pref_FilterPreview;
  dec(frmUpdating);

  PreviewChanged;
end;

end.
