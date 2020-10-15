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
unit dlgBlur;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, AdjustControl, dlgDoc,
  Layers, DocClass, BitmapEx, Filters, ieShared, FilterDialog;

type

  { TfrmBlur }

  TfrmBlur = class(TFilterDialog)
    rbGaussian: TRadioButton;
    rbBox: TRadioButton;
    alGaussianRadius: TAdjustLabel;
    neGaussianRadius: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    alBoxRadius: TAdjustLabel;
    neBoxRadius: TNumberEdit;
    cbPreview: TCheckBox;
    cbToric: TCheckBox;
    procedure neChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbClick(Sender: TObject);
    procedure cbPreviewClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbToricClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
    procedure PreviewChanged;
  end;

var
  frmBlur: TfrmBlur;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmBlur.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_BLUR_CUSTOM';
  NeedsApplyTransform := True;
end;

procedure TfrmBlur.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  if rbGaussian.Checked then
    fltGaussianBlur(bm, Mask, neGaussianRadius.Value, cbToric.Checked, False) else
    fltBoxBlur(bm, Mask, 2*Round(neBoxRadius.Value) + 1, False, cbToric.Checked, False);
end;

procedure TfrmBlur.PreviewChanged;
begin
  if cbPreview.Checked then InvokeEffect;
end;

procedure TfrmBlur.neChange(Sender: TObject);
begin
  if Sender = neGaussianRadius then
    rbGaussian.Checked := True else
    rbBox.Checked := True;

  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmBlur.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  inc(frmUpdating);
    cbPreview.Checked := Pref_FilterPreview;
    cbToric.Enabled := Assigned(frmDoc)
      and (frmDoc.Doc.Pages[frmDoc.ImageIndex].Layers.SelState <> stFloating);
    if not cbToric.Enabled then cbToric.Checked := false;
  dec(frmUpdating);

  PreviewChanged;
end;

procedure TfrmBlur.rbClick(Sender: TObject);
begin
  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmBlur.cbPreviewClick(Sender: TObject);
begin
  if (frmUpdating <> 0) or (frmDoc = nil) then Exit;
  if cbPreview.Checked then PreviewChanged else
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.RedrawPaintBox;
  end;
end;

procedure TfrmBlur.FormHide(Sender: TObject);
begin
  Pref_FilterPreview := cbPreview.Checked;
end;

procedure TfrmBlur.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
end;

procedure TfrmBlur.cbToricClick(Sender: TObject);
begin
  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmBlur.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_BLUR_CUSTOM');
  rbGaussian.Caption := lpGet('LABEL_GAUSSIAN_BLUR');
  alGaussianRadius.Caption := lpGet('LABEL_RADIUS')+':';
  rbBox.Caption := lpGet('LABEL_BOX_BLUR');
  alBoxRadius.Caption := lpGet('LABEL_RADIUS')+':';
  cbToric.Caption := lpGet('LABEL_TORIC');
  cbPreview.Caption := lpGet('B_PREVIEW');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.

