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
unit dlgBevel;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, AdjustControl, DocClass,
  BitmapEx, Filters, ieShared, FilterDialog, dlgDebug;

type

  { TfrmBevel }

  TfrmBevel = class(TFilterDialog)
    alIntensity: TAdjustLabel;
    neIntensity: TNumberEdit;
    neBlur: TNumberEdit;
    bReset: TButton;
    alBlur: TAdjustLabel;
    bOK: TButton;
    bCancel: TButton;
    alSize: TAdjustLabel;
    alAngle: TAdjustLabel;
    neSize: TNumberEdit;
    neAngle: TNumberEdit;
    cbPreview: TCheckBox;
    cbToric: TCheckBox;
    procedure bResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ObjectChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure cbPreviewClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
    procedure PreviewChanged;
  end;

var
  frmBevel: TfrmBevel;

implementation

{$R *.lfm}

uses LangPack, Layers;

procedure TfrmBevel.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_BEVEL';
  NeedsApplyTransform := True;
end;

procedure TfrmBevel.DoFilter(bm: TBitmap32; Mask: TBitmap1);
var
  q: double;

begin
  q := neAngle.Value * pi / 180;
  fltBevel(bm, Mask, Round(neSize.Value * Cos(q)),
    Round(neSize.Value * Sin(q)), neBlur.Value,
    Round(neIntensity.Value * 2.55), cbToric.Checked);
end;

procedure TfrmBevel.PreviewChanged;
begin
  if cbPreview.Checked then InvokeEffect;
end;

procedure TfrmBevel.bResetClick(Sender: TObject);
begin
  inc(frmUpdating);
    neSize.Value := 5;
    neAngle.Value := 45;
    neBlur.Value := 3;
    neIntensity.Value := 75;
  dec(frmUpdating);

  PreviewChanged;
end;

procedure TfrmBevel.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
  bReset.Click;
  if VerboseMode then Log('TfrmBevel created');
end;

procedure TfrmBevel.ObjectChange(Sender: TObject);
begin
  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmBevel.FormShow(Sender: TObject);
begin
  inc(frmUpdating);
    cbPreview.Checked := Pref_FilterPreview;
    cbToric.Enabled := Assigned(frmDoc)
      and (frmDoc.Doc.Pages[frmDoc.ImageIndex].Layers.SelState <> stFloating);
    if not cbToric.Enabled then cbToric.Checked := false;
  dec(frmUpdating);
  
  PreviewChanged;
end;

procedure TfrmBevel.FormHide(Sender: TObject);
begin
  Pref_FilterPreview := cbPreview.Checked;
end;

procedure TfrmBevel.cbPreviewClick(Sender: TObject);
begin
  if (frmUpdating <> 0) or (frmDoc = nil) then Exit;
  if cbPreview.Checked then PreviewChanged else
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.RedrawPaintBox;
  end;
end;

procedure TfrmBevel.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_BEVEL');
  alSize.Caption := lpGet('LABEL_SIZE')+':';
  alAngle.Caption := lpGet('LABEL_ANGLE_DEGREES')+':';
  alBlur.Caption := lpGet('LABEL_BLUR')+':';
  alIntensity.Caption := lpGet('LABEL_INTENSITY')+':';
  cbToric.Caption := lpGet('LABEL_TORIC');
  cbPreview.Caption := lpGet('B_PREVIEW');
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
