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
unit dlgShadow;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, AdjustControl, NumberEdit, dlgDoc, Layers,
  DocClass, BitmapEx, Filters, ieShared, FilterDialog, dlgDebug, Math;

type

  { TfrmShadow }

  TfrmShadow = class(TFilterDialog)
    neDistance: TNumberEdit;
    neIntensity: TNumberEdit;
    alDistance: TAdjustLabel;
    alIntensity: TAdjustLabel;
    lColor: TLabel;
    sColor: TShape;
    alAngle: TAdjustLabel;
    neAngle: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    bReset: TButton;
    alBlur: TAdjustLabel;
    neBlur: TNumberEdit;
    cbPreview: TCheckBox;
    cbToric: TCheckBox;
    procedure bResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ObjectChange(Sender: TObject);
    procedure sColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure cbPreviewClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
    procedure PreviewChanged;
  end;

var
  frmShadow: TfrmShadow;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmShadow.DoFilter(bm: TBitmap32; Mask: TBitmap1);
var
  radAngle: double;
  
begin
  radAngle := neAngle.Value * pi / 180;
  fltShadow(bm, Mask, Round(neDistance.Value * Cos(radAngle)),
    Round(neDistance.Value * Sin(radAngle)), neBlur.Value,
    sColor.Brush.Color, Round(neIntensity.Value * 2.55), cbToric.Checked);
end;

procedure TfrmShadow.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_DROP_SHADOW';
  NeedsApplyTransform := True;
end;

procedure TfrmShadow.PreviewChanged;
var
  radAngle, dx, dy, rBlur: double;

begin
  radAngle := neAngle.Value * pi / 180;
  dx := neDistance.Value * Cos(radAngle);
  dy := neDistance.Value * Sin(radAngle);
  rBlur := neBlur.Value * 3;

  FloatingSelectionMargin := Rect(Min(0, Floor(dx-rBlur)), Min(0, Floor(dy-rBlur)),
    Max(0, Ceil(dx+rBlur)), Max(0, Ceil(dy+rBlur)));
  if cbPreview.Checked then
    InvokeEffect;
end;

procedure TfrmShadow.bResetClick(Sender: TObject);
begin
  inc(frmUpdating);
    neDistance.Value := 3;
    neAngle.Value := 45;
    neBlur.Value := 1.5;
    sColor.Brush.Color := clBlack;
    neIntensity.Value := 30;
  dec(frmUpdating);

  PreviewChanged;
end;

procedure TfrmShadow.FormShow(Sender: TObject);
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

procedure TfrmShadow.ObjectChange(Sender: TObject);
begin
  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmShadow.sColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Value: TColor;
  
begin
  Value := GFIEPickColor(sColor.Brush.Color);
  if Value <> sColor.Brush.Color then
  begin
    sColor.Brush.Color := Value;
    PreviewChanged;
  end;
end;

procedure TfrmShadow.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  bReset.Click;
  if VerboseMode then Log('TfrmShadow created');
end;

procedure TfrmShadow.cbPreviewClick(Sender: TObject);
begin
  if (frmUpdating <> 0) or (frmDoc = nil) then Exit;
  if cbPreview.Checked then PreviewChanged else
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.RedrawPaintBox;
  end;
end;

procedure TfrmShadow.FormHide(Sender: TObject);
begin
  Pref_FilterPreview := cbPreview.Checked;
end;

procedure TfrmShadow.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_DROP_SHADOW');
  alDistance.Caption := lpGet('LABEL_DISTANCE')+':';
  alAngle.Caption := lpGet('LABEL_ANGLE_DEGREES')+':';
  alBlur.Caption := lpGet('LABEL_BLUR')+':';
  lColor.Caption := lpGet('LABEL_COLOR')+':';
  alIntensity.Caption := lpGet('LABEL_INTENSITY')+':';
  cbToric.Caption := lpGet('LABEL_TORIC');
  cbPreview.Caption := lpGet('B_PREVIEW');
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
