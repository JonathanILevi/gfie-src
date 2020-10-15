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
unit dlgGlow;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, ExtCtrls, AdjustControl, DocClass,
  BitmapEx, Filters, ieShared, FilterDialog, dlgDebug;

type

  { TfrmGlow }

  TfrmGlow = class(TFilterDialog)
    alIntensity: TAdjustLabel;
    lColor: TLabel;
    sColor: TShape;
    alRadius: TAdjustLabel;
    neIntensity: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    neRadius: TNumberEdit;
    bReset: TButton;
    gbKind: TGroupBox;
    cbInnerGlow: TCheckBox;
    cbOuterGlow: TCheckBox;
    cbPreview: TCheckBox;
    cbToric: TCheckBox;
    procedure bResetClick(Sender: TObject);
    procedure ObjectChange(Sender: TObject);
    procedure sColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
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
  frmGlow: TfrmGlow;

implementation

{$R *.lfm}

uses LangPack, Layers, Math;

procedure TfrmGlow.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_GLOW';
  NeedsApplyTransform := True;
end;

procedure TfrmGlow.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  fltGlow(bm, Mask, cbInnerGlow.Checked, cbOuterGlow.Checked,
    neRadius.Value, sColor.Brush.Color, Round(neIntensity.Value * 2.56),
    cbToric.Checked);
end;

procedure TfrmGlow.PreviewChanged;
var
  rOuterGlow: double;

begin
  if cbOuterGlow.Checked then
    rOuterGlow := neRadius.Value * 3
  else
    rOuterGlow := 0;

  FloatingSelectionMargin := Rect(Floor(-rOuterGlow), Floor(-rOuterGlow),
    Ceil(rOuterGlow), Ceil(rOuterGlow));
  if cbPreview.Checked then InvokeEffect;
end;

procedure TfrmGlow.bResetClick(Sender: TObject);
begin
  inc(frmUpdating);
    neRadius.Value := 3;
    sColor.Brush.Color := clWhite;
    neIntensity.Value := 200;
    cbInnerGlow.Checked := False;
    cbOuterGlow.Checked := True;
  dec(frmUpdating);

  PreviewChanged;
end;

procedure TfrmGlow.ObjectChange(Sender: TObject);
begin
  if frmUpdating = 0 then PreviewChanged;
end;

procedure TfrmGlow.sColorMouseDown(Sender: TObject; Button: TMouseButton;
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

procedure TfrmGlow.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
  bReset.Click;
  if VerboseMode then Log('TfrmGlow created');
end;

procedure TfrmGlow.FormShow(Sender: TObject);
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

procedure TfrmGlow.FormHide(Sender: TObject);
begin
  Pref_FilterPreview := cbPreview.Checked;
end;

procedure TfrmGlow.cbPreviewClick(Sender: TObject);
begin
  if (frmUpdating <> 0) or (frmDoc = nil) then Exit;
  if cbPreview.Checked then PreviewChanged else
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.RedrawPaintBox;
  end;
end;

procedure TfrmGlow.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_GLOW');
  alRadius.Caption := lpGet('LABEL_RADIUS')+':';
  lColor.Caption := lpGet('LABEL_COLOR')+':';
  alIntensity.Caption := lpGet('LABEL_INTENSITY')+':';
  gbKind.Caption := lpGet('LABEL_KIND');
  cbInnerGlow.Caption := lpGet('LABEL_INNER_GLOW');
  cbOuterGlow.Caption := lpGet('LABEL_OUTER_GLOW');
  cbToric.Caption := lpGet('LABEL_TORIC');
  cbPreview.Caption := lpGet('B_PREVIEW');
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
