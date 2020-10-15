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
unit dlgLayerProp;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdjustControl, NumberEdit, dlgDoc, BlendModes, Layers, dlgDebug;

type

  { TfrmLayerProp }

  TfrmLayerProp = class(TForm)
    lName: TLabel;
    eName: TEdit;
    cbVisible: TCheckBox;
    neOpacity: TNumberEdit;
    alOpacity: TAdjustLabel;
    cbBlendMode: TComboBox;
    bOk: TButton;
    bCancel: TButton;
    lBlendMode: TLabel;
    procedure eNameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbVisibleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure neOpacityChange(Sender: TObject);
    procedure cbBlendModeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  protected
    frmDoc: TGraphicFrame;
    Layer: TLayer;

    frmUpdating: integer;
  public
    // Object for saving layer properties
    lp: TLayerProp;

    procedure ApplyLanguagePack;
    function Execute(_frmDoc: TGraphicFrame; _Layer: TLayer): boolean;
    procedure UpdateForm;
    procedure UpdateObject;
  end;

var
  frmLayerProp: TfrmLayerProp;

implementation

{$R *.lfm}

uses LangPack, ieShared;

function TfrmLayerProp.Execute(_frmDoc: TGraphicFrame; _Layer: TLayer): boolean;
begin
  frmDoc := _frmDoc;
  Layer := _Layer;

  // save layer info
  lp.Read(Layer);
  // We do not save the entire layer object
  // That would consume too much memory

  UpdateForm;

  // show
  Result := (ShowModal = mrOk);

  // restore values
  lp.Write(Layer);
  frmDoc.RedrawPaintBox;
end;

procedure TfrmLayerProp.UpdateForm;
begin
  with Layer do
  begin
    // set fields
    inc(frmUpdating);
      eName.Text := Name;
      cbVisible.Checked := Visible;
      neOpacity.Value := Opacity;
      cbBlendMode.ItemIndex := Ord(BlendMode);
    dec(frmUpdating);
  end;
end;

procedure TfrmLayerProp.UpdateObject;
begin
  with Layer do
  begin
    Name := eName.Text;
    Visible := cbVisible.Checked;
    Opacity := Round(neOpacity.Value);
    BlendMode := TBlendMode(cbBlendMode.ItemIndex);
  end;
end;

procedure TfrmLayerProp.eNameChange(Sender: TObject);
begin
  if frmUpdating <> 0 then Exit;

  Layer.Name := eName.Text;
end;

procedure TfrmLayerProp.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  frmUpdating := 0;
  lp := TLayerProp.Create;
  if VerboseMode then Log('TfrmLayerProp created');
end;

procedure TfrmLayerProp.cbVisibleClick(Sender: TObject);
begin
  if frmUpdating <> 0 then Exit;

  Layer.Visible := cbVisible.Checked;
  frmDoc.RedrawPaintBox;
end;

procedure TfrmLayerProp.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmLayerProp.neOpacityChange(Sender: TObject);
begin
  if frmUpdating <> 0 then Exit;

  Layer.Opacity := Round(neOpacity.Value);
  frmDoc.RedrawPaintBox;
end;

procedure TfrmLayerProp.cbBlendModeChange(Sender: TObject);
begin
  if (frmUpdating <> 0) or (cbBlendMode.ItemIndex < 0) then Exit;

  Layer.BlendMode := TBlendMode(cbBlendMode.ItemIndex);
  frmDoc.RedrawPaintBox;
end;

procedure TfrmLayerProp.FormDestroy(Sender: TObject);
begin
  lp.Free;
end;

procedure TfrmLayerProp.ApplyLanguagePack;
begin
  Caption := lpGet('MI_LAYERS_PROP');
  lName.Caption := lpGet('LP_NAME')+':';
  cbVisible.Caption := lpGet('LP_VISIBLE');
  alOpacity.Caption := lpGet('LABEL_OPACITY')+':';
  lBlendMode.Caption := lpGet('LP_BLEND_MODE')+':';
  SetComboItems(cbBlendMode,
    lpGet('LP_BM_NORMAL')+LineEnding+
    lpGet('LP_BM_MASK')+LineEnding+
    lpGet('LP_BM_BEHIND')+LineEnding+
    lpGet('LP_BM_DISSOLVE')+LineEnding+
    lpGet('LP_BM_HUE')+LineEnding+
    lpGet('LP_BM_HUE_SHIFT')+LineEnding+
    lpGet('LP_BM_SAT')+LineEnding+
    lpGet('LP_BM_DARKEN')+LineEnding+
    lpGet('LP_BM_MULTIPLY')+LineEnding+
    lpGet('LP_BM_COLOR_BURN')+LineEnding+
    lpGet('LP_BM_LINEAR_BURN')+LineEnding+
    lpGet('LP_BM_DARKER_COLOR')+LineEnding+
    lpGet('LP_BM_LIGHTEN')+LineEnding+
    lpGet('LP_BM_SCREEN')+LineEnding+
    lpGet('LP_BM_COLOR_DODGE')+LineEnding+
    lpGet('LP_BM_LINEAR_DODGE')+LineEnding+
    lpGet('LP_BM_LIGHTER_COLOR')+LineEnding+
    lpGet('LP_BM_OVERLAY')+LineEnding+
    lpGet('LP_BM_SOFT_LIGHT')+LineEnding+
    lpGet('LP_BM_HARD_LIGHT')+LineEnding+
    lpGet('LP_BM_VIVID_LIGHT')+LineEnding+
    lpGet('LP_BM_LINEAR_LIGHT')+LineEnding+
    lpGet('LP_BM_PIN_LIGHT')+LineEnding+
    lpGet('LP_BM_HARD_MIX')+LineEnding+
    lpGet('LP_BM_DIFFERENCE')+LineEnding+
    lpGet('LP_BM_EXCLUSION')
  );
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
