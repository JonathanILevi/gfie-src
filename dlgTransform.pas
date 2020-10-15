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
unit dlgTransform;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, AdjustControl, dlgDoc, DocClass, Math,
  ieShared, LangPack, Layers, gfMath;

type

  { TfrmTransform }

  TfrmTransform = class(TForm)
    alAngle: TAdjustLabel;
    gbPosition: TGroupBox;
    alX: TAdjustLabel;
    neAngle: TNumberEdit;
    neX: TNumberEdit;
    alY: TAdjustLabel;
    neY: TNumberEdit;
    gbSize: TGroupBox;
    alWidth: TAdjustLabel;
    neWidth: TNumberEdit;
    cbUnits: TComboBox;
    neHeight: TNumberEdit;
    alHeight: TAdjustLabel;
    bOK: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ControlChange(Sender: TObject);
    procedure cbUnitsChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    frmUpdating: integer;
    frmDoc: TGraphicFrame;
    ls: TLayers;

    procedure ApplyLanguagePack;
    procedure InvokeEffect;
    procedure Execute(_frmDoc: TGraphicFrame);
  end;

var
  frmTransform: TfrmTransform;

implementation

{$R *.lfm}

procedure TfrmTransform.InvokeEffect;
var
  Delta: integer;

begin
  if frmDoc = nil then Exit;

  with ls.Selection.Box do
  begin
    Delta := Round(neX.Value) - Left;
    inc(Left, Delta);
    inc(Right, Delta);

    Delta := Round(neY.Value) - Top;
    inc(Top, Delta);
    inc(Bottom, Delta);

    Right := Left + Round(neWidth.Value * IfThen(cbUnits.ItemIndex = 1,
      0.01*ls.Selection.Image.Width, 1));
    Bottom := Top + Round(neHeight.Value * IfThen(cbUnits.ItemIndex = 1,
      0.01*ls.Selection.Image.Height, 1));
  end;

  ls.Selection.Angle := neAngle.Value * pi / 180;

  frmDoc.RedrawPaintBox;
end;

procedure TfrmTransform.Execute(_frmDoc: TGraphicFrame);
var
  mr: TModalResult;

begin
  frmDoc := _frmDoc;
  ls := frmDoc.Doc.Pages[frmDoc.ImageIndex].Layers;
  if ls.SelState = stNone then Exit;

  // save state
  frmDoc.lsSave.Assign(ls);
  try
    if ls.SelState = stSelecting then
    begin
      ls.CreateFloatingSelection;
      frmDoc.RedrawPaintBox;
    end;

    // load values
    inc(frmUpdating);
      with ls.Selection do
      begin
        neX.Value := Box.Left;
        neY.Value := Box.Top;

        neWidth.Value := Box.Width *
          IfThen(cbUnits.ItemIndex = 1, 100 / Image.Width, 1);
        neHeight.Value := Box.Height *
          IfThen(cbUnits.ItemIndex = 1, 100 / Image.Height, 1);

        neAngle.Value := Angle * 180 / pi;
      end;
    dec(frmUpdating);

    // showmodal
    // if successful, invoke effect
    mr := ShowModal;
    ls.Assign(frmDoc.lsSave);

    if mr = mrOk then
    begin
      with frmDoc do
      begin
        Modified := True;
        cuTransform('TOOL_' + ToolNameRes[dtTransform]);
      end;
      
      if ls.SelState = stSelecting then ls.CreateFloatingSelection;
      InvokeEffect;
    end else
      frmDoc.RedrawPaintBox;
  except
    ls.Assign(frmDoc.lsSave);
  end;

  // free the resources
  frmDoc.lsSave.Clear;
end;

procedure TfrmTransform.FormCreate(Sender: TObject);
begin
  frmUpdating := 0;
  frmDoc := nil;
  ls := nil;
  ApplyLanguagePack;
end;

procedure TfrmTransform.ControlChange(Sender: TObject);
begin
  if frmUpdating = 0 then InvokeEffect;
end;

procedure TfrmTransform.cbUnitsChange(Sender: TObject);
begin
  inc(frmUpdating);
    with ls.Selection do
    begin
      neWidth.Value := Box.Width * IfThen(cbUnits.ItemIndex = 1,
        100 / Image.Width, 1);
      neHeight.Value := Box.Height * IfThen(cbUnits.ItemIndex = 1,
        100 / Image.Height, 1);
    end;
  dec(frmUpdating);
end;

procedure TfrmTransform.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmTransform.ApplyLanguagePack;
begin
  Caption := lpGet('MI_EDIT_SEL_TRANSFORM');
  gbPosition.Caption := lpGet('LABEL_POSITION');
  gbSize.Caption := lpGet('LABEL_SIZE');
  alWidth.Caption := lpGet('LABEL_WIDTH')+':';
  alHeight.Caption := lpGet('LABEL_HEIGHT')+':';
  SetComboItems(cbUnits, lpGet('XF_UNITS_ITEMS'));
  alAngle.Caption := lpGet('LABEL_ANGLE_DEGREES')+':';
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
