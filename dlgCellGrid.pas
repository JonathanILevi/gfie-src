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
unit dlgCellGrid;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, AdjustControl, dlgDoc, dlgDebug;

type

  { TfrmCellGrid }

  TfrmCellGrid = class(TForm)
    cbEnabled: TCheckBox;
    gbSize: TGroupBox;
    alSizeX: TAdjustLabel;
    neSizeX: TNumberEdit;
    alSizeY: TAdjustLabel;
    neSizeY: TNumberEdit;
    gbSpacing: TGroupBox;
    alSpacingX: TAdjustLabel;
    alSpacingY: TAdjustLabel;
    neSpacingX: TNumberEdit;
    neSpacingY: TNumberEdit;
    gbOffset: TGroupBox;
    alOffsetX: TAdjustLabel;
    alOffsetY: TAdjustLabel;
    neOffsetX: TNumberEdit;
    neOffsetY: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure neChange(Sender: TObject);
    procedure cbEnabledClick(Sender: TObject);
  private
    { Private declarations }
  public
    cgSave: TCellGrid;
    frmDoc: TGraphicFrame;

    frmUpdating: integer;

    procedure ApplyLanguagePack;
    function Execute(_frmDoc: TGraphicFrame): boolean;
    procedure UpdateForm;
    procedure UpdateObject;
  end;

var
  frmCellGrid: TfrmCellGrid;

implementation

{$IFNDEF LCL}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses LangPack;

// TfrmCellGrid

function TfrmCellGrid.Execute(_frmDoc: TGraphicFrame): boolean;
begin
  frmDoc := _frmDoc;

  // save object
  cgSave.Assign(frmDoc.cg);
  // update form
  UpdateForm;

  Result := (ShowModal = mrOk);
  if not Result then with frmDoc do
  begin
    // restore object
    cg.Assign(cgSave);
    RedrawPaintBox;
  end;
end;

procedure TfrmCellGrid.UpdateForm;
begin
  inc(frmUpdating);
    with frmDoc.cg do
    begin
      cbEnabled.Checked := Enabled;
      neSizeX.Value := Size.X;
      neSizeY.Value := Size.Y;
      neSpacingX.Value := Spacing.X;
      neSpacingY.Value := Spacing.Y;
      neOffsetX.Value := Offset.X;
      neOffsetY.Value := Offset.Y;
    end;
  dec(frmUpdating);
end;

procedure TfrmCellGrid.UpdateObject;
begin
  with frmDoc.cg do
  begin
    Enabled := cbEnabled.Checked;
    Size.X := Round(neSizeX.Value);
    Size.Y := Round(neSizeY.Value);
    Spacing.X := Round(neSpacingX.Value);
    Spacing.Y := Round(neSpacingY.Value);
    Offset.X := Round(neOffsetX.Value);
    Offset.Y := Round(neOffsetY.Value);
  end;

  frmDoc.RedrawPaintBox;
end;

procedure TfrmCellGrid.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  cgSave := TCellGrid.Create;
  frmUpdating := 0;
  if VerboseMode then Log('TfrmCellGrid created');
end;

procedure TfrmCellGrid.FormDestroy(Sender: TObject);
begin
  cgSave.Free;
end;

procedure TfrmCellGrid.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmCellGrid.neChange(Sender: TObject);
begin
  if frmUpdating <> 0 then Exit;

  inc(frmUpdating);
    cbEnabled.Checked := True;
  dec(frmUpdating);
  
  UpdateObject;
end;

procedure TfrmCellGrid.cbEnabledClick(Sender: TObject);
begin
  if frmUpdating <> 0 then Exit;
  UpdateObject;
end;

procedure TfrmCellGrid.ApplyLanguagePack;
begin
  Caption := lpGet('MI_VIEW_CELL_GRID');
  cbEnabled.Caption := lpGet('CG_ENABLED');
  gbSize.Caption := lpGet('CG_SIZE');
  gbSpacing.Caption := lpGet('CG_SPACING');
  gbOffset.Caption := lpGet('CG_OFFSET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
