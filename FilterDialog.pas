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
unit FilterDialog;

{$mode delphi}

interface

uses
  Classes, SysUtils, Dialogs, Forms, Controls, dlgDoc, Layers, BitmapEx, gfMath;

type

  { TFilterDialog }

  TFilterDialog = class(TForm)
  private
    FFloatingSelectionMargin: TRect;
    procedure SetFloatingSelectionMargin(AValue: TRect);
  protected
    procedure RefreshSourceLayers;
  public
    // 0 = the form is currently not being updated
    // >0 = ui events should be discarded
    frmUpdating: integer;
    // The current document
    frmDoc: TGraphicFrame;
    // Stores frmDoc.Doc.Pages[frmDoc.ImageIndex].Layers
    ls: TLayers;

    // Information
    // What undo text should be displayed
    UndoText: string; // default: ''
    // Whether the dialog needs a floating selection whose size
    // is equal to that of SelRect
    NeedsApplyTransform: boolean; // default: false

    // Can set values for UndoText and NeedsApplyTransform
    procedure FillInfo; virtual;
    // Override this to provide custom filter behavior
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); virtual;
    // Apply the filter on the layers
    procedure InvokeEffect;
    // Show the filter dialog
    procedure Execute(_frmDoc: TGraphicFrame);

    constructor Create(AOwner: TComponent); override;

    // If the floating selection must be enlarged by a margin,
    // this rectangle contains the top, left, right, bottom margin values.
    // Left and top must be <=0, and right and bottom must be >=0.
    property FloatingSelectionMargin: TRect read FFloatingSelectionMargin write SetFloatingSelectionMargin;
  end;

implementation

// TFilterDialog

procedure TFilterDialog.SetFloatingSelectionMargin(AValue: TRect);
begin
  if FFloatingSelectionMargin.Equals(AValue) then Exit;
  FFloatingSelectionMargin:=AValue;
  RefreshSourceLayers;
end;

procedure TFilterDialog.RefreshSourceLayers;
begin
  // create source for filter
  if frmDoc <> nil then
  with frmDoc.lsSrc do
  begin
    Assign(frmDoc.lsSave);
    if (SelState = stFloating) and (NeedsApplyTransform
      or not FloatingSelectionMargin.IsZero) then
      ApplySelectionTransform(FloatingSelectionMargin);
  end;
end;

procedure TFilterDialog.FillInfo;
begin
  UndoText := '';
  NeedsApplyTransform := False;
end;

procedure TFilterDialog.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  // do nothing
end;

procedure TFilterDialog.InvokeEffect;
begin
  if frmDoc <> nil then
  begin
    ls.Assign(frmDoc.lsSrc);
    frmDoc.DoFilter(DoFilter);
  end;
end;

procedure TFilterDialog.Execute(_frmDoc: TGraphicFrame);
var
  mr: TModalResult;

begin
  frmDoc := _frmDoc;
  with frmDoc do
    ls := Doc.Pages[ImageIndex].Layers;

  // save image
  frmDoc.lsSave.Assign(ls);
  try
    RefreshSourceLayers;

    mr := ShowModal;
    // restore
    ls.Assign(frmDoc.lsSave);

    with frmDoc do
      if mr = mrOk then
      begin
        Modified := True;
        cuFilter(UndoText);
        InvokeEffect;
      end else
        RedrawPaintBox;
  except
    // restore image
    ls.Assign(frmDoc.lsSave);
  end;

  // free the resources
  frmDoc.lsSave.Clear;
  frmDoc.lsSrc.Clear;
end;

constructor TFilterDialog.Create(AOwner: TComponent);
begin
  inherited;

  frmUpdating := 0;
  FFloatingSelectionMargin := Rect(0, 0, 0, 0);
  FillInfo;
end;

end.

