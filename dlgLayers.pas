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
unit dlgLayers;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, gfListBox, ExtCtrls, BitmapEx, bmExUtils, Filters, Layers, ieShared,
  Math, gfMath, LangPack, dlgDoc, UndoObject, Buttons, Menus, BlendModes;

type

  { TLayersFrame }

  TLayersFrame = class(TFrame)
    pmLayers: TPopupMenu;
    pToolbar: TPanel;
    lb: TgfListBox;
    sbNew: TSpeedButton;
    sbDelete: TSpeedButton;
    sbProp: TSpeedButton;
    sbMergeSelected: TSpeedButton;
    procedure lbGetCount(Sender: TObject; var Value: Integer);
    procedure lbListDblClick(Sender: TObject);
    procedure lbItemGetSelected(Sender: TObject; Index: Integer;
      var Value: Boolean);
    procedure lbItemSetSelected(Sender: TObject; Index: Integer;
      Value: Boolean);
    procedure lbKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lbItemPaint(Sender: TObject; ACanvas: TCanvas; Index: Integer;
      ItemRect: TRect; ListFocused: boolean);
    procedure lbItemDragDrop(Sender: TObject; TargetIndex: Integer);
    procedure pmLayersPopup(Sender: TObject);
    procedure sbNewClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure sbPropClick(Sender: TObject);
    procedure sbMergeSelectedClick(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lbItemMouseDown(Sender: TObject; Index: Integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      var MouseTrap: Boolean);
  public
    SelectionSelected: boolean;
    lbLastCount: integer;

    constructor Create(AOwner: TComponent); override;
    function DocForm: TGraphicFrame;
  end;

implementation

uses Main;

{$R *.lfm}

const
  // visibility indicator
  EYE_ICON_POS = 10;
  EYE_ICON_SIZE = 20;

constructor TLayersFrame.Create(AOwner: TComponent);
begin
  inherited;
  
  // load glyphs
  GetMiscGlyph(sbNew.Glyph, mgNew);
  GetMiscGlyph(sbDelete.Glyph, mgDelete);
  GetMiscGlyph(sbProp.Glyph, mgProperties);
  GetMiscGlyph(sbMergeSelected.Glyph, mgMergeSelected);

  SelectionSelected := False;
  lbLastCount := 0;
end;

function TLayersFrame.DocForm;
begin
  Result := frmMain.frmGraphicActive;
end;

procedure TLayersFrame.lbGetCount(Sender: TObject; var Value: Integer);
var
  ls: TLayers;

begin
  if Assigned(DocForm) then
  begin
    with DocForm do ls := Doc.Pages[ImageIndex].Layers;
    Value := ls.LayerCount;
    if ls.SelState = stFloating then inc(Value);
  end else
    Value := 0;
  lbLastCount := Value;
end;

procedure TLayersFrame.lbListDblClick(Sender: TObject);
begin
  sbProp.Click;
end;

procedure TLayersFrame.lbItemGetSelected(Sender: TObject; Index: Integer;
  var Value: Boolean);
var
  ls: TLayers;
  
begin
  if Assigned(DocForm) then
  begin
    with DocForm do ls := Doc.Pages[ImageIndex].Layers;

    if (ls.SelState = stFloating) and (Index = ls.Selection.Depth) then
      Value := SelectionSelected else
    begin
      if (ls.SelState = stFloating) and (Index > ls.Selection.Depth) then dec(Index);
      Value := ls[Index].Selected;
    end;
  end;
end;

procedure TLayersFrame.lbItemSetSelected(Sender: TObject; Index: Integer;
  Value: Boolean);
var
  ls: TLayers;

begin
  if Assigned(DocForm) then
  begin
    with DocForm do ls := Doc.Pages[ImageIndex].Layers;

    if (ls.SelState = stFloating) and (Index = ls.Selection.Depth) then
      SelectionSelected := Value else
    begin
      if (ls.SelState = stFloating) and (Index > ls.Selection.Depth) then dec(Index);
      ls[Index].Selected := Value;
    end;
  end;
end;

procedure TLayersFrame.lbKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift * [ssAlt, ssCtrl, ssShift] = []) then
    sbDelete.Click;
end;

procedure TLayersFrame.lbItemPaint(Sender: TObject; ACanvas: TCanvas;
  Index: Integer; ItemRect: TRect; ListFocused: boolean);
const
  HighlightColor: array[Boolean, Boolean] of TColor32 =
    (($20000000, $30a03030), ($20107010, $30008000));

var
  i, j, x, y: integer;
  IsSelection: boolean;
  s: string;
  ls: TLayers;
  l: TLayer;
  bmItem, bm: TBitmap32;
  r: TRect;
  Size: TPoint;

begin
  if not Assigned(DocForm) then Exit;

  with DocForm do ls := Doc.Pages[ImageIndex].Layers;
  IsSelection := (ls.SelState = stFloating) and (Index = ls.Selection.Depth);

  bmItem := TBitmap32.Create;
  try
    with bmItem do
    begin
      Resize(ItemRect.Width, ItemRect.Height);
      FillColor(TColor32(lb.Color) or cl32Opaque);

      // background
      if lb.Selected[Index] then
      begin
        r := ClientRect.Inflate(-4, -4);
        Rectangle(r, HighlightColor[IsSelection, ListFocused], True, 0);
        fltBoxBlur(bmItem, nil, 5, False, False, False);
        bmItem.Gradient(cl32Transparent, $20000000,
          Point(0, 0), Point(0, bmItem.Height),
          r, gkLinear, grNone);
      end;

      if not IsSelection then
      begin
        if (ls.SelState = stFloating) and (Index > ls.Selection.Depth) then
          l := ls[Index - 1] else l := ls[Index];

        // eye icon
        if l.Visible then
        begin
          if l.BlendMode = bmMask then i := EYE_ICON_SIZE else i := 0;
          x := EYE_ICON_POS; y := (Height - EYE_ICON_SIZE) div 2;
          CopyRect(x, y, bmEye, Rect(i, 0, i + EYE_ICON_SIZE, EYE_ICON_SIZE));
        end;
      end;

      // thumbnail
      if not IsSelection then
      begin
        bm := TBitmap32.Create;
        try
          bm.CreateThumbnail(l.Image, 30, False);

          x := 50 - bm.Width div 2;
          y := (Height - bm.Height) div 2;

          for i := Max(0, x) to Min(bmItem.Width, x + bm.Width) - 1 do
          for j := Max(0, y) to Min(bmItem.Height, y + bm.Height) - 1 do
            bmItem.PixelAddr(i, j)^ := cl32Transparent;

          Draw(x, y, bm);
        finally
          bm.Free;
        end;
      end;

      // caption
      Font.Name := 'Tahoma';
      Font.Size := 8;
      if IsSelection then
      begin
        Font.Style := [fsItalic];
        s := lpGet('LY_FLOATING_SEL');
      end else
        s := l.Name;
      Size := TextExtent(s);
      TextOut(75, (Height - Size.Y) div 2, s, cl32Black, True);

      DrawToCanvas(ACanvas, ItemRect.Left, ItemRect.Top, Pref_Hatch);
    end;
  finally
    bmItem.Free;
  end;
end;

procedure TLayersFrame.lbItemDragDrop(Sender: TObject; TargetIndex: Integer);
type
  TLayerEntry = record
    LayerIndex: integer; // -1 if selection
    Selected: boolean;
  end;
  
var
  i, n, SelectedPos, SelCount: integer;
  Found: boolean;
  ls: TLayers;
  a: array of TLayerEntry;
  OrderWithSel, Order: array of integer;
  tmpLayers: array of TLayer;
  uo: TLayerOrderUndo;

begin
  if not Assigned(DocForm) then Exit;
  ls := DocForm.Doc.Pages[DocForm.ImageIndex].Layers;

  SetLength(a, ls.LayerCount + Byte(ls.SelState = stFloating));

  // Create a temporary layer list which includes selection
  n := 0;
  for i := 0 to ls.LayerCount - 1 do
  begin
    if (ls.SelState = stFloating) and (ls.Selection.Depth = i) then
    begin
      a[n].LayerIndex := -1;
      a[n].Selected := SelectionSelected;
      inc(n);
    end;

    a[n].LayerIndex := i;
    a[n].Selected := ls[i].Selected;
    inc(n);
  end;

  SelCount := ls.SelectedCount +
    Byte((ls.SelState = stFloating) and SelectionSelected);

  // place non-selected layers at the beginning and the end of the list
  n := 0;
  SelectedPos := -1;
  SetLength(OrderWithSel, Length(a));
  for i := 0 to Length(a) - 1 do
    if not a[i].Selected then
  begin
    // found the place where selected layers will be inserted?
    if (i >= TargetIndex) and (SelectedPos < 0) then
    begin
      SelectedPos := n;
      inc(n, SelCount);
    end;

    OrderWithSel[n] := a[i].LayerIndex;
    inc(n);
  end;

  // place selected layers at SelectedPos, or to the end of the list
  if SelectedPos >= 0 then n := SelectedPos;
  for i := 0 to Length(a) - 1 do
    if a[i].Selected then
  begin
    OrderWithSel[n] := a[i].LayerIndex;
    inc(n);
  end;

  // Now we have the destination layer order in OrderWithSel
  // This may not differ from the source order, and then no undo is necessary
  Found := False;
  for i := 0 to Length(a) - 1 do if OrderWithSel[i] <> a[i].LayerIndex then
  begin
    Found := True;
    Break;
  end;
  if not Found then Exit; // no change to layer order

  uo := TLayerOrderUndo.Create('UNDO_MOVE_LAYERS', DocForm, DocForm.ImageIndex);

  // calculate layer order and new selection depth
  n := 0;
  SetLength(Order, ls.LayerCount);
  for i := 0 to Length(OrderWithSel) - 1 do
  begin
    if OrderWithSel[i] = -1 then ls.Selection.Depth := Min(ls.LayerCount - 1, i) else
    begin
      Order[n] := OrderWithSel[i];
      inc(n);
    end;
  end;

  // calculate and register undo
  for i := 0 to ls.LayerCount - 1 do uo.Permutation[i] := Order[i];
  DocForm.AddUndo(uo);

  // perform rearranging layers
  SetLength(tmpLayers, ls.LayerCount);
  for i := 0 to ls.LayerCount - 1 do tmpLayers[i] := ls[Order[i]];
  for i := 0 to ls.LayerCount - 1 do ls[i] := tmpLayers[i];

  // OK
  DocForm.RedrawPaintBox;
end;

procedure TLayersFrame.pmLayersPopup(Sender: TObject);
begin
  MenuItemToPopupMenu(frmMain.miLayers, Sender as TPopupMenu);
end;

procedure TLayersFrame.sbNewClick(Sender: TObject);
begin
  if Assigned(DocForm) then DocForm.DoLayerNew;
end;

procedure TLayersFrame.sbDeleteClick(Sender: TObject);
begin
  if Assigned(DocForm) then DocForm.DoLayerDelete;
end;

procedure TLayersFrame.sbPropClick(Sender: TObject);
begin
  if Assigned(DocForm) then DocForm.DoLayerProp;
end;

procedure TLayersFrame.sbMergeSelectedClick(Sender: TObject);
begin
  if Assigned(DocForm) then DocForm.DoLayerMerge(lssSelected);
end;

procedure TLayersFrame.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  frmMain.SetStatus('');
end;

procedure TLayersFrame.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  frmMain.SetStatus((Sender as TControl).Hint);
end;

procedure TLayersFrame.lbItemMouseDown(Sender: TObject; Index: Integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
  var MouseTrap: Boolean);
var
  ls: TLayers;
  
begin
  if not Assigned(DocForm) then Exit;

  if Button = mbLeft then
  begin
    ls := DocForm.Doc.Pages[DocForm.ImageIndex].Layers;
    if ls.SelState = stFloating then
    begin
      if Index = ls.Selection.Depth then Exit;
      if Index > ls.Selection.Depth then dec(Index);
    end;

    // toggle visibility
    if (X >= EYE_ICON_POS) and (X < EYE_ICON_POS + EYE_ICON_SIZE) then
    begin
      MouseTrap := True;

      ls[Index].Visible := not ls[Index].Visible;
      DocForm.RedrawPaintBox;
    end;
  end;
end;

end.
