(*
	Greenfish Controls - gfListBox
	Copyright © 2013 Balázs Szalkai
	This file is released under the zlib license:

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	   1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgment in the product documentation would be
	   appreciated but is not required.

	   2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.

	   3. This notice may not be removed or altered from any source
	   distribution.
*)
unit gfListBox;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Types, Classes, Controls, Forms, Graphics,
  StdCtrls, Math, DoubleBufPB, dialogs, GenericList;

const
  DBL_CLICK_INTERVAL = 400; // ms
  DBL_CLICK_JITTER_MAX = 2; // pixels

type
  TgflbLayout = (lbloHorizontal, lbloVertical);
  TgflbMouseAction = (lbmaNone, lbmaSelecting, lbmaStartDrag, lbmaDragging);

  TgflbGetInt = procedure(Sender: TObject; var Value: integer) of object;
  TgflbItemGetBool = procedure(Sender: TObject; Index: integer; var Value: boolean) of object;
  TgflbItemSetBool = procedure(Sender: TObject; Index: integer; Value: boolean) of object;
  TgflbItemGetPoint = procedure(Sender: TObject; Index: integer; var Value: TPoint) of object;

  TgflbItemDragDrop = procedure(Sender: TObject; TargetIndex: integer) of object;
  // Coordinates are relative to item bounding box
  // MouseTrap = True prevents further event handling
  TgflbItemMouseDown = procedure(Sender: TObject; Index: integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
    var MouseTrap: boolean) of object;
  TgflbItemMouseMove = procedure(Sender: TObject; Index: integer;
    Shift: TShiftState; X, Y: Integer) of object;
  TgflbItemMouseUp = procedure(Sender: TObject; Index: integer;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer) of object;
  TgflbItemPaint = procedure(Sender: TObject; ACanvas: TCanvas;
    Index: integer; ItemRect: TRect; ListFocused: boolean) of object;

  { TgfListBox }

  TgfListBox = class(TCustomControl)
  private
    FCanDragItems, FCanSelectNone, FMultiSelect: boolean;
    FLayout: TgflbLayout;
    FItemWidth, FItemHeight, FColumns: integer;

    FOnGetCount: TgflbGetInt;

    FOnItemGetSelected: TgflbItemGetBool;
    FOnItemGetSize: TgflbItemGetPoint;
    FOnItemMouseDown: TgflbItemMouseDown;
    FOnItemMouseMove: TgflbItemMouseMove;
    FOnItemMouseUp: TgflbItemMouseUp;
    FOnItemPaint: TgflbItemPaint;
    FOnItemSetSelected: TgflbItemSetBool;
    FOnItemDragDrop: TgflbItemDragDrop;

    FOnListDblClick: TNotifyEvent;
    FOnListMouseDown, FOnListMouseUp: TMouseEvent;
    FOnListMouseMove: TMouseMoveEvent;

    function GetSelected(Index: integer): boolean;
    procedure SetSelected(Index: integer; const Value: boolean);
    function GetItemIndex: integer;
    procedure SetItemIndex(Value: integer);

    procedure SetCanSelectNone(const Value: boolean);
    procedure SetMultiSelect(const Value: boolean);
    procedure SetLayout(const Value: TgflbLayout);
    procedure SetItemWidth(const Value: integer);
    procedure SetItemHeight(const Value: integer);
    procedure SetColumns(Value: integer);
  protected
    // The Count property must be cached to preserve integrity
    FCount: integer;
    FItemRect: array of TRect;

    FClientArea: TDoubleBufPB;
    FScrollBar: TScrollBar;

    LastPaintColor: TColor;
    NeedRedraw: boolean; // whole list
    NeedRedrawItemIndex: TGenericList<integer>;
    PrevClick: Cardinal; // GetTickCount() at previous mouse down event
    ma: TgflbMouseAction;
    pOrig: TPoint;
    iiOrig: integer;

    procedure ListDblClick; dynamic;
    procedure Resize; override;

    procedure caKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure caKillFocus(Sender: TObject);
    procedure caMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure caMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure caMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure caMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure caPaint(Sender: TObject);
    procedure caSetFocus(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);

    procedure SetScrollBarParams;
    procedure SetScrollBarPlacement;
    // Queries Count, item sizes and recalculates item rectangles
    procedure RefreshLayout;
    procedure RedrawListBoxWithoutSwapBuffers;

    function GetCursor: TCursor; override;
    procedure SetCursor(AValue: TCursor); override;
  public
    property Count: integer read FCount;
    property Selected[Index: integer]: boolean read GetSelected write SetSelected;
    // Returns the first selected item, or -1 if none.
    // ItemIndex := x -> only the xth item will be selected
    // ItemIndex := -1 (invalid index) -> all items deselected,
    //   except in CanSelectNone = False (in that case one item remains selected)
    property ItemIndex: integer read GetItemIndex write SetItemIndex;

    property ClientArea: TDoubleBufPB read FClientArea;
    property ScrollBar: TScrollBar read FScrollBar;

    function LastSelected: integer;
    function SelectedCount: integer;

    function ItemRect(Index: integer): TRect;
    function BorderSize: TPoint;
    // The rectangle of the client area of ClientArea :)
    function ListRect: TRect;
    // Returns an offset which is added to all FItemRect[]'s when drawn
    // This depends on border settings and ScrollBar.Position
    function LayoutOrigin: TPoint;

    function ItemVisible(Index: integer; Partial: boolean): boolean;
    // First and last visible items
    function FirstVisible(Partial: boolean): integer;
    function LastVisible(Partial: boolean): integer;
    function ItemAtPos(x, y: integer): integer;
    // Scrolls to an item
    procedure ScrollTo(Index: integer);
    // Must call Invalidate after this.
    procedure RedrawItem(Index: integer);
    procedure InvalidateItem(Index: integer);
    // Re-renders the whole listbox area. Call this instead of Repaint.
    procedure RedrawListBox;
    // Invalidates the whole listbox area. Call this instead of Invalidate.
    procedure InvalidateListBox;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property CanDragItems: boolean read FCanDragItems write FCanDragItems default False;
    property CanSelectNone: boolean read FCanSelectNone write SetCanSelectNone default True;
    property MultiSelect: boolean read FMultiSelect write SetMultiSelect default False;
    property Layout: TgflbLayout read FLayout write SetLayout default lbloVertical;
    property ItemWidth: integer read FItemWidth write SetItemWidth default 16;
    property ItemHeight: integer read FItemHeight write SetItemHeight default 16;
    // The following property specifies the column count (lbloVertical)
    // or the row count (lbloHorizontal)
    property Columns: integer read FColumns write SetColumns default 1;

    property OnGetCount: TgflbGetInt read FOnGetCount write FOnGetCount;
    property OnItemGetSelected: TgflbItemGetBool read FOnItemGetSelected write FOnItemGetSelected;
    property OnItemGetSize: TgflbItemGetPoint read FOnItemGetSize write FOnItemGetSize;
    property OnItemMouseDown: TgflbItemMouseDown read FOnItemMouseDown write FOnItemMouseDown;
    property OnItemMouseMove: TgflbItemMouseMove read FOnItemMouseMove write FOnItemMouseMove;
    property OnItemMouseUp: TgflbItemMouseUp read FOnItemMouseUp write FOnItemMouseUp;
    property OnItemPaint: TgflbItemPaint read FOnItemPaint write FOnItemPaint;
    property OnItemSetSelected: TgflbItemSetBool read FOnItemSetSelected write FOnItemSetSelected;
    property OnItemDragDrop: TgflbItemDragDrop read FOnItemDragDrop write FOnItemDragDrop;
    property OnListDblClick: TNotifyEvent read FOnListDblClick write FOnListDblClick;
    property OnListMouseDown: TMouseEvent read FOnListMouseDown write FOnListMouseDown;
    property OnListMouseMove: TMouseMoveEvent read FOnListMouseMove write FOnListMouseMove;
    property OnListMouseUp: TMouseEvent read FOnListMouseUp write FOnListMouseUp;

    property Align;
    property Anchors;
    property Color;
    property ParentColor;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;

    property OnKeyDown;
    property OnResize;
    property Cursor: TCursor read GetCursor write SetCursor default crDefault;
  end;

procedure Register;

implementation

function intvIntersect(i1, j1, i2, j2: integer): boolean;
begin
  Result := (i1 < j2) and (i2 < j1);
end;

function intvContains(i1, j1, i2, j2: integer): boolean;
begin
  Result := (i2 >= i1) and (j2 < j1);
end;

// TgfListBox

function TgfListBox.GetSelected(Index: integer): boolean;
begin
  Result := False;
  if Assigned(OnItemGetSelected) then OnItemGetSelected(Self, Index, Result);
end;

function TgfListBox.GetCursor: TCursor;
begin
  result := FClientArea.Cursor;
end;

procedure TgfListBox.SetCursor(AValue: TCursor);
begin
  FClientArea.Cursor := AValue;
end;

procedure TgfListBox.SetSelected(Index: integer; const Value: boolean);
var
  ItemIndexSave: integer;
begin
  if not Assigned(OnItemSetSelected) or (Index < 0) or (Index >= Count) then
    Exit;

  if Value <> Selected[Index] then
  begin
    // The user wants to select more than allowed
    if Value and (MultiSelect = False) and (SelectedCount = 1) then
    begin
      ItemIndexSave := ItemIndex;
      OnItemSetSelected(Self, ItemIndexSave, False);
      OnItemSetSelected(Self, Index, True);

      RedrawItem(ItemIndexSave);
      RedrawItem(Index);
      ClientArea.SwapBuffers;
    end else

    // The user wants to select less than allowed
    if not Value and (CanSelectNone = False) and (SelectedCount = 1) then
    begin
      // do nothing
    end else

    // Allowed operation
    begin
      OnItemSetSelected(Self, Index, Value);
      InvalidateItem(Index);
    end;
  end;
end;

function TgfListBox.GetItemIndex: integer;
begin
  for Result := 0 to Count - 1 do if Selected[Result] then Exit;
  Result := -1;
end;

procedure TgfListBox.SetItemIndex(Value: integer);
var
  i: integer;
  
begin
  if (Value >= 0) and (Value < Count) then
  begin
    // select Value
    Selected[Value] := True;
    // deselect other elements
    for i := 0 to Count - 1 do if i <> Value then Selected[i] := False;
  end else
  // select none
  begin
    if CanSelectNone then
      // deselect all
      for i := 0 to Count - 1 do Selected[i] := False else
      // select only one item
      for i := 0 to Count - 1 do if i <> ItemIndex then Selected[i] := False;
  end;
end;

procedure TgfListBox.SetCanSelectNone(const Value: boolean);
begin
  if FCanSelectNone <> Value then
  begin
    FCanSelectNone := Value;
    
    if (CanSelectNone = False) and (SelectedCount = 0) and (Count > 0) then
      Selected[0] := True;
  end;
end;

procedure TgfListBox.SetMultiSelect(const Value: boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;

    if (MultiSelect = False) and (SelectedCount > 1) then
      // select only one item
      ItemIndex := ItemIndex;
  end;
end;

procedure TgfListBox.SetLayout(const Value: TgflbLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;

    SetScrollBarPlacement;
    InvalidateListBox;
  end;
end;

procedure TgfListBox.SetItemWidth(const Value: integer);
begin
  if FItemWidth <> Value then
  begin
    FItemWidth := Value;
    InvalidateListBox;
  end;
end;

procedure TgfListBox.SetItemHeight(const Value: integer);
begin
  if FItemHeight <> Value then
  begin
    FItemHeight := Value;
    InvalidateListBox;
  end;
end;

procedure TgfListBox.SetColumns(Value: integer);
begin
  if Value < 1 then Value := 1;
  if FColumns <> Value then
  begin
    FColumns := Value;
    InvalidateListBox;
  end;
end;

procedure TgfListBox.ListDblClick;
begin
  if Assigned(OnListDblClick) then OnListDblClick(Self);
end;

procedure TgfListBox.Resize;
begin
  SetScrollBarParams;
  inherited;
end;

procedure TgfListBox.caKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i, Delta, iiNew, fv, lv: integer;

begin
  KeyDown(Key, Shift);
  // prevent capturing some command keystrokes
  if (ssAlt in Shift) or (ssAltGr in Shift) then Exit;
  
  case Key of
    VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_HOME, VK_END:
    begin
      if ItemIndex < 0 then
      begin
        ItemIndex := 0;
        ScrollTo(ItemIndex);
      end else
      begin
        case Key of
          VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT: begin
            Delta := IfThen((Layout = lbloHorizontal) xor
              ((Key = VK_LEFT) or (Key = VK_RIGHT)), 1, Columns);

            if (Key = VK_UP) or (Key = VK_LEFT) then
              iiNew := Max(0, ItemIndex - Delta) else
              iiNew := Min(Count - 1, LastSelected + Delta);
          end;

          VK_PRIOR, VK_NEXT: begin
            fv := FirstVisible(True);
            lv := LastVisible(True);
            
            if Key = VK_PRIOR then
              iiNew := IfThen(ItemIndex = fv, Max(0, 2*fv - lv), fv) else
              iiNew := IfThen(ItemIndex = lv, Min(Count - 1, 2*lv - fv), lv);
          end;

          VK_HOME: iiNew := 0;
          VK_END: iiNew := Count - 1;
        end;

        if (ssShift in Shift) and MultiSelect then
          // make a contiguous selection
          for i := Min(ItemIndex, iiNew) to Max(LastSelected, iiNew) do
            Selected[i] := True else
          // jump
          ItemIndex := iiNew;

        ScrollTo(iiNew);
      end; // if ItemIndex < 0
    end; // arrow keys

    VK_RETURN: ListDblClick;
  end;
end;

procedure TgfListBox.caKillFocus(Sender: TObject);
begin
  InvalidateListBox;
end;

procedure TgfListBox.caMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i, Index: integer;
  tc: Cardinal;
  IsDblClick, MouseTrap: boolean;
  r: TRect;

begin
  // set focus
  ClientArea.SetFocus;

  // double-click?
  if Button = mbLeft then
  begin
    tc := {$IFDEF LCL}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
    IsDblClick := (tc - PrevClick < DBL_CLICK_INTERVAL) and
      (Max(Abs(X - pOrig.X), Abs(Y - pOrig.Y)) < DBL_CLICK_JITTER_MAX);
    PrevClick := tc;

    // perform double click action
    if IsDblClick then
    begin
      ListDblClick;
      Exit;
    end;
  end;

  // locate selected item
  Index := ItemAtPos(X, Y);
  if Index >= 0 then
  begin
    // generate item event
    r := ItemRect(Index);
    MouseTrap := False;
    if Assigned(OnItemMouseDown) then OnItemMouseDown(Self, Index,
      Button, Shift, X - r.Left, Y - r.Top, MouseTrap);

    // selecting/dragging
    if (ma = lbmaNone) and not MouseTrap then
    begin
      pOrig := Point(X, Y);
      iiOrig := Index;
      ScrollTo(Index);

      if ssCtrl in Shift then Selected[Index] := not Selected[Index] else
      begin
        // select
        if (ssShift in Shift) and MultiSelect and (ItemIndex >= 0) then
          for i := Min(ItemIndex + 1, Index) to
            Max(LastSelected - 1, Index) do Selected[i] := True else
        // do not deselect other items if item is already selected and can drag
        if not (Selected[Index] and ((Button = mbRight) or CanDragItems)) then
          ItemIndex := Index;

        // determine mouse action
        if Button = mbLeft then
        if CanDragItems then ma := lbmaStartDrag else ma := lbmaSelecting;
      end;
    end; // select/drag
  end; // Index >= 0

  if Assigned(OnListMouseDown) then OnListMouseDown(Self, Button, Shift, X, Y);
end;

procedure TgfListBox.caMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  i, Index: integer;
  r: TRect;

begin
  // locate selected item
  Index := ItemAtPos(X, Y);

  case ma of
    lbmaSelecting: begin
      // select
      if (ssShift in Shift) and MultiSelect and (ItemIndex >= 0) then
        for i := Min(ItemIndex + 1, Index) to
          Max(LastSelected - 1, Index) do Selected[i] := True else
      ItemIndex := Index;

      ScrollTo(Index);
    end;

    lbmaStartDrag: if Max(Abs(X - pOrig.X), Abs(Y - pOrig.Y)) > 3 then
    begin
      ma := lbmaDragging;
      Screen.Cursor := crDrag;
    end;
  end;

  // events
  if Assigned(OnListMouseMove) then OnListMouseMove(Self, Shift, X, Y);
  if Assigned(OnItemMouseMove) and (Index >= 0) then
  begin
    r := ItemRect(Index);
    OnItemMouseMove(Self, Index, Shift, X - r.Left, Y - r.Top);
  end;
end;

procedure TgfListBox.caMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Index, DropIndex: integer;
  r: TRect;
  
begin
  // locate selected item
  Index := ItemAtPos(X, Y);

  // perform mouse actions
  case ma of
    lbmaStartDrag: if not (ssShift in Shift) then ItemIndex := iiOrig;
    lbmaDragging: if Assigned(OnItemDragDrop)
      and PtInRect(ClientArea.ClientRect, Point(X, Y)) then
    begin
      DropIndex := IfThen(Index < 0, Count, Index);
      OnItemDragDrop(Self, DropIndex);
      ScrollTo(DropIndex);
    end;
  end;

  ma := lbmaNone;
  Screen.Cursor := crDefault;

  // events
  if Assigned(OnListMouseUp) then OnListMouseUp(Self, Button, Shift, X, Y);
  if Assigned(OnItemMouseUp) and (Index >= 0) then
  begin
    r := ItemRect(Index);
    OnItemMouseUp(Self, Index, Button, Shift, X - r.Left, Y - r.Top);
  end;
end;

procedure TgfListBox.caMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  ScrollBar.Position := ScrollBar.Position - Sign(WheelDelta) * 30;
  Handled := True;
end;

procedure TgfListBox.caPaint(Sender: TObject);
var
  i: integer;
begin
  if LastPaintColor <> Color then
  begin
    LastPaintColor := Color;
    NeedRedraw := True;
  end;
  if NeedRedraw then
    RedrawListBoxWithoutSwapBuffers
  else if NeedRedrawItemIndex.Count > 0 then
  begin
    for i := 0 to NeedRedrawItemIndex.Count - 1 do
      RedrawItem(NeedRedrawItemIndex[i]);
    NeedRedrawItemIndex.Clear;
  end;
end;

procedure TgfListBox.RedrawListBox;
begin
  RedrawListBoxWithoutSwapBuffers;
  ClientArea.SwapBuffers;
end;

procedure TgfListBox.RedrawListBoxWithoutSwapBuffers;
var
  i, iMin, iMax: integer;

begin
  // initialize
  RefreshLayout;
  SetScrollBarParams;

  with ClientArea.Canvas do
  begin
    // draw background
    Brush.Color := Color;
    FillRect(ClipRect);

    // draw items
    if Assigned(OnItemPaint) then
    begin
      iMin := FirstVisible(True); iMax := LastVisible(True);
      if (iMin >= 0) and (iMax >= 0) then for i := iMin to iMax do
        OnItemPaint(Self, ClientArea.Canvas, i, ItemRect(i), ClientArea.Focused);
    end;

    // draw frame
    Brush.Color := clGray;
    FrameRect(ClipRect);
  end;
  NeedRedraw := False;
  NeedRedrawItemIndex.Clear;
end;

procedure TgfListBox.InvalidateListBox;
begin
  NeedRedraw := True;
  ClientArea.Invalidate;
end;

procedure TgfListBox.caSetFocus(Sender: TObject);
begin
  InvalidateListBox;
end;

procedure TgfListBox.ScrollBarChange(Sender: TObject);
begin
  RedrawListBox;
end;

procedure TgfListBox.SetScrollBarParams;
var
  sbMax: integer;
  
begin
  if Count = 0 then sbMax := 0 else
    sbMax := Max(0, IfThen(Layout = lbloHorizontal,
        FItemRect[Count - 1].Left,
        FItemRect[Count - 1].Top));

  ScrollBar.SetParams(ScrollBar.Position, 0, sbMax);
end;

procedure TgfListBox.SetScrollBarPlacement;
begin
  with ScrollBar do
  if Layout = lbloHorizontal then
  begin
    Align := alBottom;
    Kind := sbHorizontal;
    Height := 16;
  end else
  begin
    Align := alRight;
    Kind := sbVertical;
    Width := 16;
  end;
end;

procedure TgfListBox.RefreshLayout;
var
  i, j, w, h, q, Start, c: integer;
  pSize, Position: TPoint;

begin
  // Query count
  FCount := 0;
  if Assigned(OnGetCount) then OnGetCount(Self, FCount);

  // Calculate rectangles
  SetLength(FItemRect, Count);
  with ListRect do
  if Layout = lbloHorizontal then
    q := (Bottom - Top) div Columns else
    q := (Right - Left) div Columns;

  Position := Point(0, 0);
  w := 0; h := 0;
  for i := 0 to Count - 1 do
  begin
    // Query item size
    pSize := Point(ItemWidth, ItemHeight);
    if Assigned(OnItemGetSize) then OnItemGetSize(Self, i, pSize);

    w := Max(w, IfThen(Layout = lbloHorizontal, Max(0, pSize.X), q));
    h := Max(h, IfThen(Layout = lbloVertical, Max(0, pSize.Y), q));

    // end of row?
    if ((i+1) mod Columns = 0) or (i = Count - 1) then
    begin
      c := i mod Columns;
      Start := i - c;
      for j := 0 to c do
      begin
        FItemRect[Start + j] := Rect(Position.X, Position.Y, Position.X + w, Position.Y + h);
        if Layout = lbloHorizontal then inc(Position.Y, h) else inc(Position.X, w);
      end;

      // start next row
      if Layout = lbloHorizontal then
        Position := Point(Position.X + w, 0) else
        Position := Point(0, Position.Y + h);
      w := 0;
      h := 0;
    end; // end of row
  end; // for i
end;

procedure TgfListBox.RedrawItem(Index: integer);
begin
  // TODO query item size anew
  if Assigned(OnItemPaint) and ItemVisible(Index, true) then
  begin
    with ClientArea.Canvas do
    begin
      // draw item
      Brush.Color := Color;
      FillRect(ItemRect(Index));
      OnItemPaint(Self, ClientArea.Canvas, Index, ItemRect(Index), ClientArea.Focused);

      // draw frame
      Brush.Color := clGray;
      FrameRect(ClipRect);
    end;
  end;
end;

procedure TgfListBox.InvalidateItem(Index: integer);
begin
  if NeedRedrawItemIndex.IndexOf(Index) < 0 then
  begin
    NeedRedrawItemIndex.Add(Index);
    ClientArea.Invalidate;
  end;
end;

function TgfListBox.LastSelected: integer;
begin
  for Result := Count - 1 downto 0 do if Selected[Result] then Exit;
  Result := -1;
end;

function TgfListBox.SelectedCount: integer;
var
  i: integer;
  
begin
  Result := 0;
  for i := 0 to Count - 1 do if Selected[i] then inc(Result);
end;

function TgfListBox.ItemRect(Index: integer): TRect;
var
  p: TPoint;
  
begin
  if (Index < 0) or (Index >= Count) then Exit;

  Result := FItemRect[Index];
  p := LayoutOrigin;
  with Result do
  begin
    inc(Left, p.X); inc(Top, p.Y);
    inc(Right, p.X); inc(Bottom, p.Y);
  end;
end;

function TgfListBox.BorderSize: TPoint;
begin
  Result := Point(1, 1);
end;

function TgfListBox.ListRect: TRect;
begin
  Result := ClientArea.ClientRect;
  InflateRect(Result, -BorderSize.X, -BorderSize.Y);
end;

function TgfListBox.LayoutOrigin: TPoint;
begin
  Result := ListRect.TopLeft;
  if Layout = lbloHorizontal then
    dec(Result.X, ScrollBar.Position) else
    dec(Result.Y, ScrollBar.Position);
end;

function TgfListBox.ItemVisible(Index: integer; Partial: boolean): boolean;
var
  i1, j1, i2, j2: integer;
  r: TRect;
  
begin
  r := ItemRect(Index);
  if Layout = lbloHorizontal then
  begin
    i1 := ListRect.Left; j1 := ListRect.Right;
    i2 := r.Left; j2 := r.Right;
  end else
  begin
    i1 := ListRect.Top; j1 := ListRect.Bottom;
    i2 := r.Top; j2 := r.Bottom;
  end;

  Result := (Index >= 0) and (Index < Count) and
    ( (Partial and intvIntersect(i1, j1, i2, j2)) or
      (not Partial and intvContains(i1, j1, i2, j2)) );
end;

function TgfListBox.FirstVisible(Partial: boolean): integer;
var
  i: integer;

begin
  // TODO: binary search
  for i := 0 to Count - 1 do
    if ItemVisible(i, Partial) then
  begin
    Result := i;
    Exit;
  end;

  // no items are visible
  Result := -1;
end;

function TgfListBox.LastVisible(Partial: boolean): integer;
var
  i: integer;

begin
  // TODO: binary search
  for i := Count - 1 downto 0 do
    if ItemVisible(i, Partial) then
  begin
    Result := i;
    Exit;
  end;

  // no items are visible
  Result := -1;
end;

function TgfListBox.ItemAtPos(x, y: integer): integer;
var
  i: integer;
  
begin
  // TODO: binary search
  for i := 0 to Count - 1 do
    if PtInRect(ItemRect(i), Point(x, y)) then
  begin
    Result := i;
    Exit;
  end;

  Result := -1;
end;

procedure TgfListBox.ScrollTo(Index: integer);
var
  i: integer;
  r: TRect;
  
begin
  if Count <= 1 then
    Exit;
  Index := Max(0, Min(Index, Count-1));
  if not ItemVisible(Index, False) then
  begin 
    r := ItemRect(Index);
    
    with ListRect do
    if (r.Top < Top) or (r.Left < Left) then
      i := IfThen(Layout = lbloHorizontal, r.Left - Left, r.Top - Top) else
      i := IfThen(Layout = lbloHorizontal, r.Right - Right, r.Bottom - Bottom);
    ScrollBar.Position := ScrollBar.Position + i;
  end;
end;

constructor TgfListBox.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered := True;

  // initialize
  FCanDragItems := False;
  FCanSelectNone := True;
  FMultiSelect := False;
  FLayout := lbloVertical;
  FItemWidth := 16;
  FItemHeight := 16;
  FColumns := 1;

  FOnGetCount := nil;
  FOnItemGetSelected := nil;
  FOnItemGetSize := nil;
  FOnItemMouseDown := nil;
  FOnItemMouseMove := nil;
  FOnItemMouseUp := nil;
  FOnItemPaint := nil;
  FOnItemSetSelected := nil;
  FOnItemDragDrop := nil;
  FOnListDblClick := nil;
  FOnListMouseDown := nil;
  FOnListMouseMove := nil;
  FOnListMouseUp := nil;

  FCount := 0;

  PrevClick := 0;
  ma := lbmaNone;

  // create subcontrols
  FScrollBar := TScrollBar.Create(Self);
  with ScrollBar do
  begin
    Parent := Self;
    LargeChange := 20;
    OnChange := ScrollBarChange;
  end;

  FClientArea := TDoubleBufPB.Create(Self);
  ClientArea.Color := Color;
  LastPaintColor := Color;
  NeedRedraw := True;
  NeedRedrawItemIndex := TGenericList<integer>.Create;
  with ClientArea do
  begin
    ControlStyle := ControlStyle+[csOpaque];
    DrawToBitmap := true;
    EraseBkGnd := false;
    Parent := Self;
    Align := alClient;
    OnKeyDown := caKeyDown;
    OnKillFocus := caKillFocus;
    OnMouseDown := caMouseDown;
    OnMouseMove := caMouseMove;
    OnMouseUp := caMouseUp;
    OnMouseWheel := caMouseWheel;
    OnPaint := caPaint;
    OnSetFocus := caSetFocus;
  end;

  SetScrollBarPlacement;
end;

destructor TgfListBox.Destroy;
begin
  FreeAndNil(FClientArea);
  FreeAndNil(FScrollBar);
  FreeAndNil(NeedRedrawItemIndex);
  
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TgfListBox]);
end;

{$IFDEF LCL}
initialization
  {$I gfListBox.lrs}
{$ENDIF}
end.
