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
unit dlgDoc;

interface

uses
  {$IFDEF WINDOWS} Windows, {$ENDIF} LclIntf, LclType,
  Types, SysUtils, Variants, Classes, Graphics, Controls, Forms, FileUtil,
  Dialogs, Math, StdCtrls, ExtCtrls, DoubleBufPB,
  BitmapEx, bmExUtils, Layers, DocClass, UndoObject, ieShared, Buttons,
  dlgDocPage, Menus, Clipbrd, ImageTransform, Filters, PixelFormats,
  gfMath, LangPack, BMP, PNG, ICNS, StrUtils, gfListBox,
  BlendModes, dlgLib, dlgStartupFrame, dlgDebug;

const
  MaxShrink = 8;
  MaxZoom = 30;
  MaxPolyCount = 10000;

  clPaintBoxBgr = $f0f0f0;
  dtHasInvertedOutline = [dtSelRect, dtSelEllipse, dtLasso, dtSelPencil, dtCrop];
  FAST_SCROLL = true;

type
  TPolyPoints = array[0..MaxPolyCount - 1] of TPoint;
  TColor32Pair = array[0..1] of TColor32;
  // Icon Editor mouse action
  TieMouseAction = (iemaNone, iemaDrawing,
    iemaSizing, iemaMoving, iemaRotating, iemaHand);
  TdtTransformHitTest = (xfNone, xfSize, xfMove, xfRotate);
  TFilterCallback = procedure(bm: TBitmap32; Mask: TBitmap1) of object;

  TCellGrid = class(TPersistent)
  public
    Enabled: boolean;
    Size, Spacing, Offset: TPoint;

    constructor Create;
    procedure Clear;
    procedure Assign(Src: TPersistent); override;
  end;

  { TGraphicFrame }

  TGraphicFrame = class(TDocumentFrame)
    pBody: TPanel;
    pmPages: TPopupMenu;
    pToolbar: TPanel;
    sbPages: TSpeedButton;
    sbZoomOut: TSpeedButton;
    sbGrid: TSpeedButton;
    Bevel1: TBevel;
    sbZoomIn: TSpeedButton;
    sbZoom1: TSpeedButton;
    sbZoomFit: TSpeedButton;
    cbZoom: TComboBox;
    tmMarquee: TTimer;
    Bevel2: TBevel;
    sbNewPage: TSpeedButton;
    sbDeletePage: TSpeedButton;
    sbCenterLines: TSpeedButton;
    pDrawing: TPanel;
    sbX: TScrollBar;
    sbY: TScrollBar;
    pb: TDoubleBufPB;
    Bevel3: TBevel;
    sbTest: TSpeedButton;
    sbSave: TSpeedButton;
    Bevel4: TBevel;
    pPages: TPanel;
    lb: TgfListBox;
    procedure cbZoomChange(Sender: TObject);
    procedure lbListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure lbResize(Sender: TObject);
    procedure pmPagesPopup(Sender: TObject);
    procedure sbPagesClick(Sender: TObject);
    procedure sbXChange(Sender: TObject);
    procedure sbYChange(Sender: TObject);
    procedure pbResize(Sender: TObject);
    procedure sbZoomInClick(Sender: TObject);
    procedure sbZoomOutClick(Sender: TObject);
    procedure sbGridClick(Sender: TObject);
    procedure sbZoomFitClick(Sender: TObject);
    procedure pbUpdateCursor;
    procedure sbZoom1Click(Sender: TObject);
    procedure tmMarqueeTimer(Sender: TObject);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure sbNewPageClick(Sender: TObject);
    procedure sbDeletePageClick(Sender: TObject);
    procedure miNewPageClick(Sender: TObject);
    procedure miDeletePageClick(Sender: TObject);
    procedure lbKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure miPagePropClick(Sender: TObject);
    procedure pbKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure pbKillFocus(Sender: TObject);
    procedure pbSetFocus(Sender: TObject);
    procedure sbCenterLinesClick(Sender: TObject);
    procedure pbMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure sbTestClick(Sender: TObject);
    procedure sbSaveClick(Sender: TObject);
    procedure lbGetCount(Sender: TObject; var Value: integer);
    procedure lbListDblClick(Sender: TObject);
    procedure lbItemGetSelected(Sender: TObject; Index: integer;
      var Value: boolean);
    procedure lbItemSetSelected(Sender: TObject; Index: integer; Value: boolean);
    procedure lbItemMouseMove(Sender: TObject; Index: integer;
      Shift: TShiftState; X, Y: integer);
    procedure lbItemMouseDown(Sender: TObject; Index: integer;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer;
      var MouseTrap: boolean);
    procedure lbItemDragDrop(Sender: TObject; TargetIndex: integer);
    procedure lbItemPaint(Sender: TObject; ACanvas: TCanvas; Index: integer;
      ItemRect: TRect; ListFocused: boolean);
  private
    FZoom, FImageIndex: integer;

    function GetImageIndex: integer;
    procedure SetImageIndex(Value: integer);
    procedure SetZoom(Value: integer);
  public
    // <> 0: blocks events
    frmUpdating: integer;
    // true -> prevents repainting
    frmDestroying: boolean;

    // The document object
    Doc: TIconDoc;
    // Cell grid properties
    cg: TCellGrid;
    // 1) A buffer for saving the current document layers state
    // 2) A buffer for storing the original layers (when applying filters)
    //  --- lsSrc can differ from lsSave because
    // TDocPage.ApplySelectionTransform may have been called
    lsSave, lsSrc: TLayers;
    // Last scroll bar positions.
    lastScrollPosValid: boolean;
    lastScrollPos: TPoint;
    lastDrawPos: TPoint; // the last value of xToView(0),yToView(0)
    lastDrawnRect: TRect; // last part of bitmap drawn when calling RedrawPaintBox

    // DrawMask: A bm for storing the pencil/brush layer mask
    // DrawSketch: A bm for storing a shape overlay
    DrawMask: TBitmap1;
    DrawSketch: TBitmap32;
    // Indicates the part of DrawSketch which contains the useful information
    // i.e. the "support" of the currently drawn shape
    rUpdate: TRect;
    // lsResult: Layers for showing the result of drawing w/o altering the original ones
    lsResult: TLayers;
    // The active drawing and background color
    // Swapped if right button was pressed
    SelColor: TColor32Pair;
    // A bitmap for storing temporary selection masks
    SelSketch: TBitmap1;
    // The current selection/hot spot marquee offset
    MarqueeOffset: integer;
    NeedRedrawPaintBoxOnActivate: boolean;

    // Current Mouse Action
    ma: TieMouseAction;
    // Points which store information while using a tool
    pOrig, pPrev, pCurr, pKnob, pOrigView, pOrigSBPos: TPoint;
    qOrig: TQuad; // original selection quad, before transforming
    xResizeDiff, yResizeDiff, AngleDiff: double;
    // Mouse button pressed
    mb: TMouseButton;
    // Supersampling ratio (1 shl AA_BIT_SHIFT, if aaEnabled)
    aaRes: integer;
    // Used when drawing a polygon
    FPolyCount: integer;
    FPoints: TPolyPoints;

    // In fact a property, however, there is no need to handle it that way
    FileType: TImageFileType;

    // For measuring performance
    FPSCounter: integer;
    FPSLastTick: integer;

    procedure DocFrameTabAssigned; override;
    destructor Destroy; override;
    function DocFrameCanClose: boolean; override;

    procedure ApplyLanguagePack;

    function xToView(i: double): integer;
    function yToView(i: double): integer;
    function xFromView(i: integer): double;
    function yFromView(i: integer): double;
    // rect of (even partially) visible pixels
    function GetVisibleRect: TRect;
    // quadrilateral enclosing then floating selection
    // (in window coordinates, if ToView is enabled)
    function GetFloatingQuad(out q: TQuad; ToView: boolean): boolean;
    procedure zoomFitWindow;

    // update scroll bar ranges
    procedure UpdateScrollBars;
    // updates caption (filename, modified)
    procedure UpdateCaption; override;
    // if dtTransform is selected, returns if the mouse is over a
    // knob/edge/floating image
    function dtTransformHitTest(X, Y: integer; out xKnob, yKnob: integer;
      out xDiff, yDiff: double): TdtTransformHitTest;

    // redraws the selection marquee
    procedure DrawSelectionMarquee;
    // redraws the red hot spot cross
    procedure DrawHotSpotFlasher;
    // redraws all marquees needed, returns if any was drawn
    function DrawMarquees: boolean;
    // draws an inverted rect/ellipse/etc. when using a tool
    procedure DrawInvertedOutline;
    // User bitmap: the [temporary drawing result] or [page image]
    // That's what the user sees.
    procedure RenderUserBitmap(Buffer: TBitmap32; const rClip: TRect; IndexOfPage: integer);
    function ClipClippingRect(rClip: TRect): TRect; // returns the actual rClip
    function DrawUserBitmap(rClip: TRect): TRect; // returns the actual rClip
    // draw pixel grid and cell grid
    procedure DrawGrid;
    // draw image frame
    procedure DrawImageFrame;
    // draw knobs which resize the floating selection
    procedure DrawSelectionKnobs;
    // scrolls bmPaintBox (call this when scrollbars change)
    procedure ScrollPaintBox;
    // redraws bmPaintBox and invalidates the PB
    procedure RedrawPaintBox;
    procedure RedrawPageListItem;
    procedure RedrawLayerList;
    // draws other image view elements (which are not the pixels)
    // and updates controls
    procedure DrawEverythingElseAndUpdateView;
    // redraws a part of bmPaintBox -- call this if a rectangle of the current document
    // changed
    procedure RedrawPaintBoxPart(const rImageChanged: TRect;
      UpdateLastDrawnRect: boolean=false; OnlyDrawPixels: boolean=false);

    // Call when the count of pages have changed
    procedure PageCountChanged;
    // Call when the size of the active page has changed
    procedure PageSizeChanged;

    // Insert a new page, with a 'Background' layer
    function pgInsert(Index: integer): TDocPage;
    // Delete an existing page
    procedure pgDelete(Index: integer);
    // Move a page to another position
    procedure pgMove(Index, Index2: integer);

    procedure PerformUndoRedo(Undo: boolean); override;

    // --- Undo constructors
    // Saves all layers and selection state
    // Saves only selected layers if OnlySelected
    procedure cuAllLayersChanged(const Caption: string; OnlySelected: boolean);
    // Use this before the floating selection is created
    procedure cuCreateFloating(const Caption: string);
    procedure cuEditDelete(const Caption: string);
    // cREATEuNDO Filter: case SelState
    // stNone, stSelecting -> saves selected layers
    // stFloating -> saves floating selection
    procedure cuFilter(const Caption: string);
    procedure cuFlipRotate(const Caption: string);
    // Creates an undo before a change to multiple frame rates
    procedure cuFrameRates(const Caption: string);
    // Creates an undo before hot spot change
    procedure cuHotSpot(const Caption: string);

    // Call this before deleting all selected layers
    procedure cuLayerDelete(const Caption: string);
    // Call this AFTER inserting a layer at Depth
    procedure cuLayerInsert(const Caption: string; Depth: integer);
    procedure cuLayerMerge(const Caption: string; Subset: TLayerSubset);
    procedure cuLayerProp(const Caption: string; LayerIndex: integer);

    procedure cuPageDelete(const Caption: string; Index: integer);
    procedure cuPageInsert(const Caption: string; Index: integer);
    procedure cuPageMove(const Caption: string; Index, Index2: integer);

    // Saves all selected layers
    procedure cuPaint(const Caption: string);
    // Use this undo constructor before
    //          - the floating selection (if present) is flattened
    // and then - the Selection object is edited
    // e.g.: selecting something, pasting, etc.
    // Saves Selection and, if stFloating, the layer underneath
    procedure cuSelect(const Caption: string);
    // Use this undo before modifying the floating selection box
    // (whilst creating a floating selection, if state was stSelecting)
    procedure cuTransform(const Caption: string);

    // Actions
    procedure DoLayerNew;
    procedure DoLayerDupl;
    procedure DoLayerDelete;
    procedure DoLayerProp;
    procedure DoLayerMerge(Subset: TLayerSubset);
    procedure DoLayerFromSel;

    procedure DoPageNew;
    procedure DoPageDelete;
    procedure DoPageMove(Index, Index2: integer);
    procedure DoPageProp;
    procedure DoPageImport;
    procedure DoPageExport(ExportAll: boolean);

    procedure DoTogglePages;
    procedure DoUniformFrameRate(fr: integer);

    procedure DoEditCopy;
    procedure DoEditPaste(NewPage: boolean);
    procedure DoEditDelete;

    procedure DoInsertText(const Text: string; _Font: TFont;
      Color: TColor32; AntiAlias: boolean);

    procedure DoSelectAll;
    procedure DoDeselectAll;
    procedure DoInvertSelection;
    procedure DoLoadSelection(const FileName: string);
    procedure DoSaveSelection(const FileName: string; FileType: TImageFileType);

    procedure DoCropTransparency(CreateUndo: boolean);
    procedure DoFlipRotate(Mode: TSimpleFlipRotate);

    function DoLoad: boolean;
    function DoSave(const fn: string; ft: TImageFileType;
      Options: TDocumentSaveOptions): boolean;

    // Create undo, apply transform and use the filter proc
    procedure UseFilter(Proc: TFilterCallback; NeedApplySelTransform: boolean;
      const sUndoText: string);
    // Call only the filter algorithm
    procedure DoFilter(Proc: TFilterCallback);
    // Filter callbacks
    // They are to be passed to DoFilter
    procedure DoFilter_Grayscale(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_Invert(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_Solarize(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_Average(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_SoftBlur(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_BlurMore(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_Sharpen(bm: TBitmap32; Mask: TBitmap1);
    procedure DoFilter_PaintContour(bm: TBitmap32; Mask: TBitmap1);

    // Draws the floating selection on all selected layers
    procedure MakeSelectionStamp(ls: TLayers);

    property ImageIndex: integer read GetImageIndex write SetImageIndex;
    property Zoom: integer read FZoom write SetZoom;
  end;

implementation

uses dlgToolSet, Main, dlgText,
  dlgTest, dlgLayerProp, dlgSaveOptions, dlgSvgOpenOptions;

{$R *.lfm}

procedure PaintAntialiasMask4(Dest: TBitmap32; Mask: TBitmap1;
  Color: TColor32; rClip: TRect);
const
  SumOfDigits: array[0..$f] of integer = (0, 1, 1, 2, 1, 2, 2, 3,
    1, 2, 2, 3, 2, 3, 3, 4);

var
  i, j, k, x, y, xMin, xMax, iMax: integer;
  Gradient: array[0..16] of TColor32;
  pDest: PColor32;

begin
  // cache colors
  for i := 0 to 16 do
    if Color = cl32Inverted then
      Gradient[i] := IfThen(i < 8, cl32Transparent, cl32Inverted)
    else
      Gradient[i] := BlendColors(cl32Transparent, Color, ($ff * i + 8) div 16);

  // NOT shr 2, we may deal with negative numbers !
  xMin := rClip.Left div 4;
  xMax := (rClip.Right - 1) div 4;
  for y := rClip.Top div 4 to (rClip.Bottom - 1) div 4 do
  begin
    pDest := Dest.PixelAddr(xMin, y);
    iMax := Min(Mask.Height, (y + 1) shl 2) - 1;

    for x := xMin to xMax do
    begin
      k := 0;

      for i := y shl 2 to iMax do
      begin
        j := Mask.ScanLine(i)[x shr 1];
        if Odd(x) then
          j := j shr 4
        else
          j := j and $f;
        Inc(k, SumOfDigits[j]);
      end;

      pDest^ := Gradient[k];
      inc(pDest);
    end;
  end;
end;

// TCellGrid

constructor TCellGrid.Create;
begin
  inherited;
  Clear;
end;

procedure TCellGrid.Clear;
begin
  Enabled := False;
  Size := Point(16, 16);
  Spacing := Point(0, 0);
  Offset := Point(0, 0);
end;

procedure TCellGrid.Assign;
var
  x: TCellGrid;

begin
  if Src is TCellGrid then
  begin
    x := Src as TCellGrid;

    Enabled := x.Enabled;
    Size := x.Size;
    Spacing := x.Spacing;
    Offset := x.Offset;
  end
  else
    inherited;
end;

// TGraphicFrame

function TGraphicFrame.GetImageIndex: integer;
begin
  // range check
  SetImageIndex(FImageIndex);

  Result := FImageIndex;
end;

procedure TGraphicFrame.SetImageIndex(Value: integer);
begin
  Value := Max(0, Min(Doc.PageCount - 1, Value));

  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    lb.ScrollTo(Value);
    PageSizeChanged;
  end;
end;

procedure TGraphicFrame.SetZoom(Value: integer);
var
  s: string;

begin
  if (Value = 0) or (Value = -1) then
    Value := 1;
  Value := Max(-MaxShrink, Min(MaxZoom, Value));

  if FZoom <> Value then
  begin
    FZoom := Value;

    Inc(frmUpdating);
    s := IntToStr(Abs(Zoom));
    cbZoom.ItemIndex := cbZoom.Items.IndexOf(IfThen(Zoom > 0, s + 'x', '1/' + s));
    Dec(frmUpdating);

    UpdateScrollBars;
    RedrawPaintBox;
  end;
end;

procedure TGraphicFrame.DocFrameTabAssigned;
var
  i: integer;

begin
  frmUpdating := 0;
  frmDestroying := False;

  // initialize Zoom combo box
  Inc(frmUpdating);
  for i := MaxShrink downto 2 do
    cbZoom.Items.Add('1/' + IntToStr(i));
  for i := 1 to MaxZoom do
    cbZoom.Items.Add(IntToStr(i) + 'x');
  cbZoom.ItemIndex := MaxShrink - 1;
  Dec(frmUpdating);
  pToolbar.Height := Max(pToolbar.Height, cbZoom.Top+cbZoom.Height); // for Linux

  // load tool button glyphs
  GetMiscGlyph(sbSave.Glyph, mgSave);
  GetMiscGlyph(sbNewPage.Glyph, mgNew);
  GetMiscGlyph(sbDeletePage.Glyph, mgDelete);
  GetMiscGlyph(sbZoom1.Glyph, mgZoom1);
  GetMiscGlyph(sbZoomOut.Glyph, mgMinus);
  GetMiscGlyph(sbZoomIn.Glyph, mgPlus);
  GetMiscGlyph(sbZoomFit.Glyph, mgZoomFit);
  GetMiscGlyph(sbGrid.Glyph, mgGrid);
  GetMiscGlyph(sbCenterLines.Glyph, mgCenterLines);
  GetMiscGlyph(sbPages.Glyph, mgPages);
  GetMiscGlyph(sbTest.Glyph, mgTest);

  Doc := TIconDoc.Create;
  cg := TCellGrid.Create;
  lsSave := TLayers.Create;
  lsSrc := TLayers.Create;
  lastScrollPosValid := False;
  lastScrollPos := Point(sbX.Position, sbY.Position);

  DrawMask := TBitmap1.Create;
  DrawSketch := TBitmap32.Create;
  lsResult := TLayers.Create;
  SelSketch := TBitmap1.Create;

  MarqueeOffset := 0;
  NeedRedrawPaintBoxOnActivate := false;

  // we have to insert at least one page
  pgInsert(0);

  // init the properties
  FZoom := 1;
  FImageIndex := 0;
  FileType := iftNone;

  UpdateCaption;
  ApplyLanguagePack;

  frmMain.TabVisible[TStartupScreenTab] := False;

  FPSCounter := 0;
  FPSLastTick := GetTickCount; // do not use GetTickCount64! Won't work on older systems.
end;

destructor TGraphicFrame.Destroy;
begin
  // prevent form from painting and accessing freed components
  frmDestroying := True;

  FreeAndNil(Doc);
  FreeAndNil(cg);
  FreeAndNil(lsSave);
  FreeAndNil(lsSrc);
  FreeAndNil(DrawMask);
  FreeAndNil(DrawSketch);
  FreeAndNil(lsResult);
  FreeAndNil(SelSketch);

  inherited;
end;

procedure TGraphicFrame.ApplyLanguagePack;
begin
  UpdateCaption;
  lb.InvalidateListBox;

  sbSave.Hint := lpGet('MI_FILE_SAVE') + ' (Ctrl+S)';
  sbNewPage.Hint := lpGet('MI_ICON_PAGE_NEW') + ' (Shift+Ctrl+N)';
  sbDeletePage.Hint := lpGet('MI_ICON_PAGE_DELETE');
  sbZoom1.Hint := lpGet('MI_VIEW_100_PERCENT') + ' (*)';
  sbZoomOut.Hint := lpGet('MI_VIEW_ZOOM_OUT') + ' (-)';
  cbZoom.Hint := lpGet('HINT_DOC_ZOOM_MENU');
  sbZoomIn.Hint := lpGet('MI_VIEW_ZOOM_IN') + ' (+)';
  sbZoomFit.Hint := lpGet('MI_VIEW_FIT_WINDOW') + ' (/)';
  sbGrid.Hint := lpGet('MI_VIEW_GRID') + ' (Ctrl+G)';
  sbCenterLines.Hint := lpGet('MI_VIEW_CENTER_LINES');
  sbTest.Hint := lpGet('MI_ICON_TEST') + ' (Shift+Ctrl+T)';
  sbPages.Hint := lpGet('HINT_DOC_TOGGLE_PAGES') + ' (F5)';
end;

function TGraphicFrame.xToView(i: double): integer;
begin
  if Zoom < 0 then
    // shrink
    Result := pb.Width div 2 - Doc.Pages[ImageIndex].Layers.Width div
      (2 * -Zoom) + Floor(i / -Zoom) - sbX.Position div -Zoom
  else
    // zoom
    Result := pb.Width div 2 - Doc.Pages[ImageIndex].Layers.Width *
      Zoom div 2 + Round((i - sbX.Position) * Zoom);
end;

function TGraphicFrame.yToView(i: double): integer;
begin
  if Zoom < 0 then
    // shrink
    Result := pb.Height div 2 - Doc.Pages[ImageIndex].Layers.Height div
      (2 * -Zoom) + Floor(i / -Zoom) - sbY.Position div -Zoom
  else
    // zoom
    Result := pb.Height div 2 - Doc.Pages[ImageIndex].Layers.Height *
      Zoom div 2 + Round((i - sbY.Position) * Zoom);
end;

function TGraphicFrame.xFromView(i: integer): double;
begin
  if Zoom < 0 then
    Result := (i - xToView(0)) * -Zoom
  else
    Result := (i - xToView(0)) / Zoom;
end;

function TGraphicFrame.yFromView(i: integer): double;
begin
  if Zoom < 0 then
    Result := (i - yToView(0)) * -Zoom
  else
    Result := (i - yToView(0)) / Zoom;
end;

function TGraphicFrame.GetVisibleRect: TRect;
begin
  with Doc.Pages[ImageIndex].Layers do
    Result := Rect(Max(0, Floor(xFromView(0))), Max(0, Floor(yFromView(0))),
      Min(Width, Ceil(xFromView(pb.Width))),
      Min(Height, Ceil(yFromView(pb.Height))));
end;

function TGraphicFrame.GetFloatingQuad(out q: TQuad; ToView: boolean): boolean;
var
  i: integer;

begin
  with Doc.Pages[ImageIndex].Layers do
  begin
    if SelState <> stFloating then
      Exit(False);

    // rotate the box by angle
    with Selection do
      q := RotateRectToQuad(Box, Box.xCenter, Box.yCenter, Selection.Angle);
    // convert to screen coords
    if ToView then
      for i := 0 to 3 do
      begin
        q[i].x := xToView(q[i].x);
        q[i].y := yToView(q[i].y);
      end;

    Result := True;
  end;
end;

procedure TGraphicFrame.zoomFitWindow;
var
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  if (ls.Width = 0) or (ls.Height = 0) or (pb.Width = 0) or (pb.Height = 0) then
    Zoom := 1
  else
  if (ls.Width > pb.Width) or (ls.Height > pb.Height) then
    Zoom := -(1 + Max(ls.Width div pb.Width, ls.Height div pb.Height))
  else
    Zoom := Min(pb.Width div ls.Width, pb.Height div ls.Height);

  sbX.Position := 0;
  sbY.Position := 0;
end;

procedure TGraphicFrame.UpdateScrollBars;
begin
  lastScrollPosValid := False;
  with Doc.Pages[ImageIndex].Layers do
  begin
    sbX.SetParams(sbX.Position, -Width div 2, Width div 2);
    sbY.SetParams(sbY.Position, -Height div 2, Height div 2);
  end;
end;

procedure TGraphicFrame.UpdateCaption;
var
  s: string;

begin
  if FileName = '' then
    s := lpGet('UNTITLED')
  else
    s := SysToUTF8(ExtractFileName(FileName));
  if Modified then
    s := s + '*';

  TabCaption := s;
end;

function TGraphicFrame.dtTransformHitTest(X, Y: integer;
  out xKnob, yKnob: integer; out xDiff, yDiff: double): TdtTransformHitTest;
const
  htBuffer = 4;

var
  q: TQuad;
  OnLeft, OnRight, OnTop, OnBottom: boolean;

begin
  Result := xfNone;
  xKnob := 0;
  yKnob := 0;
  xDiff := 0;
  yDiff := 0;

  if Doc.Pages[ImageIndex].Layers.SelState <> stFloating then
    Exit;
  if not GetFloatingQuad(q, True) then
    Exit;

  // rotate?
  if Sqr(X - 0.5 * (q[0].x + q[3].x)) + Sqr(Y - 0.5 * (q[0].y + q[3].y)) <
    Sqr(htBuffer * 2) then
    Exit(xfRotate);

  OnLeft := PointSegmentDist(X, Y, q[0].x, q[0].y, q[1].x, q[1].y) < htBuffer;
  OnBottom := PointSegmentDist(X, Y, q[1].x, q[1].y, q[2].x, q[2].y) < htBuffer;
  OnRight := PointSegmentDist(X, Y, q[2].x, q[2].y, q[3].x, q[3].y) < htBuffer;
  OnTop := PointSegmentDist(X, Y, q[3].x, q[3].y, q[0].x, q[0].y) < htBuffer;

  if OnLeft or OnRight or OnTop or OnBottom then
  begin
    Result := xfSize;

    if OnLeft then
    begin
      xKnob := -1;
      xDiff := PointLineDist(X, Y, q[2].x, q[2].y, q[3].x, q[3].y) -
        PointLineDist(q[0].x, q[0].y, q[2].x, q[2].y, q[3].x, q[3].y);
    end
    else
    if OnRight then
    begin
      xKnob := 1;
      xDiff := PointLineDist(X, Y, q[0].x, q[0].y, q[1].x, q[1].y) -
        PointLineDist(q[2].x, q[2].y, q[0].x, q[0].y, q[1].x, q[1].y);
    end
    else
      xKnob := 0;

    if OnTop then
    begin
      yKnob := -1;
      yDiff := PointLineDist(X, Y, q[1].x, q[1].y, q[2].x, q[2].y) -
        PointLineDist(q[3].x, q[3].y, q[1].x, q[1].y, q[2].x, q[2].y);
    end
    else
    if OnBottom then
    begin
      yKnob := 1;
      yDiff := PointLineDist(X, Y, q[3].x, q[3].y, q[0].x, q[0].y) -
        PointLineDist(q[1].x, q[1].y, q[3].x, q[3].y, q[0].x, q[0].y);
    end
    else
      yKnob := 0;
  end
  else
    Result := xfMove;
end;

procedure TGraphicFrame.DrawSelectionMarquee;
var
  ls: TLayers;
  c: array[boolean] of TColor;
  sl: TScanLines;
  bmMask: TBitmap1;
  i, j, x, y, x2, y2, q: integer;
  rVisible: TRect;
  Quad: TQuad;
  pt: TDynPointArray;

  procedure _PutPixel(x, y: integer);
  begin
    if (x >= 0) and (x < pb.Width) and (y >= 0) and
      (y < pb.Height) then
      Move3(@c[(x + y + MarqueeOffset) and 4 <> 0], @sl[y][x * sl.BytesPerPixel]);
  end;

  procedure _QuadLine(n1, n2: integer);
  var
    i: integer;

  begin
    EnumLinePoints(Round(Quad[n1].x), Round(Quad[n1].y),
      Round(Quad[n2].x), Round(Quad[n2].y), pt);
    for i := 0 to Length(pt) - 1 do
      _PutPixel(pt[i].x, pt[i].y);
  end;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  case ls.SelState of
    stNone: Exit;
    stSelecting:
    begin
      c[False] := clWhite;
      c[True] := clBlack;
    end;
    stFloating:
    begin
      c[False] := clWhite;
      c[True] := FlipColor(clBlue);
    end;
  end;

  sl := TScanLines.Create(pb.Buffer);
  try
    case ls.SelState of
      stSelecting:
      begin
        bmMask := ls.Selection.Mask;
        rVisible := GetVisibleRect;

        // one-to-one/shrunk?
        if (Zoom = 1) or (Zoom < 0) then
        begin
          q := Abs(Zoom);
          for x := rVisible.Left div q to Ceil(rVisible.Right / q) - 1 do
            for y := rVisible.Top div q to Ceil(rVisible.Bottom / q) - 1 do
            begin
              i := x * q;
              j := y * q;

              if bmMask[i, j] and ((i < q) or not bmMask[i - q, j] or
                (j < q) or not bmMask[i, j - q] or (i >= bmMask.Width - q) or
                not bmMask[i + q, j] or (j >= bmMask.Height - q) or
                not bmMask[i, j + q]) then
                _PutPixel(xToView(i), yToView(j));
            end;
        end
        else
          // zoomed
        begin
          for x := rVisible.Left to rVisible.Right - 1 do
            for y := rVisible.Top to rVisible.Bottom - 1 do
            begin
              // left side
              if (x = 0) and bmMask[x, y] then
              begin
                x2 := xToView(x);
                for y2 := yToView(y) to yToView(y + 1) do
                  _PutPixel(x2, y2);
              end;
              // right side
              if ((x = bmMask.Width - 1) and bmMask[x, y]) or
                ((x <> bmMask.Width - 1) and (bmMask[x, y] xor bmMask[x + 1, y])) then
              begin
                x2 := xToView(x + 1);
                for y2 := yToView(y) to yToView(y + 1) do
                  _PutPixel(x2, y2);
              end;
              // bottom side
              if ((y = bmMask.Height - 1) and bmMask[x, y]) or
                ((y <> bmMask.Height - 1) and (bmMask[x, y] xor bmMask[x, y + 1])) then
              begin
                y2 := yToView(y + 1);
                for x2 := xToView(x) to xToView(x + 1) do
                  _PutPixel(x2, y2);
              end; // bottom
            end; // for x, y

          // left side
          if rVisible.Left = 0 then
          begin
            x2 := xToView(0);
            for y := rVisible.Top to rVisible.Bottom - 1 do
              if bmMask[0, y] then
                for y2 := yToView(y) to yToView(y + 1) do
                  _PutPixel(x2, y2);
          end;
          // top side
          if rVisible.Top = 0 then
          begin
            y2 := yToView(0);
            for x := rVisible.Left to rVisible.Right - 1 do
              if bmMask[x, 0] then
                for x2 := xToView(x) to xToView(x + 1) do
                  _PutPixel(x2, y2);
          end;
        end; // zoomed
      end; // stSelecting

      stFloating:
      begin
        if not GetFloatingQuad(Quad, True) then
          Exit;

        _QuadLine(0, 1);
        _QuadLine(1, 2);
        _QuadLine(2, 3);
        _QuadLine(3, 0);
      end;
    end; // case SelState
  finally
    sl.Free;
  end;
end;

procedure TGraphicFrame.DrawHotSpotFlasher;
var
  i, x, y, Size: integer;
  Page: TDocPage;

begin
  with pb.Canvas do
  begin
    Page := Doc.Pages[ImageIndex];

    x := xToView(Page.HotSpot.X + 0.5);
    y := yToView(Page.HotSpot.Y + 0.5);
    if Zoom >= 8 then
      Size := Zoom div 2
    else
      Size := 4;

    // calc the flasher color
    i := IfThen(MarqueeOffset <= 4, MarqueeOffset, 8 - MarqueeOffset);
    Pen.Color := Min($ff, i * $40);
    Pen.Width := IfThen(Size <= 4, 2, 3);
    MoveTo(x - Size, y - Size);
    LineTo(x + Size, y + Size);
    MoveTo(x - Size, y + Size);
    LineTo(x + Size, y - Size);
    Pen.Width := 1;
  end;
end;

function TGraphicFrame.DrawMarquees;
begin
  Result := False;

  if Doc.Pages[ImageIndex].Layers.SelState <> stNone then
  begin
    DrawSelectionMarquee;
    DrawSelectionKnobs;
    Result := True;
  end;

  if frmMain.frmToolbar.DrawTool = dtHotSpot then
  begin
    DrawHotSpotFlasher;
    Result := True;
  end;
end;

procedure TGraphicFrame.DrawInvertedOutline;
var
  i: integer;
  r, rDest, rSrc: TRect;
  pp: array of TPoint;
  bm: TBitmap;
  bx: TBitmap32;

begin
  if not ((ma = iemaDrawing) and (frmMain.frmToolbar.DrawTool in
    dtHasInvertedOutline)) then
    Exit;

  r := WinCanvasConvert(Rect(pOrig.X, pOrig.Y, pCurr.X, pCurr.Y));
  r := Rect(xToView(r.Left), yToView(r.Top), xToView(r.Right), yToView(r.Bottom));

  with pb.Canvas do
  begin
    Pen.Mode := pmNot;
    Brush.Style := bsClear;

    case frmMain.frmToolbar.DrawTool of
      dtSelRect, dtCrop: Rectangle(r);

      dtSelEllipse: Ellipse(r.Left, r.Top, r.Right, r.Bottom);

      dtLasso:
      begin
        SetLength(pp, FPolyCount);
        for i := 0 to FPolyCount - 1 do
          pp[i] :=
            Point(xToView(FPoints[i].X + 0.5), yToView(FPoints[i].Y + 0.5));
        PolyLine(pp);
      end;

      dtSelPencil:
      begin
        rSrc := GetVisibleRect;
        rDest := Rect(xToView(rSrc.Left), yToView(rSrc.Top),
          xToView(rSrc.Right), yToView(rSrc.Bottom));

        // TODO: fix this complicated and slow solution and provide an easier one
        bm := TBitmap.Create;
        try
          bx := TBitmap32.Create;
          try
            bx.Resize(rSrc.Width, rSrc.Height);
            bx.CopyRectOverwrite(0, 0, SelSketch, rSrc);
            bx.ToBitmap(bm, clBlack);
          finally
            bx.Free;
          end;

          CopyMode := cmSrcInvert;
          CopyRect(rDest, bm.Canvas, Rect(0, 0, bm.Width, bm.Height));
          CopyMode := cmSrcCopy;
        finally
          bm.Free;
        end;
      end;

        (*dtCrop: begin
          Pen.Mode := pmCopy;
          Brush.Style := bsSolid;
          Brush.Color := $fadeba;
            ls := Doc.Pages[ImageIndex].Layers;
            r2 := Rect(xToView(0), yToView(0), xToView(ls.Width), yToView(ls.Height));
            if (r.Top >= r2.Bottom) or (r2.Top >= r.Bottom) or
              (r.Left >= r2.Right) or (r2.Left >= r.Right) then
              FillRect(r) else
            begin
              if r.Left < r2.Left then FillRect(Rect(r.Left, r.Top, r2.Left, r.Bottom));
              if r.Top < r2.Top then FillRect(Rect(r.Left, r.Top, r.Right, r2.Top));
              if r2.Right < r.Right then FillRect(Rect(r2.Right, r.Top, r.Right, r.Bottom));
              if r2.Bottom < r.Bottom then FillRect(Rect(r.Left, r2.Bottom, r.Right, r.Bottom));
            end;
          Pen.Mode := pmNot;
          Brush.Style := bsClear;
            Rectangle(r);
        end; *)
    end; // case DrawTool

    Brush.Style := bsSolid;
    Pen.Mode := pmCopy;
  end;
end;

procedure TGraphicFrame.RenderUserBitmap(Buffer: TBitmap32; const rClip: TRect;
  IndexOfPage: integer);
var
  ls: TLayers;

begin
  if (ma = iemaDrawing) and (IndexOfPage = ImageIndex) and
    (frmMain.frmToolbar.DrawTool in [dtRect, dtEllipse, dtLine, dtPencil,
    dtBrush, dtEraser, dtRecolor, dtBucket, dtGradient]) then
    ls := lsResult
  else
    ls := Doc.Pages[IndexOfPage].Layers;

  ls.Render(Buffer, rClip, lssAll, True);
end;

function TGraphicFrame.ClipClippingRect(rClip: TRect): TRect;
var
  m: integer;
begin
  // only render and draw visible and allowed part
  if not IntersectRect(rClip, rClip, GetVisibleRect) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;

  // 'round' the rectangle coordinates
  if Zoom < 0 then
  begin
    Dec(rClip.Left, rClip.Left mod -Zoom);
    Dec(rClip.Top, rClip.Top mod -Zoom);
    // shrink image -> round coordinates
    m := rClip.Right mod -Zoom;
    if m <> 0 then
      Inc(rClip.Right, -Zoom - m);
    m := rClip.Bottom mod -Zoom;
    if m <> 0 then
      Inc(rClip.Bottom, -Zoom - m);
  end;
  Result := rClip;
end;

function TGraphicFrame.DrawUserBitmap(rClip: TRect): TRect;
var
  xPos, yPos: integer;
  bmUser: TBitmap32;

begin
  Result := ClipClippingRect(rClip);
  if (Result.Width <= 0) or (Result.Height <= 0) then
    Exit;

  bmUser := TBitmap32.Create;
  try
    RenderUserBitmap(bmUser, Result, ImageIndex);

    xPos := xToView(Result.Left);
    yPos := yToView(Result.Top);
    if Zoom >= 1 then
      bmUser.DrawZoomedHatchedTo24(pb.Buffer, xPos, yPos, xToView(0), yToView(0), Zoom, Pref_Hatch)
    else
      bmUser.DrawShrunkHatchedTo24(pb.Buffer, xPos, yPos, xToView(0), yToView(0), -Zoom, Pref_Hatch, False);
  finally
    bmUser.Free;
  end;
end;

procedure TGraphicFrame.DrawGrid;
var
  i: integer;
  bDrawGrid: boolean;
  r: TRect;
  ls: TLayers;
  sl: TScanLines;

  procedure hDottedLine(y: integer; c: TColor2);
  var
    x: integer;

  begin
    if (y >= 0) and (y < pb.Height) then
    begin
      c[0] := FlipColor(c[0]);
      c[1] := FlipColor(c[1]);
      for x := Max(0, r.Left) to Min(pb.Width, r.Right) - 1 do
        Move3(@c[(x + y) and 1], @sl[y][x * sl.BytesPerPixel]);
    end;
  end;

  procedure vDottedLine(x: integer; c: TColor2);
  var
    y: integer;

  begin
    if (x >= 0) and (x < pb.Width) then
    begin
      c[0] := FlipColor(c[0]);
      c[1] := FlipColor(c[1]);
      for y := Max(0, r.Top) to Min(pb.Height, r.Bottom) - 1 do
        Move3(@c[(x + y) and 1], @sl[y][x * sl.BytesPerPixel]);
    end;
  end;

  function xCellGrid(i: integer): boolean;
  var
    m: integer;

  begin
    Result := cg.Enabled and (cg.Size.X >= 4) and (i >= cg.Offset.X);
    if Result then
    begin
      m := (i - cg.Offset.X) mod (cg.Size.X + Max(0, cg.Spacing.X));
      Result := (m = 0) or (m = cg.Size.X);
    end;
  end;

  function yCellGrid(i: integer): boolean;
  var
    m: integer;

  begin
    Result := cg.Enabled and (cg.Size.Y >= 4) and (i >= cg.Offset.Y);
    if Result then
    begin
      m := (i - cg.Offset.Y) mod (cg.Size.Y + Max(0, cg.Spacing.Y));
      Result := (m = 0) or (m = cg.Size.Y);
    end;
  end;

begin
  ls := Doc.Pages[ImageIndex].Layers;
  r := Rect(xToView(0), yToView(0), xToView(ls.Width), yToView(ls.Height));
  sl := TScanLines.Create(pb.Buffer);
  try
    bDrawGrid := sbGrid.Down and (Zoom >= 4);

    for i := 1 to ls.Width - 1 do
      if xCellGrid(i) then
        vDottedLine(xToView(i), Pref_clGrid2)
      else
      if bDrawGrid then
        vDottedLine(xToView(i), Pref_clGrid);

    for i := 1 to ls.Height - 1 do
      if yCellGrid(i) then
        hDottedLine(yToView(i), Pref_clGrid2)
      else
      if bDrawGrid then
        hDottedLine(yToView(i), Pref_clGrid);

    // draw center lines
    if sbCenterLines.Down then
    begin
      hDottedLine(yToView(ls.Height * 0.5), Pref_clGrid2);
      vDottedLine(xToView(ls.Width * 0.5), Pref_clGrid2);
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TGraphicFrame.DrawImageFrame;
const
  FrameSize = 20;

var
  r: TRect;
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;
  r := Rect(xToView(0), yToView(0), xToView(ls.Width), yToView(ls.Height));
  with pb.Canvas do
  begin
    // draw the frame
    Pen.Color := IfThen(pb.Focused, $303030, clSilver);
    MoveTo(r.Left - 1, r.Top - 1);
    LineTo(r.Left - 1, r.Top - FrameSize);
    MoveTo(r.Left - 1, r.Top - 1);
    LineTo(r.Left - FrameSize, r.Top - 1);
    MoveTo(r.Right + 1, r.Top - 1);
    LineTo(r.Right + 1, r.Top - FrameSize);
    MoveTo(r.Right + 1, r.Top - 1);
    LineTo(r.Right + FrameSize, r.Top - 1);
    MoveTo(r.Left - 1, r.Bottom + 1);
    LineTo(r.Left - 1, r.Bottom + FrameSize);
    MoveTo(r.Left - 1, r.Bottom + 1);
    LineTo(r.Left - FrameSize, r.Bottom + 1);
    MoveTo(r.Right + 1, r.Bottom + 1);
    LineTo(r.Right + 1, r.Bottom + FrameSize);
    MoveTo(r.Right + 1, r.Bottom + 1);
    LineTo(r.Right + FrameSize, r.Bottom + 1);
  end;
end;

procedure TGraphicFrame.DrawSelectionKnobs;
var
  i: integer;
  q: TQuad;
  ls: TLayers;

  procedure DrawKnob(x, y: integer; cl: TColor; Size: integer);
  begin
    with pb.Canvas do
    begin
      Brush.Color := cl;
      FillRect(Rect(x - Size, y - Size, x + Size, y + Size));
    end;
  end;

begin
  ls := Doc.Pages[ImageIndex].Layers;
  if (frmMain.frmToolbar.DrawTool = dtTransform) and (ls.SelState = stFloating) then
  begin
    if not GetFloatingQuad(q, True) then
      Exit;
    for i := 0 to 3 do
    begin
      // knob for scaling
      DrawKnob(Round(q[i].x), Round(q[i].y), clBlue, 2);
      // knob for rotating
      if i = 3 then
        DrawKnob(Round(0.5 * (q[i].x + q[(i + 1) and 3].x)),
          Round(0.5 * (q[i].y + q[(i + 1) and 3].y)), clRed, 3);
    end;
  end;
end;

procedure TGraphicFrame.ScrollPaintBox;
var
  i: integer;
  bm: TBitmap;
  delta, newScroll, newDraw: TPoint;
  rClip: TRect;
  sr: TSubtractRect;

begin
  if frmDestroying then
    Exit;
  newScroll := Point(sbX.Position, sbY.Position);
  if lastScrollPosValid and (lastScrollPos.x = newScroll.x) and (lastScrollPos.y = newScroll.y) then
    Exit;

  if not lastScrollPosValid or not FAST_SCROLL or (Zoom >= 4) then
    RedrawPaintBox
  else
  begin
    lastScrollPosValid := False;
    newDraw := Point(xToView(0), yToView(0));
    bm := TBitmap.Create;
    try
      bm.PixelFormat:=pf24bit;
      bm.Width := pb.Width;
      bm.Height := pb.Height;
      bm.Canvas.Brush.Color := clPaintBoxBgr;
      bm.Canvas.FillRect(Rect(0, 0, bm.Width, bm.Height));

      delta := Point(newDraw.X - lastDrawPos.X, newDraw.Y - lastDrawPos.Y);
      bm.Canvas.Draw(delta.X, delta.Y, pb.Buffer);

      rClip := ClipClippingRect(Rect(Low(integer), Low(integer), High(integer), High(integer)));
      sr.Subtract(rClip, lastDrawnRect);
      lastDrawnRect := rClip;

      bm := pb.ReleaseBuffer(bm);

      for i := 0 to sr.Count-1 do
      begin
        if Zoom > 1 then
          InflateRect(sr.Rects[i], 1, 1); // draw incomplete pixels
        RedrawPaintBoxPart(sr.Rects[i], false, true);
      end;
      DrawEverythingElseAndUpdateView;
    finally
      bm.Free;
    end;
    lastScrollPosValid := True;
    lastScrollPos := newScroll;
    lastDrawPos := newDraw;
  end;
end;

procedure TGraphicFrame.RedrawPaintBox;
begin
  if frmDestroying then
    Exit;

  pb.Canvas.Brush.Color := clPaintBoxBgr;
  pb.Canvas.FillRect(Rect(0, 0, pb.Width, pb.Height));

  RedrawPaintBoxPart(Rect(0, 0, MaxInt, MaxInt), true);
  lastScrollPosValid := True;
  lastScrollPos := Point(sbX.Position, sbY.Position);
  lastDrawPos := Point(xToView(0), yToView(0));
end;

procedure TGraphicFrame.RedrawPageListItem;
begin
  lb.InvalidateItem(ImageIndex);
end;

procedure TGraphicFrame.RedrawLayerList;
begin
  if frmMain.frmLayers.DocForm = Self then
    frmMain.frmLayers.lb.InvalidateListBox;
end;

procedure TGraphicFrame.DrawEverythingElseAndUpdateView;
begin
  DrawGrid;
  DrawImageFrame;
  DrawMarquees;
  DrawInvertedOutline;

  pb.SwapBuffers;
  if ma = iemaNone then
  begin
    RedrawPageListItem;
    RedrawLayerList;
  end;
end;

procedure TGraphicFrame.RedrawPaintBoxPart(const rImageChanged: TRect;
  UpdateLastDrawnRect: boolean; OnlyDrawPixels: boolean);
var
  r: TRect;
begin
  if frmDestroying then
    Exit;

//  Log(inttoStr(rImageChanged.Left)+' '+inttoStr(rImageChanged.top)+' '+inttoStr(rImageChanged.right)+' '+inttoStr(rImageChanged.bottom)+' ');
  r := DrawUserBitmap(rImageChanged);
  if UpdateLastDrawnRect then
    lastDrawnRect := r;
  if not OnlyDrawPixels then
  begin
    DrawEverythingElseAndUpdateView;
  end;
end;

procedure TGraphicFrame.PageCountChanged;
begin
  ImageIndex := ImageIndex; // validate value
  lb.InvalidateListBox;
end;

procedure TGraphicFrame.PageSizeChanged;
begin
  UpdateScrollBars;
  if sbZoomFit.Down then
    zoomFitWindow;

  RedrawPaintBox;
end;

function TGraphicFrame.pgInsert(Index: integer): TDocPage;
begin
  Result := Doc.InsertPage(Index);
  with Result.Layers.NewLayer do
  begin
    Name := lpGet('LY_BACKGROUND');
    Selected := True;
  end;

  ImageIndex := Index;
  PageCountChanged;
end;

procedure TGraphicFrame.pgDelete(Index: integer);
begin
  if Doc.PageCount > 1 then
  begin
    Doc.DeletePage(Index);

    if Index = ImageIndex then
    begin
      ImageIndex := ImageIndex; // avoid ImageIndex >= PageCount
      PageSizeChanged;
    end
    else
    if Index < ImageIndex then
      ImageIndex := ImageIndex - 1;

    PageCountChanged;
  end;
end;

procedure TGraphicFrame.pgMove(Index, Index2: integer);
var
  d: integer;

begin
  Doc.MovePage(Index, Index2);

  if ImageIndex = Index then
    ImageIndex := Index2
  else
  begin
    d := 0;
    if Index < ImageIndex then
      Dec(d);
    if Index2 <= ImageIndex then
      Inc(d);
    ImageIndex := ImageIndex + d;
  end;

  lb.InvalidateListBox;
end;

procedure TGraphicFrame.PerformUndoRedo(Undo: boolean);
var
  OldSize: TPoint;

begin
  with Doc.Pages[ImageIndex].Layers do
    OldSize := Point(Width, Height);

  inherited;

  lb.InvalidateListBox;
  with Doc.Pages[ImageIndex].Layers do
    if (Width <> OldSize.X) or (Height <> OldSize.Y) then
      PageSizeChanged
    else
      RedrawPaintBox;
end;

procedure TGraphicFrame.cuAllLayersChanged(const Caption: string; OnlySelected: boolean);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  for i := 0 to ls.LayerCount - 1 do
    if not OnlySelected or ls[i].Selected then
      uo.LayerChanged(i);
  uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuCreateFloating(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  for i := 0 to ls.LayerCount - 1 do
    if ls[i].Selected then
      uo.LayerChanged(i);
  uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuEditDelete(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  // were the layers changed?
  if ls.SelState = stSelecting then
    for i := 0 to ls.LayerCount - 1 do
      if ls[i].Selected then
        uo.LayerChanged(i);
  // selection is always changed
  uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuFilter(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  ls := Doc.Pages[ImageIndex].Layers;
  if ls.SelState in [stNone, stSelecting] then
    // filter applies to selected layers, selection object is unchanged
    for i := 0 to ls.LayerCount - 1 do
    begin
      if ls[i].Selected then
        uo.LayerChanged(i);
    end
  else
    // filter applies to floating selection
    uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuFlipRotate(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  ls := Doc.Pages[ImageIndex].Layers;
  if ls.SelState in [stNone, stSelecting] then
    for i := 0 to ls.LayerCount - 1 do
      uo.LayerChanged(i);
  if ls.SelState in [stSelecting, stFloating] then
    uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuFrameRates(const Caption: string);
begin
  AddUndo(TFrameRatesUndo.Create(Caption, Self));
end;

procedure TGraphicFrame.cuHotSpot(const Caption: string);
begin
  AddUndo(THotSpotUndo.Create(Caption, Self, ImageIndex));
end;

procedure TGraphicFrame.cuLayerDelete(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  with Doc.Pages[ImageIndex].Layers do
    for i := LayerCount - 1 downto 0 do
      if Layers[i].Selected then
        uo.LayerDeleted(i);

  AddUndo(uo);
end;

procedure TGraphicFrame.cuLayerInsert(const Caption: string; Depth: integer);
var
  uo: TPageChangeUndo;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);
  uo.LayerInserted(Depth);
  AddUndo(uo);
end;

procedure TGraphicFrame.cuLayerMerge(const Caption: string; Subset: TLayerSubset);
var
  i: integer;
  Last: boolean;
  ls: TLayers;
  uo: TPageChangeUndo;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  Last := True;
  for i := ls.LayerCount - 1 downto 0 do
    if LayerInSubset(ls[i], Subset) then
      if Last then
      begin
        uo.LayerChanged(i);
        Last := False;
      end
      else
        uo.LayerDeleted(i);

  if (ls.SelState = stFloating) and LayerInSubset(ls[ls.Selection.Depth], Subset) then
    uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuLayerProp(const Caption: string; LayerIndex: integer);
begin
  AddUndo(TLayerPropUndo.Create(Caption, Self, ImageIndex, LayerIndex));
end;

procedure TGraphicFrame.cuPageDelete(const Caption: string; Index: integer);
begin
  AddUndo(TDeletePageUndo.Create(Caption, Self, Index));
end;

procedure TGraphicFrame.cuPageInsert(const Caption: string; Index: integer);
begin
  AddUndo(TInsertPageUndo.Create(Caption, Index));
end;

procedure TGraphicFrame.cuPageMove(const Caption: string; Index, Index2: integer);
begin
  AddUndo(TMovePageUndo.Create(Caption, Index, Index2));
end;

procedure TGraphicFrame.cuPaint(const Caption: string);
var
  i: integer;
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  ls := Doc.Pages[ImageIndex].Layers;
  for i := 0 to ls.LayerCount - 1 do
    if ls[i].Selected then
      uo.LayerChanged(i);

  AddUndo(uo);
end;

procedure TGraphicFrame.cuSelect(const Caption: string);
var
  uo: TPageChangeUndo;
  ls: TLayers;

begin
  uo := TPageChangeUndo.Create(Caption, Self, ImageIndex);

  ls := Doc.Pages[ImageIndex].Layers;
  if ls.SelState = stFloating then
    uo.LayerChanged(ls.Selection.Depth);
  uo.SelectionChanged;

  AddUndo(uo);
end;

procedure TGraphicFrame.cuTransform(const Caption: string);
begin
  if Doc.Pages[ImageIndex].Layers.SelState = stSelecting then
    // 1) create floating selection 2) modify box (but this is autosaved)
    cuCreateFloating(Caption)
  else
    // we only need Selection.Box to be saved, this is done automatically by TPCU
    AddUndo(TPageChangeUndo.Create(Caption, Self, ImageIndex));
end;

procedure TGraphicFrame.DoLayerMerge(Subset: TLayerSubset);
var
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  Modified := True;
  cuLayerMerge('UNDO_MERGE_LAYERS', Subset);

  ls.SelectOne(ls.Merge(Subset));
  RedrawPaintBox;
end;

procedure TGraphicFrame.DoLayerNew;
var
  ls: TLayers;
  l: TLayer;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  Modified := True;
  cuLayerInsert('MI_LAYERS_NEW', 0);

  l := ls.NewLayerAtDepth(0);
  l.Name := lpGet('LY_LAYER') + ' ' + IntToStr(ls.LayerCount);
  ls.SelectOne(0);

  RedrawLayerList; // no RedrawPaintBox needed, new layer is empty
end;

procedure TGraphicFrame.DoLayerDupl;
var
  i, Start: integer;
  ls: TLayers;
  uo: TPageChangeUndo;

begin
  ls := Doc.Pages[ImageIndex].Layers;
  Start := ls.FirstSelected;

  // create undo
  Modified := True;
  uo := TPageChangeUndo.Create('MI_LAYERS_DUPLICATE', Self, ImageIndex);
  for i := ls.LayerCount + ls.SelectedCount - 1 downto ls.LayerCount do
    uo.LayerInserted(i);
  AddUndo(uo);

  // duplicate layers
  for i := Start to ls.LayerCount - 1 do
    if ls[i].Selected then
    begin
      with ls.NewLayer do
      begin
        Assign(ls[i]);
        Name := Name + ' (2)';
        Selected := True;
      end;

      ls[i].Selected := False;
    end;

  RedrawPaintBox;
end;

procedure TGraphicFrame.DoLayerDelete;
var
  i, FirstSelected: integer;
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  if ls.SelectedCount = 0 then
    ErrorBeep
  else
  begin
    Modified := True;
    cuLayerDelete('MI_LAYERS_DELETE');

    for i := ls.LayerCount - 1 downto 0 do
      if ls[i].Selected then
      begin
        FirstSelected:=i;
        ls.DeleteLayer(i);
      end;
    if ls.LayerCount > 0 then
      ls[Max(0, Min(ls.LayerCount-1, FirstSelected))].Selected:=true;

    RedrawPaintBox;
  end;
end;

procedure TGraphicFrame.DoLayerProp;
var
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  DoCreateForm(TfrmLayerProp, frmLayerProp);
  if ls.SelectedCount <> 1 then
    ErrorBeep
  else
  if frmLayerProp.Execute(Self, ls[ls.FirstSelected]) then
  begin
    Modified := True;
    cuLayerProp('MI_LAYERS_PROP', ls.FirstSelected);

    frmLayerProp.UpdateObject;
    RedrawPaintBox;
  end;
end;

procedure TGraphicFrame.DoLayerFromSel;
var
  i, Depth: integer;
  ls: TLayers;
  lSrc, lDest: TLayer;
  uo: TPageChangeUndo;

begin
  ls := Doc.Pages[ImageIndex].Layers;
  case ls.SelState of
    stNone: Exit;
    stSelecting: if ls.SelectedCount = 0 then
        Exit;
  end;

  Modified := True;

  // create new layer
  if ls.SelState = stFloating then
    Depth := ls.Selection.Depth
  else
    Depth := ls.FirstSelected;
  lSrc := ls[Depth];

  // create undo
  uo := TPageChangeUndo.Create('MI_LAYERS_FROM_SEL', Self, ImageIndex);

  uo.SelectionChanged;
  if ls.SelState = stSelecting then
  begin
    for i := ls.LayerCount - 1 downto 0 do
      if ls[i].Selected then
        uo.LayerChanged(i);
    ls.CreateFloatingSelection;
  end;
  uo.LayerInserted(Depth);
  AddUndo(uo);

  // project selection to a newly created layer
  lDest := ls.NewLayerAtDepth(Depth);
  Dec(ls.Selection.Depth);
  with lDest do
  begin
    Name := '(' + lpGet('LY_FLOATING_SEL') + ')';
    Visible := lSrc.Visible;
    Opacity := lSrc.Opacity;
    BlendMode := lSrc.BlendMode;
  end;
  ls.FlattenFloatingSelection;

  RedrawPaintBox;
end;

procedure TGraphicFrame.DoPageNew;
var
  st: TdpSettings;
  pgOrig, pgNew: TDocPage;

begin
  pgOrig := Doc.Pages[ImageIndex];

  st.Caption := lpGet('PG_CAPTION_NEW_PAGE');
  st.showColors := True;
  st.showCreateFrom := True;
  st.showWhenResizing := True;
  st.Width := pgOrig.Layers.Width;
  st.Height := pgOrig.Layers.Height;
  st.FrameRate := pgOrig.FrameRate;
  st.DPI := pgOrig.DPI;

  DoCreateForm(TfrmDocPage, frmDocPage);
  if not frmDocPage.Execute(st) then
    Exit;

  Modified := True;
  cuPageInsert('PG_CAPTION_NEW_PAGE', Doc.PageCount);

  pgNew := pgInsert(Doc.PageCount);

  if st.CreateFrom then
  begin
    Screen.Cursor := crHourGlass;
    try
      pgNew.Assign(pgOrig);
      DoCreateForm(TfrmDocPage, frmDocPage);
      frmDocPage.ConvertSizeAndColors(pgNew, st);
    finally
      Screen.Cursor := crDefault;
    end;
  end else // st.CreateFrom
    pgNew.Layers.Resize(st.Width, st.Height);

  pgNew.FrameRate := st.FrameRate;
  pgNew.DPI := st.DPI;

  PageSizeChanged;
end;

procedure TGraphicFrame.DoPageDelete;
begin
  if Doc.PageCount <= 1 then
    ErrorBeep
  else
  begin
    Modified := True;
    cuPageDelete('MI_ICON_PAGE_DELETE', ImageIndex);
    pgDelete(ImageIndex);
  end;
end;

procedure TGraphicFrame.DoPageMove(Index, Index2: integer);
begin
  Index := Max(0, Min(Doc.PageCount - 1, Index));
  Index2 := Max(0, Min(Doc.PageCount - 1, Index2));

  if Index <> Index2 then
  begin
    Modified := True;
    cuPageMove('UNDO_MOVE_PAGE', Index, Index2);

    pgMove(Index, Index2);
  end;
end;

procedure TGraphicFrame.DoPageProp;
var
  i: integer;
  st: TdpSettings;
  pgCurr: TDocPage;
  ImageChanged: boolean;
  uo: TPageChangeUndo;

begin
  pgCurr := Doc.Pages[ImageIndex];

  st.Caption := lpGet('PG_CAPTION_PAGE_PROP');
  st.showColors := True;
  st.showCreateFrom := False;
  st.showWhenResizing := True;
  st.Width := pgCurr.Layers.Width;
  st.Height := pgCurr.Layers.Height;
  st.FrameRate := pgCurr.FrameRate;
  st.DPI := pgCurr.DPI;

  DoCreateForm(TfrmDocPage, frmDocPage);
  if not frmDocPage.Execute(st) then
    Exit;

  ImageChanged := (st.Width <> pgCurr.Layers.Width) or
    (st.Height <> pgCurr.Layers.Height) or (st.cr <> cr32);
  Modified := True;

  // create undo
  uo := TPageChangeUndo.Create('PG_CAPTION_PAGE_PROP', Self, ImageIndex);

  if ImageChanged then
  begin
    for i := 0 to pgCurr.Layers.LayerCount - 1 do
      uo.LayerChanged(i);
    uo.SelectionChanged;
  end;
  if (st.FrameRate <> pgCurr.FrameRate) or (st.DPI <> pgCurr.DPI) then
    uo.FrameRateOrDPIChanged;

  AddUndo(uo);

  // convert size and colors
  if ImageChanged then
  begin
    Screen.Cursor := crHourGlass;
    try
      DoCreateForm(TfrmDocPage, frmDocPage);
      frmDocPage.ConvertSizeAndColors(pgCurr, st);
    finally
      Screen.Cursor := crDefault;
    end;
  end;

  // set other fields
  pgCurr.FrameRate := st.FrameRate;
  pgCurr.DPI := st.DPI;

  // update the user interface
  PageSizeChanged;
end;

procedure TGraphicFrame.DoPageImport;
var
  i: integer;
  d: TIconDoc;
  fn: string;
  sl: TStringList;
  Failed: boolean;

begin
  if not frmMain.od.Execute then
    Exit;

  Failed := False;
  d := TIconDoc.Create;
  try
    // open the files by names sorted
    sl := TStringList.Create;
    try
      sl.Assign(frmMain.od.Files);
      sl.Sorted := True;
      for i := 0 to sl.Count - 1 do
      begin
        fn := UTF8ToSys(sl[i]);
        // TODO show open options
        if d.LoadFromFile(fn, Pref_MaxWidth, Pref_MaxHeight, 1,
          lpGet('LY_BACKGROUND')) <> iftNone then
          Doc.Append(d)
        else
          Failed := True;
      end;
    finally
      sl.Free;
    end;
  finally
    d.Free;
  end;

  PageCountChanged;
  RedrawPaintBox;

  if Failed then
    ShowMessage(lpGet('MSG_FAIL_MULTIPLE_OPEN'));
end;

procedure TGraphicFrame.DoPageExport(ExportAll: boolean);
var
  fn: string;
  ft: TImageFileType;
  i: integer;

  procedure _Export(Index: integer; const FileName: string);
  var
    d: TIconDoc;
    dl: TieDataLosses;

  begin
    d := TIconDoc.Create;
    try
      d.NewPage.Assign(Doc.Pages[Index]);
      DoCreateForm(TfrmSaveOptions, frmSaveOptions);
      d.SaveToFile(FileName, ft, Pref_PNGLimit,
        ExtractOnlyFileName(FileName),
        frmSaveOptions.neQuality.Value,
        frmSaveOptions.cbLossless.Checked,
        dl);
    finally
      d.Free;
    end;
  end;

begin
  fn := '';
  ft := iftPng;

  if frmMain.sdGraphicExec(fn, ft) then
    if ft = iftNone then
      ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [fn]))
    else
    begin
      DoCreateForm(TfrmSaveOptions, frmSaveOptions);
      if not frmSaveOptions.Execute(Self, ft) then
        Exit;

      Screen.Cursor := crHourGlass;
      try
        if ExportAll then
        begin
          for i := 0 to Doc.PageCount - 1 do
            _Export(i,
              Format('%s_%.4d%s', [WithoutExt(fn), i, ExtractFileExt(fn)]));
        end
        else
          _Export(ImageIndex, fn);
      finally
        Screen.Cursor := crDefault;
      end;
    end;
end;

procedure TGraphicFrame.DoTogglePages;
begin
  pPages.Visible := not pPages.Visible;
  sbPages.Down := pPages.Visible;
end;

procedure TGraphicFrame.DoUniformFrameRate(fr: integer);
var
  i: integer;

begin
  // create undo
  Modified := True;
  cuFrameRates('MI_ICON_UNIFORM_RATE');

  // set frame rates
  for i := 0 to Doc.PageCount - 1 do
    Doc.Pages[i].FrameRate := fr;

  lb.InvalidateListBox;
end;

procedure TGraphicFrame.DoEditCopy;
var
  i: integer;
  pc: PColor32;
  ls, lsWrite: TLayers;
  lsWriteFree: boolean;
  bm: TBitmap;
  st: TStream;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  // SelState must be floating
  case ls.SelState of
    stNone: Exit;

    stSelecting:
    begin
      lsWrite := TLayers.Create;
      lsWrite.Assign(ls);
      lsWrite.CreateFloatingSelection;
      lsWriteFree := True;
    end;

    stFloating:
    begin
      lsWrite := ls;
      lsWriteFree := False;
    end;
  end;

  Clipboard.Open;
  try
    st := TMemoryStream.Create;
    try
      // Write using the following formats:
      // - 32-bit png (primary) + inversion mask
      // - bmp (secondary)
      // - additional info (selection size)

      // Inversion mask is needed because cl32Inverted becomes transparent
      // when writing as PNG

      // Bitmap for compatibility with other programs
      bm := TBitmap.Create;
      try
        lsWrite.Selection.Image.ToBitmap(bm, clWhite);
        OptimizeBitmapPF(bm);
        Clipboard.Assign(bm);
      finally
        bm.Free;
      end;

      // PNG
      st.Size := 0;
      pngSaveToStream(lsWrite.Selection.Image, st, PNG_COMPRESSION_NORMAL, 0.0);
      Clipboard.AddFormat(CF_PNG, st);

      // Inversion mask
      with lsWrite.Selection.Image do
        if HasColor(cl32Inverted) then
        begin
          st.Size := 0;
          pc := Data;
          for i := 1 to Width * Height do
          begin
            st.WriteByte(byte(pc^ = cl32Inverted));
            Inc(pc);
          end;
          Clipboard.AddFormat(CF_GFIE_INVERSION, st);
        end;

      // Selection size
      st.Size := 0;
      with lsWrite.Selection.Box do
      begin
        st.WriteWord(Width);
        st.WriteWord(Height);
      end;
      Clipboard.AddFormat(CF_GFIE_SIZE, st);

      // Selection angle
      st.Size := 0;
      st.WriteQWord(PInt64(@lsWrite.Selection.Angle)^);
      Clipboard.AddFormat(CF_GFIE_ANGLE, st);
    finally
      st.Free;
    end;
  finally
    Clipboard.Close;
    if lsWriteFree then
      lsWrite.Free;
  end;
end;

procedure TGraphicFrame.DoEditPaste(NewPage: boolean);
var
  i, w, h: integer;
  pc: PColor32;
  bx: TBitmap32;
  bm: TBitmap;
  pg: TDocPage;
  ls: TLayers;
  Size: TPoint;
  q: TQuad;
  dpi, AAngle: double;
  st: TStream;

begin
  if not IconEditorCanPaste then
    Exit;

  // get image and, optionally, selection size and AND mask
  bx := TBitmap32.Create;

  try
    st := TMemoryStream.Create;
    try
      // image
      if Clipboard.HasFormat(CF_PNG) or Clipboard.HasFormat(CF_PNG_2) then
      begin
        st.Size := 0;
        Clipboard.GetFormat(IfThen(Clipboard.HasFormat(CF_PNG), CF_PNG, CF_PNG_2), st);
        st.Position := 0;
        pngLoadFromStream(bx, st, dpi);
      end
      else
      begin
        // no png, read as BMP or maybe other picture format
        bm := TBitmap.Create;
        try
          {$IFDEF WINDOWS}
          // Lazarus bug workaround: Clipboard.AssignToGraphic does not always work
          if Clipboard.HasFormat(Windows.CF_BITMAP) then
          begin
            st.Size := 0;
            Clipboard.GetFormat(Windows.CF_BITMAP, st);
            st.Position := 0;
            bm.LoadFromStream(st);
          end
          else
          {$ENDIF}
          if Clipboard.HasPictureFormat then
            bm.Assign(Clipboard);
          bx.Assign(bm);
        finally
          bm.Free;
        end;
      end;

      if (bx.Width = 0) or (bx.Height = 0) then
      begin
        ErrorBeep;
        Exit;
      end;

      // inversion mask
      if Clipboard.HasFormat(CF_GFIE_INVERSION) then
      begin
        st.Size := 0;
        Clipboard.GetFormat(CF_GFIE_INVERSION, st);
        st.Position := 0;

        pc := bx.Data;
        for i := 1 to bx.Width * bx.Height do
        begin
          if st.ReadByte <> 0 then
            pc^ := cl32Inverted;
          Inc(pc);
        end;
      end;

      // size
      if Clipboard.HasFormat(CF_GFIE_SIZE) then
      begin
        st.Size := 0;
        Clipboard.GetFormat(CF_GFIE_SIZE, st);
        st.Position := 0;

        Size.X := st.ReadWord;
        Size.Y := st.ReadWord;
      end
      else
        Size := Point(bx.Width, bx.Height);

      // angle
      if Clipboard.HasFormat(CF_GFIE_ANGLE) then
      begin
        st.Size := 0;
        Clipboard.GetFormat(CF_GFIE_ANGLE, st);
        st.Position := 0;

        PInt64(@AAngle)^ := st.ReadQWord;
      end
      else
        AAngle := 0;
    finally
      st.Free;
    end;

    Modified := True;

    if NewPage then
    begin
      cuPageInsert('MI_EDIT_PASTE_PAGE', Doc.PageCount);

      pg := pgInsert(Doc.PageCount);
      if AAngle = 0 then
      begin
        w := Size.X;
        h := Size.Y;
      end
      else
      begin
        q := RotateRectToQuad(Rect(0, 0, Size.X, Size.Y), 0, 0, AAngle);
        // ! We should be careful not to create a too big page !
        w := Min(Pref_MaxWidth, Ceil(Max4d(q[0].x, q[1].x, q[2].x, q[3].x) -
          Min4d(q[0].x, q[1].x, q[2].x, q[3].x)));
        h := Min(Pref_MaxHeight, Ceil(Max4d(q[0].y, q[1].y, q[2].y, q[3].y) -
          Min4d(q[0].y, q[1].y, q[2].y, q[3].y)));
      end;

      pg.Layers.Resize(w, h);
      pg.Layers[0].Image.TransformDraw(
        Rect(Round(0.5 * (w - Size.X)), Round(0.5 * (h - Size.Y)),
        Round(0.5 * (w + Size.X)), Round(0.5 * (h + Size.Y))),
        AAngle, bx, TLayers.AntialiasSelection);

      PageSizeChanged;
    end
    else
    begin
      ls := Doc.Pages[ImageIndex].Layers;
      if ls.LayerCount = 0 then
        ErrorBeep
      else
      begin
        cuSelect('MI_EDIT_PASTE');

        // flatten existing selection
        if ls.SelState = stFloating then
          ls.FlattenFloatingSelection;
        ls.SelState := stFloating;

        // configure the selection
        with ls.Selection do
        begin
          Image.Assign(bx);
          with Box do
          begin
            Left := GetVisibleRect.Left;
            Top := GetVisibleRect.Top;
            Right := Left + Size.X;
            Bottom := Top + Size.Y;
          end;
          Angle := AAngle;
          Depth := Max(0, ls.FirstSelected);
        end;

        // update the user interface
        frmMain.frmToolbar.DrawTool := dtTransform;
        RedrawPaintBox;
      end;
    end; // paste as floating selection
  finally
    bx.Free;
  end;
end;

procedure TGraphicFrame.DoEditDelete;
var
  ls: TLayers;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  if ls.SelState <> stNone then
  begin
    Modified := True;
    cuEditDelete('MI_EDIT_DELETE');

    if ls.SelState = stSelecting then
      ls.CreateFloatingSelection;
    ls.SelState := stNone;

    RedrawPaintBox;
  end;
end;

procedure TGraphicFrame.DoInsertText(const Text: string; _Font: TFont;
  Color: TColor32; AntiAlias: boolean);
var
  i, x, y, w, h: integer;
  sl: TStringList;

begin
  with Doc.Pages[ImageIndex].Layers do
  begin
    if LayerCount = 0 then
    begin
      ShowMessage(lpGet('MSG_NO_LAYERS'));
      Exit;
    end;

    x := GetVisibleRect.Left;
    y := GetVisibleRect.Top;

    Modified := True;
    cuSelect('TOOL_' + ToolNameRes[dtText]);

    // create a new floating selection
    if SelState = stFloating then
      FlattenFloatingSelection;
    SelState := stFloating;

    with Selection.Image do
    begin
      // get text extent
      Font.Assign(_Font);
      w := 0;
      sl := TStringList.Create;
      try
        sl.Text := Text;

        for i := 0 to sl.Count - 1 do
          w := Max(w, TextExtent(sl[i]).X);
        Inc(w, TextExtent('M').X); // some letters exceed the bounding box
        h := TextExtent('Wg').Y;

        // draw text
        Resize(w, h * sl.Count);
        FillTransparent;
        for i := 0 to sl.Count - 1 do
          TextOut(0, h * i, sl[i], Color, AntiAlias);
      finally
        sl.Free;
      end;

      Selection.Box := Rect(x, y, x + Width, y + Height);
      Selection.Angle := 0;
      Selection.Depth := Max(0, FirstSelected);
    end; // with Selection.Image

    // crop excess transparency
    DoCropTransparency(False);
  end; // with Page

  // change to transform tool
  frmMain.frmToolbar.DrawTool := dtTransform;
end;

procedure TGraphicFrame.DoSelectAll;
begin
  Modified := True;
  cuSelect('MI_EDIT_SEL_ALL');

  with Doc.Pages[ImageIndex].Layers do
  begin
    if SelState = stFloating then
      FlattenFloatingSelection;
    SelState := stSelecting;
    Selection.Mask.FillColor(cl32White);
  end;

  RedrawPaintBox;
end;

procedure TGraphicFrame.DoDeselectAll;
begin
  with Doc.Pages[ImageIndex].Layers do
    if SelState <> stNone then
    begin
      Modified := True;
      cuSelect('MI_EDIT_SEL_NONE');

      case SelState of
        stSelecting: SelState := stNone;
        stFloating: FlattenFloatingSelection;
      end;

      RedrawPaintBox;
    end;
end;

procedure TGraphicFrame.DoInvertSelection;
begin
  with Doc.Pages[ImageIndex].Layers do
    if SelState = stSelecting then
    begin
      Modified := True;
      cuSelect('MI_EDIT_SEL_INVERT');

      Selection.Mask.Invert;
      CheckEmptySelection;

      RedrawPaintBox;
    end;
end;

procedure TGraphicFrame.DoLoadSelection(const FileName: string);
var
  ls: TLayers;
  d: TIconDoc;
  bm: TBitmap32;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  d := TIconDoc.Create;
  try
    // TODO show open options
    if d.LoadFromFile(FileName, ls.Width, ls.Height, 1, '') = iftNone then
    begin
      ShowMessage(lpGet('MSG_FAIL_SINGLE_OPEN'));
      Exit;
    end;
    if d.PageCount = 0 then Exit;

    Modified := True;
    cuSelect('MI_EDIT_SEL_LOAD');

    if ls.SelState = stFloating then
      ls.FlattenFloatingSelection;
    ls.SelState := stSelecting;

    bm := TBitmap32.Create;
    try
      bm.Assign(d.Pages[0].Layers);
      bm.Resize(ls.Width, ls.Height);
      ls.Selection.Mask.Assign(bm);
    finally
      bm.Free;
    end;
  finally
    d.Free;
  end;

  ls.CheckEmptySelection;
  RedrawPaintBox;
end;

procedure TGraphicFrame.DoSaveSelection(const FileName: string; FileType: TImageFileType);
var
  ls: TLayers;
  d: TIconDoc;
  pg: TDocPage;
  l: TLayer;
  dl: TieDataLosses;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  if ls.SelState <> stSelecting then
    Exit;

  d := TIconDoc.Create;
  try
    pg := d.NewPage;
    pg.Layers.Resize(ls.Width, ls.Height);
    l := pg.Layers.NewLayer;
    l.Image.FillColor(cl32Black);
    l.Image.Draw(0, 0, ls.Selection.Mask);
    // TODO: ask for quality, lossless, etc... ?
    d.SaveToFile(FileName, FileType,
      Pref_PNGLimit, ExtractOnlyFileName(FileName), 100, True, dl);
  finally
    d.Free;
  end;
end;

procedure TGraphicFrame.DoCropTransparency(CreateUndo: boolean);
var
  w, h: integer;
  r, rSel: TRect;
  ls: TLayers;
  bm: TBitmap32;

begin
  Modified := True;
  ls := Doc.Pages[ImageIndex].Layers;

  with ls do
  begin
    if ls.SelState = stFloating then
      r := ls.Selection.Image.GetAutoCropRect
    else
    begin
      bm := TBitmap32.Create;
      try
        bm.Assign(ls);
        r := bm.GetAutoCropRect;
      finally
        bm.Free;
      end;
    end;
    if r.Right <= r.Left then
      Exit;

    if CreateUndo then
    begin
      if SelState in [stNone, stSelecting] then
        cuAllLayersChanged('MI_EDIT_CROP_TRANSPARENCY', False)
      else
        cuFilter('MI_EDIT_CROP_TRANSPARENCY');
    end;

    case SelState of
      stNone, stSelecting:
      begin
        ls.Crop(r);
        PageSizeChanged;
      end; // NOT stFloating

      stFloating:
      begin
        rSel := Selection.Box;
        with Selection.Image do
        begin
          w := Width;
          h := Height;
        end;
        Selection.Box := Rect(rSel.Left + rSel.Width * r.Left div w,
          rSel.Top + rSel.Height * r.Top div h, rSel.Left +
          rSel.Width * r.Right div w, rSel.Top + rSel.Height * r.Bottom div h);
        Selection.Image.Crop(r);

        RedrawPaintBox;
      end; // stFloating
    end; // case SelState
  end; // with Page
end;

procedure TGraphicFrame.DoFlipRotate(Mode: TSimpleFlipRotate);
var
  i, w, h: integer;
  ls: TLayers;

begin
  Modified := True;
  cuFlipRotate('UNDO_FLIP_ROTATE');

  // do flipping/rotation
  ls := Doc.Pages[ImageIndex].Layers;
  if ls.SelState = stFloating then
  begin
    SimpleFlipRotate(ls.Selection.Image, Mode);

    if Mode in [frRot90Left, frRot90Right] then
      with ls.Selection.Box do
      begin
        w := Width;
        h := Height;
        Right := Left + h;
        Bottom := Top + w;
      end;

    RedrawPaintBox;
  end
  else
  begin
    for i := 0 to ls.LayerCount - 1 do
      SimpleFlipRotate(ls.Layers[i].Image, Mode);
    if ls.SelState = stSelecting then
      SimpleFlipRotate(ls.Selection.Mask, Mode);

    // make Layers.size consistent with contents
    if Mode in [frRot90Left, frRot90Right] then
    begin
      ls.Resize(ls.Height, ls.Width);
      PageSizeChanged;
    end
    else
      RedrawPaintBox;
  end; // SelState <> floating
end;

function TGraphicFrame.DoLoad: boolean;
var
  i: integer;
  vectorScaleFactor: double;
  Cropped: boolean;
  ls: TLayers;
  ft: TImageFileType;
  fs: TFileStream;

begin
  Result := False;

  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    ft := DetectImageFileType(fs);
  finally
    fs.Free;
  end;
  if ft = iftNone then
    Exit;

  // show open options dialog
  vectorScaleFactor := 1;
  if ft = iftSvg then
  begin
    DoCreateForm(TfrmSvgOpenOptions, frmSvgOpenOptions);
    frmSvgOpenOptions.Execute(FileName);
    vectorScaleFactor := frmSvgOpenOptions.neScale.Value;
  end;

  Screen.Cursor := crHourGlass;
  try
    FileType := Doc.LoadFromFile(FileName, Pref_MaxWidth + 1,
      Pref_MaxHeight + 1, vectorScaleFactor, lpGet('LY_BACKGROUND'));
    Result := (FileType <> iftNone);

    // unsuccessful?
    if not Result then
      Exit;

    // store file type according to filename
    // e.g. there may be .ico files which are actually .bmp files
    // in this cases we want to avoid saving these files in also .bmp format
    // instead we want to save in .ico format if the ext is .ico
    ft := DetectImageFileTypeFromName(FileName);
    if ft <> iftNone then
      FileType := ft;

    if ft >= iftFirstReadOnly then
      FileType := iftNone; // prompt on save

    Modified := False;

    // crop the image to the maximum allowed size
    Cropped := False;
    for i := 0 to Doc.PageCount - 1 do
    begin
      ls := Doc.Pages[i].Layers;
      if (ls.Width > Pref_MaxWidth) or (ls.Height > Pref_MaxHeight) then
      begin
        Cropped := True;
        ls.Resize(Min(Pref_MaxWidth, ls.Width), Min(Pref_MaxHeight, ls.Height));
      end;
    end;

    // clear undo and redo stacks
    PurgeUndoRedo(UndoStack);
    PurgeUndoRedo(RedoStack);

    PageCountChanged;
    PageSizeChanged;

    if Cropped then
      ShowMessage(Format(lpGet('MSG_CROPPED'), [Pref_MaxWidth, Pref_MaxHeight]));
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TGraphicFrame.DoSave(const fn: string; ft: TImageFileType;
  Options: TDocumentSaveOptions): boolean;
var
  DataLoss: TieDataLosses;
  dl: TieDataLoss;
  s: string;

begin
  Result := False;
  if (ft = iftNone) or (ft >= iftFirstReadOnly) then
    Exit;

  if not (dsoSilent in Options) then
  begin
    // show save options dialog
    DoCreateForm(TfrmSaveOptions, frmSaveOptions);
    if not frmSaveOptions.Execute(Self, ft) then
      Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    if not (dsoSaveACopy in Options) then
    begin
      FileName := fn;
      FileType := ft;
    end;

    DoCreateForm(TfrmSaveOptions, frmSaveOptions);
    Doc.SaveToFile(fn, ft, Pref_PNGLimit,
      ExtractOnlyFileName(fn),
      frmSaveOptions.neQuality.Value,
      frmSaveOptions.cbLossless.Checked,
      DataLoss);
    if not (dlError in DataLoss) and not (dsoSaveACopy in Options) then
      Modified := False;
  finally
    Screen.Cursor := crDefault;
  end;

  if not (dsoSilent in Options) and (DataLoss <> []) then
  begin
    // show message about data loss
    s := LineEnding + lpGet('MSG_DATA_LOSS_INFO');
    for dl in TieDataLoss do
      if dl in DataLoss then
        s := MsgDataLoss[dl] + LineEnding + s;
    ShowMessage(s);
  end;

  Result := not (dlError in DataLoss);
end;

procedure TGraphicFrame.UseFilter(Proc: TFilterCallback;
  NeedApplySelTransform: boolean; const sUndoText: string);
begin
  Modified := True;
  cuFilter(sUndoText);

  with Doc.Pages[ImageIndex].Layers do
    if (SelState = stFloating) and NeedApplySelTransform then
      ApplySelectionTransform(Rect(0, 0, 0, 0));
  DoFilter(Proc);
end;

procedure TGraphicFrame.DoFilter(Proc: TFilterCallback);
var
  i: integer;
  ls: TLayers;
  Mask: TBitmap1;

begin
  ls := Doc.Pages[ImageIndex].Layers;

  Screen.Cursor := crHourGlass;
  try
    case ls.SelState of
      stNone, stSelecting:
      begin
        if ls.SelState = stNone then
          Mask := nil
        else
          Mask := ls.Selection.Mask;
        for i := 0 to ls.LayerCount - 1 do
          if ls[i].Selected then
            Proc(ls[i].Image, Mask);
      end;

      stFloating: Proc(ls.Selection.Image, nil);
    end; // case
  finally
    Screen.Cursor := crDefault;
  end;

  RedrawPaintBox;
end;

procedure TGraphicFrame.DoFilter_Grayscale(bm: TBitmap32; Mask: TBitmap1);
begin
  fltSimple(bm, Mask, fsfGrayscale);
end;

procedure TGraphicFrame.DoFilter_Invert(bm: TBitmap32; Mask: TBitmap1);
begin
  fltSimple(bm, Mask, fsfInvert);
end;

procedure TGraphicFrame.DoFilter_Solarize(bm: TBitmap32; Mask: TBitmap1);
begin
  fltSimple(bm, Mask, fsfSolarize);
end;

procedure TGraphicFrame.DoFilter_Average(bm: TBitmap32; Mask: TBitmap1);
begin
  fltAverage(bm, Mask);
end;

procedure TGraphicFrame.DoFilter_SoftBlur(bm: TBitmap32; Mask: TBitmap1);
begin
  fltGaussianBlur(bm, Mask, 0.5, False, False);
end;

procedure TGraphicFrame.DoFilter_BlurMore(bm: TBitmap32; Mask: TBitmap1);
begin
  fltBoxBlur(bm, Mask, 3, False, False, False);
end;

procedure TGraphicFrame.DoFilter_Sharpen(bm: TBitmap32; Mask: TBitmap1);
begin
  fltUnsharpMask(bm, Mask, 256, 0.7, 0, False);
end;

procedure TGraphicFrame.DoFilter_PaintContour(bm: TBitmap32; Mask: TBitmap1);
begin
  fltPaintContour(bm, Mask, $80, Round(frmMain.frmToolSet.neBrushSize.Value),
    frmMain.frmToolSet.BrushShape, frmMain.frmColor.SelColor[0], False);
end;

procedure TGraphicFrame.MakeSelectionStamp(ls: TLayers);
var
  i: integer;

begin
  for i := 0 to ls.LayerCount - 1 do
    if ls[i].Selected then
      ls[i].Image.TransformDraw(ls.Selection.Box, ls.Selection.Angle,
        ls.Selection.Image, TLayers.AntialiasSelection);
end;

procedure TGraphicFrame.cbZoomChange(Sender: TObject);
var
  s: string;

begin
  if frmUpdating = 0 then
  begin
    s := cbZoom.Items[cbZoom.ItemIndex];
    if s[2] = '/' then
      // shrink
      Zoom := -StrToInt(Copy(s, 3, Length(s) - 2))
    else
      // zoom
      Zoom := StrToInt(Copy(s, 1, Length(s) - 1));
  end;
end;

procedure TGraphicFrame.lbListMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  ii: integer;
begin
  ii := lb.ItemAtPos(x, y);
  if ii = lb.Count-1 then // new page
    lb.Cursor:=crHandPoint
  else
    lb.Cursor:=crDefault;
end;

procedure TGraphicFrame.lbResize(Sender: TObject);
begin
  lb.ItemHeight := lb.ListRect.Width;
end;

procedure TGraphicFrame.pmPagesPopup(Sender: TObject);
begin
  MenuItemToPopupMenu(frmMain.miIcon, Sender as TPopupMenu);
end;

procedure TGraphicFrame.sbPagesClick(Sender: TObject);
begin
  DoTogglePages;
end;

procedure TGraphicFrame.sbXChange(Sender: TObject);
begin
  ScrollPaintBox;
end;

procedure TGraphicFrame.sbYChange(Sender: TObject);
begin
  ScrollPaintBox;
end;

procedure TGraphicFrame.pbResize(Sender: TObject);
begin
  if sbZoomFit.Down then
    zoomFitWindow;
  RedrawPaintBox;
end;

procedure TGraphicFrame.sbZoomInClick(Sender: TObject);
begin
  if Zoom = -2 then
    Zoom := 1
  else
    Zoom := Zoom + 1;
end;

procedure TGraphicFrame.sbZoomOutClick(Sender: TObject);
begin
  if Zoom = 1 then
    Zoom := -2
  else
    Zoom := Zoom - 1;
end;

procedure TGraphicFrame.sbGridClick(Sender: TObject);
begin
  if Zoom > 1 then
    RedrawPaintBox;
end;

procedure TGraphicFrame.sbZoomFitClick(Sender: TObject);
begin
  if sbZoomFit.Down then
    zoomFitWindow;
end;

procedure TGraphicFrame.pbUpdateCursor;
var
  p: TPoint;
  i, xKnob, yKnob, X, Y: integer;
  d1, d2, CursorAngle: double;
  isShift, isCtrl: boolean;
  dt: TDrawTool;
  cr: TCursor;
  iec: TieCursor;

begin
  p := pb.ScreenToClient(Mouse.CursorPos);
  X := p.x;
  Y := p.y;
  if (x < 0) or (y < 0) or (x >= pb.Width) or (y >= pb.Height) then
    Exit;

  dt := frmMain.frmToolbar.DrawTool;

  // Space pressed -> hand tool
  if (ma = iemaNone) and (GetKeyState(VK_SPACE) < 0) then
    cr := ieCursorBase + Ord(iecHand)
  else
  if ma = iemaHand then
    cr := ieCursorBase + Ord(iecHandGrab)
  else
  // Are there any layers selected?
  if (dt in [dtWand, dtRetouch, dtRect, dtEllipse, dtLine, dtPencil, dtBrush,
    dtEraser, dtRecolor, dtBucket, dtGradient]) and
    (Doc.Pages[ImageIndex].Layers.SelectedCount = 0) then
    cr := crNo
  else
  begin
    isShift := GetKeyState(VK_SHIFT) < 0;
    isCtrl := GetKeyState(VK_CONTROL) < 0;

    case dt of
      dtSelRect, dtSelEllipse, dtLasso, dtWand, dtSelPencil:
      begin
        if isShift then
          if isCtrl then
            iec := iecSelectInt
          else
            iec := iecSelectAdd
        else
        if isCtrl then
          iec := iecSelectSub
        else
          iec := iecSelect;

        cr := ieCursorBase + Ord(iec);
      end;
      dtTransform:
      begin
        case dtTransformHitTest(X, Y, xKnob, yKnob, d1, d2) of
          xfMove: cr := crSizeAll;

          xfSize:
          begin
            // get cursor angle
            CursorAngle := Doc.Pages[ImageIndex].Layers.Selection.Angle;
            if xKnob = 0 then
              CursorAngle += 0.5 * pi
            else
            if yKnob = 0 then
            begin
            end
            else
            if xKnob = yKnob then
              CursorAngle += 0.25 * pi
            else
              CursorAngle -= 0.25 * pi;
            // get cursor
            i := Floor(FloatMod(CursorAngle, pi) / pi * 8);
            case i of
              0, 7: cr := crSizeWE;
              1, 2: cr := crSizeNWSE;
              3, 4: cr := crSizeNS;
              else
                cr := crSizeNESW;
            end;
          end;

          xfRotate: cr := ieCursorBase + Ord(iecRotate);
          else
            cr := crNo;
        end;
      end;
      dtHotSpot: cr := crArrow;
      dtText: cr := crIBeam;
      else
        cr := ieCursorBase + Ord(ToolCursor[dt]);
    end;
  end; // if some layers are selected

  pb.Cursor := cr;
end;

procedure TGraphicFrame.sbZoom1Click(Sender: TObject);
begin
  Zoom := 1;
end;

procedure TGraphicFrame.tmMarqueeTimer(Sender: TObject);
begin
  // TODO: maybe visible is not good here
  if (ma = iemaNone) and Visible then
  begin
    MarqueeOffset := (MarqueeOffset + 1) mod 8;
    if DrawMarquees then
      pb.SwapBuffers;
  end;
end;

procedure TGraphicFrame.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  dt: TDrawTool;
  ls: TLayers;
  xfHitTest: TdtTransformHitTest;
  Quad: TQuad;
  qx, qy: double;

begin
  // Do not allow drawing with a button, if the other one is still pressed
  if ma <> iemaNone then
    Exit;

  pOrigView := Point(X, Y);
  pOrig := Point(Floor(xFromView(X)), Floor(yFromView(Y)));
  pCurr := pOrig;
  dt := frmMain.frmToolbar.DrawTool;
  ls := Doc.Pages[ImageIndex].Layers;

  mb := Button;
  SelColor[0] := frmMain.frmColor.SelColor[Ord(mb = mbRight)];
  SelColor[1] := frmMain.frmColor.SelColor[Ord(mb <> mbRight)];
  aaRes := IfThen(frmMain.frmToolSet.CommonToolSettings[dt].Antialias, 4, 1);

  // Hand tool?
  if GetKeyState(VK_SPACE) < 0 then
  begin
    pOrigSBPos := Point(sbX.Position, sbY.Position);
    ma := iemaHand;
    pbUpdateCursor;
  end
  else
    case dt of
      dtSelRect, dtSelEllipse, dtLasso, dtWand, dtSelPencil:
        if (dt <> dtWand) or (ls.SelectedCount <> 0) then
      begin
        Modified := True;
        cuSelect('TOOL_' + ToolNameRes[dt]);

        // make sure that we are in the proper selection state
        if ls.SelState = stFloating then
          ls.FlattenFloatingSelection;
        ls.SelState := stSelecting;

        // initialize selection sketch
        SelSketch.Resize(ls.Width, ls.Height);
        SelSketch.FillTransparent;

        if dt = dtLasso then
          FPolyCount := 0;

        ma := iemaDrawing;
      end;

      dtTransform:
      begin
        xfHitTest := dtTransformHitTest(X, Y, pKnob.X, pKnob.Y,
          xResizeDiff, yResizeDiff);
        if xfHitTest <> xfNone then
        begin
          Modified := True;
          cuAllLayersChanged('TOOL_' + ToolNameRes[dt], True);

          // ctrl -> leave a copy in the original position
          if ssCtrl in Shift then
            MakeSelectionStamp(ls);
          if not GetFloatingQuad(qOrig, False) then
            Exit;

          case xfHitTest of
            xfSize: ma := iemaSizing;
            xfMove: ma := iemaMoving;
            xfRotate:
            begin
              if not GetFloatingQuad(Quad, True) then
                Exit;
              GetGravityCenter(Quad, qx, qy);
              AngleDiff := GetPhi(X - qx, Y - qy) - ls.Selection.Angle;
              ma := iemaRotating;
            end; // rotate
          end;
        end; // hit test <> none
      end;

      dtCrop:
      begin
        Modified := True;

        cuAllLayersChanged('TOOL_' + ToolNameRes[dt], False);

        ma := iemaDrawing;
      end;

      dtHotSpot: if PtInRect(ls.ClientRect, pCurr) then
        begin
          Modified := True;
          cuHotSpot('TOOL_' + ToolNameRes[dt]);

          ma := iemaDrawing;
        end;

      dtEyedropper: if PtInRect(ls.ClientRect, pCurr) then
          ma := iemaDrawing;

      dtRetouch:
        if ls.SelectedCount <> 0 then
        begin
          Modified := True;
          cuPaint('TOOL_' + ToolNameRes[dt]);

          ma := iemaDrawing;
        end;

      dtRect, dtEllipse, dtLine, dtPencil, dtBrush, dtEraser, dtRecolor,
      dtBucket, dtGradient:
        if ls.SelectedCount <> 0 then
        begin
          Modified := True;
          cuPaint('TOOL_' + ToolNameRes[dt]);

          // initialize auxiliary bitmaps
          if dt <> dtBucket then
            DrawSketch.Resize(ls.Width, ls.Height);
          if dt in [dtEllipse, dtLine, dtPencil, dtBrush, dtEraser, dtRecolor] then
          begin
            DrawSketch.FillTransparent;
            DrawMask.Resize(ls.Width * aaRes, ls.Height * aaRes);
            DrawMask.FillTransparent;
          end;
          rUpdate := Rect(0, 0, ls.Width, ls.Height);
          lsResult.Assign(ls);

          ma := iemaDrawing;
        end;

      dtText: ma := iemaDrawing;
    end; // case dt

  DrawInvertedOutline;
  pbMouseMove(pb, Shift, X, Y);
end;

procedure TGraphicFrame.pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  i, xDelta, yDelta, x2, y2, iFix, iFixHoriz, iFixVert, xFix, yFix: integer;
  First, SampleAll: boolean;
  Quad: TQuad;
  Opacity: byte;
  c0, c1, clSketch: TColor32;
  q, Angle, qx, qy, qw, qh, oldWidth, oldHeight, cx, cy, zoomFactor: double;
  dt: TDrawTool;
  ls: TLayers;
  lss: TLayerSubset;
  bmMerged: TBitmap32;
  Mask, ffMask: TBitmap1;
  r, rUpdatePrev: TRect;
  Pattern: TDrawPattern;
  bmPattern: TDrawPatternBitmap;
  sl: PColor32Array;

begin
  (*inc(FPSCounter);
  i := GetTickCount;
  if i - FPSLastTick >= 1000 then
  begin
    TabCaption := IntToStr(FPSCounter);
    FPSCounter := 0;
    FPSLastTick := i;
  end;*)

  if ma = iemaNone then
    pbUpdateCursor;

  // Remove inverted outline (for e.g. selection tools)
  DrawInvertedOutline;

  pPrev := pCurr;
  pCurr := Point(Floor(xFromView(X)), Floor(yFromView(Y)));
  rUpdatePrev := rUpdate;

  case ma of
    iemaNone:
    begin
      frmMain.SetStatus(Format('X: %d, Y: %d', [pCurr.X, pCurr.Y]));
      Exit;
    end;
    iemaHand:
    begin
      q := IfThen(Zoom > 0, 1 / Zoom, -Zoom);
      sbX.Position := pOrigSBPos.X - Round(q * (X - pOrigView.X));
      sbY.Position := pOrigSBPos.Y - Round(q * (Y - pOrigView.Y));
      Exit;
    end;
  end;

  dt := frmMain.frmToolbar.DrawTool;
  ls := Doc.Pages[ImageIndex].Layers;

  // keep pCurr inside bitmap
  if dt in [dtHotSpot, dtEyedropper] then
    pCurr := Point(Max(0, Min(ls.Width - 1, pCurr.X)),
      Max(0, Min(ls.Height - 1, pCurr.Y)));

  // holding down Shift = shape must be regular
  if ssShift in Shift then
    case dt of
      dtCrop, dtRect, dtEllipse:
      begin
        xDelta := Abs(pCurr.X - pOrig.X);
        yDelta := Abs(pCurr.Y - pOrig.Y);

        if xDelta > yDelta then
          if pCurr.X > pOrig.X then
            pCurr.X := pOrig.X + yDelta
          else
            pCurr.X := pOrig.X - yDelta
        else
        if pCurr.Y > pOrig.Y then
          pCurr.Y := pOrig.Y + xDelta
        else
          pCurr.Y := pOrig.Y - xDelta;
      end; // rectangular tools

      dtLine:
      begin
        Angle := GetPhi(pCurr.X - pOrig.X, pCurr.Y - pOrig.Y);

        // horizontal
        if (Angle >= pi * 15 / 8) or (Angle < pi * 1 / 8) or
          ((Angle >= pi * 7 / 8) and (Angle < pi * 9 / 8)) then
          pCurr.Y := pOrig.Y
        else
        // vertical
        if ((Angle >= pi * 3 / 8) and (Angle < pi * 5 / 8)) or
          ((Angle >= pi * 11 / 8) and (Angle < pi * 13 / 8)) then
          pCurr.X := pOrig.X
        else
        // backslash
        if ((Angle >= pi * 1 / 8) and (Angle < pi * 3 / 8)) or
          ((Angle >= pi * 9 / 8) and (Angle < pi * 11 / 8)) then
        begin
          i := (pCurr.X - pOrig.X + pCurr.Y - pOrig.Y) div 2;
          pCurr := Point(pOrig.X + i, pOrig.Y + i);
        end
        else
        // slash
        if ((Angle >= pi * 5 / 8) and (Angle < pi * 7 / 8)) or
          ((Angle >= pi * 13 / 8) and (Angle < pi * 15 / 8)) then
        begin
          i := (pOrig.X - pCurr.X + pCurr.Y - pOrig.Y) div 2;
          pCurr := Point(pOrig.X - i, pOrig.Y + i);
        end;
      end; // dtLine
    end;

  // status bar
  case dt of
    dtSelRect, dtSelEllipse, dtRect, dtEllipse, dtLine: frmMain.SetStatus(
        Format('%d x %d @ (%d, %d)..(%d, %d)',
        [Abs(pCurr.X - pOrig.X) + 1, Abs(pCurr.Y - pOrig.Y) + 1,
        pOrig.X, pOrig.Y, pCurr.X, pCurr.Y]));

    dtLasso: frmMain.SetStatus(Format(lpGet('HINT_DOC_NODES'), [FPolyCount]));

    dtTransform: with ls.Selection.Box do
        frmMain.SetStatus
        (Format('%d x %d @ (%d, %d)', [Width, Height, Left, Top]));

    dtCrop: frmMain.SetStatus(Format('%d x %d @ (%d, %d)',
        [Abs(pCurr.X - pOrig.X) + 1, Abs(pCurr.Y - pOrig.Y) + 1,
        Min(pOrig.X, pCurr.X), Min(pOrig.Y, pCurr.Y)]));

    dtHotSpot: frmMain.SetStatus(Format('X: %d, Y: %d', [pCurr.X, pCurr.Y]));
  end;

  // actual drawing
  case dt of
    //      dtSelRect, dtSelEllipse, dtCrop: nothing special, just a marquee

    dtLasso: if (FPolyCount < MaxPolyCount) and
        ((FPolyCount = 0) or (FPoints[FPolyCount - 1].X <> pCurr.X) or
        (FPoints[FPolyCount - 1].Y <> pCurr.Y)) then
      begin
        Inc(FPolyCount);
        FPoints[FPolyCount - 1] := pCurr;
      end;

    dtSelPencil: SelSketch.Line(pPrev.X, pPrev.Y, pCurr.X, pCurr.Y,
        1, bshRounded, cl32White, nil);

    dtTransform:
    begin
      // shift -> path after moving object
      if ssShift in Shift then
        MakeSelectionStamp(ls);

      case ma of
        iemaSizing:
        begin
          // determine which sides of the quadrilateral will be preserved
          xFix := IfThen(pKnob.X = -1, 1, 0);
          yFix := IfThen(pKnob.Y = -1, 1, 0);
          iFix := GetQuadIndex(xFix, yFix); // this vertex is certainly unmoved
          iFixHoriz := GetQuadIndex(1 - xFix, yFix); // horizontal neighbor of iFix
          iFixVert := GetQuadIndex(xFix, 1 - yFix); // vertical -"-

          // calculate new width and height of quad
          zoomFactor := IfThen(Zoom > 0, Zoom, -1 / Zoom);
          qw := Max(1, (PointLineDist(xFromView(X), yFromView(Y),
            qOrig[iFix].x, qOrig[iFix].y, qOrig[iFixVert].x,
            qOrig[iFixVert].y) - xResizeDiff / zoomFactor) *
            Sign(PointLineDist(qOrig[iFixHoriz].x, qOrig[iFixHoriz].y,
            qOrig[iFix].x, qOrig[iFix].y, qOrig[iFixVert].x,
            qOrig[iFixVert].y)));
          qh := Max(1, (PointLineDist(xFromView(X), yFromView(Y),
            qOrig[iFix].x, qOrig[iFix].y, qOrig[iFixHoriz].x,
            qOrig[iFixHoriz].y) - yResizeDiff / zoomFactor) *
            Sign(PointLineDist(qOrig[iFixVert].x, qOrig[iFixVert].y,
            qOrig[iFix].x, qOrig[iFix].y, qOrig[iFixHoriz].x,
            qOrig[iFixHoriz].y)));

          // calculate new center of quad:
          // ptFix + qw/2*Normed(ptHoriz - ptFix) + qh/2*Normed(ptVert - ptFix)
          oldWidth := Sqrt(Sqr(qOrig[iFix].x - qOrig[iFixHoriz].x) +
            Sqr(qOrig[iFix].y - qOrig[iFixHoriz].y));
          oldHeight := Sqrt(Sqr(qOrig[iFix].x - qOrig[iFixVert].x) +
            Sqr(qOrig[iFix].y - qOrig[iFixVert].y));
          // this should never happen, but...
          if (oldWidth < 1e-6) or (oldHeight < 1e-6) then
            Exit;

          // maybe we shouldn't change width or height?
          if pKnob.x = 0 then
            qw := oldWidth;
          if pKnob.y = 0 then
            qh := oldHeight;

          // this will be the new center
          cx := qOrig[iFix].x + 0.5 * qw * (qOrig[iFixHoriz].x -
            qOrig[iFix].x) / oldWidth + 0.5 * qh * (qOrig[iFixVert].x -
            qOrig[iFix].x) / oldHeight;
          cy := qOrig[iFix].y + 0.5 * qw * (qOrig[iFixHoriz].y -
            qOrig[iFix].y) / oldWidth + 0.5 * qh * (qOrig[iFixVert].y -
            qOrig[iFix].y) / oldHeight;

          // set everything
          with ls.Selection.Box do
          begin
            Left := Round(cx - 0.5 * qw);
            Top := Round(cy - 0.5 * qh);
            Right := Left + Round(qw);
            Bottom := Top + Round(qh);
          end;
        end; // iemaSizing

        iemaMoving:
        begin
          with ls.Selection.Box do
          begin
            Inc(Left, pCurr.X - pPrev.X);
            Inc(Top, pCurr.Y - pPrev.Y);
            Inc(Right, pCurr.X - pPrev.X);
            Inc(Bottom, pCurr.Y - pPrev.Y);
          end;
        end; // iemaMoving

        iemaRotating:
        begin
          if not GetFloatingQuad(Quad, True) then
            Exit;
          GetGravityCenter(Quad, qx, qy);
          Angle := FloatMod(GetPhi(X - qx, Y - qy) - AngleDiff, 2 * pi);
          // snap to common values
          for i := 0 to 4 do
          begin
            q := i * 0.5 * pi;
            if Abs(Angle - q) < 0.01 then
            begin
              Angle := q;
              Break;
            end;
          end;
          // apply
          ls.Selection.Angle := Angle;
        end;
      end; // case ma
    end;

    dtHotSpot: Doc.Pages[ImageIndex].HotSpot := pCurr;

    dtEyedropper:
    begin
      bmMerged := TBitmap32.Create;
      try
        if frmMain.frmToolSet.CommonToolSettings[dt].SampleAllLayers then
          lss := lssAll
        else
          lss := lssSelected;
        ls.Render(bmMerged, ls.ClientRect, lss, True);
        frmMain.frmColor.SelColor[Ord(mb = mbRight)] :=
          bmMerged.SafeGetPixel(pCurr.X, pCurr.Y);
      finally
        bmMerged.Free;
      end;
    end;

    dtRetouch, dtRect, dtEllipse, dtLine, dtPencil, dtBrush,
    dtEraser, dtRecolor, dtBucket, dtGradient:
    begin
      // undo result of previous drawing,
      // if DrawSketch will completely change after this event
      if dt in [dtRect, dtEllipse, dtLine, dtGradient] then
        for i := 0 to ls.LayerCount - 1 do
          if ls[i].Selected then
            lsResult[i].Image.CopyRectOverwrite(rUpdate.Left, rUpdate.Top,
              ls[i].Image, rUpdate);

      // clear temporary bitmaps, if needed
      if dt in [dtEllipse, dtLine] then
        DrawMask.FillTransparent;
      if dt in [dtRect, dtEllipse, dtLine, dtGradient] then
        DrawSketch.FillTransparent;

      // sketch color
      clSketch := IfThen((dt in [dtEraser, dtRecolor]) or
        IsTransparent(SelColor[0]), cl32Black, SelColor[0]);

      // clipping mask
      if ls.SelState = stSelecting then
        Mask := ls.Selection.Mask
      else
        Mask := nil;

      // pattern
      Pattern := frmMain.frmToolSet.CommonToolSettings[dt].Pattern;
      bmPattern := DrawPatternBitmap[Pattern];

      if dt = dtEraser then
        Opacity := Round(frmMain.frmToolSet.neEraserAlpha.Value * 2.55)
      else
        Opacity := $ff;

      if ( ((dt in [dtPencil, dtBrush])
            and (IsOpaque(SelColor[0]) or IsTransparent(SelColor[0]) or (SelColor[0]=cl32Inverted)))
          or (dt = dtEraser) )
        and (aaRes = 1)
        and (Mask = nil)
        and (Pattern = dpFull)
        and (Opacity = $ff)
      then begin
        // eliminate DrawMask and DrawSketch, draw directly to lsResult
        clSketch := IfThen(dt = dtEraser, cl32Transparent, SelColor[0]);
        for i := 0 to ls.LayerCount - 1 do
          if ls[i].Selected then
        case dt of
          dtPencil: rUpdate := lsResult[i].Image.Line(pPrev.X, pPrev.Y, pCurr.X, pCurr.Y,
            1, bshRounded, clSketch, nil);
          dtBrush, dtEraser: rUpdate := lsResult[i].Image.Line(pPrev.X, pPrev.Y, pCurr.X, pCurr.Y,
            Round(frmMain.frmToolSet.neBrushSize.Value), frmMain.frmToolSet.BrushShape, clSketch, nil);
        end;
      end else
      begin
        case dt of
          dtRetouch: for i := 0 to ls.LayerCount - 1 do
              if ls[i].Selected then
                rUpdate := fltRetouch(ls[i].Image, Mask, pPrev.X, pPrev.Y,
                  pCurr.X, pCurr.Y, Round(frmMain.frmToolSet.neBrushSize.Value),
                  frmMain.frmToolSet.BrushShape,
                  TRetouchMode(frmMain.frmToolSet.cbRetouchMode.ItemIndex));

          dtRect: rUpdate := DrawSketch.Rectangle(Rect(pOrig.X, pOrig.Y,
              pCurr.X, pCurr.Y), clSketch, frmMain.frmToolSet.FilledShapes,
              Round(frmMain.frmToolSet.neLineWidth.Value));
          dtEllipse: rUpdate :=
              DrawMask.Ellipse(Rect(pOrig.X * aaRes, pOrig.Y * aaRes,
              pCurr.X * aaRes, pCurr.Y * aaRes), cl32White,
              frmMain.frmToolSet.FilledShapes,
              Round(frmMain.frmToolSet.neLineWidth.Value * aaRes));
          dtLine: rUpdate := DrawMask.Line(pOrig.X * aaRes, pOrig.Y *
              aaRes, pCurr.X * aaRes, pCurr.Y * aaRes,
              Round(frmMain.frmToolSet.neLineWidth.Value * aaRes),
              bshRounded, cl32White, nil);

          dtPencil: rUpdate := DrawMask.Line(pPrev.X * aaRes, pPrev.Y *
              aaRes, pCurr.X * aaRes, pCurr.Y * aaRes, aaRes, bshRounded, cl32White, nil);
          dtBrush, dtEraser, dtRecolor: rUpdate :=
              DrawMask.Line(pPrev.X * aaRes, pPrev.Y * aaRes, pCurr.X *
              aaRes, pCurr.Y * aaRes, Round(frmMain.frmToolSet.neBrushSize.Value * aaRes),
              frmMain.frmToolSet.BrushShape, cl32White, nil);

          dtBucket:
          begin
            SampleAll := frmMain.frmToolSet.CommonToolSettings[dt].SampleAllLayers;
            ffMask := TBitmap1.Create; // floodfill mask
            try
              if SampleAll then
              begin
                // sample ALL the layers!
                bmMerged := TBitmap32.Create;
                try
                  bmMerged.Assign(ls);
                  // create mask
                  rUpdate := bmMerged.CreateFloodFillMask(ffMask, pCurr.X,
                    pCurr.Y, Round(frmMain.frmToolSet.neTolerance.Value * 2.55),
                    frmMain.frmToolSet.sbContiguous.Down);
                finally
                  bmMerged.Free;
                end;
              end; // sample all layers

              // paint
              First := True;
              for i := 0 to ls.LayerCount - 1 do
                if ls[i].Selected then
                begin
                  lsResult[i].Assign(ls[i]);

                  // create mask if needed
                  if not frmMain.frmToolSet.CommonToolSettings[dt].SampleAllLayers then
                  begin
                    // create mask
                    r := lsResult[i].Image.CreateFloodFillMask(ffMask,
                      pCurr.X, pCurr.Y, Round(frmMain.frmToolSet.neTolerance.Value * 2.55),
                      frmMain.frmToolSet.sbContiguous.Down);
                    if First then
                      rUpdate := r
                    else
                      UnionRect(rUpdate, rUpdate, r);
                  end
                  else
                    r := rUpdate;

                  if First or not SampleAll then
                  begin
                    // apply clipping mask and pattern
                    if (Mask <> nil) or (Pattern <> dpFull) then
                    begin
                      for x2 := r.Left to r.Right - 1 do
                        for y2 := r.Top to r.Bottom - 1 do
                          if ((Mask <> nil) and not Mask[x2, y2]) or
                            not bmPattern[y2 and (PatternSize - 1),
                            x2 and (PatternSize - 1)] then
                            ffMask.Bits[x2, y2] := False;
                    end; // apply mask and pattern
                  end;

                  lsResult[i].Image.PaintMask(0, 0, ffMask, SelColor[0]);
                  First := False;
                end; // if layer is selected
            finally
              ffMask.Free;
            end;
          end; // dtBucket

          dtGradient:
          begin
            if frmMain.frmToolSet.GradientMode = gmColor then
            begin
              c0 := SelColor[0];
              c1 := SelColor[1];
            end
            else
            begin
              if mb = mbLeft then
              begin
                c0 := cl32Transparent;
                c1 := cl32Black;
              end
              else
              begin
                c0 := cl32Black;
                c1 := cl32Transparent;
              end;
            end;

            DrawSketch.Gradient(c0, c1, pOrig, pCurr, DrawSketch.ClientRect,
              frmMain.frmToolSet.GradientKind, frmMain.frmToolSet.GradientRep);
            rUpdate := DrawSketch.ClientRect;
          end;
        end;

        // project a small part of DrawMask onto DrawSketch, if needed
        if dt in [dtEllipse, dtLine, dtPencil, dtBrush, dtEraser, dtRecolor] then
        begin
          if aaRes = 4 then
          begin
            PaintAntialiasMask4(DrawSketch, DrawMask, clSketch, rUpdate);
            rUpdate.Left := rUpdate.Left div aaRes;
            rUpdate.Top := rUpdate.Top div aaRes;
            rUpdate.Right := (rUpdate.Right - 1) div aaRes + 1;
            rUpdate.Bottom := (rUpdate.Bottom - 1) div aaRes + 1;
          end
          else
          begin
            for y2 := rUpdate.Top to rUpdate.Bottom - 1 do
            begin
              sl := DrawSketch.ScanLine(y2);
              for x2 := rUpdate.Left to rUpdate.Right - 1 do
                if DrawMask[x2, y2] then
                  sl[x2] := clSketch;
            end;
          end;
        end; // if render DrawMask to DrawSketch

        // render lsResult
        if not (dt in [dtRetouch, dtBucket]) then
        begin
          // intersect DrawSketch with the clipping mask and pattern
          if (Mask <> nil) or (Pattern <> dpFull) then
          begin
            for x2 := rUpdate.Left to rUpdate.Right - 1 do
              for y2 := rUpdate.Top to rUpdate.Bottom - 1 do
                if ((Mask <> nil) and not Mask[x2, y2]) or not
                  bmPattern[y2 and (PatternSize - 1), x2 and (PatternSize - 1)] then
                  DrawSketch.FastSetPixel(x2, y2, cl32Transparent);
          end; // apply mask and pattern

          // make lsResult
          for i := 0 to ls.LayerCount - 1 do
            if ls[i].Selected then
            begin
              // erase
              if dt in [dtPencil, dtBrush, dtEraser, dtRecolor] then
                lsResult[i].Image.CopyRectOverwrite(rUpdate.Left,
                  rUpdate.Top, ls[i].Image, rUpdate);

              if dt = dtRecolor then
                fltRecolor(lsResult[i].Image, DrawSketch, rUpdate,
                  SelColor[1], SelColor[0],
                  Round(frmMain.frmToolSet.neTolerance.Value))
              else if (dt = dtEraser) or ((dt = dtGradient) and
                (frmMain.frmToolSet.GradientMode = gmTransparency)) or
                ((dt <> dtGradient) and IsTransparent(SelColor[0])) then
                EraseWith(lsResult[i].Image, DrawSketch, rUpdate, Opacity)
              else
                lsResult[i].Image.CopyRect(rUpdate.Left, rUpdate.Top, DrawSketch, rUpdate);
            end; // for layers
        end; // if draw sketch
      end; // if need to use DrawMask and DrawSketch
    end; // shape / brush tools
  end; // case DrawTool

  // refresh the paintbox
  if dt in dtHasInvertedOutline then
  begin
    DrawInvertedOutline;
    pb.SwapBuffers;
  end
  else
  if dt in [dtRetouch, dtPencil, dtBrush, dtEraser, dtRecolor] then // brush-like tools
  begin
    RedrawPaintBoxPart(rUpdate);
  end
  else
  if dt in [dtRect, dtEllipse, dtLine, dtBucket, dtGradient] then
  begin
    UnionRect(r, rUpdate, rUpdatePrev);
    RedrawPaintBoxPart(r);
  end
  else
  if not (dt = dtEyedropper) then
    RedrawPaintBox;
end;

procedure TGraphicFrame.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  dt: TDrawTool;
  ls: TLayers;
  lss: TLayerSubset;
  bmMerged: TBitmap32;
  r: TRect;
  bcm: TBitsCombineMode;

begin
  if ma in [iemaNone, iemaHand] then
    ma := iemaNone
  else
  begin
    dt := frmMain.frmToolbar.DrawTool;
    ls := Doc.Pages[ImageIndex].Layers;
    r := Rect(pOrig.X, pOrig.Y, pCurr.X, pCurr.Y);
    if dt = dtCrop then
      r := WinCanvasConvert(r);

    case dt of
      dtSelRect, dtSelEllipse, dtLasso, dtWand, dtSelPencil:
      begin
        // draw the selection sketch if needed
        case dt of
          dtSelRect: SelSketch.Rectangle(r, cl32White, True, 1);
          dtSelEllipse: SelSketch.Ellipse(r, cl32White, True, 1);
          dtLasso: SelSketch.CreatePolygonMask(FPolyCount, @FPoints);
          dtWand: if PtInRect(ls.ClientRect, pCurr) then
            begin
              bmMerged := TBitmap32.Create;
              try
                if frmMain.frmToolSet.CommonToolSettings[dt].SampleAllLayers then
                  lss := lssAll
                else
                  lss := lssSelected;
                ls.Render(bmMerged, ls.ClientRect, lss, True);
                bmMerged.CreateFloodFillMask(SelSketch, pCurr.X, pCurr.Y,
                  Round(frmMain.frmToolSet.neTolerance.Value * 2.55),
                  frmMain.frmToolSet.sbContiguous.Down);
              finally
                bmMerged.Free;
              end;
            end;
        end; // case DrawTool

        // combine the selection sketch with the current selection
        if ssShift in Shift then
          if ssCtrl in Shift then
            bcm := bcmAnd
          else
            bcm := bcmOr
        else
        if ssCtrl in Shift then
          bcm := bcmAndNot
        else
          bcm := bcmCopy;
        ls.Selection.Mask.Combine(SelSketch, bcm);

        // check if the selection is empty
        ls.CheckEmptySelection;
      end;

      // dtTransform: nothing

      dtCrop:
      begin
        // too large images are not allowed
        r.Right := Min(r.Left + Pref_MaxWidth, r.Right);
        r.Bottom := Min(r.Top + Pref_MaxHeight, r.Bottom);

        ls.Crop(r);
        PageSizeChanged;
      end; // dtCrop

      dtEyedropper: if frmMain.frmToolSet.sbEyedropperBack.Down then
          // switch back to previous tool
          frmMain.frmToolbar.DrawTool := frmMain.frmToolbar.PrevTool;

      dtRect, dtEllipse, dtLine, dtPencil, dtBrush, dtEraser, dtRecolor,
      dtBucket, dtGradient:
        ls.Assign(lsResult);

      dtText:
      begin
        DoCreateForm(TfrmText, frmText);
        frmText.Show;
      end;
    end; // case DrawTool

    ma := iemaNone;

    // redraw, just in case
    RedrawPaintBox;
    // clear status bar
    frmMain.SetStatus('');
  end;

  pbUpdateCursor;
end;

procedure TGraphicFrame.sbNewPageClick(Sender: TObject);
begin
  DoPageNew;
end;

procedure TGraphicFrame.sbDeletePageClick(Sender: TObject);
begin
  DoPageDelete;
end;

procedure TGraphicFrame.miNewPageClick(Sender: TObject);
begin
  DoPageNew;
end;

procedure TGraphicFrame.miDeletePageClick(Sender: TObject);
begin
  DoPageDelete;
end;

procedure TGraphicFrame.lbKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if (Key = VK_DELETE) and (Shift * [ssAlt, ssCtrl, ssShift] = []) then
    DoPageDelete;
end;

procedure TGraphicFrame.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  frmMain.SetStatus((Sender as TControl).Hint);
end;

procedure TGraphicFrame.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  frmMain.SetStatus('');
end;

procedure TGraphicFrame.miPagePropClick(Sender: TObject);
begin
  DoPageProp;
end;

function TGraphicFrame.DocFrameCanClose;
begin
  if not Modified then
    Exit(True);

  case QuerySaveChanges(FileName) of
    idYes: if (FileName <> '') and (FileType <> iftNone) then
        Result := DoSave(FileName, FileType, [])
      else
        Result := frmMain.DoSaveGraphicAs(Self, False);
    idNo: Result := True;
    else
      Result := False;
  end;
end;

procedure TGraphicFrame.pbKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ArrowKey: boolean;
  o: TPoint;
  ls: TLayers;

begin
  ArrowKey := True;
  case Key of
    VK_LEFT: o := Point(-1, 0);
    VK_RIGHT: o := Point(1, 0);
    VK_UP: o := Point(0, -1);
    VK_DOWN: o := Point(0, 1);
    else
      ArrowKey := False;
  end;

  if ArrowKey then
  begin
    ls := Doc.Pages[ImageIndex].Layers;

    if ls.SelState = stFloating then // move selection
    begin
      with ls.Selection do
        Box := Rect(Box.Left + o.x, Box.Top + o.y, Box.Right + o.x, Box.Bottom + o.y);
      RedrawPaintBox;
    end
    else // move view
    begin
      sbX.Position := sbX.Position + o.x;
      sbY.Position := sbY.Position + o.y;
    end;
  end;
end;

procedure TGraphicFrame.pbKillFocus(Sender: TObject);
begin
  DrawImageFrame;
end;

procedure TGraphicFrame.pbSetFocus(Sender: TObject);
begin
  DrawImageFrame;
end;

procedure TGraphicFrame.sbCenterLinesClick(Sender: TObject);
begin
  RedrawPaintBox;
end;

procedure TGraphicFrame.pbMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  d: integer;

begin
  d := -Sign(WheelDelta);
  case Pref_MWA of
    mwaScroll: if ssCtrl in Shift then
        sbX.Position := sbX.Position + 4 * d
      else
        sbY.Position := sbY.Position + 4 * d;
    mwaZoom: if d > 0 then
        sbZoomOutClick(nil)
      else
        sbZoomInClick(nil);
  end;

  Handled := True;
end;

procedure TGraphicFrame.sbTestClick(Sender: TObject);
begin
  DoCreateForm(TfrmTest, frmTest);
  frmTest.Execute(Self);
end;

procedure TGraphicFrame.sbSaveClick(Sender: TObject);
begin
  frmMain.DoSave;
end;

procedure TGraphicFrame.lbGetCount(Sender: TObject; var Value: integer);
begin
  // +1: "New Page" fake item
  Value := Doc.PageCount + 1;
end;

procedure TGraphicFrame.lbListDblClick(Sender: TObject);
begin
  DoPageProp;
end;

procedure TGraphicFrame.lbItemGetSelected(Sender: TObject; Index: integer;
  var Value: boolean);
begin
  Value := Index = ImageIndex;
end;

procedure TGraphicFrame.lbItemSetSelected(Sender: TObject; Index: integer;
  Value: boolean);
begin
  if Value and (Index < Doc.PageCount) then
    ImageIndex := Index;
end;

procedure TGraphicFrame.lbItemMouseMove(Sender: TObject; Index: integer;
  Shift: TShiftState; X, Y: integer);
var
  s: string;
  pg: TDocPage;

begin
  // set the status bar text
  if Index < Doc.PageCount then
  begin
    pg := Doc.Pages[Index];
    s := Format(lpGet('HINT_DOC_LB_PAGE'), [IntToStr(Index + 1)+'/'+IntToStr(Doc.PageCount),
      pg.Layers.Width, pg.Layers.Height]);

    if pg.DPI > 0 then
      s := s + ', ' + Format(lpGet('HINT_DOC_LB_DPI'), [pg.DPI])
    else if Doc.Metadata.DPI > 0 then
      s := s + ', ' + Format(lpGet('HINT_DOC_LB_DOC_DPI'), [Doc.Metadata.DPI]);

    if PNGCompressIcon(pg.Layers.Width, pg.Layers.Height, Pref_PNGLimit) then
      s := s + ', ' + lpGet('HINT_DOC_LB_PNG');
  end
  else
    s := lpGet('MI_ICON_PAGE_NEW');

  frmMain.SetStatus(s);
end;

procedure TGraphicFrame.lbItemMouseDown(Sender: TObject; Index: integer;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer; var MouseTrap: boolean);
begin
  if (Button = mbLeft) and (Index = Doc.PageCount) then
  begin
    MouseTrap := True;
    sbNewPage.Click;
  end;
end;

procedure TGraphicFrame.lbItemDragDrop(Sender: TObject; TargetIndex: integer);
begin
  DoPageMove(ImageIndex, Min(Doc.PageCount - 1, TargetIndex));
end;

procedure TGraphicFrame.lbItemPaint(Sender: TObject; ACanvas: TCanvas;
  Index: integer; ItemRect: TRect; ListFocused: boolean);
var
  x, y, fr, dpi: integer;
  r: TRect;
  Size: TPoint;
  s: string;
  bmItem, bmUser, bmThumb: TBitmap32;

begin
  if frmDestroying then
    Exit;

  bmItem := TBitmap32.Create;
  try
    bmItem.Resize(ItemRect.Width, ItemRect.Height);

    with bmItem do
    begin
      FillColor(TColor32(lb.Color) or cl32Opaque);

      // background
      if lb.Selected[Index] then
      begin
        r := ClientRect.Inflate(-8, -8);
        Rectangle(r, IfThen(ListFocused, $30a03030, $10000000), True, 0);
        fltBoxBlur(bmItem, nil, 9, False, False, False);
        bmItem.Gradient(cl32Transparent, $20000000,
          Point(0, 0), Point(0, bmItem.Height),
          r, gkLinear, grNone);
      end;

      // "New page" fake item?
      if Index >= Doc.PageCount then
      begin
        Font.Name := 'Arial';
        Font.Size := 9;

        s := lpGet('MI_ICON_PAGE_NEW');
        TextOut((Width - TextExtent(s).X) div 2, 40, s, cl32Gray, True);
      end
      else
      begin
        Font.Name := 'Tahoma';
        Font.Size := 8;

        // render user bitmap and thumbnail
        bmUser := TBitmap32.Create;
        bmThumb := TBitmap32.Create;
        try
          RenderUserBitmap(bmUser,
            Rect(Low(integer), Low(integer), High(integer), High(integer)), Index);
          bmThumb.CreateThumbnail(bmUser, 48, False);

          x := (Width - bmThumb.Width) div 2;
          y := Height - 32 - (48 + bmThumb.Height) div 2;
          Draw(x, y, bmThumb);

          // draw text
          fr := Doc.Pages[Index].FrameRate;
          if fr <> 0 then
            TextOut(12, 10, Format('%d ms (%d jifs)',
              [fr, (fr * 60 + 500) div 1000]), $a0000000, True);

          s := Format('%d x %d', [bmUser.Width, bmUser.Height]);
          if PNGCompressIcon(bmUser.Width, bmUser.Height, Pref_PNGLimit) then
            s := s + ' #';
          Size := TextExtent(s);
          TextOut((Width - Size.X) div 2, Height - 20 - Size.Y, s, $ff000000, True);

          s := '@' + pf32ToStr[GetPixelFormat32(bmUser, palBW, palWin16, nil, nil)];
          dpi := Round(Doc.Pages[Index].DPI);
          if dpi > 0 then s += '/'+IntToStr(dpi)+'dpi';
          Size := TextExtent(s);
          TextOut((Width - Size.X) div 2, Height - 8 - Size.Y, s, $a0000000, True);
        finally
          bmUser.Free;
          bmThumb.Free;
        end;
      end; // if last list item

      DrawToCanvas(ACanvas, ItemRect.Left, ItemRect.Top, Pref_Hatch);
    end; // for i
  finally
    bmItem.Free;
  end;
end;

end.

