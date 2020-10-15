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
unit Layers;

interface

uses
  LclIntf, LclType, SysUtils, Classes, Graphics, Math, BitmapEx,
  ImageTransform, bmExUtils, BlendModes, gfMath;

const
  // Hardcoded constant for maximum selection image size.
  // This should be worked around.
  // On the other hand, we cannot allow the selection to grow without limits.
  MaxSelImageSize = 5000;

type
  TLayer = class(TPersistent)
  public
    Name: string;
    Visible: boolean;
    Selected: boolean;
    Image: TBitmap32;
    Opacity: byte;
    BlendMode: TBlendMode;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
  end;

  // Separate object for storing only layer properties (no Image, Selected)
  // For undo purposes / GUI layer properties dialog
  TLayerProp = class(TPersistent)
  public
    Name: string;
    Visible: boolean;
    Opacity: byte;
    BlendMode: TBlendMode;

    constructor Create;
    procedure Assign(Src: TPersistent); override;

    procedure Read(x: TLayer);
    procedure Write(x: TLayer);
  end;

  TSelState = (stNone, stSelecting, stFloating);
  TSelInfo = class(TPersistent)
  public
    Image: TBitmap32; // used if stFloating
    Mask: TBitmap1; // used if stSelecting
    Box: TRect; // used if stFloating
    Angle: double; // used if stFloating
    // If the selection were a layer, it would be the Depth'th in the list
    Depth: integer; // used if stFloating

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;

    function IsRotated: boolean;
    function IsStretched: boolean;
  end;

  TLayerSubset = (lssAll, lssSelected, lssVisible);

  TLayers = class(TPersistent)
  private
    FWidth, FHeight: integer;

    FLayers: TList; // list of TLayer
    FSelState: TSelState;

    function GetLayer(Index: integer): TLayer;
    procedure SetLayer(Index: integer; const Value: TLayer);
    procedure SetSelState(const Value: TSelState);
  public
    Selection: TSelInfo;

    constructor Create;
    destructor Destroy; override;
    procedure Assign(Src: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear;
    function HasNonTrivialLayers: boolean;

    // Managing the collection
    function LayerCount: integer;
    function SelectedCount: integer;
    function VisibleCount: integer;
    function FirstSelected: integer; // returns -1 if no layers are selected
    procedure SelectOne(Index: integer);

    function NewLayerAtDepth(Depth: integer): TLayer;
    function NewLayer: TLayer; // at bottom
    procedure DeleteLayer(Index: integer);

    // Merges layers to a single trivial layer (max opacity, blend mode: normal).
    // Returns depth of the resulting layer.
    function Merge(Subset: TLayerSubset): integer;

    // Size
    function ClientRect: TRect;
    procedure Resize(NewWidth, NewHeight: integer);
    procedure Crop(const r: TRect);

    // Renders the layers onto the bitmap Dest
    // Clips by the rectangle r and only considers a specified subset of the layers
    // If ClearScreen is true, then resizes the bitmap so it will fit the size of r
    // and clears it
    // Otherwise, it draws layers on existing contents of the bitmap
    procedure Render(Dest: TBitmap32; r: TRect;
      Subset: TLayerSubset; ClearScreen: boolean);

    // Selection
    // If SelState = stSelecting and SelMask is completely blank,
    // SelState := stNone;
    function CheckEmptySelection: boolean;
    // Creates the floating selection masking the original image with SelMask
    procedure CreateFloatingSelection;
    // Draws SelImage on the corresponding layer and sets SelState to stNone
    procedure FlattenFloatingSelection;
    // If the dimensions of SelRect differ from SelImage,
    // SelImage will be resized to fit SelRect
    procedure ApplySelectionTransform(const FloatingSelectionMargin: TRect);
    class function AntialiasSelection: boolean;

    property Width: integer read FWidth;
    property Height: integer read FHeight;
    // WARNING: avoid SetLayer when possible (when using it, the old layer won't be freed)
    property Layers[Index: integer]: TLayer read GetLayer write SetLayer; default;
    property SelState: TSelState read FSelState write SetSelState;
  end;

function LayerInSubset(l: TLayer; Subset: TLayerSubset): boolean;

implementation

uses
  Main, ieShared;

function LayerInSubset;
begin
  case Subset of
    lssAll: Result := True;
    lssSelected: Result := l.Selected;
    lssVisible: Result := l.Visible;
  end;
end;

// TLayer

constructor TLayer.Create;
begin
  Name := '';
  Visible := True;
  Selected := False;
  Image := TBitmap32.Create;
  Opacity := $ff;
  BlendMode := bmNormal;
end;

destructor TLayer.Destroy;
begin
  Image.Free;
end;

procedure TLayer.Assign;
var
  l: TLayer;

begin
  if Src is TLayer then
  begin
    l := Src as TLayer;

    Name := l.Name;
    Visible := l.Visible;
    Selected := l.Selected;
    Image.Assign(l.Image);
    Opacity := l.Opacity;
    BlendMode := l.BlendMode;
  end else
    inherited;
end;

// TLayerProp

constructor TLayerProp.Create;
begin
  Name := '';
  Visible := True;
  Opacity := $ff;
  BlendMode := bmNormal;
end;

procedure TLayerProp.Assign;
var
  x: TLayerProp;

begin
  if Src is TLayerProp then
  begin
    x := Src as TLayerProp;

    Name := x.Name;
    Visible := x.Visible;
    Opacity := x.Opacity;
    BlendMode := x.BlendMode;
  end else
    inherited;
end;

procedure TLayerProp.Read;
begin
  Name := x.Name;
  Visible := x.Visible;
  Opacity := x.Opacity;
  BlendMode := x.BlendMode;
end;

procedure TLayerProp.Write;
begin
  x.Name := Name;
  x.Visible := Visible;
  x.Opacity := Opacity;
  x.BlendMode := BlendMode;
end;

// TSelInfo

constructor TSelInfo.Create;
begin
  Image := TBitmap32.Create;
  Mask := TBitmap1.Create;
  Box := Rect(0, 0, 1, 1);
  Angle := 0;
  Depth := 0;
end;

destructor TSelInfo.Destroy;
begin
  Image.Free;
  Mask.Free;
  
  inherited;
end;

procedure TSelInfo.Assign;
var
  x: TSelInfo;
  
begin
  if Src is TSelInfo then
  begin
    x := Src as TSelInfo;

    Image.Assign(x.Image);
    Mask.Assign(x.Mask);
    Box := x.Box;
    Angle := x.Angle;
    Depth := x.Depth;
  end else
    inherited;
end;

function TSelInfo.IsRotated;
begin
  Result := not IsZero(Angle, 1e-5);
end;

function TSelInfo.IsStretched;
begin
  Result := (Box.Width <> Image.Width) or (Box.Height <> Image.Height);
end;

// TLayers

function TLayers.GetLayer;
begin
  Result := FLayers[Index];
end;

procedure TLayers.SetLayer;
begin
  FLayers[Index] := Value;
end;

procedure TLayers.SetSelState;
begin
  if FSelState <> Value then
  begin
    if Value = stSelecting then Selection.Mask.FillTransparent;
    FSelState := Value;
  end;
end;

constructor TLayers.Create;
begin
  FLayers := TList.Create;
  Selection := TSelInfo.Create;

  Clear;
end;

destructor TLayers.Destroy;
begin
  Clear; // free all layers
  Selection.Free;
  FLayers.Free;
end;

procedure TLayers.Assign;
var
  i: integer;
  x: TLayers;
  bm: TBitmap32;

begin
  if Src is TLayers then
  begin
    x := Src as TLayers;

    Clear;
    
    FWidth := x.FWidth;
    FHeight := x.FHeight;
    for i := 0 to x.LayerCount - 1 do NewLayer.Assign(x[i]);

    FSelState := x.FSelState;
    Selection.Assign(x.Selection);
  end else
  if Src is TBitmap32 then
  begin
    bm := Src as TBitmap32;

    Clear;
    
    Resize(bm.Width, bm.Height);
    with NewLayer do
    begin
      Image.Assign(bm);
      Selected := True;
    end;

    FSelState := stNone;
  end else
    inherited;
end;

procedure TLayers.AssignTo;
var
  x: TBitmap32 absolute Dest;

begin
  if Dest is TBitmap32 then
    Render(x, ClientRect, lssAll, True) else
    inherited;
end;

procedure TLayers.Clear;
var
  i: integer;

begin
  for i := 0 to LayerCount - 1 do Layers[i].Free;
  FLayers.Clear;

  FWidth := 0;
  FHeight := 0;
  FSelState := stNone;
end;

function TLayers.HasNonTrivialLayers: boolean;
begin
  Result := not ( (LayerCount = 1) and
      Layers[0].Visible and (Layers[0].Opacity = $ff) and
      (Layers[0].BlendMode = bmNormal) );
end;

function TLayers.LayerCount;
begin
  Result := FLayers.Count;
end;

function TLayers.SelectedCount;
var
  i: integer;

begin
  Result := 0;
  for i := 0 to LayerCount - 1 do if Layers[i].Selected then inc(Result);
end;

function TLayers.VisibleCount;
var
  i: integer;

begin
  Result := 0;
  for i := 0 to LayerCount - 1 do if Layers[i].Visible then inc(Result);
end;

function TLayers.FirstSelected;
var
  i: integer;
  
begin
  for i := 0 to LayerCount - 1 do if Layers[i].Selected then
  begin
    Result := i;
    Exit;
  end;
  
  Result := -1;
end;

procedure TLayers.SelectOne;
var
  i: integer;
  
begin
  for i := 0 to LayerCount - 1 do Layers[i].Selected := (i = Index);
end;

function TLayers.NewLayerAtDepth(Depth: integer): TLayer;
begin
  Result := TLayer.Create;
  Result.Image.Resize(Width, Height);
  FLayers.Insert(Depth, Result);

  if (SelState = stFloating) and (Depth <= Selection.Depth) then
    inc(Selection.Depth);
end;

function TLayers.NewLayer: TLayer;
begin
  Result := NewLayerAtDepth(LayerCount);
end;

procedure TLayers.DeleteLayer;
begin
  Layers[Index].Free;
  FLayers.Delete(Index);

  if SelState = stFloating then
  begin
    if Index = Selection.Depth then SelState := stNone else
    if Index < Selection.Depth then dec(Selection.Depth);
  end;
end;

function TLayers.Merge;
var
  i, Count: integer;
  bm: TBitmap32;

begin
  Result := -1;
  Count := 0;
  for i := 0 to LayerCount - 1 do
    if LayerInSubset(Layers[i], Subset) then
  begin
    Result := i;
    inc(Count);
  end;

  if Count = 0 then Exit;
  if not HasNonTrivialLayers then
  begin
//    Assert(Count = 1);
    Exit; // Already flattened
  end;

  bm := TBitmap32.Create;
  try
    Render(bm, ClientRect, Subset, True);

    // overwrite bottom layer
    with Layers[Result] do
    begin
      Visible := True;
      Opacity := $ff;
      BlendMode := bmNormal;
      Image.Assign(bm);
    end;
  finally
    bm.Free;
  end;

  // delete unneeded layers
  if (SelState = stFloating) and LayerInSubset(Layers[Selection.Depth], Subset) then
    SelState := stNone;
  for i := Result - 1 downto 0 do if LayerInSubset(Layers[i], Subset) then
  begin
    DeleteLayer(i);
    dec(Result); // moved up
  end;
end;

function TLayers.ClientRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

procedure TLayers.Resize;
var
  i: integer;

begin
  FWidth := NewWidth;
  FHeight := NewHeight;

  for i := 0 to LayerCount - 1 do Layers[i].Image.Resize(NewWidth, NewHeight);

  Selection.Mask.Resize(NewWidth, NewHeight);
end;

procedure TLayers.Crop;
var
  i: integer;

begin
  for i := 0 to LayerCount - 1 do Layers[i].Image.Crop(r);

  // crop selection
  case SelState of
    stSelecting: Selection.Mask.Crop(r);

    stFloating: with Selection.Box do
    begin
      dec(Left, r.Left); dec(Right, r.Left);
      dec(Top, r.Top); dec(Bottom, r.Top);
    end;
  end;

  // preserve consistency
  Resize(r.Width, r.Height);
end;

procedure TLayers.Render;
var
  i, j, xSize, ySize: integer;
  isMasked: boolean;
  bmTemp: TBitmap32;
  DestIsClear, bmTempIsClear: boolean; // Is that bitmap fully transparent?

  procedure DrawLayer(Index: integer; bm: TBitmap32;
    Opacity: byte; BlendMode: TBlendMode; bmIsClear: PBoolean);
  var
    bmImage: TBitmap32;

  begin
    if (SelState = stFloating) and (Index = Selection.Depth) then
    begin
      bmImage := TBitmap32.Create;
      try
        bmImage.Assign(Layers[Index].Image);
        bmImage.TransformDraw(Selection.Box, Selection.Angle,
          Selection.Image, AntialiasSelection);
        DrawBlended(bm, r, bmImage, Opacity, BlendMode, bmIsClear);
      finally
        bmImage.Free;
      end;
    end else
      DrawBlended(bm, r, Layers[Index].Image, Opacity, BlendMode, bmIsClear);
  end;

begin
  r := Rect(Max(0, r.Left), Max(0, r.Top), Min(Width, r.Right), Min(Height, r.Bottom));
  xSize := Max(0, r.Width);
  ySize := Max(0, r.Height);

  if ClearScreen
    and (FSelState = stNone)
    and (LayerCount = 1)
    and LayerInSubset(Layers[0], Subset)
    and (Layers[0].BlendMode = bmNormal) and (Layers[0].Opacity = 255) and Layers[0].Visible
  then begin
    Dest.Resize(xSize, ySize);
    if (xSize > 0) and (ySize > 0) then
      Dest.CopyRectOverwrite(0, 0, Layers[0].Image, r);
    Exit;
  end;

  if ClearScreen then
  begin
    Dest.Resize(xSize, ySize);
    Dest.FillTransparent;
    DestIsClear := True;
  end;

  if (xSize = 0) or (ySize = 0) then Exit;

  bmTemp := TBitmap32.Create;
  try
    i := LayerCount - 1;
    while i >= 0 do
    begin
      if LayerInSubset(Layers[i], Subset) and
        Layers[i].Visible and (Layers[i].BlendMode <> bmMask) then
      begin
        isMasked := (i > 0) and Layers[i - 1].Visible and
          (Layers[i - 1].BlendMode = bmMask);
        if isMasked then
        // Render temporary image, if there is a mask
        begin
          bmTemp.Resize(xSize, ySize);
          bmTemp.FillTransparent;
          bmTempIsClear := True;
          DrawLayer(i, bmTemp, $ff, bmNormal, @bmTempIsClear);

          // draw masks
          j := i - 1;
          while (j >= 0) and (Layers[j].BlendMode = bmMask) do
          begin
            DrawLayer(j, bmTemp, Layers[j].Opacity, bmMask, @bmTempIsClear);
            dec(j);
          end;

          DrawBlended(Dest, bmTemp.ClientRect, bmTemp,
            Layers[i].Opacity, Layers[i].BlendMode, @DestIsClear);
          i := j;
        end else
        begin
          DrawLayer(i, Dest, Layers[i].Opacity, Layers[i].BlendMode, @DestIsClear);
          dec(i);
        end;
      end else
        dec(i);
    end;
  finally
    bmTemp.Free;
  end;
end;

function TLayers.CheckEmptySelection;
begin
  Result := not Selection.Mask.HasColor(cl32White);
  if (SelState = stSelecting) and Result then SelState := stNone;
end;

procedure TLayers.CreateFloatingSelection;
var
  i, x, y: integer;
  l: TLayer;
  pSrc, pDest: PColor32;

begin
  if (SelState <> stSelecting) or CheckEmptySelection or
    (SelectedCount = 0) then Exit;

  with Selection do
  begin
    // set bounding box and depth
    Box := Mask.GetAutoCropRect;
    Angle := 0;
    Depth := FirstSelected;

    // create Selection.Image and clear the selected pixels on the source layers
    Image.Resize(Box.Width, Box.Height);
    Image.FillTransparent;

    for i := LayerCount - 1 downto 0 do if Layers[i].Selected then
    begin
      l := Layers[i];

      pDest := Image.Data;
      for y := 0 to Image.Height - 1 do
      begin
        pSrc := l.Image.PixelAddr(Box.Left, Box.Top + y);

        for x := 0 to Image.Width - 1 do
        begin
          if Mask[Box.Left + x, Box.Top + y] then
          begin
            pDest^ := PutPixel32(pSrc^, pDest^);
            pSrc^ := cl32Transparent;
          end;

          inc(pSrc);
          inc(pDest);
        end; // for x
      end; // for y
    end; // for i
  end;

  // ok
  SelState := stFloating;
end;

procedure TLayers.FlattenFloatingSelection;
begin
  if SelState <> stFloating then Exit;

  // draw
  with Selection do Layers[Depth].Image.TransformDraw(Box, Angle, Image, AntialiasSelection);
  // ok
  SelState := stNone;
end;

procedure TLayers.ApplySelectionTransform(const FloatingSelectionMargin: TRect);
var
  NewBox, r: TRect;
  bmTemp: TBitmap32;

begin
  with Selection do
  begin
    if not IsRotated and not IsStretched and FloatingSelectionMargin.IsZero then Exit;

    NewBox := RotateRect(Box, Box.xCenter, Box.yCenter, Angle);
    NewBox.Right := Min(NewBox.Right, NewBox.Left + MaxSelImageSize);
    NewBox.Bottom := Min(NewBox.Bottom, NewBox.Top + MaxSelImageSize);
    NewBox.AddMargin(FloatingSelectionMargin);

    bmTemp := TBitmap32.Create;
    try
      bmTemp.Assign(Selection.Image);
      with Selection.Image do
      begin
        Resize(NewBox.Width, NewBox.Height);
        FillTransparent;
        r := Box;
        OffsetRect(r, -NewBox.Left, -NewBox.Top);
        TransformDraw(r, Angle, bmTemp, AntialiasSelection);
      end;
    finally
      bmTemp.Free;
    end; // try bmTemp

    Box := NewBox;
    Angle := 0;
  end; // with sel
end;

class function TLayers.AntialiasSelection;
begin
  Result := frmMain.frmToolSet.CommonToolSettings[dtTransform].Antialias;
end;

end.

