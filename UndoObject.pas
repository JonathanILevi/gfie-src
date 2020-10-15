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
unit UndoObject;

interface

uses
  LclIntf, LclType,
  SysUtils, Classes, Graphics, BitmapEx, bmExUtils, Layers, DocClass, ieShared,
  Forms, ExtCtrls, ComCtrls;

const
  MaxUndo = 32;

type
  TDocumentFrame = class;

  TUndoObject = class(TPersistent)
  public
    Caption: string;

    procedure Perform(frm: TDocumentFrame); virtual;
    // This will return a redo object if Self was an undo object
    // So calling Self.Perform and Inverted.Perform will result in no change
    function Invert(frm: TDocumentFrame): TUndoObject; virtual;
  end;

  TDeletePageUndo = class(TUndoObject)
  protected
    FPageIndex: integer;
    PageData: TDocPage;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex: integer);
    destructor Destroy; override;

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  TFrameRatesUndo = class(TUndoObject)
  protected
    a: TArrayOfInteger;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  THotSpotUndo = class(TUndoObject)
  protected
    FPageIndex: integer;
    FHotSpot: TPoint;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex: integer);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  TLayerOrderUndo = class(TUndoObject)
  protected
    FPageIndex: integer;

    SelDepth: integer; // selection may be moved as well
  public
    Permutation: TArrayOfInteger;

    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex: integer);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  TLayerPropUndo = class(TUndoObject)
  protected
    FPageIndex, FLayerIndex: integer;
    lp: TLayerProp;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex, LayerIndex: integer);
    destructor Destroy; override;

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  TInsertPageUndo = class(TUndoObject)
  protected
    FPageIndex: integer;
  public
    constructor Create(const ACaption: string; PageIndex: integer);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  TMovePageUndo = class(TUndoObject)
  protected
    FIndexFrom, FIndexTo: integer;
  public
    constructor Create(const ACaption: string; IndexFrom, IndexTo: integer);

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  // -- Can store a layer index and (optionally) layer data
  TLayerSave = class
  private
    FLayerSaved: boolean;
    FLayer: TLayer;

    procedure SetLayerSaved(const Value: boolean);
  public
    Index: integer;

    constructor Create;
    destructor Destroy; override;

    property LayerSaved: boolean read FLayerSaved write SetLayerSaved;
    property Layer: TLayer read FLayer;
  end;

  // This undo object encodes modifications
  // which were done to the layers and selection of a single page
  // and, optionally, to the frame rate

  // Call the LayerChanged/~Deleted/etc. functions
  // in the following order:
  // - changed
  // - inserted
  // - deleted
  // and specify changed/deleted/etc. layer indexes in descending order.

  // Call SelectionChanged if the selection state, Image or Mask was changed
  // Box, Angle and Depth are automatically saved and restored

  // Always create the undo object before operations, e.g. insertion and deletion

  TPageChangeUndo = class(TUndoObject)
  protected
    FDoc: TIconDoc;
    FPageIndex: integer;

    Layers: TList; // list of TLayerSave

    // selection info
    SelSaved: boolean; // whether State, Image and Mask should be used
    SelState: TSelState;
    SelImage: TBitmap32; // can be nil if no snapshot
    SelMask: TBitmap1; // can be nil if no snapshot
    SelBox: TRect; // always saved
    SelAngle: double; // always saved
    SelDepth: integer; // always saved

    // other info
    FrameRate: integer;
    DPI: double;

    function GetLayer(Index: integer): TLayerSave;
    function PageLayers: TLayers;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex: integer);
    destructor Destroy; override;
    procedure Clear;

    procedure SelectionChanged;
    
    procedure LayerChanged(AIndex: integer);
    procedure LayerDeleted(AIndex: integer);
    procedure LayerInserted(AIndex: integer);

    procedure FrameRateOrDPIChanged;

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  // Use this undo object when multiple pages are completely modified
  // e.g. when resizing multiple pages at once
  TMultiPageUndo = class(TUndoObject)
  protected
    // This icon document contains only the modified pages
    FDoc: TIconDoc;
    // Which pages should be overwritten by the pages of FDoc
    FPageIndex: TArrayOfInteger;
  public
    constructor Create(const ACaption: string; frm: TDocumentFrame; PageIndex: TArrayOfInteger);
    destructor Destroy; override;

    procedure Perform(frm: TDocumentFrame); override;
    function Invert(frm: TDocumentFrame): TUndoObject; override;
  end;

  { TDocumentFrame }

  TDocumentFrame = class(TFrame)
  private
    FFileName: string;
    FModified: boolean;
    FTabCaption: string;
    procedure SetFileName(const Value: string);
    procedure SetModified(Value: boolean);
    procedure SetTabCaption(AValue: string);
  protected
    procedure UpdateCaption; virtual; abstract;
    property TabCaption: string read FTabCaption write SetTabCaption;
  public
    // Undo and Redo stacks
    UndoStack, RedoStack: TList;
    Tab: TTabSheet; // TDocumentTab

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Gets called after Tab has been assigned
    procedure DocFrameTabAssigned; virtual;
    procedure DocFrameActivate; virtual;
    function DocFrameCanClose: boolean; virtual;
    procedure RealSetTabCaption;

    procedure PushUndoRedo(List: TList; uo: TUndoObject);
    procedure AddUndo(uo: TUndoObject);
    function CanUndo: boolean;
    function CanRedo: boolean;
    procedure PerformUndoRedo(Undo: boolean); virtual;
    procedure PurgeUndoRedo(Stack: TList);

    property FileName: string read FFileName write SetFileName;
    property Modified: boolean read FModified write SetModified;
  end;

  TDocumentFrameClass = class of TDocumentFrame;

  TDocumentTab = class(TTabSheet)
  public
    Frame: TDocumentFrame;
    constructor CreateTab(AOwner: TComponent; FrameClass: TDocumentFrameClass);
  end;

implementation

uses
  dlgDoc;

procedure TUndoObject.Perform(frm: TDocumentFrame);
begin
  // do nothing
end;

function TUndoObject.Invert(frm: TDocumentFrame): TUndoObject;
begin
  Result := TUndoObject.Create;
end;

// TDeletePageUndo

constructor TDeletePageUndo.Create;
begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := PageIndex;
  PageData := TDocPage.Create;
  PageData.Assign(TGraphicFrame(frm).Doc.Pages[FPageIndex]);
end;

destructor TDeletePageUndo.Destroy;
begin
  PageData.Free;

  inherited;
end;

procedure TDeletePageUndo.Perform;
begin
  TGraphicFrame(frm).Doc.InsertPage(FPageIndex).Assign(PageData);
end;

function TDeletePageUndo.Invert;
begin
  Result := TInsertPageUndo.Create(Caption, FPageIndex);
end;

// TLayerOrderUndo

constructor TLayerOrderUndo.Create;
var
  i: integer;
  ls: TLayers;

begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := PageIndex;

  ls := TGraphicFrame(frm).Doc.Pages[FPageIndex].Layers;
  if ls.SelState = stFloating then SelDepth := ls.Selection.Depth;
  SetLength(Permutation, ls.LayerCount);
  for i := 0 to Length(Permutation) - 1 do Permutation[i] := i;
end;

procedure TLayerOrderUndo.Perform;
var
  i: integer;
  tmp: array of TLayer;
  
begin
  with TGraphicFrame(frm).Doc.Pages[FPageIndex].Layers do
  begin
    SetLength(tmp, LayerCount);
    for i := 0 to LayerCount - 1 do tmp[i] := Layers[i];
    for i := 0 to LayerCount - 1 do Layers[Permutation[i]] := tmp[i];

    if SelState = stFloating then Selection.Depth := SelDepth;
  end;
end;

function TLayerOrderUndo.Invert;
var
  i: integer;
  lou: TLayerOrderUndo;
  
begin
  lou := TLayerOrderUndo.Create(Caption, frm, FPageIndex);
  // simply invert the permutation
  for i := 0 to Length(Permutation) - 1 do lou.Permutation[Permutation[i]] := i;

  Result := lou;
end;

// TFrameRatesUndo

constructor TFrameRatesUndo.Create;
var
  i: integer;

begin
  inherited Create;

  Caption := ACaption;

  SetLength(a, TGraphicFrame(frm).Doc.PageCount);
  for i := 0 to TGraphicFrame(frm).Doc.PageCount - 1 do
    a[i] := TGraphicFrame(frm).Doc.Pages[i].FrameRate;
end;

procedure TFrameRatesUndo.Perform;
var
  i: integer;

begin
  with TGraphicFrame(frm) do
    for i := 0 to Doc.PageCount - 1 do
      Doc.Pages[i].FrameRate := a[i];
end;

function TFrameRatesUndo.Invert;
begin
  Result := TFrameRatesUndo.Create(Caption, frm);
end;

// THotSpotUndo

constructor THotSpotUndo.Create;
begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := PageIndex;
  FHotSpot := TGraphicFrame(frm).Doc.Pages[PageIndex].HotSpot;
end;

procedure THotSpotUndo.Perform;
begin
  TGraphicFrame(frm).Doc.Pages[FPageIndex].HotSpot := FHotSpot;
end;

function THotSpotUndo.Invert;
begin
  Result := THotSpotUndo.Create(Caption, frm, FPageIndex);
end;

// TLayerPropUndo

constructor TLayerPropUndo.Create;
begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := PageIndex;
  FLayerIndex := LayerIndex;
  
  lp := TLayerProp.Create;
  lp.Read(TGraphicFrame(frm).Doc.Pages[PageIndex].Layers[LayerIndex]);
end;

destructor TLayerPropUndo.Destroy;
begin
  lp.Free;
  inherited;
end;

procedure TLayerPropUndo.Perform;
begin
  lp.Write(TGraphicFrame(frm).Doc.Pages[FPageIndex].Layers[FLayerIndex]);
end;

function TLayerPropUndo.Invert;
begin
  Result := TLayerPropUndo.Create(Caption, frm, FPageIndex, FLayerIndex);
end;

// TInsertPageUndo

constructor TInsertPageUndo.Create;
begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := PageIndex;
end;

procedure TInsertPageUndo.Perform;
begin
  TGraphicFrame(frm).Doc.DeletePage(FPageIndex);
end;

function TInsertPageUndo.Invert;
begin
  Result := TDeletePageUndo.Create(Caption, frm, FPageIndex);
end;

// TMovePageUndo

constructor TMovePageUndo.Create;
begin
  inherited Create;

  Caption := ACaption;
  FIndexFrom := IndexFrom;
  FIndexTo := IndexTo;
end;

procedure TMovePageUndo.Perform;
begin
  TGraphicFrame(frm).Doc.MovePage(FIndexTo, FIndexFrom);
end;

function TMovePageUndo.Invert;
begin
  Result := TMovePageUndo.Create(Caption, FIndexTo, FIndexFrom);
end;

// TLayerSave

procedure TLayerSave.SetLayerSaved;
begin
  if FLayerSaved <> Value then
  begin
    FLayerSaved := Value;
    if Value then FLayer := TLayer.Create else FLayer.Free;
  end;
end;

constructor TLayerSave.Create;
begin
  Index := 0;

  FLayerSaved := False;
  FLayer := nil;
end;

destructor TLayerSave.Destroy;
begin
  if Assigned(FLayer) then FLayer.Free;

  inherited;
end;

// TPageChangeUndo

function TPageChangeUndo.GetLayer;
begin
  Result := Layers[Index];
end;

function TPageChangeUndo.PageLayers;
begin
  Result := FDoc.Pages[FPageIndex].Layers;
end;

constructor TPageChangeUndo.Create;
var
  i: integer;
  ls: TLayerSave;

begin
  inherited Create;

  Caption := ACaption;
  FDoc := TGraphicFrame(frm).Doc;
  FPageIndex := PageIndex;

  Layers := TList.Create;

  SelSaved := False;
  SelImage := nil;
  SelMask := nil;
  with PageLayers.Selection do
  begin
    SelBox := Box;
    SelAngle := Angle;
    SelDepth := Depth;
  end;

  FrameRate := -1;
  DPI := -1;

  // default
  for i := 0 to PageLayers.LayerCount - 1 do
  begin
    ls := TLayerSave.Create;
    ls.Index := i;
    Layers.Add(ls);
  end;
end;

destructor TPageChangeUndo.Destroy;
begin
  Clear;
  Layers.Free;

  inherited;
end;

procedure TPageChangeUndo.Clear;
var
  i: integer;

begin
  for i := 0 to Layers.Count - 1 do GetLayer(i).Free;
  Layers.Clear;

  SelSaved := False;
  if Assigned(SelImage) then FreeAndNil(SelImage);
  if Assigned(SelMask) then FreeAndNil(SelMask);

  FrameRate := -1;
  DPI := -1;
end;

procedure TPageChangeUndo.SelectionChanged;
begin
  if not SelSaved then
  begin
    SelSaved := True;
    SelState := PageLayers.SelState;

    if SelState = stFloating then
    begin
      if not Assigned(SelImage) then SelImage := TBitmap32.Create;
      SelImage.Assign(PageLayers.Selection.Image);
    end else
    if SelState = stSelecting then
    begin
      if not Assigned(SelMask) then SelMask := TBitmap1.Create;
      SelMask.Assign(PageLayers.Selection.Mask);
    end;
  end;
end;

procedure TPageChangeUndo.LayerChanged;
var
  i: integer;

begin
  for i := 0 to Layers.Count - 1 do with GetLayer(i) do
    if Index = AIndex then
  begin
    if not LayerSaved then
    begin
      LayerSaved := True;
      Layer.Assign(PageLayers[Index]);
    end;
    
    Break;
  end;
end;

procedure TPageChangeUndo.LayerDeleted;
var
  i, j: integer;

begin
  for i := 0 to Layers.Count - 1 do with GetLayer(i) do
    if Index = AIndex then
  begin
    if not LayerSaved then
    begin
      LayerSaved := True;
      Layer.Assign(PageLayers[Index]);

      // shift all following indexes
      for j := i + 1 to Layers.Count - 1 do dec(GetLayer(j).Index);
    end;

    Break;
  end;
end;

procedure TPageChangeUndo.LayerInserted;
var
  i: integer;

begin
  for i := 0 to Layers.Count - 1 do with GetLayer(i) do
    if Index >= AIndex then inc(Index);
end;

procedure TPageChangeUndo.FrameRateOrDPIChanged;
begin
  FrameRate := FDoc.Pages[FPageIndex].FrameRate;
  DPI := FDoc.Pages[FPageIndex].DPI;
end;

procedure TPageChangeUndo.Perform;
var
  i: integer;
  lsOrig, ls: TLayers;
  l: TLayer;

begin
  lsOrig := TGraphicFrame(frm).Doc.Pages[FPageIndex].Layers;

  ls := TLayers.Create;
  try
    // perform on layers
    if Layers.Count = 0 then
      ls.Resize(lsOrig.Width, lsOrig.Height) else
    begin
      for i := 0 to Layers.Count - 1 do
      begin
        with GetLayer(i) do
          if LayerSaved then
            l := Layer else
            l := lsOrig[Index];

        ls.NewLayer.Assign(l);
      end; // for i in Layers

      // set correct dimension fields
      with ls.Layers[0].Image do ls.Resize(Width, Height);
    end;

    // perform on selection
    if SelSaved then
    begin
      ls.SelState := SelState;
      case SelState of
        stSelecting: ls.Selection.Mask.Assign(SelMask);
        stFloating: ls.Selection.Image.Assign(SelImage);
      end;
    end else
    begin
      ls.SelState := lsOrig.SelState;
      ls.Selection.Assign(lsOrig.Selection);
    end;

    with ls.Selection do
    begin
      Box := SelBox;
      Angle := SelAngle;
      Depth := SelDepth;
    end;

    // re-assign
    lsOrig.Assign(ls);
  finally
    ls.Free;
  end;

  // other fields
  if FrameRate >= 0 then TGraphicFrame(frm).Doc.Pages[FPageIndex].FrameRate := FrameRate;
  if DPI >= 0 then TGraphicFrame(frm).Doc.Pages[FPageIndex].DPI := DPI;
end;

function TPageChangeUndo.Invert;
var
  i, j: integer;
  Found: boolean;
  lsOrig: TLayers;
  lu: TPageChangeUndo;
  lso: TLayerSave;

begin
  lu := TPageChangeUndo.Create(Caption, frm, FPageIndex);

  lu.Clear;

  lsOrig := TGraphicFrame(frm).Doc.Pages[FPageIndex].Layers;
  for i := 0 to lsOrig.LayerCount - 1 do
  begin
    lso := TLayerSave.Create;

    // search
    Found := False;
    for j := 0 to Layers.Count - 1 do with GetLayer(j) do
      if (Index = i) and not LayerSaved then
    begin
      lso.Index := j;
      Found := True;
      Break;
    end;

    // save layer
    if not Found then
    begin
      lso.LayerSaved := True;
      lso.Layer.Assign(lsOrig[i]);
    end;

    lu.Layers.Add(lso);
  end;

  // selection
  if SelSaved then lu.SelectionChanged;

  // other
  if (FrameRate >= 0) or (DPI >= 0) then lu.FrameRateOrDPIChanged;

  Result := lu;
end;

// TMultiPageUndo

constructor TMultiPageUndo.Create;
var
  i: integer;

begin
  inherited Create;

  Caption := ACaption;
  FPageIndex := Copy(PageIndex);

  // save pages
  FDoc := TIconDoc.Create;
  for i := 0 to Length(FPageIndex) do
    FDoc.NewPage.Assign(TGraphicFrame(frm).Doc.Pages[FPageIndex[i]]);
end;

destructor TMultiPageUndo.Destroy;
begin
  FDoc.Free;
  inherited;
end;

procedure TMultiPageUndo.Perform;
var
  i: integer;

begin
  for i := 0 to Length(FPageIndex) - 1 do
    TGraphicFrame(frm).Doc.Pages[FPageIndex[i]].Assign(FDoc.Pages[i]);
end;

function TMultiPageUndo.Invert;
begin
  Result := TMultiPageUndo.Create(Caption, frm, FPageIndex);
end;

// TDocumentFrame

procedure TDocumentFrame.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    UpdateCaption;
  end;
end;

procedure TDocumentFrame.SetModified(Value: boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    UpdateCaption;
  end;
end;

procedure TDocumentFrame.SetTabCaption(AValue: string);
begin
  FTabCaption := AValue;
  RealSetTabCaption;
end;

constructor TDocumentFrame.Create(AOwner: TComponent);
begin
  inherited;
  UndoStack := TList.Create;
  RedoStack := TList.Create;
end;

destructor TDocumentFrame.Destroy;
begin
  PurgeUndoRedo(UndoStack);
  FreeAndNil(UndoStack);
  PurgeUndoRedo(RedoStack);
  FreeAndNil(RedoStack);
  inherited;
end;

procedure TDocumentFrame.DocFrameTabAssigned;
begin
  // Do nothing
end;

procedure TDocumentFrame.DocFrameActivate;
begin
  // Do nothing
end;

function TDocumentFrame.DocFrameCanClose: boolean;
begin
  Result := True;
end;

procedure TDocumentFrame.RealSetTabCaption;
begin
  try
    if (Tab <> nil) and (Tab.Caption <> TabCaption) then
    begin
      Tab.Caption := TabCaption;
      Tab.Repaint;
    end;
  except
  end;
end;

procedure TDocumentFrame.PushUndoRedo(List: TList; uo: TUndoObject);
begin
  with List do
  begin
    Insert(0, uo);
    if Count > MaxUndo then
    begin
      TUndoObject(List[Count - 1]).Free;
      Delete(Count - 1);
    end;
  end;
end;

procedure TDocumentFrame.AddUndo(uo: TUndoObject);
begin
  // clear the redo stack
  PurgeUndoRedo(RedoStack);

  // add the object to the undo stack
  PushUndoRedo(UndoStack, uo);
end;

function TDocumentFrame.CanUndo;
begin
  Result := Assigned(UndoStack) and (UndoStack.Count <> 0);
end;

function TDocumentFrame.CanRedo;
begin
  Result := Assigned(RedoStack) and (RedoStack.Count <> 0);
end;

procedure TDocumentFrame.PerformUndoRedo(Undo: boolean);
var
  uo, ro: TUndoObject;
  ListFrom, ListTo: TList;

begin
  if Undo then
  begin
    ListFrom := UndoStack;
    ListTo := RedoStack;
  end
  else
  begin
    ListFrom := RedoStack;
    ListTo := UndoStack;
  end;
  if ListFrom.Count = 0 then
    Exit;

  Modified := True;

  uo := ListFrom[0];
  ListFrom.Delete(0);

  // We need to add a snapshot to the other list
  ro := uo.Invert(Self);
  PushUndoRedo(ListTo, ro);

  uo.Perform(Self);
  uo.Free;
end;

procedure TDocumentFrame.PurgeUndoRedo(Stack: TList);
var
  i: integer;

begin
  for i := 0 to Stack.Count - 1 do
    TUndoObject(Stack[i]).Free;
  Stack.Clear;
end;

// TDocumentTab

constructor TDocumentTab.CreateTab(AOwner: TComponent; FrameClass: TDocumentFrameClass);
begin
  inherited Create(AOwner);
  Frame := FrameClass.Create(Self);
  Frame.Parent := Self;
  Frame.Tab := Self;
  Frame.DocFrameTabAssigned;
end;

end.

