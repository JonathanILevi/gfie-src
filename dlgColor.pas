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
unit dlgColor;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DoubleBufPB, ColorSwatches, StdCtrls, PaintScrollBar,
  AdjustControl, NumberEdit, ExtCtrls, BitmapEx, bmExUtils, ComCtrls, Buttons,
  Menus, ieShared, HSBMap, Math, FileUtil, gfMath;

const
  cl32NoPreview = $00123456; // special color value

type

  { TColorFrame }

  TColorFrame = class(TFrame)
    iNextPage: TImage;
    pc: TPageControl;
    tsHSBMap: TTabSheet;
    tsSwatches: TTabSheet;
    Swatches: TColorSwatches;
    pbForeColor: TDoubleBufPB;
    pbBackColor: TDoubleBufPB;
    sbTransparent: TSpeedButton;
    sbInverted: TSpeedButton;
    sbSwapColors: TSpeedButton;
    sbDefault: TSpeedButton;
    sbSwatchLoad: TSpeedButton;
    sbSwatchSave: TSpeedButton;
    odSwatches: TOpenDialog;
    sdSwatches: TSaveDialog;
    HSBMap: THSBMap;
    pRGBA: TPanel;
    lRed: TAdjustLabel;
    lGreen: TAdjustLabel;
    lBlue: TAdjustLabel;
    lAlpha: TAdjustLabel;
    lHTML: TLabel;
    sbRed: TPaintScrollBar;
    sbGreen: TPaintScrollBar;
    sbBlue: TPaintScrollBar;
    sbAlpha: TPaintScrollBar;
    neRed: TNumberEdit;
    neGreen: TNumberEdit;
    neBlue: TNumberEdit;
    neAlpha: TNumberEdit;
    eHTML: TEdit;
    imWCP: TImage;
    procedure iNextPageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbRGBPaint(Sender: TObject; bm: TBitmap; Rect: TRect);
    procedure sbRGBAChange(Sender: TObject);
    procedure imWCPClick(Sender: TObject);
    procedure neRGBAChange(Sender: TObject);
    procedure eHTMLChange(Sender: TObject);
    procedure SwatchesMouseLeave(Sender: TObject);
    procedure SwatchesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SwatchesSelect(Sender: TObject; X, Y: Integer);
    procedure sbAlphaPaint(Sender: TObject; bm: TBitmap; Rect: TRect);
    procedure sbTransparentClick(Sender: TObject);
    procedure sbInvertedClick(Sender: TObject);
    procedure pbColorPaint(Sender: TObject);
    procedure pbColorClick(Sender: TObject);
    procedure sbSwapColorsClick(Sender: TObject);
    procedure sbDefaultClick(Sender: TObject);
    procedure SwatchesEditing(Sender: TObject; X, Y: Integer);
    procedure sbSwatchLoadClick(Sender: TObject);
    procedure sbSwatchSaveClick(Sender: TObject);
    procedure HSBMapChange(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FActiveColor: integer;
    FSelColor: array[0..1] of TColor32;
    FPreviewColor: TColor32;

    // aliases of sbRed, sbGreen, ... components
    sbComp: array[0..3] of TPaintScrollBar;
    neComp: array[0..3] of TNumberEdit;

    procedure SetActiveColor(const Value: integer);
    function GetSelColor(Index: integer): TColor32;
    procedure SetSelColor(Index: integer; const Value: TColor32);
    procedure SetPreviewColor(const Value: TColor32);
  public
    // If nonzero, the form is being updated
    frmUpdating: integer;
    // Can be set to False to prevent controls from updating their values
    UpdateHTML: boolean;
    UpdateHSBMap: boolean;

    constructor Create(AOwner: TComponent); override;
	
    procedure UpdateForm;
    procedure LoadSwatches(const fn: string);
    procedure SaveSwatches(const fn: string);

    // which color is being edited?
    // 0: foreground
    // 1: background
    property ActiveColor: integer read FActiveColor write SetActiveColor;
    // selected color (which is contained by the controls)
    property SelColor[Index: integer]: TColor32 read GetSelColor write SetSelColor;
    // this color is shown as a preview in the active color shape
    property PreviewColor: TColor32 read FPreviewColor write SetPreviewColor;
  end;

implementation

uses Main;

{$R *.lfm}

procedure TColorFrame.SetActiveColor(const Value: integer);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    UpdateForm;
  end;
end;

function TColorFrame.GetSelColor(Index: integer): TColor32;
begin
  Result := FSelColor[Index];
end;

procedure TColorFrame.SetSelColor(Index: integer; const Value: TColor32);
begin
  if FSelColor[Index] <> Value then
  begin
    FSelColor[Index] := Value;
    UpdateForm;
  end;
end;

procedure TColorFrame.SetPreviewColor(const Value: TColor32);
begin
  if FPreviewColor <> Value then
  begin
    FPreviewColor := Value;
    if ActiveColor = 0 then
      pbForeColor.Repaint else
      pbBackColor.Repaint;
  end;
end;

procedure TColorFrame.UpdateForm;
var
  c: TColor32;
  i: integer;

begin
  inc(frmUpdating);
    // paintboxes
    pbForeColor.Repaint;
    pbBackColor.Repaint;
    // components
    c := SelColor[ActiveColor];
    for i := 0 to 3 do
    begin
      sbComp[i].Position := c and $ff;
      sbComp[i].Repaint;
      neComp[i].Value := c and $ff;
      c := c shr 8;
    end;
    // html
    if UpdateHTML then eHTML.Text := ColorToHTML(SelColor[ActiveColor] and $ffffff);
    if UpdateHSBMap then HSBMap.SelColor := SelColor[ActiveColor] and $ffffff;
  dec(frmUpdating);
end;

procedure TColorFrame.LoadSwatches(const fn: string);
var
  f: TextFile;
  i, j: integer;
  s: string;
  c: TColor;

begin
  AssignFile(f, fn);
  Reset(f);
    for j := 0 to Swatches.Rows - 1 do
    for i := 0 to Swatches.Cols - 1 do
    begin
      Readln(f, s);
      if not TryStrToInt(s, Integer(c)) then c := clWhite;
      Swatches.Colors[i, j] := c;
    end;
  CloseFile(f);
end;

procedure TColorFrame.SaveSwatches(const fn: string);
var
  f: TextFile;
  i, j: integer;

begin
  AssignFile(f, fn);
  Rewrite(f);
    for j := 0 to Swatches.Rows - 1 do for i := 0 to Swatches.Cols - 1 do
      Writeln(f, '$' + IntToHex(Swatches.Colors[i, j], 6));
  CloseFile(f);
end;

constructor TColorFrame.Create(AOwner: TComponent);
var
  s: string;
  cr: TCursor;

begin
  inherited;
  
  // load tool button glyphs
  GetMiscGlyph(sbSwapColors.Glyph, mgSwap);
  GetMiscGlyph(sbDefault.Glyph, mgBW);
  GetMiscGlyph(sbTransparent.Glyph, mgTransparent);
  GetMiscGlyph(sbInverted.Glyph, mgInverted);
  GetMiscGlyph(sbSwatchLoad.Glyph, mgOpen);
  GetMiscGlyph(sbSwatchSave.Glyph, mgSave);

  // set cursors
  cr := ieCursorBase + Ord(iecEyedropper);
  sbRed.Cursor := cr; sbGreen.Cursor := cr;
  sbBlue.Cursor := cr; sbAlpha.Cursor := cr;
  HSBMap.Cursor := cr; Swatches.Cursor := cr;

  sbComp[0] := sbRed; sbComp[1] := sbGreen;
  sbComp[2] := sbBlue; sbComp[3] := sbAlpha;
  neComp[0] := neRed; neComp[1] := neGreen;
  neComp[2] := neBlue; neComp[3] := neAlpha;

  frmUpdating := 0;
  UpdateHTML := True;
  UpdateHSBMap := True;

  FSelColor[0] := cl32Black;
  FSelColor[1] := cl32White;
  FActiveColor := 0;
  FPreviewColor := cl32NoPreview;
  UpdateForm;

  s := AppDir + 'Swatches'+DirectorySeparator+'Default.swa';
  if FileExists(s) then LoadSwatches(s);
end;

procedure TColorFrame.sbRGBPaint(Sender: TObject; bm: TBitmap; Rect: TRect);
var
  sb: TPaintScrollBar;
  Bit, i: integer;
  c: TColor;

begin
  sb := Sender as TPaintScrollBar;
  Bit := sb.Tag;
  // disable alpha and the current channel
  c := SelColor[ActiveColor] and $ffffff;

  for i := Rect.Left to Rect.Right - 1 do
    with bm.Canvas do
  begin
    Pen.Color := c and not ($ff shl Bit) or ((i - Rect.Left)*$ff div
      (Rect.Width - 1) shl Bit);
    MoveTo(i, Rect.Top);
    LineTo(i, Rect.Bottom);
  end;
end;

procedure TColorFrame.iNextPageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pc.ActivePageIndex := (pc.ActivePageIndex + 1) mod pc.PageCount;
end;

procedure TColorFrame.sbRGBAChange(Sender: TObject);
var
  sb: TPaintScrollBar;

begin
  if frmUpdating = 0 then
  begin
    sb := Sender as TPaintScrollBar;
    SelColor[ActiveColor] := SelColor[ActiveColor]
      and not ($ff shl sb.Tag) or (Byte(sb.Position) shl sb.Tag);
  end;
end;

procedure TColorFrame.imWCPClick(Sender: TObject);
begin
  SelColor[ActiveColor] := TColor32(GFIEPickColor(SelColor[ActiveColor] and $ffffff))
    or cl32Opaque;
end;

procedure TColorFrame.neRGBAChange(Sender: TObject);
var
  ne: TNumberEdit;

begin   
  if frmUpdating = 0 then
  begin
    ne := Sender as TNumberEdit;
    SelColor[ActiveColor] := SelColor[ActiveColor]
      and not ($ff shl ne.Tag) or (Round(ne.Value) shl ne.Tag);
  end;
end;

procedure TColorFrame.eHTMLChange(Sender: TObject);
begin
  if frmUpdating = 0 then
  begin
    UpdateHTML := False;
    try
      SelColor[ActiveColor] := TColor32(HTMLToColor(eHTML.Text)) or cl32Opaque;
    finally
      UpdateHTML := True;
    end;
  end;
end;

procedure TColorFrame.SwatchesMouseLeave(Sender: TObject);
begin
  PreviewColor := cl32NoPreview;
end;

procedure TColorFrame.SwatchesMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  pt: TPoint;

begin
  ControlMouseMove(Sender, Shift, X, Y);
  pt := Swatches.ItemAtPos(X, Y);
  PreviewColor := TColor32(Swatches.Colors[pt.X, pt.Y]) or cl32Opaque;
end;

procedure TColorFrame.SwatchesSelect(Sender: TObject; X, Y: Integer);
begin
  SelColor[ActiveColor] := TColor32(Swatches.Colors[x, y]) or cl32Opaque;
end;

procedure TColorFrame.sbAlphaPaint(Sender: TObject; bm: TBitmap;
  Rect: TRect);
var
  bx: TBitmap32;
  c: TColor32;
  x, y: integer;

begin
  // create a 32-bit bitmap
  bx := TBitmap32.Create;
  try
    bx.Resize(Rect.Width, Rect.Height);
    for x := 0 to bx.Width - 1 do
    begin
      c := (SelColor[ActiveColor] and $ffffff) or
        (Byte(x * $ff div (bx.Width - 1)) shl 24);
        
      for y := 0 to bx.Height - 1 do bx.PixelAddr(x, y)^ := c;
    end;

    bx.DrawToCanvas(bm.Canvas, Rect.Left, Rect.Top, Pref_Hatch);
  finally
    bx.Free;
  end;
end;

procedure TColorFrame.sbTransparentClick(Sender: TObject);
begin
  SelColor[ActiveColor] := cl32Transparent;
end;

procedure TColorFrame.sbInvertedClick(Sender: TObject);
begin
  SelColor[ActiveColor] := cl32Inverted;
end;

procedure TColorFrame.pbColorPaint(Sender: TObject);
const
  Edge = 4;
  
var
  pb: TDoubleBufPB;
  bx: TBitmap32;
  ColorToShow: TColor32;

begin
  pb := Sender as TDoubleBufPB;

  with pb.Canvas do
  begin
    ColorToShow := SelColor[pb.Tag];
    if ActiveColor = pb.Tag then
    begin
      Pen.Color := $0000a0;
      Brush.Color := $89c1fd;
      Rectangle(pb.ClientRect);

      if PreviewColor <> cl32NoPreview then
        ColorToShow := PreviewColor;
    end;

    bx := TBitmap32.Create;
    try
      bx.Resize(pb.Width - 2*Edge, pb.Height - 2*Edge);
      bx.FillColor(ColorToShow);

      bx.DrawToCanvas(pb.Canvas, Edge, Edge, Pref_Hatch);
    finally
      bx.Free;
    end;

    Brush.Color := clBlack;
    FrameRect(Rect(Edge, Edge, pb.Width - Edge, pb.Height - Edge));
  end;
end;

procedure TColorFrame.pbColorClick(Sender: TObject);
begin
  ActiveColor := (Sender as TDoubleBufPB).Tag;
end;

procedure TColorFrame.sbSwapColorsClick(Sender: TObject);
var
  tmp: TColor32;

begin
  tmp := SelColor[ActiveColor];
  SelColor[ActiveColor] := SelColor[1 - ActiveColor];
  SelColor[1 - ActiveColor] := tmp;
end;

procedure TColorFrame.sbDefaultClick(Sender: TObject);
begin
  SelColor[0] := cl32Black;
  SelColor[1] := cl32White;
end;

procedure TColorFrame.SwatchesEditing(Sender: TObject; X, Y: Integer);
begin
  Swatches.Colors[x, y] := SelColor[ActiveColor] and $ffffff;
end;

procedure TColorFrame.sbSwatchLoadClick(Sender: TObject);
begin
  odSwatches.InitialDir := AppDir + 'Swatches';
  if odSwatches.Execute then LoadSwatches(UTF8ToSys(odSwatches.FileName));
end;

procedure TColorFrame.sbSwatchSaveClick(Sender: TObject);
begin
  sdSwatches.InitialDir := AppDir + 'Swatches';
  if sdExecuteWithCanClose(sdSwatches) then
    SaveSwatches(UTF8ToSys(sdSwatches.FileName));
end;

procedure TColorFrame.HSBMapChange(Sender: TObject);
begin
  if frmUpdating = 0 then
  begin
    UpdateHSBMap := False;
    try
      SelColor[ActiveColor] := TColor32(HSBMap.SelColor) or cl32Opaque;
    finally
      UpdateHSBMap := True;
    end;
  end;
end;            

procedure TColorFrame.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  frmMain.SetStatus('');
end;

procedure TColorFrame.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  frmMain.SetStatus((Sender as TControl).Hint);
end;

end.
