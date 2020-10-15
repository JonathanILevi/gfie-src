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
unit dlgToolSet;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, NumberEdit, Math,
  AdjustControl, BitmapEx, ieShared, IniFiles, LangPack, Types, gfMath;

type
  TGradientMode = (gmColor, gmTransparency);

  TToolSetPanel = (tspNothing,
    tspAntialias, tspBrushTip, tspLineWidth,
    tspTolerance, tspContiguous, tspSampleAllLayers,
    tspShapeFill, tspEyedropperBack,
    tspRetouchMode, tspEraserAlpha, tspGradient,
    tspPattern);

// Patterns for drawing
const
  PatternSize = 2; // Should be a power of two

type
  TDrawPattern = (dpFull, dpEven, dpOdd,
    dpEvenRow, dpOddRow, dpEvenCol, dpOddCol, dpSparse);
  TDrawPatternBitmap = array[0..PatternSize-1, 0..PatternSize-1] of boolean;

const
  DrawPatternBitmap: array[TDrawPattern] of TDrawPatternBitmap =
  (((True, True), (True, True)),
  ((True, False), (False, True)),
  ((False, True), (True, False)),
  ((True, True), (False, False)),
  ((False, False), (True, True)),
  ((True, False), (True, False)),
  ((False, True), (False, True)),
  ((True, False), (False, False)));

type
  { TToolSetFrame }

  TCommonToolSettings = record
    Antialias: boolean;
    Pattern: TDrawPattern;
    SampleAllLayers: boolean;
  end;
  TCommonToolSettingsArray = array[TDrawTool] of TCommonToolSettings;

  TToolSetFrame = class(TFrame)
    cbPattern: TComboBox;
    pPattern: TPanel;
    pContiguous: TPanel;
    pSampleAllLayers: TPanel;
    pGradient: TPanel;
    sbContiguous: TSpeedButton;
    sbSampleAllLayers: TSpeedButton;
    sbLinear: TSpeedButton;
    sbRadial: TSpeedButton;
    sbConical: TSpeedButton;
    sbRepNone: TSpeedButton;
    sbRepSym: TSpeedButton;
    sbRepAsym: TSpeedButton;
    sbSpiral: TSpeedButton;
    sbColor: TSpeedButton;
    sbTransparency: TSpeedButton;
    pShapeFill: TPanel;
    pLineWidth: TPanel;
    pBrushTip: TPanel;
    pTolerance: TPanel;
    sbFramed: TSpeedButton;
    sbFilled: TSpeedButton;
    aiLineWidth: TAdjustImage;
    neLineWidth: TNumberEdit;
    aiBrushSize: TAdjustImage;
    neBrushSize: TNumberEdit;
    cbBrushShape: TComboBox;
    pRetouchMode: TPanel;
    pEraserAlpha: TPanel;
    aiTolerance: TAdjustImage;
    neTolerance: TNumberEdit;
    cbRetouchMode: TComboBox;
    pEyedropperBack: TPanel;
    aiEraserAlpha: TAdjustImage;
    neEraserAlpha: TNumberEdit;
    sbEyedropperBack: TSpeedButton;
    pNothing: TPanel;
    pAntialias: TPanel;
    sbAntialias: TSpeedButton;
    procedure cbGlyphDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbPatternChange(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ControlMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sbAntialiasClick(Sender: TObject);
    procedure sbSampleAllLayersClick(Sender: TObject);
  protected
    ToolSetPanels: array[TToolSetPanel] of TPanel;

    function GetFilledShapes: boolean;
    procedure SetFilledShapes(const Value: boolean);
    function GetGradientKind: TGradientKind;
    procedure SetGradientKind(const Value: TGradientKind);
    function GetGradientRep: TGradientRep;
    procedure SetGradientRep(const Value: TGradientRep);
    function GetGradientMode: TGradientMode;
    procedure SetGradientMode(const Value: TGradientMode);
  public
    CommonToolSettings: TCommonToolSettingsArray;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateLayout(dt: TDrawTool);
    function BrushShape: TBrushShape;

    property FilledShapes: boolean read GetFilledShapes write SetFilledShapes;
    property GradientKind: TGradientKind read GetGradientKind write SetGradientKind;
    property GradientRep: TGradientRep read GetGradientRep write SetGradientRep;
    property GradientMode: TGradientMode read GetGradientMode write SetGradientMode;
  end;

implementation

uses Main;

{$R *.lfm}

function TToolSetFrame.GetFilledShapes: boolean;
begin
  Result := sbFilled.Down;
end;

procedure TToolSetFrame.SetFilledShapes(const Value: boolean);
begin
  if Value then sbFilled.Down := True else sbFramed.Down := True;
end;

function TToolSetFrame.GetGradientKind: TGradientKind;
begin
  if sbLinear.Down then Result := gkLinear else
  if sbRadial.Down then Result := gkRadial else
  if sbConical.Down then Result := gkConical else
    Result := gkSpiral;
end;

procedure TToolSetFrame.SetGradientKind(const Value: TGradientKind);
begin
  case Value of
    gkLinear: sbLinear.Down := True;
    gkRadial: sbRadial.Down := True;
    gkConical: sbConical.Down := True;
    else sbSpiral.Down := True;
  end;
end;

function TToolSetFrame.GetGradientRep: TGradientRep;
begin
  if sbRepNone.Down then Result := grNone else
  if sbRepSym.Down then Result := grSym else
    Result := grAsym;
end;

procedure TToolSetFrame.SetGradientRep(const Value: TGradientRep);
begin
  case Value of
    grNone: sbRepNone.Down := True;
    grSym: sbRepSym.Down := True;
    else sbRepAsym.Down := True;
  end;
end;

function TToolSetFrame.GetGradientMode: TGradientMode;
begin
  if sbColor.Down then Result := gmColor else Result := gmTransparency;
end;

procedure TToolSetFrame.SetGradientMode(const Value: TGradientMode);
begin
  if Value = gmColor then sbColor.Down := True else sbTransparency.Down := True;
end;

procedure TToolSetFrame.UpdateLayout(dt: TDrawTool);
const
  ToolsForPanel: array[TToolSetPanel] of TDrawTools =
    ([dtSelRect, dtSelEllipse, dtLasso,
      dtSelPencil, dtCrop, dtHotSpot], // tspNothing
      dtAntialiasable, // tspAntialias
     [dtRetouch, dtBrush, dtEraser, dtRecolor], // tspBrushTip
     [dtRect, dtEllipse, dtLine], // tspLineWidth
     [dtWand, dtRecolor, dtBucket], // tspTolerance
     [dtWand, dtBucket], // tspContiguous
     dtCanSampleAllLayers, // tspSampleAllLayers
     [dtRect, dtEllipse], // tspShapeFill
     [dtEyedropper], // tspEyedropperBack
     [dtRetouch], // tspRetouchMode
     [dtEraser], // tspEraserAlpha
     [dtGradient], // tspGradient
     dtCanUsePattern // tspPattern
     );

var
  y: integer;
  p: TToolSetPanel;
  Panel: TPanel;

begin
  y := 8;

  Hide;
    for p in TToolSetPanel do
    begin
      Panel := ToolSetPanels[p];
      Panel.Visible := dt in ToolsForPanel[p];

      if Panel.Visible then
      begin
        Panel.Left := 0;
        Panel.Top := y;
        inc(y, Panel.Height);
      end;
    end; // for p

    // apply stored settings to UI
    sbAntialias.Down := CommonToolSettings[frmMain.frmToolbar.DrawTool].Antialias;
    cbPattern.ItemIndex := Ord(CommonToolSettings[frmMain.frmToolbar.DrawTool].Pattern);
    sbSampleAllLayers.Down := CommonToolSettings[frmMain.frmToolbar.DrawTool].SampleAllLayers;
  Show;
end;

function TToolSetFrame.BrushShape: TBrushShape;
begin
  Result := TBrushShape(cbBrushShape.ItemIndex);
end;

constructor TToolSetFrame.Create(AOwner: TComponent);
var
  dt: TDrawTool;
  ini: TIniFile;

begin
  inherited;

  GetToolSetGlyph(sbAntialias.Glyph, tsgAntialias);
  GetToolSetGlyph(aiBrushSize.Picture.Bitmap, tsgBrush);
  GetToolSetGlyph(aiLineWidth.Picture.Bitmap, tsgLine);
  GetToolSetGlyph(aiTolerance.Picture.Bitmap, tsgTolerance);
  GetToolSetGlyph(sbContiguous.Glyph, tsgContiguous);
  GetToolSetGlyph(sbSampleAllLayers.Glyph, tsgAllLayers);
  GetToolSetGlyph(sbFramed.Glyph, tsgFramed);
  GetToolSetGlyph(sbFilled.Glyph, tsgFilled);
  GetToolSetGlyph(sbEyedropperBack.Glyph, tsgEyedropperBack);
  GetToolSetGlyph(aiEraserAlpha.Picture.Bitmap, tsgEraser);
  GetToolSetGlyph(sbLinear.Glyph, tsgLinear);
  GetToolSetGlyph(sbRadial.Glyph, tsgRadial);
  GetToolSetGlyph(sbConical.Glyph, tsgConical);
  GetToolSetGlyph(sbSpiral.Glyph, tsgSpiral);
  GetToolSetGlyph(sbRepNone.Glyph, tsgRepNone);
  GetToolSetGlyph(sbRepSym.Glyph, tsgRepSym);
  GetToolSetGlyph(sbRepAsym.Glyph, tsgRepAsym);
  GetToolSetGlyph(sbColor.Glyph, tsgColor);
  GetToolSetGlyph(sbTransparency.Glyph, tsgTransparency);

  ToolSetPanels[tspNothing] := pNothing;
  ToolSetPanels[tspAntialias] := pAntialias;
  ToolSetPanels[tspBrushTip] := pBrushTip;
  ToolSetPanels[tspLineWidth] := pLineWidth;
  ToolSetPanels[tspTolerance] := pTolerance;
  ToolSetPanels[tspContiguous] := pContiguous;
  ToolSetPanels[tspSampleAllLayers] := pSampleAllLayers;
  ToolSetPanels[tspShapeFill] := pShapeFill;
  ToolSetPanels[tspEyedropperBack] := pEyedropperBack;
  ToolSetPanels[tspRetouchMode] := pRetouchMode;
  ToolSetPanels[tspEraserAlpha] := pEraserAlpha;
  ToolSetPanels[tspGradient] := pGradient;
  ToolSetPanels[tspPattern] := pPattern;

  // load tool settings
  if Pref_SaveToolSettings then
  begin
    ini := TIniFile.Create(Config_ToolSettings);
    try
      for dt in TDrawTool do
      begin
        if dt in dtAntialiasable then
        CommonToolSettings[dt].Antialias := ini.ReadBool('Antialias', ToolNameRes[dt],
          dt in [dtTransform, dtText]);
        if dt in dtCanUsePattern then
        CommonToolSettings[dt].Pattern := TDrawPattern(ini.ReadInteger('Pattern', ToolNameRes[dt],
          Ord(dpFull)));
        if dt in dtCanSampleAllLayers then
        CommonToolSettings[dt].SampleAllLayers := ini.ReadBool('SampleAllLayers', ToolNameRes[dt],
          dt = dtEyedropper);
      end;

      FilledShapes := ini.ReadBool('x', 'FilledShapes', False);
      neLineWidth.Value := ini.ReadFloat('x', 'LineWidth', 1);
      sbEyedropperBack.Down := ini.ReadBool('x', 'EyedropperBack', True);
      neBrushSize.Value := ini.ReadFloat('x', 'BrushSize', 5);
      cbBrushShape.ItemIndex := ini.ReadInteger('x', 'BrushShape', 0);
      neTolerance.Value := ini.ReadFloat('x', 'Tolerance', 20);
      sbContiguous.Down := ini.ReadBool('x', 'Contiguous', True);
      GradientKind := TGradientKind(ini.ReadInteger('x', 'GradientKind', 0));
      neEraserAlpha.Value := ini.ReadFloat('x', 'EraserAlpha', 100);
      cbRetouchMode.ItemIndex := ini.ReadInteger('x', 'RetouchMode', 0);
      GradientRep := TGradientRep(ini.ReadInteger('x', 'GradientRep', 0));
      GradientMode := TGradientMode(ini.ReadInteger('x', 'GradientMode', 0));
    finally
      ini.Free;
    end;
  end;

  UpdateLayout(dtSelRect);
end;

procedure TToolSetFrame.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  frmMain.SetStatus('');
end;

procedure TToolSetFrame.cbGlyphDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  GlyphSize = 16;

var
  bm: TBitmap;
  bx, bmSrc: TBitmap32;
  x, y: integer;

begin
  bm := TBitmap.Create;
  try
    bm.PixelFormat := pf24bit;
    bm.Width := ARect.Width;
    bm.Height := ARect.Height;
    bm.Canvas.Brush.Color := IfThen(odSelected in State, clHighlight, clWhite);
    bm.Canvas.FillRect(bm.Canvas.ClipRect);

    with bm.Canvas do
    begin
      x := (bm.Width - GlyphSize) div 2;
      y := (bm.Height - GlyphSize) div 2;

      bx := TBitmap32.Create;
      try
        bx.Resize(GlyphSize, GlyphSize);
        if Control = cbBrushShape then bmSrc := bmBrushShape else bmSrc := bmPattern;
        bx.CopyRect(0, 0, bmSrc,
          Rect(Index * GlyphSize, 0, (Index+1) * GlyphSize, GlyphSize));
        bx.DrawTo24(bm, x, y);
      finally
        bx.Free;
      end;
    end; // with bm.Canvas

    TComboBox(Control).Canvas.Draw(ARect.Left, ARect.Top, bm);
  finally
    bm.Free;
  end;
end;

procedure TToolSetFrame.cbPatternChange(Sender: TObject);
begin
  CommonToolSettings[frmMain.frmToolbar.DrawTool].Pattern :=
    TDrawPattern(Max(0, cbPattern.ItemIndex));
end;

destructor TToolSetFrame.Destroy;
var
  dt: TDrawTool;
  ini: TIniFile;
  
begin
  // save tool settings
  if Pref_SaveToolSettings then
  begin
    ini := TIniFile.Create(Config_ToolSettings);
    try
      for dt in TDrawTool do
      begin
        if dt in dtAntialiasable then
          ini.WriteBool('Antialias', ToolNameRes[dt], CommonToolSettings[dt].Antialias);
        if dt in dtCanUsePattern then
          ini.WriteInteger('Pattern', ToolNameRes[dt], Ord(CommonToolSettings[dt].Pattern));
        if dt in dtCanSampleAllLayers then
          ini.WriteBool('SampleAllLayers', ToolNameRes[dt], CommonToolSettings[dt].SampleAllLayers);
      end;
      ini.WriteBool('x', 'FilledShapes', FilledShapes);
      ini.WriteFloat('x', 'LineWidth', neLineWidth.Value);
      ini.WriteBool('x', 'EyedropperBack', sbEyedropperBack.Down);
      ini.WriteFloat('x', 'BrushSize', neBrushSize.Value);
      ini.WriteInteger('x', 'BrushShape', cbBrushShape.ItemIndex);
      ini.WriteFloat('x', 'Tolerance', neTolerance.Value);
      ini.WriteBool('x', 'Contiguous', sbContiguous.Down);
      ini.WriteInteger('x', 'GradientKind', Integer(GradientKind));
      ini.WriteFloat('x', 'EraserAlpha', neEraserAlpha.Value);
      ini.WriteInteger('x', 'RetouchMode', cbRetouchMode.ItemIndex);
      ini.WriteInteger('x', 'GradientRep', Integer(GradientRep));
      ini.WriteInteger('x', 'GradientMode', Integer(GradientMode));
    finally
      ini.Free;
    end;
  end;
  
  inherited;
end;

procedure TToolSetFrame.ControlMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  frmMain.SetStatus((Sender as TControl).Hint);
end;

procedure TToolSetFrame.sbAntialiasClick(Sender: TObject);
begin
  CommonToolSettings[frmMain.frmToolbar.DrawTool].Antialias := sbAntialias.Down;
  // TODO redraw only those which have something selected
  if frmMain.frmToolbar.DrawTool = dtTransform then
    frmMain.RedrawAllGraphicFrames;
end;

procedure TToolSetFrame.sbSampleAllLayersClick(Sender: TObject);
begin
  CommonToolSettings[frmMain.frmToolbar.DrawTool].SampleAllLayers := sbSampleAllLayers.Down;
end;

end.
