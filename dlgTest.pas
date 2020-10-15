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
unit dlgTest;

interface

uses
  LclIntf, LclType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DoubleBufPB, dlgDoc, DocClass, ExtCtrls, bmExUtils,
  BitmapEx, Buttons, ComCtrls, ieShared, Math, FileUtil, OpenGLContext, Layers,
  dlgDebug, Utils3d, gl, glu, glext, types, NinePatch, WSControls, WSLCLClasses;

type
  TCustomOpenGLControl_fixed = class(TCustomOpenGLControl)
  end;

  TWSOpenGLControlClass = class of TWSOpenGLControl;
  TWSOpenGLControl_fixed = class(TWSOpenGLControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
  end;

  { TfrmTest }

  TfrmTest = class(TForm)
    cbTextureMultiplier: TComboBox;
//    glTextureTest: TOpenGLControl; // create manually because of OpenGL exception catching
    iNinePatch: TImage;
    lNotNinePatch: TLabel;
    od3dObject: TOpenDialog;
    pc: TPageControl;
    pOpenGLControls: TPanel;
    pb: TDoubleBufPB;
    pOpenGLControls1: TPanel;
    sbObjLoad: TSpeedButton;
    sbLighting: TSpeedButton;
    sbRedrawNinePatch: TSpeedButton;
    tmGLMessageUpdate: TTimer;
    ts9Patch: TTabSheet;
    tsTexture: TTabSheet;
    tsGeneral: TTabSheet;
    tmCursor: TTimer;
    tmAni: TTimer;
    pTop: TPanel;
    sbClear: TSpeedButton;
    sbBgrLoad: TSpeedButton;
    sbBgrDefault: TSpeedButton;
    procedure cbTextureMultiplierChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure glTextureTestMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glTextureTestMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure glTextureTestMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glTextureTestMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure glTextureTestPaint(Sender: TObject);
    procedure pbPaint(Sender: TObject);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure sbLightingClick(Sender: TObject);
    procedure sbObjLoadClick(Sender: TObject);
    procedure sbRedrawNinePatchClick(Sender: TObject);
    procedure tmCursorTimer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure tmAniTimer(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure sbClearClick(Sender: TObject);
    procedure sbBgrLoadClick(Sender: TObject);
    procedure sbBgrDefaultClick(Sender: TObject);
    procedure tmGLMessageUpdateTimer(Sender: TObject);
  private
    procedure CreateGL;
    procedure ShowGLError;
  public
    bmBgr, bmSketch: TBitmap;
    pPrev, pCursor: TPoint;
    Drawing: boolean;
    TexturedObject: T3dObject;

    Doc: TIconDoc;
    FrameEnd: TArrayOfInteger;
    SelectedImageIndex: integer;
    ImageIndex, LoopCount: integer;
    timeAniStart: integer;

    glCreated: boolean;
    glWorking: boolean;
    glTextureTest: TCustomOpenGLControl_fixed;
    glErrorPanel: TPanel;
    texture: GLuint;
    glRotation: TPoint;
    glZoom: double;
    glRotating: boolean; // user is rotating with mouse
    glPrevMousePoint: TPoint; // previous mouse point when rotating

    NinePatch: TNinePatch;

    procedure ApplyLanguagePack;
    procedure ClearSketch;
    procedure LoadBackground;
    procedure Execute(frmDoc: TGraphicFrame);
    procedure RefreshPreview;
    procedure DrawNinePatch;
    function GetFrame(index: integer): TBitmap32;
  end;

var
  frmTest: TfrmTest = nil;
  glErrorMessage: string = '';

implementation

uses Main, LangPack;

{$R *.lfm}

{ TWSOpenGLControl_fixed }

class function TWSOpenGLControl_fixed.CreateHandle(
  const AWinControl: TWinControl; const AParams: TCreateParams): HWND;
begin
  // do not use "inherited CreateHandle", because the LCL changes the hierarchy at run time
  try
    Result := TWSOpenGLControl.CreateHandle(AWinControl, AParams);
  except
    on e: Exception do
    begin
      glErrorMessage := e.Message;
      Result := 0;
    end;
  end;
end;

procedure TfrmTest.ClearSketch;
begin
  bmSketch.PixelFormat := pf24bit;
  bmSketch.Width := pb.Width;
  bmSketch.Height := pb.Height;
  bmSketch.Canvas.StretchDraw(Rect(0, 0, bmSketch.Width, bmSketch.Height), bmBgr);

  pb.Repaint;
end;

procedure TfrmTest.LoadBackground;
var
  Success: boolean;
  docBackground: TIconDoc;
  bx: TBitmap32;

begin
  Success := FileExists(Pref_TestBackground);
  if Success then
  begin
    docBackground := TIconDoc.Create;
    try
      Success := (docBackground.LoadFromFile(Pref_TestBackground,
        Pref_MaxWidth, Pref_MaxHeight, 1, '') <> iftNone);
      if Success then
      begin
        bx := TBitmap32.Create;
        try
          bx.Assign(docBackground.Pages[0].Layers);
          bx.ToBitmap(bmBgr, clWhite);
        finally
          bx.Free;
        end;
      end;
    finally
      docBackground.Free;
    end;
  end;

  if not Success then
  begin
    bmBgr.PixelFormat := pf24bit;
    bmBgr.Width := 4;
    bmBgr.Height := 1;

    with bmBgr.Canvas do
    begin
      Pixels[0, 0] := clBlack;
      Pixels[1, 0] := clGray;
      Pixels[2, 0] := clSilver;
      Pixels[3, 0] := clWhite;
    end;
  end;
end;

procedure TfrmTest.Execute(frmDoc: TGraphicFrame);
var
  i, t: integer;
  ls: TLayers;

begin
  if frmDoc.Doc.PageCount = 0 then
    Exit;
  Doc.Assign(frmDoc.Doc);
  for i := 0 to Doc.PageCount - 1 do
  begin
    ls := Doc.Pages[i].Layers;
    ls.FlattenFloatingSelection;
    ls.Merge(lssAll);
    if ls.LayerCount = 0 then
    begin
      // avoid index out of bounds
      ls.Resize(1, 1);
      ls.NewLayer;
    end;
  end;

  // preprocess animation
  LoopCount := 0;
  SetLength(FrameEnd, Doc.PageCount);
  t := 0;
  for i := 0 to Doc.PageCount - 1 do with Doc.Pages[i] do
  begin
    if FrameRate <> 0 then LoopCount :=
      IfThen(Doc.Metadata.LoopCount = 0, MaxInt, Doc.Metadata.LoopCount);

    // calculate time constraints of the frame
    inc(t, FrameRate);
    FrameEnd[i] := t;
  end;
  SelectedImageIndex:=frmDoc.ImageIndex;
  if LoopCount <> 0 then
  begin
    ImageIndex := 0;
    timeAniStart := GetTickCount;
  end else
    ImageIndex := SelectedImageIndex;

  // cursor
  ClearSketch;
  pCursor := Point(10, 10);
  Mouse.CursorPos := pb.ClientToScreen(pCursor);
  tmCursor.Enabled := True;
  tmAni.Enabled := LoopCount <> 0;

  // 9-patch
  NinePatch.Init(GetFrame(SelectedImageIndex));
  DrawNinePatch;

  ShowModal;
end;

procedure TfrmTest.RefreshPreview;
begin
  DrawNinePatch;
end;

procedure TfrmTest.DrawNinePatch;
var
  bm32: TBitmap32;
  i: integer;
begin
  lNotNinePatch.Visible := not NinePatch.IsNinePatch;
  iNinePatch.Visible := NinePatch.IsNinePatch;
  if NinePatch.IsNinePatch then
  begin
    bm32 := TBitmap32.Create;
    try
      bm32.Resize(iNinePatch.Width, iNinePatch.Height);
      for i := 1 to 10 do
        NinePatch.Draw(bm32, Rect(Random(bm32.Width), Random(bm32.Height), Random(bm32.Width), Random(bm32.Height)));
      iNinePatch.Picture.Assign(bm32);
    finally
      bm32.Free;
    end;
  end;
end;

function TfrmTest.GetFrame(index: integer): TBitmap32;
begin
  Result := Doc.Pages[index].Layers[0].Image;
end;

procedure TfrmTest.CreateGL;
begin
  if glCreated then
    Exit;
  glCreated := true;

{$ifdef LINUX}
  // TODO fix OpenGL on Linux, or at least the error handling,
  // because even that does not work
  glErrorMessage := 'GFIE does not support OpenGL on Linux.';
  Exit;
{$endif}

  try
    glTextureTest := TCustomOpenGLControl_fixed.Create(tsTexture);
  except
    on e: Exception do
    begin
      glErrorMessage := e.Message;
      glTextureTest := nil;
    end;
  end;

  if Assigned(glTextureTest) then
  try
    with glTextureTest do
    begin
      Parent := tsTexture;
      Align := alClient;
      AutoResizeViewport := True;
      MultiSampling := 4;
      AlphaBits := 8;
      OnMouseDown := glTextureTestMouseDown;
      OnMouseMove := glTextureTestMouseMove;
      OnMouseUp := glTextureTestMouseUp;
      OnMouseWheel := glTextureTestMouseWheel;
      OnPaint := glTextureTestPaint;
      Visible := true;
      Cursor := ieCursorBase + Ord(iecHand);
    end;

    glWorking := true;
  except
    on e: Exception do
    begin
      glErrorMessage := e.Message;
      FreeAndNil(glTextureTest);
    end;
  end;
end;

procedure TfrmTest.ShowGLError;
begin
  if glErrorMessage = '' then
    Exit;

  glWorking := false;
  if glErrorPanel = nil then
  begin
    glErrorPanel := TPanel.Create(tsTexture);
    with glErrorPanel do
    begin
      Parent := tsTexture;
      Align := alClient;
      Caption := '';
      Visible := true;
      BringToFront;
    end;
  end;
  if glErrorPanel.Caption <> '' then
    glErrorMessage := ' ' + glErrorMessage;
  glErrorPanel.Caption := glErrorPanel.Caption + glErrorMessage;
  glErrorMessage := '';
end;

procedure TfrmTest.FormCreate(Sender: TObject);
begin
  glCreated := false;
  glWorking := false;

  RegisterWSComponent(TCustomOpenGLControl_fixed, TWSOpenGLControl_fixed);

  ApplyLanguagePack;
  // load tool button glyphs
  GetMiscGlyph(sbClear.Glyph, mgDelete);
  GetMiscGlyph(sbBgrLoad.Glyph, mgOpen);
  GetMiscGlyph(sbBgrDefault.Glyph, mgBgrDefault);
  GetMiscGlyph(sbObjLoad.Glyph, mgOpen);
  GetMiscGlyph(sbLighting.Glyph, mgLighting);
  GetMiscGlyph(sbRedrawNinePatch.Glyph, mgRefresh);
  pc.ActivePage := pc.Pages[0];

  bmBgr := TBitmap.Create;
  bmSketch := TBitmap.Create;
  Doc := TIconDoc.Create;

  pb.Cursor := crNone;
  Drawing := False;

  LoadBackground;
  TexturedObject := T3dObject.Create;
  TexturedObject.LoadFromObjFile(AppDir+'Data/teapot.obj');
  NinePatch:=TNinePatch.Create;

  texture := 0;
  glRotation := Point(0, 0);
  glZoom := 1;
  glRotating := false;
  glPrevMousePoint := Point(0, 0);

  if VerboseMode then Log('TfrmTest created');
end;

procedure TfrmTest.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if texture <> 0 then
  begin
    glDeleteTextures(1, @texture);
    texture := 0;
  end;
end;

procedure TfrmTest.cbTextureMultiplierChange(Sender: TObject);
begin
  if glWorking then
    glTextureTest.Repaint;
end;

procedure TfrmTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(bmBgr);
  FreeAndNil(bmSketch);
  FreeAndNil(Doc);
  FreeAndNil(TexturedObject);
  FreeAndNil(NinePatch);
end;

procedure TfrmTest.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then Close else
  if Key = VK_F5 then RefreshPreview;
end;

procedure TfrmTest.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
  CreateGL;
end;

procedure TfrmTest.glTextureTestMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not glWorking then
    Exit;

  if Button = mbLeft then
  begin
    glRotating := True;
    glPrevMousePoint := Point(X, Y);
  end;
end;

procedure TfrmTest.glTextureTestMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not glWorking then
    Exit;

  if glRotating then
  begin
    glRotation.X += X - glPrevMousePoint.X;
    glRotation.Y += Y - glPrevMousePoint.Y;
    glPrevMousePoint := Point(X, Y);
    glTextureTest.Repaint;
  end;
end;

procedure TfrmTest.glTextureTestMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not glWorking then
    Exit;

  glRotating := False;
end;

procedure TfrmTest.glTextureTestMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const
  Factor = 1.1;
begin
  if not glWorking then
    Exit;

  if WheelDelta > 0 then
    glZoom *= Factor
  else
    glZoom /= Factor;
  Handled := True;
  glTextureTest.Repaint;
end;

procedure TfrmTest.glTextureTestPaint(Sender: TObject);
const
  GL_TEXTURE_BASE_LEVEL = $813C;
  GL_TEXTURE_MAX_LEVEL = $813D;

var
  scale, texScale: double;
  DiffuseLight: array[0..3] of GLfloat = (0.8, 0.8, 0.8, 1);
  LightPosition: array[0..3] of GLfloat = (0.2, 0.2, -1, 0);

begin
  if not glWorking then
    Exit;

  glEnable(GL_DEPTH_TEST);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_BLEND);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POLYGON_SMOOTH);

  glMatrixMode(GL_MODELVIEW);
  glViewport(0, 0, glTextureTest.Width, glTextureTest.Height);
  glLoadIdentity;

  if sbLighting.Down then
  begin
    glEnable(GL_LIGHTING);
    glLightfv(GL_LIGHT0, GL_DIFFUSE, DiffuseLight);
    glLightfv(GL_LIGHT0, GL_POSITION, LightPosition);
    glEnable(GL_LIGHT0);
    glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);
    glEnable(GL_COLOR_MATERIAL);
  end else
    glDisable(GL_LIGHTING);

  texScale := StrToInt(StringReplace(cbTextureMultiplier.Text, 'x', '', []));
  if texScale <> 0 then
  begin
    glEnable(GL_TEXTURE_2D);
    if texture = 0 then
    begin
      glGenTextures(1, @texture);
      glBindTexture(GL_TEXTURE_2D, texture);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_BASE_LEVEL, 0);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAX_LEVEL, 7);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      if not Assigned(glGenerateMipmap) then
        glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // until gl 3.0

      glTextureFromBitmap32(texture, GetFrame(SelectedImageIndex));
      glEnable(GL_TEXTURE_2D);
      if Assigned(glGenerateMipmap) then
        glGenerateMipmap(GL_TEXTURE_2D); // from gl 3.0
    end else
      glBindTexture(GL_TEXTURE_2D, texture);
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity;
    glScaled(texScale, texScale, 1);
    glMatrixMode(GL_MODELVIEW);
  end else
    glDisable(GL_TEXTURE_2D);

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glShadeModel(GL_SMOOTH);

  glMatrixMode(GL_MODELVIEW);
  scale := glZoom*0.05;
  glScaled(scale, scale, scale);
  glRotated(glRotation.y / 10, -1, 0, 0);
  glRotated(glRotation.x / 10, 0, -1, 0);

  TexturedObject.glDraw;
  glFlush;
  glTextureTest.SwapBuffers;
end;

procedure TfrmTest.pbPaint(Sender: TObject);
begin
  pb.Canvas.Draw(0, 0, bmSketch);
  GetFrame(ImageIndex).DrawTo24(pb.Buffer, pCursor.X - Doc.Pages[ImageIndex].HotSpot.X,
    pCursor.Y - Doc.Pages[ImageIndex].HotSpot.Y);
end;

procedure TfrmTest.pbMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Drawing then
  with bmSketch.Canvas do
  begin
    Pen.Color := clRed;
    MoveTo(pPrev.X, pPrev.Y);
    pPrev := Point(X, Y);
    LineTo(pPrev.X, pPrev.Y);
  end;

  if (X <> pCursor.X) or (Y <> pCursor.Y) then pCursor := Point(X, Y);

  pb.Repaint;
end;

procedure TfrmTest.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    Drawing := True;
    pPrev := Point(X, Y);
  end;
end;

procedure TfrmTest.pbMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Drawing := False;
end;

procedure TfrmTest.sbLightingClick(Sender: TObject);
begin
  if glWorking then
    glTextureTest.Repaint;
end;

procedure TfrmTest.sbObjLoadClick(Sender: TObject);
var
  t: T3dObject;

begin
  if not glWorking then
    Exit;

  if od3dObject.Execute then
  begin
    t := T3dObject.Create;
    try
      t.LoadFromObjFile(od3dObject.FileName);
      if t.HasTextureCoords then
      begin
        // TODO assign, do not load 2nd time
        TexturedObject.LoadFromObjFile(od3dObject.FileName);
        glTextureTest.Repaint;
      end else
        ShowMessage(lpGet('MSG_ERROR_NO_TEXTURE_COORDS'));
    finally
      t.Free;
    end;
  end;
end;

procedure TfrmTest.sbRedrawNinePatchClick(Sender: TObject);
begin
  DrawNinePatch;
end;

procedure TfrmTest.tmCursorTimer(Sender: TObject);
var
  p: TPoint;
  
begin
  p := pb.ScreenToClient(Mouse.CursorPos);
  if (p.X <> pCursor.X) or (p.Y <> pCursor.Y) then
  begin
    pCursor := p;
    pb.Repaint;
  end;
end;

procedure TfrmTest.FormHide(Sender: TObject);
begin
  tmCursor.Enabled := False;
  tmAni.Enabled := False;
end;

procedure TfrmTest.tmAniTimer(Sender: TObject);
var
  AniTicks: integer;
  
begin
  // the number of msecs elapsed from the beginning of the animation
  AniTicks := GetTickCount - timeAniStart;

  if AniTicks >= FrameEnd[ImageIndex] then
  // next frame
  begin
    while AniTicks >= FrameEnd[ImageIndex] do
    begin
      inc(ImageIndex);
      if ImageIndex >= Doc.PageCount then
      begin
        // loop
        ImageIndex := 0;
        timeAniStart := GetTickCount;

        dec(LoopCount);
        if LoopCount = 0 then tmAni.Enabled := False;
        
        Break;
      end;
    end; // while a new frame is needed

    // draw the new frame
    pb.Repaint;
  end;
end;

procedure TfrmTest.FormResize(Sender: TObject);
begin
  ClearSketch;
end;

procedure TfrmTest.sbClearClick(Sender: TObject);
begin
  ClearSketch;
end;

procedure TfrmTest.sbBgrLoadClick(Sender: TObject);
begin
  if frmMain.od.Execute then
  begin
    Pref_TestBackground := UTF8ToSys(frmMain.od.FileName);
    LoadBackground;
    ClearSketch;
  end;
end;

procedure TfrmTest.sbBgrDefaultClick(Sender: TObject);
begin
  Pref_TestBackground := '';
  LoadBackground;
  ClearSketch;
end;

procedure TfrmTest.tmGLMessageUpdateTimer(Sender: TObject);
begin
  ShowGLError;
end;

procedure TfrmTest.ApplyLanguagePack;
begin
  Caption := lpGet('MI_ICON_TEST');
  tsGeneral.Caption := lpGet('TS_CURSOR');
  tsTexture.Caption := lpGet('TS_TEXTURE_3D');
  ts9Patch.Caption := lpGet('TS_9PATCH');
  sbClear.Hint := lpGet('B_CLEAR');
  sbBgrLoad.Hint := lpGet('HINT_TEST_BGR_LOAD')+'...';
  sbBgrDefault.Hint := lpGet('HINT_TEST_BGR_DEFAULT');
  sbObjLoad.Hint := lpGet('HINT_TEST_OBJ_LOAD')+'...';
  sbLighting.Hint := lpGet('HINT_TEST_LIGHTING');
  od3dObject.Title := lpGet('HINT_TEST_OBJ_LOAD');
  od3dObject.Filter := Format('%s (*.obj)|*.obj', [lpGet('FF_OBJ_3D')]);
  cbTextureMultiplier.Hint := lpGet('TS_TEXTURE_MULTIPLIER');
  sbRedrawNinePatch.Hint := lpGet('LABEL_REDRAW');
  lNotNinePatch.Caption := lpGet('TS_NOT_9PATCH');
end;

end.
