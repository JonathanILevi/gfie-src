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
unit ieShared;

interface

uses
  LclIntf, LclType, FileUtil, Process,
  SysUtils, Types, Classes, Controls, StdCtrls, ExtCtrls, ComCtrls,
  Graphics, Forms, Math, Menus,
  Buttons, Clipbrd, IniFiles, BitmapEx, bmExUtils, PNG, Dialogs, dlgDebug;

const
  sGreenfishHomepage = 'http://greenfishsoftware.blogspot.com/';
  sGreenfishSupport = 'http://greenfishsoftware.blogspot.com/2012/07/contact-us.html';

  CascadeValue = 16; // when cascading child windows
  RunCommandHidden_Exception_ExitStatus = -123;

type
  TDrawTool = (dtSelRect, dtSelEllipse, dtLasso, dtWand,
    dtSelPencil, dtTransform, dtCrop, dtHotSpot,
    dtEyedropper, dtRetouch,
    dtRect, dtEllipse, dtLine, dtText, dtPencil, dtBrush,
    dtEraser, dtRecolor, dtBucket, dtGradient,
    dtNone);
  TDrawTools = set of TDrawTool;

  // Mouse wheel action, when scrolling the wheel over the image area
  TPrefMWA = (mwaNone, mwaScroll, mwaZoom);

  // Cursor resources in gfie.exe
  TieCursor = (iecHand, iecHandGrab, iecBigCross, iecRotate,
    iecSelect, iecSelectAdd, iecSelectSub, iecSelectInt,
    iecCrop, iecEyedropper, iecRetouch, iecRect, iecEllipse, iecLine,
    iecPencil, iecBrush, iecEraser, iecRecolor, iecBucket, iecGradient);

  // toolset.png
  TToolSetGlyph = (tsgAntialias, tsgFramed, tsgFilled,
    tsgLine, tsgBrush, tsgEraser,
    tsgTolerance, tsgContiguous, tsgAllLayers, tsgEyedropperBack,
    tsgLinear, tsgRadial, tsgConical, tsgSpiral,
    tsgRepNone, tsgRepSym, tsgRepAsym, tsgColor, tsgTransparency);

  // Glyphs contained by misc.png
  TMiscGlyph = (mgNew, mgOpen, mgSave, mgDelete, mgPlus, mgMinus,
    mgZoom1, mgZoomFit, mgGrid, mgCenterLines, mgPages, mgTest,
    mgSwap, mgBW, mgTransparent, mgInverted, mgMergeSelected,
    mgReplace, mgProperties, mgExtractEdit, mgExtractSave,
    mgBgrDefault, mgLighting, mgRefresh);

  TDocumentSaveOption = (dsoSilent, dsoSaveACopy);
  TDocumentSaveOptions = set of TDocumentSaveOption;

const
  ieCursorBase = 2300;
  iecNone = iecSelect;

  // Currently inverted cursors do not work on Linux.
  CursorSuffix = {$ifdef LINUX}'NOINV'{$else}''{$endif};
  ieCursorRes: array[TieCursor] of string =
    ('HAND', 'HANDGRAB', 'BIGCROSS'+CursorSuffix, 'ROTATE',
    'SELECT'+CursorSuffix, 'SELECTADD'+CursorSuffix, 'SELECTSUB'+CursorSuffix, 'SELECTINT'+CursorSuffix,
    'CROP'+CursorSuffix, 'EYEDROPPER'+CursorSuffix, 'RETOUCH'+CursorSuffix, 'RECT'+CursorSuffix, 'ELLIPSE'+CursorSuffix, 'LINE'+CursorSuffix,
    'PENCIL'+CursorSuffix, 'BRUSH'+CursorSuffix, 'ERASER'+CursorSuffix, 'RECOLOR'+CursorSuffix, 'BUCKET'+CursorSuffix, 'GRADIENT'+CursorSuffix);

  ToolCursor: array[TDrawTool] of TieCursor =
    (iecSelect, iecSelect, iecSelect, iecSelect, iecSelect, iecSelect,
    iecCrop, iecNone, iecEyedropper, iecRetouch, iecRect, iecEllipse, iecLine,
    iecNone, iecPencil, iecBrush, iecEraser, iecRecolor, iecBucket, iecGradient,
    iecNone);

  dtSelect = [dtSelRect, dtSelEllipse, dtLasso, dtWand, dtSelPencil];
  dtAntialiasable = [dtTransform, dtEllipse, dtLine, dtText, dtPencil,
    dtBrush, dtEraser, dtRecolor];
  dtCanUsePattern = [dtRect, dtEllipse, dtLine, dtPencil, dtBrush,
    dtEraser, dtRecolor, dtBucket, dtGradient];
  dtCanSampleAllLayers = [dtWand, dtEyedropper, dtBucket];
  ToolShortcut = 'SAOWIFXHYURJLTPBEQKG';

const
  Default_Pref_MaxWidth = 5000;
  Default_Pref_MaxHeight = 5000;
  Default_Pref_LanguagePack = 'English.txt';

var
  AppDir: string;
  DataDir: string;
  LanguageDir: string;
  ConfigDir: string;
  HelpDir: string;
  Config_Preferences: string;
  Config_RecentFiles: string;
  Config_WindowPos: string;
  Config_ToolSettings: string;
  CF_PNG, CF_PNG_2: cardinal;
  CF_GFIE_INVERSION: cardinal;
  CF_GFIE_SIZE: cardinal;
  CF_GFIE_ANGLE: cardinal;
  bmToolbar: TBitmap;
  bmMiscGlyphs: TBitmap32;
  bmToolSettings: TBitmap;
  bmBrushShape: TBitmap32;
  bmPattern: TBitmap32;
  bmEye: TBitmap32;

  // preferences are stored here
  Pref_LanguagePack: string = Default_Pref_LanguagePack;
  Pref_TestBackground: string = '';
  Pref_MaxWidth: integer = Default_Pref_MaxWidth;
  Pref_MaxHeight: integer = Default_Pref_MaxHeight;
  Pref_PNGLimit: integer = 256;

  Pref_Hatch: THatchDesc = (Color: (clWhite, $c0c0c0));
  Pref_clGrid: TColor2 = (clGray, clSilver);
  Pref_clGrid2: TColor2 = (clMaroon, clRed);
  Pref_DialogViewMode: integer = 3;
  Pref_MWA: TPrefMWA = mwaZoom;
  Pref_SaveToolSettings: boolean = True;

  Pref_ShowStartupScreen: boolean = True;
  Pref_FilterPreview: boolean = True;
  Pref_CustomColors: array[0..15] of TColor;
  Pref_DisplayScriptWarning: boolean = true;

// If the form is nil, creates it.
procedure DoCreateForm(FormClass: TComponentClass; var frm);
// Converts a color to its HTML representation (without Alpha)
function ColorToHTML(c: TColor): string;
// Whether the mouse is over the control
function ControlHasMouse(c: TControl): boolean;
// version can be e.g. '3.5'
function DotNetInstalled(const version: string): boolean;
// Beeps
procedure ErrorBeep;
// Deletes extension and path parts
function ExtractOnlyFileName(const fn: string): string;
// Finds a character in a zero-terminated string
function FindInPChar(p: PChar; c: char): PChar;
// Returns a slice of a bitmap which consists of glyphs arranged in rows
procedure bmGetSlice(bmSrc, bmDest: TBitmap; RowCount, CellWidth, Index: integer);
procedure bmGetSlice32(bmSrc, bmDest: TBitmap32; RowCount, CellWidth, Index: integer);
// Easier to use than FindFirst
type
  TSearchRecs = array of TSearchRec;
procedure FindAll(const Path: string; Attr: integer; out sr: TSearchRecs);
// Extract glyphs from image lists
procedure GetToolGlyph(bm: TBitmap; id: TDrawTool);
procedure GetToolSetGlyph(bm: TBitmap; id: TToolSetGlyph);
procedure GetMiscGlyph(bm: TBitmap; id: TMiscGlyph; BackColor: TColor = clForm);
// Displays a color dialog with Pref_CustomColors
function GFIEPickColor(Value: TColor): TColor;
// Parses a HTML color
function HTMLToColor(s: string): TColor;
// Whether an Icon Editor-compatible selection is on the clipboard
function IconEditorCanPaste: boolean;
// Shortcut -> tool, ONLY for upper case letters
function KeyToDrawTool(Key: char): TDrawTool;
// Lazarus bug workaround -- OnCanClose does not currently work
function sdExecuteWithCanClose(sd: TSaveDialog): boolean;
// Makes a progress bar like [***           ]
function MakeProgressString(q: double): string;
// Makes a popup menu from the subitems of a specified menu item
procedure MenuItemToPopupMenu(mi: TMenuItem; pm: TPopupMenu);
// Reads whole contents of a stream into a string.
function ReadAllText(s: TStream): string;
// Deletes all files and subfolders in the folder, then calls RemoveDir
procedure BrutallyRemoveDir(s: string);
// Returns true on success, false otherwise.
function RunCommandAsync(folder, command: string; swo: TShowWindowOptions; params: array of string): boolean;
// Returns process return value.
function RunCommandHidden(folder, command: string; params: array of string): integer;
// Sets the Items.Text property WHILE preserving the ItemIndex property
procedure SetComboItems(cb: TComboBox; const Items_Text: string);
// If the filename has no extension, it appends 'ext' to it
function SetDefaultExt(const fn, ext: string): string;
// Returns tab at position
function TabAtPos(pc: TPageControl; const p: TPoint): TTabSheet;
// Sets .Down to not .Down and calls .Click
procedure ToggleSpeedButton(Control: TSpeedButton);
// Makes a valid file name from a given string
// Having to be cross-platform, this function is very conservative.
function ValidFileName(const s: string): string;
// Converts a rectangle to another one which will yield the original one
// drawn on a windows canvas
function WinCanvasConvert(r: TRect): TRect;
function WithoutAmp(s: string): string;
// Deletes the extension part of a filename
function WithoutExt(fn: string): string;

type
  TRect4 = array[0..3] of TRect;
  TSubtractRect = record
    Count: integer;
    Rects: TRect4;

    procedure Subtract(const r1, r2: TRect);
  end;

// Preferences
procedure prefLoad;
procedure prefSave;

// Window positions
procedure wndPosLoad;
procedure wndPosSave;

implementation

uses
  Main, Registry;

// ------------------

function ColorToHTML(c: TColor): string;
begin
  // RGB
  Result := '#' + IntToHex(FlipColor(c), 6);
end;

procedure DoCreateForm(FormClass: TComponentClass; var frm);
begin
  if PPointer(@frm)^ = nil then
    Application.CreateForm(FormClass, frm);
end;

function ControlHasMouse(c: TControl): boolean;
begin
  Result := c.Visible and PtInRect(c.ClientRect, c.ScreenToClient(Mouse.CursorPos));
end;

function DotNetInstalled(const version: string): boolean;
{$ifdef WINDOWS}
var
  reg: TRegistry;
begin
  Result := False;
  reg := TRegistry.Create(KEY_READ);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := reg.KeyExists('Software\Microsoft\.NETFramework\AssemblyFolders\v'+version);
  finally
    reg.Free;
  end;
{$endif}
{$ifdef LINUX}
begin
  Result := (RunCommandHidden(AppDir, 'mono', ['--help']) = 0)
    or FileExists('dotnet_installed'); // for suppressing any error in the future
{$endif}
end;

procedure ErrorBeep;
begin
  SysUtils.Beep;
end;

function ExtractOnlyFileName;
var
  i: integer;

begin
  Result := ExtractFileName(fn);
  i := Pos('.', Result);
  if i <> 0 then
    Delete(Result, i, Length(Result) - i + 1);
end;

function FindInPChar;
begin
  while (p^ <> #0) and (p^ <> c) do
    Inc(p);
  Result := p;
end;

procedure GetCellLocation(RowCount, CellWidth, Index, bmWidth, bmHeight: integer;
  var x, y, w, h: integer);
var
  ColCount: integer;
begin
  if CellWidth <= 0 then CellWidth := 1;
  if RowCount <= 0 then RowCount := 1;

  w := CellWidth;
  h := bmHeight div RowCount;
  ColCount := bmWidth div w;
  if ColCount <= 0 then ColCount := 1;
  x := (Index mod ColCount) * w;
  y := (Index div ColCount) * h;
end;

procedure bmGetSlice(bmSrc, bmDest: TBitmap; RowCount, CellWidth, Index: integer);
var
  w, h, x, y: integer;

begin
  GetCellLocation(RowCount, CellWidth, Index, bmSrc.Width, bmSrc.Height, x, y, w, h);
  with bmDest do
  begin
    PixelFormat := pf24bit;
    Width := w;
    Height := h;
    Canvas.CopyRect(Rect(0, 0, w, h), bmSrc.Canvas, Rect(x, y, x + w, y + h));
  end;
end;

procedure bmGetSlice32(bmSrc, bmDest: TBitmap32; RowCount, CellWidth, Index: integer);
var
  w, h, x, y: integer;

begin
  GetCellLocation(RowCount, CellWidth, Index, bmSrc.Width, bmSrc.Height, x, y, w, h);
  bmDest.Resize(w, h);
  bmDest.FillTransparent;
  bmDest.CopyRect(0, 0, bmSrc, Rect(x, y, x + w, y + h));
end;

procedure FindAll;
var
  Count: integer;
  r: TSearchRec;

begin
  Count := 0;
  SetLength(sr, 0);

  if FindFirst(Path, Attr, r) <> 0 then
    Exit;
  repeat
    Inc(Count);
    if Count > Length(sr) then
      SetLength(sr, Count or $ff);
    sr[Count - 1] := r;
  until FindNext(r) <> 0;
  FindClose(r);

  SetLength(sr, Count);
end;

procedure GetToolGlyph;
begin
  bmGetSlice(bmToolbar, bm, 2, bmToolbar.Height div 2, Ord(id));
end;

procedure GetToolSetGlyph;
begin
  bmGetSlice(bmToolSettings, bm, 1, bmToolSettings.Height, Ord(id));
end;

procedure GetMiscGlyph(bm: TBitmap; id: TMiscGlyph; BackColor: TColor);
var
  bm32: TBitmap32;

begin
  bm32 := TBitmap32.Create;
  try
    bmGetSlice32(bmMiscGlyphs, bm32, 1, bmMiscGlyphs.Height, Ord(id));
    bm32.ToBitmap(bm, BackColor);
  finally
    bm32.Free;
  end;
end;

function GFIEPickColor;
var
  i: integer;
  s: string;
  cd: TColorDialog;

begin
  cd := TColorDialog.Create(Application);
  try
    cd.Color := Value;
    s := '';
    for i := 0 to 15 do
      s := s + Format('Color%s=%.6x' + LineEnding,
        [char(Ord('A') + i), Pref_CustomColors[i]]);
    cd.CustomColors.Text := s;
    //cd.Options := [cdFullOpen];

    if cd.Execute then
      Result := cd.Color
    else
      Result := Value;

    for i := 0 to cd.CustomColors.Count - 1 do
    begin
      s := cd.CustomColors[i];
      Pref_CustomColors[Ord(s[6]) - Ord('A')] := StrToInt('$' + Copy(s, 8, 6));
    end;
  finally
    cd.Free;
  end;
end;

function HTMLToColor(s: string): TColor;
begin
  s[1] := '$';
  while Length(s) < 7 do
    s := s + '0';
  TryStrToInt(s, integer(Result));
  Result := FlipColor(Result);
end;

function IconEditorCanPaste: boolean;
begin
  if Clipboard.HasFormat(CF_PNG) or Clipboard.HasFormat(CF_PNG_2) or Clipboard.HasPictureFormat then
    Exit(true);
  Exit(false);
end;

function KeyToDrawTool;
var
  dt: TDrawTool;

begin
  Result := dtNone;

  for dt in TDrawTool do
    if ToolShortcut[Ord(dt) + 1] = Key then
    begin
      Result := dt;
      Exit;
    end;
end;

function sdExecuteWithCanClose;
var
  OnCanClose: TCloseQueryEvent;
  Executed, CanClose: boolean;
  fn: string;

begin
  OnCanClose := sd.OnCanClose;
  while True do
  begin
    sd.OnCanClose := nil;
    fn := sd.FileName;
    if Pos(DirectorySeparator, fn) > 0 then
    begin
      sd.InitialDir := ExtractFilePath(fn);
      sd.FileName := ExtractFileName(fn);
    end;
    Executed := sd.Execute;
    sd.OnCanClose := OnCanClose;

    if not Executed then Exit(False);
    CanClose := True;
    if Assigned(OnCanClose) then OnCanClose(sd, CanClose);
    if CanClose then Exit(True);
  end;
end;

function MakeProgressString;
var
  i: integer;

begin
  Result := '[                    ]';
  for i := 0 to Round(q * 20) - 1 do
    Result[2 + i] := '*';
end;

procedure MenuItemToPopupMenu;
var
  i: integer;
  mi2: TMenuItem;

begin
  pm.Items.Clear;
  mi.Click;
  for i := 0 to mi.Count - 1 do
  begin
    mi2 := TMenuItem.Create(pm);

    mi2.Caption := mi[i].Caption;
    mi2.Enabled := mi[i].Enabled;
    mi2.Tag := mi[i].Tag;
    mi2.Visible := mi[i].Visible;
    mi2.OnClick := mi[i].OnClick;

    pm.Items.Add(mi2);
  end;
end;

function ReadAllText(s: TStream): string;
var
  c: char;
begin
  Result := '';
  while s.Read(c, 1) = 1 do
    Result += c;
end;

procedure BrutallyRemoveDir;
var
  sr: TSearchRecs;
  i: integer;

begin
  s := IncludeTrailingPathDelimiter(s);
  FindAll(s + '*', faAnyFile, sr);
  for i := 0 to Length(sr) - 1 do
  begin
    if (sr[i].Attr and faVolumeID) <> 0 then
      Continue;
    if (sr[i].Name = '') or (sr[i].Name[1] = '.') then
      Continue;
    if (sr[i].Attr and faDirectory) <> 0 then
      BrutallyRemoveDir(s + sr[i].Name)
    else
      DeleteFile(s + sr[i].Name);
  end;
  RemoveDir(s);
end;

function RunCommandAsync(folder, command: string; swo: TShowWindowOptions; params: array of string): boolean;
var
  p: TProcess;
  i: integer;

begin
  p := TProcess.Create(nil);
  try
    p.CurrentDirectory := folder;
    p.Executable := command;
    for i := 0 to Length(params)-1 do
    begin
      p.Parameters.Add(params[i]);
      {$IFDEF GFIEDEBUG}
        Log(params[i]);
      {$ENDIF}
    end;
    p.ShowWindow := swo;
    p.Options := [];

    try
      p.Execute;
      Result := true;
    except
      on e: Exception do
      begin
        {$IFDEF GFIEDEBUG}
          Log(e.ClassName);
          Log(e.Message);
        {$ENDIF}
        Result := false;
      end;
    end;
  finally
    p.Free;
  end;
end;

function RunCommandHidden(folder, command: string; params: array of string): integer;
var
  p: TProcess;
  i: integer;

begin
  p := TProcess.Create(nil);
  try
    p.CurrentDirectory := folder;
    p.Executable := command;
    for i := 0 to Length(params)-1 do
    begin
      p.Parameters.Add(params[i]);
      {$IFDEF GFIEDEBUG}
        Log(params[i]);
      {$ENDIF}
    end;
    p.ShowWindow := swoHIDE;
    p.Options := [poWaitOnExit
    {$IFDEF GFIEDEBUG}
    , poUsePipes
    {$ENDIF}
    ];

    try
      p.Execute;
      Result := p.ExitStatus;
    except
      on e: Exception do
      begin
        {$IFDEF GFIEDEBUG}
          Log(e.ClassName);
          Log(e.Message);
        {$ENDIF}
        Result := RunCommandHidden_Exception_ExitStatus;
      end;
    end;

{$IFDEF GFIEDEBUG}
    Log(ReadAllText(p.Output));
{$ENDIF}
  finally
    p.Free;
  end;
end;

procedure SetComboItems;
var
  i: integer;

begin
  i := cb.ItemIndex;
  cb.Items.Text := Items_Text;
  cb.ItemIndex := i;
end;

function SetDefaultExt;
var
  i: integer;

begin
  Result := fn;

  for i := Length(fn) downto 1 do
    if fn[i] = '.' then
      Exit
    else
    if fn[i] = DirectorySeparator then
      Break;

  // append extension
  Result := Result + ext;
end;

function TabAtPos;
var
  i: integer;
begin
  i := pc.TabIndexAtClientPos(p);
  if (i >= 0) and (i < pc.PageCount) then
    Exit(pc.Pages[i]);
  Exit(nil);
end;

procedure ToggleSpeedButton;
begin
  with Control do
  begin
    Down := not Down;
    Click;
  end;
end;

function ValidFileName;
var
  i: integer;
begin
  Result := UTF8toSys(s);
  for i := 1 to Length(Result) do
  begin
    if (Ord(Result[i]) >= 128)
      or ((Result[i] >= 'A') and (Result[i] <= 'Z'))
      or ((Result[i] >= 'a') and (Result[i] <= 'z'))
      or ((Result[i] >= '0') and (Result[i] <= '9'))
      or (Result[i] in [' ', '+', '-', '_', '$', '(', ')', ',', ';', '.'])
    then Continue;
    Result[i] := '_';
  end;
end;

function WinCanvasConvert;
begin
  Result := Rect(Min(r.Left, r.Right), Min(r.Top, r.Bottom),
    Max(r.Left, r.Right) + 1, Max(r.Top, r.Bottom) + 1);
end;

function WithoutAmp;
var
  i: integer;

begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] <> '&' then
      Result := Result + s[i];
end;

function WithoutExt;
var
  i: integer;

begin
  for i := Length(fn) downto 1 do
    if fn[i] = '.' then
    begin
      Result := Copy(fn, 1, i - 1);
      Exit;
    end
    else
    if fn[i] = DirectorySeparator then
      Break;

  // file has no extension
  Result := fn;
end;

procedure TSubtractRect.Subtract;
begin
  Count := 4;
  // left pane
  SetRect(Rects[0], r1.Left, r1.Top, Min(r1.Right, r2.Left), r1.Bottom);
  // right pane
  SetRect(Rects[1], Max(r1.Left, r2.Right), r1.Top, r1.Right, r1.Bottom);
  // top pane
  SetRect(Rects[2], Max(r1.Left, r2.Left), r1.Top, Min(r1.Right, r2.Right), Min(r1.Bottom, r2.Top));
  // bottom pane
  SetRect(Rects[3], Max(r1.Left, r2.Left), Max(r1.Top, r2.Bottom), Min(r1.Right, r2.Right), r1.Bottom);
end;

procedure prefLoad;
var
  ini: TIniFile;
  i: integer;

begin
  if FileExists(Config_Preferences) then
  begin
    ini := TIniFile.Create(Config_Preferences);
    try
      Pref_LanguagePack := ini.ReadString('x', 'LanguagePack', '');
      Pref_TestBackground := ini.ReadString('x', 'TestBackground', '');
      Pref_MaxWidth := ini.ReadInteger('x', 'MaxWidth', Default_Pref_MaxWidth);
      Pref_MaxHeight := ini.ReadInteger('x', 'MaxHeight', Default_Pref_MaxHeight);
      Pref_PNGLimit := ini.ReadInteger('x', 'PNGLimit', 0);
      Pref_Hatch.Color[0] := ini.ReadInteger('x', 'Hatch1', 0);
      Pref_Hatch.Color[1] := ini.ReadInteger('x', 'Hatch2', 0);
      Pref_clGrid[0] := ini.ReadInteger('x', 'Grid1', 0);
      Pref_clGrid[1] := ini.ReadInteger('x', 'Grid2', 0);
      Pref_clGrid2[0] := ini.ReadInteger('x', 'Grid2_1', 0);
      Pref_clGrid2[1] := ini.ReadInteger('x', 'Grid2_2', 0);
      Pref_DialogViewMode := ini.ReadInteger('x', 'DialogViewMode', 0);
      byte(Pref_MWA) := ini.ReadInteger('x', 'MWA', Ord(mwaZoom));
      Pref_SaveToolSettings := ini.ReadBool('x', 'SaveToolSettings', True);
      Pref_ShowStartupScreen := ini.ReadBool('x', 'ShowStartupScreen', True);
      Pref_FilterPreview := ini.ReadBool('x', 'FilterPreview', True);
      Pref_DisplayScriptWarning := ini.ReadBool('x', 'DisplayScriptWarning', True);

      // load custom colors
      for i := 0 to 15 do
        Pref_CustomColors[i] :=
          ini.ReadInteger('CustomColors', IntToStr(i), clBlack);
    finally
      ini.Free;
    end;
  end;

  if VerboseMode then Log('Preferences loaded');
end;

procedure prefSave;
var
  ini: TIniFile;
  i: integer;

begin
  ini := TIniFile.Create(Config_Preferences);
  try
    ini.WriteString('x', 'LanguagePack', Pref_LanguagePack);
    ini.WriteString('x', 'TestBackground', Pref_TestBackground);
    ini.WriteInteger('x', 'MaxWidth', Pref_MaxWidth);
    ini.WriteInteger('x', 'MaxHeight', Pref_MaxHeight);
    ini.WriteInteger('x', 'PNGLimit', Pref_PNGLimit);
    ini.WriteInteger('x', 'Hatch1', Pref_Hatch.Color[0]);
    ini.WriteInteger('x', 'Hatch2', Pref_Hatch.Color[1]);
    ini.WriteInteger('x', 'Grid1', Pref_clGrid[0]);
    ini.WriteInteger('x', 'Grid2', Pref_clGrid[1]);
    ini.WriteInteger('x', 'Grid2_1', Pref_clGrid2[0]);
    ini.WriteInteger('x', 'Grid2_2', Pref_clGrid2[1]);
    ini.WriteInteger('x', 'DialogViewMode', Pref_DialogViewMode);
    ini.WriteInteger('x', 'MWA', Ord(Pref_MWA));
    ini.WriteBool('x', 'SaveToolSettings', Pref_SaveToolSettings);
    ini.WriteBool('x', 'ShowStartupScreen', Pref_ShowStartupScreen);
    ini.WriteBool('x', 'FilterPreview', Pref_FilterPreview);
    ini.WriteBool('x', 'DisplayScriptWarning', Pref_DisplayScriptWarning);

    // save custom colors
    for i := 0 to 15 do
      ini.WriteInteger('CustomColors', IntToStr(i),
        Pref_CustomColors[i]);

  finally
    ini.Free;
  end;
end;

procedure wndPosLoad;
var
  ini: TIniFile;

  procedure wndLoad(const Section: string; Form: TForm;
    loadMaximized, loadSize, loadVisible: boolean);
  var
    Maximized: boolean;

  begin
    if loadMaximized then
    begin
      Maximized := ini.ReadBool(Section, 'Maximized', False);
      if Maximized then
        Form.WindowState := wsMaximized
      else
        Form.WindowState := wsNormal;
    end
    else
      Maximized := False;

    if not Maximized then
    begin
      Form.Left := ini.ReadInteger(Section, 'Left', 0);
      Form.Top := ini.ReadInteger(Section, 'Top', 0);
      if loadSize then
      begin
        Form.Width := ini.ReadInteger(Section, 'Width', 100);
        Form.Height := ini.ReadInteger(Section, 'Height', 100);
      end;
    end;

    if loadVisible then
      Form.Visible :=
        ini.ReadBool(Section, 'Visible', True);
  end;

begin
  if FileExists(Config_WindowPos) then
  begin
    ini := TIniFile.Create(Config_WindowPos);
    try
      wndLoad('Main', frmMain, True, True, False);
    finally
      ini.Free;
    end;
  end; // ini file exists
end;

procedure wndPosSave;
var
  ini: TIniFile;

  procedure wndSave(const Section: string; Form: TForm;
    saveMaximized, saveSize, saveVisible: boolean);
  var
    Maximized: boolean;

  begin
    Maximized := Form.WindowState = wsMaximized;

    if saveMaximized then
      ini.WriteBool(Section, 'Maximized', Maximized);

    if not (saveMaximized and Maximized) then
    begin
      ini.WriteInteger(Section, 'Left', Form.Left);
      ini.WriteInteger(Section, 'Top', Form.Top);
      if saveSize then
      begin
        ini.WriteInteger(Section, 'Width', Form.Width);
        ini.WriteInteger(Section, 'Height', Form.Height);
      end;
    end;

    if saveVisible then
      ini.WriteBool(Section, 'Visible', Form.Visible);
  end;

begin
  ini := TIniFile.Create(Config_WindowPos);
  try
    wndSave('Main', frmMain, True, True, False);
  finally
    ini.Free;
  end;
end;

// Used to determine app config dir
function _OnGetApplicationName: string;
begin
  Result := 'gfie';
end;

initialization
  OnGetApplicationName := _OnGetApplicationName;

  AppDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  DataDir := AppDir + 'Data' + DirectorySeparator;
  LanguageDir := AppDir + 'Language' + DirectorySeparator;
  if FileExists(DataDir + 'store-settings-here.txt') then
    // portable version -> store config in app dir
    ConfigDir := DataDir
  else
    // non-portable -> store settings at standard location
    ConfigDir := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
  ForceDirectories(ConfigDir);
  HelpDir := AppDir + 'Help' + DirectorySeparator;

  Config_Preferences := ConfigDir + 'pref.ini';
  Config_RecentFiles := ConfigDir + 'recent.txt';
  Config_WindowPos := ConfigDir + 'wndpos.ini';
  Config_ToolSettings := ConfigDir + 'toolset.ini';

  CF_PNG := RegisterClipboardFormat('image/png');
  CF_PNG_2 := RegisterClipboardFormat('PNG');
  CF_GFIE_INVERSION := RegisterClipboardFormat('GFIE inversion mask');
  CF_GFIE_SIZE := RegisterClipboardFormat('GFIE selection size');
  CF_GFIE_ANGLE := RegisterClipboardFormat('GFIE selection angle');

  // load resources
  bmToolbar := TBitmap.Create;
  pngLoadFromFileBM(bmToolbar, DataDir + 'toolbar.png', clForm);
  bmMiscGlyphs := TBitmap32.Create;
  pngLoadFromFile(bmMiscGlyphs, DataDir + 'misc.png');
  bmToolSettings := TBitmap.Create;
  pngLoadFromFileBM(bmToolSettings, DataDir + 'toolset.png', clForm);
  bmBrushShape := TBitmap32.Create;
  pngLoadFromFile(bmBrushShape, DataDir + 'brushshape.png');
  bmPattern := TBitmap32.Create;
  pngLoadFromFile(bmPattern, DataDir + 'pattern.png');
  bmEye := TBitmap32.Create;
  pngLoadFromFile(bmEye, DataDir + 'eye.png');
finalization
  FreeAndNil(bmEye);
  FreeAndNil(bmPattern);
  FreeAndNil(bmBrushShape);
  FreeAndNil(bmToolSettings);
  FreeAndNil(bmMiscGlyphs);
  FreeAndNil(bmToolbar);
end.

