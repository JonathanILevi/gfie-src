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
program gfie;

uses
  {$ifdef LINUX}
  cwstring,
  {$endif}
  Interfaces, SysUtils, Forms, Controls, Graphics,
  Main in 'Main.pas' {frmMain},
  dlgColor in 'ColorFrame.pas' {frmColor},
  ieShared in 'ieShared.pas',
  dlgToolbar in 'ToolbarFrame.pas' {frmToolbar},
  dlgDoc in 'dlgDoc.pas' {frmDoc},
  DocClass in 'DocClass.pas',
  dlgDocPage in 'dlgDocPage.pas' {frmDocPage},
  ColQuant in 'ColQuant.pas',
  dlgToolSet in 'ToolSetFrame.pas' {frmToolSet},
  dlgText in 'dlgText.pas' {frmText},
  dlgTransform in 'dlgTransform.pas' {frmTransform},
  ImageTransform in 'ImageTransform.pas',
  Filters in 'Filters.pas',
  dlgBlur in 'dlgBlur.pas' {frmBlur},
  dlgExposure in 'dlgExposure.pas' {frmExposure},
  dlgRGBChannels in 'dlgRGBChannels.pas' {frmRGBChannels},
  dlgHueSaturation in 'dlgHueSaturation.pas' {frmHueSaturation},
  dlgShadow in 'dlgShadow.pas' {frmShadow},
  dlgMatte in 'dlgMatte.pas' {frmRemoveMatte},
  dlgOpacity in 'dlgOpacity.pas' {frmOpacity},
  dlgCreateIcon in 'dlgCreateIcon.pas' {frmCreateIcon},
  dlgCreateAndroidIcon in 'dlgCreateAndroidIcon.pas' {frmCreateAndroidIcon},
  dlgTest in 'dlgTest.pas' {frmTest},
  PNG in 'PNG.pas',
  dlgPrint in 'dlgPrint.pas' {frmPrint},
  XPM in 'XPM.pas',
  dlgSplash in 'dlgSplash.pas' {frmSplash},
  dlgUnsharpMask in 'dlgUnsharpMask.pas' {frmUnsharpMask},
  dlgPreferences in 'dlgPreferences.pas' {frmPreferences},
  dlgGlow in 'dlgGlow.pas' {frmGlow},
  {$IFDEF WINDOWS} FileAssoc in 'FileAssoc.pas', {$ENDIF}
  dlgBevel in 'dlgBevel.pas' {frmBevel},
  ShellEx in 'ShellEx.pas',
  gfMath in 'gfMath.pas',
  LangPack in 'LangPack.pas',
  dlgBatchConvert in 'dlgBatchConvert.pas' {frmBatchConvert},
  {$IFDEF WINDOWS} DdeServer in 'DdeServer.pas', {$ENDIF}
  dlgLanguage in 'dlgLanguage.pas' {frmLanguage},
  ColSpaces in 'ColSpaces.pas',
  StreamEx in 'StreamEx.pas',
  RIFF in 'RIFF.pas',
  GIF in 'GIF.pas',
  dlgMetadata in 'dlgMetadata.pas' {frmMetadata},
  ResList in 'ResList.pas',
  dlgResProp in 'dlgResProp.pas' {frmResProp},
  FloatFormula in 'FloatFormula.pas',
  dlgFormulae in 'dlgFormulae.pas' {frmFormulae},
  BMP in 'BMP.pas',
  PCX in 'PCX.pas',
  Layers in 'Layers.pas',
  BlendModes in 'BlendModes.pas',
  ICO in 'ICO.pas',
  ANI in 'ANI.pas',
  UndoObject in 'UndoObject.pas',
  dlgLayerProp in 'dlgLayerProp.pas' {frmLayerProp},
  dlgLayers in 'dlgLayers.pas' {frmLayers},
  gfDataTree in 'gfDataTree.pas',
  GFIEFormat in 'GFIEFormat.pas',
  dlgLib in 'dlgLib.pas' {frmLib},
  dlgCellGrid in 'dlgCellGrid.pas' {frmCellGrid},
  fnvHash, BitmapEx, bmExUtils, gfcomp, printer4lazarus,
  lazopenglcontext, CIntf, Locales, Jpeg2000, ICNS, FilterDialog,
  dlgSaveOptions, FastDiv, dlgStartupFrame, dlgDebug, PixelFormats,
  dlgCreateMacIcon, dlgExeFormat, LResources, dlgScript, uPSI_AllMySources,
  Dialogs, ImageConverter_Intf, dlgSvgOpenOptions, Utils3d, NinePatch, Targa,
  PortableAnyMap;

{$IFDEF MSWINDOWS}
{$R *.res}
{$ENDIF}

var
  _iec: TieCursor;

const
  ShowSplash = True;

procedure PutPixel32_Opaque;
begin
  PutPixel32(cl32Red, cl32Transparent);
end;

procedure PutPixel32_Transparent;
begin
  PutPixel32(cl32Transparent, cl32Transparent);
end;

procedure Put32to24_Opaque;
begin
  Put32to24(clGray, cl32Green);
end;

procedure Put32to24_Transparent;
begin
  Put32to24(clGray, cl32Transparent);
end;

procedure Benchmark(const Name: string; p: TProcedure);
var
  i: integer;
  t: integer;
begin
  t := GetTickCount; // do not use GetTickCount64! Won't work on older systems.
  for i := 1 to 1000000 do
  begin
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
    p;p;p;p;p;p;p;p;p;p;
  end;
  ShowMessage(Name+': '+IntToStr(GetTickCount - t));
end;

procedure RunBenchmarks;
begin
  Benchmark('PutPixel32_Opaque', @PutPixel32_Opaque);
  Benchmark('PutPixel32_Transparent', @PutPixel32_Transparent);
  Benchmark('Put32to24_Opaque', @Put32to24_Opaque);
  Benchmark('Put32to24_Transparent', @Put32to24_Transparent);
end;

{$ifdef GFIEDEBUG}
procedure SetUpHeapTrace;
var
  f: file of char;
  fnTrace: string;
  size: int64;
begin
  fnTrace := ParamStr(0)+'.heaptrace.log';
  size := 0;
  if FileExists(fnTrace) then
  begin
    AssignFile(f, fnTrace);
    Reset(f);
    size := FileSize(f);
    CloseFile(f);
  end;
  if size > 1000 then
  begin
    useheaptrace := false;
    ShowMessage('There were memory leaks last time. Use Tools|Leak view in Lazarus.');
    halt(1);
  end;
  DeleteFile(fnTrace);
  SetHeapTraceOutput(fnTrace);
end;
{$endif}

begin
{$ifdef GFIEDEBUG}
  SetUpHeapTrace;
{$endif}

  Application.Title:='Greenfish Icon Editor Pro';
  DefaultFormatSettings.DecimalSeparator := '.';
  Randomize;

  Application.Initialize;

  {$I misc.lrs}
  // load cursor resources
  Screen.Cursors[crHandPoint] := LoadCursorFromLazarusResource('handpoint');
  for _iec in TieCursor do
    Screen.Cursors[ieCursorBase + Ord(_iec)] :=
      LoadCursorFromLazarusResource(ieCursorRes[_iec]);

  // Parameter /uninstall: remove system dependencies
  if UpperCase(ParamStr(1)) = '/UNINSTALL' then
  begin
{$IFDEF WINDOWS}
    UnAssociateAll;
{$ENDIF}
    Exit;
  end;
  VerboseMode := UpperCase(ParamStr(1)) = '/VERBOSE';

  Application.CreateForm(TfrmMain, frmMain);
  if ShowSplash then
  begin
    frmSplash := TfrmSplash.Create(Application);
    frmSplash.RedrawSplash;
  end;

  // load preferences
  try
    prefLoad;
  except
  end;

  // loaded
  if ShowSplash then
    frmSplash.tmFadeOut.Enabled := True;
  if VerboseMode then Log('About to run application');
  Application.Run;

  // save preferences
  try
    prefSave;
  except
  end;
end.
