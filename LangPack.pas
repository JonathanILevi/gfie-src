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
unit LangPack;

interface

uses
  LclIntf, LclType, FileUtil,
  SysUtils, Classes, PixelFormats, DocClass, ieShared, Forms, Controls, Dialogs,
  dlgDebug;

// when loading tool names from the language pack
const
  sExecExt = '*.exe;*.dll;*.scr;*.cpl;*.ocx;*.vbx;*.bpl;*.icl;*.il';

  ToolNameRes: array[TDrawTool] of string = ('SEL_RECT', 'SEL_ELLIPSE',
    'LASSO', 'WAND', 'SEL_PENCIL', 'TRANSFORM', 'CROP', 'HOTSPOT',
    'EYEDROPPER', 'RETOUCH', 'RECT', 'ELLIPSE', 'LINE', 'TEXT', 'PENCIL',
    'BRUSH', 'ERASER', 'RECOLOR', 'BUCKET', 'GRADIENT', 'NONE');

type
  TlpEntry = record
    Id: string;
    Data: string;
  end;

  TlpEntryArray = array of TlpEntry;
  Tpf32ToStr = array[TPixelFormat32] of string;
  TDrawToolToStr = array[TDrawTool] of string;
  TieDataLossToStr = array[TieDataLoss] of string;

var
  // database
  lpRes: array of TlpEntry;

  // strings loaded from lang. packs
  pf32ToStr: Tpf32ToStr;
  ToolName: TDrawToolToStr;
  MsgDataLoss: TieDataLossToStr;

function lpGet(const Id: string): string;
procedure lpClear;
procedure lpLoad(const fn: string);
procedure lpApplyToUI;
//function lpShortCutToText(ShortCut: TShortCut): string;

function QueryOverwrite(const FileName: string): boolean;
// Displays a "Do you want to save changes?" dialog
function QuerySaveChanges(const FileName: string): integer;

implementation

uses
  dlgBatchConvert, dlgBevel, dlgBlur, dlgCellGrid, dlgCreateIcon, dlgDoc, dlgDocPage,
  dlgExposure, dlgFormulae, dlgGlow, dlgHueSaturation, dlgLanguage, dlgLayerProp,
  dlgLib, Main, dlgMetadata, dlgOpacity, dlgPreferences, dlgPrint, dlgMatte,
  dlgResProp, dlgRGBChannels, dlgShadow,
  dlgTest, dlgText, dlgTransform, dlgUnsharpMask,
  dlgSaveOptions, dlgScript, dlgStartupFrame, dlgCreateMacIcon,
  dlgCreateAndroidIcon, dlgExeFormat, UndoObject;

function lpGet(const Id: string): string;
var
  Lo, Hi, Mid: integer;

begin
  Result := '[' + Id + ' not in language pack]';

  if (Length(lpRes) <> 0) and (Id >= lpRes[0].Id) and
    (Id <= lpRes[Length(lpRes) - 1].Id) then
  // find the corresponding entry using binary search
  begin
    Lo := 0; Hi := Length(lpRes) - 1;

    while Hi - Lo > 1 do
    begin
      Mid := (Lo + Hi) div 2;
      if Id < lpRes[Mid].Id then Hi := Mid - 1 else Lo := Mid;
    end;

    if lpRes[Lo].Id = Id then Result := lpRes[Lo].Data else
    if lpRes[Hi].Id = Id then Result := lpRes[Hi].Data;
  end;
//  if result[1]='[' then log(id+' is missing!');
end;

procedure lpLoadInternal(const fn: string);
var
  f: textfile;
  Count, i, j, p: integer;
  tmp: TlpEntry;
  s, key, value: string;
  Found: boolean;
  
begin
  Count := Length(lpRes);
  AssignFile(f, fn);
  Reset(f);
  try
    while not Eof(f) do
    begin
      Readln(f, s);
      p := Pos('=', s);
      if (p <> 0) and (s[1] <> ';') then
      begin
        key := Copy(s, 1, p-1);
        value := StringReplace(Copy(s, p+1, Length(s) - p),
            '<br>', LineEnding, [rfReplaceAll]);

        Found := false;
        for i := 0 to Count - 1 do
          if lpRes[i].Id = key then
        begin
          lpRes[i].Data := value;
          Found := true;
          break;
        end;

        if not Found then
        begin
          inc(Count);
          if Count > Length(lpRes) then SetLength(lpRes, Count or $ff);

          with lpRes[Count - 1] do
          begin
            Id := Copy(s, 1, p-1);
            Data := StringReplace(Copy(s, p+1, Length(s) - p),
              '<br>', LineEnding, [rfReplaceAll]);
          end;
        end;
      end; // if s is OK
    end; // while not eof
  finally
    CloseFile(f);
  end;
  SetLength(lpRes, Count);

  // sort entries by Id
  for i := 0 to Count - 2 do
  begin
    p := i;
    for j := i + 1 to Count - 1 do if lpRes[j].Id < lpRes[p].Id then p := j;
    if p <> i then
    begin
      tmp := lpRes[i];
      lpRes[i] := lpRes[p];
      lpRes[p] := tmp;
    end;
  end;
end;

procedure lpLoad(const fn: string);
var
  fnDefault: string;
begin
  lpClear;
  fnDefault := LanguageDir + Default_Pref_LanguagePack;
  if fnDefault <> fn then
    lpLoadInternal(fnDefault);
  lpLoadInternal(fn);
end;

function lpResourceIterator(Name, Value: string; Hash: integer; arg: Pointer): string;
begin
  // MessageDlg
  if Name = 'lclstrconsts.rsmbok' then Result := lpGet('B_OK') else
  if Name = 'lclstrconsts.rsmbcancel' then Result := lpGet('B_CANCEL') else
  if Name = 'lclstrconsts.rsmbyes' then Result := lpGet('B_YES') else
  if Name = 'lclstrconsts.rsmbno' then Result := lpGet('B_NO') else
  if Name = 'lclstrconsts.rsmball' then Result := lpGet('B_ALL') else
  if Name = 'lclstrconsts.rsmbclose' then Result := lpGet('B_CLOSE') else
  if Name = 'lclstrconsts.rsmtconfirmation' then Result := lpGet('MSG_CONFIRMATION') else
  // Menu key shortcuts
  if Name = 'lclstrconsts.smkcctrl' then Result := lpGet('KEY_CTRL')+'+' else
  if Name = 'lclstrconsts.smkcalt' then Result := lpGet('KEY_ALT')+'+' else
  if Name = 'lclstrconsts.smkcshift' then Result := lpGet('KEY_SHIFT')+'+' else
  if Name = 'lclstrconsts.smkcdel' then Result := lpGet('KEY_DEL') else
  if Name = 'lclstrconsts.smkcenter' then Result := lpGet('KEY_ENTER') else
    Result := '';
end;

procedure lpClear;
begin
  SetLength(lpRes, 0);
end;

procedure lpApplyToUI;
var
  i: integer;
  dt: TDrawTool;
  frm: TDocumentFrame;
  s: string;

begin
  // load strings from database
  // UI resource strings
  SetUnitResourceStrings('LclStrConsts', lpResourceIterator, nil);

  // pixel format names
  pf32ToStr[pf32_1bit] := lpGet('PF_1_BIT');
  pf32ToStr[pf32_4bit] := lpGet('PF_16_COLORS');
  pf32ToStr[pf32_8bit] := lpGet('PF_256_COLORS');
  pf32ToStr[pf32_24Bit] := lpGet('PF_24_BIT');
  pf32ToStr[pf32_32Bit] := lpGet('PF_32_BIT');

  // tool names
  for dt in TDrawTool do ToolName[dt] := lpGet('TOOL_' + ToolNameRes[dt]);

  // data loss messages
  MsgDataLoss[dlMultiPage] := lpGet('MSG_DL_MULTIPAGE');
  MsgDataLoss[dlLayers] := lpGet('MSG_DL_LAYERS');
  MsgDataLoss[dlSize256] := lpGet('MSG_DL_SIZE_256');
  MsgDataLoss[dlTransparency] := lpGet('MSG_DL_TRANSPARENCY');
  MsgDataLoss[dlColorDepth] := lpGet('MSG_DL_COLOR_DEPTH');
  MsgDataLoss[dlIcns] := lpGet('MSG_DL_ICNS');
  MsgDataLoss[dlError] := lpGet('MSG_DL_ERROR');

  if frmBatchConvert <> nil then
    frmBatchConvert.ApplyLanguagePack;
  if frmBevel <> nil then
    frmBevel.ApplyLanguagePack;
  if frmBlur <> nil then
    frmBlur.ApplyLanguagePack;
  if frmCellGrid <> nil then
    frmCellGrid.ApplyLanguagePack;

  // frmColor
  with frmMain.frmColor do
  begin
    Caption := lpGet('MI_VIEW_COLOR_PICKER');

    pbForeColor.Hint := lpGet('HINT_CP_FORE_COLOR');
    pbBackColor.Hint := lpGet('HINT_CP_BACK_COLOR');
    sbSwapColors.Hint := lpGet('HINT_CP_SWAP_COLORS');
    sbDefault.Hint := lpGet('HINT_CP_DEFAULT');
    sbTransparent.Hint := lpGet('HINT_CP_TRANSPARENT');
    sbInverted.Hint := lpGet('HINT_CP_INVERTED');

    //tsHSBMap.Caption := lpGet('TAB_HSB_MAP');
    //tsSwatches.Caption := lpGet('TAB_SWATCHES');
    iNextPage.Hint := lpGet('HINT_CP_TOGGLE_CHOOSER');
    Swatches.Hint := lpGet('HINT_CP_SWATCHES');
    sbSwatchLoad.Hint := lpGet('HINT_CP_SWATCH_LOAD');
    sbSwatchSave.Hint := lpGet('HINT_CP_SWATCH_SAVE');

    lRed.Caption := lpGet('LABEL_R')+':';
    lGreen.Caption := lpGet('LABEL_G')+':';
    lBlue.Caption := lpGet('LABEL_B')+':';
    lAlpha.Caption := lpGet('LABEL_A')+':';

    s := lpGet('HINT_CP_RED');
    lRed.Hint := s; sbRed.Hint := s; neRed.Hint := s;
    s := lpGet('HINT_CP_GREEN');
    lGreen.Hint := s; sbGreen.Hint := s; neGreen.Hint := s;
    s := lpGet('HINT_CP_BLUE');
    lBlue.Hint := s; sbBlue.Hint := s; neBlue.Hint := s;
    s := lpGet('HINT_CP_ALPHA');
    lAlpha.Hint := s; sbAlpha.Hint := s; neAlpha.Hint := s;
    s := lpGet('HINT_CP_HTML');
    lHTML.Hint := s; eHTML.Hint := s;

    imWCP.Hint := lpGet('HINT_CP_WCP');
  end;

  // frmCreateIcon
  if frmCreateIcon <> nil then
    frmCreateIcon.ApplyLanguagePack;
  if frmCreateMacIcon <> nil then
    frmCreateMacIcon.ApplyLanguagePack;
  if frmCreateAndroidIcon <> nil then
    frmCreateAndroidIcon.ApplyLanguagePack;

  // frmDoc
  for i := 0 to frmMain.pc.PageCount - 1 do
  begin
    if not (frmMain.pc.Pages[i] is TDocumentTab) then Continue;

    frm := TDocumentTab(frmMain.pc.Pages[i]).Frame;
    if frm = nil then Continue;
    if frm is TGraphicFrame then
      (frm as TGraphicFrame).ApplyLanguagePack else
    if frm is TLibraryFrame then
      (frm as TLibraryFrame).ApplyLanguagePack;
  end;

  if frmDocPage <> nil then
    frmDocPage.ApplyLanguagePack;
  if frmExeFormat <> nil then
    frmExeFormat.ApplyLanguagePack;
  if frmExposure <> nil then
    frmExposure.ApplyLanguagePack;
  if frmFormulae <> nil then
    frmFormulae.ApplyLanguagePack;
  if frmGlow <> nil then
    frmGlow.ApplyLanguagePack;
  if frmHueSaturation <> nil then
    frmHueSaturation.ApplyLanguagePack;
  if frmLanguage <> nil then
    frmLanguage.ApplyLanguagePack;
  if frmLayerProp <> nil then
    frmLayerProp.ApplyLanguagePack;

  // frmLayers
  with frmMain.frmLayers do
  begin
    Caption := lpGet('MI_VIEW_LAYERS');
    sbNew.Hint := lpGet('MI_LAYERS_NEW');
    sbDelete.Hint := lpGet('MI_LAYERS_DELETE');
    sbProp.Hint := lpGet('MI_LAYERS_PROP');
    sbMergeSelected.Hint := lpGet('MI_LAYERS_MERGE_SELECTED');
    lb.Hint := lpGet('MI_VIEW_LAYERS');
  end;

  // frmMain
  frmMain.ApplyLanguagePack;

  if frmMetadata <> nil then
    frmMetadata.ApplyLanguagePack;
  if frmOpacity <> nil then
    frmOpacity.ApplyLanguagePack;
  if frmPreferences <> nil then
    frmPreferences.ApplyLanguagePack;
  if frmPrint <> nil then
    frmPrint.ApplyLanguagePack;
  if frmRemoveMatte <> nil then
    frmRemoveMatte.ApplyLanguagePack;
  if frmResProp <> nil then
    frmResProp.ApplyLanguagePack;
  if frmRGBChannels <> nil then
    frmRGBChannels.ApplyLanguagePack;
  if frmSaveOptions <> nil then
    frmSaveOptions.ApplyLanguagePack;
  if frmScript <> nil then
    frmScript.ApplyLanguagePack;
  if frmShadow <> nil then
    frmShadow.ApplyLanguagePack;

  // StartupFrame
  with TStartupScreenTab(frmMain.GetTabByClass(TStartupScreenTab)) do
  begin
    Caption := lpGet('MI_VIEW_STARTUP_SCREEN');
    Frame.lTitle.Caption := lpGet('ST_WELCOME');
    Frame.lSubtitle.Caption := lpGet('ST_CHOOSE_ACTION')+':';
    Frame.imHelp.Hint := lpGet('HINT_STARTUP_HELP');
    Frame.bRecentFiles.Caption := lpGet('MI_FILE_RECENT_FILES');
    Frame.bRecentFiles.Hint := lpGet('ST_RECENT_INFO');
    Frame.cbShow.Caption := lpGet('ST_SHOW');
  end;

  if frmTest <> nil then
    frmTest.ApplyLanguagePack;
  if frmText <> nil then
    frmText.ApplyLanguagePack;

  // frmToolbar
  frmMain.frmToolbar.ApplyLanguagePack;

  // frmToolSet
  with frmMain.frmToolSet do
  begin
    Caption := lpGet('MI_VIEW_TOOL_BEHAVIOR');

    sbAntiAlias.Hint := lpGet('LABEL_ANTIALIAS');
    cbPattern.Hint := lpGet('TB_PATTERN');

    aiBrushSize.Hint := lpGet('TB_BRUSH_SIZE');
    neBrushSize.Hint := lpGet('TB_BRUSH_SIZE');
    cbBrushShape.Hint := lpGet('TB_BRUSH_SHAPE');

    aiLineWidth.Hint := lpGet('TB_LINE_WIDTH');
    neLineWidth.Hint := lpGet('TB_LINE_WIDTH');

    aiTolerance.Hint := lpGet('TB_TOLERANCE');
    neTolerance.Hint := lpGet('TB_TOLERANCE');

    sbContiguous.Hint := lpGet('TB_CONTIGUOUS');
    sbSampleAllLayers.Hint := lpGet('TB_SAMPLE_ALL_LAYERS');

    sbFramed.Hint := lpGet('TB_SHAPE_FRAMED');
    sbFilled.Hint := lpGet('TB_SHAPE_FILLED');

    sbEyedropperBack.Hint := lpGet('TB_EYEDROPPER_BACK');

    SetComboItems(cbRetouchMode, lpGet('TB_RETOUCH_ITEMS'));
    cbRetouchMode.Hint := lpGet('TB_RETOUCH_MODE');

    aiEraserAlpha.Hint := lpGet('TB_ERASER_STRENGTH');
    neEraserAlpha.Hint := lpGet('TB_ERASER_STRENGTH');

    sbLinear.Hint := lpGet('TB_G_LINEAR');
    sbRadial.Hint := lpGet('TB_G_RADIAL');
    sbConical.Hint := lpGet('TB_G_CONICAL');
    sbSpiral.Hint := lpGet('TB_G_SPIRAL');
    sbRepNone.Hint := lpGet('TB_REP_NONE');
    sbRepSym.Hint := lpGet('TB_REP_SYM');
    sbRepAsym.Hint := lpGet('TB_REP_ASYM');
    sbColor.Hint := lpGet('TB_MODE_COLOR');
    sbTransparency.Hint := lpGet('TB_MODE_TRANSPARENCY');
  end;

  if frmTransform <> nil then
    frmTransform.ApplyLanguagePack;
  if frmUnsharpMask <> nil then
    frmUnsharpMask.ApplyLanguagePack;
end;

function QueryOverwrite;
begin
  Result := (MessageDlg(Format(lpGet('MSG_OVERWRITE'),
    [SysToUTF8(FileName)]), mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

function QuerySaveChanges;
begin
  Result := MessageDlg(Format(lpGet('MSG_SAVE_CHANGES'),
    [SysToUTF8(FileName)]), mtConfirmation, [mbYes, mbNo, mbCancel], 0);
end;

end.
