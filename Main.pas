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
unit Main;

interface

uses
{$ifdef WINDOWS}
  Windows,
{$endif}
  LclIntf, LclType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  FileUtil, Dialogs, Menus, ExtCtrls, ComCtrls, StdCtrls,
  DocClass, ieShared, dlgDoc, dlgLib, ImageTransform, ShellEx, Math, LangPack,
  Layers, UndoObject, dlgToolbar, dlgLayers, dlgColor, dlgToolSet, DoubleBufPB,
  Accordion, bmExUtils, NumberEdit, dlgDebug, dlgCreateMacIcon, IniFiles,
  ResList, PNG, BitmapEx, Clipbrd, Process, StrUtils;

const
  nRecentFiles = 16;

type
  TmiContents = record
    mi: TMenuItem;
    Language: string;
	FileName: string;
  end;

  TTabSheetClass = class of TTabSheet;

  { TfrmMain }

  TfrmMain = class(TForm)
    accLayers: TAccordion;
    accColorPicker: TAccordion;
    ApplicationProperties1: TApplicationProperties;
    Bevel1: TBevel;
    Bevel2: TBevel;
    frmColor: TColorFrame;
    frmLayers: TLayersFrame;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miTabClose: TMenuItem;
    MenuItem4: TMenuItem;
    miTabCloseOthers: TMenuItem;
    miTabCopyFullPath: TMenuItem;
    miTabCopyFilename: TMenuItem;
    miTabCopyFolder: TMenuItem;
    MenuItem6: TMenuItem;
    miTabOpenFolder: TMenuItem;
    miTabOpenFolderCmd: TMenuItem;
    miTabName: TMenuItem;
    miScript: TMenuItem;
    miExportAndroid: TMenuItem;
    miCreateAndroidIcon: TMenuItem;
    miSaveACopy: TMenuItem;
    miPanelRight: TMenuItem;
    miPanelLeft: TMenuItem;
    N28: TMenuItem;
    miPageExportAll: TMenuItem;
    pc: TPageControl;
    pbStatusBar: TDoubleBufPB;
    frmToolbar: TToolbarFrame;
    mm: TMainMenu;
    miFile: TMenuItem;
    miNewGraphic: TMenuItem;
    miExit: TMenuItem;
    miIcon: TMenuItem;
    miPageNew: TMenuItem;
    miPageDelete: TMenuItem;
    miPageProp: TMenuItem;
    miEdit: TMenuItem;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    N3: TMenuItem;
    miCut: TMenuItem;
    miCopy: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    N4: TMenuItem;
    miSelectAll: TMenuItem;
    miDeselectAll: TMenuItem;
    miInvertSelection: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    miPrint: TMenuItem;
    N5: TMenuItem;
    miCreateWinIcon: TMenuItem;
    miCreateMacIcon: TMenuItem;
    N6: TMenuItem;
    N2: TMenuItem;
    miTest: TMenuItem;
    miSaveSelection: TMenuItem;
    miLoadSelection: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    miRecentFiles: TMenuItem;
    miClose: TMenuItem;
    N10: TMenuItem;
    miFilters: TMenuItem;
    miGrayscale: TMenuItem;
    miInvert: TMenuItem;
    miRGBChannels: TMenuItem;
    miHueSaturation: TMenuItem;
    miExposure: TMenuItem;
    miAverage: TMenuItem;
    miSoftBlur: TMenuItem;
    miBlurMore: TMenuItem;
    miCustomBlur: TMenuItem;
    miPaintContour: TMenuItem;
    miDropShadow: TMenuItem;
    N12: TMenuItem;
    pLeft: TPanel;
    pmTab: TPopupMenu;
    pRight: TPanel;
    N11: TMenuItem;
    miRemoveMatte: TMenuItem;
    N13: TMenuItem;
    miView: TMenuItem;
    miZoomIn: TMenuItem;
    miZoomOut: TMenuItem;
    mi100Percent: TMenuItem;
    miFitWindow: TMenuItem;
    N14: TMenuItem;
    miGrid: TMenuItem;
    N15: TMenuItem;
    miHelp: TMenuItem;
    miAbout: TMenuItem;
    N17: TMenuItem;
    N18: TMenuItem;
    miXformSel: TMenuItem;
    miSolarize: TMenuItem;
    miPasteAsPage: TMenuItem;
    miOpacity: TMenuItem;
    od: TOpenDialog;
    sdGraphic: TSaveDialog;
    miClearList: TMenuItem;
    N19: TMenuItem;
    miRevert: TMenuItem;
    N9: TMenuItem;
    N16: TMenuItem;
    miHomepage: TMenuItem;
    miUnsharpMask: TMenuItem;
    miSharpen: TMenuItem;
    N20: TMenuItem;
    miGlow: TMenuItem;
    miCloseAll: TMenuItem;
    miBevel: TMenuItem;
    miSaveAll: TMenuItem;
    miTransform: TMenuItem;
    N21: TMenuItem;
    miFlipHoriz: TMenuItem;
    miFlipVert: TMenuItem;
    miRotate90Left: TMenuItem;
    miRotate90Right: TMenuItem;
    miRotate180: TMenuItem;
    miStartupScreen: TMenuItem;
    miCenterLines: TMenuItem;
    miBatchConvert: TMenuItem;
    miUniformRate: TMenuItem;
    miCropTransparency: TMenuItem;
    miPageImport: TMenuItem;
    miPageExport: TMenuItem;
    miMetadata: TMenuItem;
    N22: TMenuItem;
    sdLibrary: TSaveDialog;
    miNewLibrary: TMenuItem;
    miLibrary: TMenuItem;
    miResAdd: TMenuItem;
    miResRemove: TMenuItem;
    miResReplace: TMenuItem;
    miResProp: TMenuItem;
    miExtractEdit: TMenuItem;
    miExtractSave: TMenuItem;
    N23: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    miViewPages: TMenuItem;
    miFormulae: TMenuItem;
    miLayers: TMenuItem;
    miLayerNew: TMenuItem;
    miLayerDupl: TMenuItem;
    miLayerDelete: TMenuItem;
    miLayerProp: TMenuItem;
    N26: TMenuItem;
    miMergeSelected: TMenuItem;
    miMergeVisible: TMenuItem;
    miFlattenImage: TMenuItem;
    N27: TMenuItem;
    miLayerFromSel: TMenuItem;
    miCellGrid: TMenuItem;
    miSettings: TMenuItem;
    miPreferences: TMenuItem;
    miLanguage: TMenuItem;
    miPasteAsDoc: TMenuItem;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure CDPageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure miCreateAndroidIconClick(Sender: TObject);
    procedure miExportAndroidClick(Sender: TObject);
    procedure miNewGraphicClick(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miPageExportAllClick(Sender: TObject);
    procedure miPanelLeftClick(Sender: TObject);
    procedure miPanelRightClick(Sender: TObject);
    procedure miSaveACopyClick(Sender: TObject);
    procedure miScriptClick(Sender: TObject);
    procedure miTabCloseClick(Sender: TObject);
    procedure miTabCloseOthersClick(Sender: TObject);
    procedure miTabCopyFilenameClick(Sender: TObject);
    procedure miTabCopyFolderClick(Sender: TObject);
    procedure miTabCopyFullPathClick(Sender: TObject);
    procedure miTabOpenFolderClick(Sender: TObject);
    procedure miTabOpenFolderCmdClick(Sender: TObject);
    procedure miUndoRedoClick(Sender: TObject);
    procedure miPageNewClick(Sender: TObject);
    procedure miPageDeleteClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miViewClick(Sender: TObject);
    procedure miFitWindowClick(Sender: TObject);
    procedure miGridClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
    procedure miDeselectAllClick(Sender: TObject);
    procedure miInvertSelectionClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miCopyClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miLoadSelectionClick(Sender: TObject);
    procedure miSaveSelectionClick(Sender: TObject);
    procedure miXformSelClick(Sender: TObject);
    procedure miIconClick(Sender: TObject);
    procedure miPasteAsPageClick(Sender: TObject);
    procedure miFlipRotateClick(Sender: TObject);
    procedure miGrayscaleClick(Sender: TObject);
    procedure miInvertClick(Sender: TObject);
    procedure miSolarizeClick(Sender: TObject);
    procedure miRGBChannelsClick(Sender: TObject);
    procedure miHueSaturationClick(Sender: TObject);
    procedure miExposureClick(Sender: TObject);
    procedure miAverageClick(Sender: TObject);
    procedure miFiltersClick(Sender: TObject);
    procedure miBlurMoreClick(Sender: TObject);
    procedure miCustomBlurClick(Sender: TObject);
    procedure miSoftBlurClick(Sender: TObject);
    procedure miRemoveMatteClick(Sender: TObject);
    procedure miPaintContourClick(Sender: TObject);
    procedure miDropShadowClick(Sender: TObject);
    procedure miOpacityClick(Sender: TObject);
    procedure miPagePropClick(Sender: TObject);
    procedure miCreateWinIconClick(Sender: TObject);
    procedure miCreateMacIconClick(Sender: TObject);
    procedure miTestClick(Sender: TObject);
    procedure miZoomInClick(Sender: TObject);
    procedure miZoomOutClick(Sender: TObject);
    procedure mi100PercentClick(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miCloseClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure miClearListClick(Sender: TObject);
    procedure miPrintClick(Sender: TObject);
    procedure miRevertClick(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miContentsClick(Sender: TObject);
    procedure miHomepageClick(Sender: TObject);
    procedure miUnsharpMaskClick(Sender: TObject);
    procedure miSharpenClick(Sender: TObject);
    procedure miPreferencesClick(Sender: TObject);
    procedure miGlowClick(Sender: TObject);
    procedure miCloseAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miBevelClick(Sender: TObject);
    procedure miSaveAllClick(Sender: TObject);
    procedure DialogFolderChange(Sender: TObject);
    procedure miStartupScreenClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure miCenterLinesClick(Sender: TObject);
    procedure miBatchConvertClick(Sender: TObject);
    procedure miLanguageClick(Sender: TObject);
    procedure miUniformRateClick(Sender: TObject);
    procedure miCropTransparencyClick(Sender: TObject);
    procedure miPageImportClick(Sender: TObject);
    procedure miPageExportClick(Sender: TObject);
    procedure miMetadataClick(Sender: TObject);
    procedure pbStatusBarPaint(Sender: TObject);
    procedure pcChange(Sender: TObject);
    procedure pcMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pcMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pLeftMouseEnter(Sender: TObject);
    procedure pmTabPopup(Sender: TObject);
    procedure sdGraphicCanClose(Sender: TObject; var CanClose: boolean);
    procedure sdLibraryCanClose(Sender: TObject; var CanClose: boolean);
    procedure miNewLibraryClick(Sender: TObject);
    procedure miLibraryClick(Sender: TObject);
    procedure miResAddClick(Sender: TObject);
    procedure miResRemoveClick(Sender: TObject);
    procedure miResReplaceClick(Sender: TObject);
    procedure miResPropClick(Sender: TObject);
    procedure miExtractEditClick(Sender: TObject);
    procedure miExtractSaveClick(Sender: TObject);
    procedure miViewPagesClick(Sender: TObject);
    procedure miFormulaeClick(Sender: TObject);
    procedure miLayersClick(Sender: TObject);
    procedure miLayerPropClick(Sender: TObject);
    procedure miLayerNewClick(Sender: TObject);
    procedure miLayerDuplClick(Sender: TObject);
    procedure miLayerDeleteClick(Sender: TObject);
    procedure miMergeLayersClick(Sender: TObject);
    procedure miLayerFromSelClick(Sender: TObject);
    procedure miCellGridClick(Sender: TObject);
    procedure miSupportClick(Sender: TObject);
    procedure miPasteAsDocClick(Sender: TObject);
  private
    PreviousAndroidFolder: string;

    rf: array[0..nRecentFiles - 1] of string;
    miRecentFile: array[0..nRecentFiles - 1] of TMenuItem;
    // Installed Help languages
    miContents: array of TmiContents;

    function GetTabVisible(TabClass: TTabSheetClass): boolean;
    procedure SetTabVisible(TabClass: TTabSheetClass; Value: boolean);
  public
    StatusBarText: string;
    StatusBarError: boolean;

    // Number of open documents and other tabs in total
    // pc.PageCount does not refresh often enough, so we need to count
    // the tabs ourselves
    NumberOfTabs: integer;
    TabSheetAtCursor: TTabSheet;

    // We create this component at runtime
    // (Frames in Lazarus designer do not always reflect changes)
    frmToolSet: TToolSetFrame;

    procedure ApplyLanguagePack;
    procedure SetStatus(Value: string; Error: boolean = False);

    function frmDocActive: TDocumentFrame;
    function frmGraphicActive: TGraphicFrame;
    function frmLibraryActive: TLibraryFrame;

    function AddTab(tab: TTabSheet; ShowTab: boolean): TTabSheet;
    // Returns '' on error.
    function GetTabFilename(tab: TTabSheet): string;
    function NewDocument(c: TDocumentFrameClass): TDocumentFrame;
    function NewGraphic: TGraphicFrame;
    function NewLibrary: TLibraryFrame;
    function DoCloseTab(const ts: TTabSheet): boolean;
    function DoCloseAll(tsDontClose: TTabSheet): boolean;
    // Useful for e.g. getting the startup screen
    function GetTabByClass(TabClass: TTabSheetClass): TTabSheet;

    procedure RedrawGraphicFrame(frmGraphic: TGraphicFrame);
    procedure RedrawAllGraphicFrames;

    function DoOpen(const fn: string): TDocumentFrame;
    procedure GetSaveGraphicFileNameAndType(var fn: string; var ft: TImageFileType);
    function sdGraphicExec(var fn: string; var ft: TImageFileType): boolean;
    procedure GetSaveLibraryFileNameAndType(var fn: string; var ft: TResFileType);
    function sdLibraryExec(var fn: string; var ft: TResFileType): boolean;
    procedure DoSave;
    function DoSaveGraphicAs(frm: TGraphicFrame; SaveACopy: boolean): boolean;
    function DoSaveLibraryAs(frm: TLibraryFrame; SaveACopy: boolean): boolean;
    procedure PasteAsNewDocument;

    // Recent files
    procedure rfUpdateMenu;
    procedure rfLoad;
    procedure rfSave;
    procedure rfAdd(const _FileName: string);
    procedure rfRemoveAt(index: integer);
    procedure rfMoveToFirst(index: integer);
    procedure rfClear;

    procedure miRecentFileClick(Sender: TObject);
    procedure ShowDefaultHelp;

    property TabVisible[TabClass: TTabSheetClass]: boolean
      read GetTabVisible write SetTabVisible;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dlgDocPage, dlgTransform,
  dlgBlur, dlgExposure, dlgRGBChannels, dlgHueSaturation,
  dlgShadow, dlgMatte, dlgOpacity, dlgCreateIcon,
  dlgCreateAndroidIcon, dlgTest, dlgPrint,
  dlgUnsharpMask, dlgPreferences, dlgGlow, dlgBevel, dlgStartupFrame,
  dlgBatchConvert, dlgLanguage, dlgMetadata, dlgFormulae,
  dlgCellGrid, dlgScript, ImageConverter_Intf;

{$R *.lfm}

procedure TfrmMain.ApplyLanguagePack;
var
  i: integer;
  s, fltAll, fltAllKnown, fltGraphicOpen, fltGraphicSave, fltLibraryOpen, fltLibrarySave: string;

begin
  miFile.Caption := lpGet('MI_FILE');
  miNewGraphic.Caption := lpGet('MI_FILE_NEW_GRAPHIC')+'...';
  miNewLibrary.Caption := lpGet('MI_FILE_NEW_LIBRARY');
  miOpen.Caption := lpGet('MI_FILE_OPEN')+'...';
  miRecentFiles.Caption := lpGet('MI_FILE_RECENT_FILES');
  miClearList.Caption := lpGet('MI_FILE_RF_CLEAR');
  miBatchConvert.Caption := lpGet('MI_FILE_BATCH_CONVERT')+'...';
  miClose.Caption := lpGet('MI_FILE_CLOSE');
  miCloseAll.Caption := lpGet('MI_FILE_CLOSE_ALL');
  miSave.Caption := lpGet('MI_FILE_SAVE');
  miSaveAs.Caption := lpGet('MI_FILE_SAVE_AS')+'...';
  miSaveACopy.Caption := lpGet('MI_FILE_SAVE_COPY')+'...';
  miSaveAll.Caption := lpGet('MI_FILE_SAVE_ALL');
  miExportAndroid.Caption := lpGet('MI_FILE_EXPORT_ANDROID')+'...';
  miRevert.Caption := lpGet('MI_FILE_REVERT');
  miMetadata.Caption := lpGet('MI_FILE_METADATA')+'...';
  miPrint.Caption := lpGet('MI_FILE_PRINT')+'...';
  miExit.Caption := lpGet('MI_FILE_EXIT');

  miEdit.Caption := lpGet('MI_EDIT');
  miCut.Caption := lpGet('MI_EDIT_CUT');
  miCopy.Caption := lpGet('MI_EDIT_COPY');
  miPaste.Caption := lpGet('MI_EDIT_PASTE');
  miPasteAsPage.Caption := lpGet('MI_EDIT_PASTE_PAGE');
  miPasteAsDoc.Caption := lpGet('MI_EDIT_PASTE_DOC');
  miDelete.Caption := lpGet('MI_EDIT_DELETE');
  miSelectAll.Caption := lpGet('MI_EDIT_SEL_ALL');
  miDeselectAll.Caption := lpGet('MI_EDIT_SEL_NONE');
  miInvertSelection.Caption := lpGet('MI_EDIT_SEL_INVERT');
  miLoadSelection.Caption := lpGet('MI_EDIT_SEL_LOAD')+'...';
  miSaveSelection.Caption := lpGet('MI_EDIT_SEL_SAVE')+'...';
  miCropTransparency.Caption := lpGet('MI_EDIT_CROP_TRANSPARENCY');
  miTransform.Caption := lpGet('MI_EDIT_TRANSFORM');
  miXformSel.Caption := lpGet('MI_EDIT_SEL_TRANSFORM')+'...';
  miFlipHoriz.Caption := lpGet('MI_EDIT_FLIP_HORIZ');
  miFlipVert.Caption := lpGet('MI_EDIT_FLIP_VERT');
  miRotate90Left.Caption := lpGet('MI_EDIT_ROTATE_LEFT');
  miRotate90Right.Caption := lpGet('MI_EDIT_ROTATE_RIGHT');
  miRotate180.Caption := lpGet('MI_EDIT_ROTATE_180');

  miView.Caption := lpGet('MI_VIEW');
  miZoomIn.Caption := lpGet('MI_VIEW_ZOOM_IN');
  miZoomOut.Caption := lpGet('MI_VIEW_ZOOM_OUT');
  mi100Percent.Caption := lpGet('MI_VIEW_100_PERCENT');
  miFitWindow.Caption := lpGet('MI_VIEW_FIT_WINDOW');
  miGrid.Caption := lpGet('MI_VIEW_GRID');
  miCellGrid.Caption := lpGet('MI_VIEW_CELL_GRID')+'...';
  miCenterLines.Caption := lpGet('MI_VIEW_CENTER_LINES');
  miViewPages.Caption := lpGet('MI_VIEW_PAGES');
  miPanelLeft.Caption := lpGet('MI_VIEW_PANEL_LEFT');
  miPanelRight.Caption := lpGet('MI_VIEW_PANEL_RIGHT');
  miStartupScreen.Caption := lpGet('MI_VIEW_STARTUP_SCREEN');

  miFilters.Caption := lpGet('MI_FLT');
  miGrayscale.Caption := lpGet('MI_FLT_GRAYSCALE');
  miInvert.Caption := lpGet('MI_FLT_INVERT');
  miSolarize.Caption := lpGet('MI_FLT_SOLARIZE');
  miRGBChannels.Caption := lpGet('MI_FLT_RGB')+'...';
  miHueSaturation.Caption := lpGet('MI_FLT_HS')+'...';
  miExposure.Caption := lpGet('MI_FLT_EXPOSURE')+'...';
  miFormulae.Caption := lpGet('MI_FLT_FORMULAE')+'...';
  miAverage.Caption := lpGet('MI_FLT_AVERAGE');
  miSoftBlur.Caption := lpGet('MI_FLT_BLUR_SOFT');
  miBlurMore.Caption := lpGet('MI_FLT_BLUR_MORE');
  miCustomBlur.Caption := lpGet('MI_FLT_BLUR_CUSTOM')+'...';
  miSharpen.Caption := lpGet('MI_FLT_SHARPEN');
  miUnsharpMask.Caption := lpGet('MI_FLT_UNSHARP_MASK')+'...';
  miRemoveMatte.Caption := lpGet('MI_FLT_REMOVE_MATTE')+'...';
  miOpacity.Caption := lpGet('MI_FLT_OPACITY')+'...';
  miPaintContour.Caption := lpGet('MI_FLT_PAINT_CONTOUR');
  miDropShadow.Caption := lpGet('MI_FLT_DROP_SHADOW')+'...';
  miGlow.Caption := lpGet('MI_FLT_GLOW')+'...';
  miBevel.Caption := lpGet('MI_FLT_BEVEL')+'...';
  miScript.Caption := lpGet('MI_FLT_SCRIPTING')+' [BETA] ...';

  miLayers.Caption := lpGet('MI_LAYERS');
  miLayerNew.Caption := lpGet('MI_LAYERS_NEW');
  miLayerDupl.Caption := lpGet('MI_LAYERS_DUPLICATE');
  miLayerDelete.Caption := lpGet('MI_LAYERS_DELETE');
  miLayerProp.Caption := lpGet('MI_LAYERS_PROP')+'...';
  miMergeSelected.Caption := lpGet('MI_LAYERS_MERGE_SELECTED');
  miMergeVisible.Caption := lpGet('MI_LAYERS_MERGE_VISIBLE');
  miFlattenImage.Caption := lpGet('MI_LAYERS_FLATTEN');
  miLayerFromSel.Caption := lpGet('MI_LAYERS_FROM_SEL');

  miIcon.Caption := lpGet('MI_ICON');
  miPageNew.Caption := lpGet('MI_ICON_PAGE_NEW')+'...';
  miPageDelete.Caption := lpGet('MI_ICON_PAGE_DELETE');
  miPageProp.Caption := lpGet('MI_ICON_PAGE_PROP')+'...';
  miPageImport.Caption := lpGet('MI_ICON_PAGE_IMPORT')+'...';
  miPageExport.Caption := lpGet('MI_ICON_PAGE_EXPORT')+'...';
  miPageExportAll.Caption := lpGet('MI_ICON_PAGE_EXPORT_ALL')+'...';
  miUniformRate.Caption := lpGet('MI_ICON_UNIFORM_RATE');
  miCreateWinIcon.Caption := lpGet('MI_ICON_CREATE_WIN')+'...';
  miCreateMacIcon.Caption := lpGet('MI_ICON_CREATE_MAC')+'...';
  miCreateAndroidIcon.Caption := lpGet('MI_ICON_CREATE_ANDROID')+'...';
  miTest.Caption := lpGet('MI_ICON_TEST')+'...';

  miLibrary.Caption := lpGet('MI_LIB');
  miResAdd.Caption := lpGet('MI_LIB_ADD')+'...';
  miResRemove.Caption := lpGet('MI_LIB_REMOVE');
  miResReplace.Caption := lpGet('MI_LIB_REPLACE')+'...';
  miResProp.Caption := lpGet('MI_LIB_PROP')+'...';
  miExtractEdit.Caption := lpGet('MI_LIB_EXTRACT_EDIT');
  miExtractSave.Caption := lpGet('MI_LIB_EXTRACT_SAVE')+'...';

  miSettings.Caption := lpGet('MI_SET');
  miPreferences.Caption := lpGet('MI_SET_PREFERENCES')+'...';
  s := lpGet('MI_SET_LANGUAGE');
  if s = 'Language' then miLanguage.Caption := s+'...' else
    miLanguage.Caption := s+' (Language)...';

  miHelp.Caption := lpGet('MI_HELP');
  for i := 0 to Length(miContents) - 1 do with miContents[i] do
    mi.Caption := lpGet('MI_HELP_CONTENTS') + ' (' + Language + ')...';
  miHomepage.Caption := lpGet('MI_HELP_HOMEPAGE');
  miAbout.Caption := lpGet('MI_HELP_ABOUT')+'...';

  miTabClose.Caption := lpGet('MI_TAB_CLOSE');
  miTabCloseOthers.Caption := lpGet('MI_TAB_CLOSE_OTHERS');
  miTabCopyFullPath.Caption := lpGet('MI_TAB_COPY_FULL_PATH');
  miTabCopyFilename.Caption := lpGet('MI_TAB_COPY_FILENAME');
  miTabCopyFolder.Caption := lpGet('MI_TAB_COPY_FOLDER');
  miTabOpenFolder.Caption := lpGet('MI_TAB_OPEN_FOLDER');
  miTabOpenFolderCmd.Caption := lpGet('MI_TAB_OPEN_FOLDER_CMD');

  fltAll := lpGet('FF_ALL') + ' (*.*)|*.*';
  fltAllKnown := lpGet('FF_ALL_KNOWN') + '|*.gfie;*.gfi;*.ico;*.cur;*.ani;'+
    '*.icns;*.png;*.xpm;*.bmp;*.jpg;*.jpeg;*.jpe;*.gif;*.tif;*.tiff;'+
    '*.jp2;*.j2k;*.jpf;*.jpx;*.pcx;*.tga;*.pbm;*.pgm;*.pnm;*.ppm;*.webp;*.svg;'+
    sExecExt+';*.res';
  fltGraphicSave := Format('%s (*.gfie;*.gfi)|*.gfie;*.gfi|' +
    '%s (*.ico)|*.ico|%s (*.cur)|*.cur|%s (*.ani)|*.ani|%s (*.icns)|*.icns|' +
    '%s (*.png)|*.png|%s (*.xpm)|*.xpm|%s (*.bmp)|*.bmp|' +
    '%s (*.jpg;*.jpeg;*.jpe)|*.jpg;*.jpeg;*.jpe|' +
    '%s (*.gif)|*.gif|%s (*.tif;*.tiff)|*.tif;*.tiff|%s (*.jp2;*.j2k;*.jpf;*.jpx)|*.jp2;*.j2k;*.jpf;*.jpx|' +
    '%s (*.pcx)|*.pcx|%s (*.tga)|*.tga|' +
    '%s (*.pbm)|*.pbm|%s (*.pgm)|*.pgm|%s (*.pnm)|*.pnm|%s (*.ppm)|*.ppm|%s (*.webp)|*.webp|',

    [lpGet('FF_GFIE'), lpGet('FF_ICO'), lpGet('FF_CUR'), lpGet('FF_ANI'), lpGet('FF_ICNS'),
     lpGet('FF_PNG'), lpGet('FF_XPM'), lpGet('FF_BMP'), lpGet('FF_JPEG'),
     lpGet('FF_GIF'), lpGet('FF_TIFF'), lpGet('FF_JPEG_2000'), lpGet('FF_PCX'), lpGet('FF_TGA'),
     lpGet('FF_PBM'), lpGet('FF_PGM'), lpGet('FF_PNM'), lpGet('FF_PPM'), lpGet('FF_WEBP')]);
  fltGraphicOpen := fltGraphicSave + Format('|%s (*.svg)|*.svg',
    [lpGet('FF_SVG')]);
  fltLibraryOpen := Format('%s|%s|%s (*.res)|*.res',
    [lpGet('FF_EXEC'), sExecExt, lpGet('FF_RES')]);
  fltLibrarySave := Format('%s (*.icl;*.dll)|*.icl;*.dll|%s (*.res)|*.res',
    [lpGet('FF_ICL'), lpGet('FF_RES')]);

  sdGraphic.Filter := fltAll + '|' + fltGraphicSave;
  sdLibrary.Filter := fltAll + '|' + fltLibrarySave;
  od.Filter := fltAll + '|' + fltAllKnown + '|' + fltGraphicOpen + '|' + fltLibraryOpen;

  od.Title := lpGet('MI_FILE_OPEN');
  sdGraphic.Title := lpGet('MI_FILE_SAVE');
  sdLibrary.Title := lpGet('MI_FILE_SAVE');

  // TODO: auto-invalidate the accordion control when Caption changes
  accLayers.Caption := lpGet('MI_VIEW_LAYERS');
  accLayers.Invalidate;
  accColorPicker.Caption := lpGet('MI_VIEW_COLOR_PICKER');
  accColorPicker.Invalidate;
end;

procedure TfrmMain.SetStatus(Value: string; Error: boolean = False);
var
  ls: TLayers;

begin
  // Add a warning, if needed
  if (Value <> '') and (frmGraphicActive <> nil) then
  begin
    ls := frmGraphicActive.Doc.Pages[frmGraphicActive.ImageIndex].Layers;
    if ls.LayerCount = 0 then
    begin
      Value += ' [' + lpGet('MSG_NO_LAYERS') + '!]';
      Error := true;
    end
    else
    if ls.SelectedCount = 0 then
    begin
      Value += ' [' + lpGet('MSG_NO_LAYERS_SELECTED') + '!]';
      Error := true;
    end;
  end;

  if (StatusBarText <> Value) or (StatusBarError <> Error) then
  begin
    StatusBarText := Value;
    StatusBarError := Error;
    pbStatusBar.Repaint;
  end;
end;

function TfrmMain.frmDocActive: TDocumentFrame;
var
  ts: TTabSheet;

begin
  ts := pc.ActivePage;
  if not (ts is TDocumentTab) then
    Exit(nil);
  Exit(TDocumentTab(ts).Frame);
end;

function TfrmMain.frmGraphicActive: TGraphicFrame;
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;
  if not (frm is TGraphicFrame) then
    Result := nil
  else
    Result := TGraphicFrame(frm);
end;

function TfrmMain.frmLibraryActive: TLibraryFrame;
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;
  if not (frm is TLibraryFrame) then
    Result := nil
  else
    Result := TLibraryFrame(frm);
end;

function TfrmMain.AddTab(tab: TTabSheet; ShowTab: boolean): TTabSheet;
begin
  tab.PageControl := pc;
  tab.TabVisible := ShowTab;
  if ShowTab then
  begin
    Inc(NumberOfTabs);
    pc.Visible := True;
    pc.ActivePage := tab;
  end;
  Result := tab;
end;

function TfrmMain.GetTabFilename(tab: TTabSheet): string;
var
  tsDoc: TDocumentTab;
  frm: TDocumentFrame;

begin
  Result := '';
  if tab is TDocumentTab then
  begin
    tsDoc := TDocumentTab(tab);
    frm := tsDoc.Frame;
    if frm <> nil then Result := frm.FileName;
  end;
end;

function TfrmMain.NewDocument(c: TDocumentFrameClass): TDocumentFrame;
var
  tab: TDocumentTab;

begin
  tab := TDocumentTab(AddTab(TDocumentTab.CreateTab(Self, c), True));
  Result := tab.Frame;
end;

function TfrmMain.NewGraphic: TGraphicFrame;
begin
  Result := TGraphicFrame(NewDocument(TGraphicFrame));
end;

function TfrmMain.NewLibrary: TLibraryFrame;
begin
  Result := TLibraryFrame(NewDocument(TLibraryFrame));
end;

function TfrmMain.DoCloseTab(const ts: TTabSheet): boolean;
begin
  if (ts = nil) or (ts.TabVisible = False) then
    Exit(True);
  if (ts is TDocumentTab) and (TDocumentTab(ts).Frame <> nil) then
    Result := TDocumentTab(ts).Frame.DocFrameCanClose
  else
    Result := True;

  // close
  if Result then
  begin
    // free the tab sheet for good, if it contained a document
    if ts is TDocumentTab then
      Application.ReleaseComponent(ts)
    else
      ts.TabVisible := False;

    Dec(NumberOfTabs);
    pc.Visible := NumberOfTabs <> 0;
  end;
end;

function TfrmMain.DoCloseAll(tsDontClose: TTabSheet): boolean;
var
  i: integer;
  ts: array of TTabSheet;

begin
  Result := True;
  SetLength(ts, pc.PageCount);
  for i := 0 to Length(ts) - 1 do
    ts[i] := pc.Pages[i];
  for i := 0 to Length(ts) - 1 do
  begin
    if ts[i] = tsDontClose then
      continue;
    if not DoCloseTab(ts[i]) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TfrmMain.GetTabByClass(TabClass: TTabSheetClass): TTabSheet;
var
  i: integer;

begin
  for i := 0 to pc.PageCount - 1 do
    if pc.Pages[i] is TabClass then
      Exit(pc.Pages[i]);
  Result := nil;
end;

procedure TfrmMain.RedrawGraphicFrame(frmGraphic: TGraphicFrame);
begin
  if frmGraphic = frmDocActive then
    frmGraphic.RedrawPaintBox
  else
    frmGraphic.NeedRedrawPaintBoxOnActivate := true;
end;

procedure TfrmMain.RedrawAllGraphicFrames;
var
  i: integer;
  frm: TDocumentFrame;

begin
  for i := 0 to pc.PageCount - 1 do
  begin
    if not (pc.Pages[i] is TDocumentTab) then
      Continue;

    frm := TDocumentTab(pc.Pages[i]).Frame;
    if frm is TGraphicFrame then
      RedrawGraphicFrame(TGraphicFrame(frm));
  end;
end;

function TfrmMain.DoOpen(const fn: string): TDocumentFrame;
var
  LoadOK: boolean;
  s: TStream;
  ift: TImageFileType;
  rft: TResFileType;
  Ext: string;

begin
  Result := nil;

  if not FileExists(fn) then
  begin
    ShowMessage(Format(lpGet('MSG_NOT_EXIST'), [SysToUTF8(fn)]));
    Exit;
  end;

  ift := iftNone;
  rft := rftNone;
  try
    s := TFileStream.Create(fn, fmOpenRead);
    try
      ift := DetectImageFileType(s);
      rft := DetectResFileType(s);
    finally
      s.Free;
    end;
  except
  end;

  if (ift = iftNone) and (rft = rftNone) then
  begin
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]));
    Exit;
  end;

  // Graphic file?
  if ift <> iftNone then
  begin
    if (ift in iftUseImageConverter) and not ImageConverterAvailable then
    begin
      if not ImageConverterInstalled then
        ShowMessage(Format(lpGet('MSG_PROGRAM_NEEDED'),
          ['Greenfish Image Converter',
          SysToUTF8(fn)]));
      if not ImageConverterFrameworkInstalled then
        ShowMessage(Format(lpGet('MSG_PROGRAM_NEEDED'),
          ['Microsoft .NET Framework '+IMAGE_CONVERTER_DOTNET_VERSION,
          SysToUTF8(fn)]));

      Exit;
    end;
    if (ift = iftWebp) and not webpInstalled then
    begin
      ShowMessage(Format(lpGet('MSG_PROGRAM_NEEDED'),
        ['WebP tools', SysToUTF8(fn)]));
      Exit;
    end;

    Result := NewGraphic;
    with TGraphicFrame(Result) do
    begin
      FileName := fn;
      LoadOK := DoLoad;
    end;
  end else
  // Library file
  begin
    Result := NewLibrary;
    with TLibraryFrame(Result) do
    begin
      FileName := fn;
      LoadOK := DoLoad;
    end;
  end;

  if not LoadOK then
    // cannot load file
  begin
    if not CanHandlePE and (exeDetectFormat(fn) = exePE) then
      ShowMessage(Format(lpGet('MSG_PE_NOT_SUPPORTED'), [SysToUTF8(fn)]))
    else
    begin
      if ift <> iftNone then Ext := iftDefaultExt[ift] else Ext := rftDefaultExt[rft];
      ShowMessage(Format(lpGet('MSG_INVALID_FILE_FORMAT'), [SysToUTF8(fn), Ext]));
    end;
    DoCloseTab(Result.Tab);
    Result := nil;
  end
  else
    // file was successfully loaded
    rfAdd(fn);
end;

procedure TfrmMain.GetSaveGraphicFileNameAndType(var fn: string; var ft: TImageFileType);
begin
  // File type: detected from file name, then from selected filter
  // File name: default extension is appended, if none was specified
  fn := UTF8ToSys(sdGraphic.FileName);
  ft := DetectImageFileTypeFromName(fn);

  if ft = iftNone then
  begin
    ft := TImageFileType(sdGraphic.FilterIndex - 1);
    fn := SetDefaultExt(fn, iftDefaultExt[ft]);
  end;

  if ft >= iftFirstReadOnly then
    ft := iftNone;
end;

function TfrmMain.sdGraphicExec(var fn: string; var ft: TImageFileType): boolean;
begin
  // Filter: set according to FileType
  // File name: if extension meets format, then it is omitted
  if (ft <> iftNone) and (UpperCase(ChangeFileExt(fn, iftDefaultExt[ft])) =
    UpperCase(fn)) then
    fn := ChangeFileExt(fn, '');

  sdGraphic.FileName := SysToUTF8(fn);
  sdGraphic.FilterIndex := 1 + Ord(ft);

  Result := sdExecuteWithCanClose(sdGraphic);
  if Result then
    GetSaveGraphicFileNameAndType(fn, ft);
end;

procedure TfrmMain.GetSaveLibraryFileNameAndType(var fn: string; var ft: TResFileType);
begin
  // File type: detected from file name, then from selected filter
  // File name: default extension is appended, if none was specified
  fn := UTF8ToSys(sdLibrary.FileName);
  ft := DetectResFileTypeFromName(fn);

  if ft = rftNone then
  begin
    ft := TResFileType(sdLibrary.FilterIndex - 1);
    fn := SetDefaultExt(fn, rftDefaultExt[ft]);
  end;
end;

function TfrmMain.sdLibraryExec(var fn: string; var ft: TResFileType): boolean;
begin
  // Filter: set according to FileType
  // File name: if extension meets format, then it is omitted
  if (ft <> rftNone) and (UpperCase(ChangeFileExt(fn, rftDefaultExt[ft])) =
    UpperCase(fn)) then
    fn := ChangeFileExt(fn, '');

  sdLibrary.FileName := SysToUTF8(fn);
  sdLibrary.FilterIndex := 1 + Ord(ft);

  Result := sdExecuteWithCanClose(sdLibrary);
  if Result then
    GetSaveLibraryFileNameAndType(fn, ft);
end;

procedure TfrmMain.DoSave;
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;

  if frm is TGraphicFrame then
    with TGraphicFrame(frm) do
    begin
      if (FileName = '') or (FileType = iftNone) then
        DoSaveGraphicAs(TGraphicFrame(frm), False)
      else
        DoSave(FileName, FileType, []);
    end
  else
  if frm is TLibraryFrame then
    with TLibraryFrame(frm) do
    begin
      if (FileName = '') or (FileType = rftNone) then
        DoSaveLibraryAs(TLibraryFrame(frm), False)
      else
        DoSave(FileName, FileType, []);
    end;
end;

function TfrmMain.DoSaveGraphicAs(frm: TGraphicFrame; SaveACopy: boolean): boolean;
var
  fn: string;
  ft: TImageFileType;
  dso: TDocumentSaveOptions;

begin
  Result := False;

  if frm = nil then
    Exit;

  fn := frm.FileName;
  ft := frm.FileType;
  if not sdGraphicExec(fn, ft) then
    Exit;

  if ft = iftNone then
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]))
  else
  begin
    if SaveACopy then dso := [dsoSaveACopy] else dso := [];
    if not frm.DoSave(fn, ft, dso) then
      Exit;

    // success
    Result := True;
    if not SaveACopy then rfAdd(frm.FileName);
  end; // known format
end;

function TfrmMain.DoSaveLibraryAs(frm: TLibraryFrame; SaveACopy: boolean): boolean;
var
  fn: string;
  ft: TResFileType;
  dso: TDocumentSaveOptions;

begin
  Result := False;

  if frm = nil then
    Exit;

  fn := frm.FileName;
  ft := frm.FileType;
  if not sdLibraryExec(fn, ft) then
    Exit;

  if ft = rftNone then
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]))
  else
  begin
    if SaveACopy then dso := [dsoSaveACopy] else dso := [];
    if not frm.DoSave(fn, ft, dso) then
      Exit;

    // success
    Result := True;
    if not SaveACopy then rfAdd(frm.FileName);
  end; // known format
end;

procedure TfrmMain.PasteAsNewDocument;
var
  f: TGraphicFrame;

begin
  if not IconEditorCanPaste then
    Exit;
  f := NewGraphic;
  f.DoEditPaste(True);
  f.pgDelete(0);
end;

procedure TfrmMain.rfUpdateMenu;
var
  i: integer;

begin
  // Update menu items
  for i := 0 to nRecentFiles - 1 do
    with miRecentFile[i] do
    begin
      Visible := rf[i] <> '';
      if Visible then
        Caption := Format('%d %s', [i + 1, SysToUTF8(rf[i])]);
    end;
end;

procedure TfrmMain.rfLoad;
var
  i: integer;
  ini: TIniFile;

begin
  if not FileExists(Config_RecentFiles) then
    rfClear
  else
  begin
    ini := TIniFile.Create(Config_RecentFiles);
    try
      for i := 0 to nRecentFiles - 1 do
        rf[i] := ini.ReadString('File'+IntToStr(i), 'Name', '');
    finally
      ini.Free;
    end;

    // invalidate
    rfUpdateMenu;
  end;
end;

procedure TfrmMain.rfSave;
var
  i: integer;
  ini: TIniFile;

begin
  ini := TIniFile.Create(Config_RecentFiles);
  try
    for i := 0 to nRecentFiles - 1 do
      ini.WriteString('File'+IntToStr(i), 'Name', rf[i]);
  finally
    ini.Free;
  end;
end;

procedure TfrmMain.rfAdd(const _FileName: string);
var
  i, m: integer;

begin
  // if the file already exists, move it to the top
  m := nRecentFiles - 1;
  for i := 0 to nRecentFiles - 1 do
    if rf[i] = _FileName then
    begin
      m := i;
      Break;
    end;

  // shift files
  // we cannot use Move! (because of managed strings)
  for i := m downto 1 do
    rf[i] := rf[i - 1];

  // insert record
  rf[0]  := _FileName;

  // invalidate
  rfUpdateMenu;
end;

procedure TfrmMain.rfRemoveAt(index: integer);
var
  i: integer;
begin
  for i := index to nRecentFiles - 2 do
    rf[i] := rf[i+1];
  rf[nRecentFiles-1] := '';

  rfUpdateMenu;
end;

procedure TfrmMain.rfMoveToFirst(index: integer);
var
  i: integer;
  t: string;
begin
  t := rf[index];
  for i := index downto 1 do
    rf[i] := rf[i-1];
  rf[0] := t;

  rfUpdateMenu;
end;

procedure TfrmMain.rfClear;
var
  i: integer;

begin
  // clear all records
  for i := 0 to nRecentFiles - 1 do
    rf[i] := '';

  // invalidate
  rfUpdateMenu;
end;

procedure TfrmMain.miRecentFileClick(Sender: TObject);
var
  index: integer;
begin
  index := (Sender as TMenuItem).Tag;
  if DoOpen(rf[index]) = nil then
    rfRemoveAt(index);
end;

procedure TfrmMain.ShowDefaultHelp;
var
  i: integer;
  mi: TMenuItem;

begin
  for i := 0 to Length(miContents) - 1 do
  begin
    mi := miContents[i].mi;
    if mi.ShortCut = VK_F1 then
    begin
      mi.Click;
      Break;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
const
  Padding = 2;

var
  i, j: integer;
  sr: TSearchRecs;
  fn: string;

begin
{$IFDEF CPU32}
  Caption := Caption + ' 32-bit';
{$ENDIF}
{$IFDEF CPU64}
  Caption := Caption + ' 64-bit';
{$ENDIF}
{$IFDEF GFIEDEBUG}
  Caption := Caption + ' [DEBUG]';
{$ENDIF}

  StatusBarText := '';
  StatusBarError := false;

  // create some components
  frmToolSet := TToolSetFrame.Create(Self);
  frmToolSet.Parent := pLeft;
  frmToolSet.Align := alClient;

  SetBounds(Padding, Padding, Screen.Width - Padding * 2, Screen.Height -
    50 - Padding * 2);
  WindowState := wsMaximized;
  frmColor.pc.ActivePage := frmColor.tsHSBMap;

  // create recent file menu items
  for i := nRecentFiles - 1 downto 0 do
  begin
    miRecentFile[i] := TMenuItem.Create(Self);

    with miRecentFile[i] do
    begin
      Tag := i;
      OnClick := miRecentFileClick;
    end;

    miRecentFiles.Insert(0, miRecentFile[i]);
  end;

  // create Help Contents menu items
  FindAll(HelpDir + '*', faDirectory, sr);
  for i := 0 to Length(sr) - 1 do
  begin
    if (sr[i].Name = '.') or (sr[i].Name = '..') then
      Continue;
    fn := HelpDir + sr[i].Name + DirectorySeparator + 'index.html';
    if not FileExists(fn) then
      Continue;

    j := Length(miContents);
    SetLength(miContents, j + 1);
    with miContents[j] do
    begin
      Language := sr[i].Name;
      FileName := fn;

      mi := TMenuItem.Create(Self);
      mi.Tag := j;
      if UpperCase(Language) = 'ENGLISH' then
        mi.ShortCut := VK_F1;
      mi.OnClick := miContentsClick;

      miHelp.Insert(0, mi);
    end;
  end;
end;

procedure TfrmMain.CDPageControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if not Assigned(frmGraphicActive) and (frmLayers.lbLastCount <> 0) then
    frmLayers.lb.InvalidateListBox;
  Done := True;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := DoCloseAll(nil);
  if CanClose and (frmScript<>nil) and frmScript.Visible then
    CanClose := frmScript.CloseQuery;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
begin
  for i := 0 to Length(FileNames) - 1 do
    DoOpen(UTF8ToSys(FileNames[i]));
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  NoShift: boolean;
//  newTabIndex: integer;
  gf: TGraphicFrame;
  dt: TDrawTool;

begin
  // let controls capture keys
  if ActiveControl is TCustomEdit then
    Exit;

  NoShift := (Shift * [ssAlt, ssCtrl, ssShift] = []);
  gf := frmGraphicActive;

  // not drawing?
  if (gf = nil) or (gf.ma = iemaNone) then
  begin
    if Shift = [ssCtrl] then
    begin
      if (Key = VK_PRIOR) or (Key = VK_NEXT) then
      begin
        // TODO does not work, maybe TPageControl is buggy
(*        newTabIndex := pc.TabIndex + IfThen(Key = VK_PRIOR, -1, 1);
        if (newTabIndex >= 0) and (newTabIndex < NumberOfTabs) then
          pc.ActivePage := pc.Pages[pc.TabToPageIndex(newTabIndex)];*)
      end;
    end else
    if NoShift then
    begin
      if Key < 256 then
      begin
        // tool shortcut?
        dt := KeyToDrawTool(char(Key));
        if dt <> dtNone then frmToolbar.DrawTool := dt;
      end; // simple char

      if gf <> nil then
        case Key of
          Ord('1')..Ord('9'): gf.Zoom := Key - Ord('0');
          Ord('0'): gf.Zoom := 10;
          VK_ADD: gf.sbZoomInClick(nil);
          VK_SUBTRACT: gf.sbZoomOutClick(nil);
          VK_MULTIPLY: gf.Zoom := 1;
          VK_DIVIDE: gf.zoomFitWindow;
          VK_ESCAPE: gf.DoDeselectAll;
        end; // if gf <> nil
    end; // if NoShift
  end; // not drawing

  // update cursor
  if gf <> nil then gf.pbUpdateCursor;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
var
  gf: TGraphicFrame;

begin
  gf := frmGraphicActive;
  if gf <> nil then
    gf.pbUpdateCursor;
end;

procedure TfrmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  SetStatus('');
end;

procedure TfrmMain.miCreateAndroidIconClick(Sender: TObject);
begin
  DoCreateForm(TfrmCreateAndroidIcon, frmCreateAndroidIcon);
  if frmGraphicActive <> nil then
    frmCreateAndroidIcon.Execute(frmGraphicActive);
end;

procedure TfrmMain.miExportAndroidClick(Sender: TObject);
var
  i, pass: integer;
  Doc: TIconDoc;
  error, folder, subfolder, fn, FilesToOverwrite: string;
  ar: TAndroidResolution;
  bm: TBitmap32;
  sr: TSearchRecs;

begin
  if frmGraphicActive = nil then Exit;

  Doc := frmGraphicActive.Doc;
  error := '';

  if Doc.Metadata.Title = '' then
    error += lpGet('MSG_EXPORT_ANDROID_NO_TITLE')+LineEnding;

  for i := 0 to Doc.PageCount - 1 do
  if not GetAndroidResolution(Doc.Pages[i].DPI, ar) then
  begin
    error += Format(lpGet('MSG_EXPORT_ANDROID_NO_DPI')+LineEnding,
      [AndroidDPIString]);
    Break;
  end;

  if error <> '' then
  begin
    error += LineEnding+lpGet('MSG_EXPORT_ANDROID_ERROR');
    ShowMessage(error);
    Exit;
  end;

  folder := BrowseForFolder(lpGet('MSG_EXPORT_ANDROID_SELECT_FOLDER'),
    PreviousAndroidFolder);
  if folder = '' then Exit;
  folder := IncludeTrailingPathDelimiter(folder);

  FindAll(folder + 'drawable-*', faDirectory, sr);
  if Length(sr) = 0 then
  begin
    ShowMessage(lpGet('MSG_EXPORT_ANDROID_INVALID_RES_FOLDER'));
    Exit;
  end;
  PreviousAndroidFolder := folder;

  FilesToOverwrite := '';
  for pass := 0 to 1 do
  begin
    // overwrite prompt?
    if (pass = 1) and (FilesToOverwrite <> '') then
    begin
      if (MessageDlg(lpGet('MSG_OVERWRITE_MULTIPLE')+LineEnding+FilesToOverwrite,
        mtConfirmation, [mbYes, mbNo], 0) <> mrYes) then
      Exit;
    end;

    for i := 0 to Doc.PageCount - 1 do
    begin
      GetAndroidResolution(Doc.Pages[i].DPI, ar);
      subfolder := folder + 'drawable-' + AndroidResolutionInfo[ar].Name +
        DirectorySeparator;
      fn := subfolder + ValidFileName(Doc.Metadata.Title) + '.png';

      case pass of
        // determine which files need to be overwritten
        0: if FileExists(fn) then FilesToOverwrite += LineEnding + fn;

        // actually save the files
        1: begin
          ForceDirectories(subfolder);
          bm := TBitmap32.Create;
          try
            bm.Assign(Doc.Pages[i].Layers);
            pngSaveToFile(bm, fn, PNG_COMPRESSION_HIGH);
          finally
            bm.Free;
          end;
        end;
      end; // case pass
    end; // for i
  end; // for pass
end;

procedure TfrmMain.miNewGraphicClick(Sender: TObject);
var
  st: TdpSettings;
  frm: TGraphicFrame;

begin
  st.Caption := lpGet('MI_FILE_NEW_GRAPHIC');
  st.showColors := False;
  st.showCreateFrom := False;
  st.showWhenResizing := False;
  st.Width := 48;
  st.Height := 48;
  st.FrameRate := 0;
  st.DPI := 0;

  DoCreateForm(TfrmDocPage, frmDocPage);
  if frmDocPage.Execute(st) then
  begin
    frm := NewGraphic;

    with frm.Doc.Pages[0] do
    begin
      Layers.Resize(st.Width, st.Height);
      FrameRate := st.FrameRate;
      DPI := st.DPI;
    end;

    frm.PageSizeChanged;
  end;
end;

procedure TfrmMain.FormDblClick(Sender: TObject);
begin
  TabVisible[TStartupScreenTab] := True;
end;

procedure TfrmMain.miEditClick(Sender: TObject);
var
  frm: TDocumentFrame;
  frmGraphic: TGraphicFrame;
  ls: TLayers;

begin
  frm := frmDocActive;
  frmGraphic := frmGraphicActive;
  if Assigned(frmGraphic) then
    ls := frmGraphic.Doc.Pages[frmGraphic.ImageIndex].Layers
  else
    ls := nil;

  miUndo.Enabled := Assigned(frm) and frm.CanUndo;
  if miUndo.Enabled then
    miUndo.Caption :=
      lpGet('MI_EDIT_UNDO') + ': ' + lpGet(TUndoObject(frm.UndoStack[0]).Caption)
  else
    miUndo.Caption := lpGet('MI_EDIT_CANT_UNDO');

  miRedo.Enabled := Assigned(frm) and frm.CanRedo;
  if miRedo.Enabled then
    miRedo.Caption :=
      lpGet('MI_EDIT_REDO') + ': ' + lpGet(TUndoObject(frm.RedoStack[0]).Caption)
  else
    miRedo.Caption := lpGet('MI_EDIT_CANT_REDO');

  miCut.Enabled := Assigned(frmGraphic) and (ls.SelState <> stNone);
  miCopy.Enabled := miCut.Enabled;
  miPaste.Enabled := IconEditorCanPaste;
  miPasteAsPage.Enabled := Assigned(frmGraphic) and IconEditorCanPaste;
  miPasteAsDoc.Enabled := IconEditorCanPaste;
  miDelete.Enabled := miCut.Enabled;

  miSelectAll.Enabled := Assigned(frmGraphic);
  miDeselectAll.Enabled := Assigned(frmGraphic) and (ls.SelState <> stNone);

  miInvertSelection.Enabled := Assigned(frmGraphic) and (ls.SelState = stSelecting);
  miLoadSelection.Enabled := Assigned(frmGraphic);
  miSaveSelection.Enabled := miInvertSelection.Enabled;

  miCropTransparency.Enabled := Assigned(frmGraphic);
  miTransform.Enabled := Assigned(frmGraphic);
  miXformSel.Enabled := Assigned(frmGraphic) and (ls.SelState <> stNone);
end;

procedure TfrmMain.miPageExportAllClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageExport(True);
end;

procedure TfrmMain.miPanelLeftClick(Sender: TObject);
begin
  pLeft.Visible := not pLeft.Visible;
end;

procedure TfrmMain.miPanelRightClick(Sender: TObject);
begin
  pRight.Visible := not pRight.Visible;
end;

procedure TfrmMain.miSaveACopyClick(Sender: TObject);
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;

  if frm is TGraphicFrame then
    DoSaveGraphicAs(TGraphicFrame(frm), True)
  else
  if frm is TLibraryFrame then
    DoSaveLibraryAs(TLibraryFrame(frm), True);
end;

procedure TfrmMain.miScriptClick(Sender: TObject);
begin
  DoCreateForm(TfrmScript, frmScript);
  frmScript.Show;
end;

procedure TfrmMain.miTabCloseClick(Sender: TObject);
begin
  DoCloseTab(TabSheetAtCursor);
end;

procedure TfrmMain.miTabCloseOthersClick(Sender: TObject);
begin
  DoCloseAll(TabSheetAtCursor);
end;

procedure TfrmMain.miTabCopyFilenameClick(Sender: TObject);
begin
  Clipboard.AsText := SysToUTF8(ExtractFileName(GetTabFilename(TabSheetAtCursor)));
end;

procedure TfrmMain.miTabCopyFolderClick(Sender: TObject);
begin
  Clipboard.AsText := SysToUTF8(ExtractFilePath(GetTabFilename(TabSheetAtCursor)));
end;

procedure TfrmMain.miTabCopyFullPathClick(Sender: TObject);
begin
  Clipboard.AsText := SysToUTF8(GetTabFilename(TabSheetAtCursor));
end;

procedure TfrmMain.miTabOpenFolderClick(Sender: TObject);
begin
  OpenDocument(SysToUTF8(ExtractFilePath(GetTabFilename(TabSheetAtCursor))));
end;

procedure TfrmMain.miTabOpenFolderCmdClick(Sender: TObject);
var
  dir: string;
begin
  dir := ExtractFilePath(GetTabFilename(TabSheetAtCursor));
{$ifdef windows}
  ShellExecute(Handle, 'open', 'cmd', '', PChar(dir), SW_SHOW);
{$else}
{$ifdef linux}
  RunCommandAsync(dir, 'gnome-terminal', swoShowDefault, ['--working-directory='+dir]);
{$else}
{$error not implemented}
{$endif}
{$endif}
end;

procedure TfrmMain.miUndoRedoClick(Sender: TObject);
begin
  if frmDocActive <> nil then
    frmDocActive.PerformUndoRedo(boolean((Sender as TMenuItem).Tag));
end;

procedure TfrmMain.miPageNewClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageNew;
end;

procedure TfrmMain.miPageDeleteClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageDelete;
end;

procedure TfrmMain.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.miViewClick(Sender: TObject);
var
  frm: TGraphicFrame;

begin
  frm := frmGraphicActive;

  miZoomIn.Enabled := Assigned(frm);
  miZoomOut.Enabled := Assigned(frm);
  mi100percent.Enabled := Assigned(frm);
  miFitWindow.Enabled := Assigned(frm);
  miGrid.Enabled := Assigned(frm);
  miCellGrid.Enabled := Assigned(frm);
  miCenterLines.Enabled := Assigned(frm);
  miViewPages.Enabled := Assigned(frm);

  if Assigned(frm) then
  begin
    miFitWindow.Checked := frm.sbZoomFit.Down;
    miGrid.Checked := frm.sbGrid.Down;
    miCellGrid.Checked := frm.cg.Enabled;
    miCenterLines.Checked := frm.sbCenterLines.Down;
    miViewPages.Checked := frm.pPages.Visible;
  end;

  miPanelLeft.Checked := pLeft.Visible;
  miPanelRight.Checked := pRight.Visible;
  miStartupScreen.Checked := TabVisible[TStartupScreenTab];
end;

procedure TfrmMain.miViewPagesClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoTogglePages;
end;

procedure TfrmMain.miFitWindowClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    ToggleSpeedButton(frmGraphicActive.sbZoomFit);
end;

procedure TfrmMain.miGridClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    ToggleSpeedButton(frmGraphicActive.sbGrid);
end;

procedure TfrmMain.miSelectAllClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoSelectAll;
end;

procedure TfrmMain.miDeselectAllClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoDeselectAll;
end;

procedure TfrmMain.miInvertSelectionClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoInvertSelection;
end;

procedure TfrmMain.miCutClick(Sender: TObject);
begin
  miCopy.Click;
  miDelete.Click;
end;

procedure TfrmMain.miCopyClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoEditCopy;
end;

procedure TfrmMain.miPasteClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoEditPaste(False)
  else
    PasteAsNewDocument;
end;

procedure TfrmMain.miDeleteClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoEditDelete;
end;

procedure TfrmMain.miLoadSelectionClick(Sender: TObject);
begin
  if (frmGraphicActive <> nil) and od.Execute then
    frmGraphicActive.DoLoadSelection(UTF8ToSys(od.FileName));
end;

procedure TfrmMain.miSaveSelectionClick(Sender: TObject);
var
  frm: TGraphicFrame;
  fn: string;
  ft: TImageFileType;

begin
  frm := frmGraphicActive;
  if frm = nil then Exit;

  fn := '';
  ft := iftPng;
  if not sdGraphicExec(fn, ft) then
    Exit;

  if ft = iftNone then
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]))
  else
    frm.DoSaveSelection(fn, ft);
end;

procedure TfrmMain.miXformSelClick(Sender: TObject);
begin
  DoCreateForm(TfrmTransform, frmTransform);
  if frmGraphicActive <> nil then
    frmTransform.Execute(frmGraphicActive);
end;

procedure TfrmMain.miIconClick(Sender: TObject);
var
  frm: TGraphicFrame;

begin
  frm := frmGraphicActive;

  miPageNew.Enabled := Assigned(frm);
  miPageDelete.Enabled := Assigned(frm) and (frm.Doc.PageCount > 1);
  miPageProp.Enabled := Assigned(frm);
  miPageImport.Enabled := Assigned(frm);
  miPageExport.Enabled := Assigned(frm);
  miPageExportAll.Enabled := Assigned(frm);
  miUniformRate.Enabled := Assigned(frm) and (frm.Doc.PageCount > 1);
  miCreateWinIcon.Enabled := Assigned(frm);
  miCreateMacIcon.Enabled := Assigned(frm);
  miCreateAndroidIcon.Enabled := Assigned(frm);
  miTest.Enabled := Assigned(frm);
end;

procedure TfrmMain.miPasteAsPageClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoEditPaste(True);
end;

procedure TfrmMain.miFlipRotateClick(Sender: TObject);
begin
  if (frmGraphicActive <> nil) then
    frmGraphicActive.DoFlipRotate(TSimpleFlipRotate((Sender as TComponent).Tag));
end;

procedure TfrmMain.miFormulaeClick(Sender: TObject);
begin
  DoCreateForm(TfrmFormulae, frmFormulae);
  if frmGraphicActive <> nil then
    frmFormulae.Execute(frmGraphicActive);
end;

procedure TfrmMain.miGrayscaleClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_Grayscale, False, 'MI_FLT_GRAYSCALE');
end;

procedure TfrmMain.miInvertClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_Invert, False, 'MI_FLT_INVERT');
end;

procedure TfrmMain.miSolarizeClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_Solarize, False, 'MI_FLT_SOLARIZE');
end;

procedure TfrmMain.miRGBChannelsClick(Sender: TObject);
begin
  DoCreateForm(TfrmRGBChannels, frmRGBChannels);
  if frmGraphicActive <> nil then
    frmRGBChannels.Execute(frmGraphicActive);
end;

procedure TfrmMain.miHueSaturationClick(Sender: TObject);
begin
  DoCreateForm(TfrmHueSaturation, frmHueSaturation);
  if frmGraphicActive <> nil then
    frmHueSaturation.Execute(frmGraphicActive);
end;

procedure TfrmMain.miExposureClick(Sender: TObject);
begin
  DoCreateForm(TfrmExposure, frmExposure);
  if frmGraphicActive <> nil then
    frmExposure.Execute(frmGraphicActive);
end;

procedure TfrmMain.miAverageClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_Average, False, 'MI_FLT_AVERAGE');
end;

procedure TfrmMain.miFiltersClick(Sender: TObject);
var
  frmGraphic: TGraphicFrame;
  b, l: boolean;

begin
  frmGraphic := frmGraphicActive;
  // is there a graphic document selected?
  b := frmGraphic <> nil;
  // is there at least one image layer selected?
  l := b and (frmGraphic.Doc.Pages[frmGraphic.ImageIndex].Layers.SelectedCount > 0);

  miGrayscale.Enabled := l;
  miInvert.Enabled := l;
  miSolarize.Enabled := l;
  miRGBChannels.Enabled := l;
  miHueSaturation.Enabled := l;
  miExposure.Enabled := l;
  miFormulae.Enabled := l;

  miAverage.Enabled := l;
  miSoftBlur.Enabled := l;
  miBlurMore.Enabled := l;
  miCustomBlur.Enabled := l;

  miSharpen.Enabled := l;
  miUnsharpMask.Enabled := l;

  miRemoveMatte.Enabled := l;
  miOpacity.Enabled := l;

  miPaintContour.Enabled := l;
  miDropShadow.Enabled := l;
  miGlow.Enabled := l;
  miBevel.Enabled := l;
end;

procedure TfrmMain.miBlurMoreClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_BlurMore, True, 'MI_FLT_BLUR_MORE');
end;

procedure TfrmMain.miCustomBlurClick(Sender: TObject);
begin
  DoCreateForm(TfrmBlur, frmBlur);
  if frmGraphicActive <> nil then
    frmBlur.Execute(frmGraphicActive);
end;

procedure TfrmMain.miSoftBlurClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_SoftBlur, True, 'MI_FLT_BLUR_SOFT');
end;

procedure TfrmMain.miRemoveMatteClick(Sender: TObject);
begin
  DoCreateForm(TfrmRemoveMatte, frmRemoveMatte);
  if frmGraphicActive <> nil then
    frmRemoveMatte.Execute(frmGraphicActive);
end;

procedure TfrmMain.miPaintContourClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_PaintContour, True, 'MI_FLT_PAINT_CONTOUR');
end;

procedure TfrmMain.miDropShadowClick(Sender: TObject);
var
  f: TGraphicFrame;
  ls: TLayers;
  bm: TBitmap32;

begin
  f := frmGraphicActive;
  if f <> nil then
  begin
    ls := f.Doc.Pages[f.ImageIndex].Layers;
    if ls.SelState <> stFloating then
    begin
      bm := TBitmap32.Create;
      try
        f.RenderUserBitmap(bm, ls.ClientRect, f.ImageIndex);
        if bm.IsOpaque then
        begin
          ShowMessage(lpGet('MSG_ERROR_FILTER_OPAQUE'));
          Exit;
        end;
      finally
        bm.Free;
      end;
    end;

    DoCreateForm(TfrmShadow, frmShadow);
    frmShadow.Execute(f);
  end;
end;

procedure TfrmMain.miOpacityClick(Sender: TObject);
begin
  DoCreateForm(TfrmOpacity, frmOpacity);
  if frmGraphicActive <> nil then
    frmOpacity.Execute(frmGraphicActive);
end;

procedure TfrmMain.miPagePropClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageProp;
end;

procedure TfrmMain.miCreateWinIconClick(Sender: TObject);
begin
  DoCreateForm(TfrmCreateIcon, frmCreateIcon);
  if frmGraphicActive <> nil then
    frmCreateIcon.Execute(frmGraphicActive);
end;

procedure TfrmMain.miCreateMacIconClick(Sender: TObject);
begin
  DoCreateForm(TfrmCreateMacIcon, frmCreateMacIcon);
  if frmGraphicActive <> nil then
    frmCreateMacIcon.Execute(frmGraphicActive);
end;

procedure TfrmMain.miTestClick(Sender: TObject);
begin
  DoCreateForm(TfrmTest, frmTest);
  if frmGraphicActive <> nil then
    frmTest.Execute(frmGraphicActive);
end;

procedure TfrmMain.miZoomInClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.sbZoomIn.Click;
end;

procedure TfrmMain.miZoomOutClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.sbZoomOut.Click;
end;

procedure TfrmMain.mi100PercentClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.sbZoom1.Click;
end;

procedure TfrmMain.miFileClick(Sender: TObject);
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;

  miClose.Enabled := Assigned(frm);
  miCloseAll.Enabled := Assigned(frm);
  miSave.Enabled := Assigned(frm);
  miSaveAs.Enabled := Assigned(frm);
  miSaveACopy.Enabled := Assigned(frm);
  miSaveAll.Enabled := Assigned(frm);
  miExportAndroid.Enabled := frm is TGraphicFrame;
  // The user may want to revert unmodified files.
  // E.g., if the file changed in the meanwhile.
  miRevert.Enabled := Assigned(frm) and (frm.FileName <> '');
  miMetadata.Enabled := frm is TGraphicFrame;
  miPrint.Enabled := frm is TGraphicFrame;
end;

procedure TfrmMain.miOpenClick(Sender: TObject);
var
  i: integer;

begin
  if od.Execute then
    for i := 0 to od.Files.Count - 1 do
      DoOpen(UTF8ToSys(od.Files[i]));
end;

procedure TfrmMain.miSaveClick(Sender: TObject);
begin
  DoSave;
end;

procedure TfrmMain.miCloseClick(Sender: TObject);
begin
  DoCloseTab(pc.ActivePage);
end;

procedure TfrmMain.miSaveAsClick(Sender: TObject);
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;

  if frm is TGraphicFrame then
    DoSaveGraphicAs(TGraphicFrame(frm), False)
  else
  if frm is TLibraryFrame then
    DoSaveLibraryAs(TLibraryFrame(frm), False);
end;

procedure TfrmMain.miClearListClick(Sender: TObject);
begin
  rfClear;
end;

procedure TfrmMain.miPrintClick(Sender: TObject);
begin
  DoCreateForm(TfrmPrint, frmPrint);
  if frmGraphicActive <> nil then
    frmPrint.Execute(frmGraphicActive);
end;

procedure TfrmMain.miRevertClick(Sender: TObject);
var
  frm: TDocumentFrame;

begin
  frm := frmDocActive;

  if (frm.FileName <> '') and (MessageDlg(Format(lpGet('MSG_CONFIRM_REVERT'),
    [frm.FileName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    if frm is TGraphicFrame then
      TGraphicFrame(frm).DoLoad
    else
    if frm is TLibraryFrame then
      TLibraryFrame(frm).DoLoad;
  end;
end;

procedure TfrmMain.miAboutClick(Sender: TObject);
begin
  OpenDocument(SysToUTF8(AppDir + 'readme.html'));
end;

procedure TfrmMain.miContentsClick(Sender: TObject);
begin
  OpenDocument(SysToUTF8(miContents[(Sender as TComponent).Tag].FileName));
end;

procedure TfrmMain.miHomepageClick(Sender: TObject);
begin
  OpenURL(sGreenfishHomepage);
end;

procedure TfrmMain.miUnsharpMaskClick(Sender: TObject);
begin
  DoCreateForm(TfrmUnsharpMask, frmUnsharpMask);
  if frmGraphicActive <> nil then
    frmUnsharpMask.Execute(frmGraphicActive);
end;

procedure TfrmMain.miSharpenClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      UseFilter(DoFilter_Sharpen, True, 'MI_FLT_SHARPEN');
end;

procedure TfrmMain.miPreferencesClick(Sender: TObject);
begin
  DoCreateForm(TfrmPreferences, frmPreferences);
  frmPreferences.UpdateForm;
  if frmPreferences.ShowModal = mrOk then
    frmPreferences.UpdateData;
end;

procedure TfrmMain.miGlowClick(Sender: TObject);
begin
  DoCreateForm(TfrmGlow, frmGlow);
  if frmGraphicActive <> nil then
    frmGlow.Execute(frmGraphicActive);
end;

procedure TfrmMain.miCloseAllClick(Sender: TObject);
begin
  DoCloseAll(nil);
end;

procedure TfrmMain.FormShow(Sender: TObject);
var
  i: integer;
  s: string;

begin
  if VerboseMode then Log('Showing TfrmMain...');
  pc.Visible := False;

  // create special tabs
  if VerboseMode then Log('Creating Startup screen');
  AddTab(TStartupScreenTab.Create(Self), Pref_ShowStartupScreen);
  if VerboseMode then Log('Startup screen created');

  try
    // load language pack
    lpLoad(LanguageDir + Pref_LanguagePack);
    if VerboseMode then Log('Language pack loaded');
    lpApplyToUI;
    if VerboseMode then Log('Language pack applied');
    // load recent file list from config
    rfLoad;
    if VerboseMode then Log('Recent files loaded');
    // load window positions
    wndPosLoad;
    if VerboseMode then Log('Window positions loaded');
  except
  end;
  if VerboseMode then Log('Finished loading attempts');

  // check parameter list
  for i := 1 to ParamCount do
  begin
    s := ParamStr(i);
    if (s <> '') and not (s[1] in ['-', '/']) then
      DoOpen(s);
  end;

  if VerboseMode then Log('TfrmMain shown');
end;

procedure TfrmMain.miBevelClick(Sender: TObject);
begin
  DoCreateForm(TfrmBevel, frmBevel);
  if frmGraphicActive <> nil then
    frmBevel.Execute(frmGraphicActive);
end;

procedure TfrmMain.miSaveAllClick(Sender: TObject);
var
  i: integer;
  frm: TDocumentFrame;

begin
  for i := 0 to pc.PageCount - 1 do
  begin
    if not (pc.Pages[i] is TDocumentTab) then
      Continue;

    frm := TDocumentTab(pc.Pages[i]).Frame;

    if frm is TGraphicFrame then
      with TGraphicFrame(frm) do
      begin
        if FileName <> '' then
        begin
          if not DoSave(FileName, FileType, []) then
            Break;
        end
        else
        if not DoSaveGraphicAs(TGraphicFrame(frm), False) then
          Break;
      end
    else
    if frm is TLibraryFrame then
      with TLibraryFrame(frm) do
      begin
        if FileName <> '' then
        begin
          if not DoSave(FileName, FileType, []) then
            Break;
        end
        else
        if not DoSaveLibraryAs(TLibraryFrame(frm), False) then
          Break;
      end;
  end;
end;

procedure TfrmMain.DialogFolderChange(Sender: TObject);
begin
  // We cannot do it at OnShow(), because the listview does not exist then
  DlgSetViewMode((Sender as TOpenDialog).Handle, dvm[Pref_DialogViewMode]);
end;

procedure TfrmMain.miStartupScreenClick(Sender: TObject);
begin
  TabVisible[TStartupScreenTab] := not TabVisible[TStartupScreenTab];
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    // save recent file list to Config
    rfSave;
    // save window positions: we cannot do it in ICONEDIT.DPR,
    // because this must be done when windows are not yet destroyed
    wndPosSave;
  except
  end;
end;

procedure TfrmMain.miCenterLinesClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    ToggleSpeedButton(frmGraphicActive.sbCenterLines);
end;

procedure TfrmMain.miBatchConvertClick(Sender: TObject);
begin
  DoCreateForm(TfrmBatchConvert, frmBatchConvert);
  frmBatchConvert.ShowModal;
end;

procedure TfrmMain.miLanguageClick(Sender: TObject);
begin
  DoCreateForm(TfrmLanguage, frmLanguage);
  frmLanguage.Execute;
end;

procedure TfrmMain.miUniformRateClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    with frmGraphicActive do
      DoUniformFrameRate(Doc.Pages[ImageIndex].FrameRate);
end;

procedure TfrmMain.miCropTransparencyClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoCropTransparency(True);
end;

procedure TfrmMain.miPageImportClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageImport;
end;

procedure TfrmMain.miPageExportClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoPageExport(False);
end;

procedure TfrmMain.miMetadataClick(Sender: TObject);
begin
  DoCreateForm(TfrmMetadata, frmMetadata);
  if frmGraphicActive <> nil then
    frmMetadata.Execute(frmGraphicActive);
end;

// A panel instead of a paintbox was too slow
procedure TfrmMain.pbStatusBarPaint(Sender: TObject);
var
  h: integer;
  c: TCanvas;

begin
  c := pbStatusBar.Canvas;

  c.Brush.Color := IfThen(StatusBarError, $c0c0f0, clForm);
  c.FillRect(c.ClipRect);
  c.Font.Assign(Font);
  h := c.TextHeight(StatusBarText);
  c.TextOut(4, (pbStatusBar.Height - h) div 2, StatusBarText);
end;

procedure TfrmMain.pcChange(Sender: TObject);
begin
  // active document changed
  if (frmGraphicActive <> nil) and frmGraphicActive.NeedRedrawPaintBoxOnActivate then
  begin
    frmGraphicActive.NeedRedrawPaintBoxOnActivate:=false;
    frmGraphicActive.RedrawPaintBox;
  end;
  frmLayers.lb.InvalidateListBox;
end;

procedure TfrmMain.pcMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  ScreenPos: TPoint;

begin
  TabSheetAtCursor := TabAtPos(pc, Point(X, Y));
  if TabSheetAtCursor = nil then Exit;

  case Button of
    mbLeft: if frmDocActive <> nil then
      frmDocActive.DocFrameActivate;
    mbMiddle: DoCloseTab(TabSheetAtCursor);
    mbRight: begin
      ScreenPos := pc.ClientToScreen(Point(X, Y));
      pmTab.PopUp(ScreenPos.X, ScreenPos.Y);
    end;
  end;
end;

procedure TfrmMain.pcMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  ts: TTabSheet;
  frm: TDocumentFrame;
  fn: string;

begin
  fn := '';

  ts := TabAtPos(pc, Point(X, Y));
  if ts is TDocumentTab then
  begin
    frm := TDocumentTab(ts).Frame;
    fn := frm.FileName;
  end;

  SetStatus(SysToUTF8(fn));
end;

procedure TfrmMain.pLeftMouseEnter(Sender: TObject);
begin
  frmToolSet.Visible := True;
end;

procedure TfrmMain.pmTabPopup(Sender: TObject);
var
  fn: string;
begin
  fn := GetTabFilename(TabSheetAtCursor);

  miTabName.Caption := StrUtils.IfThen(fn <> '', SysToUTF8(fn), TabSheetAtCursor.Caption);
  miTabCloseOthers.Enabled := NumberOfTabs > 1;
  miTabCopyFullPath.Enabled := fn <> '';
  miTabCopyFilename.Enabled := fn <> '';
  miTabCopyFolder.Enabled := fn <> '';
  miTabOpenFolder.Enabled := fn <> '';
  miTabOpenFolderCmd.Enabled := fn <> '';
end;

procedure TfrmMain.sdGraphicCanClose(Sender: TObject; var CanClose: boolean);
var
  fn: string;
  ft: TImageFileType;

begin
  GetSaveGraphicFileNameAndType(fn, ft);
  if ft = iftNone then
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]));
  CanClose := (ft <> iftNone) and (not FileExists(fn) or QueryOverwrite(fn));
end;

procedure TfrmMain.sdLibraryCanClose(Sender: TObject; var CanClose: boolean);
var
  fn: string;
  ft: TResFileType;

begin
  GetSaveLibraryFileNameAndType(fn, ft);
  if ft = rftNone then
    ShowMessage(Format(lpGet('MSG_UNKNOWN_FILE_TYPE'), [SysToUTF8(fn)]));
  CanClose := (ft <> rftNone) and (not FileExists(fn) or QueryOverwrite(fn));
end;

procedure TfrmMain.miNewLibraryClick(Sender: TObject);
begin
  NewDocument(TLibraryFrame);
end;

procedure TfrmMain.miLibraryClick(Sender: TObject);
var
  frm: TLibraryFrame;
  rSelected: boolean;

begin
  frm := frmLibraryActive;
  rSelected := Assigned(frm) and (frm.lb.SelectedCount <> 0);

  miResAdd.Enabled := Assigned(frm);
  miResRemove.Enabled := rSelected;
  miResReplace.Enabled := rSelected;
  miResProp.Enabled := rSelected;
  miExtractEdit.Enabled := rSelected;
  miExtractSave.Enabled := rSelected;
end;

procedure TfrmMain.miResAddClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoAdd;
end;

procedure TfrmMain.miResRemoveClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoRemove;
end;

procedure TfrmMain.miResReplaceClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoReplace;
end;

procedure TfrmMain.miResPropClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoProperties;
end;

procedure TfrmMain.miExtractEditClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoExtractEdit;
end;

procedure TfrmMain.miExtractSaveClick(Sender: TObject);
begin
  if frmLibraryActive <> nil then
    frmLibraryActive.DoExtractSave;
end;

procedure TfrmMain.miLayersClick(Sender: TObject);
var
  frm: TGraphicFrame;
  ls: TLayers;

begin
  frm := frmGraphicActive;
  if Assigned(frm) then
    ls := frm.Doc.Pages[frm.ImageIndex].Layers;

  miLayerNew.Enabled := Assigned(frm);
  miLayerDupl.Enabled := Assigned(frm) and (ls.SelectedCount > 0);
  miLayerDelete.Enabled := Assigned(frm) and (ls.SelectedCount > 0);
  miLayerProp.Enabled := Assigned(frm) and (ls.SelectedCount = 1);
  miMergeSelected.Enabled := Assigned(frm) and (ls.SelectedCount > 0);
  miMergeVisible.Enabled := Assigned(frm) and (ls.VisibleCount > 0);
  miFlattenImage.Enabled := Assigned(frm) and (ls.LayerCount > 0);
  miLayerFromSel.Enabled := Assigned(frm) and
    ((ls.SelState = stFloating) or ((ls.SelState = stSelecting) and (ls.SelectedCount <> 0)));
end;

procedure TfrmMain.miLayerPropClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerProp;
end;

procedure TfrmMain.miLayerNewClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerNew;
end;

procedure TfrmMain.miLayerDuplClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerDupl;
end;

procedure TfrmMain.miLayerDeleteClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerDelete;
end;

procedure TfrmMain.miMergeLayersClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerMerge(TLayerSubset((Sender as TComponent).Tag));
end;

procedure TfrmMain.miLayerFromSelClick(Sender: TObject);
begin
  if frmGraphicActive <> nil then
    frmGraphicActive.DoLayerFromSel;
end;

procedure TfrmMain.miCellGridClick(Sender: TObject);
begin
  DoCreateForm(TfrmCellGrid, frmCellGrid);
  if frmGraphicActive <> nil then
    frmCellGrid.Execute(frmGraphicActive);
end;

procedure TfrmMain.miSupportClick(Sender: TObject);
begin
  OpenURL(sGreenfishSupport);
end;

procedure TfrmMain.miPasteAsDocClick(Sender: TObject);
begin
  PasteAsNewDocument;
end;

function TfrmMain.GetTabVisible(TabClass: TTabSheetClass): boolean;
begin
  Result := GetTabByClass(TabClass).TabVisible;
end;

procedure TfrmMain.SetTabVisible(TabClass: TTabSheetClass; Value: boolean);
var
  tab: TTabSheet;

begin
  tab := GetTabByClass(TabClass);
  if tab.TabVisible <> Value then
  begin
    tab.TabVisible := Value;
    if Value then
    begin
      Inc(NumberOfTabs);
      pc.ActivePage := tab;
    end
    else
      Dec(NumberOfTabs);
    pc.Visible := NumberOfTabs <> 0;
  end;
end;

end.

