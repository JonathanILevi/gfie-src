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
unit dlgPreferences;

interface

uses
  {$IFDEF WINDOWS} DdeServer, FileAssoc, {$ENDIF} LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ieShared,
  LangPack, ComCtrls, NumberEdit, Math, AdjustControl;

type

  { TfrmPreferences }

  TfrmPreferences = class(TForm)
    bOK: TButton;
    bCancel: TButton;
    bReset: TButton;
    cbPBM: TCheckBox;
    cbPGM: TCheckBox;
    cbPNM: TCheckBox;
    cbPPM: TCheckBox;
    cbWEBP: TCheckBox;
    cbTGA: TCheckBox;
    cbSVG: TCheckBox;
    cbJpeg2000: TCheckBox;
    cbTIFF: TCheckBox;
    pc: TPageControl;
    tsEnvironment: TTabSheet;
    tsInstall: TTabSheet;
    gbColors: TGroupBox;
    sHatch1: TShape;
    sHatch2: TShape;
    bResetColors: TButton;
    lDVM: TLabel;
    cbDVM: TComboBox;
    gbFileAssoc: TGroupBox;
    cbICO: TCheckBox;
    cbCUR: TCheckBox;
    cbPNG: TCheckBox;
    cbXPM: TCheckBox;
    cbJPEG: TCheckBox;
    cbBMP: TCheckBox;
    tsMisc: TTabSheet;
    gbUsePNG: TGroupBox;
    cbUsePNG: TCheckBox;
    nePNGLimit: TNumberEdit;
    lTransparentHatch: TLabel;
    sGrid2: TShape;
    sGrid1: TShape;
    lGrid: TLabel;
    sGrid2_1: TShape;
    sGrid2_2: TShape;
    lGrid2: TLabel;
    cbANI: TCheckBox;
    cbSaveToolSettings: TCheckBox;
    cbMWA: TComboBox;
    lMWA: TLabel;
    cbGIF: TCheckBox;
    gbImageMax: TGroupBox;
    alMaxWidth: TAdjustLabel;
    neMaxWidth: TNumberEdit;
    alMaxHeight: TAdjustLabel;
    neMaxHeight: TNumberEdit;
    cbPCX: TCheckBox;
    cbGFIE: TCheckBox;
    lAdminMode: TLabel;
    cbICL: TCheckBox;
    cbICNS: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bResetClick(Sender: TObject);
    procedure bResetColorsClick(Sender: TObject);
    procedure nePNGLimitChange(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    // update the controls with the current Pref_... values
    procedure UpdateForm;
    // write info from controls to Pref_... vars
    procedure UpdateData;
  end;

var
  frmPreferences: TfrmPreferences;

// Remove all associations
procedure UnAssociateAll;

implementation

uses Main, dlgColor;

{$R *.lfm}

{$ifdef WINDOWS}
function IsAssociated(const Ext: string): boolean;
var
  r: TFileAssoc;

begin
  Result := GetFileAssoc(Ext, r) and
    (Pos(AnsiUpperCase(ParamStr(0)), AnsiUpperCase(r.cmdOpen)) <> 0);
end;

procedure SetAssociation(const Ext: string; SelfIcon, Value: boolean);
var
  r: TFileAssoc;

begin
  if IsAssociated(Ext) <> Value then
  if Value then
  begin
    // fill in TFileAssoc.fields
    r.TypeId := Application.Title + Ext + 'file';
    r.TypeDesc := r.TypeId;
    if SelfIcon then r.DefaultIcon := '%1' else
      r.DefaultIcon := ParamStr(0) + ',0';
    r.cmdOpen := ParamStr(0);

    r.ddeUse := True;
    r.ddeMacro := '[Open("%1")]';
    r.ddeServerName := sDDEServerName;
    r.ddeTopic := sDDETopic;

    // register
    SetFileAssoc(Ext, r);
  end else
    UndoFileAssoc(Ext);
end;
{$endif}

procedure UnAssociateAll;
begin
{$ifdef WINDOWS}
  try
    SetAssociation('.gfie', False, False);
    SetAssociation('.gfi', False, False);
    SetAssociation('.ico', True, False);
    SetAssociation('.cur', True, False);
    SetAssociation('.ani', True, False);
    SetAssociation('.icns', False, False);
    SetAssociation('.png', False, False);
    SetAssociation('.xpm', False, False);
    SetAssociation('.bmp', False, False);
    SetAssociation('.jpg', False, False);
    SetAssociation('.jpeg', False, False);
    SetAssociation('.jpe', False, False);
    SetAssociation('.gif', False, False);
    SetAssociation('.tiff', False, False);
    SetAssociation('.tif', False, False);
    SetAssociation('.jp2', False, False);
    SetAssociation('.j2k', False, False);
    SetAssociation('.jpf', False, False);
    SetAssociation('.jpx', False, False);
    SetAssociation('.pcx', False, False);
    SetAssociation('.tga', False, False);
    SetAssociation('.pbm', False, False);
    SetAssociation('.pgm', False, False);
    SetAssociation('.pnm', False, False);
    SetAssociation('.ppm', False, False);
    SetAssociation('.webp', False, False);
    SetAssociation('.svg', False, False);
    SetAssociation('.icl', False, False);
  except
    ShowMessage('Error removing file associations.'); // Do not localize
  end;
{$else}
{$warning UnAssociateAll: procedure is specific to Windows}
{$endif}
end;

procedure TfrmPreferences.UpdateForm;
begin
  neMaxWidth.Value := Pref_MaxWidth;
  neMaxHeight.Value := Pref_MaxHeight;
  nePNGLimit.Value := Min(256, Pref_PNGLimit);
  cbUsePNG.Checked := Pref_PNGLimit <= 256;

  sHatch1.Brush.Color := Pref_Hatch.Color[0];
  sHatch2.Brush.Color := Pref_Hatch.Color[1];
  sGrid1.Brush.Color := Pref_clGrid[0];
  sGrid2.Brush.Color := Pref_clGrid[1];
  sGrid2_1.Brush.Color := Pref_clGrid2[0];
  sGrid2_2.Brush.Color := Pref_clGrid2[1];
  cbDVM.ItemIndex := Pref_DialogViewMode;
  cbMWA.ItemIndex := Ord(Pref_MWA);
  cbSaveToolSettings.Checked := Pref_SaveToolSettings;

{$IFDEF WINDOWS}
  // load associations
  cbGFIE.Checked := IsAssociated('.gfie') and IsAssociated('.gfi');
  cbICO.Checked := IsAssociated('.ico');
  cbCUR.Checked := IsAssociated('.cur');
  cbANI.Checked := IsAssociated('.ani');
  cbICNS.Checked := IsAssociated('.icns');
  cbPNG.Checked := IsAssociated('.png');
  cbXPM.Checked := IsAssociated('.xpm');
  cbBMP.Checked := IsAssociated('.bmp');
  cbJPEG.Checked := IsAssociated('.jpg') and IsAssociated('.jpeg') and
    IsAssociated('.jpe');
  cbGIF.Checked := IsAssociated('.gif');
  cbTIFF.Checked := IsAssociated('.tiff') and IsAssociated('.tif');
  cbJpeg2000.Checked := IsAssociated('.jp2') and IsAssociated('.j2k') and
    IsAssociated('.jpf') and IsAssociated('.jpx');
  cbPCX.Checked := IsAssociated('.pcx');
  cbTGA.Checked := IsAssociated('.tga');
  cbPBM.Checked := IsAssociated('.pbm');
  cbPGM.Checked := IsAssociated('.pgm');
  cbPNM.Checked := IsAssociated('.pnm');
  cbPPM.Checked := IsAssociated('.ppm');
  cbWEBP.Checked := IsAssociated('.webp');
  cbSVG.Checked := IsAssociated('.svg');
  cbICL.Checked := IsAssociated('.icl');
{$ENDIF}
end;

procedure TfrmPreferences.UpdateData;
var
  i, j: integer;
  Changed_Hatch: boolean;

begin
  // update variables
  Pref_MaxWidth := Round(neMaxWidth.Value);
  Pref_MaxHeight := Round(neMaxHeight.Value);
  Pref_PNGLimit := IfThen(cbUsePNG.Checked, Round(nePNGLimit.Value), MaxInt);

  i := sHatch1.Brush.Color; j := sHatch2.Brush.Color;
  with Pref_Hatch do
  begin
    Changed_Hatch := (Color[0] <> i) or (Color[1] <> j);
    Color[0] := i; Color[1] := j;
  end;
  Pref_clGrid[0] := sGrid1.Brush.Color;
  Pref_clGrid[1] := sGrid2.Brush.Color;
  Pref_clGrid2[0] := sGrid2_1.Brush.Color;
  Pref_clGrid2[1] := sGrid2_2.Brush.Color;

  Pref_DialogViewMode := cbDVM.ItemIndex;
  Byte(Pref_MWA) := cbMWA.ItemIndex;
  Pref_SaveToolSettings := cbSaveToolSettings.Checked;

{$IFDEF WINDOWS}
  // save associations
  try
    SetAssociation('.gfie', False, cbGFIE.Checked);
    SetAssociation('.gfi', False, cbGFIE.Checked);
    SetAssociation('.ico', True, cbICO.Checked);
    SetAssociation('.cur', True, cbCUR.Checked);
    SetAssociation('.ani', True, cbANI.Checked);
    SetAssociation('.icns', False, cbICNS.Checked);
    SetAssociation('.png', False, cbPNG.Checked);
    SetAssociation('.xpm', False, cbXPM.Checked);
    SetAssociation('.bmp', False, cbBMP.Checked);
    SetAssociation('.jpg', False, cbJPEG.Checked);
    SetAssociation('.jpeg', False, cbJPEG.Checked);
    SetAssociation('.jpe', False, cbJPEG.Checked);
    SetAssociation('.gif', False, cbGIF.Checked);
    SetAssociation('.tiff', False, cbTIFF.Checked);
    SetAssociation('.tif', False, cbTIFF.Checked);
    SetAssociation('.jp2', False, cbJpeg2000.Checked);
    SetAssociation('.j2k', False, cbJpeg2000.Checked);
    SetAssociation('.jpf', False, cbJpeg2000.Checked);
    SetAssociation('.jpx', False, cbJpeg2000.Checked);
    SetAssociation('.pcx', False, cbPCX.Checked);
    SetAssociation('.tga', False, cbTGA.Checked);
    SetAssociation('.pbm', False, cbPBM.Checked);
    SetAssociation('.pgm', False, cbPGM.Checked);
    SetAssociation('.pnm', False, cbPNM.Checked);
    SetAssociation('.ppm', False, cbPPM.Checked);
    SetAssociation('.webp', False, cbWEBP.Checked);
    SetAssociation('.svg', False, cbSVG.Checked);
    SetAssociation('.icl', False, cbICL.Checked);
  except
    ShowMessage(lpGet('MSG_ERROR_ASSOC'));
  end;
{$ENDIF}

  // "OnChange" events

  // repaint child form, if hatch color/PNG limit has changed
  frmMain.RedrawAllGraphicFrames;

  // repaint other controls which display the hatch
  if Changed_Hatch then
  with frmMain.frmColor do
  begin
    pbForeColor.Invalidate;
    pbBackColor.Invalidate;
    sbAlpha.Invalidate;
  end;
end;

procedure TfrmPreferences.ShapeMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with Sender as TShape do Brush.Color := GFIEPickColor(Brush.Color);
end;

procedure TfrmPreferences.FormCreate(Sender: TObject);
begin
{$IFDEF WINDOWS}
  gbFileAssoc.Enabled := CanSetFileAssoc;
  lAdminMode.Visible := not gbFileAssoc.Enabled;
{$ELSE}
  tsInstall.TabVisible := False;
  lDVM.Visible := False;
  cbDVM.Visible := False;
{$ENDIF}
  pc.ActivePage := pc.Pages[0];
  ApplyLanguagePack;
end;

procedure TfrmPreferences.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmPreferences.bResetClick(Sender: TObject);
begin
  neMaxWidth.Value := Default_Pref_MaxWidth;
  neMaxHeight.Value := Default_Pref_MaxHeight;
  cbUsePNG.Checked := True;
  nePNGLimit.Value := 256;
  bResetColors.Click;
  cbDVM.ItemIndex := 3;
  cbSaveToolSettings.Checked := True;
end;

procedure TfrmPreferences.bResetColorsClick(Sender: TObject);
begin
  sHatch1.Brush.Color := clWhite;
  sHatch2.Brush.Color := clSilver;
  sGrid1.Brush.Color := clGray;
  sGrid2.Brush.Color := clSilver;
  sGrid2_1.Brush.Color := clMaroon;
  sGrid2_2.Brush.Color := clRed;
end;

procedure TfrmPreferences.nePNGLimitChange(Sender: TObject);
begin
  cbUsePNG.Checked := True;
end;

procedure TfrmPreferences.ApplyLanguagePack;
begin
  Caption := lpGet('MI_SET_PREFERENCES');

  tsMisc.Caption := lpGet('PREF_MISC');
  gbImageMax.Caption := lpGet('PREF_IMAGE_MAX');
  alMaxWidth.Caption := lpGet('LABEL_WIDTH')+':';
  alMaxHeight.Caption := lpGet('LABEL_HEIGHT')+':';
  gbUsePNG.Caption := lpGet('PREF_PNG_ICONS');
  cbUsePNG.Caption := lpGet('PREF_PNG_LIMIT');

  tsEnvironment.Caption := lpGet('PREF_ENVIRONMENT');
  gbColors.Caption := lpGet('PREF_COLORS');
  lTransparentHatch.Caption := lpGet('PREF_TRANSPARENT_HATCH')+':';
  lGrid.Caption := lpGet('MI_VIEW_GRID')+':';
  lGrid2.Caption := lpGet('PREF_GRID2')+':';
  bResetColors.Caption := lpGet('B_RESET');
  lDVM.Caption := lpGet('PREF_DVM')+':';
  SetComboItems(cbDVM, lpGet('PREF_DVM_ITEMS'));
  lMWA.Caption := lpGet('PREF_MWA')+':';
  SetComboItems(cbMWA, lpGet('PREF_MWA_ITEMS'));
  cbSaveToolSettings.Caption := lpGet('PREF_SAVE_TOOL_SETTINGS');

  tsInstall.Caption := lpGet('PREF_INSTALL');
  gbFileAssoc.Caption := lpGet('PREF_FILE_ASSOC');
  lAdminMode.Caption := lpGet('PREF_ADMIN');

  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.

