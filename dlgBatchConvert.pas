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
unit dlgBatchConvert;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms, FileUtil,
  Dialogs, StdCtrls, DocClass, dlgCreateIcon, LangPack, PixelFormats,
  ieShared, ComCtrls, ShellEx, BitmapEx, dlgDebug;

type

  { TfrmBatchConvert }

  TfrmBatchConvert = class(TForm)
    bSaveOptions: TButton;
    gbFiles: TGroupBox;
    lb: TListBox;
    bAdd: TButton;
    bClear: TButton;
    gbSettings: TGroupBox;
    cbFormat: TComboBox;
    lFormat: TLabel;
    bIconFormats: TButton;
    bOK: TButton;
    bCancel: TButton;
    pb: TProgressBar;
    cbOpen: TCheckBox;
    lFolder: TLabel;
    eFolder: TEdit;
    bFolder: TButton;
    procedure bClearClick(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure bSaveOptionsClick(Sender: TObject);
    procedure cbFormatChange(Sender: TObject);
    procedure bIconFormatsClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bFolderClick(Sender: TObject);
    procedure gbSettingsClick(Sender: TObject);
  private
    { Private declarations }
  public
    ci: TCreateIcon;

    procedure ApplyLanguagePack;
    function GetFileType: TImageFileType;
    procedure DoConversion;
  end;

var
  frmBatchConvert: TfrmBatchConvert;

implementation

uses Main, dlgSaveOptions;

{$R *.lfm}

procedure TfrmBatchConvert.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FILE_BATCH_CONVERT');

  gbFiles.Caption := lpGet('BC_FILES');
  bAdd.Caption := lpGet('B_ADD')+'...';
  bClear.Caption := lpGet('B_CLEAR');

  gbSettings.Caption := lpGet('BC_SETTINGS');
  lFormat.Caption := lpGet('BC_FORMAT')+':';
  SetComboItems(cbFormat, lpGet('FF_GFIE')+LineEnding+
    lpGet('FF_ICO')+LineEnding+lpGet('FF_CUR')+LineEnding+
    lpGet('FF_ANI')+LineEnding+lpGet('FF_ICNS')+LineEnding+
    lpGet('FF_PNG')+LineEnding+lpGet('FF_XPM')+LineEnding+
    lpGet('FF_BMP')+LineEnding+lpGet('FF_JPEG')+LineEnding+
    lpGet('FF_GIF')+LineEnding+
    lpGet('FF_TIFF')+LineEnding+
    lpGet('FF_JPEG_2000')+LineEnding+
    lpGet('FF_PCX')+LineEnding+
    lpGet('FF_TGA')+LineEnding+
    lpGet('FF_PBM')+LineEnding+
    lpGet('FF_PGM')+LineEnding+
    lpGet('FF_PNM')+LineEnding+
    lpGet('FF_PPM')+LineEnding+
    lpGet('FF_WEBP')+LineEnding);
  lFolder.Caption := lpGet('BC_FOLDER')+':';

  cbOpen.Caption := lpGet('BC_OPEN');
  bIconFormats.Caption := lpGet('BC_ICON_FORMATS')+'...';
  bSaveOptions.Caption := lpGet('SO_TITLE')+'...';

  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

function TfrmBatchConvert.GetFileType;
begin
  // +1: skip iftNone
  Result := TImageFileType(cbFormat.ItemIndex + 1);
end;

procedure TfrmBatchConvert.DoConversion;
var
  i, j, Index, sizeOpt, sizeCurr: integer;
  iftSrc, iftDest: TImageFileType;
  Doc: TIconDoc;
  bm: TBitmap32;
  sName, sExt, fn, SourceFileName: string;
  pfOpt, pfCurr: TPixelFormat32;
  dl: TieDataLosses;

begin
  Screen.Cursor := crHourglass;
  try
    pb.Max := lb.Count;
    pb.Position := 0;
    pb.Visible := True;

    iftDest := GetFileType;
    sExt := iftDefaultExt[iftDest];
    if (eFolder.Text <> '') and not DirectoryExistsUTF8(eFolder.Text) then
      MkDir(UTF8ToSys(eFolder.Text));

    Doc := TIconDoc.Create;
    try
      bm := TBitmap32.Create;
      try
        // for all selected files
        for i := 0 to lb.Items.Count - 1 do
        begin
          SourceFileName := UTF8ToSys(lb.Items[i]);

          pb.Position := pb.Position + 1;
          pb.Repaint;

          // open file
          iftSrc := Doc.LoadFromFile(SourceFileName,
            Pref_MaxWidth, Pref_MaxHeight, 1,
            lpGet('LY_BACKGROUND'));
          if iftSrc = iftNone then Continue;

          // generate destination file name
          if eFolder.Text = '' then sName := ExtractFilePath(SourceFileName) else
            sName := IncludeTrailingPathDelimiter(UTF8ToSys(eFolder.Text));
          sName := sName + WithoutExt(ExtractFileName(SourceFileName));
        
          fn := sName + sExt;
          Index := 1;
          while FileExists(fn) do
          begin
            inc(Index);
            fn := Format('%s (%d)%s', [sName, Index, sExt]);
          end;

          // generate destination file
          // animated -> static: select first page to keep
          if (iftSrc in [iftAni, iftGif]) and not (iftDest in [iftGfie, iftAni, iftGif]) then
            for j := Doc.PageCount - 1 downto 1 do Doc.DeletePage(j);

          // multipage icon -> not multipage icon: delete unnecessary pages
          if (iftSrc in [iftIco, iftCur, iftIcns]) and not
            (iftDest in [iftGfie, iftIco, iftCur, iftIcns]) then
          begin
            Index := 0;

            // keep page with maximal size and color depth
            bm.Assign(Doc.Pages[Index].Layers);
            sizeOpt := bm.Width * bm.Height;
            pfOpt := GetPixelFormat32(bm, palBW, palWin16, nil, nil);

            for j := 1 to Doc.PageCount - 1 do
            begin
              bm.Assign(Doc.Pages[j].Layers);
              sizeCurr := bm.Width * bm.Height;
              pfCurr := GetPixelFormat32(bm, palBW, palWin16, nil, nil);

              if (sizeCurr > sizeOpt) or
                ((sizeCurr = sizeOpt) and (pfCurr > pfOpt)) then
              begin
                Index := j;
                sizeOpt := sizeCurr;
                pfOpt := pfCurr;
              end;
            end; // for j

            // keep the best page and delete the others
            for j := 0 to Index - 1 do Doc.DeletePage(0);
            for j := Doc.PageCount - 1 downto 1 do Doc.DeletePage(j);
          end;

          // other -> ico, cur: create icon formats
          if not (iftSrc in [iftGfie, iftIco, iftCur]) and (iftDest in [iftIco, iftCur]) then
          begin
            bm.Assign(Doc.Pages[0].Layers);
            Doc.Clear;
            dlgCreateIcon.DoCreateIcon(bm, Doc, ci);
          end;

          DoCreateForm(TfrmSaveOptions, frmSaveOptions);
          Doc.SaveToFile(fn, iftDest, Pref_PNGLimit,
            ExtractOnlyFileName(fn),
            frmSaveOptions.neQuality.Value,
            frmSaveOptions.cbLossless.Checked,
            dl);
          if cbOpen.Checked then frmMain.DoOpen(fn);
        end; // for i
      finally
        bm.Free;
      end;
    finally
      Doc.Free;
    end;

    pb.Visible := False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmBatchConvert.bClearClick(Sender: TObject);
begin
  lb.Clear;
end;

procedure TfrmBatchConvert.bFolderClick(Sender: TObject);
var
  s: string;
begin
  s := BrowseForFolder('', eFolder.Text);
  if s <> '' then eFolder.Text := s;
end;

procedure TfrmBatchConvert.gbSettingsClick(Sender: TObject);
begin

end;

procedure TfrmBatchConvert.bAddClick(Sender: TObject);
var
  i: integer;
  s: string;
  
begin
  if frmMain.od.Execute then for i := 0 to frmMain.od.Files.Count - 1 do
  begin
    s := frmMain.od.Files[i];
    if lb.Items.IndexOf(s) < 0 then lb.Items.Add(s);
  end;
end;

procedure TfrmBatchConvert.bSaveOptionsClick(Sender: TObject);
var
  ft: TImageFileType;

begin
  ft := GetFileType;
  DoCreateForm(TfrmSaveOptions, frmSaveOptions);
  if ft in dlgSaveOptions.SupportedFileTypes then
    frmSaveOptions.Execute(nil, ft) else
    ShowMessage(lpGet('MSG_FT_NO_OPTIONS'));
end;

procedure TfrmBatchConvert.cbFormatChange(Sender: TObject);
var
  ft: TImageFileType;

begin
  ft := GetFileType;
  bIconFormats.Enabled := ft in [iftICO, iftCUR];
  bSaveOptions.Enabled := ft in dlgSaveOptions.SupportedFileTypes;
end;

procedure TfrmBatchConvert.bIconFormatsClick(Sender: TObject);
begin
  DoCreateForm(TfrmCreateIcon, frmCreateIcon);
  with frmCreateIcon do
  begin
    UpdateForm(ci);
    if ShowModal = mrOk then UpdateObject(ci);
  end;
end;

procedure TfrmBatchConvert.bOKClick(Sender: TObject);
begin
  if lb.Items.Count > 0 then
  begin
    DoConversion;
    ModalResult := mrOk;
  end else
    ShowMessage(lpGet('MSG_ADD_FILES'));
end;

procedure TfrmBatchConvert.FormShow(Sender: TObject);
var
  i: integer;
  
begin
  // move form to screen center only the first time
  Position := poDesigned;

  FillChar(ci, SizeOf(ci), 0);
  for i := 0 to fmtSizeCount - 1 do if (i <> 3) then
  begin
    ci[i, 1] := True;
    ci[i, 2] := True;
    ci[i, 4] := True;
  end;

  lb.Clear;
  cbFormatChange(cbFormat);
end;

procedure TfrmBatchConvert.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
  bFolder.Height := eFolder.Height;
  if VerboseMode then Log('TfrmBatchConvert created');
end;

end.
