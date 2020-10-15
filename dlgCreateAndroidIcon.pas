unit dlgCreateAndroidIcon;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AdjustControl, NumberEdit, dlgDoc, BitmapEx, DocClass;

type
  TAndroidResolution = (arLdpi, arMdpi, arHdpi, arXhdpi, arXXhdpi, arXXXhdpi);
  TAndroidResolutions = set of TAndroidResolution;
  PAndroidResolutionInfo = ^TAndroidResolutionInfo;
  TAndroidResolutionInfo = record
    Name: string;
    StandardDPI: integer;
  end;

const
  arDefault = arXhdpi;
  AndroidDPIUnit = 40;
  AndroidResolutionInfo: array[TAndroidResolution] of TAndroidResolutionInfo =
    ((Name: 'ldpi';  StandardDPI: 3*AndroidDPIUnit),
     (Name: 'mdpi';  StandardDPI: 4*AndroidDPIUnit),
     (Name: 'hdpi';  StandardDPI: 6*AndroidDPIUnit),
     (Name: 'xhdpi'; StandardDPI: 8*AndroidDPIUnit),
     (Name: 'xxhdpi'; StandardDPI: 12*AndroidDPIUnit),
     (Name: 'xxxhdpi'; StandardDPI: 16*AndroidDPIUnit));
  AndroidDPIString = '120, 160, 240, 320, 480, 640';

type
  TCreateAndroidIcon = record
    Name: string;
    CurrentDPI: double;
    CreateResolutions: set of TAndroidResolution;

    // Only input
    CurrentWidth: integer;
    CurrentHeight: integer;
  end;

  { TfrmCreateAndroidIcon }

  TfrmCreateAndroidIcon = class(TForm)
    alCurrentDPI: TAdjustLabel;
    bAll: TButton;
    bCancel: TButton;
    bNone: TButton;
    bOK: TButton;
    cbLdpi: TCheckBox;
    cbMdpi: TCheckBox;
    cbHdpi: TCheckBox;
    cbXhdpi: TCheckBox;
    cbXXhdpi: TCheckBox;
    cbXXXhdpi: TCheckBox;
    eName: TEdit;
    gbResolutions: TGroupBox;
    lName: TLabel;
    neCurrentDPI: TNumberEdit;
    procedure bAllNoneClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure neCurrentDPIChange(Sender: TObject);
  private
    CurrentWidth: integer;
    CurrentHeight: integer;
    cbResolution: array[TAndroidResolution] of TCheckBox;
  public
    procedure ApplyLanguagePack;
    procedure UpdateForm(const ci: TCreateAndroidIcon);
    procedure UpdateCaptions;
    procedure UpdateObject(var ci: TCreateAndroidIcon);
    function Execute(frmSrc: TGraphicFrame): TGraphicFrame;
  end;

var
  frmCreateAndroidIcon: TfrmCreateAndroidIcon;

function GetAndroidResolution(dpi: double; out ar: TAndroidResolution): boolean;
procedure DoCreateAndroidIcon(Src: TBitmap32; Doc: TIconDoc; const ci: TCreateAndroidIcon);

implementation

uses
  LangPack, ieShared, Main;

procedure DoCreateAndroidIcon;
var
  ar: TAndroidResolution;
  pg: TDocPage;
  bm: TBitmap32;
begin
  Doc.Metadata.Title := ci.Name;
  for ar in ci.CreateResolutions do
  begin
    pg := Doc.NewPage;
    pg.DPI := AndroidResolutionInfo[ar].StandardDPI;
    bm := TBitmap32.Create;
    try
      bm.Resize(Round(ci.CurrentWidth / ci.CurrentDPI * pg.DPI),
        Round(ci.CurrentHeight / ci.CurrentDPI * pg.DPI));
      bm.TransformDraw(bm.ClientRect, 0, Src, true);
      pg.Layers.Assign(bm);
    finally
      bm.Free;
    end;
  end;
end;

function GetAndroidResolution;
begin
  for ar in TAndroidResolution do
    if AndroidResolutionInfo[ar].StandardDPI = dpi then Exit(true);
  Exit(false);
end;

{$R *.lfm}

{ TfrmCreateAndroidIcon }

procedure TfrmCreateAndroidIcon.bAllNoneClick(Sender: TObject);
var
  ar: TAndroidResolution;
begin
  for ar in TAndroidResolution do
    cbResolution[ar].Checked:=(Sender = bAll);
end;

procedure TfrmCreateAndroidIcon.bOKClick(Sender: TObject);
var
  ci: TCreateAndroidIcon;

begin
  UpdateObject(ci);
  if ci.Name = '' then
  begin
    ShowMessage(lpGet('CAI_ERROR_EMPTY_ID'));
    Exit;
  end;
  if ci.CreateResolutions = [] then
  begin
    ShowMessage(lpGet('CAI_ERROR_NO_RESOLUTIONS'));
    Exit;
  end;

  ModalResult:=mrOk;
end;

procedure TfrmCreateAndroidIcon.FormCreate(Sender: TObject);
begin
  cbResolution[arLdpi] := cbLdpi;
  cbResolution[arMdpi] := cbMdpi;
  cbResolution[arHdpi] := cbHdpi;
  cbResolution[arXhdpi] := cbXhdpi;
  cbResolution[arXXhdpi] := cbXXhdpi;
  cbResolution[arXXXhdpi] := cbXXXhdpi;
  ApplyLanguagePack;
end;

procedure TfrmCreateAndroidIcon.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmCreateAndroidIcon.neCurrentDPIChange(Sender: TObject);
begin
  UpdateCaptions;
end;

procedure TfrmCreateAndroidIcon.ApplyLanguagePack;
var
  ar: TAndroidResolution;
begin
  Caption := lpGet('MI_ICON_CREATE_ANDROID');
  lName.Caption := lpGet('CAI_ID')+':';
  alCurrentDPI.Caption := lpGet('CAI_CURRENT_DPI')+':';
  gbResolutions.Caption := lpGet('CAI_RESOLUTIONS');

  for ar in TAndroidResolution do
    cbResolution[ar].Hint := Format(lpGet('CAI_CREATE_DPI'),
      [AndroidResolutionInfo[ar].Name]);

  bNone.Caption := lpGet('B_NONE');
  bAll.Caption := lpGet('B_ALL');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

procedure TfrmCreateAndroidIcon.UpdateForm(const ci: TCreateAndroidIcon);
var
  ar: TAndroidResolution;
begin
  eName.Text := ci.Name;
  neCurrentDPI.Value := ci.CurrentDPI;
  CurrentWidth := ci.CurrentWidth;
  CurrentHeight := ci.CurrentHeight;
  for ar in TAndroidResolution do
    cbResolution[ar].Checked := (ar in ci.CreateResolutions);
  UpdateCaptions;
end;

procedure TfrmCreateAndroidIcon.UpdateCaptions;
var
  ar: TAndroidResolution;
  ari: PAndroidResolutionInfo;
begin
  for ar in TAndroidResolution do
  begin
    ari := @AndroidResolutionInfo[ar];
    cbResolution[ar].Caption := Format('%s (%d x %d @ %d DPI)',
      [ari.Name,
       Round(CurrentWidth / neCurrentDPI.Value * ari.StandardDPI),
       Round(CurrentHeight / neCurrentDPI.Value * ari.StandardDPI),
       ari.StandardDPI]);
  end;
end;

procedure TfrmCreateAndroidIcon.UpdateObject(var ci: TCreateAndroidIcon);
var
  ar: TAndroidResolution;
begin
  ci.Name := eName.Text;
  ci.CurrentDPI := neCurrentDPI.Value;
  ci.CreateResolutions := [];
  for ar in TAndroidResolution do
    if cbResolution[ar].Checked then
      ci.CreateResolutions += [ar];
end;

function TfrmCreateAndroidIcon.Execute(frmSrc: TGraphicFrame): TGraphicFrame;
var
  ci: TCreateAndroidIcon;
  pg: TDocPage;
  bm: TBitmap32;
begin
  UpdateObject(ci);
  if frmSrc.FileName <> '' then
    ci.Name := ExtractOnlyFileName(frmSrc.FileName)
  else ci.Name := 'drawable1';
  bm := TBitmap32.Create;
  try
    pg := frmSrc.Doc.Pages[frmSrc.ImageIndex];
    bm.Assign(pg.Layers);
    ci.CurrentWidth := bm.Width;
    ci.CurrentHeight := bm.Height;
    ci.CurrentDPI := frmSrc.Doc.ActualPageDPI(pg,
      AndroidResolutionInfo[arXhdpi].StandardDPI);

    UpdateForm(ci);

    if ShowModal = mrOk then
    begin
      UpdateObject(ci);

      Result := TGraphicFrame(frmMain.NewDocument(TGraphicFrame));
      with Result do
      begin
        Doc.Clear;
        DoCreateAndroidIcon(bm, Doc, ci);
        Modified := true;
        PageCountChanged;
        PageSizeChanged;
      end;
    end else
      Result := nil;
  finally
    bm.Free;
  end;
end;

end.

