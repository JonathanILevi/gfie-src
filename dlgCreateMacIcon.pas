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
unit dlgCreateMacIcon;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ICNS, BitmapEx, PixelFormats, LangPack, dlgDoc,
  DocClass, Layers;

type
  { TfrmCreateMacIcon }

  TfrmCreateMacIcon = class(TForm)
    bAll: TButton;
    bCancel: TButton;
    bNone: TButton;
    bOK: TButton;
    bReset: TButton;
    cgFormats: TCheckGroup;
    procedure bAllNoneClick(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    FirstShow: boolean;
    procedure ApplyLanguagePack;
    procedure UpdateObject(out fs: TicnsFormatSet);
    function Execute(frmSrc: TGraphicFrame): TGraphicFrame;
  end;

var
  frmCreateMacIcon: TfrmCreateMacIcon;

procedure DoCreateIcon(Src: TBitmap32; Dest: TIconDoc; const fs: TicnsFormatSet);

implementation

uses
  Main;

{$R *.lfm}

const
  fsDefault: TicnsFormatSet =
    ((False, False, False, False, False),
     (False, False, False, False, False),
     (False, True, True, True, True),
     (False, True, True, True, True),
     (False, False, False, False, False),
     (False, False, False, False, True),
     (False, False, False, False, True),
     (False, False, False, False, True),
     (False, False, False, False, False));

procedure DoCreateIcon;
var
  iis: TicnsImageSize;
  ipf: TicnsPixelFormat;
  Page: TDocPage;
  l: TLayer;
  Palette: TiePalette;

begin
  for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if fs[iis, ipf] then
      begin
        Page := Dest.NewPage;
        Page.Layers.Resize(icnsWidth[iis], icnsHeight[iis]);

        l := Page.Layers.NewLayerAtDepth(0);
        with l do
        begin
          Name := lpGet('LY_BACKGROUND');
          Selected := True;
          Image.TransformDraw(Image.ClientRect, 0, Src, True);
        end;

        case ipf of
          ipf1bit: Palette := palBW;
          ipf4bit: Palette := palMac16;
          ipf8bit: Palette := palMac256;
          else
            Palette := nil;
        end;
        if Palette <> nil then
          Palette.ApplyToBitmap(l.Image, True);
      end;
end;

{ TfrmCreateMacIcon }

procedure TfrmCreateMacIcon.bAllNoneClick(Sender: TObject);
var
  i: integer;

begin
  for i := 0 to cgFormats.Items.Count - 1 do
    cgFormats.Checked[i] := (Sender = bAll);
end;

procedure TfrmCreateMacIcon.bOKClick(Sender: TObject);
var
  i: integer;

begin
  for i := 0 to cgFormats.Items.Count - 1 do
    if cgFormats.Checked[i] then
    begin
      ModalResult := mrOk;
      Exit;
    end;

  ShowMessage(lpGet('MSG_SELECT_PAGES'));
end;

procedure TfrmCreateMacIcon.bResetClick(Sender: TObject);
var
  iis: TicnsImageSize;
  ipf: TicnsPixelFormat;
  i: integer;

begin
  i := 0;
  for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if icnsValidFormat[iis, ipf] then
      begin
        cgFormats.Checked[i] := fsDefault[iis, ipf];
        Inc(i);
      end;
end;

procedure TfrmCreateMacIcon.FormCreate(Sender: TObject);
begin
  FirstShow := True;
  ApplyLanguagePack;
end;

procedure TfrmCreateMacIcon.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmCreateMacIcon.ApplyLanguagePack;
var
  iis: TicnsImageSize;
  ipf: TicnsPixelFormat;
  pf32: TPixelFormat32;

begin
  Caption := lpGet('MI_ICON_CREATE_MAC');
  cgFormats.Caption := lpGet('CI_SELECT_FORMATS') + ':';
  bReset.Caption := lpGet('B_RESET');
  bNone.Caption := lpGet('B_NONE');
  bAll.Caption := lpGet('B_ALL');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');

  cgFormats.Items.Clear;
  for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if icnsValidFormat[iis, ipf] then
      begin
        case ipf of
          ipf1bit: pf32 := pf32_1bit;
          ipf4bit: pf32 := pf32_4bit;
          ipf8bit: pf32 := pf32_8bit;
          else
            pf32 := pf32_32bit;
        end;

        cgFormats.Items.Add(Format('%d x %d @ %s', [icnsWidth[iis],
          icnsHeight[iis], pf32ToStr[pf32]]));
      end;
end;

procedure TfrmCreateMacIcon.UpdateObject(out fs: TicnsFormatSet);
var
  iis: TicnsImageSize;
  ipf: TicnsPixelFormat;
  i: integer;

begin
  i := 0;
  for iis in TicnsImageSize do
    for ipf in TicnsPixelFormat do
      if not icnsValidFormat[iis, ipf] then
        fs[iis, ipf] := False
      else
      begin
        fs[iis, ipf] := cgFormats.Checked[i];
        Inc(i);
      end;
end;

function TfrmCreateMacIcon.Execute(frmSrc: TGraphicFrame): TGraphicFrame;
var
  bm: TBitmap32;
  fs: TicnsFormatSet;

begin
  if FirstShow then bReset.Click;
  FirstShow := False;

  if ShowModal = mrOk then
  begin
    UpdateObject(fs);

    Result := TGraphicFrame(frmMain.NewDocument(TGraphicFrame));
    with Result do
    begin
      Doc.Clear;

      bm := TBitmap32.Create;
      try
        bm.Assign(frmSrc.Doc.Pages[frmSrc.ImageIndex].Layers);
        DoCreateIcon(bm, Doc, fs);
      finally
        bm.Free;
      end;

      Modified := true;
      PageCountChanged;
      PageSizeChanged;
    end;
  end
  else
    Result := nil;
end;

end.

