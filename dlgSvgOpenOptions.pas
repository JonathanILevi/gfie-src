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
unit dlgSvgOpenOptions;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  NumberEdit, AdjustControl, dlgDoc, Layers, DocClass, Jpeg2000, BitmapEx,
  LangPack, Math;

type

  { TfrmSvgOpenOptions }

  TfrmSvgOpenOptions = class(TForm)
    alScale: TAdjustLabel;
    alWidth: TAdjustLabel;
    alHeight: TAdjustLabel;
    bOK: TButton;
    lDefaultSize: TLabel;
    lPixels1: TLabel;
    lPixels2: TLabel;
    neScale: TNumberEdit;
    neWidth: TNumberEdit;
    neHeight: TNumberEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure neScaleChange(Sender: TObject);
    procedure neHeightChange(Sender: TObject);
    procedure neWidthChange(Sender: TObject);
  private
    frmUpdating: integer;
    defaultWidth: double;
    defaultHeight: double;
    procedure UpdateLabels;
    procedure UpdateSizeFromScale;
    procedure UpdateAllFromWidth;
    procedure UpdateAllFromHeight;
    procedure InitForm(_defaultWidth, _defaultHeight: double);
  public
    function Execute(const fn: string): boolean;
    procedure ApplyLanguagePack;
  end; 

var
  frmSvgOpenOptions: TfrmSvgOpenOptions;

implementation

{$R *.lfm}

uses
  ImageConverter_Intf;

procedure TfrmSvgOpenOptions.FormCreate(Sender: TObject);
begin
  frmUpdating := 0;
  ApplyLanguagePack;
  InitForm(0, 0);
end;

procedure TfrmSvgOpenOptions.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;
end;

procedure TfrmSvgOpenOptions.neScaleChange(Sender: TObject);
begin
  if frmUpdating = 0 then
    UpdateSizeFromScale;
end;

procedure TfrmSvgOpenOptions.neHeightChange(Sender: TObject);
begin
  if frmUpdating = 0 then
    UpdateAllFromHeight;
end;

procedure TfrmSvgOpenOptions.neWidthChange(Sender: TObject);
begin
  if frmUpdating = 0 then
    UpdateAllFromWidth;
end;

procedure TfrmSvgOpenOptions.UpdateLabels;
begin
  lDefaultSize.Caption := Format(lpGet('SOO_DEFAULT_SIZE'), [1.0*Round(defaultWidth), 1.0*Round(defaultHeight)]);
end;

procedure TfrmSvgOpenOptions.UpdateSizeFromScale;
begin
  inc(frmUpdating);
  neWidth.Value := Round(defaultWidth * neScale.Value);
  neHeight.Value := Round(defaultHeight * neScale.Value);
  dec(frmUpdating);
end;

procedure TfrmSvgOpenOptions.UpdateAllFromWidth;
var
  scale: double;
begin
  if defaultWidth = 0 then
    scale := 0
  else
    scale := neWidth.Value / defaultWidth;
  inc(frmUpdating);
  neScale.Value := scale;
  neHeight.Value := Round(defaultHeight * scale);
  dec(frmUpdating);
end;

procedure TfrmSvgOpenOptions.UpdateAllFromHeight;
var
  scale: double;
begin
  if defaultHeight = 0 then
    scale := 0
  else
    scale := neHeight.Value / defaultHeight;
  inc(frmUpdating);
  neScale.Value := scale;
  neWidth.Value := Round(defaultWidth * scale);
  dec(frmUpdating);
end;

procedure TfrmSvgOpenOptions.InitForm(_defaultWidth, _defaultHeight: double);
begin
  defaultWidth := _defaultWidth;
  defaultHeight := _defaultHeight;
  inc(frmUpdating);
  neScale.Value := 1;
  UpdateSizeFromScale;
  dec(frmUpdating);
  UpdateLabels;
end;

function TfrmSvgOpenOptions.Execute(const fn: string): boolean;
var
  info: TSvgInfo;
begin
  Screen.Cursor := crHourGlass;
  try
    svgGetInfo(info, fn);
    InitForm(info.Width, info.Height);
  finally
    Screen.Cursor := crDefault;
  end;
  Result := (ShowModal = mrOk);
end;

procedure TfrmSvgOpenOptions.ApplyLanguagePack;
begin
  Caption := lpGet('SOO_SVG_OPTIONS');
  alScale.Caption := lpGet('SOO_SCALE_FACTOR')+':';
  alWidth.Caption := lpGet('LABEL_WIDTH')+':';
  alHeight.Caption := lpGet('LABEL_HEIGHT')+':';
  lPixels1.Caption := lpGet('LABEL_PIXELS_SHORT');
  lPixels2.Caption := lpGet('LABEL_PIXELS_SHORT');
  bOK.Caption := lpGet('B_OK');
end;

end.

