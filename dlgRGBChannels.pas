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
unit dlgRGBChannels;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, NumberEdit, ComCtrls, DocClass, BitmapEx,
  Filters, FilterDialog;

type

  { TfrmRGBChannels }

  TfrmRGBChannels = class(TFilterDialog)
    lRed: TLabel;
    tbRed: TTrackBar;
    neRed: TNumberEdit;
    lGreen: TLabel;
    tbGreen: TTrackBar;
    neGreen: TNumberEdit;
    lBlue: TLabel;
    tbBlue: TTrackBar;
    neBlue: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    bReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure neChange(Sender: TObject);
    procedure tbChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure bResetClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
  end;

var
  frmRGBChannels: TfrmRGBChannels;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmRGBChannels.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_RGB';
end;

procedure TfrmRGBChannels.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  fltRGBChannels(bm, Mask, neRed.Value, neGreen.Value, neBlue.Value);
end;

procedure TfrmRGBChannels.neChange(Sender: TObject);
var
  tb: TTrackBar;
  
begin
  if frmUpdating = 0 then
  begin
    inc(frmUpdating);
      if Sender = neRed then tb := tbRed else
      if Sender = neGreen then tb := tbGreen else
        tb := tbBlue;

      tb.Position := Round(tb.Max * (Sender as TNumberEdit).Value);
    dec(frmUpdating);

    InvokeEffect;
  end;
end;

procedure TfrmRGBChannels.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmRGBChannels.tbChange(Sender: TObject);
var
  ne: TNumberEdit;

begin
  if frmUpdating = 0 then
  begin
    inc(frmUpdating);
      if Sender = tbRed then ne := neRed else
      if Sender = tbGreen then ne := neGreen else
        ne := neBlue;

      with Sender as TTrackBar do ne.Value := Position / Max;
    dec(frmUpdating);

    InvokeEffect;
  end;
end;

procedure TfrmRGBChannels.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  InvokeEffect;
end;

procedure TfrmRGBChannels.bResetClick(Sender: TObject);
begin
  tbRed.Position := 0;
  tbGreen.Position := 0;
  tbBlue.Position := 0;
end;

procedure TfrmRGBChannels.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_RGB');
  lRed.Caption := lpGet('LABEL_RED')+':';
  lGreen.Caption := lpGet('LABEL_GREEN')+':';
  lBlue.Caption := lpGet('LABEL_BLUE')+':';
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.

