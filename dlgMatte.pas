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
unit dlgMatte;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DocClass, BitmapEx, Filters,
  ieShared, FilterDialog;

type

  { TfrmRemoveMatte }

  TfrmRemoveMatte = class(TFilterDialog)
    lMatteColor: TLabel;
    sMatte: TShape;
    bWhiteMatte: TButton;
    bBlackMatte: TButton;
    bOK: TButton;
    bCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure sMatteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure bWhiteMatteClick(Sender: TObject);
    procedure bBlackMatteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
  end;

var
  frmRemoveMatte: TfrmRemoveMatte;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmRemoveMatte.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_REMOVE_MATTE';
end;

procedure TfrmRemoveMatte.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  fltRemoveMatte(bm, Mask, sMatte.Brush.Color);
end;

procedure TfrmRemoveMatte.sMatteMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Value: TColor;
  
begin
  Value := GFIEPickColor(sMatte.Brush.Color);
  if Value <> sMatte.Brush.Color then
  begin
    sMatte.Brush.Color := Value;
    InvokeEffect;
  end;
end;

procedure TfrmRemoveMatte.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmRemoveMatte.bWhiteMatteClick(Sender: TObject);
begin
  sMatte.Brush.Color := clWhite;
  InvokeEffect;
end;

procedure TfrmRemoveMatte.bBlackMatteClick(Sender: TObject);
begin
  sMatte.Brush.Color := clBlack;
  InvokeEffect;
end;

procedure TfrmRemoveMatte.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  InvokeEffect;
end;

procedure TfrmRemoveMatte.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_REMOVE_MATTE');
  lMatteColor.Caption := lpGet('LABEL_MATTE_COLOR')+':';
  bWhiteMatte.Caption := lpGet('LABEL_WHITE_MATTE');
  bBlackMatte.Caption := lpGet('LABEL_BLACK_MATTE');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
