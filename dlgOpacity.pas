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
unit dlgOpacity;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, AdjustControl, NumberEdit,
  Layers, DocClass, BitmapEx, Filters, FilterDialog;

type

  { TfrmOpacity }

  TfrmOpacity = class(TFilterDialog)
    bOK: TButton;
    bCancel: TButton;
    neOpacity: TNumberEdit;
    alOpacity: TAdjustLabel;
    procedure FormCreate(Sender: TObject);
    procedure neOpacityChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
  end;

var
  frmOpacity: TfrmOpacity;

implementation

{$R *.lfm}

uses LangPack;

procedure TfrmOpacity.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_OPACITY';
end;

procedure TfrmOpacity.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  fltOpacity(bm, Mask, Round(neOpacity.Value * 2.56));
end;

procedure TfrmOpacity.neOpacityChange(Sender: TObject);
begin
  InvokeEffect;
end;

procedure TfrmOpacity.FormCreate(Sender: TObject);
begin
  ApplyLanguagePack;
end;

procedure TfrmOpacity.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  InvokeEffect;
end;

procedure TfrmOpacity.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_OPACITY');
  alOpacity.Caption := lpGet('LABEL_OPACITY')+':';
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.
