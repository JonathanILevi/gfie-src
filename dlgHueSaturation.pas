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
unit dlgHueSaturation;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PaintScrollBar, StdCtrls, NumberEdit, AdjustControl,
  DocClass, BitmapEx, Filters, ColSpaces, LangPack, FilterDialog, gfMath, dlgDebug;

type

  { TfrmHueSaturation }

  TfrmHueSaturation = class(TFilterDialog)
    sbHue: TPaintScrollBar;
    sbSat: TPaintScrollBar;
    neHue: TNumberEdit;
    neSat: TNumberEdit;
    bOK: TButton;
    bCancel: TButton;
    bReset: TButton;
    alHue: TAdjustLabel;
    alSat: TAdjustLabel;
    procedure bResetClick(Sender: TObject);
    procedure neChange(Sender: TObject);
    procedure sbChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure sbHuePaint(Sender: TObject; bm: TBitmap; Rect: TRect);
    procedure sbSatPaint(Sender: TObject; bm: TBitmap; Rect: TRect);
  private
    { Private declarations }
  public
    bmHue, bmSat: TBitmap;
    procedure ApplyLanguagePack;
    procedure FillInfo; override;
    procedure DoFilter(bm: TBitmap32; Mask: TBitmap1); override;
  end;

var
  frmHueSaturation: TfrmHueSaturation;

implementation

{$R *.lfm}

procedure TfrmHueSaturation.FillInfo;
begin
  inherited;
  UndoText := 'MI_FLT_HS';
end;

procedure TfrmHueSaturation.DoFilter(bm: TBitmap32; Mask: TBitmap1);
begin
  fltHueSaturation(bm, Mask, bm.ClientRect,
    sbHue.Position * HSBMAX div 100,
    sbSat.Position / sbSat.MaxValue);
end;

procedure TfrmHueSaturation.bResetClick(Sender: TObject);
begin
  sbHue.Position := 0;
  sbSat.Position := 0;
end;

procedure TfrmHueSaturation.neChange(Sender: TObject);
var
  sb: TPaintScrollBar;
  
begin
  if frmUpdating = 0 then
  begin
    inc(frmUpdating);
      if Sender = neHue then sb := sbHue else sb := sbSat;
      sb.Position := Round((Sender as TNumberEdit).Value);
    dec(frmUpdating);

    InvokeEffect;
  end;
end;

procedure TfrmHueSaturation.sbChange(Sender: TObject);
var
  ne: TNumberEdit;

begin
  if frmUpdating = 0 then
  begin
    inc(frmUpdating);
      if Sender = sbHue then ne := neHue else ne := neSat;
      ne.Value := (Sender as TPaintScrollBar).Position;
    dec(frmUpdating);

    InvokeEffect;
  end;
end;

procedure TfrmHueSaturation.FormCreate(Sender: TObject);
var
  i: integer;
  r: TRect;

begin
  ApplyLanguagePack;
  frmUpdating := 0;
  bmHue := TBitmap.Create;
  bmSat := TBitmap.Create;

  r := sbHue.GetSiteRect;
  bmHue.Width := r.Width;
  bmHue.Height := (r.Bottom + r.Top) div 2 - r.Top;
  for i := 0 to bmHue.Width - 1 do
    with bmHue.Canvas do
  begin
    Pen.Color := Int3toColor(HSBtoRGB(i * HSBMAX div (bmHue.Width - 1),
      HSBMAX, HSBMAX div 2));
    MoveTo(i, 0); LineTo(i, bmHue.Height);
  end;

  r := sbSat.GetSiteRect;
  bmSat.Width := r.Width + 1;
  bmSat.Height := r.Height + 1;
  for i := 0 to bmSat.Width - 1 do
    with bmSat.Canvas do
  begin
    Pen.Color := Int3toColor(HSBtoRGB(0,
      i * HSBMAX div (bmSat.Width - 1), HSBMAX div 2));
    MoveTo(i, 0); LineTo(i, bmSat.Height);
  end;

  if VerboseMode then Log('TfrmHueSaturation created');
end;

procedure TfrmHueSaturation.FormShow(Sender: TObject);
begin
  // move form to screen center only the first time
  Position := poDesigned;

  InvokeEffect;
end;

procedure TfrmHueSaturation.FormDestroy(Sender: TObject);
begin
  bmHue.Free;
  bmSat.Free;
end;

procedure TfrmHueSaturation.sbHuePaint(Sender: TObject; bm: TBitmap;
  Rect: TRect);
var
  x, y: integer;
  
begin
  with bm.Canvas, Rect do
  begin
    Draw(Left, Top, bmHue);
    x := -sbHue.Position; if x < 0 then inc(x, 100);
    x := Left + (Right - Left) * x div 100;
    y := (Top + Bottom) div 2;
    Draw(x, y, bmHue);
    Draw(x - bmHue.Width, y, bmHue);
  end;
end;

procedure TfrmHueSaturation.sbSatPaint(Sender: TObject; bm: TBitmap;
  Rect: TRect);
begin
  bm.Canvas.Draw(Rect.Left, Rect.Top, bmSat);
end;

procedure TfrmHueSaturation.ApplyLanguagePack;
begin
  Caption := lpGet('MI_FLT_HS');
  alHue.Caption := lpGet('LABEL_HUE_SHIFT')+':';
  alSat.Caption := lpGet('LABEL_SATURATION')+':';
  bReset.Caption := lpGet('B_RESET');
  bOK.Caption := lpGet('B_OK');
  bCancel.Caption := lpGet('B_CANCEL');
end;

end.

