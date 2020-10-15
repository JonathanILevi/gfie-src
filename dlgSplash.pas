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
unit dlgSplash;

interface

uses
  LclIntf, LclType,
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Math, ieShared, LazUTF8, dlgDebug;

type
  TfrmSplash = class(TForm)
    imSplash: TImage;
    tmFadeOut: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmFadeOutTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure imSplashMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    Progress, MaxProgress: integer;
    bmSplash: TBitmap;

    procedure RedrawSplash;
    procedure IncProgress;
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.lfm}

(*
// properties of the progress bar
const
  pbLeft = 0;
  pbTop = 278;
  pbWidth = 560;
  pbHeight = 2;
*)

procedure TfrmSplash.RedrawSplash;
begin
  imSplash.Picture.Bitmap.Assign(bmSplash);
  Repaint;
end;

procedure TfrmSplash.IncProgress;
begin
  // There is no progress bar currently
(*
  inc(Progress);

  // update the progress bar
  with bmSplash.Canvas do
  begin
    Brush.Color := clGray;
    FillRect(Rect(pbLeft, pbTop,
      pbLeft + (pbWidth * Progress div (MaxProgress-1)), pbTop + pbHeight));
  end;

  // update the image
  RedrawSplash;
*)
end;

procedure TfrmSplash.FormCreate(Sender: TObject);
var
  j: TJPEGImage;

begin
  // load splash bitmap
  bmSplash := TBitmap.Create;

  try
    j := TJPEGImage.Create;
    try
      j.LoadFromFile(SysToUTF8(DataDir + 'splash.jpg'));
      bmSplash.Assign(j);
    finally
      j.Free;
    end;
  except
    bmSplash.Width := 100;
    bmSplash.Height := 50;
    bmSplash.Canvas.TextOut(0, 0, 'Loading...');
  end;

  ClientWidth := bmSplash.Width;
  ClientHeight := bmSplash.Height;

  RedrawSplash;
  if VerboseMode then Log('TfrmSplash created');
end;

procedure TfrmSplash.FormDestroy(Sender: TObject);
begin
  bmSplash.Free;
end;

procedure TfrmSplash.tmFadeOutTimer(Sender: TObject);
begin
  AlphaBlendValue := Max(0, AlphaBlendValue - 30);
  if AlphaBlendValue = 0 then
  begin
    tmFadeOut.Enabled := False;
    Hide;
  end;
end;

procedure TfrmSplash.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmSplash.imSplashMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Hide;
end;

end.
