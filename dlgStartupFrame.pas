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
unit dlgStartupFrame;

{$mode delphi}

interface

uses
  LclIntf, LclType, Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls,
  ExtCtrls, ieShared, DocClass, PNG, LangPack, ComCtrls, Graphics, Menus,
  Math, Dialogs, BitmapEx, Filters, dlgDebug;

const
  sscrItemCount = 5;
  sscrItems: array[0..sscrItemCount - 1] of string =
    ('NG', 'NL', 'OPEN', 'CONVERT', 'NONE');

type
  TImageState = (isNone, isNormal, isRollover);

  { TStartupFrame }

  TStartupFrame = class(TFrame)
    bRecentFiles: TButton;
    cbShow: TCheckBox;
    imHelp: TImage;
    lInfoCaption: TLabel;
    lSubtitle: TLabel;
    lTitle: TLabel;
    lInfo: TLabel;
    pAll: TPanel;
    pm: TPopupMenu;
    pTooltip: TPanel;
    procedure bRecentFilesClick(Sender: TObject);
    procedure bRecentFilesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure cbShowChange(Sender: TObject);
    procedure FrameMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure FrameResize(Sender: TObject);
    procedure imHelpClick(Sender: TObject);
    procedure ImageMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ImageClick(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
  private
    FImageState: array[0..sscrItemCount - 1] of TImageState;
    imIcon: array[0..sscrItemCount - 1] of TImage;

    function GetImageState(Index: integer): TImageState;
    procedure SetImageState(Index: integer; const Value: TImageState);
  public
    bmIcons, bmIconsRollover: TBitmap;
    IconSize: integer;

    // Call this after creation and after setting Parent
    procedure InitializeStartupFrame;
    destructor Destroy; override;

    property ImageState[Index: integer]: TImageState
      read GetImageState write SetImageState;
  end;

  { TStartupScreenTab }

  TStartupScreenTab = class(TTabSheet)
  public
    Frame: TStartupFrame;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Main;

{$R *.lfm}

{ TStartupScreenTab }

constructor TStartupScreenTab.Create(AOwner: TComponent);
begin
  inherited;
  Frame := TStartupFrame.Create(Self);
  Frame.Parent := Self;
  Frame.InitializeStartupFrame;
end;

{ TStartupFrame }

function TStartupFrame.GetImageState(Index: integer): TImageState;
begin
  Result := FImageState[Index];
end;

procedure TStartupFrame.SetImageState(Index: integer; const Value: TImageState);
var
  Src: TBitmap;

begin
  if FImageState[Index] = Value then Exit;

  FImageState[Index] := Value;
  case Value of
    isRollover: Src := bmIconsRollover;
    else Src := bmIcons;
  end;
  bmGetSlice(Src, imIcon[Index].Picture.Bitmap, 1, IconSize, Index);
end;

procedure TStartupFrame.InitializeStartupFrame;
var
  bx: TBitmap32;
  i: integer;

begin
  // load icons
  bmIcons := TBitmap.Create;
  bmIconsRollover := TBitmap.Create;

  bx := TBitmap32.Create;
  try
    pngLoadFromFile(bx, DataDir + 'startup.png');
    // normal state
    bx.ToBitmap(bmIcons, clForm);
    // rollover state
    fltExposure(bx, nil, bx.ClientRect, 1.25, 0, 1);
    bx.ToBitmap(bmIconsRollover, clForm);
  finally
    bx.Free;
  end;
  IconSize := bmIcons.Height;

  for i := 0 to Length(sscrItems) - 1 do
  begin
    imIcon[i] := TImage.Create(Self);
    FImageState[i] := isNone;

    with imIcon[i] do
    begin
      Parent := pAll;
      Tag := i;

      Width := IconSize;
      Height := IconSize;
      Cursor := crHandPoint;

      OnMouseMove := ImageMouseMove;
      OnClick := ImageClick;
      OnMouseLeave := ImageMouseLeave;
    end;

    ImageState[i] := isNormal;
  end; // for i

  // checkbox
  cbShow.Checked := Pref_ShowStartupScreen;
end;

destructor TStartupFrame.Destroy;
begin
  bmIconsRollover.Free;
  bmIcons.Free;

  inherited;
end;

procedure TStartupFrame.imHelpClick(Sender: TObject);
begin
  frmMain.ShowDefaultHelp;
end;

procedure TStartupFrame.cbShowChange(Sender: TObject);
begin
  Pref_ShowStartupScreen := cbShow.Checked;
end;

procedure TStartupFrame.FrameMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  pTooltip.Visible := False;
end;

procedure TStartupFrame.FrameResize(Sender: TObject);
const
  Margin = 10;

var
  i, xMid, yStart, CellSize, RowSize: integer;

begin
  // re-arrange icons
  xMid := Width div 2;
  yStart := lSubtitle.Top + lSubtitle.Height + Margin;
  CellSize := imIcon[0].Width + Margin;
  for i := 0 to Length(sscrItems) - 1 do
  with imIcon[i] do
  begin
    RowSize := IfThen(i < 3, 3, 2);
    Left := xMid - (CellSize*RowSize div 2) + CellSize*(i mod 3);
    Top := yStart + (imIcon[i].Height + Margin)*(i div 3);
  end;

  pTooltip.Left := (Width - pTooltip.Width) div 2;
end;

procedure TStartupFrame.bRecentFilesClick(Sender: TObject);
begin
  MenuItemToPopupMenu(frmMain.miRecentFiles, pm);
  pm.Popup;
end;

procedure TStartupFrame.bRecentFilesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  lInfoCaption.Caption := lpGet('MI_FILE_RECENT_FILES');
  lInfo.Caption := lpGet('ST_RECENT_INFO');
  pTooltip.Visible := True;
end;

procedure TStartupFrame.ImageMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Index: integer;
  s: string;

begin
  Index := (Sender as TComponent).Tag;

  // set image state
  ImageState[Index] := isRollover;
  // display info
  s := sscrItems[Index];
  lInfoCaption.Caption := lpGet('ST_' + s);
  lInfo.Caption := lpGet('ST_' + s + '_INFO');
  pTooltip.Visible := True;
end;

procedure TStartupFrame.ImageClick(Sender: TObject);
begin
  with frmMain do
  begin
    TabVisible[TStartupScreenTab] := False;
    case (Sender as TImage).Tag of
      0: miNewGraphic.Click;
      1: miNewLibrary.Click;
      2: miOpen.Click;
      3: miBatchConvert.Click;
      // 4: nothing
    end;
  end;
end;

procedure TStartupFrame.ImageMouseLeave(Sender: TObject);
var
  Index: integer;

begin
  Index := (Sender as TComponent).Tag;

  // set image state
  ImageState[Index] := isNormal;
end;

end.

