(*
	Greenfish Controls - ImageAnchors
	Copyright © 2013 Balázs Szalkai
	This file is released under the zlib license:

	This software is provided 'as-is', without any express or implied
	warranty. In no event will the authors be held liable for any damages
	arising from the use of this software.

	Permission is granted to anyone to use this software for any purpose,
	including commercial applications, and to alter it and redistribute it
	freely, subject to the following restrictions:

	   1. The origin of this software must not be misrepresented; you must not
	   claim that you wrote the original software. If you use this software
	   in a product, an acknowledgment in the product documentation would be
	   appreciated but is not required.

	   2. Altered source versions must be plainly marked as such, and must not be
	   misrepresented as being the original software.

	   3. This notice may not be removed or altered from any source
	   distribution.
*)
unit ImageAnchors;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Math;

type
  TImageAnchorHoriz = (iahLeft, iahCenter, iahRight);
  TImageAnchorVert = (iavTop, iavCenter, iavBottom);

  TImageAnchors = class(TGraphicControl)
  private
    FAnchorHoriz: TImageAnchorHoriz;
    FAnchorVert: TImageAnchorVert;
    FOnChange: TNotifyEvent;

    procedure SetAnchorHoriz(const Value: TImageAnchorHoriz);
    procedure SetAnchorVert(const Value: TImageAnchorVert);
  protected
    procedure Changed; dynamic;

    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
  published
    property Anchors;
    property Enabled;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;

    property AnchorHoriz: TImageAnchorHoriz read FAnchorHoriz write SetAnchorHoriz;
    property AnchorVert: TImageAnchorVert read FAnchorVert write SetAnchorVert;

    property OnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

implementation

// TImageAnchors

procedure TImageAnchors.SetAnchorHoriz;
begin
  if FAnchorHoriz <> Value then
  begin
    FAnchorHoriz := Value;
    Changed;
  end;
end;

procedure TImageAnchors.SetAnchorVert;
begin
  if FAnchorVert <> Value then
  begin
    FAnchorVert := Value;
    Changed;
  end;
end;

procedure TImageAnchors.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  Invalidate;
end;

procedure TImageAnchors.Paint;
var
  x, y, SquareSize: integer;
  xPos: array[TImageAnchorHoriz] of integer;
  yPos: array[TImageAnchorVert] of integer;
  iah: TImageAnchorHoriz;
  iav: TImageAnchorVert;

begin
  SquareSize := Ceil(Min(Width, Height) * 3 / 13);

  xPos[iahLeft] := SquareSize div 2;
  xPos[iahCenter] := Width div 2;
  xPos[iahRight] := Width-1 - SquareSize div 2;
  yPos[iavTop] := SquareSize div 2;
  yPos[iavCenter] := Height div 2;
  yPos[iavBottom] := Height-1 - SquareSize div 2;

  with Canvas do
  begin
    Pen.Color := IfThen(Enabled, clBlack, clGray);

    // draw lines
    for iah := iahLeft to iahRight do
    begin
      MoveTo(xPos[iah], yPos[iavTop]);
      LineTo(xPos[iah], yPos[iavBottom]);
    end;
    for iav := iavTop to iavBottom do
    begin
      MoveTo(xPos[iahLeft], yPos[iav]);
      LineTo(xPos[iahRight], yPos[iav]);
    end;

    // draw squares
    for iah := iahLeft to iahRight do
    for iav := iavTop to iavBottom do
    begin
      if (iah = AnchorHoriz) and (iav = AnchorVert) then
        Brush.Color := IfThen(Enabled, clBlack, clGray) else
        Brush.Color := clWhite;

      x := xPos[iah] - SquareSize div 2;
      y := yPos[iav] - SquareSize div 2;
      Rectangle(x, y, x + SquareSize, y + SquareSize);
    end;
  end; // with Canvas
end;

procedure TImageAnchors.MouseDown;
begin
  inherited;

  AnchorHoriz := TImageAnchorHoriz((3*X) div Width);
  AnchorVert := TImageAnchorVert((3*Y) div Height);
end;

constructor TImageAnchors.Create;
begin
  inherited;

  FAnchorHoriz := iahLeft;
  FAnchorVert := iavTop;

  FOnChange := nil;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TImageAnchors]);
end;

{$IFDEF LCL}
initialization
  {$I ImageAnchors.lrs}
{$ENDIF}
end.
