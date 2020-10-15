(*
	Greenfish Controls - Accordion
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
unit Accordion;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

type

  { TAccordion }

  TAccordion = class(TCustomControl)
  private
    FContents: TControl;

    function GetContentsVisible: boolean;
    procedure SetContentsVisible(const Value: boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property ContentsVisible: boolean read GetContentsVisible write SetContentsVisible;
  published
    property Contents: TControl read FContents write FContents;

    property Align;
    property Anchors;
    property Caption;
    property Color;
    property ParentColor;
    property Font;
    property ShowHint;
    property ParentShowHint;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

{ TAccordion }

function TAccordion.GetContentsVisible: boolean;
begin
  if not Assigned(Contents) then Exit(False);
  Result := Contents.Visible;
end;

procedure TAccordion.SetContentsVisible(const Value: boolean);
begin
  if Assigned(Contents) and (Contents.Visible <> Value) then
  begin
    Contents.Visible := Value;
    Invalidate;
  end;
end;

procedure TAccordion.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  if Button = mbLeft then ContentsVisible := not ContentsVisible;
end;

procedure TAccordion.Paint;
const
  // Plusminus properties
  PM_BOX_SIZE = 4;
  PM_SIZE = 2;
  PM_MARGIN_LEFT = 2;
  PM_MARGIN_RIGHT = 4;

var
  x, y: integer;

begin
  inherited;

  // fill bkgnd
  Canvas.Brush.Color := Color;
  Canvas.FillRect(Canvas.ClipRect);

  // draw plus / minus
  // draw box
  x := PM_MARGIN_LEFT + PM_BOX_SIZE;
  y := Height div 2;
  Canvas.Pen.Color := clGray;
  Canvas.Brush.Color := clWhite;
  Canvas.Rectangle(x-PM_BOX_SIZE, y-PM_BOX_SIZE, x+PM_BOX_SIZE+1, y+PM_BOX_SIZE+1);
  // draw + or -
  Canvas.Pen.Color := clGreen;
  Canvas.MoveTo(x - PM_SIZE, y);
  Canvas.LineTo(x + PM_SIZE+1, y);
  if ContentsVisible then
  begin
    Canvas.MoveTo(x, y - PM_SIZE);
    Canvas.LineTo(x, y + PM_SIZE+1);
  end;

  // draw text
  Canvas.Font.Assign(Font);
  x += PM_BOX_SIZE + PM_MARGIN_RIGHT;
  y := (Height - Canvas.TextHeight(Caption)) div 2;
  Canvas.Brush.Style := bsClear;
  Canvas.TextOut(x, y, Caption);
  Canvas.Brush.Style := bsSolid;
end;

constructor TAccordion.Create(AOwner: TComponent);
begin
  inherited;

  FContents := nil;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TAccordion]);
end;

{$IFDEF LCL}
initialization
  {$I Accordion.lrs}
{$ENDIF}
end.

