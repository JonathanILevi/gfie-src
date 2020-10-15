(*
	Greenfish Controls - PaintScrollBar
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
unit PaintScrollBar;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Math;

type
  TpsbPaintEvent = procedure(Sender: TObject; bm: TBitmap; Rect: TRect) of object;
  TpsbCanScrollEvent = procedure(Sender: TObject; var NewPos: integer) of object;

  TPaintScrollBar = class(TCustomControl)
  private
    FIsMouseDown: boolean;
    FMinValue, FMaxValue, FPosition: integer;
    FOnChange: TNotifyEvent;
    FOnPaint: TpsbPaintEvent;
    FOnCanScroll: TpsbCanScrollEvent;

    procedure SetIsMouseDown(const Value: boolean);
    procedure SetMinValue(const Value: integer);
    procedure SetMaxValue(const Value: integer);
    procedure SetPosition(const Value: integer);
  protected
    bmWork: TBitmap;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Paint; override;

    property IsMouseDown: boolean read FIsMouseDown write SetIsMouseDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSiteRect: TRect;
    procedure SetParams(APosition, AMin, AMax: integer);
    function xToPosition(x: integer): integer;
  published
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Position: integer read FPosition write SetPosition;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPaint: TpsbPaintEvent read FOnPaint write FOnPaint;
    property OnCanScroll: TpsbCanScrollEvent read FOnCanScroll write FOnCanScroll;

    property Align;
    property Anchors;
    property Color;
    property ParentColor;
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

const
  psbEdge = 3;

procedure TPaintScrollBar.SetIsMouseDown;
begin
  if FIsMouseDown <> Value then
  begin
    FIsMouseDown := Value;
    Invalidate;
  end;
end;

procedure TPaintScrollBar.SetMinValue;
begin
  SetParams(FPosition, Value, FMaxValue);
end;

procedure TPaintScrollBar.SetMaxValue;
begin
  SetParams(FPosition, FMinValue, Value);
end;

procedure TPaintScrollBar.SetPosition;
begin
  SetParams(Value, FMinValue, FMaxValue);
end;

procedure TPaintScrollBar.MouseDown;
begin
  inherited;

  if Button = mbLeft then
  begin
    IsMouseDown := True;
    Position := xToPosition(X);
  end;
end;

procedure TPaintScrollBar.MouseMove;
begin
  inherited;

  if IsMouseDown then Position := xToPosition(X);
end;

procedure TPaintScrollBar.MouseUp;
begin
  inherited;

  IsMouseDown := False;
end;

procedure TPaintScrollBar.Paint;
var
  rSite: TRect;
  a: array[0..2] of TPoint;
  x: integer;

begin
  bmWork.Width := Width;
  bmWork.Height := Height;

  with bmWork.Canvas do
  begin
    // draw background
{$IFDEF LCL}
    Brush.Color := IfThen(Color = clDefault, GetDefaultColor(dctBrush), Color);
{$ELSE}
    Brush.Color := Color;
{$ENDIF}
    FillRect(ClientRect);

    // draw site
    rSite := GetSiteRect;
    if Assigned(FOnPaint) then FOnPaint(Self, bmWork, rSite);

    // draw cursor
    Pen.Color := IfThen(IsMouseDown, clGray, clBlack);
    Brush.Color := Pen.Color;
    if MinValue < MaxValue then
      x := rSite.Left + Round((rSite.Right-1 - rSite.Left) *
        (Position - MinValue) / (MaxValue - MinValue)) else
      x := rSite.Left;

    a[0].x := x - psbEdge; a[0].y := 0;
    a[1].x := x; a[1].y := psbEdge;
    a[2].x := x + psbEdge; a[2].y := 0;
    Polygon(a);

    a[0].x := x - psbEdge; a[0].y := Height-1;
    a[1].x := x; a[1].y := Height-1 - psbEdge;
    a[2].x := x + psbEdge; a[2].y := Height-1;
    Polygon(a);

    MoveTo(x, 0);
    LineTo(x, Height);
  end;

  Canvas.Draw(0, 0, bmWork);
end;

constructor TPaintScrollBar.Create;
begin
  inherited;

  DoubleBuffered := True;
  bmWork := TBitmap.Create;
  bmWork.PixelFormat := pf24bit;
  FIsMouseDown := False;
  FMinValue := 0;
  FMaxValue := 100;
  FPosition := 0;
end;

destructor TPaintScrollBar.Destroy;
begin
  bmWork.Free;

  inherited;
end;

function TPaintScrollBar.GetSiteRect;
begin
  Result := Rect(psbEdge, psbEdge, Width - psbEdge, Height - psbEdge);
end;

procedure TPaintScrollBar.SetParams;
begin
  if (APosition <> Position) or (AMin <> FMinValue) or (AMax <> FMaxValue) then
  begin
    if AMax < AMin then AMax := AMin;

    // Verify APosition
    APosition := Max(AMin, Min(AMax, APosition));
    // Allow OnCanScroll to make some changes
    if Assigned(FOnCanScroll) then
    begin
      FOnCanScroll(Self, APosition);
      // Verify APosition _once more_, in case it became erroneus
      APosition := Max(AMin, Min(AMax, APosition));
    end;

    FMaxValue := AMax;
    FMinValue := AMin;
    if FPosition <> APosition then
    begin
      FPosition := APosition;
      if Assigned(FOnChange) then FOnChange(Self);
    end;

    Repaint;
  end;
end;

function TPaintScrollBar.xToPosition;
var
  rSite: TRect;
  
begin
  rSite := GetSiteRect;

  if rSite.Left <> rSite.Right then
    Result := MinValue + Round((MaxValue - MinValue) * (x - rSite.Left) /
      (rSite.Right-1 - rSite.Left)) else
    Result := MinValue;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TPaintScrollBar]);
end;

{$IFDEF LCL}
initialization
  {$I PaintScrollBar.lrs}
{$ENDIF}
end.
