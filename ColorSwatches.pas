(*
	Greenfish Controls - ColorSwatches
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
unit ColorSwatches;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Types, Classes, Controls, Graphics, ExtCtrls, Math;

const
  MaxSwatch = 16;

type
  TcswSelectEvent = procedure(Sender: TObject; X, Y: integer) of object;

  TColorSwatches = class(TCustomControl)
  private
    FRollOverPos: TPoint;
    FColors: array[0..MaxSwatch - 1, 0..MaxSwatch - 1] of TColor;
    FDim: array[0..1] of integer;
    FColorsUsed: integer;
    FDrawGrid: boolean;
    FOnSelect, FOnEditing: TcswSelectEvent;

    procedure SetRollOverPos(const Value: TPoint);
    function GetColors(x, y: integer): TColor;
    procedure SetColors(x, y: integer; const Value: TColor);
    function GetDim(Index: integer): integer;
    procedure SetDim(Index, Value: integer);
    procedure SetColorsUsed(const Value: integer);
    procedure SetDrawGrid(const Value: boolean);
  protected
    tmMouse: TTimer;

    procedure tmMouseTimer(Sender: TObject);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure ToggleRollOver;
    procedure Paint; override;

    property RollOverPos: TPoint read FRollOverPos write SetRollOverPos;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ItemRect(x, y: integer): TRect;
    function ItemAtPos(x, y: integer): TPoint;

    property Colors[x, y: integer]: TColor read GetColors write SetColors;
  published
    property Rows: integer index 0 read GetDim write SetDim;
    property Cols: integer index 1 read GetDim write SetDim;
    property ColorsUsed: integer read FColorsUsed write SetColorsUsed;
    property DrawGrid: boolean read FDrawGrid write SetDrawGrid default True;

    property Align;
    property Anchors;
    property ShowHint;
    property ParentShowHint;

    property OnSelect: TcswSelectEvent read FOnSelect write FOnSelect;
    property OnEditing: TcswSelectEvent read FOnEditing write FOnEditing;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

implementation

procedure TColorSwatches.SetRollOverPos;
begin
  if (FRollOverPos.X <> Value.X) or (FRollOverPos.Y <> Value.Y) then
  begin
    ToggleRollOver;
    FRollOverPos := Value;
    ToggleRollOver;
  end;
end;

function TColorSwatches.GetColors;
begin
  Result := FColors[x, y];
end;

procedure TColorSwatches.SetColors;
begin
  if FColors[x, y] <> Value then
  begin
    FColors[x, y] := Value;
    Invalidate;
  end;
end;

function TColorSwatches.GetDim;
begin
  Result := FDim[Index];
end;

procedure TColorSwatches.SetDim;
begin
  Value := Max(1, Min(MaxSwatch, Value));
  if FDim[Index] <> Value then
  begin
    FDim[Index] := Value;
    Invalidate;
  end;
end;

procedure TColorSwatches.SetColorsUsed;
begin
  if FColorsUsed <> Value then
  begin
   FColorsUsed := Value;
   Invalidate;
  end;
end;

procedure TColorSwatches.SetDrawGrid;
begin
  if FDrawGrid <> Value then
  begin
   FDrawGrid := Value;
   Invalidate;
  end;
end;

procedure TColorSwatches.tmMouseTimer;
begin
  if not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then
    RollOverPos := Point(-1, -1);
end;

procedure TColorSwatches.MouseDown;
begin
  inherited;

  with ItemAtPos(X, Y) do
    if x + y*Cols < ColorsUsed then
      if (Button = mbLeft) and Assigned(FOnSelect) then FOnSelect(Self, x, y) else
      if (Button = mbRight) and Assigned(FOnEditing) then FOnEditing(Self, x, y);
end;

procedure TColorSwatches.MouseMove;
begin
  inherited;

  RollOverPos := ItemAtPos(X, Y);
end;

procedure TColorSwatches.ToggleRollOver;
var
  r: TRect;
  i: integer;
  
begin
  with RollOverPos do i := x + y*Cols;
  
  if (i >= 0) and (i < ColorsUsed) then
  with Canvas do
  begin
    Pen.Mode := pmNot;
    Pen.Width := 2;
    Brush.Style := bsClear;

      r := ItemRect(RollOverPos.X, RollOverPos.Y);
      inc(r.Left, 4);
      inc(r.Top, 4);
      dec(r.Right, 2);
      dec(r.Bottom, 2);
      Rectangle(r);

    Brush.Style := bsSolid;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
  end;
end;

procedure TColorSwatches.Paint;
var
  x, y, i: integer;
  r: TRect;

begin
  with Canvas do
  begin
    for x := 0 to Cols - 1 do for y := 0 to Rows - 1 do
    begin
      r := ItemRect(x, y);

      if x + y*Cols < ColorsUsed then
      begin
        Brush.Color := Self.Colors[x, y];
        FillRect(r);
      end else
      begin
        Brush.Color := clSilver;
        FillRect(r);
        Pen.Color := clGray;
        MoveTo(r.Left, r.Top); LineTo(r.Right, r.Bottom);
      end;
    end;

    // draw frame
    Brush.Color := $303030;
    FrameRect(ClientRect);
  end;

  if DrawGrid then
  begin
    Canvas.Pen.Color := $505050;
    for i := 1 to Cols - 1 do
    begin
      x := i * (Width - 1) div Cols;
      Canvas.MoveTo(x, 0); Canvas.LineTo(x, Height);
    end;
    for i := 1 to Rows - 1 do
    begin
      y := i * (Height - 1) div Rows;
      Canvas.MoveTo(0, y); Canvas.LineTo(Width, y);
    end;
  end;

  // rollover
  ToggleRollOver;
end;

constructor TColorSwatches.Create;
var
  i, j: integer;
  
begin
  inherited;

  DoubleBuffered := True;
  
  for i := 0 to MaxSwatch - 1 do for j := 0 to MaxSwatch - 1 do
    FColors[i, j] := clWhite;
  FDim[0] := 8;
  FDim[1] := 8;
  FColorsUsed := 64;
  FDrawGrid := True;

  FRollOverPos := Point(-1, -1);
  tmMouse := TTimer.Create(Self);
  tmMouse.Interval := 70;
  tmMouse.OnTimer := tmMouseTimer;
  tmMouse.Enabled := True;
end;

destructor TColorSwatches.Destroy;
begin
  tmMouse.Free;

  inherited;
end;

function TColorSwatches.ItemRect;
begin
  Result := Rect(x*(Width - 1) div Cols, y*(Height - 1) div Rows,
        (x + 1)*(Width - 1) div Cols, (y + 1)*(Height - 1) div Rows);
end;

function TColorSwatches.ItemAtPos;
begin
  Result := Point(Max(0, Min(Cols - 1, x * Cols div (Width - 1))),
    Max(0, Min(Rows - 1, y * Rows div (Height - 1))));
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TColorSwatches]);
end;

{$IFDEF LCL}
initialization
  {$I ColorSwatches.lrs}
{$ENDIF}
end.
