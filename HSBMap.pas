(*
	Greenfish Controls - HSBMap
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
unit HSBMap;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Controls, Graphics, ColSpaces, Math;

type
  THSBMap = class(TCustomControl)
  private
    FBBarSize: integer;
    FOnChange: TNotifyEvent;

    procedure SetBBarSize(const Value: integer);
    function GetSelColor: TColor;
    procedure SetSelColor(const Value: TColor);
  protected
    bmBack: TBitmap;
    ma: integer; // mouse action

    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;

    function CtrlSize: integer;
    procedure PaintBackground;
    procedure Paint; override;
  public
    SelHSB: TInt3;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Change; dynamic;
  published
    property BBarSize: integer read FBBarSize write SetBBarSize;
    property SelColor: TColor read GetSelColor write SetSelColor;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property Align;
    property Anchors;
    property ShowHint;
    property ParentShowHint;
  end;

procedure Register;

implementation

// HSBMap mouse actions
const
  hmmaNone = 0;
  hmmaHS = 1;
  hmmaB = 2;

procedure THSBMap.SetBBarSize;
begin
  if FBBarSize <> Value then
  begin
    FBBarSize := Value;
    PaintBackground;
    Invalidate;
  end;
end;

function THSBMap.GetSelColor;
begin
  Result := Int3toColor(HSBtoRGB(SelHSB[0], SelHSB[1], SelHSB[2]));
end;

procedure THSBMap.SetSelColor;
begin
  if SelColor <> Value then
  begin
    SelHSB := RGBtoHSB(Value and $ff, (Value shr 8) and $ff,
      (Value shr 16) and $ff);
    Change;
  end;
end;

procedure THSBMap.Resize;
begin
  inherited;
  
  // recreate bmBack
  PaintBackground;
end;

procedure THSBMap.MouseDown;
var
  Size: integer;
  
begin
  inherited;

  Size := CtrlSize;

  if (Y >= BBarSize div 2) and (Y < Size) then
    if X < Size then ma := hmmaHS else ma := hmmaB;

  MouseMove(Shift, X, Y);
end;

procedure THSBMap.MouseMove;
var
  Size: integer;
  
begin
  inherited;

  Size := CtrlSize;

  case ma of
    hmmaHS: begin
      SelHSB[0] := HSBMAX * Max(0, Min(Size-1, X)) div (Size - 1);
      SelHSB[1] := HSBMAX - HSBMAX *
        Max(0, Min(Size-1, Y - BBarSize div 2)) div (Size - 1);
      Change;
    end;
    hmmaB: begin
      SelHSB[2] := HSBMAX - HSBMAX *
        Max(0, Min(Size-1, Y - BBarSize div 2)) div (Size - 1);
      Change;
    end;
  end;
end;

procedure THSBMap.MouseUp;
begin
  inherited;

  ma := hmmaNone;
end;

function THSBMap.CtrlSize;
begin
  Result := Min(Width - 2*BBarSize, Height - BBarSize);
end;

procedure THSBMap.PaintBackground;
var
  Size, x, y: integer;
  c: TColor;

{$IFDEF DELPHI}
var
  sl: PByteArray;

  function FlipColor(c: TColor): TColor;
  asm
    mov dx, ax
    shr eax, 16
    xchg al, dl
    shl eax, 16
    mov ax, dx
  end;

  procedure Move3(var Src, Dest);
  asm
    mov cx, [eax]
    mov [edx], cx
    mov cl, [eax + 2]
    mov [edx + 2], cl
  end;
{$ENDIF}

begin
  Size := CtrlSize;

  bmBack.Width := 0;
  bmBack.Width := Size;
  bmBack.Height := Size;

  bmBack.Canvas.Pen.Color := clGray;
  bmBack.Canvas.Brush.Color := Color;
  bmBack.Canvas.Rectangle(0, 0, Size, Size);

{$IFDEF LCL}
// TODO: optimize
for y := 1 to Size - 2 do
begin
  for x := 1 to Size - 2 do
  begin
    c := Int3toColor(HSBtoRGB(HSBMAX * x div (Size-1),
      HSBMAX - HSBMAX * y div (Size-1), HSBMAX div 2));
    bmBack.Canvas.Pixels[x, y] := c;
  end;
end;
{$ELSE}
for y := 1 to Size - 2 do
begin
  sl := bmBack.ScanLine[y];

  for x := 1 to Size - 2 do
  begin
    c := FlipColor(Int3toColor(HSBtoRGB(HSBMAX * x div (Size-1),
      HSBMAX - HSBMAX * y div (Size-1), HSBMAX div 2)));
    Move3(c, sl[x*3]);
  end;
end;
{$ENDIF}
end;

procedure THSBMap.Paint;
const
  sizeHSCircle = 7;
  
var
  i, Size, x, y, yOffset: integer;
  pp: array[0..2] of TPoint;

begin
  Size := CtrlSize;

  // draw H-S map with marker
  x := (Size - 1) * SelHSB[0] div HSBMAX;
  y := Size - 1 - (Size - 1) * SelHSB[1] div HSBMAX;

  with Canvas do
  begin
    yOffset := BBarSize div 2;
    Draw(0, yOffset, bmBack);

    Brush.Style := bsClear;
      Pen.Color := clWhite;
      Ellipse(x - sizeHSCircle + 1, yOffset+y - sizeHSCircle + 1,
        x + sizeHSCircle - 1, yOffset+y + sizeHSCircle - 1);
      Pen.Color := clBlack;
      Ellipse(x - sizeHSCircle, yOffset+y - sizeHSCircle,
        x + sizeHSCircle, yOffset+y + sizeHSCircle);
    Brush.Style := bsSolid;

    // draw B scrollbar
    for i := 0 to Size - 1 do
    begin
      Pen.Color := Int3toColor( HSBtoRGB(SelHSB[0], SelHSB[1],
        HSBMAX - HSBMAX * i div (Size - 1)) );
      MoveTo(Size + BBarSize, BBarSize div 2 + i);
      LineTo(Size + 2*BBarSize, BBarSize div 2 + i);
    end;

   (* Pen.Color := clGray;
    MoveTo(Size + BBarSize, Size + BBarSize div 2);
    LineTo(Size + BBarSize, BBarSize div 2);
    LineTo(Size + 2*BBarSize, BBarSize div 2);
    Pen.Color := clWhite;
    LineTo(Size + 2*BBarSize, Size + BBarSize div 2);
    LineTo(Size + BBarSize, Size + BBarSize div 2);*)

    // draw B marker
    Pen.Color := clBlack;
    Brush.Color := clBlack;

    y := BBarSize div 2 + Size - 1 - (Size - 1) * SelHSB[2] div HSBMAX;
    x := Size + 3*BBarSize div 4;
    i := BBarSize div 2 - 1;
    pp[0] := Point(x, y);
    pp[1] := Point(x - i, y - i);
    pp[2] := Point(x - i, y + i);

    Polygon(pp);
  end;
end;

constructor THSBMap.Create;
begin
  inherited;

  DoubleBuffered := True;
{$IFDEF LCL}
  CaptureMouseButtons := CaptureMouseButtons + [mbLeft, mbRight, mbMiddle];
{$ENDIF}

  bmBack := TBitmap.Create;
  bmBack.PixelFormat := pf24bit;
  ma := hmmaNone;

  FBBarSize := 10;
  SelHSB := RGBtoHSB(0, 0, 0);
end;

destructor THSBMap.Destroy;
begin
  bmBack.Free;

  inherited;
end;

procedure THSBMap.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
  Repaint;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [THSBMap]);
end;

{$IFDEF LCL}
initialization
  {$I HSBMap.lrs}
{$ENDIF}
end.
