(*
	Greenfish Controls - Double-buffered paint box
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
unit DoubleBufPB;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LMessages, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Controls, ExtCtrls, Forms, Graphics;

type

  { TDoubleBufPB }

  TDoubleBufPB = class(TCustomControl)
  private
    FEraseBkGnd: boolean;
    FOnPaint: TNotifyEvent;
    FOnSetFocus, FOnKillFocus: TNotifyEvent;
    FBuffer: TBitmap;
    function GetDrawToBitmap: boolean;
    procedure SetDrawToBitmap(AValue: boolean);
    procedure SetEraseBkGnd(AValue: boolean);
    function GetCanvas: TCanvas;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
{$IFNDEF LCL}
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
{$ELSE}
    procedure WMGetDlgCode(var Message: TLMNoParams); message LM_GETDLGCODE;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
{$ENDIF}
  public
    property Buffer: TBitmap read FBuffer; // nil if DrawToBitmap is false
    property Canvas: TCanvas read GetCanvas;

    procedure EraseBackground(DC: HDC); override;
    function ReleaseBuffer(bmNewBuffer: TBitmap): TBitmap; // returns the old buffer
    procedure SwapBuffers; // draws the buffer (if present) to the screen
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnResize;

    property DrawToBitmap: boolean read GetDrawToBitmap write SetDrawToBitmap default false;
    property EraseBkGnd: boolean read FEraseBkGnd write SetEraseBkGnd default true;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnSetFocus: TNotifyEvent read FOnSetFocus write FOnSetFocus;
    property OnKillFocus: TNotifyEvent read FOnKillFocus write FOnKillFocus;

    property Anchors;
    property Align;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
  end;

procedure Register;

implementation

procedure TDoubleBufPB.EraseBackground(DC: HDC);
begin
  if EraseBkGnd and not DrawToBitmap then
    inherited;
end;

function TDoubleBufPB.ReleaseBuffer(bmNewBuffer: TBitmap): TBitmap;
begin
  Result := FBuffer;
  FBuffer := bmNewBuffer;
  if FBuffer <> nil then
  begin
    FBuffer.PixelFormat := pf24bit;
    FBuffer.Canvas.Brush.Color:=Color;
    FBuffer.Width := Width;
    FBuffer.Height := Height;
  end;
end;

procedure TDoubleBufPB.SwapBuffers;
begin
  if (FBuffer <> nil) and HandleAllocated and not (csDestroying in ComponentState) then
    (inherited Canvas).Draw(0, 0, FBuffer);
end;

procedure TDoubleBufPB.SetEraseBkGnd(AValue: boolean);
begin
  if FEraseBkGnd=AValue then Exit;
  FEraseBkGnd:=AValue;
end;

procedure TDoubleBufPB.SetDrawToBitmap(AValue: boolean);
begin
  if DrawToBitmap = AValue then Exit;

  if not AValue then
    FreeAndNil(FBuffer)
  else
  begin
    FBuffer := TBitmap.Create;
    FBuffer.PixelFormat:=pf24bit;
    FBuffer.Canvas.Brush.Color:=Color;
    FBuffer.Width:=Width;
    FBuffer.Height:=Height;
  end;
end;

function TDoubleBufPB.GetDrawToBitmap: boolean;
begin
  Result := FBuffer <> nil;
end;

function TDoubleBufPB.GetCanvas: TCanvas;
begin
  if DrawToBitmap then
    Result := FBuffer.Canvas
  else
    Result := inherited Canvas;
end;

procedure TDoubleBufPB.Paint;
begin
  if Assigned(FOnPaint) then FOnPaint(Self);
  SwapBuffers;
end;

procedure TDoubleBufPB.Resize;
begin
  inherited Resize;
  if FBuffer <> nil then
  begin
    FBuffer.Canvas.Brush.Color:=Color;
    FBuffer.Width := Width;
    FBuffer.Height := Height;
  end;
end;

procedure TDoubleBufPB.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if CanFocus then SetFocus;
end;

procedure TDoubleBufPB.WMGetDlgCode(var Message: TLMNoParams);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
end;

procedure TDoubleBufPB.WMSetFocus(var Message: TLMSetFocus);
begin
  inherited;
  if Assigned(FOnSetFocus) then FOnSetFocus(Self);
end;

procedure TDoubleBufPB.WMKillFocus(var Message: TLMKillFocus);
begin
  inherited;
  if Assigned(FOnKillFocus) then FOnKillFocus(Self);
end;

constructor TDoubleBufPB.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered := True;
  FEraseBkGnd := true;
  FOnPaint := nil;
  FBuffer := nil;
{$IFDEF LCL}
  ControlStyle := ControlStyle + [csCaptureMouse];
  CaptureMouseButtons := CaptureMouseButtons + [mbLeft, mbRight, mbMiddle];
{$ENDIF}
end;

destructor TDoubleBufPB.Destroy;
begin
  if FBuffer <> nil then FreeAndNil(FBuffer);
  inherited;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TDoubleBufPB]);
end;

{$IFDEF LCL}
initialization
  {$I DoubleBufPB.lrs}
{$ENDIF}
end.
