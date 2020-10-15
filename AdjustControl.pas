(*
	Greenfish Controls - AdjustControl
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
unit AdjustControl;

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Forms, Controls, StdCtrls, NumberEdit,
  ExtCtrls;

type
  TAdjustLabel = class(TLabel)
  private
    FAttachedEdit: TNumberEdit;
  protected
    Dragging: boolean;
    PrevX: integer;
  public
    constructor Create(AOwner: TComponent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  published
    property AttachedEdit: TNumberEdit read FAttachedEdit write FAttachedEdit;

    property Caption;
    property Align;
    property Anchors;
    property Color;
    property Font;
    property Hint;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
  end;

  TAdjustImage = class(TImage)
  private
    FAttachedEdit: TNumberEdit;
  protected
    Dragging: boolean;
    PrevX: integer;
  public
    constructor Create(AOwner: TComponent); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
  published
    property AttachedEdit: TNumberEdit read FAttachedEdit write FAttachedEdit;

    property Center;
    property Picture;
    property Proportional;
    property Stretch;
    property Transparent;
    
    property Align;
    property Anchors;
    property AutoSize;
    property Hint;
    property ShowHint;
    property ParentShowHint;
  end;
  
procedure Register;

implementation

const
  crAdjust = 500;

// TAdjustLabel

constructor TAdjustLabel.Create;
begin
  inherited;

  FAttachedEdit := nil;
  Dragging := False;
  if not (csDesigning in ComponentState) then Cursor := crAdjust;
{$IFDEF LCL}
  ControlStyle := ControlStyle + [csCaptureMouse];
  CaptureMouseButtons := CaptureMouseButtons + [mbLeft, mbRight, mbMiddle];
{$ENDIF}
end;

procedure TAdjustLabel.MouseDown;
begin
  inherited;

  Dragging := True;
  PrevX := X;
end;

procedure TAdjustLabel.MouseMove;
begin
  inherited;

  if Dragging and Assigned(AttachedEdit) then
  begin
    with AttachedEdit do if Enabled then
      Value := Value + Increment * (X - PrevX);
    PrevX := X;
  end;
end;

procedure TAdjustLabel.MouseUp;
begin
  inherited;

  Dragging := False;
end;

// TAdjustImage

constructor TAdjustImage.Create;
begin
  inherited;

  FAttachedEdit := nil;
  Dragging := False;
  if not (csDesigning in ComponentState) then Cursor := crAdjust;
{$IFDEF LCL}
  ControlStyle := ControlStyle + [csCaptureMouse];
  CaptureMouseButtons := CaptureMouseButtons + [mbLeft, mbRight, mbMiddle];
{$ENDIF}
end;

procedure TAdjustImage.MouseDown;
begin
  inherited;

  Dragging := True;
  PrevX := X;
end;

procedure TAdjustImage.MouseMove;
begin
  inherited;

  if Dragging and Assigned(AttachedEdit) then
  begin
    with AttachedEdit do if Enabled then
      Value := Value + Increment * (X - PrevX);
    PrevX := X;
  end;
end;

procedure TAdjustImage.MouseUp;
begin
  inherited;

  Dragging := False;
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TAdjustLabel, TAdjustImage]);
end;

initialization
  Screen.Cursors[crAdjust] := Screen.Cursors[crHSplit];
end.
