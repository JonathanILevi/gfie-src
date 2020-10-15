(*
	Greenfish Controls - NumberEdit
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
unit NumberEdit;

// Edit box for number input
// Can parse mathematical expressions like 230%(8-5)

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, {$ENDIF}
  SysUtils, Classes, Controls, StdCtrls, Math;

type
  TNumberEdit = class(TEdit)
  private
    FMin, FMax, FIncrement: double;
    function GetValue: double;
    procedure SetValue(const NewValue: double);
  protected
    procedure DoExit; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
  public
    function IntoBounds(d: double): double;
    class function TryExprToFloat(s: string; out Value: double): boolean;
    constructor Create(AOwner: TComponent); override;
  published
    property Min: double read FMin write FMin;
    property Max: double read FMax write FMax;
    property Value: double read GetValue write SetValue stored False;
    property Increment: double read FIncrement write FIncrement;

    property Align;
    property Hint;
    property ShowHint;
    property ParentShowHint;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
  end;

procedure Register;

implementation

function TNumberEdit.GetValue;
begin
  TryExprToFloat(Text, Result);
  Result := IntoBounds(Result);
end;

procedure TNumberEdit.SetValue;
begin
  Text := FloatToStr(IntoBounds(NewValue));
end;

procedure TNumberEdit.DoExit;
begin
  Value := Value; // put value into bounds
  
  inherited;
end;

function TNumberEdit.DoMouseWheel;
begin
  Result := True;
  Value := Value + Increment * Sign(WheelDelta);

  // why does inherited; not work??
  inherited DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TNumberEdit.IntoBounds;
begin
  Result := d;
  
  if Min < Max then
  begin
    if Result < Min then Result := Min else
    if Result > Max then Result := Max;
  end;
end;

(* Parse a mathematical expression
  Tokens are:
  - number: starts with 0..9 and has chars [0..9 DecimalSeparator +-e]
  - ( ) + - * / % ^(power)
  - exp ln sqrt sqr sin cos tan asin acos atan round floor ceil
*)
class function TNumberEdit.TryExprToFloat(s: string; out Value: double): boolean;
const
  cWhiteSpace = [#0..#32, #255];

type
  TOperator = (opParOpen, opParClose, // fake operators
    opAdd, opSub, opNeg, opMul, opDiv, opMod, opPower,
    opExp, opLn, opSqrt, opSqr, opSin, opCos, opTan, opAsin, opAcos, opAtan,
    opRound, opFloor, opCeil);

const
  // Smaller value -> higher precedence
  // Paropen must have a very low precedence
  // to prevent popping it out from the stack
  // Parclose is never pushed, so its precedence does not matter
  Preced: array[TOperator] of integer =
    (10, 0,
     3, 3, 0, 2, 2, 2, 1,
     0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 0, 0);
  UnaryPreced = 0;
  opStr: array[TOperator] of string =
    ('(', ')',
     '+', '-', '~', '*', '/', '%', '^',
     'exp', 'ln', 'sqrt', 'sqr', 'sin', 'cos', 'tan', 'asin', 'acos', 'atan',
     'round', 'floor', 'ceil');
  // The minimum stack size an operator requires
  opMinStack: array[TOperator] of integer =
    (0, 0,
     2, 2, 1, 2, 2, 2, 2,
     1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
     1, 1, 1);

const
  STACKMAX = 20;

var
  p, p2: integer;
  d: double;
  dot: char;
  op, _op: TOperator;
  // operator stack
  stOp: array[0..STACKMAX] of TOperator;
  stOpCount: integer;
  // number stack
  stNum: array[0..STACKMAX] of double;
  stNumCount: integer;
  PrevWasOperand: boolean;

label
  Continue_Parsing;

  function PopOperator: boolean;
  var
    op: TOperator;
    x, a, b: double;
    emSave: TFPUExceptionMask;

  begin
    Result := False;

    // stack underflow?
    if stOpCount = 0 then Exit;
    op := stOp[stOpCount-1];
    dec(stOpCount);

    // perform
    if stNumCount < opMinStack[op] then Exit;
    x := 0; // result
    b := stNum[stNumCount-1]; // last parameter
    a := stNum[stNumCount-opMinStack[op]]; // first parameter

    // TODO: maybe this FPU exception catching is not done properly
    try
      emSave := GetExceptionMask;
      SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
        exOverflow, exUnderflow, exPrecision]);
      try
        case op of
          opAdd: x := a+b;
          opSub: x := a-b;
          opNeg: x := -a;
          opMul: x := a*b;
          opDiv: x := a/b;
          opMod: x := a - Floor(a/b)*b;
          opPower: x := Exp(Ln(a)*b);
          opExp: x := Exp(a);
          opLn: x := Ln(a);
          opSqrt: x := Sqrt(a);
          opSqr: x := Sqr(a);
          opSin: x := Sin(a);
          opCos: x := Cos(a);
          opTan: x := Tan(a);
          opAsin: x := ArcSin(a);
          opAcos: x := ArcCos(a);
          opAtan: x := ArcTan(a);
          opRound: x := Round(a);
          opFloor: x := Floor(a);
          opCeil: x := Ceil(a);
        end;

        // prevent further floating-point exceptions when dealing with x
        if IsNan(x) or IsInfinite(x) then Exit;
      finally
        ClearExceptions(False);
        SetExceptionMask(emSave);
      end;
    except
    end;

    dec(stNumCount, opMinStack[op] - 1);
    stNum[stNumCount-1] := x;

    Result := True;
  end;

begin
  s := LowerCase(s);
  Value := 0; // init
  Result := False;

  stOpCount := 0;
  stNumCount := 0;
  dot := DefaultFormatSettings.DecimalSeparator;

  PrevWasOperand := False;
  p := 1;
  while p <= Length(s) do
  begin
    // whitespace?
    if s[p] in cWhiteSpace then
    begin
      inc(p);
      Continue;
    end;

    // number?
    if (s[p] >= '0') and (s[p] <= '9') then
    begin
      p2 := p+1;
      while (p2 <= Length(s)) and
        ( (s[p2] in ['0'..'9', dot, 'e']) or
         ((s[p2-1] = 'e') and (s[p2] in ['+', '-'])) ) do inc(p2);
      // parse number
      if not TryStrToFloat(Copy(s, p, p2-p), d) then Exit;
      // push to stack
      if stNumCount >= STACKMAX then Exit;
      inc(stNumCount);
      stNum[stNumCount - 1] := d;

      PrevWasOperand := True;
      p := p2;
      Continue;
    end;

    // operator or parenthesis?
    for _op := Low(op) to High(op) do
      if Copy(s, p, Length(opStr[_op])) = opStr[_op] then
    begin
      // yes, this is an operator
      // does this minus denote negation?
      op := _op;
      if (op = opSub) and not PrevWasOperand then op := opNeg;

      case op of
        opParOpen: // push to stack
        begin
          if stOpCount >= STACKMAX then Exit;
          inc(stOpCount);
          stOp[stOpCount - 1] := op;
        end; // (

        opParClose: // pop all operators from stack, until opParOpen is reached
        begin
          while (stOpCount >= 1) and (stOp[stOpCount-1] <> opParOpen) do
            if not PopOperator then Exit;
          // no paropen was reached?
          if stOpCount = 0 then Exit;
          // pop the paropen as well
          dec(stOpCount);
        end; // )

        else // pop all operators with greater or equal precedence, then push
        begin
          while (stOpCount >= 1) and
            ( (Preced[stOp[stOpCount - 1]] < Preced[op]) or
              ((Preced[stOp[stOpCount - 1]] = Preced[op]) and
               (Preced[op] <> UnaryPreced)) ) do
              if not PopOperator then Exit;
          if stOpCount = STACKMAX then Exit;
          inc(stOpCount);
          stOp[stOpCount - 1] := op;
        end; // operators
      end; // case op

      // iterate
      PrevWasOperand := (op = opParClose);
      inc(p, Length(opStr[op]));
      goto Continue_Parsing;
    end;

    // unknown token
    Exit;
  Continue_Parsing:
  end; // while p

  // pop all operators remaining
  while stOpCount <> 0 do
  begin
    // unpaired paropen?
    if stOp[stOpCount - 1] = opParOpen then Exit;
    if not PopOperator then Exit;
  end;

  // return the result
  // stack underflow or overflow?
  if stNumCount <> 1 then Exit;
  // ok
  Value := stNum[0];
  Result := True;
end;

constructor TNumberEdit.Create;
begin
  inherited;

  FMin := 0;
  FMax := 0;
  FIncrement := 1;
  Text := '0';
end;

procedure Register;
begin
  RegisterComponents('Greenfish', [TNumberEdit]);
end;

end.
