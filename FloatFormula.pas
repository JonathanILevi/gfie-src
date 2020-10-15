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
unit FloatFormula;

interface

uses
  SysUtils, Math;

type
  TffStrArray = array of string;

  TffError = (ffeNone, ffeNumeric, ffeUndef, ffeParOpen, ffeParClose, ffeChar,
    ffeStackUnderflow, ffeStackOverflow, ffeFuncOverflow);

  TffInstruction = (ocNone,
    ocConst, // load constant
    ocVar, // load variable by index
    ocAdd, ocSub, ocMul, ocDiv,
    ocNeg, ocAbs, ocMax, ocMin, ocPow,
    ocSqrt, ocExp, ocLn, ocSin, ocCos, ocTan);

  TffOpcode = record
    Instruction: TffInstruction;
    Payload: double;
  end;

  TFloatFormula = class
  private
    Opcodes: array of TffOpCode;
  public
    procedure Clear;
    // FormulaStackSize: max stack size to allow
    function MakeFromString(const Code: string; VarNames: TffStrArray;
      FormulaStackSize: integer): TffError;
    // Stack must have at least 8 elements
    // Result will be at Stack[0]
    procedure Invoke(Variables: PDouble; Stack: PDouble);
  end;

implementation

procedure TFloatFormula.Clear;
begin
  SetLength(Opcodes, 0);
end;

function TFloatFormula.MakeFromString;
type
  TStackKind = (skParOpen, skComma, skInstruction);
  TStackItem = record
    Kind: TStackKind;
    Instruction: TffInstruction;
    Preced: integer;
  end;
  TFuncStackItem = record
    Instruction: TffInstruction;
  end;

const
  StackSize = 100;
  PrecedParOpen = -1;
  PrecedComma = 0;
  PrecedAdd = 1;
  PrecedMul = 2;
  PrecedUnary = 10;

var
  i, p, p2, StackCount, FuncStackCount, fsp, fspMin, fspMax: integer;
  Found, CurrOp, PrevOp, PushSuccess: boolean;
  s: string;
  d: double;
  Stack: array[1..StackSize] of TStackItem;
  FuncStack: array[1..StackSize] of TFuncStackItem; // stores 2-arity functions, e.g. max(,)
  oc: TffOpcode;

  procedure ModifyFsp(Down, Up: integer);
  begin
    Dec(fsp, Down);
    if fsp < fspMin then fspMin := fsp;
    Inc(fsp, Up);
    if fsp > fspMax then fspMax := fsp;
  end;

  function CreateStackItem(Kind: TStackKind; Instruction: TffInstruction;
    Preced: integer): TStackItem;
  begin
    Result.Kind := Kind;
    Result.Instruction := Instruction;
    Result.Preced := Preced;
  end;

  function CreateFuncStackItem(Instruction: TffInstruction): TFuncStackItem;
  begin
    Result.Instruction := Instruction;
  end;

  procedure Output(const Opcode: TffOpcode);
  begin
    SetLength(Opcodes, Length(Opcodes)+1);
    Opcodes[Length(Opcodes)-1] := Opcode;
    case Opcode.Instruction of
      ocConst, ocVar: ModifyFsp(0, 1);
      ocAdd, ocSub, ocMul, ocDiv, ocMax, ocMin, ocPow: ModifyFsp(2, 1);
      else ModifyFsp(1, 1);
    end;
  end;

  procedure Pop;
  begin
    with Stack[StackCount] do
    begin
      if Kind = skInstruction then
      begin
        oc.Instruction := Instruction;
        Output(oc);
      end;
    end;

    dec(StackCount);
  end;

  function Push(const Item: TStackItem): boolean; overload;
  begin
    CurrOp := True;

    if Item.Kind <> skParOpen then
      // pop some operators
      while (StackCount <> 0) and
        ( (Stack[StackCount].Preced > Item.Preced) or
          ((Item.Preced <> PrecedUnary) and
           (Stack[StackCount].Preced = Item.Preced)) ) do Pop;
    
    // push the operator
    inc(StackCount);
    Result := (StackCount >= 1) and (StackCount <= StackSize);
    if Result then Stack[StackCount] := Item;
  end;

  function Push(const Item: TFuncStackItem): boolean; overload;
  begin
    inc(FuncStackCount);
    Result := (FuncStackCount >= 1) and (FuncStackCount <= StackSize);
    if Result then FuncStack[FuncStackCount] := Item;
  end;

begin
  Result := ffeNone;

  // initialize
  Clear;
  StackCount := 0;
  FuncStackCount := 0;
  PrevOp := True;
  p := 1;
  fsp := 0;
  fspMin := fsp;
  fspMax := fsp;
  while p <= Length(Code) do
  begin
    p2 := p + 1;
    CurrOp := False;
    PushSuccess := True;

    case Code[p] of
      // whitespace
      #9, #10, #13, #32: begin end;

      // constant
      '0'..'9': begin
        while (p2 <= Length(Code)) and (Code[p2] in ['0'..'9', '.']) do inc(p2);
        if TryStrToFloat(Copy(Code, p, p2 - p), d) then
        begin
          oc.Instruction := ocConst;
          oc.Payload := d;
          Output(oc);
        end else
          begin Result := ffeNumeric; Exit; end;
      end;

      // identifier
      'a'..'z', 'A'..'Z', '_': begin
        while (p2 <= Length(Code)) and
          (Code[p2] in ['a'..'z', 'A'..'Z', '_']) do inc(p2);
        s := Copy(Code, p, p2 - p);

        // constant?
        if s = 'pi' then
        begin
          oc.Instruction := ocConst;
          oc.Payload := Pi;
          Output(oc);
        end else
        if s = 'e' then
        begin
          oc.Instruction := ocConst;
          oc.Payload := Exp(1);
          Output(oc);
        end else
        // function?
        if s = 'abs' then PushSuccess := Push(CreateStackItem(skInstruction, ocAbs, PrecedUnary)) else
        if s = 'max' then PushSuccess := Push(CreateFuncStackItem(ocMax)) else
        if s = 'min' then PushSuccess := Push(CreateFuncStackItem(ocMin)) else
        if s = 'pow' then PushSuccess := Push(CreateFuncStackItem(ocPow)) else
        if s = 'sqrt' then PushSuccess := Push(CreateStackItem(skInstruction, ocSqrt, PrecedUnary)) else
        if s = 'exp' then PushSuccess := Push(CreateStackItem(skInstruction, ocExp, PrecedUnary)) else
        if s = 'ln' then PushSuccess := Push(CreateStackItem(skInstruction, ocLn, PrecedUnary)) else
        if s = 'sin' then PushSuccess := Push(CreateStackItem(skInstruction, ocSin, PrecedUnary)) else
        if s = 'cos' then PushSuccess := Push(CreateStackItem(skInstruction, ocCos, PrecedUnary)) else
        if s = 'tan' then PushSuccess := Push(CreateStackItem(skInstruction, ocTan, PrecedUnary)) else
        // variable?
        begin
          Found := False;

          for i := 0 to Length(VarNames) - 1 do if s = VarNames[i] then
          begin
            oc.Instruction := ocVar;
            oc.Payload := i;
            Output(oc);

            Found := True;
            Break;
          end;

          if not Found then begin Result := ffeUndef; Exit; end;
        end; // var?
      end; // case id

      // operators
      ',': begin
        if FuncStackCount > 0 then
        begin
          // comma of 2-arity function -> handle that function now as a binary operator
          PushSuccess := Push(CreateStackItem(skInstruction, FuncStack[FuncStackCount].Instruction, PrecedComma));
          dec(FuncStackCount);
        end else
          PushSuccess := Push(CreateStackItem(skComma, ocNone, PrecedComma));
      end;
      '+': if not PrevOp then PushSuccess := Push(CreateStackItem(skInstruction, ocAdd, PrecedAdd));
      '-': if PrevOp then
        PushSuccess := Push(CreateStackItem(skInstruction, ocNeg, PrecedUnary))
        else PushSuccess := Push(CreateStackItem(skInstruction, ocSub, PrecedAdd));
      '*': PushSuccess := Push(CreateStackItem(skInstruction, ocMul, PrecedMul));
      '/': PushSuccess := Push(CreateStackItem(skInstruction, ocDiv, PrecedMul));
      '(': PushSuccess := Push(CreateStackItem(skParOpen, ocNone, PrecedParOpen));
      ')': begin
        // pop all operators and the opening parenthesis
        while (StackCount <> 0) and (Stack[StackCount].Kind <> skParOpen) do Pop;
        if StackCount = 0 then begin Result := ffeParClose; Exit; end;
        dec(StackCount);
      end;

      else Exit(ffeChar);
    end; // case Code[p]
    if not PushSuccess then
      Exit(ffeStackOverflow);

    PrevOp := CurrOp;
    p := p2;
  end; // while p

  // pop all operators
  while (StackCount <> 0) and (Stack[StackCount].Kind <> skParOpen) do Pop;
  // unpaired opening par?
  if StackCount <> 0 then begin Result := ffeParOpen; Exit; end;
  // stack underflow?
  if (fspMin < 0) or (fsp < 1) then begin Result := ffeStackUnderflow; Exit; end;
  // stack overflow?
  if (fspMax > FormulaStackSize) or (fsp > 1) then begin Result := ffeStackOverflow; Exit; end;
  // invalid 2-arity function?
  if FuncStackCount > 0 then begin Result := ffeFuncOverflow; Exit; end;
end;

procedure TFloatFormula.Invoke;
var
  i, sp: integer;

begin
  sp := 0;
  for i := 0 to Length(Opcodes) - 1 do
  with Opcodes[i] do
  case Instruction of
    ocConst: begin Stack[sp] := Payload; Inc(sp); end;
    ocVar: begin Stack[sp] := Variables[Trunc(Payload)]; Inc(sp); end;
    ocAdd: begin Dec(sp); Stack[sp-1] += Stack[sp]; end;
    ocSub: begin Dec(sp); Stack[sp-1] -= Stack[sp]; end;
    ocMul: begin Dec(sp); Stack[sp-1] *= Stack[sp]; end;
    ocDiv: begin Dec(sp); Stack[sp-1] /= Stack[sp]; end;
    ocNeg: Stack[sp-1] := -Stack[sp-1];
    ocAbs: Stack[sp-1] := Abs(Stack[sp-1]);
    ocMax: begin Dec(sp); if Stack[sp] > Stack[sp-1] then Stack[sp-1] := Stack[sp]; end;
    ocMin: begin Dec(sp); if Stack[sp] < Stack[sp-1] then Stack[sp-1] := Stack[sp]; end;
    ocPow: begin Dec(sp); Stack[sp-1] := Power(Stack[sp-1], Stack[sp]); end;
    ocSqrt: Stack[sp-1] := Sqrt(Stack[sp-1]);
    ocExp: Stack[sp-1] := Exp(Stack[sp-1]);
    ocLn: Stack[sp-1] := Ln(Stack[sp-1]);
    ocSin: Stack[sp-1] := Sin(Stack[sp-1]);
    ocCos: Stack[sp-1] := Cos(Stack[sp-1]);
    ocTan: Stack[sp-1] := Tan(Stack[sp-1]);
  end;
end;

end.
