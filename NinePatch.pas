unit NinePatch;

{$mode delphi}

interface

uses
  Classes, SysUtils, BitmapEx, bmExUtils, ImageTransform, Math, gfMath, Dialogs,
  GenericList;

type

  { TNinePatch }

  TNinePatch = class
  private
    FBitmap: TBitmap32; // reference
    FValid: boolean;
    // The first element is always 0, and signals the start of a fixed part.
    // Each element signals the start of an expandable/fixed part (alternating).
    // The parts can have a length of 0.
    // The last part is bm.Width or bm.Height
    HorzBlocks, VertBlocks: TGenericList<integer>;
    HorzFixedSum, VertFixedSum: integer;
    function IsStripOk(xMin, yMin, xMax, yMax: integer): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Init(bm: TBitmap32);
    function IsNinePatch: boolean;
    procedure Draw(bmDest: TBitmap32; rDest: TRect);
  end;

implementation

uses
  dlgDebug;

{ TNinePatch }

function TNinePatch.IsStripOk(xMin, yMin, xMax, yMax: integer): boolean;
var
  x, y: integer;
  hasBlack: boolean;
  c, otherColor: TColor32;
begin
  hasBlack := false;
  otherColor := cl32Black;
  for x := xMin to xMax-1 do
  for y := yMin to yMax-1 do
  begin
    c := FBitmap.FastGetPixel(x, y);
    if c = cl32Black then
      hasBlack := true
    else if otherColor = cl32Black then
      otherColor := c
    else if c <> otherColor then
      Exit(false); // black + two different colors
  end;
  Result := hasBlack;
end;

constructor TNinePatch.Create;
begin
  HorzBlocks := TGenericList<integer>.Create;
  VertBlocks := TGenericList<integer>.Create;
  Clear;
end;

destructor TNinePatch.Destroy;
begin
  Clear;
  HorzBlocks.Free;
  VertBlocks.Free;
  inherited Destroy;
end;

procedure TNinePatch.Clear;
begin
  FBitmap := nil;
  FValid := false;
  HorzBlocks.Clear;
  VertBlocks.Clear;
  HorzFixedSum:=0;
  VertFixedSum:=0;
end;

procedure TNinePatch.Init(bm: TBitmap32);
var
  prevExpandable, currExpandable: boolean;
  x, y: integer;

begin
  Clear;
  FBitmap := bm;
  if (bm.Width >= 3) and (bm.Height >= 3)
    and IsStripOk(0, 0, bm.Width, 1) and IsStripOk(0, 0, 1, bm.Height) then
  begin
    // decode horizontal blocks
    prevExpandable := false;
    HorzBlocks.Add(1);
    for x := 1 to bm.Width - 2 do
    begin
      currExpandable := (bm.FastGetPixel(x, 0) = cl32Black);
      if not currExpandable then
        inc(HorzFixedSum);
      if currExpandable <> prevExpandable then
        HorzBlocks.Add(x);
      prevExpandable := currExpandable;
    end;
    HorzBlocks.Add(bm.Width-1);
    // decode vertical blocks
    prevExpandable := false;
    VertBlocks.Add(1);
    for y := 1 to bm.Height - 2 do
    begin
      currExpandable := (bm.FastGetPixel(0, y) = cl32Black);
      if not currExpandable then
        inc(VertFixedSum);
      if currExpandable <> prevExpandable then
        VertBlocks.Add(y);
      prevExpandable := currExpandable;
    end;
    VertBlocks.Add(bm.Height-1);

    FValid := (HorzFixedSum < bm.Width-2) and (VertFixedSum < bm.Height-2);
  end;
end;

function TNinePatch.IsNinePatch: boolean;
begin
  Result := FValid;
end;

procedure CalculateNewBlocks(fixedsum, newSize: integer;
  OldBlocks: TGenericList<integer>; var NewBlocks: TArrayOfInteger);
var
  i, sum, w, oldSize, prevSize, prevPos, currSize: integer;
  q: double;

begin
  SetLength(NewBlocks, OldBlocks.Count);
  oldSize := OldBlocks[OldBlocks.Count-1] - OldBlocks[0];

  q := (newSize - fixedsum) / (oldSize - fixedsum);

  sum := 0;
  for i := 0 to OldBlocks.Count-2 do
  begin
    w := OldBlocks[i+1] - OldBlocks[i];
    NewBlocks[i] := Floor(IfThen(Odd(i), q, 1.0) * w);
    sum += NewBlocks[i];
  end;
  NewBlocks[Length(NewBlocks)-1] := newSize;

  if sum < newSize then
  for i := 1 to OldBlocks.Count-2 do if Odd(i) then
  begin
    inc(NewBlocks[i]);
    inc(sum);
    if sum = newSize then
      break;
  end;

  prevSize := 0;
  prevPos := 0;
  for i := 0 to Length(NewBlocks)-2 do
  begin
    currSize := NewBlocks[i];
    NewBlocks[i] := prevPos + prevSize;
    prevSize := currSize;
    prevPos := NewBlocks[i];
  end;
end;

procedure TNinePatch.Draw(bmDest: TBitmap32; rDest: TRect);
var
  xi, yi, xSrc, ySrc, x, y, t: integer;
  NewHorzBlocks, NewVertBlocks: TArrayOfInteger;

begin
  if not FValid then
    Exit;

  if rDest.Width < 0 then
  begin
    t := rDest.Left;
    rDest.Left := rDest.Right;
    rDest.Right := t;
  end;
  if rDest.Height < 0 then
  begin
    t := rDest.Top;
    rDest.Top := rDest.Bottom;
    rDest.Bottom := t;
  end;

  if rDest.Width < HorzFixedSum then
    rDest.Right := rDest.Left + HorzFixedSum;
  if rDest.Height < VertFixedSum then
    rDest.Bottom := rDest.Top + VertFixedSum;

  CalculateNewBlocks(HorzFixedSum, rDest.Width, HorzBlocks, NewHorzBlocks);
  CalculateNewBlocks(VertFixedSum, rDest.Height, VertBlocks, NewVertBlocks);

  xi := 0;
  for x := rDest.Left to rDest.Right - 1 do
  begin
    while (xi < Length(NewHorzBlocks)-2) and (NewHorzBlocks[xi+1] <= x-rDest.Left) do
      inc(xi);
    if (x >= 0) and (x < bmDest.Width) then
    begin
      if (HorzBlocks[xi+1] - HorzBlocks[xi] <= 1) or (NewHorzBlocks[xi+1] - NewHorzBlocks[xi] <= 1) then
        xSrc := HorzBlocks[xi]
      else
        xSrc := Round((x-rDest.Left - NewHorzBlocks[xi]) / (NewHorzBlocks[xi+1]-1 - NewHorzBlocks[xi])
          * (HorzBlocks[xi+1]-1 - HorzBlocks[xi]) + HorzBlocks[xi]);
      yi := 0;
      for y := rDest.Top to rDest.Bottom - 1 do
      begin
        while (yi < Length(NewVertBlocks)-2) and (NewVertBlocks[yi+1] <= y-rDest.Top) do
          inc(yi);
        if (y >= 0) and (y < bmDest.Height) then
        begin
          if (VertBlocks[yi+1] - VertBlocks[yi] <= 1) or (NewVertBlocks[yi+1] - NewVertBlocks[yi] <= 1) then
            ySrc := VertBlocks[yi]
          else
            ySrc := Round((y-rDest.Top - NewVertBlocks[yi]) / (NewVertBlocks[yi+1]-1 - NewVertBlocks[yi])
              * (VertBlocks[yi+1]-1 - VertBlocks[yi]) + VertBlocks[yi]);
          bmDest.PutPixel(x, y, FBitmap.FastGetPixel(xSrc, ySrc));
        end;
      end;
    end;
  end;
end;

end.

