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
// 1-bit and RGBA bitmap objects
unit BitmapEx;

interface

uses
  LclIntf, LclType,
  SysUtils, Types, Classes, bmExUtils, Graphics, Math, gfMath, fnvHash,
  GraphType;

type
  TBrushShape = (bshRounded, bshSquare, bshSlash, bshBackslash,
    bshHoriz, bshVert, bshSpray);
  TGradientKind = (gkLinear, gkRadial, gkConical, gkSpiral);
  TGradientRep = (grNone, grSym, grAsym);

  TBitmap1 = class;

  // Generic virtual canvas with alpha channel support
  // Descendants must implement some basic methods (pixel read/write, painting)
  TBitmapEx = class(TPersistent)
  private
    FFont: TFont;
    // Descendants must use these variables to store the current image extents
    FWidth, FHeight: integer;
    function GetFont: TFont;
  public
    constructor Create;
    destructor Destroy; override;

    // General
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function Hash(OnlyAlpha: boolean): TfnvKey; virtual;

    // Setting and getting image size
    property Width: integer read FWidth;
    property Height: integer read FHeight;
    procedure Resize(NewWidth, NewHeight: integer); virtual; abstract;
    function ClientRect: TRect;

    // Canvas functions
    procedure FillTransparent; virtual; abstract; // fills with transparency
    procedure FillColor(Color: TColor32); virtual; abstract;
    function GetPixel(x, y: integer): TColor32; virtual; abstract;
    procedure SetPixel(x, y: integer; Color: TColor32); virtual; abstract;

    // These perform range-checking
    function SafeGetPixel(x, y: integer): TColor32;
    procedure SafeSetPixel(x, y: integer; Color: TColor32);

    procedure PutPixel(x, y: integer; Color: TColor32); virtual; abstract;
    // --- The following 5 functions must use RGBA blending,
    // and they must erase if Color is transparent
    procedure PutPixelErase(x, y: integer; Color: TColor32); virtual; abstract;
    // These 3 functions must work with all x1-2, y1-2 values
    procedure PaintHorizLine(x1, x2, y: integer; Color: TColor32); virtual; abstract;
    procedure PaintHorizLineMasked(x1, x2, y: integer; Color: TColor32; Mask: TBitmap1); virtual; abstract;
    procedure PaintVertLine(x, y1, y2: integer; Color: TColor32); virtual; abstract;
    // Paints with the specified color according to Mask
    procedure PaintMask(x, y: integer; Mask: TBitmap1; Color: TColor32); virtual;

    // Whether the image has pixel with alpha other than $ff
    function HasTransparency: boolean;
    // Whether the image has the specified pixel value
    function HasColor(Color: TColor32): boolean;
    // Whether the image has only cl32Black and cl32White pixels
    function IsBlackAndWhite: boolean;
    // convert alpha values to 0 or $ff
    procedure ThresholdAlpha;

    // Get the smallest rectangle which contains all the non-transparent pixels
    function GetAutoCropRect: TRect;
    procedure Crop(const r: TRect);

    procedure Draw(xPos, yPos: integer; bm: TBitmapEx);
    procedure TransformDraw(const r: TRect; Angle: double; bm: TBitmapEx; Antialias: boolean);
    procedure CopyRect(x, y: integer; bm: TBitmapEx; const rSrc: TRect);
    procedure CopyRectOverwrite(x, y: integer; Src: TBitmapEx; const rSrc: TRect);
    // Can be overridden for faster operation.
    // Same as CopyRect[Overwrite], but no range checks are necessary.
    procedure CopyRectImpl(x, y: integer; bm: TBitmapEx; const rSrc: TRect); virtual;
    procedure CopyRectOverwriteImpl(x, y: integer; Src: TBitmapEx; const rSrc: TRect); virtual;
    procedure CreateThumbnail(Src: TBitmapEx; MaxSize: integer; Antialias: boolean);

    // Renders the mask of a brush stroke and returns the bounding box of it
    // Mask can be nil
    function StrokeBrush(Count: integer; pt: PPointArray; BrushSize: integer;
      BrushShape: TBrushShape; Color: TColor32; Mask: TBitmap1): TRect;

    // Returns bounding box
    function Rectangle(r: TRect; Color: TColor32; Filled: boolean;
      LineWidth: integer): TRect;
    // Returns bounding box
    function Ellipse(r: TRect; Color: TColor32; Filled: boolean;
      LineWidth: integer): TRect;
    // Returns bounding box
    // Mask can be nil
    function Line(x1, y1, x2, y2: integer; BrushSize: integer;
      BrushShape: TBrushShape; Color: TColor32; Mask: TBitmap1): TRect;
    procedure Polygon(Count: integer; pt: PPointArray; Color: TColor32;
      Filled: boolean; LineWidth: integer);

    property Font: TFont read GetFont;
    function TextExtent(const Text: string): TPoint;
    procedure TextOut(x, y: integer; const Text: string; Color: TColor32;
      AntiAlias: boolean);
  end;

  TBitsCombineMode = (bcmCopy, bcmOr, bcmAnd, bcmAndNot);

  // This implementation can store a 1-bit bitmap
  // whose pixels can be either cl32White (true) or cl32Transparent (false)
  TBitmap1 = class(TBitmapEx)
  protected
    FData: Pointer;

    function GetBit(x, y: integer): boolean; inline;
    procedure SetBit(x, y: integer; const Value: boolean); inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure Resize(NewWidth, NewHeight: integer); override;

    function LineSize: integer; inline;
    function ScanLine(y: integer): PByteArray; inline;

    procedure FillTransparent; override;
    procedure FillColor(Color: TColor32); override;
    function GetPixel(x, y: integer): TColor32; override;
    procedure SetPixel(x, y: integer; Color: TColor32); override;
    procedure PutPixel(x, y: integer; Color: TColor32); override;
    procedure PutPixelErase(x, y: integer; Color: TColor32); override;
    procedure PaintHorizLine(x1, x2, y: integer; Color: TColor32); override;
    procedure PaintHorizLineMasked(x1, x2, y: integer; Color: TColor32; Mask: TBitmap1); override;
    procedure PaintVertLine(x, y1, y2: integer; Color: TColor32); override;
    procedure PaintMask(x, y: integer; Mask: TBitmap1; Color: TColor32); override;

    procedure Invert;
    procedure Combine(bm: TBitmap1; Mode: TBitsCombineMode);

    // Returns bounding box
    function CreateBrushMask(Count: integer; pt: PPointArray;
      BrushSize: integer; BrushShape: TBrushShape): TRect;
    // Creates the mask for a filled polygon
    procedure CreatePolygonMask(Count: integer; pt: PPointArray);

    property Data: Pointer read FData;
    property Bits[x, y: integer]: boolean read GetBit write SetBit; default;
  end;

  // This descendant is a fully-implemented 32-bit bitmap
  TBitmap32 = class(TBitmapEx)
  private
    FData: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ToBitmap(bm: TBitmap; BackColor: TColor);

    procedure Resize(NewWidth, NewHeight: integer); override;

    procedure FillTransparent; override;
    procedure FillColor(Color: TColor32); override;
    function IsOpaque: boolean;

    function FastGetPixel(x, y: integer): TColor32; inline;
    procedure FastSetPixel(x, y: integer; Color: TColor32); inline;
    function GetPixel(x, y: integer): TColor32; override;
    procedure SetPixel(x, y: integer; Color: TColor32); override;

    procedure PutPixel(x, y: integer; Color: TColor32); override;
    procedure PutPixelErase(x, y: integer; Color: TColor32); override;
    procedure PaintHorizLine(x1, x2, y: integer; Color: TColor32); override;
    procedure PaintHorizLineMasked(x1, x2, y: integer; Color: TColor32; Mask: TBitmap1); override;
    procedure PaintVertLine(x, y1, y2: integer; Color: TColor32); override;

    function ScanLine(y: integer): PColor32Array; inline;
    function PixelAddr(x, y: integer): PColor32; inline;
    procedure FillBehind(c: TColor32);
    procedure Colorize(c: TColor); // preserves opacity
    // Moves the contents of the rSrc region in Src
    // to position (x, y)
    procedure CopyRectImpl(x, y: integer; bm: TBitmapEx; const rSrc: TRect); override;
    procedure CopyRectOverwriteImpl(x, y: integer; Src: TBitmapEx; const rSrc: TRect); override;

    procedure Gradient(c1, c2: TColor32; p1, p2: TPoint; rClip: TRect;
      Kind: TGradientKind; Repetition: TGradientRep);
    // These floodfill functions return the bounding box of the drawn blob
    function CreateFloodFillMask(Mask: TBitmap1;
      x, y, Tolerance: integer; Contiguous: boolean): TRect;
    // IsTransparent(Color) -> erases
    // Mask can be nil
    function FloodFill(x, y, Tolerance: integer; Contiguous: boolean;
      Color: TColor32; Mask: TBitmap1): TRect;

    procedure DrawTo24(bm: TBitmap; xPos, yPos: integer);
    procedure DrawZoomedHatchedTo24(bm: TBitmap; xPos, yPos, xHatchOrigin, yHatchOrigin, Zoom: integer;
      Hatch: THatchDesc);
    // Draws only to pixels whose y is in the interval [yMin, yMax)
    // Shrink can be at most 100.
    procedure DrawShrunkHatchedTo24(bm: TBitmap; xPos, yPos, xHatchOrigin, yHatchOrigin, Shrink: integer;
      Hatch: THatchDesc; Draft: boolean);
    procedure DrawToCanvas(c: TCanvas; x, y: integer; const Hatch: THatchDesc);

    property Data: Pointer read FData;
  end;

  TBitmapEx_Impl = TBitmap32; // for PascalScript import

implementation

uses
  ImageTransform;

procedure CreateRoundedBrushRanges(BrushSize: integer; var xRanges: TArrayOfIntegerRange);
var
  y, Sqr_R: integer;
  q: double;
begin
  SetLength(xRanges, BrushSize);
  // this way the brush tip looks nicer, size = 3 is a special case
  if BrushSize = 3 then
    Sqr_R := Sqr(2)
  else
    Sqr_R := Sqr(BrushSize);
  for y := 0 to BrushSize - 1 do
  begin
    q := Sqrt(Sqr_R - Sqr(2 * y - BrushSize + 1));
    xRanges[y].Max := Floor((BrushSize - 1 + q) * 0.5);
    xRanges[y].Min := Ceil((BrushSize - 1 - q) * 0.5);
  end;
end;

// TBitmapEx

constructor TBitmapEx.Create;
begin
  inherited;
  FWidth := 0;
  FHeight := 0;
end;

destructor TBitmapEx.Destroy;
begin
  if FFont <> nil then
    FFont.Free;
  inherited;
end;

function TBitmapEx.GetFont: TFont;
begin
  if FFont = nil then
    FFont := TFont.Create;
  Result := FFont;
end;

procedure TBitmapEx.Assign;
var
  x, y: integer;
  c: TColor32;
  p: PByteArray;
  bx: TBitmapEx;
  riSrc: TRasterImage;
  sl: TScanLines;

begin
  if Source is TBitmapEx then
  begin
    bx := Source as TBitmapEx;

    Resize(bx.Width, bx.Height);
    for x := 0 to Width - 1 do
      for y := 0 to Height - 1 do
        SetPixel(x, y, bx.GetPixel(x, y));
  end
  else
  if Source is TRasterImage then
  begin
    riSrc := Source as TRasterImage;
    Resize(riSrc.Width, riSrc.Height);

    // This is some workaround
    // direct pixel access in Lazarus is a bit buggy...
    if riSrc.PixelFormat = pf32bit then
    begin
      sl := TScanLines.Create(riSrc);
      try
        for y := 0 to Height - 1 do
        begin
          p := sl[y];
          for x := 0 to Width - 1 do
          begin
            c := FlipColor32(PColor32(p)^);
            // prevent inverted color
            if c and cl32Opaque = 0 then
              c := cl32Transparent;
            SetPixel(x, y, c);
            Inc(PColor32(p));
          end;
        end;
      finally
        sl.Free;
      end;
    end else // no alpha channel
    if riSrc.PixelFormat = pf24bit then
    begin
      c := cl32Opaque;
      sl := TScanLines.Create(riSrc);
      try
        for y := 0 to Height - 1 do
        begin
          p := sl[y];
          for x := 0 to Width - 1 do
          begin
            Move3(p, @c);
            SetPixel(x, y, FlipColor32(c));
            Inc(PByte(p), sl.BytesPerPixel);
          end;
        end;
      finally
        sl.Free;
      end;
    end else // other pixel formats
    begin
      for y := 0 to Height - 1 do
        for x := 0 to Width - 1 do
          SetPixel(x, y, TColor32(riSrc.Canvas.Pixels[x, y]) or cl32Opaque);
    end;
  end
  else
    inherited;
end;

procedure TBitmapEx.AssignTo;
var
  bm: TBitmap;
  pic: TPicture;
  sl: TScanLines;
  x, y: integer;
  p: PColor32;

begin
  if Dest is TBitmap then
  begin
    bm := Dest as TBitmap;
    bm.PixelFormat := pf32bit;
    bm.Width := Width;
    bm.Height := Height;

    sl := TScanLines.Create(bm);
    try
      for y := 0 to Height - 1 do
      begin
        p := PColor32(sl[y]);
        for x := 0 to Width - 1 do
        begin
          PColor32(p)^ := FlipColor32(GetPixel(x, y));
          Inc(PByte(p), sl.BytesPerPixel);
        end;
      end;
    finally
      sl.Free;
    end;
  end
  else if Dest is TPicture then
  begin
    pic := Dest as TPicture;
    bm := TBitmap.Create;
    try
      AssignTo(bm);
      pic.Assign(bm);
    finally
      bm.Free;
    end;
  end else
    inherited;
end;

function TBitmapEx.Hash;
var
  i, x, y: integer;
  c: TColor32;

begin
  Result := fnvNull;

  i := Width;
  Result := fnvAppend(Result, @i, SizeOf(i));
  i := Height;
  Result := fnvAppend(Result, @i, SizeOf(i));

  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
    begin
      c := GetPixel(x, y);
      if OnlyAlpha then
        Result := fnvAppend(Result, PByte(@c) + 3, 1) else
        Result := fnvAppend(Result, @c, SizeOf(c));
    end;
end;

function TBitmapEx.ClientRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

function TBitmapEx.SafeGetPixel;
begin
  if (x >= 0) and (x < Width) and (y >= 0) and (y < Height) then
    Result := GetPixel(x, y)
  else
    Result := cl32Transparent;
end;

procedure TBitmapEx.SafeSetPixel;
begin
  if (x >= 0) and (x < Width) and (y >= 0) and (y < Height) then
    SetPixel(x, y, Color);
end;

procedure TBitmapEx.PaintMask;
var
  i, j: integer;

begin
  for i := Max(0, x) to Min(Width, x + Mask.Width) - 1 do
    for j := Max(0, y) to Min(Height, y + Mask.Height) - 1 do
      if Mask[i - x, j - y] then
        PutPixelErase(i, j, Color);
end;

function TBitmapEx.HasTransparency;
var
  x, y: integer;

begin
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
      if GetPixel(x, y) shr 24 <> $ff then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TBitmapEx.HasColor;
var
  x, y: integer;

begin
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
      if GetPixel(x, y) = Color then
      begin
        Result := True;
        Exit;
      end;
  Result := False;
end;

function TBitmapEx.IsBlackAndWhite;
var
  x, y: integer;
  c: TColor32;

begin
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
    begin
      c := GetPixel(x, y);
      if (c <> cl32Black) and (c <> cl32White) then
      begin
        Result := False;
        Exit;
      end;
    end;
  Result := True;
end;

procedure TBitmapEx.ThresholdAlpha;
var
  x, y: integer;
  c: TColor32;

begin
  for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
    begin
      c := GetPixel(x, y);

      if (c shr 24) and $80 <> 0 then
        SetPixel(x, y, c or cl32Opaque)
      else
      if c <> cl32Inverted then
        SetPixel(x, y, cl32Transparent);
    end;
end;

function TBitmapEx.GetAutoCropRect;
var
  x, y: integer;

label
  Break_yMin, Break_yMax, Break_xMin, Break_xMax;

begin
  Result := Rect(Width, Height, 0, 0);

  for y := 0 to Height - 1 do
    for x := 0 to Width - 1 do
      if not IsTransparent(GetPixel(x, y)) then
      begin
        Result.Top := y;
        goto Break_yMin;
      end;
  Break_yMin:

    // is the whole image transparent?
    if Result.Top = Height then
      Exit;

  for y := Height - 1 downto 0 do
    for x := 0 to Width - 1 do
      if not IsTransparent(GetPixel(x, y)) then
      begin
        Result.Bottom := y + 1;
        goto Break_yMax;
      end;
  Break_yMax:

    for x := 0 to Width - 1 do
      for y := 0 to Height - 1 do
        if not IsTransparent(GetPixel(x, y)) then
        begin
          Result.Left := x;
          goto Break_xMin;
        end;
  Break_xMin:

    for x := Width - 1 downto 0 do
      for y := 0 to Height - 1 do
        if not IsTransparent(GetPixel(x, y)) then
        begin
          Result.Right := x + 1;
          goto Break_xMax;
        end;
  Break_xMax: ;
end;

procedure TBitmapEx.Crop;
var
  bm: TBitmapEx;

begin
  if (r.Width <= 0) or (r.Height <= 0) then
    Exit;

  bm := (ClassType.Create) as TBitmapEx;
  try
    bm.Resize(r.Width, r.Height);
    bm.Draw(-r.Left, -r.Top, Self);
    Assign(bm);
  finally
    bm.Free;
  end;
end;

procedure TBitmapEx.Draw;
var
  x, y, xMin, xMax: integer;

begin
  xMin := Max(0, xPos);
  xMax := Min(Width, xPos + bm.Width) - 1;
  for y := Max(0, yPos) to Min(Height, yPos + bm.Height) - 1 do
    for x := xMin to xMax do
      PutPixel(x, y, bm.GetPixel(x - xPos, y - yPos));
end;

procedure TBitmapEx.TransformDraw;
var
  bmTemp: TBitmap32;
  NoRotation, NoStretch: boolean;

begin
  if (bm.Width = 0) or (bm.Height = 0) then Exit;

  NoRotation := IsZero(Angle, 1e-5);
  NoStretch := (r.Width = bm.Width) and (r.Height = bm.Height);

  if NoRotation then
    StretchDraw(bm, Self, r, Antialias) else
  begin // rotate
    if NoStretch then
      RotateDraw(bm, Self, r.xCenter, r.yCenter, Angle, Antialias) else
    begin
      // stretch and rotate
      bmTemp := TBitmap32.Create;
      try
        bmTemp.Resize(r.Width, r.Height);
        StretchDraw(bm, bmTemp, bmTemp.ClientRect, Antialias);
        RotateDraw(bmTemp, Self, r.xCenter, r.yCenter, Angle, Antialias)
      finally
        bmTemp.Free;
      end;
    end; // if stretch
  end; // if rotate
end;

procedure TBitmapEx.CopyRect;
var
  r: TRect;

begin
  if bm = Self then Exit;

  // calculate a new rSrc
  r.Left := Max(0, rSrc.Left);
  r.Right := Min3i(rSrc.Left + Width - x, bm.Width, rSrc.Right);
  if r.Right <= r.Left then Exit;
  r.Top := Max(0, rSrc.Top);
  r.Bottom := Min3i(rSrc.Top + Height - y, bm.Height, rSrc.Bottom);
  if r.Bottom <= r.Top then Exit;

  // calculate new x and y values
  inc(x, r.Left - rSrc.Left);
  inc(y, r.Top - rSrc.Top);

  CopyRectImpl(x, y, bm, r);
end;

procedure TBitmapEx.CopyRectImpl;
var
  i, j: integer;
begin
  for i := rSrc.Left to rSrc.Right - 1 do
  for j := rSrc.Top to rSrc.Bottom - 1 do
    PutPixel(x + i - rSrc.Left, y + j - rSrc.Top, bm.GetPixel(i, j));
end;

procedure TBitmapEx.CopyRectOverwrite;
var
  r: TRect;

begin
  if Src = Self then Exit;

  r.Left := Max(0, rSrc.Left);
  r.Right := Min3i(rSrc.Left + Width - x, Src.Width, rSrc.Right);
  if r.Width <= 0 then Exit;
  r.Top := Max(0, rSrc.Top);
  r.Bottom := Min3i(rSrc.Top + Height - y, Src.Height, rSrc.Bottom);
  if r.Height <= 0 then Exit;

  x += r.Left - rSrc.Left;
  y += r.Top - rSrc.Top;

  CopyRectOverwriteImpl(x, y, Src, r);
end;

procedure TBitmapEx.CopyRectOverwriteImpl;
var
  x2, y2: integer;

begin
  for y2 := rSrc.Top to rSrc.Bottom - 1 do
    for x2 := rSrc.Left to rSrc.Right - 1 do
      SetPixel(x2 + x - rSrc.Left, y2 + y - rSrc.Top, Src.GetPixel(x2, y2));
end;

procedure TBitmapEx.CreateThumbnail;
begin
  if (Src.Width <= MaxSize) and (Src.Height <= MaxSize) then
    Assign(Src)
  else
  begin
    if Src.Width > Src.Height then
      Resize(MaxSize, MaxSize * Src.Height div Src.Width)
    else
      Resize(MaxSize * Src.Width div Src.Height, MaxSize);

    FillTransparent;
    TransformDraw(ClientRect, 0, Src, Antialias);
  end;
end;

function TBitmapEx.StrokeBrush;
var
  BrushMask: TBitmap1;
  x, y: integer;

begin
  BrushMask := TBitmap1.Create;
  try
    // render the mask
    BrushMask.Resize(Width, Height);
    Result := BrushMask.CreateBrushMask(Count, pt, BrushSize, BrushShape);
    if Assigned(Mask) then
      BrushMask.Combine(Mask, bcmAnd);

    // paint/recolor/... the area
    for x := Result.Left to Result.Right - 1 do
      for y := Result.Top to Result.Bottom - 1 do
        if BrushMask[x, y] then
          PutPixelErase(x, y, Color);
  finally
    BrushMask.Free;
  end;
end;

function TBitmapEx.Rectangle;
var
  rInner: TRect;

  procedure DoFill(x1, y1, x2, y2: integer);
  var
    y: integer;

  begin
    for y := Max(0, y1) to Min(Height - 1, y2) do
      PaintHorizLine(x1, x2, y, Color);
  end;

begin
  // change orientation to default
  r := DefaultRectOrientation(r);

  // too small rectangles act as if they were filled
  if Filled or (r.Width + 1 <= 2 * LineWidth) or
    (r.Height + 1 <= 2 * LineWidth) then
  begin
    DoFill(r.Left, r.Top, r.Right, r.Bottom);
  end
  else
  begin
    rInner := r.Inflate(-LineWidth, -LineWidth);

    DoFill(r.Left, r.Top, r.Right, rInner.Top - 1);
    DoFill(r.Left, rInner.Bottom + 1, r.Right, r.Bottom);
    DoFill(r.Left, rInner.Top, rInner.Left - 1, rInner.Bottom);
    DoFill(rInner.Right + 1, rInner.Top, r.Right, rInner.Bottom);
  end;

  Result := Rect(Max(0, r.Left), Max(0, r.Top), Min(Width, r.Right + 1),
    Min(Height, r.Bottom + 1));
end;

function TBitmapEx.Ellipse;
var
  a, b, x, y, x2, y2, xMin, xMax, xMin2, xMax2, xMinPrev, Count: integer;
  Points: array of TPoint;
  xMid, yMid, q: double;

begin
  // change orientation to default
  r := DefaultRectOrientation(r);

  if not Filled then
  begin
    Count := 0;
    SetLength(Points, (r.Width + r.Height + 2) * 2);
  end;

  // calculate sizes of two axes (a = Sqr(2*a))
  a := Sqr(r.Width + 1);
  b := Sqr(r.Height + 1);
  // special case: looks nicer this way
  if (a = 9) and (b = 9) then
  begin
    a := 4;
    b := 4;
  end;
  // cache
  xMid := (r.Left + r.Right) * 0.5;
  yMid := (r.Top + r.Bottom) * 0.5;

  xMinPrev := 0;
  for y := r.Top to (r.Top + r.Bottom) div 2 do
  begin
    // determine two endpoints of intersection line
    if b = 0 then
      q := 0
    else
      q := Sqrt(a * (b - 4 * Sqr(y - yMid)) / (4 * b));
    xMin := Ceil(xMid - q);
    xMax := Floor(xMid + q);
    y2 := r.Top + r.Bottom - y;

    if Filled then
    begin
      xMin := Max(0, xMin);
      xMax := Min(Width - 1, xMax);
      // draw two segments
      if (y >= 0) and (y < Height) then
        PaintHorizLine(xMin, xMax, y, Color);
      // y = y2 -> the segment would be drawn twice
      if (y <> y2) and (y2 >= 0) and (y2 < Height) then
        PaintHorizLine(xMin, xMax, y2, Color);
    end
    else
    begin
      if y = r.Top then
      begin
        xMin2 := xMin;
        xMax2 := (r.Left + r.Right) div 2;
      end
      else
      begin
        xMin2 := xMin;
        xMax2 := xMinPrev;
        if xMinPrev > xMin then
          Dec(xMax2);
      end;

      for x := xMin2 to xMax2 do
      begin
        x2 := r.Left + r.Right - x;

        Inc(Count);
        Points[Count - 1] := Point(x, y);
        if x2 <> x then
        begin
          Inc(Count);
          Points[Count - 1] := Point(x2, y);
        end;
        if y2 <> y then
        begin
          Inc(Count);
          Points[Count - 1] := Point(x, y2);
          if x2 <> x then
          begin
            Inc(Count);
            Points[Count - 1] := Point(x2, y2);
          end;
        end;
      end;

      xMinPrev := xMin;
    end; // if Filled
  end; // for y

  // paint the outline
  if Filled then
    Result := Rect(Max(0, r.Left), Max(0, r.Top), Min(Width, r.Right + 1),
      Min(Height, r.Bottom + 1))
  else
    Result := StrokeBrush(Count, @Points[0], LineWidth, bshRounded, Color, nil);
end;

function TBitmapEx.Line(x1, y1, x2, y2: integer; BrushSize: integer;
      BrushShape: TBrushShape; Color: TColor32; Mask: TBitmap1): TRect;
var
  i, mid, x, y, yIndex, xLeft, yTop: integer;
  Points: TDynPointArray;
  brushXRanges: TArrayOfIntegerRange;
  xSlices: TArrayOfIntegerRange; // x slices for each y (offset is yMin)

begin
  if BrushSize <= 0 then
    Exit(Rect(0, 0, 0, 0));

  EnumLinePoints(x1, y1, x2, y2, Points);
  if (BrushShape = bshSpray) then
    Result := StrokeBrush(Length(Points), @Points[0], BrushSize, BrushShape, Color, Mask)
  else
  begin
    mid := (BrushSize - 1) div 2;
    Result := Rect(Max(0, Min(x1, x2) - mid), Max(0, Min(y1, y2) - mid),
      Min(Width, Max(x1, x2) - mid + BrushSize), Min(Height, Max(y1, y2) - mid + BrushSize));
    if (Result.Width <= 0) or (Result.Height <= 0) then
      Exit;
    if BrushSize = 1 then
    begin
      for i := 0 to Length(Points) - 1 do
      if PtInRect(Result, Points[i]) then
      begin
        x := Points[i].X;
        y := Points[i].Y;
        if not Assigned(Mask) or Mask.Bits[x, y] then
          PutPixelErase(x, y, Color);
      end;
    end else
    begin
      SetLength(xSlices, Result.Height);
      for i := 0 to Length(xSlices) - 1 do
        xSlices[i].Clear;

      // create a brush tip image if needed
      if BrushShape = bshRounded then
        CreateRoundedBrushRanges(BrushSize, brushXRanges);

      for i := 0 to Length(Points) - 1 do
      begin
        xLeft := Points[i].X - mid;
        yTop := Points[i].Y - mid;

        case BrushShape of
          bshRounded: for y := 0 to BrushSize-1 do
          begin
            yIndex := yTop+y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].IncludeRange(brushXRanges[y].Min + xLeft, brushXRanges[y].Max + xLeft);
          end;
          bshSquare: for y := 0 to BrushSize-1 do
          begin
            yIndex := yTop+y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].IncludeRange(xLeft, xLeft+BrushSize-1);
          end;

          bshSlash: for y := 0 to BrushSize-1 do
          begin
            yIndex := yTop+y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].Include(xLeft + BrushSize-1 - y);
          end;
          bshBackslash: for y := 0 to BrushSize-1 do
          begin
            yIndex := yTop+y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].Include(xLeft + y);
          end;

          bshHoriz:
          begin
            yIndex := Points[i].Y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].IncludeRange(xLeft, xLeft+BrushSize-1);
          end;
          bshVert: for y := 0 to BrushSize-1 do
          begin
            yIndex := yTop+y - Result.Top;
            if (yIndex >= 0) and (yIndex < Length(xSlices)) then
              xSlices[yIndex].Include(Points[i].X);
          end;
        end; // case shape
      end; // for i (points)

      for i := 0 to Length(xSlices) - 1 do
      begin
        if not xSlices[i].Empty then
        begin
          y := Result.Top + i;
          PaintHorizLineMasked(xSlices[i].Min, xSlices[i].Max, y, Color, Mask);
        end;
      end;
    end; // if brush greater than 1
  end; // if not spray
end;

procedure TBitmapEx.Polygon;
var
  i, i2: integer;
  Mask: TBitmap1;

begin
  Mask := TBitmap1.Create;
  try
    Mask.Resize(Width, Height);

    if Filled then
      Mask.CreatePolygonMask(Count, pt)
    else
      for i := 0 to Count - 1 do
      begin
        i2 := (i + 1) mod Count;
        Mask.Line(pt[i].X, pt[i].Y, pt[i2].X, pt[i2].Y, LineWidth, bshRounded,
          cl32White, nil);
      end;

    PaintMask(0, 0, Mask, Color);
  finally
    Mask.Free;
  end;
end;

function TBitmapEx.TextExtent;
var
  bm: TBitmap;

begin
  bm := TBitmap.Create;
  try
    bm.Canvas.Font.Assign(Font);
    with bm.Canvas.TextExtent(Text) do
      Result := Point(cx, cy);
  finally
    bm.Free;
  end;
end;

procedure TBitmapEx.TextOut;
var
  bm: TBitmap;
  xMin, xMax, x2, y2, xSize, ySize, i, j, AlphaStart, aaLevel, Alpha: integer;
  c: TColor32;
  a: array[0..3] of byte absolute c;
  sl: TScanLines;
  isInverted: boolean;

const
  log_aaLevel = 2;

begin
  if IsTransparent(Color) then
    Exit;
  isInverted := (Color = cl32Inverted);
  if isInverted then
    Antialias := False;

  if Antialias then
    aaLevel := 1 shl log_aaLevel
  else
    aaLevel := 1;

  // create mask
  bm := TBitmap.Create;
  try
    bm.PixelFormat := pf24bit;
    bm.Canvas.Brush.Color := clBlack;
    bm.Canvas.Font.Assign(Font);
    with bm.Canvas.Font do
      Size := Size * aaLevel;

    // + ('M') : some letters exceed the bounding box
    xSize := bm.Canvas.TextWidth(Text+'M');
    ySize := bm.Canvas.TextHeight('Wg');
    bm.Width := aaLevel * xSize;
    bm.Height := aaLevel * ySize;
    bm.Canvas.FillRect(Rect(0, 0, bm.Width, bm.Height));

    // draw
    bm.Canvas.Font.Color := clWhite;
    bm.Canvas.TextOut(0, 0, Text);

    // convert
    xMin := Max(0, x);
    xMax := Min(Width, x + xSize) - 1;
    c := Color;
    AlphaStart := a[3];
    sl := TScanLines.Create(bm);
    try
      for y2 := Max(0, y) to Min(Height, y + ySize) - 1 do
      begin
        for x2 := xMin to xMax do
        begin
          if Antialias then
          begin
            Alpha := 0;
            for i := 0 to aaLevel - 1 do
              for j := 0 to aaLevel - 1 do
                Inc(Alpha, sl[(y2 - y) * aaLevel + i][((x2 - x) * aaLevel + j) * sl.BytesPerPixel]);
            Alpha := Alpha shr (2 * log_aaLevel);
          end
          else
          if sl[y2 - y][(x2 - x) * sl.BytesPerPixel] >= $80 then
            Alpha := 255
          else
            Alpha := 0;

          Alpha := Alpha255to256(Alpha);

          if Alpha <> 0 then
          begin
            a[3] := (Alpha * AlphaStart) shr 8;
            if isInverted or (a[3] <> 0) then
              PutPixel(x2, y2, c);
          end;
        end; // for x2
      end; // for y2
    finally
      sl.Free;
    end;
  finally
    bm.Free;
  end;
end;

// TBitmap1

function TBitmap1.GetBit;
begin
  Result := ScanLine(y)[x shr 3] and (1 shl (x and 7)) <> 0;
end;

procedure TBitmap1.SetBit;
var
  sl: PByteArray;
  p: PByte;
  Mask: byte;

begin
  sl := ScanLine(y);
  p := @sl[x shr 3];
  Mask := 1 shl (x and 7);
  if Value then
    p^ := p^ or Mask
  else
    p^ := p^ and not Mask;
end;

constructor TBitmap1.Create;
begin
  inherited;

  FData := nil;
end;

destructor TBitmap1.Destroy;
begin
  FreeAndNilMem(FData);

  inherited;
end;

procedure TBitmap1.Assign;
var
  bm: TBitmap1;

begin
  if Source = Self then Exit;

  if Source is TBitmap1 then
  begin
    bm := Source as TBitmap1;

    Resize(bm.Width, bm.Height);
    Move(bm.Data^, Data^, LineSize * Height);
  end
  else
    inherited;
end;

procedure TBitmap1.Resize;
var
  bmTemp: TBitmap1;

begin
  if NewWidth < 0 then
    NewWidth := 0;
  if NewHeight < 0 then
    NewHeight := 0;

  if (NewWidth = Width) and (NewHeight = Height) then
    Exit;

  if (NewWidth = 0) or (NewHeight = 0) then
  begin
    FWidth := NewWidth;
    FHeight := NewHeight;
    FreeAndNilMem(FData);
  end
  else
  begin
    bmTemp := TBitmap1.Create;
    try
      bmTemp.Assign(Self);

      FWidth := NewWidth;
      FHeight := NewHeight;
      FreeAndNilMem(FData);
      if LineSize * Height > 0 then
        FData := GetMem(LineSize * Height);
      FillTransparent;

      Draw(0, 0, bmTemp);
    finally
      bmTemp.Free;
    end;
  end;
end;

function TBitmap1.LineSize;
begin
  Result := (Width + 7) shr 3;
end;

function TBitmap1.ScanLine;
begin
  PByte(Result) := @PByteArray(Data)[LineSize*y];
end;

procedure TBitmap1.FillTransparent;
begin
  FastZeroMemory(Data, LineSize * Height);
end;

procedure TBitmap1.FillColor;
begin
  FillChar(Data^, LineSize * Height, byte(-integer(Color = cl32White)));
end;

function TBitmap1.GetPixel;
begin
  Result := TColor32(-integer(Bits[x, y]));
end;

procedure TBitmap1.SetPixel;
begin
  Bits[x, y] := (Color = cl32White);
end;

procedure TBitmap1.PutPixel;
begin
  if Color = cl32White then
    Bits[x, y] := True;
end;

procedure TBitmap1.PutPixelErase;
begin
  Bits[x, y] := (Color = cl32White);
end;

procedure TBitmap1.PaintHorizLine;
var
  i, Count: integer;
  Mask1, Mask2: byte;
  p: PByte;
  sl: PByteArray;

begin
  if (y < 0) or (y >= Height) then
    Exit;
  if x1 > x2 then
  begin
    i := x1;
    x1 := x2;
    x2 := i;
  end;
  if (x2 < 0) or (x1 >= Width) then
    Exit;
  x1 := Max(0, x1);
  x2 := Min(Width - 1, x2);

  Mask1 := $ff shl (x1 and 7);
  Mask2 := (1 shl ((x2 and 7) + 1)) - 1;
  sl := ScanLine(y);
  p := @sl[x1 shr 3];

  if Color = cl32White then
  begin
    if x1 xor x2 < 8 then
      // changes are made within the same byte
      p^ := p^ or (Mask1 and Mask2)
    else
    begin
      p^ := p^ or Mask1;
      Inc(p);

      Count := (x2 shr 3) - (x1 shr 3) - 1;
      FillChar(p^, Count, $ff);
      Inc(p, Count);

      p^ := p^ or Mask2;
    end;
  end
  else
  begin
    if x1 xor x2 < 8 then
      // changes are made within the same byte
      p^ := p^ and not (Mask1 and Mask2)
    else
    begin
      p^ := p^ and not Mask1;
      Inc(p);

      Count := (x2 shr 3) - (x1 shr 3) - 1;
      FillChar(p^, Count, 0);
      Inc(p, Count);

      p^ := p^ and not Mask2;
    end;
  end;
end;

procedure TBitmap1.PaintHorizLineMasked(x1, x2, y: integer; Color: TColor32; Mask: TBitmap1);
var
  i, x: integer;

begin
  if Mask = nil then
    PaintHorizLine(x1, x2, y, Color)
  else
  begin
    // TODO deduplicate this code
    if (y < 0) or (y >= Height) then
      Exit;
    if x1 > x2 then
    begin
      i := x1;
      x1 := x2;
      x2 := i;
    end;
    if (x2 < 0) or (x1 >= Width) then
      Exit;
    x1 := Max(0, x1);
    x2 := Min(Width - 1, x2);

    // TODO optimize?
    for x := x1 to x2 do
      if Mask.Bits[x, y] then
        SetPixel(x, y, Color);
  end;
end;

procedure TBitmap1.PaintVertLine;
var
  i, y: integer;
  Mask: byte;
  sl: PByteArray;
  p: PByte;

begin
  if (x < 0) or (x >= Width) then
    Exit;
  if y1 > y2 then
  begin
    i := y1;
    y1 := y2;
    y2 := i;
  end;
  if (y2 < 0) or (y1 >= Height) then
    Exit;
  y1 := Max(0, y1);
  y2 := Min(Height - 1, y2);

  Mask := 1 shl (x and 7);
  sl := ScanLine(y1);
  p := @sl[x shr 3];
  i := LineSize;

  if Color = cl32White then
    for y := y1 to y2 do
    begin
      p^ := p^ or Mask;
      Inc(p, i);
    end
  else
    for y := y1 to y2 do
    begin
      p^ := p^ and not Mask;
      Inc(p, i);
    end;
end;

procedure TBitmap1.PaintMask;
var
  i, j: integer;
  b: boolean;

begin
  b := (Color = cl32White);
  for i := Max(0, x) to Min(Width, x + Mask.Width) - 1 do
    for j := Max(0, y) to Min(Height, y + Mask.Height) - 1 do
      if Mask[i - x, j - y] then
        Bits[i, j] := b;
end;

procedure TBitmap1.Invert;
var
  i: integer;
  p: PByte;

begin
  p := Data;
  for i := 1 to LineSize * Height do
  begin
    p^ := not p^;
    Inc(p);
  end;
end;

procedure TBitmap1.Combine;
var
  x, y: integer;

begin
  for x := 0 to Min(Width, bm.Width) - 1 do
    for y := 0 to Min(Height, bm.Height) - 1 do
      case Mode of
        bcmCopy: Bits[x, y] := bm[x, y];
        bcmOr: if bm[x, y] then
            Bits[x, y] := True;
        bcmAnd: if not bm[x, y] then
            Bits[x, y] := False;
        bcmAndNot: if bm[x, y] then
            Bits[x, y] := False;
      end;
end;

function TBitmap1.CreateBrushMask(Count: integer; pt: PPointArray;
      BrushSize: integer; BrushShape: TBrushShape): TRect;
var
  i, mid, x, y, xLeft, yTop, x1, y1, x2, y2: integer;
  xRanges: TArrayOfIntegerRange;

begin
  // initialize
  FillTransparent;
  if BrushSize <= 0 then
    Exit(Rect(0, 0, 0, 0));

  Result := Rect(High(integer), High(integer), Low(integer), Low(integer));

  // create a brush tip image if needed
  if (BrushSize > 1) and (BrushShape in [bshRounded, bshSpray]) then
    CreateRoundedBrushRanges(BrushSize, xRanges);
  mid := (BrushSize - 1) div 2;

  for i := 0 to Count - 1 do
  begin
    xLeft := pt[i].X - mid;
    yTop := pt[i].Y - mid;

    x1 := Max(0, xLeft);
    y1 := Max(0, yTop);
    x2 := Min(Width, xLeft + BrushSize) - 1;
    y2 := Min(Height, yTop + BrushSize) - 1;

    with Result do
    begin
      if Left > x1 then
        Left := x1;
      if Top > y1 then
        Top := y1;
      if Right < x2 then
        Right := x2;
      if Bottom < y2 then
        Bottom := y2;
    end;

    if BrushSize = 1 then
    begin
      if (xLeft >= 0) and (yTop >= 0) and (xLeft < Width) and (yTop < Height) then
        Bits[xLeft, yTop] := True;
    end
    else
      case BrushShape of
        bshRounded: for y := y1 to y2 do
            PaintHorizLine(xLeft + xRanges[y - yTop].Min, xLeft + xRanges[y - yTop].Max, y, cl32White);
        bshSquare: for y := y1 to y2 do
            PaintHorizLine(x1, x2, y, cl32White);

        bshSlash: for y := y1 to y2 do
          begin
            x := xLeft + BrushSize - 1 - (y - yTop);
            if (x >= 0) and (x < Width - 1) then
              Bits[x, y] := True;
            if y > yTop then
            begin
              Inc(x);
              if (x >= 0) and (x < Width - 1) then
                Bits[x, y] := True;
            end;
          end;
        bshBackslash: for y := y1 to y2 do
          begin
            x := xLeft + y - yTop;
            if (x >= 0) and (x < Width - 1) then
              Bits[x, y] := True;
            if y < yTop + BrushSize - 1 then
            begin
              Inc(x);
              if (x >= 0) and (x < Width - 1) then
                Bits[x, y] := True;
            end;
          end;

        bshHoriz: PaintHorizLine(x1, x2, yTop + mid, cl32White);
        bshVert: PaintVertLine(xLeft + mid, y1, y2, cl32White);

        bshSpray: for y := y1 to y2 do
            for x := Max(0, xLeft + xRanges[y - yTop].Min) to
              Min(Width - 1, xLeft + xRanges[y - yTop].Max) do
              if Random($100) * BrushSize < 80 then
                Bits[x, y] := True;
      end; // case shape
  end; // for i (points)

  Inc(Result.Right);
  Inc(Result.Bottom);
end;

procedure TBitmap1.CreatePolygonMask;
var
  bm: TBitmap;

begin
  bm := TBitmap.Create;
  try
    bm.PixelFormat := pf24bit;
    bm.Width := Width;
    bm.Height := Height;

    bm.Canvas.Brush.Color := clBlack;
    bm.Canvas.FillRect(Rect(0, 0, bm.Width, bm.Height));

    bm.Canvas.Pen.Color := clWhite;
    bm.Canvas.Brush.Color := clWhite;
    bm.Canvas.Polygon(Slice(pt^, Count));

    Assign(bm);
  finally
    bm.Free;
  end;
end;

// TBitmap32

constructor TBitmap32.Create;
begin
  inherited;

  FData := nil;
end;

destructor TBitmap32.Destroy;
begin
  FreeAndNilMem(FData);

  inherited;
end;

procedure TBitmap32.Assign;
var
  bm: TBitmap32;

begin
  if Source = Self then Exit;

  if Source is TBitmap32 then
  begin
    bm := Source as TBitmap32;

    Resize(bm.Width, bm.Height);
    Move(bm.Data^, Data^, 4 * Width * Height);
  end
  else
    inherited;
end;

procedure TBitmap32.ToBitmap;
begin
  bm.PixelFormat := pf24bit;
  bm.Width := Width;
  bm.Height := Height;
  bm.Canvas.Brush.Color := BackColor;
  bm.Canvas.FillRect(ClientRect);
  DrawTo24(bm, 0, 0);
end;

procedure TBitmap32.Resize;
var
  NewData, pSrc, pDest: Pointer;
  i, LineSize: integer;

begin
  if NewWidth < 0 then
    NewWidth := 0;
  if NewHeight < 0 then
    NewHeight := 0;

  if (NewWidth = Width) and (NewHeight = Height) then
    Exit;

  // is the new block an empty memory block?
  if (NewWidth = 0) or (NewHeight = 0) then
    FreeAndNilMem(FData)
  else
  begin
    // allocate new block filled with transparency (zeroes)
    NewData := GetMem(NewWidth * NewHeight * 4);
    FillChar(NewData^, NewWidth*NewHeight*4, 0);

    // copy contents
    if (Width <> 0) and (Height <> 0) then
    begin
      LineSize := Min(Width, NewWidth);
      pSrc := Data;
      pDest := NewData;
      for i := 0 to Min(Height, NewHeight) - 1 do
      begin
        Move(pSrc^, pDest^, 4 * LineSize);
        Inc(PByte(pSrc), Width * 4);
        Inc(PByte(pDest), NewWidth * 4);
      end;

      // delete old block
      FreeAndNilMem(FData);
    end;

    FData := NewData;
  end;

  FWidth := NewWidth;
  FHeight := NewHeight;
end;

procedure TBitmap32.FillTransparent;
begin
  FastZeroMemory(Data, Width * Height * 4);
end;

procedure TBitmap32.FillColor;
begin
  FillDWord(Data^, Width * Height, Cardinal(Color));
end;

function TBitmap32.IsOpaque: boolean;
var
  i: integer;
  p: PColor32;
begin
  Result := True;
  p := Data;
  for i := 0 to Width*Height-1 do
  begin
    if not bmExUtils.IsOpaque(p^) then
    begin
      Result := False;
      break;
    end;
    inc(p);
  end;
end;

function TBitmap32.FastGetPixel;
begin
  Result := PixelAddr(x, y)^;
end;

procedure TBitmap32.FastSetPixel;
begin
  PixelAddr(x, y)^ := Color;
end;

function TBitmap32.GetPixel;
begin
  Result := FastGetPixel(x, y);
end;

procedure TBitmap32.SetPixel;
begin
  FastSetPixel(x, y, Color);
end;

procedure TBitmap32.PutPixel;
var
  p: PColor32;

begin
  p := PixelAddr(x, y);
  p^ := PutPixel32(Color, p^);
end;

procedure TBitmap32.PutPixelErase;
var
  p: PColor32;

begin
  p := PixelAddr(x, y);
  if IsTransparent(Color) then
    p^ := cl32Transparent
  else
    p^ := PutPixel32(Color, p^);
end;

procedure TBitmap32.PaintHorizLine;
var
  i: integer;
  p: PColor32;

begin
  if (y < 0) or (y >= Height) then
    Exit;
  if x1 > x2 then
  begin
    i := x1;
    x1 := x2;
    x2 := i;
  end;
  if (x2 < 0) or (x1 >= Width) then
    Exit;
  x1 := Max(0, x1);
  x2 := Min(Width - 1, x2);

  p := PixelAddr(x1, y);

  if IsTransparent(Color) then
    FillDWord(p^, x2 - x1 + 1, cardinal(cl32Transparent))
  else
    for i := x1 to x2 do
    begin
      p^ := PutPixel32(Color, p^);
      Inc(p);
    end;
end;

procedure TBitmap32.PaintHorizLineMasked(x1, x2, y: integer; Color: TColor32; Mask: TBitmap1);
var
  i, x: integer;

begin
  if Mask = nil then
    PaintHorizLine(x1, x2, y, Color)
  else
  begin
    // TODO deduplicate this code
    if (y < 0) or (y >= Height) then
      Exit;
    if x1 > x2 then
    begin
      i := x1;
      x1 := x2;
      x2 := i;
    end;
    if (x2 < 0) or (x1 >= Width) then
      Exit;
    x1 := Max(0, x1);
    x2 := Min(Width - 1, x2);

    for x := x1 to x2 do
      if Mask.Bits[x, y] then
        PutPixelErase(x, y, Color);
  end;
end;

procedure TBitmap32.PaintVertLine;
var
  i: integer;

begin
  if (x < 0) or (x >= Width) then
    Exit;
  if y1 > y2 then
  begin
    i := y1;
    y1 := y2;
    y2 := i;
  end;
  if (y2 < 0) or (y1 >= Height) then
    Exit;
  y1 := Max(0, y1);
  y2 := Min(Height - 1, y2);

  for i := y1 to y2 do
    PutPixelErase(x, i, Color);
end;

function TBitmap32.ScanLine(y: integer): PColor32Array;
begin
  Result := @PColor32Array(Data)[y * Width];
end;

function TBitmap32.PixelAddr;
begin
  Result := @PColor32Array(Data)[y * Width + x];
end;

procedure TBitmap32.FillBehind;
var
  p: PColor32;
  i: integer;

begin
  p := Data;
  for i := 1 to Width * Height do
  begin
    p^ := PutPixel32(p^, c);
    Inc(p);
  end;
end;

procedure TBitmap32.Colorize;
var
  i: integer;
  p: PColor32;

begin
  p := Data;
  for i := 1 to Width * Height do
  begin
    // inverted counts as a fully opaque color
    if p^ = cl32Inverted then
      p^ := TColor32(c) or cl32Opaque
    else
    // avoid getting inverted
    if PByteArray(p)[3] = 0 then
      p^ := cl32Transparent
    else
      Move3(@c, p);

    Inc(p);
  end;
end;

procedure TBitmap32.CopyRectImpl(x, y: integer; bm: TBitmapEx; const rSrc: TRect);
var
  bm32: TBitmap32;
  i, j: integer;
  pSrc, pDest: PColor32;

begin
  if bm is TBitmap32 then
  begin
    bm32 := TBitmap32(bm);
    for j := rSrc.Top to rSrc.Bottom - 1 do
    begin
      pDest := PixelAddr(x, y + j-rSrc.Top);
      pSrc := bm32.PixelAddr(rSrc.Left, j);
      for i := rSrc.Left to rSrc.Right - 1 do
      begin
        pDest^ := PutPixel32(pSrc^, pDest^);
        inc(pSrc);
        inc(pDest);
      end;
    end;
  end else
    inherited;
end;

procedure TBitmap32.CopyRectOverwriteImpl;
var
  i: integer;
  Src32: TBitmap32;

begin
  if Src is TBitmap32 then
  begin
    Src32 := TBitmap32(Src);
    if (x = 0) and (rSrc.Left = 0) and (rSrc.Right = Src.Width) and (Src.Width = Width) then
      Move(Src32.PixelAddr(0, rSrc.Top)^, PixelAddr(0, y)^, 4*Width*rSrc.Height) else
    for i := rSrc.Top to rSrc.Bottom - 1 do
      Move(Src32.PixelAddr(rSrc.Left, i)^, PixelAddr(x, i-rSrc.Top+y)^, 4*rSrc.Width);
  end else
    inherited;
end;

procedure TBitmap32.Gradient;
const
  FFDivPi = $ff / pi;

var
  Cache: array[0..$ff] of TColor32;
  i, x, y, Grade, Radius: integer;
  q, FFDivRadius, Angle: double;
  p: PColor32;

  function DoSymRep(x: byte): byte; inline;
  begin
    if x >= $80 then Result := not x else Result := x;
  end;

begin
  if not IntersectRect(rClip, rClip, ClientRect) then Exit;

  // calculate some magic values
  Radius := Round(Sqrt(Sqr(p2.X - p1.X) + Sqr(p2.Y - p1.Y)));
  Angle := GetPhi(p2.X - p1.X, p2.Y - p1.Y);
  if Radius <> 0 then FFDivRadius := $ff / Radius else FFDivRadius := 1;
  // build color table
  for i := 0 to $ff do Cache[i] := BlendColors(c1, c2, i);

  for y := rClip.Top to rClip.Bottom - 1 do
  begin
    p := PixelAddr(rClip.Left, y);
    for x := rClip.Left to rClip.Right - 1 do
    begin
      // calculate the grade of this point
      case Kind of
        gkLinear: if Radius = 0 then Grade := 0 else
          Grade := (((p2.Y - p1.Y)*(Y - p1.Y) +
            (p2.X - p1.X)*(X - p1.X)) * $ff) div Sqr(Radius);
        gkRadial: if Radius = 0 then Grade := $ff else
          Grade := Round(Sqrt(Sqr(x - p1.X) + Sqr(y - p1.Y)) * FFDivRadius);
        gkConical, gkSpiral: begin
          q := GetPhi(x - p1.X, y - p1.Y) - Angle;
          if q < 0 then q := q + 2*pi;

          if Kind = gkConical then
          begin
            Grade := Round(q * FFDivPi);
            if Grade and $100 <> 0 then Grade := $1ff  - Grade;
          end else
          // force symmetric repetition
            Grade := DoSymRep(Round(Sqrt(Sqr(x - p1.X) + Sqr(y - p1.Y)) *
              FFDivRadius + q * FFDivPi));
        end;
      end; // case Kind

      // apply Repetition
      case Repetition of
        grNone: if Grade < 0 then Grade := 0 else
          if Grade > $ff then Grade := $ff;
        grSym: Grade := DoSymRep(Grade);
        grAsym: Grade := Grade and $ff;
      end; // case Repetition

      // draw
      p^ := PutPixel32(Cache[Grade], p^);

      // iterate
      inc(p);
    end; // for x
  end; // for y
end;

function TBitmap32.CreateFloodFillMask;
type
  TParams = packed record
    x, y: integer;
  end;

var
  c: TColor32;
  i, j, StackCount, tmp1, tmp2, x2, y2, yMin, yMax: integer;
  Stack: array of TParams;
  xRange: array[0..1] of integer;
  yRange: array of array[0..1] of integer;

  procedure Recurse(x, y: integer); inline;
  begin
    Inc(StackCount);
    if StackCount > Length(Stack) then
      SetLength(Stack, StackCount or $ffff);
    Stack[StackCount - 1].x := x;
    Stack[StackCount - 1].y := y;
  end;

  procedure PopParams(var x, y: integer); inline;
  begin
    x := Stack[StackCount - 1].x;
    y := Stack[StackCount - 1].y;
    Dec(StackCount);
  end;

  function CanRecurse(x, y: integer): boolean; inline;
  begin
    Result := not Mask.Bits[x, y] and CanFloodFill(c,
      PixelAddr(x, y)^, Tolerance);
  end;

  procedure BBoxAddX(x: integer); inline;
  begin
    if x < Result.Left then Result.Left := x else
    if x+1 > Result.Right then Result.Right := x+1;
  end;

  procedure BBoxAddY(y: integer); inline;
  begin
    if y < Result.Top then Result.Top := y else
    if y+1 > Result.Bottom then Result.Bottom := y+1;
  end;

  procedure GetYRange(x, y: integer); inline;
  begin
    // init range
    tmp1 := y;
    tmp2 := y;
    // paint pixel
    Mask.Bits[x, tmp1] := True;

    // move up
    while (tmp1 > 0) and CanRecurse(x, tmp1 - 1) do
    begin
      dec(tmp1);
      Mask.Bits[x, tmp1] := True;
    end;
    // move down
    while (tmp2 < Height-1) and CanRecurse(x, tmp2 + 1) do
    begin
      inc(tmp2);
      Mask.Bits[x, tmp2] := True;
    end;
    // finished finding y range
    yRange[x][0] := tmp1;
    yRange[x][1] := tmp2;

    // update bounding box
    BBoxAddX(x);
    BBoxAddY(tmp1);
    BBoxAddY(tmp2);
  end;

begin
  // initialize result
  Result := Rect(x, y, x+1, y+1);
  Mask.Resize(Width, Height);
  Mask.FillTransparent;
  // range check
  if (x < 0) or (y < 0) or (x >= Width) or (y >= Height) then
    Exit;

  // sample color
  c := PixelAddr(x, y)^;

  // also fill not connected pixels?
  if not Contiguous then
  begin
    for i := 0 to Width - 1 do
    for j := 0 to Height - 1 do
    if CanRecurse(i, j) then
    begin
      Mask.Bits[i, j] := True;
      BBoxAddX(i);
      BBoxAddY(j);
    end;

    Exit;
  end;

  // contiguous fill
  // init
  StackCount := 0;
  SetLength(yRange, Width);
  Recurse(x, y);
  // loop
  while StackCount > 0 do
  begin
    PopParams(x, y);

    // paint a range of pixels
    xRange[0] := x;
    xRange[1] := x;
    GetYRange(x, y);
    while (xRange[0] > 0) and CanRecurse(xRange[0] - 1, y) do
    begin
      dec(xRange[0]);
      GetYRange(xRange[0], y);
    end;
    while (xRange[1] < Width - 1) and CanRecurse(xRange[1] + 1, y) do
    begin
      inc(xRange[1]);
      GetYRange(xRange[1], y);
    end;

    // do the recursion on the boundary of painted pixels
    // but we do not need to examine all of the boundary points
    for x2 := xRange[0] to xRange[1] do
    begin
      if (x2 = xRange[0]) or (x2 = xRange[1]) then begin yMin := y+1; yMax := y-1; end else
			begin
		  	yMin := Min(yRange[x2-1][1], yRange[x2+1][1]) + 1;
		  	yMax := Max(yRange[x2-1][0], yRange[x2+1][0]) - 1;
      end;

      for y2 := yMin to yRange[x2][1] do Recurse(x2, y2);
      for y2 := yRange[x2][0] to yMax do Recurse(x2, y2);
    end;
  end;
end;

function TBitmap32.FloodFill;
var
  bmMask: TBitmap1;

begin
  bmMask := TBitmap1.Create;
  try
    Result := CreateFloodFillMask(bmMask, x, y, Tolerance, Contiguous);
    if Assigned(Mask) then
      bmMask.Combine(Mask, bcmAnd);
    PaintMask(0, 0, bmMask, Color);
  finally
    bmMask.Free;
  end;
end;

procedure TBitmap32.DrawTo24;
var
  y, xStart, xLen: integer;
  sl: TScanLines;

begin
  bm.PixelFormat := pf24bit;
  xStart := Max(xPos, 0);
  xLen := Min(xPos + Width, bm.Width) - xStart;

  sl := TScanLines.Create(bm);
  try
    if sl.BytesPerPixel = 4 then
    begin
      for y := Max(yPos, 0) to Min(yPos + Height, bm.Height) - 1 do
        Put32to24_BGRXScanLine(PixelAddr(xStart - xPos, y - yPos), @sl[y][xStart * 4], xLen)
    end
    else
    begin
      for y := Max(yPos, 0) to Min(yPos + Height, bm.Height) - 1 do
        Put32to24_BGRScanLine(PixelAddr(xStart - xPos, y - yPos), @sl[y][xStart * 3], xLen);
    end;
  finally
    sl.Free;
  end;
end;

procedure TBitmap32.DrawZoomedHatchedTo24;
var
  x, y, xStart, yStart, xEnd, yEnd, xDest, yDest, x2, y2, x2Min, x2Max: integer;
  p: PColor32;
  pDest: Pointer;
  sl: TScanLines;
  c: array[0..1] of TColor;

begin
  if Zoom = 1 then
  begin
    MakeTransparentHatch(bm, Rect(xPos, yPos, xPos + Width, yPos + Height), xHatchOrigin, yHatchOrigin, Hatch);
    DrawTo24(bm, xPos, yPos);
    Exit;
  end;

  bm.PixelFormat := pf24bit;

  // calculate the rectangle of visible pixels
  // the first x value at which (xPos + (x+1) * Zoom >= 0) is xStart
  xStart := Max(0, Ceil(-xPos / Zoom) - 1);
  yStart := Max(0, Ceil(-yPos / Zoom) - 1);
  // the last x value at which (xPos + x * Zoom < bm.Width) is xEnd
  xEnd := Min(Width, Ceil((bm.Width - xPos) / Zoom)) - 1;
  yEnd := Min(Height, Ceil((bm.Height - yPos) / Zoom)) - 1;

  sl := TScanLines.Create(bm);
  try
    yDest := yPos + yStart * Zoom;
    for y := yStart to yEnd do
    begin
      p := PixelAddr(xStart, y);
      xDest := xPos + xStart * Zoom;

      for x := xStart to xEnd do
      begin
        // calculate the two hatch colors
        c[0] := FlipColor(Put32to24(Hatch.Color[0], p^));
        c[1] := FlipColor(Put32to24(Hatch.Color[1], p^));

        // draw the hatch square
        x2Min := Max(0, xDest);
        x2Max := Min(bm.Width, xDest + Zoom) - 1;
        for y2 := Max(0, yDest) to Min(bm.Height, yDest + Zoom) - 1 do
        begin
          pDest := @sl[y2][x2Min * sl.BytesPerPixel];
          for x2 := x2Min to x2Max do
          begin
            Move3(@c[(((y2-yHatchOrigin) shr 2) xor ((x2-xHatchOrigin) shr 2)) and 1], pDest);
            Inc(PByte(pDest), sl.BytesPerPixel);
          end;
        end;

        // iterate
        Inc(p);
        Inc(xDest, Zoom);
      end; // for x

      Inc(yDest, Zoom);
    end; // for y
  finally
    sl.Free;
  end;
end;

procedure TBitmap32.DrawShrunkHatchedTo24;
var
  x, y, x2, y2, xStart, yStart, xEnd, yEnd, xShrink, yShrink, BlockWidth, BlockHeight: integer;
  p: PColor32;
  c: TColor;
  c32: TColor32;
  cPremul32: TPremulColor32;
  avg: TSmallColor32Average;
  sl: TScanLines;
  pDest: PByte;

label
  MoveColor;

begin
  Assert(Shrink <= 100); // For too large values, TSmallColor32Average is not acceptable

  if Shrink = 1 then
  begin
    MakeTransparentHatch(bm, Rect(xPos, yPos, xPos + Width, yPos + Height), xHatchOrigin, yHatchOrigin, Hatch);
    DrawTo24(bm, xPos, yPos);
    Exit;
  end;

  // calculate the rectangle of visible pixels
  // the first x value at which (xPos + x >= 0) is xStart
  xStart := Max(0, -xPos);
  // the last x value at which (xPos + x < sl.Width) is xEnd
  xEnd := Min(Width div Shrink, bm.Width - xPos) - 1;

  yStart := Max(0, -yPos);
  yEnd := Min(Height div Shrink, bm.Height - yPos) - 1;

  bm.PixelFormat := pf24bit;
  sl := TScanLines.Create(bm);
  try
    avg := TSmallColor32Average.Create;
    try
      yShrink := yStart*Shrink;
      for y := yStart to yEnd do
      begin
        BlockHeight := Min(Shrink, Height - yShrink);
        pDest := @sl[yPos + y][(xPos + xStart) * sl.BytesPerPixel];
        xShrink := xStart*Shrink;
        for x := xStart to xEnd do
        begin
          BlockWidth := Min(Shrink, Width - xShrink);
          // calc. background color
          c := Hatch.Color[(((yPos+y-yHatchOrigin) xor (xPos+x-xHatchOrigin)) shr 2) and 1];

          if Draft then
          begin
            c32 := PixelAddr(xShrink + x and (BlockWidth-1), yShrink + y and (BlockHeight-1))^;
            c := Put32to24(c, c32);
          end else
          begin
            // calculate average of colors
            avg.Clear;
            p := PixelAddr(xShrink, yShrink);
            for y2 := 0 to BlockHeight - 1 do
            begin
              for x2 := 0 to BlockWidth - 1 do
              begin
                if p^ = cl32Inverted then
                begin
                  c := c xor clWhite;
                  goto MoveColor;
                end;

                avg.AddAlpha256(p^);

                // iterate
                Inc(p);
              end; // for x2
              Inc(p, Width-BlockWidth);
            end; // for y2
            avg.GetPremulAverage(cPremul32);
            c := PutPremul32to24(c, @cPremul32);
          end;

          c := FlipColor(c);

        MoveColor:
          Move3(@c, pDest);
          inc(pDest, sl.BytesPerPixel);
          inc(xShrink, Shrink);
        end; // for x
        inc(yShrink, Shrink);
      end; // for y
    finally
      avg.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure TBitmap32.DrawToCanvas;
var
  bm: TBitmap;

begin
  bm := TBitmap.Create;
  try
    bm.PixelFormat := pf24bit;
    bm.Width := Width;
    bm.Height := Height;

    MakeTransparentHatch(bm, ClientRect, 0, 0, Hatch);
    DrawTo24(bm, 0, 0);
    c.Draw(x, y, bm);
  finally
    bm.Free;
  end;
end;

end.

