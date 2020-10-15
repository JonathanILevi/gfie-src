unit gfGauge;

{$MODE Delphi}

interface

uses
  {$IFNDEF LCL} Windows, Messages,
  {$ELSE} LclIntf, LclType, LResources, {$ENDIF}
  SysUtils, Classes, Controls, Graphics, Math;

type
  TgfGaugeKind = (gfgHorizontal, gfgVertical, gfgPie);
  TgfGaugeTextKind = (gfgtNone, gfgtPercent, gfgtProgress);
  
  TgfGauge = class(TCustomControl)
  private
    FForeColor: TColor;
    FMinValue: integer;
    FBackColor: TColor;
    FKind: TgfGaugeKind;
    FProgress: integer;
    FMaxValue: integer;
    FFrameColor: TColor;
    FTextKind: TgfGaugeTextKind;
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetKind(const Value: TgfGaugeKind);
    procedure SetMaxValue(const Value: integer);
    procedure SetMinValue(const Value: integer);
    procedure SetProgress(const Value: integer);
    procedure SetFrameColor(const Value: TColor);
    procedure SetTextKind(const Value: TgfGaugeTextKind);
    { Private declarations }
  protected
    procedure GetGaugeAnchors(out q: double; out x, y: integer);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Progress: integer read FProgress write SetProgress;

    property Kind: TgfGaugeKind read FKind write SetKind;
    property BackColor: TColor read FBackColor write SetBackColor;
    property ForeColor: TColor read FForeColor write SetForeColor;
    property FrameColor: TColor read FFrameColor write SetFrameColor;
    property TextKind: TgfGaugeTextKind read FTextKind write SetTextKind default gfgtNone;

    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
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

procedure Register;
begin
  RegisterComponents('Greenfish', [TgfGauge]);
end;

{ TgfGauge }

constructor TgfGauge.Create(AOwner: TComponent);
begin
  inherited;

  DoubleBuffered := True;
  
  FMinValue := 0;
  FMaxValue := 100;
  FProgress := 0;
  FKind := gfgHorizontal;
  FBackColor := clWhite;
  FForeColor := clNavy;
  FFrameColor := clBlack;
  FTextKind := gfgtNone;
end;

procedure TgfGauge.GetGaugeAnchors;
var
  p: integer;

begin
  p := Max(MinValue, Min(MaxValue, Progress));
  if MinValue <> MaxValue then q := (p - MinValue) / (MaxValue - MinValue) else
    q := 0;

  case Kind of
    gfgHorizontal: x := Round(q * Width);
    gfgVertical: y := Round(q * Height);
    gfgPie: begin
      x := Round(Width*0.5*Sin(q*2*pi));
      y := Round(Height*0.5*Cos(q*2*pi));
    end;
    else begin
      x := 0;
      y := 0;
    end; // to suppress warnings
  end;
end;

procedure TgfGauge.Paint;
var
  x, y, w, h: integer;
  q: double;
  s: string;
  r: TRect;
  bm: TBitmap;

begin
  inherited;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  GetGaugeAnchors(q, x, y);
  case Kind of
    gfgHorizontal, gfgVertical: begin
      Canvas.Brush.Color := BackColor;
      Canvas.FillRect(ClientRect);

      if MinValue <> MaxValue then
      begin
        r := ClientRect;
        if Kind = gfgHorizontal then r.Right := x else r.Bottom := y;
        Canvas.Brush.Color := ForeColor;
        Canvas.FillRect(r);
      end;

      Canvas.Brush.Color := FrameColor;
      Canvas.FrameRect(ClientRect);
    end;

    gfgPie: begin
      Canvas.Pen.Color := FrameColor;
      Canvas.Brush.Color := BackColor;
      Canvas.Ellipse(0, 0, Width, Height);

      Canvas.Brush.Color := ForeColor;

      if (x <> 0) or (2*Progress > MinValue + MaxValue) then
        Canvas.Pie(0, 0, Width, Height,
          Width div 2 + x, Height div 2 - y, Width div 2, 0);
    end;
  end; // case Kind

  // text
  if TextKind <> gfgtNone then
  begin
    bm := TBitmap.Create;
    try
      bm.Width := Width;
      bm.Height := Height;
      bm.Canvas.Brush.Color := clBlack;
      bm.Canvas.FillRect(ClientRect);

      bm.Canvas.Font.Assign(Font);
      bm.Canvas.Font.Color := clWhite;
      bm.Canvas.Brush.Style := bsClear;

      case TextKind of
        gfgtPercent: s := IntToStr(Round(q * 100)) + '%';
        gfgtProgress: s := IntToStr(Progress);
      end;

      w := bm.Canvas.TextWidth(s); h := bm.Canvas.TextHeight(s);
      bm.Canvas.TextOut((Width - w) div 2, (Height - h) div 2, s);

      Canvas.CopyMode := cmSrcInvert;
      Canvas.Draw(0, 0, bm);
      Canvas.CopyMode := cmSrcCopy;
    finally
      bm.Free;
    end;
  end;
end;

procedure TgfGauge.SetBackColor(const Value: TColor);
begin
  if FBackColor <> Value then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetForeColor(const Value: TColor);
begin
  if FForeColor <> Value then
  begin
    FForeColor := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetFrameColor(const Value: TColor);
begin
  if FFrameColor <> Value then
  begin
    FFrameColor := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetKind(const Value: TgfGaugeKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetMaxValue(const Value: integer);
begin
  if FMaxValue <> Value then
  begin
    FMaxValue := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetMinValue(const Value: integer);
begin
  if FMinValue <> Value then
  begin
    FMinValue := Value;
    Invalidate;
  end;
end;

procedure TgfGauge.SetProgress(const Value: integer);
var
  x, y, x2, y2: integer;
  q: double;

begin
  if FProgress <> Value then
  begin
    GetGaugeAnchors(q, x, y);
    FProgress := Value;
    GetGaugeAnchors(q, x2, y2);

    if (x <> x2) or (y <> y2) then Invalidate;
  end;
end;

procedure TgfGauge.SetTextKind(const Value: TgfGaugeTextKind);
begin
  if FTextKind <> Value then
  begin
    FTextKind := Value;
    Invalidate;
  end;
end;

end.
