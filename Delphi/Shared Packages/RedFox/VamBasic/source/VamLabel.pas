unit VamLabel;

interface

uses
  Agg2D,
  Classes, RedFox, RedFoxWinControl, VamWinControl;

type
  TVamLabel = class(TVamWinControl)
  private
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fAutoSize: boolean;
    fAutoTrimText: boolean;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    procedure SetAutoTrimText(const Value: boolean);

  protected
    DisplayText : string;

    procedure Paint; override;

    procedure SetAutoSize(Value: boolean); override;

    procedure CalculateDisplayText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PerformAutoSize(const s : string);
  published
    property AutoTrimText : boolean read fAutoTrimText write SetAutoTrimText;
    property AutoSize     : boolean read fAutoSize     write SetAutoSize;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property Font;

    {$INCLUDE TControlProperties.inc}
  end;


implementation

uses
  Types, AggPixelFormat;



{ TRedFoxLabel }

constructor TVamLabel.Create(AOwner: TComponent);
begin
  inherited;

  fText := '';
  DisplayText := '';

  fTextAlign := TRedFoxAlign.AlignCenter;
  fTextVAlign := TRedFoxAlign.AlignCenter;
end;

destructor TVamLabel.Destroy;
begin

  inherited;
end;

procedure TVamLabel.CalculateDisplayText;
begin
  if (AutoTrimText) and (not AutoSize) then
  begin
    BackBuffer.UpdateFont(self.Font);
    DisplayText := BackBuffer.AutoTrimTextToFitBufferWidth(fText);
  end else
  begin
    DisplayText := fText;
  end;
end;

procedure TVamLabel.SetAutoSize(Value: boolean);
begin
  fAutoSize := Value;

  CalculateDisplayText;
  if (Value) then
  begin
    PerformAutoSize(DisplayText);
  end;
end;

procedure TVamLabel.SetAutoTrimText(const Value: boolean);
begin
  if Value <> fAutoTrimText then
  begin
    fAutoTrimText := Value;
    CalculateDisplayText;
    Invalidate;
  end;
end;

procedure TVamLabel.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    CalculateDisplayText;
    if AutoSize then PerformAutoSize(DisplayText);
    Invalidate;
  end;
end;

procedure TVamLabel.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamLabel.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamLabel.PerformAutoSize(const s : string);
var
  tw : integer;
begin
  if Assigned(self.BackBuffer) then
  begin
    BackBuffer.UpdateFont(Font);
    tw := round(BackBuffer.TextWidth(s));

    if Self.Width <> tw then
    begin
      self.Width := tw;
    end;
  end;

end;


procedure TVamLabel.Paint;
var
  TextBounds : TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  TextBounds := Rect(0,0, Width, Height);
  BackBuffer.DrawText(DisplayText, Font, TextAlign, TextVAlign, TextBounds);
end;


end.
