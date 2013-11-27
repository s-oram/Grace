unit VamLabel;

interface

uses
  Classes, RedFox, RedFoxWinControl, VamWinControl;

type
  TVamLabel = class(TVamWinControl)
  private
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fAutoSize: boolean;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    procedure SetAutoSize(const Value: boolean);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PerformAutoSize;
  published
    property AutoSize : boolean read fAutoSize write SetAutoSize;
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

  fTextAlign := TRedFoxAlign.AlignCenter;
  fTextVAlign := TRedFoxAlign.AlignCenter;
end;

destructor TVamLabel.Destroy;
begin

  inherited;
end;

procedure TVamLabel.SetAutoSize(const Value: boolean);
begin

  fAutoSize := Value;

  if Value then
  begin
    PerformAutoSize;
  end;
end;

procedure TVamLabel.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    if AutoSize then PerformAutoSize;
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

procedure TVamLabel.Paint;
var
  TextBounds : TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  TextBounds := Rect(0,0, Width, Height);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);
end;

procedure TVamLabel.PerformAutoSize;
var
  tw : integer;
begin
  if Assigned(self.BackBuffer) then
  begin
    BackBuffer.UpdateFont(Font);
    tw := round(BackBuffer.TextWidth(Text));

    if Self.Width <> tw then
    begin
      self.Width := tw;
    end;
  end;

end;

end.
