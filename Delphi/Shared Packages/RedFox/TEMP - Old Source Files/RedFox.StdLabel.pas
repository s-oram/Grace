unit RedFox.StdLabel;

interface

uses
  RedFox,
  Classes, RedFoxCustomControl;

type
  TRedFoxLabel = class(TRedFoxCustomControl)
  private
    fText: string;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
  protected
    procedure DoPaintBuffer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property Font;
  end;


implementation

uses
  Types;



{ TRedFoxLabel }

constructor TRedFoxLabel.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TRedFoxLabel.Destroy;
begin

  inherited;
end;

procedure TRedFoxLabel.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TRedFoxLabel.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TRedFoxLabel.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TRedFoxLabel.DoPaintBuffer;
var
  TextBounds : TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  TextBounds := Rect(0,0, Width, Height);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);

end;



end.
