unit uKeyStateTrackerOverlay;

interface

uses
  VamKeyStateTracker,
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

const
  kMinimumKeys = 24;
  kMaximumKeys = 128;

type

  {
  TKeyTrackerOverlay is designed to be overlaid over a TVamSampleMap GUI
  component. The overlay shows what MIDI keys have been pressed.
  }

  TKeyStateTrackerOverlay = class(TVamWinControl)
  private
    fOffset: single;
    fZoom: single;
    procedure SetOffset(const Value: single);
    procedure SetZoom(const Value: single);
  protected
    fKeyStateData : TKeyStateData;

    NumberOfKeysToShow : integer;
    KeyBedPixelWidth   : integer;
    KeyBedPixelOffset  : integer;
    procedure ZoomOffsetChanged;

    procedure CalcKeyStateIndicatorPos(const MidiNote, MidiVelocity:byte; out x1, y1, x2, y2 : single);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetKeyStateData : PKeyStateData;

    procedure Paint; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Zoom   : single read fZoom   write SetZoom;   //Range 0..1. 0=Show all keys, 1=Show minimum keys.
    property Offset : single read fOffset write SetOffset; //Range 0..1  0=Show lowest keys, 1=show highest keys.
  end;

implementation

uses
  Math,
  AggPixelFormat;

{ TKeyTrackerOverlay }

constructor TKeyStateTrackerOverlay.Create(AOwner: TComponent);
begin
  inherited;

  fZoom := 0;
  fOffset := 0;
  ZoomOffsetChanged;

end;

destructor TKeyStateTrackerOverlay.Destroy;
begin

  inherited;
end;



procedure TKeyStateTrackerOverlay.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  ZoomOffsetChanged;
end;

procedure TKeyStateTrackerOverlay.SetOffset(const Value: single);
begin
  if Value <> fOffset then
  begin
    fOffset := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TKeyStateTrackerOverlay.SetZoom(const Value: single);
begin
  if Value <> fZoom then
  begin
    fZoom := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TKeyStateTrackerOverlay.ZoomOffsetChanged;
var
  KeyPixelWidth : integer;
begin
  NumberOfKeysToShow := round((kMaximumKeys * (1 - fZoom)) + (kMinimumKeys * fZoom));

  if fZoom = 0 then
  begin
    KeyBedPixelWidth  := self.Width;
    KeyBedPixelOffset := 0;
  end else
  begin
    KeyPixelWidth     := ceil(self.Width / NumberOfKeysToShow);
    KeyBedPixelWidth  := KeyPixelWidth * kMaximumKeys;
    KeyBedPixelOffset := round((KeyBedPixelWidth - self.Width) * Offset);
  end;

  Invalidate;
end;

procedure TKeyStateTrackerOverlay.CalcKeyStateIndicatorPos(const MidiNote, MidiVelocity: byte; out x1, y1, x2, y2: single);
begin
  x1 := (MidiNote       / 128) * KeyBedPixelWidth - KeyBedPixelOffset;
  x2 := ((MidiNote + 1) / 128) * KeyBedPixelWidth - KeyBedPixelOffset;

  y1 := (1 - ((MidiVelocity + 1) / 128)) * (Height);
  y1 := round(y1) + 0.5;
  y2 := y1;
end;

function TKeyStateTrackerOverlay.GetKeyStateData: PKeyStateData;
begin
  result := @fKeyStateData;
end;

procedure TKeyStateTrackerOverlay.Paint;
var
  x1, y1, x2, y2 : single;
  c1: Integer;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  //BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmAlpha;

  BackBuffer.BufferInterface.LineWidth := 4;

  BackBuffer.BufferInterface.LineColor := GetRedFoxColor('$FFFF3333').AsAggRgba8;
  BackBuffer.BufferInterface.NoFill;

  //BackBuffer.BufferInterface.Rectangle(0,0, Width, Height);

  for c1 := 0 to Length(fKeyStateData) - 1 do
  begin
    CalcKeyStateIndicatorPos(fKeyStateData[c1].Note, fKeyStateData[c1].Velocity, x1, y1, x2, y2);
    BackBuffer.BufferInterface.Line(x1, y1, x2, y2);
  end;

  {
  CalcKeyStateIndicatorPos(60, 60, x1, y1, x2, y2);
  BackBuffer.BufferInterface.Line(x1, y1, x2, y2);


  BackBuffer.BufferInterface.LineColor := GetRedFoxColor('$FFFFFFFF').AsAggRgba8;
  BackBuffer.BufferInterface.FillColor := GetRedFoxColor('$FFFFFFFF').AsAggRgba8;

  x1 := random(Width - 20);
  y1 := random(Height - 20);
  x2 := x1 + 20;
  y2 := y1 + 20;

  BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);
  }

end;



end.
