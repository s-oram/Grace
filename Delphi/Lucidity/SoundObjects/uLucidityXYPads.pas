unit uLucidityXYPads;

interface

uses
  VamLib.MoreTypes,
  eeGlobals,
  uConstants;

type
  TLucidityXYPads = class
  private
    fPadX2: single;
    fPadX3: single;
    fPadY2: single;
    fPadY3: single;
    fPadX1: single;
    fPadY1: single;
    fPadX4: single;
    fPadY4: single;
    procedure SetPadX1(Value: single);
    procedure SetPadX2(Value: single);
    procedure SetPadX3(Value: single);
    procedure SetPadX4(Value: single);
    procedure SetPadY1(Value: single);
    procedure SetPadY2(Value: single);
    procedure SetPadY3(Value: single);
    procedure SetPadY4(Value: single);
  protected
    GlobalModPoints : PGlobalModulationPoints;
    Globals         : TGlobals;
    ParSmoothingFactor : double;
    SmoothingCoefficient : single;
    procedure SampleRateChanged(Sender : TObject);
  public
    constructor Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals : TGlobals);
    destructor Destroy; override;

    procedure ControlRateProcess;

    property PadX1 : single read fPadX1 write SetPadX1;
    property PadY1 : single read fPadY1 write SetPadY1;
    property PadX2 : single read fPadX2 write SetPadX2;
    property PadY2 : single read fPadY2 write SetPadY2;
    property PadX3 : single read fPadX3 write SetPadX3;
    property PadY3 : single read fPadY3 write SetPadY3;
    property PadX4 : single read fPadX4 write SetPadX4;
    property PadY4 : single read fPadY4 write SetPadY4;
  end;

implementation

uses
  eeCustomGlobals,
  eeParSmoothingUtils;

{ TLucidityXYPads }

constructor TLucidityXYPads.Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals : TGlobals);
begin
  GlobalModPoints := aGlobalModPoints;
  Globals := aGlobals;
  Globals.AddEventListener(TPluginEvent.SampleRateChanged, SampleRateChanged);

  fPadX2 := 0;
  fPadX3 := 0;
  fPadY2 := 0;
  fPadY3 := 0;
  fPadX1 := 0;
  fPadY1 := 0;
  fPadX4 := 0;
  fPadY4 := 0;
end;

destructor TLucidityXYPads.Destroy;
begin

  inherited;
end;

procedure TLucidityXYPads.SampleRateChanged(Sender: TObject);
const
  kSmoothingTime = 100;
begin
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689&highlight=filter+envelope
  //SmoothingCoefficient := CalculateSmoothingCoefficient(100, Globals.ControlRate);
  SmoothingCoefficient := CalculateSmoothingCoefficient(1, Globals.ControlRate);
end;

procedure TLucidityXYPads.SetPadX1(Value: single);
begin
  // TODO:MED: all these set Pad X/Y methonds are clamping the value.
  // this shouldn't be needed. Change to asserts, fix the crashes.
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadX1 := Value;
end;

procedure TLucidityXYPads.SetPadX2(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadX2 := Value;
end;

procedure TLucidityXYPads.SetPadX3(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadX3 := Value;
end;

procedure TLucidityXYPads.SetPadX4(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadX4 := Value;
end;

procedure TLucidityXYPads.SetPadY1(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadY1 := Value;
end;

procedure TLucidityXYPads.SetPadY2(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadY2 := Value;
end;

procedure TLucidityXYPads.SetPadY3(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadY3 := Value;
end;

procedure TLucidityXYPads.SetPadY4(Value: single);
begin
  if Value < 0 then Value := 0;
  if Value > 1 then Value := 1;
  fPadY4 := Value;
end;

procedure TLucidityXYPads.ControlRateProcess;
begin
  GlobalModPoints^.Source_PadX1 := SmoothParameterChange(GlobalModPoints^.Source_PadX1, fPadX1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY1 := SmoothParameterChange(GlobalModPoints^.Source_PadY1, fPadY1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX2 := SmoothParameterChange(GlobalModPoints^.Source_PadX2, fPadX2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY2 := SmoothParameterChange(GlobalModPoints^.Source_PadY2, fPadY2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX3 := SmoothParameterChange(GlobalModPoints^.Source_PadX3, fPadX3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY3 := SmoothParameterChange(GlobalModPoints^.Source_PadY3, fPadY3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX4 := SmoothParameterChange(GlobalModPoints^.Source_PadX4, fPadX4, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY4 := SmoothParameterChange(GlobalModPoints^.Source_PadY4, fPadY4, SmoothingCoefficient);
end;



end.
