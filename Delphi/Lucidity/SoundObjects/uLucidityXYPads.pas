unit uLucidityXYPads;

interface

uses
  VamLib.MoreTypes,
  eeParSmoothingUtils,
  eeDsp,
  eeGlobals,
  uConstants;

type
  TLucidityXYPads = class
  private
    procedure SetPadX1(const Value: single);
    procedure SetPadX2(const Value: single);
    procedure SetPadX3(const Value: single);
    procedure SetPadX4(const Value: single);
    procedure SetPadY1(const Value: single);
    procedure SetPadY2(const Value: single);
    procedure SetPadY3(const Value: single);
    procedure SetPadY4(const Value: single);
  protected
    GlobalModPoints : PGlobalModulationPoints;
    Globals         : TGlobals;
    ParSmoothingFactor : double;
    SmoothingCoefficient : single;
    procedure SampleRateChanged(Sender : TObject);
  public
    fPadX2: single;
    fPadX3: single;
    fPadY2: single;
    fPadY3: single;
    fPadX1: single;
    fPadY1: single;
    fPadX4: single;
    fPadY4: single;

    constructor Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals : TGlobals);
    destructor Destroy; override;

    procedure ControlRateProcess; inline;

    //PadX/Y values are limited to 0..1 range.
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
  eeCustomGlobals;


{ TLucidityXYPads }

constructor TLucidityXYPads.Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals : TGlobals);
begin
  GlobalModPoints := aGlobalModPoints;
  Globals := aGlobals;
  Globals.AddEventListener(TPluginEvent.SampleRateChanged, SampleRateChanged);

  fPadX1 := 0;
  fPadY1 := 0;
  fPadX2 := 0;
  fPadY2 := 0;
  fPadX3 := 0;
  fPadY3 := 0;
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

procedure TLucidityXYPads.SetPadX1(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadX1 := Value;
  //UniSmoothedPadX1 := Value;
  //BiSmoothedPadX1 := Value * 2 - 1;
end;

procedure TLucidityXYPads.SetPadX2(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadX2 := Value;
end;

procedure TLucidityXYPads.SetPadX3(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadX3 := Value;
end;

procedure TLucidityXYPads.SetPadX4(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadX4 := Value;
end;

procedure TLucidityXYPads.SetPadY1(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadY1 := Value;
end;

procedure TLucidityXYPads.SetPadY2(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadY2 := Value;
end;

procedure TLucidityXYPads.SetPadY3(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadY3 := Value;
end;

procedure TLucidityXYPads.SetPadY4(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fPadY4 := Value;
end;

procedure TLucidityXYPads.ControlRateProcess;
begin

end;



end.
