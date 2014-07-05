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
    fPadX2: single;
    fPadX3: single;
    fPadY2: single;
    fPadY3: single;
    fPadX1: single;
    fPadY1: single;
    fPadX4: single;
    fPadY4: single;

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
    UniSmoothedPadX1 : single;
    UniSmoothedPadY1 : single;
    UniSmoothedPadX2 : single;
    UniSmoothedPadY2 : single;
    UniSmoothedPadX3 : single;
    UniSmoothedPadY3 : single;
    UniSmoothedPadX4 : single;
    UniSmoothedPadY4 : single;

    BiSmoothedPadX1 : single;
    BiSmoothedPadY1 : single;
    BiSmoothedPadX2 : single;
    BiSmoothedPadY2 : single;
    BiSmoothedPadX3 : single;
    BiSmoothedPadY3 : single;
    BiSmoothedPadX4 : single;
    BiSmoothedPadY4 : single;

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

  UniSmoothedPadX1 := 0;
  UniSmoothedPadY1 := 0;
  UniSmoothedPadX2 := 0;
  UniSmoothedPadY2 := 0;
  UniSmoothedPadX3 := 0;
  UniSmoothedPadY3 := 0;
  UniSmoothedPadX4 := 0;
  UniSmoothedPadY4 := 0;
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
  //TODO:MED currently the pad inputs are being smoothed all the time. :(

  //UniSmoothedPadX1 := 0;

  // NOTE: Each single assignment here is three MOV opcodes. One assignment alone causes
  // a CPU spike. I'm not sure how to avoid the assignment or CPU spike here.

  //UniSmoothedPadX1 := fPadX1;
  //UniSmoothedPadY1 := fPadY1;
  //UniSmoothedPadX2 := fPadX2;
  //UniSmoothedPadY2 := fPadY2;
  //UniSmoothedPadX3 := fPadX3;
  //UniSmoothedPadY3 := fPadY3;
  //UniSmoothedPadX4 := fPadX4;
  //UniSmoothedPadY4 := fPadY4;

  {
  UniSmoothedPadX1 := SmoothParameterChange(UniSmoothedPadX1, fPadX1, SmoothingCoefficient);
  UniSmoothedPadY1 := SmoothParameterChange(UniSmoothedPadY1, fPadY1, SmoothingCoefficient);
  UniSmoothedPadX2 := SmoothParameterChange(UniSmoothedPadX2, fPadX2, SmoothingCoefficient);
  UniSmoothedPadY2 := SmoothParameterChange(UniSmoothedPadY2, fPadY2, SmoothingCoefficient);
  UniSmoothedPadX3 := SmoothParameterChange(UniSmoothedPadX3, fPadX3, SmoothingCoefficient);
  UniSmoothedPadY3 := SmoothParameterChange(UniSmoothedPadY3, fPadY3, SmoothingCoefficient);
  UniSmoothedPadX4 := SmoothParameterChange(UniSmoothedPadX4, fPadX4, SmoothingCoefficient);
  UniSmoothedPadY4 := SmoothParameterChange(UniSmoothedPadY4, fPadY4, SmoothingCoefficient);

  BiSmoothedPadX1 := BiSmoothedPadX1 * 2 - 1;
  BiSmoothedPadY1 := BiSmoothedPadY1 * 2 - 1;
  BiSmoothedPadX2 := BiSmoothedPadX2 * 2 - 1;
  BiSmoothedPadY2 := BiSmoothedPadY2 * 2 - 1;
  BiSmoothedPadX3 := BiSmoothedPadX3 * 2 - 1;
  BiSmoothedPadY3 := BiSmoothedPadY3 * 2 - 1;
  BiSmoothedPadX4 := BiSmoothedPadX4 * 2 - 1;
  BiSmoothedPadY4 := BiSmoothedPadY4 * 2 - 1;
  }
  {
  GlobalModPoints^.Source_PadX1_Unipolar := fPadX1;
  GlobalModPoints^.Source_PadY1_Unipolar := fPadY1;
  GlobalModPoints^.Source_PadX2_Unipolar := fPadX2;
  GlobalModPoints^.Source_PadY2_Unipolar := fPadY2;
  GlobalModPoints^.Source_PadX3_Unipolar := fPadX3;
  GlobalModPoints^.Source_PadY3_Unipolar := fPadY3;
  GlobalModPoints^.Source_PadX4_Unipolar := fPadX4;
  GlobalModPoints^.Source_PadY4_Unipolar := fPadY4;
  }

  {
  GlobalModPoints^.Source_PadX1_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX1_Unipolar, fPadX1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY1_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY1_Unipolar, fPadY1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX2_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX2_Unipolar, fPadX2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY2_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY2_Unipolar, fPadY2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX3_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX3_Unipolar, fPadX3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY3_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY3_Unipolar, fPadY3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX4_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX4_Unipolar, fPadX4, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY4_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY4_Unipolar, fPadY4, SmoothingCoefficient);
  }
  {
  GlobalModPoints^.Source_PadX1_Bipolar := GlobalModPoints^.Source_PadX1_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY1_Bipolar := GlobalModPoints^.Source_PadY1_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX2_Bipolar := GlobalModPoints^.Source_PadX2_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY2_Bipolar := GlobalModPoints^.Source_PadY2_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX3_Bipolar := GlobalModPoints^.Source_PadX3_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY3_Bipolar := GlobalModPoints^.Source_PadY3_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX4_Bipolar := GlobalModPoints^.Source_PadX4_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY4_Bipolar := GlobalModPoints^.Source_PadY4_Unipolar * 2 - 1;
  }
end;



end.
