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

    //PadX/Y values are limited to 0..1 range.
    property PadX1 : single read fPadX1 write fPadX1;
    property PadY1 : single read fPadY1 write fPadY1;
    property PadX2 : single read fPadX2 write fPadX2;
    property PadY2 : single read fPadY2 write fPadY2;
    property PadX3 : single read fPadX3 write fPadX3;
    property PadY3 : single read fPadY3 write fPadY3;
    property PadX4 : single read fPadX4 write fPadX4;
    property PadY4 : single read fPadY4 write fPadY4;
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
  //TODO:MED currently the pad inputs are being smoothed all the time. :(

  GlobalModPoints^.Source_PadX1_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX1_Unipolar, fPadX1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY1_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY1_Unipolar, fPadY1, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX2_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX2_Unipolar, fPadX2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY2_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY2_Unipolar, fPadY2, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX3_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX3_Unipolar, fPadX3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY3_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY3_Unipolar, fPadY3, SmoothingCoefficient);
  GlobalModPoints^.Source_PadX4_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadX4_Unipolar, fPadX4, SmoothingCoefficient);
  GlobalModPoints^.Source_PadY4_Unipolar := SmoothParameterChange(GlobalModPoints^.Source_PadY4_Unipolar, fPadY4, SmoothingCoefficient);


  GlobalModPoints^.Source_PadX1_Bipolar := GlobalModPoints^.Source_PadX1_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY1_Bipolar := GlobalModPoints^.Source_PadY1_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX2_Bipolar := GlobalModPoints^.Source_PadX2_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY2_Bipolar := GlobalModPoints^.Source_PadY2_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX3_Bipolar := GlobalModPoints^.Source_PadX3_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY3_Bipolar := GlobalModPoints^.Source_PadY3_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadX4_Bipolar := GlobalModPoints^.Source_PadX4_Unipolar * 2 - 1;
  GlobalModPoints^.Source_PadY4_Bipolar := GlobalModPoints^.Source_PadY4_Unipolar * 2 - 1;
end;



end.
