unit uLucidityXYPads;

interface

uses
  MoreTypes,
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
