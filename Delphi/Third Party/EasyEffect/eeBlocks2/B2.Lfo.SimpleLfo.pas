unit B2.Lfo.SimpleLfo;

interface

const
  MaxCardinal     : cardinal = 4294967295; //High(Cardinal)
  OneOverCardinal : double = 1 / 4294967295;
  UniOutputScaler : double = 1 / 4294967295; //One over High(Cardinal). Use to scale the phase position to 0..1, ie Uni-polar LFO output.
  BiOutputScaler  : double = 2 / 4294967295; //Two over High(Cardinal). Use to scale the phase position to -1..1, ie Bi-polar LFO output.

type
  TSimpleLfoShape = (Ramp, Saw, Triangle, Sine);

  TSimpleLfo = class
  private
    fShape: TSimpleLfoShape;
    fRate: single;
    function GetPhase: single;
    procedure SetPhase(const Value: single);
    procedure SetRate(const Value: single);
  protected
    PhaseCounter: cardinal;
    StepSize    : cardinal;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function StepUnipolar : single; inline;
    function StepBipolar  : single; inline;


    property Shape : TSimpleLfoShape read fShape   write fShape;
    property Phase : single          read GetPhase write SetPhase; //range 0..1
    property Rate  : single          read fRate    write SetRate;  //range 0..1. (Rate = Frequency / (SampleRate * 0.5))
    // NOTE: See B2.Lfo.Extra for functions to set the rate.
  end;

implementation

uses
  SysUtils;

{ TSimpleLfo }

constructor TSimpleLfo.Create;
begin
  PhaseCounter := 0;
  StepSize     := 0;
end;

destructor TSimpleLfo.Destroy;
begin

  inherited;
end;

function TSimpleLfo.GetPhase: single;
begin
  result := PhaseCounter * OneOverCardinal;
end;

procedure TSimpleLfo.SetPhase(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  PhaseCounter := round(High(Cardinal) * Value);
end;

procedure TSimpleLfo.SetRate(const Value: single);
begin
  //assert(Value >= 0);
  //assert(Value <= 1);
  fRate := Value;
  StepSize := round(MaxCardinal * Value);
end;

function TSimpleLfo.StepBipolar: single;
var
  PhaseAsFloat : single;
  tx           : single;
begin
  PhaseAsFloat := PhaseCounter * BiOutputScaler - 1;

  case Shape of
    Ramp:     tx := PhaseAsFloat;
    Saw:      tx := PhaseAsFloat * -1;
    Triangle: tx := abs(PhaseAsFloat) * 2 - 1;
    Sine:     tx := (PhaseAsFloat * (1 - abs(PhaseAsFloat))) * 4;  //Approximation of a sine. It has overtones.
  else
    raise Exception.Create('Shape type not handled.');
  end;

  assert(tx <= 1);
  assert(tx >= -1);


  result := tx;

  PhaseCounter := PhaseCounter + StepSize;
end;

function TSimpleLfo.StepUnipolar: single;
var
  PhaseAsFloat : single;
  tx           : single;
begin
  PhaseAsFloat := PhaseCounter * BiOutputScaler - 1;

  case Shape of
    Ramp:     tx := PhaseAsFloat;
    Saw:      tx := PhaseAsFloat * -1;
    Triangle: tx := abs(PhaseAsFloat) * 2 - 1;
    Sine:     tx := (PhaseAsFloat * (1 - abs(PhaseAsFloat))) * 4;  //Approximation of a sine. It has overtones.
  else
    raise Exception.Create('Shape type not handled.');
  end;

  assert(tx <= 1);
  assert(tx >= -1);

  result := tx * 0.5 + 0.5;

  PhaseCounter := PhaseCounter + StepSize;
end;

end.
