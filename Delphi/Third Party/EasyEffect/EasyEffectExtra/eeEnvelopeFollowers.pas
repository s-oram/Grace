unit eeEnvelopeFollowers;

interface

uses
  VamLib.MoreTypes;



  // The Peak Followers are simple classes that calculate the level of a signal (stereo or mono).
  // The output of the follower will need to be fed into an Envelope Generator (EG) to create
  // a control signal for compressors etc, or anything else that uses 'Envelope Followers'
  //
  // Splitting the 'Envelope Follower' into two classes, (the follower and the envelope generator)
  // should hopefully make the design of them more apparent and allow for easier reuse of elements
  // and experimentation with structors.
  //



type
  TPeakFollowerStereo = class
  private
    function GetLevelAsDB: single;
  protected
    fLevel:single;
  public
    constructor Create;

    procedure Step(const In1, In2:Single); inline;

    property Level : single read fLevel;
    property LevelAsDB : single read GetLevelAsDB;
  end;


  // TRmsFollowerStereo
  // The RMS Follower uses a algorithim described on KVR Audio.
  // The algorithm is described as using an exponential moving average.
  //   http://www.kvraudio.com/forum/viewtopic.php?p=4183665&highlight=calc+rms#4183665
  TRmsFollowerStereo = class
  private
    fIntergrationTime: single;
    fSampleRate: single;
    function GetLevelAsDB: single;
    procedure SetIntergrationTime(const Value: single);
    procedure SetSampleRate(const Value: single);
  protected
    SquareSum1, SquareSum2:double;
    Rms1, Rms2:Double;
    Coeff:double;
    fLevel:single;
    procedure UpdateCoeff;
  public
    constructor Create;

    procedure Step(const In1, In2:Single); inline;

    property Level : single read fLevel;
    property LevelAsDB : single read GetLevelAsDB;

    property IntergrationTime:single read fIntergrationTime write SetIntergrationTime; //Milliseconds.
    property SampleRate : single read fSampleRate write SetSampleRate;
  end;



  TEnvGenAR = class
  private
    fSamplerate: single;
    fAttackTime: single;
    fHoldTime: single;
    fReleaseTime: single;
    procedure SetSampleRate(const Value: single);
    procedure SetAttackTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
  protected
    Level        : double;
    AttackCoeff  : double;
    ReleaseCoeff : double;
  public
    constructor Create;
	  destructor Destroy; override;

    function Step(In1:single):single; {inline;}

    property SampleRate : single read fSamplerate write SetSampleRate;

    property AttackTime  : single read fAttackTime  write SetAttackTime; //Milliseconds
    property ReleaseTime : single read fReleaseTime write SetReleaseTime; //milliseconds;
  end;


  //NOTE: The AHRFollower is untested.
  // - It is designed to be a generic follower with Attack, Hold and Release times...
  TAHRFollower = class
  private
    fSamplerate: single;
    fAttackTime: single;
    fHoldTime: single;
    fReleaseTime: single;
    procedure SetAttackTime(const Value: single);
    procedure SetHoldTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
    procedure SetSampleRate(const Value: single);
  protected
    AttackCoefficient  : double;
    ReleaseCoefficient : double;
    HoldSamples        : cardinal;
    HoldCount          : cardinal;
    LevelA             : double;
    LevelB             : double;
  public
    constructor Create;
	  destructor Destroy; override;

    function Step(In1:single):single; {inline;}
    function StepInverted(In1:single):single; {inline;}

    property SampleRate : single read fSamplerate write SetSampleRate;

    property AttackTime  : single read fAttackTime  write SetAttackTime; //Milliseconds
    property HoldTime    : single read fHoldTime    write SetHoldTime;
    property ReleaseTime : single read fReleaseTime write SetReleaseTime; //milliseconds;

    property Level       : double read LevelB;
  end;



implementation

uses
  eeDsp;

{ TPeakFollowerStereo }

procedure TPeakFollowerStereo.Step(const In1, In2: Single);
const
  GainReduction = 0.59566214352901; // -4.5 dB gain reduction.
begin
  fLevel := (abs(In1) + abs(In2)) * GainReduction;
end;

constructor TPeakFollowerStereo.Create;
begin
  fLevel := 0;
end;

function TPeakFollowerStereo.GetLevelAsDB: single;
var
  x:single;
begin
  //TODO: Insert fast LinearToDB conversion here.
  x := LinearToDecibels(fLevel);
  if x < -120
    then result := -120
    else result := x;
end;



{ TRmsFollowerStereo }

constructor TRmsFollowerStereo.Create;
begin
  fSampleRate       := 44100;
  fIntergrationTime := 10;

  UpdateCoeff;

  SquareSum1 := 0;
  SquareSum2 := 0;
  Rms1       := 0;
  Rms2       := 0;
end;



procedure TRmsFollowerStereo.SetIntergrationTime(const Value: single);
begin
  fIntergrationTime := Value;
  UpdateCoeff;
end;

procedure TRmsFollowerStereo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  UpdateCoeff;
end;


procedure TRmsFollowerStereo.UpdateCoeff;
begin
  Coeff := 1 - exp(-1 / (IntergrationTime * 0.001 * SampleRate))
end;

procedure TRmsFollowerStereo.Step(const In1, In2: Single);
const
  GainReduction = 0.59566214352901; // -4.5 dB gain reduction.
begin
  SquareSum1 := SquareSum1 * (1 - coeff) + coeff * (In1 * In1);
  SquareSum2 := SquareSum2 * (1 - coeff) + coeff * (In2 * In2);

  //TODO: Insert fast sqrt() function.
  Rms1 := sqrt(SquareSum1);
  Rms2 := sqrt(SquareSum2);

  fLevel := (Rms1 + Rms2) * GainReduction;
end;


function TRmsFollowerStereo.GetLevelAsDB: single;
var
  x:single;
begin
  //TODO: Insert fast LinearToDB conversion here.
  x := LinearToDecibels(fLevel);
  if x < -120
    then result := -120
    else result := x;
end;



{ TEnvGenAHR }

constructor TEnvGenAR.Create;
begin
  Level := 0;
  fSampleRate := 44100;
end;

destructor TEnvGenAR.Destroy;
begin

  inherited;
end;

procedure TEnvGenAR.SetAttackTime(const Value: single);
begin
  fAttackTime := Value;
  AttackCoeff := CalcRcEnvelopeCoefficient(Value, SampleRate);
end;

procedure TEnvGenAR.SetReleaseTime(const Value: single);
begin
  fReleaseTime := Value;
  ReleaseCoeff := CalcRcEnvelopeCoefficient(Value, SampleRate);
end;

procedure TEnvGenAR.SetSampleRate(const Value: single);
begin
  fSamplerate := Value;

  AttackCoeff  := CalcRcEnvelopeCoefficient(fAttackTime,  fSampleRate);
  ReleaseCoeff := CalcRcEnvelopeCoefficient(fReleaseTime, fSampleRate);
end;

function TEnvGenAR.Step(In1: single): single;
begin
  if In1 > Level
    then Level := In1 + AttackCoeff  * (Level - In1)
    else Level := In1 + ReleaseCoeff * (Level - In1);

  result := Level;
end;



{ TAHRFollower }

constructor TAHRFollower.Create;
begin
  fSampleRate := 44100;
  HoldCount := 0;
  LevelA := 0;
  LevelB := 0;
end;

destructor TAHRFollower.Destroy;
begin

  inherited;
end;

procedure TAHRFollower.SetAttackTime(const Value: single);
begin
  fAttackTime        := Value;
  AttackCoefficient  := CalcRcEnvelopeCoefficient(fAttackTime, fSampleRate);
end;

procedure TAHRFollower.SetHoldTime(const Value: single);
begin
  fHoldTime   := Value;
  HoldSamples := round(MillisecondsToSamples(fHoldTime, fSampleRate));
end;

procedure TAHRFollower.SetReleaseTime(const Value: single);
begin
  fReleaseTime       := Value;
  ReleaseCoefficient := CalcRcEnvelopeCoefficient(fReleaseTime, fSampleRate);
end;

procedure TAHRFollower.SetSampleRate(const Value: single);
begin
  fSamplerate := Value;

  AttackCoefficient  := CalcRcEnvelopeCoefficient(fAttackTime, fSampleRate);
  ReleaseCoefficient := CalcRcEnvelopeCoefficient(fReleaseTime, fSampleRate);
  HoldSamples        := round(MillisecondsToSamples(fHoldTime, fSampleRate));
end;

function TAHRFollower.Step(In1: single): single;
begin
  if In1 > LevelA then
  begin
    LevelA := In1;
    HoldCount := 0;
  end else
  begin
    if HoldCount < HoldSamples
      then inc(HoldCount)
      else LevelA := In1 + ReleaseCoefficient * (LevelA-In1);
  end;

  if LevelB < LevelA
    then LevelB := LevelA + AttackCoefficient * (LevelB-LevelA)
    else LevelB := LevelA;

  result := LevelB;
end;


function TAHRFollower.StepInverted(In1: single): single;
begin
  if In1 < LevelA then
  begin
    LevelA := In1;
    HoldCount := 0;
  end else
  begin
    if HoldCount < HoldSamples
      then inc(HoldCount)
      else LevelA := In1 + ReleaseCoefficient * (LevelA-In1);
  end;

  if LevelB > LevelA
    then LevelB := LevelA + AttackCoefficient * (LevelB-LevelA)
    else LevelB := LevelA;

  result := LevelB;
end;

end.
