unit eeFilters.EnvFollowerA;

interface

uses
  VamLib.MoreTypes;

type
  TEnvFollowerA = class
  private
    fSampleRate : integer;
    fAttackTime : single;
    fReleaseTime  : single;
    procedure SetAttackTime(const Value: single);
    procedure SetReleaseTime(const Value: single);
    procedure SetSampleRate(const Value: integer);
  protected
    DecayEnvLevel          : single;
    EnvLevel               : single;
    QuickAttackCoefficient : single;
    AttackCoefficient      : single;
    ReleaseCoefficient     : single;
  public
    constructor Create;
	  destructor Destroy; override;

    function Step(const In1: single) : single; inline;
    procedure Process(In1:PSingle; const SampleFrames : integer);  overload; inline;
    procedure Process(In1, Out1:PSingle; const SampleFrames : integer);  overload; inline;

    property AttackTime  : single  read fAttackTime  write SetAttackTime;   //milliseconds.
    property ReleaseTime : single  read fReleaseTime write SetReleaseTime;  //milliseconds.
    property SampleRate  : integer read fSampleRate  write SetSampleRate;
  end;

implementation

{ TEnvelopeFollower }

constructor TEnvFollowerA.Create;
begin
  EnvLevel       := 0;
  SampleRate     := 44100;

  fAttackTime := 0;
  fReleaseTime  := 0;

  AttackTime := 0;
  ReleaseTime  := 0.1;

  DecayEnvLevel := 0;
end;

destructor TEnvFollowerA.Destroy;
begin

  inherited;
end;

procedure TEnvFollowerA.SetAttackTime(const Value: single);
var
  tau:double;
begin
  if Value > 0 then
  begin
    fAttackTime := Value;
    tau  := Value / 1000;
    AttackCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
    if AttackCoefficient > 1 then AttackCoefficient := 1;
  end else
  begin
    AttackCoefficient := 1;
  end;

end;

procedure TEnvFollowerA.SetReleaseTime(const Value: single);
var
  tau:double;
begin
  if Value > 0 then
  begin
    fReleaseTime := Value;
    tau  := Value / 1000;
    ReleaseCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
    if ReleaseCoefficient > 1 then ReleaseCoefficient := 1;
  end else
  begin

  end;
end;

procedure TEnvFollowerA.SetSampleRate(const Value: integer);
var
  Time:single;
  tau:double;
begin
  fSampleRate := Value;

  Time := 0.001;
  tau  := Time / 1000;
  QuickAttackCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
end;

function TEnvFollowerA.Step(const In1: single): single;
var
  x1:single;
begin
  x1 := abs(In1);

  //The envelope uses a two stage detection process.
  //The first stage is an envelope follower with a quick attack and the correct decay time.
  //The second stage uses the proper attack coefficient and rises until it meets the first envelope.

  //First stage...
  if x1 > DecayEnvLevel
    then DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * QuickAttackCoefficient
    else DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * ReleaseCoefficient;

  //Second stage...
  if (DecayEnvLevel > EnvLevel)
    then EnvLevel := EnvLevel + (DecayEnvLevel - EnvLevel) * AttackCoefficient
    else EnvLevel := DecayEnvLevel;

  result := EnvLevel;
end;

procedure TEnvFollowerA.Process(In1: PSingle; const SampleFrames: integer);
var
  x1:single;
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    x1 := abs(In1^);

    //The envelope uses a two stage detection process.
    //The first stage is an envelope follower with a quick attack and the correct decay time.
    //The second stage uses the proper attack coefficient and rises until it meets the first envelope.

    //First stage...
    if x1 > DecayEnvLevel
      then DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * QuickAttackCoefficient
      else DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * ReleaseCoefficient;

    //Second stage...
    if (DecayEnvLevel > EnvLevel)
      then EnvLevel := EnvLevel + (DecayEnvLevel - EnvLevel) * AttackCoefficient
      else EnvLevel := DecayEnvLevel;

    In1^ := EnvLevel;

    inc(In1);
  end;
end;

procedure TEnvFollowerA.Process(In1, Out1: PSingle; const SampleFrames: integer);
var
  x1:single;
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    x1 := abs(In1^);

    //The envelope uses a two stage detection process.
    //The first stage is an envelope follower with a quick attack and the correct decay time.
    //The second stage uses the proper attack coefficient and rises until it meets the first envelope.

    //First stage...
    if x1 > DecayEnvLevel
      then DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * QuickAttackCoefficient
      else DecayEnvLevel := DecayEnvLevel + (x1 - DecayEnvLevel) * ReleaseCoefficient;

    //Second stage...
    if (DecayEnvLevel > EnvLevel)
      then EnvLevel := EnvLevel + (DecayEnvLevel - EnvLevel) * AttackCoefficient
      else EnvLevel := DecayEnvLevel;

    Out1^ := EnvLevel;

    inc(In1);
    inc(Out1);
  end;
end;









end.
