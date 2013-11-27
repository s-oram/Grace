

unit eeEnvelopeFollower;

interface

type
  TEnvelopeFollower = class
  private
    fSampleRate : integer;
    fAttackTime : single;
    fHoldTime   : single;
    fDecayTime  : single;
    procedure SetAttackTime(const Value: single);
    procedure SetDecayTime(const Value: single);
    procedure SetHoldTime(const Value: single);
    procedure SetSampleRate(const Value: integer);
  protected
    DecayEnvLevel:single;
    EnvLevel:single;

    QuickAttackCoefficient : single;
    AttackCoefficient      : single;
    DecayCoefficient       : single;
  public
    constructor Create;
	  destructor Destroy; override;

    function Step_Stereo(In1, In2:single):single;
    

    property SampleRate:integer read fSampleRate write SetSampleRate;

    property Level:single read EnvLevel;

    property AttackTime :single read fAttackTime write SetAttackTime;  //milliseconds.
    property HoldTime   :single read fHoldTime   write SetHoldTime;    //milliseconds.
    property DecayTime  :single read fDecayTime  write SetDecayTime;   //milliseconds.

  end;

implementation

{ TEnvelopeFollower }

constructor TEnvelopeFollower.Create;
begin
  EnvLevel       := 0;
  SampleRate     := 44100;

  fAttackTime := 0;
  fDecayTime  := 0;

  AttackTime := 0;
  DecayTime  := 0.1;

  DecayEnvLevel := 0;
end;

destructor TEnvelopeFollower.Destroy;
begin

  inherited;
end;

procedure TEnvelopeFollower.SetAttackTime(const Value: single);
var
  tau:double;
begin
  fAttackTime := Value;
  tau  := Value / 1000;
  AttackCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
end;

procedure TEnvelopeFollower.SetDecayTime(const Value: single);
var
  tau:double;
begin
  fDecayTime := Value;
  tau  := Value / 1000;
  DecayCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
end;



procedure TEnvelopeFollower.SetHoldTime(const Value: single);
begin
  fHoldTime := Value;
end;

procedure TEnvelopeFollower.SetSampleRate(const Value: integer);
var
  Time:single;
  tau:double;
begin
  fSampleRate := Value;

  Time := 0.001;
  tau  := Time / 1000;
  QuickAttackCoefficient :=  1 - exp( -1.0 / (tau*sampleRate));
end;

function TEnvelopeFollower.Step_Stereo(In1, In2: single): single;
var
  x1,x2:single;
  StereoLevel:single;
begin
  x1 := abs(In1);
  x2 := abs(In2);

  //This is a bit crude. could investigate different schemes for calculating the level of a stereo signal.
  if x1 > x2
    then StereoLevel := x1
    else StereoLevel := x2;

  //The envelope uses a two stage detection process.
  //The first stage is an envelope follower with a quick attack and the correct decay time.
  //The second stage uses the proper attack coefficient and rises until it meets the first envelope.

  //First stage...
  if StereoLevel > DecayEnvLevel
    then DecayEnvLevel := DecayEnvLevel + (StereoLevel - DecayEnvLevel) * QuickAttackCoefficient
    else DecayEnvLevel := DecayEnvLevel + (StereoLevel - DecayEnvLevel) * DecayCoefficient;

  //Second stage...
  if (DecayEnvLevel > EnvLevel)
    then EnvLevel := EnvLevel + (DecayEnvLevel - EnvLevel) * AttackCoefficient
    else EnvLevel := DecayEnvLevel;

  result := EnvLevel;
end;

end.
