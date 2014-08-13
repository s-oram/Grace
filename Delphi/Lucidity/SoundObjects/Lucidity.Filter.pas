unit Lucidity.Filter;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.Utils,
  VamLib.MoreTypes,
  B2.DelayLine.StereoDelayBuffer,
  uConstants,
  uLucidityEnums,
  Math,
  Lucidity.Types,
  eeVirtualCV,
  AudioEffect.Lofi,
  FilterCore.SimperSVF,
  soFilter.Test,
  soFilter.MoogLadder,
  soFilter.RingModA,
  soFilter.DistortionA,
  soFilter.CombA,
  soFilter.BlueFilter,
  soFilter.OptimisedFilter,
  soSineOsc;

type
  TLucidityFilter = class
  private
    fSampleRate: single;
    fFilterType: TFilterType;
    fKeyFollow: single;
    fPar2: PSynthPar;
    fPar3: PSynthPar;
    fPar1: PSynthPar;
    fPar4: PSynthPar;
    fKeyFollowFreqMultiplier: single;
    fIsActive: boolean;

    procedure SetSampleRate(const Value: single);
    procedure SetFilterType(const Value: TFilterType);
    procedure SetKeyFollow(const Value: single);
  protected
    DistortionA : TDistortionA;
    RingModA    : TRingModA;
    LofiA       : TLofi;
    CombA       : TCombA;
    BlueFilter  : TBlueFilter;
    MoogLadder  : TMoogLadder;
    OptimisedFilter : TOptimisedFilter;

    TestFilter : TTestFilter;

    DummyModValue : single; //TODO: Delete this.

    VoiceModPoints : PVoiceModulationPoints;

    procedure UpdateFilterParameters; {$IFDEF AudioInline}inline;{$ENDIF}

    property IsActive : boolean read fIsActive;
  public
    constructor Create(const aVoiceModPoints : PVoiceModulationPoints);
    destructor Destroy; override;

    property Par1 : PSynthPar read fPar1 write fPar1;
    property Par2 : PSynthPar read fPar2 write fPar2;
    property Par3 : PSynthPar read fPar3 write fPar3;
    property Par4 : PSynthPar read fPar4 write fPar4;

    property KeyFollowFreqMultiplier : single read fKeyFollowFreqMultiplier write fKeyFollowFreqMultiplier;

    // TODO: delete this old mod pointer stuff.
    function GetModPointer(const Name:string):PSingle;

    procedure ResetAndMakeActive; //call before starting audio processing.
    procedure Kill;               //call when audio processing has finished.

    property SampleRate : single read fSampleRate write SetSampleRate;

    procedure AudioRateStep(var x1, x2 : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    //==== Parameters ====
    property FilterType : TFilterType read fFilterType write SetFilterType;
    property KeyFollow  : single      read fKeyFollow  write SetKeyFollow;  //range 0..1.




  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeDsp,
  SysUtils,
  LucidityParameterScaling;

{ TLucidityFilter }

constructor TLucidityFilter.Create(const aVoiceModPoints : PVoiceModulationPoints);
begin
  fIsActive := false;

  VoiceModPoints := aVoiceModPoints;

  //TODO:MED Currently all filters are active at once. It would be better
  // to only instantiate the active filter and leave the others off.

  TestFilter  := TTestFilter.Create;
  RingModA    := TRingModA.Create;
  DistortionA := TDistortionA.Create;
  LofiA       := TLofi.Create;
  CombA       := TCombA.Create;
  BlueFilter  := TBlueFilter.Create;
  MoogLadder  := TMoogLadder.Create;
  OptimisedFilter := TOptimisedFilter.Create;
end;

destructor TLucidityFilter.Destroy;
begin
  TestFilter.Free;
  BlueFilter.Free;
  LofiA.Free;
  CombA.Free;
  RingModA.Free;
  DistortionA.Free;
  MoogLadder.Free;
  OptimisedFilter.Free;
  inherited;
end;

procedure TLucidityFilter.ResetAndMakeActive;
begin
  fIsActive := true;

  DistortionA.Reset;
  RingModA.Reset;
  BlueFilter.Reset;
  MoogLadder.Reset;
  OptimisedFilter.Reset;

  UpdateFilterParameters;
end;

function TLucidityFilter.GetModPointer(const Name: string): PSingle;
begin
  if Name ='Par1Mod' then exit(@DummyModValue);
  if Name ='Par2Mod' then exit(@DummyModValue);  if Name ='Par3Mod' then exit(@DummyModValue);  if Name ='Par4Mod' then exit(@DummyModValue);  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');end;

procedure TLucidityFilter.Kill;
begin
  fIsActive := false;
end;

procedure TLucidityFilter.SetFilterType(const Value: TFilterType);
begin
  fFilterType := Value;

  // TODO:HIGH probably need a mutex around IsActive.
  // SetFilterType can be called from the GUI thread as far as I know.
  if IsActive then
  begin
    LofiA.Reset;
    CombA.Reset;
    BlueFilter.Reset;
    RingModA.Reset;
    DistortionA.Reset;
    MoogLadder.Reset;
    OptimisedFilter.Reset;

    UpdateFilterParameters;
  end;
end;

procedure TLucidityFilter.SetKeyFollow(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  fKeyFollow := Value;
end;

procedure TLucidityFilter.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;

  BlueFilter.SampleRate := Value;
  LofiA.SampleRate := Value;
  CombA.SampleRate := Value;
  RingModA.SampleRate := Value;
  DistortionA.SampleRate := Value;
  MoogLadder.SampleRate := value;
  OptimisedFilter.SampleRate := Value;
end;

procedure TLucidityFilter.UpdateFilterParameters;
const
  kBaseFilterFreq = 4.0878994578;
  kMinFreq = 0.001;
  kMaxFreq = 20000;
  kMinQ = 0;
  kMaxQ = 1;
var
  cFreq : single;
  cQ    : single;
  CV    : single;
  Gain  : single;

  FreqMultFactor : single;
begin
  assert(Par1^ >= 0);
  assert(Par1^ <= 1);

  assert(Par2^ >= 0);
  assert(Par2^ <= 1);

  assert(Par3^ >= 0);
  assert(Par3^ <= 1);


  FreqMultFactor := LinearInterpolation(1, VoiceModPoints^.KeyFollowFreqMultiplier, fKeyFollow);

  case FilterType of
    ftNone:
    begin
    end;

    ftLofiA:
    begin
      //==== Lofi A ====
      LofiA.RateReduction := Par1^;
      LofiA.BitReduction  := Par2^;
      LofiA.BitEmphasis   := Par3^;
    end;

    ftRingModA:
    begin
      //==== Ring Mod A ====
      CV := (Par1^ * 12);
      cFreq := VoltsToFreq(15, CV)  * FreqMultFactor;
      cFreq := Clamp(cFreq, 15, 18000);   //TODO:MED this clamp might not be needed. Par1^ is already limited to a 0-1 range.
      RingModA.OscFreq := cFreq;

      RingModA.Depth := Par2^;
    end;


    {
    ftDistA:
    begin
      //==== Distortion A ====
      px1 := Par1 + Par1Mod;
      Clamp(px1, 0, 1);
      DistortionA.Par1 := px1;

      px2 := Par2 + Par2Mod;
      clamp(px2, 0, 1);
      DistortionA.Par2 := px2;

      px3 := Par3 + Par3Mod;
      clamp(px3, 0, 1);
      DistortionA.Par3 := px3;
    end;
    }

    ftCombA:
    begin
      //==== Comb A ====
      CombA.KeyFollowFreqMultiplier := KeyFollowFreqMultiplier;

      CombA.Par1 := Par1^;
      CombA.Par2 := Par2^;
      CombA.Par3 := Par3^;
    end;

    ft2PoleLowPass,
    ft2PoleBandPass,
    ft2PoleHighPass,
    ft4PoleLowPass,
    ft4PoleBandPass,
    ft4PoleHighPass:
    begin
      cFreq := VstFloatToFilterFrequency(Par1^) * FreqMultFactor;
      cFreq := Clamp(cFreq, kMinFreq, kMaxFreq);
      cQ := (Par2^) * 0.98;
      Gain := Par3^;
      BlueFilter.UpdateParameters(cFreq, cQ, Gain);
    end;
  end;

end;

procedure TLucidityFilter.FastControlProcess;
begin
  UpdateFilterParameters;
end;

procedure TLucidityFilter.SlowControlProcess;
begin
end;

procedure TLucidityFilter.AudioRateStep(var x1, x2: single);
begin
  case FilterType of
    ftNone: ;
    ftLofiA:         LofiA.Step(x1, x2);
    ftRingModA:      RingModA.AudioRateStep(x1, x2);
    //ftDistA:     DistortionA.AudioRateStep(x1, x2);
    ftCombA:         CombA.AudioRateStep(x1, x2);
    ft2PoleLowPass:  BlueFilter.StepAsLowpass2P(x1, x2);
    ft2PoleBandPass: BlueFilter.StepAsBandpass2P(x1, x2);
    ft2PoleHighPass: BlueFilter.StepAsHighpass2P(x1, x2);
    ft4PoleLowPass:  BlueFilter.StepAsLowpass4P(x1, x2);
    ft4PoleBandPass: BlueFilter.StepAsBandpass4P(x1, x2);
    ft4PoleHighPass: BlueFilter.StepAsHighpass4P(x1, x2);
  else
    raise Exception.Create('Unexpected filter type.');
  end;
end;

end.
