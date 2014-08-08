unit uLucidity.Lfo;

interface

{$INCLUDE Defines.inc}

{$SCOPEDENUMS ON}

uses
  VamLib.Utils,
  eeDsp,
  B2.Filter.CriticallyDampedLowpass,
  VamLib.MoreTypes, eeBiquadFilterCore, eeBiquadFilters,
  uLucidityEnums,
  Lucidity.Types,
  B2.MovingAverageFilter,
  soLfo.WaveTableLfo,
  soLfo.RandomLfo,
  soLfo.SlopeGen,
  eeVirtualCV, Math, uLucidityClock,
  uConstants;

type
  TActiveLFO = (WaveTable, Random, Slope);

  TLucidityLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fShape: TLfoShape;
    fPar1: PSynthPar;
    fPar2: PSynthPar;
    fPar3: PSynthPar;
    fFreqMode: TLfoFreqMode;
    procedure SetSampleRate(const Value: single);
    procedure SetBpm(const Value: single);
    procedure SetPar1(const Value: PSynthPar);
    procedure SetPar2(const Value: PSynthPar);
    procedure SetPar3(const Value: PSynthPar);
    procedure SetShape(const Value: TLfoShape);
    procedure SetFreqMode(const Value: TLfoFreqMode);
  protected
    IsActive : boolean;
    VoiceClockManager : TLucidityVoiceClockManager;
    FModuleIndex  : integer;
    LfoOutput_Unipolar : single;

    ActiveLfo     : TActiveLfo;

    WaveTableLfo  : TWaveTableLfo;
    RandomLFO     : TRandomLfo;
    SlopeGen      : TSlopeGen;

    MaxLfoFreq : single;

    procedure UpdateLfoParameters; inline;
    procedure UpdateMaxLfoFreq; inline;
  public
    constructor Create(const aModuleIndex : integer; const aVoiceClockManager : TLucidityVoiceClockManager);
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure Trigger;
    procedure Release;
    procedure Kill;
    procedure ResetLfoPhase;

    property SampleRate : single       read fSampleRate write SetSampleRate;
    property Shape      : TLfoShape    read fShape      write SetShape;
    property Bpm        : single       read fBpm        write SetBpm;
    property FreqMode   : TLfoFreqMode read fFreqMode   write SetFreqMode;

    property Par1 : PSynthPar read fPar1 write SetPar1;
    property Par2 : PSynthPar read fPar2 write SetPar2;
    property Par3 : PSynthPar read fPar3 write SetPar3;

    //==== reset methods ====
    procedure ZeroOutput;
    procedure StepResetA;
    procedure StepResetB;
    //=======================

    procedure FastControlProcess;
    procedure SlowControlProcess;
  end;

//TODO:HIGH delete these methods.
function ComputeLfoFrequency(const FreqPar : single; const FreqMode : TLfoFreqMode; const RangeMult : single; const Bpm, SampleRate : single):single;
function ComputeSlopeTime(const TimePar : single; const FreqMode : TLfoFreqMode; const RangeMult : single; const Bpm, SampleRate : single):single;

implementation

uses
  eePitch,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  LucidityParameterScaling,
  SysUtils;


function ComputeLfoFrequency(const FreqPar : single; const FreqMode : TLfoFreqMode; const RangeMult : single; const Bpm, SampleRate : single):single;
  function CalcSyncFreq(const FreqPar, Bpm, SampleRate : single; const BeatDivision : integer; const Bars : single): single; inline;
  var
    LfoFreq : single;
    BeatSync : double;
    Beats : integer;
  begin
    Beats := round(BeatDivision * Bars * (1-FreqPar));
    if Beats < 1
      then Beats := 1;
    BeatSync := Beats / BeatDivision;
    LfoFreq := SyncToSamples(BeatSync, Bpm, SampleRate);
    LfoFreq := SampleRate / LfoFreq;
    result := LfoFreq;
  end;
var
  LfoFreq : single;
  MinFreq, MaxFreq : single;
begin
  case FreqMode of
    TLfoFreqMode.Hertz:
    begin
      MaxFreq := 30;
      MinFreq := 1 / Power(2,RangeMult);
      LfoFreq := (FreqPar * FreqPar) * (MaxFreq-MinFreq) + MinFreq;
    end;
    TLfoFreqMode.Sync4:   LfoFreq := CalcSyncFreq(FreqPar, Bpm, SampleRate, 4,   RangeMult);
    TLfoFreqMode.Sync8:   LfoFreq := CalcSyncFreq(FreqPar, Bpm, SampleRate, 8,   RangeMult);
    TLfoFreqMode.Sync16:  LfoFreq := CalcSyncFreq(FreqPar, Bpm, SampleRate, 16,  RangeMult);
    TLfoFreqMode.Sync32:  LfoFreq := CalcSyncFreq(FreqPar, Bpm, SampleRate, 32,  RangeMult);
    TLfoFreqMode.Sync64:  LfoFreq := CalcSyncFreq(FreqPar, Bpm, SampleRate, 64,  RangeMult);
  else
    raise Exception.Create('Type not handled.');
  end;

  result := LfoFreq;
end;



function ComputeSlopeTime(const TimePar : single; const FreqMode : TLfoFreqMode; const RangeMult : single; const Bpm, SampleRate : single):single;
  function CalcTime(const TimePar, Bpm, SampleRate : single; const BeatDivision : integer; const Bars : single): single; inline;
  var
    TimeInSamples : single;
    BeatSync : double;
    Beats : integer;
  begin
    Beats := round(BeatDivision * Bars * TimePar);
    if Beats < 1
      then Beats := 1;
    BeatSync := Beats / BeatDivision;
    TimeInSamples := SyncToSamples(BeatSync, Bpm, SampleRate);
    result := SamplesToMilliSeconds(TimeInSamples, SampleRate);
  end;
var
  SlopeTime : single;
  MaxTime : single;
begin
  case FreqMode of
    TLfoFreqMode.Hertz:
    begin
      MaxTime := 1000 * RangeMult;
      SlopeTime := (TimePar * TimePar) * MaxTime;
    end;
    TLfoFreqMode.Sync4:   SlopeTime := CalcTime(TimePar, Bpm, SampleRate, 4,   RangeMult);
    TLfoFreqMode.Sync8:   SlopeTime := CalcTime(TimePar, Bpm, SampleRate, 8,   RangeMult);
    TLfoFreqMode.Sync16:  SlopeTime := CalcTime(TimePar, Bpm, SampleRate, 16,  RangeMult);
    TLfoFreqMode.Sync32:  SlopeTime := CalcTime(TimePar, Bpm, SampleRate, 32,  RangeMult);
    TLfoFreqMode.Sync64:  SlopeTime := CalcTime(TimePar, Bpm, SampleRate, 64,  RangeMult);
  else
    raise Exception.Create('Type not handled.');
  end;

  if SlopeTime < 20
    then SlopeTime := 20;


  result := SlopeTime;
end;


{ TLucidityLfo }

constructor TLucidityLfo.Create(const aModuleIndex : integer; const aVoiceClockManager : TLucidityVoiceClockManager);
begin
  VoiceClockManager := aVoiceClockManager;
  WaveTableLfo := TWaveTableLfo.Create;
  RandomLFO    := TRandomLfo.Create;
  SlopeGen     := TSlopeGen.Create;
  fModuleIndex := aModuleIndex;
end;


destructor TLucidityLfo.Destroy;
begin
  WaveTableLfo.Free;
  RandomLfo.Free;
  SlopeGen.Free;
  inherited;
end;

procedure TLucidityLfo.UpdateMaxLfoFreq;
begin
  case FreqMode of
    TLfoFreqMode.Hertz:   MaxLfoFreq := 15;
    TLfoFreqMode.Sync4:   MaxLfoFreq := SyncToFreq(1/4, BPM);
    TLfoFreqMode.Sync8:   MaxLfoFreq := SyncToFreq(1/8, BPM);
    TLfoFreqMode.Sync16:  MaxLfoFreq := SyncToFreq(1/16, BPM);
    TLfoFreqMode.Sync32:  MaxLfoFreq := SyncToFreq(1/32, BPM);
    TLfoFreqMode.Sync64:  MaxLfoFreq := SyncToFreq(1/64, BPM);
  else
    raise Exception.Create('Type not handled.');
  end;

  SlopeGen.MinTotalTime := 1000 / MaxLfoFreq;
end;



function TLucidityLfo.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'LfoOutput_Uni' then Exit(@LfoOutput_Unipolar);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidityLfo.SetPar1(const Value: PSynthPar);
begin
  fPar1 := Value;
end;

procedure TLucidityLfo.SetPar2(const Value: PSynthPar);
begin
  fPar2 := Value;
end;

procedure TLucidityLfo.SetPar3(const Value: PSynthPar);
begin
  fPar3 := Value;
end;

procedure TLucidityLfo.ResetLfoPhase;
begin
  WaveTableLFO.ResetPhase;
  RandomLfo.ResetPhase;
end;

procedure TLucidityLfo.SetBpm(const Value: single);
begin
  fBpm := Value;
  UpdateMaxLfoFreq;

  WaveTableLfo.Bpm := Value; //TODO:MED this shouldn't be needed anymore.
  RandomLFO.Bpm    := Value; //TODO:MED this shouldn't be needed anymore.
  SlopeGen.Bpm     := Value; //TODO:MED this shouldn't be needed anymore.
end;

procedure TLucidityLfo.SetFreqMode(const Value: TLfoFreqMode);
begin
  fFreqMode := Value;
  UpdateMaxLfoFreq;
end;

procedure TLucidityLfo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  WaveTableLfo.SampleRate := Value;
  RandomLfo.SampleRate    := Value;
  SlopeGen.SampleRate     := Value;
end;

procedure TLucidityLfo.SetShape(const Value: TLfoShape);
begin
  fShape := Value;

  case Value of
    TLfoShape.SawUp:         ActiveLfo := TActiveLfo.WaveTable;
    TLfoShape.SawDown:       ActiveLfo := TActiveLfo.WaveTable;
    TLfoShape.Square:        ActiveLfo := TActiveLfo.WaveTable;
    TLfoShape.Triangle:      ActiveLfo := TActiveLfo.WaveTable;
    TLfoShape.Sine:          ActiveLfo := TActiveLfo.WaveTable;
    TLfoShape.RandomSmooth:  ActiveLfo := TActiveLfo.Random;
    TLfoShape.RandomStepped: ActiveLfo := TActiveLfo.Random;
    TLfoShape.AttackDecay:   ActiveLfo := TActiveLfo.Slope;
    TLfoShape.AttackRelease: ActiveLfo := TActiveLfo.Slope;
    TLfoShape.Cycle:         ActiveLfo := TActiveLfo.Slope;
  else
    raise Exception.Create('Type not handled.');
  end;



  case Value of
    TLfoShape.SawUp:         WaveTableLfo.WaveShape := TWaveTableLfoShape.Saw;
    TLfoShape.SawDown:       WaveTableLfo.WaveShape := TWaveTableLfoShape.Ramp;
    TLfoShape.Square:        WaveTableLfo.WaveShape := TWaveTableLfoShape.Sqr;
    TLfoShape.Triangle:      WaveTableLfo.WaveShape := TWaveTableLfoShape.Tri;
    TLfoShape.Sine:          WaveTableLfo.WaveShape := TWaveTableLfoShape.Sine;

    TLfoShape.RandomSmooth:  RandomLfo.WaveShape := TRandomLfoShape.RandomSmooth;
    TLfoShape.RandomStepped: RandomLfo.WaveShape := TRandomLfoShape.RandomStepped;

    TLfoShape.AttackDecay:   SlopeGen.SlopeMode := TSlopeMode.AD;
    TLfoShape.AttackRelease: SlopeGen.SlopeMode := TSlopeMode.AR;
    TLfoShape.Cycle:         SlopeGen.SlopeMode := TSlopeMode.Cycle;
  else
    raise Exception.Create('Type not handled.');
  end;

  if IsActive
    then UpdateLfoParameters;
end;

procedure TLucidityLfo.ZeroOutput;
begin
  UpdateLfoParameters;

  WaveTableLFO.ResetPhase;
  RandomLfo.ResetPhase;

  case ActiveLFO of
    TActiveLFO.WaveTable: LfoOutput_Unipolar := WaveTableLfo.Step;
    TActiveLFO.Random:    LfoOutput_Unipolar := RandomLfo.Step;
    TActiveLFO.Slope:     LfoOutput_Unipolar := 0;
  else
    raise Exception.Create('Type not handled');
  end;
end;

procedure TLucidityLfo.StepResetA;
begin
  UpdateLfoParameters;
end;

procedure TLucidityLfo.StepResetB;
begin
  UpdateLfoParameters;
end;


procedure TLucidityLfo.UpdateLfoParameters;
begin
  assert(InRange(Par1^, 0, 1));
  assert(InRange(Par2^, 0, 1));
  assert(InRange(Par3^, 0, 1));

  case ActiveLFO of
    TActiveLFO.WaveTable:
    begin
      //WaveTableLFO.Freq          := ComputeLfoFrequency(Par1^, FreqMode, RangeMult, Bpm, SampleRate);
      WaveTableLfo.Freq          := Par1^ * MaxLfoFreq;
      WaveTableLFO.PhaseOffset   := Par2^;
      WaveTableLFO.Symmetry      := Par3^;
      WaveTableLFO.UpdateStepSize;
    end;

    TActiveLFO.Random:
    begin
      //RandomLFO.Freq     := ComputeLfoFrequency(Par1^, FreqMode, RangeMult, Bpm, SampleRate);
      RandomLFO.Freq     := Par1^ * MaxLfoFreq;
      RandomLFO.Density  := Par2^;
      RandomLFO.Flux     := Par3^;
      RandomLfo.UpdateStepSize;
    end;

    TActiveLFO.Slope:
    begin
      //TODO: Slope gen needs to have it's attack and decay times quantised as well.
      SlopeGen.EnvRate := Par1^;
      SlopeGen.Curve   := Par3^;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TLucidityLfo.Trigger;
begin
  IsActive := true;
  SlopeGen.Trigger;
end;

procedure TLucidityLfo.Release;
begin
  SlopeGen.Release;
end;

procedure TLucidityLfo.Kill;
begin
  SlopeGen.Kill;
  IsActive := false;
end;



procedure TLucidityLfo.FastControlProcess;
var
  IsCycleEnd : boolean;
begin
  case ActiveLFO of
    TActiveLFO.WaveTable: LfoOutput_Unipolar := WaveTableLfo.Step(IsCycleEnd);
    TActiveLFO.Random:    LfoOutput_Unipolar := RandomLfo.Step(IsCycleEnd);
    TActiveLFO.Slope:     LfoOutput_Unipolar := SlopeGen.Step(IsCycleEnd);
  else
    raise Exception.Create('Type not handled');
  end;

  // HACK: NOTE: This bit feels a bit hackish because
  // the LFO "knows" whether it's LFO 1 or LFO 2.
  // The LFO shouldn't really know that. A better design would
  // be to fire an event here. This particular bit of code is a
  // hangover from an earlier version of the LFO, most of which
  // has since been rewritten or refactored away. While I don't
  // like the code below I've not changed it yet because it's
  // possibly slightly more CPU efficient with one less layer of
  // indirection. Lately I'm also coming to the opion that
  // it's really hard to reuse sound generation objects across
  // multiple projects and perhaps it's not worth doing so.
  if IsCycleEnd then
  begin
    if FModuleIndex = 0
      then VoiceClockManager.SendClockEvent(ClockID_Lfo1)
      else VoiceClockManager.SendClockEvent(ClockID_Lfo2);
  end;
end;

procedure TLucidityLfo.SlowControlProcess;
begin
  UpdateLfoParameters;
end;



end.
