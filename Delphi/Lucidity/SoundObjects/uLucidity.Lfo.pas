unit uLucidity.Lfo;

interface

{$INCLUDE Defines.inc}

{$SCOPEDENUMS ON}

uses
  B2.Filter.CriticallyDampedLowpass,
  VamLib.MoreTypes, eeBiquadFilterCore, eeBiquadFilters,
  uLucidityEnums,
  Lucidity.Types,
  B2.MovingAverageFilter,
  soLfo.WaveTableLfo,
  soLfo.RandomLfo,
  soLfo.SlopeGen,
  eeVirtualCV, Math, uLucidityClock, eeDsp,
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
    procedure SetSampleRate(const Value: single);
    procedure SetBpm(const Value: single);
    procedure SetPar1(const Value: PSynthPar);
    procedure SetPar2(const Value: PSynthPar);
    procedure SetPar3(const Value: PSynthPar);
    procedure SetShape(const Value: TLfoShape);
  protected
    VoiceClockManager : TLucidityVoiceClockManager;
    FModuleIndex  : integer;
    LfoOutput     : single;

    ActiveLfo     : TActiveLfo;

    WaveTableLfo  : TWaveTableLfo;
    RandomLFO     : TRandomLfo;
    SlopeGen      : TSlopeGen;

    procedure UpdateLfoParameters;
  public
    constructor Create(const aModuleIndex : integer; const aVoiceClockManager : TLucidityVoiceClockManager);
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure Trigger;
    procedure Release;
    procedure ResetLfoPhase;

    property SampleRate : single    read fSampleRate write SetSampleRate;
    property Bpm        : single    read fBpm        write SetBpm;
    property Shape      : TLfoShape read fShape      write SetShape;

    property Par1 : PSynthPar read fPar1 write SetPar1;
    property Par2 : PSynthPar read fPar2 write SetPar2;
    property Par3 : PSynthPar read fPar3 write SetPar3;

    procedure StepResetA;
    procedure StepResetB;

    procedure FastControlProcess;
    procedure SlowControlProcess;
  end;




implementation

uses
  VamLib.Utils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  LucidityParameterScaling,
  SysUtils;


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

function TLucidityLfo.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'LfoOutput' then Exit(@LfoOutput);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
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
  WaveTableLfo.Bpm := Value;
  RandomLFO.Bpm    := Value;
  SlopeGen.Bpm     := Value;
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
end;


procedure TLucidityLfo.StepResetA;
begin
  WaveTableLFO.ResetPhase;
  RandomLfo.ResetPhase;

  UpdateLfoParameters;

  case ActiveLFO of
    TActiveLFO.WaveTable: LfoOutput := WaveTableLfo.Step;
    TActiveLFO.Random:    LfoOutput := RandomLfo.Step;
    TActiveLFO.Slope:     LfoOutput := 0;
  else
    raise Exception.Create('Type not handled');
  end;
end;

procedure TLucidityLfo.StepResetB;
begin
  UpdateLfoParameters;
end;


procedure TLucidityLfo.UpdateLfoParameters;
var
  LfoFreq : single;
begin
  assert(InRange(Par1^, 0, 1));
  assert(InRange(Par2^, 0, 1));
  assert(InRange(Par3^, 0, 1));

  LfoFreq := (Par1^ * Par1^) * 60 + 0.01; //TODO: Maybe use 1v/oct scaling here as well.

  WaveTableLFO.Freq          := LfoFreq;
  WaveTableLFO.PhaseOffset   := Par2^;
  WaveTableLFO.Symmetry      := Par3^;

  RandomLFO.Freq     := LfoFreq;
  RandomLFO.Density  := Par2^;
  RandomLFO.Flux     := Par3^;

  SlopeGen.Curve      := Par1^;
  SlopeGen.AttackTime := (Par2^ * Par2^) * 2000;
  SlopeGen.DecayTime  := (Par3^ * Par3^) * 2000;

  WaveTableLFO.UpdateStepSize;
  RandomLfo.UpdateStepSize;
end;

procedure TLucidityLfo.Trigger;
begin
  SlopeGen.Trigger;
end;

procedure TLucidityLfo.Release;
begin
  SlopeGen.Release;
end;

procedure TLucidityLfo.FastControlProcess;
var
  IsCycleEnd : boolean;
begin
  case ActiveLFO of
    TActiveLFO.WaveTable: LfoOutput := WaveTableLfo.Step(IsCycleEnd);
    TActiveLFO.Random:    LfoOutput := RandomLfo.Step(IsCycleEnd);
    TActiveLFO.Slope:     LfoOutput := SlopeGen.Step(IsCycleEnd);
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
  UpdateLfoParameters; //TODO: this should probably be moved to slowControlProcess().
end;



end.
