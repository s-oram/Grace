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
  eeVirtualCV, Math, uLucidityClock, eeDsp,
  uConstants;

type
  TActiveLFO = (WaveTable, Random);

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

    procedure UpdateLfoParameters;
  public
    constructor Create(const aModuleIndex : integer; const aVoiceClockManager : TLucidityVoiceClockManager);
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

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
  fModuleIndex := aModuleIndex;
end;

destructor TLucidityLfo.Destroy;
begin
  WaveTableLfo.Free;
  RandomLfo.Free;
  inherited;
end;

function TLucidityLfo.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'LfoOutput' then Exit(@LfoOutput);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
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

procedure TLucidityLfo.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  WaveTableLfo.SampleRate := Value;
  RandomLfo.SampleRate    := Value;
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
  else
    raise Exception.Create('Type not handled.');
  end;



  case Value of
    TLfoShape.SawUp:    WaveTableLfo.WaveShape := TWaveTableLfoShape.Saw;
    TLfoShape.SawDown:  WaveTableLfo.WaveShape := TWaveTableLfoShape.Ramp;
    TLfoShape.Square:   WaveTableLfo.WaveShape := TWaveTableLfoShape.Sqr;
    TLfoShape.Triangle: WaveTableLfo.WaveShape := TWaveTableLfoShape.Tri;
    TLfoShape.Sine:     WaveTableLfo.WaveShape := TWaveTableLfoShape.Sine;

    TLfoShape.RandomSmooth:  RandomLfo.WaveShape := TRandomLfoShape.RandomSmooth;
    TLfoShape.RandomStepped: RandomLfo.WaveShape := TRandomLfoShape.RandomStepped;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TLucidityLfo.FastControlProcess;
begin
  case ActiveLFO of
    TActiveLFO.WaveTable: LfoOutput := WaveTableLfo.Step;
    TActiveLFO.Random:    LfoOutput := RandomLfo.Step;
  else
    raise Exception.Create('Type not handled');
  end;



  // TODO: need to send clock event when LFO loops.

  {
  if Lfo.FastControlProcess then
  begin
    if FModuleIndex = 0
      then VoiceClockManager.SendClockEvent(ClockID_Lfo1)
      else VoiceClockManager.SendClockEvent(ClockID_Lfo2);
  end;
  }
end;

procedure TLucidityLfo.SlowControlProcess;
begin
  UpdateLfoParameters; //TODO: this should probably be moved to slowControlProcess().
end;

procedure TLucidityLfo.StepResetA;
begin
  WaveTableLFO.ResetPhase;
  RandomLfo.ResetPhase;

  UpdateLfoParameters;

  LfoOutput := WaveTableLfo.Step;
  LfoOutput := RandomLfo.Step;
end;

procedure TLucidityLfo.StepResetB;
begin
  WaveTableLFO.ResetPhase;
  //RandomLfo.ResetPhase;

  UpdateLfoParameters;

  LfoOutput := WaveTableLfo.Step;
  //LfoOutput := RandomLfo.Step;
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

  WaveTableLFO.UpdateStepSize;
  RandomLfo.UpdateStepSize;
end;


end.
