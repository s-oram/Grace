unit soLucidityWaveOsc;

interface

{$INCLUDE Defines.inc}

uses
  uBlitOsc,
  Math,
  soSineOsc,
  soSawSquareOsc,
  soDynamicWaveTableOsc,
  soDynamicWaveTableOsc.WaveGen;

type
  TLucidityWaveOsc = class
  private
    fSampleRate: single;
    fPitchTwo: single;
    fPitchOne: single;
    fShape: single;
    fPulseWidth: single;
    fAmAmount: single;
    fFmAmount: single;
    procedure SetSampleRate(const Value: single);
  protected
    CurPhase : single;
    StepSize : single;
    TriggerMidiNote : single;

    SineOsc : TSineOsc;
    SawSquareOsc : TSawSquareOsc;
    BlitOsc : TBlitOsc;
    DynamicWaveTableOsc : TDynamicWaveTableOsc;

    function CalcOscFreq(const MidiNote : single; const PitchOne, PitchTwo:single):single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger(const MidiNote : single);

    procedure FastControlRateProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure Step(const SampleOsc1, SampleOsc2 : Single; out Out1, Out2: Single); {$IFDEF AudioInline}inline;{$ENDIF}

    property SampleRate : single read fSampleRate write SetSampleRate;

    //== Parameters ===
    property PitchOne   : single read fPitchOne   write fPitchOne;   //range -1..1
    property PitchTwo   : single read fPitchTwo   write fPitchTwo;   //range -1..1
    property Shape      : single read fShape      write fShape;      //range 0..1
    property PulseWidth : single read fPulseWidth write fPulseWidth; //range 0..1
    property FmAmount   : single read fFmAmount   write fFmAmount; //range 0..1
    property AmAmount   : single read fAmAmount   write fAmAmount; //range 0..1
  end;

implementation

uses
  SysUtils,
  eePitch;

{ TLucidityOsc }

constructor TLucidityWaveOsc.Create;
begin
  fSampleRate := 44100;
  CurPhase := 0;

  BlitOsc := TBlitOsc.Create;
  DynamicWaveTableOsc := TDynamicWaveTableOsc.Create;

  SawSquareOsc := TSawSquareOsc.Create;

  SineOsc := TSineOsc.Create;

  fPitchOne := 0;
  fPitchTwo := 0;

  fFmAmount := 0;
  fAmAmount := 0;
end;

destructor TLucidityWaveOsc.Destroy;
begin
  BlitOsc.Free;
  DynamicWaveTableOsc.Free;
  SawSquareOsc.Free;
  SineOsc.Free;
  inherited;
end;

procedure TLucidityWaveOsc.Trigger(const MidiNote: single);
var
  NoteFreq : single;
begin
  TriggerMidiNote := MidiNote;

  StepSize := (1 / fSampleRate) * MidiToCps(MidiNote);
  assert(StepSize < 1);

  NoteFreq := CalcOscFreq(TriggerMidiNote, PitchOne, PitchTwo);

  BlitOsc.Freq := NoteFreq;
  DynamicWaveTableOsc.Freq := NoteFreq;
  DynamicWaveTableOsc.FillTableWithSineWave;
  DynamicWaveTableOsc.FillBufferFrames;
  //GenerateSawWaveform(DynamicWaveTableOsc, NoteFreq, 22000);

  SawSquareOsc.RootFreq   := NoteFreq;
  SawSquareOsc.Freq       := NoteFreq;
  SawSquareOsc.Shape      := Shape;
  SawSquareOsc.PulseWidth := PulseWidth;
  SawSquareOsc.FmAmount   := FmAmount;
  SawSquareOsc.AmAmount   := AmAmount;

  SineOsc.Freq := NoteFreq;

end;

procedure TLucidityWaveOsc.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  BlitOsc.SampleRate := Value;
  DynamicWaveTableOsc.SampleRate := Value;
  SawSquareOsc.SampleRate := Value;
  SineOsc.SampleRate := Value;
end;

function TLucidityWaveOsc.CalcOscFreq(const MidiNote, PitchOne, PitchTwo: single): single;
var
  Hertz : single;
begin
  Hertz := MidiToCps(MidiNote + round(PitchOne * 12) + PitchTwo);
  result := Hertz;
end;

procedure TLucidityWaveOsc.FastControlRateProcess;
begin
  DynamicWaveTableOsc.Freq := CalcOscFreq(TriggerMidiNote, PitchOne, PitchTwo);

  SawSquareOsc.Freq       := CalcOscFreq(TriggerMidiNote, PitchOne, PitchTwo);
  SawSquareOsc.Shape      := Shape;
  SawSquareOsc.PulseWidth := PulseWidth;
  SawSquareOsc.FmAmount   := FmAmount;
  SawSquareOsc.AmAmount   := AmAmount;

  SineOsc.Freq := CalcOscFreq(TriggerMidiNote, PitchOne, PitchTwo);
end;

procedure TLucidityWaveOsc.Step(const SampleOsc1, SampleOsc2 : Single; out Out1, Out2: Single);
var
  x : single;
begin
  x := SineOsc.AudioRateStep;
  Out1 := x;
  Out2 := x;
end;


end.
