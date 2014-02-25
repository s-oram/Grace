unit soLfo.WaveTableLfo;

interface

type
  TWaveTableLfo = class
  private
    fSampleRate: single;
    fBpm: single;
    fPhaseOffset: single;
    fPulseWidthMod: single;
    fFreq: single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPhase;

    procedure UpdateStepSize; //call when the step size needs to be re-calculated. Normally after changing any LFO parameter.
    function Step : single; // generate the next LFO output sample.

    property Bpm        : single read fBpm        write fBpm;
    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq          : single read fFreq          write fFreq;        // LFO frequency in hertz.
    property PhaseOffset   : single read fPhaseOffset   write fPhaseOffset; //Range 0..1
    property PulseWidthMod : single read fPulseWidthMod write fPulseWidthMod; //Range 0..1, with 0.5 being no modulation.

  end;

implementation

{ TWaveTableLfo }

constructor TWaveTableLfo.Create;
begin

end;

destructor TWaveTableLfo.Destroy;
begin

  inherited;
end;

procedure TWaveTableLfo.ResetPhase;
begin
  //TODO:
end;

function TWaveTableLfo.Step: single;
begin

  // output should be ranged 0..1.
  //result := 0;
  result := random * 0.5 + 0.5;
end;

procedure TWaveTableLfo.UpdateStepSize;
begin

end;

end.
