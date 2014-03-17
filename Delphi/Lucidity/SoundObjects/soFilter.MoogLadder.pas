unit soFilter.MoogLadder;

interface

uses
  FilterCore.MoogLadder,
  eeDsp,
  eeFastCode;

type
  TMoogLadder = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;

    CoreA, CoreB : TMoogLadderFilterCore;
    procedure SetFreq(const Value: single);
    procedure SetQ(const Value: single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure Step(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq : single read fFreq write SetFreq; // range 10?..1/4 Nyquist?
    property Q    : single read fQ    write SetQ;    // range 0..1
  end;

implementation

{ TMoogLadder }

constructor TMoogLadder.Create;
begin
  CoreA := TMoogLadderFilterCore.Create;
  CoreB := TMoogLadderFilterCore.Create;
end;

destructor TMoogLadder.Destroy;
begin
  CoreA.Free;
  CoreB.Free;
  inherited;
end;

procedure TMoogLadder.Reset;
begin
  CoreA.Reset;
  CoreB.Reset;
end;

procedure TMoogLadder.SetFreq(const Value: single);
var
  wc : double;
  g : double;
begin
  fFreq := Value;


  wc := 2 * pi * Value / SampleRate;
  g := (0.9892 * wc) - (0.4342 * wc * wc) + (0.1381 * wc * wc * wc) - (0.0202 * wc * wc * wc * wc);

  CoreA.CutoffGain := G;
  CoreB.CutoffGain := G;
end;

procedure TMoogLadder.SetQ(const Value: single);
begin
  fQ := Value;

  CoreA.FeedbackGain := Value;
  CoreB.FeedbackGain := Value;
end;

procedure TMoogLadder.Step(var x1, x2: single);
begin
  x1 := CoreA.Step(x1);
  x2 := CoreB.Step(x2);
end;

end.
