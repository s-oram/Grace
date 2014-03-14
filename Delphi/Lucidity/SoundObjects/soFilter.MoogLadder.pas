unit soFilter.MoogLadder;

interface

uses
  eeDsp,
  eeFastCode;

type
  TMoogLadder = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;
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

end;

destructor TMoogLadder.Destroy;
begin

  inherited;
end;

procedure TMoogLadder.Reset;
begin

end;

procedure TMoogLadder.SetFreq(const Value: single);
begin
  fFreq := Value;
end;

procedure TMoogLadder.SetQ(const Value: single);
begin
  fQ := Value;
end;

procedure TMoogLadder.Step(var x1, x2: single);
begin

end;

end.
