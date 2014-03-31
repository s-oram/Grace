unit soFilter.OptimisedFilter;

interface

uses
  eeDsp,
  eeFastCode;

type
  TOptimisedFilter = class
  private
    fSampleRate: single;
    fFreq: single;
    fQ: single;
    fInputGain: single;
    procedure SetFreq(const Value: single);
    procedure SetQ(const Value: single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;

    procedure StepAs2PoleLP(var x1, x2 : single); inline;
    procedure StepAs2PoleBP(var x1, x2 : single); inline;
    procedure StepAs2PoleHP(var x1, x2 : single); inline;

    procedure StepAs4PoleLP(var x1, x2 : single); inline;
    procedure StepAs4PoleBP(var x1, x2 : single); inline;
    procedure StepAs4PoleHP(var x1, x2 : single); inline;

    property SampleRate : single read fSampleRate write fSampleRate;

    property Freq      : single read fFreq      write SetFreq; // range 10?..1/4 Nyquist?
    property Q         : single read fQ         write SetQ;    // range 0..1
    property InputGain : single read fInputGain write fInputGain; //Linear value.
  end;

implementation

{ TOptimisedFilter }

constructor TOptimisedFilter.Create;
begin

end;

destructor TOptimisedFilter.Destroy;
begin

  inherited;
end;

procedure TOptimisedFilter.Reset;
begin

end;

procedure TOptimisedFilter.SetFreq(const Value: single);
begin

end;

procedure TOptimisedFilter.SetQ(const Value: single);
begin

end;

procedure TOptimisedFilter.StepAs2PoleBP(var x1, x2: single);
begin

end;

procedure TOptimisedFilter.StepAs2PoleHP(var x1, x2: single);
begin

end;

procedure TOptimisedFilter.StepAs2PoleLP(var x1, x2: single);
begin

end;

procedure TOptimisedFilter.StepAs4PoleBP(var x1, x2: single);
begin

end;

procedure TOptimisedFilter.StepAs4PoleHP(var x1, x2: single);
begin

end;

procedure TOptimisedFilter.StepAs4PoleLP(var x1, x2: single);
begin

end;

end.
