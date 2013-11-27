unit B2.Lfo.SmoothedLfo;

interface

uses
  B2.Lfo.SimpleLfo,
  B2.Filter.LeakyInteger;

type
  TSmoothedLfo = class(TSimpleLfo)
  private
    fSmoothingFilter: TLeakyInteger;
  public
    constructor Create; override;
    destructor Destroy; override;

    function StepUnipolar : single; inline;
    function StepBipolar  : single; inline;

    property SmoothingFilter : TLeakyInteger read fSmoothingFilter write fSmoothingFilter;
  end;

implementation

{ TSmoothedLfo }

constructor TSmoothedLfo.Create;
begin
  inherited;


  SmoothingFilter := TLeakyInteger.Create;
end;

destructor TSmoothedLfo.Destroy;
begin
  SmoothingFilter.Free;
  inherited;
end;

function TSmoothedLfo.StepBipolar: single;
begin
  result := SmoothingFilter.Step(inherited StepBipolar);
end;

function TSmoothedLfo.StepUnipolar: single;
begin
  result := SmoothingFilter.Step(inherited StepUnipolar);
end;

end.
