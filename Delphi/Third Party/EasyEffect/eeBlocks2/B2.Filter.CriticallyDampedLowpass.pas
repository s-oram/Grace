unit B2.Filter.CriticallyDampedLowpass;

interface

uses
  eeBiquadFilters, eeBiquadFilterCore;

type
  TCriticallyDampedLowpass = class
  private
  protected
    FilterCore : TBiquadFilterCore;
    Offset : double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(const Value : double = 0);

    function Step(const Input : single):single; //TODO: inline these functions?

    procedure SetCutoff(const NormalisedFreq : single); overload;
    procedure SetCutoff(const Hz, SampleRate : single); overload;
    procedure SetTransitionTime(const ms, SampleRate: single);
  end;

implementation

{ TCritcallyDampedLowpass }

constructor TCriticallyDampedLowpass.Create;
begin
  FilterCore := TBiquadFilterCore.Create;
end;

destructor TCriticallyDampedLowpass.Destroy;
begin
  FilterCore.Free;
  inherited;
end;

procedure TCriticallyDampedLowpass.Reset(const Value : double);
begin
  Offset := Value;
  FilterCore.Reset;
end;

procedure TCriticallyDampedLowpass.SetCutoff(const NormalisedFreq: single);
var
  b0, b1, b2, a1, a2 : double;
begin
  assert(NormalisedFreq >= 0);
  assert(NormalisedFreq <= 0.5);
  CalcBiquad_CriticallyDamped_LowPass(NormalisedFreq, 2, b0, b1, b2, a1, a2);

  FilterCore.B0_Coeff := b0;
  FilterCore.B1_Coeff := b1;
  FilterCore.B2_Coeff := b2;
  FilterCore.a1_Coeff := a1;
  FilterCore.a2_Coeff := a2;
end;

procedure TCriticallyDampedLowpass.SetCutoff(const Hz, SampleRate: single);
var
  b0, b1, b2, a1, a2 : double;
begin
  assert(Hz >= 0);
  assert(Hz <= SampleRate * 0.25);
  CalcBiquad_CriticallyDamped_LowPass(Hz, SampleRate, b0, b1, b2, a1, a2);

  FilterCore.B0_Coeff := b0;
  FilterCore.B1_Coeff := b1;
  FilterCore.B2_Coeff := b2;
  FilterCore.a1_Coeff := a1;
  FilterCore.a2_Coeff := a2;
end;

procedure TCriticallyDampedLowpass.SetTransitionTime(const ms, SampleRate: single);
var
  Hz : single;
  b0, b1, b2, a1, a2 : double;
begin
  //TODO: It might be possible to find some optimisations in here somewhere...
  if ms > 0
    then Hz := 1 / (ms * 0.001)
    else Hz := SampleRate * 0.25;

  if Hz > SampleRate * 0.25 then Hz := SampleRate * 0.25;

  CalcBiquad_CriticallyDamped_LowPass(Hz, SampleRate, b0, b1, b2, a1, a2);

  FilterCore.B0_Coeff := b0;
  FilterCore.B1_Coeff := b1;
  FilterCore.B2_Coeff := b2;
  FilterCore.a1_Coeff := a1;
  FilterCore.a2_Coeff := a2;
end;


function TCriticallyDampedLowpass.Step(const Input: single): single;
begin
  // TODO: DF1 is the most stable of the three options when the coefficients are
  // dynamically changed. I wonder how much of a CPU advantage the other options
  // have? It might be worth testing. If so I could use multiple step methods.

  result := FilterCore.Step_DirectForm1(Input - Offset) + Offset;
  //result := FilterCore.Step_DirectForm2(Input - Offset) + Offset;
  //result := FilterCore.Step_DirectForm2Transposed(Input - Offset) + Offset;
end;



end.
