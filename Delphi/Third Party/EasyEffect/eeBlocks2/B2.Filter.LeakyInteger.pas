{
  A leaky integer filter is equlivient to a RC filter.
}

unit B2.Filter.LeakyInteger;

interface

type
  TLeakyInteger = class
  private
    fCoefficient: single;
    OldValue : double;
  public
    constructor Create; virtual;

    function Step(const Value:double):double; inline;
    procedure Reset(const Value:double = 0);

    // RC Filter coefficients.
    // http://www.kvraudio.com/forum/viewtopic.php?t=300689&highlight=filter+envelope
    //
    // To set decay length in seconds use:
    //   coeff = exp(-1 / (tau * fs))
    //
    property Coefficient : single read fCoefficient write fCoefficient;

    procedure SetCoefficientAsDecayTime(const DecayTimeMilliseconds, SampleRate : single);




  end;

implementation



{ TLeakyInteger }

constructor TLeakyInteger.Create;
begin
  Reset(0);
end;

procedure TLeakyInteger.Reset(const Value: double);
begin
  OldValue := Value;
end;

procedure TLeakyInteger.SetCoefficientAsDecayTime(const DecayTimeMilliseconds, SampleRate: single);
begin
  Coefficient := exp(-1 / (DecayTimeMilliseconds * 0.001 * SampleRate));
end;

function TLeakyInteger.Step(const Value: double): double;
begin
  OldValue := Value + (OldValue - Value) * Coefficient;
  result := OldValue;
end;

end.
