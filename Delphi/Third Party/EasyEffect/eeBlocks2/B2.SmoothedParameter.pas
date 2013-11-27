unit B2.SmoothedParameter;

interface


type
  // NOTE: TSmoothParameter uses a lowpass RC filter / leaky integer to smooth
  // value changes.
  TSmoothedParameter=class
  private
    fTargetValue: double;
    fCurrentValue: double;
    fSmoothingTime: double;
    fSampleRate: double;
    procedure SetSmoothingTime(const Value: double);
    procedure SetSampleRate(const Value: double);
  protected
    SmoothingCoefficient : double;
    procedure UpdateFilterCoefficient; inline;
  public
    constructor Create;

    procedure Step; inline;
    procedure Reset(Value : double = 0); inline;

    property CurrentValue : double read fCurrentValue;
    property TargetValue  : double read fTargetValue write fTargetValue;

    property SmoothingTime : double read fSmoothingTime write SetSmoothingTime; //in milliseconds.
    property SampleRate    : double read fSampleRate    write SetSampleRate;
  end;


implementation

uses
  SysUtils;

{ TSmoothedParameter }

constructor TSmoothedParameter.Create;
begin
  fSmoothingTime := 10;
  fSampleRate    := 44100;
  fCurrentValue  := 0;
  fTargetValue   := 0;
end;

procedure TSmoothedParameter.Reset(Value: double);
begin
  fSmoothingTime := 10;
  fSampleRate    := 44100;
end;

procedure TSmoothedParameter.SetSampleRate(const Value: double);
begin
  fSampleRate := Value;
  UpdateFilterCoefficient;
end;

procedure TSmoothedParameter.SetSmoothingTime(const Value: double);
begin
  fSmoothingTime := Value;
  UpdateFilterCoefficient;
end;

procedure TSmoothedParameter.Step;
begin
  fCurrentValue := TargetValue + (fCurrentValue - TargetValue) * SmoothingCoefficient;
end;

procedure TSmoothedParameter.UpdateFilterCoefficient;
begin
  // RC Filter coefficients.
  // http://www.kvraudio.com/forum/viewtopic.php?t=300689&highlight=filter+envelope
  SmoothingCoefficient := exp(-1 / (SmoothingTime * 0.001 * SampleRate));
  if SmoothingCoefficient > 1 then raise Exception.Create('SmoothingCoefficient is bigger then 1.');
end;

end.
