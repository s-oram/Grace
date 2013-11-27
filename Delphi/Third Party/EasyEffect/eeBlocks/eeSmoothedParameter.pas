unit eeSmoothedParameter;

interface

type
  TSmoothedParameter = class
  private
    fParValueSmoothed: single;
    fParValue: single;
    fSmoothingCoefficient: single;
  protected
    TargetValue:single;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure SetValue(Value:single; Smooth:boolean = true);

    function Step:single; inline;

    property ParValue             :single  read fParValue;
    property ParValueSmoothed     :single  read fParValueSmoothed;
    property SmoothingCoefficient :single  read fSmoothingCoefficient write fSmoothingCoefficient;
  end;

implementation

{ TSmoothedParameter }

constructor TSmoothedParameter.Create;
begin
  SmoothingCoefficient := 20 / 44100;
  fParValue            := 0;
  fParValueSmoothed    := 0;
  TargetValue          := 0;
end;

destructor TSmoothedParameter.Destroy;
begin
  inherited;
end;

procedure TSmoothedParameter.SetValue(Value: single; Smooth: boolean);
begin
  if Smooth = true then
  begin
    fParValue    := Value;
    TargetValue  := Value;
  end else
  begin
    fParValue         := Value;
    fParValueSmoothed := Value;
    TargetValue       := Value;
  end;
end;

function TSmoothedParameter.Step: single;
begin
  fParValueSmoothed := fParValueSmoothed + (TargetValue - fParValueSmoothed) * SmoothingCoefficient;
  result := fParValueSmoothed;
end;

end.
