unit SoundElement.RealTime.DcBlocker;

interface

uses
  VamLib.MoreTypes;

type
  //http://www.embedded.com/design/configurable-systems/4007653/DSP-Tricks-DC-Removal

  TDcBlocker = class
  private
    OldL : double;
    OldR : double;
    Coeff : double;
    FSampleRate: single;
    procedure SetSampleRate(const Value: single);
  public
    constructor Create;
    destructor Destroy; override;

    procedure StepReplace(var In1, In2:single); inline;

    property SampleRate : single read FSampleRate write SetSampleRate;
  end;

implementation

{ TDcBlocker }

constructor TDcBlocker.Create;
begin
  OldL := 0;
  OldR := 0;
end;

destructor TDcBlocker.Destroy;
begin

  inherited;
end;

procedure TDcBlocker.SetSampleRate(const Value: single);
begin
  FSampleRate := Value;
  //Coeff := 1 - (1/Value)
  Coeff := (1/Value);
end;

procedure TDcBlocker.StepReplace(var In1, In2: single);
begin
  In1 := In1 - OldL;
  OldL := OldL + In1 * Coeff;

  In2 := In2 - OldR;
  OldR := OldR + In2 * Coeff;
end;

end.
