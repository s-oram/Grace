unit Lucidity.SequencerDataObject;

interface

uses
  uConstants,
  VamLib.ZeroObject;

type
  IVectorSequenceDataObject = interface
    ['{2CB04233-08C0-425A-B122-C8D476A5D50F}']
    function GetStepValue(Index : integer):single;
    procedure SetStepValue(Index : integer; const Value:single);
  end;

  TSequencerDataObject = class(TZeroObject)
  private
    function GetStepValue(Index: integer): single;
    procedure SetStepValue(Index: integer; const Value: single);
  protected
    fStepValues : array[0..kMaxStepSequencerLength-1] of single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Source : TSequencerDataObject);

    property StepValue[Index : integer]:single read GetStepValue write SetStepValue;
  end;

implementation



{ TSequencerDataObject }

constructor TSequencerDataObject.Create;
begin

end;

destructor TSequencerDataObject.Destroy;
begin

  inherited;
end;

procedure TSequencerDataObject.AssignFrom(Source: TSequencerDataObject);
var
  c1 : integer;
begin
  for c1 := 0 to kMaxStepSequencerLength-1 do
  begin
    self.fStepValues[c1] := source.fStepValues[c1];
  end;
end;

function TSequencerDataObject.GetStepValue(Index: integer): single;
begin
  result := fStepValues[Index];
end;

procedure TSequencerDataObject.SetStepValue(Index: integer; const Value: single);
begin
  fStepValues[Index] := Value;
end;



end.
