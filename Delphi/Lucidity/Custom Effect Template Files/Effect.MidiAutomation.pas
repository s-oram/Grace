unit Effect.MidiAutomation;

interface

uses

  Lucidity.Interfaces,
  eeMidiAutomationV2;

type

  IMidiBinding = interface(ICustomMidiBinding)
    ['{10775DF2-C58D-4EC6-A687-7DA1BB4C8CE9}']
    function GetParName : string;
    procedure SetParName(const Value : string);
  end;

  TMidiBinding = class(TCustomMidiBinding, IMidiBinding)
  private
    fParName: string;
    function GetParName : string;
    procedure SetParName(const Value : string);
  public
    property ParName : string read fParName write fParName;
  end;

  TMidiAutomation = class(TCustomMidiAutomation)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation


{ TMidiAutomation }

constructor TMidiAutomation.Create;
begin
  inherited;

end;

destructor TMidiAutomation.Destroy;
begin

  inherited;
end;

{ TMidiBinding }

function TMidiBinding.GetParName: string;
begin
  result := fParName;
end;

procedure TMidiBinding.SetParName(const Value: string);
begin
  fParName := Value;
end;

end.
