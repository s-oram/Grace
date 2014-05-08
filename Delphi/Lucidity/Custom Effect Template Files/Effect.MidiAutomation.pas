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


    // TODO: write method to save/load automation to a XML node.

    // TODO: write method to get MIDI learn CC for a parameter
    function FindBinding(const ParName : string):IMidiBinding;
    procedure ClearBinding(const ParName : string);


    // TODO: write method to clear parameter for a MIDI cc.
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

function TMidiAutomation.FindBinding(const ParName: string): IMidiBinding;
var
  c1 : integer;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    if (BindingList[c1] as IMidiBinding).GetParName = ParName
      then exit(BindingList[c1] as IMidiBinding);
  end;

  // no match found
  result := nil;
end;

procedure TMidiAutomation.ClearBinding(const ParName: string);
var
  c1 : integer;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    if (BindingList[c1] as IMidiBinding).GetParName = ParName then
    begin
      BindingList.Delete(c1);
      //TODO: possible need to make this thread-safe - maybe by executing in
      // the audio thread actions...
    end;
  end;
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
