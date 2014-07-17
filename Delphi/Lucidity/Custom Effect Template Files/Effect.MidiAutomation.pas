unit Effect.MidiAutomation;

interface

uses
  eeTypes,
  eeMidiAutomationV2,
  NativeXML,
  Lucidity.Interfaces;

type
  IMidiBinding = interface(ICustomMidiBinding)
    ['{10775DF2-C58D-4EC6-A687-7DA1BB4C8CE9}']
    function GetParName : string;
    procedure SetParName(const Value : string);
    function GetParID : TPluginParameterID;
    procedure SetParID(const Value : TPluginParameterID);
  end;

  TMidiBinding = class(TCustomMidiBinding, IMidiBinding)
  private
    fParName: string;
    fParID: TPluginParameterID;
    function GetParName : string;
    procedure SetParName(const Value : string);
    function GetParID : TPluginParameterID;
    procedure SetParID(const Value : TPluginParameterID);
  public
    property ParName : string read fParName write fParName;
    property ParId   : TPluginParameterID read fParID write fParID;
  end;

  TMidiAutomation = class(TCustomMidiAutomation)
  private
    function GetBinding(Index: integer): IMidiBinding;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;

    property Binding[Index : integer]:IMidiBinding read GetBinding;

    // TODO: write method to save/load automation to a XML node.
    procedure ReadStateFromXML(var XML : TXmlNode);
    procedure WriteStateToXML(var XML : TXmlNode);

    function FindBinding(const ParName : string):IMidiBinding;
    procedure ClearBinding(const ParName : string);
  end;

implementation

uses
  VamLib.Utils;

{ TMidiBinding }

function TMidiBinding.GetParID: TPluginParameterID;
begin
  result := fParId;
end;

function TMidiBinding.GetParName: string;
begin
  result := fParName;
end;

procedure TMidiBinding.SetParID(const Value: TPluginParameterID);
begin
  fParId := Value;
end;

procedure TMidiBinding.SetParName(const Value: string);
begin
  fParName := Value;
end;

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

function TMidiAutomation.GetBinding(Index: integer): IMidiBinding;
begin
  result := BindingList[Index] as IMidiBinding;
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


procedure TMidiAutomation.WriteStateToXML(var XML: TXmlNode);
var
  c1: Integer;
  ChildNode : TXmlNode;
  mb : IMidiBinding;
begin
  for c1 := 0 to BindingList.Count-1 do
  begin
    mb := (BindingList[c1] as IMidiBinding);

    ChildNode := xml.NodeNew('Binding');
    ChildNode.NodeNew('ParName').ValueUnicode := mb.GetParName;
    ChildNode.NodeNew('MidiCC').ValueUnicode := DataIO_IntToStr(mb.GetMidiCC);
  end;
end;

procedure TMidiAutomation.ReadStateFromXML(var XML: TXmlNode);
var
  NodeList : TsdNodeList;
  c1: Integer;
  ParName : string;
  MidiCC  : integer;
  DataNode : TXmlNode;
  mb : IMidiBinding;
begin
  Clear;

  NodeList := TsdNodeList.Create;
  AutoFree(@NodeList);

  XML.FindNodes('Binding', NodeList);

  for c1 := 0 to NodeList.Count-1 do
  begin
    DataNode := NodeList[c1].FindNode('ParName');
    if assigned(DataNode)
      then ParName := DataNode.ValueUnicode
      else ParName := '';



    DataNode := NodeList[c1].FindNode('MidiCC');
    if assigned(DataNode)
      then MidiCC := DataIO_StrToInt(DataNode.ValueUnicode, 1)
      else MidiCC := -1;

    if (ParName <> '')  and (MidiCC >= 0) and (MidiCC <= 127) then
    begin
      mb := TMidiBinding.Create;
      mb.SetParName(ParName);
      mb.SetMidiCC(MidiCC);
      BindingList.Add(mb);
    end;
  end;

end;




end.
