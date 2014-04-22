unit eeMidiMap;

interface

uses
  eeIniFile, Classes;

type
  // TTargetType allows different types of parameters to be automated.
  //
  // ttVSTParameter automates public vst parameters. ie those accessed by
  // GetParameter and SetParameterAutomated. ParameterId should be equal to
  // ParameterIndex in this case to allow automatic automation.
  //
  // ttPrivateParameter allows for parameters that aren't exposed to the host
  // to be automated.

  TTargetType = (ttVstParameter, ttPrivateParameter);

  PMidiBinding = ^TMidiBinding;
  TMidiBinding = record
    MidiCC:byte;
    ParameterId:integer;   //should corrospond to ParameterIndex for public VST parameters
    ParameterName:string;  //Name of target parameter. Should be unique. Human readable.
    TargetType:TTargetType;
  end;


  TMidiMap = class
  private
    function GetCount: integer;
    function GetBinding(Index: integer): PMidiBinding;
  protected
    BindingList:TList;
  public
    constructor Create;
   	destructor Destroy; override;

    function AddBinding(MidiCC:byte; ParameterID:integer; ParameterName:string; TargetType:TTargetType):integer;
    procedure DeleteBinding(Index:integer);

    function FindByMidiCC(MidiCC:byte):integer;
    function FindByParameterID(ParameterID:integer; TargetType:TTargetType):integer;

    procedure Clear;

    procedure Save(Filename:string);
    procedure Load(Filename:string);

    procedure SaveToXmlFile(Filename:string);
    procedure LoadFromXmlFile(Filename:string);

    property Binding[Index:integer]:PMidiBinding read GetBinding;
    property Count:integer read GetCount;

  end;

implementation

uses
  SysUtils, uAutoFree, NativeXML, eeFunctions;



{$IFDEF VER230}
function StrToInt_Safe(Text:UTF8String; FallBack:integer):integer; inline;
begin
  try
    result := StrToInt(String(Text));
  except
    result := FallBack;
  end;
end;

function DataIO_StrToInt(Value:UTF8String; FallbackValue:integer):integer;
begin
  try
    result := StrToInt(String(Value))
  except
    result := FallBackValue;
  end;
end;
{$ENDIF}



{ TMidiMap }

constructor TMidiMap.Create;
begin
  BindingList := TList.Create;
end;

destructor TMidiMap.Destroy;
begin
  Clear;
  BindingList.Free;
  inherited;
end;

function TMidiMap.GetCount: integer;
begin
  result := BindingList.Count;
end;

function TMidiMap.AddBinding(MidiCC: byte; ParameterID: integer; ParameterName: string; TargetType: TTargetType): integer;
var
  amb:PMidiBinding;
begin
  New(amb);
  amb^.MidiCC := MidiCC;
  amb^.ParameterId := ParameterId;
  amb^.ParameterName := ParameterName;
  amb^.TargetType := TargetType;

  result := BindingList.Add(amb);
end;

procedure TMidiMap.DeleteBinding(Index: integer);
var
  amb:PMidiBinding;
begin
  amb := BindingList[Index];
  Dispose(amb);
  BindingList.Delete(Index);
end;

procedure TMidiMap.Clear;
var
  c1:integer;
begin
  for c1 := Count-1 downto 0 do
  begin
    DeleteBinding(c1);
  end;
end;

function TMidiMap.GetBinding(Index: integer): PMidiBinding;
begin
  result := PMidiBinding(BindingList[index]);
end;

function TMidiMap.FindByMidiCC(MidiCC: byte): integer;
var
  c1: Integer;
  amb:PMidiBinding;
begin
  for c1 := 0 to Count - 1 do
  begin
    amb := Binding[c1];
    if amb^.MidiCC = MidiCC then
    begin
      result := c1;
      exit; //====================================================================>
    end;
  end;

  result := -1;

end;


function TMidiMap.FindByParameterID(ParameterID: integer; TargetType: TTargetType): integer;
var
  c1: Integer;
  amb:PMidiBinding;
begin
  for c1 := 0 to Count - 1 do
  begin
    amb := Binding[c1];
    if (amb^.ParameterId = ParameterID) and (amb^.TargetType = TargetType) then
    begin
      result := c1;
      exit; //====================================================================>
    end;
  end;

  result := -1;

end;

procedure TMidiMap.Save(Filename: string);
var
  c1:integer;
  Root:string;
  Section,Ident:string;
  Value:string;
  Ini:TIniFile;
begin
  Ini := TIniFile.Create;
  AutoFree(@Ini);

  Section := 'Midi_Bindings';

  Ident := 'BindingCount';
  Value := IntToStr(Count);
  Ini.WriteString(Section, Ident, Value);

  for c1 := 0 to Count - 1 do
  begin
    Root := 'Binding' + IntToStr(c1) + '_';

    Ident := Root + 'ParameterName';
    Value := Binding[c1]^.ParameterName;
    Ini.WriteString(Section, Ident, Value);

    Ident := Root + 'ParameterID';
    Value := IntToStr(Binding[c1]^.ParameterId);
    Ini.WriteString(Section, Ident, Value);

    Ident := Root + 'MidiCC';
    Value := IntToStr(Binding[c1]^.MidiCC);
    Ini.WriteString(Section, Ident, Value);

    Ident := Root + 'AutomationType';
    case Binding[c1]^.TargetType of
      ttVstParameter:     Value := '0';
      ttPrivateParameter: Value := '1';
    else
      Value := '0';
    end;
    Ini.WriteString(Section, Ident, Value);
  end;

  Ini.SaveToFile(FileName);
end;




procedure TMidiMap.SaveToXmlFile(Filename: string);
var
  c1    : integer;
  Name  : UTF8String;
  Value : UTF8String;
  xml   : TNativeXML;
  Node  : TXmlNode;
begin
  xml := TNativeXML.CreateName('root');
  AutoFree(@xml);

  Name := 'FileType';
  Value := 'MidiMap';
  xml.Root.AttributeAdd(Name, Value);

  for c1 := 0 to Count - 1 do
  begin
    Node := xml.Root.NodeNew('MidiBinding');

    Name := 'ParameterName';
    Value := UTF8String(Binding[c1]^.ParameterName);
    Node.AttributeAdd(Name, Value);


    Name := 'ParameterID';
    Value := IntToUTF8Str(Binding[c1]^.ParameterId);
    Node.AttributeAdd(Name, Value);


    Name := 'MidiCC';
    Value := IntToUTF8Str(Binding[c1]^.MidiCC);
    Node.AttributeAdd(Name, Value);

    Name := 'AutomationType';
    case Binding[c1]^.TargetType of
      ttVstParameter:     Value := '0';
      ttPrivateParameter: Value := '1';
    else
      Value := '0';
    end;
    Node.AttributeAdd(Name, Value);
  end;

  xml.XmlFormat := xfReadable;
  xml.SaveToFile(FileName);
end;

procedure TMidiMap.Load(Filename: string);
var
  c1:integer;
  Root:string;
  Section,Ident:string;
  Value:string;
  BindingsToLoad:integer;
  amb:TMidiBinding;
  BindingIsValid:boolean;
  Ini:TIniFile;
begin
  Ini := TIniFile.Create;
  AutoFree(@Ini);

  Ini.LoadFromFile(FileName);

  Section := 'Midi_Bindings';

  Ident := 'BindingCount';

  if Ini.Exists(Section, Ident)
    then Value := Ini.ReadString(Section, Ident)
    else Value := '0';

  BindingsToLoad := StrToInt(Value);

  for c1 := 0 to BindingsToLoad - 1 do
  begin
    Root := 'Binding' + IntToStr(c1) + '_';

    BindingIsValid := true;

    Ident := Root + 'ParameterName';
    if Ini.Exists(Section, Ident) then
    begin
      Value := Ini.ReadString(Section, Ident);
      amb.ParameterName := Value;
    end else
    begin
      BindingIsValid := false;
    end;


    Ident := Root + 'ParameterID';
    if Ini.Exists(Section, Ident) then
    begin
      Value := Ini.ReadString(Section, Ident);
      amb.ParameterId := StrToInt(Value);
    end else
    begin
      BindingIsValid := false;
    end;


    Ident := Root + 'MidiCC';
    if Ini.Exists(Section, Ident) then
    begin
      Value := Ini.ReadString(Section, Ident);
      amb.MidiCC := StrToInt(Value);
    end else
    begin
      BindingIsValid := false;
    end;


    Ident := Root + 'AutomationType';
    if Ini.Exists(Section, Ident) then
    begin
      Value := Ini.ReadString(Section, Ident);

      if Value = '0' then amb.TargetType := ttVstParameter;
      if Value = '1' then amb.TargetType := ttPrivateParameter;
    end else
    begin
      BindingIsValid := false;
    end;

    if BindingIsValid then
    begin
      AddBinding(amb.MidiCC, amb.ParameterId, amb.ParameterName, amb.TargetType);
    end;

  end;

end;

procedure TMidiMap.LoadFromXmlFile(Filename: string);
var
  xml:TNativeXml;
  Nodes:TsdNodeList;
  Node:TXmlNode;
  Name, Value: UTF8String;
  c1: Integer;
  BindingIsValid:boolean;
  amb:TMidiBinding;
begin
  xml := TNativeXml.Create(nil);
  AutoFree(@xml);

  Nodes := TsdNodeList.Create;
  AutoFree(@Nodes);

  xml.LoadFromFile(FileName);

  if not assigned(xml.Root) then exit;

  Name := 'FileType';
  Value := 'MidiMap';

  if xml.Root.HasAttribute(Name) = false then raise Exception.Create('File can not be loaded. (No file type attribute exists.)');
  //if xml.Root.AttributeByName[Name] <> Value then raise Exception.Create('File can not be loaded. (File is not correct format.)');
  if xml.Root.AttributeValueByName[Name] <> Value then raise Exception.Create('File can not be loaded. (File is not correct format.)');


  xml.Root.FindNodes('MidiBinding', Nodes);

  for c1 := 0 to Nodes.Count - 1 do
  begin
    Node := Nodes[c1];


    BindingIsValid := true;

    Name := 'ParameterName';
    if Node.HasAttribute(Name) then
    begin
      Value := Node.AttributeValueByName[Name];
      amb.ParameterName := String(Value);
    end else
    begin
      BindingIsValid := false;
    end;


    Name := 'ParameterID';
    if Node.HasAttribute(Name) then
    begin
      Value := Node.AttributeValueByName[Name];
      amb.ParameterId := DataIO_StrToInt(Value, -1);
    end else
    begin
      BindingIsValid := false;
    end;


    Name := 'MidiCC';
    if Node.HasAttribute(Name) then
    begin
      Value := Node.AttributeValueByName[Name];
      amb.MidiCC := DataIO_StrToInt(Value, -1);
    end else
    begin
      BindingIsValid := false;
    end;


    Name := 'AutomationType';
    if Node.HasAttribute(Name) then
    begin
      Value := Node.AttributeValueByName[Name];

      case DataIO_StrToInt(Value, -1) of
      0: amb.TargetType := ttVstParameter;
      1: amb.TargetType := ttPrivateParameter;
      else
        amb.TargetType := ttVstParameter;
      end;
    end else
    begin
      BindingIsValid := false;
    end;

    if BindingIsValid then
    begin
      AddBinding(amb.MidiCC, amb.ParameterId, amb.ParameterName, amb.TargetType);
    end;
  end;


end;

end.
