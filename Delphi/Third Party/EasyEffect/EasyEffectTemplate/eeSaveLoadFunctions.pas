unit eeSaveLoadFunctions;

interface

uses
  VamLib.MoreTypes,
  NativeXml;

{
procedure SaveMidiMapToXML(const MidiAutomation:TMidiAutomation; var XML:TNativeXML);
procedure LoadMidiMapFromXML(var MidiAutomation:TMidiAutomation; const XML:TNativeXML);
}

procedure SaveObjectToXML(const obj:TObject; var XmlNode:TXmlNode);
procedure LoadObjectFromXML(obj:TObject; const XmlNode:TXmlNode);

procedure SaveObjectPropertyToXML(const XmlNode : TXmlNode; const Obj : TObject; const PropertyName : string);
procedure LoadObjectPropertyFromXML(const XmlNode : TXmlNode; const Obj : TObject; const PropertyName : string);

function ReadNodeValue(const ParentXmlNode : TXmlNode; const ChildNodeName : string; out Value : string):boolean;

implementation

uses
  eeLogging,
  TypInfo, uAutoFree, SysUtils, eeMidiMap, eeFunctions;

  {
procedure SaveMidiMapToXML(const MidiAutomation:TMidiAutomation; var XML:TNativeXML);
var
  MidiMapNode:TXmlNode;
  BindingNode:TXmlNode;
  c1: Integer;
  Name, Value: UTF8String;
begin
  MidiMapNode := XML.Root.NodeNew('MIDIMap');
  try
    for c1 := 0 to MidiAutomation.MidiMap.Count - 1 do
    begin
      BindingNode := MidiMapNode.NodeNew('Binding');
      try
        Name  := 'ParameterName';
        Value := UTF8String(MidiAutomation.MidiMap.Binding[c1]^.ParameterName);
        BindingNode.AttributeAdd(Name, Value);

        Name  := 'ParameterID';
        Value := UTF8String(IntToStr(MidiAutomation.MidiMap.Binding[c1]^.ParameterId));
        BindingNode.AttributeAdd(Name, Value);

        Name  := 'CC';
        Value := UTF8String(IntToStr(MidiAutomation.MidiMap.Binding[c1]^.MidiCC));
        BindingNode.AttributeAdd(Name, Value);

        Name  := 'ParameterType';
        case MidiAutomation.MidiMap.Binding[c1]^.TargetType of
          ttVstParameter:     Value := 'VSTParameter';
          ttPrivateParameter: Value := 'PrivateParameter';
        else
          Value := '';
        end;
        BindingNode.AttributeAdd(Name, Value);
      finally
        //BindingNode.Free;
      end;
    end;
  finally
    //MidiMapNode.Free;
  end;
end;
}

{
procedure LoadMidiMapFromXML(var MidiAutomation:TMidiAutomation; const XML:TNativeXML);
var
  MidiMapNode:TXmlNode;
  BindingNodes:TsdNodeList;
  c1: Integer;
  Name : UTF8String;
  Value : UTF8String;
  amb:TMidiBinding;
begin
  MidiMapNode := xml.Root.FindNode('MIDIMap');

  if assigned(MidiMapNode) then
  begin
    //NOTE: Only clear the midi map if midi binding information exists in the XML.
    MidiAutomation.MidiMap.Clear;


    BindingNodes := TsdNodeList.Create;
    AutoFree(@BindingNodes);


    MidiMapNode.FindNodes('Binding', BindingNodes);

    for c1 := 0 to BindingNodes.Count - 1 do
    begin
      amb.ParameterName := '';
      amb.ParameterId   := -1;
      amb.MidiCC        := 0;
      amb.TargetType    := ttVstParameter;

      Name  := 'ParameterName';
      if BindingNodes[c1].HasAttribute(Name) then
      begin
        Value := BindingNodes[c1].AttributeValueByName[Name];
        amb.ParameterName := String(Value);
      end;

      Name  := 'ParameterID';
      if BindingNodes[c1].HasAttribute(Name) then
      begin
        Value := BindingNodes[c1].AttributeValueByName[Name];
        amb.ParameterID := StrToInt_Safe(string(Value), -1);
      end;

      Name  := 'CC';
      if BindingNodes[c1].HasAttribute(Name) then
      begin
        Value := BindingNodes[c1].AttributeValueByName[Name];
        amb.MidiCC := StrToInt_Safe(string(Value), 0);
      end;

      Name  := 'ParameterType';
      if BindingNodes[c1].HasAttribute(UTF8String(Name)) then
      begin
        if Value = 'VSTParameter'     then amb.TargetType := ttVstParameter;
        if Value = 'PrivateParameter' then amb.TargetType := ttPrivateParameter;
      end;

      MidiAutomation.MidiMap.AddBinding(amb.MidiCC, amb.ParameterId, amb.ParameterName, amb.TargetType);
    end;

  end;

end;
}

procedure SaveObjectToXML(const obj:TObject; var XmlNode:TXmlNode);
var
  PropList  : PPropList;
  PropCount : integer;
  c1: Integer;
  Name  : string;
  Value : string;
  Namef8  : UTF8String;
  Valuef8 : UTF8String;
  tx    : single;
begin
   PropCount := GetPropList(obj, PropList);
   for c1 := 0 to PropCount - 1 do
   begin
     Name := String(PropList^[c1]^.Name);

     case PropList^[c1]^.PropType^^.Kind of
       tkFloat:
       begin
         tx := GetFloatProp(obj, Name);
         Value := DataIO_FloatToStr(tx);
       end;

       tkInteger:
       begin
         Value := GetPropValue(obj, Name, true);
       end
     else
       Value := '';
     end;

     //Create new node and assign value.
     Namef8  := UTF8String(Name);
     Valuef8 := UTF8String(Value);
     XmlNode.NodeNew(Namef8).ValueUnicode := String(Valuef8);

   end;
end;


procedure LoadObjectFromXML(obj:TObject; const XmlNode:TXmlNode);
var
  Name, Value : string;
  PropInfo : PPropInfo;
  c1 : integer;
begin
  for c1 := 0 to XmlNode.NodeCount-1 do
  begin
    Name  := string(XmlNode[c1].Name);
    Value := string(XmlNode[c1].ValueUnicode);

    PropInfo := GetPropInfo(Obj, Name);

    if (PropInfo <> nil) and (Value <> '') then
    begin
      case PropInfo^.PropType^^.Kind of

        tkFloat:
        begin
           SetFloatProp(obj, Name, DataIO_StrToFloat(Value, 0));
        end;

        tkInteger:
        begin
          SetPropValue(obj, Name, Value);
        end
      end;
    end;
  end;
end;


procedure SaveObjectPropertyToXML(const XmlNode : TXmlNode; const Obj : TObject; const PropertyName : string);
var
  Value : string;
  PropInfo : PPropInfo;
  tx : single;
  ti : integer;
begin
  PropInfo := GetPropInfo(Obj, PropertyName);
  if PropInfo = nil then raise Exception.Create('Property not found.');

  case PropInfo^.PropType^^.Kind of
    tkEnumeration:
    begin
      Value := GetEnumProp(obj, PropertyName);
    end;

    tkFloat:
    begin
      tx := GetFloatProp(obj, PropertyName);
      Value := DataIO_FloatToStr(tx);
    end;

    tkInteger:
    begin
      ti := GetOrdProp(obj, PropertyName);
      Value := DataIO_IntToStr(ti);
    end;

    tkUString, tkString:
    begin
      Value := GetStrProp(obj, PropertyName);
    end;

  else
    raise Exception.Create('Property type not supported.');
  end;


  XmlNode.NodeNew(UTF8String(PropertyName)).ValueUnicode := Value;
end;

procedure LoadObjectPropertyFromXML(const XmlNode : TXmlNode; const Obj : TObject; const PropertyName : string);
var
  aNode : TXmlNode;
  Value : string;
  PropInfo : PPropInfo;
  tx : single;
  ti : integer;
begin
  PropInfo := GetPropInfo(Obj, PropertyName);
  if PropInfo = nil then
    raise Exception.Create('Object Property not found. (Property Name = "' + PropertyName + '")');

  aNode := XmlNode.FindNode(UTF8String(PropertyName));

  if not assigned(aNode) then exit; //====== node with property name not found so exit. ====>>


  Value := aNode.ValueUnicode;


  case PropInfo^.PropType^^.Kind of
    tkEnumeration:
    begin
      try
        SetEnumProp(Obj, PropertyName, Value);
      except
        on E: EPropertyConvertError do LogException(E);
        else raise;
      end;
    end;

    tkFloat:
    begin
      // TODO: Here we should only set the property
      // value if the string is a valid float value.
      // If not, log it as an error.
      tx := DataIO_StrToFloat(Value, 0);
      SetFloatProp(Obj, PropertyName, tx);
    end;

    tkInteger:
    begin
      // TODO: Here we should only set the property
      // value if the string is a valid integer value.
      // If not, log it as an error.
      ti := DataIO_StrToInt(Value, 0);
      SetOrdProp(Obj, PropertyName, ti);
    end;

    tkUString, tkString:
    begin
      SetStrProp(Obj, PropertyName, Value);
    end;

  else
    raise Exception.Create('Property type not supported.');
  end;

end;


function ReadNodeValue(const ParentXmlNode : TXmlNode; const ChildNodeName : string; out Value : string):boolean;
var
  childNode : TXmlNode;
begin
  ChildNode := ParentXmlNode.FindNode(UTF8String(ChildNodeName));

  if assigned(ChildNode) then
  begin
    Value := ChildNode.ValueUnicode;
    result := true;
  end else
  begin
    Value := '';
    result := false;
  end;
end;

end.




