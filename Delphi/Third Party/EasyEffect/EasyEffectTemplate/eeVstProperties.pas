{
  VST GUI's need a way to get and set a variety of values from the VST audio core.
  For example, sample file names, float values, integer values, interface references.

  Vst Parameters provide one way to get and set values that can be translated to the
  0..1 float range. Vst Parameters aren't appropiate for all use cases.

  "Vst Properties" provide an alternative abstracted way for the GUI to communicate
  with the VST audio core.

  Abstraction is good because the GUI doesn't need to know about the internal workings
  of the audio classes.


  NOTE: This isn't currently being used.
  I think this should be morphed into some sort of boardcast system when
  object can register as listeners, and objects can boardcast messages.

}


unit eeVstProperties;

interface

uses
  Generics.Collections;

type
  PVstPropertyValue = ^TVstPropertyValue;
  TVstPropertyValue = record
    AsFloat   : single;
    AsText    : string;
    AsPointer : Pointer;
  end;

  TVstPropertyHandler = procedure(const PropertyID:string; const Data:PVstPropertyValue) of object;

  TVstPropertyNode = class
  public
    NodeID : string;
    GetVstPropertyHandler : TVstPropertyHandler;
    SetVstPropertyHandler : TVstPropertyHandler;
  end;

  TCustomVstPropertyNodeList = TObjectList<TVstPropertyNode>;

  TVstPropertyNodeList = class(TCustomVstPropertyNodeList)
  private
  public
    function FindNodeByID(const aNodeID : string):TVstPropertyNode;
  end;



  // TVstPropertyControl maps calls to Get/Set property to the correct handler.
  TVstPropertyController = class
  private
  protected
    NodeList : TVstPropertyNodeList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(const NodeID:string; const GetPropertyHandler, SetPropertyHandler : TVstPropertyHandler);
    procedure RemoveListener(const NodeID:string);

    procedure SetProperty(const NodeID, PropertyID:string; const Data:PVstPropertyValue);
    procedure GetProperty(const NodeID, PropertyID:string; const Data:PVstPropertyValue);
  end;

implementation

{ TVstPropertyController }

constructor TVstPropertyController.Create;
begin
  NodeList := TVstPropertyNodeList.Create;
  NodeList.OwnsObjects := true;
end;

destructor TVstPropertyController.Destroy;
begin
  NodeList.Free;
  inherited;
end;

procedure TVstPropertyController.AddListener(const NodeID:string; const GetPropertyHandler, SetPropertyHandler : TVstPropertyHandler);
var
  aNode : TVstPropertyNode;
begin
  aNode := NodeList.FindNodeByID(NodeID);
  if assigned(aNode) then
  begin
    aNode.GetVstPropertyHandler := GetPropertyHandler;
    aNode.SetVstPropertyHandler := SetPropertyHandler;
  end else
  begin
    aNode := TVstPropertyNode.Create;
    aNode.NodeID := NodeID;
    aNode.GetVstPropertyHandler := GetPropertyHandler;
    aNode.SetVstPropertyHandler := SetPropertyHandler;

    NodeList.Add(aNode);
  end;
end;

procedure TVstPropertyController.RemoveListener(const NodeID: string);
var
  aNode : TVstPropertyNode;
begin
  aNode := NodeList.FindNodeByID(NodeID);
  if assigned(aNode) then
  begin
    NodeList.Remove(aNode);
  end;
end;

procedure TVstPropertyController.SetProperty(const NodeID, PropertyID: string; const Data: PVstPropertyValue);
var
  aNode : TVstPropertyNode;
begin
  aNode := NodeList.FindNodeByID(NodeID);
  if assigned(aNode) then
  begin
    aNode.SetVstPropertyHandler(PropertyID, Data);
  end;
end;

procedure TVstPropertyController.GetProperty(const NodeID, PropertyID: string; const Data: PVstPropertyValue);
var
  aNode : TVstPropertyNode;
begin
  aNode := NodeList.FindNodeByID(NodeID);
  if assigned(aNode) then
  begin
    aNode.GetVstPropertyHandler(PropertyID, Data);
  end;
end;


{ TVstPropertyNodeList }

function TVstPropertyNodeList.FindNodeByID(const aNodeID: string): TVstPropertyNode;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if Items[c1].NodeID = aNodeID then
    begin
      exit(Items[c1]);
    end;
  end;

  // If we've made it this far, no matching node has been found.
  result := nil;
end;

end.
