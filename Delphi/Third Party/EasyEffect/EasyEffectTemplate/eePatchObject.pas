{
  Vst Plugins need a way to save and load their state to and from various places.
  The state data can be in various formats. Typically my plugins will save their state
  as XML but I've used other formats in the past. Sometimes my plugins will import
  'patches' from other products which can potentially be in any format. (Binary, xml, yaml etc)

  TPatchObject is an abstraction layer.
  - Plugins read and write their state to a PatchObject instance.
  - Patch loading/saving code translates between the source/destination format
    (binary, xml, yaml, etc) and the PatchObject which is used internally.

  The advantage is that patch loading/saving code doesn't need to know about the plugin
  internals and the plugin internals do not need to be able to read/write various patch
  formats.
}

unit eePatchObject;

interface

uses
  SysUtils, Classes, Contnrs, eeMultiCast;

type
  EPatchObjectException = Exception;

  // forward declarations
  TPatchObject = class;

  TPatchValue = class
  strict private
    fName: string;
    fValue : string;
  public
    constructor Create;
    destructor Destroy; override;

    property Name  : string read fName  write fName;
    property Value : string read fValue write fValue;
  end;

  TPatchNode = class
  strict private
    fValueList : TObjectList;
    fChildNodeList : TObjectList;
  private
    fModuleName: string;
    function GetPatchValue(Index: integer): TPatchValue;
    function GetPatchValueCount: integer;
    function GetChildNode(Index: integer): TPatchNode;
    function GetChildNodeCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    property NodeName : string read fModuleName write fModuleName;

    procedure AddValue(const Name:string; x:string); overload;
    procedure AddValue(const Name:string; x:integer); overload;
    procedure AddValue(const Name:string; x:single); overload;
    procedure AddValue(const Name:string; x:boolean); overload;

    function GetValue(const Name:string; Default: string =''): string; overload;
    function GetValue(const Name:string; Default: integer = 0): integer; overload;
    function GetValue(const Name:string; Default: single = 0): single; overload;
    function GetValue(const Name:string; Default: boolean = false): boolean; overload;

    function NewPatchValue(const Name:string):TPatchValue;
    function FindPatchValue(const Name:string):TPatchValue;
    function ValueExists(const Name:string):boolean;

    property PatchValue[Index:integer]:TPatchValue read GetPatchValue;
    property PatchValueCount : integer read GetPatchValueCount;

    function NewChildNode(const Name:string):TPatchNode;
    function FindChildNode(const Name:string):TPatchNode;
    function ChildNodeExists(const Name:string):boolean;

    property ChildNode[Index:integer]:TPatchNode read GetChildNode;
    property ChildNodeCount : integer read GetChildNodeCount;
  end;


  TPatchObject = class
  strict private
    fNodeList : TObjectList;
  private
    function GetNode(Index: integer): TPatchNode;
    function GetNodeCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function NewNode(const Name:string):TPatchNode;
    function FindNode(const Name:string):TPatchNode;
    function NodeExists(const Name:string):boolean;

    procedure Clear;

    property Node[Index:integer]:TPatchNode read GetNode;
    property NodeCount : integer read GetNodeCount;
  end;


  TPatchObjectEvent = procedure(Sender:TObject; PatchObject:TPatchObject) of object;

  TMultiCastPatchObjectEvent = class(TMultiCastEvent)
  public
    procedure Trigger(Sender:TObject; PatchObject:TPatchObject);
    procedure AddEventHandler(EventHandler:TPatchObjectEvent);
    procedure RemoveEventHandler(EventHandler:TPatchObjectEvent);
  end;


implementation

uses
  eeFunctions;

type
  PMethod = ^TMethod;

{ TPatchValue }

constructor TPatchValue.Create;
begin

end;

destructor TPatchValue.Destroy;
begin

  inherited;
end;



{ TPatchValueList }

constructor TPatchNode.Create;
begin
  fModuleName := '';

  fValueList := TObjectList.Create;
  fValueList.OwnsObjects := true;

  fChildNodeList := TObjectList.Create;
  fChildNodeList.OwnsObjects := true;
end;

destructor TPatchNode.Destroy;
begin
  fValueList.Free;
  fChildNodeList.Free;
  inherited;
end;

function TPatchNode.ValueExists(const Name: string): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to fValueList.count-1 do
  begin
    if (fValueList[c1] as TPatchValue).Name = Name then
    begin
      result := true;
      exit; //============>>exit>>==========>>
    end;
  end;

  // If we've made it this far, no matching value has been found.
  result := false;
end;

function TPatchNode.FindChildNode(const Name: string): TPatchNode;
var
  c1: Integer;
begin
  for c1 := 0 to fChildNodeList.Count-1 do
  begin
    if (fChildNodeList[c1] as TPatchNode).NodeName = Name then
    begin
        result := fChildNodeList[c1] as TPatchNode;
        exit; //===============>>exit>>=============>>
    end;
  end;

  // If we've made it this far, no matching module has been found.
  result := nil;
end;

function TPatchNode.ChildNodeExists(const Name: string): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to fChildNodeList.Count-1 do
  begin
    if (fChildNodeList[c1] as TPatchNode).NodeName = Name then
    begin
        result := true;
        exit; //===============>>exit>>=============>>
    end;
  end;

  // If we've made it this far, no matching module has been found.
  result := false;
end;

function TPatchNode.NewChildNode(const Name: string): TPatchNode;
var
  aModule : TPatchNode;
begin
  aModule := TPatchNode.Create;
  aModule.NodeName := Name;
  fChildNodeList.Add(aModule);
  result := aModule;
end;

function TPatchNode.GetChildNode(Index: integer): TPatchNode;
begin
  result := fChildNodeList[Index] as TPatchNode;
end;

function TPatchNode.GetChildNodeCount: integer;
begin
  result := fChildNodeList.Count;
end;

function TPatchNode.FindPatchValue(const Name: string): TPatchValue;
var
  c1: Integer;
begin
  for c1 := 0 to fValueList.count-1 do
  begin
    if (fValueList[c1] as TPatchValue).Name = Name then
    begin
      result := fValueList[c1] as TPatchValue;
      exit; //============>>exit>>==========>>
    end;
  end;

  // If we've made it this far, no matching value has been found.
  result := nil;
end;

function TPatchNode.NewPatchValue(const Name: string): TPatchValue;
var
  fValue : TPatchValue;
begin
  if ValueExists(Name) then EPatchObjectException.Create('PatchObject: A valued named ''' + Name + ''' already exists.');
  fValue := TPatchValue.Create;
  fValue.Name := Name;
  fValueList.Add(fValue);
  result := fValue;
end;

procedure TPatchNode.AddValue(const Name: string; x: single);
begin
  //Create a new patch value and set it's value property.
  NewPatchValue(Name).Value := DataIO_FloatToStr(x);
end;

procedure TPatchNode.AddValue(const Name: string; x: boolean);
begin
  //Create a new patch value and set it's value property.
  NewPatchValue(Name).Value := DataIO_BoolToStr(x);
end;

procedure TPatchNode.AddValue(const Name: string; x: string);
begin
  //Create a new patch value and set it's value property.
  NewPatchValue(Name).Value := x;
end;

procedure TPatchNode.AddValue(const Name: string; x: integer);
begin
  //Create a new patch value and set it's value property.
  NewPatchValue(Name).Value := DataIO_IntToStr(x);
end;

function TPatchNode.GetPatchValue(Index: integer): TPatchValue;
begin
  result := fValueList[Index] as TPatchValue;
end;

function TPatchNode.GetPatchValueCount: integer;
begin
  result := fValueList.Count;
end;

function TPatchNode.GetValue(const Name: string; Default: single): single;
var
  pv : TPatchValue;
begin
  pv := self.FindPatchValue(Name);
  if not assigned(pv) then
  begin
    result := Default;
  end else
  begin
    result := DataIO_StrToFloat(pv.Value, Default);
  end;
end;

function TPatchNode.GetValue(const Name: string; Default: boolean): boolean;
var
  pv : TPatchValue;
begin
  pv := self.FindPatchValue(Name);
  if not assigned(pv) then
  begin
    result := Default;
  end else
  begin
    result := DataIO_StrToBool(pv.Value, Default);
  end;
end;

function TPatchNode.GetValue(const Name: string; Default: string): string;
var
  pv : TPatchValue;
begin
  pv := self.FindPatchValue(Name);
  if not assigned(pv) then
  begin
    result := Default;
  end else
  begin
    result := pv.Value;
  end;
end;

function TPatchNode.GetValue(const Name: string; Default: integer): integer;
var
  pv : TPatchValue;
begin
  pv := self.FindPatchValue(Name);
  if not assigned(pv) then
  begin
    result := Default;
  end else
  begin
    result := DataIO_StrToInt(pv.Value, Default);
  end;
end;

{ TPatchObject }

constructor TPatchObject.Create;
begin
  fNodeList := TObjectList.Create;
  fNodeList.OwnsObjects := true;

end;

destructor TPatchObject.Destroy;
begin
  fNodeList.Free;
  inherited;
end;

procedure TPatchObject.Clear;
begin
  fNodeList.Clear;
end;

function TPatchObject.FindNode(const Name: string): TPatchNode;
var
  c1: Integer;
  aModule : TPatchNode;
begin
  for c1 := 0 to fNodeList.Count-1 do
  begin
    aModule := (fNodeList[c1] as TPatchNode);
    if aModule.NodeName = Name then
    begin
        result := fNodeList[c1] as TPatchNode;
        exit; //===============>>exit>>=============>>
    end;
  end;

  // If we've made it this far, no matching module has been found.
  result := nil;
end;

function TPatchObject.GetNode(Index: integer): TPatchNode;
begin
  result := fNodeList[Index] as TPatchNode;
end;

function TPatchObject.GetNodeCount: integer;
begin
  result := fNodeList.Count;
end;

function TPatchObject.NodeExists(const Name: string): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to fNodeList.Count-1 do
  begin
    if (fNodeList[c1] as TPatchNode).NodeName = Name then
    begin
        result := true;
        exit; //===============>>exit>>=============>>
    end;
  end;

  // If we've made it this far, no matching module has been found.
  result := false;
end;

function TPatchObject.NewNode(const Name: string): TPatchNode;
var
  aModule : TPatchNode;
begin
  aModule := TPatchNode.Create;
  aModule.NodeName := Name;
  fNodeList.Add(aModule);
  result := aModule;
end;


{ TMultiCastPatchObjectEvent }

procedure TMultiCastPatchObjectEvent.AddEventHandler(EventHandler: TPatchObjectEvent);
begin
  Add(TMethod(EventHandler));
end;

procedure TMultiCastPatchObjectEvent.RemoveEventHandler(EventHandler: TPatchObjectEvent);
begin
  Remove(TMethod(EventHandler));
end;

procedure TMultiCastPatchObjectEvent.Trigger(Sender: TObject; PatchObject: TPatchObject);
var
  c1: Integer;
  MyEvent : TPatchObjectEvent;
begin
  for c1 := 0 to Count-1 do
  begin
    //Cast the event handler reference to a variable of our event type.
    MyEvent := TPatchObjectEvent(Method[c1]);
    //Call the event handler.
    MyEvent(Sender, PatchObject);
  end;
end;

end.
