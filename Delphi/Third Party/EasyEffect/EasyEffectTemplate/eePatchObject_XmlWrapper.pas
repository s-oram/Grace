{
  TXmlPatchWrapper is used to convert between XML formatted patch data and
  TPatchObject (which is used internally).

  Addtionally TXmlPatchWrapper is intended to translate older patches to
  the newest used format where required. The advantage of translating to
  newer patch formats at the XML layer is that TPatchObject interactions only
  need to be compatible with current patch format.

  If it was possible to load multiple patch versions into a PatchObject, the plugin
  internals would need know how to read all potential versions.
}


unit eePatchObject_XmlWrapper;

interface

uses
  SysUtils,
  Classes,
  NativeXML,
  eePatchObject;

type
  EXmlPatchWrapperException = Exception;

  TUpdateXmlPatchData = procedure(Sender:TObject; var aXmlPatchData : TNativeXML; var IsUpdateSuccessful:boolean) of object;

  TXmlPatchWrapper = class
  private
    fOnUpdateXmlPatchData: TUpdateXmlPatchData;
    fCurrentPatchFormat: string;
    fCurrentPatchVersion: integer;
  protected
    XmlPatchData : TNativeXML;
    procedure CheckValidityOfXmlPatchData;
    function UpdateXmlPatchData:boolean;

    procedure ReadFromPatchModule(Node:TXmlNode; PatchModule : TPatchNode);
    procedure WriteToPatchModule(Node:TXmlNode; PatchModule : TPatchNode);


  public
    // Set aPatchFormat and aPatchVersion to values appropiate for the current version of the product.
    // These values will be used as a reference when loading patches from external sources. If the external source
    // doesn't match the current format/version, the patch will be updated.
    constructor Create(const aCurrentPatchFormat:string; const aCurrentPatchVersion:integer);
    destructor Destroy; override;

    property CurrentPatchFormat  : string  read fCurrentPatchFormat;
    property CurrentPatchVersion : integer read fCurrentPatchVersion;

    procedure Clear;

    // The LoadFrom/SaveTo methods read/write XML data.
    procedure LoadFrom(const FileName : string); overload;
    procedure LoadFrom(var Stream : TMemoryStream); overload;
    procedure SaveTo(const FileName : string); overload;
    procedure SaveTo(var Stream : TMemoryStream); overload;

    // The ConvertFrom/ConvertTo methods read/write patch data from a generic TPatchObject.
    procedure ReadFrom(var PatchObject : TPatchObject);
    procedure WriteTo(var PatchObject : TPatchObject);


    // OnUpdateXmlPatchData will be called when a patch is loaded from an external source that doesn't match the current format/version.
    // This event allows application developers to add support for importing older format patch files.
    property OnUpdateXmlPatchData : TUpdateXmlPatchData read fOnUpdateXmlPatchData write fOnUpdateXmlPatchData;
  end;


//Helper functions
procedure GetXmlPatchInfo(const XmlPatchData:TNativeXML; out Format:string; out FormatVersion:integer);


implementation

uses
  eeFunctions;


procedure GetXmlPatchInfo(const XmlPatchData:TNativeXML; out Format:string; out FormatVersion:integer);
var
  RootNode      : TXmlNode;
  PatchInfoNode : TXmlNode;
  FormatNode, FormatVersionNode : TXmlNode;
begin
  if not assigned(XmlPatchData)      then raise EXmlPatchWrapperException.Create('XmlPatchData: XML data is not assigned.');
  if not assigned(XmlPatchData.Root) then raise EXmlPatchWrapperException.Create('XmlPatchData: Root node not assigned.');

  RootNode := XmlPatchData.Root;
  if RootNode.Name <> 'PatchObject' then
  begin
    Format := 'unknown';
    FormatVersion := -1;
    exit; //============>>exit>>======>>
  end;

  PatchInfoNode := RootNode.FindNode('PatchInfo');
  if not assigned(PatchInfoNode) then
  begin
    Format := 'unknown';
    FormatVersion := -1;
    exit; //============>>exit>>======>>
  end;

  FormatNode := PatchInfoNode.FindNode('Format');
  if not assigned(FormatNode) then
  begin
    Format := 'unknown';
    FormatVersion := -1;
    exit; //============>>exit>>======>>
  end;

  FormatVersionNode := PatchInfoNode.FindNode('FormatVersion');
  if not assigned(FormatVersionNode) then
  begin
    Format := 'unknown';
    FormatVersion := -1;
    exit; //============>>exit>>======>>
  end;

  //If we've made it this far, we might have found some useful patch format info...
  Format := string(FormatNode.ValueUnicode);
  FormatVersion := DataIO_StrToInt(string(FormatVersionNode.ValueUnicode), -1);
end;

{ TPatchXmlWrapper }

constructor TXmlPatchWrapper.Create(const aCurrentPatchFormat:string; const aCurrentPatchVersion:integer);
begin
  fCurrentPatchFormat := aCurrentPatchFormat;
  fCurrentPatchVersion := aCurrentPatchVersion;

  XmlPatchData := TNativeXml.Create(nil);
end;

destructor TXmlPatchWrapper.Destroy;
begin
  XmlPatchData.Free;
  inherited;
end;

function TXmlPatchWrapper.UpdateXmlPatchData: boolean;
var
  IsUpdateSuccessful : boolean;
begin
  IsUpdateSuccessful := false;
  if assigned(OnUpdateXmlPatchData) then OnUpdateXmlPatchData(self, XmlPatchData, IsUpdateSuccessful);
  result := IsUpdateSuccessful;
end;

procedure TXmlPatchWrapper.LoadFrom(const FileName: string);
var
  xf : string;
  xv : integer;
  IsUpdateSuccessful:boolean;
begin
  if not FileExists(FileName) then EXmlPatchWrapperException.Create('XmlPatchWrapper: File not found.');
  XmlPatchData.Clear;
  XmlPatchData.LoadFromFile(FileName);

  GetXmlPatchInfo(XmlPatchData, xf, xv);
  if (xf <> CurrentPatchFormat) or (xv <> CurrentPatchVersion) then
  begin
    IsUpdateSuccessful := UpdateXmlPatchData;
    if IsUpdateSuccessful = false then raise EXmlPatchWrapperException.Create('Couldn''t update patch format.');
  end;

  // Note: This will raise exceptions if XmlPatchData is not valid for this application.
  CheckValidityOfXmlPatchData;
end;

procedure TXmlPatchWrapper.LoadFrom(var Stream: TMemoryStream);
var
  xf : string;
  xv : integer;
  IsUpdateSuccessful:boolean;
begin
  XmlPatchData.Clear;
  XmlPatchData.LoadFromStream(Stream);

  GetXmlPatchInfo(XmlPatchData, xf, xv);
  if (xf <> CurrentPatchFormat) or (xv <> CurrentPatchVersion) then
  begin
    IsUpdateSuccessful := UpdateXmlPatchData;
    if IsUpdateSuccessful = false then raise EXmlPatchWrapperException.Create('Couldn''t update patch format.');
  end;

  // NOTE: This will raise exceptions if XmlPatchData is not valid for this application.
  CheckValidityOfXmlPatchData;
end;

procedure TXmlPatchWrapper.SaveTo(var Stream: TMemoryStream);
begin
  XmlPatchData.XmlFormat := xfCompact;
  XmlPatchData.SaveToStream(Stream);
end;

procedure TXmlPatchWrapper.SaveTo(const FileName: string);
begin
  XmlPatchData.XmlFormat := xfReadable;
  XmlPatchData.SaveToFile(FileName);
end;

procedure TXmlPatchWrapper.CheckValidityOfXmlPatchData;
var
  RootNode      : TXmlNode;
  PatchInfoNode : TXmlNode;
  PatchDataNode : TXmlNode;
begin
  if not assigned(XmlPatchData)      then raise EXmlPatchWrapperException.Create('XmlPatchData: XML data is not assigned.');
  if not assigned(XmlPatchData.Root) then raise EXmlPatchWrapperException.Create('XmlPatchData: Root node not assigned.');

  RootNode := XmlPatchData.Root;
  if RootNode.Name <> 'PatchObject' then raise EXmlPatchWrapperException.Create('XmlPatchData: Root node isn''t named ''PatchObject''.');

  PatchInfoNode := RootNode.FindNode('PatchInfo');
  if not assigned(PatchInfoNode) then raise EXmlPatchWrapperException.Create('XmlPatchData: Cannot find ''PatchInfoNode''.');

  PatchDataNode := RootNode.FindNode('PatchData');
  if not assigned(PatchDataNode) then raise EXmlPatchWrapperException.Create('XmlPatchData: Cannot find ''PatchDataNode''.');
end;

procedure TXmlPatchWrapper.Clear;
begin
  XmlPatchData.Clear;
end;

procedure TXmlPatchWrapper.ReadFrom(var PatchObject: TPatchObject);
var
  c1 : integer;
  aModule : TPatchNode;
  RootNode      : TXmlNode;
  PatchInfoNode : TXmlNode;
  PatchDataNode : TXmlNode;
  ModuleNode    : TXmlNode;
begin
  XmlPatchData.Clear;

  XmlPatchData.CreateName('PatchObject');
  RootNode := XmlPatchData.Root;

  PatchInfoNode := RootNode.NodeNew('PatchInfo');
  PatchInfoNode.NodeNew('Format').ValueUnicode := CurrentPatchFormat;
  PatchInfoNode.NodeNew('FormatVersion').ValueUnicode := IntToStr(CurrentPatchVersion);

  PatchDataNode := RootNode.NodeNew('PatchData');

  for c1 := 0 to PatchObject.NodeCount-1 do
  begin
    aModule := PatchObject.Node[c1];
    ModuleNode := PatchDataNode.NodeNew(UTF8String(aModule.NodeName));

    ReadFromPatchModule(ModuleNode, aModule);
  end;

  // Important: Check the validity of XmlPatchData after creating it. This will
  // help ensure this class can load any XML documents it creates.
  CheckValidityOfXmlPatchData;
end;



procedure TXmlPatchWrapper.WriteTo(var PatchObject: TPatchObject);
var
  c1 : integer;
  aModule : TPatchNode;
  RootNode      : TXmlNode;
  //PatchInfoNode : TXmlNode; //TODO:LOW this variable doesn't seem to be getting used. Maybe delete.
  PatchDataNode : TXmlNode;
  ModuleNode    : TXmlNode;
begin
  //============================================================================
  // NOTE: This will raise exceptions if XmlPatchData is not valid for this application.
  CheckValidityOfXmlPatchData;

  // NOTE: The ConvertTo() method requires the nodes below to be found in the XmlPatchData document.
  // If they don't exist, the routine will fail. Normally I would use exceptions to check for the
  // existence of these nodes. (It's better to raise errors and Fail Fast!).
  // In this case the method above 'CheckValidityOfXmlPatchData' does all the checking and
  // will raise exceptions if required.
  RootNode := XmlPatchData.Root;
  //PatchInfoNode := RootNode.FindNode('PatchInfo');
  PatchDataNode := RootNode.FindNode('PatchData');

  // Sanity checks...
  assert(assigned(RootNode));
  //assert(assigned(PatchInfoNode));
  assert(assigned(PatchDataNode));
  //============================================================================

  if not assigned(PatchObject) then raise EXmlPatchWrapperException.Create('XmlPatchData: PatchObject variable not assigned.');
  PatchObject.Clear;

  for c1 := 0 to PatchDataNode.NodeCount-1 do
  begin
    ModuleNode := PatchDataNode.Nodes[c1];
    aModule := PatchObject.NewNode(string(ModuleNode.Name));

    WriteToPatchModule(ModuleNode, aModule);
  end;
end;

procedure TXmlPatchWrapper.ReadFromPatchModule(Node: TXmlNode; PatchModule: TPatchNode);
var
  c1 : integer;
  ValuesNode : TXmlNode;
  ModulesNode : TXmlNode;
  ParName, ParValue : UTF8String;
  ChildMod     : TPatchNode;
  ChildModNode : TXmlNode;
  ChildModName : UTF8String;
begin
  if PatchModule.ChildNodeCount > 0 then
  begin
    ModulesNode := Node.NodeNew('Modules');

    for c1 := 0 to PatchModule.ChildNodeCount-1 do
    begin
      ChildMod     := PatchModule.ChildNode[c1];
      ChildModName := UTF8String(ChildMod.NodeName);
      ChildModNode := ModulesNode.NodeNew(ChildModName);

      ReadFromPatchModule(ChildModNode, ChildMod);
    end;
  end;


  if PatchModule.PatchValueCount > 0 then
  begin
    ValuesNode := Node.NodeNew('Values');

    for c1 := 0 to PatchModule.PatchValueCount-1 do
    begin
      ParName  := UTF8String(PatchModule.PatchValue[c1].Name);
      ParValue := UTF8String(PatchModule.PatchValue[c1].Value);

      ValuesNode.NodeNew(ParName).ValueUnicode := String(ParValue);
    end;
  end;

end;

procedure TXmlPatchWrapper.WriteToPatchModule(Node: TXmlNode; PatchModule: TPatchNode);
var
  c1 : integer;
  ValuesNode : TXmlNode;
  ModulesNode : TXmlNode;
  ParName, ParValue : string;
  ChildMod     : TPatchNode;
  ChildModNode : TXmlNode;
begin
  ModulesNode := Node.FindNode('Modules');
  if assigned(ModulesNode) then
  begin
    for c1 := 0 to ModulesNode.NodeCount-1 do
    begin
      ChildModNode := ModulesNode[c1];
      ChildMod := PatchModule.NewChildNode(string(ChildModNode.Name));

      WriteToPatchModule(ChildModNode, ChildMod);
    end;
  end;


  ValuesNode := Node.FindNode('Values');
  if assigned(ValuesNode) then
  begin
    for c1 := 0 to ValuesNode.NodeCount-1 do
    begin
      ParName := string(ValuesNode[c1].Name);
      ParValue := string(ValuesNode[c1].ValueUnicode);

      PatchModule.AddValue(ParName, ParValue);
    end;
  end;
end;



end.
