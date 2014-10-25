unit Lucidity.ProgramFileUtils;

interface

uses
  Types, Classes;

function GetProgramFileFormat(fn : string):integer;

procedure GetUsedSamples(const fn : string; var SampleFileNames : TStringList);

implementation

uses
  VamLib.Utils,
  NativeXML, NativeXMLEx;

function GetProgramFileFormat(fn : string):integer;
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(fn);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.FindNode('PatchFileFormatVersion');
  if assigned(aNode)
    then PatchFormatVersion := DataIO_StrToInt(aNode.ValueUnicode, -1)
    else PatchFormatVersion := -1;

 result := PatchFormatVersion;
end;

procedure GetUsedSamples(const fn : string; var SampleFileNames : TStringList);
var
  xml : TNativeXML;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
  nd : TXmlNode;
  Nodes : TList;
  c1: Integer;
  p : Pointer;
begin
  xml := TNativeXML.Create(nil);
  autoFree(@xml);
  xml.LoadFromFile(fn);

  Nodes := TList.Create;
  AutoFree(@Nodes);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  FindNodes(RootNode, 'SampleFileName', Nodes);

  for c1 := 0 to Nodes.Count-1 do
  begin
    nd := TXmlNode(Nodes.Items[c1]);
    SampleFileNames.Add(nd.ValueUnicode);
  end;


end;

end.
