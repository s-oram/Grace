{
  VST-XML is a data format used by Cubase.

  VST-XML is used when drag samples from a cubase project
  to other applications. (Many other DAW's use
  the audio file as the drag object data.)
}

unit eeVstXML;

interface

type
  TVstXmlRegion = record
    FileName : string;
    SampleStart : integer;
    SampleEnd   : integer;
  end;


// NOTE: These methods are *very* inefficient. That's ok for now as I think they
// will only be used very occasionally. Plenty of ways to optmise this if better
// performance is required in the future.
function GetVstXmlRegionCount(Text : string):integer;

function GetVstXmlRegionInfo(const RegionIndex : integer; const VstXmlText:string):TVstXmlRegion;

implementation

uses
  Classes, SysUtils,
  uAutoFree, NativeXML;

function GetVstXmlRegionCount(Text : string):integer;
var
  xml : TNativeXML;
  SS : TStringStream;

  VstXmlNode : TXmlNode;
  RegionNodes : TSdNodeList;
  c1: Integer;
begin
  VstXmlNode := nil;

  ss := TStringStream.Create(Text);
  AutoFree(@ss);

  xml := TNativeXML.Create(nil);
  AutoFree(@xml);

  Xml.LoadFromStream(ss);

  for c1 := 0 to xml.RootNodes.Count-1 do
  begin
    if xml.RootNodes[c1].Name = 'vst-xml' then
    begin
      VstXmlNode := xml.RootNodes[c1];
      break;
    end;
  end;

  if (assigned(VstXmlNode)) then
  begin
    RegionNodes := TSdNodeList.Create;
    AutoFree(@RegionNodes);

    VstXmlNode.FindNodes('region', RegionNodes);
    result := RegionNodes.Count;
  end else
  begin
    result := 0;
  end;

end;


function GetVstXmlRegionInfo(const RegionIndex : integer; const VstXmlText:string):TVstXmlRegion;
var
  xml : TNativeXML;
  SS : TStringStream;

  VstXmlNode : TXmlNode;
  RegionNodes : TSdNodeList;
  c1: Integer;
  aNode : TXmlNode;
begin
  ss := TStringStream.Create(VstXmlText);
  AutoFree(@ss);

  xml := TNativeXML.Create(nil);
  AutoFree(@xml);

  Xml.LoadFromStream(ss);

  for c1 := 0 to xml.RootNodes.Count-1 do
  begin
    if xml.RootNodes[c1].Name = 'vst-xml' then
    begin
      VstXmlNode := xml.RootNodes[c1];
      break;
    end;
  end;

  if not assigned(VstXMLNode)
    then raise Exception.Create('Vst-XML node not found.');

  RegionNodes := TSdNodeList.Create;
  AutoFree(@RegionNodes);

  VstXmlNode.FindNodes('region', RegionNodes);


  if RegionIndex >= RegionNodes.Count
    then raise Exception.Create('Region Index is out of list bounds.');


  // pull the region info out of the region node....

  aNode := RegionNodes[RegionIndex].FindNode('filename');
  if assigned(aNode)
    then result.FileName := aNode.ValueUnicode
    else result.FileName := '';

  aNode := RegionNodes[RegionIndex].FindNode('start');
  if assigned(aNode)
    then result.SampleStart := aNode.ValueAsInteger
    else result.SampleStart := -1;

    aNode := RegionNodes[RegionIndex].FindNode('end');
  if assigned(aNode)
    then result.SampleEnd := aNode.ValueAsInteger
    else result.SampleEnd := -1;

end;


end.
