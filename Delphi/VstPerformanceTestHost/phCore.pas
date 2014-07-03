unit phCore;

interface

type
  TTestInfo = record
    VstPluginFile : string;

    BufferSize : integer;
    SampleRate : integer;
  end;


function LoadTestInfo:TTestInfo;

implementation

uses
  uAutoFree,
  Vcl.Dialogs, SysUtils,
  NativeXML, eeVstExtra;


function LoadTestInfo:TTestInfo;
var
  fn : string;
  xml : TNativeXML;
  RootNode : TXmlNode;
  NodeName : string;
  aNode : TXmlNode;
begin
  fn := GetDLLDirectory;

  fn := IncludeTrailingPathDelimiter(fn) + 'TestInfo.xml';

  xml := TNativeXml.Create(nil);
  AutoFree(@xml);

  if fileExists(fn) then
  begin
    xml.LoadFromFile(fn);
    RootNode := xml.Root;

    if not assigned(RootNode) then raise Exception.Create('Can not find root node.');

    NodeName := 'VstPluginFilePath';
    aNode := RootNode.NodeByName(NodeName);
    if not assigned(aNode) then raise Exception.Create('Can not find node (' + NodeName + ').');
    if not FileExists(aNode.ValueUnicode) then raise Exception.Create('VST Plugin file doesn''t exist.');
    result.VstPluginFile := aNode.ValueUnicode;



    NodeName := 'BufferSize';
    aNode := RootNode.NodeByName(NodeName);
    if not assigned(aNode) then raise Exception.Create('Can not find node (' + NodeName + ').');
    result.BufferSize := aNode.ValueAsInteger;



    NodeName := 'SampleRate';
    aNode := RootNode.NodeByName(NodeName);
    if not assigned(aNode) then raise Exception.Create('Can not find node (' + NodeName + ').');
    result.SampleRate := aNode.ValueAsInteger;







  end;



end;

end.
