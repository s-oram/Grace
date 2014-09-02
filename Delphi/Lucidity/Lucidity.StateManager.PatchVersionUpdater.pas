unit Lucidity.StateManager.PatchVersionUpdater;

{
  This unit contains code to updote old Lucidity patch formats
  to the current patch format. The patch file XML is updated
  in place before being Lucidity attempts to load it. This way
  Lucidity only needs to work with the latest version of the patch
  file format.
}


interface

{$INCLUDE Defines.inc}

uses
  NativeXML,
  NativeXmlEx;

function UpdatePatchVersionFrom2To3(var XML: TNativeXML):boolean;

implementation

uses
  SysUtils,
  VamLib.Utils;

function UpdatePatchVersionFrom2To3(var XML: TNativeXML):boolean;
var
  c1 : integer;
  SampleGroupNodeList : TsdNodeList;
  SampleGroupNode : TXmlNode;
  TargetNode : TXmlNode;
begin
  // Required changes.
  // A LFO sync mode name was changed from "Sync5" to "Sync5_1"

  if not assigned(XML.Root) then
  begin
    exit(false);
  end;

  SampleGroupNodeList := TsdNodeList.Create;
  AutoFree(@SampleGroupNodeList);

  XML.Root.FindNodes('SampleGroup', SampleGroupNodeList);

  for c1 := 0 to SampleGroupNodeList.Count-1 do
  begin
    SampleGroupNode := SampleGroupNodeList[c1];

    // == Change #1 ==
    TargetNode := NodeWiz(SampleGroupNode).Child('VoiceParameters/LfoFreqMode1');
    if (assigned(TargetNode)) then
    begin
      if SameText('Sync5', TargetNode.ValueUnicode)
        then TargetNode.ValueUnicode := 'Sync5_1';
    end;

    // == Change #2 ==
    TargetNode := NodeWiz(SampleGroupNode).Child('VoiceParameters/LfoFreqMode2');
    if (assigned(TargetNode)) then
    begin
      if SameText('Sync5', TargetNode.ValueUnicode)
        then TargetNode.ValueUnicode := 'Sync5_1';
    end;

  end;


  result := true;
end;

end.
