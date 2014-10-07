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


//===== public methods =======

function CheckPatchVersionAndUpdateIfRequred(var XML: TNativeXML):boolean;


//===== private methods =======

// NOTE: Compatibility hasn't been maintained with version 0 or version 1 patches.
// Instead I call UpdatePatch_2To3() and hope for the best!
function UpdatePatchVersionFrom2To3(var XML: TNativeXML):boolean;
function UpdatePatchVersionFrom3To4(var XML: TNativeXML):boolean;

implementation

uses
  SysUtils,
  VamLib.Utils;



function CheckPatchVersionAndUpdateIfRequred(var XML: TNativeXML):boolean;
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.FindNode('PatchFileFormatVersion');
  if assigned(aNode)
    then PatchFormatVersion := DataIO_StrToInt(aNode.ValueUnicode, -1)
    else PatchFormatVersion := -1;

 if PatchFormatVersion < 3 then UpdatePatchVersionFrom2To3(XML);
 if PatchFormatVersion < 4 then UpdatePatchVersionFrom3To4(XML);

 result := true; //TODO:MED lets assume it always works for the time being.
end;

function UpdatePatchVersionFrom2To3(var XML: TNativeXML):boolean;
var
  c1 : integer;
  SampleGroupNodeList : TsdNodeList;
  SampleGroupNode : TXmlNode;
  TargetNode : TXmlNode;
begin
  // Required changes.
  // A LFO sync mode name was changed from "Sync5" to "Sync5_1"

  if not assigned(XML.Root) then exit(false);

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


  //== finally update the patch version info ==
  NodeWiz(XML.Root).FindOrCreateNode('PatchFileFormatVersion').Value := '3';
  result := true;
end;

function UpdatePatchVersionFrom3To4(var XML: TNativeXML):boolean;
var
  c1 : integer;
  SampleGroupNodeList : TsdNodeList;
  SampleGroupNode : TXmlNode;
  FilterTypeNode : TXmlNode;
  Par3Node, Par4Node : TXmlNode;
begin
  ///  Required changes:
  ///  All filters now MIX as the 4th parameter.
  ///  The Comb filter currently has MIX as parameter 3. This parameter value needs
  ///  to be moved to parameter 4.
  ///  All other filter types do not have a MIX parameter. The new MIX parameter
  ///  should be reset to a default of 1 (100%)
  ///
  ///  if not assigned(XML.Root) then exit(false);

  if not assigned(XML.Root) then exit(false);

  SampleGroupNodeList := TsdNodeList.Create;
  AutoFree(@SampleGroupNodeList);

  XML.Root.FindNodes('SampleGroup', SampleGroupNodeList);

  for c1 := 0 to SampleGroupNodeList.Count-1 do
  begin
    SampleGroupNode := SampleGroupNodeList[c1];


    //==== Check for filter 1 ====
    FilterTypeNode := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter1Type');
    if (assigned(FilterTypeNode)) and (SameText('ftCombA', FilterTypeNode.ValueUnicode)) then
    begin
      Par3Node := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter1Par3');
      Par4Node := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter1Par4');
      if (assigned(Par3Node)) then Par3Node.Name := 'Filter1Par4';
      if (assigned(Par4Node)) then Par4Node.Name := 'Filter1Par3';
    end else
    if (assigned(FilterTypeNode)) and (not SameText('ftCombA', FilterTypeNode.ValueUnicode)) then
    begin
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4').ValueUnicode := '1';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount1').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount2').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount3').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount4').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount5').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount6').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount7').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter1Par4/ModAmount8').ValueUnicode := '0';
    end;

    //==== Check for filter 2 ====
    FilterTypeNode := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter2Type');
    if (assigned(FilterTypeNode)) and (SameText('ftCombA', FilterTypeNode.ValueUnicode)) then
    begin
      Par3Node := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter2Par3');
      Par4Node := NodeWiz(SampleGroupNode).Child('VoiceParameters/Filter2Par4');
      if (assigned(Par3Node)) then Par3Node.Name := 'Filter2Par4';
      if (assigned(Par4Node)) then Par4Node.Name := 'Filter2Par3';
    end else
    if (assigned(FilterTypeNode)) and (not SameText('ftCombA', FilterTypeNode.ValueUnicode)) then
    begin
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4').ValueUnicode := '1';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount1').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount2').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount3').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount4').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount5').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount6').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount7').ValueUnicode := '0';
      NodeWiz(SampleGroupNode).FindOrCreateNode('VoiceParameters/Filter2Par4/ModAmount8').ValueUnicode := '0';
    end;
  end;

  //== finally update the patch version info ==
  NodeWiz(XML.Root).FindOrCreateNode('PatchFileFormatVersion').Value := '4';
  result := true;
end;

end.
