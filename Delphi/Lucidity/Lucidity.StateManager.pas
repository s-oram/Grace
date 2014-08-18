unit Lucidity.StateManager;

interface

{$INCLUDE Defines.inc}

{.$GenerateTemporaryConversionFiles}

//TODO:MED perhaps it might be possible to reduce the executable size by remove the M+
{$M+}

uses
  VamLib.Utils,
  VamGuiControlInterfaces,
  LucidityModConnections,
  Lucidity.PluginParameters,
  uLucidityEnums, soModMatrix,
  SysUtils, Lucidity.KeyGroup,
  eePlugin, Classes, NativeXML,
  eeTypes,
  Lucidity.StateManager.DataClasses;

type
  ELucidityStateException = Exception;

  TLucidityStateManager = class
  strict private
    {$Hints Off}
    // SaveModulatedParametersToNode() isn't being used, but I'm keeping it here
    // for the time being as a reference to how version 1 lucidity patches are saved.
    procedure SaveModulatedParametersToNode(ParentNode : TXmlNode; sg : TKeyGroup);
    {$Hints On}
    procedure LoadModulatedParametersFromNode(ParentNode : TXmlNode; sg : TKeyGroup);

    procedure SaveModulatedParameterToNode(ParentNode : TXmlNode; kg : TKeyGroup; ParName : string);
    procedure LoadModulatedParameterFromNode(ParentNode : TXmlNode; kg : TKeyGroup; ParName : string);
  private
  protected
    Plugin : TeePlugin;

    // The "state" contains all data specific to a Lucidity patch. Sample locations, patch info etc.
    // "State" contains the information that will be written out to a file when exporting patches.
    procedure ReadStateFromXML(var XML : TNativeXML);
    procedure WriteStateToXML(var XML : TNativeXML);

    // "Preset Info" is an extra chunk of data used when the saving/restoring from within
    // a host application.
    procedure ReadPresetInfoFromXML(var XML : TNativeXML);
    procedure WritePresetInfoToXML(var XML : TNativeXML);

    procedure ReadMidiMapFromXML(var XML : TNativeXML);
    procedure WriteMidiMapToXML(var XML : TNativeXML);

    procedure CheckPatchFormatVersion(var XML : TNativeXML);
    procedure WritePatchFormatVersionToXML(var XML : TNativeXML);

    procedure NewRegion(const RegionLoadInfo : TRegionLoadInfo; const SampleGroup : IKeyGroup);
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure LoadMidiMapFromFile(const FileName : string);
    procedure SaveMidiMapToFile(const FileName : string);

    procedure ImportProgram_Sfz(const FileName : string);

    procedure LoadPesetFromFile(const FileName : string); //called when plugin saves a "program" file.
    procedure SavePesetToFile(const FileName : string);   //called when plugin loads a "program" file.

    procedure GetPreset(var ms: TMemoryStream); //called when host saves the plugins state.
    procedure SetPreset(var ms: TMemoryStream); //called when host restores the plugins state.
  end;






  TModLinkSaveObject = class
  private
    fModVia: TModSource;
    fModSource: TModSource;
    fIsModMute: boolean;
    fModSourcePolarity: TModSourcePolarity;
  public
    procedure Clear;
  published
    property ModSource : TModSource read fModSource write fModSource;
    property ModVia    : TModSource read fModVia    write fModVia;
    property IsModMute : boolean    read fIsModMute write fIsModMute;
    property ModSourcePolarity : TModSourcePolarity read fModSourcePolarity write fModSourcePolarity;
  end;

  {$M+}

  TModParSaveObject = class
  private
    fParName: string;
    fParValue: single;
    fModAmount2: single;
    fModAmount3: single;
    fModAmount1: single;
    fModAmount6: single;
    fModAmount7: single;
    fModAmount4: single;
    fModAmount5: single;
    fModAmount8: single;
  public
  published
    property ParName  : string read fParName write fParName;
    property ParValue : single read fParValue write fParValue;
    property ModAmount1 : single read fModAmount1 write fModAmount1;
    property ModAmount2 : single read fModAmount2 write fModAmount2;
    property ModAmount3 : single read fModAmount3 write fModAmount3;
    property ModAmount4 : single read fModAmount4 write fModAmount4;
    property ModAmount5 : single read fModAmount5 write fModAmount5;
    property ModAmount6 : single read fModAmount6 write fModAmount6;
    property ModAmount7 : single read fModAmount7 write fModAmount7;
    property ModAmount8 : single read fModAmount8 write fModAmount8;
  end;

implementation

uses
  AudioIO,
  NativeXmlEx,
  Lucidity.SequencerDataObject,
  LucidityUtils,
  Lucidity.StateHelpers,
  uAutoFree,
  uConstants,
  eeSaveLoadFunctions,
  eeEnumHelper,
  Lucidity.SampleMap,
  eeFunctions, Lucidity.KeyGroupManager,
  Lucidity.Sfz;

const
  kCurrentFileVersion : integer = 1;

{ TLucidityStatemanager }

constructor TLucidityStateManager.Create(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TLucidityStateManager.Destroy;
begin

  inherited;
end;

procedure TLucidityStateManager.SetPreset(var ms: TMemoryStream);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromStream(ms);
    CheckPatchFormatVersion(XML);

    // TODO: add Preset version info check. if the check is out of date will
    // need to update file format.
    ReadStateFromXML(XML);
    ReadPresetInfoFromXML(XML);
    ReadMidiMapFromXML(XML);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStateManager.GetPreset(var ms: TMemoryStream);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.CreateName('root');
  try
    WriteStateToXML(XML);
    WritePatchFormatVersionToXML(XML);
    WritePresetInfoToXML(XML);
    WriteMidiMapToXML(XML);
    XML.SaveToStream(ms);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStateManager.ImportProgram_Sfz(const FileName: string);
var
  SfzImporter : TSfzImporter;
  XML : TNativeXML;
begin
  {$IFDEF GenerateTemporaryConversionFiles}
  CopyFile(FileName, 'D:\temp\Lucidity\Source.sfz');
  {$ENDIF}

  SfzImporter := TSfzImporter.Create;
  AutoFree(@SfzImporter);

  XML := nil;
  try
    SfzImporter.ConvertFile(FileName, XML);

    {$IFDEF GenerateTemporaryConversionFiles}
    XML.XmlFormat := xfReadable;
    xml.SaveToFile('D:\temp\Lucidity\test.lpg');
    {$ENDIF}

    // TODO:HIGH - whats going on here.
    CheckPatchFormatVersion(XML);
    MakeSampleFileNamesAbsolute(Xml.Root, FileName);
    ReadStateFromXML(XML);
  finally
    XML.Free;
  end;

end;

procedure TLucidityStateManager.LoadPesetFromFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(FileName);
    CheckPatchFormatVersion(XML);
    MakeSampleFileNamesAbsolute(Xml.Root, FileName);
    ReadStateFromXML(XML);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStateManager.LoadModulatedParameterFromNode(ParentNode: TXmlNode; kg: TKeyGroup; ParName: string);
var
  Par : TPluginParameter;
  ParID : TPluginParameterID;
  ModParValue : single;
  ModAmountValue : single;
  ModParIndex : integer;
  c1: Integer;
  NodePath : string;
  ParameterNode : TXmlNode;
  ModDataNode : TXmlNode;
begin
  //TODO:HIGH test what happens with an incorrect plugin parameter name.
  Par := PluginParFromName(ParName);
  ParID := PluginParToID(Par);

  if not IsModPar(ParID, ModParIndex) then raise Exception.Create('This is not a modulated parameter.');

  ParameterNode := ParentNode.NodeByName(UTF8String(ParName));
  if assigned(ParameterNode) then
  begin
    ModParValue := DataIO_StrToFloat(ParameterNode.ValueUnicode, 0.5);
    ModParValue := Clamp(ModParValue, 0, 1);
    kg.SetModParValue(ModParIndex, ModParValue, false);

    for c1 := 0 to kModSlotCount-1 do
    begin
      NodePath := 'ModAmount' + IntToStr(c1 + 1);
      ModDataNode := ParameterNode.FindNode(UTF8String(NodePath));
      if assigned(ModDataNode) then
      begin
        ModAmountValue := DataIO_StrToFloat(ModDataNode.ValueUnicode, 0);
        ModAmountValue := Clamp(ModAmountValue, 0, 1);
        kg.SetModParModAmount(ModParIndex, c1, ModAmountValue);
      end;
    end;
  end;
end;

procedure TLucidityStateManager.SaveModulatedParameterToNode(ParentNode: TXmlNode; kg: TKeyGroup; ParName: string);
var
  Par : TPluginParameter;
  ParID : TPluginParameterID;
  ModParValue : single;
  ModAmountValue : single;
  ModParIndex : integer;
  c1: Integer;
  NodePath : string;
begin
  //TODO:HIGH test what happens with an incorrect plugin parameter name.
  Par := PluginParFromName(ParName);
  ParID := PluginParToID(Par);

  if not IsModPar(ParID, ModParIndex) then raise Exception.Create('This is not a modulated parameter.');

  ModParValue := kg.GetModParValue(ModParIndex);
  NodeWiz(ParentNode).FindOrCreateNode(ParName).ValueUnicode := DataIO_FloatToStr(ModParValue);

  for c1 := 0 to kModSlotCount-1 do
  begin
    NodePath := ParName + '/ModAmount' + IntToStr(c1+1);
    ModAmountValue := kg.GetModParModAmount(ModParIndex, c1);
    NodeWiz(ParentNode).FindOrCreateNode(NodePath).ValueUnicode := DataIO_FloatToStr(ModAmountValue);
  end;
end;



procedure TLucidityStateManager.SaveModulatedParametersToNode(ParentNode: TXmlNode; sg: TKeyGroup);
var
  c1 : integer;
  ModParNode   : TXmlNode;
  ModParSaveObject : TModParSaveObject;
  Par : TPluginParameter;
  ModLinkIndex : integer;
begin
  ModParSaveObject := TModParSaveObject.Create;
  AutoFree(@ModParSaveObject);

  for c1 := 0 to GetPluginParameterCount-1 do
  begin
    Par := IndexToPluginParameter(c1);
    if IsModPar(Par) then
    begin
      ModLinkIndex := GetModParIndex(Par);
      ModParSaveObject.ParName    := PluginParToName(Par);
      ModParSaveObject.ParValue   := sg.GetModParValue(ModLinkIndex);
      ModParSaveObject.ModAmount1 := sg.GetModParModAmount(ModLinkIndex, 0);
      ModParSaveObject.ModAmount2 := sg.GetModParModAmount(ModLinkIndex, 1);
      ModParSaveObject.ModAmount3 := sg.GetModParModAmount(ModLinkIndex, 2);
      ModParSaveObject.ModAmount4 := sg.GetModParModAmount(ModLinkIndex, 3);
      ModParSaveObject.ModAmount5 := sg.GetModParModAmount(ModLinkIndex, 4);
      ModParSaveObject.ModAmount6 := sg.GetModParModAmount(ModLinkIndex, 5);
      ModParSaveObject.ModAmount7 := sg.GetModParModAmount(ModLinkIndex, 6);
      ModParSaveObject.ModAmount8 := sg.GetModParModAmount(ModLinkIndex, 7);

      ModParNode := ParentNode.NodeNew('ModulatedParameter');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ParName');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ParValue');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount1');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount2');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount3');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount4');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount5');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount6');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount7');
      SaveObjectPropertyToXML(ModParNode, ModParSaveObject, 'ModAmount8');
    end;


  end;
end;

procedure TLucidityStateManager.LoadModulatedParametersFromNode(ParentNode: TXmlNode; sg: TKeyGroup);
var
  c1 : integer;
  ModParNodeList : TsdNodeList;
  ModParSaveObject : TModParSaveObject;
  aNode : TXmlNode;

  ModLinkIndex : integer;

  Par : TPluginParameter;
begin
  ModParSaveObject := TModParSaveObject.Create;
  AutoFree(@ModParSaveObject);

  ModParNodeList := TsdNodeList.Create;
  AutoFree(@ModParNodeList);

  ParentNode.FindNodes('ModulatedParameter', ModParNodeList);

  for c1 := 0 to ModParNodeList.Count-1 do
  begin
    aNode := ModParNodeList[c1];
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ParName');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ParValue');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount1');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount2');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount3');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount4');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount5');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount6');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount7');
    LoadObjectPropertyFromXML(aNode, ModParSaveObject, 'ModAmount8');



    if IsValidPluginParName(ModParSaveObject.ParName) then
    begin
      Par := PluginParFromName(ModParSaveObject.ParName);
      if IsModPar(Par) then
      begin
        ModLinkIndex := GetModParIndex(Par);

        sg.SetModParValue(ModLinkIndex, ModParSaveObject.ParValue);
        sg.SetModParModAmount(ModLinkIndex, 0, ModParSaveObject.ModAmount1);
        sg.SetModParModAmount(ModLinkIndex, 1, ModParSaveObject.ModAmount2);
        sg.SetModParModAmount(ModLinkIndex, 2, ModParSaveObject.ModAmount3);
        sg.SetModParModAmount(ModLinkIndex, 3, ModParSaveObject.ModAmount4);
        sg.SetModParModAmount(ModLinkIndex, 4, ModParSaveObject.ModAmount5);
        sg.SetModParModAmount(ModLinkIndex, 5, ModParSaveObject.ModAmount6);
        sg.SetModParModAmount(ModLinkIndex, 6, ModParSaveObject.ModAmount7);
        sg.SetModParModAmount(ModLinkIndex, 7, ModParSaveObject.ModAmount8);
      end;
    end;
  end;
end;



procedure TLucidityStateManager.SavePesetToFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.CreateName('root');
  try
    WritePatchFormatVersionToXML(XML);
    WriteStateToXML(XML);
    MakeSampleFileNamesRelative(Xml.Root, FileName);
    XML.XmlFormat := xfReadable;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStateManager.WriteStateToXML(var XML: TNativeXML);
var
  c1 : integer;
  RootNode : TXMLNode;
  GlobalParametersNode : TXmlNode;
  SampleGroupNode : TXmlNode;
  VoiceParNode : TXmlNode;

  RegionNode : TXmlNode;
  RegionPropertiesNode : TXmlNode;
  SamplePropertiesNode : TXmlNode;
  StepSeqNode : TXmlNode;
  SGInfo : IKeyGroupsInfo;
  sg : TKeyGroup;
  RegionList : TRegionInterfaceList;
  c2: Integer;
  c3: Integer;

  ModLinkState : TModLinkSaveObject;
  ModConnections : TModConnections;
  ModConnectionsNode : TXmlNode;
  ModLinkNode        : TXmlNode;

  SeqData : IStepSequenceDataObject;

  KeyGroupStateInfo : TKeyGroupStateInfo;
begin
  ModLinkState := TModLinkSaveObject.Create;
  AutoFree(@ModLinkState);

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);

  KeyGroupStateInfo := TKeyGroupStateInfo.Create;
  AutoFree(@KeyGroupStateInfo);

  RootNode := xml.Root;
  assert(assigned(RootNode));

  GlobalParametersNode := RootNode.NodeNew('GlobalParameters');
  SaveObjectPropertyToXML(GlobalParametersNode, Plugin, 'VoiceMode');
  SaveObjectPropertyToXML(GlobalParametersNode, Plugin, 'VoiceGlide');


  sgInfo := Plugin.KeyGroups.GetInfo;
  for c1 := 0 to sgInfo.GetKeyGroupCount-1 do
  begin
    sg := sgInfo.GetKeyGroup(c1).GetObject as TKeyGroup;

    SampleGroupNode := RootNode.NodeNew('SampleGroup');
    SampleGroupNode.NodeNew('Name').ValueUnicode := sg.Name;

    RegionList.Clear;

    Plugin.SampleMap.FindRegionsByKeyGroup(sg.Name, RegionList);

    for c2 := 0 to RegionList.Count-1 do
    begin
      RegionNode := SampleGroupNode.NodeNew('Region');

      //===== Region Properties ======
      RegionPropertiesNode := RegionNode.NodeNew('RegionProperties');

      RegionPropertiesNode.NodeNew('LowNote').ValueUnicode        := DataIO_IntToStr(RegionList[c2].GetProperties^.LowNote);
      RegionPropertiesNode.NodeNew('HighNote').ValueUnicode       := DataIO_IntToStr(RegionList[c2].GetProperties^.HighNote);
      RegionPropertiesNode.NodeNew('LowVelocity').ValueUnicode    := DataIO_IntToStr(RegionList[c2].GetProperties^.LowVelocity);
      RegionPropertiesNode.NodeNew('HighVelocity').ValueUnicode   := DataIO_IntToStr(RegionList[c2].GetProperties^.HighVelocity);
      RegionPropertiesNode.NodeNew('RootNote').ValueUnicode       := DataIO_IntToStr(RegionList[c2].GetProperties^.RootNote);
      RegionPropertiesNode.NodeNew('SampleStart').ValueUnicode    := DataIO_IntToStr(RegionList[c2].GetProperties^.SampleStart);
      RegionPropertiesNode.NodeNew('SampleEnd').ValueUnicode      := DataIO_IntToStr(RegionList[c2].GetProperties^.SampleEnd);
      RegionPropertiesNode.NodeNew('LoopStart').ValueUnicode      := DataIO_IntToStr(RegionList[c2].GetProperties^.LoopStart);
      RegionPropertiesNode.NodeNew('LoopEnd').ValueUnicode        := DataIO_IntToStr(RegionList[c2].GetProperties^.LoopEnd);
      RegionPropertiesNode.NodeNew('SampleBeats').ValueUnicode    := DataIO_IntToStr(RegionList[c2].GetProperties^.SampleBeats);
      RegionPropertiesNode.NodeNew('SampleVolume').ValueUnicode   := DataIO_FloatToStr(RegionList[c2].GetProperties^.SampleVolume);
      RegionPropertiesNode.NodeNew('SamplePan').ValueUnicode      := DataIO_FloatToStr(RegionList[c2].GetProperties^.SamplePan);
      RegionPropertiesNode.NodeNew('SampleTune').ValueUnicode     := DataIO_FloatToStr(RegionList[c2].GetProperties^.SampleTune);
      RegionPropertiesNode.NodeNew('SampleFine').ValueUnicode     := DataIO_FloatToStr(RegionList[c2].GetProperties^.SampleFine);



      //===== Sample Properties ======
      SamplePropertiesNode := RegionNode.NodeNew('SampleProperties');

      SamplePropertiesNode.NodeNew('SampleFileName').ValueUnicode := RegionList[c2].GetProperties^.SampleFileName;
      SamplePropertiesNode.NodeNew('SampleFrames').ValueUnicode   := DataIO_IntToStr(RegionList[c2].GetSample^.Properties.SampleFrames);
    end;


    //==== Voice Parameters =====
    KeyGroupStateInfo.ResetToDefaultValues;
    sg.SaveState(KeyGroupStateInfo);

    VoiceParNode := SampleGroupNode.NodeNew('VoiceParameters');

    //==== Save standard parameters ====
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'PitchTracking');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'SampleReset');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'SamplerLoopBounds');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'SamplerTriggerMode');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'FilterRouting');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Filter1Type');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Filter2Type');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Filter1KeyFollow');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Filter2KeyFollow');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'AmpVelocityDepth');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'ModVelocityDepth');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'LfoShape1');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'LfoShape2');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'LfoFreqMode1');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'LfoFreqMode2');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Seq1Clock');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Seq1Direction');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'StepSeq1Length');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Seq2Clock');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'Seq2Direction');
    SaveObjectPropertyToXML(VoiceParNode, KeyGroupStateInfo, 'StepSeq2Length');


    // NOTE: Patch Format Version 1 vs Version 2
    // I've changed the way modulated parameter values are saved into a patch file.
    // Therefore the patch version has been changed to version 2. It's possible
    // for the patch loading code to simultaneously support version 1 and version 2
    // patch formats.

    //==== Save modulated parameters -- Patch Format Version 1 ====
    //SaveModulatedParametersToNode(VoiceParNode, sg);

    //==== Save modulated parameters -- Patch Format Version 2 ====
    SaveModulatedParameterToNode(VoiceParNode, sg, 'OutputGain');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'OutputPan');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'VoicePitchOne');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'VoicePitchTwo');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'SampleStart');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'SampleEnd');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'LoopStart');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'LoopEnd');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'AmpAttack');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'AmpHold');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'AmpDecay');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'AmpSustain');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'AmpRelease');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'ModAttack');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'ModHold');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'ModDecay');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'ModSustain');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'ModRelease');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'FilterOutputBlend');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter1Par1');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter1Par2');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter1Par3');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter1Par4');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter2Par1');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter2Par2');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter2Par3');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Filter2Par4');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo1Par1');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo1Par2');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo1Par3');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo2Par1');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo2Par2');
    SaveModulatedParameterToNode(VoiceParNode, sg, 'Lfo2Par3');
    //==========================================================================

    StepSeqNode := SampleGroupNode.NodeNew('StepSeq1');
    SeqData := sg.Seq1Data;
    for c3 := 0 to kMaxStepSequencerLength-1 do
    begin
      StepSeqNode.NodeNew('StepValue').ValueUnicode := DataIO_FloatToStr(SeqData.GetStepValue(c3));
    end;

    StepSeqNode := SampleGroupNode.NodeNew('StepSeq2');
    SeqData := sg.Seq2Data;
    for c3 := 0 to kMaxStepSequencerLength-1 do
    begin
      StepSeqNode.NodeNew('StepValue').ValueUnicode := DataIO_FloatToStr(SeqData.GetStepValue(c3));
    end;



    //==== save mod slot values =====
    ModConnections := sg.ModConnections;
    ModConnectionsNode := SampleGroupNode.NodeNew('ModConnections');

    for c3 := 0 to kModSlotCount-1 do
    begin
      ModLinkState.ModSource         := ModConnections.GetModSource(c3);
      ModLinkState.ModVia            := ModConnections.GetModVia(c3);
      ModLinkState.IsModMute         := ModConnections.GetModMute(c3);
      ModLinkState.ModSourcePolarity := ModConnections.GetModSourcePolarity(c3);

      ModLinkNode := ModConnectionsNode.NodeNew('ModLink');

      SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'ModSource');
      SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'ModVia');
      SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'IsModMute');
      SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'ModSourcePolarity');
    end;


  end;
end;

procedure TLucidityStateManager.ReadStateFromXML(var XML: TNativeXML);
var
  c1, c2 : integer;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  sgIntF : IKeyGroup;
  sg     : TKeyGroup; //TODO:MED it's likely this could be deleted once the state is loaded via the TKeyGroupStateInfo class.
  SampleGroupNodeList : TsdNodeList;
  RegionNodeList : TsdNodeList;
  ModLinkNodeList : TsdNodeList;
  StepValuesNodeList : TsdNodeList;

  GlobalParametersNode : TXmlNode;
  SampleGroupNode : TXmlNode;
  VoiceParNode : TXmlNode;
  RegionNode : TXmlNode;
  RegionPropertiesNode : TXmlNode;
  SamplePropertiesNode : TXmlNode;
  StepSeqNode : TXmlNode;

  KeyGroupLoadInfo : TKeyGroupStateInfo;
  RegionLoadInfo : TRegionLoadInfo;
  c3: Integer;

  StepValue : single;

  ModLinkState : TModLinkSaveObject;
  ModConnections : TModConnections;
  ModConnectionsNode : TXmlNode;
  ModLinkNode        : TXmlNode;
  ModLinkNodes       : TsdNodeList;

  SeqData : IStepSequenceDataObject;
begin
  SampleGroupNodeList := TsdNodeList.Create;
  AutoFree(@SampleGroupNodeList);

  RegionNodeList      := TsdNodeList.Create;
  AutoFree(@RegionNodeList);

  RegionLoadInfo := TRegionLoadInfo.Create;
  AutoFree(@RegionLoadInfo);

  KeyGroupLoadInfo := TKeyGroupStateInfo.Create;
  AutoFree(@KeyGroupLoadInfo);

  ModLinkNodeList := TsdNodeList.Create;
  AutoFree(@ModLinkNodeList);

  ModLinkNodes := TsdNodeList.Create;
  AutoFree(@ModLinkNodes);

  StepValuesNodeList := TsdNodeList.Create;
  AutoFree(@StepValuesNodeList);

  ModLinkState := TModLinkSaveObject.Create;
  AutoFree(@ModLinkState);

  RootNode := Xml.Root;

  //=============================================================================
  // IMPORTANT: TODO:
  //if FileVersion < kCurrentFileVersion then UpdateXmlPresetFile(XML);
  //=============================================================================

  GlobalParametersNode := RootNode.FindNode('GlobalParameters');
  if assigned(GlobalParametersNode) then
  begin
    LoadObjectPropertyFromXML(GlobalParametersNode, Plugin, 'VoiceMode');
    LoadObjectPropertyFromXML(GlobalParametersNode, Plugin, 'VoiceGlide');
  end;

  RootNode.FindNodes('SampleGroup', SampleGroupNodeList);

  for c1 := 0 to SampleGroupNodeList.Count-1 do
  begin
    SampleGroupNode := SampleGroupNodeList[c1];
    aNode := SampleGroupNode.FindNode('Name');
    if assigned(aNode) then
    begin
      if aNode.ValueUnicode <> ''
        then sgIntF := Plugin.KeyGroups.NewKeyGroup(aNode.ValueUnicode)
        else sgIntF := Plugin.KeyGroups.NewKeyGroup;
    end;

    if not assigned(sgIntF) then raise Exception.Create('SG (sample group interface variable not assigned.');

    sg := (sgIntF.GetObject as TKeyGroup);

    //===== Sample Regions ======
    RegionNodeList.Clear;
    SampleGroupNode.FindNodes('Region', RegionNodeList);

    for c2 := 0 to RegionNodeList.Count-1 do
    begin
      RegionNode := RegionNodeList[c2];

      RegionPropertiesNode := RegionNode.FindNode('RegionProperties');
      SamplePropertiesNode := RegionNode.FindNode('SampleProperties');

      if (assigned(RegionPropertiesNode)) and (assigned(SamplePropertiesNode)) then
      begin
        RegionLoadInfo.ResetToDefaultValues;

        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'LowNote');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'HighNote');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'LowVelocity');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'HighVelocity');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'RootNote');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleStart');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleEnd');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'LoopStart');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'LoopEnd');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleBeats');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleVolume');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SamplePan');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleTune');
        LoadObjectPropertyFromXML(RegionPropertiesNode, RegionLoadInfo, 'SampleFine');
        LoadObjectPropertyFromXML(SamplePropertiesNode, RegionLoadInfo, 'SampleFileName');
        LoadObjectPropertyFromXML(SamplePropertiesNode, RegionLoadInfo, 'SampleFrames');

        RegionLoadInfo.SanitiseData;

        // TODO:MED
        // add a validity check here before attempting to load a new region.
        // I can't remember what I wanted to validate before loading... oh well.
        NewRegion(RegionLoadInfo, sgIntF);
      end;
    end;


    //===== Voice parameters ======
    KeyGroupLoadInfo.ResetToDefaultValues;

    VoiceParNode := SampleGroupNode.FindNode('VoiceParameters');

    if assigned(VoiceParNode) then
    begin
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'PitchTracking');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'SampleReset');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'SamplerLoopBounds');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'SamplerTriggerMode');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'FilterRouting');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Filter1Type');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Filter2Type');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Filter1KeyFollow');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Filter2KeyFollow');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'AmpVelocityDepth');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'ModVelocityDepth');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'LfoShape1');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'LfoShape2');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'LfoFreqMode1');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'LfoFreqMode2');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Seq1Clock');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Seq1Direction');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'StepSeq1Length');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Seq2Clock');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'Seq2Direction');
      LoadObjectPropertyFromXML(VoiceParNode, KeyGroupLoadInfo, 'StepSeq2Length');
    end;


    //==== Load modulated parameters -- Patch Format Version 1 ====
    LoadModulatedParametersFromNode(VoiceParNode, sg);

    //==== Load modulated parameters -- Patch Format Version 2 ====
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'OutputGain');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'OutputPan');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'VoicePitchOne');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'VoicePitchTwo');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'SampleStart');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'SampleEnd');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'LoopStart');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'LoopEnd');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'AmpAttack');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'AmpHold');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'AmpDecay');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'AmpSustain');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'AmpRelease');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'ModAttack');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'ModHold');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'ModDecay');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'ModSustain');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'ModRelease');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'FilterOutputBlend');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter1Par1');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter1Par2');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter1Par3');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter1Par4');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter2Par1');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter2Par2');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter2Par3');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Filter2Par4');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo1Par1');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo1Par2');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo1Par3');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo2Par1');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo2Par2');
    LoadModulatedParameterFromNode(VoiceParNode, sg, 'Lfo2Par3');
    //================================================================================


    StepSeqNode := SampleGroupNode.FindNode('StepSeq1');
    SeqData := sg.Seq1Data;
    if assigned(StepSeqNode) then
    begin
      StepSeqNode.NodesByName('StepValue', StepValuesNodeList);

      for c3 := 0 to StepValuesNodeList.Count-1 do
      begin
        if c3 < kMaxStepSequencerLength then
        begin
          StepValue := DataIO_StrToFloat(StepValuesNodeList[c3].ValueUnicode, 0.5);
          StepValue := Clamp(StepValue, 0, 1);
          SeqData.SetStepValue(c3, StepValue);
        end;
      end;
    end;

    StepSeqNode := SampleGroupNode.FindNode('StepSeq2');
    SeqData := sg.Seq2Data;
    if assigned(StepSeqNode) then
    begin
      StepSeqNode.NodesByName('StepValue', StepValuesNodeList);

      for c3 := 0 to StepValuesNodeList.Count-1 do
      begin
        if c3 < kMaxStepSequencerLength then
        begin
          StepValue := DataIO_StrToFloat(StepValuesNodeList[c3].ValueUnicode, 0.5);
          Clamp(StepValue, 0, 1);
          SeqData.SetStepValue(c3, StepValue);
        end;
      end;
    end;

    KeyGroupLoadInfo.SanitiseData;

    sg.LoadState(KeyGroupLoadInfo);

    //===== restore mod slot values =====
    ModConnections := sg.ModConnections;
    ModConnectionsNode := SampleGroupNode.FindNode('ModConnections');
    if assigned(ModConnectionsNode) then
    begin
      ModConnectionsNode.FindNodes('ModLink', ModLinkNodes);

      for c3 := 0 to LowestValue(kModSlotCount, ModLinkNodes.Count)-1 do
      begin
        ModLinkNode := ModLinkNodes[c3];
        ModLinkState.Clear;

        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'ModSource');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'ModVia');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'IsModMute');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'ModSourcePolarity');

        ModConnections.SetModSource(c3, ModLinkState.ModSource);
        ModConnections.SetModVia(c3, ModLinkState.ModVia);
        ModConnections.SetModMute(c3, ModLinkState.IsModMute);
        ModConnections.SetModSourcePolarity(c3, ModLinkState.ModSourcePolarity);
      end;
    end;

  end;





end;


procedure TLucidityStateManager.NewRegion(const RegionLoadInfo: TRegionLoadInfo; const SampleGroup: IKeyGroup);
var
  fn : string;
  aRegion : TRegion;
  //LoadResult : boolean;
  SampleFrames : integer;
  SourceSampleLoopStart, SourceSampleLoopEnd : integer;
begin
  if not assigned(SampleGroup) then raise Exception.Create('SG (sample group interface variable not assigned.');


  // TODO:MED There's a few things needing to be done here.
  // - delayed sample loading. (don't load the sample immediately.)
  // - check the RegionLoadInfo is valid and will load a correctly configured region.
  // - the delayed sample loading will also need to handle missing files.
  aRegion := TRegion.Create;



  // TODO:HIGH I need to re-architecture the sample loading for a region.
  // The sample loading *should* happen once all the region properties have
  // been set. The sample loading shouldn't overwrite any region properties
  // that have previously been set. LoadSample() initialises some values to
  // default states, so right now it would overright things.
  // One of the reasons for this change would be to have delayed sample loading.
  // -- load a patch and load all samples in a background thread. This
  //    will allow a project to load quickly and not be bogged down by sample
  //    loading. Perhaps make this option. (Maybe a pop-up notification in
  //    the windows notification area show sample loading progress.
  // -- This will also be important when loading missing samples. Currently
  //    loading a missing sample will cause properties be to over-written.
  // TODO:HIGH IMPORTANT: Don't forget to move the sample loading sanitisation
  // code below when making the above changes.
  aRegion.LoadSample(RegionLoadInfo.SampleFileName);

  aRegion.ZeroCrossings.CalcZeroCrossingData(aRegion.Sample);
  aRegion.KeyGroup := SampleGroup;

  // TODO:HIGH In the case of a missing sample being loaded after these properties
  // have been assigned. Check that these property values persist and aren't
  // over-written.
  aRegion.Properties^.SampleFileName := RegionLoadInfo.SampleFileName;
  aRegion.Properties^.LowNote        := RegionLoadInfo.LowNote;
  aRegion.Properties^.HighNote       := RegionLoadInfo.HighNote;
  aRegion.Properties^.LowVelocity    := RegionLoadInfo.LowVelocity;
  aRegion.Properties^.HighVelocity   := RegionLoadInfo.HighVelocity;
  aRegion.Properties^.RootNote       := RegionLoadInfo.RootNote;
  aRegion.Properties^.SampleStart    := RegionLoadInfo.SampleStart;
  aRegion.Properties^.SampleEnd      := RegionLoadInfo.SampleEnd;
  aRegion.Properties^.LoopStart      := RegionLoadInfo.LoopStart;
  aRegion.Properties^.LoopEnd        := RegionLoadInfo.LoopEnd;
  aRegion.Properties^.SampleVolume   := RegionLoadInfo.SampleVolume;
  aRegion.Properties^.SampleTune     := RegionLoadInfo.SampleTune;
  aRegion.Properties^.SampleFine     := RegionLoadInfo.SampleFine;
  aRegion.Properties^.SamplePan      := RegionLoadInfo.SamplePan;
  aRegion.Properties^.SampleBeats    := RegionLoadInfo.SampleBeats;


  //============================================================================
  // Perform some sanitisation after the sample has been loaded.
  if (aRegion.Sample.Properties.IsValid) then
  begin
    SampleFrames := aRegion.Sample.Properties.SampleFrames;

    // NOTE: Check for negative values because the default 'unassigned' values will be -1.
    // As set in TRegionLoadInfo.ResetToDefaultValues().
    if aRegion.Properties^.SampleStart < 0 then aRegion.Properties^.SampleStart := 0;
    if aRegion.Properties^.SampleEnd < 0   then aRegion.Properties^.SampleEnd   := SampleFrames-1;

    fn := RegionLoadInfo.SampleFileName;
    if (FileExists(fn)) and (ReadLoopPoints(fn, SourceSampleLoopStart, SourceSampleLoopEnd)) then
    begin
      if aRegion.Properties^.LoopStart = -1
        then aRegion.Properties^.LoopStart := SourceSampleLoopStart;

      if aRegion.Properties^.LoopEnd = -0
        then aRegion.Properties^.LoopEnd := SourceSampleLoopEnd;
    end;



    // NOTE: When loop mode isn't defined, reset it in a way that is compatible with the SFZ format.
    // If loop_start is not specified and the sample has a loop defined, the sample start point will be used.
    // If loop_start is specified, it will overwrite the loop start point defined in the sample.
    // If loop_end is not specified and the sample have a loop defined, the sample loop end point will be used.
    // If loop_end is specified, it will overwrite the loop end point defined in the sample.
    if aRegion.Properties^.LoopStart < 0   then aRegion.Properties^.LoopStart   := aRegion.Properties^.SampleStart;
    if aRegion.Properties^.LoopEnd < 0     then aRegion.Properties^.LoopEnd     := aRegion.Properties^.SampleEnd;

    // Clamp start/end points to fit inside sample boundaries.
    aRegion.Properties^.SampleStart := Clamp(aRegion.Properties^.SampleStart, 0, SampleFrames-1);
    aRegion.Properties^.SampleEnd   := Clamp(aRegion.Properties^.SampleEnd, 0, SampleFrames-1);
    aRegion.Properties^.LoopStart   := Clamp(aRegion.Properties^.LoopStart, 0, SampleFrames-1);
    aRegion.Properties^.LoopEnd     := Clamp(aRegion.Properties^.LoopEnd, 0, SampleFrames-1);
  end;
  //============================================================================




  Plugin.SampleMap.AddRegion(aRegion);
end;



procedure TLucidityStatemanager.ReadPresetInfoFromXML(var XML: TNativeXML);
var
  RootNode : TXMLNode;
  PresetInfoNode : TXmlNode;
  aNode : TXmlNode;

  PresetName : string;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  PresetName := '';
  PresetInfoNode := RootNode.FindNode('PresetInfo');
  if assigned(PresetInfoNode) then
  begin
    aNode := PresetInfoNode.FindNode('PresetName');
    if assigned(aNode)
      then PresetName := aNode.ValueUnicode;
  end;

  Plugin.PresetName := PresetName;
end;


procedure TLucidityStatemanager.WritePresetInfoToXML(var XML: TNativeXML);
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));
  aNode := RootNode.NodeNew('PresetInfo');
  aNode.NodeNew('PresetName').ValueUnicode := Plugin.PresetName;
end;

procedure TLucidityStatemanager.WriteMidiMapToXML(var XML: TNativeXML);
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.NodeNew('MidiMap');
  Plugin.MidiAutomation.WriteStateToXML(aNode);
end;

procedure TLucidityStatemanager.ReadMidiMapFromXML(var XML: TNativeXML);
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.FindNode('MidiMap');
  if assigned(aNode)
    then Plugin.MidiAutomation.ReadStateFromXML(aNode);
end;


procedure TLucidityStatemanager.WritePatchFormatVersionToXML(var XML: TNativeXML);
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.NodeNew('PatchFileFormatVersion');
  aNode.ValueUnicode := DataIO_IntToStr(2);

  RootNode.NodeNew('PatchFileType').ValueUnicode := 'LucidityPatchFile';
end;

procedure TLucidityStatemanager.CheckPatchFormatVersion(var XML: TNativeXML);
const
  CurrentPatchFormatVersion : integer = 1;
var
  RootNode : TXMLNode;
  aNode : TXmlNode;
  PatchFormatVersion : integer;
begin
  RootNode := xml.Root;
  assert(assigned(RootNode));

  aNode := RootNode.FindNode('PatchFileFormatVersion');
  if assigned(aNode) then
  begin
    PatchFormatVersion := DataIO_StrToInt(aNode.ValueUnicode, -1);

    if (PatchFormatVersion > 0) and (PatchFormatVersion < CurrentPatchFormatVersion) then
    begin
      // Update patch file!
    end;

  end;

end;


procedure TLucidityStateManager.SaveMidiMapToFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.CreateName('root');
  try
    //TODO:HIGH  write MIDI map file info here.
    //WritePatchFormatVersionToXML(XML);

    WriteMidiMapToXML(XML);
    MakeSampleFileNamesRelative(Xml.Root, FileName);
    XML.XmlFormat := xfReadable;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStateManager.LoadMidiMapFromFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(FileName);

    //TODO:HIGH check MIDI map file version info here.
    //CheckPatchFormatVersion(XML);

    ReadMidiMapFromXML(XML);
  finally
    XML.Free;
  end;
end;




{ TModLinkSaveObject }

procedure TModLinkSaveObject.Clear;
begin
  self.fModSource := TModSource.None;
  self.fModVia    := TModSource.None;
  self.IsModMute  := false;
end;

end.

