unit uLucidityStateManager;

interface

uses
  LucidityModConnections,
  uLucidityEnums, soModMatrix,
  SysUtils, uLucidityKeyGroup,
  eePlugin, Classes, NativeXML;

type
  ELucidityStateException = Exception;

  TRegionLoadInfo = class;

  TLucidityStatemanager = class
  private
  protected
    Plugin : TeePlugin;

    // The "state" contains all data specific to Lucidity. Sample locations, patch info etc.
    // "State" contains the information that will be written out to a file when exporting patches.
    procedure ReadStateFromXML(var XML : TNativeXML);
    procedure WriteStateToXML(var XML : TNativeXML);

    // "Preset Info" is an extra chunk of data used when the saving/restoring from within
    // a host application.
    procedure ReadPresetInfoFromXML(var XML : TNativeXML);
    procedure WritePresetInfoToXML(var XML : TNativeXML);

    procedure NewRegion(const RegionLoadInfo : TRegionLoadInfo; const SampleGroup : IKeyGroup);
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;

    procedure ImportProgram_Sfz(const FileName : string);

    procedure LoadPesetFromFile(const FileName : string); //called when plugin saves a "program" file.
    procedure SavePesetToFile(const FileName : string);   //called when plugin loads a "program" file.

    procedure GetPreset(var ms: TMemoryStream); //called when host saves the plugins state.
    procedure SetPreset(var ms: TMemoryStream); //called when host restores the plugins state.
  end;



  TRegionLoadInfo = class
  strict private
    fHighNote: integer;
    fLoopEnd: integer;
    fSampleStart: integer;
    fSampleBeats: integer;
    fSampleVolume: single;
    fSampleFrames: integer;
    fRootNote: integer;
    fLowVelocity: integer;
    fSampleEnd: integer;
    fSampleFileName: string;
    fLowNote: integer;
    fLoopStart: integer;
    fSamplePan: single;
    fHighVelocity: integer;
    fSampleTune: integer;
    fSampleFine: integer;
  private
  public
  published
    procedure SanitiseData; // call after reading data from a save file.

    property LowNote        : integer read fLowNote        write fLowNote;
    property HighNote       : integer read fHighNote       write fHighNote;
    property LowVelocity    : integer read fLowVelocity    write fLowVelocity;
    property HighVelocity   : integer read fHighVelocity   write fHighVelocity;
    property RootNote       : integer read fRootNote       write fRootNote;
    property SampleStart    : integer read fSampleStart    write fSampleStart;
    property SampleEnd      : integer read fSampleEnd      write fSampleEnd;
    property LoopStart      : integer read fLoopStart      write fLoopStart;
    property LoopEnd        : integer read fLoopEnd        write fLoopEnd;
    property SampleBeats    : integer read fSampleBeats    write fSampleBeats;
    property SampleVolume   : single  read fSampleVolume   write fSampleVolume;
    property SamplePan      : single  read fSamplePan      write fSamplePan;
    property SampleTune     : integer read fSampleTune     write fSampleTune;
    property SampleFine     : integer read fSampleFine     write fSampleFine;
    property SampleFileName : string  read fSampleFileName write fSampleFileName;
    property SampleFrames   : integer read fSampleFrames   write fSampleFrames;
  end;


  TModLinkLoadInfo = class
  strict private
    fVia: TModSource;
    fSource: TModSource;
    fDest: TModDest;
    fAmount: single;
    fOffset: single;
  private
    fUniqueID: string;
  public
    procedure Clear;
    procedure AssignFrom(const aSource:TModLink_OLD);
    procedure AssignTo(var aDest: TModLink_OLD);
  published
    procedure SanitiseData; // call after reading data from a save file.

    property UniqueID : string     read  fUniqueID write fUniqueID;
    property Source   : TModSource read fSource    write fSource;
    property Dest     : TModDest   read fDest      write fDest;
    property Via      : TModSource read fVia       write fVia;
    property Amount   : single     read fAmount    write fAmount;
    property Offset   : single     read fOffset    write fOffset;
  end;


implementation

uses
  Lucidity.StateHelpers,
  uAutoFree,
  uConstants,
  eeSaveLoadFunctions,
  eeEnumHelper,
  Lucidity.SampleMap,
  eeFunctions, uKeyGroupManager,
  Lucidity.Sfz;

const
  kCurrentFileVersion : integer = 1;

{ TLucidityStatemanager }

constructor TLucidityStatemanager.Create(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TLucidityStatemanager.Destroy;
begin

  inherited;
end;

procedure TLucidityStatemanager.SetPreset(var ms: TMemoryStream);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromStream(ms);
    //XML.LoadFromBinaryStream(ms);
    ReadStateFromXML(XML);
    ReadPresetInfoFromXML(XML);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStatemanager.GetPreset(var ms: TMemoryStream);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.CreateName('root');
  try
    WriteStateToXML(XML);
    WritePresetInfoToXML(XML);
    XML.SaveToStream(ms);
    //XML.SaveToBinaryStream(ms);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStatemanager.ImportProgram_Sfz(const FileName: string);
var
  SfzImporter : TSfzImporter;
  XML : TNativeXML;
begin
  SfzImporter := TSfzImporter.Create;
  AutoFree(@SfzImporter);

  XML := TNativeXML.Create(nil);
  AutoFree(@xml);

  SfzImporter.ConvertFile(FileName, XML);


  XML.XmlFormat := xfReadable;
  xml.SaveToFile('C:\Users\Shannon Matthews\Desktop\test convert.lpg');

  //ReadStateFromXML(XML);
end;

procedure TLucidityStatemanager.LoadPesetFromFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(FileName);
    MakeSampleFileNamesAbsolute(Xml.Root, FileName);
    ReadStateFromXML(XML);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStatemanager.SavePesetToFile(const FileName: string);
var
  XML : TNativeXML;
begin
  XML := TNativeXML.CreateName('root');
  try
    WriteStateToXML(XML);
    MakeSampleFileNamesRelative(Xml.Root, FileName);
    XML.XmlFormat := xfReadable;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

procedure TLucidityStatemanager.WriteStateToXML(var XML: TNativeXML);
var
  c1 : integer;
  RootNode : TXMLNode;
  GlobalParametersNode : TXmlNode;
  SampleGroupNode : TXmlNode;
  VoiceParNode : TXmlNode;
  RegionNode : TXmlNode;
  RegionPropertiesNode : TXmlNode;
  SamplePropertiesNode : TXmlNode;
  ModLinkNode : TXmlNode;
  StepSeqNode : TXmlNode;
  SGInfo : IKeyGroupsInfo;
  sg : TKeyGroup;
  RegionList : TRegionInterfaceList;
  c2: Integer;
  c3: Integer;
  ModLinkState : TModLinkLoadInfo;
begin
  ModLinkState := TModLinkLoadInfo.Create;
  AutoFree(@ModLinkState);

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);

  Xml.Clear;
  RootNode := XML.Root;
  RootNode.NodeNew('FileType').ValueUnicode := 'LucidityPatch';
  RootNode.NodeNew('FileVersion').ValueUnicode := IntToStr(kCurrentFileVersion);

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
    VoiceParNode := SampleGroupNode.NodeNew('VoiceParameters');

    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'PitchTracking');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'SampleReset');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'VoiceGain');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'VoicePan');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'VoicePitchOne');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'VoicePitchTwo');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'GrainLoop');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'GrainLength');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'GrainRate');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'GrainPosition');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'SamplerLoopBounds');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'SamplerLoopMode');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'OscShape');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'OscPulseWidth');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter1Type');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter2Type');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par1');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par3');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par1');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par3');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpAttack');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpHold');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpDecay');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpSustain');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpRelease');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'AmpVelocityDepth');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterAttack');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterHold');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterDecay');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterSustain');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterRelease');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'FilterVelocityDepth');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoShape1');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoShape2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoRate1');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoRate2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoAPar2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'LfoBPar2');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvAAttack');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvADecay');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvAMode');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBAttack');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBDecay');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBMode');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Seq1Clock');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Seq1Direction');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'StepSeq1Length');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Seq2Clock');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'Seq2Direction');
    SaveObjectPropertyToXML(VoiceParNode, sg.VoiceParameters, 'StepSeq2Length');

    for c3 := 0 to sg.ModConnections_OLD.ModLinkCount-1 do
    begin
      ModLinkState.AssignFrom(sg.ModConnections_OLD.ModLinks[c3]^);

      if (ModLinkState.Source <> TModSource.None) or (ModLinkState.Via <> TModSource.None) then
      begin
        ModLinkNode := SampleGroupNode.NodeNew('ModLink');

        SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'UniqueID');
        SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'Source');
        SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'Via');
        SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'Amount');
        SaveObjectPropertyToXML(ModLinkNode, ModLinkState, 'Offset');
      end;
    end;

    StepSeqNode := SampleGroupNode.NodeNew('StepSeq1');
    for c3 := 0 to kMaxStepSequencerLength-1 do
    begin
      StepSeqNode.NodeNew('StepValue').ValueUnicode := DataIO_FloatToStr(sg.VoiceParameters.Seq1StepValue[c3]);
    end;

    StepSeqNode := SampleGroupNode.NodeNew('StepSeq2');
    for c3 := 0 to kMaxStepSequencerLength-1 do
    begin
      StepSeqNode.NodeNew('StepValue').ValueUnicode := DataIO_FloatToStr(sg.VoiceParameters.Seq2StepValue[c3]);
    end;
  end;
end;

procedure TLucidityStatemanager.ReadStateFromXML(var XML: TNativeXML);
var
  NodeValue : string;
  c1, c2 : integer;
  RootNode : TXMLNode;
  aNode : TXmlNode;
  FileVersion : integer;

  sgIntF : IKeyGroup;
  sg     : TKeyGroup;

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
  ModLinkNode : TXmlNode;
  StepSeqNode : TXmlNode;

  RegionLoadInfo : TRegionLoadInfo;
  ModLinkState : TModLinkLoadInfo;
  c3: Integer;

  Index : integer;
  TempModLink : TModLink_OLD;
  TargetModLink : PModLink_OLD;

  StepValue : single;
begin
  SampleGroupNodeList := TsdNodeList.Create;
  AutoFree(@SampleGroupNodeList);

  RegionNodeList      := TsdNodeList.Create;
  AutoFree(@RegionNodeList);

  RegionLoadInfo := TRegionLoadInfo.Create;
  AutoFree(@RegionLoadInfo);

  ModLinkNodeList := TsdNodeList.Create;
  AutoFree(@ModLinkNodeList);

  StepValuesNodeList := TsdNodeList.Create;
  AutoFree(@StepValuesNodeList);

  ModLinkState := TModLinkLoadInfo.Create;
  AutoFree(@ModLinkState);

  RootNode := Xml.Root;

  aNode := RootNode.FindNode('FileType');
  if (not assigned(aNode)) or (aNode.ValueUnicode <> 'LucidityPatch') then raise ELucidityStateException.Create('File is not a Lucidity Patch file.');

  aNode := RootNode.FindNode('FileVersion');
  if (not assigned(aNode)) then raise ELucidityStateException.Create('File is not a valid Lucidity Patch file.');

  FileVersion := DataIO_StrToInt(aNode.ValueUnicode, -1);
  if FileVersion = -1 then raise ELucidityStateException.Create('File is not a valid Lucidity Patch file.');

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

        //TODO: add a validity check here before attempting to load a new region.
        NewRegion(RegionLoadInfo, sgIntF);
      end;
    end;


    //===== Voice parameters ======
    VoiceParNode := SampleGroupNode.FindNode('VoiceParameters');

    if assigned(VoiceParNode) then
    begin
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'PitchTracking');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'SampleReset');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'VoiceGain');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'VoicePan');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'VoicePitchOne');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'VoicePitchTwo');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'GrainLoop');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'GrainLength');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'GrainRate');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'GrainPosition');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'SamplerLoopBounds');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'SamplerLoopMode');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'OscShape');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'OscPulseWidth');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter1Type');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter2Type');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par1');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter1Par3');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par1');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Filter2Par3');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpAttack');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpHold');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpDecay');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpSustain');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpRelease');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'AmpVelocityDepth');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterAttack');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterHold');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterDecay');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterSustain');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterRelease');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'FilterVelocityDepth');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoShape1');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoShape2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoRate1');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoRate2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoAPar2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'LfoBPar2');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvAAttack');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvADecay');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvAMode');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBAttack');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBDecay');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'ModEnvBMode');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Seq1Clock');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Seq1Direction');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'StepSeq1Length');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Seq2Clock');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'Seq2Direction');
      LoadObjectPropertyFromXML(VoiceParNode, sg.VoiceParameters, 'StepSeq2Length');
    end;


    ModLinkNodeList.Clear;
    SampleGroupNode.FindNodes('ModLink', ModLinkNodeList);

    for c3 := 0 to ModLinkNodeList.Count-1 do
    begin
      ModLinkNode := ModLinkNodeList[c3];

      ModLinkState.Clear;

      LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'UniqueID');
      LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Source');
      LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Via');
      LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Amount');
      LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Offset');

      ModLinkState.SanitiseData; //Call sanitiseData() to ensure all parameters are within valid ranges.

      TargetModLink := sg.ModConnections_OLD.FindModLinkByID(ModLinkState.UniqueID);

      if (assigned(TargetModLink)) then
      begin
        ModLinkState.AssignTo(TempModLink);
        TempModLink.Dest := TargetModLink.Dest;
        sg.ModConnections_OLD.UpdateModLinkByID(TempModLink.UniqueID, @TempModLink);
      end;

    end;


    {
    for c3 := 0 to ModLinkNodeList.Count-1 do
    begin
      ModLinkNode := ModLinkNodeList[c3];

      if ReadNodeValue(ModLinkNode, 'LinkIndex', NodeValue)
        then Index := DataIO_StrToInt(NodeValue, -1)
        else Index := -1;

      // NOTE: For readability, Index values begin at 1. There is no '0' index.

      Index := Index-1; //Minus 1 from the index to go back to zero based array indexing.

      //TODO: Load and save mod links.
      if (Index >= 0) and (Index < kModLinkCount) then
      begin
        ModLinkState.Clear;

        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Source');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Dest');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Via');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Amount');
        LoadObjectPropertyFromXML(ModLinkNode, ModLinkState, 'Offset');

        ModLinkState.AssignTo(TempModLink);
        sg.VoiceParameters.ModLink[Index] := TempModLink;
      end;

    end;
    }

    StepSeqNode := SampleGroupNode.FindNode('StepSeq1');
    if assigned(StepSeqNode) then
    begin
      StepSeqNode.NodesByName('StepValue', StepValuesNodeList);

      for c3 := 0 to StepValuesNodeList.Count-1 do
      begin
        if c3 < kMaxStepSequencerLength then
        begin
          StepValue := DataIO_StrToFloat(StepValuesNodeList[c3].ValueUnicode, 0.5);
          Clamp(StepValue, 0, 1);
          sg.VoiceParameters.Seq1StepValue[c3] := StepValue;
        end;
      end;
    end;


    StepSeqNode := SampleGroupNode.FindNode('StepSeq2');
    if assigned(StepSeqNode) then
    begin
      StepSeqNode.NodesByName('StepValue', StepValuesNodeList);

      for c3 := 0 to StepValuesNodeList.Count-1 do
      begin
        if c3 < kMaxStepSequencerLength then
        begin
          StepValue := DataIO_StrToFloat(StepValuesNodeList[c3].ValueUnicode, 0.5);
          Clamp(StepValue, 0, 1);
          sg.VoiceParameters.Seq2StepValue[c3] := StepValue;
        end;
      end;
    end;


  end;





end;


procedure TLucidityStatemanager.NewRegion(const RegionLoadInfo: TRegionLoadInfo; const SampleGroup: IKeyGroup);
var
  aRegion : TRegion;
begin
  if not assigned(SampleGroup) then raise Exception.Create('SG (sample group interface variable not assigned.');


  // TODO: There's a few things needing to be done here.
  // - delayed sample loading. (don't load the sample immediately.)
  // - check the RegionLoadInfo is valid and will load a correctly configured region.
  // - the delayed sample loading will also need to handle missing files.

  aRegion := TRegion.Create;

  aRegion.Sample.LoadFromFile(RegionLoadInfo.SampleFileName);
  aRegion.ZeroCrossings.CalcZeroCrossingData(aRegion.Sample);

  aRegion.KeyGroup := SampleGroup;

  // TODO: the following three properties are new and have just been added here.
  // I'm not yet sure what will be the best way to use them.
  aRegion.Properties^.SampleDataLoaded := true;
  aRegion.Properties^.IsSampleError    := false;
  aRegion.Properties^.ErrorMessage     := '';
  //============================================================================

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

  Plugin.SampleMap.AddRegion(aRegion);
end;






{ TModLinkLoadInfo }

procedure TModLinkLoadInfo.AssignFrom(const aSource: TModLink_OLD);
begin
  self.UniqueID := aSource.UniqueID;
  self.Source   := aSource.Source;
  self.Dest     := aSource.Dest;
  self.Via      := aSource.Via;
  self.Amount   := aSource.Amount;
  self.Offset   := aSource.Offset;
end;

procedure TModLinkLoadInfo.AssignTo(var aDest: TModLink_OLD);
begin
  aDest.UniqueID := self.UniqueID;
  aDest.Source   := self.Source;
  aDest.Dest     := self.Dest;
  aDest.Via      := self.Via;
  aDest.Amount   := self.Amount;
  aDest.Offset   := self.Offset;
end;

procedure TModLinkLoadInfo.Clear;
begin
  self.UniqueID := '';
  self.Source   := TModSource.None;
  self.Via      := TModSource.None;
  self.Dest     := TModDest.None;
  self.Amount   := 0;
  self.Offset   := 0;
end;

procedure TModLinkLoadInfo.SanitiseData;
begin
  Clamp(self.fAmount, -1, 1);
  Clamp(self.fOffset, -1, 1);
end;

{ TRegionLoadInfo }

procedure TRegionLoadInfo.SanitiseData;
begin
  //TODO: add range checks.
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



end.

