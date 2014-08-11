unit Lucidity.Sfz;

//   SFZ Opcode documentation
//   http://www.cakewalk.com/DevXchange/article.aspx?aid=108

interface

uses
  uLucidityEnums,
  NativeXML, SfzParser, Contnrs;

type
//================================================================================
//    PUBLIC - for external use
//================================================================================

  TSfzImporter = class
  private
    Parser : TSfzParser;
    RootNode : TXmlNode;
    CurrentGroup  : TXmlNode;
    CurrentRegion : TXmlNode;
    GroupCount : integer;
    SupportedOpcodeList : TObjectList;
    procedure Event_OnOpcode(Sender : TObject; OpcodeName, OpcodeValue : string);
    procedure Event_OnRegionStart(Sender : TObject);
    procedure Event_OnRegionEnd(Sender : TObject);
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConvertFile(SourceFileName : string; out Dest : TNativeXML);
  end;



//================================================================================
//    PRIVATE - for internal use
//================================================================================

// These methods convert SFZ opcodes to Lucidity parameter values...
function OpcodeToTriggerMode(const Value : string): TKeyGroupTriggerMode;

function OpcodeToInteger(const Value : string):integer; overload;
function OpcodeToInteger(const Value : string; const MinValue, MaxValue:integer):integer; overload;
function OpcodeToFloat(const Value : string):single; overload;
function OpcodeToFloat(const Value : string; const MinValue, MaxValue:integer):single; overload;


implementation

uses
  VamLib.Utils,
  SysUtils,
  NativeXmlEx,
  uAutoFree;

function OpcodeToTriggerMode(const Value : string): TKeyGroupTriggerMode;
begin
  if SameText(Value, 'no_loop')         then exit(TKeyGroupTriggerMode.LoopOff);
  if SameText(Value, 'one_shot')        then exit(TKeyGroupTriggerMode.OneShot);
  if SameText(Value, 'loop_continuous') then exit(TKeyGroupTriggerMode.LoopContinuous);
  if SameText(Value, 'loop_sustain')    then exit(TKeyGroupTriggerMode.LoopSustain);

  // If we've made it this far, the value isn't a valid SFZ opcode.
  raise EConvertError.Create('Value is not an integer.');
end;

function OpcodeToInteger(const Value : string):integer;
var
  x : integer;
begin
  if IsMidiKeyNameString(Value, x) then
  begin
    result := x;
  end else
  if TryStrToInt(Value, x) then
  begin
    result := x;
  end else
  begin
    raise EConvertError.Create('Value is not an integer.');
  end;
end;

function OpcodeToInteger(const Value : string; const MinValue, MaxValue:integer):integer; overload;
var
  x : integer;
begin
  if IsMidiKeyNameString(Value, x) then
  begin
    if x < MinValue then x := MinValue;
    if x > MaxValue then x := MaxValue;
    result := x;
  end else
  if TryStrToInt(Value, x) then
  begin
    if x < MinValue then x := MinValue;
    if x > MaxValue then x := MaxValue;
    result := x;
  end else
  begin
    raise EConvertError.Create('Value is not an integer.');
  end;
end;

function OpcodeToFloat(const Value : string):single;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  try
    result := StrToFloat(Value, fs)
  except
    raise EConvertError.Create('Value is not a valid float.');
  end;
end;

function OpcodeToFloat(const Value : string; const MinValue, MaxValue:integer):single; overload;
var
  x : single;
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  try
    x := StrToFloat(Value, fs);
    if x < MinValue then x := MinValue;
    if x > MaxValue then x := MaxValue;
    result := x;
  except
    raise EConvertError.Create('Value is not a valid float.');
  end;
end;

{ TSfzImporter }

constructor TSfzImporter.Create;
begin
  Parser := TSfzParser.Create;
  Parser.OnRegionStart := Event_OnRegionStart;
  Parser.OnRegionEnd   := Event_OnRegionEnd;
  Parser.OnOpcode      := Event_OnOpcode;
end;

destructor TSfzImporter.Destroy;
begin
  SupportedOpcodeList.Free;
  Parser.Free;
  inherited;
end;

procedure TSfzImporter.ConvertFile(SourceFileName: string; out Dest: TNativeXML);
begin
  if assigned(Dest) then raise Exception.Create('Dest should not be assigned.');

  Dest := TNativeXML.CreateName('root');

  RootNode      := nil;
  CurrentRegion := nil;
  CurrentGroup  := nil;
  GroupCount    := 0;

  if assigned(Dest.Root) then
  begin
    RootNode := Dest.Root;

    Dest.Root.Name := 'root';
    Dest.Root.NodeNew('PatchFileType').ValueUnicode := 'LucidityPatchFile';
    Dest.Root.NodeNew('PatchFileFormatVersion').ValueUnicode := '1';

    NodeWiz(Dest.Root).CreateNode('GlobalParameters/VoiceMode').ValueUnicode := 'Poly';
    NodeWiz(Dest.Root).CreateNode('GlobalParameters/VoiceGlide').ValueUnicode := '0';

    Parser.ParseFile(SourceFileName);

    //do some further post-processing after parsing the raw file..

    // - Check the filenames are relative. put them into a form Lucidity can understand.
    // - check that all regions have region bounds. (low key, hi key, low vel, high vel, root note etc).
  end;
end;

procedure TSfzImporter.Event_OnRegionStart(Sender: TObject);
begin
  if not assigned(CurrentGroup) then
  begin
    CurrentGroup := NodeWiz(RootNode).CreateNode('SampleGroup');
    CurrentGroup.NodeNew('Name').ValueUnicode := 'Group ' + IntToStr(GroupCount + 1);

    //== default voice parameters ==
    NodeWiz(CurrentGroup).CreateNode('VoiceParameters/SamplePlaybackType').ValueUnicode := 'NoteSampler'; //TODO:MED delete this. Node isn't needed.

    inc(GroupCount);
  end;

  CurrentRegion := NodeWiz(CurrentGroup).CreateNode('Region');
end;


procedure TSfzImporter.Event_OnRegionEnd(Sender: TObject);
begin
  CurrentRegion := nil;
end;

procedure TSfzImporter.Event_OnOpcode(Sender : TObject; OpcodeName, OpcodeValue : string);
var
  DataInt   : integer;
  DataText  : string;
  DataFloat : single;
  TargetNode : TXmlNode;
  TriggerMode : TKeyGroupTriggerMode;
begin
  if not assigned(CurrentRegion) then exit;

  //DataInt   := 0;
  DataText  := '';
  //DataFloat := 0;
  //TargetNode := nil;

  //   SFZ Opcode documentation
  //   http://www.cakewalk.com/DevXchange/article.aspx?aid=108



  {
  AddTextOpcode('sample');
  AddIntegerOpcode('pitch_keycenter', 0, 127); //actual range is -127..127
  AddIntegerOpcode('lokey', 0, 127);
  AddIntegerOpcode('hikey', 0, 127);
  AddIntegerOpcode('key', 0, 127);
  AddIntegerOpcode('lovel', 0, 127);
  AddIntegerOpcode('hivel', 0, 127);
  AddTextOpcode('trigger'); //attack, release, first, legato,
  AddIntegerOpcode('group', 0, kMaxInt);
  AddTextOpcode('loop_mode');
  AddIntegerOpcode('loop_start', 0, kMaxInt); //should be unsigned.
  AddIntegerOpcode('loop_end', 0, kMaxInt);   //should be unsigned.

  AddFloatOpcode('volume', -144, 6); //db.
  AddFloatOpcode('pan', -100, 100); //%


  // Amplitude envelope
  AddFloatOpcode('ampeg_attack', 0, 100); //seconds
  AddFloatOpcode('ampeg_hold', 0, 100); //seconds
  AddFloatOpcode('ampeg_decay', 0, 100); //seconds
  AddFloatOpcode('ampeg_sustain', 0, 100); //seconds
  AddFloatOpcode('ampeg_release', 0, 100); //seconds

  // filter envelope
  AddFloatOpcode('fileg_attack', 0, 100); //seconds
  AddFloatOpcode('fileg_hold', 0, 100); //seconds
  AddFloatOpcode('fileg_decay', 0, 100); //seconds
  AddFloatOpcode('fileg_sustain', 0, 100); //seconds
  AddFloatOpcode('fileg_release', 0, 100); //seconds
  }


  try
    if SameText(OpcodeName, 'sample') then
    begin
      DataText := OpcodeValue;
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('SampleProperties/SampleFileName').ValueUnicode := DataText;
    end;

    if SameText(OpcodeName, 'lokey') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue, 0, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowNote').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'hikey') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue, 0, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighNote').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'lovel') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue, 0, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowVelocity').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'hivel') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue, 0, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighVelocity').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'pitch_keycenter') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue, 0, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/RootNote').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'end') then
    begin
      // === SFZ Import Notes ===
      // The endpoint of the sample, in sample units.
      // The player will reproduce the whole sample if end is not specified.
      // If end value is -1, the sample will not play. Marking a region end
      // with -1 can be used to use a silent region to turn off other
      // regions by using the group and off_by opcodes.
      DataInt := OpcodeToInteger(OpcodeValue);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleEnd').ValueUnicode := DataIO_IntToStr(DataInt);
    end;



    if SameText(OpcodeName, 'loop_start') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LoopStart').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'loop_end') then
    begin
      DataInt := OpcodeToInteger(OpcodeValue);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LoopEnd').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'transpose') then
    begin
      // === SFZ Import Notes ===
      // The transposition value for this region which will be applied to the sample.
      // (I think it's in semitones but the documentation doesn't say)
      // == Lucidity Import Notes ==
      // Lucidty format units are *semitones*.
      DataInt := OpcodeToInteger(OpcodeValue, -127, 127);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleTune').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'tune') then
    begin
      // === SFZ Import Notes ===
      // The fine tuning for the sample, in cents. Range is *ERROR* semitone,
      // from -100 to 100. Only negative values must be prefixed with sign.
      // == Lucidity Import Notes ==
      // Lucidty format units are *cents*.
      DataInt := OpcodeToInteger(OpcodeValue, -100, 100);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleFine').ValueUnicode := DataIO_IntToStr(DataInt);
    end;

    if SameText(OpcodeName, 'volume') then
    begin
      // === SFZ Import Notes ===
      // The volume for the region, in decibels. -144 to 6 dB
      // == Lucidity Import Notes ==
      // Lucidty format units are *decibels*.
      DataFloat := OpcodeToFloat(OpcodeValue, -144, 6);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleVolume').ValueUnicode := DataIO_FloatToStr(DataFloat);
    end;

    if SameText(OpcodeName, 'pan') then
    begin
      // === SFZ Import Notes ===
      // The panoramic position for the region.
      // If a mono sample is used, pan value defines the position in the stereo
      // image where the sample will be placed.
      // When a stereo sample is used, the pan value the relative amplitude of one channel respect the other.
      // == Lucidity Import Notes ==
      // Lucidty format units are *decibels*.
      DataFloat := OpcodeToFloat(OpcodeValue, -100, 100);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SamplePan').ValueUnicode := DataIO_FloatToStr(DataFloat);
    end;





    if SameText(OpcodeName, 'loop_mode') then
    begin
      TriggerMode := OpcodeToTriggerMode(OpcodeValue);
      TargetNode := CurrentRegion;
      NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/SamplerTriggerMode').ValueUnicode := TKeyGroupTriggerModeHelper.ToUnicodeString(TriggerMode);
    end;



  except
    on EConvertError do {nothing};
    else
      raise;
  end;



end;





end.
