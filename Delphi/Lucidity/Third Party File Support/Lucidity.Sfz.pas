unit Lucidity.Sfz;

//   SFZ Opcode documentation
//   http://www.cakewalk.com/DevXchange/article.aspx?aid=108

interface

uses
  NativeXML, SfzParser, Contnrs;

type
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

    procedure ConvertFile(SourceFileName : string; var Dest : TNativeXML);
  end;


implementation

uses
  eeFunctions,
  SysUtils,
  NativeXmlEx,
  uAutoFree;

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

procedure TSfzImporter.ConvertFile(SourceFileName: string; var Dest: TNativeXML);
begin
  RootNode      := nil;
  CurrentRegion := nil;
  CurrentGroup  := nil;
  GroupCount    := 0;

  Dest.Clear;

  if not assigned(Dest.Root) then
  begin
    Dest.NodeNew('root');
  end;

  if assigned(Dest.Root) then
  begin
    RootNode := Dest.Root;

    Dest.Root.Name := 'root';
    Dest.Root.NodeNew('FileType').ValueUnicode := 'LucidityPatch';
    Dest.Root.NodeNew('FileVersion').ValueUnicode := '1';

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
    NodeWiz(CurrentGroup).CreateNode('VoiceParameters/SamplePlaybackType').ValueUnicode := 'NoteSampler';


    inc(GroupCount);
  end;

  CurrentRegion := NodeWiz(CurrentGroup).CreateNode('Region');
end;


procedure TSfzImporter.Event_OnRegionEnd(Sender: TObject);
begin
  CurrentRegion := nil;
end;

procedure TSfzImporter.Event_OnOpcode(Sender : TObject; OpcodeName, OpcodeValue : string);
  function ConvertOpcode(Value:string; MinValue, MaxValue:integer):integer; overload;
  var
    x : integer;
  begin
    x := DataIO_StrToInt(Value, MinValue);
    if x < MinValue then x := MinValue;
    if x > MaxValue then x := MaxValue;
    result := x;
  end;
  function ConvertOpcode(Value:string; MinValue, MaxValue:single):single; overload;
  var
    x : single;
  begin
    x := DataIO_StrToFloat(Value, MinValue);
    if x < MinValue then x := MinValue;
    if x > MaxValue then x := MaxValue;
    result := x;
  end;
var
  DataInt   : integer;
  DataText  : string;
  //DataFloat : single;
  TargetNode : TXmlNode;
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



  if SameText(OpcodeName, 'sample') then
  begin
    DataText := OpcodeValue;
    TargetNode := CurrentRegion;
    NodeWiz(TargetNode).FindOrCreateNode('SampleProperties/SampleFileName').ValueUnicode := DataText;
  end;

  if SameText(OpcodeName, 'lokey') then
  begin
    DataInt := ConvertOpcode(OpcodeValue, 0, 127);
    TargetNode := CurrentRegion;
    NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowNote').ValueUnicode := DataIO_IntToStr(DataInt);
  end;

  if SameText(OpcodeName, 'hikey') then
  begin
    DataInt := ConvertOpcode(OpcodeValue, 0, 127);
    TargetNode := CurrentRegion;
    NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighNote').ValueUnicode := DataIO_IntToStr(DataInt);
  end;

  if SameText(OpcodeName, 'lovel') then
  begin
    DataInt := ConvertOpcode(OpcodeValue, 0, 127);
    TargetNode := CurrentRegion;
    NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowVelocity').ValueUnicode := DataIO_IntToStr(DataInt);
  end;

  if SameText(OpcodeName, 'hivel') then
  begin
    DataInt := ConvertOpcode(OpcodeValue, 0, 127);
    TargetNode := CurrentRegion;
    NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighVelocity').ValueUnicode := DataIO_IntToStr(DataInt);
  end;



end;





end.
