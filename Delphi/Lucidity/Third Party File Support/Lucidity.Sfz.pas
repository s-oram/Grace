unit Lucidity.Sfz;

//   SFZ Opcode documentation
//   http://www.cakewalk.com/DevXchange/article.aspx?aid=108

interface

uses
  SfzParser, SfzParser.SfzOpcodes,
  uLucidityEnums,
  NativeXML,
  Contnrs;

type
//================================================================================
//    PUBLIC - for external use
//================================================================================

  TSfzGroupLoadData = record
  public
    IsCutoffSet : boolean;
    procedure Reset;
  end;

  TSfzImporter = class
  private
    Parser : TSfzParser;
    RootNode : TXmlNode;
    CurrentGroup  : TXmlNode;
    CurrentRegion : TXmlNode;
    GroupCount : integer;
    SupportedOpcodeList : TObjectList;
    GroupLoadData : TSfzGroupLoadData;
    procedure Event_OnGroupStart(Sender : TObject);
    procedure Event_OnGroupEnd(Sender : TObject);
    procedure Event_OnGroupOpcode(Sender : TObject; Opcode : TSfzOpcode; OpcodeValue : string);
    procedure Event_OnRegionStart(Sender : TObject);
    procedure Event_OnRegionEnd(Sender : TObject);
    procedure Event_OnRegionOpcode(Sender : TObject; Opcode : TSfzOpcode; OpcodeValue : string);
  protected
    procedure GenerateModMatrixPatchData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConvertFile(SourceFileName : string; out Dest : TNativeXML);
  end;

implementation

uses
  VamLib.Utils,
  SysUtils,
  NativeXmlEx,
  Lucidity.SfzOpcodeConversion;


{ TSfzImporter }

constructor TSfzImporter.Create;
begin
  Parser := TSfzParser.Create;

  Parser.OnGroupStart  := Event_OnGroupStart;
  Parser.OnGroupEnd    := Event_OnGroupEnd;
  Parser.OnGroupOpcode := Event_OnGroupOpcode;

  Parser.OnRegionStart  := Event_OnRegionStart;
  Parser.OnRegionEnd    := Event_OnRegionEnd;
  Parser.OnRegionOpcode := Event_OnRegionOpcode;
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
    Dest.Root.NodeNew('PatchFileFormatVersion').ValueUnicode := '2';

    NodeWiz(Dest.Root).CreateNode('GlobalParameters/VoiceMode').ValueUnicode := 'Poly';
    NodeWiz(Dest.Root).CreateNode('GlobalParameters/VoiceGlide').ValueUnicode := '0';

    Parser.ParseFile(SourceFileName);

    //do some further post-processing after parsing the raw file..

    // - Check the filenames are relative. put them into a form Lucidity can understand.
    // - check that all regions have region bounds. (low key, hi key, low vel, high vel, root note etc).
  end;
end;


procedure TSfzImporter.Event_OnGroupStart(Sender: TObject);
begin
  if not assigned(CurrentGroup) then
  begin
    GroupLoadData.Reset;
    CurrentGroup := NodeWiz(RootNode).CreateNode('SampleGroup');
    CurrentGroup.NodeNew('Name').ValueUnicode := 'Group ' + IntToStr(GroupCount + 1);
    inc(GroupCount);

    //== default voice parameters ==
    //NodeWiz(CurrentGroup).CreateNode('VoiceParameters/SamplePlaybackType').ValueUnicode := 'NoteSampler'; //TODO:MED delete this. Node isn't needed.
    NodeWiz(CurrentGroup).CreateNode('VoiceParameters');
  end;
end;

procedure TSfzImporter.Event_OnGroupEnd(Sender: TObject);
var
  TargetNode : TXmlNode;
begin
  //****************************************************************************
  // Do some final adjustments here...
  TargetNode := CurrentGroup;


  //===============================================================================
  if (GroupLoadData.IsCutoffSet) then
  begin
    // The filter type value is optional for SFZ files.
    if not NodeWiz(TargetNode).Exists('VoiceParameters/Filter1Type') then
    begin
      NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Type').ValueUnicode := TFilterTypeHelper.ToUnicodeString(TFiltertype.ft2PoleLowPass);
    end;

    // Set filter resonance if not set.
    if not NodeWiz(TargetNode).Exists('VoiceParameters/Filter1Par2') then
    begin
      NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Par2').ValueUnicode := '0';
    end;

    // Set filter gain to 0.
    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Par3').ValueUnicode := '0';
  end;
  //===============================================================================

  // Amp envelope Sustain
  if not NodeWiz(TargetNode).Exists('VoiceParameters/AmpSustain') then
  begin
    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpSustain').ValueUnicode := '1';
  end;

  // Filter envelope sustain.
  if not NodeWiz(TargetNode).Exists('VoiceParameters/ModSustain') then
  begin
    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModSustain').ValueUnicode := '1';
  end;


  //****************************************************************************

  GenerateModMatrixPatchData;


  //****************************************************************************
  CurrentGroup := nil;
end;

procedure TSfzImporter.Event_OnGroupOpcode(Sender: TObject; Opcode : TSfzOpcode; OpcodeValue: string);
var
  TargetNode : TXmlNode;
begin
  if not assigned(CurrentGroup) then exit;
  TargetNode := CurrentGroup;

  case Opcode of
    TSfzOpcode.lokey: ;
    TSfzOpcode.hikey: ;
    TSfzOpcode.key: ;
    TSfzOpcode.lovel: ;
    TSfzOpcode.hivel: ;
    TSfzOpcode.lobend: ;
    TSfzOpcode.hibend: ;
    TSfzOpcode.lochanaft: ;
    TSfzOpcode.hichanaft: ;
    TSfzOpcode.lopolyaft: ;
    TSfzOpcode.hipolyaft: ;
    TSfzOpcode.lorand: ;
    TSfzOpcode.hirand: ;
    TSfzOpcode.lobpm: ;
    TSfzOpcode.hibpm: ;
    TSfzOpcode.seq_length: ;
    TSfzOpcode.seq_position: ;
    TSfzOpcode.sw_lokey: ;
    TSfzOpcode.sw_hikey: ;
    TSfzOpcode.sw_last: ;
    TSfzOpcode.sw_down: ;
    TSfzOpcode.sw_up: ;
    TSfzOpcode.sw_previous: ;
    TSfzOpcode.sw_vel: ;
    TSfzOpcode.trigger: ;
    TSfzOpcode.group: ;
    TSfzOpcode.off_by: ;
    TSfzOpcode.off_mode: ;
    TSfzOpcode.on_loccN: ;
    TSfzOpcode.on_hiccN: ;
    TSfzOpcode.delay: ;
    TSfzOpcode.delay_random: ;
    TSfzOpcode.delay_ccN: ;
    TSfzOpcode.offset: ;
    TSfzOpcode.offset_random: ;
    TSfzOpcode.offset_ccN: ;
    TSfzOpcode.sample_end: ;
    TSfzOpcode.count: ;
    TSfzOpcode.loop_mode: ;
    TSfzOpcode.loop_start: ;
    TSfzOpcode.loop_end: ;
    TSfzOpcode.sync_beats: ;
    TSfzOpcode.sync_offset: ;
    TSfzOpcode.transpose: ; //TODO:HIGH
    TSfzOpcode.tune: ; //TODO:HIGH
    TSfzOpcode.pitch_keycenter: ;
    TSfzOpcode.pitch_keytrack: ;
    TSfzOpcode.pitch_veltrack: ;
    TSfzOpcode.pitch_random: ;
    TSfzOpcode.bend_up: ;
    TSfzOpcode.bend_down: ;
    TSfzOpcode.bend_step: ;
    TSfzOpcode.pitcheg_delay: ;
    TSfzOpcode.pitcheg_start: ;
    TSfzOpcode.pitcheg_attack: ;
    TSfzOpcode.pitcheg_hold: ;
    TSfzOpcode.pitcheg_decay: ;
    TSfzOpcode.pitcheg_sustain: ;
    TSfzOpcode.pitcheg_release: ;
    TSfzOpcode.pitcheg_depth: ;
    TSfzOpcode.pitcheg_vel2delay: ;
    TSfzOpcode.pitcheg_vel2attack: ;
    TSfzOpcode.pitcheg_vel2hold: ;
    TSfzOpcode.pitcheg_vel2decay: ;
    TSfzOpcode.pitcheg_vel2sustain: ;
    TSfzOpcode.pitcheg_vel2release: ;
    TSfzOpcode.pitcheg_vel2depth: ;
    TSfzOpcode.pitchlfo_delay: ;
    TSfzOpcode.pitchlfo_fade: ;
    TSfzOpcode.pitchlfo_freq: ;
    TSfzOpcode.pitchlfo_depth: ;
    TSfzOpcode.pitchlfo_depthccN: ;
    TSfzOpcode.pitchlfo_depthchanaft: ;
    TSfzOpcode.pitchlfo_depthpolyaft: ;
    TSfzOpcode.pitchlfo_freqccN: ;
    TSfzOpcode.pitchlfo_freqchanaft: ;
    TSfzOpcode.pitchlfo_freqpolyaft: ;
    TSfzOpcode.fil_type: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Type').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.cutoff:
    begin
      GroupLoadData.IsCutoffSet := true;
      NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Par1').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    end;
    TSfzOpcode.cutoff_ccN: ;
    TSfzOpcode.cutoff_chanaft: ;
    TSfzOpcode.cutoff_polyaft: ;
    TSfzOpcode.resonance:    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Par2').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fil_keytrack: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1KeyFollow').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fil_keycenter: ;
    TSfzOpcode.fil_veltrack: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModVelocityDepth').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fil_random: ;
    TSfzOpcode.fileg_delay: ;
    TSfzOpcode.fileg_start: ;
    TSfzOpcode.fileg_attack:  NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModAttack').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_hold:    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModHold').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_decay:   NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModDecay').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_sustain: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModSustain').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_release: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/ModRelease').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_depth:   NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/Filter1Par1/ModAmount2').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.fileg_vel2delay: ;
    TSfzOpcode.fileg_vel2attack: ;
    TSfzOpcode.fileg_vel2hold: ;
    TSfzOpcode.fileg_vel2decay: ;
    TSfzOpcode.fileg_vel2sustain: ;
    TSfzOpcode.fileg_vel2release: ;
    TSfzOpcode.fileg_vel2depth: ;
    TSfzOpcode.fillfo_delay: ;
    TSfzOpcode.fillfo_fade: ;
    TSfzOpcode.fillfo_freq: ;
    TSfzOpcode.fillfo_depth: ;
    TSfzOpcode.fillfo_depthccN: ;
    TSfzOpcode.fillfo_depthchanaft: ;
    TSfzOpcode.fillfo_depthpolyaft: ;
    TSfzOpcode.fillfo_freqccN: ;
    TSfzOpcode.fillfo_freqchanaft: ;
    TSfzOpcode.fillfo_freqpolyaft: ;
    TSfzOpcode.volume: ; //TODO:HIGH
    TSfzOpcode.pan: ;    //TODO:HIGH
    TSfzOpcode.width: ;
    TSfzOpcode.position: ;
    TSfzOpcode.amp_keytrack: ;
    TSfzOpcode.amp_keycenter: ;
    TSfzOpcode.amp_veltrack: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpVelocityDepth').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.amp_velcurve_1: ;
    TSfzOpcode.amp_velcurve_127: ;
    TSfzOpcode.amp_random: ;
    TSfzOpcode.rt_decay: ;
    TSfzOpcode.output: ;
    TSfzOpcode.gain_ccN: ;
    TSfzOpcode.xfin_lokey: ;
    TSfzOpcode.xfin_hikey: ;
    TSfzOpcode.xfout_lokey: ;
    TSfzOpcode.xfout_hikey: ;
    TSfzOpcode.xf_keycurve: ;
    TSfzOpcode.xfin_lovel: ;
    TSfzOpcode.xfin_hivel: ;
    TSfzOpcode.xfout_lovel: ;
    TSfzOpcode.xfout_hivel: ;
    TSfzOpcode.xf_velcurve: ;
    TSfzOpcode.xfin_loccN: ;
    TSfzOpcode.xfin_hiccN: ;
    TSfzOpcode.xfout_loccN: ;
    TSfzOpcode.xfout_hiccN: ;
    TSfzOpcode.xf_cccurve: ;
    TSfzOpcode.ampeg_delay: ;
    TSfzOpcode.ampeg_start: ;
    TSfzOpcode.ampeg_attack:  NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpAttack').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.ampeg_hold:    NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpHold').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.ampeg_decay:   NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpDecay').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.ampeg_sustain: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpSustain').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.ampeg_release: NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/AmpRelease').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.ampeg_vel2delay: ;
    TSfzOpcode.ampeg_vel2attack: ;
    TSfzOpcode.ampeg_vel2hold: ;
    TSfzOpcode.ampeg_vel2decay: ;
    TSfzOpcode.ampeg_vel2sustain: ;
    TSfzOpcode.ampeg_vel2release: ;
    TSfzOpcode.ampeg_delayccN: ;
    TSfzOpcode.ampeg_startccN: ;
    TSfzOpcode.ampeg_attackccN: ;
    TSfzOpcode.ampeg_holdccN: ;
    TSfzOpcode.ampeg_decayccN: ;
    TSfzOpcode.ampeg_sustainccN: ;
    TSfzOpcode.ampeg_releaseccN: ;
    TSfzOpcode.amplfo_delay: ;
    TSfzOpcode.amplfo_fade: ;
    TSfzOpcode.amplfo_freq: ;
    TSfzOpcode.amplfo_depth: ;
    TSfzOpcode.amplfo_depthccN: ;
    TSfzOpcode.amplfo_depthchanaft: ;
    TSfzOpcode.amplfo_depthpolyaft: ;
    TSfzOpcode.amplfo_freqccN: ;
    TSfzOpcode.amplfo_freqchanaft: ;
    TSfzOpcode.amplfo_freqpolyaft: ;
    TSfzOpcode.eq1_freq: ;
    TSfzOpcode.eq2_freq: ;
    TSfzOpcode.eq3_freq: ;
    TSfzOpcode.eq1_freqccN: ;
    TSfzOpcode.eq2_freqccN: ;
    TSfzOpcode.eq3_freqccN: ;
    TSfzOpcode.eq1_vel2freq: ;
    TSfzOpcode.eq2_vel2freq: ;
    TSfzOpcode.eq3_vel2freq: ;
    TSfzOpcode.eq1_bw: ;
    TSfzOpcode.eq2_bw: ;
    TSfzOpcode.eq3_bw: ;
    TSfzOpcode.eq1_bwccN: ;
    TSfzOpcode.eq2_bwccN: ;
    TSfzOpcode.eq3_bwccN: ;
    TSfzOpcode.eq1_gain: ;
    TSfzOpcode.eq2_gain: ;
    TSfzOpcode.eq3_gain: ;
    TSfzOpcode.eq1_gainccN: ;
    TSfzOpcode.eq2_gainccN: ;
    TSfzOpcode.eq3_gainccN: ;
    TSfzOpcode.eq1_vel2gain: ;
    TSfzOpcode.eq2_vel2gain: ;
    TSfzOpcode.eq3_vel2gain: ;
    TSfzOpcode.effect1: ;
    TSfzOpcode.effect2: ;
  end;
end;

procedure TSfzImporter.Event_OnRegionStart(Sender: TObject);
begin
  if assigned(CurrentGroup) then
  begin
    CurrentRegion := NodeWiz(CurrentGroup).CreateNode('Region');
  end;
end;

procedure TSfzImporter.Event_OnRegionEnd(Sender: TObject);
begin
  CurrentRegion := nil;
end;

procedure TSfzImporter.Event_OnRegionOpcode(Sender : TObject; Opcode : TSfzOpcode; OpcodeValue : string);
var
  TargetNode : TXmlNode;
  PatchValue : string;
begin
  if not assigned(CurrentRegion) then exit;

  TargetNode := CurrentRegion;

  case Opcode of
    TSfzOpcode.Unknown: ;
    TSfzOpcode.lochan: ;
    TSfzOpcode.hichan: ;
    TSfzOpcode.sample:          NodeWiz(TargetNode).FindOrCreateNode('SampleProperties/SampleFileName').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.pitch_keycenter: NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/RootNote').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.lokey:           NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowNote').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.hikey:           NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighNote').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.key:
    begin
      PatchValue := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/RootNote').ValueUnicode := PatchValue;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowNote').ValueUnicode := PatchValue;
      NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighNote').ValueUnicode := PatchValue;
    end;
    TSfzOpcode.lovel: NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LowVelocity').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.hivel: NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/HighVelocity').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.lobend: ;
    TSfzOpcode.hibend: ;
    TSfzOpcode.lochanaft: ;
    TSfzOpcode.hichanaft: ;
    TSfzOpcode.lopolyaft: ;
    TSfzOpcode.hipolyaft: ;
    TSfzOpcode.lorand: ;
    TSfzOpcode.hirand: ;
    TSfzOpcode.lobpm: ;
    TSfzOpcode.hibpm: ;
    TSfzOpcode.seq_length: ;
    TSfzOpcode.seq_position: ;
    TSfzOpcode.sw_lokey: ;
    TSfzOpcode.sw_hikey: ;
    TSfzOpcode.sw_last: ;
    TSfzOpcode.sw_down: ;
    TSfzOpcode.sw_up: ;
    TSfzOpcode.sw_previous: ;
    TSfzOpcode.sw_vel: ;
    TSfzOpcode.trigger: ;
    TSfzOpcode.group: ;
    TSfzOpcode.off_by: ;
    TSfzOpcode.off_mode: ;
    TSfzOpcode.on_loccN: ;
    TSfzOpcode.on_hiccN: ;
    TSfzOpcode.delay: ;
    TSfzOpcode.delay_random: ;
    TSfzOpcode.delay_ccN: ;
    TSfzOpcode.offset: ;
    TSfzOpcode.offset_random: ;
    TSfzOpcode.offset_ccN: ;
    TSfzOpcode.sample_end: NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleEnd').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.count: ;
    TSfzOpcode.loop_mode:  NodeWiz(TargetNode).FindOrCreateNode('VoiceParameters/SamplerTriggerMode').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.loop_start: NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LoopStart').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.loop_end:   NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/LoopEnd').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.sync_beats: ;
    TSfzOpcode.sync_offset: ;
    TSfzOpcode.transpose:  NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleTune').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.tune:       NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleFine').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.pitch_keytrack: ;
    TSfzOpcode.pitch_veltrack: ;
    TSfzOpcode.pitch_random: ;
    TSfzOpcode.bend_up: ;
    TSfzOpcode.bend_down: ;
    TSfzOpcode.bend_step: ;
    TSfzOpcode.pitcheg_delay: ;
    TSfzOpcode.pitcheg_start: ;
    TSfzOpcode.pitcheg_attack: ;
    TSfzOpcode.pitcheg_hold: ;
    TSfzOpcode.pitcheg_decay: ;
    TSfzOpcode.pitcheg_sustain: ;
    TSfzOpcode.pitcheg_release: ;
    TSfzOpcode.pitcheg_depth: ;
    TSfzOpcode.pitcheg_vel2delay: ;
    TSfzOpcode.pitcheg_vel2attack: ;
    TSfzOpcode.pitcheg_vel2hold: ;
    TSfzOpcode.pitcheg_vel2decay: ;
    TSfzOpcode.pitcheg_vel2sustain: ;
    TSfzOpcode.pitcheg_vel2release: ;
    TSfzOpcode.pitcheg_vel2depth: ;
    TSfzOpcode.pitchlfo_delay: ;
    TSfzOpcode.pitchlfo_fade: ;
    TSfzOpcode.pitchlfo_freq: ;
    TSfzOpcode.pitchlfo_depth: ;
    TSfzOpcode.pitchlfo_depthccN: ;
    TSfzOpcode.pitchlfo_depthchanaft: ;
    TSfzOpcode.pitchlfo_depthpolyaft: ;
    TSfzOpcode.pitchlfo_freqccN: ;
    TSfzOpcode.pitchlfo_freqchanaft: ;
    TSfzOpcode.pitchlfo_freqpolyaft: ;
    TSfzOpcode.fil_type: ;
    TSfzOpcode.cutoff: ;
    TSfzOpcode.cutoff_ccN: ;
    TSfzOpcode.cutoff_chanaft: ;
    TSfzOpcode.cutoff_polyaft: ;
    TSfzOpcode.resonance: ;
    TSfzOpcode.fil_keytrack: ;
    TSfzOpcode.fil_keycenter: ;
    TSfzOpcode.fil_veltrack: ;
    TSfzOpcode.fil_random: ;
    TSfzOpcode.fileg_delay: ;
    TSfzOpcode.fileg_start: ;
    TSfzOpcode.fileg_attack: ;
    TSfzOpcode.fileg_hold: ;
    TSfzOpcode.fileg_decay: ;
    TSfzOpcode.fileg_sustain: ;
    TSfzOpcode.fileg_release: ;
    TSfzOpcode.fileg_depth: ;
    TSfzOpcode.fileg_vel2delay: ;
    TSfzOpcode.fileg_vel2attack: ;
    TSfzOpcode.fileg_vel2hold: ;
    TSfzOpcode.fileg_vel2decay: ;
    TSfzOpcode.fileg_vel2sustain: ;
    TSfzOpcode.fileg_vel2release: ;
    TSfzOpcode.fileg_vel2depth: ;
    TSfzOpcode.fillfo_delay: ;
    TSfzOpcode.fillfo_fade: ;
    TSfzOpcode.fillfo_freq: ;
    TSfzOpcode.fillfo_depth: ;
    TSfzOpcode.fillfo_depthccN: ;
    TSfzOpcode.fillfo_depthchanaft: ;
    TSfzOpcode.fillfo_depthpolyaft: ;
    TSfzOpcode.fillfo_freqccN: ;
    TSfzOpcode.fillfo_freqchanaft: ;
    TSfzOpcode.fillfo_freqpolyaft: ;
    TSfzOpcode.volume:  NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SampleVolume').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.pan:     NodeWiz(TargetNode).FindOrCreateNode('RegionProperties/SamplePan').ValueUnicode := ConvertOpcodeToPatchValue(Opcode, OpcodeValue);
    TSfzOpcode.width: ;
    TSfzOpcode.position: ;
    TSfzOpcode.amp_keytrack: ;
    TSfzOpcode.amp_keycenter: ;
    TSfzOpcode.amp_veltrack: ;
    TSfzOpcode.amp_velcurve_1: ;
    TSfzOpcode.amp_velcurve_127: ;
    TSfzOpcode.amp_random: ;
    TSfzOpcode.rt_decay: ;
    TSfzOpcode.output: ;
    TSfzOpcode.gain_ccN: ;
    TSfzOpcode.xfin_lokey: ;
    TSfzOpcode.xfin_hikey: ;
    TSfzOpcode.xfout_lokey: ;
    TSfzOpcode.xfout_hikey: ;
    TSfzOpcode.xf_keycurve: ;
    TSfzOpcode.xfin_lovel: ;
    TSfzOpcode.xfin_hivel: ;
    TSfzOpcode.xfout_lovel: ;
    TSfzOpcode.xfout_hivel: ;
    TSfzOpcode.xf_velcurve: ;
    TSfzOpcode.xfin_loccN: ;
    TSfzOpcode.xfin_hiccN: ;
    TSfzOpcode.xfout_loccN: ;
    TSfzOpcode.xfout_hiccN: ;
    TSfzOpcode.xf_cccurve: ;
    TSfzOpcode.ampeg_delay: ;
    TSfzOpcode.ampeg_start: ;
    TSfzOpcode.ampeg_attack: ;
    TSfzOpcode.ampeg_hold: ;
    TSfzOpcode.ampeg_decay: ;
    TSfzOpcode.ampeg_sustain: ;
    TSfzOpcode.ampeg_release: ;
    TSfzOpcode.ampeg_vel2delay: ;
    TSfzOpcode.ampeg_vel2attack: ;
    TSfzOpcode.ampeg_vel2hold: ;
    TSfzOpcode.ampeg_vel2decay: ;
    TSfzOpcode.ampeg_vel2sustain: ;
    TSfzOpcode.ampeg_vel2release: ;
    TSfzOpcode.ampeg_delayccN: ;
    TSfzOpcode.ampeg_startccN: ;
    TSfzOpcode.ampeg_attackccN: ;
    TSfzOpcode.ampeg_holdccN: ;
    TSfzOpcode.ampeg_decayccN: ;
    TSfzOpcode.ampeg_sustainccN: ;
    TSfzOpcode.ampeg_releaseccN: ;
    TSfzOpcode.amplfo_delay: ;
    TSfzOpcode.amplfo_fade: ;
    TSfzOpcode.amplfo_freq: ;
    TSfzOpcode.amplfo_depth: ;
    TSfzOpcode.amplfo_depthccN: ;
    TSfzOpcode.amplfo_depthchanaft: ;
    TSfzOpcode.amplfo_depthpolyaft: ;
    TSfzOpcode.amplfo_freqccN: ;
    TSfzOpcode.amplfo_freqchanaft: ;
    TSfzOpcode.amplfo_freqpolyaft: ;
    TSfzOpcode.eq1_freq: ;
    TSfzOpcode.eq2_freq: ;
    TSfzOpcode.eq3_freq: ;
    TSfzOpcode.eq1_freqccN: ;
    TSfzOpcode.eq2_freqccN: ;
    TSfzOpcode.eq3_freqccN: ;
    TSfzOpcode.eq1_vel2freq: ;
    TSfzOpcode.eq2_vel2freq: ;
    TSfzOpcode.eq3_vel2freq: ;
    TSfzOpcode.eq1_bw: ;
    TSfzOpcode.eq2_bw: ;
    TSfzOpcode.eq3_bw: ;
    TSfzOpcode.eq1_bwccN: ;
    TSfzOpcode.eq2_bwccN: ;
    TSfzOpcode.eq3_bwccN: ;
    TSfzOpcode.eq1_gain: ;
    TSfzOpcode.eq2_gain: ;
    TSfzOpcode.eq3_gain: ;
    TSfzOpcode.eq1_gainccN: ;
    TSfzOpcode.eq2_gainccN: ;
    TSfzOpcode.eq3_gainccN: ;
    TSfzOpcode.eq1_vel2gain: ;
    TSfzOpcode.eq2_vel2gain: ;
    TSfzOpcode.eq3_vel2gain: ;
    TSfzOpcode.effect1: ;
    TSfzOpcode.effect2: ;
  end;
end;


procedure TSfzImporter.GenerateModMatrixPatchData;
var
  ModLinkNode : TXmlNode;
begin
  // Mod Slot 1 = Amp Env
  // Mod Slot 2 = Mod Env
  // Mod Slot 3 = LFO 1
  // Mod Slot 4 = LFO 2
  // Mod Slot 5 = Mod Wheel
  // Mod Slot 6 = MIDI Velocity
  // Mod Slot 7 = Empty
  // Mod Slot 8 = Empty


  //==== Mod Slot 1 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.AmpEnv_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 2 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.ModEnv_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 3 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.Lfo1_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 4 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.Lfo2_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 5 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.Midi_ModWheel_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 6 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.Midi_Velocity_Unipolar);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 7 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';

  //==== Mod Slot 8 =====
  ModLinkNode := NodeWiz(CurrentGroup).CreateNode('ModConnections/ModLink');
  ModLinkNode.NodeNew('ModSource').ValueUnicode         := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('ModVia').ValueUnicode            := TModSourceHelper.ToUnicodeString(TModSource.None);
  ModLinkNode.NodeNew('IsModMute').ValueUnicode         := 'False';
  ModLinkNode.NodeNew('ModSourcePolarity').ValueUnicode := 'Unipolar';
end;





{ TSfzGroupLoadData }

procedure TSfzGroupLoadData.Reset;
begin
  IsCutoffSet := false;
end;

end.
