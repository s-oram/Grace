unit Lucidity.SfzOpcodeConversion;

interface

uses
  uLucidityEnums,
  SfzParser.SfzOpcodes;

//================================================================================
//                HIGH LEVEL METHODS
//================================================================================


// converts a SFZ Opcode to a Lucidity patch value string.
// This method takes the SFZ value, ensures the source is using a valid range,
// converts to a correctly scaled Lucidity Patch value.
function ConvertOpcodeToPatchValue(const Opcode : TSfzOpcode; const OpcodeValue : string):string;



//================================================================================
//                LOW LEVEL METHODS
//================================================================================

// These methods convert SFZ opcodes to Lucidity parameter values...
function OpcodeToTriggerMode(const Value : string): TKeyGroupTriggerMode;

function OpcodeToInteger(const Value : string):integer; overload;
function OpcodeToInteger(const Value : string; const MinValue, MaxValue:integer):integer; overload;
function OpcodeToFloat(const Value : string):single; overload;
function OpcodeToFloat(const Value : string; const MinValue, MaxValue:integer):single; overload;

implementation

uses
  Math,
  VamLib.Utils,
  SysUtils;

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



function ConvertOpcodeToPatchValue(const Opcode : TSfzOpcode; const OpcodeValue : string):string;
var
  xInt : integer;
  xFloat : single;
  TriggerMode : TKeyGroupTriggerMode;
begin
  {$Hints Off}
  // Set the xInt and xFloat to some nonsense values.
  // Hopefully this will help quickly expose errors
  // if they are used in the wrong location
  xInt := Low(Integer);
  xFloat := NaN;
  // NOTE: Hints are off because these variable assignments *shouldn't* actually
  // show up as used. "never used" is exactly what we want.
  {$Hints On}

  // set Result to a default value.
  result := '';

  // do the conversion.
  case Opcode of
    TSfzOpcode.Unknown: ;
    TSfzOpcode.sample:
    begin
      // This opcode defines which sample file the region will play.
      // The value of this opcode is the filename of the sample file,
      // including the extension. The filename must be stored in the
      // same folder where the definition file is, or specified relatively to it.
      result := OpcodeValue;
    end;

    TSfzOpcode.lochan:
    begin

    end;

    TSfzOpcode.hichan:
    begin

    end;
    //==========================================================================
    // == lokey, hikey, key ==
    // If a note equal to or higher than lokey AND equal to or lower than hikey is played, the region will play.
    // lokey and hikey can be entered in either MIDI note numbers (0 to 127) or in MIDI note names (C-1 to G9)
    // The key opcode sets lokey, hikey and pitch_keycenter to the same note.
    // range:
    //   0 to 127
    //   C-1 to G9
    // TODO:HIGH notice the SFZ key range. I need to write a function
    // to translate SFZ midi key names to integers. My routine in VamLib.Utils
    // isn't correct WRT the SFZ format.
    TSfzOpcode.lokey:
    begin
       xInt := OpcodeToInteger(OpcodeValue, 0, 127);
       result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.hikey:
    begin
      xInt := OpcodeToInteger(OpcodeValue, 0, 127);
       result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.key:
    begin
      xInt := OpcodeToInteger(OpcodeValue, 0, 127);
      result := DataIO_IntToStr(xInt);
    end;
    //==========================================================================
    // If a note with velocity value equal to or higher than lovel AND equal
    // to or lower than hivel is played, the region will play.
    // range: 0-127
    TSfzOpcode.lovel:
    begin
      xInt := OpcodeToInteger(OpcodeValue, 0, 127);
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.hivel:
    begin
      xInt := OpcodeToInteger(OpcodeValue, 0, 127);
      result := DataIO_IntToStr(xInt);
    end;
    //==========================================================================
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
    TSfzOpcode.sample_end:
    begin
      // === SFZ Import Notes ===
      // The endpoint of the sample, in sample units.
      // The player will reproduce the whole sample if end is not specified.
      // If end value is -1, the sample will not play. Marking a region end
      // with -1 can be used to use a silent region to turn off other
      // regions by using the group and off_by opcodes.
      xInt := OpcodeToInteger(OpcodeValue);
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.count: ;
    TSfzOpcode.loop_mode:
    begin
      TriggerMode := OpcodeToTriggerMode(OpcodeValue);
      result := TKeyGroupTriggerModeHelper.ToUnicodeString(TriggerMode);
    end;
    TSfzOpcode.loop_start:
    begin
      // The loop start point, in samples.
      // If loop_start is not specified and the sample has a loop defined,
      // the sample start point will be used.
      // If loop_start is specified, it will overwrite the loop start point
      // defined in the sample.
      // This opcode will not have any effect if loopmode is set to no_loop.
      // SFZ Range: 0 to 4 Gb (4294967296)
      xInt := OpcodeToInteger(OpcodeValue);
      if xInt <0 then xInt := 0;
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.loop_end:
    begin
      // The loop end point, in samples. This opcode will not have any effect
      // if loopmode is set to no_loop.
      // If loop_end is not specified and the sample have a loop defined, the
      // sample loop end point will be used.
      // If loop_end is specified, it will overwrite the loop end point defined
      // in the sample.
      xInt := OpcodeToInteger(OpcodeValue);
      if xInt <0 then xInt := 0;
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.sync_beats: ;
    TSfzOpcode.sync_offset: ;
    TSfzOpcode.transpose:
    begin
      // === SFZ Import Notes ===
      // The transposition value for this region which will be applied to the sample.
      // (I think it's in semitones but the documentation doesn't say)
      // == Lucidity Import Notes ==
      // Lucidty format units are *semitones*.
      xInt := OpcodeToInteger(OpcodeValue, -127, 127);
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.tune:
    begin
      // === SFZ Import Notes ===
      // The fine tuning for the sample, in cents. Range is *ERROR* semitone,
      // from -100 to 100. Only negative values must be prefixed with sign.
      // == Lucidity Import Notes ==
      // Lucidty format units are *cents*.
      xInt := OpcodeToInteger(OpcodeValue, -100, 100);
      result := DataIO_IntToStr(xInt);
    end;
    TSfzOpcode.pitch_keycenter:
    begin
      // Root key for the sample.
      // SFZ range: -127 to 127.
      // Lucidity range is 0 to 127
      xInt := OpcodeToInteger(OpcodeValue, 0, 127);
      result := DataIO_IntToStr(xInt);
    end;
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
    TSfzOpcode.volume:
    begin
      // === SFZ Import Notes ===
      // The volume for the region, in decibels. -144 to 6 dB
      // == Lucidity Import Notes ==
      // Lucidty format units are *decibels*.
      xFloat := OpcodeToFloat(OpcodeValue, -144, 6);
      result := DataIO_FloatToStr(xFloat);
    end;
    TSfzOpcode.pan:
    begin
      // === SFZ Import Notes ===
      // The panoramic position for the region.
      // If a mono sample is used, pan value defines the position in the stereo
      // image where the sample will be placed.
      // When a stereo sample is used, the pan value the relative amplitude of one channel respect the other.
      // == Lucidity Import Notes ==
      // Lucidty format units are *decibels*.
      xFloat := OpcodeToFloat(OpcodeValue, -100, 100);
      result := DataIO_FloatToStr(xFloat);
    end;
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
  else
    raise Exception.Create('Opcode type not handled.');
  end;
end;

end.
