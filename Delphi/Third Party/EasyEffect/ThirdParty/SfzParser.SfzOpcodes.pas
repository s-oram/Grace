unit SfzParser.SfzOpcodes;

interface

{$SCOPEDENUMS ON}

type
  TSfzOpcode = (
    Unknown,
    sample,
    lochan,
    hichan,
    lokey,
    hikey,
    key,
    lovel,
    hivel,
    lobend,
    hibend,
    lochanaft,
    hichanaft,
    lopolyaft,
    hipolyaft,
    lorand,
    hirand,
    lobpm,
    hibpm,
    seq_length,
    seq_position,
    sw_lokey,
    sw_hikey,
    sw_last,
    sw_down,
    sw_up,
    sw_previous,
    sw_vel,
    trigger,
    group,
    off_by,
    off_mode,
    on_loccN,
    on_hiccN,
    delay,
    delay_random,
    delay_ccN,
    offset,
    offset_random,
    offset_ccN,
    sample_end, //SFZ opcode name is "end" but that conflicts with Delphi.
    count,
    loop_mode,
    loop_start,
    loop_end,
    sync_beats,
    sync_offset,
    transpose,
    tune,
    pitch_keycenter,
    pitch_keytrack,
    pitch_veltrack,
    pitch_random,
    bend_up,
    bend_down,
    bend_step,
    pitcheg_delay,
    pitcheg_start,
    pitcheg_attack,
    pitcheg_hold,
    pitcheg_decay,
    pitcheg_sustain,
    pitcheg_release,
    pitcheg_depth,
    pitcheg_vel2delay,
    pitcheg_vel2attack,
    pitcheg_vel2hold,
    pitcheg_vel2decay,
    pitcheg_vel2sustain,
    pitcheg_vel2release,
    pitcheg_vel2depth,
    pitchlfo_delay,
    pitchlfo_fade,
    pitchlfo_freq,
    pitchlfo_depth,
    pitchlfo_depthccN,
    pitchlfo_depthchanaft,
    pitchlfo_depthpolyaft,
    pitchlfo_freqccN,
    pitchlfo_freqchanaft,
    pitchlfo_freqpolyaft,
    fil_type,
    cutoff,
    cutoff_ccN,
    cutoff_chanaft,
    cutoff_polyaft,
    resonance,
    fil_keytrack,
    fil_keycenter,
    fil_veltrack,
    fil_random,
    fileg_delay,
    fileg_start,
    fileg_attack,
    fileg_hold,
    fileg_decay,
    fileg_sustain,
    fileg_release,
    fileg_depth,
    fileg_vel2delay,
    fileg_vel2attack,
    fileg_vel2hold,
    fileg_vel2decay,
    fileg_vel2sustain,
    fileg_vel2release,
    fileg_vel2depth,
    fillfo_delay,
    fillfo_fade,
    fillfo_freq,
    fillfo_depth,
    fillfo_depthccN,
    fillfo_depthchanaft,
    fillfo_depthpolyaft,
    fillfo_freqccN,
    fillfo_freqchanaft,
    fillfo_freqpolyaft,
    volume,
    pan,
    width,
    position,
    amp_keytrack,
    amp_keycenter,
    amp_veltrack,
    amp_velcurve_1,
    amp_velcurve_127,
    amp_random,
    rt_decay,
    output,
    gain_ccN,
    xfin_lokey,
    xfin_hikey,
    xfout_lokey,
    xfout_hikey,
    xf_keycurve,
    xfin_lovel,
    xfin_hivel,
    xfout_lovel,
    xfout_hivel,
    xf_velcurve,
    xfin_loccN,
    xfin_hiccN,
    xfout_loccN,
    xfout_hiccN,
    xf_cccurve,
    ampeg_delay,
    ampeg_start,
    ampeg_attack,
    ampeg_hold,
    ampeg_decay,
    ampeg_sustain,
    ampeg_release,
    ampeg_vel2delay,
    ampeg_vel2attack,
    ampeg_vel2hold,
    ampeg_vel2decay,
    ampeg_vel2sustain,
    ampeg_vel2release,
    ampeg_delayccN,
    ampeg_startccN,
    ampeg_attackccN,
    ampeg_holdccN,
    ampeg_decayccN,
    ampeg_sustainccN,
    ampeg_releaseccN,
    amplfo_delay,
    amplfo_fade,
    amplfo_freq,
    amplfo_depth,
    amplfo_depthccN,
    amplfo_depthchanaft,
    amplfo_depthpolyaft,
    amplfo_freqccN,
    amplfo_freqchanaft,
    amplfo_freqpolyaft,
    eq1_freq,
    eq2_freq,
    eq3_freq,
    eq1_freqccN,
    eq2_freqccN,
    eq3_freqccN,
    eq1_vel2freq,
    eq2_vel2freq,
    eq3_vel2freq,
    eq1_bw,
    eq2_bw,
    eq3_bw,
    eq1_bwccN,
    eq2_bwccN,
    eq3_bwccN,
    eq1_gain,
    eq2_gain,
    eq3_gain,
    eq1_gainccN,
    eq2_gainccN,
    eq3_gainccN,
    eq1_vel2gain,
    eq2_vel2gain,
    eq3_vel2gain,
    effect1,
    effect2
  );


function StrToSfzOpcode(const Value : string):TSfzOpcode;
function SfzOpcodeToStr(const Opcode : TSfzOpcode):string;

implementation

uses
  SysUtils;


function StrToSfzOpcode(const Value : string):TSfzOpcode;
begin
  if SameText(Value, 'sample')                 then exit(TSfzOpcode.sample);
  if SameText(Value, 'lochan')                 then exit(TSfzOpcode.lochan);
  if SameText(Value, 'hichan')                 then exit(TSfzOpcode.hichan);
  if SameText(Value, 'lokey')                  then exit(TSfzOpcode.lokey);
  if SameText(Value, 'hikey')                  then exit(TSfzOpcode.hikey);
  if SameText(Value, 'key')                    then exit(TSfzOpcode.key);
  if SameText(Value, 'lovel')                  then exit(TSfzOpcode.lovel);
  if SameText(Value, 'hivel')                  then exit(TSfzOpcode.hivel);
  if SameText(Value, 'lobend')                 then exit(TSfzOpcode.lobend);
  if SameText(Value, 'hibend')                 then exit(TSfzOpcode.hibend);
  if SameText(Value, 'lochanaft')              then exit(TSfzOpcode.lochanaft);
  if SameText(Value, 'hichanaft')              then exit(TSfzOpcode.hichanaft);
  if SameText(Value, 'lopolyaft')              then exit(TSfzOpcode.lopolyaft);
  if SameText(Value, 'hipolyaft')              then exit(TSfzOpcode.hipolyaft);
  if SameText(Value, 'lorand')                 then exit(TSfzOpcode.lorand);
  if SameText(Value, 'hirand')                 then exit(TSfzOpcode.hirand);
  if SameText(Value, 'lobpm')                  then exit(TSfzOpcode.lobpm);
  if SameText(Value, 'hibpm')                  then exit(TSfzOpcode.hibpm);
  if SameText(Value, 'seq_length')             then exit(TSfzOpcode.seq_length);
  if SameText(Value, 'seq_position')           then exit(TSfzOpcode.seq_position);
  if SameText(Value, 'sw_lokey')               then exit(TSfzOpcode.sw_lokey);
  if SameText(Value, 'sw_hikey')               then exit(TSfzOpcode.sw_hikey);
  if SameText(Value, 'sw_last')                then exit(TSfzOpcode.sw_last);
  if SameText(Value, 'sw_down')                then exit(TSfzOpcode.sw_down);
  if SameText(Value, 'sw_up')                  then exit(TSfzOpcode.sw_up);
  if SameText(Value, 'sw_previous')            then exit(TSfzOpcode.sw_previous);
  if SameText(Value, 'sw_vel')                 then exit(TSfzOpcode.sw_vel);
  if SameText(Value, 'trigger')                then exit(TSfzOpcode.trigger);
  if SameText(Value, 'group')                  then exit(TSfzOpcode.group);
  if SameText(Value, 'off_by')                 then exit(TSfzOpcode.off_by);
  if SameText(Value, 'off_mode')               then exit(TSfzOpcode.off_mode);
  if SameText(Value, 'on_loccN')               then exit(TSfzOpcode.on_loccN);
  if SameText(Value, 'on_hiccN')               then exit(TSfzOpcode.on_hiccN);
  if SameText(Value, 'delay')                  then exit(TSfzOpcode.delay);
  if SameText(Value, 'delay_random')           then exit(TSfzOpcode.delay_random);
  if SameText(Value, 'delay_ccN')              then exit(TSfzOpcode.delay_ccN);
  if SameText(Value, 'offset')                 then exit(TSfzOpcode.offset);
  if SameText(Value, 'offset_random')          then exit(TSfzOpcode.offset_random);
  if SameText(Value, 'offset_ccN')             then exit(TSfzOpcode.offset_ccN);
  if SameText(Value, 'sample_end')             then exit(TSfzOpcode.sample_end);
  if SameText(Value, 'count')                  then exit(TSfzOpcode.count);
  if SameText(Value, 'loop_mode')              then exit(TSfzOpcode.loop_mode);
  if SameText(Value, 'loop_start')             then exit(TSfzOpcode.loop_start);
  if SameText(Value, 'loop_end')               then exit(TSfzOpcode.loop_end);
  if SameText(Value, 'sync_beats')             then exit(TSfzOpcode.sync_beats);
  if SameText(Value, 'sync_offset')            then exit(TSfzOpcode.sync_offset);
  if SameText(Value, 'transpose')              then exit(TSfzOpcode.transpose);
  if SameText(Value, 'tune')                   then exit(TSfzOpcode.tune);
  if SameText(Value, 'pitch_keycenter')        then exit(TSfzOpcode.pitch_keycenter);
  if SameText(Value, 'pitch_keytrack')         then exit(TSfzOpcode.pitch_keytrack);
  if SameText(Value, 'pitch_veltrack')         then exit(TSfzOpcode.pitch_veltrack);
  if SameText(Value, 'pitch_random')           then exit(TSfzOpcode.pitch_random);
  if SameText(Value, 'bend_up')                then exit(TSfzOpcode.bend_up);
  if SameText(Value, 'bend_down')              then exit(TSfzOpcode.bend_down);
  if SameText(Value, 'bend_step')              then exit(TSfzOpcode.bend_step);
  if SameText(Value, 'pitcheg_delay')          then exit(TSfzOpcode.pitcheg_delay);
  if SameText(Value, 'pitcheg_start')          then exit(TSfzOpcode.pitcheg_start);
  if SameText(Value, 'pitcheg_attack')         then exit(TSfzOpcode.pitcheg_attack);
  if SameText(Value, 'pitcheg_hold')           then exit(TSfzOpcode.pitcheg_hold);
  if SameText(Value, 'pitcheg_decay')          then exit(TSfzOpcode.pitcheg_decay);
  if SameText(Value, 'pitcheg_sustain')        then exit(TSfzOpcode.pitcheg_sustain);
  if SameText(Value, 'pitcheg_release')        then exit(TSfzOpcode.pitcheg_release);
  if SameText(Value, 'pitcheg_depth')          then exit(TSfzOpcode.pitcheg_depth);
  if SameText(Value, 'pitcheg_vel2delay')      then exit(TSfzOpcode.pitcheg_vel2delay);
  if SameText(Value, 'pitcheg_vel2attack')     then exit(TSfzOpcode.pitcheg_vel2attack);
  if SameText(Value, 'pitcheg_vel2hold')       then exit(TSfzOpcode.pitcheg_vel2hold);
  if SameText(Value, 'pitcheg_vel2decay')      then exit(TSfzOpcode.pitcheg_vel2decay);
  if SameText(Value, 'pitcheg_vel2sustain')    then exit(TSfzOpcode.pitcheg_vel2sustain);
  if SameText(Value, 'pitcheg_vel2release')    then exit(TSfzOpcode.pitcheg_vel2release);
  if SameText(Value, 'pitcheg_vel2depth')      then exit(TSfzOpcode.pitcheg_vel2depth);
  if SameText(Value, 'pitchlfo_delay')         then exit(TSfzOpcode.pitchlfo_delay);
  if SameText(Value, 'pitchlfo_fade')          then exit(TSfzOpcode.pitchlfo_fade);
  if SameText(Value, 'pitchlfo_freq')          then exit(TSfzOpcode.pitchlfo_freq);
  if SameText(Value, 'pitchlfo_depth')         then exit(TSfzOpcode.pitchlfo_depth);
  if SameText(Value, 'pitchlfo_depthccN')      then exit(TSfzOpcode.pitchlfo_depthccN);
  if SameText(Value, 'pitchlfo_depthchanaft')  then exit(TSfzOpcode.pitchlfo_depthchanaft);
  if SameText(Value, 'pitchlfo_depthpolyaft')  then exit(TSfzOpcode.pitchlfo_depthpolyaft);
  if SameText(Value, 'pitchlfo_freqccN')       then exit(TSfzOpcode.pitchlfo_freqccN);
  if SameText(Value, 'pitchlfo_freqchanaft')   then exit(TSfzOpcode.pitchlfo_freqchanaft);
  if SameText(Value, 'pitchlfo_freqpolyaft')   then exit(TSfzOpcode.pitchlfo_freqpolyaft);
  if SameText(Value, 'fil_type')               then exit(TSfzOpcode.fil_type);
  if SameText(Value, 'cutoff')                 then exit(TSfzOpcode.cutoff);
  if SameText(Value, 'cutoff_ccN')             then exit(TSfzOpcode.cutoff_ccN);
  if SameText(Value, 'cutoff_chanaft')         then exit(TSfzOpcode.cutoff_chanaft);
  if SameText(Value, 'cutoff_polyaft')         then exit(TSfzOpcode.cutoff_polyaft);
  if SameText(Value, 'resonance')              then exit(TSfzOpcode.resonance);
  if SameText(Value, 'fil_keytrack')           then exit(TSfzOpcode.fil_keytrack);
  if SameText(Value, 'fil_keycenter')          then exit(TSfzOpcode.fil_keycenter);
  if SameText(Value, 'fil_veltrack')           then exit(TSfzOpcode.fil_veltrack);
  if SameText(Value, 'fil_random')             then exit(TSfzOpcode.fil_random);
  if SameText(Value, 'fileg_delay')            then exit(TSfzOpcode.fileg_delay);
  if SameText(Value, 'fileg_start')            then exit(TSfzOpcode.fileg_start);
  if SameText(Value, 'fileg_attack')           then exit(TSfzOpcode.fileg_attack);
  if SameText(Value, 'fileg_hold')             then exit(TSfzOpcode.fileg_hold);
  if SameText(Value, 'fileg_decay')            then exit(TSfzOpcode.fileg_decay);
  if SameText(Value, 'fileg_sustain')          then exit(TSfzOpcode.fileg_sustain);
  if SameText(Value, 'fileg_release')          then exit(TSfzOpcode.fileg_release);
  if SameText(Value, 'fileg_depth')            then exit(TSfzOpcode.fileg_depth);
  if SameText(Value, 'fileg_vel2delay')        then exit(TSfzOpcode.fileg_vel2delay);
  if SameText(Value, 'fileg_vel2attack')       then exit(TSfzOpcode.fileg_vel2attack);
  if SameText(Value, 'fileg_vel2hold')         then exit(TSfzOpcode.fileg_vel2hold);
  if SameText(Value, 'fileg_vel2decay')        then exit(TSfzOpcode.fileg_vel2decay);
  if SameText(Value, 'fileg_vel2sustain')      then exit(TSfzOpcode.fileg_vel2sustain);
  if SameText(Value, 'fileg_vel2release')      then exit(TSfzOpcode.fileg_vel2release);
  if SameText(Value, 'fileg_vel2depth')        then exit(TSfzOpcode.fileg_vel2depth);
  if SameText(Value, 'fillfo_delay')           then exit(TSfzOpcode.fillfo_delay);
  if SameText(Value, 'fillfo_fade')            then exit(TSfzOpcode.fillfo_fade);
  if SameText(Value, 'fillfo_freq')            then exit(TSfzOpcode.fillfo_freq);
  if SameText(Value, 'fillfo_depth')           then exit(TSfzOpcode.fillfo_depth);
  if SameText(Value, 'fillfo_depthccN')        then exit(TSfzOpcode.fillfo_depthccN);
  if SameText(Value, 'fillfo_depthchanaft')    then exit(TSfzOpcode.fillfo_depthchanaft);
  if SameText(Value, 'fillfo_depthpolyaft')    then exit(TSfzOpcode.fillfo_depthpolyaft);
  if SameText(Value, 'fillfo_freqccN')         then exit(TSfzOpcode.fillfo_freqccN);
  if SameText(Value, 'fillfo_freqchanaft')     then exit(TSfzOpcode.fillfo_freqchanaft);
  if SameText(Value, 'fillfo_freqpolyaft')     then exit(TSfzOpcode.fillfo_freqpolyaft);
  if SameText(Value, 'volume')                 then exit(TSfzOpcode.volume);
  if SameText(Value, 'pan')                    then exit(TSfzOpcode.pan);
  if SameText(Value, 'width')                  then exit(TSfzOpcode.width);
  if SameText(Value, 'position')               then exit(TSfzOpcode.position);
  if SameText(Value, 'amp_keytrack')           then exit(TSfzOpcode.amp_keytrack);
  if SameText(Value, 'amp_keycenter')          then exit(TSfzOpcode.amp_keycenter);
  if SameText(Value, 'amp_veltrack')           then exit(TSfzOpcode.amp_veltrack);
  if SameText(Value, 'amp_velcurve_1')         then exit(TSfzOpcode.amp_velcurve_1);
  if SameText(Value, 'amp_velcurve_127')       then exit(TSfzOpcode.amp_velcurve_127);
  if SameText(Value, 'amp_random')             then exit(TSfzOpcode.amp_random);
  if SameText(Value, 'rt_decay')               then exit(TSfzOpcode.rt_decay);
  if SameText(Value, 'output')                 then exit(TSfzOpcode.output);
  if SameText(Value, 'gain_ccN')               then exit(TSfzOpcode.gain_ccN);
  if SameText(Value, 'xfin_lokey')             then exit(TSfzOpcode.xfin_lokey);
  if SameText(Value, 'xfin_hikey')             then exit(TSfzOpcode.xfin_hikey);
  if SameText(Value, 'xfout_lokey')            then exit(TSfzOpcode.xfout_lokey);
  if SameText(Value, 'xfout_hikey')            then exit(TSfzOpcode.xfout_hikey);
  if SameText(Value, 'xf_keycurve')            then exit(TSfzOpcode.xf_keycurve);
  if SameText(Value, 'xfin_lovel')             then exit(TSfzOpcode.xfin_lovel);
  if SameText(Value, 'xfin_hivel')             then exit(TSfzOpcode.xfin_hivel);
  if SameText(Value, 'xfout_lovel')            then exit(TSfzOpcode.xfout_lovel);
  if SameText(Value, 'xfout_hivel')            then exit(TSfzOpcode.xfout_hivel);
  if SameText(Value, 'xf_velcurve')            then exit(TSfzOpcode.xf_velcurve);
  if SameText(Value, 'xfin_loccN')             then exit(TSfzOpcode.xfin_loccN);
  if SameText(Value, 'xfin_hiccN')             then exit(TSfzOpcode.xfin_hiccN);
  if SameText(Value, 'xfout_loccN')            then exit(TSfzOpcode.xfout_loccN);
  if SameText(Value, 'xfout_hiccN')            then exit(TSfzOpcode.xfout_hiccN);
  if SameText(Value, 'xf_cccurve')             then exit(TSfzOpcode.xf_cccurve);
  if SameText(Value, 'ampeg_delay')            then exit(TSfzOpcode.ampeg_delay);
  if SameText(Value, 'ampeg_start')            then exit(TSfzOpcode.ampeg_start);
  if SameText(Value, 'ampeg_attack')           then exit(TSfzOpcode.ampeg_attack);
  if SameText(Value, 'ampeg_hold')             then exit(TSfzOpcode.ampeg_hold);
  if SameText(Value, 'ampeg_decay')            then exit(TSfzOpcode.ampeg_decay);
  if SameText(Value, 'ampeg_sustain')          then exit(TSfzOpcode.ampeg_sustain);
  if SameText(Value, 'ampeg_release')          then exit(TSfzOpcode.ampeg_release);
  if SameText(Value, 'ampeg_vel2delay')        then exit(TSfzOpcode.ampeg_vel2delay);
  if SameText(Value, 'ampeg_vel2attack')       then exit(TSfzOpcode.ampeg_vel2attack);
  if SameText(Value, 'ampeg_vel2hold')         then exit(TSfzOpcode.ampeg_vel2hold);
  if SameText(Value, 'ampeg_vel2decay')        then exit(TSfzOpcode.ampeg_vel2decay);
  if SameText(Value, 'ampeg_vel2sustain')      then exit(TSfzOpcode.ampeg_vel2sustain);
  if SameText(Value, 'ampeg_vel2release')      then exit(TSfzOpcode.ampeg_vel2release);
  if SameText(Value, 'ampeg_delayccN')         then exit(TSfzOpcode.ampeg_delayccN);
  if SameText(Value, 'ampeg_startccN')         then exit(TSfzOpcode.ampeg_startccN);
  if SameText(Value, 'ampeg_attackccN')        then exit(TSfzOpcode.ampeg_attackccN);
  if SameText(Value, 'ampeg_holdccN')          then exit(TSfzOpcode.ampeg_holdccN);
  if SameText(Value, 'ampeg_decayccN')         then exit(TSfzOpcode.ampeg_decayccN);
  if SameText(Value, 'ampeg_sustainccN')       then exit(TSfzOpcode.ampeg_sustainccN);
  if SameText(Value, 'ampeg_releaseccN')       then exit(TSfzOpcode.ampeg_releaseccN);
  if SameText(Value, 'amplfo_delay')           then exit(TSfzOpcode.amplfo_delay);
  if SameText(Value, 'amplfo_fade')            then exit(TSfzOpcode.amplfo_fade);
  if SameText(Value, 'amplfo_freq')            then exit(TSfzOpcode.amplfo_freq);
  if SameText(Value, 'amplfo_depth')           then exit(TSfzOpcode.amplfo_depth);
  if SameText(Value, 'amplfo_depthccN')        then exit(TSfzOpcode.amplfo_depthccN);
  if SameText(Value, 'amplfo_depthchanaft')    then exit(TSfzOpcode.amplfo_depthchanaft);
  if SameText(Value, 'amplfo_depthpolyaft')    then exit(TSfzOpcode.amplfo_depthpolyaft);
  if SameText(Value, 'amplfo_freqccN')         then exit(TSfzOpcode.amplfo_freqccN);
  if SameText(Value, 'amplfo_freqchanaft')     then exit(TSfzOpcode.amplfo_freqchanaft);
  if SameText(Value, 'amplfo_freqpolyaft')     then exit(TSfzOpcode.amplfo_freqpolyaft);
  if SameText(Value, 'eq1_freq')               then exit(TSfzOpcode.eq1_freq);
  if SameText(Value, 'eq2_freq')               then exit(TSfzOpcode.eq2_freq);
  if SameText(Value, 'eq3_freq')               then exit(TSfzOpcode.eq3_freq);
  if SameText(Value, 'eq1_freqccN')            then exit(TSfzOpcode.eq1_freqccN);
  if SameText(Value, 'eq2_freqccN')            then exit(TSfzOpcode.eq2_freqccN);
  if SameText(Value, 'eq3_freqccN')            then exit(TSfzOpcode.eq3_freqccN);
  if SameText(Value, 'eq1_vel2freq')           then exit(TSfzOpcode.eq1_vel2freq);
  if SameText(Value, 'eq2_vel2freq')           then exit(TSfzOpcode.eq2_vel2freq);
  if SameText(Value, 'eq3_vel2freq')           then exit(TSfzOpcode.eq3_vel2freq);
  if SameText(Value, 'eq1_bw')                 then exit(TSfzOpcode.eq1_bw);
  if SameText(Value, 'eq2_bw')                 then exit(TSfzOpcode.eq2_bw);
  if SameText(Value, 'eq3_bw')                 then exit(TSfzOpcode.eq3_bw);
  if SameText(Value, 'eq1_bwccN')              then exit(TSfzOpcode.eq1_bwccN);
  if SameText(Value, 'eq2_bwccN')              then exit(TSfzOpcode.eq2_bwccN);
  if SameText(Value, 'eq3_bwccN')              then exit(TSfzOpcode.eq3_bwccN);
  if SameText(Value, 'eq1_gain')               then exit(TSfzOpcode.eq1_gain);
  if SameText(Value, 'eq2_gain')               then exit(TSfzOpcode.eq2_gain);
  if SameText(Value, 'eq3_gain')               then exit(TSfzOpcode.eq3_gain);
  if SameText(Value, 'eq1_gainccN')            then exit(TSfzOpcode.eq1_gainccN);
  if SameText(Value, 'eq2_gainccN')            then exit(TSfzOpcode.eq2_gainccN);
  if SameText(Value, 'eq3_gainccN')            then exit(TSfzOpcode.eq3_gainccN);
  if SameText(Value, 'eq1_vel2gain')           then exit(TSfzOpcode.eq1_vel2gain);
  if SameText(Value, 'eq2_vel2gain')           then exit(TSfzOpcode.eq2_vel2gain);
  if SameText(Value, 'eq3_vel2gain')           then exit(TSfzOpcode.eq3_vel2gain);
  if SameText(Value, 'effect1')                then exit(TSfzOpcode.effect1);
  if SameText(Value, 'effect2')                then exit(TSfzOpcode.effect2);

  // if we make it this far..
  result := TSfzOpcode.Unknown;
end;

function SfzOpcodeToStr(const Opcode : TSfzOpcode):string;
begin
  case Opcode of
    TSfzOpcode.Unknown:                    result := 'unknown';
    TSfzOpcode.sample:                     result := 'sample';
    TSfzOpcode.lochan:                     result := 'lochan';
    TSfzOpcode.hichan:                     result := 'hichan';
    TSfzOpcode.lokey:                      result := 'lokey';
    TSfzOpcode.hikey:                      result := 'hikey';
    TSfzOpcode.key:                        result := 'key';
    TSfzOpcode.lovel:                      result := 'lovel';
    TSfzOpcode.hivel:                      result := 'hivel';
    TSfzOpcode.lobend:                     result := 'lobend';
    TSfzOpcode.hibend:                     result := 'hibend';
    TSfzOpcode.lochanaft:                  result := 'lochanaft';
    TSfzOpcode.hichanaft:                  result := 'hichanaft';
    TSfzOpcode.lopolyaft:                  result := 'lopolyaft';
    TSfzOpcode.hipolyaft:                  result := 'hipolyaft';
    TSfzOpcode.lorand:                     result := 'lorand';
    TSfzOpcode.hirand:                     result := 'hirand';
    TSfzOpcode.lobpm:                      result := 'lobpm';
    TSfzOpcode.hibpm:                      result := 'hibpm';
    TSfzOpcode.seq_length:                 result := 'seq_length';
    TSfzOpcode.seq_position:               result := 'seq_position';
    TSfzOpcode.sw_lokey:                   result := 'sw_lokey';
    TSfzOpcode.sw_hikey:                   result := 'sw_hikey';
    TSfzOpcode.sw_last:                    result := 'sw_last';
    TSfzOpcode.sw_down:                    result := 'sw_down';
    TSfzOpcode.sw_up:                      result := 'sw_up';
    TSfzOpcode.sw_previous:                result := 'sw_previous';
    TSfzOpcode.sw_vel:                     result := 'sw_vel';
    TSfzOpcode.trigger:                    result := 'trigger';
    TSfzOpcode.group:                      result := 'group';
    TSfzOpcode.off_by:                     result := 'off_by';
    TSfzOpcode.off_mode:                   result := 'off_mode';
    TSfzOpcode.on_loccN:                   result := 'on_loccN';
    TSfzOpcode.on_hiccN:                   result := 'on_hiccN';
    TSfzOpcode.delay:                      result := 'delay';
    TSfzOpcode.delay_random:               result := 'delay_random';
    TSfzOpcode.delay_ccN:                  result := 'delay_ccN';
    TSfzOpcode.offset:                     result := 'offset';
    TSfzOpcode.offset_random:              result := 'offset_random';
    TSfzOpcode.offset_ccN:                 result := 'offset_ccN';
    TSfzOpcode.sample_end:                 result := 'sample_end';
    TSfzOpcode.count:                      result := 'count';
    TSfzOpcode.loop_mode:                  result := 'loop_mode';
    TSfzOpcode.loop_start:                 result := 'loop_start';
    TSfzOpcode.loop_end:                   result := 'loop_end';
    TSfzOpcode.sync_beats:                 result := 'sync_beats';
    TSfzOpcode.sync_offset:                result := 'sync_offset';
    TSfzOpcode.transpose:                  result := 'transpose';
    TSfzOpcode.tune:                       result := 'tune';
    TSfzOpcode.pitch_keycenter:            result := 'pitch_keycenter';
    TSfzOpcode.pitch_keytrack:             result := 'pitch_keytrack';
    TSfzOpcode.pitch_veltrack:             result := 'pitch_veltrack';
    TSfzOpcode.pitch_random:               result := 'pitch_random';
    TSfzOpcode.bend_up:                    result := 'bend_up';
    TSfzOpcode.bend_down:                  result := 'bend_down';
    TSfzOpcode.bend_step:                  result := 'bend_step';
    TSfzOpcode.pitcheg_delay:              result := 'pitcheg_delay';
    TSfzOpcode.pitcheg_start:              result := 'pitcheg_start';
    TSfzOpcode.pitcheg_attack:             result := 'pitcheg_attack';
    TSfzOpcode.pitcheg_hold:               result := 'pitcheg_hold';
    TSfzOpcode.pitcheg_decay:              result := 'pitcheg_decay';
    TSfzOpcode.pitcheg_sustain:            result := 'pitcheg_sustain';
    TSfzOpcode.pitcheg_release:            result := 'pitcheg_release';
    TSfzOpcode.pitcheg_depth:              result := 'pitcheg_depth';
    TSfzOpcode.pitcheg_vel2delay:          result := 'pitcheg_vel2delay';
    TSfzOpcode.pitcheg_vel2attack:         result := 'pitcheg_vel2attack';
    TSfzOpcode.pitcheg_vel2hold:           result := 'pitcheg_vel2hold';
    TSfzOpcode.pitcheg_vel2decay:          result := 'pitcheg_vel2decay';
    TSfzOpcode.pitcheg_vel2sustain:        result := 'pitcheg_vel2sustain';
    TSfzOpcode.pitcheg_vel2release:        result := 'pitcheg_vel2release';
    TSfzOpcode.pitcheg_vel2depth:          result := 'pitcheg_vel2depth';
    TSfzOpcode.pitchlfo_delay:             result := 'pitchlfo_delay';
    TSfzOpcode.pitchlfo_fade:              result := 'pitchlfo_fade';
    TSfzOpcode.pitchlfo_freq:              result := 'pitchlfo_freq';
    TSfzOpcode.pitchlfo_depth:             result := 'pitchlfo_depth';
    TSfzOpcode.pitchlfo_depthccN:          result := 'pitchlfo_depthccN';
    TSfzOpcode.pitchlfo_depthchanaft:      result := 'pitchlfo_depthchanaft';
    TSfzOpcode.pitchlfo_depthpolyaft:      result := 'pitchlfo_depthpolyaft';
    TSfzOpcode.pitchlfo_freqccN:           result := 'pitchlfo_freqccN';
    TSfzOpcode.pitchlfo_freqchanaft:       result := 'pitchlfo_freqchanaft';
    TSfzOpcode.pitchlfo_freqpolyaft:       result := 'pitchlfo_freqpolyaft';
    TSfzOpcode.fil_type:                   result := 'fil_type';
    TSfzOpcode.cutoff:                     result := 'cutoff';
    TSfzOpcode.cutoff_ccN:                 result := 'cutoff_ccN';
    TSfzOpcode.cutoff_chanaft:             result := 'cutoff_chanaft';
    TSfzOpcode.cutoff_polyaft:             result := 'cutoff_polyaft';
    TSfzOpcode.resonance:                  result := 'resonance';
    TSfzOpcode.fil_keytrack:               result := 'fil_keytrack';
    TSfzOpcode.fil_keycenter:              result := 'fil_keycenter';
    TSfzOpcode.fil_veltrack:               result := 'fil_veltrack';
    TSfzOpcode.fil_random:                 result := 'fil_random';
    TSfzOpcode.fileg_delay:                result := '';
    TSfzOpcode.fileg_start:                result := 'fileg_start';
    TSfzOpcode.fileg_attack:               result := 'fileg_attack';
    TSfzOpcode.fileg_hold:                 result := 'fileg_hold';
    TSfzOpcode.fileg_decay:                result := 'fileg_decay';
    TSfzOpcode.fileg_sustain:              result := 'fileg_sustain';
    TSfzOpcode.fileg_release:              result := '';
    TSfzOpcode.fileg_depth:                result := 'fileg_depth';
    TSfzOpcode.fileg_vel2delay:            result := 'fileg_vel2delay';
    TSfzOpcode.fileg_vel2attack:           result := 'fileg_vel2attack';
    TSfzOpcode.fileg_vel2hold:             result := 'fileg_vel2hold';
    TSfzOpcode.fileg_vel2decay:            result := 'fileg_vel2decay';
    TSfzOpcode.fileg_vel2sustain:          result := 'fileg_vel2sustain';
    TSfzOpcode.fileg_vel2release:          result := 'fileg_vel2release';
    TSfzOpcode.fileg_vel2depth:            result := '';
    TSfzOpcode.fillfo_delay:               result := '';
    TSfzOpcode.fillfo_fade:                result := 'fillfo_fade';
    TSfzOpcode.fillfo_freq:                result := 'fillfo_freq';
    TSfzOpcode.fillfo_depth:               result := 'fillfo_depth';
    TSfzOpcode.fillfo_depthccN:            result := 'fillfo_depthccN';
    TSfzOpcode.fillfo_depthchanaft:        result := 'fillfo_depthchanaft';
    TSfzOpcode.fillfo_depthpolyaft:        result := 'fillfo_depthpolyaft';
    TSfzOpcode.fillfo_freqccN:             result := 'fillfo_freqccN';
    TSfzOpcode.fillfo_freqchanaft:         result := 'fillfo_freqchanaft';
    TSfzOpcode.fillfo_freqpolyaft:         result := 'fillfo_freqpolyaft';
    TSfzOpcode.volume:                     result := 'volume';
    TSfzOpcode.pan:                        result := 'pan';
    TSfzOpcode.width:                      result := 'width';
    TSfzOpcode.position:                   result := 'position';
    TSfzOpcode.amp_keytrack:               result := 'amp_keytrack';
    TSfzOpcode.amp_keycenter:              result := 'amp_keycenter';
    TSfzOpcode.amp_veltrack:               result := 'amp_veltrack';
    TSfzOpcode.amp_velcurve_1:             result := 'amp_velcurve_1';
    TSfzOpcode.amp_velcurve_127:           result := 'amp_velcurve_127';
    TSfzOpcode.amp_random:                 result := 'amp_random';
    TSfzOpcode.rt_decay:                   result := 'rt_decay';
    TSfzOpcode.output:                     result := 'output';
    TSfzOpcode.gain_ccN:                   result := 'gain_ccN';
    TSfzOpcode.xfin_lokey:                 result := 'xfin_lokey';
    TSfzOpcode.xfin_hikey:                 result := 'xfin_hikey';
    TSfzOpcode.xfout_lokey:                result := 'xfout_lokey';
    TSfzOpcode.xfout_hikey:                result := 'xfout_hikey';
    TSfzOpcode.xf_keycurve:                result := 'xf_keycurve';
    TSfzOpcode.xfin_lovel:                 result := 'xfin_lovel';
    TSfzOpcode.xfin_hivel:                 result := 'xfin_hivel';
    TSfzOpcode.xfout_lovel:                result := 'xfout_lovel';
    TSfzOpcode.xfout_hivel:                result := 'xfout_hivel';
    TSfzOpcode.xf_velcurve:                result := 'xf_velcurve';
    TSfzOpcode.xfin_loccN:                 result := 'xfin_loccN';
    TSfzOpcode.xfin_hiccN:                 result := 'xfin_hiccN';
    TSfzOpcode.xfout_loccN:                result := 'xfout_loccN';
    TSfzOpcode.xfout_hiccN:                result := 'xfout_hiccN';
    TSfzOpcode.xf_cccurve:                 result := 'xf_cccurve';
    TSfzOpcode.ampeg_delay:                result := 'ampeg_delay';
    TSfzOpcode.ampeg_start:                result := 'ampeg_start';
    TSfzOpcode.ampeg_attack:               result := 'ampeg_attack';
    TSfzOpcode.ampeg_hold:                 result := '';
    TSfzOpcode.ampeg_decay:                result := '';
    TSfzOpcode.ampeg_sustain:              result := 'ampeg_sustain';
    TSfzOpcode.ampeg_release:              result := 'ampeg_release';
    TSfzOpcode.ampeg_vel2delay:            result := 'ampeg_vel2delay';
    TSfzOpcode.ampeg_vel2attack:           result := 'ampeg_vel2attack';
    TSfzOpcode.ampeg_vel2hold:             result := 'ampeg_vel2hold';
    TSfzOpcode.ampeg_vel2decay:            result := 'ampeg_vel2decay';
    TSfzOpcode.ampeg_vel2sustain:          result := 'ampeg_vel2sustain';
    TSfzOpcode.ampeg_vel2release:          result := 'ampeg_vel2release';
    TSfzOpcode.ampeg_delayccN:             result := 'ampeg_delayccN';
    TSfzOpcode.ampeg_startccN:             result := 'ampeg_startccN';
    TSfzOpcode.ampeg_attackccN:            result := 'ampeg_attackccN';
    TSfzOpcode.ampeg_holdccN:              result := 'ampeg_holdccN';
    TSfzOpcode.ampeg_decayccN:             result := 'ampeg_decayccN';
    TSfzOpcode.ampeg_sustainccN:           result := 'ampeg_sustainccN';
    TSfzOpcode.ampeg_releaseccN:           result := 'ampeg_releaseccN';
    TSfzOpcode.amplfo_delay:               result := 'amplfo_delay';
    TSfzOpcode.amplfo_fade:                result := 'amplfo_fade';
    TSfzOpcode.amplfo_freq:                result := 'amplfo_freq';
    TSfzOpcode.amplfo_depth:               result := 'amplfo_depth';
    TSfzOpcode.amplfo_depthccN:            result := 'amplfo_depthccN';
    TSfzOpcode.amplfo_depthchanaft:        result := 'amplfo_depthchanaft';
    TSfzOpcode.amplfo_depthpolyaft:        result := 'amplfo_depthpolyaft';
    TSfzOpcode.amplfo_freqccN:             result := 'amplfo_freqccN';
    TSfzOpcode.amplfo_freqchanaft:         result := 'amplfo_freqchanaft';
    TSfzOpcode.amplfo_freqpolyaft:         result := 'amplfo_freqpolyaft';
    TSfzOpcode.eq1_freq:                   result := 'eq1_freq';
    TSfzOpcode.eq2_freq:                   result := 'eq2_freq';
    TSfzOpcode.eq3_freq:                   result := 'eq3_freq';
    TSfzOpcode.eq1_freqccN:                result := 'eq1_freqccN';
    TSfzOpcode.eq2_freqccN:                result := 'eq2_freqccN';
    TSfzOpcode.eq3_freqccN:                result := 'eq3_freqccN';
    TSfzOpcode.eq1_vel2freq:               result := 'eq1_vel2freq';
    TSfzOpcode.eq2_vel2freq:               result := '';
    TSfzOpcode.eq3_vel2freq:               result := 'eq3_vel2freq';
    TSfzOpcode.eq1_bw:                     result := 'eq1_bw';
    TSfzOpcode.eq2_bw:                     result := 'eq2_bw';
    TSfzOpcode.eq3_bw:                     result := 'eq3_bw';
    TSfzOpcode.eq1_bwccN:                  result := 'eq1_bwccN';
    TSfzOpcode.eq2_bwccN:                  result := 'eq2_bwccN';
    TSfzOpcode.eq3_bwccN:                  result := 'eq3_bwccN';
    TSfzOpcode.eq1_gain:                   result := 'eq1_gain';
    TSfzOpcode.eq2_gain:                   result := 'eq2_gain';
    TSfzOpcode.eq3_gain:                   result := 'eq3_gain';
    TSfzOpcode.eq1_gainccN:                result := 'eq1_gainccN';
    TSfzOpcode.eq2_gainccN:                result := 'eq2_gainccN';
    TSfzOpcode.eq3_gainccN:                result := 'eq3_gainccN';
    TSfzOpcode.eq1_vel2gain:               result := 'eq1_vel2gain';
    TSfzOpcode.eq2_vel2gain:               result := 'eq2_vel2gain';
    TSfzOpcode.eq3_vel2gain:               result := 'eq3_vel2gain';
    TSfzOpcode.effect1:                    result := 'effect1';
    TSfzOpcode.effect2:                    result := 'effect2';
  else
    raise Exception.Create('Type not handled.');
  end;
end;

end.
