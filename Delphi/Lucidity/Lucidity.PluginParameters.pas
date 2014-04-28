unit Lucidity.PluginParameters;

interface

uses
  eeEnumHelper;

{$SCOPEDENUMS ON}

type
  TPluginParameter = (
    VoiceMode,
    VoiceGlide,
    PitchTracking,
    SamplePlaybackType,
    SampleResetClockSource,
    SamplerLoopBounds,
    SamplerLoopMode,
    OutputGain,
    OutputPan,
    VoicePitchOne,
    VoicePitchTwo,
    SampleStart,
    SampleEnd,
    LoopStart,
    LoopEnd,
    AmpAttack,
    AmpHold,
    AmpDecay,
    AmpSustain,
    AmpRelease,
    AmpVelocity,
    FilterAttack,
    FilterHold,
    FilterDecay,
    FilterSustain,
    FilterRelease,
    FilterVelocity,
    FilterRouting,
    FilterOutputBlend,
    Filter1Type,
    Filter2Type,
    Filter1KeyFollow,
    Filter2KeyFollow,
    Filter1Par1,
    Filter1Par2,
    Filter1Par3,
    Filter1Par4,
    Filter2Par1,
    Filter2Par2,
    Filter2Par3,
    Filter2Par4,
    Lfo1Shape,
    Lfo2Shape,
    Lfo1FreqMode,
    Lfo2FreqMode,
    Lfo1Range,
    Lfo2Range,
    Lfo1Par1,
    Lfo1Par2,
    Lfo1Par3,
    Lfo2Par1,
    Lfo2Par2,
    Lfo2Par3,
    Seq1Clock,
    Seq1Direction,
    Seq1Length,
    Seq2Clock,
    Seq2Direction,
    Seq2Length,
    PreviewVolume,
    Preview
  );

  TPluginParameterHelper = class(TEnumHelper<TPluginParameter>)
  end;


  TModulatedPluginParameter = (
    OutputGain,
    OutputPan,
    VoicePitchOne,
    VoicePitchTwo,
    SampleStart,
    SampleEnd,
    LoopStart,
    LoopEnd,
    AmpAttack,
    AmpHold,
    AmpDecay,
    AmpSustain,
    AmpRelease,
    AmpVelocity,
    FilterAttack,
    FilterHold,
    FilterDecay,
    FilterSustain,
    FilterRelease,
    FilterVelocity,
    FilterOutputBlend,
    Filter1Par1,
    Filter1Par2,
    Filter1Par3,
    Filter1Par4,
    Filter2Par1,
    Filter2Par2,
    Filter2Par3,
    Filter2Par4,
    Lfo1Par1,
    Lfo1Par2,
    Lfo1Par3,
    Lfo2Par1,
    Lfo2Par2,
    Lfo2Par3
  );

  TModParHelper = class(TEnumHelper<TModulatedPluginParameter>)
  end;



function PluginParToName(const Par : TPluginParameter):string;
function PluginParFromName(const Name : string):TPluginParameter;

function IsModPar(const Par : TPluginParameter):boolean;
function GetModParIndex(const Par : TPluginParameter):integer;


// "Global Plugin Parameters" are members of the TeePlugin class.
// They are appliced globally and effect all voices. Other non-global
// parameters are generally applied to "Key Groups".
function IsGlobalPluginPar(const Par : TPluginParameter):boolean;

implementation

uses
  SysUtils,
  Rtti;

function PluginParToName(const Par : TPluginParameter):string;
begin
  result := TPluginParameterHelper.ToUnicodeString(Par);
end;

function PluginParFromName(const Name : string):TPluginParameter;
begin
  result := TPluginParameterHelper.ToEnum(Name);
end;

function IsModPar(const Par : TPluginParameter):boolean;
var
  s : string;
  ModParIndex : integer;
  i: Int64;
  c1 : integer;
  TestString : string;
begin
  s := TPluginParameterHelper.ToUnicodeString(Par);

  for c1 := 0 to TModParHelper.GetEnumTypeCount-1 do
  begin
    TestString := TModParHelper.ToUnicodeString(c1);
    if SameText(TestString, s)
      then exit(true);
  end;

  //== if we've made it this far the par isn't a modulated parameter.
  result := false;
end;

function GetModParIndex(const Par : TPluginParameter):integer;
var
  s : string;
  ModParIndex : integer;
  i: Int64;
  c1 : integer;
  TestString : string;
begin
  s := TPluginParameterHelper.ToUnicodeString(Par);

  for c1 := 0 to TModParHelper.GetEnumTypeCount-1 do
  begin
    TestString := TModParHelper.ToUnicodeString(c1);
    if SameText(TestString, s)
      then exit(c1);
  end;

  //== if we've made it this far the par isn't a modulated parameter.
  result := -1;
end;

function IsGlobalPluginPar(const Par : TPluginParameter):boolean;
begin
  case Par of
    TPluginParameter.VoiceMode:     result := true;
    TPluginParameter.VoiceGlide:    result := true;
    TPluginParameter.PreviewVolume: result := true;
    TPluginParameter.Preview:       result := true;
  else
    result := false;
  end;
end;

end.
