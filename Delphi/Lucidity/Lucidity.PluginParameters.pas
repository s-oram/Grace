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

  TPluginParameterInfo = record
  public
    DefaultValue : single;
  end;

function GetPluginParInfo(const Par : TPluginParameter):TPluginParameterInfo;

function IsValidPluginParName(const Name : string):boolean;

function PluginParToName(const Par : TPluginParameter):string;
function PluginParFromName(const Name : string):TPluginParameter;

function IsModPar(const Par : TPluginParameter):boolean; inline;
function IsModPar_Slow(const Par : TPluginParameter):boolean;

function GetModParIndex(const Par : TPluginParameter):integer; inline;
function GetModParIndex_Slow(const Par : TPluginParameter):integer;

// "Global Plugin Parameters" are members of the TeePlugin class.
// They are appliced globally and effect all voices. Other non-global
// parameters are generally applied to "Key Groups".
function IsGlobalPluginPar(const Par : TPluginParameter):boolean; inline;

function GetPluginParameterCount:integer; inline;
function IndexToPluginParameter(Index : integer):TPluginParameter; inline;




var
  BufferedModParIndex : array of integer; //don't access this directly.

implementation

uses
  SysUtils,
  Rtti;

function IsValidPluginParName(const Name : string):boolean;
var
  c1: Integer;
  s : string;
begin
  for c1 := 0 to TPluginParameterHelper.GetEnumTypeCount-1 do
  begin
    s := TPluginParameterHelper.ToUnicodeString(c1);
    if Name = s
      then exit(true);
  end;
  //=== no match if we've made it this far ==
  result := false;
end;

function PluginParToName(const Par : TPluginParameter):string;
begin
  result := TPluginParameterHelper.ToUnicodeString(Par);
end;

function PluginParFromName(const Name : string):TPluginParameter;
begin
  result := TPluginParameterHelper.ToEnum(Name);
end;

function IsModPar(const Par : TPluginParameter):boolean;
begin
  if BufferedModParIndex[Ord(Par)] <> -1
    then result := true
    else result := false;
end;

function IsModPar_Slow(const Par : TPluginParameter):boolean;
var
  s : string;
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
begin
  result := BufferedModParIndex[Ord(Par)];
end;

function GetModParIndex_Slow(const Par : TPluginParameter):integer;
var
  s : string;
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



function GetPluginParInfo(const Par : TPluginParameter):TPluginParameterInfo;
begin
  result.DefaultValue := 0.5;

  case Par of
    TPluginParameter.VoiceMode: ;
    TPluginParameter.VoiceGlide: ;
    TPluginParameter.PitchTracking: ;
    TPluginParameter.SamplePlaybackType: ;
    TPluginParameter.SampleResetClockSource: ;
    TPluginParameter.SamplerLoopBounds: ;
    TPluginParameter.SamplerLoopMode: ;
    TPluginParameter.OutputGain: ;
    TPluginParameter.OutputPan: ;
    TPluginParameter.VoicePitchOne: ;
    TPluginParameter.VoicePitchTwo: ;
    TPluginParameter.SampleStart: ;
    TPluginParameter.SampleEnd: ;
    TPluginParameter.LoopStart: ;
    TPluginParameter.LoopEnd: ;
    TPluginParameter.AmpAttack: ;
    TPluginParameter.AmpHold: ;
    TPluginParameter.AmpDecay: ;
    TPluginParameter.AmpSustain: ;
    TPluginParameter.AmpRelease: ;
    TPluginParameter.AmpVelocity: ;
    TPluginParameter.FilterAttack: ;
    TPluginParameter.FilterHold: ;
    TPluginParameter.FilterDecay: ;
    TPluginParameter.FilterSustain: ;
    TPluginParameter.FilterRelease: ;
    TPluginParameter.FilterVelocity: ;
    TPluginParameter.FilterRouting: ;
    TPluginParameter.FilterOutputBlend: ;
    TPluginParameter.Filter1Type: ;
    TPluginParameter.Filter2Type: ;
    TPluginParameter.Filter1KeyFollow: ;
    TPluginParameter.Filter2KeyFollow: ;
    TPluginParameter.Filter1Par1: ;
    TPluginParameter.Filter1Par2: ;
    TPluginParameter.Filter1Par3: ;
    TPluginParameter.Filter1Par4: ;
    TPluginParameter.Filter2Par1: ;
    TPluginParameter.Filter2Par2: ;
    TPluginParameter.Filter2Par3: ;
    TPluginParameter.Filter2Par4: ;
    TPluginParameter.Lfo1Shape: ;
    TPluginParameter.Lfo2Shape: ;
    TPluginParameter.Lfo1FreqMode: ;
    TPluginParameter.Lfo2FreqMode: ;
    TPluginParameter.Lfo1Range: ;
    TPluginParameter.Lfo2Range: ;
    TPluginParameter.Lfo1Par1: ;
    TPluginParameter.Lfo1Par2: ;
    TPluginParameter.Lfo1Par3: ;
    TPluginParameter.Lfo2Par1: ;
    TPluginParameter.Lfo2Par2: ;
    TPluginParameter.Lfo2Par3: ;
    TPluginParameter.Seq1Clock: ;
    TPluginParameter.Seq1Direction: ;
    TPluginParameter.Seq1Length: ;
    TPluginParameter.Seq2Clock: ;
    TPluginParameter.Seq2Direction: ;
    TPluginParameter.Seq2Length: ;
    TPluginParameter.PreviewVolume: ;
    TPluginParameter.Preview: ;
  end;
end;


function GetPluginParameterCount:integer;
begin
  result := TPluginParameterHelper.GetEnumTypeCount;
end;

function IndexToPluginParameter(Index : integer):TPluginParameter;
begin
  assert(Index >= 0);
  assert(Index <= TPluginParameterHelper.GetEnumTypeCount);
  result := TPluginParameterHelper.ToEnum(Index);
end;


var
  c1 : integer;
  Par : TPluginParameter;

initialization
  SetLength(BufferedModParIndex, TPluginParameterHelper.GetEnumTypeCount);

  for c1 := 0 to TPluginParameterHelper.GetEnumTypeCount-1 do
  begin
    Par := TPluginParameterHelper.ToEnum(c1);
    if IsModPar_Slow(Par)
      then BufferedModParIndex[c1] := GetModParIndex_Slow(Par)
      else BufferedModParIndex[c1] := -1;
  end;

finalization
  SetLength(BufferedModParIndex, 0);

end.
