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
    Preview,
    PadX1,
    PadY1,
    PadX2,
    PadY2,
    PadX3,
    PadY3,
    PadX4,
    PadY4
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


function IsPublishedVstParameter(const Par : TPluginParameter):boolean;


var
  BufferedModParIndex : array of integer; //don't access this directly.

implementation

uses
  SysUtils,
  Rtti,
  uLucidityEnums;

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
    TPluginParameter.PadX1:         result := true;
    TPluginParameter.PadY1:         result := true;
    TPluginParameter.PadX2:         result := true;
    TPluginParameter.PadY2:         result := true;
    TPluginParameter.PadX3:         result := true;
    TPluginParameter.PadY3:         result := true;
    TPluginParameter.PadX4:         result := true;
    TPluginParameter.PadY4:         result := true;
  else
    result := false;
  end;
end;



function GetPluginParInfo(const Par : TPluginParameter):TPluginParameterInfo;
begin
  result.DefaultValue := 0.5;

  case Par of
    TPluginParameter.VoiceMode:                result.DefaultValue := 0;
    TPluginParameter.VoiceGlide:               result.DefaultValue := 0;
    TPluginParameter.PitchTracking:            result.DefaultValue := 0;
    TPluginParameter.SamplePlaybackType:       result.DefaultValue := 0;
    TPluginParameter.SampleResetClockSource:   result.DefaultValue := 0;
    TPluginParameter.SamplerLoopBounds:        result.DefaultValue := 1;
    TPluginParameter.SamplerLoopMode:          result.DefaultValue := 0;
    TPluginParameter.OutputGain:               result.DefaultValue := 0.5;
    TPluginParameter.OutputPan:                result.DefaultValue := 0.5;
    TPluginParameter.VoicePitchOne:            result.DefaultValue := 0.5;
    TPluginParameter.VoicePitchTwo:            result.DefaultValue := 0.5;
    TPluginParameter.SampleStart:              result.DefaultValue := 0;
    TPluginParameter.SampleEnd:                result.DefaultValue := 1;
    TPluginParameter.LoopStart:                result.DefaultValue := 0;
    TPluginParameter.LoopEnd:                  result.DefaultValue := 1;
    TPluginParameter.AmpAttack:                result.DefaultValue := 0;
    TPluginParameter.AmpHold:                  result.DefaultValue := 0;
    TPluginParameter.AmpDecay:                 result.DefaultValue := 0.3;
    TPluginParameter.AmpSustain:               result.DefaultValue := 0.3;
    TPluginParameter.AmpRelease:               result.DefaultValue := 0.3;
    TPluginParameter.AmpVelocity:              result.DefaultValue := 0.2;
    TPluginParameter.FilterAttack:             result.DefaultValue := 0;
    TPluginParameter.FilterHold:               result.DefaultValue := 0;
    TPluginParameter.FilterDecay:              result.DefaultValue := 0.3;
    TPluginParameter.FilterSustain:            result.DefaultValue := 0.3;
    TPluginParameter.FilterRelease:            result.DefaultValue := 0.3;
    TPluginParameter.FilterVelocity:           result.DefaultValue := 0.2;
    TPluginParameter.FilterRouting:            result.DefaultValue := 0;
    TPluginParameter.FilterOutputBlend:        result.DefaultValue := 1;
    TPluginParameter.Filter1Type:              result.DefaultValue := 0;
    TPluginParameter.Filter2Type:              result.DefaultValue := 0;
    TPluginParameter.Filter1KeyFollow:         result.DefaultValue := 0.5;
    TPluginParameter.Filter2KeyFollow:         result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par1:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par2:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par3:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par4:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par1:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par2:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par3:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par4:              result.DefaultValue := 0.5;
    TPluginParameter.Lfo1Shape:                result.DefaultValue := TLfoShapeHelper.ToSingle(TLfoShape.Triangle);
    TPluginParameter.Lfo2Shape:                result.DefaultValue := TLfoShapeHelper.ToSingle(TLfoShape.Triangle);
    TPluginParameter.Lfo1FreqMode:             result.DefaultValue := 0;
    TPluginParameter.Lfo2FreqMode:             result.DefaultValue := 0;
    TPluginParameter.Lfo1Range:                result.DefaultValue := 0.3;
    TPluginParameter.Lfo2Range:                result.DefaultValue := 0.3;
    TPluginParameter.Lfo1Par1:                 result.DefaultValue := 0.5;
    TPluginParameter.Lfo1Par2:                 result.DefaultValue := 0.5;
    TPluginParameter.Lfo1Par3:                 result.DefaultValue := 0.5;
    TPluginParameter.Lfo2Par1:                 result.DefaultValue := 0.5;
    TPluginParameter.Lfo2Par2:                 result.DefaultValue := 0.5;
    TPluginParameter.Lfo2Par3:                 result.DefaultValue := 0.5;
    TPluginParameter.Seq1Clock:                result.DefaultValue := TSequencerClockHelper.ToSingle(TSequencerClock.Div_4);
    TPluginParameter.Seq1Direction:            result.DefaultValue := 0;
    TPluginParameter.Seq1Length:               result.DefaultValue := TStepSequencerLengthHelper.ToSingle(TStepSequencerLength.Eight);
    TPluginParameter.Seq2Clock:                result.DefaultValue := TSequencerClockHelper.ToSingle(TSequencerClock.Div_4);
    TPluginParameter.Seq2Direction:            result.DefaultValue := 0;
    TPluginParameter.Seq2Length:               result.DefaultValue := TStepSequencerLengthHelper.ToSingle(TStepSequencerLength.Eight);
    TPluginParameter.PreviewVolume:            result.DefaultValue := 0.5;
    TPluginParameter.Preview:                  result.DefaultValue := 0.5;
    TPluginParameter.PadX1:                    result.DefaultValue := 0.5;
    TPluginParameter.PadY1:                    result.DefaultValue := 0.5;
    TPluginParameter.PadX2:                    result.DefaultValue := 0.5;
    TPluginParameter.PadY2:                    result.DefaultValue := 0.5;
    TPluginParameter.PadX3:                    result.DefaultValue := 0.5;
    TPluginParameter.PadY3:                    result.DefaultValue := 0.5;
    TPluginParameter.PadX4:                    result.DefaultValue := 0.5;
    TPluginParameter.PadY4:                    result.DefaultValue := 0.5;
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


function IsPublishedVstParameter(const Par : TPluginParameter):boolean;
begin
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
    TPluginParameter.PadX1: ;
    TPluginParameter.PadY1: ;
    TPluginParameter.PadX2: ;
    TPluginParameter.PadY2: ;
    TPluginParameter.PadX3: ;
    TPluginParameter.PadY3: ;
    TPluginParameter.PadX4: ;
    TPluginParameter.PadY4: ;
  else
    raise Exception.Create('Type not handled.');
  end;
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
