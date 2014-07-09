unit Lucidity.PluginParameters;

interface

uses
  eeTypes,
  eeEnumHelper;

{
======= Goal ===============

Currently methods are address using the plugin parameter name
as a string. To avoid having hardcode strings scattered everywhere
I am using a TPluginParameter enumerated type that is
converted to and from the parameter name as needed. This makes for
easy usage but I'm getting CPU spikes when automating parameters.

I think some of the code to convert to and from strings is a bit slow.

My goal is to use integer parameter ID's instead of parameter name strings.

After that I may also rewrite the way parameter changes are applied to
keygroups, voices, etc. Instead of using the PluginParameterController
I might use the ZeroObject system to send messages.


NOTE: At the moment the parameter system is kind of two systems mashed together.
== TPluginParameter and TPluginParameterClass ==
TPluginParameter came first and I've refactored some sections so that TPluginParameter
is being used instead.
It's all a bit of a mess.
}




{$SCOPEDENUMS ON}

type
  TPluginParameterClass = class
  private
    fParameterValue: single;
    fName: string;
    fID: TPluginParameterID;
    fVstParameterIndex: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function IsPublishedVstParameter : boolean;


    property ParameterID : TPluginParameterID read fID;
    property Name        : string             read fName write fName;

    property VstParameterIndex : integer read fVstParameterIndex write fVstParameterIndex;

    property ParameterValue : single read fParameterValue write fParameterValue;
  end;


  TPluginParameterManager = class
  private
    CurrentCount    : integer;
    fParameterCount : integer;
    function GetParameter(Index: integer): TPluginParameterClass;
  public
    // NOTE: It's intended that all plugin parameters will be
    // created when the plugin is initialised. Once created, plugin parameters
    // will not be added, changed or deleted.
    Raw : array of TPluginParameterClass;

    constructor Create(const aParameterCount : integer);
    destructor Destroy; override;

    function FindByName(const ParameterName : string):TPluginParameterClass;
    function FindByParameterID(const ParameterID : TPluginParameterID):TPluginParameterClass;

    procedure Add(const aParameter : TPluginParameterClass);

    property Parameter[Index : integer] : TPluginParameterClass read GetParameter;

    property Count : integer read CurrentCount;
  end;



  //========= Stuff below here will need to be reconsidered ====================


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
    ModAttack,
    ModHold,
    ModDecay,
    ModSustain,
    ModRelease,
    ModVelocity,
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

  kPluginParameterID = record
  const
    VoiceMode               = 0;
    VoiceGlide              = 1;
    PitchTracking           = 2;
    SamplePlaybackType      = 3;
    SampleResetClockSource  = 4;
    SamplerLoopBounds       = 5;
    SamplerLoopMode         = 6;
    OutputGain              = 7;
    OutputPan               = 8;
    VoicePitchOne           = 9;
    VoicePitchTwo           = 10;
    SampleStart             = 11;
    SampleEnd               = 12;
    LoopStart               = 13;
    LoopEnd                 = 14;
    AmpAttack               = 15;
    AmpHold                 = 16;
    AmpDecay                = 17;
    AmpSustain              = 18;
    AmpRelease              = 19;
    AmpVelocity             = 20;
    ModAttack               = 21;
    ModHold                 = 22;
    ModDecay                = 23;
    ModSustain              = 24;
    ModRelease              = 25;
    ModVelocity             = 26;
    FilterRouting           = 27;
    FilterOutputBlend       = 28;
    Filter1Type             = 29;
    Filter2Type             = 30;
    Filter1KeyFollow        = 31;
    Filter2KeyFollow        = 32;
    Filter1Par1             = 33;
    Filter1Par2             = 34;
    Filter1Par3             = 35;
    Filter1Par4             = 36;
    Filter2Par1             = 37;
    Filter2Par2             = 38;
    Filter2Par3             = 39;
    Filter2Par4             = 40;
    Lfo1Shape               = 41;
    Lfo2Shape               = 42;
    Lfo1FreqMode            = 43;
    Lfo2FreqMode            = 44;
    Lfo1Range               = 45;
    Lfo2Range               = 46;
    Lfo1Par1                = 47;
    Lfo1Par2                = 48;
    Lfo1Par3                = 49;
    Lfo2Par1                = 50;
    Lfo2Par2                = 51;
    Lfo2Par3                = 52;
    Seq1Clock               = 53;
    Seq1Direction           = 54;
    Seq1Length              = 55;
    Seq2Clock               = 56;
    Seq2Direction           = 57;
    Seq2Length              = 58;
    PreviewVolume           = 59;
    Preview                 = 60;
    PadX1                   = 61;
    PadY1                   = 62;
    PadX2                   = 63;
    PadY2                   = 64;
    PadX3                   = 65;
    PadY3                   = 66;
    PadX4                   = 67;
    PadY4                   = 68;
    //=========================
    ParameterCount          = 69;
    //=========================
  end;

  TPluginParameterInfo = record
  public
    DefaultValue : single;
  end;


//======= New functions =================================================

procedure CheckPluginParIDs;
function PluginParToID(const Par : TPluginParameter):TPluginParameterID; inline;
function PluginParFromID(const Par : TPluginParameterID):TPluginParameter; inline;

function PluginParToName(const Par : TPluginParameter):string;
function PluginParFromName(const Name : string):TPluginParameter;

// TODO:MED these methods feel like a stop-gap measure and might be better
// if removed eventually.
function PluginParNameToID(const Name : string):TPluginParameterID; inline;
function PluginParIDToName(const ID : TPluginParameterID):string; inline;

// TODO:HIGH Inline these methods.
function IsModPar(const Par : TPluginParameterID; out ModParIndex:integer):boolean; {inline;} overload;
function IsModPar(const Par : TPluginParameter):boolean; {inline;} overload; //deprecated; //TODO:MED deprecate this method.

function GetModParIndex(const Par : TPluginParameter):integer; //inline;

///===== All functions below this line will need to be reconsidered =====

function GetPluginParInfo(const Par : TPluginParameter):TPluginParameterInfo;

function IsValidPluginParName(const Name : string):boolean;


// "Global Plugin Parameters" are members of the TeePlugin class.
// They are appliced globally and effect all voices. Other non-global
// parameters are generally applied to "Key Groups".
function IsGlobalPluginPar(const Par : TPluginParameter):boolean; inline;

function GetPluginParameterCount:integer; inline;
function IndexToPluginParameter(Index : integer):TPluginParameter; inline;

implementation

uses
  uConstants,
  SysUtils,
  Rtti,
  TypInfo,
  uLucidityEnums;


//============= NEW METHODS ===========================================

procedure CheckPluginParIDs;
var
  x1 : integer;
  x2 : integer;
begin
  x1 := kPluginParameterID.VoiceMode;
  x2 := PluginParToID(TPluginParameter.VoiceMode);
  if x1 <> x2 then raise Exception.Create('Plugin Parameter IDs do not match.');

  // TODO:HIGH
  // All plugin ID's need to be checked.
end;

function PluginParToID(const Par : TPluginParameter):TPluginParameterID;
begin
  result := Ord(Par);
end;

function PluginParFromID(const Par : TPluginParameterID):TPluginParameter;
begin
  assert(Par >= Ord(Low(TPluginParameter)));
  assert(Par <= Ord(High(TPluginParameter)));

  result := TPluginParameter(Par);
end;

function PluginParToName(const Par : TPluginParameter):string;
begin
  result := GetEnumName(TypeInfo(TPluginParameter), integer(Par));
end;

function PluginParFromName(const Name : string):TPluginParameter;
begin
  result := TPluginParameter(GetEnumValue(TypeInfo(TPluginParameter),Name));
end;

function PluginParNameToID(const Name : string):TPluginParameterID;
var
  Par : TPluginParameter;
begin
  Par := PluginParFromName(Name);
  result := PluginParToID(Par);
end;

function PluginParIDToName(const ID : TPluginParameterID):string;
var
  Par : TPluginParameter;
begin
  Par := PluginParFromID(ID);
  result := PluginParToName(Par);
end;


function IsModPar(const Par : TPluginParameterID; out ModParIndex:integer):boolean;
begin
  case Par of
    kPluginParameterID.OutputGain:         ModParIndex := 0;
    kPluginParameterID.OutputPan:          ModParIndex := 1;
    kPluginParameterID.VoicePitchOne:      ModParIndex := 2;
    kPluginParameterID.VoicePitchTwo:      ModParIndex := 3;
    kPluginParameterID.SampleStart:        ModParIndex := 4;
    kPluginParameterID.SampleEnd:          ModParIndex := 5;
    kPluginParameterID.LoopStart:          ModParIndex := 6;
    kPluginParameterID.LoopEnd:            ModParIndex := 7;
    kPluginParameterID.AmpAttack:          ModParIndex := 8;
    kPluginParameterID.AmpHold:            ModParIndex := 9;
    kPluginParameterID.AmpDecay:           ModParIndex := 10;
    kPluginParameterID.AmpSustain:         ModParIndex := 11;
    kPluginParameterID.AmpRelease:         ModParIndex := 12;
    kPluginParameterID.AmpVelocity:        ModParIndex := 13;
    kPluginParameterID.ModAttack:          ModParIndex := 14;
    kPluginParameterID.ModHold:            ModParIndex := 15;
    kPluginParameterID.ModDecay:           ModParIndex := 16;
    kPluginParameterID.ModSustain:         ModParIndex := 17;
    kPluginParameterID.ModRelease:         ModParIndex := 18;
    kPluginParameterID.ModVelocity:        ModParIndex := 19;
    kPluginParameterID.FilterOutputBlend:  ModParIndex := 20;
    kPluginParameterID.Filter1Par1:        ModParIndex := 21;
    kPluginParameterID.Filter1Par2:        ModParIndex := 22;
    kPluginParameterID.Filter1Par3:        ModParIndex := 23;
    kPluginParameterID.Filter1Par4:        ModParIndex := 24;
    kPluginParameterID.Filter2Par1:        ModParIndex := 25;
    kPluginParameterID.Filter2Par2:        ModParIndex := 26;
    kPluginParameterID.Filter2Par3:        ModParIndex := 27;
    kPluginParameterID.Filter2Par4:        ModParIndex := 28;
    kPluginParameterID.Lfo1Par1:           ModParIndex := 29;
    kPluginParameterID.Lfo1Par2:           ModParIndex := 30;
    kPluginParameterID.Lfo1Par3:           ModParIndex := 31;
    kPluginParameterID.Lfo2Par1:           ModParIndex := 32;
    kPluginParameterID.Lfo2Par2:           ModParIndex := 33;
    kPluginParameterID.Lfo2Par3:           ModParIndex := 34;
  else
    ModParIndex := -1;
  end;

  assert(kModulatedParameterCount = 35);

  if ModParIndex <> -1
    then result := true
    else result := false;
end;

function IsModPar(const Par : TPluginParameter):boolean;
var
  ID : TPluginParameterID;
  ModIndex : integer;
begin
  ID := PluginParToID(Par);
  result := IsModPar(ID, ModIndex);
end;

function GetModParIndex(const Par : TPluginParameter):integer;
var
  ID : TPluginParameterID;
  ModIndex : integer;
begin
  ID := PluginParToID(Par);
  if IsModPar(ID, ModIndex)
    then result := ModIndex
    else result := -1;
  //result := ModIndex;
end;


//============= OLD METHODS ===========================================
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
    TPluginParameter.ModAttack:             result.DefaultValue := 0;
    TPluginParameter.ModHold:               result.DefaultValue := 0;
    TPluginParameter.ModDecay:              result.DefaultValue := 0.3;
    TPluginParameter.ModSustain:            result.DefaultValue := 0.3;
    TPluginParameter.ModRelease:            result.DefaultValue := 0.3;
    TPluginParameter.ModVelocity:           result.DefaultValue := 0.2;
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
  //TODO:HIGH reliance on TPluginParameter needs to be removed.
  assert(Index >= 0);
  assert(Index <= TPluginParameterHelper.GetEnumTypeCount);
  result := TPluginParameterHelper.ToEnum(Index);
end;

{
var
  c1 : integer;
  Par : TPluginParameter;
}
{ TPluginParameterClass }

constructor TPluginParameterClass.Create;
begin
  fID := 0;
  fVstParameterIndex := -1;
end;

destructor TPluginParameterClass.Destroy;
begin

  inherited;
end;

function TPluginParameterClass.IsPublishedVstParameter: boolean;
begin
  if fVstParameterIndex >= 0
    then result := true
    else result := false;
end;

{ TPluginParameterManager }

constructor TPluginParameterManager.Create(const aParameterCount: integer);
begin
  fParameterCount := aParameterCount;
  SetLength(Raw, fParameterCount);

  CurrentCount := 0;
end;

destructor TPluginParameterManager.Destroy;
var
  c1: Integer;
begin
  for c1 := 0 to CurrentCount-1 do
  begin
    Raw[c1].Free;
  end;

  SetLength(Raw, 0);

  inherited;
end;

function TPluginParameterManager.FindByName(const ParameterName: string): TPluginParameterClass;
var
  c1: Integer;
begin
  for c1 := 0 to CurrentCount-1 do
  begin
    if Raw[c1].Name = ParameterName
      then exit(Raw[c1]);
  end;

  // No result found if we make it this far.
  result := nil;
end;

function TPluginParameterManager.FindByParameterID(const ParameterID: TPluginParameterID): TPluginParameterClass;
begin
  result := Raw[ParameterID];
end;

procedure TPluginParameterManager.Add(const aParameter: TPluginParameterClass);
begin
  Raw[CurrentCount] := aParameter;
  aParameter.fID := CurrentCount;
  inc(CurrentCount);
end;



function TPluginParameterManager.GetParameter(Index: integer): TPluginParameterClass;
begin
  assert(Index >= 0);
  assert(Index < fParameterCount);
  result := Raw[Index];
end;

initialization
  //==========================
  // TODO:HIGH in debug mode only.
  CheckPluginParIDs;
  //==========================
finalization


end.
