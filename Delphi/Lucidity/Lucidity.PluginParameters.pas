unit Lucidity.PluginParameters;

interface

uses
  Lucidity.Enums,
  uConstants,
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
  //=== forward declarations ================
  TKeyGroupStateBuffer = class;
  //=========================================


  TPluginParameterClass = class
  private
    fParameterValue: single;
    fName: string;
    fID: TPluginParameterID;
    fVstParameterIndex: integer;
    fIsQuantised: boolean;
    fQuantisedMin: integer;
    fQuantisedMax: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function IsPublishedVstParameter : boolean;

    property ParameterID : TPluginParameterID read fID;
    property Name        : string             read fName write fName;

    property VstParameterIndex : integer read fVstParameterIndex write fVstParameterIndex;

    property ParameterValue : single read fParameterValue write fParameterValue;

    property IsQuantised  : boolean read fIsQuantised  write fIsQuantised;
    property QuantisedMin : integer read fQuantisedMin write fQuantisedMin;
    property QuantisedMax : integer read fQuantisedMax write fQuantisedMax;
  end;


  TPluginParameterManager = class
  private
    CurrentCount    : integer; //TODO:MED CurrentCount is misleading and not needed anymore. It should be changed to parameter count.
    fParameterCount : integer;
    function GetParameter(Index: integer): TPluginParameterClass;
    procedure Add(const aParameter : TPluginParameterClass);
  public
    // NOTE: It's intended that all plugin parameters will be
    // created when the plugin is initialised. Once created, plugin parameters
    // will not be added, changed or deleted.
    Raw : array of TPluginParameterClass;

    constructor Create;
    destructor Destroy; override;

    function FindByName(const ParameterName : string):TPluginParameterClass;
    function FindByParameterID(const ParameterID : TPluginParameterID):TPluginParameterClass;

    property Parameter[Index : integer] : TPluginParameterClass read GetParameter;

    property Count : integer read CurrentCount;

    procedure AssignTo(var ParStateBuffer : TKeyGroupStateBuffer);
    procedure AssignFrom(const ParStateBuffer : TKeyGroupStateBuffer);
  end;

  TKeyGroupStateBuffer = class
  private
  protected type
    TParValue = record
      ParValue  : single; //range 0..1
      //Stores the modulation amount for each mod slot. ModAmount range = -1 to 1.
      ModAmount : array[0..kModSlotCount-1] of single;
    end;
  protected
    ParCount  : integer;
    ParValues : array of TParValue;
  public
    ModSourcePolarity : array[0..kModSlotCount-1] of TModSourcePolarity;
    ModSource : array[0..kModSlotCount-1] of TModSource;
    ModVia    : array[0..kModSlotCount-1] of TModSource;
    ModMute   : array[0..kModSlotCount-1] of boolean;

    constructor Create;
    destructor Destroy; override;

    function GetParameterValueByID(const ParameterID : TPluginParameterID):single;
    procedure SetParameterValueByID(const ParameterID : TPluginParameterID; const Value : single);

    function GetParameterModAmount(const ParameterID : TPluginParameterID; const ModSlot : integer):single;
    procedure SetParameterModAmount(const ParameterID : TPluginParameterID; const ModSlot : integer; const ModAmount:single);
  end;



  //========= Stuff below here will need to be reconsidered ====================


  TPluginParameter = (
    VoiceMode,
    VoiceGlide,
    PitchTracking,
    SamplePlaybackType,
    SampleResetClockSource,
    SamplerLoopBounds,
    SamplerTriggerMode,
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
    AmpEnvSnap,
    ModAttack,
    ModHold,
    ModDecay,
    ModSustain,
    ModRelease,
    ModVelocity,
    ModEnvSnap,
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
    SamplerTriggerMode      = 6;
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
    AmpEnvSnap              = 21;
    ModAttack               = 22;
    ModHold                 = 23;
    ModDecay                = 24;
    ModSustain              = 25;
    ModRelease              = 26;
    ModVelocity             = 27;
    ModEnvSnap              = 28;
    FilterRouting           = 29;
    FilterOutputBlend       = 30;
    Filter1Type             = 31;
    Filter2Type             = 32;
    Filter1KeyFollow        = 33;
    Filter2KeyFollow        = 34;
    Filter1Par1             = 35;
    Filter1Par2             = 36;
    Filter1Par3             = 37;
    Filter1Par4             = 38;
    Filter2Par1             = 39;
    Filter2Par2             = 40;
    Filter2Par3             = 41;
    Filter2Par4             = 42;
    Lfo1Shape               = 43;
    Lfo2Shape               = 44;
    Lfo1FreqMode            = 45;
    Lfo2FreqMode            = 46;
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
function PluginParToID(const Par : TPluginParameter):TPluginParameterID; inline;
function PluginParFromID(const ParID : TPluginParameterID):TPluginParameter; inline;

function PluginParToName(const Par : TPluginParameter):string;    //This should be the true enumerated name.
function PluginParFromName(const Name : string):TPluginParameter; //This should be the true enumerated name.

function PluginParToDisplayName(const Par : TPluginParameter):string;
function PluginParToDisplayValue(const Par : TPluginParameter; const PluginState : TPluginParameterManager):string;


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


function QuantiseParameterValue(const ParValue : single; const MinValue, MaxValue : integer):single;
function QuantiseParameterValueAndExpand(const ParValue : single; const MinValue, MaxValue : integer):integer;

implementation

uses
  Math,
  SysUtils,
  Rtti,
  TypInfo;


//============= NEW METHODS ===========================================
function QuantiseParameterValue(const ParValue : single; const MinValue, MaxValue : integer):single;
var
  x : integer;
begin
  assert(InRange(ParValue, 0, 1));
  assert(MinValue < MaxValue);

  x := round(ParValue * (MaxValue - MinValue));


  //if x > (MaxValue - MinValue) then x := (MaxValue - MinValue);

  result := x / (MaxValue - MinValue);


  {
  x := floor(ParValue * (MaxValue - MinValue)) - MinValue;

  if x > MaxValue
    then x := MaxValue;

  result := (x + MinValue) / (MaxValue - MinValue);
  }
  // exit checking...
  assert(InRange(Result, 0, 1));
end;

function QuantiseParameterValueAndExpand(const ParValue : single; const MinValue, MaxValue : integer):integer;
var
  x : integer;
begin
  assert(InRange(ParValue, 0, 1));
  assert(MinValue < MaxValue);

  x := round(ParValue * (MaxValue - MinValue)) + MinValue;

  //if x > MaxValue then x := MaxValue;

  result := x;
end;

function PluginParToID(const Par : TPluginParameter):TPluginParameterID;
begin
  result := Ord(Par);
end;

function PluginParFromID(const ParID : TPluginParameterID):TPluginParameter;
begin
  assert(ParID >= Ord(Low(TPluginParameter)));
  assert(ParID <= Ord(High(TPluginParameter)));

  result := TPluginParameter(ParID);
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
    kPluginParameterID.ModAttack:          ModParIndex := 13;
    kPluginParameterID.ModHold:            ModParIndex := 14;
    kPluginParameterID.ModDecay:           ModParIndex := 15;
    kPluginParameterID.ModSustain:         ModParIndex := 16;
    kPluginParameterID.ModRelease:         ModParIndex := 17;
    kPluginParameterID.FilterOutputBlend:  ModParIndex := 18;
    kPluginParameterID.Filter1Par1:        ModParIndex := 19;
    kPluginParameterID.Filter1Par2:        ModParIndex := 20;
    kPluginParameterID.Filter1Par3:        ModParIndex := 21;
    kPluginParameterID.Filter1Par4:        ModParIndex := 22;
    kPluginParameterID.Filter2Par1:        ModParIndex := 23;
    kPluginParameterID.Filter2Par2:        ModParIndex := 24;
    kPluginParameterID.Filter2Par3:        ModParIndex := 25;
    kPluginParameterID.Filter2Par4:        ModParIndex := 26;
    kPluginParameterID.Lfo1Par1:           ModParIndex := 27;
    kPluginParameterID.Lfo1Par2:           ModParIndex := 28;
    kPluginParameterID.Lfo1Par3:           ModParIndex := 29;
    kPluginParameterID.Lfo2Par1:           ModParIndex := 30;
    kPluginParameterID.Lfo2Par2:           ModParIndex := 31;
    kPluginParameterID.Lfo2Par3:           ModParIndex := 32;
  else
    ModParIndex := -1;
  end;

  assert(kModulatedParameterCount = 33);

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
    TPluginParameter.SamplerTriggerMode:       result.DefaultValue := TKeyGroupTriggerModeHelper.ToSingle(TKeyGroupTriggerMode.LoopOff);
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
    TPluginParameter.AmpEnvSnap:               result.DefaultValue := 0;
    TPluginParameter.ModAttack:                result.DefaultValue := 0;
    TPluginParameter.ModHold:                  result.DefaultValue := 0;
    TPluginParameter.ModDecay:                 result.DefaultValue := 0.3;
    TPluginParameter.ModSustain:               result.DefaultValue := 0.3;
    TPluginParameter.ModRelease:               result.DefaultValue := 0.3;
    TPluginParameter.ModVelocity:              result.DefaultValue := 0.2;
    TPluginParameter.ModEnvSnap:               result.DefaultValue := 0;
    TPluginParameter.FilterRouting:            result.DefaultValue := 0;
    TPluginParameter.FilterOutputBlend:        result.DefaultValue := 1;
    TPluginParameter.Filter1Type:              result.DefaultValue := 0;
    TPluginParameter.Filter2Type:              result.DefaultValue := 0;
    TPluginParameter.Filter1KeyFollow:         result.DefaultValue := 0.5;
    TPluginParameter.Filter2KeyFollow:         result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par1:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par2:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par3:              result.DefaultValue := 0.5;
    TPluginParameter.Filter1Par4:              result.DefaultValue := 1;
    TPluginParameter.Filter2Par1:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par2:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par3:              result.DefaultValue := 0.5;
    TPluginParameter.Filter2Par4:              result.DefaultValue := 1;
    TPluginParameter.Lfo1Shape:                result.DefaultValue := TLfoShapeHelper.ToSingle(TLfoShape.Triangle);
    TPluginParameter.Lfo2Shape:                result.DefaultValue := TLfoShapeHelper.ToSingle(TLfoShape.Triangle);
    TPluginParameter.Lfo1FreqMode:             result.DefaultValue := 0;
    TPluginParameter.Lfo2FreqMode:             result.DefaultValue := 0;
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
  fIsQuantised := false;

  fQuantisedMin := 0;
  fQuantisedMax := 100;
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

constructor TPluginParameterManager.Create;
var
  c1: Integer;
  aPar      : TPluginParameter;
  aParCount : integer;
  aParClass : TPluginParameterClass;
begin
  aParCount := TPluginParameterHelper.GetEnumTypeCount;


  fParameterCount := aParCount;
  SetLength(Raw, aParCount);

  CurrentCount := 0;

  for c1 := 0 to aParCount-1 do
  begin
    aPar := PluginParFromID(c1);

    aParClass := TPluginParameterClass.Create;
    aParClass.Name := PluginParToName(aPar);
    aParClass.fID := c1;

    Add(aParClass);
  end;




  //====== More parameter setup ===============
  aParClass := FindByName(PluginParToName(TPluginParameter.VoicePitchOne));
  aParClass.IsQuantised := true;
  aParClass.QuantisedMin := -24;
  aParClass.QuantisedMax := 24;





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

procedure TPluginParameterManager.AssignFrom(const ParStateBuffer: TKeyGroupStateBuffer);
var
  c1: Integer;
  ParValue : single;
begin
  for c1 := 0 to CurrentCount-1 do
  begin
    assert(self.Raw[c1].ParameterID = c1);

    ParValue := ParStateBuffer.GetParameterValueByID(c1);

    if Raw[c1].IsQuantised then
    begin
      ParValue := QuantiseParameterValue(ParValue, Raw[c1].QuantisedMin, Raw[c1].QuantisedMax);
    end;

    Raw[c1].ParameterValue := ParValue;
  end;
end;

procedure TPluginParameterManager.AssignTo(var ParStateBuffer: TKeyGroupStateBuffer);
var
  c1: Integer;
begin
  for c1 := 0 to CurrentCount-1 do
  begin
    assert(self.Raw[c1].ParameterID = c1);
    ParStateBuffer.SetParameterValueByID(c1, self.Raw[c1].ParameterValue);
  end;
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
  inc(CurrentCount);
end;

function TPluginParameterManager.GetParameter(Index: integer): TPluginParameterClass;
begin
  assert(Index >= 0);
  assert(Index < fParameterCount);
  result := Raw[Index];
end;

function PluginParToDisplayName(const Par : TPluginParameter):string;
begin
  case Par of
    TPluginParameter.VoicePitchOne:           result := 'Tune';
    TPluginParameter.VoicePitchTwo:           result := 'Fine';
  else
    result := '';
  end;
  {
  case Par of
    TPluginParameter.VoiceMode:               result := 'Voice Mode';
    TPluginParameter.VoiceGlide:              result := 'Glide';
    TPluginParameter.PitchTracking:           result := 'Tracking';
    TPluginParameter.SamplePlaybackType:      result := 'Playback';
    TPluginParameter.SampleResetClockSource:  result := 'Reset';
    TPluginParameter.SamplerLoopBounds:       result := 'Loop Bounds';
    TPluginParameter.SamplerTriggerMode:         result := 'Loop Mode';
    TPluginParameter.OutputGain:              result := 'Gain';
    TPluginParameter.OutputPan:               result := 'Pan';
    TPluginParameter.VoicePitchOne:           result := 'Tune';
    TPluginParameter.VoicePitchTwo:           result := 'Fine';
    TPluginParameter.SampleStart:             result := 'Sample Start';
    TPluginParameter.SampleEnd:               result := 'Sample End';
    TPluginParameter.LoopStart:               result := 'Loop Start';
    TPluginParameter.LoopEnd:                 result := 'Loop End';
    TPluginParameter.AmpAttack:               result := 'Attack';
    TPluginParameter.AmpHold:                 result := 'Hold';
    TPluginParameter.AmpDecay:                result := 'Decay';
    TPluginParameter.AmpSustain:              result := 'Sustain';
    TPluginParameter.AmpRelease:              result := 'Release';
    TPluginParameter.AmpVelocity:             result := 'Velocity ';
    TPluginParameter.ModAttack:               result := 'Attack';
    TPluginParameter.ModHold:                 result := 'Hold';
    TPluginParameter.ModDecay:                result := 'Decay';
    TPluginParameter.ModSustain:              result := 'Sustain';
    TPluginParameter.ModRelease:              result := 'Release';
    TPluginParameter.ModVelocity:             result := 'Velocity';
    TPluginParameter.FilterRouting:           result := 'Filter Routing';
    TPluginParameter.FilterOutputBlend:       result := 'Filter Output';
    TPluginParameter.Filter1Type:             result := 'Filter Type';
    TPluginParameter.Filter2Type:             result := 'Filter Type';
    TPluginParameter.Filter1KeyFollow:        result := 'Key Follow';
    TPluginParameter.Filter2KeyFollow:        result := 'Key Follow';
    TPluginParameter.Filter1Par1:             result := 'Parameter One';
    TPluginParameter.Filter1Par2:             result := 'Parameter Two';
    TPluginParameter.Filter1Par3:             result := 'Parameter Three';
    TPluginParameter.Filter1Par4:             result := 'Parameter Four';
    TPluginParameter.Filter2Par1:             result := 'Parameter One';
    TPluginParameter.Filter2Par2:             result := 'Parameter Two';
    TPluginParameter.Filter2Par3:             result := 'Parameter Three';
    TPluginParameter.Filter2Par4:             result := 'Parameter Four';
    TPluginParameter.Lfo1Shape:               result := 'LFO Shape';
    TPluginParameter.Lfo2Shape:               result := 'LFO Shape';
    TPluginParameter.Lfo1FreqMode:            result := 'LFO Frequency';
    TPluginParameter.Lfo2FreqMode:            result := 'LFO Frequency';
    TPluginParameter.Lfo1Range:               result := 'LFO Range';
    TPluginParameter.Lfo2Range:               result := 'LFO Range';
    TPluginParameter.Lfo1Par1:                result := 'Parameter One';
    TPluginParameter.Lfo1Par2:                result := 'Parameter Two';
    TPluginParameter.Lfo1Par3:                result := 'Parameter Three';
    TPluginParameter.Lfo2Par1:                result := 'Parameter One';
    TPluginParameter.Lfo2Par2:                result := 'Parameter Two';
    TPluginParameter.Lfo2Par3:                result := 'Parameter Three';
    TPluginParameter.Seq1Clock:               result := 'Clock';
    TPluginParameter.Seq1Direction:           result := 'Direction';
    TPluginParameter.Seq1Length:              result := 'Length';
    TPluginParameter.Seq2Clock:               result := 'Clock';
    TPluginParameter.Seq2Direction:           result := 'Direction';
    TPluginParameter.Seq2Length:              result := 'Length';
    TPluginParameter.PreviewVolume:           result := 'Preview Volume';
    TPluginParameter.Preview:                 result := 'Preview On/Off';
    TPluginParameter.PadX1:                   result := 'X1';
    TPluginParameter.PadY1:                   result := 'Y1';
    TPluginParameter.PadX2:                   result := 'X2';
    TPluginParameter.PadY2:                   result := 'Y2';
    TPluginParameter.PadX3:                   result := 'X3';
    TPluginParameter.PadY3:                   result := 'Y3';
    TPluginParameter.PadX4:                   result := 'X4';
    TPluginParameter.PadY4:                   result := 'Y4';
  else
    result := '';
  end;
  }
end;

function PluginParToDisplayValue(const Par : TPluginParameter; const PluginState : TPluginParameterManager):string;
var
  ParID    : TPluginParameterID;
  ParValue : single;
begin
  ParID := PluginParToID(Par);
  ParValue := PluginState.FindByParameterID(ParID).ParameterValue;

  case Par of
    TPluginParameter.VoicePitchOne: result := IntToStr(QuantiseParameterValueAndExpand(ParValue, -24, 24)) + ' st';
    TPluginParameter.VoicePitchTwo: result := IntToStr(QuantiseParameterValueAndExpand(ParValue, -100, 100)) + ' cnt';
  else
    result := IntToStr(round(ParValue * 100));
  end;
end;



{ TPluginParameterStateBuffer }

constructor TKeyGroupStateBuffer.Create;
begin
  ParCount := TPluginParameterHelper.GetEnumTypeCount;
  SetLength(ParValues, ParCount);
end;

destructor TKeyGroupStateBuffer.Destroy;
begin
  SetLength(ParValues, 0);
  inherited;
end;

function TKeyGroupStateBuffer.GetParameterValueByID(const ParameterID: TPluginParameterID): single;
begin
  result := ParValues[ParameterID].ParValue;
end;

procedure TKeyGroupStateBuffer.SetParameterValueByID(const ParameterID: TPluginParameterID; const Value: single);
begin
  ParValues[ParameterID].ParValue := Value;
end;

function TKeyGroupStateBuffer.GetParameterModAmount(const ParameterID: TPluginParameterID; const ModSlot: integer): single;
begin
  result := ParValues[ParameterID].ModAmount[ModSlot];
end;

procedure TKeyGroupStateBuffer.SetParameterModAmount(const ParameterID: TPluginParameterID; const ModSlot: integer; const ModAmount: single);
begin
  ParValues[ParameterID].ModAmount[ModSlot] := ModAmount;
end;



initialization
    //===== IMPORTANT: Check all the plugin parameter ID constant values are correct =============
    assert(kPluginParameterID.VoiceMode                    = Integer(TPluginParameter.VoiceMode));
    assert(kPluginParameterID.VoiceGlide                   = Integer(TPluginParameter.VoiceGlide));
    assert(kPluginParameterID.PitchTracking                = Integer(TPluginParameter.PitchTracking));
    assert(kPluginParameterID.SamplePlaybackType           = Integer(TPluginParameter.SamplePlaybackType));
    assert(kPluginParameterID.SampleResetClockSource       = Integer(TPluginParameter.SampleResetClockSource));
    assert(kPluginParameterID.SamplerLoopBounds            = Integer(TPluginParameter.SamplerLoopBounds));
    assert(kPluginParameterID.SamplerTriggerMode           = Integer(TPluginParameter.SamplerTriggerMode));
    assert(kPluginParameterID.OutputGain                   = Integer(TPluginParameter.OutputGain));
    assert(kPluginParameterID.OutputPan                    = Integer(TPluginParameter.OutputPan));
    assert(kPluginParameterID.VoicePitchOne                = Integer(TPluginParameter.VoicePitchOne));
    assert(kPluginParameterID.VoicePitchTwo                = Integer(TPluginParameter.VoicePitchTwo));
    assert(kPluginParameterID.SampleStart                  = Integer(TPluginParameter.SampleStart));
    assert(kPluginParameterID.SampleEnd                    = Integer(TPluginParameter.SampleEnd));
    assert(kPluginParameterID.LoopStart                    = Integer(TPluginParameter.LoopStart));
    assert(kPluginParameterID.LoopEnd                      = Integer(TPluginParameter.LoopEnd));
    assert(kPluginParameterID.AmpAttack                    = Integer(TPluginParameter.AmpAttack));
    assert(kPluginParameterID.AmpHold                      = Integer(TPluginParameter.AmpHold));
    assert(kPluginParameterID.AmpDecay                     = Integer(TPluginParameter.AmpDecay));
    assert(kPluginParameterID.AmpSustain                   = Integer(TPluginParameter.AmpSustain));
    assert(kPluginParameterID.AmpRelease                   = Integer(TPluginParameter.AmpRelease));
    assert(kPluginParameterID.AmpVelocity                  = Integer(TPluginParameter.AmpVelocity));
    assert(kPluginParameterID.AmpEnvSnap                   = Integer(TPluginParameter.AmpEnvSnap));
    assert(kPluginParameterID.ModAttack                    = Integer(TPluginParameter.ModAttack));
    assert(kPluginParameterID.ModHold                      = Integer(TPluginParameter.ModHold));
    assert(kPluginParameterID.ModDecay                     = Integer(TPluginParameter.ModDecay));
    assert(kPluginParameterID.ModSustain                   = Integer(TPluginParameter.ModSustain));
    assert(kPluginParameterID.ModRelease                   = Integer(TPluginParameter.ModRelease));
    assert(kPluginParameterID.ModVelocity                  = Integer(TPluginParameter.ModVelocity));
    assert(kPluginParameterID.ModEnvSnap                   = Integer(TPluginParameter.ModEnvSnap));
    assert(kPluginParameterID.FilterRouting                = Integer(TPluginParameter.FilterRouting));
    assert(kPluginParameterID.FilterOutputBlend            = Integer(TPluginParameter.FilterOutputBlend));
    assert(kPluginParameterID.Filter1Type                  = Integer(TPluginParameter.Filter1Type));
    assert(kPluginParameterID.Filter2Type                  = Integer(TPluginParameter.Filter2Type));
    assert(kPluginParameterID.Filter1KeyFollow             = Integer(TPluginParameter.Filter1KeyFollow));
    assert(kPluginParameterID.Filter2KeyFollow             = Integer(TPluginParameter.Filter2KeyFollow));
    assert(kPluginParameterID.Filter1Par1                  = Integer(TPluginParameter.Filter1Par1));
    assert(kPluginParameterID.Filter1Par2                  = Integer(TPluginParameter.Filter1Par2));
    assert(kPluginParameterID.Filter1Par3                  = Integer(TPluginParameter.Filter1Par3));
    assert(kPluginParameterID.Filter1Par4                  = Integer(TPluginParameter.Filter1Par4));
    assert(kPluginParameterID.Filter2Par1                  = Integer(TPluginParameter.Filter2Par1));
    assert(kPluginParameterID.Filter2Par2                  = Integer(TPluginParameter.Filter2Par2));
    assert(kPluginParameterID.Filter2Par3                  = Integer(TPluginParameter.Filter2Par3));
    assert(kPluginParameterID.Filter2Par4                  = Integer(TPluginParameter.Filter2Par4));
    assert(kPluginParameterID.Lfo1Shape                    = Integer(TPluginParameter.Lfo1Shape));
    assert(kPluginParameterID.Lfo2Shape                    = Integer(TPluginParameter.Lfo2Shape));
    assert(kPluginParameterID.Lfo1FreqMode                 = Integer(TPluginParameter.Lfo1FreqMode));
    assert(kPluginParameterID.Lfo2FreqMode                 = Integer(TPluginParameter.Lfo2FreqMode));
    assert(kPluginParameterID.Lfo1Par1                     = Integer(TPluginParameter.Lfo1Par1));
    assert(kPluginParameterID.Lfo1Par2                     = Integer(TPluginParameter.Lfo1Par2));
    assert(kPluginParameterID.Lfo1Par3                     = Integer(TPluginParameter.Lfo1Par3));
    assert(kPluginParameterID.Lfo2Par1                     = Integer(TPluginParameter.Lfo2Par1));
    assert(kPluginParameterID.Lfo2Par2                     = Integer(TPluginParameter.Lfo2Par2));
    assert(kPluginParameterID.Lfo2Par3                     = Integer(TPluginParameter.Lfo2Par3));
    assert(kPluginParameterID.Seq1Clock                    = Integer(TPluginParameter.Seq1Clock));
    assert(kPluginParameterID.Seq1Direction                = Integer(TPluginParameter.Seq1Direction));
    assert(kPluginParameterID.Seq1Length                   = Integer(TPluginParameter.Seq1Length));
    assert(kPluginParameterID.Seq2Clock                    = Integer(TPluginParameter.Seq2Clock));
    assert(kPluginParameterID.Seq2Direction                = Integer(TPluginParameter.Seq2Direction));
    assert(kPluginParameterID.Seq2Length                   = Integer(TPluginParameter.Seq2Length));
    assert(kPluginParameterID.PreviewVolume                = Integer(TPluginParameter.PreviewVolume));
    assert(kPluginParameterID.Preview                      = Integer(TPluginParameter.Preview));
    assert(kPluginParameterID.PadX1                        = Integer(TPluginParameter.PadX1));
    assert(kPluginParameterID.PadY1                        = Integer(TPluginParameter.PadY1));
    assert(kPluginParameterID.PadX2                        = Integer(TPluginParameter.PadX2));
    assert(kPluginParameterID.PadY2                        = Integer(TPluginParameter.PadY2));
    assert(kPluginParameterID.PadX3                        = Integer(TPluginParameter.PadX3));
    assert(kPluginParameterID.PadY3                        = Integer(TPluginParameter.PadY3));
    assert(kPluginParameterID.PadX4                        = Integer(TPluginParameter.PadX4));
    assert(kPluginParameterID.PadY4                        = Integer(TPluginParameter.PadY4));

    assert(kPluginParameterID.ParameterCount = TPluginParameterHelper.GetEnumTypeCount);

finalization


end.
