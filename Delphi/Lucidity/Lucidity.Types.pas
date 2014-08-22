unit Lucidity.Types;

interface

uses
  Lucidity.PluginParameters,
  Windows,
  uConstants,
  eeParSmoother,
  VamLib.UniqueID,
  VamLib.MoreTypes,
  VamLib.Collections.Lists;

{$SCOPEDENUMS ON}


type
  TGuidList = TSimpleList<TGUID>;


  // These structures are intended for storing the modulated parameter values.
  // I'm not entirely sure of the variable naming yet. It doesn't seem quite right.
  TModulatedPar = record
  public
    //Holds the actual parameter value. Range should be 0..1. This is also the target for the SmoothedParValue.
    ParValue  : single;
    SmoothedParValue : single; //range should be 0..1.

    IsSmoothingActive : boolean;
    ParSmootherState : TParSmootherState;

    //Stores the modulation amount for each mod slot. ModAmount range = -1 to 1.
    ModAmount : array[0..kModSlotCount-1] of single;

    // ModMin / ModMax stores the manimum and maximum modulation depth when the
    // considering all modulation sources. Given:
    //  - Slot One = 30%
    //  - Slot Two = 50%
    //  - Slot Three = -10%
    // Min Mod will be -10%
    // Max Slot will be 50%
    ModMin    : single;
    ModMax    : single;

    function IsModulated:boolean;

    procedure Reset;
  end;

  PModulatedPars = ^TModulatedPars;
  TModulatedPars = array[0..kModulatedParameterCount-1] of TModulatedPar;


  // TODO: the summed modulation is now calculated in the ModMatrix class
  // and stored in the TModulatedPar structure. This structure below should
  // be removed from use and deleted.
  //
  // NOTE: TParModulationData is a structure intended to hold summed modulation amounts
  // for all parameters.
  PParModulationData = ^TParModulationData;
  TParModulationData = record
  public
    // Raw[] contains the parameter value + the applied modulation value. It's
    // "Raw" because the array is addressed with a index, not a TPluginParameter value.
    Raw : array[0..kModulatedParameterCount-1] of single;
    // The combined modulation for a parameter. This shouldn't be needed in most places.
    // It might be better to try and remove it. Maybe there will be a slight  CPU bonus.
    SummedModulation : array[0..kModulatedParameterCount-1] of single;
    function GetModulatedParameterValue(const Par : TPluginParameter) : single; inline;
    function GetModulatedParameterPointer(const Par : TPluginParameter) : PSingle; inline;
  end;





  // NOTE: PSynthPar is intended for usage in
  // the voice processing classes. Individual modules need
  // access to the combined parameter value plus modulation amount.
  // This combined value is calculated by the Mod Matrix and
  // stored in the voice class. (If these comments are up to date!)
  PSynthPar = PSingle;

  TSampleError = (None, FileNotFound, ErrorLoadingData);

  PRegionProperties = ^TRegionProperties;
  TRegionProperties = record
    UniqueID         : TGuid;
    SampleFileName   : string;
    SampleDataLoaded : boolean;
    IsSampleError    : boolean;
    SampleErrorType  : TSampleError;
    ErrorMessage     : string;

    IsSelected     : boolean; //Multiple regions can be selected.
    IsFocused      : boolean; //only a single region should be 'focused' at once.

    //== Sample Map ==
    LowNote        : integer;
    HighNote       : integer;
    LowVelocity    : integer;
    HighVelocity   : integer;
    RootNote       : integer;

    //== Markers ==
    SampleStart    : integer;
    SampleEnd      : integer;
    RefLoopStart   : integer; // Loop start from source sample file.
    RefLoopEnd     : integer; // loop end from source sample file.
    UserLoopStart  : integer; // Loop start/end specified by user. This overrides
    UserLoopEnd    : integer; // the source sample file loop points.

    //== Sample ==
    SampleVolume  : single;  // In db. Range -96..+12
    SamplePan     : single;  // range -100..+100 (%)
    SampleTune    : integer; // Semitones. -24..+24
    SampleFine    : integer; // FineTune - Cents -100..100
    SampleBeats   : integer; // Length of sample in beats. Beats are used for looping.


    procedure GetRegionLoopPoints(out aLoopStart, aLoopEnd : integer);
  end;



  TKeyGroupID = TUniqueID;



  TSampleMarker = (smNone, smSampleStartMarker, smSampleEndMarker, smLoopStartMarker, smLoopEndMarker,
                   smSampleStartModMarker, smSampleEndModMarker, smLoopStartModMarker, smLoopEndModMarker);




  //=================================================================================================
  //     Load Info Data Classes.
  //=================================================================================================






  //=================================================================================================



implementation

{ TModulatedPar }

function TModulatedPar.IsModulated: boolean;
var
  c1 : integer;
begin
  for c1 := 0 to kModSlotCount-1 do
  begin
    if ModAmount[c1] <> 0 then exit(true);
  end;

  result := false;
end;

procedure TModulatedPar.Reset;
begin
  self.ParValue := 0;
  self.SmoothedParValue := 0;
  self.IsSmoothingActive := false;
  self.ParSmootherState.Reset(0);
end;


{ TParModulationData }

function TParModulationData.GetModulatedParameterValue(const Par: TPluginParameter): single;
begin
  // TODO: check asm for stack juggling.
  result := Raw[GetModParIndex(Par)];
end;

function TParModulationData.GetModulatedParameterPointer(const Par: TPluginParameter): PSingle;
begin
  // TODO: check asm for stack juggling.
  result := @Raw[GetModParIndex(Par)];
end;

{ TRegionProperties }

procedure TRegionProperties.GetRegionLoopPoints(out aLoopStart, aLoopEnd: integer);
begin
  //=== loop start ===
  if (self.UserLoopStart <> -1) then
  begin
    aLoopStart := self.UserLoopStart;
  end else
  if (self.RefLoopStart <> -1) then
  begin
    aLoopStart := self.RefLoopStart;
  end else
  begin
    aLoopStart := -1;
  end;

  //=== loop end ===
  if (self.UserLoopEnd <> -1) then
  begin
    aLoopEnd := self.UserLoopEnd;
  end else
  if (self.RefLoopEnd <> -1) then
  begin
    aLoopEnd := self.RefLoopEnd;
  end else
  begin
    aLoopEnd := -1;
  end;
end;

end.
