unit Lucidity.Types;

interface

uses
  Windows,
  uConstants,
  VamLib.MoreTypes,
  VamLib.Collections.Lists;


type
  TGuidList = TSimpleList<TGUID>;


  // These structures are intended for storing the modulated parameter values.
  // I'm not entirely sure of the variable naming yet. It doesn't seem quite right.
  TModulatedPar = record
  public
    //Holds the actual parameter value. Range should be 0..1
    ParValue  : single;

    // The above Parameter value with the modulation applied.
    // range should be 0..1
    ModulatedParValue : single;

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
  TParModulationData = array[0..kModulatedParameterCount-1] of single;


  // NOTE: PSynthPar is intended for usage in
  // the voice processing classes. Individual modules need
  // access to the combined parameter value plus modulation amount.
  // This combined value is calculated by the Mod Matrix and
  // stored in the voice class. (If these comments are up to date!)
  PSynthPar = PSingle;







  PRegionProperties = ^TRegionProperties;
  TRegionProperties = record
    UniqueID         : TGuid;
    SampleFileName   : string;
    SampleDataLoaded : boolean;
    IsSampleError    : boolean;
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
    SampleStart : integer;
    SampleEnd   : integer;
    LoopStart   : integer;
    LoopEnd     : integer;

    //== Sample ==
    SampleVolume  : single;  // In db. Range -96..+12
    SamplePan     : single;  // range -100..+100 (%)
    SampleTune    : integer; // Semitones. -24..+24
    SampleFine    : integer; // FineTune - Cents -100..100
    //SamplePitch   : double;  // In Semitones. -24..+24
    SampleBeats   : integer; // Length of sample in beats. Beats are used for looping.
  end;

  TKeyGroupID = cardinal;



  TSampleMarker = (smNone, smSampleStartMarker, smSampleEndMarker, smLoopStartMarker, smLoopEndMarker,
                   smSampleStartModMarker, smSampleEndModMarker, smLoopStartModMarker, smLoopEndModMarker);

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

end.
