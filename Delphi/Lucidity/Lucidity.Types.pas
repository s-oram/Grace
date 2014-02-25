unit Lucidity.Types;

interface

uses
  uConstants,
  VamLib.Collections.Lists,
  Windows;

type
  TGuidList = TSimpleList<TGUID>;


  // These structures are intended for storing the modulated parameter values.
  // I'm not entirely sure of the variable naming yet. It doesn't seem quite right.
  TModulatedPar = record
    //Holds the actual parameter value. Range should be 0..1
    ParValue  : single;

    // The above Parameter value with the modulation applied.
    // range should be 0..1
    ModulatedParValue : single;

    //Stores the modulation amount for each mod slot.
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
  end;

  PModulatedPars = ^TModulatedPars;
  TModulatedPars = array[0..kModulatedParameterCount-1] of TModulatedPar;

  // TParModulationData is a structure intended to hold summed modulation amounts
  // for all parameters.
  PParModulationData = ^TParModulationData;
  TParModulationData = array[0..kModulatedParameterCount-1] of single;

implementation

end.
