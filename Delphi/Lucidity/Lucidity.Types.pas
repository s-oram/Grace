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
    ParValue  : single;
    ModAmount : array[0..kModSlotCount-1] of single;
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
