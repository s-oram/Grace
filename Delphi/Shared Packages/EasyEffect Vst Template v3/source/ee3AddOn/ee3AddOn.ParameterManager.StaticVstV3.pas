unit ee3AddOn.ParameterManager.StaticVstV3;

{
  Static VST Parameter Manager is designed for VST plugins
  with relatively simple VST parameter setups. Typically this
  will be suitable for plugins with a limited number of parameters
  that perform one function. It's a good match for plugins
  that mimic hardware where each control is linked to one parameter.
  Compressors, EQs etc.

  It's probably not suitable for VST plugins that dynamically map
  a limited number of VST parameters to multiple internal parameters.
  Drum machines and complex poly-timbral synths.


  == Usage Requirements and Assumptions ==
  - This parameter manager expects to be the only VST parameter manager used in
    the project. It might not work well when combined with other parameter
    managers.

  == Version 2 Changes ==
  - remove dependence on the parameter prototype class.

  == Version 3 Changes ==
  - remove the TCustomVstParInfoClass dependency injection class. It's too
    complicated. Instead. The parameter manager should be a 'custom' class
    the projects will inherit from.

}


interface

uses
  Contnrs,
  Classes,
  VamLib.Types,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX,
  ee3.CustomPlugin.PublishedVstParInfo;


type
  TVstParameterChanged = procedure(Sender : TObject; const VstParIndex:integer; NewValue : single) of object;

  PVstParData = ^TVstParData;
  TVstParData = record
    VstParIndex        : integer;
    CurrentValue       : single;
    TargetValue        : single;
    ParChangedCallback : TVstParameterChanged;
    IsSmoothingActive  : boolean;
    SmoothingCoeff     : double;
  end;

  PVstParDataArray = ^TVstParDataArray;
  TVstParDataArray = array of TVstParData;

  TCustomVstParameterManager = class(TPureInterfacedObject, IPublishedVstParInfo)
  private
    fParameterCount: integer;
    fFastControlRate: single;
  protected
    ParDataArray : TVstParDataArray;

    procedure SetFastControlRate(const Value: single); virtual;
    procedure ApplyParameterSmoothing(var ParData: TVstParData);

    procedure VstPar_SetParameter(const Index: Integer; Value: single);  // Called when a parameter changed
    function VstPar_GetParameter(const Index: Integer): single;          // Return the value of the parameter with index
    function VstPar_GetParameterName(const Index: Integer):string;    // Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to kVstMaxParamStrLen.
    function VstPar_GetParameterDisplay(const Index: Integer):string; // Stuff text with a string representation ("0.5", "-3", "PLATE", etc...) of the value of parameter index. Limited to kVstMaxParamStrLen.
    function VstPar_GetParameterLabel(const Index: Integer):string;   // Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to kVstMaxParamStrLen.
  public
    constructor Create(DefaultParChangedCallBack : TVstParameterChanged); virtual;
    destructor Destroy; override;

    // IMPORTANT: FastControlRate is the (plugin samplerate / fast control rate block size). It's important it's set.
    property FastControlRate : single read fFastControlRate write SetFastControlRate;

    function ParData(Index : integer) : PVstParData;

    procedure FastControlProcess; inline;

    function GetTargetParValue(const ParIndex:integer):single; inline;
    function GetCurrentParValue(const ParIndex:integer):single; inline;
    procedure ResetParValue(const ParIndex:integer; const NewValue:single); inline;
    procedure ChangeParValue(const ParIndex:integer; const NewValue:single); inline;


    // reset all parameters to default values.
    procedure ResetAllParameters;

    // call the parameter changed handler for all parameters with the current parameter value.
    // This is useful to do in the Resume() method after a plugin samplerate has been changed.
    procedure BumpAllParameters;

    property ParameterCount : integer read fParameterCount;

    //== Override these functions in descendent to provide info about the plugin parameter properties ==
    class function Par_Count:integer; virtual; abstract;
    class function Par_Name(VstParIndex : integer):string; virtual; abstract;
    class function Par_Display(VstParIndex : integer; Value:single):string; virtual; abstract;
    class function Par_Units(VstParIndex : integer; Value:single):string; virtual; abstract;
    class function Par_DefaultValue(VstParIndex : integer):single; virtual; abstract;
    // SmoothingTime result is in milliseconds.
    class function Par_SmoothingTime(VstParIndex : integer):single; virtual; abstract;

    // use Par_(Get|Set)Value() when reading/writing the plugin state.
    function Par_GetValue(const VstParIndex : integer):single;
    procedure Par_SetValue(const VstParIndex : integer; const Value : single);

  end;

implementation

uses
  SoundElement.Dsp,
  VamLib.Utils;

{ TStaticVstParameterManager }

constructor TCustomVstParameterManager.Create(DefaultParChangedCallBack: TVstParameterChanged);
var
  c1: Integer;
begin
  fFastControlRate := 0;

  fParameterCount := Par_Count;

  SetLength(ParDataArray, ParameterCount);

  for c1 := 0 to ParameterCount-1 do
  begin
    ParDataArray[c1].VstParIndex := c1;
    ParDataArray[c1].ParChangedCallback := DefaultParChangedCallback;
  end;
end;

destructor TCustomVstParameterManager.Destroy;
begin
  SetLength(ParDataArray, 0);
  inherited;
end;

function TCustomVstParameterManager.VstPar_GetParameter(const Index: Integer): single;
begin
  result := GetTargetParValue(Index);
end;

function  TCustomVstParameterManager.VstPar_GetParameterDisplay(const Index: Integer):string;
var
  vx : single;
begin
  vx   := GetTargetParValue(Index);
  result := Par_Display(Index, vx);
end;

function  TCustomVstParameterManager.VstPar_GetParameterLabel(const Index: Integer):string;
var
  vx : single;
begin
  vx := GetTargetParValue(Index);
  result := Par_Units(Index, vx)
end;

function TCustomVstParameterManager.VstPar_GetParameterName(const Index: Integer):string;
begin
  result := Par_Name(Index);
end;

procedure TCustomVstParameterManager.VstPar_SetParameter(const Index: Integer; Value: single);
begin
  ChangeParValue(Index, Value);
end;

function TCustomVstParameterManager.ParData(Index: integer): PVstParData;
begin
  result := @ParDataArray[Index];
end;

function TCustomVstParameterManager.Par_GetValue(const VstParIndex: integer): single;
begin
  assert(VstParIndex >= 0);
  assert(VstParIndex < ParameterCount);
  result := self.GetTargetParValue(VstParIndex);
end;

procedure TCustomVstParameterManager.Par_SetValue(const VstParIndex: integer; const Value: single);
var
  VX : single;
begin
  assert(VstParIndex >= 0);
  assert(VstParIndex < ParameterCount);
  VX := Clamp(Value, 0, 1);
  ChangeParValue(VstParIndex, VX);
end;

function TCustomVstParameterManager.GetCurrentParValue(const ParIndex: integer): single;
begin
  result := ParDataArray[ParIndex].CurrentValue;
end;

function TCustomVstParameterManager.GetTargetParValue(const ParIndex: integer): single;
begin
  result := ParDataArray[ParIndex].TargetValue;
end;

procedure TCustomVstParameterManager.ResetAllParameters;
var
  c1: Integer;
  dv : single;
begin
  for c1 := 0 to ParameterCount-1 do
  begin
    dv := Par_DefaultValue(c1);
    ResetParValue(c1, dv);
  end;
end;

procedure TCustomVstParameterManager.ResetParValue(const ParIndex: integer; const NewValue: single);
begin
  ParDataArray[ParIndex].TargetValue := NewValue;
  ParDataArray[ParIndex].CurrentValue := NewValue;
  ParDataArray[ParIndex].ParChangedCallback(self, ParIndex, NewValue);
end;


procedure TCustomVstParameterManager.ChangeParValue(const ParIndex: integer; const NewValue: single);
begin
  assert(ParIndex = ParDataArray[ParIndex].VstParIndex);

  if ParDataArray[ParIndex].SmoothingCoeff = 0 then
  begin
    ParDataArray[ParIndex].TargetValue := NewValue;
    ParDataArray[ParIndex].CurrentValue := NewValue;
    ParDataArray[ParIndex].ParChangedCallback(self, ParIndex, NewValue);
  end else
  begin
    ParDataArray[ParIndex].IsSmoothingActive := true;
    ParDataArray[ParIndex].TargetValue := NewValue;
  end;
end;

procedure TCustomVstParameterManager.BumpAllParameters;
var
  CurVal : single;
  c1 : integer;
begin
  for c1 := 0 to ParameterCount-1 do
  begin
    CurVal := ParDataArray[c1].CurrentValue;
    ParDataArray[c1].ParChangedCallback(self, c1, CurVal);
  end;
end;

procedure TCustomVstParameterManager.FastControlProcess;
var
  c1: Integer;
begin
  assert(fFastControlRate <> 0);

  for c1 := 0 to self.ParameterCount-1 do
  begin
    if ParDataArray[c1].IsSmoothingActive then ApplyParameterSmoothing(ParDataArray[c1]);
  end;
end;

procedure TCustomVstParameterManager.SetFastControlRate(const Value: single);
var
  c1: Integer;
  st : single;
begin
  fFastControlRate := Value;

  for c1 := 0 to ParameterCount-1 do
  begin
    st := Par_SmoothingTime(c1);
    if st = 0
      then ParDataArray[c1].SmoothingCoeff := 0
      else ParDataArray[c1].SmoothingCoeff := CalcRcEnvelopeCoefficient(st, FastControlRate);
  end;
end;

procedure TCustomVstParameterManager.ApplyParameterSmoothing(var ParData: TVstParData);
const
  MinParDelta = 0.001;
begin
  ParData.CurrentValue := RcEnvFilter(ParData.CurrentValue, ParData.TargetValue, ParData.SmoothingCoeff);
  ParData.ParChangedCallback(self, ParData.VstParIndex, ParData.CurrentValue);

  // TODO:HIGH this smoothing finished check should be moved to the slow control rate process.
  if (abs(ParData.CurrentValue - ParData.TargetValue)) <= MinParDelta then
  begin
    ParData.CurrentValue := ParData.TargetValue;
    ParData.ParChangedCallback(self, ParData.VstParIndex, ParData.TargetValue);
    ParData.IsSmoothingActive := false;
  end;
end;



end.
