unit ee3AddOn.ParameterManager.StaticVstV2;

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

}


interface

uses
  Contnrs,
  Classes,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX,
  SoundElement.Dsp;

type
  TVstParSmoother = class;
  TCustomVstParInfo = class;
  TCustomVstParInfoClass = class of TCustomVstParInfo;

  TVstParameterChanged = procedure(Sender : TObject; const VstParIndex:integer; NewValue : single) of object;

  PVstParData = ^TVstParData;
  TVstParData = record
    VstParIndex  : integer;
    CurrentValue : single;
    TargetValue  : single;
    ParChangedCallback : TVstParameterChanged;
    IsSmoothingActive : boolean;
    SmoothingCoeff : double;
  end;

  PVstParDataArray = ^TVstParDataArray;
  TVstParDataArray = array of TVstParData;

  TVstParameterManager = class
  private
    fParameterCount: integer;
    fSampleRate: single;
    procedure SetSampleRate(const Value: single);
  protected
    ParDataArray : TVstParDataArray;
    ParSmoother : TVstParSmoother;
    FParInfo : TCustomVstParInfoClass;
  public
    constructor Create(aNumberOfParameters : integer; DefaultParChangedCallBack : TVstParameterChanged; aParInfo : TCustomVstParInfoClass); virtual;
    destructor Destroy; override;

    property SampleRate : single read fSampleRate write SetSampleRate;

    function ParData(Index : integer) : PVstParData;

    procedure FastControlProcess; inline;

    property ParInfo : TCustomVstParInfoClass read FParInfo;

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

  end;


  TVstParSmoother = class
  private
  protected
    ParCount     : integer;
    ParDataArray : PVstParDataArray;

    procedure ApplySmoothing(var ParData : TVstParData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateParDataArrayPointer(aParData: PVstParDataArray);
    procedure FastControlProcess; //inline;
  end;

  TCustomVstParInfo = class
  public
    class function Name(VstParIndex : integer):string; virtual; abstract;
    class function Display(VstParIndex : integer; Value:single):string; virtual; abstract;
    class function Units(VstParIndex : integer; Value:single):string; virtual; abstract;
    class function DefaultValue(VstParIndex : integer):single; virtual; abstract;
    // SmoothingTime result is in milliseconds.
    class function SmoothingTime(VstParIndex : integer):single; virtual; abstract;
  end;


implementation






{ TStaticVstParameterManager }

constructor TVstParameterManager.Create(aNumberOfParameters: integer; DefaultParChangedCallBack: TVstParameterChanged; aParInfo : TCustomVstParInfoClass);
var
  c1: Integer;
begin
  fParInfo := aParInfo;

  fParameterCount := aNumberOfParameters;


  SetLength(ParDataArray, ParameterCount);

  for c1 := 0 to ParameterCount-1 do
  begin
    ParDataArray[c1].VstParIndex := c1;
    ParDataArray[c1].ParChangedCallback := DefaultParChangedCallback;
  end;

  ParSmoother := TVstParSmoother.Create;
  ParSmoother.UpdateParDataArrayPointer(@ParDataArray);
end;

destructor TVstParameterManager.Destroy;
begin
  ParSmoother.Free;
  SetLength(ParDataArray, 0);
  inherited;
end;

procedure TVstParameterManager.SetSampleRate(const Value: single);
var
  c1: Integer;
  st : single;
begin
  fSampleRate := Value;

  for c1 := 0 to ParameterCount-1 do
  begin
    st := ParInfo.SmoothingTime(c1);
    if st = 0
      then ParDataArray[c1].SmoothingCoeff := 0
      else ParDataArray[c1].SmoothingCoeff := CalcRcEnvelopeCoefficient(st, SampleRate);
  end;
end;

procedure TVstParameterManager.FastControlProcess;
begin
  ParSmoother.FastControlProcess;
end;

function TVstParameterManager.ParData(Index: integer): PVstParData;
begin
  result := @ParDataArray[Index];
end;

function TVstParameterManager.GetCurrentParValue(const ParIndex: integer): single;
begin
  result := ParDataArray[ParIndex].CurrentValue;
end;

function TVstParameterManager.GetTargetParValue(const ParIndex: integer): single;
begin
  result := ParDataArray[ParIndex].TargetValue;
end;

procedure TVstParameterManager.ResetAllParameters;
var
  c1: Integer;
  dv : single;
begin
  for c1 := 0 to ParameterCount-1 do
  begin
    dv := ParInfo.DefaultValue(c1);
    ResetParValue(c1, dv);
  end;
end;

procedure TVstParameterManager.ResetParValue(const ParIndex: integer; const NewValue: single);
begin
  ParDataArray[ParIndex].TargetValue := NewValue;
  ParDataArray[ParIndex].CurrentValue := NewValue;
  ParDataArray[ParIndex].ParChangedCallback(self, ParIndex, NewValue);
end;


procedure TVstParameterManager.ChangeParValue(const ParIndex: integer; const NewValue: single);
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

procedure TVstParameterManager.BumpAllParameters;
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





{ TVstParSmoother }

constructor TVstParSmoother.Create;
begin

end;

destructor TVstParSmoother.Destroy;
begin

  inherited;
end;

procedure TVstParSmoother.UpdateParDataArrayPointer(aParData: PVstParDataArray);
begin
  ParDataArray := aParData;
  ParCount := Length(ParDataArray^);

end;

procedure TVstParSmoother.FastControlProcess;
var
  c1: Integer;
begin
  for c1 := 0 to ParCount-1 do
  begin
    if ParDataArray^[c1].IsSmoothingActive then ApplySmoothing(ParDataArray^[c1]);
  end;
end;

procedure TVstParSmoother.ApplySmoothing(var ParData: TVstParData);
//const
  //StepSize : single = 0.00000000001; //this needs to be scaled to samplerate and made optional.
var
  StepSize : single;
begin
  ///  TODO:HIGH
  ///  Must make parameter smoothing configurable.
  ///  Can either use one pole filter or a linear slope with
  ///  a variable step size. Parameter smoothing will be constant-time with both methods.
  ///


  // TODO:HIGH par smoothing needs to be configurable. Maybe it also needs to be a one pole filter instead
  // of a linear change.
  StepSize := 1 / (44100 * 0.05);

  if ParData.CurrentValue < ParData.TargetValue - StepSize then
  begin
    ParData.CurrentValue := ParData.CurrentValue + StepSize;
    ParData.ParChangedCallback(self, ParData.VstParIndex, ParData.CurrentValue);
  end else
  if ParData.CurrentValue > ParData.TargetValue + StepSize then
  begin
    ParData.CurrentValue := ParData.CurrentValue - StepSize;
    ParData.ParChangedCallback(self, ParData.VstParIndex, ParData.CurrentValue);
  end else
  begin
    ParData.IsSmoothingActive := false;
    ParData.CurrentValue := ParData.TargetValue;
    ParData.ParChangedCallback(self, ParData.VstParIndex, ParData.TargetValue);
  end;


end;


end.
