unit ee3AddOn.ParameterManager.StaticVst;

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

}


interface

uses
  Contnrs,
  Classes,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX;

type
  TVstParSugar = class;
  TStaticVstParameterManager = class;
  TVstParSmoother = class;
  TCustomVstParProto = class;
  TVstParProtoClass = class of TCustomVstParProto;

  TStaticVstParameterChanged = procedure(Sender : TObject; const VstParIndex:integer; NewValue : single) of object;

  PVstParData = ^TVstParData;
  TVstParData = record
    VstParIndex  : integer;
    CurrentValue : single;
    TargetValue  : single;
    ParChangedCallback : TStaticVstParameterChanged;
    Prototype    : TVstParProtoClass;
    IsSmoothingActive : boolean;
  end;

  PVstParDataArray = ^TVstParDataArray;
  TVstParDataArray = array of TVstParData;

  TStaticVstParameterManager = class
  private
    fParSugar: TVstParSugar;
    function GetParProto(Index: integer): TVstParProtoClass;
  protected
    ParDataArray : TVstParDataArray;
    ParSmoother : TVstParSmoother;
    property ParProto[Index : integer]:TVstParProtoClass read GetParProto;
  public
    constructor Create;
    destructor Destroy; override;

    function GetParDataArray:PVstParDataArray; inline;

    function GetParameterName(Index: VstInt32):string;    overload;
    procedure GetParameterName(Index: VstInt32; Text: PAnsiChar);    overload;// Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to kVstMaxParamStrLen.

    function GetParameterDisplay(Index: VstInt32):string; overload;
    procedure GetParameterDisplay(Index: VstInt32; Text: PAnsiChar); overload;// Stuff text with a string representation ("0.5", "-3", "PLATE", etc...) of the value of parameter index. Limited to kVstMaxParamStrLen.

    function GetParameterLabel(Index: VstInt32):string;   overload;
    procedure GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar); overload;// Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to kVstMaxParamStrLen.

    function AddParameter(aPrototype : TVstParProtoClass; aParChangeCallBack : TStaticVstParameterChanged):integer;
    function ParameterCount : integer;

    procedure ResetAllToDefault;

    procedure FastControlProcess; inline;

    property Par : TVstParSugar read fParSugar;
  end;

  TCustomVstParProto = class
  public
    class function Name(Sender : TObject; VstParIndex : integer):string; virtual; abstract;
    class function Display(Sender : TObject; VstParIndex : integer; Value:single):string; virtual; abstract;
    class function Units(Sender : TObject; VstParIndex : integer):string; virtual; abstract;
    class function DefaultValue(Sender : TObject; VstParIndex : integer):single; virtual;
  end;

  TVstParSugar = class
  private
  protected
    ParDataArray : PVstParDataArray;
  public
    constructor Create;

    procedure UpdateParDataArrayPointer(aParData: PVstParDataArray);

    function GetTargetValue(const ParIndex:integer):single; inline;
    function GetCurrentValue(const ParIndex:integer):single; inline;

    procedure ResetToValue(const ParIndex:integer; const NewValue:single); inline;
    procedure ChangeToValue(const ParIndex:integer; const NewValue:single); inline;
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

implementation

uses
  SysUtils;

{ TStaticVstParameterManager }

constructor TStaticVstParameterManager.Create;
begin
  ParSmoother := TVstParSmoother.Create;
  fParSugar := TVstParSugar.Create;
end;

destructor TStaticVstParameterManager.Destroy;
begin

  ParSmoother.Free;
  fParSugar.Free;

  SetLength(ParDataArray, 0);
  inherited;
end;

procedure TStaticVstParameterManager.FastControlProcess;
begin
  ParSmoother.FastControlProcess;
end;

procedure TStaticVstParameterManager.GetParameterDisplay(Index: VstInt32; Text: PAnsiChar);
var
  s : string;
  Value : single;
begin
  Value := ParDataArray[Index].TargetValue;
  s := ParProto[Index].Display(self, Index, Value);
  StrPCopy(Text, AnsiString(s));
end;

function TStaticVstParameterManager.GetParameterDisplay(Index: VstInt32): string;
var
  Value : single;
begin
  Value := ParDataArray[Index].TargetValue;
  result := ParProto[Index].Display(self, Index, Value);
end;

function TStaticVstParameterManager.GetParameterLabel(Index: VstInt32): string;
begin
  result := ParProto[Index].Units(self, Index);
end;

procedure TStaticVstParameterManager.GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar);
var
  s : string;
begin
  s := ParProto[Index].Units(self, Index);
  StrPCopy(aLabel, AnsiString(s));
end;

function TStaticVstParameterManager.GetParameterName(Index: VstInt32): string;
begin
  result := ParProto[Index].Name(self, Index);
end;

procedure TStaticVstParameterManager.GetParameterName(Index: VstInt32; Text: PAnsiChar);
var
  s : string;
begin
  s := ParProto[Index].Name(self, Index);
  StrPCopy(Text, AnsiString(s));
end;

function TStaticVstParameterManager.GetParProto(Index: integer): TVstParProtoClass;
begin
  result := ParDataArray[Index].Prototype;
end;

function TStaticVstParameterManager.ParameterCount: integer;
begin
  result := Length(ParDataArray);
end;

function TStaticVstParameterManager.GetParDataArray: PVstParDataArray;
begin
  result := @ParDataArray;
end;

procedure TStaticVstParameterManager.ResetAllToDefault;
var
  c1: Integer;
  DV : single;
  proto : TVstParProtoClass;
begin
  for c1 := 0 to self.ParameterCount-1 do
  begin
    proto := self.GetParProto(c1);
    DV := proto.DefaultValue(self, c1);
    Par.ResetToValue(c1, DV);
  end;
end;

function TStaticVstParameterManager.AddParameter(aPrototype: TVstParProtoClass; aParChangeCallBack : TStaticVstParameterChanged):integer;
var
  Index : integer;
begin
  Index := Length(ParDataArray);
  SetLength(ParDataArray, Index + 1);
  ParDataArray[Index].VstParIndex := Index;
  ParDataArray[Index].Prototype := aPrototype;
  ParDataArray[Index].ParChangedCallback := aParChangeCallBack;
  ParDataArray[Index].CurrentValue := 0;
  ParDataArray[Index].TargetValue  := 0;
  result := Index;

  Par.UpdateParDataArrayPointer(@ParDataArray); //IMPORTANT: do after resizing the ParDataArray.
  ParSmoother.UpdateParDataArrayPointer(@ParDataArray); //IMPORTANT: do after resizing the ParDataArray.
end;

{ TCustomVstParProto }

class function TCustomVstParProto.DefaultValue(Sender: TObject; VstParIndex: integer): single;
begin
  result := 0;
end;

{ TVstParSugar }

constructor TVstParSugar.Create;
begin
end;

procedure TVstParSugar.UpdateParDataArrayPointer(aParData: PVstParDataArray);
begin
  ParDataArray := aParData;
end;

function TVstParSugar.GetCurrentValue(const ParIndex: integer): single;
begin
  result := ParDataArray^[ParIndex].CurrentValue;
end;

function TVstParSugar.GetTargetValue(const ParIndex: integer): single;
begin
  result := ParDataArray^[ParIndex].TargetValue;
end;

procedure TVstParSugar.ResetToValue(const ParIndex: integer; const NewValue: single);
begin
  ParDataArray^[ParIndex].TargetValue := NewValue;
  ParDataArray^[ParIndex].CurrentValue := NewValue;
  ParDataArray^[ParIndex].ParChangedCallback(self, ParDataArray^[ParIndex].VstParIndex, NewValue);
end;

procedure TVstParSugar.ChangeToValue(const ParIndex: integer; const NewValue: single);
begin
  //ParDataArray^[ParIndex].CurrentValue := NewValue;
  ParDataArray^[ParIndex].IsSmoothingActive := true;
  ParDataArray^[ParIndex].TargetValue := NewValue;
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
