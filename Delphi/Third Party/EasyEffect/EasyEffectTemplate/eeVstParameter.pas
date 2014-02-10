unit eeVstParameter;

interface

uses
  Math;

type
  //========== forward declarations ============================================
  TVstParameter = class;
  TVstParameterClass = class of TVstParameter;
  //============================================================================

  TStringFunction = reference to function:string;

  TGetParValueProcedure = reference to procedure(Sender : TVstParameter; out Value:single);
  TSetParValueProcedure = reference to procedure(Sender : TVstParameter; Value:single);

  TParameterChanged = procedure(const Par:TVstParameter) of object;
  TGetParameterValue = procedure(const Par:TVstParameter) of object;

  TParInputCurve = (icLinear, icSquare, icCube);


  TVstParameter = class
  private
    fName       : string;
    fMaxScaled  : single;
    fMinScaled  : single;
    fUnits      : string;
    fInputCurve : TParInputCurve;
    fOnParameterChanged : TParameterChanged;
    fOnGetParameter     : TGetParameterValue;
    fDefaultVST         : single;
    fIsPublished        : boolean;
    fGetParInfoMethod: TStringFunction;

    GetParValueProcedure : TGetParValueProcedure;
    SetParValueProcedure : TSetParValueProcedure;

    procedure SetValueVST(const Value: single);
    function GetValueScaled: single;
    procedure SetValueScaled(Value: single);
    function GetDefaultScaled: single;
    function GetValueVST: single;
    function GetParInfo:string;
  protected
    fPublishedVstParameterIndex: integer;

    function ConvertScaledToVst(const x : single):single;
    function ConvertVstToScaled(const x : single):single;
  public
    constructor Create(const aName : string); virtual;
    destructor Destroy; override;




    function SetCallback_SetParInfoMethod(aCallback:TStringFunction):TVstParameter;
    function SetCallback_SetParValue(aCallback : TSetParValueProcedure):TVstParameter;
    function SetCallback_GetParValue(aCallback : TGetParValueProcedure):TVstParameter;

    function SetName(aName:string):TVstParameter;
    function SetPublished(aIsPublished : boolean):TVstParameter;
    function SetInputCurve(const aCurve : TParInputCurve):TVstParameter;
    function SetMinMax(const aMin, aMax : single):TVstParameter; // the SetMinMax() function sets the minimum and maximum scaled value ranges.
    function SetDefault(const aDefault : single):TVstParameter; //The default value is always set using a range of 0..1.
    // NOTE: SetMinMax() and SetDefault() don't quite sit together. SetMinMax() sets the min/max *scaled* values.
    // SetDefault() sets the default value using the regular 0..1 VST range.

    procedure ResetToDefault;

    property Name          : string   read fName;
    property ShortName     : string   read fName;
    property Units         : string   read fUnits;
    property InputCurve    : TParInputCurve read fInputCurve;
    property MinScaled     : single   read fMinScaled;
    property MaxScaled     : single   read fMaxScaled;

    property DefaultVST    : single   read fDefaultVST;
    property DefaultScaled : single   read GetDefaultScaled;

    property ValueVST    : single read GetValueVST    write SetValueVST;
    property ValueScaled : single read GetValueScaled write SetValueScaled;

    function ValueAsEnum<TEnum>:TEnum;

    // ParInfo() returns a textual description of the parameter and it's value.
    property ParInfo : string read GetParInfo;

    // When IsPublished is true, the parameter will be exposed to the VST Host.
    // The host will be able to change the parameter using the
    // GetParameter and SetParameter methods as part of the VST protocole.
    property IsPublished : boolean read fIsPublished;
    property PublishedVSTParameterIndex : integer read fPublishedVstParameterIndex;


    // OnParameterChanged will be called anytime the parameter value changes.
    // This give the plugin a change to respond.
    property OnParameterChanged : TParameterChanged  read fOnParameterChanged write fOnParameterChanged;
    property OnGetParameter     : TGetParameterValue read fOnGetParameter     write fOnGetParameter;
  end;

implementation

uses
  eeEnumHelper,
  eeFunctions,
  SysUtils, eeDSP, eeHashes;

{ TVstParameter }

constructor TVstParameter.Create(const aName : string);
begin
  fName := aName;
  fUnits := '';

  fMinScaled := 0;
  fMaxScaled := 1;

  fDefaultVST := 0;
  fInputCurve := icLinear;
  fIsPublished := false;
end;

destructor TVstParameter.Destroy;
begin

  inherited;
end;

function TVstParameter.SetDefault(const aDefault: single): TVstParameter;
begin
  assert((aDefault >= 0) and (aDefault <= 1), 'Default value must be in 0..1 VST parameter range.');

  fDefaultVST := aDefault;
  result := self;
end;

function TVstParameter.SetInputCurve(const aCurve: TParInputCurve): TVstParameter;
begin
  fInputCurve := aCurve;
  result := self;
end;

function TVstParameter.SetMinMax(const aMin, aMax: single): TVstParameter;
begin
  fMinScaled := aMin;
  fMaxScaled := aMax;
  result     := self;
end;

function TVstParameter.SetName(aName: string): TVstParameter;
begin
  fName := aName;
  result := self;
end;

function TVstParameter.SetPublished(aIsPublished: boolean): TVstParameter;
begin
  fIsPublished := aIsPublished;
  result := self;
end;

function TVstParameter.ConvertScaledToVst(const x: single): single;
var
  tx : single;
begin
  case InputCurve of
    icLinear: tx := (x - MinScaled) / (MaxScaled - MinScaled);
    icSquare: tx := ParScaleSquareInv((x - MinScaled) / (MaxScaled - MinScaled));
    icCube:   tx := ParScaleCubeInv((x - MinScaled) / (MaxScaled - MinScaled));
  else
    raise Exception.Create('Unexpected InputCurve value.');
  end;

  if tx > 1 then tx := 1;
  if tx < 0 then tx := 0;

  result := tx;
end;

function TVstParameter.ConvertVstToScaled(const x: single): single;
begin
  case InputCurve of
    icLinear: result := x * (MaxScaled - MinScaled) + MinScaled;
    icSquare: result := ParScaleSquare(x) * (MaxScaled - MinScaled) + MinScaled;
    icCube:   result := ParScaleCube(x) * (MaxScaled - MinScaled) + MinScaled;
  else
    raise Exception.Create('Unexpected InputCurve value.');
  end;
end;

function TVstParameter.ValueAsEnum<TEnum>: TEnum;
begin
  result := TEnumHelper<TEnum>.ToEnum(GetValueVST);
end;

function TVstParameter.GetDefaultScaled: single;
begin
  result := ConvertVstToScaled(fDefaultVST);
end;


function TVstParameter.GetParInfo: string;
begin
  if assigned(fGetParInfoMethod) then
  begin
    result := fGetParInfoMethod;
  end else
  begin
    result := self.Name + ' ' + IntToSTr(Round(self.GetValueVST * 100));
  end;
end;

function TVstParameter.GetValueScaled: single;
var
  x : single;
begin
  if assigned(GetParValueProcedure) then
  begin
    GetParValueProcedure(self, x);
    result := ConvertVstToScaled(x);
  end else
  begin
    result := self.MinScaled;
  end;
end;

function TVstParameter.GetValueVST: single;
var
  x : single;
begin
  if assigned(GetParValueProcedure) then
  begin
    GetParValueProcedure(self, x);
    result := x;
  end else
  begin
    result := 0;
  end;
end;

function TVstParameter.SetCallback_SetParInfoMethod(aCallback: TStringFunction): TVstParameter;
begin
  fGetParInfoMethod := aCallback;
  result := self;
end;

procedure TVstParameter.ResetToDefault;
begin
  if assigned(SetParValueProcedure) then
  begin
    SetParValueProcedure(self, fDefaultVST);
  end;
end;

procedure TVstParameter.SetValueVST(const Value: single);
begin
  if assigned(SetParValueProcedure) then
  begin
    SetParValueProcedure(self, Value);
  end;
end;

procedure TVstParameter.SetValueScaled(Value: single);
var
  x : single;
begin
  if assigned(SetParValueProcedure) then
  begin
    x := ConvertScaledToVst(Value);
    SetParValueProcedure(self, x);
  end;
end;

function TVstParameter.SetCallback_SetParValue(aCallback: TSetParValueProcedure): TVstParameter;
begin
  SetParValueProcedure := aCallback;
end;

function TVstParameter.SetCallback_GetParValue(aCallback: TGetParValueProcedure): TVstParameter;
begin
  GetParValueProcedure := aCallback;
end;





end.
