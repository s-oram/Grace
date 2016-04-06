unit AudioPlugin.Globals;

interface

uses
  PlugLib.AirControl,
  AudioPlugin.PlugMain,
  Classes,
  VamVst2.DAEffect,
  VamVst2.DAEffectX;

type
  //===============================================
  //    Vst2 Plug Methods
  //===============================================
  TGetVstParameter = function (Index: integer): single of object;
  TSetVstParameter = procedure(Index: VstInt32; Value: single) of object;
  TSetVstParameterAutomated = procedure (Index: VstInt32; Value: single) of object;
  TVstBeginEdit = function(Index: VstInt32): boolean of object;
  TVstEndEdit = function(Index: VstInt32): boolean of object;
  TVstIncrementMidiEventDelta = procedure(const SampleFrames : integer) of object;
  TVstSendMidiEvent = procedure(const Status, Channel, Data1, Data2: integer) of object;
  TVstSendMidiEventWithDeltaOffset = procedure(const Status, Channel, Data1, Data2, DeltaOffset: integer) of object;
  TResizeGuiMethod = reference to procedure(const XOffset, YOffset: integer);

  TVstTimeInfoEx = record
    IsTempoValid     : boolean;
    IsPpqPosValid    : boolean;
    IsBarsValid      : boolean;
    IsMidiClockValid : boolean;
  end;

  PVst2Methods = ^TVst2Methods;
  TVst2Methods = record
    // Methods..
    GetParameter : TGetVstParameter;
    SetParameter : TSetVstParameter;
    SetParameterAutomated : TSetVstParameterAutomated;
    BeginEdit : TVstBeginEdit;
    EndEdit   : TVstEndEdit;
    ResizeGui : TResizeGuiMethod;
    // MIDI Methods..
    IncrementMidiEventDelta : TVstIncrementMidiEventDelta;
    SendMidiEvent : TVstSendMidiEvent;
    SendMidiEventWithDeltaOffset : TVstSendMidiEventWithDeltaOffset;
    // Time-Info data.
    TimeInfoIsValid : boolean;
    TimeInfo : VstTimeInfo;
    TimeInfoEx : TVstTimeInfoEx;
  end;

  //=============================================================================
  // Anything above of this should eventually be deleted. Leaving it here
  // now for reference while reducing the globals implementation down to
  // it's minimal parts.
  //=============================================================================

  TVst2Proxy = record
  private
    VstPtr : Pointer; // This pointer must be set to the TVst2Adapter instance.
    function GetIsValid: boolean;
  public
    property IsValid : boolean read GetIsValid;
    function GetParameter(Index: integer): single;
    procedure SetParameter(Index: VstInt32; Value: single);
    procedure SetParameterAutomated(Index: VstInt32; Value: single);
    function BeginEdit(Index: VstInt32): boolean;
    function EndEdit(Index: VstInt32): boolean;
  end;

  TGlobals = class;
  TGlobalsClass = class of TGlobals;

  TGlobals = class
  private
    FVst2Proxy: TVst2Proxy;
    FAirControl: TAirControl;
  protected
    PlugMain : TAudioPlug;

  public
    constructor Create;
    destructor Destroy; override;

    // Link & Unlink methods : Rather than allow different classes to call directly into other classes,
    // the globals object provides some standard channels of communication. Where possible plugins should
    // use these channels. This will hopefully help reduce coupling between the different base classes.
    procedure LinkPlugMain(const Source : TAudioPlug);
    procedure UnlinkPlugMain;

    procedure LinkVst2Adapter(const Source : Pointer);
    procedure UnlinkVst2Adapter;

    // Get/Set Private Parameter methods call into the main plugin class.
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single); overload;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); overload;

    property Vst2 : TVst2Proxy read FVst2Proxy;

    property AirControl : TAirControl read FAirControl;
  end;

implementation

uses
  SysUtils,
  AudioPlugin.Vst2Adapter;

{ TGlobals }

constructor TGlobals.Create;
begin
  PlugMain := nil;
  FVst2Proxy.VstPtr:= nil;
  FAirControl := TAirControl.Create(10,10,100);
end;

destructor TGlobals.Destroy;
begin
  FAirControl.TerminateProcessingAndPrepareForShutDown;
  FAirControl.Free;

  PlugMain := nil;
  FVst2Proxy.VstPtr:= nil;
  inherited;
end;

procedure TGlobals.LinkPlugMain(const Source: TAudioPlug);
begin
  assert(assigned(Source));
  PlugMain := Source;
end;

procedure TGlobals.UnlinkPlugMain;
begin
  PlugMain := nil;
end;

procedure TGlobals.LinkVst2Adapter(const Source: Pointer);
begin
  assert(assigned(Source));
  assert(TObject(Source) is TVst2Adapter);
  FVst2Proxy.VstPtr:= Source;
end;

procedure TGlobals.UnlinkVst2Adapter;
begin
  FVst2Proxy.VstPtr:= nil;
end;

procedure TGlobals.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single);
begin
  assert(assigned(PlugMain));
  PlugMain.SetPrivateParameter(ParIndex, ParName, ValueA, ValueB, []);
end;

procedure TGlobals.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single; const Data: array of pointer);
begin
  assert(assigned(PlugMain));
  PlugMain.SetPrivateParameter(ParIndex, ParName, ValueA, ValueB, Data);
end;

function TGlobals.GetPrivateParameter(const ParIndex: integer; const ParName: string): TPrivateParResult;
begin
  assert(assigned(PlugMain));
  result := PlugMain.GetPrivateParameter(ParIndex, ParName);
end;




{ TVst2Proxy }

function TVst2Proxy.GetIsValid: boolean;
begin
  result := Assigned(VstPtr);
end;

function TVst2Proxy.BeginEdit(Index: VstInt32): boolean;
begin
  assert(IsValid);
  result := TVst2Adapter(VstPtr).BeginEdit(Index);
end;

function TVst2Proxy.EndEdit(Index: VstInt32): boolean;
begin
  assert(IsValid);
  result := TVst2Adapter(VstPtr).EndEdit(Index);
end;

function TVst2Proxy.GetParameter(Index: integer): single;
begin
  assert(IsValid);
  result := TVst2Adapter(VstPtr).GetParameter(Index);
end;

procedure TVst2Proxy.SetParameter(Index: VstInt32; Value: single);
begin
  assert(IsValid);
  TVst2Adapter(VstPtr).SetParameter(Index, Value);
end;

procedure TVst2Proxy.SetParameterAutomated(Index: VstInt32; Value: single);
begin
  assert(IsValid);
  TVst2Adapter(VstPtr).SetParameterAutomated(Index, Value);
end;

end.
