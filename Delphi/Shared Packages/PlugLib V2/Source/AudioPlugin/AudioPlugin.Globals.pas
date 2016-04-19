unit AudioPlugin.Globals;

interface

uses
  Classes,
  PlugLib.AirControl,
  AudioPlugin.Types,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  AudioPlugin.Interfaces;

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
    AFX : IVst2AudioEffectX;
  public
    function GetParameter(Index: integer): single;
    procedure SetParameter(Index: VstInt32; Value: single);
    procedure SetParameterAutomated(Index: VstInt32; Value: single);
    function BeginEdit(Index: VstInt32): boolean;
    function EndEdit(Index: VstInt32): boolean;
  end;

  TPlugMainProxy = record
  private
    FPlugMain : IPlugMain;
  public
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single); overload;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); overload;
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult;
  end;

  TGlobals = class
  private
    FVst2Proxy: TVst2Proxy;
    FAirControl: TAirControl;
    FPlug: TPlugMainProxy;
    FMidiOut: IMidiOut;
  protected
  public
    constructor Create(const GlobalExceptionHander : TExceptionHandlerFunc); virtual;
    destructor Destroy; override;

    // Link & Unlink methods : Inject the dependencies here!
    procedure LinkPlugMain(const Source : IPlugMain);
    procedure UnlinkPlugMain;

    procedure LinkVst2Adapter(const Source : IVst2AudioEffectX);
    procedure UnlinkVst2Adapter;

    procedure LinkMidiOut(const Source : IMidiOut);
    procedure UnlinkMidiOut;


    // NOTE TO SELF: These proxy classes provide a means for classes to call
    // methods in other classes without hardcoding any dependencies. However there
    // are a few steps of indirection here. It would probably be better to avoid
    // calling any of these proxy methods in time critical code.
    property Vst2 : TVst2Proxy read FVst2Proxy;
    property Plug : TPlugMainProxy read FPlug;

    property MidiOut : IMidiOut read FMidiOut; // Audio thread usage only. There is no guarantee the implementation is thread safe.

    // AirControl is a thread synchronisation utility. It lets the audio thread run code in the GUI context
    // and vice-versa. It also provides a background thread for slow tasks that can't block the audio or GUI thread.
    property AirControl : TAirControl read FAirControl;
  end;

  TGlobalsClass = class of TGlobals;

implementation

uses
  SysUtils;

{ TGlobals }

constructor TGlobals.Create(const GlobalExceptionHander : TExceptionHandlerFunc);
begin
  FPlug.FPlugMain := nil;
  FVst2Proxy.AFX:= nil;

  FAirControl := TAirControl.Create(10,10,100, GlobalExceptionHander);

  FMidiOut := nil;
end;

destructor TGlobals.Destroy;
begin
  FAirControl.TerminateProcessingAndPrepareForShutDown;
  FAirControl.Free;

  FPlug.FPlugMain := nil;
  FVst2Proxy.AFX:= nil;
  FMidiOut := nil;

  inherited;
end;

procedure TGlobals.LinkMidiOut(const Source: IMidiOut);
begin
  assert(assigned(Source));
  FMidiOut := Source;
end;

procedure TGlobals.LinkPlugMain(const Source: IPlugMain);
begin
  assert(assigned(Source));
  FPlug.FPlugMain := Source;
end;

procedure TGlobals.UnlinkMidiOut;
begin
  FMidiOut := TDoNothingMidiOut.Create;
end;

procedure TGlobals.UnlinkPlugMain;
begin
  FPlug.FPlugMain := nil;
end;

procedure TGlobals.LinkVst2Adapter(const Source: IVst2AudioEffectX);
begin
  assert(assigned(Source));
  FVst2Proxy.AFX := Source;
end;

procedure TGlobals.UnlinkVst2Adapter;
begin
  FVst2Proxy.AFX := nil;
end;

{ TVst2Proxy }

function TVst2Proxy.BeginEdit(Index: VstInt32): boolean;
begin
  if assigned(AFX)
    then result := AFX.BeginEdit(Index)
    else result := false;
end;

function TVst2Proxy.EndEdit(Index: VstInt32): boolean;
begin
  if assigned(AFX)
    then result := AFX.EndEdit(Index)
    else result := false;
end;

function TVst2Proxy.GetParameter(Index: integer): single;
begin
  if assigned(AFX)
    then result := AFX.GetParameter(Index)
    else result := 0;
end;

procedure TVst2Proxy.SetParameter(Index: VstInt32; Value: single);
begin
  if assigned(AFX)
    then AFX.SetParameter(Index, Value);
end;

procedure TVst2Proxy.SetParameterAutomated(Index: VstInt32; Value: single);
begin
  if assigned(AFX)
    then AFX.SetParameterAutomated(Index, Value);
end;

{ TPlugMainProxy }

function TPlugMainProxy.GetPrivateParameter(const ParIndex: integer; const ParName: string): TPrivateParResult;
begin
  if assigned(FPlugMain) then
  begin
    result := FPlugMain.GetPrivateParameter(ParIndex, ParName);
  end else
  begin
    result.ValueA := 0;
    result.ValueB := 0;
  end;
end;

procedure TPlugMainProxy.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single);
begin
  if assigned(FPlugMain)
    then FPlugMain.SetPrivateParameter(ParIndex, ParName, ValueA, ValueB);
end;

procedure TPlugMainProxy.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single; const Data: array of pointer);
begin
  if assigned(FPlugMain)
    then FPlugMain.SetPrivateParameter(ParIndex, ParName, ValueA, ValueB, Data);
end;

end.
