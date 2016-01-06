unit AudioPlugin.Globals;

interface

uses
  Classes,
  Helm.Dispatcher,
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

  //===============================================
  //    Audio Plug Methods
  //===============================================
  TGetPrivateParameter = procedure(const ParIndex : integer; const ParValue : Pointer; const DataSize : integer) of object;
  TSetPrivateParameter = procedure(const ParIndex : integer; const ParValue : Pointer; const DataSize : integer) of object;

  PAudioPlugMethods = ^TAudioPlugMethods;
  TAudioPlugMethods = record
    GetPrivateParameter : TGetPrivateParameter;
    SetPrivateParameter : TSetPrivateParameter;
  end;



  TObjectStore = class
  private
    FObjectList: TStringList;
    function GetObj(Key: string): TObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Exists(const Key : string):boolean;
    procedure Add(const Key : string; const Obj : TObject);
    procedure Delete(const Key : string);
    property Obj[Key : string]:TObject read GetObj; default;
  end;

  TGlobals = class
  private
    FObjectStore: TObjectStore;
    FEventDispatcher: THelmDispatcher;
    FAudioPlugMethods: PAudioPlugMethods;
    FVst2: PVst2Methods;
  public
    constructor Create;
    destructor Destroy; override;

    property EventDispatcher : THelmDispatcher read FEventDispatcher;

    property ObjectStore : TObjectStore read FObjectStore;

    property AudioPlugMethods : PAudioPlugMethods  read FAudioPlugMethods;
    property Vst2 : PVst2Methods read FVst2;
  end;

implementation

uses
  SysUtils;

{ TGlobals }

constructor TGlobals.Create;
begin
  New(FAudioPlugMethods);
  New(FVst2);

  FObjectStore := TObjectStore.Create;
  FEventDispatcher := THelmDispatcher.Create;
end;

destructor TGlobals.Destroy;
begin
  FObjectStore.Free;
  FEventDispatcher.Free;

  Dispose(FAudioPlugMethods);
  Dispose(FVst2);
  inherited;
end;

{ TGlobalObjectStore }

constructor TObjectStore.Create;
begin
  FObjectList := TStringList.Create;
  FObjectList.OwnsObjects := true;
  FObjectList.Duplicates := dupError;
end;

destructor TObjectStore.Destroy;
begin
  FObjectList.Free;
  inherited;
end;

function TObjectStore.Exists(const Key: string): boolean;
begin
  if FObjectList.IndexOf(Key) <> -1
    then result := true
    else result := false;
end;

procedure TObjectStore.Add(const Key: string; const Obj: TObject);
begin
  FObjectList.AddObject(Key, Obj);
end;

function TObjectStore.GetObj(Key: string): TObject;
var
  Index : integer;
begin
  Index := FObjectList.IndexOf(Key);
  if Index <> -1
    then result := FObjectList.Objects[Index]
    else result := nil;
end;

procedure TObjectStore.Delete(const Key: string);
var
  Index : integer;
begin
  Index := FObjectList.IndexOf(Key);
  if Index <> -1
    then FObjectList.Delete(Index)
    else raise Exception.Create('Object could not be found.');
end;


end.
