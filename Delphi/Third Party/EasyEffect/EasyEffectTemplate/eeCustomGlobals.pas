{
  The globals class is designed to be passed to effect processing objects
  (filter, envelope, fx classes etc). Instead of individually updating
  properties like samplerate/blocksize/tempo for all objects, objects
  can register with the TGlobals class and automatically be notified
  of changes.

  NOTE: It would probably be a good idea to ensure the usage of the global class
  is entirely optional.
}

unit eeCustomGlobals;

interface

uses
  Vcl.Forms,
  VamLib.ZeroObject,
  eeTypes,
  SysUtils,
  eeGuiStandard_Types,
  Windows, Classes, uEventList, eeMidiEvents, DAEffectX;

type
  //=== Forward Declarations ============
  TCustomGlobals = class;
  //=====================================

  PHostProperties = ^THostProperties;
  THostProperties = record
    HostName : string;
    HostVersion : integer;
  end;

  PCpuUsageInfo = ^TCpuUsageInfo;
  TCpuUsageInfo = record
    ProcessReplacingTime : double;
    ProcessReplacingLoad : double;
  end;

  TPluginEvent = (SampleRateChanged, BlockSizeChanged, TempoChanged, TransportChanged, PlayStateChanged, VstSuspendEvent, VstResumeEvent);

  TCustomGlobals = class
  private
    fSampleRate        : integer;
    fBlockSize         : integer;
    fTempo             : single;
    fSampleRateList    : TEventList;
    fBlockSizeList     : TEventList;
    fTempoList         : TEventList;
    fOneOverSampleRate : double;
    fDeltaOffset       : integer;
    fMidiOutput        : TeeMidiEventList;
    fTimeInfo          : PVstTimeInfo;
    fMidiInput         : TeeMidiEventList;
    fTransportChangedList : TEventList;
    fTransportPlaying     : boolean;
    fTransportCycleActive : boolean;
    fTransportRecording   : boolean;
    fppqPos               : double;
    fBarStartPos          : double;
    fSamplesToNextClock   : integer;
    fTimeSigNumerator     : integer;
    fTimeSigDenominator   : integer;
    fBarTime              : double;
    fBarPos               : double;
    fBarPosNormalised     : double;
    fSamplesToBarEnd      : double;
    fPlayStateChangedList : TEventList;
    fVstSuspendList: TEventList;
    fVstResumeList: TEventList;
    fSyncKeeper: Pointer;
    fOverSampleFactor: integer;
    fFastControlRate: integer;
    fSlowControlRate: integer;
    FMotherShip: TMotherShip;
    fVstSystemWindow: Hwnd;
    fTopGuiWindow: TForm;
    procedure SetBlockSize(const Value: integer);
    procedure SetTempo(const Value: single);
    procedure SetTransportPlaying(const Value: boolean);
    procedure SetTransportCycleActive(const Value: boolean);
    procedure SetTransportRecording(const Value: boolean);
    procedure SetOverSampleFactor(const Value: integer);
    function GetHostProperties: PHostProperties;
    function GetMotherShip: IMotherShip;
  protected
    fHostProperties : THostProperties;
    fCpuUsage      : PCpuUsageInfo;
    fVstMethods    : PVstMethodReferences;

    //====================================================================================================
    // Event notification lists. Register an event handler to be notified when something changes.
    property SampleRateList       : TEventList read fSampleRateList       write fSampleRateList;
    property BlockSizeList        : TEventList read fBlockSizeList        write fBlockSizeList;
    property TempoList            : TEventList read fTempoList            write fTempoList;
      //Event fired whenever TransportPlaying, TransportCycleActive or TransportRecording changes.
    property TransportChangedList : TEventList read fTransportChangedList write fTransportChangedList;
    property PlayStateChangedList : TEventList read fPlayStateChangedList write fPlayStateChangedList;
    property VstSuspendList       : TEventList read fVstSuspendList       write fVstSuspendList;
    property VstResumeList        : TEventList read fVstResumeList        write fVstResumeList;
    //======================================================================================================
  protected
  public
    constructor Create; virtual;
	  destructor Destroy; override;

    procedure UpdateSampleRates(const aSampleRate, aFastControlRate, aSlowControlRate : integer);




    procedure AddEventListener(EventType : TPluginEvent; EventHandler : TNotifyEvent);
    procedure RemoveEventListener(EventType : TPluginEvent; EventHandler : TNotifyEvent);

    procedure TriggerEvent(EventType : TPluginEvent);

    procedure TriggerVstSuspendEvent;
    procedure TriggerVstResumeEvent;

    function NextQuarterNote(aDeltaOffset:integer):integer;      //result in samples.
    function NextEighthNote(aDeltaOffset:integer):integer;       //result in samples.
    function NextSixteenthNote(aDeltaOffset:integer):integer;    //result in samples.
    function NextThirtySecondNote(aDeltaOffset:integer):integer; //result in samples.

    // UpdateSyncInfo should be called once ppqPos, BarStartPos & TimeSig info has been updated.
    procedure UpdateSyncInfo;
    procedure UpdateTransportState(Playing, CycleActive, Recording:boolean);

    //=============================================================================
    // NOTE: These values are updated by calling UpdateSyncInfo.
    property BarPos             : double  read fBarPos;  //in quarter beats.
    property BarPosNormalised   : double  read fBarPosNormalised;
    property BarTime            : double  read fBarTime; //in quarter beats.
    property SamplesToBarEnd    : double  read fSamplesToBarEnd;
    //=============================================================================


    property OneOverSampleRate    : double  read fOneOverSampleRate;
    property SampleRate           : integer read fSampleRate;
    property ControlRate          : integer read fFastControlRate;
    property FastControlRate      : integer read fFastControlRate;
    property SlowControlRate      : integer read fSlowControlRate;

    property BlockSize            : integer read fBlockSize          write SetBlockSize;
    property Tempo                : single  read fTempo              write SetTempo;
    property OverSampleFactor     : integer read fOverSampleFactor   write SetOverSampleFactor;

    property ppqPos               : double  read fppqPos             write fppqPos;
    property BarStartPos          : double  read fBarStartPos        write fBarStartPos;
    property SamplesToNextClock   : integer read fSamplesToNextClock write fSamplesToNextClock;

    property TimeSigNumerator     : integer read fTimeSigNumerator   write fTimeSigNumerator;
    property TimeSigDenominator   : integer read fTimeSigDenominator write fTimeSigDenominator;

    property TransportPlaying     : boolean read fTransportPlaying     write SetTransportPlaying;
    property TransportCycleActive : boolean read fTransportCycleActive write SetTransportCycleActive;
    property TransportRecording   : boolean read fTransportRecording   write SetTransportRecording;

    //=============================================================================

    property DeltaOffset : integer read fDeltaOffset write fDeltaOffset;

    //=============================================================================

    property MidiInput  : TeeMidiEventList read fMidiInput  write fMidiInput;
    property MidiOutput : TeeMidiEventList read fMidiOutput write fMidiOutput;

    property TimeInfo : PVstTimeInfo read fTimeInfo write fTimeInfo;

    property CpuUsage : PCpuUsageInfo read fCpuUsage;

    property SyncKeeper : Pointer read fSyncKeeper write fSyncKeeper;

    // VstMethods holds pointers to some standard VST methods that are used by the GUI.
    property VstMethods : PVstMethodReferences read fVstMethods;

    property TopLevelForm   : TForm read fTopGuiWindow write fTopGuiWindow;      // The top level form in the GUI.
    property TopLevelWindow : Hwnd read fVstSystemWindow write fVstSystemWindow; // The VST window handle. Provided by host application.

    property HostProperties : PHostProperties read GetHostProperties;

    property MotherShip : IMotherShip read GetMotherShip;
  end;

implementation

uses
  Dialogs,
  eeDSP, Math;

{ TCustomGlobals }

constructor TCustomGlobals.Create;
begin
  FMotherShip := TMotherShip.Create;

  TopLevelWindow := 0;

  SampleRateList       := TEventList.Create;
  BlockSizeList        := TEventList.Create;
  TempoList            := TEventList.Create;
  TransportChangedList := TEventList.Create;
  PlayStateChangedList := TEventList.Create;
  VstSuspendList       := TEventList.Create;
  VstResumeList        := TEventList.Create;

  fOverSampleFactor  := 1;
  fSampleRate        := 44100;
  fFastControlRate   := 44100;
  fSlowControlRate   := 44100;

  fOneOverSampleRate := 1 / 44100;
  fBlockSize         := 512;
  fTempo             := 120;
  fppqPos            := 0;
  fBarStartPos       := 0;

  TimeSigNumerator   := 4;
  TimeSigDenominator := 4;


  DeltaOffset := 0;

  new(fCpuUsage);
  new(fVstMethods);
end;

destructor TCustomGlobals.Destroy;
begin
  dispose(fCpuUsage);
  dispose(fVstMethods);

  SampleRateList.Free;
  BlockSizeList.Free;
  TempoList.Free;
  TransportChangedList.Free;
  PlayStateChangedList.Free;
  VstSuspendList.Free;
  VstResumeList.Free;
  FMotherShip.Free;
  inherited;
end;

function TCustomGlobals.GetHostProperties: PHostProperties;
begin
  result := @fHostProperties;
end;

function TCustomGlobals.GetMotherShip: IMotherShip;
begin
  result := FMotherShip;
end;

procedure TCustomGlobals.UpdateSampleRates(const aSampleRate, aFastControlRate, aSlowControlRate: integer);
begin
  fSampleRate := aSampleRate;
  fFastControlRate := aFastControlRate;
  fSlowControlRate := aSlowControlRate;
  SampleRateList.TriggerAll(self);
end;


procedure TCustomGlobals.SetBlockSize(const Value: integer);
begin
  if Value > 0 then
  begin
    fBlockSize := Value;
    BlockSizeList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.SetOverSampleFactor(const Value: integer);
begin
  if Value <> fOversampleFactor then
  begin
    fOverSampleFactor := Value;
    SampleRateList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.SetTempo(const Value: single);
begin
  if Value > 0  then
  begin
    fTempo := Value;
    TempoList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.SetTransportCycleActive(const Value: boolean);
begin
  if Value <> fTransportCycleActive then
  begin
    fTransportCycleActive := Value;
    TransportChangedList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.SetTransportPlaying(const Value: boolean);
begin
  if Value <> fTransportPlaying then
  begin
    fTransportPlaying := Value;
    TransportChangedList.TriggerAll(self);
    PlayStateChangedList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.SetTransportRecording(const Value: boolean);
begin
  if Value <> fTransportRecording then
  begin
    fTransportRecording := Value;
    TransportChangedList.TriggerAll(self);
  end;
end;

procedure TCustomGlobals.UpdateTransportState(Playing, CycleActive, Recording: boolean);
begin
  // NOTE: The control logic for this method doesn't look that clear. Two different
  // event handlers may need to be called depending on the calling parameters.


  if (Playing <> fTransportPlaying) then
  begin
    //Play state has changed, so update all variables and call both event handlers.
    fTransportPlaying     := Playing;
    fTransportCycleActive := CycleActive;
    fTransportRecording   := Recording;

    PlayStateChangedList.TriggerAll(self);
    TransportChangedList.TriggerAll(self);
  end
    else
  if (CycleActive <> fTransportCycleActive) or (Recording <> fTransportRecording) then
  begin
    // Play state hasn't changed but something else has, update other variables and
    // call the TransportChanged event handlers.
    fTransportCycleActive := CycleActive;
    fTransportRecording   := Recording;

    TransportChangedList.TriggerAll(self);
  end;

end;

procedure TCustomGlobals.UpdateSyncInfo;
var
  SamplesPerBar:single;
begin
  fBarPos  := ppqPos - BarStartPos;
  fBarTime := TimeSigNumerator / TimeSigDenominator * 4;

  fBarPosNormalised := fBarPos / fBarTime;

  SamplesPerBar := SyncToSamples(TimeSigNumerator / TimeSigDenominator, Tempo, SampleRate);

  fSamplesToBarEnd := (1 - fBarPosNormalised) * SamplesPerBar;
end;

// function SamplesToNextQuarterNote(DeltaOffset: integer): integer;
//
// DeltaOffset is the number of sample frames relative to the start of the current sample buffer.
function TCustomGlobals.NextQuarterNote(aDeltaOffset: integer): integer;
var
  x:double;
  QuarterNoteLength:integer;
  NextNote: integer;
begin
  QuarterNoteLength := round(SyncToSamples(1/4, Tempo, SampleRate));
  x := ppqPos - floor(ppqPos);
  NextNote := round((1 - x) * QuarterNoteLength);
  NextNote := NextNote - aDeltaOffset;
  if NextNote < 0 then NextNote := NextNote + QuarterNoteLength;
  result := NextNote;
end;

function TCustomGlobals.NextEighthNote(aDeltaOffset: integer): integer;
var
  x:double;
  EighthNoteLength:integer;
  NextNote: integer;
begin
  EighthNoteLength := round(SyncToSamples(1/8, Tempo, SampleRate));
  x := (ppqPos * 2) - floor(ppqPos * 2);
  NextNote := round((1 - x) * EighthNoteLength);
  NextNote := NextNote - aDeltaOffset;
  if NextNote < 0 then NextNote := NextNote + EighthNoteLength;
  result := NextNote;
end;


function TCustomGlobals.NextSixteenthNote(aDeltaOffset: integer): integer;
var
  x:double;
  SixteenthNoteLength:integer;
  NextNote: integer;
begin
  SixteenthNoteLength := round(SyncToSamples(1/16, Tempo, SampleRate));
  x := (ppqPos * 4) - floor(ppqPos * 4);
  NextNote := round((1 - x) * SixteenthNoteLength);
  NextNote := NextNote - aDeltaOffset;
  if NextNote < 0 then NextNote := NextNote + SixteenthNoteLength;
  result := NextNote;
end;

function TCustomGlobals.NextThirtySecondNote(aDeltaOffset: integer): integer;
var
  x:double;
  ThirtySecondNoteLength:integer;
  NextNote: integer;
begin
  ThirtySecondNoteLength := round(SyncToSamples(1/32, Tempo, SampleRate));
  x := (ppqPos * 8) - floor(ppqPos * 8);
  NextNote := round((1 - x) * ThirtySecondNoteLength);
  NextNote := NextNote - aDeltaOffset;
  if NextNote < 0 then NextNote := NextNote + ThirtySecondNoteLength;
  result := NextNote;
end;

procedure TCustomGlobals.TriggerEvent(EventType: TPluginEvent);
begin
  case EventType of
    SampleRateChanged: SampleRateList.TriggerAll(self);
    BlockSizeChanged:  BlockSizeList.TriggerAll(self);
    TempoChanged:      TempoList.TriggerAll(self);
    TransportChanged:  TransportChangedList.TriggerAll(self);
    PlayStateChanged:  PlayStateChangedList.TriggerAll(self);
    VstSuspendEvent:   VstSuspendList.TriggerAll(self);
    VstResumeEvent:    VstResumeList.TriggerAll(self);
  else
    raise Exception.Create('Event type not handled.');
  end;
end;

procedure TCustomGlobals.TriggerVstResumeEvent;
begin
  VstResumeList.TriggerAll(self);
end;

procedure TCustomGlobals.TriggerVstSuspendEvent;
begin
  VstSuspendList.TriggerAll(self);
end;

procedure TCustomGlobals.AddEventListener(EventType: TPluginEvent; EventHandler: TNotifyEvent);
var
  List : TEventList;
begin
  case EventType of
    SampleRateChanged: List := SampleRateList;
    BlockSizeChanged:  List := BlockSizeList;
    TempoChanged:      List := TempoList;
    TransportChanged:  List := TransportChangedList;
    PlayStateChanged:  List := PlayStateChangedList;
    VstSuspendEvent:   List := VstSuspendList;
    VstResumeEvent:    List := VstResumeList;
  else
    raise Exception.Create('Event Type not handled.');
    exit;
  end;

  List.Add(EventHandler);
end;

procedure TCustomGlobals.RemoveEventListener(EventType: TPluginEvent; EventHandler: TNotifyEvent);
var
  List : TEventList;
begin
  case EventType of
    SampleRateChanged: List := SampleRateList;
    BlockSizeChanged:  List := BlockSizeList;
    TempoChanged:      List := TempoList;
    TransportChanged:  List := TransportChangedList;
    PlayStateChanged:  List := PlayStateChangedList;
    VstSuspendEvent:   List := VstSuspendList;
    VstResumeEvent:    List := VstResumeList;
  else
    raise Exception.Create('Event Type not handled.');
    exit;
  end;

  List.Remove(EventHandler);
end;

end.


