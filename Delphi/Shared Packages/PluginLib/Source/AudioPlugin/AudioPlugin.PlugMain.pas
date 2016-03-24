unit AudioPlugin.PlugMain;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamLib.MoreTypes,
  VamVst2.MidiEvent, //TODO:MED <- Maybe this unit shoud be pulled into the AudioPlugin section.
  AudioPlugin.Globals,
  AudioPlugin.RunTimeInfo;

type
  TAudioPlug = class;
  TAudioPlugClass = class of TAudioPlug;

  TAudioPlug = class
  private
    FVstParameterCount: integer;
    FGlobals: TGlobals;
  protected
    FVstPar : TArrayOfSingle;
    procedure SetVstParameterCount(Value : integer);

    procedure SetVstParameter(Index : integer; Value: single); virtual;
    function GetVstParameter(Index: integer): single; virtual;

    property Globals : TGlobals read FGlobals;
  public
    constructor Create(const aGlobals : TGlobals); virtual;
    destructor Destroy; override;

    procedure GetPrivateParameter(const ParIndex : integer; const ParValue : Pointer; const DataSize : integer); virtual;
    procedure SetPrivateParameter(const ParIndex : integer; const ParValue : Pointer; const DataSize : integer); virtual;

    // LoadDefaultPatch() is called once after a plugin is newly created.
    // Initialise the plugin to the "default" initial state here.
    procedure LoadDefaultPatch; virtual;

    // ResetPlugState().
    // Initialise the plugin to the "zero" or "empty" state here.
    procedure ResetPlugState; virtual;

    property VstParameter[Index : integer] : single read GetVstParameter write SetVstParameter;
    property VstParameterCount : integer read FVstParameterCount;

    // State Transitions
    procedure Open; virtual;      // Called when plug-in is initialized
    procedure Close; virtual;     // Called when plug-in will be released
    procedure Suspend; virtual;   // Called when plug-in is switched to off
    procedure Resume(const RunTimeInfo : TRunTimeInfo); virtual;    // Called when plug-in is switched to on

    procedure ProcessMidiEvent(ev : PMidiEvent); virtual;
    procedure ProcessControlStepSlow(const SampleFrames : integer); virtual;
    procedure ProcessControlStepFast(const SampleFrames : integer); virtual;
    procedure ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer); virtual;

    procedure ProcessVstTimeInfo(const TimeInfo : PVstTimeInfo); virtual;
  end;

implementation

uses
  Helm.Message,
  AudioPlugin.Types,
  AudioPlugin.Events;

{ TAudioPlugin }

constructor TAudioPlug.Create(const aGlobals : TGlobals);
var
  msg : THelmMessage;
begin
  FGlobals := aGlobals;
  SetLength(FVstPar, 0);
  FVstParameterCount := 0;

  Globals.AudioPlugMethods.GetPrivateParameter := self.GetPrivateParameter;
  Globals.AudioPlugMethods.SetPrivateParameter := self.SetPrivateParameter;
end;

destructor TAudioPlug.Destroy;
begin
  SetLength(FVstPar, 0);
  inherited;
end;

procedure TAudioPlug.Open;
begin

end;

procedure TAudioPlug.Close;
begin

end;

procedure TAudioPlug.Suspend;
begin

end;

procedure TAudioPlug.Resume(const RunTimeInfo : TRunTimeInfo);
begin
  // NOTE: RE: RunTimeInfo.InputCount & RunTimeInfo.OutputCount.
  //
  // The number of inputs and outputs is specified in the resume method. While
  // many plugins will never change their input/output configuration, specifying
  // it here will help with those that do.
  // The VST2 plugin specification makes no requirement for plugin hosts to support
  // plugins updating their input/output config. As such, requests may be refused.
  // Specifying the input/output here will provide a simple means for halting input/output
  // config changes.
end;

procedure TAudioPlug.SetVstParameterCount(Value: integer);
begin
  assert(Value >= 0);
  FVstParameterCount := Value;
  SetLength(FVstPar, Value);
end;

procedure TAudioPlug.SetVstParameter(Index: integer; Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  FVstPar[Index] := Value;

  // TODO:HIGH. Globals isn't assigned here in the test runner.
  //Globals.EventDispatcher.Broadcast(TVstParameterChanged.Create(Index, Value));
end;

procedure TAudioPlug.GetPrivateParameter(const ParIndex: integer; const ParValue: Pointer; const DataSize: integer);
begin
  // do nothing.
end;

procedure TAudioPlug.SetPrivateParameter(const ParIndex: integer; const ParValue: Pointer; const DataSize: integer);
begin
  // do nothing.
end;



function TAudioPlug.GetVstParameter(Index: integer): single;
begin
  result := FVstPar[Index];
  assert(result >= 0);
  assert(result <= 1);
end;

procedure TAudioPlug.LoadDefaultPatch;
begin
  ResetPlugState;
end;

procedure TAudioPlug.ResetPlugState;
var
  c1: Integer;
begin
  for c1 := 0 to VstParameterCount-1 do
  begin
    VstParameter[c1] := 0.5;
  end;
end;

procedure TAudioPlug.ProcessMidiEvent(ev: PMidiEvent);
begin

end;

procedure TAudioPlug.ProcessControlStepSlow(const SampleFrames: integer);
begin

end;

procedure TAudioPlug.ProcessControlStepFast(const SampleFrames: integer);
begin

end;

procedure TAudioPlug.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer);
begin

end;

procedure TAudioPlug.ProcessVstTimeInfo(const TimeInfo: PVstTimeInfo);
begin

end;





end.
