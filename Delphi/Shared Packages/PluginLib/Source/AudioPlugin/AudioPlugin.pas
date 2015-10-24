unit AudioPlugin;

interface

uses
  VamLib.MoreTypes,
  VamVst2.MidiEvent, //TODO:MED <- Maybe this unit shoud be pulled into the AudioPlugin section.
  AudioPlugin.Globals,
  AudioPlugin.RunTimeInfo;

type
  TAudioPlugin = class;
  TAudioPluginClass = class of TAudioPlugin;

  TAudioPlugin = class
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
  end;

implementation

{ TAudioPlugin }

constructor TAudioPlugin.Create(const aGlobals : TGlobals);
begin
  FGlobals := aGlobals;
  SetLength(FVstPar, 0);
  FVstParameterCount := 0;
end;

destructor TAudioPlugin.Destroy;
begin
  SetLength(FVstPar, 0);
  inherited;
end;

procedure TAudioPlugin.Open;
begin

end;

procedure TAudioPlugin.Close;
begin

end;

procedure TAudioPlugin.Suspend;
begin

end;

procedure TAudioPlugin.Resume(const RunTimeInfo : TRunTimeInfo);
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

procedure TAudioPlugin.SetVstParameterCount(Value: integer);
begin
  assert(Value >= 0);
  FVstParameterCount := Value;
  SetLength(FVstPar, Value);
end;

procedure TAudioPlugin.SetVstParameter(Index: integer; Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  FVstPar[Index] := Value;
end;

function TAudioPlugin.GetVstParameter(Index: integer): single;
begin
  result := FVstPar[Index];
  assert(result >= 0);
  assert(result <= 1);
end;

procedure TAudioPlugin.LoadDefaultPatch;
begin
  ResetPlugState;
end;

procedure TAudioPlugin.ResetPlugState;
var
  c1: Integer;
begin
  for c1 := 0 to VstParameterCount-1 do
  begin
    VstParameter[c1] := 0.5;
  end;
end;

procedure TAudioPlugin.ProcessMidiEvent(ev: PMidiEvent);
begin

end;

procedure TAudioPlugin.ProcessControlStepSlow(const SampleFrames: integer);
begin

end;

procedure TAudioPlugin.ProcessControlStepFast(const SampleFrames: integer);
begin

end;

procedure TAudioPlugin.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer);
begin

end;



end.
