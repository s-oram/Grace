unit AudioPlugin.PlugMain;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamLib.MoreTypes,
  VamVst2.MidiEvent, //TODO:MED <- Maybe this unit shoud be pulled into the AudioPlugin section.
  AudioPlugin.RunTimeInfo;

type
  TAudioPlug = class;
  TAudioPlugClass = class of TAudioPlug;

  TPrivateParResult = record
  public
    ValueA : integer;
    ValueB : single;
    Data   : array of pointer;
  end;

  TAudioPlug = class
  private
  protected
  public
    constructor Create(const GlobalsPtr : Pointer); virtual;
    destructor Destroy; override;

    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single); overload;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); overload; virtual; abstract;
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult; virtual; abstract;

    // LoadDefaultPatch() is called once after a plugin is newly created.
    // Initialise the plugin to the "default" initial state here.
    procedure LoadDefaultPatch; virtual;

    // ResetPlugState().
    // Initialise the plugin to the "zero" or "empty" state here.
    procedure ResetPlugState; virtual;

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
  AudioPlugin.Types;

{ TAudioPlugin }

constructor TAudioPlug.Create(const GlobalsPtr : Pointer);
begin
end;

destructor TAudioPlug.Destroy;
begin
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

procedure TAudioPlug.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single);
begin
  SetPrivateParameter(ParIndex, ParName, ValueA, ValueB, []);
end;

procedure TAudioPlug.LoadDefaultPatch;
begin
  ResetPlugState;
end;

procedure TAudioPlug.ResetPlugState;
begin
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
