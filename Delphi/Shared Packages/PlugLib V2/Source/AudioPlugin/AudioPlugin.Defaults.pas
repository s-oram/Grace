unit AudioPlugin.Defaults;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.MidiEvent,
  AudioPlugin.Types,
  AudioPlugin.PlugMain,
  AudioPlugin.Globals,
  AudioPlugin.Vst2Adapter;

type
  TDefaultPlug = class(TAudioPlug)
  private
  public
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); override;
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult; override;

    // State Transitions
    procedure Open; override;     // Called when plug-in is initialized
    procedure Close; override;    // Called when plug-in will be released
    procedure Suspend; override;  // Called when plug-in is switched to off
    procedure Resume(const Config : TRunTimeConfig); override;    // Called when plug-in is switched to on

    procedure ProcessMidiEvent(ev : PMidiEvent); override;

    procedure ProcessVstBlockStart(const TimeInfo : PVstTimeInfo; const Inputs, Outputs: PPSingle); override;
    procedure ProcessControlStepSlow; override;
    procedure ProcessControlStepFast; override;
    procedure ProcessAudio(const SampleFrames: integer); override;
  end;

  TDefaultGlobals = class(TGlobals);


implementation

uses
  VamVst2.DAudioEffectX,
  SysUtils;

{ TDefaultPlug }

procedure TDefaultPlug.Close;
begin
  inherited;

end;

function TDefaultPlug.GetPrivateParameter(const ParIndex: integer; const ParName: string): TPrivateParResult;
begin
  result.ValueA := 0;
  result.ValueB := 0;
end;

procedure TDefaultPlug.Open;
begin
  inherited;

end;

procedure TDefaultPlug.ProcessAudio(const SampleFrames: integer);
begin
  inherited;

end;

procedure TDefaultPlug.ProcessControlStepFast;
begin
  inherited;

end;

procedure TDefaultPlug.ProcessControlStepSlow;
begin
  inherited;

end;

procedure TDefaultPlug.ProcessMidiEvent(ev: PMidiEvent);
begin
  inherited;

end;

procedure TDefaultPlug.ProcessVstBlockStart(const TimeInfo: PVstTimeInfo; const Inputs, Outputs: PPSingle);
begin
  inherited;

end;

procedure TDefaultPlug.Resume(const Config : TRunTimeConfig);
begin
  inherited;

end;

procedure TDefaultPlug.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single; const Data: array of pointer);
begin
  inherited;

end;

procedure TDefaultPlug.Suspend;
begin
  inherited;

end;

end.
