unit AudioPlugin.PlugMain;

{
  TODO:MED should remove all dependence on non-standard packages. That would make it an easy open source project
  to share.


  ## About the audio plugin template ##

  The plugin template has been created to facilitate writing VST 2.4 plugins.

  Features:
  * The plugin template is format agnostic. The majority of a plugin will be implemented
    in three main classes (TAudioPlug, TAudioPlugEdit & TGlobals), which will used in
    all plugin projects irrespective of output plugin format.
  * VST 2.4 plugin format support. VST specific functionality is contained within an adapter class.
    It should be possible to add aditional adapter classes to support other plugin formats.
  * Template classes are designed to be extended by client code so they can be adapted to
    unique project requirements. (The template uses dependency injection to achieve this.)
  * AirControl component facitates syncing the audio, GUI and background worker threads.
  * Sample accurate MIDI Input and Output.
  * Support for multi-rate processing. The plugin supports processing at audio rate and two
    control rates. The control rate process methods are useful for heavier calculations that
    don't need to be run at the audio rate.


  This Plugin Template VS the original VST 2.4 SDK:

  The VST 2.4 SDK provides a minimal starting point for writing VST plugins. It is the better
  choice if you need complete control over all project elements.

  The Plugin Template provides more functionality out of the box. The trade off is it's a
  little more opinionated and imposes a certain project structure. The main moving parts
  are designed to be extendable on a project by project basis, so hopefully it is still a
  useful starting point for all project types.

  The key advantages of the plugin template are:
  * It's structure supports writing multi-format plugins.
  * Thead awareness with means to run code in specific theads.
  * Key plugin functionality provided out of the box to get up and running sooner.

}

interface

uses
  PlugLib.Types,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.MidiEvent,
  AudioPlugin.Types,
  AudioPlugin.Interfaces;

type
  TRunTimeConfig = record
  public
    InputCount  : integer; // Number of audio inputs.  1 = mono. 2 = stereo.
    OutputCount : integer; // Number of audio outputs. 1 = mono. 2 = stereo.
    SampleRate  : integer; // Samplerate of audio inputs & outputs.
    BlockSize   : integer; // the maximum audio buffer size.
    SlowControlStepBufferSize : integer; // How many samples per slow control step.
    FastControlStepBufferSize : integer; // How many samples per fast control step.
    procedure Assign(const Source : TRunTimeConfig);
  end;

  TAbstractAudioPlug = class(TPureInterfacedObject, IPlugMain)
  private
  public
    constructor Create(const GlobalsPtr : Pointer); virtual;

    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single); overload;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); overload; virtual; abstract;
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult; virtual; abstract;

    // State Transitions
    procedure Open; virtual; abstract;     // Called when plug-in is initialized
    procedure Close; virtual; abstract;    // Called when plug-in will be released
    procedure Suspend; virtual; abstract;  // Called when plug-in is switched to off
    procedure Resume(const Config : TRunTimeConfig); virtual; abstract;    // Called when plug-in is switched to on

    procedure ProcessMidiEvent(ev : PMidiEvent); virtual; abstract;

    procedure ProcessMidiPulse(const Data : Pointer); virtual; abstract;

    procedure ProcessVstBlockStart(const TimeInfo : PVstTimeInfo; const Inputs, Outputs: PPSingle); virtual; abstract;
    procedure ProcessControlStepFast; virtual; abstract;
    procedure ProcessControlStepSlow; virtual; abstract;
    procedure ProcessAudio(const SampleFrames: integer); virtual; abstract;
  end;

  TAudioPlug = class(TAbstractAudioPlug);
  TAudioPlugClass = class of TAudioPlug;

implementation


{ TAbstractAudioPlug }

constructor TAbstractAudioPlug.Create(const GlobalsPtr: Pointer);
begin
  // do nothing...
end;

procedure TAbstractAudioPlug.SetPrivateParameter(const ParIndex: integer; const ParName: string; const ValueA: integer; const ValueB: single);
begin
  self.SetPrivateParameter(ParIndex, ParName, ValueA, ValueB, []);
end;



{ TRunTimeConfig }

procedure TRunTimeConfig.Assign(const Source: TRunTimeConfig);
begin
 self.InputCount  := Source.InputCount;
 self.OutputCount := Source.OutputCount;
 self.SampleRate  := Source.SampleRate;
 self.BlockSize   := Source.BlockSize;
 self.SlowControlStepBufferSize := Source.SlowControlStepBufferSize;
 self.FastControlStepBufferSize := Source.FastControlStepBufferSize;
end;

end.
