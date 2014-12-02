unit ee3.ProcessController;

{
  VST Plugins process audio and MIDI events. Plugins may:
  - receive audio
  - send audio
  - receive MIDI
  - send MIDI
  - or any combination of the above.

  Additionally some plugins will
  - use internally oversampled audio.
  - split the audio buffer according the the MIDI event time stamps for sample accurate MIDI processing.
  - split the audio buffer for control-rate processing.
  - and so on...

  The Porocess Controller:
  - readies the raw input and calls the effect processing routines as required.
  - readies the effect output for sending via the VST protocol.

  Because the Process Controller requirements can be radically different from plugin to plugin, the
  plugin template uses dependency injection. The plugin developer must provide the plugin template
  with an appropiate Process Controller. All Process Controllers should inherit from TCustomProcessController.
}

interface

uses
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX;

type
  TCustomProcessController = class
  private
  public
    procedure Suspend; virtual; abstract;
    procedure Resume; virtual; abstract;

    procedure ProcessVstEvents(Events: PVstEvents); virtual; abstract;

    procedure SetNumInputs(Inputs: VstInt32); virtual; abstract;  // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
    procedure SetNumOutputs(Outputs: VstInt32); virtual; abstract; // Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available ouputs.

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); virtual; abstract;       // Process 32 bit (single precision) floats (always in a resume state)
    procedure ProcessDoubleReplacing(Inputs, Outputs: PPDouble; SampleFrames: VstInt32); virtual; abstract; // Process 64 bit (double precision) floats (always in a resume state)
  end;

procedure IncrementVstAudioBufferPointers(BufferPointers : PPSingle; const BufferCount : integer; const SampleFrames : integer); overload; inline;
procedure IncrementVstAudioBufferPointers(BufferPointers : PPDouble; const BufferCount : integer; const SampleFrames : integer); overload; inline;

implementation

procedure IncrementVstAudioBufferPointers(BufferPointers : PPSingle; const BufferCount : integer; const SampleFrames : integer);
var
  c1: Integer;
begin
  for c1 := 0 to BufferCount-1 do
  begin
    inc(BufferPointers^, SampleFrames);
    inc(BufferPointers);
  end;
end;

procedure IncrementVstAudioBufferPointers(BufferPointers : PPDouble; const BufferCount : integer; const SampleFrames : integer);
var
  c1: Integer;
begin
  for c1 := 0 to BufferCount-1 do
  begin
    inc(BufferPointers^, SampleFrames);
    inc(BufferPointers);
  end;
end;


end.
