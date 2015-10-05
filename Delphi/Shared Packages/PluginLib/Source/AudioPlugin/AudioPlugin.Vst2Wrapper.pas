unit AudioPlugin.Vst2Wrapper;

interface

uses
  VamLib.MoreTypes,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.DAudioEffect,
  VamVst2.DAudioEffectX,
  AudioPlugin,
  AudioPlugin.Vst2PluginInfo,
  AudioPlugin.ProcessController;

type
  TVst2WrapperCreateInfo = record
    PlugClass         : TAudioPluginClass;
    PlugInfo          : TVst2PluginInfoClass;
    ProcessController : TProcessControllerClass;
  end;

  TVst2Wrapper = class(AudioEffectX)
  private
  protected
    Plug : TAudioPlugin;
    PlugInfo : TVst2PluginInfo;
    ProcessController : TProcessController;
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2WrapperCreateInfo); reintroduce;
    destructor Destroy; override;

    // State Transitions
    procedure Open; override;      // Called when plug-in is initialized
    procedure Close; override;     // Called when plug-in will be released
    procedure Suspend; override;   // Called when plug-in is switched to off
    procedure Resume; override;    // Called when plug-in is switched to on


    // Parameters
    procedure SetParameter(index: Longint; value: Single); override;
    function GetParameter(index: Longint): Single; override;

    procedure GetParameterLabel(index: Longint; aLabel: PAnsiChar); override;
    procedure GetParameterDisplay(index: Longint; text: PAnsiChar); override;
    procedure GetParameterName(index: Longint; text: PAnsiChar); override;


    function  ProcessEvents(ev: PVstEvents): longint; override;
    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;
  end;

implementation

uses
  SysUtils;

{ TVst2Wrapper }

constructor TVst2Wrapper.Create(anAudioMaster: TAudioMasterCallbackFunc; const CreateInfo : TVst2WrapperCreateInfo);
begin
  Plug := CreateInfo.PlugClass.Create;
  PlugInfo := CreateInfo.PlugInfo.Create(Plug);
  ProcessController := CreateInfo.ProcessController.Create(Plug);

  inherited Create(anAudioMaster, PlugInfo.GetNumberOfPrograms, Plug.VstParameterCount);

  setNumInputs(PlugInfo.GetNumberOfAudioInputs);
  setNumOutputs(PlugInfo.GetNumberOfAudioOutputs);


  // finally..
  Plug.LoadDefaultPatch;
end;

destructor TVst2Wrapper.Destroy;
begin
  Plug.Free;
  PlugInfo.Free;
  ProcessController.Free;
  inherited;
end;

procedure TVst2Wrapper.Open;
begin
  inherited;
  Plug.Open;
end;

procedure TVst2Wrapper.Close;
begin
  inherited;
  Plug.Close;
end;

procedure TVst2Wrapper.Suspend;
begin
  inherited;
  Plug.Suspend;
  ProcessController.Suspend;
end;

procedure TVst2Wrapper.Resume;
begin
  inherited;
  Plug.Resume;
  ProcessController.Resume(PlugInfo.GetNumberOfAudioInputs, PlugInfo.GetNumberOfAudioOutputs, 4410, 44100);
end;

procedure TVst2Wrapper.SetParameter(index: Integer; value: Single);
begin
  Plug.VstParameter[Index] := Value;
end;

function TVst2Wrapper.GetParameter(index: Integer): Single;
begin
  result := Plug.VstParameter[Index];
end;

procedure TVst2Wrapper.GetParameterDisplay(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterDisplay(Index)));
end;

procedure TVst2Wrapper.GetParameterLabel(index: Integer; aLabel: PAnsiChar);
begin
  StrPCopy(aLabel, AnsiString(PlugInfo.GetParameterLabel(Index)));
end;

procedure TVst2Wrapper.GetParameterName(index: Integer; text: PAnsiChar);
begin
  StrPCopy(text, AnsiString(PlugInfo.GetParameterName(Index)));
end;

function TVst2Wrapper.ProcessEvents(ev: PVstEvents): longint;
begin
  ProcessController.ProcessEvents(ev);
  //TODO:HIGH what should the return value be?
end;

procedure TVst2Wrapper.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
begin
  ProcessController.ProcessReplacing(VamLib.MoreTypes.PPSingle(Inputs), VamLib.MoreTypes.PPSingle(Outputs), SampleFrames);
end;



end.
