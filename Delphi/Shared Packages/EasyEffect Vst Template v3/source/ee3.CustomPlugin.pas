unit ee3.CustomPlugin;

interface

uses
  Vcl.Forms,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX,
  ee3.VstConstants,
  ee3.ProcessController;

type
  TCustomVstPlugin = class;
  TCustomVstEditor = class;
  TCustomVstGuiForm = class;

  TCustomVstPlugin = class(AudioEffectX)
  private
    ProcessController : TCustomProcessController;
  protected
    // InjectDependency() - TCustomVstPlugin will assume ownership of injected objects.
    procedure InjectDependency(aProcessController : TCustomProcessController);
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32); reintroduce;
    destructor Destroy; override;

    procedure SetUniqueID(ID: string); reintroduce;

    function ProcessEvents(Events: PVstEvents): VstInt32; override;

    procedure Suspend; override;   // Called when plug-in is switched to off
    procedure Resume; override;    // Called when plug-in is switched to on

    procedure SetNumInputs(Inputs: VstInt32); override;   // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
    procedure SetNumOutputs(Outputs: VstInt32); override; // Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available ouputs.

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;
  end;

  TCustomVstEditor = class(AEffEditor)
  private
  protected
  public
  end;

  TCustomVstGuiForm = class(TForm)
  end;

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;

implementation

uses
  SysUtils,
  DVstUtils;

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
begin
  assert(assigned(TCustomVstPlugin(e^.vObject).ProcessController), 'ProcessController not assigned.');
  TCustomVstPlugin(e^.vObject).ProcessController.ProcessReplacing(Inputs, Outputs, SampleFrames);
end;

procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;
begin
  assert(assigned(TCustomVstPlugin(e^.vObject).ProcessController), 'ProcessController not assigned.');
  TCustomVstPlugin(e^.vObject).ProcessController.ProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;

{ TCustomPlugin }

constructor TCustomVstPlugin.Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32);
begin
  inherited Create(anAudioMaster, aNumPrograms, aNumParams);

  Effect.processReplacing := ProcessClassReplacing;
  Effect.processDoubleReplacing := ProcessClassDoubleReplacing;

  Randomize;
end;

destructor TCustomVstPlugin.Destroy;
begin
  if assigned(ProcessController) then ProcessController.Free;
  inherited;
end;

procedure TCustomVstPlugin.InjectDependency(aProcessController: TCustomProcessController);
begin
  if assigned(ProcessController) then ProcessController.Free;
  ProcessController := aProcessController;
end;

procedure TCustomVstPlugin.Suspend;
begin
  inherited;
  ProcessController.Suspend;
end;

procedure TCustomVstPlugin.Resume;
begin
  inherited;
  ProcessController.Resume;
end;

function TCustomVstPlugin.ProcessEvents(Events: PVstEvents): VstInt32;
begin
  ProcessController.ProcessVstEvents(Events);
  result := 0;
end;

procedure TCustomVstPlugin.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
begin
  inherited;
end;

procedure TCustomVstPlugin.SetNumInputs(Inputs: VstInt32);
begin
  inherited;
  ProcessController.SetNumInputs(Inputs);
end;

procedure TCustomVstPlugin.SetNumOutputs(Outputs: VstInt32);
begin
  inherited;
  ProcessController.SetNumOutputs(Outputs);
end;

procedure TCustomVstPlugin.SetUniqueID(ID: string);
var
  UniqueId:AnsiString;
begin
  assert(Length(ID) = 4);
  UniqueId := AnsiString(ID);
  inherited SetUniqueID(FourCharToLong(UniqueID[1],UniqueID[2],UniqueID[3],UniqueID[4]));
end;



end.
