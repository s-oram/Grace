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
  end;

  TCustomVstEditor = class(AEffEditor)
  private
    r : ERect;
    UseCount : Longint;
    GuiForm : TCustomVstGuiForm;
    GuiMeta : TObject;
  protected
  public
    constructor Create(aEffect: AudioEffect; aInitialGuiWidth, aInitialGuiHeight : integer); reintroduce;
    destructor Destroy; override;
    function GetRect(var rect: PERect): Longint; override;
    function Open(ptr: Pointer): Longint; override;
    procedure Close; override;
    procedure Idle; override;

    function OnKeyDown(var KeyCode: VstKeyCode): boolean; override;
    function OnKeyUp(var KeyCode: VstKeyCode): boolean; override;
  end;

  TCustomVstGuiForm = class(TForm)
  end;

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;

implementation

uses
  SysUtils,
  ee3.VstPluginFactory,
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

{ TCustomVstPluginEditor }

constructor TCustomVstEditor.Create(aEffect: AudioEffect; aInitialGuiWidth, aInitialGuiHeight: integer);
begin
  inherited Create(aEffect);

  r.top    := 0;
  r.left   := 0;
  r.right  := aInitialGuiWidth;
  r.bottom := aInitialGuiHeight;

  UseCount := 0;
end;

destructor TCustomVstEditor.Destroy;
begin
  if assigned(GuiForm) then GuiForm.Free;
  if assigned(GuiMeta) then GuiMeta.Free;

  inherited;
end;

function TCustomVstEditor.GetRect(var rect: PERect): Longint;
begin
  rect   := @r;
  Result := 1;
end;

function TCustomVstEditor.Open(ptr: Pointer): Longint;
var
  CreateGuiResult : TCreateVstPluginGuiResult;
begin
  if (UseCount = 0) then
  begin
    try
      CreateGuiResult := VstPluginFactory.CreateVstPluginGui(self.Effect, ptr, r.right, r.bottom);
      GuiForm := CreateGuiResult.GuiForm;
      GuiMeta := CreateGuiResult.GuiMeta;
    finally
      UseCount := 1;
    end;
  end;
  result := 1;
end;

procedure TCustomVstEditor.Close;
begin
  inherited;

  if (UseCount > 0) then
  begin
    try
      FreeAndNil(GuiForm);
    finally
      UseCount := 0;
    end;
  end;
end;

procedure TCustomVstEditor.Idle;
begin
  inherited;
end;

function TCustomVstEditor.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin
  result := false;
end;

function TCustomVstEditor.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin
  result := false;
end;


end.
