unit ee3.CustomPlugin;

interface

uses
  Vcl.Forms,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX,
  ee3.VstConstants;

type
  TCustomVstPlugin = class;
  TCustomVstEditor = class;
  TCustomVstGuiForm = class;

  TCustomVstPlugin = class(AudioEffectX)
  private
  public
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32); reintroduce;
    destructor Destroy; override;

    procedure SetUniqueID(ID: string); reintroduce;
  end;

  TCustomVstEditor = class(AEffEditor)
  private
    r : ERect;
    UseCount : Longint;
    GuiForm : TCustomVstGuiForm;
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


implementation

uses
  SysUtils,
  ee3.VstPluginFactory,
  DVstUtils;

{ TCustomPlugin }

constructor TCustomVstPlugin.Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32);
begin
  inherited Create(anAudioMaster, aNumPrograms, aNumParams);

  Randomize;
end;

destructor TCustomVstPlugin.Destroy;
begin

  inherited;
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

  inherited;
end;

function TCustomVstEditor.GetRect(var rect: PERect): Longint;
begin
  rect   := @r;
  Result := 1;
end;

function TCustomVstEditor.Open(ptr: Pointer): Longint;
begin
  if (UseCount = 0) then
  begin
    try
      GuiForm := VstPluginFactory.CreateVstPluginGui(self.Effect, ptr, r.right, r.bottom);
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
