unit AudioPlugin.Vst2Adapter;

interface

uses
  Windows,
  ExtCtrls,
  VamVst2.MidiEvent,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.DAudioEffect,
  VamVst2.DAudioEffectX,
  VamVst2.MidiEventInputBuffer,
  VamVst2.MidiEventOutputBuffer,
  AudioPlugin.Types,
  AudioPlugin.Interfaces,
  AudioPlugin.PlugMain,
  AudioPlugin.Globals,
  AudioPlugin.PlugEdit,
  AudioPlugin.ProcessController;

type
  // Implement interface support without reference counting.
  TAudioEffectXWithPureInterface = class(AudioEffectX, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


  TVst2Adapter = class(TAudioEffectXWithPureInterface, IVst2AudioEffectX)
  private
  protected
    // These are setup methods and should only be called from the constructor.
    procedure SetNumPrograms(const Value : integer);
    procedure SetNumParameters(const Value : integer);
  public
    constructor Create(const AMC : TAudioMasterCallbackFunc); reintroduce; virtual;
  end;


  //==== The VST Editor ====

  TVst2EditAdapter = class(AEffEditor)
  private
    FGlobals: TGlobals;
  protected
    CurrentGuiSize : record
      OriginalRatio : double; // Original Width/Height
      Width  : integer;
      Height : integer;
    end;

    Editor      : TAudioPlugEditor;
    EditorClass : TAudioPlugEditorClass;
    UseCount : integer;
    GuiRect : ERect;
    WindowHandle : HWND;
    AirControlTimer : TTimer;

    property Globals : TGlobals read FGlobals;
    procedure HandleAirControlTimerEvent(Sender : TObject);
  public
    constructor Create(const aEffect: AudioEffect; const aEditorClass : TAudioPlugEditorClass; const GlobalsPtr : Pointer); reintroduce;
    destructor Destroy; override;

    function GetRect(var rect: PERect): longint; override;
    function Open(ptr: pointer): longint; override;
    procedure Close; override;
    function IsOpen: boolean; override;
    procedure Idle; override;

    function OnKeyDown(var KeyCode: VstKeyCode): boolean; override;
    function OnKeyUp(var KeyCode: VstKeyCode): boolean; override;
    function SetKnobMode(Val: VstInt32): boolean; override;
    function OnWheel(Distance: single): boolean; override;
  end;

implementation



uses
  SysUtils,
  Classes,
  AudioPlugin.Functions;


{ TAudioEffectXWithPureInterface }

function TAudioEffectXWithPureInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TAudioEffectXWithPureInterface._AddRef: Integer;
begin
  Result := -1;
end;

function TAudioEffectXWithPureInterface._Release: Integer;
begin
  Result := -1;
end;




{ TVst2Adapter }

constructor TVst2Adapter.Create(const AMC : TAudioMasterCallbackFunc);
begin
  inherited Create(AMC, 0, 0);
end;

procedure TVst2Adapter.SetNumParameters(const Value: integer);
begin
  FEffect.numParams := Value;
end;

procedure TVst2Adapter.SetNumPrograms(const Value: integer);
begin
  FEffect.numPrograms := Value;
end;

{ TVst2EditAdapter }

constructor TVst2EditAdapter.Create(const aEffect: AudioEffect; const aEditorClass : TAudioPlugEditorClass; const GlobalsPtr : Pointer);
var
  r : TRect;
begin
  inherited Create(aEffect);

  AirControlTimer := TTimer.Create(nil);
  AirControlTimer.Enabled := false;
  AirControlTimer.Interval := 20;
  AirControlTimer.OnTimer := self.HandleAirControlTimerEvent;

  assert(assigned(GlobalsPtr));
  assert(TObject(GlobalsPtr) is TGlobals);
  FGlobals := TGlobals(GlobalsPtr);

  UseCount := 0;
  EditorClass := aEditorClass;

  r := EditorClass.GetInitialGuiSize;
  CurrentGuiSize.Width  := r.Width;
  CurrentGuiSize.Height := r.Height;
  CurrentGuiSize.OriginalRatio := r.Width / r.Height;
end;

destructor TVst2EditAdapter.Destroy;
begin
  AirControlTimer.Free;
  inherited;
end;

function TVst2EditAdapter.GetRect(var rect: PERect): longint;
begin
  GuiRect.top  := 0;
  GuiRect.left := 0;
  GuiRect.right  := CurrentGuiSize.Width;
  GuiRect.bottom := CurrentGuiSize.Height;

  rect := @GuiRect;
  result := 1;
end;

procedure TVst2EditAdapter.HandleAirControlTimerEvent(Sender: TObject);
begin
  assert(assigned(Globals));
  Globals.AirControl.ProcessGuiSync;
end;

procedure TVst2EditAdapter.Idle;
begin
  inherited;

end;

function TVst2EditAdapter.IsOpen: boolean;
begin
  if UseCount > 0
    then result := true
    else result := false;
end;

function TVst2EditAdapter.Open(ptr: pointer): longint;
begin
  inherited;

  inc(UseCount);
  if UseCount = 1 then
  begin
    WindowHandle := hwnd(ptr);

    // IMPORTANT: Start the timer before opening the GUI.

    // 1)
    Editor := EditorClass.Create(Globals);
    Editor.Open(WindowHandle, CurrentGuiSize.Width, CurrentGuiSize.Height);

    // 2)
    AirControlTimer.Enabled := true;
  end;
  result := 1;
end;

procedure TVst2EditAdapter.Close;
begin
  inherited;

  dec(UseCount);
  if UseCount = 0 then
  begin
    if assigned(Editor) then
    begin
      // IMPORTANT: Stop the timer before closing the GUI.

      // 1)
      AirControlTimer.Enabled := false;

      // 2)
      Editor.Close;
      FreeAndNil(Editor);
    end;
  end;
end;



function TVst2EditAdapter.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin
  result := false;
end;

function TVst2EditAdapter.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin
  result := false;
end;

function TVst2EditAdapter.OnWheel(Distance: single): boolean;
begin
  result := false;
end;

function TVst2EditAdapter.SetKnobMode(Val: VstInt32): boolean;
begin
  result := false;
end;



end.
