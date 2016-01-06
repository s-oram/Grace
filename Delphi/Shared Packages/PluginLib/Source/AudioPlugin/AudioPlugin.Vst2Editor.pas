unit AudioPlugin.Vst2Editor;

interface

uses
  Windows,
  AudioPlugin,
  AudioPlugin.Globals,
  AudioPlugin.Editor,
  AudioPlugin.EditorForm,
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  VamVst2.DAudioEffect,
  VamVst2.DAudioEffectX,
  Messages;

type
  TVstEditor = class(AEffEditor)
  private
    FGlobals: TGlobals;
  protected
    CurrentGuiSize : record
      OriginalRatio : double; // Original Width/Height
      Width  : integer;
      Height : integer;
    end;

    Effect : AudioEffect;
    Editor      : TAudioPluginEditor;
    EditorClass : TAudioPluginEditorClass;
    UseCount : integer;
    GuiRect : ERect;
    Form : TVclEditorForm;
    WindowHandle : HWND;

    property Globals : TGlobals read FGlobals;

    procedure ResizeGui(const OffsetX, OffsetY : integer);
  public
    constructor Create(aEffect: AudioEffect; const aPlug : TAudioPlugin; const aEditorClass : TAudioPluginEditorClass; const aGlobals : TGlobals); reintroduce;
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
  AudioPlugin.Functions;

{ TVstEditor }

constructor TVstEditor.Create(aEffect: AudioEffect; const aPlug: TAudioPlugin; const aEditorClass : TAudioPluginEditorClass; const aGlobals : TGlobals);
var
  r : TRect;
begin
  inherited Create(aEffect);

  FGlobals := aGlobals;
  Globals.Vst2.ResizeGui := self.ResizeGui;

  Effect := aEffect;

  UseCount := 0;
  EditorClass := aEditorClass;

  r := EditorClass.GetInitialGuiSize;
  CurrentGuiSize.Width  := r.Width;
  CurrentGuiSize.Height := r.Height;
  CurrentGuiSize.OriginalRatio := r.Width / r.Height;
end;

destructor TVstEditor.Destroy;
begin
  Globals.Vst2.ResizeGui := nil;
  inherited;
end;

function TVstEditor.GetRect(var rect: PERect): longint;
begin
  GuiRect.top  := 0;
  GuiRect.left := 0;
  GuiRect.right  := CurrentGuiSize.Width;
  GuiRect.bottom := CurrentGuiSize.Height;

  rect := @GuiRect;
  result := 1;
end;

procedure TVstEditor.ResizeGui(const OffsetX, OffsetY: integer);
var
  NewSize : TSize;
  MinGuiSize : TSize;
  ResizeResult : boolean;
begin
  MinGuiSize := EditorClass.GetMinGuiSize;

  NewSize := CalcNewEditorSize(CurrentGuiSize.OriginalRatio, CurrentGuiSize.Width, CurrentGuiSize.Height, OffsetX, OffsetY);

  if NewSize.cx < MinGuiSize.cx
    then NewSize := MinGuiSize;

  ResizeResult := (Effect as AudioEffectX).SizeWindow(NewSize.cx, NewSize.cy);
  if (ResizeResult = true) then
  begin
    CurrentGuiSize.Width  := NewSize.Width;
    CurrentGuiSize.Height := NewSize.Height;

    Form.Width  := NewSize.Width;
    Form.Height := NewSize.Height;
  end else
  begin
    // TODO:MED
    // try to implement GUI sizing by directly manipulating the window handle.
  end;



end;

procedure TVstEditor.Idle;
begin
  inherited;

end;

function TVstEditor.IsOpen: boolean;
begin
  if UseCount > 0
    then result := true
    else result := false;
end;

function TVstEditor.Open(ptr: pointer): longint;
begin
  inherited;

  inc(UseCount);
  if UseCount = 1 then
  begin
    WindowHandle := hwnd(ptr);
    Form := TVclEditorForm.CreateParented(WindowHandle);
    Form.Width  := CurrentGuiSize.Width;
    Form.Height := CurrentGuiSize.Height;

    Editor := EditorClass.Create(Form, Globals);

    Form.Visible := true;
  end;
  result := 1;
end;

procedure TVstEditor.Close;
begin
  inherited;

  dec(UseCount);
  if UseCount = 0 then
  begin
    if assigned(Editor) then FreeAndNil(Editor);
    if assigned(Form)   then FreeAndNil(Form);
  end;
end;



function TVstEditor.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin

end;

function TVstEditor.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin

end;

function TVstEditor.OnWheel(Distance: single): boolean;
begin

end;



function TVstEditor.SetKnobMode(Val: VstInt32): boolean;
begin

end;

end.
