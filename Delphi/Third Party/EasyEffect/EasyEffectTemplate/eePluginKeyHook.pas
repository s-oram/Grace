unit eePluginKeyHook;

interface

uses
  Windows,
  eeKeyboardHook,
  eeKeyboardHookConfig,
  DAEffect, DAEffectX;

type
  TVstKeyEvent = function(var KeyCode: VstKeyCode): boolean of object;

  TPluginKeyHook = class
  private
    fOnKeyDown: TVstKeyEvent;
    fOnKeyUp: TVstKeyEvent;
  protected
    x : integer;
    fDefaultKeyHookTarget:HWND;
    KeyboardHook:TKeyboardHook;

    procedure UpdateKeyboardHook;

    //These methods are event handlers for the keyboard hook class.
    function Hook_KeyDown(VirtualKeyCode:word):boolean;
    function Hook_KeyUp(VirtualKeyCode:word):boolean;

    property DefaultKeyHookTarget  : HWND    read fDefaultKeyHookTarget;
  public
    constructor Create(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName:string);
    destructor Destroy; override;

    procedure RefreshKeyHookTarget;

    property OnKeyDown : TVstKeyEvent read fOnKeyDown write fOnKeyDown;
    property OnKeyUp   : TVstKeyEvent read fOnKeyUp   write fOnKeyUp;

  end;

implementation

uses
  //Dialogs,
  SysUtils;

const
  //In Windows.pas the declarations for these virtual
  //key codes have been commented out.
  VK_0 = $30;
  VK_1 = $31;
  VK_2 = $32;
  VK_3 = $33;
  VK_4 = $34;
  VK_5 = $35;
  VK_6 = $36;
  VK_7 = $37;
  VK_8 = $38;
  VK_9 = $39;

  VK_A = $41;
  VK_B = $42;
  VK_C = $43;
  VK_D = $44;
  VK_E = $45;
  VK_F = $46;
  VK_G = $47;
  VK_H = $48;
  VK_I = $49;
  VK_J = $4A;
  VK_K = $4B;
  VK_L = $4C;
  VK_M = $4D;
  VK_N = $4E;
  VK_O = $4F;
  VK_P = $50;
  VK_Q = $51;
  VK_R = $52;
  VK_S = $53;
  VK_T = $54;
  VK_U = $55;
  VK_V = $56;
  VK_W = $57;
  VK_X = $58;
  VK_Y = $59;
  VK_Z = $5A;

function MapWindowsVirtualKeyCodeToVSTAscii(VirtualKeyCode: word):integer;
begin
  case VirtualKeyCode of
  VK_0: result := Ord('0');
  VK_1: result := Ord('1');
  VK_2: result := Ord('2');
  VK_3: result := Ord('3');
  VK_4: result := Ord('4');
  VK_5: result := Ord('5');
  VK_6: result := Ord('6');
  VK_7: result := Ord('7');
  VK_8: result := Ord('8');
  VK_9: result := Ord('9');
  VK_A: result := Ord('a');
  VK_B: result := Ord('b');
  VK_C: result := Ord('c');
  VK_D: result := Ord('d');
  VK_E: result := Ord('e');
  VK_F: result := Ord('f');
  VK_G: result := Ord('g');
  VK_H: result := Ord('h');
  VK_I: result := Ord('i');
  VK_J: result := Ord('j');
  VK_K: result := Ord('k');
  VK_L: result := Ord('l');
  VK_M: result := Ord('m');
  VK_N: result := Ord('n');
  VK_O: result := Ord('o');
  VK_P: result := Ord('p');
  VK_Q: result := Ord('q');
  VK_R: result := Ord('r');
  VK_S: result := Ord('s');
  VK_T: result := Ord('t');
  VK_U: result := Ord('u');
  VK_V: result := Ord('v');
  VK_W: result := Ord('w');
  VK_X: result := Ord('x');
  VK_Y: result := Ord('y');
  VK_Z: result := Ord('z');
  else
    result := 0;
  end;
end;


function MapWindowsVirtualKeyCodeToVstVirtualKeyCode(VirtualKeyCode: word):byte;
begin
  case VirtualKeyCode of
    VK_BACK:      result := VKEY_BACK;
    VK_TAB:       result := VKEY_TAB;
    VK_CLEAR:     result := VKEY_CLEAR;
    VK_RETURN:    result := VKEY_RETURN;
    VK_PAUSE:     result := VKEY_PAUSE;
    VK_ESCAPE:    result := VKEY_ESCAPE;
    VK_SPACE:     result := VKEY_SPACE;
    VK_END:       result := VKEY_END;
    VK_HOME:      result := VKEY_HOME;
    VK_LEFT:      result := VKEY_LEFT;
    VK_UP:        result := VKEY_UP;
    VK_RIGHT:     result := VKEY_RIGHT;
    VK_DOWN:      result := VKEY_DOWN;
    VK_PRIOR:     result := VKEY_PAGEUP;
    VK_NEXT:      result := VKEY_PAGEDOWN;
    VK_SELECT:    result := VKEY_SELECT;
    VK_PRINT:     result := VKEY_PRINT;
    VK_SNAPSHOT:  result := VKEY_SNAPSHOT;
    VK_INSERT:    result := VKEY_INSERT;
    VK_DELETE:    result := VKEY_DELETE;
    VK_HELP:      result := VKEY_HELP;
    VK_NUMPAD0:   result := VKEY_NUMPAD0;
    VK_NUMPAD1:   result := VKEY_NUMPAD1;
    VK_NUMPAD2:   result := VKEY_NUMPAD2;
    VK_NUMPAD3:   result := VKEY_NUMPAD3;
    VK_NUMPAD4:   result := VKEY_NUMPAD4;
    VK_NUMPAD5:   result := VKEY_NUMPAD5;
    VK_NUMPAD6:   result := VKEY_NUMPAD6;
    VK_NUMPAD7:   result := VKEY_NUMPAD7;
    VK_NUMPAD8:   result := VKEY_NUMPAD8;
    VK_NUMPAD9:   result := VKEY_NUMPAD9;
    VK_MULTIPLY:  result := VKEY_MULTIPLY;
    VK_ADD:       result := VKEY_ADD;
    VK_SEPARATOR: result := VKEY_SEPARATOR;
    VK_SUBTRACT:  result := VKEY_SUBTRACT;
    VK_DECIMAL:   result := VKEY_DECIMAL;
    VK_DIVIDE:    result := VKEY_DIVIDE;
    VK_F1:        result := VKEY_F1;
    VK_F2:        result := VKEY_F2;
    VK_F3:        result := VKEY_F3;
    VK_F4:        result := VKEY_F4;
    VK_F5:        result := VKEY_F5;
    VK_F6:        result := VKEY_F6;
    VK_F7:        result := VKEY_F7;
    VK_F8:        result := VKEY_F8;
    VK_F9:        result := VKEY_F9;
    VK_F10:       result := VKEY_F10;
    VK_F11:       result := VKEY_F11;
    VK_F12:       result := VKEY_F12;
    VK_NUMLOCK:   result := VKEY_NUMLOCK;
    VK_SCROLL:    result := VKEY_SCROLL;
    VK_SHIFT:     result := VKEY_SHIFT;
    VK_CONTROL:   result := VKEY_CONTROL;
    VK_MENU:      result := VKEY_ALT;
    //if KeyCode.virt = VKEY_EQUALS    then wkc := 0; //I can't see a windows Virtual key code for this key.
  else
    result := 0;
  end;
end;


{ TPluginKeyHook }

constructor TPluginKeyHook.Create(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName:string);
var
  ID : string;
  fUseKeyHook : boolean;
begin
  fDefaultKeyHookTarget  := GetHandleForKeyHook(aHostName, aHostVersion, aEditorWindow, aConfigFileName, ID, fUseKeyHook);

  if fUseKeyHook then KeyboardHook := TKeyboardHook.Create(fDefaultKeyHookTarget, Hook_KeyDown, Hook_KeyUp);
end;

destructor TPluginKeyHook.Destroy;
begin
  if assigned(KeyboardHook) then KeyboardHook.Free;
  inherited;
end;

// procedure RefreshKeyHookTarget;
//
// The keyboard hook is a global object and shared between all plugin instances. This method
// should be called regularly to ensure the keyboard hook is linked to the correct (active) plugin GUI.
procedure TPluginKeyHook.RefreshKeyHookTarget;
var
  h:hwnd;
begin
  if not assigned(KeyboardHook) then exit;

  //Check if hotkey target window is in focus....
  h := GetForegroundWindow;
  while (h <> 0) and (h <> DefaultKeyHookTarget) do h := GetParent(h);
  if h = DefaultKeyHookTarget
    then UpdateKeyboardHook;


end;


procedure TPluginKeyHook.UpdateKeyboardHook;
begin
  if not assigned(KeyboardHook) then exit;

  KeyboardHook.UpdateValues(DefaultKeyHookTarget, Hook_KeyDown, Hook_KeyUp);
end;


function TPluginKeyHook.Hook_KeyDown(VirtualKeyCode: word): boolean;
var
  KeyCode: VstKeyCode;
begin
  if (VirtualKeyCode = VK_Shift) or (VirtualKeyCode = VK_MENU) or(VirtualKeyCode = VK_CONTROL) then exit(true);

  KeyCode.character := MapWindowsVirtualKeyCodeToVSTAscii(VirtualKeyCode);
  KeyCode.virt      := MapWindowsVirtualKeyCodeToVstVirtualKeyCode(VirtualKeyCode);

  KeyCode.modifier  := 0;

  if GetKeyState(VK_SHIFT) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_SHIFT;

  if GetKeyState(VK_MENU) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_ALTERNATE;

  if GetKeyState(VK_CONTROL) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_CONTROL;


  if assigned(OnKeyDown)
    then result := OnKeyDown(KeyCode)
    else result := false;

end;

function TPluginKeyHook.Hook_KeyUp(VirtualKeyCode: word): boolean;
var
  KeyCode: VstKeyCode;
begin
  if (VirtualKeyCode = VK_Shift) or (VirtualKeyCode = VK_MENU) or(VirtualKeyCode = VK_CONTROL) then exit(true);

  KeyCode.character := MapWindowsVirtualKeyCodeToVSTAscii(VirtualKeyCode);
  KeyCode.virt      := MapWindowsVirtualKeyCodeToVstVirtualKeyCode(VirtualKeyCode);

  KeyCode.modifier  := 0;

  if GetKeyState(VK_SHIFT) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_SHIFT;

  if GetKeyState(VK_MENU) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_ALTERNATE;

  if GetKeyState(VK_CONTROL) < 0
    then KeyCode.modifier := KeyCode.modifier or MODIFIER_CONTROL;


  if assigned(OnKeyUp)
    then result := OnKeyUp(KeyCode)
    else result := false;

end;



end.
