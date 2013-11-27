{
  THotKeySupport adds support for hotkeys in VST plugins.

  Hotkey support in VST plugins is problematic because not all hosts send keystrokes to the plugin GUI.
  For those plugins that don't, THotkeySupport uses a global keyhook.

  THotkeySupport will capture key strokes when the attached GUI/form is in focus.

  By Shannon Oram

  Updated: 12th December 2009




  NOTE: June 11th 2013. This unit does too much. I'm going to phase it out.
  eeHotkeySupport.pas will be replaced by two units.
  - eePluginHotkeys.pas
  - eePluginKeyHook.pas

}


unit eeHotKeySupport;

interface

{$INCLUDE Defines.inc}

uses
  {$IFDEF VER230}
    Vcl.Forms,
  {$ELSE}
    Forms,
  {$ENDIF}
  Windows,
  Contnrs,
  Classes,
  eeKeyboardHook,
  eeKeyboardHookConfig;

type
  THotKeyPressed = procedure (FunctionID:integer) of object;

  THotKey = class
  private
    fShift: boolean;
    fCtrl: boolean;
    fAlt: boolean;
    fKey: word;
    fFunctionID: integer;
  public
    constructor Create;
      //IsEqual - Use to check if the HotKey uses the same
      //combination of Key+Modifiers.
    function IsEqual(aHotKey:THotKey):boolean;

    property Shift:boolean read fShift write fShift;
    property Alt:boolean read fAlt write fAlt;
    property Ctrl:boolean read fCtrl write fCtrl;
    property Key:word read fKey write fKey;  //Use windows virtual key code constants.

      //FunctionID is a key to identify what function this
      //HotKey is assigned to. Multiple HotKeys can have the
      //same FunctionID, ie, they do the same thing.
    property FunctionID:integer read fFunctionID write fFunctionID;
  end;


  THotkeySupport = class
  private
    fOnHotKeyPressed: THotKeyPressed;
    fShiftState: TShiftState;
    fIsKeyboardHookEnabled : boolean;
    fIsConfigOverrideActive: boolean;
    fOverrideHookLevel: integer;
    fHotkeyConfigID: string;
    fHotkeyHandleFound: boolean;
    function GetHotKey(Index: integer): THotKey;
    function GetHotkeyCount: integer;
    procedure SetHotKey(Index: integer; const Value: THotKey);
    procedure SetIsConfigOverrideActive(const Value: boolean);
    procedure SetOverrideHookLevel(const Value: integer);
  protected
    HotKeyList:TObjectList;
    KeyboardHook:TKeyboardHook;
    fDefaultKeyHookTarget:HWND;

    HostName     : string;  //as provided by the VST.
    HostVersion  : integer; //as provided by the VST.
    EditorWindow : hwnd;    //Editor window handle.

    procedure UpdateModifierKeyState;
    property ShiftState:TShiftState read fShiftState write fShiftState;

    procedure UpdateKeyboardHook;

    //These methods are event handlers for the keyboard hook class.
    function Hook_KeyDown(VirtualKeyCode:word):boolean;
    function Hook_KeyUp(VirtualKeyCode:word):boolean;
  public
    constructor Create(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName:string);
    destructor Destroy; override;

    procedure RefreshKeyHookTarget;

    procedure Add(aHotKey:THotKey);  //owns the hot key.
    procedure Delete(Index:integer);
    function IsHotKeyComboAssigned(aHotKey:THotKey):integer;

    //Used for processing VST-spec hotkeys...
    function DoProcessKeyDown(VirtualKeyCode:word):boolean;
    function DoProcessKeyUp(VirtualKeyCode:word):boolean;

    property HotKeyCount:integer read GetHotkeyCount;
    property HotKey[Index:integer]:THotKey read GetHotKey write SetHotKey; default;

    //Disables the keyboard hook, still allows VST keypress events to trigger hotkeys.
    property IsKeyboardHookEnabled  : boolean read fIsKeyboardHookEnabled  write fIsKeyboardHookEnabled;
    property IsConfigOverrideActive : boolean read fIsConfigOverrideActive write SetIsConfigOverrideActive;
    property OverrideHookLevel      : integer read fOverrideHookLevel      write SetOverrideHookLevel;

    // Read HotkeyConfigID to find out which 'config' was loaded for the current host environment. Useful for debugging hotkey config errors.
    property HotkeyConfigID        : string  read fHotkeyConfigID;
    property HotkeyHandleFound     : boolean read fHotkeyHandleFound;

    property DefaultKeyHookTarget  : HWND    read fDefaultKeyHookTarget; 

    // Handle OnHotKeyPress events to respond to hot key presses
    property OnHotKeyPress:THotKeyPressed read fOnHotKeyPressed write fOnHotKeyPressed;
  end;

implementation

uses
  {$IFDEF Logging} siAuto, {$ENDIF}
  SysUtils, uAutoFree;


function GetParentHandle(aHandle:hwnd; Level:integer):hwnd;
var
  c1:integer;
  h:hwnd;
begin
  h := aHandle;
  for c1 := 0 to Level - 1 do
  begin
    h := GetParent(h);
  end;
  result := h;
end;

{ THotKey }

constructor THotKey.Create;
begin
  Shift := false;
  Alt   := false;
  Ctrl  := false;

  Key   := 0;

  FunctionID := -1;
end;

function THotKey.IsEqual(aHotKey: THotKey): boolean;
begin
  if  (Self.Key   = aHotKey.Key)
  and (Self.Shift = aHotKey.Shift)
  and (Self.Alt   = aHotKey.Alt)
  and (Self.Ctrl  = aHotKey.Ctrl)
    then result := true
    else result := false;

end;

{ THotkeySupport }

constructor THotkeySupport.Create(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName:string);
var
  ID : string;
  fUseKeyHook : boolean;
begin
  HostName     := aHostName;
  HostVersion  := aHostVersion;
  EditorWindow := aEditorWindow;

  HotKeyList := TObjectList.create;
  HotKeyList.OwnsObjects := true;

  fIsConfigOverrideActive := false;
  fOverrideHookLevel      := 0;
  fIsKeyboardHookEnabled  := true;

  fDefaultKeyHookTarget  := GetHandleForKeyHook(aHostName, aHostVersion, aEditorWindow, aConfigFileName, ID, fUseKeyHook);
  fHotkeyConfigID        := ID;
  fIsKeyboardHookEnabled := fUseKeyHook; 

  if DefaultKeyHookTarget <> 0
    then fHotkeyHandleFound := true
    else fHotkeyHandleFound := false;

  if fUseKeyHook then KeyboardHook := TKeyboardHook.Create(DefaultKeyHookTarget, Hook_KeyDown, Hook_KeyUp);
end;

destructor THotkeySupport.Destroy;
begin
  if assigned(KeyboardHook) then KeyboardHook.Free;
  HotKeyList.Free;

  inherited;
end;

procedure THotkeySupport.UpdateKeyboardHook;
var
  OverrideHandle : hwnd;
begin

  if assigned(KeyboardHook) then
  begin
    if IsConfigOverrideActive = false then
    begin
      KeyboardHook.UpdateValues(DefaultKeyHookTarget, Hook_KeyDown, Hook_KeyUp);
    end else
    begin
      OverrideHandle := GetParentHandle(EditorWindow, OverrideHookLevel);
      KeyboardHook.UpdateValues(OverrideHandle, Hook_KeyDown, Hook_KeyUp);
    end;
  end;
end;

procedure THotkeySupport.Add(aHotKey: THotKey);
begin
  HotKeyList.Add(aHotkey);
end;


procedure THotkeySupport.Delete(Index: integer);
begin
  HotKeyList.Delete(Index);
end;


function THotkeySupport.GetHotKey(Index: integer): THotKey;
begin
  result := HotKeyList[Index] as THotKey;
end;

function THotkeySupport.GetHotkeyCount: integer;
begin
  result := HotKeyList.Count;
end;

function THotkeySupport.IsHotKeyComboAssigned(aHotKey: THotKey): integer;
var
  c1:integer;
begin
  //First set the result to say that this HotKey isn't in the list.
  result := -1;

  //Then run though the list, check if the HotKey combination is
  //already in the list. If so, return the index of that HotKey.
  for c1 := 0 to HotKeyCount - 1 do
  begin
    if HotKey[c1].IsEqual(aHotKey) then result := c1;
  end;

end;


// procedure RefreshKeyHookTarget;
//
// The keyboard hook is a global object and shared between all plugin instances. This method
// should be called regularly to ensure the keyboard hook is linked to the correct (active) plugin GUI.
procedure THotkeySupport.RefreshKeyHookTarget;
var
  h:hwnd;
  OverrideHandle : hwnd;
begin
  if not assigned(KeyboardHook) then exit;

  //Check if hotkey target window is in focus....
  if IsConfigOverrideActive = false then
  begin
    h := GetForegroundWindow;
    while (h <> 0) and (h <> DefaultKeyHookTarget) do h := GetParent(h);
    if h = DefaultKeyHookTarget then UpdateKeyboardHook;
  end else
  begin
    h := GetForegroundWindow;
    OverrideHandle := GetParentHandle(EditorWindow, OverrideHookLevel);
    while (h <> 0) and (h <> OverrideHandle) do h := GetParent(h);
    if h = OverrideHandle then UpdateKeyboardHook;
  end;

  //NOTE: This method is a little messy, but I'm feeling lazy right now.

  // TODO: This method should also check what the current hotkey hook window is, and
  // only update if it's not the same.
  
end;

procedure THotkeySupport.SetHotKey(Index: integer; const Value: THotKey);
begin
  HotKeyList[Index] := Value;
end;

procedure THotkeySupport.SetIsConfigOverrideActive(const Value: boolean);
begin
  fIsConfigOverrideActive := Value;
  UpdateKeyboardHook; 
end;

procedure THotkeySupport.SetOverrideHookLevel(const Value: integer);
begin
  fOverrideHookLevel := Value;
  UpdateKeyboardHook; 
end;

procedure THotkeySupport.UpdateModifierKeyState;
var
  KeyState:TKeyboardState;
begin

  if GetKeyboardState(KeyState) = true then
  begin
    //Find out if any hot-key modifier keys are pressed.

    if ((KeyState[VK_SHIFT] and 128) <> 0)
      then ShiftState := ShiftState + [ssShift]
      else ShiftState := ShiftState - [ssShift];

    if ((KeyState[VK_CONTROL] and 128) <> 0)
      then ShiftState := ShiftState + [ssCtrl]
      else ShiftState := ShiftState - [ssCtrl];

    if ((KeyState[VK_MENU] and 128) <> 0)
      then ShiftState := ShiftState + [ssAlt]
      else ShiftState := ShiftState - [ssAlt];

  end else
  begin
    //GetKeyboardState wasn't succesful. An error happened
    //somewhere so just reset all modifier keys.
    ShiftState := ShiftState - [ssShift];
    ShiftState := ShiftState - [ssCtrl];
    ShiftState := ShiftState - [ssAlt];
  end;

end;

function THotkeySupport.DoProcessKeyDown(VirtualKeyCode: word): boolean;
var
  x:integer;
  TempHotKey:THotKey;
begin
  {$IFDEF Logging}
  SiMain.LogMessage('KeyDown ' + IntToStr(VirtualKeyCode));
  {$ENDIF}


  TempHotKey := THotKey.Create;
  AutoFree(@TempHotKey);

  // NOTE: DoProcessKeyDown should return false if it doesn't process the Key,
  //  so the key message can be passed on to other appications/controls.
  //result := false;

  if VirtualKeyCode = VK_MENU then
  begin
    //The Alt key was pressed. Always capture this keypress, so
    //as to not let it trigger menu's in other applications.
    result := true;
    exit;
  end;

  if (VirtualKeyCode = VK_SHIFT) or (VirtualKeyCode = VK_CONTROL) then
  begin
    result := false;
    exit;
  end;

  UpdateModifierKeyState;

  TempHotKey.Key   := VirtualKeyCode;
  TempHotKey.Shift := (ssShift in ShiftState);
  TempHotKey.Alt   := (ssAlt   in ShiftState);
  TempHotKey.Ctrl  := (ssCtrl  in ShiftState);

  x := IsHotKeyComboAssigned(TempHotKey);

  if x = -1 then
  begin
    result := false;
    exit;
  end else
  begin
    result := true;
    if assigned(OnHotKeyPress) then OnHotKeyPress(HotKey[x].FunctionID);
  end;

end;


function THotkeySupport.DoProcessKeyUp(VirtualKeyCode: word): boolean;
begin
  result := false;
end;


function THotkeySupport.Hook_KeyDown(VirtualKeyCode: word): boolean;
begin
  if IsKeyboardHookEnabled
    then result := DoProcessKeyDown(VirtualKeyCode)
    else result := false;

end;

function THotkeySupport.Hook_KeyUp(VirtualKeyCode: word): boolean;
begin
  if IsKeyboardHookEnabled
    then result := DoProcessKeyUp(VirtualKeyCode)
    else result := false;
end;











end.
