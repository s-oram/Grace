unit eePluginHotkeys;

interface





uses
  Generics.Collections, DAEffect, DAEffectX;

type
  TCommandEvent = procedure(Sender : TObject; const CommandID : string) of object;

  {$SCOPEDENUMS ON}

  TVstVirtualKey = (
     vkNone = 0,
     vkBACK = VKEY_BACK,
     vkTAB = VKEY_TAB,
     vkCLEAR = VKEY_CLEAR,
     vkRETURN = VKEY_RETURN,
     vkENTER = VKEY_ENTER,
     vkPAUSE = VKEY_PAUSE,
     vkESCAPE = VKEY_ESCAPE,
     vkSPACE = VKEY_SPACE,
     vkNEXT = VKEY_NEXT,
     vkEND = VKEY_END,
     vkHOME = VKEY_HOME,
     vkLEFT = VKEY_LEFT,
     vkUP = VKEY_UP,
     vkRIGHT = VKEY_RIGHT,
     vkDOWN = VKEY_DOWN,
     vkPAGEUP = VKEY_PAGEUP,
     vkPAGEDOWN = VKEY_PAGEDOWN,
     vkSELECT = VKEY_SELECT,
     vkPRINT = VKEY_PRINT,
     vkSNAPSHOT = VKEY_SNAPSHOT,
     vkINSERT = VKEY_INSERT,
     vkDELETE = VKEY_DELETE,
     vkHELP = VKEY_HELP,
     vkNUMPAD0 = VKEY_NUMPAD0,
     vkNUMPAD1 = VKEY_NUMPAD1,
     vkNUMPAD2 = VKEY_NUMPAD2,
     vkNUMPAD3 = VKEY_NUMPAD3,
     vkNUMPAD4 = VKEY_NUMPAD4,
     vkNUMPAD5 = VKEY_NUMPAD5,
     vkNUMPAD6 = VKEY_NUMPAD6,
     vkNUMPAD7 = VKEY_NUMPAD7,
     vkNUMPAD8 = VKEY_NUMPAD8,
     vkNUMPAD9 = VKEY_NUMPAD9,
     vkMULTIPLY = VKEY_MULTIPLY,
     vkADD = VKEY_ADD,
     vkSEPARATOR = VKEY_SEPARATOR,
     vkSUBTRACT = VKEY_SUBTRACT,
     vkDECIMAL = VKEY_DECIMAL,
     vkDIVIDE = VKEY_DIVIDE,
     vkF1 = VKEY_F1,
     vkF2 = VKEY_F2,
     vkF3 = VKEY_F3,
     vkF4 = VKEY_F4,
     vkF5 = VKEY_F5,
     vkF6 = VKEY_F6,
     vkF7 = VKEY_F7,
     vkF8 = VKEY_F8,
     vkF9 = VKEY_F9,
     vkF10 = VKEY_F10,
     vkF11 = VKEY_F11,
     vkF12 = VKEY_F12,
     vkNUMLOCK = VKEY_NUMLOCK,
     vkSCROLL = VKEY_SCROLL,
     vkSHIFT = VKEY_SHIFT,
     vkCONTROL = VKEY_CONTROL,
     vkALT = VKEY_ALT,
     vkEQUALS = VKEY_EQUALS
  );


  // NOTE: KeyChar and VirtualKey could be combined to the one key.
  THotkeyAssignment = record
    KeyChar    : integer; //ASCII charactor.
    VirtualKey : TVstVirtualKey;
    CommandID  : string; //The application specific 'command' ID that is called when the hotkey combo is pressed.
    WithShift  : boolean;
    WithAlt    : boolean;
    WithCtrl   : boolean;
  end;

  THotkeyAssignmentList = TList<THotkeyAssignment>;

  TPluginHotkeys = class
  private
    fOnCommandKeyDown: TCommandEvent;
    fOnCommandKeyUp: TCommandEvent;
  protected
    x : integer;
    AssignmentList : THotkeyAssignmentList;

    procedure CommandKeyDown(const CommandID : string);
    procedure CommandKeyUp(const CommandID : string);

    procedure AddHotkey(const CommandID, Key, WithShift, WithAlt, WithCtrl : string); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromXML(const fn:string);

    procedure AddHotkey(const Key : Ansistring; const VirtualKey : TVstVirtualKey; const WithShift, WithAlt, WithCtrl : boolean; const CommandID : string); overload;

    function KeyDown(var KeyCode: VstKeyCode): boolean;
    function KeyUp(var KeyCode: VstKeyCode): boolean;

    property OnCommandKeyDown : TCommandEvent read fOnCommandKeyDown write fOnCommandKeyDown;
    property OnCommandKeyUp   : TCommandEvent read fOnCommandKeyUp   write fOnCommandKeyUp;
  end;

implementation

uses
  Dialogs,
  NativeXML,
  uAutoFree,
  SysUtils,
  eeFunctions;

function IsValidVstKeyChar(Key : Ansistring):boolean;
var
  x : integer;
begin
  if Key = '' then exit(true);
  if (Length(Key) <> 0) and (Length(Key) <> 1) then exit(false);

  x := Ord(Key[1]);

  if x = Ord('0') then exit(true);
  if x = Ord('1') then exit(true);
  if x = Ord('2') then exit(true);
  if x = Ord('3') then exit(true);
  if x = Ord('4') then exit(true);
  if x = Ord('5') then exit(true);
  if x = Ord('6') then exit(true);
  if x = Ord('7') then exit(true);
  if x = Ord('8') then exit(true);
  if x = Ord('9') then exit(true);
  if x = Ord('a') then exit(true);
  if x = Ord('b') then exit(true);
  if x = Ord('c') then exit(true);
  if x = Ord('d') then exit(true);
  if x = Ord('e') then exit(true);
  if x = Ord('f') then exit(true);
  if x = Ord('g') then exit(true);
  if x = Ord('h') then exit(true);
  if x = Ord('i') then exit(true);
  if x = Ord('j') then exit(true);
  if x = Ord('k') then exit(true);
  if x = Ord('l') then exit(true);
  if x = Ord('m') then exit(true);
  if x = Ord('n') then exit(true);
  if x = Ord('o') then exit(true);
  if x = Ord('p') then exit(true);
  if x = Ord('q') then exit(true);
  if x = Ord('r') then exit(true);
  if x = Ord('s') then exit(true);
  if x = Ord('t') then exit(true);
  if x = Ord('u') then exit(true);
  if x = Ord('v') then exit(true);
  if x = Ord('w') then exit(true);
  if x = Ord('x') then exit(true);
  if x = Ord('y') then exit(true);
  if x = Ord('z') then exit(true);


  // if we've made it this far, the key is not valid.
  result := false;

end;

procedure StringToVstKey(const Text : string; out KeyChar : integer; out VstVirtualKey : TVstVirtualKey);
begin
  KeyChar := 0;
  VstVirtualKey := TVstVirtualKey.vkNone;


  if SameText(Text, '0') then
  begin
    KeyChar := Ord('0');
    exit;
  end;

  if SameText(Text, '1') then
  begin
    KeyChar := Ord('1');
    exit;
  end;

  if SameText(Text, '2') then
  begin
    KeyChar := Ord('2');
    exit;
  end;

  if SameText(Text, '3') then
  begin
    KeyChar := Ord('3');
    exit;
  end;

  if SameText(Text, '4') then
  begin
    KeyChar := Ord('4');
    exit;
  end;

  if SameText(Text, '5') then
  begin
    KeyChar := Ord('5');
    exit;
  end;

  if SameText(Text, '6') then
  begin
    KeyChar := Ord('6');
    exit;
  end;

  if SameText(Text, '7') then
  begin
    KeyChar := Ord('7');
    exit;
  end;

  if SameText(Text, '8') then
  begin
    KeyChar := Ord('8');
    exit;
  end;

  if SameText(Text, '9') then
  begin
    KeyChar := Ord('9');
    exit;
  end;

  if SameText(Text, 'a') then
  begin
    KeyChar := Ord('a');
    exit;
  end;

  if SameText(Text, 'b') then
  begin
    KeyChar := Ord('b');
    exit;
  end;

  if SameText(Text, 'c') then
  begin
    KeyChar := Ord('c');
    exit;
  end;

  if SameText(Text, 'd') then
  begin
    KeyChar := Ord('d');
    exit;
  end;

  if SameText(Text, 'e') then
  begin
    KeyChar := Ord('e');
    exit;
  end;

  if SameText(Text, 'f') then
  begin
    KeyChar := Ord('f');
    exit;
  end;

  if SameText(Text, 'g') then
  begin
    KeyChar := Ord('g');
    exit;
  end;

  if SameText(Text, 'h') then
  begin
    KeyChar := Ord('h');
    exit;
  end;

  if SameText(Text, 'i') then
  begin
    KeyChar := Ord('i');
    exit;
  end;

  if SameText(Text, 'j') then
  begin
    KeyChar := Ord('j');
    exit;
  end;

  if SameText(Text, 'k') then
  begin
    KeyChar := Ord('k');
    exit;
  end;

  if SameText(Text, 'l') then
  begin
    KeyChar := Ord('l');
    exit;
  end;

  if SameText(Text, 'm') then
  begin
    KeyChar := Ord('m');
    exit;
  end;

  if SameText(Text, 'n') then
  begin
    KeyChar := Ord('n');
    exit;
  end;

  if SameText(Text, 'o') then
  begin
    KeyChar := Ord('o');
    exit;
  end;

  if SameText(Text, 'p') then
  begin
    KeyChar := Ord('p');
    exit;
  end;

  if SameText(Text, 'q') then
  begin
    KeyChar := Ord('q');
    exit;
  end;

  if SameText(Text, 'r') then
  begin
    KeyChar := Ord('r');
    exit;
  end;

  if SameText(Text, 's') then
  begin
    KeyChar := Ord('s');
    exit;
  end;

  if SameText(Text, 't') then
  begin
    KeyChar := Ord('t');
    exit;
  end;

  if SameText(Text, 'u') then
  begin
    KeyChar := Ord('u');
    exit;
  end;

  if SameText(Text, 'v') then
  begin
    KeyChar := Ord('v');
    exit;
  end;

  if SameText(Text, 'w') then
  begin
    KeyChar := Ord('w');
    exit;
  end;

  if SameText(Text, 'x') then
  begin
    KeyChar := Ord('x');
    exit;
  end;

  if SameText(Text, 'y') then
  begin
    KeyChar := Ord('y');
    exit;
  end;

  if SameText(Text, 'z') then
  begin
    KeyChar := Ord('z');
    exit;
  end;

  if SameText(Text, 'None') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNone;
    exit;
  end;

  if SameText(Text, 'BACK') then
  begin
    VstVirtualKey := TVstVirtualKey.vkBACK;
    exit;
  end;

  if SameText(Text, 'TAB') then
  begin
    VstVirtualKey := TVstVirtualKey.vkTAB;
    exit;
  end;

  if SameText(Text, 'CLEAR') then
  begin
    VstVirtualKey := TVstVirtualKey.vkCLEAR;
    exit;
  end;

  if SameText(Text, 'RETURN') then
  begin
    VstVirtualKey := TVstVirtualKey.vkRETURN;
    exit;
  end;

  if SameText(Text, 'ENTER') then
  begin
    VstVirtualKey := TVstVirtualKey.vkRETURN;
    exit;
  end;

  if SameText(Text, 'PAUSE') then
  begin
    VstVirtualKey := TVstVirtualKey.vkPAUSE;
    exit;
  end;

  if SameText(Text, 'ESCAPE') then
  begin
    VstVirtualKey := TVstVirtualKey.vkESCAPE;
    exit;
  end;

  if SameText(Text, 'SPACE') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSPACE;
    exit;
  end;

  if SameText(Text, 'NEXT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNEXT;
    exit;
  end;

  if SameText(Text, 'END') then
  begin
    VstVirtualKey := TVstVirtualKey.vkEND;
    exit;
  end;

  if SameText(Text, 'HOME') then
  begin
    VstVirtualKey := TVstVirtualKey.vkHOME;
    exit;
  end;

  if SameText(Text, 'LEFT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkLEFT;
    exit;
  end;

  if SameText(Text, 'UP') then
  begin
    VstVirtualKey := TVstVirtualKey.vkUP;
    exit;
  end;

  if SameText(Text, 'RIGHT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkRIGHT;
    exit;
  end;

  if SameText(Text, 'DOWN') then
  begin
    VstVirtualKey := TVstVirtualKey.vkDOWN;
    exit;
  end;

  if SameText(Text, 'PAGEUP') then
  begin
    VstVirtualKey := TVstVirtualKey.vkPAGEUP;
    exit;
  end;

  if SameText(Text, 'PAGEDOWN') then
  begin
    VstVirtualKey := TVstVirtualKey.vkPAGEDOWN;
    exit;
  end;

  if SameText(Text, 'SELECT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSELECT;
    exit;
  end;

  if SameText(Text, 'PRINT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkPRINT;
    exit;
  end;

  if SameText(Text, 'SNAPSHOT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSNAPSHOT;
    exit;
  end;

  if SameText(Text, 'INSERT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkINSERT;
    exit;
  end;

  if SameText(Text, 'DELETE') then
  begin
    VstVirtualKey := TVstVirtualKey.vkDELETE;
    exit;
  end;

  if SameText(Text, 'HELP') then
  begin
    VstVirtualKey := TVstVirtualKey.vkHELP;
    exit;
  end;

  if SameText(Text, 'NUMPAD0') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD0;
    exit;
  end;

  if SameText(Text, 'NUMPAD1') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD1;
    exit;
  end;

  if SameText(Text, 'NUMPAD2') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD2;
    exit;
  end;

  if SameText(Text, 'NUMPAD3') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD3;
    exit;
  end;

  if SameText(Text, 'NUMPAD4') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD4;
    exit;
  end;

  if SameText(Text, 'NUMPAD5') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD5;
    exit;
  end;

  if SameText(Text, 'NUMPAD6') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD6;
    exit;
  end;

  if SameText(Text, 'NUMPAD7') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD7;
    exit;
  end;

  if SameText(Text, 'NUMPAD8') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD8;
    exit;
  end;

  if SameText(Text, 'NUMPAD9') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMPAD9;
    exit;
  end;

  if SameText(Text, 'MULTIPLY') then
  begin
    VstVirtualKey := TVstVirtualKey.vkMULTIPLY;
    exit;
  end;

  if SameText(Text, 'ADD') then
  begin
    VstVirtualKey := TVstVirtualKey.vkADD;
    exit;
  end;

  if SameText(Text, 'SEPARATOR') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSEPARATOR;
    exit;
  end;

  if SameText(Text, 'SUBTRACT') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSUBTRACT;
    exit;
  end;

  if SameText(Text, 'DECIMAL') then
  begin
    VstVirtualKey := TVstVirtualKey.vkDECIMAL;
    exit;
  end;

  if SameText(Text, 'DIVIDE') then
  begin
    VstVirtualKey := TVstVirtualKey.vkDIVIDE;
    exit;
  end;

  if SameText(Text, 'F1') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF1;
    exit;
  end;

  if SameText(Text, 'F2') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF2;
    exit;
  end;

  if SameText(Text, 'F3') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF3;
    exit;
  end;

  if SameText(Text, 'F4') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF4;
    exit;
  end;

  if SameText(Text, 'F5') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF5;
    exit;
  end;

  if SameText(Text, 'F6') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF6;
    exit;
  end;

  if SameText(Text, 'F7') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF7;
    exit;
  end;

  if SameText(Text, 'F8') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF8;
    exit;
  end;

  if SameText(Text, 'F9') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF9;
    exit;
  end;

  if SameText(Text, 'F10') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF10;
    exit;
  end;

  if SameText(Text, 'F11') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF11;
    exit;
  end;

  if SameText(Text, 'F12') then
  begin
    VstVirtualKey := TVstVirtualKey.vkF12;
    exit;
  end;

  if SameText(Text, 'NUMLOCK') then
  begin
    VstVirtualKey := TVstVirtualKey.vkNUMLOCK;
    exit;
  end;

  if SameText(Text, 'SCROLL') then
  begin
    VstVirtualKey := TVstVirtualKey.vkSCROLL;
    exit;
  end;

  // if we've made it this far, there is something wrong.
  raise Exception.Create('Key ID (' + Text + ') is not recognised.');

end;


{ TPluginHotkeys }

constructor TPluginHotkeys.Create;
begin
  AssignmentList := THotkeyAssignmentList.Create;
end;

destructor TPluginHotkeys.Destroy;
begin
  AssignmentList.Free;
  inherited;
end;

procedure TPluginHotkeys.CommandKeyDown(const CommandID: string);
begin
  if assigned(OnCommandKeyDown) then OnCommandKeyDown(self, CommandID);
end;

procedure TPluginHotkeys.CommandKeyUp(const CommandID: string);
begin
  if assigned(OnCommandKeyUp) then OnCommandKeyUp(self, CommandID);
end;

procedure TPluginHotkeys.AddHotkey(const Key: Ansistring; const VirtualKey: TVstVirtualKey; const WithShift, WithAlt, WithCtrl : boolean; const CommandID: string);
var
  hka : THotkeyAssignment;
  msg : string;
begin
  msg := 'Key (' + String(Key) + ') is not a valid VST short cut option.';
  if IsValidVstKeyChar(Key) = false then raise Exception.Create(msg);

  if (Key = '') and (VirtualKey = TVstVirtualKey.vkNone) then raise Exception.Create('No key code specified for hot key.');

  if Key = ''
    then hka.KeyChar := 0
    else hka.KeyChar := Ord(Key[1]);
  hka.VirtualKey := VirtualKey;
  hka.WithShift  := WithShift;
  hka.WithAlt    := WithAlt;
  hka.WithCtrl   := WithCtrl;
  hka.CommandID  := CommandID;

  AssignmentList.Add(hka);
end;

procedure TPluginHotkeys.AddHotkey(const CommandID, Key, WithShift, WithAlt, WithCtrl: string);
var
  aKeyChar : integer;
  aVirtualKey : TVstVirtualKey;
  hka : THotkeyAssignment;
begin
  // This AddHotKey() method is primarily intended for internal usage when loading
  // hotkey configuration info from a file.

  if (CommandID = '') or (Key = '') then raise exception.Create('No command or key specified.');

  StringToVstKey(Key, aKeyChar, aVirtualKey);


  hka.KeyChar    := aKeyChar;
  hka.VirtualKey := aVirtualKey;
  hka.CommandID  := CommandID;
  hka.WithShift  := DataIO_StrToBool(WithShift, false);
  hka.WithAlt    := DataIO_StrToBool(WithAlt,   false);
  hka.WithCtrl   := DataIO_StrToBool(WithCtrl,  false);

  AssignmentList.Add(hka);
end;



function TPluginHotkeys.KeyDown(var KeyCode: VstKeyCode): boolean;
var
  c1: Integer;
  hka : THotkeyAssignment;
  IsShift : boolean;
  IsAlt   : boolean;
  IsCtrl  : boolean;
begin
  IsShift := (KeyCode.modifier and MODIFIER_SHIFT) > 0;
  IsAlt   := (KeyCode.modifier and MODIFIER_ALTERNATE) > 0;
  IsCtrl  := (KeyCode.modifier and MODIFIER_CONTROL) > 0;

  for c1 := 0 to AssignmentList.Count-1 do
  begin
    hka := AssignmentList[c1];
    if (KeyCode.character = hka.KeyChar) and (SmallInt(KeyCode.virt) = ord(hka.VirtualKey)) and (IsShift = hka.WithShift) and (IsAlt = hka.WithAlt) and (IsCtrl = hka.WithCtrl) then
    begin
      CommandKeyDown(hka.CommandID);
      exit(true);
    end;
  end;

  //if we've made it this far, a matching hotkey assignment hasn't been found.
  result := false;
end;

function TPluginHotkeys.KeyUp(var KeyCode: VstKeyCode): boolean;
var
  c1: Integer;
  hka : THotkeyAssignment;
  IsShift : boolean;
  IsAlt   : boolean;
  IsCtrl  : boolean;
begin
  IsShift := (KeyCode.modifier and MODIFIER_SHIFT) > 0;
  IsAlt   := (KeyCode.modifier and MODIFIER_ALTERNATE) > 0;
  IsCtrl  := (KeyCode.modifier and MODIFIER_CONTROL) > 0;

  for c1 := 0 to AssignmentList.Count-1 do
  begin
    hka := AssignmentList[c1];
    if (KeyCode.character = hka.KeyChar) and (SmallInt(KeyCode.virt) = ord(hka.VirtualKey)) and (IsShift = hka.WithShift) and (IsAlt = hka.WithAlt) and (IsCtrl = hka.WithCtrl) then
    begin
      CommandKeyUp(hka.CommandID);
      exit(true);
    end;
  end;

  //if we've made it this far, a matching hotkey assignment hasn't been found.
  result := false;
end;

procedure TPluginHotkeys.LoadFromXML(const fn: string);
type
  THotkeyInfo = record
    Command   : string;
    Key       : string;
    WithShift : string;
    WithAlt   : string;
    WithCtrl  : string;
  end;
var
  xml:TNativeXml;
  KeyCommandNodes : TsdNodeList;
  aNode : TXmlNode;
  c1: Integer;
  hk : THotkeyInfo;
begin
  // load the new data...
  xml := TNativeXml.Create(nil);
  AutoFree(@xml);

  KeyCommandNodes := TsdNodeList.Create;
  AutoFree(@KeyCommandNodes);

  xml.LoadFromFile(fn);

  if assigned(xml.Root) then
  begin
    xml.Root.FindNodes('KeyCommand', KeyCommandNodes);

    for c1 := 0 to KeyCommandNodes.Count-1 do
    begin
      hk.Command   := '';
      hk.Key       := '';
      hk.WithShift := '';
      hk.WithAlt   := '';
      hk.WithCtrl  := '';

      aNode := KeyCommandNodes[c1].FindNode('Command');
      if assigned(aNode) then hk.Command := aNode.ValueUnicode;

      aNode := KeyCommandNodes[c1].FindNode('Key');
      if assigned(aNode) then hk.Key := aNode.ValueUnicode;

      aNode := KeyCommandNodes[c1].FindNode('WithShift');
      if assigned(aNode) then hk.WithShift := aNode.ValueUnicode;

      aNode := KeyCommandNodes[c1].FindNode('WithAlt');
      if assigned(aNode) then hk.WithAlt := aNode.ValueUnicode;

      aNode := KeyCommandNodes[c1].FindNode('WithCtrl');
      if assigned(aNode) then hk.WithCtrl := aNode.ValueUnicode;

      AddHotKey(hk.Command, hk.Key, hk.WithShift, hk.WithAlt, hk.WithCtrl);
    end;
  end;


end;

end.
