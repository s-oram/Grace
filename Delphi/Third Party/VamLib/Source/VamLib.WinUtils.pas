unit VamLib.WinUtils;

interface


function IsCtrlKeyDown  : Boolean;
function IsShiftKeyDown : Boolean;
function IsAltKeyDown   : Boolean;

procedure SendDebugMesssage(const Msg: String);

implementation

uses
  VamLib.DebugString,
  WinApi.Windows;

function IsCtrlKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Control] And 128) <> 0) ;
end;

function IsShiftKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Shift] and 128) <> 0) ;
end;

function IsAltKeyDown : Boolean;
var
   State : TKeyboardState;
begin
   GetKeyboardState(State) ;
   Result := ((State[vk_Menu] and 128) <> 0) ;
end;


procedure SendDebugMesssage(const Msg: String);
begin
  //OutputDebugString(PChar(Msg))
  DbWin__OutputDebugStringU(PWideChar(Msg));
end;

end.
