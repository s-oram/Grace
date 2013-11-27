unit eeLogging;

interface

{$INCLUDE Defines.inc}

uses
  SysUtils;

procedure LogException(const E:Exception); overload;
procedure LogException(const E:Exception; const Title : string); overload;

implementation

uses
  MadExcept,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeTypes;


procedure LogException(const E:Exception);
begin
  {$IFDEF Logging}
  LogMain.LogException(E);
  {$ENDIF}
end;


procedure LogException(const E:Exception; const Title : string);
begin
  {$IFDEF Logging}
  LogMain.LogException(E, Title);
  {$ENDIF}
end;

end.
