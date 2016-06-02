unit eeLogging;

interface

{$INCLUDE Defines.inc}

uses
  SysUtils;

// Code using these procedures should call directly in VamLib.Logging.pas.
procedure LogError(const ErrorMessage : string); deprecated;
procedure LogException(const E:Exception); overload; deprecated;
procedure LogException(const E:Exception; const Title : string); overload; deprecated;

implementation

uses
  {$IFDEF Logging}VamLib.Logging,{$ENDIF}
  eeTypes;

procedure LogError(const ErrorMessage : string);
begin
  {$IFDEF Logging}
  Log.Main.LogError(ErrorMessage);
  {$ENDIF}
end;


procedure LogException(const E:Exception);
begin
  {$IFDEF Logging}
  // TODO:HIGH reimplement this
  //Log.Main.LogException(E);
  {$ENDIF}
end;


procedure LogException(const E:Exception; const Title : string);
begin
  {$IFDEF Logging}
  // TODO:HIGH reimplement this
  //Log.Main.LogException(E, Title);
  {$ENDIF}
end;

end.
