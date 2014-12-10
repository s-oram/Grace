unit VamLib.SmartInspectEx;

interface

uses
  SysUtils,
  Windows,
  SmartInspect,
  VamLib.Utils;

type
  ISiMethodTimer = interface
  end;

  TSiMethodTimer = class(TInterfacedObject, ISiMethodTimer)
  private
    FLevel      : TSiLevel;
    FMethodName : UnicodeString;
    FSession    : TSiSession;
    FMinTime    : cardinal;
    StartTicks  : DWord;
    EndTicks    : DWord;

  public
    constructor Create(const ALevel: TSiLevel; const ASession: TSiSession; const AMethodName: UnicodeString; const AMinTime : cardinal);
    destructor Destroy; override;
  end;

implementation

{ TSiMethodTimer }

constructor TSiMethodTimer.Create(const ALevel: TSiLevel; const ASession: TSiSession; const AMethodName: UnicodeString; const AMinTime : cardinal);
begin
  FMinTime := AMinTime;
  FSession := ASession;
  FMethodName := AMethodName;
  FLevel := ALevel;
  StartTicks := GetTickCount;
end;

destructor TSiMethodTimer.Destroy;
begin
  EndTicks := GetTickCount;

  if (EndTicks - StartTicks) >= FMinTime
    then FSession.LogMessage('Method Time: ' + FMethodName + kChar.Space + IntToStr(EndTicks-StartTicks) + 'ms');

  inherited;
end;

end.
