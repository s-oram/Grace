unit SmartInspect.SectionTimer;


{
  TODO:HIGH Test this code!!

}

interface

uses
  Windows,
  SmartInspect;

type

  ISmartInspectSectionTimer = interface
    ['{0E629E5C-F8B8-43BA-9C4E-9434EBB514EA}']
    procedure Start(const aSectionName : string); overload;
    procedure Start(const aSectionTag : integer); overload;
    procedure Stop;
  end;


  TSmartInspectSectionTimer = class(TInterfacedObject, ISmartInspectSectionTimer)
  private
    FLevel : TSiLevel;
    FMethodName: UnicodeString;
    FSession: TSiSession;
    StartTicks : DWord;
    EndTicks   : DWord;
    SectionName : string;
    IsSectionTimerActive : boolean;
  public
    constructor Create(const ASession: TSiSession; const AMethodName: UnicodeString);
    destructor Destroy; override;
    procedure Start(const aSectionName : string); overload;
    procedure Start(const aSectionTag : integer); overload;
    procedure Stop;
  end;

implementation

uses
  SysUtils,
  VamLib.Utils;

{ TSmartInspectSectionTimer }

constructor TSmartInspectSectionTimer.Create(const ASession: TSiSession; const AMethodName: UnicodeString);
begin
  IsSectionTimerActive := false;
  FSession := ASession;
  FMethodName := AMethodName;

  FSession.EnterMethod(AMethodName);
end;

destructor TSmartInspectSectionTimer.Destroy;
begin
  if IsSectionTimerActive then Stop;
  FSession.LeaveMethod(FMethodName);
  inherited;
end;

procedure TSmartInspectSectionTimer.Start(const aSectionName: string);
begin
  if IsSectionTimerActive then Stop;

  IsSectionTimerActive := true;
  SectionName := aSectionName;
  StartTicks := GetTickCount;
end;

procedure TSmartInspectSectionTimer.Start(const aSectionTag: integer);
begin
  if IsSectionTimerActive then Stop;

  IsSectionTimerActive := true;
  SectionName := '#' + IntToStr(aSectionTag);
  StartTicks := GetTickCount;
end;

procedure TSmartInspectSectionTimer.Stop;
var
  SectionTime : DWord;
begin
  if IsSectionTimerActive then
  begin
    EndTicks := GetTickCount;
    IsSectionTimerActive := false;
    SectionTime := EndTicks - StartTicks;

    FSession.LogMessage('Section Time: ' + SectionName + kChar.Space + IntToStr(EndTicks-StartTicks) + 'ms');
  end;
end;

end.
