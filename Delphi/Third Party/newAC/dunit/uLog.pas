unit uLog;

{ uLog
    v1.0 By Wayne Thursby
    
    This logging unit implements a class to log lines of text to a file
    in a threadsafe manner, flushing the cache afterwards. It is based on
    SULog.pas by ASI/EDI Inc. Though very little of the original code remains,
    it was a very well written and well commented unit. Many of the original
    comments still remain.

  Section: Additions and Improvements
    I used the original logging unit as a stable and well thought out base
    and started adding features to it. The features I have added are as follows.
 
  - Logging is accomplished via global functions that are completely threadsafe.
  - Design has been modernized, <TGUILog> and <TConsoleLog> have been created
    to facilitate usage of a TMemo or similar to display logged information
    in a GUI environment. Screen logging is accomplished through either WriteLn
    in a console or TStrings.Add in a GUI. This includes seperate <LogLevels> for
    file and screen logging.
  - LogLevels can be set programmatically to filter messages logged to either screen
    or file independently
  - Optional timestamps

  Procedure: LogIt
    Many overridden LogIt procedures make recording data easy. It's also simple to use
    the Format version of LogIt to create your own overridden LogIt procedures dealing
    with any datatype.
 
(code)

  //Log "message" with default LogLevel of 0 (0 = always display message to user)
  LogIt('message');

  // Log "message" with LogLevel of 5 (5  = least critical, debug messages)
  LogIt(5, 'message');

  // Log Format('s1: %s s2: %s', [s1, s2]); this is the "Format Version"
  LogIt('s1: %s s2: %s', [s1, s2]);

  // Log Format('Count: %d', i); %d is added if not already in the string)
  LogIt('Count: ', i);

(end)

}

{ Topic: Todo List

  - Set max logfile size and trim, possibly backing up (compressing?) old logs  
}

{$I NewAC.inc}

interface

{$I uLogOpts.inc}

uses
  {
    Section: DCPCrypt
    Usage of Twofish requires DCPCrypt which can be found at
    http://www.cityinthesky.co.uk/files/dcpcrypt2.zip courtesy of David at
    http://www.cityinthesky.co.uk/cryptography.html
  }
{$IFDEF Twofish}
  DCPCrypt2,
  DCPTwofish,
  DCPSHA256,
{$ENDIF Twofish}
  Classes,
  SysUtils,
  Windows;

type

  { Class: TLockLog
    The base class for TGUILog and TConsoleLog. This class provides the 
    common features of both, including encryption and thread safety. This
    class was not designed to be instantiated directly, but this functionality
    has been included though not thoroughly tested. When using TLockLog directly,
    all it does is write to file, this may be desirable in some circumstances..
  }

  TLockLog = class
  private
    FLogLevel,
      FScreenLevel: integer;
    FLogLock: TRTLCriticalSection;
    FFilename: string;
  protected
    { Procedure: LogToFile
        Write a string plus carriage-return+linefeed to a text file.
        File is appended to if it existed, created otherwise.
        Buffer is flushed and file is closed before function returns.
        Exception raised on error.

      - upgraded to Enter/Leave CriticalSection
      - upgraded to optionally encode text using Twofish

      Parameters:

        sLogText - string; the string to write
        sFileName - string; the name of the file to write to

      See Also:

        <TGUILog.LogText>
        <TConsoleLog.LogText>
      
    }
    procedure LogToFile(const sLogText: string; sFileName: string);
  public
    Timestamp: Boolean;
    { Constructor: Create
      Initialize the CriticalSection, $IFDEF DEBUG Erases FILENAME_LOG if it exists. }
    constructor Create;
    { Destructor: Destroy; 
      Removes the CriticalSection. }
    destructor Destroy; override;
    { Function: LogText
      Override this function in any custom implementations. }
    function LogText(const sLogText: string; iLogLevel: integer = 0): string;
      virtual;
    { Function: DeleteLogFile; 
      Does what it says. }
    procedure DeleteLogfile;
    { Property: Filename
      If blank, defaults to EXE name + .log. }
    property Filename: string read FFilename write FFilename;
    { Property: LogLevel
      0=always display, 5= debug }
    property LogLevel: integer read FLogLevel write FLogLevel;
    { Property: ScreenLevel
      The level of messages to display visually. }
    property ScreenLevel: integer read FScreenLevel write FScreenLevel;
  end;

  { Class: TConsoleLog
    A descendent of TLockLog designed for use in a console application. This class
    implements output via WriteLn which has its own LogLevel.
  }

  TConsoleLog = class(TLockLog)
  public
    { Function: LogText
      Write a line to the logfile and writes it to console. }
    function LogText(const sLogText: string; iLogLevel: integer = 0): string;
      override;
  end;

  { Class: TGUILog
    A descendent of TLockLog designed for use in GUI applications. This class
    implements output to an assigned TStrings object for easy message display in GUI
    applications.
  }

  TGUILog = class(TLockLog)
    FLines: TStrings;
  public
    { Function: LogText
      Write a line to the logfile and adds it to FLines. }
    function LogText(const sLogText: string; iLogLevel: integer = 0): string;
      override;
    { Property LogLines
      TStrings object used to display messages, after writing to file of course. }
    property LogLines: TStrings read FLines write FLines;
  end;

{ Section: Utility Functions
    These functions are provided to try and make this unit as accessible as possible.
}

{ Procedure: WriteToFile
  The heart of the unit, writes a single line to the  file specified in sFilename, or 
  FFilename if sFilename is blank. }
procedure WriteToFile(sLine: string; sFileName: string = '');

{ Procedure: LogIt
    Call LogIt with the default LogLevel of 0 }
procedure LogIt(const sLogText: string); overload;
{ Procedure: LogIt
    Call LogIt with Format }
procedure LogIt(const sLogText: string; const Args: array of const); overload;
{ Procedure: LogIt
  Call Logit with Format(sLogText, [i]) adding '%d' if not found in sLogText }
procedure LogIt(sLogText: string; i: integer); overload;
{ Procedure: LogIt
  Call Logit with Format(sLogText, [d]) except with a double }
procedure LogIt(sLogText: string; d: double); overload;
{ Procedure: LogIt
  Call Logit a specified LogLevel }
procedure LogIt(const iLogLevel: integer; const sLogText: string); overload;
{ Procedire: LogIt
Call LogIt with LogLevel and Format }
procedure LogIt(const iLogLevel: integer; const sLogText: string; const Args:
  array of const); overload;
{ Procedure: LogIt
  Calls Logit with Format(sLogText, [i]) }
procedure LogIt(const iLogLevel: integer; sLogText: string; i: integer);
overload;

var
  IsConsole: Boolean;
  { Global Variable: CurrentLog
    Points to the single allowed instance of a TLockLog descendent. }
  CurrentLog: TLockLog;
  IsInitialized: Boolean;

implementation

{ TLockLog }

constructor TLockLog.Create;
begin
  if Assigned(CurrentLog) then
    raise Exception.Create('Only one log at a time!');
  if FFilename = '' then
    FFilename := ChangeFileExt(ParamStr(0), '.log');
  InitializeCriticalSection(FLogLock);
  {$IFDEF DELPHI16_UP}FormatSettings.{$ENDIF}ShortDateFormat := 'yyyymmdd';
  Timestamp := true;
  FLogLevel := 5;
  FScreenLevel := 5;
  CurrentLog := Self;
  IsInitialized := true;
{$IFDEF DEBUG}
  DeleteLogfile;
{$ELSE}
  LogToFile('------------------------------------------', FFilename);
{$ENDIF}
end;

procedure TLockLog.DeleteLogfile;
begin
  if FileExists(FFilename) then
    DeleteFile(PChar(FFilename));
end;

destructor TLockLog.Destroy;
begin
  CurrentLog := nil;
  IsInitialized := false;
  DeleteCriticalSection(FLogLock);
end;

function TLockLog.LogText(const sLogText: string; iLogLevel: integer =
  0): string;
begin
  if Timestamp then
    Result := Format('[%s] %d: %s', [DateTimeToStr(Now), iLogLevel, sLogText])
  else if iLogLevel = 0 then
    Result := sLogText
  else
    Result := Format('%d: %s', [iLogLevel, sLogText]);
  LogToFile(Result, FFilename);
end;

procedure TLockLog.LogToFile(const sLogText: string; sFileName: string);
begin
  EnterCriticalSection(FLogLock);
  try
    if sFileName = '' then
      sFileName := FFilename;
    if sFileName = '' then
      sFileName := ChangeFileExt(ParamStr(0), '.log');
    WriteToFile(sLogText, sFileName);
  finally
    LeaveCriticalSection(FLogLock);
  end;
end;

procedure WriteToFile(sLine: string; sFileName: string = '');
var
  F: TextFile;
begin
  sFileName := Trim(sFileName);
  AssignFile(F, sFileName);
  if FileExists(sFileName) then
    Append(F)
  else
    Rewrite(F);
  try
    Writeln(F, sLine);
    Flush(F);
  finally
    CloseFile(F);
  end;
end;

{ LogIt utility overloads }

procedure LogIt(const sLogText: string); overload;
begin
  LogIt(0, sLogText);
end;

procedure LogIt(const sLogText: string;
  const Args: array of const);
begin
  LogIt(Format(sLogText, Args));
end;

procedure LogIt(sLogText: string; i: integer);
begin
  if Pos('%d', sLogText) = 0 then
    sLogText := sLogText + '%d';
  LogIt(Format(sLogText, [i]));
end;

procedure LogIt(sLogText: string; d: double);
begin
  if Pos('%', sLogText) = 0 then
    sLogText := sLogText + '%.2f';
  LogIt(Format(sLogText, [d]));
end;

procedure LogIt(const iLogLevel: integer; const sLogText: string);
begin
  if CurrentLog <> nil then
  begin
    if iLogLevel <= CurrentLog.LogLevel then
      CurrentLog.LogText(sLogText, iLogLevel);
  end;
end;

procedure LogIt(const iLogLevel: integer; const sLogText: string;
  const Args: array of const);
begin
  LogIt(iLogLevel, Format(sLogText, Args));
end;

procedure LogIt(const iLogLevel: integer; sLogText: string;
  i: integer);
begin
  if Pos('%d', sLogText) = 0 then
    sLogText := sLogText + '%d';
  LogIt(iLogLevel, Format(sLogText, [i]));
end;

{ TConsoleLog }

function TConsoleLog.LogText(const sLogText: string; iLogLevel: integer =
  0): string;
var
  sLine: string;
begin
  sLine := inherited LogText(sLogText, iLogLevel);
{$IFDEF CONSOLE}
  if iLogLevel <= ScreenLevel then
    Writeln(sLine);
{$ENDIF}
end;

{ TGUILog }

function TGUILog.LogText(const sLogText: string; iLogLevel: integer = 0):
  string;
var
  sLine: string;
begin
  sLine := inherited LogText(sLogText, iLogLevel);
  if Assigned(FLines)
  and (iLogLevel <= ScreenLevel) then
    FLines.Add(sLine);
end;

{$IfDef AutoCreate}
initialization
  TLockLog.Create;
finalization
  CurrentLog.Destroy;
{$EndIf}

end.

