(*
  This file is a part of Audio Components Suite v 2.2,
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_Classes;

interface

uses

{$IFDEF WIN32}
  Windows,
{$ENDIF}
  Classes, SysUtils;

type

  TOutputStatus = (tosPlaying, tosPaused, tosIdle);

  TFileOutputMode = (foRewrite = 0, foAppend);

  TOutputFunc = function(Abort : Boolean):Boolean of object;

  TThreadDoneEvent = procedure of object;

  TThreadExceptionEvent = procedure(Sender : TComponent; E : Exception) of object;

  THandleThreadException = procedure(E : Exception) of object;

  TOutputDoneEvent = procedure(Sender : TComponent) of object;

  TOutputProgressEvent = procedure(Sender : TComponent) of object;

{$IFDEF LINUX}
// File access mask constants
const

  famUserRead = 64;
  famUserWrite = 128;
  famGroupRead = 8;
  famGroupWrite = 16;
  famOthersRead = 1;
  famOthersWrite = 2;

{$ENDIF}

type

  EACSException = class(Exception)
  end;

  TACSThread = class(TThread)
  public
    bSuspend, Terminating : Boolean;
    DoOutput : TOutputFunc;
    Stop : Boolean;
    FOnDone : TThreadDoneEvent;
    HandleException : THandleThreadException;
    Delay : Integer;
{$IFDEF WIN32}
    CS : TRTLCriticalSection;
{$ENDIF}    
    procedure Execute; override;
  end;

  TACSInput = class(TComponent)
  protected
    FPosition : Integer;
    FSize : Integer;
    Buisy : Boolean;
    BufStart, BufEnd : Integer;
    (* We don't declare the buffer variable here
     because different descendants may need different buffer sizes *)
    function GetBPS : Integer; virtual; abstract;
    function GetCh : Integer; virtual; abstract;
    function GetSR : Integer; virtual; abstract;
    function GetTotalTime : Integer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; virtual; abstract;
    procedure Reset; virtual;
    procedure Init; virtual; abstract;
    procedure Flush; virtual; abstract;
    property BitsPerSample : Integer read GetBPS;
    property Position : Integer read FPosition;
    property SampleRate : Integer read GetSR;
    property Channels : Integer read GetCh;
    property Size : Integer read FSize;
    property TotalTime : Integer read GetTotalTime;
  end;

  TACSOutput = class(TComponent)
  protected
    CanOutput : Boolean;
    CurProgr : Integer;
    Thread : TACSThread;
    FInput : TACSInput;
    FOnDone : TOutputDoneEvent;
    FOnProgress : TOutputProgressEvent;
    Buisy : Boolean;  // Set to true by Run and to False by WhenDone.
    FOnThreadException : TThreadExceptionEvent;
    InputLock : Boolean;
    function GetPriority : {$IFDEF LINUX} Integer; {$ENDIF} {$IFDEF WIN32} TThreadPriority; {$ENDIF}
    function GetSuspend : Boolean;
    function GetProgress : Integer;
    procedure SetInput(vInput : TACSInput); virtual;
    procedure SetPriority(Priority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF});
    procedure SetSuspend(v : Boolean);
    procedure WhenDone;
    function GetTE : Integer;
    function GetStatus : TOutputStatus;
    function DoOutput(Abort : Boolean):Boolean; virtual; abstract;
    procedure Done; virtual; abstract; // Calls FInput.Flush
    procedure Prepare; virtual; abstract; // Calls FInput.init
    function GetDelay : Integer;
    procedure SetDelay(Value : Integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleThreadException(E : Exception);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Pause;
    procedure Resume;
    procedure Run;
    procedure Stop;
    property Delay : Integer read GetDelay write SetDelay;
    property ThreadPriority : {$IFDEF LINUX} Integer {$ENDIF} {$IFDEF WIN32} TThreadPriority {$ENDIF} read GetPriority write SetPriority;
    property Progress : Integer read GetProgress;
    property Status : TOutputStatus read GetStatus;
    property TimeElapsed : Integer read GetTE;
  published
    property Input : TACSInput read Finput write SetInput;
    property SuspendWhenIdle : Boolean read GetSuspend write SetSuspend;
    property OnDone : TOutputDoneEvent read FOnDone write FOndone;
    property OnProgress : TOutputProgressEvent read FOnProgress write FOnProgress;
    property OnThreadException : TThreadExceptionEvent read FOnThreadException write FOnThreadException;
  end;

  TACSStreamedInput = class(TACSInput)
  protected
    FStream : TStream;
    FStreamAssigned : Boolean;
    FSeekable : Boolean;
    procedure SetStream(aStream : TStream);
  public
    property Seekable : Boolean read FSeekable write FSeekable;
    property Stream : TStream read FStream write SetStream;
    constructor Create(AOwner: TComponent); override;
  end;

  TACSStreamedOutput = class(TACSOutput)
  protected
    FStream : TStream;
    FStreamAssigned : Boolean;
    procedure SetStream(aStream : TStream);
  public
    property Stream : TStream read FStream write SetStream;
  end;

  TACSFileIn = class(TACSStreamedInput)
  protected
    FFileName : TFileName;
    FOffset : Integer;
    FOpened : Integer;
    FValid : Boolean;
    FBPS, FSR, FChan : Integer;
    FTime : Integer;
    FLoop : Boolean;
    FStartSample, FEndSample : Integer;
    FTotalSamples : Integer;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTime : Integer;
    function GetValid : Boolean;

    (* Note on FSize calculation:
      FSize is calculated in OpenFile method as the FULL file size.
      More precise calculations regarding StartSample/EndSample are done in Init. *)

    procedure OpenFile; virtual; abstract;
    procedure CloseFile; virtual; abstract;
    function GetTotalTime : Integer; override;
    procedure Reset; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Flush; override;
    procedure Init; override;
    function Seek(SampleNum : Integer) : Boolean; virtual; abstract;
    function SetStartTime(Minutes, Seconds : Integer) : Boolean;
    function SetEndTime(Minutes, Seconds : Integer) : Boolean;
    procedure Jump(Offs : Integer);
    property Time : Integer read GetTime;
    property TotalSamples : Integer read FTotalSamples;
    property Valid : Boolean read GetValid;
  published
    property EndSample : Integer read FEndSample write FEndSample;
    property FileName : TFileName read FFileName write FFileName stored True;
    property Loop : Boolean read FLoop write FLoop;
    property StartSample : Integer read FStartSample write FStartSample;
  end;

  TACSFileOut = class(TACSStreamedOutput)
  protected
    FFileName : TFileName;
    FFileMode : TFileOutputMode;
    FAccessMask : Integer;
    procedure SetFileMode(aMode : TFileOutputMode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF LINUX}
    property AccessMask : Integer read FAccessMask write FAccessMask;
{$ENDIF}
  published
    property FileMode : TFileOutputMode read FFileMode write SetFileMode;
    property FileName : TFileName read FFileName write FFileName;
  end;

  TACSConverter = class(TACSInput)
  protected
    InputLock : Boolean;
    FInput : TACSInput;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetInput(aInput : TACSInput); virtual;
  public
    procedure UnlockInput;
  published
    property Input : TACSInput read FInput write SetInput;
  end;

implementation

  constructor TACSInput.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TACSInput.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TACSThread.Execute;
  begin
    while not Terminating do
    begin
      {$IFDEF WIN32}
      EnterCriticalSection(CS);
      {$ENDIF}
      if Delay > 5 then sleep(Delay);
      try
        if not DoOutput(Stop) then
        begin
          Stop := False;
          FOnDone;
          if bSuspend then Self.Suspend;
        end;
      except
        on E : Exception do
        begin
          Stop := False;
          HandleException(E);
          if bSuspend then Self.Suspend;
        end;
      end;
      {$IFDEF WIN32}
      LeaveCriticalSection(CS);
      {$ENDIF}
    end;
    DoOutput(True);  // Why I'm doing this? I don't remember :-)
    Terminating := False;
  end;

  constructor TACSOutput.Create;
  begin
    inherited Create(AOwner);
    Thread := TACSThread.Create(True);
    Thread.DoOutput := Self.DoOutput;
    Thread.FOnDone := Self.WhenDone;
    Thread.FreeOnTerminate := True;
    Thread.HandleException := HandleThreadException;
    {$IFDEF WIN32}
      SetSuspend(True);
      InitializeCriticalSection(Thread.CS);
    {$ENDIF}
  end;

  destructor TACSOutput.Destroy;
  begin
//    if Thread.Suspended then
//    Thread.Resume;
    if not Thread.Suspended then
    Thread.Terminating := True;
    while Thread.Terminating do;
    {$IFDEF WIN32}
    DeleteCriticalSection(Thread.CS);
    {$ENDIF}
    inherited Destroy;
  end;

  procedure TACSOutput.WhenDone;
  begin
    if not Buisy then Exit;
    CanOutput := False;
    Done;
    Buisy := False;
    if Assigned(FOnDone) then FOnDone(Self);
  end;

  procedure TACSOutput.Run;
  begin
    if Buisy then raise EACSException.Create('Component is buisy');
    if not Assigned(FInput) then raise EACSException.Create('Input is not assigned');
    InputLock := False;
 //   if not Thread.Suspended then Thread.Suspend;
    try
      Prepare;
      Buisy := True;
      Thread.Stop := False;
      CanOutput := True;
      if Thread.Suspended then Thread.Resume;
    except
      on E : Exception do HandleThreadException(E);
    end;
  end;

  procedure TACSOutput.Stop;
  begin
    Thread.Stop := True;
  end;

  function TACSOutput.GetStatus;
  begin
    if Buisy then
    begin
      if Self.Thread.Suspended then Result := tosPaused
      else Result := tosPlaying;
    end else Result := tosIdle;
  end;

  procedure TACSOutput.SetPriority;
  begin
    Thread.Priority := Priority;
  end;

  function TACSOutput.GetPriority;
  begin
    Result := Thread.Priority;
  end;

  procedure TACSOutput.SetInput;
  var
    OldInput, NewInput : TACSInput;
  begin
    if Buisy then
    begin
      NewInput := vInput;
      NewInput.Init;
      OldInput := FInput;
      while InputLock do;
      InputLock := True;
      FInput := NewInput;
      InputLock := False;
      OldInput.Flush;
    end else
    FInput := vInput;
  end;

  function  TACSOutput.GetProgress;
  begin
    if not Assigned(Finput) then
    begin
      Result := 0;
      Exit;
    end;
    case Finput.Size of
      0: Result := 0;
      -1: Result := -1;
      else Result := Round((FInput.Position/FInput.Size)*100);
    end;
  end;

  procedure TACSOutput.Pause;
  begin
    If not Thread.Suspended then Thread.Suspend;
  end;

  procedure TACSOutput.Resume;
  begin
    If Thread.Suspended then Thread.Resume;
  end;

  function TACSOutput.GetSuspend;
  begin
    Result := Thread.bSuspend;
  end;

  procedure TACSOutput.SetSuspend;
  begin
    Thread.bSuspend := v;
  end;

  constructor TACSStreamedInput.Create;
  begin
    inherited Create(AOwner);
    FSeekable := True;
  end;

  function TACSFileIn.GetBPS;
  begin
    if FSeekable then
    begin
      OpenFile;
      Result := FBPS;
      CloseFile;
    end else Result := FBPS;
  end;

  function TACSFileIn.GetCh;
  begin
    if FSeekable then
    begin
      OpenFile;
      Result := FChan;
      CloseFile;
    end else Result := FChan;
  end;

  function TACSFileIn.GetSR;
  begin
    if FSeekable then
    begin
      OpenFile;
      Result := FSR;
      CloseFile;
    end else Result := FSR;
  end;

  function TACSFileIn.GetTime;
  begin
    if FSeekable then
    begin
      OpenFile;
      Result := FTime;
      CloseFile;
    end else Result := FTime;
  end;

  function TACSFileIn.GetValid;
  begin
    if (not FStreamAssigned) and (FileName = '') then
    begin
      Result := False;
    end else
    if FSeekable then
    begin
      OpenFile;
      Result := FValid;
      CloseFile;
    end else Result := FValid;
  end;

  procedure TACSFileIn.Init;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    if not FStreamAssigned then
    if FFileName = '' then raise EACSException.Create('The file name is not assigned');
    OpenFile;
    if StartSample <> 0 then Seek(StartSample);
    if (StartSample <> 0) or (FEndSample <> -1) then
    begin
      FSize := FEndSample - FStartSample;
      if FEndSample = -1 then FSize := FSize + FTotalSamples + 1;
      FSize := FSize*(BitsPerSample shr 3)*FChan;
    end;
    Buisy := True;
    BufStart := 1;
    BufEnd := 0;
    FPosition := 0;
  end;

  procedure TACSFileIn.Flush;
  begin
    CloseFile;
    Buisy := False;
  end;

  procedure TACSFileIn.Jump;
  begin
    FOffset := Offs;
  end;

  function TACSOutput.GetTE;
  begin
     if not Assigned(FInput) then
     Result := 0
     else
     Result := Round(FInput.Position/((FInput.BitsPerSample shr 3) *FInput.Channels*FInput.SampleRate));
  end;

  function TACSOutput.GetDelay;
  begin
    if Assigned(Thread) then Result := Thread.Delay;
  end;

  procedure TACSOutput.SetDelay;
  begin
    if Assigned(Thread) then
    if Value <= 100 then Thread.Delay := Value;
  end;

  function TACSInput.GetTotalTime;
  begin
    Result := 0;  // Default result for the streams.
  end;

  function TACSFileIn.GetTotalTime;
  begin
    OpenFile;
    if (SampleRate = 0) or (Channels = 0) or (BitsPerSample = 0) then Exit;
    Result := Round(Size/(SampleRate*Channels*(BitsPerSample shr 3)));
    CloseFile;
  end;

  procedure TACSStreamedInput.SetStream;
  begin
    FStream := aStream;
    if FStream <> nil then FStreamAssigned := True
    else FStreamAssigned := False;
  end;

  procedure TACSStreamedOutput.SetStream;
  begin
    FStream := aStream;
    if FStream <> nil then FStreamAssigned := True
    else FStreamAssigned := False;
  end;

  procedure TACSOutput.Notification;
  begin
    // Remove the following two lines if they cause troubles in your IDE
    if (AComponent = FInput) and (Operation = opRemove )
    then Input := nil;
    inherited Notification(AComponent, Operation);
  end;

  procedure TACSInput.Reset;
  begin
    try
      Flush;
    except
    end;
    Buisy := False;
  end;

  procedure TACSOutput.HandleThreadException;
  var
    Conv : TACSConverter;
  begin
   InputLock := False;
   if Status <> tosIdle then
   begin
     try
      if FInput is TACSConverter then
      begin
        Conv := FInput as TACSConverter;
        Conv.UnlockInput;
      end;
     except
     end;
     try
       Done;
     except
     end;
   end;
   CanOutput := False;
   Buisy := False;
   if Assigned(FOnThreadException) then FOnThreadException(Self, E);
  end;

  procedure TACSFileIn.Reset;
  begin
    inherited Reset;
    FOpened := 0;
  end;


  constructor TACSFileOut.Create;
  begin
    inherited Create(AOwner);
    {$IFDEF LINUX}
    FAccessMask := $1B6; // rw-rw-rw-
    {$ENDIF}
  end;

  procedure TACSFileOut.SetFileMode;
  begin
    FFileMode := foRewrite;
  end;

  procedure TACSConverter.Notification;
  begin
    // Remove the following two lines if they cause troubles in your IDE
    if (AComponent = FInput) and (Operation = opRemove )
    then Input := nil;
    inherited Notification(AComponent, Operation);
  end;

  procedure TACSConverter.SetInput;
  var
    OldInput, NewInput : TACSInput;
  begin
    if aInput = Self then Exit;
    if Buisy then
    begin
      NewInput := aInput;
      NewInput.Init;
      OldInput := FInput;
      while InputLock do;
      InputLock := True;
      FInput := NewInput;
      InputLock := False;
      OldInput.Flush;
    end else
    FInput := aInput;
  end;

  procedure TACSConverter.UnlockInput;
  var
    Conv : TACSConverter;
  begin
    InputLock := False;
    if Assigned(FInput) then
    if FInput is TACSConverter then
    begin
      Conv := FInput as TACSConverter;
      Conv.UnlockInput;
    end;
  end;

  function TACSFileIn.SetStartTime;
  var
    Sample : Integer;
  begin
    Result := False;
    if not FSeekable then Exit;
    OpenFile;
    CloseFile;
    Sample := (Minutes*60+Seconds)*FSR;
    if Sample > FTotalSamples then Exit;
    FStartSample := Sample;
    Result := True;
  end;

  function TACSFileIn.SetEndTime;
  var
    Sample : Integer;
  begin
    Result := False;
    if not FSeekable then Exit;
    OpenFile;
    CloseFile;
    Sample := (Minutes*60+Seconds)*FSR;
    if Sample > FTotalSamples then Exit;
    FEndSample := Sample;
    Result := True;
  end;

  constructor TACSFileIn.Create;
  begin
    inherited Create(AOwner);
    FStartSample := 0;
    FEndSample := -1;
  end;

end.


