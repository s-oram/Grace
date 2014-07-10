{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksComm;

{$I ksTools.inc}

interface

uses
  Consts, Windows, Messages, SysUtils, Classes, Forms, ksClasses;

type
  TksMessageEvent = procedure(Sender: TObject; var Msg: TMessage) of object;

  TksCommStateEvent = procedure(Sender: TObject; var DCB: TDCB) of object;
  TksCommTimeoutsEvent = procedure(Sender: TObject; var Timeouts: TCommTimeouts) of object;
  TksCommMaskEvent = procedure(Sender: TObject; var Mask: LongWord) of object;
  TksCommEvent = procedure(Sender: TObject; Value: LongWord) of object;
  TksReadStopEvent = procedure(Sender: TObject; Value: Byte; var Stop: Boolean) of object;

type
  TksComLogger = class;

  TksComPort = class(TComponent)
  public const
                            { Event types }
    etCommEvent     = 0;
    etCommError     = 1;
    etCommDataReady = 2;
    etWriteBufEmpty = 3;
    etReadOverflow  = 4;
    etFatalError    = 5;

    DefaultCommTimeouts: TCommTimeouts = (
      ReadIntervalTimeout: MAXDWORD;
      ReadTotalTimeoutMultiplier: 0;
      ReadTotalTimeoutConstant: 0;
      WriteTotalTimeoutMultiplier: 0;
      WriteTotalTimeoutConstant: 0);

    DefaultEventMask = EV_CTS + EV_DSR + EV_RLSD + EV_RING +
                      {EV_RXCHAR +} EV_ERR + EV_BREAK;

                      { Valid Parity Values, copied from Windows.pas }
    NOPARITY = 0;
    ODDPARITY = 1;
    EVENPARITY = 2;
    MARKPARITY = 3;
    SPACEPARITY = 4;

                      { Valid StopBits Values, copied from Windows.pas }
    ONESTOPBIT = 0;
    ONE5STOPBITS = 1;
    TWOSTOPBITS = 2;

                      { Valid DTR Control Flow Values, copied from Windows.pas }
    DTR_CONTROL_DISABLE = 0;
    DTR_CONTROL_ENABLE = 1;
    DTR_CONTROL_HANDSHAKE = 2;

                      { Valid RTS Control Flow Values, copied from Windows.pas }
    RTS_CONTROL_DISABLE = 0;
    RTS_CONTROL_ENABLE = 1;
    RTS_CONTROL_HANDSHAKE = 2;
    RTS_CONTROL_TOGGLE = 3;

                      { Communications Error Flags, copied from Windows.pas }
    CE_RXOVER = 1;        { Receive Queue overflow }
    CE_OVERRUN = 2;       { Receive Overrun Error }
    CE_RXPARITY = 4;      { Receive Parity Error }
    CE_FRAME = 8;         { Receive Framing error }
    CE_BREAK = $10;       { Break Detected }
    CE_TXFULL = $100;     { TX Queue is full }
    CE_PTO = $200;        { LPTx Timeout }
    CE_IOE = $400;        { LPTx I/O Error }
    CE_DNS = $800;        { LPTx Device not selected }
    CE_OOP = $1000;       { LPTx Out-Of-Paper }
    CE_MODE = $8000;      { Requested mode unsupported }

                      { Communications Events, copied from Windows.pas }
    EV_RXCHAR = 1;        { Any Character received }
    EV_RXFLAG = 2;        { Received certain character }
    EV_TXEMPTY = 4;       { Transmitt Queue Empty }
    EV_CTS = 8;           { CTS changed state }
    EV_DSR = $10;         { DSR changed state }
    EV_RLSD = $20;        { RLSD changed state }
    EV_BREAK = $40;       { BREAK received }
    EV_ERR = $80;         { Line status error occurred }
    EV_RING = $100;       { Ring signal detected }
    EV_PERR = $200;       { Printer error occured }
    EV_RX80FULL = $400;   { Receive buffer is 80 percent full }
    EV_EVENT1 = $800;     { Provider specific event 1 }
    EV_EVENT2 = $1000;    { Provider specific event 2 }

  private type
    TReadCache = record
      Buffer: PByte;
      BufSize: Integer;
      UsedSize: Integer;
//      function FreeSize: Integer;
      procedure AllocBuffer(const ASize: Integer);
//      function Read(var Buf; Count: Integer): Integer;
    end;
  private
    FWndHandle: HWND;
    FPortName: string;
    FPortHandle: THandle;
    FRBuffer: TksRingBuffer;
    FWBuffer: TksRingBuffer;
    FReadCache: TReadCache;

    FRXEvent: THandle;          // сигнал потоку чтения
                                //   от потока мониторинга
    FWriteEvent: THandle;       // сигнал потоку записи
                                //   от главного потока
    FExitEvent: THandle;        // сигнал завершения фоновых потоков

    FReadThread: TThread;
    FWriteThread: TThread;
    FEventThread: TThread;

    FEventMask: LongWord;       // маска эвентов, о которых поток мониторинга
                                //  оповещает главный поток
    FErrorMask: LongWord;

    FBaudRate: LongWord;
    FByteSize: Byte;
    FParity: Byte;
    FStopBits: Byte;
    FRBufSize: Integer;
    FWBufSize: Integer;

    FParityCheck: Boolean;
    FCTSControl: Boolean;
    FDSRControl: Boolean;
    FDSRSensitivity: Boolean;
    FDTRControl: LongWord;
    FRTSControl: LongWord;

    FPendingRead: Boolean;
    FPendingWrite: Boolean;
    FPendingCount: Integer;
    FPendingPtr: PByte;
    FTimerFired: Boolean;
//    FStopPos: Integer;
    FReadStopped: Boolean;

    FOnMessage: TksMessageEvent;
    FOnError: TksCommEvent;
    FOnEvent: TksCommEvent;
    FOnRead: TksCommEvent;
    FOnReadOverflow: TksCommEvent;
    FOnWrite: TksCommEvent;
    FOnFatalError: TksCommEvent;

    FOnReadStop: TksReadStopEvent;

    FOnSetCommState: TksCommStateEvent;
    FOnSetCommTimeouts: TksCommTimeoutsEvent;
    FOnSetCommMask: TksCommMaskEvent;

    FLogger: TksComLogger;
    procedure SetLogger(const Value: TksComLogger);

    const MsgID = WM_APP + 11;
    procedure WndProc(var Msg: TMessage);

    procedure SetRBufSize(Value: Integer);
    procedure SetWBufSize(Value: Integer);
    procedure SetBaudRate(const Value: LongWord);
    procedure SetByteSize(const Value: Byte);
    procedure SetParity(const Value: Byte);
    procedure SetStopBits(const Value: Byte);
    procedure SetRTSControl(const Value: LongWord);
    procedure SetDTRControl(const Value: LongWord);
    procedure SetDSRSensitivity(const Value: Boolean);
    procedure SetParityCheck(const Value: Boolean);
    procedure SetCTSControl(const Value: Boolean);
    procedure SetDSRControl(const Value: Boolean);

    procedure UpdateDCB(var DCB: TDCB);
    procedure UpdateProperties(const DCB: TDCB);
    procedure UpdateTimer(TimeOut: LongWord);
  protected
    procedure HandleMessage(Msg: TMessage);
    procedure PortMessage(Code, SubCode: Integer);
    function ReadFromCache(var Buf; Count: Integer): Integer;
    function ReadStop(Count: Integer): Integer;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open: Boolean;
    procedure ClearRBuf;
    procedure ClearWBuf;
    procedure Close;

    procedure LogData(AFlags: LongWord; const Buf; BufSize: Integer);

    function Connected: Boolean;
    function GetPortState(var DCB: TDCB): Boolean;
    function GetPortTimeouts(var Timeouts: TCommTimeouts): Boolean;
    function GetPortMask(var Mask: LongWord): Boolean;
    function SetPortState(const DCB: TDCB): Boolean;
    function Read(var Buf; Count: Integer): Integer; overload;
    function Read(var Buf; Count: Integer; TimeOut: LongWord): Integer; overload;
    function Write(const Buf; Count: Integer): Integer; overload;
    function Write(const Buf; Count: Integer; TimeOut: LongWord): Integer; overload;

    property Handle: THandle read FPortHandle;
//    property StopPos: Integer read FStopPos;
    property ReadStopped: Boolean read FReadStopped;

  published
    { Published declarations }
    property PortName: string read FPortName write FPortName;
    property RBufSize: Integer read FRBufSize write SetRBufSize default 4096;
    property WBufSize: Integer read FWBufSize write SetWBufSize default 4096;

    property BaudRate: LongWord read FBaudRate write SetBaudRate default 9600;
    property ByteSize: Byte read FByteSize write SetByteSize default 8;
    property Parity: Byte read FParity write SetParity default NOPARITY;
    property StopBits: Byte read FStopBits write SetStopBits default ONESTOPBIT;
    property ParityCheck: Boolean read FParityCheck write SetParityCheck default False;
    property CTSControl: Boolean read FCTSControl write SetCTSControl default False;
    property DSRControl: Boolean read FDSRControl write SetDSRControl default False;
    property DTRControl: LongWord read FDTRControl write SetDTRControl default DTR_CONTROL_DISABLE;
    property RTSControl: LongWord read FRTSControl write SetRTSControl default RTS_CONTROL_DISABLE;
    property DSRSensitivity: Boolean read FDSRSensitivity write SetDSRSensitivity default False;

    property Logger: TksComLogger read FLogger write SetLogger;
//    property ReadFileName: string read FReadName write FReadName;
    property OnMessage: TksMessageEvent read FOnMessage write FOnMessage;
    property OnReadOverflow: TksCommEvent read FOnReadOverflow write FOnReadOverflow;
    property OnError: TksCommEvent read FOnError write FOnError;
    property OnEvent: TksCommEvent read FOnEvent write FOnEvent;
    property OnRead: TksCommEvent read FOnRead write FOnRead;
    property OnWrite: TksCommEvent read FOnWrite write FOnWrite;
    property OnFatalError: TksCommEvent read FOnFatalError write FOnFatalError;
    property OnReadStop: TksReadStopEvent read FOnReadStop write FOnReadStop;
//    property OnGetCommState: TksCommStateEvent read FOnGetCommState write FOnGetCommState;
    property OnSetCommState: TksCommStateEvent read FOnSetCommState write FOnSetCommState;
    property OnSetCommTimeouts: TksCommTimeoutsEvent read FOnSetCommTimeouts write FOnSetCommTimeouts;
    property OnSetCommMask: TksCommMaskEvent read FOnSetCommMask write FOnSetCommMask;
  end;

  TksComLogger = class
  public const
                                // Log Item Flags
    lfMain  = 0;                // main thread
    lfEvent = 1;                // event thread
    lfRead  = 2;                // read thread
    lfWrite = 3;                // write thread
    lfComEvent = $10000000;     // communications event
    lfComError = $20000000;     // communications error
    lfSoftError = $40000000;    // software error like read overflow
    lfFatalError = $80000000;   // fatal error
  public type
    PLogItem = ^TLogItem;
    TLogItem = packed record
      Size: LongWord;
      Flags: LongWord;
      Occured: TDateTime;
      Data: record end;
    end;
//  private
//    FPort: TksComPort;
  protected
    procedure LogData(AFlags: LongWord; const Buf; BufSize: Integer); virtual; abstract;
  end;

type
  TksComStreamLogger = class(TksComLogger)
  public type
    PLogHeader = ^TLogHeader;
    TLogHeader = packed record
    StartTime: TDateTime;
    StopTime: TDateTime;
    SizeOfChar: LongWord;
    CommentLen: LongWord;
    Comment: record end;
  end;
  protected
    FActive: Boolean;
    FLock: array[0..2] of TRTLCriticalSection;
    FStream: array[0..3] of TStream;
    FItemCount: array[0..3] of Integer;
    FStartTime: TDateTime;
    FStopTime: TDateTime;
    FComment: string;
//    procedure Lock;
//    procedure UnLock;
    function CreateStream(Index: Integer): TStream; virtual; abstract;
//    function ResetStream(Index: Integer): TStream; virtual;
//    function LogStream(Index: Integer): TStream;
    procedure SaveToStream(Target: TStream);
    procedure LogData(AFlags: LongWord; const Buf; BufSize: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset; virtual;
    procedure SaveToFile(const AFileName: string);
    property Active: Boolean read FActive write FActive;
  end;

  TksComMemoryLogger = class(TksComStreamLogger)
  protected
    function CreateStream(Index: Integer): TStream; override;

//  public
//    constructor Create;
//    destructor Destroy; override;
//    procedure InitStreams; override;
//    procedure FreeStreams; override;
  end;

implementation

type
  TCommThread = class(TThread)
  protected
    FOwner: TksComPort;
//    FInternalError: Integer;
    procedure PortMessage(Code, SubCode: Integer);
  public
    constructor Create(AOwner: TksComPort);
//    property InternalError: Integer read FInternalError;
  end;

  TCommEventThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

  TCommReadThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

  TCommWriteThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

{ TCommThread }

constructor TCommThread.Create(AOwner: TksComPort);
begin
  FOwner:= AOwner;
  inherited Create(False);
end;

procedure TCommThread.PortMessage(Code, SubCode: Integer);
begin
  if not Terminated then
//    PostMessage(FOwner.FWndHandle, FOwner.MsgID, Code, SubCode);
    FOwner.PortMessage(Code, SubCode);
end;

{ TCommEventThread }

procedure TCommEventThread.Execute;
var
  OL: TOverlapped;
  EvtMask: DWORD;
  Junk: DWORD;
  OK: Boolean;
  H: array[0..1] of THandle;

begin
  try
    FillChar(OL, SizeOf(OL), 0);
    OL.hEvent:= CreateEvent(nil,
                            True,     // !!! - manual reset
                            False,    // initial state = not signaled
                            nil);
    H[0]:= OL.hEvent;
    H[1]:= FOwner.FExitEvent;
    try
      while not Terminated do begin
        OK:= WaitCommEvent(FOwner.FPortHandle, EvtMask, @OL);
{
  Overlapped WaitCommEvent returns False immediately;
  checking WaitCommEvent result here is not required
}
        if not OK and (GetLastError = ERROR_IO_PENDING) then begin
{
  Wait for CommEvent to occur or ExitEvent signalled
}
          case WaitForMultipleObjects(2, @H, False, INFINITE) of

            WAIT_OBJECT_0: begin
              OK:= GetOverLappedResult(FOwner.FPortHandle, OL, Junk, False);
              ResetEvent(OL.hEvent);
              FOwner.LogData(TksComLogger.lfEvent, EvtMask, SizeOf(LongWord));
            end;

            WAIT_OBJECT_0 + 1: begin
              Exit;
            end;

          end; { case }
        end;

        if OK then begin
          if (EvtMask and FOwner.FEventMask <> 0) then begin
{ Inform main thread }
            PortMessage(TksComPort.etCommEvent, LPARAM(EvtMask));
          end;
{ Signal com event to read thread}
          if (EvtMask and EV_RXCHAR <> 0) and (FOwner.FRXEvent <> 0)
            then SetEvent(FOwner.FRXEvent);
        end
        else begin
{ Port closed or other fatal condition, just exit the thread }
//          FInternalError:= 2;
          PortMessage(TksComPort.etFatalError, 1);
          Exit;
        end;

      end; { while }

    finally
      CloseHandle(OL.hEvent);
    end;

  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(TksComPort.etFatalError, 2);
//      raise;
    end;
  end;
end;

{ TCommReadThread }

procedure TCommReadThread.Execute;
const
  DefaultTimeout = 50;

var
  ReadOL: TOverlapped;
  H: packed array[0..1] of THandle;
  OK: Bool;
  ReadCount: DWORD;
  Errors: DWORD;
  Stat: TComStat;
  InBuf: packed array[0..511] of Byte;
  BytesToRead: LongWord;
  BytesLost: LongWord;
  WaitResult: Cardinal;
  Timeout: LongWord;    // Specifies the watchdog time-out in milliseconds.

begin
//  LastError:= 0;
  try
    FillChar(ReadOL, SizeOf(ReadOL), 0);
    ReadOL.hEvent:= CreateEvent(nil, True, False, nil);   // !!! manual reset
    Timeout:= DefaultTimeout;
    try
      while not Terminated do begin
{
  Wait for FReadEvent signalled from Comm Event Thread (EV_RXCHAR detected)
        or FWriteEvent signalled from main thread (write requested)
  We expect these events to happen, but we don't rely on them;
    if nothing happens for 50 milliseconds after last event, thread wakes up,
    checks port state and performs Read, if necessary.
}

        H[0]:= FOwner.FRXEvent;
        H[1]:= FOwner.FExitEvent;
        WaitResult:= WaitForMultipleObjects(2, @H, False, Timeout);

        case WaitResult of

// RX Event signalled
          WAIT_OBJECT_0: begin
            ResetEvent(FOwner.FRXEvent);
            Timeout:= DefaultTimeout;
          end;

// Exit Event signalled
          WAIT_OBJECT_0 + 1: Exit;

// Timeout
          WAIT_TIMEOUT: Timeout:= {DefaultTimeout;  //} INFINITE;

// Unknown
        else
//          FInternalError:= 5;
          PortMessage(TksComPort.etFatalError, $101);
          Exit;
        end;


        while ClearCommError(FOwner.FPortHandle, Errors, @Stat) do begin
          Errors:= Errors and FOwner.FErrorMask;
          if (Errors <> 0) then begin
            PortMessage(TksComPort.etCommError, Errors);
            FOwner.LogData(TksComLogger.lfComError or TksComLogger.lfRead,
                           Errors, SizeOf(LongWord));
          end;

          if Stat.cbInQue = 0 then Break;

          Timeout:= DefaultTimeout;

          BytesToRead:= Stat.cbInQue;
          if BytesToRead > SizeOf(InBuf) then BytesToRead:= SizeOf(InBuf);
          OK:= ReadFile(FOwner.FPortHandle,          {handle}
                        InBuf,                       {buffer}
                        BytesToRead,                 {bytes to read}
                        ReadCount,                   {bytes read}
                        @ReadOL);                    {overlap record}

          if not OK then begin
            if GetLastError = ERROR_IO_PENDING then begin
              H[0]:= ReadOL.hEvent;
              H[1]:= FOwner.FExitEvent;
              WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE);
              case WaitResult of

// Read completed
                WAIT_OBJECT_0: begin
                  if GetOverlappedResult(FOwner.FPortHandle,  {handle}
                                         ReadOL,              {overlap record}
                                         ReadCount,           {bytes read}
                                         False)               {do not wait}
                  then begin
                    ResetEvent(ReadOL.hEvent);
                    OK:= True;
                  end;
                end;

// Exit signalled
                WAIT_OBJECT_0 + 1: Exit;

// Unknown
              else
//                FInternalError:= 5;
                PortMessage(TksComPort.etFatalError, $102);
                Exit;
              end; {case}

            end;
          end;

          if OK and (ReadCount > 0) then begin
//                  if Assigned(FOwner.FReadLog) then
//                    FOwner.FReadLog.Write(InBuf, ReadCount);
            BytesLost:= ReadCount - LongWord(FOwner.FRBuffer.Write(InBuf, ReadCount));
            if BytesLost <> 0 then PortMessage(TksComPort.etReadOverflow, BytesLost);
//        Dec(Stat.cbInQue, ReadCount);
            PortMessage(TksComPort.etCommDataReady, ReadCount);
          end;

        end; {while}

      end;
    finally
      CloseHandle(ReadOL.hEvent);
    end;
  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(TksComPort.etFatalError, $103);
      raise;
    end;
  end;
end;

{ TCommWriteThread }

procedure TCommWriteThread.Execute;
const
  DefaultTimeout = 50;

var
  WriteOL: TOverlapped;
  H: packed array[0..1] of THandle;
  OK: Bool;
  WriteCount: DWORD;
  TotalCount: LongWord;
  Errors: DWORD;
  Stat: TComStat;
// OutBuf size must not exceed the driver output buffer size
  OutBuf: packed array[0..127] of Byte;
//  OutBufPtr: Pointer;
//  P: PByte;
  BytesToWrite: LongWord;
  WaitResult: Cardinal;

begin
//  PostPos:= 0;
//  LastError:= 0;
  try
    FillChar(WriteOL, SizeOf(WriteOL), 0);
    WriteOL.hEvent:= CreateEvent(nil, True, False, nil);  // !!! manual reset
//    Timeout:= INFINITE;

 //   BytesWritten:= 0;
    BytesToWrite:= 0;
    try
      while not Terminated do begin

//        H[0]:= WriteOL.hEvent;
        H[0]:= FOwner.FWriteEvent;
        H[1]:= FOwner.FExitEvent;
        WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE {TimeOut});

        case WaitResult of

// Write signalled
          WAIT_OBJECT_0: ResetEvent(FOwner.FWriteEvent);

          WAIT_OBJECT_0 + 1: Exit;
// Timeout
//          WAIT_TIMEOUT: Timeout:= {DefaultTimeout;  //} INFINITE;

// Unknown
        else
//          FInternalError:= 5;
          PortMessage(TksComPort.etFatalError, $201);
          Exit;
        end; {case}

        TotalCount:= 0;

        while True do begin

          if ClearCommError(FOwner.FPortHandle, Errors, @Stat) then begin
            Errors:= Errors and FOwner.FErrorMask;
            if (Errors <> 0) {and (Errors <> LastError)} then begin
              PortMessage(TksComPort.etCommError, Errors);
            end;
//            LastError:= Error;
          end;

          Inc(BytesToWrite, FOwner.FWBuffer.Read(OutBuf[BytesToWrite],
                                          SizeOf(OutBuf) - BytesToWrite));

//            BytesToWrite:= FOwner.FWBuffer.Read(OutBuf, SizeOf(OutBuf));

          if BytesToWrite = 0 then begin
            PortMessage(TksComPort.etWriteBufEmpty, TotalCount);
            Break;
          end;

//          Timeout:= DefaultTimeout;
//
          OK:= WriteFile(FOwner.FPortHandle,         {handle}
                         OutBuf,                     {buffer}
                         BytesToWrite,               {bytes to write}
                         WriteCount,                 {bytes written}
                         @WriteOL);                  {overlap record}

          if not OK then begin
            if GetLastError = ERROR_IO_PENDING then begin
              H[0]:= WriteOL.hEvent;
              H[1]:= FOwner.FExitEvent;
              WaitResult:= WaitForMultipleObjects(2, @H, False, INFINITE);
              case WaitResult of

// Write completed
                WAIT_OBJECT_0: begin
                  if GetOverlappedResult(FOwner.FPortHandle,  {handle}
                                         WriteOL,         {overlap record}
                                         WriteCount,      {bytes written}
                                         False)           {don't wait for completion}
                  then begin
                    ResetEvent(WriteOL.hEvent);
                    OK:= True;
//              Inc(BytesWritten, WriteCount);
                  end;
                end;

                WAIT_OBJECT_0 + 1: Exit;

              else
//                FInternalError:= 5;
                PortMessage(TksComPort.etFatalError, $202);
                Exit;
              end;  {case}
            end;
          end;

          if OK then begin
            if (WriteCount > 0) then begin
              Dec(BytesToWrite, WriteCount);
              Inc(TotalCount, WriteCount);
              if (BytesToWrite > 0) then
                Move(OutBuf[WriteCount], OutBuf, BytesToWrite);
            end;
          end
          else begin
//            FInternalError:= 2;
            PortMessage(TksComPort.etFatalError, $203);
            Exit;
          end;
        end;
      end;
    finally
      CloseHandle(WriteOL.hEvent);
    end;
  except
    on E:Exception do begin
//      FInternalError:= 1;
      PortMessage(TksComPort.etFatalError, $204);
      raise;
    end;
  end;
end;

{ TksComPort.TReadCache }

procedure TksComPort.TReadCache.AllocBuffer(const ASize: Integer);
begin
  if BufSize <> ASize then begin
    if BufSize > 0 then begin
      FreeMem(Buffer, BufSize);
      BufSize:= 0;
    end;
    if ASize > 0 then begin
      GetMem(Buffer, ASize);
      BufSize:= ASize;
    end;
  end;
end;
{
function TksComPort.TReadCache.Read(var Buf; Count: Integer): Integer;
begin
  if Count <= 0 then Result:= 0
  else begin
    if Count > UsedSize then Count:= UsedSize;
    Move(Buffer^, Buf, Count);
    if UsedSize > Count then
      Move(Buffer^[Count], Buffer^, UsedSize - Count);
    Dec(UsedSize, Count);
    Result:= Count;
  end;
end;
}
{ TksComPort }

constructor TksComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWndHandle:= Classes.AllocateHWnd(WndProc);
  FPortHandle:= INVALID_HANDLE_VALUE;
  FErrorMask:= $FFFFFFFF;
  FEventMask:= DefaultEventMask;
  FBaudRate:= 9600;
  FByteSize:= 8;
  FRBufSize:= 4096;
  FWBufSize:= 4096;
  FPortName:= 'COM1';
end;

destructor TksComPort.Destroy;
begin
  Close;
  Classes.DeallocateHWnd(FWndHandle);
  Logger:= nil;
  inherited Destroy;
end;

procedure TksComPort.HandleMessage(Msg: TMessage);
var
  Count: Integer;
//  Stop: Boolean;

begin
  if FPendingRead and (Msg.WParam = etCommDataReady) then begin
    Count:= Read(FPendingPtr^, FPendingCount);
//    Stop:= ReadStop(Count);
    Inc(FPendingPtr, Count);
    Dec(FPendingCount, Count);
    FPendingRead:= (FPendingCount > 0) and not FReadStopped;
    if not FPendingRead then begin
      UpdateTimer(0);
      if (Count < Msg.LParam) then
        PortMessage(etCommDataReady, Msg.LParam - Count);
    end;
    Msg.Result:= 0;
    Exit;
  end;

  if FPendingWrite and (Msg.WParam = etWriteBufEmpty) then begin
    FPendingWrite:= FPendingCount > Msg.LParam;
    if FPendingWrite then begin
      Dec(FPendingCount, Msg.LParam);
    end
    else begin
      FPendingCount:= 0;
      UpdateTimer(0);
    end;
    Msg.Result:= 0;
    Exit;
  end;

  Msg.Result:= 1;
//  if (Msg.Result <> 0) and Assigned(FOnMessage) then FOnMessage(Self, Msg);
  if Assigned(FOnMessage) then FOnMessage(Self, Msg);
  if Msg.Result <> 0 then begin
    case Msg.WParam of
      etCommEvent: if Assigned(FOnEvent) then FOnEvent(Self, Msg.LParam);
      etCommError: if Assigned(FOnError) then FOnError(Self, Msg.LParam);
      etCommDataReady: if Assigned(FOnRead) then FOnRead(Self, Msg.LParam);
      etWriteBufEmpty: if Assigned(FOnWrite) then FOnWrite(Self, Msg.LParam);
      etReadOverflow: if Assigned(FOnReadOverflow) then FOnReadOverflow(Self, Msg.LParam);
      etFatalError: if Assigned(FOnFatalError) then FOnFatalError(Self, Msg.LParam);
    end;
    Msg.Result:= 0;
  end;
end;

procedure TksComPort.LogData(AFlags: LongWord; const Buf; BufSize: Integer);
begin
  if Assigned(FLogger) then
    FLogger.LogData(AFlags, Buf, BufSize);
end;

function TksComPort.Open: Boolean;
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
  Mask: LongWord;

begin
  if FPortName = '' then begin
    Result:= False;
    Exit;
  end;
  FPortHandle:= CreateFile(PChar(FPortName),          {name}
                       GENERIC_READ or GENERIC_WRITE, {access attributes}
                       0,                             {no sharing}
                       nil,                           {no security}
                       OPEN_EXISTING,                 {creation action}
                       FILE_ATTRIBUTE_NORMAL          {attributes}
                    or FILE_FLAG_OVERLAPPED,          { and flags}
                       0);                            {no template}
  Result:= FPortHandle <> INVALID_HANDLE_VALUE;
  if Result then begin
    FRBuffer:= TksRingBuffer.Create(FRBufSize);
    FWBuffer:= TksRingBuffer.Create(FWBufSize);
    FReadCache.AllocBuffer(FRBufSize);

{ Signalled by Event Thread to Read Thread}
    FRXEvent:= CreateEvent(nil,
                           True,      // manual reset
                           False,     // initial state = not signaled
                           nil);

{ Signalled by main Thread to Write Thread}
    FWriteEvent:= CreateEvent(nil,
                              True,      // manual reset
                              False,     // initial state = not signaled
                              nil);

{ Signalled by main Thread to all background threads}
    FExitEvent:= CreateEvent(nil,
                             True,      // manual reset
                             False,     // initial state = not signaled
                             nil);

    if GetCommState(FPortHandle, DCB) then begin
      UpdateDCB(DCB);
      if Assigned(FOnSetCommState) then FOnSetCommState(Self, DCB);
      SetCommState(FPortHandle, DCB);
    end;
    Timeouts:= DefaultCommTimeouts;
    if Assigned(FOnSetCommTimeouts) then FOnSetCommTimeouts(Self, Timeouts);
    SetCommTimeouts(FPortHandle, Timeouts);

//    if FReadName <> '' then FReadLog:= TFileStream.Create(FReadName, fmCreate);

    Mask:= DefaultEventMask;
    if Assigned(FOnSetCommMask) then FOnSetCommMask(Self, Mask);
    SetCommMask(FPortHandle, Mask or EV_RXCHAR);
    FEventMask:= Mask;
    FReadThread:= TCommReadThread.Create(Self);
    FWriteThread:= TCommWriteThread.Create(Self);
    FEventThread:= TCommEventThread.Create(Self);
  end;
end;

procedure TksComPort.PortMessage(Code, SubCode: Integer);
begin
  PostMessage(FWndHandle, MsgID, Code, SubCode);
end;

function TksComPort.ReadFromCache(var Buf; Count: Integer): Integer;
var
  BytesToTest, BytesTested: Integer;

begin
  Result:= 0;
  if (FReadCache.UsedSize > 0) then begin
    BytesToTest:= FReadCache.UsedSize;
    if BytesToTest > Count then BytesToTest:= Count;
    BytesTested:= ReadStop(BytesToTest);
    if BytesTested > 0 then begin
//      Result:= FReadCache.Read(Buf, BytesTested);
      Move(FReadCache.Buffer^, Buf, BytesTested);
      Dec(FReadCache.UsedSize, BytesTested);
      if (FReadCache.UsedSize > 0) then
        Move(PByteArray(FReadCache.Buffer)^[BytesTested], FReadCache.Buffer^,
             FReadCache.UsedSize);
      Result:= BytesTested;
    end;
  end;
end;

function TksComPort.Read(var Buf; Count: Integer): Integer;
var
//  BytesToTest, BytesTested: Integer;
  BufPtr: PByte;

begin
  Result:= 0;
  FReadStopped:= False;
  if (FPortHandle = INVALID_HANDLE_VALUE) or (Count <= 0) then Exit;
  BufPtr:= @Buf;

// Check if the data we read is already in read cache
  Result:= ReadFromCache(Buf, Count);
  if FReadStopped or (Result = Count) then Exit;
  Inc(BufPtr, Result);

// read to cache
  FReadCache.UsedSize:= FRBuffer.Read(FReadCache.Buffer^, FReadCache.BufSize);

  Inc(Result, ReadFromCache(BufPtr^, Count - Result));
end;

function TksComPort.Read(var Buf; Count: Integer; TimeOut: LongWord): Integer;
begin
  FReadStopped:= False;
  if (FPortHandle = INVALID_HANDLE_VALUE) or (Count <= 0) then begin
    Result:= 0;
    Exit;
  end;
  Result:= Read(Buf, Count);
  if (Result = Count) or (TimeOut = 0) or FReadStopped then Exit;
  FPendingRead:= True;
  try
    FPendingPtr:= @Buf;
    Inc(FPendingPtr, Result);
    FPendingCount:= Count - Result;
    UpdateTimer(TimeOut);
    repeat
      Application.HandleMessage;
    until FTimerFired or not FPendingRead;
    Result:= Count - FPendingCount;
  finally
    FPendingRead:= False;
  end;
end;

function TksComPort.ReadStop(Count: Integer): Integer;
var
  TmpPtr: PByte;
  Stop: Boolean;

begin
  Stop:= False;
  Result:= Count;
  if Assigned(FOnReadStop) then begin
    TmpPtr:= FReadCache.Buffer;
    while (Count > 0) and not Stop do begin
      FOnReadStop(Self, TmpPtr^, Stop);
      Inc(TmpPtr);
      Dec(Count);
//      if Stop then Break;
    end;
    Result:= Result - Count;
  end;
  FReadStopped:= Stop;
end;

function TksComPort.Write(const Buf; Count: Integer): Integer;
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) or (Count <= 0)
    then Result:= 0
    else Result:= FWBuffer.Write(Buf, Count);
  SetEvent(FWriteEvent);
end;

function TksComPort.Write(const Buf; Count: Integer; TimeOut: LongWord): Integer;
begin
  FPendingCount:= Write(Buf, Count);
  if (FPendingCount = 0) then begin
    Result:= 0;
    Exit;
  end
  else begin
    FPendingWrite:= True;
    try
      UpdateTimer(TimeOut);
      repeat
        Application.HandleMessage;
//        Application.ProcessMessages;
      until FTimerFired or not FPendingWrite;
      Result:= Count - FPendingCount;
    finally
      FPendingWrite:= False;
    end;
  end;
end;

function TksComPort.Connected: Boolean;
begin
  Result:= FPortHandle <> INVALID_HANDLE_VALUE;
end;

procedure TksComPort.ClearRBuf;
begin
  FRBuffer.Clear;
end;

procedure TksComPort.ClearWBuf;
begin
  FWBuffer.Clear;
end;

procedure TksComPort.Close;
begin
  if FPortHandle <> INVALID_HANDLE_VALUE then begin
    SetEvent(FExitEvent);
    FEventThread.Free;
    FEventThread:= nil;
    FReadThread.Free;
    FReadThread:= nil;
    FWriteThread.Free;
    FWriteThread:= nil;
    ResetEvent(FExitEvent);
    CloseHandle(FRXEvent);
    CloseHandle(FWriteEvent);
    CloseHandle(FExitEvent);
//    DeleteCriticalSection(FErrLock);
    CloseHandle(FPortHandle);
    FReadCache.AllocBuffer(0);
    FRBuffer.Free;
    FWBuffer.Free;
//    FReadLog.Free;
//    FReadLog:= nil;
    FPortHandle:= INVALID_HANDLE_VALUE;
  end;
end;

function TksComPort.GetPortMask(var Mask: LongWord): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommMask(FPortHandle, Mask);
end;

function TksComPort.GetPortState(var DCB: TDCB): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommState(FPortHandle, DCB);
end;

function TksComPort.GetPortTimeouts(var Timeouts: TCommTimeouts): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
           and GetCommTimeouts(FPortHandle, Timeouts);
end;

procedure TksComPort.SetRBufSize(Value: Integer);
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) then begin
    if (Value < 64) then Value:= 64
    else if (Value > 64*1024) then Value:= 64*1024;
    FRBufSize:= Value;
  end;
end;

procedure TksComPort.SetWBufSize(Value: Integer);
begin
  if (FPortHandle = INVALID_HANDLE_VALUE) then begin
    if (Value < 64) then Value:= 64
    else if (Value > 64*1024) then Value:= 64*1024;
    FWBufSize:= Value;
  end;
end;

procedure TksComPort.SetBaudRate(const Value: LongWord);
var
  DCB: TDCB;

begin
  FBaudRate:= Value;
  if GetPortState(DCB) then begin
    DCB.BaudRate:= FBaudRate;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetByteSize(const Value: Byte);
var
  DCB: TDCB;

begin
  FByteSize:= Value;
  if GetPortState(DCB) then begin
    DCB.ByteSize:= FByteSize;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetParity(const Value: Byte);
var
  DCB: TDCB;

begin
  FParity:= Value;
  if GetPortState(DCB) then begin
    DCB.Parity:= FParity;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetStopBits(const Value: Byte);
var
  DCB: TDCB;

begin
  FStopBits:= Value;
  if GetPortState(DCB) then begin
    DCB.StopBits:= FStopBits;
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetRTSControl(const Value: LongWord);
var
  DCB: TDCB;

begin
  FRTSControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFCF) or (Value shl 4);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDSRSensitivity(const Value: Boolean);
var
  DCB: TDCB;

begin
  FDSRSensitivity:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFBF) or LongWord(Ord(Value) shl 6);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDTRControl(const Value: LongWord);
var
  DCB: TDCB;

begin
  FDTRControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFCFFF) or (Value shl 12);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetLogger(const Value: TksComLogger);
begin
  if not Connected then
    FLogger:= Value;
end;

procedure TksComPort.SetParityCheck(const Value: Boolean);
var
  DCB: TDCB;

begin
  FParityCheck:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFFD) or LongWord(Ord(Value) shl 1);
    SetCommState(FPortHandle, DCB);
  end;
end;

function TksComPort.SetPortState(const DCB: TDCB): Boolean;
begin
  Result:= (FPortHandle <> INVALID_HANDLE_VALUE)
    and SetCommState(FPortHandle, DCB);
  if Result then UpdateProperties(DCB);
end;

procedure TksComPort.SetCTSControl(const Value: Boolean);
var
  DCB: TDCB;

begin
  FCTSControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFFB) or LongWord(Ord(Value) shl 2);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.SetDSRControl(const Value: Boolean);
var
  DCB: TDCB;

begin
  FDSRControl:= Value;
  if GetPortState(DCB) then begin
    DCB.Flags:= (DCB.Flags and $FFFFFFF7) or LongWord(Ord(Value) shl 3);
    SetCommState(FPortHandle, DCB);
  end;
end;

procedure TksComPort.UpdateDCB(var DCB: TDCB);
begin
  DCB.BaudRate:= FBaudRate;
  DCB.ByteSize:= FByteSize;
  DCB.Parity:= FParity;
  DCB.StopBits:= FStopBits;
  DCB.Flags:= (DCB.Flags and $FFFF8001)
              or (FDTRControl shl 4) or (FRTSControl shl 12);
  if FParityCheck then DCB.Flags:= DCB.Flags or $02;
  if FCTSControl then DCB.Flags:= DCB.Flags or $04;
  if FDSRControl then DCB.Flags:= DCB.Flags or $08;
  if FDSRSensitivity then DCB.Flags:= DCB.Flags or $40;
end;

procedure TksComPort.UpdateProperties(const DCB: TDCB);
begin
  FBaudRate:= DCB.BaudRate;
  FByteSize:= DCB.ByteSize;
  FParity:= DCB.Parity;
  FStopBits:= DCB.StopBits;
  FParityCheck:= DCB.Flags and $02 <> 0;
  FCTSControl:= DCB.Flags and $04 <> 0;
  FDSRControl:= DCB.Flags and $08 <> 0;
  FDTRControl:= (DCB.Flags shr 4) and $03;
  FDSRSensitivity:= DCB.Flags and $40 <> 0;
  FRTSControl:= (DCB.Flags shr 12) and $03;
end;

procedure TksComPort.UpdateTimer(TimeOut: LongWord);
begin
  KillTimer(FWndHandle, 1);
  FTimerFired:= False;
  if (TimeOut <> 0) and (TimeOut <> INFINITE) then begin
    if SetTimer(FWndHandle, 1, TimeOut, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
  end;
end;

procedure TksComPort.WndProc(var Msg: TMessage);
begin
  if Msg.Msg = MsgID then begin
    try
      HandleMessage(Msg);
    except
      Application.HandleException(Self);
    end
  end
  else if Msg.Msg = WM_TIMER then begin
    try
      UpdateTimer(0);
      FTimerFired:= True;
    except
      Application.HandleException(Self);
    end
  end
  else
    Msg.Result:= DefWindowProc(FWndHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

{ TksComStreamLogger }

constructor TksComStreamLogger.Create;
var
  I: Integer;

begin
  for I:= 0 to 2 do
    InitializeCriticalSection(FLock[I]);
  for I:= 0 to 3 do
    FStream[I]:= CreateStream(I);
  FActive:= True;
end;

destructor TksComStreamLogger.Destroy;
var
  I: Integer;

begin
  FActive:= False;
  for I:= 0 to 3 do
    FStream[I].Free;
  for I:= 0 to 2 do
    DeleteCriticalSection(FLock[I]);
  inherited Destroy;
end;

{
function TksComStreamLogger.ResetStream(Index: Integer): TStream;
var
  Stream: TStream;

begin
  Stream:= LogStream(Index);
  if Assigned(Stream) then Stream.Position:= 0;
end;
}

procedure TksComStreamLogger.Reset;
var
  I: Integer;
  Stream: TStream;

begin
  for I:= 0 to 3 do begin
    if I > 0 then
      EnterCriticalSection(FLock[I-1]);
    try
      Stream:= FStream[I];
      if Assigned(Stream) then Stream.Seek(0, soFromBeginning);
      FItemCount[I]:= 0;
    finally
      if I > 0 then
        LeaveCriticalSection(FLock[I-1]);
    end;
  end;
end;

{
function TksComStreamLogger.LogStream(Index: Integer): TStream;
begin
  case Index of
    0..3: Result:= FStream[Index];
  else
    Result:= nil;
  end;
end;
}

procedure CopyData(Source, Target: TStream; Count: Integer);
var
  Buf: packed array[0..1023] of Byte;
  DataSize: Integer;

begin
  while Count > 0 do begin
    DataSize:= Count;
    if DataSize > SizeOf(Buf) then DataSize:= SizeOf(Buf);
    Source.ReadBuffer(Buf, DataSize);
    Target.WriteBuffer(Buf, DataSize);
    Dec(Count, DataSize);
  end;
end;

{
procedure TksComStreamLogger.Lock;
var
  I: Integer;

begin
  for I:= 0 to 2 do
    EnterCriticalSection(FLock[I]);
end;

procedure TksComStreamLogger.UnLock;
var
  I: Integer;

begin
  for I:= 2 downto 0 do
    LeaveCriticalSection(FLock[I]);
end;
}

procedure TksComStreamLogger.LogData(AFlags: LongWord; const Buf; BufSize: Integer);
var
  ItemBuf: packed array[0..SizeOf(TLogItem) + 512 - 1] of Byte;
  Stream: TStream;
  Index: Integer;
  P: PLogItem;

begin
  if BufSize <= 512 then P:= @ItemBuf
  else GetMem(P, SizeOf(TLogItem) + BufSize);
  try
    P^.Size:= SizeOf(TLogItem) + BufSize;
    P^.Flags:= AFlags;
    P^.Occured:= Now;
    Move(Buf, P^.Data, BufSize);
    Index:= AFlags and $3;
    Stream:= FStream[Index];
    if Assigned(Stream) and FActive then begin
      if Index > 0 then
        EnterCriticalSection(FLock[Index-1]);
      try
        Stream.WriteBuffer(P^, SizeOf(TLogItem) + BufSize);
        Inc(FItemCount[Index]);
      finally
        if Index > 0 then
          LeaveCriticalSection(FLock[Index-1]);
      end;
    end;
  finally
    if BufSize > 512 then
      FreeMem(P, SizeOf(TLogItem) + BufSize);
  end;
end;

procedure TksComStreamLogger.SaveToStream(Target: TStream);
var
  Header: TLogHeader;
  Count: Integer;
  SavePos: Integer;
  I: Integer;

begin
  Header.StartTime:= FStartTime;
  Header.StopTime:= FStopTime;
  Header.SizeOfChar:= SizeOf(Char);
  Header.CommentLen:= Length(FComment);
  Target.WriteBuffer(Header, SizeOf(Header));
  if Header.CommentLen > 0 then Target.Write(PChar(FComment)^,
    Header.CommentLen * SizeOf(Char));

// Copy user items (main stream)
  if FItemCount[0] > 0 then begin
    SavePos:= FStream[0].Seek(0, soFromCurrent);
    Count:= FStream[0].Seek(0, soFromEnd);
    FStream[0].Seek(0, soFromBeginning);
    Target.CopyFrom(FStream[0], Count);
    FStream[0].Seek(SavePos, soFromBeginning);
  end;

  for I:= 0 to 3 do begin
    if FItemCount[I] > 0 then begin
      if I > 0 then
        EnterCriticalSection(FLock[I-1]);
      try
        SavePos:= FStream[I].Seek(0, soFromCurrent);
        Count:= FStream[I].Seek(0, soFromEnd);
        FStream[I].Seek(0, soFromBeginning);
        Target.CopyFrom(FStream[I], Count);
        FStream[I].Seek(SavePos, soFromBeginning);

      finally
        if I > 0 then
          LeaveCriticalSection(FLock[I-1]);
      end;
    end;
  end;

end;

{
procedure TksComStreamLogger.SaveToStream(Target: TStream; const Comment: string);
var
  Header: TLogHeader;
  DataSize: Integer;
  ItemCount: Integer;
  SavePos: Integer;
  Index: array[0..3] of Integer;
  I: Integer;
  Completed: Boolean;
  Items: array[0..3] of TLogItem;
  MinIndex: Integer;

begin
  Header.StartTime:= FStartTime;
  Header.StopTime:= FStopTime;
  Header.SizeOfChar:= SizeOf(Char);
  Header.CommentLen:= Length(Comment);
  Target.WriteBuffer(Header, SizeOf(Header));
  if Header.CommentLen > 0 then Target.Write(PChar(Comment)^,
    Header.CommentLen * SizeOf(Char));
  Lock;
  try
    ItemCount:= 0;
    for I:= 0 to 3 do
      ItemCount:= ItemCount + FItemCount[I];

    SavePos:= Target.Position;
    Target.WriteBuffer(ItemCount, SizeOf(ItemCount));

    for I:= 0 to 3 do begin
      Index[I]:= 0;
      if FItemCount[I] > 0 then begin
        LogStream(I).Position:= 0;
        LogStream(I).ReadBuffer(Items[I], SizeOf(TLogItem));
      end;
    end;

    repeat
      Completed:= True;
      for I:= 0 to 3 do begin
        if Index[I] < FItemCount[I] then begin
          if Completed then begin
            MinIndex:= I;
            Completed:= False;
          end
          else begin
            if Items[I].Occured < Items[MinIndex].Occured then
              MinIndex:= I;
          end;
        end;
      end;
      if not Completed then begin
        Target.WriteBuffer(Items[MinIndex], SizeOf(TLogItem));
        CopyData(LogStream(MinIndex), Target,
                   Items[MinIndex].Size - SizeOf(TLogItem));
        Inc(Index[MinIndex], 1);
        if Index[MinIndex] < FItemCount[MinIndex] then
          LogStream(MinIndex).ReadBuffer(Items[MinIndex], SizeOf(TLogItem));
      end;
    until Completed;

  finally
    UnLock;
  end;
end;
}

procedure TksComStreamLogger.SaveToFile(const AFileName: string);
var
  Stream: TStream;

begin
  Stream:= TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

{ TksComMemoryLogger }

function TksComMemoryLogger.CreateStream(Index: Integer): TStream;
begin
  Result:= TMemoryStream.Create;
end;

end.
