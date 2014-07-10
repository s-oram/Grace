{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksClasses;

{$I ksTools.inc}

interface

uses
  RTLConsts, Windows, SysUtils, Classes, ksUtils;

{ TksRingBuffer }

type
  TksRingBuffer = class
  private
    FLock: TRTLCriticalSection;
    FBuffer: PByte;
    FSize: Integer;
    FOrigin: Integer;
    FLength: Integer;
  public
    constructor Create(ASize: Integer);
    destructor Destroy; override;
    function Get(var Buf; Count: Integer): Integer;
    function Read(var Buf; Count: Integer): Integer;
    function Write(const Buf; Count: Integer): Integer;
    procedure Clear;
    function Length: Integer;

    property Size: Integer read FSize;
  end;

{ TksBytesStream }

  TksBytesStream = class(TMemoryStream)
  private
    function GetBytes: TBytes;
  public
    constructor Create(const ABytes: TBytes); overload;
    property Bytes: TBytes read GetBytes;
  end;

{ TksFiler }

type
  TksFiler = class
  private
    FBitBuffer: LongWord;
    FBitBufLen: Integer;
    FBuffer: Pointer;
    FBufPos: Integer;
    FBufSize: Integer;
    FStream: TStream;
  public
    constructor Create(AStream: TStream; ABufSize: Integer);
    destructor Destroy; override;
    property Stream: TStream read FStream;
  end;

type
  TksFlushEvent = procedure(Sender: TObject; Buf: Pointer; BufSize: Integer)
          of object;

  TksReader = class(TksFiler)
  private
    FAfterFlush: TksFlushEvent;
    FBeforeFlush: TksFlushEvent;
    FBufEnd: Integer;
    FSilent: Boolean;
    function GetPosition: Int64;
    procedure ReadBuffer(Quiet: Boolean);
    procedure SetPosition(const Value: Int64);
  public
    destructor Destroy; override;
    procedure DumpBits(Count: Integer);
    procedure FlushBuffer;
    function GetBits(Count: Integer): LongWord;
    function Read(var Buf; Count: Longint): LongInt;
    function ReadBits(Count: Integer): LongWord;
    function ReadByte: Byte;
    procedure ResetBits;
    function TryReadByte(var B: Byte): Boolean;
    property AfterFlush: TksFlushEvent read FAfterFlush write FAfterFlush;
    property BeforeFlush: TksFlushEvent read FBeforeFlush write FBeforeFlush;
    property Position: Int64 read GetPosition write SetPosition;
    property Silent: Boolean read FSilent write FSilent;
  end;

  TksFileReader = class(TksReader)
  public
    constructor Create(const FileName: string; BufSize: Integer);
    destructor Destroy; override;
  end;

type
  TksWriter = class(TksFiler)
  private
    FBeforeFlush: TksFlushEvent;
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
  public
    destructor Destroy; override;
    function CopyFrom(Reader: TksReader; Count: Integer): Integer;
    procedure FlushBits;
    procedure FlushBuffer;
    procedure Write(const Buf; Count: Longint);
    procedure WriteBits(Value: LongWord; Count: Integer);
    procedure WriteByte(B: Byte);
    property BeforeFlush: TksFlushEvent read FBeforeFlush write FBeforeFlush;
    property Position: Int64 read GetPosition write SetPosition;
  end;

  TksFileWriter = class(TksWriter)
  public
    constructor Create(const FileName: string; BufSize: Integer);
    destructor Destroy; override;
  end;


implementation

{ TksRingBuffer }

constructor TksRingBuffer.Create(ASize: Integer);
begin
  GetMem(FBuffer, ASize);
  FSize:= ASize;
  InitializeCriticalSection(FLock);
end;

destructor TksRingBuffer.Destroy;
begin
  DeleteCriticalSection(FLock);
  FreeMem(FBuffer, FSize);
  inherited Destroy;
end;

procedure TksRingBuffer.Clear;
begin
  EnterCriticalSection(FLock);
  FLength:= 0;
  FOrigin:= 0;
  LeaveCriticalSection(FLock);
end;

function TksRingBuffer.Length: Integer;
begin
  EnterCriticalSection(FLock);
  Result:= FLength;
  LeaveCriticalSection(FLock);
end;

function TksRingBuffer.Get(var Buf; Count: Integer): Integer;
var
  P1, P2: PByte;
  N: Integer;

begin
  EnterCriticalSection(FLock);
  try
    if Count > FLength then Count:= FLength;
    if Count > 0 then begin
{$IFDEF OldVersion}
      P1:= @PChar(FBuffer)[FOrigin];
{$ELSE}
      P1:= @FBuffer[FOrigin];
{$ENDIF}
      P2:= @Buf;
      if FOrigin + Count <= FSize then begin
        Move(P1^, P2^, Count);
      end
      else begin
        N:= FSize - FOrigin;
        Move(P1^, P2^, N);
        Inc(P2, N);
        P1:= FBuffer;
        N:= Count - N;
        Move(P1^, P2^, N);
      end;
    end;
    Result:= Count;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TksRingBuffer.Read(var Buf; Count: Integer): Integer;
var
  P1, P2: PByte;
  N: Integer;

begin
  EnterCriticalSection(FLock);
  try
    if Count > FLength then Count:= FLength;
    if Count > 0 then begin
      Dec(FLength, Count);
{$IFDEF OldVersion}
      P1:= @PChar(FBuffer)[FOrigin];
{$ELSE}
      P1:= @FBuffer[FOrigin];
{$ENDIF}
      P2:= @Buf;
      if FOrigin + Count <= FSize then begin
        Move(P1^, P2^, Count);
        Inc(FOrigin, Count);
        if (FOrigin = FSize) then FOrigin:= 0;
      end
      else begin
        N:= FSize - FOrigin;
        Move(P1^, P2^, N);
        Inc(P2, N);
        P1:= FBuffer;
        N:= Count - N;
        Move(P1^, P2^, N);
        FOrigin:= N;
      end;
    end;
    Result:= Count;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TksRingBuffer.Write(const Buf; Count: Integer): Integer;
var
  P1, P2: PByte;
  Org, N: Integer;

begin
  Result:= 0;
//  if (Count <= 0) then Exit;
  EnterCriticalSection(FLock);
  try
    if FLength < FSize then begin
      if FLength = 0 then FOrigin:= 0;
      if Count > FSize - FLength then Count:= FSize - FLength;
      Org:= FOrigin + FLength;
      if Org >= FSize then Org:= Org - FSize;
      Inc(FLength, Count);
      P1:= @Buf;
{$IFDEF OldVersion}
      P2:= @PChar(FBuffer)[Org];
{$ELSE}
      P2:= @FBuffer[Org];
{$ENDIF}
      if Org + Count <= FSize then Move(P1^, P2^, Count)
      else begin
        N:= FSize - Org;
        Move(P1^, P2^, N);
        Inc(P1, N);
        P2:= FBuffer;
        Move(P1^, P2^, Count - N);
      end;
      Result:= Count;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TksBytesStream }

constructor TksBytesStream.Create(const ABytes: TBytes);
var
  L: Integer;

begin
  inherited Create;
  L:= Length(ABytes);
  if L > 0 then begin
    SetSize(L);
    Move(ABytes[0], Memory^, L);
  end;
end;

function TksBytesStream.GetBytes: TBytes;
begin
  SetLength(Result, Size);
  Move(Memory^, Result[0], Size);
end;

{ TksFiler }

constructor TksFiler.Create(AStream: TStream; ABufSize: Integer);
begin
  FStream:= AStream;
  GetMem(FBuffer, ABufSize);
  FBufSize:= ABufSize;
end;

destructor TksFiler.Destroy;
begin
  if FBufSize > 0 then FreeMem(FBuffer, FBufSize);
  inherited Destroy;
end;

{ TksReader }

destructor TksReader.Destroy;
begin
  FlushBuffer;
  inherited Destroy;
end;

procedure TksReader.DumpBits(Count: Integer);
begin
  FBitBuffer:= FBitBuffer shr Count;
  Dec(FBitBufLen, Count);
end;

procedure TksReader.FlushBuffer;
begin
  if Assigned(FBeforeFlush) then FBeforeFlush(Self, FBuffer, FBufPos);
  FStream.Position:= Position;
  FBufPos:= 0;
  FBufEnd:= 0;
end;

function TksReader.GetBits(Count: Integer): LongWord;
var
  Value: Byte;
begin
  while FBitBufLen < Count do begin
    if FBufPos = FBufEnd then ReadBuffer(False);
    Value:= PByte(LongInt(FBuffer) + FBufPos)^;
    Inc(FBufPos);
    FBitBuffer:= FBitBuffer or (Value shl FBitBufLen);
    Inc(FBitBufLen, 8);
  end;
  Result:= FBitBuffer and BitMask[Count];
end;

function TksReader.GetPosition: Int64;
begin
  Result:= FStream.Position - (FBufEnd - FBufPos);
end;

function TksReader.Read(var Buf; Count: Longint): LongInt;
asm
          PUSH    ESI
          PUSH    EDI
          PUSH    EBX
          PUSH    ECX
          MOV     EDI,EDX
          MOV     EBX,ECX
          MOV     ESI,EAX
          JMP     @@6
  @@1:    MOV     ECX,[ESI].TksReader.FBufEnd
          SUB     ECX,[ESI].TksReader.FBufPos
          JA      @@2
          MOV     EAX,ESI
          MOV     DL,[ESI].TksReader.FSilent
          CALL    TksReader.ReadBuffer
          MOV     ECX,[ESI].TksReader.FBufEnd
          OR      ECX,ECX
          JE      @@7
  @@2:    CMP     ECX,EBX
          JB      @@3
          MOV     ECX,EBX
  @@3:    PUSH    ESI
          SUB     EBX,ECX
          MOV     EAX,[ESI].TksReader.FBuffer
          ADD     EAX,[ESI].TksReader.FBufPos
          ADD     [ESI].TksReader.FBufPos,ECX
          MOV     ESI,EAX
          MOV     EDX,ECX
          SHR     ECX,2
          CLD
          REP     MOVSD
          MOV     ECX,EDX
          AND     ECX,3
          REP     MOVSB
          POP     ESI
  @@6:    OR      EBX,EBX
          JNE     @@1
  @@7:    POP     EAX
          SUB     EAX,EBX
          POP     EBX
          POP     EDI
          POP     ESI
end;

function TksReader.ReadBits(Count: Integer): LongWord;
var
  Value: Byte;
begin
  while FBitBufLen < Count do begin
    if FBufPos = FBufEnd then ReadBuffer(False);
    Value:= PByte(LongInt(FBuffer) + FBufPos)^;
    Inc(FBufPos);
  //    if FReversed then Value:= SwapTable[Value];
    FBitBuffer:= FBitBuffer or (Value shl FBitBufLen);
    Inc(FBitBufLen, 8);
  end;
  Result:= FBitBuffer and BitMask[Count];
  FBitBuffer:= FBitBuffer shr Count;
  Dec(FBitBufLen, Count);
end;

procedure TksReader.ReadBuffer(Quiet: Boolean);
begin
  if Assigned(FBeforeFlush) then FBeforeFlush(Self, FBuffer, FBufPos);
  FBufPos:= 0;
  FBufEnd:= FStream.Read(FBuffer^, FBufSize);
  if (FBufEnd = 0) and not Quiet
    then raise EReadError.CreateRes(@SReadError);
  if Assigned(FAfterFlush) then FAfterFlush(Self, FBuffer, FBufEnd);
end;

function TksReader.ReadByte: Byte;
begin
  if FBufPos = FBufEnd then ReadBuffer(False);
  Result:= PByte(LongInt(FBuffer) + FBufPos)^;
  Inc(FBufPos);
end;

procedure TksReader.ResetBits;
begin
  FBitBuffer:= 0;
  FBitBufLen:= 0;
end;

procedure TksReader.SetPosition(const Value: Int64);
var
  StreamPosition: Int64;
begin
  StreamPosition:= FStream.Position;
  { Flush the buffer if the repostion is outside the buffer range }
  if (Value < StreamPosition - FBufEnd) or (Value > StreamPosition) then begin
    if Assigned(FBeforeFlush) then FBeforeFlush(Self, FBuffer, FBufPos);
    FStream.Position:= Value;
    FBufPos:= 0;
    FBufEnd:= 0;
  end
  else FBufPos:= Value - StreamPosition + FBufEnd;
end;

function TksReader.TryReadByte(var B: Byte): Boolean;
begin
  if FBufPos = FBufEnd then ReadBuffer(True);
  Result:= FBufEnd <> 0;
  if Result then begin
    B:= PByte(LongInt(FBuffer) + FBufPos)^;
    Inc(FBufPos);
  end;
end;

{ TksFileReader }

constructor TksFileReader.Create(const FileName: string; BufSize: Integer);
var
  Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  inherited Create(Stream, BufSize);
end;

destructor TksFileReader.Destroy;
var
  Stream: TStream;
begin
  Stream:= FStream;
  inherited Destroy;
  Stream.Free;
end;

{ TksWriter }

destructor TksWriter.Destroy;
begin
  FlushBits;
  FlushBuffer;
  inherited Destroy;
end;

function TksWriter.CopyFrom(Reader: TksReader; Count: Integer): Integer;
var
  Save: Integer;
  BytesToCopy: Integer;
begin
  Save:= Count;
  Reader.FlushBuffer;
  while Count>0 do begin
    Reader.ReadBuffer(Reader.Silent);
    BytesToCopy:= Reader.FBufEnd;
    if BytesToCopy = 0 then Break;
    if BytesToCopy > Count then BytesToCopy:= Count;
    Reader.FBufPos:= BytesToCopy;
    Write(Reader.FBuffer^, BytesToCopy);
    Dec(Count, BytesToCopy);
  end;
  Result:= Save - Count;
end;

procedure TksWriter.FlushBits;
var
  B: Byte;
begin
  while FBitBufLen > 0 do begin
    B:= Byte(FBitBuffer);
  //    if FReversed then B:= SwapTable[B];
    if FBufPos = FBufSize then FlushBuffer;
    PByte(LongInt(FBuffer) + FBufPos)^:= B;
    Inc(FBufPos);
    FBitBuffer:= FBitBuffer shr 8;
    Dec(FBitBufLen, 8);
  end;
  FBitBufLen:= 0;
end;

procedure TksWriter.FlushBuffer;
var
  BytesWritten: Integer;
begin
  if FBufPos <> 0 then begin
    if Assigned(FBeforeFlush) then FBeforeFlush(Self, FBuffer, FBufPos);
    BytesWritten:= FStream.Write(FBuffer^, FBufPos);
    if (BytesWritten <> FBufPos) then
      raise EWriteError.CreateRes(@SWriteError);
    FBufPos:= 0;
  end;
end;

function TksWriter.GetPosition: Int64;
begin
  Result:= FStream.Position + FBufPos;
end;

procedure TksWriter.SetPosition(const Value: Int64);
var
  StreamPosition: Int64;
begin
  StreamPosition:= FStream.Position;
  { Only flush the buffer if the repostion is outside the buffer range }
  if (Value < StreamPosition) or (Value > StreamPosition + FBufPos) then begin
    FlushBuffer;
    FStream.Position:= Value;
  end
  else FBufPos:= Value - StreamPosition;
end;

procedure TksWriter.Write(const Buf; Count: Longint);
asm
          PUSH    ESI
          PUSH    EDI
          PUSH    EBX
          MOV     ESI,EDX
          MOV     EBX,ECX
          MOV     EDI,EAX
          JMP     @@6
  @@1:    MOV     ECX,[EDI].TksWriter.FBufSize
          SUB     ECX,[EDI].TksWriter.FBufPos
          JA      @@2
          MOV     EAX,EDI
          CALL    TksWriter.FlushBuffer
          MOV     ECX,[EDI].TksWriter.FBufSize
  @@2:    CMP     ECX,EBX
          JB      @@3
          MOV     ECX,EBX
  @@3:    SUB     EBX,ECX
          PUSH    EDI
          MOV     EAX,[EDI].TksWriter.FBuffer
          ADD     EAX,[EDI].TksWriter.FBufPos
          ADD     [EDI].TksWriter.FBufPos,ECX
  @@5:    MOV     EDI,EAX
          MOV     EDX,ECX
          SHR     ECX,2
          CLD
          REP     MOVSD
          MOV     ECX,EDX
          AND     ECX,3
          REP     MOVSB
          POP     EDI
  @@6:    OR      EBX,EBX
          JNE     @@1
          POP     EBX
          POP     EDI
          POP     ESI
end;

procedure TksWriter.WriteBits(Value: LongWord; Count: Integer);
var
  B: Byte;

begin
  FBitBuffer:= FBitBuffer or ((Value and BitMask[Count]) shl FBitBufLen);
  Inc(FBitBufLen, Count);
  while (FBitBufLen >= 8) do begin
    B:= Byte(FBitBuffer);
    if FBufPos = FBufSize then FlushBuffer;
    PByte(LongInt(FBuffer) + FBufPos)^:= B;
    Inc(FBufPos);
    FBitBuffer:= FBitBuffer shr 8;
    Dec(FBitBufLen, 8);
  end;
end;

procedure TksWriter.WriteByte(B: Byte);
begin
  if FBufPos = FBufSize then FlushBuffer;
  PByte(LongInt(FBuffer) + FBufPos)^:= B;
  Inc(FBufPos);
end;

{ TksFileWriter }

constructor TksFileWriter.Create(const FileName: string; BufSize: Integer);
var
  Stream: TStream;
begin
  Stream:= TFileStream.Create(FileName, fmCreate);
  inherited Create(Stream, BufSize);
end;

destructor TksFileWriter.Destroy;
var
  Stream: TStream;
begin
  Stream:= FStream;
  inherited Destroy;
  Stream.Free;
end;

end.
