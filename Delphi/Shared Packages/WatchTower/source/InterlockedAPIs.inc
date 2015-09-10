{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2011 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

{$IFDEF CPUX86}

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

function InterlockedCompareExchange(var Target: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

function InterlockedCompareExchangePointer(var Target: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      JMP InterlockedCompareExchange
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      JMP   InterlockedAdd
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
asm
      MOV     ECX,EAX
      MOV     EAX,[ECX]
@@loop:
 LOCK CMPXCHG [ECX],EDX
      JNZ     @@loop
end;

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
asm
      JMP InterlockedExchange
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      JMP   InterlockedAdd
end;

{$ENDIF CPUX86}

{$IFDEF CPUX64}

function InterlockedExchangeAdd(var Addend: Integer; Value: Integer): Integer;
asm
      .NOFRAME
      MOV   EAX,EDX
 LOCK XADD  [RCX].Integer,EAX
end;

function InterlockedDecrement(var Addend: LongInt): LongInt;
asm
      .NOFRAME
      MOV   EAX,-1
 LOCK XADD  [RCX].Integer,EAX
      DEC   EAX
end;

function InterlockedIncrement(var Addend: LongInt): LongInt;
asm
      MOV   EAX,1
 LOCK XADD  [RCX].Integer,EAX
      INC   EAX
end;

function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      .NOFRAME
      MOV     EAX,R8d
 LOCK CMPXCHG [RCX].Integer,EDX
end;

function InterlockedCompareExchange64(var Destination: Int64; Exchange: Int64; Comparand: Int64): Int64; overload;
asm
      .NOFRAME
      MOV     RAX,R8
 LOCK CMPXCHG [RCX],RDX
end;

function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      .NOFRAME
      MOV     RAX,R8
 LOCK CMPXCHG [RCX],RDX
end;

function InterlockedExchangePointer(var Target: Pointer; Value: Pointer): Pointer;
asm
       .NOFRAME
  LOCK XCHG [RCX],RDX
       MOV RAX,RDX
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;// inline;
asm
       .NOFRAME
  LOCK XCHG [RCX],EDX
       MOV EAX,EDX
end;

{$ENDIF CPUX64}
