{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Events;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Collections,
  SysUtils,
  Spring,
  Spring.Events.Base,
  TypInfo;

type
  PMethod = ^TMethod;


  {$REGION 'TMethodInvocations'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
  ///	<summary>
  ///	  Internal Use.
  ///	</summary>
  TMethodInvocations = class
  private
    const
      paEAX = Word(0);
      paEDX = Word(1);
      paECX = Word(2);
      paStack = Word(3);

    type
      PParameterInfos = ^TParameterInfos;
      TParameterInfos = array[0..255] of ^PTypeInfo;

      PParameters = ^TParameters;
      TParameters = packed record
      public
{$IFNDEF CPUX64}
        Registers: array[paEDX..paECX] of Cardinal;
        EAXRegister: Cardinal;
        ReturnAddress: Pointer;
{$ENDIF}
        Stack: array[0..1023] of Byte;
      end;

      PMethodInfo = ^TMethodInfo;
      TMethodInfo = record
        ParamInfos: PParameterInfos;
        StackSize: Integer;
        CallConvention: TCallConv;
{$IFDEF CPUX64}
        RegisterFlag: Word;
{$ENDIF}
        constructor Create(typeData: PTypeData);
      end;

      TMethodInvokeEvent = procedure(Params: Pointer; StackSize: Integer) of object;
  private
    fMethodInfo: TMethodInfo;
    fMethodInvokeEvent: TMethodInvokeEvent;
  protected
    procedure InternalInvokeHandlers(Params: PParameters);
    procedure InvokeEventHandlerStub;
  public
    constructor Create(methodTypeData: PTypeData; methodInvokeEvent: TMethodInvokeEvent);
  end;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

  {$ENDREGION}


  {$REGION 'TEvent'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
  TEvent = class(TEventBase, TProc)
  private
    fInvocations: TMethodInvocations;
    fTypeInfo: PTypeInfo;
    function Cast(const handler): TMethod;
    procedure InternalInvoke(Params: Pointer; StackSize: Integer);
    procedure Invoke;
  public
    constructor Create(typeInfo: PTypeInfo);
    destructor Destroy; override;
  end;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

  {$ENDREGION}


  {$REGION 'TEvent<T>'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
  TEvent<T> = class(TEvent, IEvent<T>)
  private
    function GetInvoke: T;
  public
    constructor Create;

    procedure Add(handler: T); overload;
    procedure Remove(handler: T); overload;
    procedure ForEach(const action: TAction<T>);
  end;

  IMulticastNotifyEvent = IEvent<TNotifyEvent>;

  TMulticastNotifyEvent = TEvent<TNotifyEvent>;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

  {$ENDREGION}


implementation

uses
  Rtti,
  Spring.ResourceStrings;


{$REGION 'TMethodInfo'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
function AdditionalInfoOf(TypeData: PTypeData): Pointer;
var
  P: PByte;
  I: Integer;
begin
  P := @TypeData^.ParamList;
  // Skip parameter names and types
  for I := 1 to TypeData^.ParamCount do
  begin
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1 );
  end;
  if TypeData^.MethodKind = mkFunction then
    // Skip return type name and info
    Inc(P, P[0] + 1 + 4);
  Result := P;
end;

procedure InvokeMethod(const Method: TMethod;
  Parameters: Pointer; StackSize: Integer);
const
  PointerSize = SizeOf(Pointer);
  paEDX = Word(1);
  paECX = Word(2);
type
  TParameters = packed record
  public
{$IFNDEF CPUX64}
    Registers: array[paEDX..paECX] of Cardinal;
    EAXRegister: Cardinal;
    ReturnAddress: Pointer;
{$ENDIF}
    Stack: array[0..1023] of Byte;
  end;
{$IFNDEF CPUX64}
asm
  push ebp
  mov ebp,esp
  push eax // ebp-4 = Method
  push ebx
  mov ebx, edx // ebx = Parameters

  // if StackSize > 0
  test ecx,ecx
  jz @@no_stack

  // stack address alignment
  {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
  // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
  add ecx,PointerSize-1
  and ecx,not(PointerSize-1)
  and ecx,$ffff
  sub esp,ecx

  // put stack address as second parameter
  mov edx,esp

  // put params on stack as first parameter
  lea eax,[ebx].TParameters.Stack

  call Move

@@no_stack:
  mov edx,[ebx].TParameters.Registers.dword[0]
  mov ecx,[ebx].TParameters.Registers.dword[4]
  mov ebx,[ebp-$04]
  mov eax,[ebx].TMethod.Data
  call [ebx].TMethod.Code

  pop ebx
  pop eax
  mov esp,ebp
  pop ebp
end;
{$ELSE}
asm
  .params 60
  mov [rbp+$200],Method
  mov [rbp+$208],Parameters
  test r8,r8
  jz @@no_stack

  // put params on stack as first parameter
  lea rcx,[Parameters].TParameters.Stack

  // put stack address as second parameter
  mov rdx,rsp

  call Move

  mov rdx,[rbp+$208]

@@no_stack:
  mov rcx,[rdx].TParameters.Stack.qword[0]
  mov r8,[rdx].TParameters.Stack.qword[16]
  mov r9,[rdx].TParameters.Stack.qword[24]

  movsd xmm0,[rdx].TParameters.Stack.qword[0]
  movsd xmm1,[rdx].TParameters.Stack.qword[8]
  movsd xmm2,[rdx].TParameters.Stack.qword[16]
  movsd xmm3,[rdx].TParameters.Stack.qword[24]

  mov rdx,[rdx].TParameters.Stack.qword[8]

  mov rax,[rbp+$200]
  lea rax,[rax]
  mov rcx,[rax].TMethod.Data
  call [rax].TMethod.Code
end;
{$ENDIF}

constructor TMethodInvocations.TMethodInfo.Create(typeData: PTypeData);

  function PassByRef(P: PByte; ParamInfos: PParameterInfos; I: Integer): Boolean;
  begin
    Result := (TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [])
      and not (ParamInfos^[I]^.Kind in [tkFloat, tkMethod, tkInt64]);
  end;

  function Align4(Value: Integer): Integer;
  begin
    {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
    // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
    Result := (Value + 3) and not 3;
  end;

var
  P: PByte;
  I: Integer;
{$IFNDEF CPUX64}
  curReg: Integer;
  Size: Integer;
{$ENDIF}
begin
  P := AdditionalInfoOf(typeData);
  CallConvention := TCallConv(PByte(p)^);
  ParamInfos := PParameterInfos(Cardinal(P) + 1);

  StackSize := SizeOf(Pointer); // Self in stack
{$IFNDEF CPUX64}
  curReg := paStack;
  if CallConvention = ccReg then
  begin
    curReg := paEDX;
    StackSize := 0;
  end;
{$ENDIF}

  P := @typeData^.ParamList;

  for I := 0 to typeData^.ParamCount - 1 do
  begin
    if not Assigned(ParamInfos^[I]) then
      raise EInvalidOperationException.CreateRes(@SNoTypeInfo);
{$IFNDEF CPUX64}
    if PassByRef(P, ParamInfos, I) then
      Size := 4
    else
      Size := GetTypeSize(ParamInfos^[I]^);
    if (Size <= 4) and (curReg <= paECX) and (ParamInfos^[I]^.Kind <> tkFloat) then
      Inc(curReg)
    else
    begin
      if Size < 4 then
        Size := 4;
      Inc(StackSize, Align4(Size));
    end;
{$ELSE}
    if I < 3 then
    begin
      if ParamInfos^[I]^.Kind = tkFloat then
        RegisterFlag := RegisterFlag or (1 shl (I + 1));
    end;
    Inc(StackSize, 8);
{$ENDIF}
    Inc(P, 1 + P[1] + 1);
    Inc(P, P[0] + 1);
  end;

{$IFDEF CPUX64}
  if StackSize < 32 then
    StackSize := 32;
{$ENDIF}
end;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

{$ENDREGION}


{$REGION 'TMethodInvocations'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
constructor TMethodInvocations.Create(methodTypeData: PTypeData;
  methodInvokeEvent: TMethodInvokeEvent);
begin
  inherited Create;
  fMethodInfo := TMethodInfo.Create(methodTypeData);
  fMethodInvokeEvent := methodInvokeEvent;
end;

procedure TMethodInvocations.InternalInvokeHandlers(Params: PParameters);
begin
  if Assigned(fMethodInvokeEvent) then
    fMethodInvokeEvent(Params, fMethodInfo.StackSize);
end;

procedure TMethodInvocations.InvokeEventHandlerStub;
{$IFNDEF CPUX64}
const
  PtrSize = SizeOf(Pointer);
asm
        // is register conversion call ?
        CMP     BYTE PTR Self.fMethodInfo.CallConvention, ccReg
        JZ      @Begin
        Mov     EAX, [esp + 4]
@Begin:
        PUSH    EAX
        PUSH    ECX
        PUSH    EDX
        MOV     EDX,ESP
        CALL    InternalInvokeHandlers
        // Pop EDX and ECX off the stack while preserving all registers.
        MOV     [ESP+4],EAX
        POP     EAX
        POP     EAX
        POP     ECX		// Self
        Mov     EAX, ECX
        MOV     ECX,[ECX].fMethodInfo.StackSize
        TEST    ECX,ECX
        JZ      @@SimpleRet
        // Jump to the actual return instruction since it is most likely not just a RET
        //JMP     ECX    // Data Exec. Prevention: Jumping into a GetMem allocated memory block

        // stack address alignment
        {TODO -o##jwp -cOSX32/MACOS : Research 16-byte stack alignment: http://docwiki.embarcadero.com/RADStudio/XE5/en/Delphi_Considerations_for_Cross-Platform_Applications#Stack_Alignment_Issue_on_OS_X }
        // http://docwiki.embarcadero.com/RADStudio/XE5/en/Conditional_compilation_(Delphi)
        // In cdecl call conversion, the caller will clear the stack
        CMP     DWORD PTR [EAX].fMethodInfo.CallConvention, ccCdecl
        JZ      @@SimpleRet
        ADD     ECX, PtrSize - 1
        AND     ECX, NOT (PtrSize - 1)
        AND     ECX, $FFFF

        // clean up the stack
        PUSH    EAX                         // we need this register, so save it
        MOV     EAX,[ESP + 4]               // Load the return address
        MOV     [ESP + ECX + 4], EAX        // Just blast it over the first param on the stack
        POP     EAX
        ADD     ESP,ECX                     // This will move the stack back to where the moved
                                            // return address is now located. The next RET
                                            // instruction will do the final stack cleanup
@@SimpleRet:
end;
{$ELSE}
asm
        MOV     AX, WORD PTR [RCX].TMethodInvocations.fMethodInfo.RegisterFlag
@@FIRST:
        TEST    AX, $01
        JZ      @@SAVE_RCX
@@SAVE_XMM0:
        MOVSD   QWORD PTR [RSP+$08], XMM0
        JMP     @@SECOND
@@SAVE_RCX:
        MOV     QWORD PTR [RSP+$08], RCX

@@SECOND:
        TEST    AX, $02
        JZ      @@SAVE_RDX
@@SAVE_XMM1:
        MOVSD   QWORD PTR [RSP+$10], XMM1
        JMP     @@THIRD
@@SAVE_RDX:
        MOV     QWORD PTR [RSP+$10], RDX

@@THIRD:
        TEST    AX, $04
        JZ      @@SAVE_R8
@@SAVE_XMM2:
        MOVSD   QWORD PTR [RSP+$18], XMM2
        JMP     @@FORTH
@@SAVE_R8:
        MOV     QWORD PTR [RSP+$18], R8

@@FORTH:
        TEST    AX, $08
        JZ      @@SAVE_R9
@@SAVE_XMM3:
        MOVSD   QWORD PTR [RSP+$20], XMM3
        JMP     @@1
@@SAVE_R9:
        MOV     QWORD PTR [RSP+$20], R9

@@1:    LEA     RDX, QWORD PTR [RSP+$08]
        MOV     RAX, RCX
        SUB     RSP, $28
        CALL    InternalInvokeHandlers
        ADD     RSP, $28
end;
{$ENDIF}
{$ENDIF SUPPORTS_GENERIC_EVENTS}

{$ENDREGION}


{$REGION 'TEvent'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
procedure GetMethodTypeData(Method: TRttiMethod; var TypeData: PTypeData);

  procedure WriteByte(var Dest: PByte; b: Byte);
  begin
    Dest[0] := b;
    Inc(Dest);
  end;

  procedure WritePackedShortString(var Dest: PByte; const s: string);
{$IFNDEF NEXTGEN}
  begin
    PShortString(Dest)^ := ShortString(s);
    Inc(Dest, Dest[0] + 1);
  end;
{$ELSE}
  var
    buffer: TBytes;
  begin
    buffer := TEncoding.ANSI.GetBytes(s);
    if (Length(buffer) > 255) then SetLength(buffer, 255);
    Dest^ := Length(buffer);
    Inc(Dest);
    Move(buffer[0], Dest^, Length(buffer));
    Inc(Dest, Length(buffer));
  end;
{$ENDIF}

  procedure WritePointer(var Dest: PByte; p: Pointer);
  begin
    PPointer(Dest)^ := p;
    Inc(Dest, SizeOf(Pointer));
  end;

var
  params: TArray<TRttiParameter>;
  i: Integer;
  p: PByte;
begin
  TypeData.MethodKind := Method.MethodKind;
  params := Method.GetParameters;
  TypeData.ParamCount := Length(params);
  p := @TypeData.ParamList;
  for i := Low(params) to High(params) do
  begin
    WriteByte(p, Byte(params[i].Flags));
    WritePackedShortString(p, params[i].Name);
    WritePackedShortString(p, params[i].ParamType.Name);
  end;
  if method.MethodKind = mkFunction then
  begin
    WritePackedShortString(p, method.ReturnType.Name);
    WritePointer(p, method.ReturnType.Handle);
  end;
  WriteByte(p, Byte(method.CallingConvention));
  for i := Low(params) to High(params) do
  begin
    WritePointer(p, Pointer(NativeInt(params[i].ParamType.Handle) - SizeOf(Pointer)));
  end;
end;

procedure MethodReferenceToMethodPointer(const AMethodReference; const AMethodPointer);
type
  TVtable = array[0..3] of Pointer;
  PVtable = ^TVtable;
  PPVtable = ^PVtable;
begin
  // 3 is offset of Invoke, after QI, AddRef, Release
  PMethod(@AMethodPointer).Code := PPVtable(AMethodReference)^^[3];
  PMethod(@AMethodPointer).Data := Pointer(AMethodReference);
end;

constructor TEvent.Create(typeInfo: PTypeInfo);
var
  typeData: PTypeData;
  ctx: TRttiContext;
  method: TRttiMethod;
begin
  //TODO: If Embt ever adds TRttiMethodType.CreateImplementation this could be done in pure pascal way

  fTypeInfo := typeInfo;
  if not Assigned(typeInfo) then
    raise EInvalidOperationException.CreateRes(@SNoTypeInfo);

  inherited Create;

  if typeInfo.Kind = tkMethod then
  begin
    typeData := GetTypeData(typeInfo);
    fInvocations := TMethodInvocations.Create(typeData, InternalInvoke);
  end
  else if typeInfo.Kind = tkInterface then
  begin
    method := ctx.GetType(typeInfo).GetMethod('Invoke');
    if not Assigned(method) then
      raise EInvalidOperationException.CreateResFmt(@STypeParameterContainsNoRtti, [typeInfo.Name]);
    New(typeData);
    try
      GetMethodTypeData(method, typeData);
      fInvocations := TMethodInvocations.Create(typeData, InternalInvoke);
    finally
      Dispose(typeData);
    end;
  end
  else
    raise EInvalidOperationException.CreateResFmt(@STypeParameterShouldBeMethod, [typeInfo.Name]);

  fInvoke.Data := fInvocations;
  fInvoke.Code := @TMethodInvocations.InvokeEventHandlerStub;
end;

destructor TEvent.Destroy;
begin
  fInvocations.Free;
  inherited Destroy;
end;

function TEvent.Cast(const handler): TMethod;
begin
  if fTypeInfo.Kind = tkInterface then
    MethodReferenceToMethodPointer(handler, Result)
  else
    Result := PMethod(@handler)^;
end;

procedure TEvent.InternalInvoke(Params: Pointer; StackSize: Integer);
var
  i: Integer;
begin
  if Enabled then
    for i := 0 to Handlers.Count - 1 do
      InvokeMethod(Handlers[i], Params, StackSize);
end;

procedure TEvent.Invoke;
asm
{$IFDEF CPUX64}
  push [rcx].fInvoke.Code
  mov rcx,[rcx].fInvoke.Data
{$ELSE}
  push [eax].fInvoke.Code
  mov eax,[eax].fInvoke.Data
{$ENDIF}
end;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

{$ENDREGION}


{$REGION 'TEvent<T>'}

{$IFDEF SUPPORTS_GENERIC_EVENTS}
constructor TEvent<T>.Create;
begin
  inherited Create(TypeInfo(T));
end;

procedure TEvent<T>.ForEach(const action: TAction<T>);
begin
  inherited ForEach(TAction<TMethod>(action));
end;

procedure TEvent<T>.Add(handler: T);
begin
  inherited Add(Cast(handler));
end;

procedure TEvent<T>.Remove(handler: T);
begin
  inherited Remove(Cast(handler));
end;

function TEvent<T>.GetInvoke: T;
begin
  if fTypeInfo.Kind = tkInterface then
    TProc(PPointer(@Result)^) := Self
  else
    PMethod(@Result)^ := fInvoke;
end;
{$ENDIF SUPPORTS_GENERIC_EVENTS}

{$ENDREGION}


end.
