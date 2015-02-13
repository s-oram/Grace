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

{$I Spring.inc}

unit Spring.Reflection.Compatibility;

interface

uses
  Generics.Collections,
  Rtti,
  TypInfo;

type
  TMethodInvokeEvent = reference to procedure(Method: TRttiMethod;
    const Args: TArray<TValue>; out Result: TValue);

  TMethodIntercept = class
  private
    fImplementation: TMethodImplementation;
    fMethod: TRttiMethod;
    function GetCodeAddress: Pointer;
    function GetVirtualIndex: SmallInt;
  public
    constructor Create(const method: TRttiMethod;
      const callback: TMethodImplementationCallback);
    destructor Destroy; override;
    property CodeAddress: Pointer read GetCodeAddress;
    property Method: TRttiMethod read fMethod;
    property VirtualIndex: SmallInt read GetVirtualIndex;
  end;

  TMethodIntercepts = TObjectList<TMethodIntercept>;

  TVirtualInterfaceInvokeEvent = TMethodInvokeEvent;

  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    fVirtualMethodTable: Pointer;
    fInterfaceID: TGUID;
    fContext: TRttiContext;
    fMethodIntercepts: TMethodIntercepts;
    fOnInvoke: TVirtualInterfaceInvokeEvent;
    function Virtual_AddRef: Integer; stdcall;
    function Virtual_Release: Integer; stdcall;
    function VirtualQueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;

    procedure DoInvoke(UserData: Pointer;
      const Args: TArray<TValue>; out Result: TValue);
    procedure ErrorProc;
  public
    constructor Create(typeInfo: PTypeInfo); overload;
    constructor Create(typeInfo: PTypeInfo;
      invokeEvent: TVirtualInterfaceInvokeEvent); overload;
    destructor Destroy; override;

    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;

    property OnInvoke: TVirtualInterfaceInvokeEvent read fOnInvoke write fOnInvoke;
  end;

implementation

uses
  RTLConsts;


{$REGION 'TMethodIntercept'}

constructor TMethodIntercept.Create(const method: TRttiMethod;
  const callback: TMethodImplementationCallback);
begin
  fImplementation := method.CreateImplementation(Self, callback);
  fMethod := method;
end;

destructor TMethodIntercept.Destroy;
begin
  fImplementation.Free;
  inherited;
end;

function TMethodIntercept.GetCodeAddress: Pointer;
begin
  Result := fImplementation.CodeAddress;
end;

function TMethodIntercept.GetVirtualIndex: SmallInt;
begin
  Result := fMethod.VirtualIndex;
end;

{$ENDREGION}


{$REGION 'TVirtualInterface'}

constructor TVirtualInterface.Create(typeInfo: PTypeInfo);
type
{$POINTERMATH ON}
  PVTable = ^Pointer;
{$POINTERMATH OFF}
var
  i: Integer;
  maxVirtualIndex: SmallInt;
  methods: TArray<TRttiMethod>;
  method: TRttiMethod;
  rttiType: TRttiType;
begin
  fMethodIntercepts := TObjectList<TMethodIntercept>.Create();
  rttiType := fContext.GetType(typeInfo);
  fInterfaceID := TRttiInterfaceType(rttiType).GUID;

  maxVirtualIndex := 2;
  methods := rttiType.GetMethods;

  for method in methods do
  begin
    if maxVirtualIndex < method.VirtualIndex then
      maxVirtualIndex := method.VirtualIndex;
    fMethodIntercepts.Add(TMethodIntercept.Create(method, DoInvoke));
  end;

  fVirtualMethodTable := AllocMem(SizeOf(Pointer) * (maxVirtualIndex + 1));

  PVTable(fVirtualMethodTable)[0] := @TVirtualInterface.VirtualQueryInterface;
  PVTable(fVirtualMethodTable)[1] := @TVirtualInterface.Virtual_AddRef;
  PVTable(fVirtualMethodTable)[2] := @TVirtualInterface.Virtual_Release;

  for i := 0 to fMethodIntercepts.Count - 1 do
    PVTable(fVirtualMethodTable)[fMethodIntercepts[i].VirtualIndex] := fMethodIntercepts[i].CodeAddress;

  for i := 3 to maxVirtualIndex do
    if not Assigned(PVTable(fVirtualMethodTable)[i]) then
      PVTable(fVirtualMethodTable)[i] := @TVirtualInterface.ErrorProc;
end;

constructor TVirtualInterface.Create(TypeInfo: PTypeInfo;
  InvokeEvent: TMethodInvokeEvent);
begin
  Create(TypeInfo);
  fOnInvoke := InvokeEvent;
end;

destructor TVirtualInterface.Destroy;
begin
  if Assigned(fVirtualMethodTable) then
    FreeMem(fVirtualMethodTable);
  fMethodIntercepts.Free;
  inherited;
end;

procedure TVirtualInterface.DoInvoke(UserData: Pointer;
  const Args: TArray<TValue>; out Result: TValue);
begin
  if Assigned(fOnInvoke) then
    fOnInvoke(TMethodIntercept(UserData).Method, Args, Result);
end;

procedure TVirtualInterface.ErrorProc;
begin
  raise EInsufficientRtti.CreateRes(@SInsufficientRtti);
end;

function TVirtualInterface.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = fInterfaceID then
  begin
    _AddRef();
    Pointer(Obj) := @fVirtualMethodTable;
    Result := S_OK;
  end
  else
    Result := inherited;
end;

function TVirtualInterface.VirtualQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.fVirtualMethodTable) - PByte(Self))).QueryInterface(IID, Obj);
end;

function TVirtualInterface.Virtual_AddRef: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.fVirtualMethodTable) - PByte(Self)))._AddRef();
end;

function TVirtualInterface.Virtual_Release: Integer;
begin
  Result := TVirtualInterface(PByte(Self) -
    (PByte(@Self.fVirtualMethodTable) - PByte(Self)))._Release();
end;

function TVirtualInterface._AddRef: Integer;
begin
  Result := inherited;
end;

function TVirtualInterface._Release: Integer;
begin
  Result := inherited;
end;

{$ENDREGION}


end.
