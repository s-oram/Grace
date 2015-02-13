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

unit Spring.Logging.Serializers;

interface

uses
  Rtti,
  StrUtils,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Logging.Extensions,
  Spring.Reflection;

type
  {$REGION 'TSerializerBase'}
  TSerializerBase = class abstract(TInterfacedObject)
  strict protected
    class function ValueToStr(const value: TValue): string; inline; static;
  end;
  {$ENDREGION}


  {$REGION 'TSimpleTypeSerializer'}
  TSimpleTypeSerializer = class(TSerializerBase, ITypeSerializer)
  private const
    SUPPORTED_KINDS = [tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
      tkSet, tkWChar, tkLString, tkWString, tkInt64, tkUString, tkClassRef,
      tkPointer];
  public
    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  end;
  {$ENDREGION}


  {$REGION 'TNestingTypeSerializer'}
  TNestingTypeSerializer = class abstract(TSerializerBase)
  private
    fUseNested: Boolean;
  protected
    property UseNested: Boolean read fUseNested;
  public
    constructor Create; overload;
    constructor Create(useNested: Boolean); overload;
  end;
  {$ENDREGION}


  {$REGION 'TReflectionTypeSerializer'}
  TReflectionTypeSerializer = class(TNestingTypeSerializer, ITypeSerializer)
  private const
    NESTING_LIMIT = 8;
  public type
    TMemberVisibilities = set of TMemberVisibility;
  private
    fVisibility: set of TMemberVisibility;
  protected
    function SerializeNested(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  public
    constructor Create(visibility: TMemberVisibilities;
      useNested: Boolean); overload;
    constructor Create; overload;

    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  end;
  {$ENDREGION}


  {$REGION 'TInterfaceSerializer'}
  TInterfaceSerializer = class(TSerializerBase, ITypeSerializer)
  public
    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  end;
  {$ENDREGION}


  {$REGION 'TArrayOfValueSerializer'}
  TArrayOfValueSerializer = class(TNestingTypeSerializer, ITypeSerializer)
  protected
    function SerializeSimple(const value: TValue): string;
    function SerializeNested(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  public
    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer): string;
  end;
  {$ENDREGION}


implementation


{$REGION 'TSerializerBase'}

class function TSerializerBase.ValueToStr(const value: TValue): string;
begin
  case value.Kind of
    tkInterface,
    tkRecord:
      Result := '(' + value.TypeInfo.TypeName + ')';
  else
    Result := value.ToString;
  end;
end;
{$ENDREGION}


{$REGION 'TSimpleTypeSerializer'}

function TSimpleTypeSerializer.HandlesType(typeInfo: PTypeInfo): Boolean;
begin
  Result := typeInfo^.Kind in SUPPORTED_KINDS;
end;

function TSimpleTypeSerializer.Serialize(
  const controller: ISerializerController; const value: TValue;
  nestingLevel: Integer): string;
begin
  Assert(value.Kind in SUPPORTED_KINDS);
  Result := ValueToStr(value);
end;
{$ENDREGION}


{$REGION 'TNestingTypeSerializer'}

constructor TNestingTypeSerializer.Create;
begin
  Create(False);
end;

constructor TNestingTypeSerializer.Create(useNested: Boolean);
begin
  inherited Create;
  fUseNested := useNested;
end;

{$ENDREGION}


{$REGION 'TReflectionTypeSerializer'}

constructor TReflectionTypeSerializer.Create(visibility: TMemberVisibilities;
  useNested: Boolean);
begin
  inherited Create(useNested);
  fVisibility := visibility;
end;

constructor TReflectionTypeSerializer.Create;
begin
  Create([mvPublished, mvPublic], False);
end;

function TReflectionTypeSerializer.HandlesType(typeInfo: PTypeInfo): Boolean;
begin
  Result := typeInfo^.Kind in [tkClass, tkRecord];
end;

function TReflectionTypeSerializer.Serialize(
  const controller: ISerializerController; const value: TValue;
  nestingLevel: Integer): string;
var
  prefix: string;
  valueType: TRttiType;
  field: TRttiField;
  prop: TRttiProperty;
  instance: Pointer;
  v: TValue;
begin
  Assert(value.TypeInfo^.Kind in [tkClass, tkRecord]);

  if value.Kind = tkClass then
  begin
    instance := value.AsObject;
    if Assigned(instance) then
      valueType := TType.GetType(TObject(instance).ClassInfo)
    else
      valueType := nil;
  end
  else
  begin
    instance := value.GetReferenceToRawData;
    valueType := TType.GetType(value.TypeInfo);
  end;

  Result := ValueToStr(value) + '(';

  if valueType <> nil then
  begin
    prefix := #$A + DupeString('  ', nestingLevel + 1);

    for field in valueType.GetFields do
    begin
      if field.Visibility in fVisibility then
      begin
        Result := Result + prefix + field.Name + ' = ';
        v := field.GetValue(instance);
        if UseNested then
          Result := Result + SerializeNested(controller, v, nestingLevel + 1)
        else
          Result := Result + ValueToStr(v);
      end;
    end;

    for prop in valueType.GetProperties do
    begin
      if (prop.Visibility in fVisibility) and prop.IsReadable then
      begin
        Result := Result + prefix + prop.Name + ' = ';
        v := prop.GetValue(instance);
        if UseNested then
          Result := Result + SerializeNested(controller, v, nestingLevel + 1)
        else
          Result := Result + ValueToStr(v);
      end;
    end;
  end;

  Result := Result + ')';
end;

function TReflectionTypeSerializer.SerializeNested(
  const controller: ISerializerController; const value: TValue;
  nestingLevel: Integer): string;
var
  serializer: ITypeSerializer;
begin
  if nestingLevel >= NESTING_LIMIT then
    Exit('(nesting)');

  Assert(controller <> nil);
  serializer := controller.FindSerializer(value.TypeInfo);
  if serializer <> nil then
    Result := serializer.Serialize(controller, value, nestingLevel)
  else
    Result := ValueToStr(value);
end;

{$ENDREGION}


{$REGION 'TInterfaceSerializer'}

function TInterfaceSerializer.HandlesType(typeInfo: PTypeInfo): Boolean;
begin
  Result := typeInfo^.Kind = tkInterface;
end;

function TInterfaceSerializer.Serialize(const controller: ISerializerController;
  const value: TValue; nestingLevel: Integer): string;
var
  intf: IInterface;
  objectSerializer: ITypeSerializer;
begin
  Assert(value.TypeInfo^.Kind = tkInterface);
  Assert(Assigned(controller));

  Result := ValueToStr(value);

  objectSerializer := controller.FindSerializer(TypeInfo(TObject));
  if Assigned(objectSerializer) then
  begin
    intf := value.AsInterface;
    if intf is TObject then
      Result := Result + ': ' + objectSerializer.Serialize(controller,
        TObject(intf), nestingLevel);
  end;
end;

{$ENDREGION}


{$REGION 'TArrayOfValueSerializer'}

function TArrayOfValueSerializer.HandlesType(typeInfo: PTypeInfo): Boolean;
begin
  Result := typeInfo = System.TypeInfo(TArray<TValue>);
end;

function TArrayOfValueSerializer.Serialize(
  const controller: ISerializerController; const value: TValue;
  nestingLevel: Integer): string;
begin
  if UseNested then
    Result := SerializeNested(controller, value, nestingLevel)
  else
    Result := SerializeSimple(value);
end;

function TArrayOfValueSerializer.SerializeNested(
  const controller: ISerializerController; const value: TValue;
  nestingLevel: Integer): string;
var
  serializer: ITypeSerializer;
  v: TValue;
begin
  Assert(Assigned(controller));
  Result := '[';
  for v in value.AsType<TArray<TValue>> do
  begin
    serializer := controller.FindSerializer(v.TypeInfo);
    if serializer <> nil then
      Result := Result + serializer.Serialize(controller, v, nestingLevel) + ', '
    else
      Result := Result + ValueToStr(v) + ', ';
  end;
  if Length(Result) > 1 then
    SetLength(Result, Length(Result) - 2);
  Result := Result + ']';
end;

function TArrayOfValueSerializer.SerializeSimple(const value: TValue): string;
var
  v: TValue;
begin
  Result := '[';
  for v in value.AsType<TArray<TValue>> do
    Result := Result + ValueToStr(v) + ', ';
  if Length(Result) > 1 then
    SetLength(Result, Length(Result) - 2);
  Result := Result + ']';
end;

{$ENDREGION}


end.

