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

// This namespace is experimental and may be changed in nearly future.
unit Spring.Reflection;

{$I Spring.inc}

interface

uses
  Rtti,
  SyncObjs,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.DesignPatterns;

type

  ///	<summary>
  ///	  Specifies the kind of a lazy type.
  ///	</summary>
  TLazyKind = (
    ///	<summary>
    ///	  Not a lazy type.
    ///	</summary>
    lkNone,

    ///	<summary>
    ///	  Type is <see cref="SysUtils|TFunc&lt;T&gt;" />.
    ///	</summary>
    lkFunc,

    ///	<summary>
    ///	  Type is <see cref="Spring|Lazy&lt;T&gt;" />.
    ///	</summary>
    lkRecord,

    ///	<summary>
    ///	  Type is <see cref="Spring|ILazy&lt;T&gt;" />.
    ///	</summary>
    lkInterface
  );

  {$REGION 'TType'}

  ///	<summary>
  ///	  Provides static methods to get RTTI information of a type.
  ///	</summary>
  ///	<remarks>
  ///	  <note type="caller">
  ///	    When using this class, a shared instance of the TRttiContext class
  ///	    will be kept, which will make all instances of RTTI types live during
  ///	    the lifetime.
  ///	  </note>
  ///	</remarks>
  TType = class
  strict private
    class var fContext: TRttiContext;
    class var fSection: TCriticalSection;
    class var fInterfaceTypes: IDictionary<TGuid, TRttiInterfaceType>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    class function GetType<T>: TRttiType; overload;
    class function GetType(typeInfo: PTypeInfo): TRttiType; overload;
    class function GetType(classType: TClass): TRttiType; overload;
    class function GetType(const value: TValue): TRttiType; overload;
//    class function GetTypes: IEnumerable<TRttiType>;
    class function GetFullName(typeInfo: PTypeInfo): string; overload;
    class function GetFullName<T>: string; overload;
    class function FindType(const qualifiedName: string): TRttiType;

    ///	<summary>
    ///	  Returns true if the typeFrom is assignable to the typeTo.
    ///	</summary>
    class function IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean; inline;

    ///	<summary>
    ///	  Returns <c>True</c> if the typeInfo is a delegate type.
    ///	</summary>
    class function IsDelegate(typeInfo: PTypeInfo): Boolean; overload;
    class function TryGetInterfaceType(const guid: TGUID; out aType: TRttiInterfaceType): Boolean;

    ///	<summary>
    ///	  Returns the <see cref="TLazyKind" /> of the typeInfo.
    ///	</summary>
    class function GetLazyKind(typeInfo: PTypeInfo): TLazyKind;

    ///	<summary>
    ///	  Returns the underlying type name of the lazy type.
    ///	</summary>
    class function GetLazyTypeName(typeInfo: PTypeInfo): string;

    ///	<summary>
    ///	  Returns <c>True</c> if the type is a lazy type.
    ///	</summary>
    class function IsLazy(typeInfo: PTypeInfo): Boolean;

    class property Context: TRttiContext read fContext;
  end;

//  IRttiPackage = interface
//    ['{7365872F-36E1-424F-96F4-522357F0A9A4}']
//    {$REGION 'Property Accessors
//      function GetHandle: HINST;
//      function GetTypes: IEnumerable<TRttiType>;
//    {$ENDREGION}
//
//    property Handle: HINST read GetHandle;
//    function FindType(const qualifiedName: string): TRttiType;
//    property Types: IEnumerable<TRttiType> read GetTypes;
//  end;

  IReflection = interface
    ['{E3B66C0B-4827-44C4-BDD9-27F1A856FDDD}']
    {$REGION 'Property Accessors'}
      function GetTypes: IEnumerable<TRttiType>;
//      function GetPackages: IEnumerable<TRttiPackage>;
    {$ENDREGION}

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
//    function GetType(const interfaceGuid: TGuid): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;

    function FindType(const qualifiedName: string): TRttiType;

//    function FindAllWhere(): IEnumerable<TRttiType>;

    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TReflection = class(TInterfacedObject, IReflection)
  private
    fContext: TRttiContext;
    fTypes: IEnumerable<TRttiType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
  public
    constructor Create;

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TRttiTypeEnumerable = class(TEnumerableBase<TRttiType>)
  private
    type
      TEnumerator = class(TEnumeratorBase<TRttiType>)
      private
        fContext: TRttiContext;
        fIndex: Integer;
        fTypes: TArray<TRttiType>;
      protected
        function GetCurrent: TRttiType; override;
      public
        constructor Create;
        function MoveNext: Boolean; override;
      end;
  public
    function GetEnumerator: IEnumerator<TRttiType>; override;
  end;

  {$ENDREGION}


  {$REGION 'Activator'}

  IObjectActivator = interface
    ['{CE05FB89-3467-449E-81EA-A5AEECAB7BB8}']
    function CreateInstance: TValue;
  end;

  TActivator = record
  public
    class function CreateInstance(instanceType: TRttiInstanceType): TValue; overload; static;
    class function CreateInstance(const typeName: string): TValue; overload; static;
    class function CreateInstance(const typeInfo: PTypeInfo): TValue; overload; static;
    class function CreateInstance(instanceType: TRttiInstanceType;
      constructorMethod: TRttiMethod; const arguments: array of TValue): TValue; overload; static;
  end;

  {$ENDREGION}


  TGetRttiMembersFunc<T: TRttiMember> = reference to function(targetType: TRttiType): TArray<T>;


  {$REGION 'TRttiMemberEnumerable<T: TRttiMember>'}

  TRttiMemberEnumerable<T: TRttiMember> = class(TEnumerableBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fCollection: TRttiMemberEnumerable<T>;
        fTargetType: TRttiType;
        fMembers: TArray<T>;
        fIndex: Integer;
      protected
        procedure Initialize(targetType: TRttiType);
        function GetCurrent: T; override;
      public
        constructor Create(collection: TRttiMemberEnumerable<T>);
        function MoveNext: Boolean; override;
      end;
  private
    fParentType: TRttiType;
    fGetMembersFunc: TGetRttiMembersFunc<T>;
    fEnumerateBaseType: Boolean;
    fPredicate: TPredicate<T>;
  public
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean); overload;
    constructor Create(parentType: TRttiType; const func: TGetRttiMembersFunc<T>;
      enumerateBaseType: Boolean; const predicate: TPredicate<T>); overload;
    function GetEnumerator: IEnumerator<T>; override;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersNamed<T: TRttiMember>'}

  TFiltersNamed<T: TRttiNamedObject> = class
  public
    class function IsNamed(const name: string): TSpecification<T>;
    class function HasAttribute(attributeClass: TAttributeClass): TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersBase<T: TRttiMember>'}

  TMethodKinds = set of TMethodKind;

  ///	<summary>
  ///	  Provides static methods to create specifications to filter TRttiMember
  ///	  objects.
  ///	</summary>
  TFiltersBase<T: TRttiMember> = class(TFiltersNamed<T>)
  public
    class function ContainsParameterType(typeInfo: PTypeInfo): TSpecification<T>;
    class function HasParameterTypes(const types: array of PTypeInfo): TSpecification<T>;
    class function HasParameterFlags(const flags: TParamFlags): TSpecification<T>;
    class function IsTypeOf<TType>: TSpecification<T>; overload;
    class function IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>; overload;
    class function IsConstructor: TSpecification<T>;
    class function IsInstanceMethod: TSpecification<T>;
    class function IsClassMethod: TSpecification<T>;
    class function IsMethodKind(const kinds : TMethodKinds): TSpecification<T>;
    class function IsInvokable: TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'Filters'}

  TPackageFilters = class(TFiltersNamed<TRttiPackage>);
  TMemberFilters = class(TFiltersBase<TRttiMember>);
  TMethodFilters = class(TFiltersBase<TRttiMethod>);
  TPropertyFilters = class(TFiltersBase<TRttiProperty>);
  TFieldFilters = class(TFiltersBase<TRttiField>);
  TTypeFilters = class(TFiltersNamed<TRttiType>)
  public
    class function IsClass : TSpecification<TRttiType>;
    class function IsInterface : TSpecification<TRttiType>;
  end;

  {$ENDREGION}


  {$REGION 'TMemberSpecificationBase<T: TRttiMember>'}

  TMemberSpecificationBase<T: TRttiObject> = class abstract(TSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; virtual; abstract;
  public
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T: TRttiMember>'}

  TNameFilter<T: TRttiNamedObject> = class(TMemberSpecificationBase<T>)
  private
    fName: string;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T: TRttiMember>'}

  TInvokableFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T: TRttiMember>'}

  THasAttributeFilter<T: TRttiObject> = class(TMemberSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  {$ENDREGION}


  {$REGION 'TTypeFilter<T: TRttiMember>'}

  TTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T: TRttiMember>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'TContainsParameterTypeFilter<T: TRttiMember>'}

  TContainsParameterTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  TRttiMemberClass = class of TRttiMember;


  {$REGION 'TMemberTypeFilter<T: TRttiMember>'}

  TMemberTypeFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T: TRttiMember>'}

  TConstructorFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T: TRttiMember>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TClassMethodFilter<T: TRttiMember>'}

  TClassMethodFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  protected
    function Accept(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T: TRttiMember>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'TMethodKindFilter<T: TRttiMember>'}

  TMethodKindFilter<T: TRttiMember> = class(TMemberSpecificationBase<T>)
  private
    fFlags: TMethodKinds;
  protected
    function Accept(const member: T): Boolean; override;
  public
    constructor Create(const flags: TMethodKinds);
  end;

  {$ENDREGION}


  {$REGION 'TIsClassFilter>'}

  TIsClassFilter = class(TMemberSpecificationBase<TRttiType>)
  protected
    function Accept(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TIsInterfaceFilter>'}

  TIsInterfaceFilter = class(TMemberSpecificationBase<TRttiType>)
  protected
    function Accept(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'Internal Class Helpers'}

  ///	<summary>
  ///	  The _InternalRttiMemberHelper class was copied from Spring.Helpers, as
  ///	  An URW1111 internal error will occured when the Spring.Helpers
  ///	  namespace was used by this unit.
  ///	</summary>
  _InternalRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
  public
    function AsProperty: TRttiProperty;
    function AsMethod: TRttiMethod;
    function AsField: TRttiField;
    property IsConstructor: Boolean read GetIsConstructor;
    property IsProperty: Boolean read GetIsProperty;
    property IsMethod: Boolean read GetIsMethod;
    property IsField: Boolean read GetIsField;
    property IsPrivate: Boolean read GetIsPrivate;
    property IsProtected: Boolean read GetIsProtected;
    property IsPublic: Boolean read GetIsPublic;
    property IsPublished: Boolean read GetIsPublished;
  end;

  {$ENDREGION}


implementation

uses
  StrUtils,
  Spring.ResourceStrings;


{$REGION 'TType'}

class constructor TType.Create;
begin
  fContext := TRttiContext.Create;
  fSection := TCriticalSection.Create;
end;

class destructor TType.Destroy;
begin
  fSection.Free;
  fContext.Free;
end;

class function TType.GetType<T>: TRttiType;
begin
  Result := GetType(TypeInfo(T));
end;

class function TType.GetType(typeInfo: PTypeInfo): TRttiType;
begin
  Result := fContext.GetType(typeInfo);
end;

class function TType.GetType(classType: TClass): TRttiType;
begin
  Result := fContext.GetType(classType);
end;

class function TType.GetType(const value: TValue): TRttiType;
begin
  Result := GetType(value.TypeInfo);
end;

class function TType.GetFullName(typeInfo: PTypeInfo): string;
begin
  Guard.CheckNotNull(typeInfo, 'typeInfo');
  Result := fContext.GetType(typeInfo).QualifiedName;
end;

class function TType.GetFullName<T>: string;
var
  typeInfo: PTypeInfo;
begin
  typeInfo := System.TypeInfo(T);
  Result := TType.GetFullName(typeInfo);
end;

const
  LazyPrefixStrings: array[lkFunc..High(TLazyKind)] of string = (
    'TFunc<', 'Lazy<', 'ILazy<');

class function TType.GetLazyKind(typeInfo: PTypeInfo): TLazyKind;
var
  name: string;
begin
  if Assigned(typeInfo) then
  begin
    name := GetTypeName(typeInfo);
    for Result := lkFunc to High(TLazyKind) do
      if StartsText(LazyPrefixStrings[Result], name) then
        Exit;
  end;
  Result := lkNone;
end;

class function TType.GetLazyTypeName(typeInfo: PTypeInfo): string;
var
  lazyKind: TLazyKind;
  name: string;
  i: Integer;
begin
  lazyKind := GetLazyKind(typeInfo);
  name := GetTypeName(typeInfo);
  if lazyKind > lkNone then
  begin
    i := Length(LazyPrefixStrings[lazyKind]) + 1;
    Result := Copy(name, i, Length(name) - i )
  end;
end;

class function TType.FindType(const qualifiedName: string): TRttiType;
var
  rttiType: TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
  if not Assigned(Result) then
  begin
    for rttiType in fContext.GetTypes do
      if SameText(rttiType.Name, qualifiedName) then
        Exit(rttiType);
    Result := nil;
  end;
end;

class function TType.IsAssignable(typeFrom, typeTo: PTypeInfo): Boolean;
begin
  Result := IsAssignableFrom(typeTo, typeFrom);
end;

class function TType.IsDelegate(typeInfo: PTypeInfo): Boolean;
const
  DelegatePrefixStrings: array[0..2] of string = (
    'TFunc<', 'TProc<', 'TPredicate<');
var
  name: string;
  i: Integer;
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkInterface);
  if Result then
  begin
    name := GetTypeName(typeInfo);
    for i := Low(DelegatePrefixStrings) to High(DelegatePrefixStrings) do
      if StartsText(DelegatePrefixStrings[i], name) then
        Exit;
  end;
end;

class function TType.IsLazy(typeInfo: PTypeInfo): Boolean;
begin
  Result := GetLazyKind(typeInfo) <> lkNone;
end;

class function TType.TryGetInterfaceType(const guid: TGUID;
  out aType: TRttiInterfaceType): Boolean;
var
  item: TRttiType;
begin
  if fInterfaceTypes = nil then
  begin
    fSection.Enter;
    try
      MemoryBarrier;
      if fInterfaceTypes = nil then
      begin
        fInterfaceTypes := TCollections.CreateDictionary<TGuid, TRttiInterfaceType>;
        for item in fContext.GetTypes do
        begin
          if (item is TRttiInterfaceType) and (ifHasGuid in TRttiInterfaceType(item).IntfFlags)
            and not fInterfaceTypes.ContainsKey(TRttiInterfaceType(item).GUID) then
          begin
            fInterfaceTypes.Add(TRttiInterfaceType(item).GUID, TRttiInterfaceType(item));
          end;
        end;
      end;
    finally
      fSection.Leave;
    end;
  end;
  Result := fInterfaceTypes.TryGetValue(guid, aType);
end;

{$ENDREGION}


{$REGION 'Internal Class Helpers

{ TInternalRttiMemberHelper }

function _InternalRttiMemberHelper.AsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function _InternalRttiMemberHelper.AsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function _InternalRttiMemberHelper.AsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

function _InternalRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function _InternalRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function _InternalRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function _InternalRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function _InternalRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function _InternalRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function _InternalRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function _InternalRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;


{$ENDREGION}


{$REGION 'TActivator'}

class function TActivator.CreateInstance(instanceType: TRttiInstanceType;
  constructorMethod: TRttiMethod; const arguments: array of TValue): TValue;
begin
  Guard.CheckNotNull(instanceType, 'instanceType');
  Guard.CheckNotNull(constructorMethod, 'constructorMethod');
  Result := constructorMethod.Invoke(instanceType.MetaclassType, arguments);
end;

class function TActivator.CreateInstance(const typeName: string): TValue;
var
  context: TRttiContext;
  typeObj: TRttiType;
begin
  typeObj := context.FindType(typeName);
  if typeObj is TRttiInstanceType then
  begin
    Result := TActivator.CreateInstance(TRttiInstanceType(typeObj));
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

class function TActivator.CreateInstance(const typeInfo: PTypeInfo): TValue;
var
  context: TRttiContext;
  typeObj: TRttiType;
begin
  Guard.CheckNotNull(typeInfo, 'typeInfo');

  typeObj := context.GetType(typeInfo);
  if typeObj is TRttiInstanceType then
  begin
    Result := TActivator.CreateInstance(TRttiInstanceType(typeObj));
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

class function TActivator.CreateInstance(
  instanceType: TRttiInstanceType): TValue;
var
  method: TRttiMethod;
begin
  Guard.CheckNotNull(instanceType, 'instanceType');

  for method in instanceType.GetMethods do
  begin
    if method.IsConstructor and (Length(method.GetParameters) = 0) then
    begin
      Result := method.Invoke(instanceType.MetaclassType, []);
      Exit;
    end;
  end;
  Result := TValue.Empty;
end;

{$ENDREGION}


{$REGION 'TRttiMemberEnumerable<T>'}

constructor TRttiMemberEnumerable<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean);
begin
  Create(parentType, func, enumerateBaseType, nil);
end;

constructor TRttiMemberEnumerable<T>.Create(parentType: TRttiType;
  const func: TGetRttiMembersFunc<T>; enumerateBaseType: Boolean;
  const predicate: TPredicate<T>);
begin
  inherited Create;
  fParentType := parentType;
  fGetMembersFunc := func;
  fEnumerateBaseType := enumerateBaseType;
  fPredicate := predicate;
end;

function TRttiMemberEnumerable<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

{$ENDREGION}


{$REGION 'TRttiMemberEnumerable<T>.TEnumerator'}

constructor TRttiMemberEnumerable<T>.TEnumerator.Create(
  collection: TRttiMemberEnumerable<T>);
begin
  inherited Create;
  fCollection := collection;
  Initialize(fCollection.fParentType);
end;

procedure TRttiMemberEnumerable<T>.TEnumerator.Initialize(
  targetType: TRttiType);
begin
  fTargetType := targetType;
  if Assigned(fTargetType) then
  begin
    fMembers := fCollection.fGetMembersFunc(fTargetType);
  end
  else
  begin
    SetLength(fMembers, 0);
  end;
  fIndex := -1;
end;

function TRttiMemberEnumerable<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < Length(fMembers) - 1;
  if Result then
  begin
    Inc(fIndex);
    if Assigned(fCollection.fPredicate) and not fCollection.fPredicate(Current) then
    begin
      Result := MoveNext;
    end;
  end
  else if fCollection.fEnumerateBaseType and (fTargetType <> nil) then
  begin
    Initialize(fTargetType.BaseType);
    Exit(MoveNext);
  end;
end;

function TRttiMemberEnumerable<T>.TEnumerator.GetCurrent: T;
begin
  Result := fMembers[fIndex];
end;

{$ENDREGION}


{$REGION 'TMemberSpecificationBase<T>'}

function TMemberSpecificationBase<T>.IsSatisfiedBy(
  const member: T): Boolean;
begin
//  Guard.CheckNotNull<T>(member, 'member');
  Result := Accept(member);
end;

{$ENDREGION}


{$REGION 'TFiltersNamed<T>'}

class function TFiltersNamed<T>.IsNamed(const name: string): TSpecification<T>;
begin
  Result := TNameFilter<T>.Create(name);
end;

class function TFiltersNamed<T>.HasAttribute(
  attributeClass: TAttributeClass): TSpecification<T>;
begin
  Result := THasAttributeFilter<T>.Create(attributeClass);
end;

{$ENDREGION}


{$REGION 'TFiltersBase<T>'}

class function TFiltersBase<T>.ContainsParameterType(
  typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TContainsParameterTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.HasParameterTypes(
  const types: array of PTypeInfo): TSpecification<T>;
begin
  Result := THasParameterTypesFilter<T>.Create(types);
end;

class function TFiltersBase<T>.HasParameterFlags(
  const flags: TParamFlags): TSpecification<T>;
begin
  Result := THasParameterFlagsFilter<T>.Create(flags);
end;

class function TFiltersBase<T>.IsTypeOf(typeInfo: PTypeInfo): TSpecification<T>;
begin
  Result := TTypeFilter<T>.Create(typeInfo);
end;

class function TFiltersBase<T>.IsTypeOf<TType>: TSpecification<T>;
begin
  Result := IsTypeOf(TypeInfo(TType));
end;

class function TFiltersBase<T>.IsClassMethod: TSpecification<T>;
begin
  Result := TClassMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsConstructor: TSpecification<T>;
begin
  Result := TConstructorFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInstanceMethod: TSpecification<T>;
begin
  Result := TInstanceMethodFilter<T>.Create;
end;

class function TFiltersBase<T>.IsInvokable: TSpecification<T>;
begin
  Result := TInvokableFilter<T>.Create;
end;

class function TFiltersBase<T>.IsMethodKind(
  const kinds: TMethodKinds): TSpecification<T>;
begin
  Result := TMethodKindFilter<T>.Create(kinds);
end;

{$ENDREGION}


{$REGION 'TTypeFilters'}

class function TTypeFilters.IsClass: TSpecification<TRttiType>;
begin
  Result := TIsClassFilter.Create;
end;

class function TTypeFilters.IsInterface: TSpecification<TRttiType>;
begin
  Result := TIsInterfaceFilter.Create;
end;

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter<T> }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass);
begin
  inherited Create;
  fAttributeClass := attributeClass;
end;

function THasAttributeFilter<T>.Accept(const member: T): Boolean;
var
  attribute: TCustomAttribute;
begin
  Result := False;
  for attribute in member.GetAttributes do
  begin
    if attribute.InheritsFrom(fAttributeClass) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.Accept(const member: T): Boolean;
begin
  Result := SameText(TRttiNamedObject(member).Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo;
  end
  else if member.IsField then
  begin
    Result := member.AsField.FieldType.Handle = fTypeInfo;
  end
  else
  begin
    Result := False;
  end;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fTypes, Length(types));
  for i := 0 to High(types) do
  begin
    fTypes[i] := types[i];
  end;
end;

function THasParameterTypesFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
  for i := 0 to Length(parameters) - 1 do
  begin
    if parameters[i].ParamType.Handle <> fTypes[i] then  // IsAssignableFrom
    begin
      Result := False;
      Break;
    end;
  end;
end;

{ TContainsParameterTypeFilter<T> }

constructor TContainsParameterTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TContainsParameterTypeFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
    begin
      if parameter.ParamType.Handle = fTypeInfo then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ THasParameterFlagsFilter<T> }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.Accept(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
    begin
      if parameter.Flags * fFlags <> [] then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

{ TMethodKindFilter<T> }

function TMethodKindFilter<T>.Accept(const member: T): Boolean;
begin
  Result := False;
  if member.IsMethod then
  begin
    Result := member.AsMethod.MethodKind in fFlags;
  end;
end;

constructor TMethodKindFilter<T>.Create(const flags: TMethodKinds);
begin
  inherited Create;
  fFlags:=flags;
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.Accept(const member: T): Boolean;
begin
  if member.IsProperty then
  begin
    Result := member.AsProperty.IsWritable;
  end
  else if member.IsMethod then
  begin
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor]);
  end
  else
  begin
    Result := True;
  end;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter<T> }

function TConstructorFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter<T> }

function TInstanceMethodFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{ TClassMethodFilter<T> }

function TClassMethodFilter<T>.Accept(const member: T): Boolean;
begin
  Result := member.IsMethod and member.AsMethod.IsClassMethod;
end;

{ TIsClassFilter }

function TIsClassFilter.Accept(const member: TRttiType): Boolean;
begin
  Result := member.IsInstance;
end;

{ TIsInterfaceFilter }

function TIsInterfaceFilter.Accept(const member: TRttiType): Boolean;
begin
  Result := member is TRttiInterfaceType;
end;

{$ENDREGION}


{$REGION 'TReflection'}

constructor TReflection.Create;
begin
  fContext := TRttiContext.Create;
  fTypes := TRttiTypeEnumerable.Create;
end;

function TReflection.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
end;

function TReflection.GetFullName(const typeInfo: PTypeInfo): string;
var
  t: TRttiType;
begin
  t := fContext.GetType(typeInfo);
  if t = nil then
    Exit('');
  if t.IsPublicType then
    Result := t.QualifiedName
  else
    Result := t.Name;
end;

function TReflection.GetType(const typeInfo: PTypeInfo): TRttiType;
begin
  Guard.CheckNotNull(typeInfo, 'typeInfo');
  Result := fContext.GetType(typeInfo);
end;

function TReflection.GetType(const classType: TClass): TRttiType;
begin
  Guard.CheckNotNull(classType, 'classType');
  Result := fContext.GetType(classType.ClassInfo);
end;

function TReflection.GetType(const instance: TObject): TRttiType;
begin
  Guard.CheckNotNull(instance, 'instance');
  Result := fContext.GetType(instance.ClassInfo);
end;

function TReflection.GetType(const instance: TValue): TRttiType;
begin
  Result := fContext.GetType(instance.TypeInfo);
end;

function TReflection.GetTypes: IEnumerable<TRttiType>;
begin
  Result := fTypes;
end;

{$ENDREGION}


{$REGION 'TRttiTypeEnumerable'}

function TRttiTypeEnumerable.GetEnumerator: IEnumerator<TRttiType>;
begin
  Result := TEnumerator.Create;
end;

{$ENDREGION}


{$REGION 'TRttiTypeEnumerable.TEnumerator'}

constructor TRttiTypeEnumerable.TEnumerator.Create;
begin
  fContext := TRttiContext.Create;
  fTypes := fContext.GetTypes;
  fIndex := -1;
end;

function TRttiTypeEnumerable.TEnumerator.GetCurrent: TRttiType;
begin
  Result := fTypes[fIndex];
end;

function TRttiTypeEnumerable.TEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < Length(fTypes) - 1;
  if Result then
    Inc(fIndex);
end;

{$ENDREGION}


end.
