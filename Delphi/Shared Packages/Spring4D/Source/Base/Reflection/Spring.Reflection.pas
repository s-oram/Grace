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
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

unit Spring.Reflection;

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
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
  {$ENDREGION}

    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
//    function GetType(const interfaceGuid: TGuid): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;

    function FindType(const qualifiedName: string): TRttiType;

//    function FindAllWhere(): IEnumerable<TRttiType>;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TReflection = class(TInterfacedObject, IReflection)
  strict private
    class var fContext: TRttiContext;
    function GetClasses: IEnumerable<TRttiInstanceType>;
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;
    function GetTypes: IEnumerable<TRttiType>;
//    function GetPackages: IEnumerable<TRttiPackage>;
    class constructor Create;
  {$HINTS OFF}
    class destructor Destroy;
  {$HINTS ON}
  public
    function GetType(const typeInfo: PTypeInfo): TRttiType; overload;
    function GetType(const classType: TClass): TRttiType; overload;
    function GetType(const instance: TObject): TRttiType; overload;
    function GetType(const instance: TValue): TRttiType; overload;

    function GetFullName(const typeInfo: PTypeInfo): string; overload;
    function FindType(const qualifiedName: string): TRttiType;

    property Classes: IEnumerable<TRttiInstanceType> read GetClasses;
    property Interfaces: IEnumerable<TRttiInterfaceType> read GetInterfaces;
    property Types: IEnumerable<TRttiType> read GetTypes;
//    property Packages: IEnumerable<TRttiPackage> read GetPackages;
  end;

  TRttiTypeIterator<T: TRttiType> = class(TIterator<T>)
  private
    fContext: TRttiContext;
    fIndex: Integer;
    fTypes: TArray<TRttiType>;
  public
    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMemberIterator<T>'}

  TRttiMemberIterator<T: TRttiMember> = class(TIterator<T>)
  private
    fParentType: TRttiType;
    fSelector: TFunc<TRttiType,TArray<T>>;
    fEnumerateBaseType: Boolean;
    fPredicate: TPredicate<T>;
    fTargetType: TRttiType;
    fMembers: TArray<T>;
    fIndex: Integer;
    procedure Initialize(const targetType: TRttiType);
  public
    constructor Create(const parentType: TRttiType;
      const selector: TFunc<TRttiType,TArray<T>>;
      enumerateBaseType: Boolean); overload;
    constructor Create(const parentType: TRttiType;
      const selector: TFunc<TRttiType,TArray<T>>;
      enumerateBaseType: Boolean;
      const predicate: TPredicate<T>); overload;

    function Clone: TIterator<T>; override;
    function MoveNext: Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TRttiObjectHelper'}

  TRttiObjectHelper = class helper for TRttiObject
  public
    function GetCustomAttributes(attributeClass: TAttributeClass): TArray<TCustomAttribute>; overload;

    ///	<summary>
    ///	  Gets an array which contains all custom attribute types which the
    ///	  type applies.
    ///	</summary>
    function GetCustomAttributes<T: TCustomAttribute>: TArray<T>; overload;

    function GetCustomAttribute(attributeClass: TAttributeClass): TCustomAttribute; overload;

    ///	<summary>
    ///	  Enumerates all applied custom attributes and returns the first one
    ///	  which is/inherits the specified type.
    ///	</summary>
    function GetCustomAttribute<T: TCustomAttribute>: T; overload;

    function TryGetCustomAttribute(attributeClass: TAttributeClass;
      out attribute: TCustomAttribute): Boolean; overload;

    ///	<summary>
    ///	  Try getting a custom attribute class which is applied by the type.
    ///	</summary>
    function TryGetCustomAttribute<T: TCustomAttribute>(out attribute: T): Boolean; overload;

    function HasCustomAttribute(attributeClass: TAttributeClass): Boolean; overload;

    ///	<summary>
    ///	  Determines whether the type applies the specified custom attribute
    ///	  class.
    ///	</summary>
    function HasCustomAttribute<T: TCustomAttribute>: Boolean; overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiTypeHelper'}

  TRttiTypeHelper =  class helper for TRttiType
  private
    function GetAsInterface: TRttiInterfaceType;
    function GetIsClass: Boolean;
    function GetIsInterface: Boolean;
    function GetIsClassOrInterface: Boolean;
    function GetAsClass: TRttiInstanceType;
    function GetIsGenericType: Boolean;
    function GetIsLazyType: Boolean;
    function GetAsDynamicArray: TRttiDynamicArrayType;
    function GetIsDynamicArray: Boolean;
    function GetIsString: Boolean;
    function InternalGetConstructors(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetMethods(enumerateBaseType: Boolean = True): IEnumerable<TRttiMethod>;
    function InternalGetProperties(enumerateBaseType: Boolean = True): IEnumerable<TRttiProperty>;
    function InternalGetFields(enumerateBaseType: Boolean = True): IEnumerable<TRttiField>;
    function GetBaseTypes: IReadOnlyList<TRttiType>;
    function GetConstructors: IEnumerable<TRttiMethod>;
    function GetMethods: IEnumerable<TRttiMethod>;
    function GetProperties: IEnumerable<TRttiProperty>;
    function GetFields: IEnumerable<TRttiField>;
    function GetDefaultName: string;
    function GetAncestorCount: Integer;
  public
    // function GetMembers: IEnumerable<TRttiMember>;

    ///	<summary>
    ///	  Returns an enumerable collection which contains all the interface
    ///	  Rtti types that the target type implements.
    ///	  <note type="note">
    ///	    Only Guid interfaces will be enumerated.
    ///	  </note>
    ///	</summary>
    ///	<seealso cref="Spring.Collections|IEnumerable&lt;T&gt;" />
    function GetInterfaces: IEnumerable<TRttiInterfaceType>;

    /// <summary>
    ///   Gets an array of types which contains all generic arguments.
    /// </summary>
    /// <remarks>
    ///   This method extracts generic arguments from the name of the generic
    ///   type. Invoking the method on the type <c>
    ///   TDictionary&lt;Integer,string&gt;</c> for example will return an
    ///   array which contains two types: <c>System.Integer</c> and <c>
    ///   System.string</c>.
    /// </remarks>
    function GetGenericArguments: TArray<TRttiType>;

    /// <summary>
    ///   Returns a string that represents a generic type definition.
    /// </summary>
    function GetGenericTypeDefinition: string;

    /// <summary>
    ///   Determines whether an instance of the current TRttiType can be
    ///   assigned from an instance of the specified TRttiType.
    /// </summary>
    /// <param name="rttiType">
    ///   The type to compare with the current type.
    /// </param>
    function IsAssignableFrom(const rttiType: TRttiType): Boolean;

    property BaseTypes: IReadOnlyList<TRttiType> read GetBaseTypes;

    ///	<summary>
    ///	  Gets an enumerable collection which contains all constructor methods
    ///	  of the type, including inherited.
    ///	</summary>
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Constructors: IEnumerable<TRttiMethod> read GetConstructors;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all methods that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Propoerties" />
    ///	<seealso cref="Fields" />
    property Methods: IEnumerable<TRttiMethod> read GetMethods;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all properties that the
    ///	  type contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Fields" />
    property Properties: IEnumerable<TRttiProperty> read GetProperties;

    ///	<summary>
    ///	  Gets a enumerable collection which contains all fields that the type
    ///	  contains, including inherited.
    ///	</summary>
    ///	<seealso cref="Constructors" />
    ///	<seealso cref="Methods" />
    ///	<seealso cref="Propoerties" />
    property Fields: IEnumerable<TRttiField> read GetFields;

    property AsClass: TRttiInstanceType read GetAsClass;
    property AsInterface: TRttiInterfaceType read GetAsInterface;
    property AsDynamicArray: TRttiDynamicArrayType read GetAsDynamicArray;
    property IsClass: Boolean read GetIsClass;
    property IsInterface: Boolean read GetIsInterface;
    property IsClassOrInterface: Boolean read GetIsClassOrInterface;
    property IsDynamicArray: Boolean read GetIsDynamicArray;
    property IsString: Boolean read GetIsString;

    ///	<summary>
    ///	  Gets a value indicates whether the current type is generic.
    ///	</summary>
    property IsGenericType: Boolean read GetIsGenericType;

    property IsLazyType: Boolean read GetIsLazyType;
    property DefaultName: string read GetDefaultName;
    property AncestorCount: Integer read GetAncestorCount;
  end;

  TRttiClassType = TRttiInstanceType;

  {$ENDREGION}


  {$REGION 'TRttiInterfaceTypeHelper'}

  TRttiInterfaceTypeHelper = class helper for TRttiInterfaceType
  private
    function GetHasGuid: Boolean;
  public
    /// <summary>
    ///   Determines whether this interface type has a guid.
    /// </summary>
    property HasGuid: Boolean read GetHasGuid;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMemberHelper'}

  TRttiMemberHelper = class helper for TRttiMember
  private
    function GetIsPrivate: Boolean;
    function GetIsProtected: Boolean;
    function GetIsPublic: Boolean;
    function GetIsPublished: Boolean;
    function GetIsConstructor: Boolean;
    function GetIsProperty: Boolean;
    function GetIsMethod: Boolean;
    function GetIsField: Boolean;
    function GetAsMethod: TRttiMethod;
    function GetAsProperty: TRttiProperty;
    function GetAsField: TRttiField;
  public
//    procedure InvokeMember(instance: TValue; const arguments: array of TValue);
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
    property AsMethod: TRttiMethod read GetAsMethod;
    property AsProperty: TRttiProperty read GetAsProperty;
    property AsField: TRttiField read GetAsField;
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


  {$REGION 'TRttiFieldHelper'}

  TRttiFieldHelper = class helper for TRttiField
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiPropertyHelper'}

  TRttiPropertyHelper = class helper for TRttiProperty
  public
    function GetValue(const instance: TValue): TValue; overload;
    procedure SetValue(const instance: TValue; const value: TValue); overload;
  end;

  {$ENDREGION}


  {$REGION 'TRttiMethodHelper'}

  TRttiMethodHelper = class helper(Spring.TRttiMethodHelper) for TRttiMethod
  private
    function InternalGetParameters: IEnumerable<TRttiParameter>;
  public
    property Parameters: IEnumerable<TRttiParameter> read InternalGetParameters;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersNamed<T>'}

  TFiltersNamed<T: TRttiNamedObject> = class
  public
    class function IsNamed(const name: string): TSpecification<T>;
    class function HasAttribute(attributeClass: TAttributeClass): TSpecification<T>;
  end;

  {$ENDREGION}


  {$REGION 'TFiltersBase<T>'}

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
  TParameterFilters = class(TFiltersNamed<TRttiParameter>)
  public
    class function HasFlags(flags: TParamFlags): TSpecification<TRttiParameter>;
  end;

  {$ENDREGION}


  {$REGION 'TNameFilter<T>'}

  TNameFilter<T: TRttiNamedObject> = class(TSpecificationBase<T>)
  private
    fName: string;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const name: string);
  end;

  {$ENDREGION}


  {$REGION 'TInvokableFilter<T>'}

  TInvokableFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasAttributeFilter<T>'}

  THasAttributeFilter<T: TRttiObject> = class(TSpecificationBase<T>)
  private
    fAttributeClass: TAttributeClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(attributeClass: TAttributeClass);
  end;

  {$ENDREGION}


  {$REGION 'TTypeFilter<T>'}

  TTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'THasParameterTypesFilter<T>'}

  THasParameterTypesFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypes: TArray<PTypeInfo>;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const types: array of PTypeInfo);
  end;

  {$ENDREGION}


  {$REGION 'TContainsParameterTypeFilter<T>'}

  TContainsParameterTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fTypeInfo: PTypeInfo;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const typeInfo: PTypeInfo);
  end;

  {$ENDREGION}


  TRttiMemberClass = class of TRttiMember;


  {$REGION 'TMemberTypeFilter<T>'}

  TMemberTypeFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fMemberClass: TRttiMemberClass;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(memberClass: TRttiMemberClass);
  end;

  {$ENDREGION}


  {$REGION 'TConstructorFilter<T>'}

  TConstructorFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TInstanceMethodFilter<T>'}

  TInstanceMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TClassMethodFilter<T>'}

  TClassMethodFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasParameterFlagsFilter<T>'}

  THasParameterFlagsFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TParamFlags);
  end;

  {$ENDREGION}


  {$REGION 'TMethodKindFilter<T>'}

  TMethodKindFilter<T: TRttiMember> = class(TSpecificationBase<T>)
  private
    fFlags: TMethodKinds;
  protected
    function IsSatisfiedBy(const member: T): Boolean; override;
  public
    constructor Create(const flags: TMethodKinds);
  end;

  {$ENDREGION}


  {$REGION 'TIsClassFilter'}

  TIsClassFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'TIsInterfaceFilter'}

  TIsInterfaceFilter = class(TSpecificationBase<TRttiType>)
  protected
    function IsSatisfiedBy(const member: TRttiType): Boolean; override;
  end;

  {$ENDREGION}


  {$REGION 'THasFlagsFilter'}

  THasFlagsFilter = class(TSpecificationBase<TRttiParameter>)
  private
    fFlags: TParamFlags;
  protected
    function IsSatisfiedBy(const parameter: TRttiParameter): Boolean; override;
  public
    constructor Create(flags: TParamFlags);
  end;

  {$ENDREGION}


implementation

uses
  RTLConsts,
  StrUtils,
  SysConst,
  Spring.Collections.Extensions,
  Spring.ResourceStrings;

const
  EmptyGuid: TGUID = ();


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
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

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
  item: TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
  if not Assigned(Result) then
    for item in fContext.GetTypes do
      if SameText(item.Name, qualifiedName) then
        Exit(item);
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
  prefix: string;
  typeData: PTypeData;
begin
  while Assigned(typeInfo) and (typeInfo.Kind = tkInterface) do
  begin
    name := GetTypeName(typeInfo);
    for prefix in DelegatePrefixStrings do
      if StartsText(prefix, name) then
        Exit(True);
    typeData := GetTypeData(typeInfo);
    if Assigned(typeData) and Assigned(typeData.IntfParent) then
      typeInfo := typeData.IntfParent^
    else
      typeInfo := nil;
  end;
  Result := False;
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


{$REGION 'TRttiTypeIterator<T>'}

function TRttiTypeIterator<T>.Clone: TIterator<T>;
begin
  Result := TRttiTypeIterator<T>.Create;
end;

function TRttiTypeIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
    fTypes := fContext.GetTypes;
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    while fIndex < High(fTypes) do
    begin
      Inc(fIndex);
      if not (fTypes[fIndex].InheritsFrom(T)) then
        Continue;
      fCurrent := T(fTypes[fIndex]);
      Exit(True);
    end;
  end;
end;

{$ENDREGION}


{$REGION 'TRttiMemberIterator<T>'}

constructor TRttiMemberIterator<T>.Create(const parentType: TRttiType;
  const selector: TFunc<TRttiType,TArray<T>>; enumerateBaseType: Boolean);
begin
  Create(parentType, selector, enumerateBaseType, nil);
end;

constructor TRttiMemberIterator<T>.Create(const parentType: TRttiType;
  const selector: TFunc<TRttiType, TArray<T>>; enumerateBaseType: Boolean;
  const predicate: TPredicate<T>);
begin
  inherited Create;
  fParentType := parentType;
  fSelector := selector;
  fEnumerateBaseType := enumerateBaseType;
  fPredicate := predicate;
end;

function TRttiMemberIterator<T>.Clone: TIterator<T>;
begin
  Result := TRttiMemberIterator<T>.Create(
    fParentType, fSelector, fEnumerateBaseType, fPredicate);
end;

procedure TRttiMemberIterator<T>.Initialize(const targetType: TRttiType);
begin
  fIndex := -1;
  fTargetType := targetType;
  if Assigned(fTargetType) then
    fMembers := fSelector(fTargetType)
  else
    SetLength(fMembers, 0);
end;

function TRttiMemberIterator<T>.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    Initialize(fParentType);
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    repeat
      while fIndex < High(fMembers) do
      begin
        Inc(fIndex);
        if Assigned(fPredicate) and not fPredicate(fMembers[fIndex]) then
          Continue;
        fCurrent := fMembers[fIndex];
        Exit(True);
      end;
      if fEnumerateBaseType then
        Initialize(fTargetType.BaseType)
      else
        Initialize(nil);
    until not Assigned(fTargetType);
    fCurrent := Default(T);
    fState := STATE_FINISHED;
  end;
end;

{$ENDREGION}


{$REGION 'TRttiObjectHelper'}

function TRttiObjectHelper.TryGetCustomAttribute(
  attributeClass: TAttributeClass; out attribute: TCustomAttribute): Boolean;
begin
  attribute := GetCustomAttribute(attributeClass);
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.TryGetCustomAttribute<T>(out attribute: T): Boolean;
begin
  attribute := GetCustomAttribute<T>;
  Result := Assigned(attribute);
end;

function TRttiObjectHelper.GetCustomAttribute(
  attributeClass: TAttributeClass): TCustomAttribute;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(attribute);
  Result := nil;
end;

function TRttiObjectHelper.GetCustomAttribute<T>: T;
begin
  Result := T(GetCustomAttribute(TAttributeClass(T)));
end;

function TRttiObjectHelper.GetCustomAttributes(
  attributeClass: TAttributeClass): TArray<TCustomAttribute>;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := attribute;
    end;
end;

function TRttiObjectHelper.GetCustomAttributes<T>: TArray<T>;
begin
  TArray<TCustomAttribute>(Result) := GetCustomAttributes(TAttributeClass(T));
end;

function TRttiObjectHelper.HasCustomAttribute(
  attributeClass: TAttributeClass): Boolean;
var
  attribute: TCustomAttribute;
begin
  for attribute in GetAttributes do
    if attribute.InheritsFrom(attributeClass) then
      Exit(True);
  Result := False;
end;

function TRttiObjectHelper.HasCustomAttribute<T>: Boolean;
begin
  Result := HasCustomAttribute(TAttributeClass(T));
end;

{$ENDREGION}


{$REGION 'TRttiTypeHelper'}

function TRttiTypeHelper.InternalGetConstructors(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
begin
  Result := TRttiMemberIterator<TRttiMethod>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end, enumerateBaseType, TMethodFilters.IsConstructor());
end;

function TRttiTypeHelper.InternalGetMethods(
  enumerateBaseType: Boolean): IEnumerable<TRttiMethod>;
begin
  Result := TRttiMemberIterator<TRttiMethod>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiMethod>
    begin
      Result := targetType.GetDeclaredMethods;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.InternalGetProperties(
  enumerateBaseType: Boolean): IEnumerable<TRttiProperty>;
begin
  Result := TRttiMemberIterator<TRttiProperty>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiProperty>
    begin
      Result := targetType.GetDeclaredProperties;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.IsAssignableFrom(const rttiType: TRttiType): Boolean;
begin
  Result := Spring.IsAssignableFrom(Handle, rttiType.Handle);
end;

function TRttiTypeHelper.InternalGetFields(
  enumerateBaseType: Boolean): IEnumerable<TRttiField>;
begin
  Result := TRttiMemberIterator<TRttiField>.Create(Self,
    function(targetType: TRttiType): TArray<TRttiField>
    begin
      Result := targetType.GetDeclaredFields;
    end, enumerateBaseType);
end;

function TRttiTypeHelper.GetConstructors: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetConstructors;
end;

function TRttiTypeHelper.GetDefaultName: string;
begin
  if IsPublicType then
    Result := QualifiedName
  else
    Result := Name;
end;

function TRttiTypeHelper.GetMethods: IEnumerable<TRttiMethod>;
begin
  Result := InternalGetMethods;
end;

function TRttiTypeHelper.GetProperties: IEnumerable<TRttiProperty>;
begin
  Result := InternalGetProperties;
end;

function TRttiTypeHelper.GetFields: IEnumerable<TRttiField>;
begin
  Result := InternalGetFields;
end;

// Nullable<TDateTime>
// TDictionary<string, TObject>
// TDictionary<string, IDictionary<string, TObject>>
function TRttiTypeHelper.GetGenericArguments: TArray<TRttiType>;

  function ScanChar(const s: string; var index: Integer): Boolean;
  var
    level: Integer;
  begin
    Result := False;
    level := 0;
    while index <= Length(s) do
    begin
      case s[index] of
        ',': if level = 0 then Exit(True);
        '<': Inc(level);
        '>': Dec(level);
      end;
      Inc(index);
      Result := level = 0;
    end;
  end;

  function SplitTypes(const s: string): TStringDynArray;
  var
    startPos, index: Integer;
  begin
    startPos := 1;
    index := 1;
    while ScanChar(s, index) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Copy(s, startPos, index - startPos);
      Inc(index);
      startPos := index;
    end;
  end;

var
  i: Integer;
  s: string;
  names: TStringDynArray;
begin
  s := Name;
  i := Pos('<', s);
  if i = 0 then
    Exit(nil);
  s := Copy(s, i + 1, Length(s) - i - 1);
  names := SplitTypes(s);
  SetLength(Result, Length(names));
  for i := Low(names) to High(names) do
    Result[i] := TType.FindType(names[i]);
end;

function TRttiTypeHelper.GetGenericTypeDefinition: string;
var
  s: string;
  i: Integer;
begin
  s := Name;
  i := Pos('<', s);
  if i = 0 then
    raise EInvalidOperationException.CreateResFmt(@SNotGenericType, [Name]);
  Result := Copy(s, 0, i) + '>';
end;

function TRttiTypeHelper.GetAncestorCount: Integer;
var
  baseType: TRttiType;
begin
  Result := 0;
  baseType := Self;
  while Assigned(baseType.BaseType) do
  begin
    Inc(Result);
    baseType := baseType.BaseType;
  end;
end;

function TRttiTypeHelper.GetAsClass: TRttiInstanceType;
begin
  Result := Self as TRttiInstanceType;
end;

function TRttiTypeHelper.GetAsDynamicArray: TRttiDynamicArrayType;
begin
  Result := Self as TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetAsInterface: TRttiInterfaceType;
begin
  Result := Self as TRttiInterfaceType;
end;

function TRttiTypeHelper.GetBaseTypes: IReadOnlyList<TRttiType>;
var
  count: Integer;
  t: TRttiType;
  baseTypes: TArray<TRttiType>;
begin
  count := 0;
  t := Self;
  while Assigned(t) do
  begin
    Inc(count);
    t := t.BaseType;
  end;

  SetLength(baseTypes, count);
  count := 0;
  t := Self;
  while Assigned(t) do
  begin
    baseTypes[count] := t;
    Inc(count);
    t := t.BaseType;
  end;

  Result := TArrayIterator<TRttiType>.Create(baseTypes);
end;

function TRttiTypeHelper.GetInterfaces: IEnumerable<TRttiInterfaceType>;
var
  list: IDictionary<TGUID, TRttiInterfaceType>;
  classType: TClass;
  table: PInterfaceTable;
  entry: TInterfaceEntry;
  intfType: TRttiInterfaceType;
  i: Integer;
begin
  if IsClass then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    classType := AsInstance.MetaclassType;
    while Assigned(classType) do
    begin
      table := classType.GetInterfaceTable;
      if Assigned(table) then
      begin
        for i := 0 to table.EntryCount - 1 do
        begin
          entry := table.Entries[i];
          if not list.ContainsKey(entry.IID)
            and not IsEqualGUID(entry.IID, EmptyGuid)
            and TType.TryGetInterfaceType(entry.IID, intfType) then
            list[entry.IID] := intfType;
        end;
      end;
      classType := classType.ClassParent;
    end;
    Result := list.Values;
  end
  else
  if IsInterface then
  begin
    list := TCollections.CreateDictionary<TGUID, TRttiInterfaceType>;
    intfType := AsInterface;
    while Assigned(intfType) do
    begin
      if intfType.HasGuid and not list.ContainsKey(intfType.GUID)
        and not IsEqualGUID(intfType.GUID, EmptyGuid) then
        list[intfType.GUID] := intfType;
      intfType := intfType.BaseType;
    end;
    Result := list.Values;
  end;
end;

function TRttiTypeHelper.GetIsClass: Boolean;
begin
  Result := Self is TRttiInstanceType;
end;

function TRttiTypeHelper.GetIsClassOrInterface: Boolean;
begin
  Result := IsClass or IsInterface;
end;

function TRttiTypeHelper.GetIsDynamicArray: Boolean;
begin
  Result := Self is TRttiDynamicArrayType;
end;

function TRttiTypeHelper.GetIsGenericType: Boolean;
begin
  Result := (Pos('<', Name) > 0) and (Pos('>', Name) > 0);
end;

function TRttiTypeHelper.GetIsInterface: Boolean;
begin
  Result := Self is TRttiInterfaceType;
end;

function TRttiTypeHelper.GetIsLazyType: Boolean;
begin
  Result := TType.IsLazy(Handle);
end;

function TRttiTypeHelper.GetIsString: Boolean;
begin
  Result := TypeKind in [tkString, tkLString, tkWString, tkUString, tkChar, tkWChar];
end;

{$ENDREGION}


{$REGION 'TRttiInterfaceTypeHelper'}

function TRttiInterfaceTypeHelper.GetHasGuid: Boolean;
begin
  Result := ifHasGuid in IntfFlags;
end;

{$ENDREGION}


{$REGION 'TRttiMemberHelper'}

function TRttiMemberHelper.GetValue(const instance: TValue): TValue;
begin
  if IsProperty then
    Result := AsProperty.GetValue(instance)
  else if IsField then
    Result := AsField.GetValue(instance)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_GetValue);
end;

procedure TRttiMemberHelper.SetValue(const instance, value: TValue);
begin
  if IsProperty then
    AsProperty.SetValue(instance, value)
  else if IsField then
    AsField.SetValue(instance, value)
  else
    raise EInvalidOperationException.CreateRes(@SInvalidOperation_SetValue);
end;

function TRttiMemberHelper.GetIsPrivate: Boolean;
begin
  Result := Visibility = mvPrivate;
end;

function TRttiMemberHelper.GetIsProtected: Boolean;
begin
  Result := Visibility = mvProtected;
end;

function TRttiMemberHelper.GetIsPublic: Boolean;
begin
  Result := Visibility = mvPublic;
end;

function TRttiMemberHelper.GetIsPublished: Boolean;
begin
  Result := Visibility = mvPublished;
end;

function TRttiMemberHelper.GetIsConstructor: Boolean;
begin
  Result := (Self is TRttiMethod) and TRttiMethod(Self).IsConstructor;
end;

function TRttiMemberHelper.GetIsProperty: Boolean;
begin
  Result := Self is TRttiProperty;
end;

function TRttiMemberHelper.GetIsMethod: Boolean;
begin
  Result := Self is TRttiMethod;
end;

function TRttiMemberHelper.GetIsField: Boolean;
begin
  Result := Self is TRttiField;
end;

function TRttiMemberHelper.GetAsMethod: TRttiMethod;
begin
  Result := Self as TRttiMethod;
end;

function TRttiMemberHelper.GetAsProperty: TRttiProperty;
begin
  Result := Self as TRttiProperty;
end;

function TRttiMemberHelper.GetAsField: TRttiField;
begin
  Result := Self as TRttiField;
end;

{$ENDREGION}


{$REGION 'TRttiFieldHelper'}

function TRttiFieldHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := AsField.GetValue(instance.AsObject)
  else
    Result := AsField.GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiFieldHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(FieldType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TRttiPropertyHelper'}

function TRttiPropertyHelper.GetValue(const instance: TValue): TValue;
begin
  if instance.IsObject then
    Result := GetValue(instance.AsObject)
  else
    Result := GetValue(instance.GetReferenceToRawData);
end;

procedure TRttiPropertyHelper.SetValue(const instance, value: TValue);
var
  temp: TValue;
begin
  temp := value.Cast(PropertyType.Handle);
  if instance.IsObject then
    SetValue(instance.AsObject, temp)
  else
    SetValue(instance.GetReferenceToRawData, temp);
end;

{$ENDREGION}


{$REGION 'TRttiMethodHelper'}

function TRttiMethodHelper.InternalGetParameters: IEnumerable<TRttiParameter>;
begin
  Result := TEnumerable.Query<TRttiParameter>(GetParameters);
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


{$REGION 'TParameterFilters'}

class function TParameterFilters.HasFlags(
  flags: TParamFlags): TSpecification<TRttiParameter>;
begin
  Result := THasFlagsFilter.Create(flags);
end;

{$ENDREGION}


{$REGION 'Filters'}

{ THasAttributeFilter<T> }

constructor THasAttributeFilter<T>.Create(attributeClass: TAttributeClass);
begin
  inherited Create;
  fAttributeClass := attributeClass;
end;

function THasAttributeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  attribute: TCustomAttribute;
begin
  for attribute in member.GetAttributes do
    if attribute.InheritsFrom(fAttributeClass) then
      Exit(True);
  Result := False;
end;

{ TNameFilter<T> }

constructor TNameFilter<T>.Create(const name: string);
begin
  inherited Create;
  fName := name;
end;

function TNameFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := SameText(TRttiNamedObject(member).Name, fName);
end;

{ TTypeFilter<T> }

constructor TTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.PropertyType.Handle = fTypeInfo
  else if member.IsField then
    Result := member.AsField.FieldType.Handle = fTypeInfo
  else
    Result := False;
end;

{ THasParameterTypesFilter<T> }

constructor THasParameterTypesFilter<T>.Create(const types: array of PTypeInfo);
var
  i: Integer;
begin
  inherited Create;
  SetLength(fTypes, Length(types));
  for i := Low(types) to High(types) do
    fTypes[i] := types[i];
end;

function THasParameterTypesFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  i: Integer;
begin
  parameters := member.AsMethod.GetParameters;
  Result := Length(parameters) = Length(fTypes);
  if Result then
    for i := Low(parameters) to High(parameters) do
      if not IsAssignableFrom(parameters[i].ParamType.Handle, fTypes[i]) then
        Exit(False);
end;

{ TContainsParameterTypeFilter<T> }

constructor TContainsParameterTypeFilter<T>.Create(const typeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := typeInfo;
end;

function TContainsParameterTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.ParamType.Handle = fTypeInfo then
        Exit(True);
  end;
end;

{ THasParameterFlagsFilter<T> }

constructor THasParameterFlagsFilter<T>.Create(const flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasParameterFlagsFilter<T>.IsSatisfiedBy(const member: T): Boolean;
var
  parameters: TArray<TRttiParameter>;
  parameter: TRttiParameter;
begin
  Result := False;
  if member.IsMethod then
  begin
    parameters := member.AsMethod.GetParameters;
    for parameter in parameters do
      if parameter.Flags * fFlags <> [] then
        Exit(True);
  end;
end;

{ TMethodKindFilter<T> }

constructor TMethodKindFilter<T>.Create(const flags: TMethodKinds);
begin
  inherited Create;
  fFlags := flags;
end;

function TMethodKindFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
{$IFDEF DELPHI2010}
  // explicit cast to prevent the compiler from choking
  Result := TRttiMember(member).IsMethod and (TRttiMember(member).AsMethod.MethodKind in fFlags);
{$ELSE}
  Result := member.IsMethod and (member.AsMethod.MethodKind in fFlags);
{$ENDIF}
end;

{ TInvokableFilter<T> }

function TInvokableFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  if member.IsProperty then
    Result := member.AsProperty.IsWritable
  else if member.IsMethod then
    Result := not (member.AsMethod.MethodKind in [mkClassConstructor, mkClassDestructor])
  else
    Result := True;
end;

{ TMemberTypeFilter<T> }

constructor TMemberTypeFilter<T>.Create(memberClass: TRttiMemberClass);
begin
  inherited Create;
  fMemberClass := memberClass;
end;

function TMemberTypeFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.InheritsFrom(fMemberClass);
end;

{ TConstructorFilter<T> }

function TConstructorFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsConstructor;
end;

{ TInstanceMethodFilter<T> }

function TInstanceMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and not member.AsMethod.IsClassMethod;
end;

{ TClassMethodFilter<T> }

function TClassMethodFilter<T>.IsSatisfiedBy(const member: T): Boolean;
begin
  Result := member.IsMethod and member.AsMethod.IsClassMethod;
end;

{ TIsClassFilter }

function TIsClassFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member.IsInstance;
end;

{ TIsInterfaceFilter }

function TIsInterfaceFilter.IsSatisfiedBy(const member: TRttiType): Boolean;
begin
  Result := member is TRttiInterfaceType;
end;

{ THasFlagsFilter }

constructor THasFlagsFilter.Create(flags: TParamFlags);
begin
  inherited Create;
  fFlags := flags;
end;

function THasFlagsFilter.IsSatisfiedBy(
  const parameter: TRttiParameter): Boolean;
begin
  Result := parameter.Flags * fFlags = fFlags;
end;

{$ENDREGION}


{$REGION 'TReflection'}

class constructor TReflection.Create;
begin
  fContext := TRttiContext.Create;
end;

class destructor TReflection.Destroy;
begin
  fContext.Free;
end;

function TReflection.FindType(const qualifiedName: string): TRttiType;
begin
  Result := fContext.FindType(qualifiedName);
end;

function TReflection.GetClasses: IEnumerable<TRttiInstanceType>;
begin
  Result := TRttiTypeIterator<TRttiInstanceType>.Create;
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

function TReflection.GetInterfaces: IEnumerable<TRttiInterfaceType>;
begin
  Result := TRttiTypeIterator<TRttiInterfaceType>.Create;
end;

function TReflection.GetType(const typeInfo: PTypeInfo): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(typeInfo, 'typeInfo');
{$ENDIF}

  Result := fContext.GetType(typeInfo);
end;

function TReflection.GetType(const classType: TClass): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(classType, 'classType');
{$ENDIF}

  Result := fContext.GetType(classType.ClassInfo);
end;

function TReflection.GetType(const instance: TObject): TRttiType;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(instance, 'instance');
{$ENDIF}

  Result := fContext.GetType(instance.ClassInfo);
end;

function TReflection.GetType(const instance: TValue): TRttiType;
begin
  Result := fContext.GetType(instance.TypeInfo);
end;

function TReflection.GetTypes: IEnumerable<TRttiType>;
begin
  Result := TRttiTypeIterator<TRttiType>.Create;
end;

{$ENDREGION}


end.
