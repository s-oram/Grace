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

unit Spring.Tests.Container;

{$I Spring.inc}
{$I Spring.Tests.inc}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Container,
  Spring.Services,
  // DO NOT CHANGE ORDER OF FOLLOWING UNITS !!!
  Spring.Tests.Container.Interfaces,
  Spring.Tests.Container.Components;

type
  TContainerTestCase = class abstract(TTestCase)
  protected
    fContainer: TContainer;
    procedure CheckIs(AInterface: IInterface; AClass: TClass; msg: string = ''); overload;
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestEmptyContainer = class(TContainerTestCase)
  published
    procedure TestResolveUnknownIntferfaceService;
//    procedure TestResolveUnknownClassService;
    procedure TestRegisterNonGuidInterfaceService;
    procedure TestRegisterGenericInterfaceService;
    procedure TestRegisterUnassignableService;
    procedure TestRegisterTwoUnnamedServicesImplicit;
    procedure TestRegisterTwoUnnamedServicesExplicit;
    procedure TestRegisterTwoNamedDifferentServices;
    procedure TestResolveReturnsUnnamedService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
  end;

  TTestSimpleContainer = class(TContainerTestCase)
  published
    procedure TestIssue13;
    procedure TestInterfaceService;
    procedure TestAbstractClassService;
    procedure TestServiceSameAsComponent;
    procedure TestBootstrap;
    procedure TestSingleton;
    procedure TestTransient;
    procedure TestPerThread;
    procedure TestPooled;
    procedure TestPooledRefCountedInterface;
    procedure TestInitializable;
    procedure TestRecyclable;

    procedure TestIssue41_DifferentName;
    procedure TestIssue41_DifferentService;
    procedure TestIssue41_DifferentLifetimes;

    procedure TestIssue49;
    procedure TestIssue50;
  end;

  // Same Service, Different Implementations
  TTestDifferentServiceImplementations = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAnotherNameService: INameService;
    fServices: TArray<INameService>;
    fServiceValues: TArray<TValue>;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAnotherNameService;
    procedure TestResolveAll;
    procedure TestResolveAllNonGeneric;
    procedure TestUnsatisfiedDependency;
//    procedure TestUnsatisfiedDependencyOfBootstrap;
  end;

  // Same Component, Different Services
  TTestImplementsDifferentServices = class(TContainerTestCase)
  private
    fNameService: INameService;
    fAgeService: IAgeService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNameService;
    procedure TestAgeService;
  end;

  TTestActivatorDelegate = class(TContainerTestCase)
  private
    fPrimitive: IPrimitive;
    fExpectedInteger: Integer;
    fExpectedString: string;
  protected
    procedure SetUp; override;
  published
    procedure TestNameService;
    procedure TestIntegerArgument;
    procedure TestStringArgument;
  end;

  TTypedInjectionTestCase = class abstract(TContainerTestCase)
  private
    fNameService: INameService;
    fInjectionExplorer: IInjectionExplorer;
  protected
    procedure DoRegisterComponents; virtual; abstract;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestTypedInjectionByCoding = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestTypedInjectionsByAttribute = class(TTypedInjectionTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TNamedInjectionsTestCase = class(TContainerTestCase)
  private
    fExplorer: IInjectionExplorer;
  protected
    procedure DoRegisterComponents; virtual;
    procedure SetUp; override;
  published
    procedure TestConstructorInjection;
    procedure TestMethodInjection;
    procedure TestPropertyInjection;
    procedure TestFieldInjection;
  end;

  TTestNamedInjectionsByCoding = class(TNamedInjectionsTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestNamedInjectionsByAttribute = class(TNamedInjectionsTestCase)
  protected
    procedure DoRegisterComponents; override;
  end;

  TTestDirectCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolve;
  end;

  TTestCrossedCircularDependency = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestResolveChicken;
    procedure TestResolveEgg;
  end;

  TTestImplementsAttribute = class(TContainerTestCase)
  published
    procedure TestImplements;
  end;

  TTestRegisterInterfaces = class(TContainerTestCase)
  published
    procedure TestOneService;
    procedure TestTwoServices;
    procedure TestInheritedService;
    procedure TestTwoServicesWithSameName;
  end;

  TTestDefaultResolve = class(TContainerTestCase)
  published
    procedure TestResolve;
    procedure TestResolveDependency;
    procedure TestRegisterDefault;
  end;

  TTestInjectionByValue = class(TContainerTestCase)
  published
    procedure TestInjectField;
  end;

  TTestResolverOverride = class(TContainerTestCase)
  private
    fDummy: TObject;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestResolve;
    procedure TestResolveWithClass;
    procedure TestResolveWithMultipleParams;
    procedure TestResolveWithDependency;
  end;

  TTestRegisterInterfaceTypes = class(TContainerTestCase)
  published
    procedure TestOneService;
    procedure TestTwoServices;
    procedure TestOneServiceAsSingleton;
    procedure TestOneServiceAsSingletonPerThread;
  end;

  TTestLazyDependencies = class(TContainerTestCase)
  private
    fCalled: Boolean;
  protected
    procedure SetUp; override;
    procedure PerformChecks; virtual;
  published
    procedure TestDependencyTypeIsFunc;
{$IFNDEF IOSSIMULATOR}
    procedure TestDependencyTypeIsRecord;
    procedure TestDependencyTypeIsInterface;
{$ELSE}
    {$MESSAGE WARN 'Test me again later'}
    // These tests fail due to some compiler/RTL problem, the circular
    // dependency excpetion is raised but during re-raise in some finally block
    // AV is raised instead. This is rare corner case not considered to be a
    // major issue.
{$ENDIF}
  end;

  TTestLazyDependenciesDetectRecursion = class(TTestLazyDependencies)
  protected
    procedure PerformChecks; override;
  end;

  TTestDecoratorExtension = class(TContainerTestCase)
  published
    procedure TestResolveReturnsDecorator;
    procedure TestResolveWithResolverOverride;
  end;

  TTestManyDependencies = class(TContainerTestCase)
  protected
    procedure SetUp; override;
  published
    procedure TestInjectArray;
    procedure TestInjectEnumerable;
    procedure TestInjectArrayOfLazy;
    procedure TestNoRecursion;
    procedure TestResolveArrayOfLazy;
  end;


implementation

uses
  Spring.Collections,
  Spring.Container.DecoratorExtension,
  Spring.Container.Resolvers;


{$REGION 'TContainerTestCase'}

procedure TContainerTestCase.CheckIs(AInterface: IInterface; AClass: TClass; msg: string);
begin
  CheckIs(AInterface as TObject, AClass, msg);
end;

procedure TContainerTestCase.SetUp;
begin
  inherited;
  fContainer := TContainer.Create;
end;

procedure TContainerTestCase.TearDown;
begin
  fContainer.Free;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestEmptyContainer'}

procedure TTestEmptyContainer.TestResolveUnknownIntferfaceService;
begin
  ExpectedException := EResolveException;
  fContainer.Resolve<INameService>;
end;

//procedure TTestEmptyContainer.TestResolveUnknownClassService;
//begin
//  ExpectedException := EResolveException;
//  fContainer.Resolve<TFoo2>;
//end;

procedure TTestEmptyContainer.TestRegisterNonGuidInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TNonGuid>.Implements<INonGuid>;
end;

procedure TTestEmptyContainer.TestRegisterTwoNamedDifferentServices;
var
  nameService: INameService;
  ageService: IAgeService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>;
  fContainer.Build;
  nameService := fContainer.Resolve<INameService>;
  CheckTrue(nameService is TNameService);
  ageService := fContainer.Resolve<IAgeService>;
  CheckTrue(ageService is TNameAgeComponent);
end;

procedure TTestEmptyContainer.TestRegisterTwoUnnamedServicesExplicit;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TAnotherNameService);
end;

procedure TTestEmptyContainer.TestRegisterTwoUnnamedServicesImplicit;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>;
  fContainer.RegisterType<TAnotherNameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TAnotherNameService);
end;

procedure TTestEmptyContainer.TestRegisterGenericInterfaceService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TNonGuid<TObject>>.Implements<INonGuid<TObject>>;
end;

procedure TTestEmptyContainer.TestRegisterUnassignableService;
begin
  ExpectedException := ERegistrationException;
  fContainer.RegisterType<TContainer>.Implements<IDispatch>;
end;

procedure TTestEmptyContainer.TestResolveAll;
var
  services: TArray<INameService>;
begin
  services := fContainer.ResolveAll<INameService>;
  CheckEquals(0, Length(services));
end;

procedure TTestEmptyContainer.TestResolveAllNonGeneric;
var
  services: TArray<TValue>;
begin
  services := fContainer.ResolveAll(TypeInfo(INameService));
  CheckEquals(0, Length(services));
end;

procedure TTestEmptyContainer.TestResolveReturnsUnnamedService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('some');
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TNameService, 'service should be a TNameService instance.');
end;

{$ENDREGION}


{$REGION 'TTestSimpleContainer'}

procedure TTestSimpleContainer.TestInterfaceService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  try
    CheckNotNull(service, 'service should not be nil.');
    CheckTrue(service is TNameService, 'service should be a TNameService instance.');
    CheckEquals(TNameService.NameString, service.Name);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ENDIF}
    service := nil;
  end;
end;

procedure TTestSimpleContainer.TestIssue13;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>.AsSingleton;
  fContainer.Build;
  FCheckCalled := True;
end;

procedure TTestSimpleContainer.TestAbstractClassService;
var
  service: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>.Implements<TAgeServiceBase>;
  fContainer.Build;
  service := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckIs(service, TAgeServiceImpl, 'service should be a TNameService instance.');
    CheckEquals(TAgeServiceImpl.DefaultAge, service.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ELSE}
    service:=nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestServiceSameAsComponent;
var
  service: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>;
  fContainer.Build;
  service := fContainer.Resolve<TAgeServiceImpl>;
  try
    CheckNotNull(service, 'service should not be null.');
    CheckEquals(TAgeServiceImpl.DefaultAge, service.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(service);
{$ELSE}
    service := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestBootstrap;
var
  component: TBootstrapComponent;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>.AsSingleton;
  fContainer.RegisterType<TAgeServiceImpl>.Implements<TAgeServiceBase>.AsSingleton;
  fContainer.RegisterType<TBootstrapComponent>;
  fContainer.Build;
  component := fContainer.Resolve<TBootstrapComponent>;
  try
    CheckNotNull(component, 'component should not be nil.');
    CheckNotNull(component.NameService, 'NameService');
    CheckEquals(TNameService.NameString, component.NameService.Name);
    CheckNotNull(component.AgeService, 'AgeService');
    CheckEquals(TAgeServiceImpl.DefaultAge, component.AgeService.Age);
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(component);
{$ELSE}
    component := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestSingleton;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>
    .Implements<TAgeServiceBase>
    .AsSingleton;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckSame(obj1, obj2, 'obj1 should be the same as obj2.');
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(obj1);
    fContainer.Release(obj2);
{$ELSE}
    obj1 := nil;
    obj2 := nil;
{$ENDIF}
  end;
end;

procedure TTestSimpleContainer.TestTransient;
var
  obj1, obj2: TAgeServiceBase;
begin
  fContainer.RegisterType<TAgeServiceImpl>
    .Implements<TAgeServiceBase>;
  fContainer.Build;
  obj1 := fContainer.Resolve<TAgeServiceBase>;
  obj2 := fContainer.Resolve<TAgeServiceBase>;
  try
    CheckNotNull(obj1, 'obj1 should not be nil');
    CheckNotNull(obj2, 'obj2 should not be nil');
    CheckTrue(obj1 <> obj2, 'obj1 should not be the same as obj2.');
  finally
{$IFNDEF AUTOREFCOUNT}
    fContainer.Release(obj1);
    fContainer.Release(obj2);
{$ELSE}
    obj1 := nil;
    obj2 := nil;
{$ENDIF}
  end;
end;

type
  TTestSingletonThread = class(TThread)
  protected
    fContainer: TContainer;
    fService1: INameService;
    fService2: INameService;
    procedure Execute; override;
  public
    constructor Create(container: TContainer);
    property Service1: INameService read fService1;
    property Service2: INameService read fService2;
  end;

{ TTestSingletonThread }

constructor TTestSingletonThread.Create(container: TContainer);
begin
  inherited Create(False);
  fContainer := container;
end;

procedure TTestSingletonThread.Execute;
begin
  fService1 := fContainer.Resolve<INameService>;
  fService2 := fContainer.Resolve<INameService>;
end;

procedure TTestSimpleContainer.TestPerThread;
var
  thread1, thread2: TTestSingletonThread;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingletonPerThread;
  fContainer.Build;
  thread1 := TTestSingletonThread.Create(fContainer);
  thread2 := TTestSingletonThread.Create(fContainer);
  try
    thread1.WaitFor;
    thread2.WaitFor;
    CheckTrue(thread1.Service1 is TNameService, 'thread1.Service1 should be TNameService.');
    CheckTrue(thread2.Service1 is TNameService, 'thread2.Service1 should be TNameService.');
    CheckSame(thread1.Service1, thread1.Service2, 'thread1');
    CheckSame(thread2.Service1, thread2.Service2, 'thread2');
    CheckTrue(thread1.Service1 <> thread2.Service2, 'thread1 and thread2 should own different instances.');
  finally
    thread1.Free;
    thread2.Free;
  end;
end;

procedure TTestSimpleContainer.TestPooled;
var
  service1, service2, service3: INameService;
  count: Integer;
begin
{$IFDEF NEXTGEN}
  {$MESSAGE WARN 'Fix me'}
  Exit;  //Fails on nextgen, probably object is nil
{$ENDIF}
  count := 0;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .DelegateTo(
      function: TNameService
      begin
        Result := TNameService.Create;
        Inc(count);
      end)
    .AsPooled(2, 2);
  fContainer.Build;
  CheckEquals(0, count); // pool did not create any instances yet
  service1 := fContainer.Resolve<INameService>;
  CheckEquals(2, count); // pool created the minimum number of instances upon first request
  service2 := fContainer.Resolve<INameService>;
  service3 := fContainer.Resolve<INameService>;
  CheckEquals(3, count); // pool created one additional instance
  service3 := nil;
  service2 := nil;
  service1 := nil;
  service1 := fContainer.Resolve<INameService>; // pool collects unused instances up to maximum count
  service2 := fContainer.Resolve<INameService>;
  CheckEquals(3, count);
  service3 := fContainer.Resolve<INameService>; // pool creates a new instance again
  CheckEquals(4, count);
end;

procedure TTestSimpleContainer.TestPooledRefCountedInterface;
var
  service1, service2, service3: INameService;
  count: Integer;
begin
{$IFDEF NEXTGEN}
  {$MESSAGE WARN 'Fix me'}
  Exit; //Fails on nextgen, probably object is nil
{$ENDIF}
  count := 0;
  fContainer.RegisterType<TCustomNameService>
    .Implements<INameService>
    .DelegateTo(
      function: TCustomNameService
      begin
        Result := TCustomNameService.Create;
        Inc(count);
      end)
    .AsPooled(2, 2);
  fContainer.Build;
  CheckEquals(0, count); // pool did not create any instances yet
  service1 := fContainer.Resolve<INameService>;
  CheckEquals(2, count); // pool created the minimum number of instances upon first request
  service2 := fContainer.Resolve<INameService>;
  service3 := fContainer.Resolve<INameService>;
  CheckEquals(3, count); // pool created one additional instance
  service3 := nil;
  service2 := nil;
  service1 := nil;
  service1 := fContainer.Resolve<INameService>; // pool collects unused instances up to maximum count
  service2 := fContainer.Resolve<INameService>;
  CheckEquals(3, count);
  service3 := fContainer.Resolve<INameService>; // pool creates a new instance again
  CheckEquals(4, count);
end;

procedure TTestSimpleContainer.TestRecyclable;
var
  service1, service2: IAnotherService;
  instance: TRecyclableComponent;
begin
{$IFDEF NEXTGEN}
  {$MESSAGE WARN 'Fix me'}
  //Fails on nextgen
  Exit;
{$ENDIF}
  fContainer.RegisterType<TRecyclableComponent>.AsPooled(2, 2);
  fContainer.Build;
  service1 := fContainer.Resolve<IAnotherService>;
  service2 := fContainer.Resolve<IAnotherService>;
  CheckTrue(service2 is TRecyclableComponent, 'Unknown component');
  instance := TRecyclableComponent(service2); // remember second because the first will be returned again later
  CheckTrue(instance.IsInitialized, 'IsInitialized');
  CheckFalse(instance.IsRecycled, 'IsRecycled');
  service1 := nil;
  service2 := nil;
  service1 := fContainer.Resolve<IAnotherService>; // pool has no available and collects all unused
  CheckFalse(instance.IsInitialized, 'IsInitialized');
  CheckTrue(instance.IsRecycled, 'IsRecycled');
end;

procedure TTestSimpleContainer.TestInitializable;
var
  service: IAnotherService;
begin
  fContainer.RegisterType<TInitializableComponent>;
  fContainer.Build;
  service := fContainer.Resolve<IAnotherService>;
  CheckTrue(service is TInitializableComponent, 'Unknown component.');
  CheckTrue(TInitializableComponent(service).IsInitialized, 'IsInitialized');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentLifetimes;
var
  service1, service2: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>;
  fContainer.RegisterType<TDynamicNameService>.Implements<IAnotherNameService>.AsSingleton;
  fContainer.Build;

  service1 := fContainer.Resolve<INameService>;
  service2 := fContainer.Resolve<IAnotherNameService>;

  Check((service1 as TObject) <> (service2 as TObject), 'resolved services should be different');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentName;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('first').DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('first');
    end
    );
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('second').DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('second');
    end
    );
  fContainer.Build;

  service := fContainer.Resolve<INameService>('second');
  CheckEquals('second', service.Name, 'resolving of service "second" failed');

  service := fContainer.Resolve<INameService>('first');
  CheckEquals('first', service.Name, 'resolving of service "first" failed');
end;

procedure TTestSimpleContainer.TestIssue41_DifferentService;
var
  service: INameService;
  anotherService: IAnotherNameService;
begin
   fContainer.RegisterType<TDynamicNameService>.Implements<INameService>.DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('first');
    end
    );
  fContainer.RegisterType<TDynamicNameService>.Implements<IAnotherNameService>.DelegateTo(
    function: TDynamicNameService
    begin
      Result := TDynamicNameService.Create('second');
    end
    );
  fContainer.Build;

  anotherService := fContainer.Resolve<IAnotherNameService>;
  CheckEquals('second', anotherService.Name, 'resolving of service "second" failed');

  service := fContainer.Resolve<INameService>;
  CheckEquals('first', service.Name, 'resolving of service "first" failed');
end;

procedure TTestSimpleContainer.TestIssue49;
var
  count: Integer;
  obj: TNameServiceWithAggregation;
begin
  fContainer.RegisterType<TNameServiceWithAggregation>.Implements<INameService>
    .InjectField('fAgeService').InjectField('fAgeService')
    .InjectProperty('AgeService').InjectProperty('AgeService')
    .InjectMethod('Init').InjectMethod('Init');
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>.DelegateTo(
    function: TNameAgeComponent
    begin
      Result := TNameAgeComponent.Create;
      Inc(count);
    end);
  fContainer.Build;
  fContainer.Build;

  count := 0;
  obj := fContainer.Resolve<INameService> as TNameServiceWithAggregation;
  CheckEquals(2, count); // field and property
  CheckEquals(1, obj.MethodCallCount);
end;

procedure TTestSimpleContainer.TestIssue50;
var
  count: Integer;
  obj: TNameServiceWithAggregation;
begin
  fContainer.RegisterType<TNameServiceWithAggregation>.Implements<INameService>.DelegateTo(
    function: TNameServiceWithAggregation
    begin
      Result := TNameServiceWithAggregation.Create;
    end);
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>.DelegateTo(
    function: TNameAgeComponent
    begin
      Result := TNameAgeComponent.Create;
      Inc(count);
    end);
  fContainer.Build;

  count := 0;
  obj := fContainer.Resolve<INameService> as TNameServiceWithAggregation;
  CheckEquals(2, count); // field and property
  CheckEquals(1, obj.MethodCallCount);
end;

{$ENDREGION}


{$REGION 'TTestDifferentImplementations'}

procedure TTestDifferentServiceImplementations.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('default')
    .AsSingleton;
  fContainer.RegisterType<TAnotherNameService>
    .Implements<INameService>('another');
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAnotherNameService := fContainer.Resolve<INameService>('another');
  fServices := fContainer.ResolveAll<INameService>;
  fServiceValues := fContainer.ResolveAll(TypeInfo(INameService));
end;

procedure TTestDifferentServiceImplementations.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fAnotherNameService);
{$ENDIF}
  fAnotherNameService := nil;

{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fNameService);
{$ENDIF}
  fNameService := nil;

  inherited TearDown;
end;

procedure TTestDifferentServiceImplementations.TestNameService;
begin
  CheckNotNull(fNameService, 'fNameService should not be nil.');
  CheckTrue(fNameService is TNameService, 'fNameService should be an instance of TNameService.');
  CheckEquals(TNameService.NameString, fNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestAnotherNameService;
begin
  CheckNotNull(fAnotherNameService, 'fAnotherNameService should not be nil.');
  CheckTrue(fAnotherNameService is TAnotherNameService, 'fAnotherNameService should be an instance of TAnotherNameService.');
  CheckEquals(TAnotherNameService.NameString, fAnotherNameService.Name);
end;

procedure TTestDifferentServiceImplementations.TestResolveAll;
begin
  CheckEquals(2, Length(fServices), 'Count of fServices should be 2.');
  CheckTrue(fServices[0] is TNameService);
  CheckTrue(fServices[1] is TAnotherNameService);
end;

procedure TTestDifferentServiceImplementations.TestResolveAllNonGeneric;
begin
  CheckEquals(2, Length(fServiceValues), 'Count of fServiceValues should be 2.');
  CheckTrue((fServiceValues[0].AsType<INameService>) is TNameService);
  CheckTrue((fServiceValues[1].AsType<INameService>) is TAnotherNameService);
end;

/// <remarks>
/// An EUnsatisfiedDependencyException will be raised when resolving a service type
//  with an ambiguous name.
/// </remarks>
procedure TTestDifferentServiceImplementations.TestUnsatisfiedDependency;
begin
  ExpectedException := EUnsatisfiedDependencyException;
  fContainer.Resolve<INameService>;
end;

//procedure TTestDifferentServiceImplementations.TestUnsatisfiedDependencyOfBootstrap;
//begin
//  ExpectedException := EUnsatisfiedDependencyException;
//  fContainer.Resolve<TBootstrapComponent>;
//end;

{$ENDREGION}


{$REGION 'TTestActivatorDelegate'}

procedure TTestActivatorDelegate.SetUp;
begin
  inherited SetUp;
  fExpectedInteger := 26;
  fExpectedString := 'String';
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TPrimitiveComponent>
    .Implements<IPrimitive>
    .DelegateTo(
      function: TPrimitiveComponent
      begin
        Result := TPrimitiveComponent.Create(
          fContainer.Resolve<INameService>,
          fExpectedInteger,
          fExpectedString
        );
      end
    );
  fContainer.Build;
  fPrimitive := fContainer.Resolve<IPrimitive>;
  CheckNotNull(fPrimitive, 'fPrimitive should not be nil.');
  CheckNotNull(fPrimitive.NameService, 'fPrimitive.NameService should not be nil.');
end;

procedure TTestActivatorDelegate.TestNameService;
begin
  CheckNotNull(fPrimitive.NameService, 'NameService should not be nil.');
  CheckTrue(fPrimitive.NameService is TNameService, 'Unexpected type.');
  CheckEquals(TNameService.NameString, fPrimitive.NameService.Name);
end;

procedure TTestActivatorDelegate.TestIntegerArgument;
begin
  CheckEquals(fExpectedInteger, fPrimitive.IntegerArg);
end;

procedure TTestActivatorDelegate.TestStringArgument;
begin
  CheckEquals(fExpectedString, fPrimitive.StringArg);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjections'}

procedure TTypedInjectionTestCase.SetUp;
begin
  inherited SetUp;
  DoRegisterComponents;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>;
  CheckIs(fNameService, TNameService, 'fNameService should be TNameService.');
  CheckEquals(TNameService.NameString, fNameService.Name, 'fNameService.Name is wrong.');
  fInjectionExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TTypedInjectionTestCase.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fInjectionExplorer);
  fContainer.Release(fNameService);
{$ENDIF}
  fInjectionExplorer := nil;
  fNameService := nil;
  inherited TearDown;
end;

procedure TTypedInjectionTestCase.TestConstructorInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.ConstructorInjection);
end;

procedure TTypedInjectionTestCase.TestPropertyInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.PropertyInjection);
end;

procedure TTypedInjectionTestCase.TestMethodInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.MethodInjection);
end;

procedure TTypedInjectionTestCase.TestFieldInjection;
begin
  CheckSame(fNameService, fInjectionExplorer.FieldInjection);
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionByCoding'}

procedure TTestTypedInjectionByCoding.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor([TypeInfo(INameService)])
    .InjectProperty('PropertyInjection')
    .InjectMethod('SetMethodInjection')
    .InjectField('fFieldInjection');
end;

{$ENDREGION}


{$REGION 'TTestTypedInjectionsByAttribute'}

procedure TTestTypedInjectionsByAttribute.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>
    .AsSingleton;
  fContainer.RegisterType<TInjectionExplorerComponent>
    .Implements<IInjectionExplorer>;
end;

{$ENDREGION}


{$REGION 'TTestDirectCircularDependency'}

procedure TTestDirectCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.Build;
end;

procedure TTestDirectCircularDependency.TestResolve;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
  Check(true, 'This must pass');
end;

{$ENDREGION}


{$REGION 'TTestCrossedCircularDependency'}

procedure TTestCrossedCircularDependency.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TCircularDependencyChicken>.Implements<IChicken>;
  fContainer.RegisterType<TEgg>.Implements<IEgg>;
  fContainer.Build;
end;

procedure TTestCrossedCircularDependency.TestResolveChicken;
var
  chicken: IChicken;
begin
  ExpectedException := ECircularDependencyException;
  chicken := fContainer.Resolve<IChicken>;
end;

procedure TTestCrossedCircularDependency.TestResolveEgg;
var
  egg: IEgg;
begin
  ExpectedException := ECircularDependencyException;
  egg := fContainer.Resolve<IEgg>;
end;

{$ENDREGION}


{$REGION 'TNamedInjectionsTestCase'}

procedure TNamedInjectionsTestCase.DoRegisterComponents;
begin
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('default')
    .AsSingleton;
  fContainer.RegisterType<TAnotherNameService>
    .Implements<INameService>('another')
    .AsSingleton;
end;

procedure TNamedInjectionsTestCase.SetUp;
begin
  inherited SetUp;
  DoRegisterComponents;
  fContainer.Build;
  fExplorer := fContainer.Resolve<IInjectionExplorer>;
end;

procedure TNamedInjectionsTestCase.TestConstructorInjection;
begin
  CheckNotNull(fExplorer.ConstructorInjection);
  CheckEquals(TNameService.NameString, fExplorer.ConstructorInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestPropertyInjection;
begin
  CheckNotNull(fExplorer.PropertyInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.PropertyInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestMethodInjection;
begin
  CheckNotNull(fExplorer.MethodInjection);
  CheckEquals(TAnotherNameService.NameString, fExplorer.MethodInjection.Name);
end;

procedure TNamedInjectionsTestCase.TestFieldInjection;
begin
  CheckNotNull(fExplorer.FieldInjection);
  CheckEquals(TNameService.NameString, fExplorer.FieldInjection.Name);
end;

{$ENDREGION}


{$REGION 'TTestNamedInjectionsByCoding'}

procedure TTestNamedInjectionsByCoding.DoRegisterComponents;
begin
  inherited DoRegisterComponents;
  fContainer.RegisterType<TInjectionExplorer>
    .Implements<IInjectionExplorer>
    .InjectConstructor(['default'])
    .InjectProperty('PropertyInjection', 'another')
    .InjectMethod('SetMethodInjection', ['another'])
    .InjectField('fFieldInjection', 'default')
    .AsSingleton;
end;

{$ENDREGION}


{$REGION 'TTestNamedInjectionsByAttribute'}

procedure TTestNamedInjectionsByAttribute.DoRegisterComponents;
begin
  inherited DoRegisterComponents;
  fContainer.RegisterType<TInjectionComponent>
    .Implements<IInjectionExplorer>;
end;

{$ENDREGION}


{$REGION 'TTestImplementsDifferentServices'}

procedure TTestImplementsDifferentServices.SetUp;
begin
  inherited SetUp;
  fContainer.RegisterType<TNameService>
    .Implements<INameService>('another');
  fContainer.RegisterType<TNameAgeComponent>
    .Implements<INameService>('default')
    .Implements<IAgeService>
    .AsSingleton;
  fContainer.Build;
  fNameService := fContainer.Resolve<INameService>('default');
  fAgeService := fContainer.Resolve<IAgeService>;
  CheckNotNull(fNameService, 'fNameService should not be nil.');
  CheckNotNull(fAgeService, 'fAgeService should not be nil.');
end;

procedure TTestImplementsDifferentServices.TearDown;
begin
{$IFNDEF AUTOREFCOUNT}
  fContainer.Release(fAgeService);
  fContainer.Release(fNameService);
{$ENDIF}
  fAgeService := nil;
  fNameService := nil;
  inherited TearDown;
end;

procedure TTestImplementsDifferentServices.TestNameService;
begin
  Check(fNameService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.NameString, fNameService.Name);
end;

procedure TTestImplementsDifferentServices.TestAgeService;
begin
  Check(fAgeService is TNameAgeComponent);
  CheckEquals(TNameAgeComponent.DefaultAge, fAgeService.Age);
end;

{$ENDREGION}


{$REGION 'TTestImplementsAttribute'}

type
  IS1 = interface
    ['{E6DE68D5-988C-4817-880E-58903EE8B78C}']
  end;

  IS2 = interface
    ['{0DCC1BD5-28C1-4A94-8667-AC72BA25C682}']
  end;

  TS1 = class(TInterfacedObject, IS1)
  end;

  [Implements(TypeInfo(IS1), 'b')]
  [Implements(TypeInfo(IS2))]
  TS2 = class(TInterfacedObject, IS1, IS2)
  end;

procedure TTestImplementsAttribute.TestImplements;
var
  s1: IS1;
  s2: IS2;
begin
  fContainer.RegisterType<TS1>.Implements<IS1>('a');
  fContainer.RegisterType<TS2>;
  fContainer.Build;
  s1 := fContainer.Resolve<IS1>('a');
  CheckTrue(s1 is TS1, 'a');
  s1 := fContainer.Resolve<IS1>('b');
  CheckTrue(s1 is TS2, 'b');
  s2 := fContainer.Resolve<IS2>;
  CheckTrue(s2 is TS2, 's2');
end;

{$ENDREGION}


{$REGION 'TTestRegisterInterfaces'}

type
  TComplex = class(TNameAgeComponent, IAnotherService)
  end;

procedure TTestRegisterInterfaces.TestOneService;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>;
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckTrue(service is TNameService);
end;

procedure TTestRegisterInterfaces.TestTwoServices;
var
  s1: INameService;
  s2: IAgeService;
begin
  fContainer.RegisterType<TNameAgeComponent>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<IAgeService>;
  CheckTrue(s1 is TNameAgeComponent, 's1');
  CheckTrue(s2 is TNameAgeComponent, 's2');
end;

procedure TTestRegisterInterfaces.TestTwoServicesWithSameName;
var
  s1: INameService;
  s2: Spring.Tests.Container.Interfaces.INameService;
begin
  fContainer.RegisterType<TNameServiceWithTwoInterfaces>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<Spring.Tests.Container.Interfaces.INameService>;
  CheckTrue(s1 is TNameServiceWithTwoInterfaces, 's1');
  CheckTrue(s2 is TNameServiceWithTwoInterfaces, 's2');
end;

procedure TTestRegisterInterfaces.TestInheritedService;
var
  s1: INameService;
  s2: IAgeService;
  s3: IAnotherService;
begin
  CheckTrue(Assigned(TypeInfo(IAnotherService)));
  fContainer.RegisterType<TComplex>;
  fContainer.Build;
  s1 := fContainer.Resolve<INameService>;
  s2 := fContainer.Resolve<IAgeService>;
  s3 := fContainer.Resolve<IAnotherService>;
  CheckTrue(s1 is TComplex, 's1');
  CheckTrue(s2 is TComplex, 's2');
  CheckTrue(s3 is TComplex, 's3');
end;

{$ENDREGION}


{$REGION 'TTestDefaultResolve'}

procedure TTestDefaultResolve.TestRegisterDefault;
begin
  StartExpectingException(ERegistrationException);
  fContainer.RegisterType<INameService, TNameService>.AsDefault<IAgeService>;
  StopExpectingException;
end;

procedure TTestDefaultResolve.TestResolve;
var
  service: INameService;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('another');
  fContainer.Build;
  service := fContainer.Resolve<INameService>;
  CheckEquals('Name', service.Name);
end;

procedure TTestDefaultResolve.TestResolveDependency;
var
  component: TBootstrapComponent;
begin
  fContainer.RegisterType<TNameService>.Implements<INameService>;
  fContainer.RegisterType<TAnotherNameService>.Implements<INameService>('another');
  fContainer.RegisterType<TBootstrapComponent>.AsSingleton;
  fContainer.Build;
  component := fContainer.Resolve<TBootstrapComponent>;
  CheckEquals('Name', component.NameService.Name);
end;

{$ENDREGION}


{$REGION 'TTestInjectionByValue'}

procedure TTestInjectionByValue.TestInjectField;
begin
  fContainer.RegisterType<TPrimitiveComponent>.Implements<IPrimitive>
    .InjectField('fNameService', TValue.From<INameService>(TNameService.Create));
  fContainer.Build;
  CheckTrue(fContainer.Resolve<IPrimitive>.NameService <> nil)
end;

{$ENDREGION}


{$REGION 'TTestResolverOverride'}

procedure TTestResolverOverride.SetUp;
begin
  inherited;
  fDummy := TObject.Create;
end;

procedure TTestResolverOverride.TearDown;
begin
  inherited;
  fDummy.Free;
end;

procedure TTestResolverOverride.TestResolve;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>(
    TOrderedParametersOverride.Create(['test'])).Name);

  CheckEquals('test', fContainer.Resolve<INameService>('dynamic',
    TOrderedParametersOverride.Create(['test'])).Name);

  CheckEquals('test', fContainer.Resolve<INameService>(
    TParameterOverride.Create('name', 'test')).Name);
end;

procedure TTestResolverOverride.TestResolveWithClass;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>(
    TOrderedParametersOverride.Create([fDummy])).Name);

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>(
    TParameterOverride.Create('obj', fDummy)).Name);
end;

procedure TTestResolverOverride.TestResolveWithDependency;
var
  service: INameService;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic')
    .InjectField('fAgeService');
  fContainer.RegisterType<TNameAgeComponent>.Implements<IAgeService>;
  fContainer.Build;

  service := fContainer.Resolve<INameService>(
    TOrderedParametersOverride.Create([fDummy]));
  CheckEquals(fdummy.ClassName, service.Name);
  CheckNotNull((service as TDynamicNameService).AgeService);

  service := fContainer.Resolve<INameService>(
    TParameterOverride.Create('obj', fDummy));
  CheckEquals(fdummy.ClassName, service.Name);
  CheckNotNull((service as TDynamicNameService).AgeService);
end;

procedure TTestResolverOverride.TestResolveWithMultipleParams;
begin
  fContainer.RegisterType<TDynamicNameService>.Implements<INameService>('dynamic');
  fContainer.Build;

  CheckEquals('test' + fdummy.ClassName, fContainer.Resolve<INameService>(
    TOrderedParametersOverride.Create(['test', fDummy])).Name);

  CheckEquals(fdummy.ClassName, fContainer.Resolve<INameService>(
    TParameterOverride.Create('obj', fDummy)).Name);
end;

{$ENDREGION}


{$REGION 'TTestRegisterFactory'}

procedure TTestRegisterInterfaceTypes.TestOneService;
begin
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
    end);
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
end;

procedure TTestRegisterInterfaceTypes.TestOneServiceAsSingleton;
var
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
      Inc(count);
    end).AsSingleton;
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.GetName());
  CheckEquals('test', fContainer.Resolve<INameService>.GetName());
  CheckEquals(1, count);
end;

procedure TTestRegisterInterfaceTypes.TestOneServiceAsSingletonPerThread;
var
  count: Integer;
begin
  count := 0;
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
      Inc(count);
    end).AsSingletonPerThread;
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.GetName());
  CheckEquals('test', fContainer.Resolve<INameService>.GetName());
  CheckEquals(1, count);
end;

procedure TTestRegisterInterfaceTypes.TestTwoServices;
begin
  fContainer.RegisterType<INameService>.DelegateTo(
    function: INameService
    begin
      Result := TDynamicNameService.Create('test');
    end).Implements<IAnotherNameService>('another');
  fContainer.Build;

  CheckEquals('test', fContainer.Resolve<INameService>.Name);
  CheckEquals('test', fContainer.Resolve<IAnotherNameService>.Name);
end;

{$ENDREGION}


{$REGION 'TTestResolveLazy'}

procedure TTestLazyDependencies.PerformChecks;
var
  nameService: INameService;
begin
  fCalled := False;
  nameService := fContainer.Resolve<INameService>('service');
  CheckFalse(fCalled);
  CheckEquals(TNameService.NameString, nameService.Name);
  CheckTrue(fCalled);
end;

procedure TTestLazyDependencies.SetUp;
begin
  inherited;

  fContainer.RegisterType<INameService>.Implements<INameService>('lazy').DelegateTo(
    function: INameService
    begin
      Result := TNameService.Create;
      fCalled := True;
    end);
end;

procedure TTestLazyDependencies.TestDependencyTypeIsFunc;
begin
  fContainer.RegisterType<TNameServiceLazyWithFunc>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;

{$IFNDEF IOSSIMULATOR}
procedure TTestLazyDependencies.TestDependencyTypeIsInterface;
begin
  fContainer.RegisterType<TNameServiceLazyWithInterface>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;

procedure TTestLazyDependencies.TestDependencyTypeIsRecord;
begin
  fContainer.RegisterType<TNameServiceLazyWithRecord>.Implements<INameService>('service');
  fContainer.Build;

  PerformChecks;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TTestResolveLazyRecursive'}

procedure TTestLazyDependenciesDetectRecursion.PerformChecks;
begin
  fContainer.Context.ComponentRegistry.FindOne('service').InjectField('fNameService', 'service');

  ExpectedException:=ECircularDependencyException;
  inherited;
end;

{$ENDREGION}


{$REGION 'TTestDecoratorExtension'}

procedure TTestDecoratorExtension.TestResolveReturnsDecorator;
var
  service: IAgeService;
begin
  fContainer.AddExtension<TDecoratorContainerExtension>;
  fContainer.RegisterType<TAgeServiceDecorator>;
  fContainer.RegisterType<TNameAgeComponent>;
  fContainer.Build;

  service := fContainer.Resolve<IAgeService>;
  CheckTrue(service is TAgeServiceDecorator);
end;

procedure TTestDecoratorExtension.TestResolveWithResolverOverride;
var
  service: IAgeService;
begin
  fContainer.AddExtension<TDecoratorContainerExtension>;
  fContainer.RegisterType<TAgeServiceDecorator>;
  fContainer.RegisterType<TNameAgeComponent>;
  fContainer.Build;

  service := fContainer.Resolve<IAgeService>(
    TParameterOverride.Create('age', 21));
  CheckTrue(service is TAgeServiceDecorator);
  CheckEquals(21, service.Age);
end;

{$ENDREGION}


{$REGION 'TTestManyDependencies'}

procedure TTestManyDependencies.SetUp;
begin
  inherited;
  fContainer.RegisterType<ICollectionItem, TCollectionItemA>('a');
  fContainer.RegisterType<ICollectionItem, TCollectionItemB>('b');
  fContainer.RegisterType<ICollectionItem, TCollectionItemC>('c');
end;

procedure TTestManyDependencies.TestInjectArray;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceA>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestInjectArrayOfLazy;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceC>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestInjectEnumerable;
var
  service: ICollectionService;
begin
  fContainer.RegisterType<ICollectionService, TCollectionServiceB>;
  fContainer.RegisterType<IInterface, TCollectionServiceB>;
  fContainer.RegisterType<IEnumerable<ICollectionItem>, TArray<ICollectionItem>>;

  fContainer.Build;
  service := fContainer.Resolve<ICollectionService>;
  CheckEquals(3, Length(service.CollectionItems));
  CheckIs(service.CollectionItems[0], TCollectionItemA);
  CheckIs(service.CollectionItems[1], TCollectionItemB);
  CheckIs(service.CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestNoRecursion;
var
  service: ICollectionItem;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  service := fContainer.Resolve<ICollectionItem>;
  CheckIs(service, TCollectionItemD);
  CheckEquals(3, Length((service as TCollectionItemD).CollectionItems));
  CheckIs((service as TCollectionItemD).CollectionItems[0], TCollectionItemA);
  CheckIs((service as TCollectionItemD).CollectionItems[1], TCollectionItemB);
  CheckIs((service as TCollectionItemD).CollectionItems[2], TCollectionItemC);
end;

procedure TTestManyDependencies.TestResolveArrayOfLazy;
var
  services: TArray<Lazy<ICollectionItem>>;
  service: Lazy<ICollectionItem>;
begin
  fContainer.RegisterType<ICollectionItem, TCollectionItemD>;
  fContainer.Build;
  services := fContainer.ResolveAll<Lazy<ICollectionItem>>;
  CheckEquals(3, Length(services));
  CheckIs(services[0].Value, TCollectionItemA);
  CheckIs(services[1].Value, TCollectionItemB);
  CheckIs(services[2].Value, TCollectionItemC);
  service := fContainer.Resolve<Lazy<ICollectionItem>>;
  CheckIs(service.Value, TCollectionItemD);
end;

{$ENDREGION}


end.
