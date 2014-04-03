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

unit Spring.TestRegistration; // should be platform neutral

{$I Spring.Tests.inc}

interface

procedure RegisterTestCases();

implementation

uses
  TestFramework,
  Spring.TestUtils,
  Spring.Tests.Base,
  Spring.Tests.Collections,
  Spring.Tests.Collections.Extensions,
  Spring.Tests.SysUtils,
  Spring.Tests.DesignPatterns,
  Spring.Tests.Helpers,
  Spring.Tests.Reflection.ValueConverters,
  Spring.Tests.Container,
  Spring.Tests.Container.LifetimeManager,
  Spring.Tests.Pool,
  Spring.Tests.Utils,
  Spring.Tests.Cryptography;

procedure RegisterTestCases();
begin
  RegisterTests('Spring.Base', [
    TRepeatedTest.Create(TTestNullableInteger.Suite, 3),
    TTestNullableBoolean.Suite,
    TTestGuard.Suite,
    TTestLazy.Suite,
{$IFDEF SUPPORTS_GENERIC_EVENTS}
    TTestMulticastEvent.Suite,
    TTestMulticastEventStackSize.Suite,
{$ENDIF}
    TTestSpringEventsMethods.Suite,
    TTestEmptyHashSet.Suite,
    TTestNormalHashSet.Suite,
    TTestIntegerList.Suite,
    TTestStringIntegerDictionary.Suite,
    TTestEmptyStringIntegerDictionary.Suite,
    TTestEmptyStackOfStrings.Suite,
    TTestStackOfInteger.Suite,
    TTestStackOfIntegerChangedEvent.Suite,
    TTestEmptyQueueOfInteger.Suite,
    TTestQueueOfInteger.Suite,
    TTestQueueOfIntegerChangedEvent.Suite,
    TTestListOfIntegerAsIEnumerable.Suite,
    TTestLinkedList.Suite,
    TTestObjectList.Suite,
    TTestCollectionList.Suite
  ]);

  RegisterTests('Spring.Base.Collections', [
    TTestWhere.Suite,
    TTestSelect.Suite,
    TTestRange.Suite,
    TTestConcat.Suite,
    TTestSelectMany.Suite,
    TTestAny.Suite,
    TTestAll.Suite,
    TTestFirst.Suite,
    TTestSingle.Suite,
    TTestLast.Suite,
    TTestFirstOrDefault.Suite,
    TTestSingleOrDefault.Suite,
    TTestLastOrDefault.Suite,
    TTestDefaultIfEmpty.Suite,
    TTestDistinct.Suite,
    TTestUnion.Suite,
    TTestIntersect.Suite,
    TTestExcept.Suite,
    TTestToLookup.Suite,
    TTestJoin.Suite,
    TTestGroupBy.Suite,
    TTestGroupJoin.Suite,
    TTestTake.Suite,
    TTestSkip.Suite,
    TTestTakeWhile.Suite,
    TTestSkipWhile.Suite,
    TTestOrderBy.Suite,
    TTestOrderByDescending.Suite,
    TTestReverse.Suite,
    TTestMaxBy.Suite
  ]);

  RegisterTests('Spring.Base.SysUtils', [
    TTestSplitString.Suite,
    TTestTryConvertStrToDateTime.Suite,
    TTestSplitNullTerminatedStrings.Suite,
    TTestEnum.Suite
  ]);

  RegisterTests('Spring.Base.DesignPatterns', [
    TTestSingleton.Suite
  ]);

  RegisterTests('Spring.Base.Helpers', [
    TTestGuidHelper.Suite
  ]);

  RegisterTests('Spring.Base.Reflection.ValueConverters', [
    TTestFromString.Suite,
{$IFNDEF NEXTGEN}
    TTestFromWideString.Suite,
{$ENDIF}
    TTestFromInteger.Suite,
    TTestFromCardinal.Suite,
    TTestFromSmallInt.Suite,
    TTestFromShortInt.Suite,
    TTestFromBoolean.Suite,
    TTestFromEnum.Suite,
    TTestFromFloat.Suite,
    TTestFromColor.Suite,
    TTestFromCurrency.Suite,
    TTestFromDateTime.Suite,
    TTestFromObject.Suite,
    TTestFromNullable.Suite,
    TTestFromInterface.Suite,
    TTestCustomTypes.Suite
  ]);

//  RegisterTests('Spring.Base.Reflection.ValueExpression', [
//    TTestValueExpression.Suite
//  ]);

  RegisterTests('Spring.Core.Container', [
    TTestEmptyContainer.Suite,
    TTestSimpleContainer.Suite,
    TTestDifferentServiceImplementations.Suite,
    TTestImplementsDifferentServices.Suite,
    TTestActivatorDelegate.Suite,
    TTestTypedInjectionByCoding.Suite,
    TTestTypedInjectionsByAttribute.Suite,
    TTestNamedInjectionsByCoding.Suite,
    TTestNamedInjectionsByAttribute.Suite,
    TTestDirectCircularDependency.Suite,
    TTestCrossedCircularDependency.Suite,
    TTestImplementsAttribute.Suite,
    TTestRegisterInterfaces.Suite,
    TTestSingletonLifetimeManager.Suite,
    TTestTransientLifetimeManager.Suite,
    TTestRefCounting.Suite,
    TTestDefaultResolve.Suite,
    TTestInjectionByValue.Suite,
    TTestObjectPool.Suite,
    TTestResolverOverride.Suite,
    TTestRegisterInterfaceTypes.Suite,
    TTestLazyDependencies.Suite,
    TTestLazyDependenciesDetectRecursion.Suite,
    TTestDecoratorExtension.Suite,
    TTestManyDependencies.Suite
  ]);

  RegisterTests('Spring.Extensions.Utils', [
    TTestVersion.Suite,
    TTestEnvironment.Suite
  ]);

  RegisterTests('Spring.Extensions.Cryptography', [
//    TTestBuffer.Suite,
//    TTestEmptyBuffer.Suite,
//    TTestFiveByteBuffer.Suite,
    TTestCRC16.Suite,
    TTestCRC32.Suite,
    TTestMD5.Suite,
    TTestSHA1.Suite,
    TTestSHA256.Suite,
    TTestSHA384.Suite,
    TTestSHA512.Suite,
    TTestPaddingModeIsNone.Suite,
    TTestPaddingModeIsPKCS7.Suite,
    TTestPaddingModeIsZeros.Suite,
    TTestPaddingModeIsANSIX923.Suite,
    TTestPaddingModeIsISO10126.Suite,
    TTestDES.Suite,
    TTestTripleDES.Suite
  ]);

// Stefan Glienke - 2011/11/20:
// removed configuration and logging tests because they break other tests in Delphi 2010
// due to some bug in Rtti.TRttiPackage.MakeTypeLookupTable
// see https://forums.embarcadero.com/thread.jspa?threadID=54471
//
//  RegisterTests('Spring.Core.Configuration', [
//    TTestConfiguration.Suite
//  ]);
//
//  RegisterTests('Spring.Core.Logging', [
//     TTestLoggingConfig.Suite
//  ]);
end;

end.
