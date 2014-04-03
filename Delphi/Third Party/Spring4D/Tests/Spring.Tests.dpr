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

program Spring.Tests;

{$I Spring.Tests.inc}

// Spring.TestBootstrap be the first unit in the uses clause since it specifies
// some units that need to be set in given order for FMX
uses
  Spring.TestBootstrap in 'Source\Spring.TestBootstrap.pas',
  TestFramework,
  FinalBuilder.XMLTestRunner in 'Source\FinalBuilder.XMLTestRunner.pas',
  Spring.TestRegistration in 'Source\Spring.TestRegistration.pas',
  Spring.TestRunner in 'Source\Spring.TestRunner.pas',
  Spring.TestUtils in 'Source\Spring.TestUtils.pas',
  Spring.Tests.Base in 'Source\Base\Spring.Tests.Base.pas',
  Spring.Tests.Collections in 'Source\Base\Spring.Tests.Collections.pas',
  Spring.Tests.Collections.Extensions in 'Source\Base\Spring.Tests.Collections.Extensions.pas',
  Spring.Tests.DesignPatterns in 'Source\Base\Spring.Tests.DesignPatterns.pas',
  Spring.Tests.Helpers in 'Source\Base\Spring.Tests.Helpers.pas',
  Spring.Tests.Reflection.ValueConverters in 'Source\Base\Spring.Tests.Reflection.ValueConverters.pas',
  Spring.Tests.SysUtils in 'Source\Base\Spring.Tests.SysUtils.pas',
  Spring.Tests.Container.Components in 'Source\Core\Spring.Tests.Container.Components.pas',
  Spring.Tests.Container.Interfaces in 'Source\Core\Spring.Tests.Container.Interfaces.pas',
  Spring.Tests.Container.LifetimeManager in 'Source\Core\Spring.Tests.Container.LifetimeManager.pas',
  Spring.Tests.Container in 'Source\Core\Spring.Tests.Container.pas',
  Spring.Tests.Pool in 'Source\Core\Spring.Tests.Pool.pas',
  Spring.Tests.Cryptography in 'Source\Extensions\Spring.Tests.Cryptography.pas',
  Spring.Tests.Utils in 'Source\Extensions\Spring.Tests.Utils.pas',
  Spring.Container;

begin
  CleanupGlobalContainer;
  RegisterTestCases();
  ReportMemoryLeaksOnShutdown := True;
  RunRegisteredTests();
  TestFramework.ClearRegistry;
end.
