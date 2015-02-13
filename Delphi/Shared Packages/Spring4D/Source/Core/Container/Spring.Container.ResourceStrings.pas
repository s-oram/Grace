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

unit Spring.Container.ResourceStrings;

interface

resourcestring
  SContainerRequiresBuild = 'Container has changed registration information - Build required';
  SMissingGuid = 'Type is missing a guid: %s';
  SMissingInterface = 'Type does not support IInterface: %s';
  SIncompatibleTypes = 'Component type "%s" incompatible with service type "%s".';
  SUnsupportedFactoryType = 'Type does not contain RTTI: %s';
  SUnsupportedFactoryMethod = 'Type contains incompatible method: %s, %s';
  SDuplicateServiceName = 'Duplicate service name found: %s';
  SServiceNotFound = 'Service not found: %s';
  SCircularDependencyDetected = 'Circular dependency detected on type: %s';
  SCannotResolveType = 'Cannot resolve type: %s';
  SResolutionStackUnbalanced = 'Resolution stack is unbalanced';
  SNoDefaultFound = 'No default found for type: %s';
  SUnsatisfiedConstructorParameters = 'Unsatisfied parameters for constructor of type: %s';
  SAmbiguousConstructor = 'Ambiguous constructor on class: %s';
  SUnsatisfiedMethodParameterTypes = 'Unsatisfied parameter types for the method: %s';
  SUnsatisfiedResolutionArgumentCount = 'Unsatisfied resolution argument count.';
  SUnresovableInjection = 'Unresovable injection on type: %s';
  SUnsatisfiedConstructor = 'Unsatisfied constructor on type: %s';
  SMethodNotFound = 'Method not found: %s.%s';
  SPropertyNotFound = 'Property not found: %s.%s';
  SFieldNotFound = 'Field not found: %s.%s';
  SInjectionTargetNeeded = 'Injection target needed.';
  SUnsatisfiedTarget = 'Unsatisfied target: %s';
  STypeNotFound = 'Type not found: %s';
  SUnexpectedLifetimeType = 'Unexpected lifetimeType.';
  SActivatorDelegateExpected = 'Activator delegate was expected.';
  SPoolingNotSupported = 'Pooling not supported for type: %s';

implementation

end.
