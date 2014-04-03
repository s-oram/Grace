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

{$IFNDEF SPRING_INC}
{$DEFINE SPRING_INC}

// Import JEDI Development environment directives
// Will assert correct CPU type and defaults to CPU386 if CPU type is unknown
// (Delphi older than XE3).
{$I jedi.inc}

{$IFDEF DELPHIXE4_UP}
  {$LEGACYIFEND ON}
{$ENDIF}

// JEDI does not handle CPUARM
{$IFDEF CPUARM}
  {$UNDEF CPU386}
  {$UNDEF CPU32}
  {$UNDEF CPUASM}
  {$DEFINE PUREPASCAL}
{$ENDIF}

// JEDI will assert that at least CPU386 is defined
{$IFDEF CPU386}
  // Keep this for older delphi compatibility (older than XE3)
  {$IFNDEF CPUX86}
    {$DEFINE CPUX86}
  {$ENDIF}
{$ENDIF}

{$IFDEF ASSEMBLER}
  {$DEFINE SUPPORTS_GENERIC_EVENTS}
{$ELSE}
  // Current System.Rtti does not support creating implementations that we need
  // so until that happens Events cannot be supported on non-Intel architectures
{$ENDIF}

{$IFDEF DELPHIXE_UP}
  // This will enable using only one generic instantiation each for lists that
  // contain objects or interfaces
  {.$DEFINE SUPPORTS_GENERIC_FOLDING}
{$ENDIF}

// If we are running on Android or iOS, we are sure we are running using FMX.
// If FMX is to be used on x86/x64 FMX conditional should be defined
// in the project settings by hand
{$IFDEF ANDROID}
  {$DEFINE FMX}
{$ENDIF}
{$IFDEF IOS}
  {$DEFINE FMX}
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
  {$MESSAGE ERROR 'This framework requires Delphi 2010 or higher.'}
{$ENDIF}

{$IFNDEF NEXTGEN}
  {$WARN UNIT_PLATFORM OFF}     // Turn off UNIT_PLATFORM warning
  {$WARN SYMBOL_PLATFORM OFF}   // Turn off SYMBOL_PLATFORM warning
{$ELSE}
  {$WARN SYMBOL_DEPRECATED OFF} // Turn off SYMBOL_DEPRECATED warning
{$ENDIF}

{$ENDIF ~SPRING_INC}
