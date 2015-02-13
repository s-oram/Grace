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

unit Spring.Logging.Extensions;

interface

uses
  Rtti,
  SysUtils,
  TypInfo;

type
  ISerializerController = interface;

  /// <summary>
  ///   Serializer that can convert some types to string
  /// </summary>
  ITypeSerializer = interface
    ['{CF783059-AD29-468C-8BAF-F2FE0EAE6FE7}']
    function HandlesType(typeInfo: PTypeInfo): Boolean;
    function Serialize(const controller: ISerializerController;
      const value: TValue; nestingLevel: Integer = 0): string;
  end;

  /// <summary>
  ///   Controller that can be used by nested serializers to convert more
  ///   complex data
  /// </summary>
  ISerializerController = interface
    ['{6390E2C6-C415-4C7A-8FBF-975B331B90B2}']
    procedure AddSerializer(const serializer: ITypeSerializer);
    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
  end;

  IStackTraceCollector = interface
    ['{59259AF9-2E14-4AC4-B14E-C4CF0AFD0A44}']
    function Collect: TArray<Pointer>;
  end;

  IStackTraceFormatter = interface
    ['{515E564E-5EB4-4B6D-B74E-4080AB0E5D8C}']
    function Format(const stack: TArray<Pointer>): TArray<string>;
  end;

implementation

end.
