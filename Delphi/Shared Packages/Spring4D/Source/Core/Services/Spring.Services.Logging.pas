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

///	<summary>
///	  Defines an independent logging service.
///	</summary>
unit Spring.Services.Logging;

interface

uses
  SysUtils,
  Spring;

type
  ILogger = interface
    ['{803DC36C-03FE-4C5C-B8BE-9CB79076DCB2}']
    {$REGION 'Property Accessors'}
      function GetName: string;
      function GetIsDebugEnabled: Boolean;
      function GetIsInfoEnabled: Boolean;
      function GetIsWarnEnabled: Boolean;
      function GetIsErrorEnabled: Boolean;
      function GetIsFatalEnabled: Boolean;
    {$ENDREGION}

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; e: Exception); overload;
    procedure DebugFormat(const format: string; const args: array of const); overload;
    procedure DebugFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; e: Exception); overload;
    procedure InfoFormat(const format: string; const args: array of const); overload;
    procedure InfoFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; e: Exception); overload;
    procedure WarnFormat(const format: string; const args: array of const); overload;
    procedure WarnFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; e: Exception); overload;
    procedure ErrorFormat(const format: string; const args: array of const); overload;
    procedure ErrorFormat(const format: string; const args: array of const; e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; e: Exception); overload;
    procedure FatalFormat(const format: string; const args: array of const); overload;
    procedure FatalFormat(const format: string; const args: array of const; e: Exception); overload;

    property Name: string read GetName;

    property IsDebugEnabled: Boolean read GetIsDebugEnabled;
    property IsInfoEnabled: Boolean read GetIsInfoEnabled;
    property IsWarnEnabled: Boolean read GetIsWarnEnabled;
    property IsErrorEnabled: Boolean read GetIsErrorEnabled;
    property IsFatalEnabled: Boolean read GetIsFatalEnabled;
  end;

  ILoggerFactory = interface
    ['{EC45E97F-79A6-46C6-BFF3-CB8C1E9339FC}']
    function GetDefaultLogger: ILogger;
    function GetLogger(const name: string): ILogger; overload;
    function GetLogger(typeInfo: PTypeInfo): ILogger; overload;
  end;

function LoggerFactory: ILoggerFactory;

function GetLogger(classType: TClass): ILogger; overload;

function GetLogger(typeInfo: PTypeInfo): ILogger; overload;

function GetLogger(const name: string): ILogger; overload;

function DefaultLogger: ILogger;

implementation

uses
  TypInfo, // solves [dcc32/dcc64/dccosx Hint] H2443 Inline function 'Guard.CheckNotNull' has not been expanded because unit 'System.TypInfo' is not specified in USES list
  Spring.Services;

function LoggerFactory: ILoggerFactory;
begin
  Result := ServiceLocator.GetService<ILoggerFactory>;
end;

function DefaultLogger: ILogger;
begin
  Result := LoggerFactory.GetDefaultLogger;
end;

function GetLogger(classType: TClass): ILogger; overload;
var
  typeInfo: PTypeInfo;
begin
  Guard.CheckNotNull(classType, 'classType');

  typeInfo := classType.ClassInfo;
  Result := LoggerFactory.GetLogger(typeInfo);
end;

function GetLogger(typeInfo: PTypeInfo): ILogger;
begin
  Result := LoggerFactory.GetLogger(typeInfo);
end;

function GetLogger(const name: string): ILogger;
begin
  Result := LoggerFactory.GetLogger(name);
end;

end.
