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

unit Spring.Logging.Appenders.Base;

{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  SysUtils,
  Spring,
  Spring.Logging;

type
  {$REGION 'TLogAppenderBase'}
  TLogAppenderBase = class abstract(TInterfacedObject, ILogAppender,
    ILoggerProperties)
  private
    fDefaultLevel: TLogLevel;
    fEnabled: Boolean;
    fLevels: TLogLevels;
    fEntryTypes: TLogEntryTypes;

    function GetDefaultLevel: TLogLevel;
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;
    function GetEntryTypes: TLogEntryTypes;

    procedure SetDefaultLevel(value: TLogLevel);
    procedure SetEnabled(value: Boolean);
    procedure SetLevels(value: TLogLevels);
    procedure SetEntryTypes(value: TLogEntryTypes);
  {$REGION 'Helper constants and functions'}
  protected const
    //May or may not be used by descendants, its here just for convenience
    LEVEL: array[TLogLevel] of string = (
      '[UNKNOWN]',
      '[VERBOSE]',
      '[DEBUG]',
      '[TEXT]',
      '[INFO]',
      '[WARN]',
      '[ERROR]',
      '[FATAL]'
    );
    LEVEL_FIXED: array[TLogLevel] of string = (
      '[UNK  ]',
      '[VERB ]',
      '[DEBUG]',
      '[TEXT ]',
      '[INFO ]',
      '[WARN ]',
      '[ERROR]',
      '[FATAL]'
    );
  public
    class function FormatText(const entry: TLogEntry): string; static; inline;
    class function FormatMsg(const entry: TLogEntry): string; static; inline;
    class function FormatException(const e: Exception): string; static; //noinline
    class function FormatMethodName(classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatEntering(classType: TClass;
      const methodName: string): string; static; inline;
    class function FormatLeaving(classType: TClass;
      const methodName: string): string; static; inline;
  {$ENDREGION}
  protected
    function IsEnabled(level: TLogLevel;
      entryType: TLogEntryType): Boolean; inline;
    procedure DoSend(const entry: TLogEntry); virtual; abstract;
  public
    constructor Create;

    procedure Send(const entry: TLogEntry);

    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TLogLevels read GetLevels write SetLevels;
    property EntryTypes: TLogEntryTypes read GetEntryTypes write SetEntryTypes;
  end;
  {$ENDREGION}


implementation

uses
  Spring.Logging.ResourceStrings;


{$REGION 'TLogAppenderBase'}

constructor TLogAppenderBase.Create;
begin
  inherited;
  fEnabled := True;
  fLevels := LOG_BASIC_LEVELS;
  fEntryTypes := LOG_BASIC_ENTRY_TYPES;
end;

class function TLogAppenderBase.FormatEntering(classType: TClass;
  const methodName: string): string;
begin
  Result := SLogEntering + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatException(const e: Exception): string;
var
  len: Integer;
begin
  SetLength(Result, 1024);
  len := ExceptionErrorMessage(e, ExceptAddr, @Result[1], Length(Result));
  SetLength(Result, len);
end;

class function TLogAppenderBase.FormatLeaving(classType: TClass;
  const methodName: string): string;
begin
  Result := SLogLeaving + FormatMethodName(classType, methodName);
end;

class function TLogAppenderBase.FormatMethodName(classType: TClass;
  const methodName: string): string;
begin
  if Assigned(classType) then
    Result := GetQualifiedClassName(classType) + '.' + methodName
  else
    Result := methodName;
end;

class function TLogAppenderBase.FormatMsg(const entry: TLogEntry): string;
begin
  if entry.Exception = nil then
    Result := FormatText(entry)
  else
    if entry.Msg <> '' then
      Result := FormatText(entry) + ': ' + FormatException(entry.Exception)
    else
      Result := FormatException(entry.Exception);
end;

class function TLogAppenderBase.FormatText(const entry: TLogEntry): string;
begin
  case entry.EntryType of
    TLogEntryType.Text,
    TLogEntryType.SerializedData,
    TLogEntryType.CallStack:
      Result := entry.Msg;

    TLogEntryType.Entering:
      Result := FormatEntering(entry.ClassType, entry.Msg);

    TLogEntryType.Leaving:
      Result := FormatLeaving(entry.ClassType, entry.Msg);
  end;
end;

function TLogAppenderBase.GetDefaultLevel: TLogLevel;
begin
  Result := fDefaultLevel;
end;

function TLogAppenderBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TLogAppenderBase.GetEntryTypes: TLogEntryTypes;
begin
  Result := fEntryTypes;
end;

function TLogAppenderBase.GetLevels: TLogLevels;
begin
  Result := fLevels;
end;

function TLogAppenderBase.IsEnabled(level: TLogLevel;
  entryType: TLogEntryType): Boolean;
begin
  Result := fEnabled and (level in fLevels) and (entryType in fEntryTypes);
end;

procedure TLogAppenderBase.Send(const entry: TLogEntry);
begin
  if IsEnabled(entry.Level, entry.EntryType) then
    DoSend(entry);
end;

procedure TLogAppenderBase.SetDefaultLevel(value: TLogLevel);
begin
  fDefaultLevel := value;
end;

procedure TLogAppenderBase.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

procedure TLogAppenderBase.SetEntryTypes(value: TLogEntryTypes);
begin
  fEntryTypes := value;
end;

procedure TLogAppenderBase.SetLevels(value: TLogLevels);
begin
  fLevels := value;
end;

{$ENDREGION}


end.
