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

unit Spring.Logging.Loggers;

interface

uses
  SysUtils,
  Rtti,
  Spring,
  Spring.Logging;

type
  {$REGION 'TLoggerBase'}
  {$M+}
  TLoggerBase = class abstract(TInterfacedObject, ILogger, ILogAppender,
    ILoggerProperties)
  private
    type
      TLogTracking = class(TInterfacedObject, IInterface)
      private
        fLogger: ILogger;
        fLevel: TLogLevel;
        fClassType: TClass;
        fMethodName: string;
      public
        constructor Create(const logger: ILogger; level: TLogLevel;
          const classType: TClass; const methodName: string);
        destructor Destroy; override;
      end;
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
  protected
    procedure DoLog(const entry: TLogEntry); virtual; abstract;

    procedure ILogAppender.Send = Log;
  public
    constructor Create;

    function IsEnabled(level: TLogLevel;
      entryTypes: TLogEntryTypes = [TLogEntryType.Text]): Boolean; inline;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;

    procedure Log(const entry: TLogEntry); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string;
      const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg : string); overload;
    procedure Log(level: TLogLevel; const msg : string;
      const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const); overload;
    procedure Log(level: TLogLevel; const fmt : string;
      const args : array of const; const e: Exception); overload;

    procedure Fatal(const msg : string); overload;
    procedure Fatal(const msg : string; const e: Exception); overload;
    procedure Fatal(const fmt : string; const args : array of const); overload;
    procedure Fatal(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Error(const msg : string); overload;
    procedure Error(const msg : string; const e: Exception); overload;
    procedure Error(const fmt : string; const args : array of const); overload;
    procedure Error(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Warn(const msg : string); overload;
    procedure Warn(const msg : string; const e: Exception); overload;
    procedure Warn(const fmt : string; const args : array of const); overload;
    procedure Warn(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Info(const msg : string); overload;
    procedure Info(const msg : string; const e: Exception); overload;
    procedure Info(const fmt : string; const args : array of const); overload;
    procedure Info(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Text(const msg : string); overload;
    procedure Text(const msg : string; const e: Exception); overload;
    procedure Text(const fmt : string; const args : array of const); overload;
    procedure Text(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Debug(const msg : string); overload;
    procedure Debug(const msg : string; const e: Exception); overload;
    procedure Debug(const fmt : string; const args : array of const); overload;
    procedure Debug(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Trace(const msg : string); overload;
    procedure Trace(const msg : string; const e: Exception); overload;
    procedure Trace(const fmt : string; const args : array of const); overload;
    procedure Trace(const fmt : string; const args : array of const;
      const e: Exception); overload;

    procedure Enter(const methodName: string); overload;
    procedure Enter(const classType: TClass;
      const methodName: string); overload;
    procedure Enter(const instance: TObject;
      const methodName: string); overload;
    procedure Enter(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    procedure Leave(const methodName: string); overload;
    procedure Leave(const classType: TClass;
      const methodName: string); overload;
    procedure Leave(const instance: TObject;
      const methodName: string); overload;
    procedure Leave(level: TLogLevel; const classType: TClass;
      const methodName: string); overload;

    function Track(const classType: TClass;
      const methodName: string): IInterface; overload;
    function Track(const instance: TObject;
      const methodName: string): IInterface; overload;
    function Track(level: TLogLevel; const classType: TClass;
      const methodName: string): IInterface; overload;
  published
    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TLogLevels read GetLevels write SetLevels;
    property EntryTypes: TLogEntryTypes read GetEntryTypes write SetEntryTypes;
  end;
  {$M-}
  {$ENDREGION}


  {$REGION 'TLogger'}
  TLogger = class(TLoggerBase)
  private
    fController: ILoggerController;
  protected
    procedure DoLog(const entry: TLogEntry); override;
  public
    constructor Create(const controller: ILoggerController);
  end;
  {$ENDREGION}


implementation

uses
  Spring.Collections;


{$REGION 'TLoggerBase'}

constructor TLoggerBase.Create;
begin
  inherited;
  fDefaultLevel := TLogLevel.Info;
  fEnabled := True;
  fLevels := LOG_BASIC_LEVELS;
  fEntryTypes := LOG_BASIC_ENTRY_TYPES;
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args)));
end;

procedure TLoggerBase.Debug(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, Format(fmt, args), e));
end;

procedure TLoggerBase.Debug(const msg: string);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg));
end;

procedure TLoggerBase.Debug(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Debug) then
    DoLog(TLogEntry.Create(TLogLevel.Debug, msg, e));
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args)));
end;

procedure TLoggerBase.Enter(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level, [TLogEntryType.Entering]) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Entering, methodName, classType));
end;

procedure TLoggerBase.Enter(const methodName: string);
begin
  Enter(fDefaultLevel, nil, methodName);
end;

procedure TLoggerBase.Enter(const classType: TClass;
  const methodName: string);
begin
  Enter(fDefaultLevel, classType, methodName);
end;

procedure TLoggerBase.Enter(const instance: TObject;
  const methodName: string);
begin
  if Assigned(instance) then
    Enter(instance.ClassType, methodName)
  else
    Enter(TClass(nil), methodName)
end;

procedure TLoggerBase.Error(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, Format(fmt, args), e));
end;

procedure TLoggerBase.Error(const msg: string);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg));
end;

procedure TLoggerBase.Error(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Error) then
    DoLog(TLogEntry.Create(TLogLevel.Error, msg, e));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args)));
end;

procedure TLoggerBase.Fatal(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, Format(fmt, args), e));
end;

procedure TLoggerBase.Fatal(const msg: string);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg));
end;

procedure TLoggerBase.Fatal(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Fatal) then
    DoLog(TLogEntry.Create(TLogLevel.Fatal, msg, e));
end;

function TLoggerBase.GetDefaultLevel: TLogLevel;
begin
  Result := fDefaultLevel;
end;

function TLoggerBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TLoggerBase.GetEntryTypes: TLogEntryTypes;
begin
  Result := fEntryTypes;
end;

function TLoggerBase.GetLevels: TLogLevels;
begin
  Result := fLevels;
end;

procedure TLoggerBase.Info(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg, e));
end;

procedure TLoggerBase.Info(const msg: string);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, msg));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args), e));
end;

procedure TLoggerBase.Info(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Info) then
    DoLog(TLogEntry.Create(TLogLevel.Info, Format(fmt, args)));
end;

function TLoggerBase.IsDebugEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Debug, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsEnabled(level: TLogLevel; entryTypes: TLogEntryTypes): Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckTrue(entryTypes <> [], 'entryTypes');
{$ENDIF}
  Result := fEnabled and (level in fLevels)
    and (entryTypes * fEntryTypes <> []);
end;

function TLoggerBase.IsErrorEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Error, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsFatalEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Fatal, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsInfoEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Info, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsTextEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Text, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsTraceEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Trace, LOG_ALL_ENTRY_TYPES);
end;

function TLoggerBase.IsWarnEnabled: Boolean;
begin
  Result := IsEnabled(TLogLevel.Warn, LOG_ALL_ENTRY_TYPES);
end;

procedure TLoggerBase.Leave(level: TLogLevel; const classType: TClass;
  const methodName: string);
begin
  if IsEnabled(level, [TLogEntryType.Leaving]) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Leaving, methodName, classType));
end;

procedure TLoggerBase.Leave(const methodName: string);
begin
  Leave(fDefaultLevel, nil, methodName);
end;

procedure TLoggerBase.Leave(const instance: TObject;
  const methodName: string);
begin
  if Assigned(instance) then
    Leave(instance.ClassType, methodName)
  else
    Leave(TClass(nil), methodName)
end;

procedure TLoggerBase.Leave(const classType: TClass;
  const methodName: string);
begin
  Leave(fDefaultLevel, classType, methodName);
end;

procedure TLoggerBase.Log(const entry: TLogEntry);
begin
  if IsEnabled(entry.Level, [entry.EntryType]) then
    DoLog(entry);
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, msg, e));
end;

procedure TLoggerBase.Log(level: TLogLevel; const msg: string);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, msg));
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, Format(fmt, args)));
end;

procedure TLoggerBase.Log(const msg: string);
begin
  Log(fDefaultLevel, msg);
end;

procedure TLoggerBase.Log(const msg: string; const e: Exception);
begin
  Log(fDefaultLevel, msg, e);
end;

procedure TLoggerBase.Log(const fmt: string; const args: array of const);
begin
  Log(fDefaultLevel, fmt, args);
end;

procedure TLoggerBase.Log(const fmt: string; const args: array of const;
  const e: Exception);
begin
  Log(fDefaultLevel, fmt, args, e);
end;

procedure TLoggerBase.Log(level: TLogLevel; const fmt: string;
  const args: array of const; const e: Exception);
begin
  if IsEnabled(level) then
    DoLog(TLogEntry.Create(level, Format(fmt, args), e));
end;

procedure TLoggerBase.LogValue(level: TLogLevel; const name: string;
  const value: TValue);
begin
  if IsEnabled(level, [TLogEntryType.Value]) then
    DoLog(TLogEntry.Create(level, TLogEntryType.Value, name, nil, value));
end;

procedure TLoggerBase.LogValue(const name: string; const value: TValue);
begin
  LogValue(fDefaultLevel, name, value);
end;

procedure TLoggerBase.SetDefaultLevel(value: TLogLevel);
begin
  fDefaultLevel := value;
end;

procedure TLoggerBase.SetEnabled(value: Boolean);
begin
  fEnabled := value;
end;

procedure TLoggerBase.SetEntryTypes(value: TLogEntryTypes);
begin
  fEntryTypes := value;
end;

procedure TLoggerBase.SetLevels(value: TLogLevels);
begin
  fLevels := value;
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args)));
end;

procedure TLoggerBase.Text(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, Format(fmt, args), e));
end;

procedure TLoggerBase.Text(const msg: string);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg));
end;

procedure TLoggerBase.Text(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Text) then
    DoLog(TLogEntry.Create(TLogLevel.Text, msg, e));
end;

function TLoggerBase.Track(level: TLogLevel; const classType: TClass;
  const methodName: string): IInterface;
begin
  if IsEnabled(level, [TLogEntryType.Entering, TLogEntryType.Leaving]) then
    Result := TLogTracking.Create(Self, level, classType, methodName)
  else
    Result := nil;
end;

function TLoggerBase.Track(const instance: TObject;
  const methodName: string): IInterface;
begin
  if Assigned(instance) then
    Result := Track(instance.ClassType, methodName)
  else
    Result := Track(TClass(nil), methodName)
end;

function TLoggerBase.Track(const classType: TClass;
  const methodName: string): IInterface;
begin
  Result := Track(fDefaultLevel, classType, methodName);
end;

procedure TLoggerBase.Trace(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, Format(fmt, args)));
end;

procedure TLoggerBase.Trace(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, Format(fmt, args), e));
end;

procedure TLoggerBase.Trace(const msg: string);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, msg));
end;

procedure TLoggerBase.Trace(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Trace) then
    DoLog(TLogEntry.Create(TLogLevel.Trace, msg, e));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, Format(fmt, args)));
end;

procedure TLoggerBase.Warn(const fmt: string; const args: array of const;
  const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, Format(fmt, args), e));
end;

procedure TLoggerBase.Warn(const msg: string);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, msg));
end;

procedure TLoggerBase.Warn(const msg: string; const e: Exception);
begin
  if IsEnabled(TLogLevel.Warn) then
    DoLog(TLogEntry.Create(TLogLevel.Warn, msg, e));
end;

{$ENDREGION}


{$REGION 'TLogger'}

constructor TLogger.Create(const controller: ILoggerController);
begin
  Guard.CheckNotNull(controller, 'controller');
  inherited Create;

  fController := controller;
end;

procedure TLogger.DoLog(const entry: TLogEntry);
begin
  fController.Send(entry);
end;

{$ENDREGION}


{$REGION 'TLoggerBase.TLogTracking'}

constructor TLoggerBase.TLogTracking.Create(const logger: ILogger;
  level: TLogLevel; const classType: TClass; const methodName: string);
begin
  inherited Create;
  fLogger := logger;
  fLevel := level;
  fClassType := classType;
  fMethodName := methodName;
  fLogger.Enter(fLevel, fClassType, fMethodName);
end;

destructor TLoggerBase.TLogTracking.Destroy;
begin
  fLogger.Leave(fLevel, fClassType, fMethodName);
  inherited;
end;

{$ENDREGION}


end.
