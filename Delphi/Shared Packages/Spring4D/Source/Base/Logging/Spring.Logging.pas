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

unit Spring.Logging;

interface

uses
  Rtti,
{$IF Defined(DELPHIXE2_UP)}
  System.UITypes, // Has minimum dependencies
{$ELSEIF NOT Defined(SPRING_DISABLE_GRAPHICS)}
  Graphics, // Has (unfortunately) VCL dependencies
{$IFEND}
  SysUtils;

{$REGION 'Shadowed Delphi types'}
{$IF Defined(DELPHIXE2_UP)}
type
  TColor = System.UITypes.TColor;
  TColors = System.UITypes.TColors;

const
  clDefault = TColors.SysDefault;
{$ELSEIF NOT Defined(SPRING_DISABLE_GRAPHICS)}
type
  TColor = Graphics.TColor;

const
  clDefault = Graphics.clDefault;
{$ELSE}
type
  TColor = LongInt;

const
  clDefault = $20000000;
{$IFEND}
{$ENDREGION}

{$SCOPEDENUMS ON}
type
  {$REGION 'Log Level and formating definitions and constants'}
  TLogLevel = (
    /// <summary>
    ///   Excluded from all states and should never be used!
    /// </summary>
    Unknown,
    Trace,
    Debug,
    Text,
    Info,
    Warn,
    Error,
    Fatal
  );
  TLogLevels = set of TLogLevel;

const
  LOG_ALL_LEVELS = [Low(TLogLevel)..High(TLogLevel)] - [TLogLevel.Unknown];
  LOG_BASIC_LEVELS = [
    TLogLevel.Info,
    TLogLevel.Warn,
    TLogLevel.Error,
    TLogLevel.Fatal
  ];

type
  TLogEntryType = (
    /// <summary>
    ///   Is the most basic logging type all loggers should keep enabled
    /// </summary>
    Text,
    Value,
    /// <summary>
    ///   Should only be called if stack is sent to the appender. The appender
    ///   may treat it in a specific way. No one else should use this entry
    ///   type.
    ///   If this entry type is not set, callstack logging will be disabled
    ///   completely, this may have significant performance impact on some
    ///   platforms.
    /// </summary>
    CallStack,
    /// <summary>
    ///   Should only be called if serialized data (object, record, etc.) is
    ///   sent to the appender. The appender may treat it in a specific way.
    ///   No one else should use this entry type. If this level is not set,
    ///   data serialization logging will be disabled completely.
    /// </summary>
    SerializedData,
    Entering,
    Leaving
  );
  TLogEntryTypes = set of TLogEntryType;

const
  LOG_ALL_ENTRY_TYPES = [Low(TLogEntryType)..High(TLogEntryType)];
  LOG_BASIC_ENTRY_TYPES = [
    TLogEntryType.Text,
    TLogEntryType.Value,
    TLogEntryType.Entering,
    TLogEntryType.Leaving
  ];

type
  TLogStyle = (
    Bold,
    Italic,
    Underline,
    /// <summary>
    ///   Text should be printed in monospace font
    /// </summary>
    Monospace,
    /// <summary>
    ///   No escaping should be done on the text of the message by the appenders
    ///   in cases when they need to do so in order to not break the output
    ///   format.
    /// </summary>
    NoEscape
  );
  TLogStyles = set of TLogStyle;
{$ENDREGION}


  {$REGION 'TLogEntry'}
  TLogEntry = record
  private
    fLevel: TLogLevel;
    fEntryType: TLogEntryType;
    fMsg: string;
    fTimeStamp: TDateTime;
    fException: Exception;
    /// <summary>
    ///   Leave as default to instruct the appender/viewer to choose the default
    ///   color based on the level or entry contents or prescribe the color of
    ///   your choosing (not that some appenders may ignore the color).
    /// </summary>
    fColor: TColor;
    /// <summary>
    ///   Similar to Color but defines a text style.
    /// </summary>
    fStyle: TLogStyles;
    fClassType: TClass;
    fAddStack: Boolean;
    /// <summary>
    ///   An arbitrary data that the logger may output to the appenders
    /// </summary>
    fData: TValue;
    /// <summary>
    ///   Additional data anyone can use to extend behavior of their appenders
    /// </summary>
    fTag: NativeInt;
  public
    constructor Create(level: TLogLevel; const msg: string); overload;
    constructor Create(level: TLogLevel; entryType: TLogEntryType;
      const msg: string); overload;
    constructor Create(level: TLogLevel; const msg: string;
      const e: Exception); overload;
    constructor Create(level: TLogLevel; entryType: TLogEntryType;
      const msg: string; const classType: TClass); overload;
    constructor Create(level: TLogLevel; entryType: TLogEntryType;
      const msg: string; const classType: TClass; const data: TValue); overload;
    {constructor Create(level : TLogLevel; const msg : string;
      color : TColor = clDefault; fontStyle : TFontStyles = []; )}

    function SetException(const e: Exception): TLogEntry;
    function SetColor(color: TColor): TLogEntry;
    function SetStyle(style: TLogStyles): TLogEntry;
    function SetClassType(const classType: TClass): TLogEntry;
    function AddStack: TLogEntry;
    function SetData(const Data: TValue): TLogEntry;
    function SetTag(tag: NativeInt): TLogEntry;

    property Level: TLogLevel read fLevel;
    property EntryType: TLogEntryType read fEntryType;
    property Msg: string read fMsg;
    property TimeStamp: TDateTime read fTimeStamp;
    property Exception: Exception read fException;
    property Color: TColor read fColor;
    property Style: TLogStyles read fStyle;
    property ClassType: TClass read FClassType;
    property AddStackValue: Boolean read fAddStack;
    property Data: TValue read fData;
    property Tag: NativeInt read fTag;
  end;
  {$ENDREGION}


  {$REGION 'ILoggerBase'}
  ILoggerBase = interface
    function GetLevels: TLogLevels;
    function GetEntryTypes: TLogEntryTypes;
    function GetEnabled: Boolean;

    property Levels: TLogLevels read GetLevels;
    property EntryTypes: TLogEntryTypes read GetEntryTypes;
    property Enabled: Boolean read GetEnabled;
  end;
  {$ENDREGION}


  {$REGION 'ILogger'}
  ILogger = interface(ILoggerBase)
    ['{8655E906-C12D-4EB3-8291-30CEAB769B26}']
    procedure Log(const entry: TLogEntry); overload;

    procedure LogValue(const name: string; const value: TValue); overload;
    procedure LogValue(level: TLogLevel; const name: string;
      const value: TValue); overload;

    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const e: Exception); overload;
    procedure Log(const fmt: string; const args: array of const); overload;
    procedure Log(const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Log(level: TLogLevel; const msg: string); overload;
    procedure Log(level: TLogLevel; const msg: string;
      const e: Exception); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const); overload;
    procedure Log(level: TLogLevel; const fmt: string;
      const args: array of const; const e: Exception); overload;

    procedure Fatal(const msg: string); overload;
    procedure Fatal(const msg: string; const e: Exception); overload;
    procedure Fatal(const fmt: string; const args: array of const); overload;
    procedure Fatal(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Error(const msg: string); overload;
    procedure Error(const msg: string; const e: Exception); overload;
    procedure Error(const fmt: string; const args: array of const); overload;
    procedure Error(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Warn(const msg: string); overload;
    procedure Warn(const msg: string; const e: Exception); overload;
    procedure Warn(const fmt: string; const args: array of const); overload;
    procedure Warn(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Info(const msg: string); overload;
    procedure Info(const msg: string; const e: Exception); overload;
    procedure Info(const fmt: string; const args: array of const); overload;
    procedure Info(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Text(const msg: string); overload;
    procedure Text(const msg: string; const e: Exception); overload;
    procedure Text(const fmt: string; const args: array of const); overload;
    procedure Text(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Debug(const msg: string); overload;
    procedure Debug(const msg: string; const e: Exception); overload;
    procedure Debug(const fmt: string; const args: array of const); overload;
    procedure Debug(const fmt: string; const args: array of const;
      const e: Exception); overload;

    procedure Trace(const msg: string); overload;
    procedure Trace(const msg: string; const e: Exception); overload;
    procedure Trace(const fmt: string; const args: array of const); overload;
    procedure Trace(const fmt: string; const args: array of const;
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

    /// <summary>
    ///   Returns <c>true</c> if level is enabled and any of the <c>entryTypes</c>
    ///    is enabled or <c>false</c> otherwise.
    /// </summary>
    /// <param name="entryTypes">
    ///   Specifies entry types to check, <b>must not be empty</b>! Defaults to
    ///   <c>Text</c>.
    /// </param>
    function IsEnabled(level: TLogLevel;
      entryTypes: TLogEntryTypes = [TLogEntryType.Text]): Boolean;
    function IsFatalEnabled: Boolean;
    function IsErrorEnabled: Boolean;
    function IsWarnEnabled: Boolean;
    function IsInfoEnabled: Boolean;
    function IsTextEnabled: Boolean;
    function IsDebugEnabled: Boolean;
    function IsTraceEnabled: Boolean;
  end;
  {$ENDREGION}


  {$REGION 'ILogAppender'}
  ILogAppender = interface(ILoggerBase)
    ['{70DDEB60-3D01-48FB-92CF-A738A8C4BC85}']
    procedure Send(const entry: TLogEntry);
  end;
  {$ENDREGION}


  {$REGION 'ILoggerController'}
  ILoggerController = interface(ILogAppender)
    ['{6556A795-6F1B-4392-92FC-8E3391E3CB07}']
    procedure AddAppender(const appender: ILogAppender);
  end;
  {$ENDREGION}


  {$REGION 'ILoggerProperties'}
  /// <summary>
  ///   Interface that can be used to change logger/appender settings during
  ///   runtime. It is hidden within this interface so that it is not widely
  ///   exposed to the consumer.
  /// </summary>
  ILoggerProperties = interface
    ['{6514ADA8-A0A0-4234-A5EE-FBAFE34B58F2}']
    function GetDefaultLevel: TLogLevel;
    function GetEnabled: Boolean;
    function GetLevels: TLogLevels;
    function GetEntryTypes: TLogEntryTypes;

    procedure SetDefaultLevel(value: TLogLevel);
    procedure SetEnabled(value: Boolean);
    procedure SetLevels(value: TLogLevels);
    procedure SetEntryTypes(value: TLogEntryTypes);

    property DefaultLevel: TLogLevel read GetDefaultLevel write SetDefaultLevel;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Levels: TLogLevels read GetLevels write SetLevels;
    property EntryTypes: TLogEntryTypes read GetEntryTypes write SetEntryTypes;
  end;
  {$ENDREGION}


implementation


{$REGION 'TLogEntry'}

constructor TLogEntry.Create(level: TLogLevel; const msg: string);
begin
  fTimeStamp := Now; // Do this ASAP
  fLevel := level;
  fEntryType := TLogEntryType.Text;
  fMsg := msg;

  // Set default values
  fColor := clDefault;
  fStyle := [];

  // Reset non-managed fields
  fException := nil;
  fAddStack := False;
  fTag := 0;

  Assert(fData.IsEmpty);
end;

constructor TLogEntry.Create(level: TLogLevel; const msg: string;
  const e: Exception);
begin
  Create(level, msg);
  fException := e;
end;

constructor TLogEntry.Create(level: TLogLevel; entryType: TLogEntryType;
  const msg: string; const classType: TClass);
begin
  Create(level, msg);
  fClassType := classType;
  fEntryType := entryType;
end;

constructor TLogEntry.Create(level: TLogLevel; entryType: TLogEntryType;
  const msg: string; const classType: TClass; const data: TValue);
begin
  Create(level, entryType, msg, classType);
  fData := data;
end;

constructor TLogEntry.Create(level: TLogLevel; entryType: TLogEntryType;
  const msg: string);
begin
  Create(level, msg);
  fEntryType := entryType;
end;

function TLogEntry.SetColor(color: TColor): TLogEntry;
begin
  Result := Self;
  Result.fColor := color;
end;

function TLogEntry.AddStack: TLogEntry;
begin
  Result := Self;
  Result.fAddStack := True;
end;

function TLogEntry.SetClassType(const classType: TClass): TLogEntry;
begin
  Result := Self;
  Result.fClassType := classType;
end;

function TLogEntry.SetData(const Data: TValue): TLogEntry;
begin
  Result := Self;
  Result.fData := Data;
end;

function TLogEntry.SetException(const e: Exception): TLogEntry;
begin
  Result := Self;
  Result.fException := e;
end;

function TLogEntry.SetStyle(style: TLogStyles): TLogEntry;
begin
  Result := Self;
  Result.fStyle := style;
end;

function TLogEntry.SetTag(tag: NativeInt): TLogEntry;
begin
  Result := Self;
  Result.fTag := tag;
end;

{$ENDREGION}


end.
