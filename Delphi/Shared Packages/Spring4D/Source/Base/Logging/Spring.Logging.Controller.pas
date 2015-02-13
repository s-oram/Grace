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

unit Spring.Logging.Controller;

interface

uses
  TypInfo,
  Spring.Collections,
  Spring.Logging,
  Spring.Logging.Appenders.Base,
  Spring.Logging.Extensions;

type
  {$REGION 'TLoggerController'}
  TLoggerController = class(TLogAppenderBase, ILoggerController, ISerializerController)
  private
    fSerializers: IList<ITypeSerializer>;
    fStackTraceCollector: IStackTraceCollector;
    fStackTraceFormatter: IStackTraceFormatter;
    fAppenders: IList<ILogAppender>;
  protected
    procedure DoSend(const entry: TLogEntry); override;

    procedure SendData(const entry: TLogEntry);
    procedure SendStack(const entry: TLogEntry);

    /// <summary>
    ///   Returns <c>true</c> if level is enabled and any of the <c>entryTypes</c>
    ///    is enabled in any of the appenders or <c>false</c> otherwise
    /// </summary>
    function IsLoggable(level: TLogLevel; entryTypes: TLogEntryTypes): Boolean;
  public
    constructor Create; overload;
    constructor Create(const appenders: TArray<ILogAppender>); overload;
    constructor Create(const appenders: array of ILogAppender); overload;

    procedure AddAppender(const appender: ILogAppender);
    procedure AddSerializer(const serializer: ITypeSerializer);

    function FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
  end;
  {$ENDREGION}


implementation

uses
  Spring;


{$REGION 'TLoggerController'}

constructor TLoggerController.Create;
begin
  inherited Create;
  fAppenders := TCollections.CreateInterfaceList<ILogAppender>;
  fSerializers := TCollections.CreateInterfaceList<ITypeSerializer>;
end;

constructor TLoggerController.Create(const appenders: TArray<ILogAppender>);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

constructor TLoggerController.Create(const appenders: array of ILogAppender);
begin
  Create;
  fAppenders.AddRange(appenders);
end;

procedure TLoggerController.AddAppender(const appender: ILogAppender);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(appender, 'appender');
{$ENDIF}
  fAppenders.Add(appender);
end;

procedure TLoggerController.AddSerializer(const serializer: ITypeSerializer);
begin
  fSerializers.Add(serializer);
end;

procedure TLoggerController.DoSend(const entry: TLogEntry);
var
  appender: ILogAppender;
begin
  // After serialization or stack logging is added, and if such action is
  // required (we have a serializer capable of serializing given data or
  // AddStack and StackCollector) log the message first then go though all
  // appenders first and get their level and enabled state to check if there is
  // something to do in the first place
  for appender in fAppenders do
    appender.Send(entry);

  if not entry.Data.IsEmpty
    and IsLoggable(entry.Level, [TLogEntryType.SerializedData]) then
      SendData(entry);

  if entry.AddStackValue and Assigned(fStackTraceCollector)
    and Assigned(fStackTraceFormatter)
    and IsLoggable(entry.Level, [TLogEntryType.CallStack]) then
      SendStack(entry);
end;

function TLoggerController.FindSerializer(typeInfo: PTypeInfo): ITypeSerializer;
var
  serializer: ITypeSerializer;
begin
  for serializer in fSerializers do
    if serializer.HandlesType(typeInfo) then
      Exit(serializer);

  Result := nil;
end;

function TLoggerController.IsLoggable(level: TLogLevel;
  entryTypes: TLogEntryTypes): Boolean;
var
  appender: ILogAppender;
begin
  for appender in fAppenders do
    if appender.Enabled and (level in appender.Levels)
      and (entryTypes * appender.EntryTypes <> []) then
        Exit(True);

  Result := False;
end;

procedure TLoggerController.SendData(const entry: TLogEntry);
var
  serializer: ITypeSerializer;
begin
  serializer := FindSerializer(entry.Data.TypeInfo);

  if Assigned(serializer) then
    DoSend(TLogEntry.Create(entry.Level, TLogEntryType.SerializedData,
      serializer.Serialize(Self, entry.Data)));
end;

procedure TLoggerController.SendStack(const entry: TLogEntry);
var
  stack: TArray<Pointer>;
  formatted: TArray<string>;
  i: Integer;
  s: string;
begin
  stack := fStackTraceCollector.Collect;
  formatted := fStackTraceFormatter.Format(stack);

  if Length(stack) = 0 then
    Exit;

  s := formatted[0];
  for i := 1 to High(formatted) do
    s := s + #$A + formatted[i];
end;

{$ENDREGION}


end.
