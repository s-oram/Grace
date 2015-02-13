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

unit Spring.Logging.Configuration.Builder;

interface

uses
  Rtti,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Logging;

type
  {$REGION 'TLoggingConfigurationBuilder'}
  TLoggingConfigurationBuilder = record
  private type
    {$REGION 'IBuilder'}
    IBuilder = interface
      procedure BeginAppender(const name: string; const className: string);
      procedure EndAppender;

      procedure BeginController(const name: string; const className: string);
      procedure EndController;

      procedure BeginLogger(const name: string; const className: string);
      procedure EndLogger;

      procedure Prop(const name: string; const value: TValue);

      function ToString: string;
    end;
    {$ENDREGION}
  public type
    {$REGION 'TAppenderBuilder'}
    TAppenderBuilder = record
    private
      fBuilder: IBuilder;
      {$IFDEF DELPHI2010}{$HINTS OFF}fDummy: Pointer;{$ENDIF}
    public
      function EndAppender: TLoggingConfigurationBuilder;

      function Enabled(value: Boolean): TAppenderBuilder;
      function Levels(value: TLogLevels): TAppenderBuilder;
      function EntryTypes(value: TLogEntryTypes): TAppenderBuilder;
      function Prop(const name: string; const value: TValue): TAppenderBuilder; overload;
      function Prop<E>(const name: string; const value: E): TAppenderBuilder; overload;
    end;
    {$ENDREGION}

    {$REGION 'TControllerBuilder'}
    TControllerBuilder = record
    private
      fBuilder: IBuilder;
      {$IFDEF DELPHI2010}{$HINTS OFF}fDummy: Pointer;{$ENDIF}
    public
      function EndController: TLoggingConfigurationBuilder;

      function Enabled(value: Boolean): TControllerBuilder;
      function Levels(value: TLogLevels): TControllerBuilder;
      function EntryTypes(value: TLogEntryTypes): TControllerBuilder;
      function AddAppender(const name: string): TControllerBuilder;
      function AddSerializer(const className: string): TControllerBuilder; overload;
      function AddSerializer(const classType: TClass): TControllerBuilder; overload;
      function Prop(const name: string; const value: TValue): TControllerBuilder; overload;
      function Prop<E>(const name: string; const value: E): TControllerBuilder; overload;
    end;
    {$ENDREGION}

    {$REGION 'TLoggerBuilder'}
    TLoggerBuilder = record
    private
      fBuilder: IBuilder;
      {$IFDEF DELPHI2010}{$HINTS OFF}fDummy: Pointer;{$ENDIF}
    public
      function EndLogger: TLoggingConfigurationBuilder;

      function Enabled(value: Boolean): TLoggerBuilder;
      function Levels(value: TLogLevels): TLoggerBuilder;
      function EntryTypes(value: TLogEntryTypes): TLoggerBuilder;
      function Controller(const name: string): TLoggerBuilder;
      function Assign(const className: string): TLoggerBuilder; overload;
      function Assign(const classType: TClass): TLoggerBuilder; overload;
      function Prop(const name: string; const value: TValue): TLoggerBuilder; overload;
      function Prop<E>(const name: string; const value: E): TLoggerBuilder; overload;
    end;
    {$ENDREGION}
  private
    fBuilder: IBuilder;
    {$IFDEF DELPHI2010}{$HINTS OFF}fDummy: Pointer;{$ENDIF}
  public
    class function Create: TLoggingConfigurationBuilder; static;

    function BeginAppender(const name: string;
      const className: string): TAppenderBuilder; overload;
    function BeginAppender(const name: string;
      const classType: TClass): TAppenderBuilder; overload;

    function BeginController(const name: string = '';
      const className: string = ''): TControllerBuilder; overload;
    function BeginController(const name: string;
      const classType: TClass): TControllerBuilder; overload;

    function BeginLogger(const name: string = '';
      const className: string = ''): TLoggerBuilder; overload;
    function BeginLogger(const name: string;
      const classType: TClass): TLoggerBuilder; overload;

    function ToString: string;
  end;
  {$ENDREGION}


implementation

const
  SClass = 'class = %s';
  SEnabled = 'enabled';
  SLevels = 'levels';
  SEntryTypes = 'entryTypes';
  SAppender = 'appender';
  SController = 'controller';
  SSerializer = 'serializer';
  SAssign = 'assign';
  SDefault = 'default';
type
  TBuilderState = (bsRoot, bsAppender, bsController, bsLogger, bsDone);
  TBuilderStates = set of TBuilderState;


{$REGION 'TBuilder'}
  TBuilder = class(TInterfacedObject, TLoggingConfigurationBuilder.IBuilder)
  private
    fString: TStringBuilder;
    fState: TBuilderState;

    constructor Create;
    procedure CheckState(const states: TBuilderStates);
  public
    destructor Destroy; override;

    procedure BeginAppender(const name: string; const className: string);
    procedure EndAppender;

    procedure BeginController(const name: string; const className: string = '');
    procedure EndController;

    procedure BeginLogger(const name: string; const className: string = '');
    procedure EndLogger;

    procedure Prop(const name: string; const value: TValue);

    function ToString: string; override;
  end;


procedure TBuilder.BeginAppender(const name: string; const className: string);
begin
  Guard.CheckNotNull(name <> '', 'className');
  CheckState([bsRoot]);
  fString
    .AppendFormat('[appenders\%s]', [name]).AppendLine
    .AppendFormat(SClass, [className]).AppendLine;
  fState := bsAppender;
end;

procedure TBuilder.BeginController(const name: string; const className: string);
begin
  CheckState([bsRoot]);
  fString.AppendFormat('[controllers\%s]', [name]).AppendLine;
  if (className <> '') then
    fString.AppendFormat(SClass, [className]).AppendLine;
  fState := bsController;
end;

procedure TBuilder.BeginLogger(const name: string; const className: string);
begin
  CheckState([bsRoot]);
  fString.AppendFormat('[loggers\%s]', [name]).AppendLine;
  if (className <> '') then
    fString.AppendFormat(SClass, [className]).AppendLine;
  fState := bsLogger;
end;

procedure TBuilder.CheckState(const states: TBuilderStates);
begin
  Assert(fState in states);
end;

constructor TBuilder.Create;
begin
  inherited;
  fString := TStringBuilder.Create;
end;

destructor TBuilder.Destroy;
begin
  fString.Free;
  inherited;
end;

procedure TBuilder.EndAppender;
begin
  CheckState([bsAppender]);
  fString.AppendLine;
  fState := bsRoot;
end;

procedure TBuilder.EndController;
begin
  CheckState([bsController]);
  fString.AppendLine;
  fState := bsRoot;
end;

procedure TBuilder.EndLogger;
begin
  CheckState([bsLogger]);
  fString.AppendLine;
  fState := bsRoot;
end;

procedure TBuilder.Prop(const name: string; const value: TValue);
begin
  CheckState([bsAppender, bsController, bsLogger]);
  fString.AppendFormat('%s = %s', [name, value.ToString]).AppendLine;
end;

function TBuilder.ToString: string;
begin
  CheckState([bsRoot]);
  fState := bsDone;
  Result := fString.ToString;
  FreeAndNil(fString);
end;
{$ENDREGION}


{$REGION 'TLoggingConfigurationBuilder'}

function TLoggingConfigurationBuilder.BeginAppender(const name,
  className: string): TAppenderBuilder;
begin
  fBuilder.BeginAppender(name, className);
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.BeginAppender(const name: string;
  const classType: TClass): TAppenderBuilder;
begin
  Guard.CheckNotNull(classType, 'classType');
  fBuilder.BeginAppender(name, GetQualifiedClassName(classType));
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.BeginController(const name,
  className: string): TControllerBuilder;
begin
  if name = '' then
    fBuilder.BeginController(SDefault, className)
  else
    fBuilder.BeginController(name, className);
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.BeginController(const name: string;
  const classType: TClass): TControllerBuilder;
begin
  Guard.CheckNotNull(classType, 'classType');
  fBuilder.BeginController(name, GetQualifiedClassName(classType));
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.BeginLogger(const name,
  className: string): TLoggerBuilder;
begin
  if name = '' then
    fBuilder.BeginLogger(SDefault, className)
  else
    fBuilder.BeginLogger(name, className);
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.BeginLogger(const name: string;
  const classType: TClass): TLoggerBuilder;
begin
  Guard.CheckNotNull(classType, 'classType');
  fBuilder.BeginLogger(name, GetQualifiedClassName(classType));
  Result.fBuilder := fBuilder;
end;

class function TLoggingConfigurationBuilder.Create: TLoggingConfigurationBuilder;
begin
  Result.fBuilder := TBuilder.Create;
end;

function TLoggingConfigurationBuilder.ToString: string;
begin
  Result := fBuilder.ToString;
end;

{$ENDREGION}


{$REGION 'TLoggingConfigurationBuilder.TAppenderBuilder'}

function TLoggingConfigurationBuilder.TAppenderBuilder.Enabled(
  value: Boolean): TAppenderBuilder;
begin
  fBuilder.Prop(SEnabled, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TAppenderBuilder.EndAppender: TLoggingConfigurationBuilder;
begin
  fBuilder.EndAppender;
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.TAppenderBuilder.EntryTypes(
  value: TLogEntryTypes): TAppenderBuilder;
begin
  fBuilder.Prop(SEntryTypes, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TAppenderBuilder.Levels(
  value: TLogLevels): TAppenderBuilder;
begin
  fBuilder.Prop(SLevels, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TAppenderBuilder.Prop(const name: string;
  const value: TValue): TAppenderBuilder;
begin
  fBuilder.Prop(name, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TAppenderBuilder.Prop<E>(
  const name: string; const value: E): TAppenderBuilder;
begin
  fBuilder.Prop(name, TValue.From<E>(value));
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TLoggingConfigurationBuilder.TControllerBuilder'}

function TLoggingConfigurationBuilder.TControllerBuilder.AddAppender(
  const name: string): TControllerBuilder;
begin
  fBuilder.Prop(SAppender, name);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.AddSerializer(
  const className: string): TControllerBuilder;
begin
  Guard.checkNotNull(className <> '', 'className');
  fBuilder.Prop(SSerializer, className);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.AddSerializer(
  const classType: TClass): TControllerBuilder;
begin
  Guard.checkNotNull(classType, 'classType');
  fBuilder.Prop(SSerializer, GetQualifiedClassName(classType));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.Enabled(
  value: Boolean): TControllerBuilder;
begin
  fBuilder.Prop(SEnabled, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.EndController: TLoggingConfigurationBuilder;
begin
  fBuilder.EndController;
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.EntryTypes(
  value: TLogEntryTypes): TControllerBuilder;
begin
  fBuilder.Prop(SEntryTypes, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.Levels(
  value: TLogLevels): TControllerBuilder;
begin
  fBuilder.Prop(SLevels, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.Prop(
  const name: string; const value: TValue): TControllerBuilder;
begin
  fBuilder.Prop(name, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TControllerBuilder.Prop<E>(
  const name: string; const value: E): TControllerBuilder;
begin
  fBuilder.Prop(name, TValue.From<E>(value));
  Result := Self;
end;

{$ENDREGION}


{$REGION 'TLoggingConfigurationBuilder.TLoggerBuilder'}

function TLoggingConfigurationBuilder.TLoggerBuilder.Assign(
  const className: string): TLoggerBuilder;
begin
  Guard.CheckNotNull(className <> '', 'className');
  fBuilder.Prop(SAssign, className);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Assign(
  const classType: TClass): TLoggerBuilder;
begin
  Guard.CheckNotNull(classType, 'classType');
  fBuilder.Prop(SAssign, GetQualifiedClassName(classType));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Controller(
  const name: string): TLoggerBuilder;
begin
  fBuilder.Prop(SController, name);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Enabled(
  value: Boolean): TLoggerBuilder;
begin
  fBuilder.Prop(SEnabled, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.EndLogger: TLoggingConfigurationBuilder;
begin
  fBuilder.EndLogger;
  Result.fBuilder := fBuilder;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.EntryTypes(
  value: TLogEntryTypes): TLoggerBuilder;
begin
  fBuilder.Prop(SEntryTypes, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Levels(
  value: TLogLevels): TLoggerBuilder;
begin
  fBuilder.Prop(SLevels, TValue.From(value));
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Prop(const name: string;
  const value: TValue): TLoggerBuilder;
begin
  fBuilder.Prop(name, value);
  Result := Self;
end;

function TLoggingConfigurationBuilder.TLoggerBuilder.Prop<E>(const name: string;
  const value: E): TLoggerBuilder;
begin
  fBuilder.Prop(name, TValue.From<E>(value));
  Result := Self;
end;

{$ENDREGION}


end.
