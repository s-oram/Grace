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

unit Spring.Logging.Appenders;

interface

uses
  Classes,
  SyncObjs,
  SysUtils,
{$IFDEF FMX}
  FMX.Platform,
{$ENDIF}
  Spring.Logging,
  Spring.Logging.Appenders.Base;

type
  {$REGION 'TLogAppenderWithTimeStampFormat'}
  TLogAppenderWithTimeStampFormat = class abstract(TLogAppenderBase)
  private
    FFormat: string;
  protected
    function FormatTimeStamp(const timeStamp: TDateTime): string; inline;
  public
    constructor Create;

    property Format: string read FFormat write FFormat;
  end;
  {$ENDREGION}


  {$REGION 'TTextLogAppender'}
  /// <summary>
  ///   Simple appender that outputs the text to given Delphi Text file
  ///   (or pipe)
  /// </summary>
  TTextLogAppender = class(TLogAppenderWithTimeStampFormat)
  public type
    PTextFile = ^TextFile;
  protected
    fFile: PTextFile;
    procedure DoSend(const entry: TLogEntry); override;
  public
    /// <summary>
    ///   Make sure that the pointer doesn;t get out of scope!
    /// </summary>
    constructor Create(output: PTextFile); overload;
    /// <summary>
    ///   Uses stderr
    /// </summary>
    constructor Create; overload;
  end;
  {$ENDREGION}


  {$REGION 'TStreamLogAppender'}
  TStreamLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
    fStream: TStream;
{$IFNDEF AUTOREFCOUNT}
    fOwnsStream: Boolean;
{$ENDIF}
    fEncoding: TEncoding;
    fLock: TCriticalSection;
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    constructor Create(const stream: TStream; ownsStream: Boolean = True;
      const encoding: TEncoding = nil);
    destructor Destroy; override;
  end;
  {$ENDREGION}


  {$REGION 'TTraceLogAppender'}
{$IFDEF MSWINDOWS}
  TTraceLogAppender = class(TLogAppenderWithTimeStampFormat)
  protected
    procedure DoSend(const entry: TLogEntry); override;
  end;
{$ENDIF}
 {$ENDREGION}


  {$REGION 'TFMXLogAppender'}
{$IFDEF FMX}
  TFMXLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
    fService: IFMXLoggingService;
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    constructor Create;
  end;
{$ENDIF}
  {$ENDREGION}


  {$REGION 'TAndroidLogAppender'}
{$IFDEF ANDROID}
  TAndroidLogAppender = class(TLogAppenderWithTimeStampFormat)
  private
    fTagMarshaller: TMarshaller;
    fTag: MarshaledAString; //fTag is valid as long as marshaller is referenced
  protected
    procedure DoSend(const entry: TLogEntry); override;
  public
    /// <summary>
    ///   Creates Android logcat log appender, the tag can be used to
    ///   differentiate applications in the adb logcat viewer.
    /// </summary>
    constructor Create(const tag: string = 'delphiapp');
  end;
{$ENDIF}
  {$ENDREGION}


  {$REGION 'Default log appender assignment'}
{$IFDEF MSWINDOWS}
 {$IFDEF CONSOLE}
  TDefaultLogAppender = TTextLogAppender;
 {$ELSE !CONSOLE}
    TDefaultLogAppender = TTraceLogAppender;
 {$ENDIF CONSOLE}
{$ELSE !MSWINDOWS}
 {$IFDEF FMX}
  {$IFDEF ANDROID}
    TDefaultLogAppender = TAndroidLogAppender;
  {$ELSE !ANDROID}
    TDefaultLogAppender = TFMXLogAppender;
  {$ENDIF ANDROID}
 {$ELSE !FMX}
    TDefaultLogAppender = TTextLogAppender;
 {$ENDIF FMX}
{$ENDIF MSWINDOWS}
  {$ENDREGION}


implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF ANDROID}
  Androidapi.Log,
{$ENDIF}
  Spring;


{$REGION 'TLogAppenderWithTimeStampFormat'}

constructor TLogAppenderWithTimeStampFormat.Create;
begin
  inherited;
  Format := 'hh:nn:ss:zzz';
end;

function TLogAppenderWithTimeStampFormat.FormatTimeStamp(
  const timeStamp: TDateTime): string;
begin
  Result := FormatDateTime(Format, timeStamp);
end;

{$ENDREGION}


{$REGION 'TTextLogAppender'}

constructor TTextLogAppender.Create(output: PTextFile);
begin
  inherited Create;
  fFile := output;
end;

constructor TTextLogAppender.Create;
begin
  Create(@ErrOutput);
end;

procedure TTextLogAppender.DoSend(const entry: TLogEntry);
begin
  Writeln(fFile^, FormatTimeStamp(entry.TimeStamp), ': ',
    LEVEL_FIXED[entry.Level], ' ', FormatMsg(entry));
  Flush(fFile^);
end;

{$ENDREGION}


{$REGION 'TStreamLogAppender'}

constructor TStreamLogAppender.Create(const stream: TStream;
  ownsStream: Boolean; const encoding: TEncoding);
begin
  Guard.CheckNotNull(stream, 'stream');

  inherited Create;
  fStream := stream;
{$IFNDEF AUTOREFCOUNT}
  fOwnsStream := ownsStream;
{$ENDIF}
  if Assigned(encoding) then
    fEncoding := encoding
  else
    fEncoding := TEncoding.UTF8;
  fLock := TCriticalSection.Create;
end;

destructor TStreamLogAppender.Destroy;
begin
{$IFNDEF AUTOREFCOUNT}
  if fOwnsStream then
    fStream.Free;
{$ENDIF}
  fLock.Free;
  inherited;
end;

procedure TStreamLogAppender.DoSend(const entry: TLogEntry);
var
  buffer: TBytes;
begin
  fLock.Enter;
  try
    buffer := fEncoding.GetBytes(FormatTimeStamp(entry.TimeStamp) + ': ' +
      LEVEL_FIXED[entry.Level] + ' ' + FormatMsg(entry));
    fStream.WriteBuffer(buffer[0], Length(buffer));
  finally
    fLock.Leave;
  end;
end;

{$ENDREGION}


{$REGION 'TTraceLogAppender'}

{$IFDEF MSWINDOWS}
procedure TTraceLogAppender.DoSend(const entry: TLogEntry);
var
  buffer: string;
begin
  buffer := FormatTimeStamp(entry.TimeStamp) + ': ' + LEVEL[entry.Level] + ' ' +
    FormatMsg(entry);
  OutputDebugString(PChar(buffer));
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TFMXLogAppender'}

{$IFDEF FMX}
constructor TFMXLogAppender.Create;
begin
  fService := TPlatformServices.Current.GetPlatformService(
    IFMXLoggingService) as IFMXLoggingService;
end;

procedure TFMXLogAppender.DoSend(const entry: TLogEntry);
begin
  fService.Log(FormatTimeStamp(entry.TimeStamp) + ': ' + LEVEL[entry.Level] + ' ' +
    FormatMsg(entry), []);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TAndroidLogAppender'}

{$IFDEF ANDROID}
constructor TAndroidLogAppender.Create(const tag: string);
begin
  inherited Create;
  fTag := FTagMarshaller.AsAnsi(Tag).ToPointer;
end;

procedure TAndroidLogAppender.DoSend(const entry: TLogEntry);
const
  LEVEL: array[TLogLevel] of android_LogPriority = (
    ANDROID_LOG_UNKNOWN,
    ANDROID_LOG_VERBOSE,
    ANDROID_LOG_DEBUG,  //Debug
    ANDROID_LOG_DEBUG,  //Text
    ANDROID_LOG_INFO,
    ANDROID_LOG_WARN,
    ANDROID_LOG_ERROR,
    ANDROID_LOG_FATAL
  );
var
  m: TMarshaller;
  buffer: string;
begin
  buffer := FormatTimeStamp(entry.TimeStamp) + ': ' + FormatMsg(entry);
  __android_log_write(LEVEL[entry.Level], fTag, m.AsAnsi(buffer).ToPointer);
end;
{$ENDIF}

{$ENDREGION}


end.
