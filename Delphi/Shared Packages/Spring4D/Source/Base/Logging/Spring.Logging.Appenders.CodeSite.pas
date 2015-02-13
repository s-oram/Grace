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

unit Spring.Logging.Appenders.CodeSite;

{$I Spring.inc}

interface

uses
  Spring.Logging,
  Spring.Logging.Appenders.Base;

type
  TCodeSiteAppender = class(TLogAppenderBase)
  protected
    procedure DoSend(const entry: TLogEntry); override;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  CodeSiteLogging,
  Spring;


{$REGION 'TCodeSiteAppender'}

procedure TCodeSiteAppender.DoSend(const entry: TLogEntry);
begin
  if entry.Color = clDefault then
    CodeSite.CategoryColor := $FFFFFF
  else
    CodeSite.CategoryColor := entry.Color;

  case entry.EntryType of
    TLogEntryType.Text:
      if Assigned(entry.Exception) then
        CodeSite.SendException(TLogAppenderBase.FormatMsg(entry), entry.Exception)
      else
        case entry.Level of
          TLogLevel.Unknown: ;

          TLogLevel.Trace:
            CodeSite.SendNote(entry.Msg);

          TLogLevel.Debug,
          TLogLevel.Text:
            CodeSite.SendMsg(entry.Msg);

          TLogLevel.Info:
            CodeSite.SendReminder(entry.Msg);

          TLogLevel.Warn:
            CodeSite.SendWarning(entry.Msg);

          TLogLevel.Error,
          TLogLevel.Fatal:
            CodeSite.SendError(entry.Msg);
        end;

    TLogEntryType.Value:
      case entry.Data.Kind of
        tkClass: CodeSite.Send(entry.Msg, entry.Data.AsObject);
      else
        CodeSite.Send(entry.Msg, entry.Data.ToString);
      end;

    TLogEntryType.Entering:
      CodeSite.EnterMethod(
        TLogAppenderBase.FormatMethodName(entry.ClassType, entry.Msg));

    TLogEntryType.Leaving:
      CodeSite.ExitMethod(
        TLogAppenderBase.FormatMethodName(entry.ClassType, entry.Msg));

    TLogEntryType.CallStack,
    TLogEntryType.SerializedData:
      CodeSite.Send(entry.Msg);
  end;
end;

{$ENDREGION}


end.
