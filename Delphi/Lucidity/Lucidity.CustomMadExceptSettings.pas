unit Lucidity.CustomMadExceptSettings;

{$INCLUDE Defines.inc}

{$IFNDEF MadExcept}
interface
implementation
{$ELSE}

interface

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules;

implementation

uses
  eePluginDataDir,
  uConstants, eePluginUtils, madTools,
  Windows,
  SysUtils;


procedure CustomExceptionHandler(const exceptIntf : IMEException; var handled : boolean);
var
  BuildDetails : string;
  fn : string;
begin
  case ExceptIntF.Phase of
    epNotHandledYet: ;
    epQuickFiltering: ;
    epMainPhase:
    begin
      BuildDetails := '';

      //===== Version Info ==============

      BuildDetails := BuildDetails + 'Application Name: ' + kProductName + sLineBreak;
      BuildDetails := BuildDetails + 'Module: ' + GetDLLFilename + sLineBreak;

      BuildDetails := BuildDetails + 'Module File Version: ' +  GetBuildVersionAsString;
      {$IFDEF Beta}
        BuildDetails := BuildDetails + ' (BETA)';
      {$ENDIF}
      {$IFDEF Demo}
        BuildDetails := BuildDetails + ' (DEMO)';
      {$ENDIF}
      BuildDetails := BuildDetails + sLineBreak;


      BuildDetails := BuildDetails + 'Host: ' + GetHostApplicationFileName + sLineBreak;
      fn := GetHostApplicationFullPath;
      BuildDetails := BuildDetails + 'Host File Version: ' + GetFileVersionStr(fn) + sLineBreak;


      //===== Platform Info ==============
      {$IFDEF WIN32}
        BuildDetails := BuildDetails + 'Platform: Win32';
      {$ENDIF}
      {$IFDEF WIN64}
        BuildDetails := BuildDetails + 'Platform: Win64';
      {$ENDIF}
      {$IFDEF MACOS32}
        BuildDetails := BuildDetails + 'Platform: MacOSx32';
      {$ENDIF}
      BuildDetails := BuildDetails + sLineBreak;


      //===== CPU Info ==============
      {$IFDEF CPUX86}
        BuildDetails := BuildDetails + 'CPU: x86';
      {$ENDIF}
      {$IFDEF CPUX64}
        BuildDetails := BuildDetails + 'CPU: x64';
      {$ENDIF}
      BuildDetails := BuildDetails + sLineBreak;


      //=== Add build details to bug report =======
      exceptIntF.BugReportSections.Add('Build Details', BuildDetails);
    end;
    epPostProcessing: ;
    epCompleteReport: ;
  end;

end;


var
  fn : string;

initialization
  //http://help.madshi.net/madExceptUnit.htm#RegisterExceptionHandler
  //RegisterExceptionHandler(ExceptionBlocker, stDontSync);
  RegisterExceptionHandler(CustomExceptionHandler, stDontSync);
  //InstallUnhandledExceptionFilter;

  // MESettings documentation here:
  // http://help.madshi.net/MESettings.htm#IMESettings
  MESettings.AutoSave          := true;
  MESettings.AutoSaveIfNotSent := true;
  MESettings.AutoSend          := true;
  MESettings.AutoSendPrgrBox   := true;
  MESettings.AutoClipboard     := true;
  MESettings.SuspendThreads    := false;
  MESettings.ShowPleaseWaitBox := true;
  //MESettings.ShowPleaseWaitBox := false;
  MESettings.AutoContinue      := false;


  MESettings.SendBtnVisible     := true;
  MESettings.SaveBtnVisible     := true;
  MESettings.PrintBtnVisible    := false;
  MESettings.ShowBtnVisible     := true;
  MESettings.ContinueBtnVisible := true;
  MESettings.RestartBtnVisible  := false;
  MESettings.CloseBtnVisible    := false;


  //==== Attachment Settings =======
  MESettings.SendInBackground := true;
  MESettings.MailAddr         := 'shannon@onesmallclue.com';

  MESettings.MailViaMapi   := true;
  MESettings.MailViaMailto := true;

  MESettings.AttachBugReport := true;
  MESettings.AttachBugReportFile := true;

  //MESettings.AdditionalAttachments

  //===== Save Settings =====
  if (PluginDataDir^.Exists) then
  begin
    fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Error Reports';
    if not DirectoryExists(fn) then CreateDir(fn);
    fn := IncludeTrailingPathDelimiter(fn) + kProductName + ' Bug Report.txt';

    MESettings.BugReportFile := fn;
    MESettings.AppendBugReports := true;
    MESettings.NoDupExcepts := true;
  end;

  //====== Bug Report Settings =====
  MESettings.ListThreads      := false;
  MESettings.ShowCpuRegisters := false;
  MESettings.ShowStackDump    := false;
  MESettings.ShowDisAsm       := false;


  MESettings.HideUglyItems     := false;
  MESettings.ShowRelativeAddrs := false;
  MESettings.ShowRelativeLines := false;

  {$IFDEF Debug}
    //MESettings.CrashOnOverrun  := true;
    //MESettings.CrashOnUnderrun := true;
  {$ELSE}
    //MESettings.CrashOnOverrun  := false;
    //MESettings.CrashOnUnderrun := false;
  {$ENDIF}


finalization

{$ENDIF}
end.



