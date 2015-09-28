unit ee3AddOn.MadExcept;

interface

type
  PMadExceptConfig = ^TMadExceptConfig;
  TMadExceptConfig = record
    ProductName : string;
    DllFileName : string;
    VersionInfoEx : string; //This info appended to version number. useful to include DEMO or DEBUG tags etc.
    EmailAddress : string; //Where to email error reports.
    ErrorReportDirectory : string; //where to save error reports.
    procedure AssignFrom(Source : PMadExceptConfig);
    procedure PerformDataSanityCheck;
  end;


procedure SetupMadExcept(aConfig : PMadExceptConfig);

implementation

uses
  SysUtils,
  Vst2Ex.Utils,
  madTools,
  MadExcept;

var
  GlobalConfig : TMadExceptConfig;

{ TMadExceptConfig }

procedure TMadExceptConfig.AssignFrom(Source: PMadExceptConfig);
begin
  self.ProductName   := Source^.ProductName;
  self.DllFileName   := Source^.DllFileName;
  self.VersionInfoEx := Source^.VersionInfoEx;
  self.ErrorReportDirectory := Source^.ErrorReportDirectory;
  self.EmailAddress := Source^.EmailAddress;
end;

procedure TMadExceptConfig.PerformDataSanityCheck;
begin
  // Check values aren't empty.
  if ProductName          = '' then raise Exception.Create('MadExcept config value cannot be empty.');
  if DllFileName          = '' then raise Exception.Create('MadExcept config value cannot be empty.');
  if VersionInfoEx        = '' then raise Exception.Create('MadExcept config value cannot be empty.');
  if ErrorReportDirectory = '' then raise Exception.Create('MadExcept config value cannot be empty.');
  if EmailAddress         = '' then raise Exception.Create('MadExcept config value cannot be empty.');
end;




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

      BuildDetails := BuildDetails + 'Application Name: ' + GlobalConfig.ProductName + sLineBreak;
      BuildDetails := BuildDetails + 'DLL: ' + GlobalConfig.DLLFilename + sLineBreak;
      BuildDetails := BuildDetails + 'DLL File Version: ' +  GetBuildVersionAsString + ' ' + GlobalConfig.VersionInfoEx;
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

procedure SetupMadExcept(aConfig : PMadExceptConfig);
var
  fn : string;
begin
  GlobalConfig.AssignFrom(aConfig);
  GlobalConfig.PerformDataSanityCheck; //a saftey measure after getting the config info.


  //http://help.madshi.net/madExceptUnit.htm#RegisterExceptionHandler
  //RegisterExceptionHandler(ExceptionBlocker, stDontSync);
  RegisterExceptionHandler(CustomExceptionHandler, stDontSync);
  InstallUnhandledExceptionFilter;

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
  MESettings.MailAddr         := GlobalConfig.EmailAddress;

  MESettings.MailViaMapi   := true;
  MESettings.MailViaMailto := true;

  MESettings.AttachBugReport := true;
  MESettings.AttachBugReportFile := true;

  //MESettings.AdditionalAttachments

  //===== Save Settings =====
  fn := IncludeTrailingPathDelimiter(GlobalConfig.ErrorReportDirectory) + GlobalConfig.ProductName + ' Bug Report.txt';
  MESettings.BugReportFile := fn;
  MESettings.AppendBugReports := true;
  MESettings.NoDupExcepts := true;

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


  //TODO:HIGH check if this code sets up all the MadExcept configuration values as required.

end;


end.
