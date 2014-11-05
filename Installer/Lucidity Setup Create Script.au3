#include <File.au3>
#include <Array.au3>
#include <_Zip.au3>

;;Delete all files in the Inno Setup output folder.
FileDelete("S:\Bin\Installers\*.*")

;; Copy files from the Delphi compiler output locations to the Inno Setup directorys. 
;; NOTE: Some files are renamed in this step.
Local $SourceFile = "S:\Bin\Lucidity_Win32_Full\Grace.dll"
Local $DestFile   = "S:\Installer\Vst32 Dir\Grace.dll"
FileCopy($SourceFile, $DestFile, 1);

Local $SourceFile = "S:\Bin\Lucidity_Win64_Full\Grace.dll"
Local $DestFile   = "S:\Installer\Vst64 Dir\Grace.dll"
FileCopy($SourceFile, $DestFile, 1);

;; Execute the Inno Setup compile scripts. 
ExecuteInnoSetupScript("S:\Installer\Lucidity_x32.iss");
ExecuteInnoSetupScript("S:\Installer\Lucidity_x64.iss");
ExecuteInnoSetupScript("S:\Installer\Lucidity_demo_x32.iss");
ExecuteInnoSetupScript("S:\Installer\Lucidity_demo_x64.iss");

;; Zip all the created setup files.
ZipAllFilesInDirectory("S:\Bin\Installers\");

;; Open the Output folder.
ShellExecute("S:\Bin\Installers");




;;============ Some Functions =============================================

Func ZipAllFilesInDirectory($Directory)
   Local $Files[1];
   
   ;; Find all files in Directory.    
   $Search = FileFindFirstFile ($Directory & "*.*");

   if $Search = -1 Then
	  MsgBox(0,"Error", "No files found.");
	  Exit
   EndIf
   
   While 1
	   Local $file = FileFindNextFile($search)
	   If @error Then ExitLoop	   
	   _arrayAdd($Files, $file);
   WEnd   
	 
   ;; Close the search handle.
   FileClose($Search)  
   
   ;; NOTE: This for loop causes a zip file names ".zip" to be created (containing
   ;; the contents of the directory. 
   For $Element In $Files	  
	  ZipFile($Directory & $Element)
   Next
   
   ;; HACK: Delete the .zip file that the above code creates. 
   FileDelete($Directory & ".zip")
EndFunc
   

Func ZipFile($SourceFileName)
   Local $szDrive, $szDir, $szFName, $szExt   
   Local $sZipFile, $sItem  
  
   _PathSplit($SourceFileName, $szDrive, $szDir, $szFName, $szExt)
   
   $szFName = StringReplace($szFName, " ", "_")
   
   $sZipFile = $szDrive & $szDir & $szFName & ".zip"
   $sItem = $SourceFileName
   
   $r = _Zip_Create($sZipFile)
   $r = _Zip_AddItem($sZipFile, $sItem)
EndFunc

Func ExecuteInnoSetupScript($ScriptFileName)
   Local $InnoSetupCompiler = 'C:\Program Files (x86)\Inno Setup 5\Compil32.exe'    
   local $CommandLineParameters = StringFormat('/cc "%s"', $ScriptFileName) 
   ShellExecuteWait($InnoSetupCompiler, $CommandLineParameters);
EndFunc