@echo off
CALL %1
%FrameworkDir%\msbuild.exe /nologo %2 /target:Build /p:%3 /p:%4
if "%5"=="" goto :eof
pause