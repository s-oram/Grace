:: Launch Delphi with a custom registery root.
:: This will start Delphi in it's default state. There will be
:: no plugins or helpers installed.
:: This allows custom packages to be installed to the IDE and only
:: be avaiable when working on Grace.
::
:: NOTES
:: http://delphi.wikia.com/wiki/IDE_Command_Line_Switches

SETLOCAL

:: IMPORTANT: Update to the location of the Delphi executable.
:: For example: "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\bds.exe"
SET Delphi="bds.exe"

start "" %Delphi% "S:\Delphi\LucidityProjectGroup.groupproj" -r Lucidity

ENDLOCAL




