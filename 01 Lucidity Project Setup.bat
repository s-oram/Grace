:: mount the project directory as a virtual drive. 
subst s: "D:\Shared Data\Programming\Delphi Source Code\0000 Released Projects\Lucidity"

:: install baseline registery enteries.
::regedit.exe /s "s:\Config\LucidityStandard.reg" 

:: install custom registery enteries.
regedit.exe /s "s:\Config\LucidityCustom.reg" 



::PAUSE