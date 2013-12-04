:: mount the project directory as a virtual drive. 
subst s: "D:\Delphi Projects\Lucidity"

:: install baseline registery enteries.
::regedit.exe /s "s:\Config\LucidityStandard.reg" 

:: install custom registery enteries.
regedit.exe /s "s:\Config\LucidityCustom.reg" 



::PAUSE