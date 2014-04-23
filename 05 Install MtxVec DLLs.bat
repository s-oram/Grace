:: Copy 64bit dlls
xcopy /y "S:\MtxVec Dlls\Single64\MtxVec.*.dll"  "C:\Windows\System32\"

xcopy /y "S:\MtxVec Dlls\Double64\MtxVec.*.dll"  "C:\Windows\System32\"
xcopy /y "S:\MtxVec Dlls\Double64\libiomp5md.dll"  "C:\Windows\System32\"


:: Copy 32bit dlls
xcopy /y "S:\MtxVec Dlls\Single\bdss.dll"  "C:\Windows\SysWOW64\"
xcopy /y "S:\MtxVec Dlls\Single\libiomp5md.dll"  "C:\Windows\SysWOW64\"
xcopy /y "S:\MtxVec Dlls\Single\MtxVec.*.dll"  "C:\Windows\SysWOW64\"

xcopy /y "S:\MtxVec Dlls\Double\bdss.dll"  "C:\Windows\SysWOW64\"
xcopy /y "S:\MtxVec Dlls\Double\libiomp5md.dll"  "C:\Windows\SysWOW64\"
xcopy /y "S:\MtxVec Dlls\Double\MtxVec.*.dll"  "C:\Windows\SysWOW64\"



:: XCopy info
:: http://stackoverflow.com/a/986505/395461