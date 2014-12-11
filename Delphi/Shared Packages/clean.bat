:: ==== Batch file tips ====
:: how to remove all directories with wildcards.
:: http://serverfault.com/a/384915
:: http://stackoverflow.com/a/4904285/395461


:: Remove all compiler generated files. 
del /q /s *.dcu
del /q /s *.exe
del /q /s *.cbk
del /q /s *.drc
del /q /s *.dsk
del /q /s *.dsm
del /q /s *.identcache
del /q /s *.local
del /q /s *.map
::del /q /s *.ico
del /q /s *.otares
del /q /s *.txvpck
del /q /s *.txapackage
del /q /s *.tvsconfig
del /q /s *.projdata

:: removing res files might be a bad idea for some packages. Lets 
:: try it for now.
del /q /s *.res


:: Remove all history directories.
for /D /R %%X in (__history) do RMDIR /S /Q "%%X"


:: Remove all model support directories.
for /D /R %%X in (ModelSupport_*) do RMDIR /S /Q "%%X"


::pause

