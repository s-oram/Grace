:: Should add locations to delete BPLs - all the installed packaged locations.

:: Should clean virtual store locations
:: http://stackoverflow.com/a/14463652/395461

:: Remove all history directories.
for /D /R %%X in (__history) do RMDIR /S /Q "%%X"


:: Remove all model support directories.
for /D /R %%X in (ModelSupport_*) do RMDIR /S /Q "%%X"