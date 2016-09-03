:: This script maps the source directory to a virtual drive
:: located at S:\
::
:: IMPORTANT: Update the ProjectPath variable to match the
:: project path location on your system.

SETLOCAL

SET ProjectPath="D:\Delphi Projects\Lucidity"

:: SUBST reference
:: http://www.computerhope.com/substhlp.htm
SUBST s: %ProjectPath%

ENDLOCAL
