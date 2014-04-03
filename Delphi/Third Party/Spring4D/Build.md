# Documentation for `Build.exe`

## Aim

`Build.exe` can build the Spring4D framework and update the Delphi Library path for each selected Delphi/Platform combination.

## Configuration files

`Build.exe` uses two configuration files:

- `Build.Settings.ini`
- `Build.Settings.Compilers.ini`

They are documented in reverse order, as the first one depends on the second one.

### `Build.Settings.Compilers.ini` - command-line compiler settings.

This file contains sections like this:

    [DelphiXE5.OSX32]
    DisplayName=RAD Studio XE5 (OSX32)
    Platform=OSX32
    Keys.BDS=Software\Embarcadero\BDS\12.0
    Keys.Library=Library\OSX32

`Build.exe` reads these sections in the order if this file.

Names that can be used per section:

- `DisplayName` (default `''`)  
descriptive name of the `Delphi Version` / `(Platform)` combination
- `Platform` (default `Win32`)  
valid platform from the list in [possible platforms](#possiblePlatforms).
- `Keys.BDS` (default `''`)  
base key in `HKEY_CURRENT_USER` pointing to the `BDS` configuration of your Delphi installation like mentioned in <https://bitbucket.org/jeroenp/besharp.net/src/tip/Native/Delphi/Scripts/List-Delphi-Installed-Packages.ps1>
- `Keys.Library` (default `Library`, for Delphi 2010 compatibility)  
specific key within `Keys.BDS` pointing to the Library configuration of the specific platform containing information like the `Search Path` for that plaform.
- `Keys.Globals` (default`'Globals'`)  
specific key within `Keys.BDS` pointing to the Global Delphi configuration that contains the value for `ForceEnvOptionsUpdate`.
- `Keys.EnvironmentVariables` (default `'Environment Variables'`)  
specific key within `Keys.BDS` pointing to the environment variables in the Delphi configuration in case they need to be adapted.
- `Names.LibraryPath` (default `'Search Path'`)  
specific name within `Keys.Library` pointing to the Library Path (usually the value named `Search Path`) for that plaform.
- `Names.BrowsingPath` (default `'Browsing Path'`)  
specific name within `Keys.Library` pointing to the Browsing Path (usually the value named `Browsing Path`) for that plaform.
- `Names.RootDir` (default `'RootDir'`)  
specific name within `Keys.BDS` pointing to the Delphi root directory (usually the value named `RootDir`) to determine the location of `'bin\rsvars.bat'`

### `Build.Settings.ini`

This contains one section with global settings:

    [Globals]
    Name=Spring Framework for Delphi
    Homepage=http://www.spring4d.org
    BaseDir=.
    Config=Release
    SourceBaseDir=Source
    SourcePaths=Base;Base\Collections;Base\Reflection;Core\Services;Core\Container;Extensions\Utils;Extensions\Cryptography
    SelectedTasks=DelphiXE5.Win32;DelphiXE5.Win64;DelphiXE5.OSX32
    RunTests=1
    ModifyDelphiRegistrySettings=0

and multiple sections like:

    [DelphiXE5.OSX32]
    Projects=Packages\DelphiXE5\Spring4D.groupproj
    UnitOutputPaths=Library\DelphiXE5\$(Platform)\$(Config)

#### `Globals` section

##### `ModifyDelphiRegistrySettings` 

if set to `1`, after building `Spring4D` it will modify the Delphi registry settings in the sub key `HKCU\Software\[Company]\BDS\[Version].0\Library\[Platform]\`.

Where 

- `Company` can be (depending on the Delphi version) any of:
    - `Codegear` (Delphi 2010)
    - `Embarcadero` (Delphi XE and higher)
- `Platform` are from the list in [possible platforms](#possiblePlatforms).
- `Version` is the [BDS version](http://theroadtodelphi.wordpress.com/2010/10/27/detecting-installed-delphi-versions/) (or Galileo version)

In the registry sub key, these string values are updated:

- `Browsing Path` (updated with the source directory paths of Spring4D)
- `Search Path` (updated with the unit output paths of Spring4D)

If those values already contain the right paths, they are not updated again.

##### `Globals\ForceEnvOptionsUpdate=1` 

to force Delphi to update `EnvOptions.proj` at the next start so the `msbuild` process stays in sync with the registry settings.

## <a name='possiblePlatforms'>Possible platforms</a>

You can find the platforms that your Delphi version supports by looking at a key like this:

    HKEY_CURRENT_USER\Software\[Company]\BDS\[Version].0\Library

This particular key is for Delphi XE5. You can find more keys in <https://bitbucket.org/jeroenp/besharp.net/src/tip/Native/Delphi/Scripts/List-Delphi-Installed-Packages.ps1>, which supports these platform subkeys:

- `Android32`
- `iOSDevice`
- `iOSSimulator`
- `OSX32`
- `Win32`
- `Win64`

Note that `Android32` and `iOSDevice` are for `Arm` support, but `iOSSimulator` is for `x86` support.
 
## Supporting newer Delphi versions and platforms

In this example, we add `OSX32` support for `DelphiXE5`. Replace these according to your Delphi version.

### Update `Build.Settings.ini`

Add this section:

    [DelphiXE5.OSX32]
    Projects=Packages\DelphiXE5\Spring4D.groupproj
    UnitOutputPaths=Library\DelphiXE5\$(Platform)\$(Config)

- Replace `DelphiXE5` with your Delphi version.
- Replace `OSX32` with a supported platform in your Delphi version from the list in [possible platforms](#possiblePlatforms).

### Update `Build.Settings.Compilers.ini`

Add this section:

    [DelphiXE5.OSX32]
    DisplayName=RAD Studio XE5 (OSX32)
    Platform=OSX32
    Keys.BDS=Software\Embarcadero\BDS\12.0
    Keys.Library=Library\OSX32

Then in this section:

1. Replace the value of `DisplayName` with a description similar to the pattern used by other `DisplayName` values.
2. Replace `DelphiXE5` with your Delphi version.
3. Replace `OSX32` with a supported platform in your Delphi version from the list in [possible platforms](#possiblePlatforms).
