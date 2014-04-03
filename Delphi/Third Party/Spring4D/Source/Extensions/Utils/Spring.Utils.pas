{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

///	<summary>
///	  This namespace provides many well-encapsulated utility classes and
///	  routines about the environment and system.
///	</summary>
unit Spring.Utils;

{$I Spring.inc}

interface

uses
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF MSWINDOWS}
  SysUtils,
  DateUtils,
  StrUtils,
  Variants,
  TypInfo,
  Types,
{$IFDEF MSWINDOWS}
  ShlObj,
  ShellAPI,
  ActiveX,
{$IFDEF HAS_UNITSCOPE}
  System.Win.ComObj,
  System.Win.Registry,
{$ELSE}
  ComObj,
  Registry,
{$ENDIF}
{$ENDIF MSWINDOWS}
  Rtti,
  Generics.Collections,
{$IFDEF MSWINDOWS}
  Spring.Utils.WinAPI,
{$ENDIF MSWINDOWS}
  Spring,
  Spring.SystemUtils,
  Spring.Collections;

type
  TEnum = Spring.SystemUtils.TEnum;


  {$REGION 'TVersion'}

  ///	<summary>
  ///	  Represents a version number in the format of
  ///	  "major.minor[.build[.revision]]", which is different from the delphi
  ///	  style format "major.minor[.release[.build]]".
  ///	</summary>
  TVersion = record
  private
    const fCUndefined: Integer = -1;
  strict private
    fMajor: Integer;
    fMinor: Integer;
    fBuild: Integer;      // -1 if undefined.
    fReversion: Integer;  // -1 if undefined.
    function GetMajorReversion: Int16;
    function GetMinorReversion: Int16;
  private
    constructor InternalCreate(defined, major, minor, build, reversion: Integer);
    function CompareComponent(a, b: Integer): Integer;
    function IsDefined(const component: Integer): Boolean; inline;
  public
    constructor Create(major, minor: Integer); overload;
    constructor Create(major, minor, build: Integer); overload;
    constructor Create(major, minor, build, reversion: Integer); overload;
    constructor Create(const versionString: string); overload;
    function CompareTo(const version: TVersion): Integer;
    function Equals(const version: TVersion): Boolean;
    function ToString: string; overload;
    function ToString(fieldCount: Integer): string; overload;
    property Major: Integer read fMajor;
    property MajorReversion: Int16 read GetMajorReversion;
    property Minor: Integer read fMinor;
    property MinorReversion: Int16 read GetMinorReversion;
    property Build: Integer read fBuild;
    property Reversion: Integer read fReversion;
    { Operator Overloads }
    class operator Equal(const left, right: TVersion): Boolean;
    class operator NotEqual(const left, right: TVersion): Boolean;
    class operator GreaterThan(const left, right: TVersion): Boolean;
    class operator GreaterThanOrEqual(const left, right: TVersion): Boolean;
    class operator LessThan(const left, right: TVersion): Boolean;
    class operator LessThanOrEqual(const left, right: TVersion): Boolean;
  end;

  {$ENDREGION}


  {$REGION 'TFileVersionInfo'}

  ///	<summary>
  ///	  Provides version information for a physical file on disk.
  ///	</summary>
  ///	<remarks>
  ///	  <para>
  ///	    Use the <see cref="GetVersionInfo(string)">GetVersionInfo</see>method
  ///	    of this class to get a FileVersionInfo containing information about a
  ///	    file, then look at the properties for information about the file.
  ///	    Call <see cref="ToString" /> to get a partial list of properties and
  ///	    their values for this file.
  ///	  </para>
  ///	  <para>
  ///	    The TFileVersionInfo properties are based on version resource
  ///	    information built into the file. Version resources are often built
  ///	    into binary files such as .exe or .dll files; text files do not have
  ///	    version resource information.
  ///	  </para>
  ///	  <para>
  ///	    Version resources are typically specified in a Win32 resource file,
  ///	    or in assembly attributes. For example the <see cref="IsDebug" />prope
  ///	    rty reflects theVS_FF_DEBUG flag value in the file's VS_FIXEDFILEINFO
  ///	    block, which is built from the VERSIONINFO resource in a Win32
  ///	    resource file. For more information about specifying version
  ///	    resources in a Win32 resource file, see "About Resource Files" and
  ///	    "VERSIONINFO Resource" in the Platform SDK.
  ///	  </para>
  ///	</remarks>
{$IFDEF MSWINDOWS}
  TFileVersionInfo = record
  private
    type
      TLangAndCodePage = record
        Language: Word;
        CodePage: Word;
      end;

      TLangAndCodePageArray  = array[0..9] of TLangAndCodePage;
      PTLangAndCodePageArray = ^TLangAndCodePageArray;

      TFileVersionResource = record
      private
        fBlock: Pointer;
        fLanguage: Word;
        fCodePage: Word;
      public
        constructor Create(block: Pointer; language, codePage: Word);
        function ReadString(const stringName: string): string;
        property Language: Word read fLanguage;
        property CodePage: Word read fCodePage;
      end;
  strict private
    fExists: Boolean;
    fFileFlags: DWORD;
    fComments: string;
    fCompanyName: string;
    fFileName: string;
    fFileVersion: string;
    fFileVersionNumber: TVersion;
    fFileDescription: string;
    fProductName: string;
    fProductVersion: string;
    fProductVersionNumber: TVersion;
    fInternalName: string;
    fLanguage: string;
    fLegalCopyright: string;
    fLegalTrademarks: string;
    fOriginalFilename: string;
    fPrivateBuild: string;
    fSpecialBuild: string;
    function GetIsDebug: Boolean;
    function GetIsPatched: Boolean;
    function GetIsPreRelease: Boolean;
    function GetIsPrivateBuild: Boolean;
    function GetIsSpecialBuild: Boolean;
  private
    constructor Create(const fileName: string);
    procedure LoadVersionResource(const resource: TFileVersionResource);
  public
    ///	<summary>
    ///	  Gets the file version info of the specified file.
    ///	</summary>
    ///	<exception cref="Spring|EFileNotFoundException">
    ///	  Raised if the file doesn't exist.
    ///	</exception>
    class function GetVersionInfo(const fileName: string): TFileVersionInfo; static;
    function ToString: string;
    property Exists: Boolean read fExists;
    property Comments: string read fComments;
    property CompanyName: string read fCompanyName;
    property FileName: string read fFileName;
    property FileDescription: string read fFileDescription;
    property FileVersion: string read fFileVersion;
    property FileVersionNumber: TVersion read fFileVersionNumber;
    property InternalName: string read fInternalName;
    property Language: string read fLanguage;
    property LegalCopyright: string read fLegalCopyright;
    property LegalTrademarks: string read fLegalTrademarks;
    property OriginalFilename: string read fOriginalFilename;
    property ProductName: string read fProductName;
    property ProductVersion: string read fProductVersion;
    property ProductVersionNumber: TVersion read fProductVersionNumber;
    property PrivateBuild: string read fPrivateBuild;
    property SpecialBuild: string read fSpecialBuild;
    property IsDebug: Boolean read GetIsDebug;
    property IsPatched: Boolean read GetIsPatched;
    property IsPreRelease: Boolean read GetIsPreRelease;
    property IsSpecialBuild: Boolean read GetIsSpecialBuild;
    property IsPrivateBuild: Boolean read GetIsPrivateBuild;
  end;
{$ENDIF MSWINDOWS}

  {$ENDREGION}


  {$REGION 'TOperatingSystem'}

{$IFDEF MSWINDOWS}
  TOSPlatformType = (
    ptUnknown,
    ptWin3x,
    ptWin9x,
    ptWinNT
  );

  TOSVersionType = (
    vtUnknown,
    vtWin95,            // DEPRECATED
    vtWin98,            // DEPRECATED
    vtWinME,            // DEPRECATED
    vtWinNT351,         // DEPRECATED
    vtWinNT4,           // DEPRECATED
    vtWinServer2000,
    vtWinXP,
    vtWinServer2003,
    vtWinVista,
    vtWinServer2008,
    vtWin7,
    vtWinServer2008R2,
    vtWin8,
    vtWinServer2012,
    vtWin81,
    vtWinServer2012R2
  );

  TOSProductType = (
    ptInvalid,
    ptWorkstation,
    ptServer,
    ptDomainController
  );

  TOSSuiteType = (
    etUnknown,
    etWorkStation,
    etServer,
    etAdvancedServer,
    etPersonal,
    etProfessional,
    etDatacenterServer,
    etEnterprise,
    etWebEdition
  );

  ///	<summary>
  ///	  Represents information about the operating system.
  ///	</summary>
  TOperatingSystem = class sealed
  strict private
    fPlatformType: TOSPlatformType;
    fProductType: TOSProductType;
    fServicePack: string;
    fVersion: TVersion;
    fVersionType: TOSVersionType;
    function GetIsWin3x: Boolean;
    function GetIsWin9x: Boolean;
    function GetIsWinNT: Boolean;
    function GetVersionString: string;
  private
    function GetOSVersionType(platformType: TOSPlatformType; productType: TOSProductType;
      majorVersion, minorVersion: Integer): TOSVersionType;
  public
    constructor Create;
    function ToString: string; override;
    property IsWin3x: Boolean read GetIsWin3x;
    property IsWin9x: Boolean read GetIsWin9x;
    property IsWinNT: Boolean read GetIsWinNT;
    property PlatformType: TOSPlatformType read fPlatformType;
    property ProductType: TOSProductType read fProductType;
    property ServicePack: string read fServicePack;
    property Version: TVersion read fVersion;
    property VersionString: string read GetVersionString;
    property VersionType: TOSVersionType read fVersionType;
  end;
{$ENDIF MSWINDOWS}

  {$ENDREGION}


  {$REGION 'Special Folder Enumeration'}

  ///	<summary>
  ///	  Specifies enumerated constants used to retrieve directory paths to
  ///	  system special folders.
  ///	</summary>
  ///	<remarks>
  ///	  <para>
  ///	    The system special folders are folders such as
  ///	    <b>Program
  ///	          Files</b>, <b>Programs</b>, <b>System</b>, or <b>Startup</b>,
  ///	    which contain common information. Special folders are set by default
  ///	    by the system, or explicitly by the user, when installing a version
  ///	    of Windows.
  ///	  </para>
  ///	  <para>
  ///	    The
  ///	    <see cref="TEnvironment.GetFolderPath(TSpecialFolder)">GetFolderPath</see>
  ///	    method returns the locations associated with this enumeration. The
  ///	    locations of these folders can have different values on different
  ///	    operating systems, the user can change some of the locations, and the
  ///	    locations are localized.
  ///	  </para>
  ///	  <para>
  ///	    For more information about special folders, see the
  ///	    <see href="http://go.microsoft.com/fwlink/?LinkId=116664">CSIDL</see>v
  ///	    alues topic.
  ///	  </para>
  ///	</remarks>
{$IFDEF MSWINDOWS}
  TSpecialFolder = (
    sfDesktop,                // <desktop>
    sfInternet,               // Internet Explorer (icon on desktop)
    sfPrograms,               // Start Menu\Programs
    sfControls,               // My Computer\Control Panel
    sfPrinters,               // My Computer\Printers
    sfPersonal,               // My Documents
    sfFavorites,              // <user name>\Favorites
    sfStartup,                // Start Menu\Programs\Startup
    sfRecent,                 // <user name>\Recent
    sfSendTo,                 // <user name>\SendTo
    sfBitBucket,              // <desktop>\Recycle Bin
    sfStartMenu,              // <user name>\Start Menu
    { For Windows >= XP }
    sfMyDocuments,            // logical "My Documents" desktop icon
    sfMyMusic,                // "My Music" folder
    { For Windows >= XP }
    sfMyVideo,                // "My Videos" folder
    sfDesktopDirectory,       // <user name>\Desktop
    sfDrives,                 // My Computer
    sfNetwork,                // Network Neighborhood (My Network Places)
    sfNethood,                // <user name>\nethood
    sfFonts,                  // windows\fonts
    sfTemplates,              // <user name>\Templates
    sfCommonStartMenu,        // All Users\Start Menu
    sfCommonPrograms,         // All Users\Start Menu\Programs
    sfCommonStartup,          // All Users\Startup
    sfCommonDesktopDirectory, // All Users\Desktop
    sfAppData,                // <user name>\Application Data
    sfPrinthood,              // <user name>\PrintHood
    sfLocalAppData,           // <user name>\Local Settings\Applicaiton Data (non roaming)
    sfALTStartup,             // non localized startup
    sfCommonALTStartup,       // non localized common startup
    sfCommonFavorites,        // All Users\Favorites
    sfInternetCache,          // <user name>\Local Settings\Temporary Internet Files
    sfCookies,                // <user name>\Cookies
    sfHistory,                // <user name>\Local Settings\History
    sfCommonAppData,          // All Users\Application Data
    sfWindows,                // GetWindowsDirectory()
    sfSystem,                 // GetSystemDirectory()
    sfProgramFiles,           // C:\Program Files
    sfMyPictures,             // C:\Program Files\My Pictures
    sfProfile,                // USERPROFILE
    sfSystemX86,              // x86 system directory on RISC
    sfProgramFilesX86,        // x86 C:\Program Files on RISC
    sfProgramFilesCommon,     // C:\Program Files\Common
    sfProgramFilesCommonX86,  // x86 Program Files\Common on RISC
    sfCommonTemplates,        // All Users\Templates
    sfCommonDocuments,        // All Users\Documents
    sfCommonAdminTools,       // All Users\Start Menu\Programs\Administrative Tools
    sfAdminTools,             // <user name>\Start Menu\Programs\Administrative Tools
    sfConnections,            // Network and Dial-up Connections
    { For Windows >= XP }
    sfCommonMusic,            // All Users\My Music
    { For Windows >= XP }
    sfCommonPictures,         // All Users\My Pictures
    { For Windows >= XP }
    sfCommonVideo,            // All Users\My Video
    sfResources,              // Resource Direcotry
    sfResourcesLocalized,     // Localized Resource Direcotry
    sfCommonOEMLinks,         // Links to All Users OEM specific apps
    { For Windows >= XP }
    sfCDBurnArea,             // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
    sfComputersNearMe         // Computers Near Me (computered from Workgroup membership)
  );
{$ENDIF MSWINDOWS}

  {$ENDREGION}


  {$REGION 'TEnvironment'}

{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Specifies the location where an environment variable is stored or
  ///	  retrieved in a set or get operation.
  ///	</summary>
  TEnvironmentVariableTarget = (
    ///	<summary>
    ///	  The environment variable is stored or retrieved from the environment
    ///	  block associated with the current process.
    ///	</summary>
    evtProcess,

    ///	<summary>
    ///	  The environment variable is stored or retrieved from the
    ///	  HKEY_CURRENT_USER\Environment key in the Windows operating system
    ///	  registry.
    ///	</summary>
    evtUser,

    ///	<summary>
    ///	  The environment variable is stored or retrieved from the
    ///	  HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\Session
    ///	  Manager\Environment key in the Windows operating system registry.
    ///	</summary>
    evtMachine
  );
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Identifies the processor and bits-per-word of the platform targeted by
  ///	  an executable.
  ///	</summary>
  TProcessorArchitecture = (
    ///	<summary>
    ///	  Unknown processor
    ///	</summary>
    paUnknown,

    ///	<summary>
    ///	  Intel x86 and compatible microprocessors.
    ///	</summary>
    paX86,

    ///	<summary>
    ///	  64-bit Intel and compatible microprocessors.
    ///	</summary>
    paIA64,

    ///	<summary>
    ///	  64-bit AMD microprocessors.
    ///	</summary>
    paAmd64
  );
{$ENDIF MSWINDOWS}

  ///	<summary>
  ///	  Provides information about, and means to manipulate, the current
  ///	  environment.
  ///	</summary>
  ///	<remarks>
  ///	  Use the TEnvironment structure to retrieve information such as
  ///	  command-line arguments, environment variable settings.
  ///	</remarks>
  TEnvironment = record
  strict private
    class var
{$IFDEF MSWINDOWS}
      fOperatingSystem: TOperatingSystem;
{$ENDIF MSWINDOWS}
      fApplicationPath: string;
{$IFDEF MSWINDOWS}
      fApplicationVersionInfo: TFileVersionInfo;
      fApplicationVersion: TVersion;
      fApplicationVersionString: string;
{$ENDIF MSWINDOWS}
      class constructor Create;
{$IFDEF MSWINDOWS}
      {$HINTS OFF}
      class destructor Destroy;
      {$HINTS ON}
{$ENDIF MSWINDOWS}
  strict private
    class function GetCurrentDirectory: string; static;
{$IFDEF MSWINDOWS}
    class function GetMachineName: string; static;
    class function GetIsAdmin: Boolean; static;
    class function GetUserDomainName: string; static;
    class function GetUserName: string; static;{TODO -o##jwp -cMACOS : Resolve using getpwuid. }
    class function GetTickCount: Cardinal; static;
{$ENDIF MSWINDOWS}
    class function GetNewLine: string; static;
{$IFDEF MSWINDOWS}
    class function GetUserInteractive: Boolean; static;
    class function GetCommandLine: string; static; {TODO -o##jwp -cEnhance : Implement this in a cross platform way. }
    class function GetSystemDirectory: string; static;
    class function GetProcessorCount: Integer; static;
    class function GetProcessorArchitecture: TProcessorArchitecture; static;
    class function GetRegisteredOrganization: string; static;
    class function GetRegisteredOwner: string; static;
{$ENDIF MSWINDOWS}
    class procedure SetCurrentDirectory(const value: string); static;
  strict private
{$IFDEF MSWINDOWS}
    class procedure OpenEnvironmentVariableKey(registry: TRegistry;
      target: TEnvironmentVariableTarget; keyAccess: Cardinal); static;
{$ENDIF MSWINDOWS}
{$IFDEF MSWINDOWS}
    class function GetCurrentVersionKey: string; static;
{$ENDIF MSWINDOWS}
    class procedure GetProcessEnvironmentVariables(list: TStrings); static;
  public
    ///	<summary>
    ///	  Returns a string array containing the command-line arguments for the
    ///	  current process.
    ///	</summary>
    class function  GetCommandLineArgs: TStringDynArray; overload; static;

    // TODO: Consider using Extract*** insteading of Get*** for the methods with a
    // TString parameter.

    ///	<summary>
    ///	  Returns a string array containing the command-line arguments for the
    ///	  current process.
    ///	</summary>
    class procedure GetCommandLineArgs(list: TStrings); overload; static;

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Returns an array of string containing the names of the logical drives
    ///	  on the current computer.
    ///	</summary>
    class function  GetLogicalDrives: TStringDynArray; overload; static;

    class procedure GetLogicalDrives(list: TStrings); overload; static;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Gets the path to the system special folder that is identified by the
    ///	  specified enumeration.
    ///	</summary>
    class function  GetFolderPath(const folder: TSpecialFolder): string; static;
{$ENDIF MSWINDOWS}

    ///	<summary>
    ///	  Retrieves the value of an environment variable from the current
    ///	  process.
    ///	</summary>
    class function  GetEnvironmentVariable(const variable: string): string; overload; static;

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Retrieves the value of an environment variable from the current
    ///	  process or from the Windows operating system registry key for the
    ///	  current user or local machine.
    ///	</summary>
    class function  GetEnvironmentVariable(const variable: string; target: TEnvironmentVariableTarget): string; overload; static;
{$ENDIF MSWINDOWS}

    ///	<summary>
    ///	  Retrieves all environment variable names and their values from the
    ///	  current process.
    ///	</summary>
    class procedure GetEnvironmentVariables(list: TStrings); overload; static;

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Retrieves the value of an environment variable from the current
    ///	  process or from the Windows operating system registry key for the
    ///	  current user or local machine.
    ///	</summary>
    class procedure GetEnvironmentVariables(list: TStrings; target: TEnvironmentVariableTarget); overload; static;
{$ENDIF MSWINDOWS}

    ///	<summary>
    ///	  Creates, modifies, or deletes an environment variable stored in the
    ///	  current process.
    ///	</summary>
    class procedure SetEnvironmentVariable(const variable, value: string); overload; static;

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Creates, modifies, or deletes an environment variable stored in the
    ///	  current process or in the Windows operating system registry key
    ///	  reserved for the current user or local machine.
    ///	</summary>
    class procedure SetEnvironmentVariable(const variable, value: string; target: TEnvironmentVariableTarget); overload; static;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Replaces the name of each environment variable embedded in the
    ///	  specified string with the string equivalent of the value of the
    ///	  variable, then returns the resulting string.
    ///	</summary>
    class function ExpandEnvironmentVariables(const variable: string): string; static; {TODO -o##jwp -cEnhance : Implement this in a cross platform way. }
{$ENDIF MSWINDOWS}

    class property ApplicationPath: string read fApplicationPath;

{$IFDEF MSWINDOWS}
    class property ApplicationVersion: TVersion read fApplicationVersion;

    class property ApplicationVersionInfo: TFileVersionInfo read fApplicationVersionInfo;

    class property ApplicationVersionString: string read fApplicationVersionString;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Gets the command line for this process.
    ///	</summary>
    class property CommandLine: string read GetCommandLine;
{$ENDIF MSWINDOWS}

    ///	<summary>
    ///	  Gets or sets the fully qualified path of the current working
    ///	  directory.
    ///	</summary>
    class property CurrentDirectory: string read GetCurrentDirectory write SetCurrentDirectory;

{$IFDEF MSWINDOWS}
    class property IsAdmin: Boolean read GetIsAdmin; { experimental }
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Gets the NetBIOS name of this local computer.
    ///	</summary>
    class property MachineName: string read GetMachineName;
{$ENDIF MSWINDOWS}

    ///	<summary>
    ///	  Gets the newline string defined for this environment.
    ///	</summary>
    class property NewLine: string read GetNewLine;

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Gets a <see cref="TOperatingSystem" /> object that contains the
    ///	  current platform identifier and version number.
    ///	</summary>
    class property OperatingSystem: TOperatingSystem read fOperatingSystem;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
    ///	<summary>
    ///	  Gets the number of processors on the current machine.
    ///	</summary>
    class property ProcessorCount: Integer read GetProcessorCount;

    class property ProcessorArchitecture: TProcessorArchitecture read GetProcessorArchitecture;

    class property RegisteredOrganization: string read GetRegisteredOrganization;

    class property RegisteredOwner: string read GetRegisteredOwner;

    ///	<summary>
    ///	  Gets the fully qualified path of the system directory.
    ///	</summary>
    class property SystemDirectory: string read GetSystemDirectory;

    ///	<summary>
    ///	  Gets the number of milliseconds elapsed since the system started.
    ///	</summary>
    class property TickCount: Cardinal read GetTickCount;

    ///	<summary>
    ///	  Gets the network domain name associated with the current user.
    ///	</summary>
    class property UserDomainName: string read GetUserDomainName;

    ///	<summary>
    ///	  Gets the user name of the person who is currently logged on to the
    ///	  Windows operating system.
    ///	</summary>
    class property UserName: string read GetUserName;

    ///	<summary>
    ///	  Gets a value indicating whether the current process is running in
    ///	  user interactive mode.
    ///	</summary>
    class property UserInteractive: Boolean read GetUserInteractive;
{$ENDIF MSWINDOWS}
  end;

  ///	<summary>
  ///	  Represents a type alias of TEnvironment class.
  ///	</summary>
  Environment = TEnvironment;

  {$ENDREGION}


  {$REGION 'TStringMatchers'}

  ///	<summary>
  ///	  Provides static methods to create various string predicates.
  ///	</summary>
  TStringMatchers = class
  public
    class function ContainsText(const s: string): TPredicate<string>;
    class function StartsText(const s: string): TPredicate<string>;
    class function EndsText(const s: string): TPredicate<string>;
    class function SameText(const s: string): TPredicate<string>;
    class function InStrings(const strings: TStrings): TPredicate<string>;
    class function InArray(const collection: array of string): TPredicate<string>;
    class function InCollection(const collection: IEnumerable<string>): TPredicate<string>; overload;
  end;

  {$ENDREGION}


  {$REGION 'Callback'}

  ///	<summary>
  ///	  Defines an anonymous function which returns a callback pointer.
  ///	</summary>
  TCallbackFunc = TFunc<Pointer>;

  ///	<summary>
  ///	  Adapts class instance (object) method as standard callback function.
  ///	</summary>
  ///	<remarks>
  ///	  Both the object method and the callback function need to be declared as
  ///	  stdcall.
  ///	</remarks>
  ///	<example>
  ///	  This sample shows how to call CreateCallback method.
  ///	  <code>
  ///	private
  ///	  fCallback: TCallbackFunc;
  ///	//...
  ///	fCallback := CreateCallback(Self, @TSomeClass.SomeMethod);</code>
  ///	</example>
  TCallback = class(TInterfacedObject, TCallbackFunc)
  private
    fInstance: Pointer;
  public
    constructor Create(objectAddress: TObject; methodAddress: Pointer);
    destructor Destroy; override;
    function Invoke: Pointer;
  end; // Consider hide the implementation.

  {$ENDREGION}

  ///	<summary>
  ///	  Provides an abstract class base of TThread that implements the
  ///	  IInterface.
  ///	</summary>
  TInterfacedThread = class(TThread, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


  EIOException                  = SysUtils.EInOutError;
  EFileNotFoundException        = SysUtils.EFileNotFoundException;
  EDirectoryNotFoundException   = SysUtils.EDirectoryNotFoundException;
  EDriveNotFoundException       = class(EIOException);


  {$REGION 'Routines'}

  ///	<summary>
  ///	  Returns the path of the application.
  ///	</summary>
  function ApplicationPath: string;

{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Returns the version number of the application.
  ///	</summary>
  function ApplicationVersion: TVersion;

  ///	<summary>
  ///	  Returns the version information of the application.
  ///	</summary>
  function ApplicationVersionString: string;
{$ENDIF MSWINDOWS}

  ///	<summary>
  ///	  Returns the last system error message.
  ///	</summary>
  function GetLastErrorMessage: string;

  ///	<summary>
  ///	  Creates a standard callback function which was adapted from a instance
  ///	  method.
  ///	</summary>
  ///	<param name="obj">
  ///	  an instance
  ///	</param>
  ///	<param name="methodAddress">
  ///	  address of an instance method
  ///	</param>
  function CreateCallback(obj: TObject; methodAddress: Pointer): TCallbackFunc;

{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Converts a Delphi TDatetime value to a Windows TFiletime value.
  ///	</summary>
  function ConvertDateTimeToFileTime(const datetime: TDateTime; const useLocalTimeZone: Boolean): TFileTime; overload;

  ///	<summary>
  ///	  Converts a Windows TFiletime value to a Delphi TDatetime value.  Not implemented on Mac as the Delphi POSIX version is time_t based, but only contains a date portion.
  ///	</summary>
  function ConvertFileTimeToDateTime(const fileTime: TFileTime; const useLocalTimeZone: Boolean): TDateTime; overload;
{$ENDIF MSWINDOWS}

  ///	<summary>
  ///	  Executes a method call within the main thread.
  ///	</summary>
  ///	<param name="threadProc">
  ///	  An anonymous method that will be executed.
  ///	</param>
  ///	<exception cref="EArgumentNullException">
  ///	  Raised if <paramref name="threadProc" /> was not assigned.
  ///	</exception>
  procedure Synchronize(threadProc: TThreadProcedure);

  ///	<summary>
  ///	  Asynchronously executes a method call within the main thread.
  ///	</summary>
  ///	<param name="threadProc">
  ///	  An anonymous method that will be executed.
  ///	</param>
  ///	<exception cref="EArgumentNullException">
  ///	  Raised if threadProc was not assigned.
  ///	</exception>
  procedure Queue(threadProc: TThreadProcedure);

  ///	<summary>
  ///	  Try getting property information of an object.
  ///	</summary>
  ///	<returns>
  ///	  Returns true if the instance has the specified property and the
  ///	  property has property information.
  ///	</returns>
  ///	<exception cref="EArgumentNullException">
  ///	  if instance is nil.
  ///	</exception>
  function TryGetPropInfo(instance: TObject; const propertyName: string;
    out propInfo: PPropInfo): Boolean;

  // >>>NOTE<<<
  // Due to the QC #80304, the following methods (with anonymous methods)
  // must not be inlined.

  ///	<summary>
  ///	  Obtains a mutual-exclusion lock for the given object, executes a
  ///	  procedure and then releases the lock.
  ///	</summary>
  ///	<param name="obj">
  ///	  the sync root.
  ///	</param>
  ///	<param name="proc">
  ///	  the procedure that will be invoked.
  ///	</param>
  ///	<exception cref="Spring|EArgumentNullException">
  ///	  Raised if <paramref name="obj" /> is nil or <paramref name="proc" /> is
  ///	  unassigned.
  ///	</exception>
  procedure Lock(obj: TObject; const proc: TProc); overload; // inline;
  procedure Lock(const intf: IInterface; const proc: TProc); overload; // inline;

  ///	<summary>
  ///	  Updates an instance of <see cref="Classes|TStrings" /> by calling its
  ///	  BeginUpdate and EndUpdate.
  ///	</summary>
  ///	<param name="strings">
  ///	  an instance of TStrings.
  ///	</param>
  ///	<exception cref="EArgumentNullException">
  ///	  Raised if <paramref name="strings" /> is nil or <paramref name="proc" />
  ///	  is not assigned.
  ///	</exception>
  procedure UpdateStrings(strings: TStrings; proc: TProc); // inline;

{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Returns True if the Control key is pressed, otherwise false.
  ///	</summary>
  function IsCtrlPressed: Boolean;

  ///	<summary>
  ///	  Returns True if the Shift key is pressed, otherwise false.
  ///	</summary>
  function IsShiftPressed: Boolean;

  ///	<summary>
  ///	  Returns True if the Alt key is pressed, otherwise false.
  ///	</summary>
  function IsAltPressed: Boolean;
{$ENDIF MSWINDOWS}

  {$REGION 'XML Documentation'}
  {$ENDREGION}

  ///	<summary>
  ///	  Determines whether a specified file exists. An
  ///	  <see cref="EFileNotFoundException" /> exception will be raised when not
  ///	  found.
  ///	</summary>
  ///	<param name="fileName">
  ///	  the file name.
  ///	</param>
  ///	<exception cref="EFileNotFoundException">
  ///	  Raised if the target file does not exist.
  ///	</exception>
  ///	<seealso cref="CheckDirectoryExists(string)" />
  procedure CheckFileExists(const fileName: string);

  ///	<summary>
  ///	  Determines whether a specified directory exists. An
  ///	  <see cref="EDirectoryNotFoundException" /> exception will be raised
  ///	  when not found.
  ///	</summary>
  ///	<exception cref="EDirectoryNotFoundException">
  ///	  Raised if the directory doesn't exist.
  ///	</exception>
  ///	<seealso cref="CheckFileExists(string)" />
  procedure CheckDirectoryExists(const directory: string);


  {$ENDREGION}


  {$REGION 'Constants'}

const
  ///	<summary>
  ///	  Represents bytes of one KB.
  ///	</summary>
  COneKB: Int64 = 1024;            // 1KB = 1024 bytes


  ///	<summary>
  ///	  Represents bytes of one MB.
  ///	</summary>
  COneMB: Int64 = 1048576;         // 1MB = 1024 KB

  ///	<summary>
  ///	  Represents bytes of one GB.
  ///	</summary>
  COneGB: Int64 = 1073741824;      // 1GB = 1024 MB

  ///	<summary>
  ///	  Represents bytes of one TB.
  ///	</summary>
  COneTB: Int64 = 1099511627776;   // 1TB = 1024 GB

  ///	<summary>
  ///	  Represents bytes of one KB.
  ///	</summary>
  OneKB: Int64 = 1024 deprecated 'Use COneKB instead.';

  ///	<summary>
  ///	  Represents bytes of one MB.
  ///	</summary>
  OneMB: Int64 = 1048576 deprecated 'Use COneMB instead.';

  ///	<summary>
  ///	  Represents bytes of one GB.
  ///	</summary>
  OneGB: Int64 = 1073741824 deprecated 'Use COneGB instead.';

  ///	<summary>
  ///	  Represents bytes of one TB.
  ///	</summary>
  OneTB: Int64 = 1099511627776 deprecated 'Use COneTB instead.';

{$IFDEF MSWINDOWS}
  const
    SpecialFolderCSIDLs: array[TSpecialFolder] of Integer = (
      CSIDL_DESKTOP,                  // <desktop>
      CSIDL_INTERNET,                 // Internet Explorer (icon on desktop)
      CSIDL_PROGRAMS,                 // Start Menu\Programs
      CSIDL_CONTROLS,                 // My Computer\Control Panel
      CSIDL_PRINTERS,                 // My Computer\Printers
      CSIDL_PERSONAL,                 // My Documents.  This is equivalent to CSIDL_MYDOCUMENTS in XP and above
      CSIDL_FAVORITES,                // <user name>\Favorites
      CSIDL_STARTUP,                  // Start Menu\Programs\Startup
      CSIDL_RECENT,                   // <user name>\Recent
      CSIDL_SENDTO,                   // <user name>\SendTo
      CSIDL_BITBUCKET,                // <desktop>\Recycle Bin
      CSIDL_STARTMENU,                // <user name>\Start Menu
      CSIDL_MYDOCUMENTS,              // logical "My Documents" desktop icon
      CSIDL_MYMUSIC,                  // "My Music" folder
      CSIDL_MYVIDEO,                  // "My Video" folder
      CSIDL_DESKTOPDIRECTORY,         // <user name>\Desktop
      CSIDL_DRIVES,                   // My Computer
      CSIDL_NETWORK,                  // Network Neighborhood (My Network Places)
      CSIDL_NETHOOD,                  // <user name>\nethood
      CSIDL_FONTS,                    // windows\fonts
      CSIDL_TEMPLATES,
      CSIDL_COMMON_STARTMENU,         // All Users\Start Menu
      CSIDL_COMMON_PROGRAMS,          // All Users\Start Menu\Programs
      CSIDL_COMMON_STARTUP,           // All Users\Startup
      CSIDL_COMMON_DESKTOPDIRECTORY,  // All Users\Desktop
      CSIDL_APPDATA,                  // <user name>\Application Data
      CSIDL_PRINTHOOD,                // <user name>\PrintHood
      CSIDL_LOCAL_APPDATA,            // <user name>\Local Settings\Application Data (non roaming)
      CSIDL_ALTSTARTUP,               // non localized startup
      CSIDL_COMMON_ALTSTARTUP,        // non localized common startup
      CSIDL_COMMON_FAVORITES,
      CSIDL_INTERNET_CACHE,
      CSIDL_COOKIES,
      CSIDL_HISTORY,
      CSIDL_COMMON_APPDATA,           // All Users\Application Data
      CSIDL_WINDOWS,                  // GetWindowsDirectory()
      CSIDL_SYSTEM,                   // GetSystemDirectory()
      CSIDL_PROGRAM_FILES,            // C:\Program Files
      CSIDL_MYPICTURES,               // C:\Program Files\My Pictures
      CSIDL_PROFILE,                  // USERPROFILE
      CSIDL_SYSTEMX86,                // x86 system directory on RISC
      CSIDL_PROGRAM_FILESX86,         // x86 C:\Program Files on RISC
      CSIDL_PROGRAM_FILES_COMMON,     // C:\Program Files\Common
      CSIDL_PROGRAM_FILES_COMMONX86,  // x86 C:\Program Files\Common on RISC
      CSIDL_COMMON_TEMPLATES,         // All Users\Templates
      CSIDL_COMMON_DOCUMENTS,         // All Users\Documents
      CSIDL_COMMON_ADMINTOOLS,        // All Users\Start Menu\Programs\Administrative Tools
      CSIDL_ADMINTOOLS,               // <user name>\Start Menu\Programs\Administrative Tools
      CSIDL_CONNECTIONS,              // Network and Dial-up Connections
      CSIDL_COMMON_MUSIC,             // All Users\My Music
      CSIDL_COMMON_PICTURES,          // All Users\My Pictures
      CSIDL_COMMON_VIDEO,             // All Users\My Video
      CSIDL_RESOURCES,                // Resource Directory
      CSIDL_RESOURCES_LOCALIZED,      // Localized Resource Directory
      CSIDL_COMMON_OEM_LINKS,         // Links to All Users OEM specific apps
      CSIDL_CDBURN_AREA,              // USERPROFILE\Local Settings\Application Data\Microsoft\CD Burning
      CSIDL_COMPUTERSNEARME           // Computers Near Me (computered from Workgroup membership)
    );
{$ENDIF MSWINDOWS}
  {$ENDREGION}

implementation

uses
{$IFDEF POSIX}
  Posix.Stdlib,
  Posix.Unistd,
  Posix.Dlfcn,
  System.IOUtils,
{$ENDIF POSIX}
  Math,
  Spring.ResourceStrings;

{$IFDEF MSWINDOWS}
const
  OSVersionTypeStrings: array[TOSVersionType] of string = (
    SUnknownOSDescription,
    SWin95Description,
    SWin98Description,
    SWinMEDescription,
    SWinNT351Description,
    SWinNT40Description,
    SWinServer2000Description,
    SWinXPDescription,
    SWinServer2003Description,
    SWinVistaDescription,
    SWinServer2008Description,
    SWin7Description,
    SWinServer2008R2Description,
    SWin8Description,
    SWinServer2012Description,
    SWin81Description,
    SWinServer2012R2Description
  );
{$ENDIF MSWINDOWS}


{$REGION 'Routines'}

function ApplicationPath: string;
begin
  Result := TEnvironment.ApplicationPath;
end;

{$IFDEF MSWINDOWS}
function ApplicationVersion: TVersion;
begin
  Result := TEnvironment.ApplicationVersion;
end;

function ApplicationVersionString: string;
begin
  Result := TEnvironment.ApplicationVersionString;
end;
{$ENDIF MSWINDOWS}

function GetLastErrorMessage: string;
begin
  Result := SysErrorMessage(GetLastError);
end;

function CreateCallback(obj: TObject; methodAddress: Pointer): TCallbackFunc;
begin
  Guard.CheckNotNull(obj, 'obj');
  Guard.CheckNotNull(methodAddress, 'methodAddress');
  Result := TCallback.Create(obj, methodAddress);
end;

{$IFDEF MSWINDOWS}
function ConvertDateTimeToFileTime(const datetime: TDateTime;
  const useLocalTimeZone: Boolean): TFileTime;
var
  systemTime: TSystemTime;
  fileTime: TFileTime;
begin
  Result.dwLowDateTime := 0;
  Result.dwHighDateTime := 0;
  DateTimeToSystemTime(datetime, systemTime);
  if SystemTimeToFileTime(systemTime, fileTime) then
  begin
    if useLocalTimeZone then
    begin
      LocalFileTimeToFileTime(fileTime, Result);
    end
    else
    begin
      Result := fileTime;
    end;
  end;
end;

function ConvertFileTimeToDateTime(const fileTime: TFileTime; const useLocalTimeZone: Boolean): TDateTime;
var
  localFileTime: TFileTime;
  systemTime: TSystemTime;
begin
  if useLocalTimeZone then
  begin
    FileTimeToLocalFileTime(fileTime, localFileTime);
  end
  else
  begin
    localFileTime := fileTime;
  end;
  if FileTimeToSystemTime(localFileTime, systemTime) then
  begin
    Result := SystemTimeToDateTime(systemTime);
  end
  else
  begin
    Result := 0;
  end;
end;
{$ENDIF MSWINDOWS}

procedure Synchronize(threadProc: TThreadProcedure);
begin
  Guard.CheckNotNull(Assigned(threadProc), 'threadProc');
  TThread.Synchronize(TThread.CurrentThread, threadProc);
end;

procedure Queue(threadProc: TThreadProcedure);
begin
  Guard.CheckNotNull(Assigned(threadProc), 'threadProc');
  TThread.Queue(TThread.CurrentThread, threadProc);
end;

function TryGetPropInfo(instance: TObject; const propertyName: string;
  out propInfo: PPropInfo): Boolean;
begin
  Guard.CheckNotNull(instance, 'instance');
  propInfo := GetPropInfo(instance, propertyName);
  Result := propInfo <> nil;
end;

function TryParseDateTime(const s, format: string; out value: TDateTime): Boolean;
begin
  Result := TryConvertStrToDateTime(s, format, value);
end;

function ParseDateTime(const s, format: string): TDateTime;
begin
  Result := ConvertStrToDateTime(s, format);
end;

procedure Lock(obj: TObject; const proc: TProc);
begin
  Guard.CheckNotNull(obj, 'obj');
  Guard.CheckNotNull(Assigned(proc), 'proc');

  System.MonitorEnter(obj);
  try
    proc;
  finally
    System.MonitorExit(obj);
  end;
end;

procedure Lock(const intf: IInterface; const proc: TProc);
var
  obj: TObject;
begin
  Guard.CheckNotNull(intf, 'intf');
  obj := TObject(intf);
  Lock(obj, proc);
end;

procedure UpdateStrings(strings: TStrings; proc: TProc);
begin
  Guard.CheckNotNull(strings, 'strings');
  Guard.CheckNotNull(Assigned(proc), 'proc');

  strings.BeginUpdate;
  try
    strings.Clear;
    proc;
  finally
    strings.EndUpdate;
  end;
end;

{$IFDEF MSWINDOWS}
function IsCtrlPressed: Boolean;
begin
  Result := GetKeyState(VK_CONTROL) < 0;
end;

function IsShiftPressed: Boolean;
begin
  Result := GetKeyState(VK_SHIFT) < 0;
end;

/// <remarks>
/// The virtual code of ALT is VK_MENU For history reasons.
/// </remarks>
function IsAltPressed: Boolean;
begin
  Result := GetKeyState(VK_MENU) < 0;
end;
{$ENDIF MSWINDOWS}

procedure CheckFileExists(const fileName: string);
begin
  if not FileExists(fileName) then
  begin
    raise EFileNotFoundException.CreateResFmt(@SFileNotFoundException, [fileName]);
  end;
end;

procedure CheckDirectoryExists(const directory: string);
begin
  if not DirectoryExists(directory) then
  begin
    raise EDirectoryNotFoundException.CreateResFmt(@SDirectoryNotFoundException, [directory]);
  end;
end;

{$ENDREGION}


{$REGION 'TVersion'}

constructor TVersion.Create(const versionString: string);
var
  components: TStringDynArray;
  major: Integer;
  minor: Integer;
  build: Integer;
  reversion: Integer;
begin
  components := SplitString(versionString, ['.']);
  if not (Length(components) in [2..4]) then
  begin
    raise EArgumentException.Create('version');
  end;
  try
    major := StrToInt(components[0]);
    minor := StrToInt(components[1]);
    if Length(components) >= 3 then
    begin
      build := StrToInt(components[2]);
    end
    else
    begin
      build := -1;
    end;
    if Length(components) = 4 then
    begin
      reversion := StrToInt(components[3]);
    end
    else
    begin
      reversion := -1;
    end;
  except on e: Exception do
    raise EFormatException.Create(e.Message);
  end;
  InternalCreate(Length(components), major, minor, build, reversion);
end;

constructor TVersion.Create(major, minor: Integer);
begin
  InternalCreate(2, major, minor, -1, -1);
end;

constructor TVersion.Create(major, minor, build: Integer);
begin
  InternalCreate(3, major, minor, build, -1);
end;

constructor TVersion.Create(major, minor, build, reversion: Integer);
begin
  InternalCreate(4, major, minor, build, reversion);
end;

constructor TVersion.InternalCreate(defined, major, minor, build, reversion: Integer);
begin
  Assert(defined in [2, 3, 4], '"defined" should be in [2, 3, 4].');
  Guard.CheckRange(IsDefined(major), 'major');
  Guard.CheckRange(IsDefined(minor), 'minor');
  fMajor := major;
  fMinor := minor;
  case defined of
    2:
    begin
      fBuild := fCUndefined;
      fReversion := fCUndefined;
    end;
    3:
    begin
      Guard.CheckRange(IsDefined(build), 'build');
      fBuild := build;
      fReversion := fCUndefined;
    end;
    4:
    begin
      Guard.CheckRange(IsDefined(build), 'build');
      Guard.CheckRange(IsDefined(reversion), 'reversion');
      fBuild := build;
      fReversion := reversion;
    end;
  end;
end;

function TVersion.IsDefined(const component: Integer): Boolean;
begin
  Result := component <> fCUndefined;
end;

function TVersion.Equals(const version: TVersion): Boolean;
begin
  Result := CompareTo(version) = 0;
end;

function TVersion.CompareComponent(a, b: Integer): Integer;
begin
  if IsDefined(a) and IsDefined(b) then
  begin
    Result := a - b;
  end
  else if IsDefined(a) and not IsDefined(b) then
  begin
    Result := 1;
  end
  else if not IsDefined(a) and IsDefined(b) then
  begin
    Result := -1;
  end
  else
  begin
    Result := 0;
  end;
end;

function TVersion.CompareTo(const version: TVersion): Integer;
begin
  Result := Major - version.Major;
  if Result = 0 then
  begin
    Result := Minor - version.Minor;
    if Result = 0 then
    begin
      Result := CompareComponent(Build, version.Build);
      if Result = 0 then
      begin
        Result := CompareComponent(Reversion, version.Reversion);
      end;
    end;
  end;
end;

function TVersion.ToString: string;
begin
  if not IsDefined(fBuild) then
    Result := ToString(2)
  else if not IsDefined(fReversion) then
    Result := ToString(3)
  else
    Result := ToString(4);
end;

function TVersion.ToString(fieldCount: Integer): string;
begin
  Guard.CheckRange(fieldCount in [0..4], 'fieldCount');
  case fieldCount of
    0: Result := '';
    1: Result := Format('%d', [major]);
    2: Result := Format('%d.%d', [major, minor]);
    3:
    begin
      Guard.CheckTrue(IsDefined(build), SIllegalFieldCount);
      Result := Format('%d.%d.%d', [major, minor, build]);
    end;
    4:
    begin
      Guard.CheckTrue(IsDefined(build) and IsDefined(reversion), SIllegalFieldCount);
      Result := Format('%d.%d.%d.%d', [major, minor, build, reversion]);
    end;
  end;
end;

function TVersion.GetMajorReversion: Int16;
begin
  Result := Reversion shr 16;
end;

function TVersion.GetMinorReversion: Int16;
begin
  Result := Reversion and $0000FFFF;
end;

class operator TVersion.Equal(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) = 0;
end;

class operator TVersion.NotEqual(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) <> 0;
end;

class operator TVersion.GreaterThan(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) > 0;
end;

class operator TVersion.GreaterThanOrEqual(const left,
  right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) >= 0;
end;

class operator TVersion.LessThan(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) < 0;
end;

class operator TVersion.LessThanOrEqual(const left, right: TVersion): Boolean;
begin
  Result := left.CompareTo(right) <= 0;
end;

{$ENDREGION}


{$REGION 'TFileVersionInfo'}

{$IFDEF MSWINDOWS}
constructor TFileVersionInfo.Create(const fileName: string);
var
  block: Pointer;
  fixedFileInfo: PVSFixedFileInfo;
  translations: PTLangAndCodePageArray;
  size: DWORD;
  valueSize: DWORD;
  translationSize: Cardinal;
  translationCount: Integer;
  dummy: DWORD;
begin
  Finalize(Self);
  ZeroMemory(@Self, SizeOf(Self));
  fFileName := fileName;
  CheckFileExists(fFileName);
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  UniqueString(fFileName);
  size := GetFileVersionInfoSize(PChar(fFileName), dummy);
  fExists := size <> 0;
  if fExists then
  begin
    block := AllocMem(size);
    try
      Win32Check(Windows.GetFileVersionInfo(
        PChar(fFileName),
        0,
        size,
        block
      ));
      Win32Check(VerQueryValue(
        block,
        '\',
        Pointer(fixedFileInfo),
        valueSize
      ));
      Win32Check(VerQueryValue(
        block,
        '\VarFileInfo\Translation',
        Pointer(translations),
        translationSize
      ));
      fFileVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwFileVersionMS),
        LoWord(fixedFileInfo.dwFileVersionMS),
        HiWord(fixedFileInfo.dwFileVersionLS),
        LoWord(fixedFileInfo.dwFileVersionLS)
      );
      fProductVersionNumber := TVersion.Create(
        HiWord(fixedFileInfo.dwProductVersionMS),
        LoWord(fixedFileInfo.dwProductVersionMS),
        HiWord(fixedFileInfo.dwProductVersionLS),
        LoWord(fixedFileInfo.dwProductVersionLS)
      );
      fFileFlags := fixedFileInfo.dwFileFlags;
      translationCount := translationSize div SizeOf(TLangAndCodePage);
      if translationCount > 0 then
      begin
        LoadVersionResource(
          TFileVersionResource.Create(
            block,
            translations[0].Language,
            translations[0].CodePage
          )
        );
      end;
    finally
      FreeMem(block);
    end;
  end;
end;

class function TFileVersionInfo.GetVersionInfo(
  const fileName: string): TFileVersionInfo;
var
  localFileName: string;
begin
  localFileName := Environment.ExpandEnvironmentVariables(fileName);
  Result := TFileVersionInfo.Create(localFileName);
end;

procedure TFileVersionInfo.LoadVersionResource(const resource: TFileVersionResource);
begin
  fCompanyName := resource.ReadString('CompanyName');
  fFileDescription := resource.ReadString('FileDescription');
  fFileVersion := resource.ReadString('FileVersion');
  fInternalName := resource.ReadString('InternalName');
  fLegalCopyright := resource.ReadString('LegalCopyright');
  fLegalTrademarks := resource.ReadString('LegalTrademarks');
  fOriginalFilename := resource.ReadString('OriginalFilename');
  fProductName := resource.ReadString('ProductName');
  fProductVersion := resource.ReadString('ProductVersion');
  fComments := resource.ReadString('Comments');
  fLanguage := Languages.NameFromLocaleID[resource.Language];
end;

function TFileVersionInfo.ToString: string;
begin
  Result := Format(SFileVersionInfoFormat, [
    FileName,
    InternalName,
    OriginalFilename,
    FileVersion,
    FileDescription,
    ProductName,
    ProductVersion,
    BoolToStr(IsDebug, True),
    BoolToStr(IsPatched, True),
    BoolToStr(IsPreRelease, True),
    BoolToStr(IsPrivateBuild, True),
    BoolToStr(IsSpecialBuild, True),
    Language
  ]);
end;

function TFileVersionInfo.GetIsDebug: Boolean;
begin
  Result := (fFileFlags and VS_FF_DEBUG) <> 0;
end;

function TFileVersionInfo.GetIsPatched: Boolean;
begin
  Result := (fFileFlags and VS_FF_PATCHED) <> 0;
end;

function TFileVersionInfo.GetIsPreRelease: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRERELEASE) <> 0;
end;

function TFileVersionInfo.GetIsPrivateBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_PRIVATEBUILD) <> 0;
end;

function TFileVersionInfo.GetIsSpecialBuild: Boolean;
begin
  Result := (fFileFlags and VS_FF_SPECIALBUILD) <> 0;
end;

{ TFileVersionInfo.TFileVersionData }

constructor TFileVersionInfo.TFileVersionResource.Create(block: Pointer;
  language, codePage: Word);
begin
  fBlock := block;
  fLanguage := language;
  fCodePage := codePage;
end;

function TFileVersionInfo.TFileVersionResource.ReadString(
  const stringName: string): string;
var
  subBlock: string;
  data: PChar;
  len: Cardinal;
const
  SubBlockFormat = '\StringFileInfo\%4.4x%4.4x\%s';   // do not localize
begin
  subBlock := Format(
    SubBlockFormat,
    [fLanguage, fCodePage, stringName]
  );
  data := nil;
  len := 0;
  VerQueryValue(fBlock, PChar(subBlock), Pointer(data), len);
  Result := data;
end;
{$ENDIF MSWINDOWS}

{$ENDREGION}


{$REGION 'TOperatingSystem'}

{$IFDEF MSWINDOWS}
constructor TOperatingSystem.Create;
var
  versionInfo: TOSVersionInfoEx;
begin
  inherited Create;
  ZeroMemory(@versionInfo, SizeOf(versionInfo));
  versionInfo.dwOSVersionInfoSize := SizeOf(versionInfo);
  Win32Check(Windows.GetVersionEx(versionInfo));
  case versionInfo.dwPlatformId of
    VER_PLATFORM_WIN32s:        fPlatformType := ptWin3x;
    VER_PLATFORM_WIN32_WINDOWS: fPlatformType := ptWin9x;
    VER_PLATFORM_WIN32_NT:      fPlatformType := ptWinNT;
    else fPlatformType := ptUnknown;
  end;
  fProductType := ptInvalid;
  case versionInfo.wProductType of
    VER_NT_WORKSTATION:       fProductType := ptWorkstation;
    VER_NT_DOMAIN_CONTROLLER: fProductType := ptDomainController;
    VER_NT_SERVER:            fProductType := ptServer;
  end;
  fVersion := TVersion.Create(
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion,
    versionInfo.dwBuildNumber
  );
  fVersionType := GetOSVersionType(
    fPlatformType,
    fProductType,
    versionInfo.dwMajorVersion,
    versionInfo.dwMinorVersion
  );
  fServicePack := versionInfo.szCSDVersion;
end;

function TOperatingSystem.GetOSVersionType(platformType: TOSPlatformType;
  productType: TOSProductType; majorVersion, minorVersion: Integer): TOSVersionType;
begin
  Result := vtUnknown;
  case platformType of
    ptWin9x:
    begin
      if majorVersion = 4 then
      case minorVersion of
        0:  Result := vtWin95;
        10: Result := vtWin98;
        90: Result := vtWinMe;
      end;
    end;
    ptWinNT:
    begin
      if (majorVersion = 3) and (minorVersion = 51) then
      begin
        Result := vtWinNT351;
      end
      else if (majorVersion = 4) and (minorVersion = 0) then
      begin
        Result := vtWinNT4;
      end
      else if majorVersion = 5 then
      case minorVersion of
        0: Result := vtWinServer2000;
        1: Result := vtWinXP;
        2: Result := vtWinServer2003;
      end
      else if majorVersion = 6 then
      case minorVersion of
        0:
        begin
          if productType = ptWorkstation then
            Result := vtWinVista
          else
            Result := vtWinServer2008;
        end;
        1:
        begin
          if productType = ptWorkstation then
            Result := vtWin7
          else
            Result := vtWinServer2008R2;
        end;
        2:
        begin
          if productType = ptWorkstation then
            Result := vtWin8
          else
            Result := vtWinServer2012;
        end;
        3:
        begin
          if productType = ptWorkstation then
            Result := vtWin81
          else
            Result := vtWinServer2012R2;
        end;
      end;
    end;
  end;
end;

function TOperatingSystem.ToString: string;
begin
  Result := OSVersionTypeStrings[fVersionType];
  if fVersionType <> vtUnknown then
  begin
    Result := Result + ' Version ' + fVersion.ToString;
    if ServicePack <> '' then
      Result := Result + ' ' + ServicePack;
  end;
end;

function TOperatingSystem.GetIsWin3x: Boolean;
begin
  Result := Self.PlatformType = ptWin3x;
end;

function TOperatingSystem.GetIsWin9x: Boolean;
begin
  Result := Self.PlatformType = ptWin9x;
end;

function TOperatingSystem.GetIsWinNT: Boolean;
begin
  Result := Self.PlatformType = ptWinNT;
end;

function TOperatingSystem.GetVersionString: string;
begin
  Result := ToString;
end;
{$ENDIF MSWINDOWS}

{$ENDREGION}


{$REGION 'TEnvironment'}

class constructor TEnvironment.Create;
begin
  fApplicationPath := ExtractFilePath(ParamStr(0));
{$IFDEF MSWINDOWS}
  fApplicationVersionInfo := TFileVersionInfo.GetVersionInfo(ParamStr(0));
  fApplicationVersion := fApplicationVersionInfo.FileVersionNumber;
  fApplicationVersionString := fApplicationVersionInfo.FileVersion;
  fOperatingSystem := TOperatingSystem.Create;
{$ENDIF MSWINDOWS}
end;

{$IFDEF MSWINDOWS}
class destructor TEnvironment.Destroy;
begin
  fOperatingSystem.Free;
end;
{$ENDIF MSWINDOWS}

class function TEnvironment.GetCommandLineArgs: TStringDynArray;
var
  i: Integer;
  count: Integer;
{$IFDEF MSWINDOWS}
  pArgs: PPWideChar;
begin
  pArgs := ShellAPI.CommandLineToArgvW(PWideChar(Windows.GetCommandLineW), count);
  if pArgs <> nil then
  try
    SetLength(Result, count);
    for i := 0 to count - 1 do
    begin
      Result[i] := string(pArgs^);
      Inc(pArgs);
    end;
  finally
    Windows.LocalFree(HLocal(pArgs));
  end;
{$ELSE}
begin
  count := ParamCount;
  SetLength(Result, count+1);
  for i := 0 to count do
    Result[i] := ParamStr(i);
{$ENDIF MSWINDOWS}
end;

class procedure TEnvironment.GetCommandLineArgs(list: TStrings);
var
  args: TStringDynArray;
begin
  args := GetCommandLineArgs;
  UpdateStrings(list,
    procedure
    var
      i: Integer;
    begin
      for i := 0 to High(args) do
      begin
        list.Add(args[i]);
      end;
    end
  );
end;

{$IFDEF MSWINDOWS}
class function TEnvironment.GetLogicalDrives: TStringDynArray;
var
  len: Cardinal;
  buffer: string;
begin
  len := Windows.GetLogicalDriveStrings(0, nil);
  SetLength(buffer, len);
  Windows.GetLogicalDriveStrings(len * SizeOf(Char), PChar(buffer));
  Result := SplitString(PChar(buffer));
end;

class procedure TEnvironment.GetLogicalDrives(list: TStrings);
var
  drives: TStringDynArray;
begin
  drives := TEnvironment.GetLogicalDrives;
  UpdateStrings(list,
    procedure
    var
      drive: string;
    begin
      for drive in drives do
      begin
        list.Add(drive);
      end;
    end
  );
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
function TryGetAccessToken(out hToken: THandle): Boolean;
begin
  Result := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not Result and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    Result := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetFolderPath(const folder: TSpecialFolder): string;
var
  pidl : PItemIDList;
  buffer: array[0..MAX_PATH-1] of Char;
//  returnCode: HRESULT;
  hToken : THandle;
begin
  if TryGetAccessToken(hToken) then
  try
    ShlObj.SHGetFolderLocation(INVALID_HANDLE_VALUE,
      SpecialFolderCSIDLs[folder], hToken, 0, pidl);
    ShlObj.SHGetPathFromIDList(pidl, @buffer[0]);
    Result := buffer;
  finally
    CloseHandle(hToken);
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class procedure TEnvironment.OpenEnvironmentVariableKey(registry: TRegistry;
  target: TEnvironmentVariableTarget; keyAccess: Cardinal);
var
  key: string;
begin
  Assert(registry <> nil, 'registry should not be nil.');
  Assert(target in [evtUser, evtMachine], Format('Illegal target: %d.', [Integer(target)]));
  if target = evtUser then
  begin
    registry.RootKey := HKEY_CURRENT_USER;
    key := 'Environment';
  end
  else
  begin
    registry.RootKey := HKEY_LOCAL_MACHINE;
    key := 'System\CurrentControlSet\Control\Session Manager\Environment';
  end;
  registry.Access := keyAccess;
  if not registry.OpenKey(key, False) then
  begin
    raise EOSError.CreateResFmt(@SCannotAccessRegistryKey, [key]);
  end;
end;
{$ENDIF MSWINDOWS}

class function TEnvironment.GetEnvironmentVariable(
  const variable: string): string;
{$IFDEF MSWINDOWS}
begin
  Result := TEnvironment.GetEnvironmentVariable(variable, evtProcess);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  variablePointer: Pointer;
  resultPointer: Pointer;
{$IFDEF DELPHIXE2}
  variableAnsi: AnsiString;
{$ELSE DELPHIXE2}
  M: TMarshaller;
{$ENDIF DELPHIXE2}
begin
{$IFDEF DELPHIXE2}
  variableAnsi := AnsiString(variable);
  variablePointer := PAnsiChar(variableAnsi);
{$ELSE DELPHIXE2}
  variablePointer := M.AsUtf8(variable).ToPointer;
{$ENDIF DELPHIXE2}

  resultPointer := getenv(variablePointer);

  // See SetEnvironmentVariable for comment about encoding
  // We cannot defer to SysUtils implementation since it doesn't support regional characters as well (QC123698)
{$IFDEF DELPHIXE2}
  Result := string(AnsiString(PAnsiChar(resultPointer)));
{$ELSE DELPHIXE2}
  Result := UTF8ToString(resultPointer);
{$ENDIF DELPHIXE2}
end;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetEnvironmentVariable(const variable: string;
  target: TEnvironmentVariableTarget): string;
var
  registry: TRegistry;

  function GetProcessEnvironmentVariable: string;
  var
    len: DWORD;
  begin
    len := Windows.GetEnvironmentVariable(PChar(variable), nil, 0);
    if len > 0 then
    begin
      SetLength(Result, len - 1);
      Windows.GetEnvironmentVariable(PChar(variable), PChar(Result), len);
    end
    else
    begin
      Result := '';
    end;
  end;
begin
  Guard.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Result := GetProcessEnvironmentVariable;
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_READ);
    if registry.ValueExists(variable) then
    begin
      Result := registry.GetDataAsString(variable);
    end
    else
    begin
      Result := '';
    end;
  finally
    registry.Free;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF DELPHIXE2}
{$IFDEF POSIX}
type
  _PPAnsiChr    = PPAnsiChar;
  PMarshaledAString = _PPAnsiChr; {$NODEFINE PMarshaledAString}
{$ENDIF POSIX}
{$ENDIF DELPHIXE2}

class procedure TEnvironment.GetProcessEnvironmentVariables(list: TStrings);
var
{$IFDEF MSWINDOWS}
  p: PChar;
  strings: TStringDynArray;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  pEnviron: PMarshaledAString;
  current: string;
{$ENDIF POSIX}
begin
  Assert(list <> nil, 'list should not be nil.');
{$IFDEF MSWINDOWS}
  p := Windows.GetEnvironmentStrings;
  try
    strings := SplitString(p);
    UpdateStrings(list,
      procedure
      var
        s: string;
      begin
        for s in strings do
        begin
          if (Length(s) > 0) and (s[1] <> '=') then // Skip entries start with '='
          begin
            list.Add(s);
          end;
        end;
      end
    );
  finally
    Win32Check(Windows.FreeEnvironmentStrings(p));
  end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  pEnviron := environ; {TODO -o##jwp -cTest : Test this code; it is based on http://stackoverflow.com/questions/2085302/printing-all-environment-variables-in-c-c/12059006#12059006 }
  if pEnviron <> nil then
  begin
    list.BeginUpdate();
    try
      while Assigned(pEnviron) do
      begin
        current := string(pEnviron^);
        list.Add(current);
      end;
    finally
      list.EndUpdate();
    end;
  end;
{$ENDIF POSIX}
end;

class procedure TEnvironment.GetEnvironmentVariables(list: TStrings);
begin
{$IFDEF MSWINDOWS}
  TEnvironment.GetEnvironmentVariables(list, evtProcess);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  TEnvironment.GetProcessEnvironmentVariables(list);
{$ENDIF POSIX}
end;

{$IFDEF MSWINDOWS}
class procedure TEnvironment.GetEnvironmentVariables(list: TStrings;
  target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
  value: string;
  i: Integer;
begin
  Guard.CheckNotNull(list, 'list');
  Guard.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    GetProcessEnvironmentVariables(list);
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_READ);
    registry.GetValueNames(list);
    for i := 0 to list.Count - 1 do
    begin
      value := registry.GetDataAsString(list[i]);
      list[i] := list[i] + list.NameValueSeparator + value;
    end;
  finally
    registry.Free;
  end;
end;
{$ENDIF MSWINDOWS}

class procedure TEnvironment.SetEnvironmentVariable(const variable, value: string);
{$IFDEF POSIX}
var
{$IFDEF DELPHIXE2}
  variableAnsiString: RawByteString;
  valueAnsiString: RawByteString;
{$ELSE}
  M1, M2: TMarshaller;
{$ENDIF DELPHIXE2}
  variablePointer: Pointer;
  valuePointer: Pointer;
{$ENDIF POSIX}
begin
{$IFDEF MSWINDOWS}
  TEnvironment.SetEnvironmentVariable(variable, value, evtProcess);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IFDEF DELPHIXE2}
  // first convert from Unicode to Ansi using DefaultSystemCodePage, then get the pointer to the AnsiString's chars, XE2 is using ANSI
  variableAnsiString := AnsiString(variable);
  valueAnsiString := AnsiString(value);
  variablePointer := PAnsiChar(variableAnsiString);
  valuePointer := PAnsiChar(valueAnsiString);
{$ELSE}
  // As obsered from other libraries (FireDAC, DBXInterbase) the actual encoding isn't ANSI but UTF8 in XE3+
  variablePointer := M1.AsUtf8(variable).ToPointer;
  valuePointer := M2.AsUtf8(value).ToPointer;
{$ENDIF DELPHIXE2}
  setenv(variablePointer, valuePointer, 1);
{$ENDIF POSIX}
end;

{$IFDEF MSWINDOWS}
class procedure TEnvironment.SetEnvironmentVariable(const variable,
  value: string; target: TEnvironmentVariableTarget);
var
  registry: TRegistry;
begin
  Guard.CheckEnum<TEnvironmentVariableTarget>(target, 'target');
  if target = evtProcess then
  begin
    Win32Check(Windows.SetEnvironmentVariable(PChar(variable), PChar(value)));
    Exit;
  end;
  registry := TRegistry.Create;
  try
    OpenEnvironmentVariableKey(registry, target, KEY_WRITE);
    if Pos('%', value) > 0 then
    begin
      registry.WriteExpandString(variable, value);
    end
    else
    begin
      registry.WriteString(variable, value);
    end;
    SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, Integer(PChar('Environment')));
  finally
    registry.Free;
  end;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class function TEnvironment.ExpandEnvironmentVariables(
  const variable: string): string;
var
  len: Cardinal;
begin
  len := MAX_PATH;
  SetLength(Result, len);
  len := Windows.ExpandEnvironmentStrings(PChar(variable), PChar(Result), len);
  Win32Check(len > 0);
  SetLength(Result, len - 1);
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetCommandLine: string;
begin
  Result := Windows.GetCommandLine;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetCurrentDirectory: string;
var
  size: DWORD;
begin
  size := Windows.GetCurrentDirectory(0, nil);
  SetLength(Result, size - 1);
  Windows.GetCurrentDirectory(size, PChar(Result));
end;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
  {$IFDEF DELPHIXE2}
    {$INLINE OFF} // Otherwise in Delphi XE2 only on OSX32: [DCC Fatal Error] F2084 Internal Error: URW1147
  {$ENDIF DELPHIXE2}
class function TEnvironment.GetCurrentDirectory: string;
begin
  Result := TDirectory.GetCurrentDirectory(); // Delphi XE2 only on OSX32: [DCC Fatal Error] F2084 Internal Error: URW1147
end;
  {$IFDEF DELPHIXE2}
    {$INLINE ON} // Presume the default ON (QC123694: as there is no $IFOPT to check for OFF/AUTO)
  {$ENDIF DELPHIXE2}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetCurrentVersionKey: string;
const
  HKLM_CURRENT_VERSION_NT      = 'SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  HKLM_CURRENT_VERSION_WINDOWS = 'SOFTWARE\Microsoft\Windows\CurrentVersion';
begin
  if OperatingSystem.IsWinNT then
    Result := HKLM_CURRENT_VERSION_NT
  else
    Result := HKLM_CURRENT_VERSION_WINDOWS;
end;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
class function TEnvironment.GetMachineName: string;
var
  size: Cardinal;
begin
  size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, size);
  if GetComputerName(PChar(Result), size) then
  begin
    SetLength(Result, size);
  end;
end; {TODO -o##jwp -cMACOS : Replace with gethostname call. }
{$ENDIF MSWINDOWS}

class function TEnvironment.GetNewLine: string;
begin
  Result := System.sLineBreak;
end;

{$IFDEF MSWINDOWS}
class function TEnvironment.GetProcessorArchitecture: TProcessorArchitecture;
var
  systemInfo: TSystemInfo;
const
  PROCESSOR_ARCHITECTURE_INTEL          = 0;
  PROCESSOR_ARCHITECTURE_AMD64          = 9;
  PROCESSOR_ARCHITECTURE_IA32_ON_WIN64  = 10;
  PROCESSOR_ARCHITECTURE_IA64           = 6;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  case systemInfo.wProcessorArchitecture of
    PROCESSOR_ARCHITECTURE_INTEL:
      Result := paX86;
    PROCESSOR_ARCHITECTURE_IA64:
      Result := paIA64;
    PROCESSOR_ARCHITECTURE_AMD64:
      Result := paAmd64;
    else
      Result := paUnknown;
  end;
end;

class function TEnvironment.GetProcessorCount: Integer;
var
  systemInfo: TSystemInfo;
begin
  ZeroMemory(@systemInfo, Sizeof(systemInfo));
  Windows.GetSystemInfo(systemInfo);
  Result := systemInfo.dwNumberOfProcessors;
end;

class function TEnvironment.GetRegisteredOrganization: string;
begin
  {$IFDEF HAS_UNITSCOPE}
  Result := System.Win.ComObj.GetRegStringValue(
  {$ELSE}
  Result :=ComObj.GetRegStringValue(
  {$ENDIF}
    GetCurrentVersionKey,
    'RegisteredOrganization',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetRegisteredOwner: string;
begin
  {$IFDEF HAS_UNITSCOPE}
  Result := System.Win.ComObj.GetRegStringValue(
  {$ELSE}
  Result :=ComObj.GetRegStringValue(
  {$ENDIF}
    GetCurrentVersionKey,
    'RegisteredOwner',  // DO NOT LOCALIZE
    HKEY_LOCAL_MACHINE
  );
end;

class function TEnvironment.GetSystemDirectory: string;
begin
  Result := TEnvironment.GetFolderPath(sfSystem);
end;

class function TEnvironment.GetUserDomainName: string;
var
  hasToken: Boolean;
  hToken: THandle;
  ptiUser: PSIDAndAttributes;
  cbti: DWORD;
  snu: SID_NAME_USE;
  userSize, domainSize: Cardinal;
  userName: string;
begin
  ptiUser := nil;
  userSize := 0;
  domainSize := 0;
  hasToken := Windows.OpenThreadToken(GetCurrentThread, TOKEN_QUERY, TRUE, hToken);
  if not hasToken and (Windows.GetLastError = ERROR_NO_TOKEN) then
  begin
    hasToken := Windows.OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hToken);
  end;
  if hasToken then
  try
    Windows.GetTokenInformation(hToken, TokenUser, nil, 0, cbti);
    ptiUser := AllocMem(cbti);
    if Windows.GetTokenInformation(hToken, TokenUser, ptiUser, cbti, cbti) then
    begin
      if not Windows.LookupAccountSid(nil, ptiUser.Sid, nil, userSize, nil, domainSize, snu) and
        (Windows.GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        SetLength(userName, userSize - 1);
        SetLength(Result, domainSize - 1);
        Win32Check(Windows.LookupAccountSid(nil, ptiUser.Sid, PChar(userName), userSize,
          PChar(Result), domainSize, snu));
      end;
    end;
  finally
    Windows.CloseHandle(hToken);
    FreeMem(ptiUser);
  end;
end;

class function TEnvironment.GetUserInteractive: Boolean;
begin
  { TODO: UserInteractive }
  Result := True;
end;

class function TEnvironment.GetUserName: string;
var
  size: Cardinal;
begin
  size := 255;
  SetLength(Result, size);
  Win32Check(Windows.GetUserName(PChar(Result), size));
  SetLength(Result, size - 1);
end;

/// http://www.gumpi.com/Blog/2007/10/02/EKON11PromisedEntry3.aspx
/// <author>Daniel Wischnewski</author>
class function TEnvironment.GetIsAdmin: Boolean;
const
  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS = $00000220;
  SE_GROUP_ENABLED = $00000004;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: DWORD;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
begin
  Result   := False;
  bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True, hAccessToken);
  if not bSuccess then
    if GetLastError = ERROR_NO_TOKEN then
      bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY, hAccessToken);
  if bSuccess then
  begin
    GetTokenInformation(hAccessToken, TokenGroups, nil, 0, dwInfoBufferSize);
    ptgGroups := GetMemory(dwInfoBufferSize);
    bSuccess := GetTokenInformation(hAccessToken, TokenGroups, ptgGroups, dwInfoBufferSize, dwInfoBufferSize);
    CloseHandle(hAccessToken);
    if bSuccess then
    begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, psidAdministrators);
      for x := 0 to ptgGroups.GroupCount - 1 do
      begin
        if (SE_GROUP_ENABLED = (ptgGroups.Groups[x].Attributes and SE_GROUP_ENABLED)) and EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
        begin
          Result := True;
          Break;
        end;
      end;
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
end;

class function TEnvironment.GetTickCount: Cardinal;
begin
  Result := Windows.GetTickCount;
end;
{$ENDIF MSWINDOWS}

class procedure TEnvironment.SetCurrentDirectory(const value: string);
begin
{$IFDEF MSWINDOWS}
  Win32Check(Windows.SetCurrentDirectory(PChar(value)));
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  SysUtils.SetCurrentDir(value);
{$ENDIF POSIX}
end;

{$ENDREGION}


{$REGION 'TCallback'}

type
  PInstruction = ^TInstruction;
  TInstruction = array[1..16] of Byte;

{----------------------------}
{        Code DASM           }
{----------------------------}
{  push  [ESP]               }
{  mov   [ESP+4], ObjectAddr }
{  jmp   MethodAddr          }
{----------------------------}

/// <author>
/// savetime
/// </author>
/// <seealso>http://savetime.delphibbs.com</seealso>
constructor TCallback.Create(objectAddress: TObject; methodAddress: Pointer);
const
  Instruction: TInstruction = (
    $FF,$34,$24,$C7,$44,$24,$04,$00,$00,$00,$00,$E9,$00,$00,$00,$00
  );
var
  p: PInstruction;
begin
  inherited Create;
  New(p);
  Move(Instruction, p^, SizeOf(Instruction));
  PInteger(@p[8])^ := Integer(objectAddress);
  PInteger(@p[13])^ := Longint(methodAddress) - (Longint(p) + SizeOf(Instruction));
  fInstance := p;
end;

destructor TCallback.Destroy;
begin
  Dispose(fInstance);
  inherited Destroy;
end;

function TCallback.Invoke: Pointer;
begin
  Result := fInstance;
end;

{$ENDREGION}


{$REGION 'TInterfacedThread'}

function TInterfacedThread.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedThread._AddRef: Integer;
begin
  Result := -1;
end;

function TInterfacedThread._Release: Integer;
begin
  Result := -1;
end;

{$ENDREGION}


{$REGION 'TStringMatchers'}

class function TStringMatchers.ContainsText(const s: string): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := StrUtils.ContainsText(value, s);
    end;
end;

class function TStringMatchers.StartsText(const s: string): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := StrUtils.StartsText(s, value);
    end;
end;

class function TStringMatchers.EndsText(const s: string): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := StrUtils.EndsText(s, value);
    end;
end;

class function TStringMatchers.SameText(const s: string): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := SysUtils.SameText(s, value);
    end;
end;

class function TStringMatchers.InArray(
  const collection: array of string): TPredicate<string>;
var
  localArray: TArray<string>;
  i: Integer;
begin
  SetLength(localArray, Length(collection));
  for i := 0 to High(collection) do
    localArray[i] := collection[i];

  Result :=
    function (const value: string): Boolean
    var
      s: string;
    begin
      for s in localArray do
      begin
        if SysUtils.SameText(s, value) then
          Exit(True);
      end;
      Result := False;
    end;
end;

class function TStringMatchers.InStrings(
  const strings: TStrings): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := strings.IndexOf(value) > -1;
    end;
end;

class function TStringMatchers.InCollection(
  const collection: IEnumerable<string>): TPredicate<string>;
begin
  Result :=
    function (const value: string): Boolean
    begin
      Result := collection.Contains(value);
    end;
end;

{$ENDREGION}


end.
