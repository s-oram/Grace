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

unit Spring.Utils.IO;

{$I Spring.inc}

interface

uses
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  SysUtils,
  IOUtils,
{$IFDEF MSWINDOWS}
{$IFDEF HAS_UNITSCOPE}
  System.Win.ComObj,
{$ELSE}
  ComObj,
{$ENDIF}
  ActiveX,
  ShellAPI,
{$ENDIF MSWINDOWS}
  Masks,
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Utils;

type
{$IFDEF MSWINDOWS}
  ///	<summary>
  ///	  Drive Type Enumeration
  ///	</summary>
  TDriveType = (
    ///	<summary>
    ///	  The type of drive is unknown.
    ///	</summary>
    dtUnknown,

    ///	<summary>
    ///	  The drive does not have a root directory.
    ///	</summary>
    dtNoRootDirectory,

    ///	<summary>
    ///	  The drive is a removable storage device, such as a floppy disk drive or a
    ///	  USB flash drive.
    ///	</summary>
    dtRemovable,

    ///	<summary>
    ///	  The drive is a fixed disk.
    ///	</summary>
    dtFixed,

    ///	<summary>
    ///	  The drive is a network drive.
    ///	</summary>
    dtNetwork,

    ///	<summary>
    ///	  The drive is an optical disc device, such as a CD or DVD-ROM.
    ///	</summary>
    dtCDRom,

    ///	<summary>
    ///	  The drive is a RAM disk.
    ///	</summary>
    dtRam
  );

  ///	<summary>
  ///	  Provides access to information on a drive.
  ///	</summary>
  ///	<remarks>
  ///	  Use the static <see cref="Spring.Utils.IO|TDriveInfo.GetDrives" />method
  ///	   to retrieve all drives of the computer.
  ///	  <note type="caller">
  ///	    Caller must use the
  ///	    <see cref="Spring.Utils.IO|TDriveInfo.IsReady">IsReady</see> property
  ///	    to check whether the drive is ready before accessing other members.
  ///	    Otherwise, an <see cref="Spring|EIOException" /> exception will be
  ///	    raised if it is not ready.
  ///	  </note>
  ///	</remarks>
  ///	<threadsafety static="true" instance="false" />
  ///	<seealso href="http://msdn.microsoft.com/en-us/library/system.io.driveinfo.aspx">
  ///	  System.IO.DriveInfo (.Net Framework)
  ///	</seealso>
  TDriveInfo = record
  strict private
    fDriveName: string;
    fRootDirectory: string;
    fIsInitialized: Boolean;
    fAvailableFreeSpace: Int64;
    fTotalSize: Int64;
    fTotalFreeSpace: Int64;
    fVolumeName: array[0..MAX_PATH] of Char;
    fFileSystemName: array[0..MAX_PATH] of Char;
    fSerialNumber: DWORD;
    fMaximumComponentLength: DWORD;
    fFileSystemFlags: DWORD;
    function GetAvailableFreeSpace: Int64;
    function GetDriveFormat: string;
    function GetDriveType: TDriveType;
    function GetDriveTypeString: string;
    function GetIsReady: Boolean;
    function GetTotalFreeSpace: Int64;
    function GetTotalSize: Int64;
    function GetVolumeLabel: string;
    procedure SetVolumeLabel(const value: string);
  private
    procedure EnsureInitialized;
  public
    constructor Create(const driveName: string);

    ///	<summary>
    ///	  Retrieves the drive names of all logical drives on a computer.
    ///	</summary>
    class function GetDrives: TArray<TDriveInfo>; static;

    ///	<summary>
    ///	  Checks whether a drive is ready.
    ///	</summary>
    ///	<exception cref="EIOException">
    ///	  Raised if a drive is not ready.
    ///	</exception>
    procedure CheckIsReady;

    ///	<summary>
    ///	  Refreshes the information of a drive.
    ///	</summary>
    procedure Refresh;

    ///	<summary>
    ///	  Gets the amount of available free space on a drive.
    ///	</summary>
    property AvailableFreeSpace: Int64 read GetAvailableFreeSpace;

    ///	<summary>
    ///	  Gets the name of the file system, such as NTFS or FAT32.
    ///	</summary>
    property DriveFormat: string read GetDriveFormat;

    ///	<summary>
    ///	  Gets the drive type.
    ///	</summary>
    property DriveType: TDriveType read GetDriveType;

    ///	<summary>
    ///	  Gets the drive type.
    ///	</summary>
    property DriveTypeString: string read GetDriveTypeString;

    ///	<summary>
    ///	  Gets a value indicating whether a drive is ready.
    ///	</summary>
    property IsReady: Boolean read GetIsReady;

    ///	<summary>
    ///	  Gets the name of a drive.
    ///	</summary>
    property Name: string read fDriveName;

    ///	<summary>
    ///	  Gets the root directory of a drive.
    ///	</summary>
    property RootDirectory: string read fRootDirectory;

    ///	<summary>
    ///	  Gets the total amount of free space available on a drive.
    ///	</summary>
    property TotalFreeSpace: Int64 read GetTotalFreeSpace;

    ///	<summary>
    ///	  Gets the total size of storage space on a drive.
    ///	</summary>
    property TotalSize: Int64 read GetTotalSize;

    ///	<summary>
    ///	  Gets or sets the volume label of a drive.
    ///	</summary>
    property VolumeLabel: string read GetVolumeLabel write SetVolumeLabel;
  end;
{$ENDIF MSWINDOWS}


  {$REGION 'TSizeUnit, TSize'}

  TSizeUnit = record
  strict private
    class var
      fBytes: TSizeUnit;
      fKB: TSizeUnit;
      fMB: TSizeUnit;
      fGB: TSizeUnit;
      fTB: TSizeUnit;
    class constructor Create;
  private
    fName: string;
    fSize: Int64;
  public
    constructor Create(const name: string; const size: Int64);
    class function From(const size: Int64): TSizeUnit; static;
    function FormatSize(const size: Int64): string;
    property Name: string read fName;
    property Size: Int64 read fSize;
    class property Bytes: TSizeUnit read fBytes;
    class property KB: TSizeUnit read fKB;
    class property MB: TSizeUnit read fMB;
    class property GB: TSizeUnit read fGB;
    class property TB: TSizeUnit read fTB;
  end;

  TSize = record
  private
    fValue: Int64;
  public
    constructor Create(const value: Int64);
    function ToString: string; overload;
    function ToString(const sizeUnit: TSizeUnit): string; overload;
    property Value: Int64 read fValue;
    class operator Implicit(const size: TSize): Int64;
    class operator Implicit(const value: Int64): TSize;
    class operator Explicit(const size: TSize): Int64;
    class operator Explicit(const value: Int64): TSize;
    class operator Equal(const left, right: TSize): Boolean;
    class operator NotEqual(const left, right: TSize): Boolean;
    class operator GreaterThan(const left, right: TSize): Boolean;
    class operator GreaterThanOrEqual(const left, right: TSize): Boolean;
    class operator LessThan(const left, right: TSize): Boolean;
    class operator LessThanOrEqual(const left, right: TSize): Boolean;
  end;

//  TFileSize = TSize;
//  TFileSizeUnit = TSizeUnit;

  {$ENDREGION}


{$IFDEF MSWINDOWS}
  {$REGION 'TFileSystemEntry'}

  PFileSystemEntry = ^TFileSystemEntry;

  ///	<summary>
  ///	  Represents a file system entry.
  ///	</summary>
  TFileSystemEntry = record
  private
    type
      TEntryScope = (
        esAllEntries,
        esDirectories,
        esFiles
      );
  private
    fName: string;
    fLocation: string;
    fAttributeFlags: Cardinal;
    fSize: TSize;
    fCreationTime: TFileTime;
    fLastAccessTime: TFileTime;
    fLastWriteTime: TFileTime;
    fExists: Boolean;
    function GetFullName: string;
    function GetIsFile: Boolean; inline;
    function GetIsEmpty: Boolean;
    function GetExtension: string;
    function GetTypeString: string;
    function GetOwner: string;
    function GetCreationTime: TDateTime;
    function GetCreationTimeUtc: TDateTime;
    function GetLastAccessTime: TDateTime;
    function GetLastAccessTimeUtc: TDateTime;
    function GetLastWriteTime: TDateTime;
    function GetLastWriteTimeUtc: TDateTime;
  private
    function GetHasAttribute(attribute: Integer): Boolean; inline;
    function DoGetEntries(scope: TEntryScope; const searchPattern: string;
      includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>;
  public
    constructor Create(const fileName: string); overload;
    constructor Create(const location: string; const data: TWin32FindData); overload;
    constructor Create(const location: string; const searchRec: TSearchRec); overload;

    ///	<summary>
    ///	  Refreshes the state of the entry.
    ///	</summary>
    procedure Refresh;
    function GetDirectories: IEnumerable<TFileSystemEntry>; overload;
    function GetDirectories(const searchPattern: string): IEnumerable<TFileSystemEntry>; overload;
    function GetDirectories(const searchPattern: string; includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>; overload;
    function GetFiles: IEnumerable<TFileSystemEntry>; overload;
    function GetFiles(const searchPattern: string): IEnumerable<TFileSystemEntry>; overload;
    function GetFiles(const searchPattern: string; includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>; overload;
    function GetEntries: IEnumerable<TFileSystemEntry>; overload;
    function GetEntries(const searchPattern: string): IEnumerable<TFileSystemEntry>; overload;
    function GetEntries(const searchPattern: string; includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>; overload;

    ///	<summary>
    ///	  Gets the name of the file or directory.
    ///	</summary>
    property Name: string read fName;

    ///	<summary>
    ///	  Gets the location, which is the full name of the parent directory, of
    ///	  the file or directory.
    ///	</summary>
    property Location: string read fLocation;

    ///	<summary>
    ///	  For files, gets the extension of the file.
    ///	</summary>
    property Extension: string read GetExtension;

    ///	<summary>
    ///	  Gets a value indicating whether the file or directory exists.
    ///	</summary>
    property Exists: Boolean read fExists;

    ///	<summary>
    ///	  Gets the full path of the file or directory.
    ///	</summary>
    property FullName: string read GetFullName;

    ///	<summary>
    ///	  Gets the size, in bytes, of the file.
    ///	</summary>
    property Size: TSize read fSize;
    // property SizeOnDisk: TSize read GetSizeOnDisk;
    property CreationTime: TDateTime read GetCreationTime;
    property CreationTimeUtc: TDateTime read GetCreationTimeUtc;
    property LastAccessTime: TDateTime read GetLastAccessTime;
    property LastAccessTimeUtc: TDateTime read GetLastAccessTimeUtc;
    property LastWriteTime: TDateTime read GetLastWriteTime;
    property LastWriteTimeUtc: TDateTime read GetLastWriteTimeUtc;
    property AttributeFlags: Cardinal read fAttributeFlags;

    ///	<summary>
    ///	  Gets the owner of the entry. Only available in the NTFS file systems.
    ///	</summary>
    property Owner: string read GetOwner;

    ///	<summary>
    ///	  Gets the friendly description of the type of the entry.
    ///	</summary>
    property TypeString: string read GetTypeString;

    ///	<summary>
    ///	  Get a value indicating whether the entry is a directory.
    ///	</summary>
    property IsDirectory: Boolean index faDirectory read GetHasAttribute;

    ///	<summary>
    ///	  Get a value indicating whether the entry is a file.
    ///	</summary>
    property IsFile: Boolean read GetIsFile;

    ///	<summary>
    ///	  For files, Gets a value indicating whether the file is empty. For
    ///	  directories, indicates whether the directory contains any entry.
    ///	</summary>
    property IsEmpty: Boolean read GetIsEmpty;
    property IsReadOnly: Boolean index faReadOnly read GetHasAttribute;
    property IsHidden: Boolean index faHidden read GetHasAttribute;
    property IsSystem: Boolean index faSysFile read GetHasAttribute;
    property IsArchive: Boolean index faArchive read GetHasAttribute;
    property IsNormal: Boolean index faNormal read GetHasAttribute;
    property IsDevice: Boolean index FILE_ATTRIBUTE_DEVICE read GetHasAttribute;
    property IsCompressed: Boolean index FILE_ATTRIBUTE_COMPRESSED read GetHasAttribute;
    property IsEncrypted: Boolean index FILE_ATTRIBUTE_ENCRYPTED read GetHasAttribute;
    property IsTemporary: Boolean index faTemporary read GetHasAttribute;
    property IsOffline: Boolean index FILE_ATTRIBUTE_OFFLINE read GetHasAttribute;

    ///	<summary>
    ///	  Returns the full name of an entry.
    ///	</summary>
    class operator Implicit(const entry: TFileSystemEntry): string;
  end;

  IFileEnumerable = IEnumerable<TFileSystemEntry>;
  IFileEnumerator = IEnumerator<TFileSystemEntry>;

  {$ENDREGION}
{$ENDIF MSWINDOWS}


  {$REGION 'Search Pattern Matcher'}

  ///	<summary>
  ///	  Defines an interface for a search pattern matcher which can determine
  ///	  if a file name matches its search pattern.
  ///	</summary>
  ISearchPatternMatcher = interface
    ['{7DB533A5-C2A3-4084-AFAE-A9960DC85CD2}']
    function GetSearchPattern: string;
    function GetPatternCount: Integer;

    ///	<summary>
    ///	  Returns true if the fileName satisfied the search pattern. Otherwise,
    ///	  returns false.
    ///	</summary>
    function Matches(const fileName: string): Boolean;

    ///	<summary>
    ///	  Gets the original search pattern string.
    ///	</summary>
    property SearchPattern: string read GetSearchPattern;

    ///	<summary>
    ///	  Gets the count of the search pattern string.
    ///	</summary>
    property PatternCount: Integer read GetPatternCount;
  end;

  ///	<summary>
  ///	  Determines if a filename matches a search pattern.
  ///	</summary>
  TSearchPatternMatcher = class(TInterfacedObject, ISearchPatternMatcher)
  private
    type
      TFileNamePredicate = reference to function (const fileName: string): Boolean;
  strict private
    class var fAll: ISearchPatternMatcher;
    class constructor Create;
  private
    fSearchPattern: string;
    fPredicate: TFileNamePredicate;
    fMask: TMask;
    fMasks: IList<TMask>;
    function GetSearchPattern: string;
    function GetPatternCount: Integer;
  protected
    function CreatePredicate(const patterns: TStrings): TFileNamePredicate; overload; virtual;
    function CreatePredicate(const searchPattern: string): TFileNamePredicate; overload; virtual;
  public
    ///	<summary>
    ///	  Initializes a new instance of the TSearchPatternMatcher class.
    ///	</summary>
    ///	<param name="searchPattern">
    ///	  The search pattern string is used to determine whether a file name is
    ///	  satisfied. e.g. '*.txt', '*.ex?', '*.txt;*.doc;*.rtf'
    ///	</param>
    constructor Create(const searchPattern: string);
    destructor Destroy; override;
    function Matches(const fileName: string): Boolean; virtual;
    property SearchPattern: string read GetSearchPattern;
    property PatternCount: Integer read GetPatternCount;

    ///	<summary>
    ///	  Gets the shared instance of the ISearchPatternMatcher interface that
    ///	  matches all file system entries.
    ///	</summary>
    class property All: ISearchPatternMatcher read fAll;
  end;

  {$ENDREGION}


{$IFDEF MSWINDOWS}
  // Spring.TPredicate<TFileSystemEntry>
  TFileSystemEntryPredicate = reference to function (const entry:TFileSystemEntry): Boolean;

  ///	<summary>
  ///	  Inspects a file enumerator.
  ///	</summary>
  IFileEnumeratorInspector = interface
    ['{DAAE3F64-AB92-4B16-A022-F2C3B64A6216}']
    procedure LocationChanged(const location: string);
    function GetIsTerminated: Boolean;
    property IsTerminated: Boolean read GetIsTerminated;
  end;

  ISupportFileEnumeratorInspector = interface
    ['{AEB47B6D-E652-453B-89D4-F24BFA90A10C}']
    procedure Initialize(const inspector: IFileEnumeratorInspector);
  end;

  TFileEnumerable = class(TEnumerableBase<TFileSystemEntry>)
  private
    fPath: string;
    fSearchPattern: string;
    fAttributes: Cardinal;
    fIncludeSubfolders: Boolean;
  protected
    property Path: string read fPath;
    property SearchPattern: string read fSearchPattern;
    property IncludeSubfolders: Boolean read fIncludeSubfolders;
  public
    constructor Create(const path, searchPattern: string; attributes: Cardinal;
      includeSubfolders: Boolean);
    function GetEnumerator: IEnumerator<TFileSystemEntry>; override;
  end;

  TFileEnumeratorBase = class abstract(TEnumeratorBase<TFileSystemEntry>, ISupportFileEnumeratorInspector)
  private
    fInspector: IFileEnumeratorInspector;
    function GetIsTerminated: Boolean;
  protected
    procedure NotifyLocationChanged(const location: string); virtual;
    property Inspector: IFileEnumeratorInspector read fInspector;
    property IsTerminated: Boolean read GetIsTerminated;
  protected
    { ISupportFileEnumeratorInspector }
    procedure Initialize(const inspector: IFileEnumeratorInspector); virtual;
  end;

  TFileEnumerator = class(TFileEnumeratorBase)
  protected
    type
      PSearchContext = ^TSearchContext;
      TSearchContext = record
      private
        const
          fCCurrentDirName: string = '.';
          fCParentDirName: string = '..';
        function GetIsTerminated: Boolean;
      strict private
        fPath: string;
        fFileName: string;
        fAttributes: Cardinal;
        fIsFirstFind: Boolean;
        fSearchHandle: THandle;
        fFindData: TWin32FindData;
        fMatcher: ISearchPatternMatcher;
        fInspector: IFileEnumeratorInspector;
        function Accept(const data: TWin32FindData): Boolean;
        procedure CloseSearchHandle;
        property IsTerminated: Boolean read GetIsTerminated;
      public
        constructor Create(const path, searchPattern: string; attributes: Cardinal); overload;
        constructor Create(const path, fileName: string; const matcher: ISearchPatternMatcher; attributes: Cardinal); overload;
        procedure Close;
        function MoveNext: Boolean;
        property Path: string read fPath;
        property Current: TWin32FindData read fFindData;
        property Inspector: IFileEnumeratorInspector read fInspector write fInspector;
      end;
  private
    fRootPath: string;
    fSearchPatten: string;
    fAttributes: Cardinal;
    fIncludeSubfolders: Boolean;
    fMatcher: ISearchPatternMatcher;
    fStacks: TStack<TSearchContext>;
    fCurrentContext: TSearchContext;
    fCurrentEntry: TFileSystemEntry;
  protected
    procedure FreeContexts;
    procedure DoDirectoryFound(var context: TSearchContext; const entry: TFileSystemEntry); virtual;
    function CreateSearchContext(const path: string): TSearchContext; virtual;
    function Accept(const entry: TFileSystemEntry): Boolean; virtual;
    property Stacks: TStack<TSearchContext> read fStacks;
  protected
    function GetCurrent: TFileSystemEntry; override;
    { ISupportFileEnumeratorInspector }
    procedure Initialize(const inspector: IFileEnumeratorInspector); override;
  public
    constructor Create(const path, searchPattern: string;
      attributes: Cardinal; includeSubfolders: Boolean); overload;
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  ///	<summary>
  ///	  Enumerates the file system entries that come from a string list.
  ///	</summary>
  TFileListEnumerable = class(TEnumerableBase<TFileSystemEntry>, IFileEnumerable)
  protected
    fFiles: TStrings;
  public
    constructor Create(files: TStrings);
    function GetEnumerator: IEnumerator<TFileSystemEntry>; override;
  end;

  TFileListEnumerator = class(TFileEnumeratorBase)
  private
    fFiles: TStrings;
    fIndex: Integer;
    fEntry: TFileSystemEntry;
  protected
    procedure GetFileSystemEntry(const path: string; out entry: TFileSystemEntry); virtual;
    function GetCurrent: TFileSystemEntry; override;
  public
    constructor Create(files: TStrings);
//    constructor Create(files: TStrings; const searchPattern: string;
//      attributes: Cardinal; includeSubfolders: Boolean);
    procedure Reset; override;
    function MoveNext: Boolean; override;
  end;

  ///	<summary>
  ///	  Enumerates the file system entries of dropped files that result from a
  ///	  successful drag-and-drop operation.
  ///	</summary>
  TDroppedFilesEnumerable = class(TEnumerableBase<TFileSystemEntry>, IFileEnumerable)
  protected
    fFiles: TStrings;
  public
    constructor Create(dropHandle: THandle); overload;
    constructor Create(const dataObject: IDataObject); overload;
    destructor Destroy; override;
    function GetEnumerator: IEnumerator<TFileSystemEntry>; override;
  end;

  {$REGION 'TFileSearcher'}

  TFileSearcher = class;
  TFileSearchWorker = class;
  TFileSearchStatistics = class;

  TFileSearchStatus = (
    ssUnknown,
    ssReady,
    ssSearching,
    ssPaused,
    ssStopped
  );

  TFileSearchScope = (
    ssDirectoriesAndFiles,
    ssDirectories,
    ssFiles
  );

  TFileSearchEvent = reference to procedure(sender: TObject);
  TFileSearchFilterEvent = reference to function(sender: TObject; const entry: TFileSystemEntry): Boolean;
  TFileSearchProgressEvent = reference to procedure(sender: TObject; const entry: TFileSystemEntry);
  TFileSearchLocationChangedEvent = reference to procedure(sender: TObject; const location: string);

  IFileSearcherListener = interface
    ['{23812ADA-03DE-421C-BB34-3266EF8BE162}']
    procedure OnSearchBegin(sender: TObject);
    procedure OnSearchEnd(sender: TObject);
    procedure OnProgress(sender: TObject; const entry: TFileSystemEntry);
    procedure OnLocationChanged(sender: TObject; const location: string);
  end;

  ///	<summary>
  ///	  Provides an abstract implementation of file searcher.
  ///	</summary>
  TFileSearcherBase = class abstract
  private
    fStatus: TFileSearchStatus;
    fOnFilter: TFileSearchFilterEvent;
    fOnProgress: TFileSearchProgressEvent;
    fOnSearchBegin: TFileSearchEvent;
    fOnSearchEnd: TFileSearchEvent;
    fOnStatusChanged: TFileSearchEvent;
    fOnLocationChanged: TFileSearchLocationChangedEvent;
    function GetCanStart: Boolean;
    function GetCanStop: Boolean;
    function GetCanPause: Boolean;
    function GetCanResume: Boolean;
    procedure SetOnFilter(const value: TFileSearchFilterEvent);
  protected
    fSync: TObject;
    procedure RaiseOnSearchBegin;
    procedure RaiseOnSearchEnd;
    procedure RaiseOnProgress(const entry: TFileSystemEntry);
    procedure RaiseOnLocationChanged(const location: string);
    procedure RaiseOnStatusChanged;
    procedure ChangeStatus(newStatus: TFileSearchStatus); virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoPause; virtual;
    procedure DoResume; virtual;
    procedure DoWorkerSearchBegin(sender: TObject); virtual;
    procedure DoWorkerSearchEnd(sender: TObject); virtual;
    procedure DoWorkerProgress(sender: TObject; const entry: TFileSystemEntry); virtual;
    procedure DoWorkerLocationChanged(sender: TObject; const location: string); virtual;
    function DoWorkerFilter(sender: TObject; const entry: TFileSystemEntry): Boolean; virtual;
    function CreateWorker: TFileSearchWorker; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    property Status: TFileSearchStatus read fStatus;
    property CanStart: Boolean read GetCanStart;
    property CanStop: Boolean read GetCanStop;
    property CanPause: Boolean read GetCanPause;
    property CanResume: Boolean read GetCanResume;
    property OnSearchBegin: TFileSearchEvent read fOnSearchBegin write fOnSearchBegin;
    property OnSearchEnd: TFileSearchEvent read fOnSearchEnd write fOnSearchEnd;
    property OnFilter: TFileSearchFilterEvent read fOnFilter write SetOnFilter;
    property OnProgress: TFileSearchProgressEvent read fOnProgress write fOnProgress;
    property OnLocationChanged: TFileSearchLocationChangedEvent read fOnLocationChanged write fOnLocationChanged;
    property OnStatusChanged: TFileSearchEvent read fOnStatusChanged write fOnStatusChanged;
  end;

  ///	<summary>
  ///	  Searches the directories, files or file sytem entries in several
  ///	  locations.
  ///	</summary>
  TFileSearcher = class(TFileSearcherBase)
  private
    const
      fCDefaultSleepTime = 10;  // milliseconds
  private
    fLocations: TStrings;
    fFileTypes: TStrings;
    fSearchScope: TFileSearchScope;
    fStatistics: TFileSearchStatistics;
    fWorker: TFileSearchWorker;
    fIncludeSubfolders: Boolean;
    procedure SetLocations(const value: TStrings);
    procedure SetFileTypes(const value: TStrings);
  protected
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoPause; override;
    procedure DoResume; override;
    procedure DoWorkerProgress(sender: TObject; const entry: TFileSystemEntry); override;
    function CreateStatistics: TFileSearchStatistics; virtual;
    function CreateFileEnumerable(const path, searchPattern: string): IFileEnumerable; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    property Locations: TStrings read fLocations write SetLocations;
    property FileTypes: TStrings read fFileTypes write SetFileTypes;
    property SearchScope: TFileSearchScope read fSearchScope write fSearchScope;
    property Statistics: TFileSearchStatistics read fStatistics;
    property IncludeSubfolders: Boolean read fIncludeSubfolders write fIncludeSubfolders;
  end;

  ///	<summary>
  ///	  Records the statistics information of a file searcher.
  ///	</summary>
  TFileSearchStatistics = class
  private
    fStopwatch: TStopwatch;
    fTotalCount: Integer;
    fTotalSize: Int64;
    fFileCount: Integer;
    fFolderCount: Integer;
    function GetElapsed: TTimeSpan;
  protected
    property Stopwatch: TStopwatch read fStopwatch;
  public
    constructor Create;
    procedure Notify(const entry: TFileSystemEntry); virtual;
    procedure Start;
    procedure StartNew;
    procedure Stop;
    procedure Reset; virtual;
    property Elapsed: TTimeSpan read GetElapsed;
    property TotalCount: Integer read fTotalCount write fTotalCount;
    property TotalSize: Int64 read fTotalSize write fTotalSize;
    property FileCount: Integer read fFileCount write fFileCount;
    property FolderCount: Integer read fFolderCount write fFolderCount;
  end;

  ///	<summary>
  ///	  Represents a background file searching thread which walk through all
  ///	  entries that come from an enumerable collection of file system entries.
  ///	</summary>
  ///	<remarks>
  ///	  All events are not synchronized or queued. It is the caller's
  ///	  responsibility to synchronize or queue the events.
  ///	</remarks>
  TFileSearchWorker = class(TInterfacedThread, IFileEnumeratorInspector)
  private
    fCollections: IEnumerable<IFileEnumerable>;
    fOnFilter: TFileSearchFilterEvent;
    fOnSearchBegin: TFileSearchEvent;
    fOnSearchEnd: TFileSearchEvent;
    fOnProgress: TFileSearchProgressEvent;
    fOnLocationChanged: TFileSearchLocationChangedEvent;
  protected
    function Accept(const entry: TFileSystemEntry): Boolean; virtual;
    procedure RaiseOnProgress(const entry: TFileSystemEntry);
    procedure ProcessEntry(const entry: TFileSystemEntry); virtual;
    procedure BeginSearch; virtual;
    procedure EndSearch; virtual;
    procedure Execute; override;
  protected
    { IFileEnumeratorInspector }
    procedure LocationChanged(const location: string);
    function GetIsTerminated: Boolean;
    property IsTerminated: Boolean read GetIsTerminated;
  public
    procedure Initialize(const collections: IEnumerable<IFileEnumerable>); virtual;
    property OnFilter: TFileSearchFilterEvent read fOnFilter write fOnFilter;
    property OnSearchBegin: TFileSearchEvent read fOnSearchBegin write fOnSearchBegin;
    property OnSearchEnd: TFileSearchEvent read fOnSearchEnd write fOnSearchEnd;
    property OnProgress: TFileSearchProgressEvent read fOnProgress write fOnProgress;
    property OnLocationChanged: TFileSearchLocationChangedEvent read fOnLocationChanged write fOnLocationChanged;
  end;

  {$ENDREGION}


  ///	<summary>
  ///	  Returns an enumerable collection of directories in a specified path.
  ///	</summary>
  function EnumerateDirectories(const path: string): IFileEnumerable; overload;
  function EnumerateDirectories(const path, searchPattern: string): IFileEnumerable; overload;
  function EnumerateDirectories(const path, searchPattern: string; includeSubfolders: Boolean): IFileEnumerable; overload;

  ///	<summary>
  ///	  Returns an enumerable collection of files in a specified path.
  ///	</summary>
  function EnumerateFiles(const path: string): IFileEnumerable; overload;
  function EnumerateFiles(const path, searchPattern: string): IFileEnumerable; overload;
  function EnumerateFiles(const path, searchPattern: string; includeSubfolders: Boolean): IFileEnumerable; overload;

  ///	<summary>
  ///	  Returns an enumerable collection of file system entries in a specified
  ///	  path.
  ///	</summary>
  function EnumerateFileSystemEntries(const path: string): IFileEnumerable; overload;
  function EnumerateFileSystemEntries(const path, searchPattern: string): IFileEnumerable; overload;
  function EnumerateFileSystemEntries(const path, searchPattern: string; includeSubfolders: Boolean): IFileEnumerable; overload;

  ///	<summary>
  ///	  GetDroppedFiles
  ///	</summary>
  procedure GetDroppedFiles(const dataObject: IDataObject; list: TStrings); overload;

  ///	<summary>
  ///	  GetDroppedFiles
  ///	</summary>
  procedure GetDroppedFiles(dropHandle: THandle; list: TStrings); overload;
{$ENDIF MSWINDOWS}

implementation

uses
  Spring.ResourceStrings,
{$IFDEF MSWINDOWS}
  Spring.Utils.WinAPI,
{$ENDIF MSWINDOWS}
  Spring.Collections.Extensions;

{$IFDEF MSWINDOWS}
const
  DriveTypeStrings: array[TDriveType] of string = (
    SUnknownDriveDescription,
    SNoRootDirectoryDescription,
    SRemovableDescription,
    SFixedDescription,
    SNetworkDescription,
    SCDRomDescription,
    SRamDescription
  );
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
{$REGION 'Routines'}

function EnumerateDirectories(const path: string): IFileEnumerable;
begin
  Result := EnumerateDirectories(path, '*.*', False);
end;

function EnumerateDirectories(const path, searchPattern: string): IFileEnumerable;
begin
  Result := EnumerateDirectories(path, searchPattern, False);
end;

function EnumerateDirectories(const path, searchPattern: string;
  includeSubfolders: Boolean): IFileEnumerable;
begin
  Result := TFileEnumerable.Create(path, searchPattern, faDirectory, includeSubfolders);
end;

function EnumerateFiles(const path: string): IFileEnumerable;
begin
  Result := EnumerateFiles(path, '*.*', False);
end;

function EnumerateFiles(const path, searchPattern: string): IFileEnumerable;
begin
  Result := EnumerateFiles(path, searchPattern, False);
end;

function EnumerateFiles(const path, searchPattern: string;
  includeSubfolders: Boolean): IFileEnumerable;
begin
  Result := TFileEnumerable.Create(path, searchPattern,
    faAnyFile and not faDirectory, includeSubfolders);
end;

function EnumerateFileSystemEntries(const path: string): IFileEnumerable;
begin
  Result := EnumerateFileSystemEntries(path, '*.*', False);
end;

function EnumerateFileSystemEntries(const path, searchPattern: string): IFileEnumerable;
begin
  Result := EnumerateFileSystemEntries(path, searchPattern, False);
end;

function EnumerateFileSystemEntries(const path, searchPattern: string;
  includeSubfolders: Boolean): IFileEnumerable;
begin
  Result := TFileEnumerable.Create(path, searchPattern, faAnyFile, includeSubfolders);
end;

procedure GetDroppedFiles(const dataObject: IDataObject; list: TStrings); overload;
var
  handle: THandle;
  medium: TStgMedium;
const
  f: tagFORMATETC = (
    cfFormat: CF_HDROP;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: LongInt($FFFFFFFF)
  );
begin
  Guard.CheckNotNull(dataObject, 'dataObject');

  OleCheck(dataObject.GetData(f, medium));
  handle := medium.hGlobal;
  GetDroppedFiles(handle, list);
end;

procedure GetDroppedFiles(dropHandle: THandle; list: TStrings);
var
  count, size, i: Integer;
  fileName: array[0..MAX_PATH] of Char;
const
  f: tagFORMATETC = (
    cfFormat: CF_HDROP;
    ptd: nil;
    dwAspect: DVASPECT_CONTENT;
    lindex: -1;
    tymed: LongInt($FFFFFFFF)
  );
begin
  Guard.CheckNotNull(list, 'list');

  count := DragQueryFile(dropHandle, $FFFFFFFF, nil, 0);
  try
    for i := 0 to count - 1 do
    begin
      size := DragQueryFile(dropHandle, i, nil, 0) + 1;
      DragQueryFile(dropHandle, i, fileName, size);
      list.Add(fileName);
    end;
  finally
    DragFinish(dropHandle);
  end;
end;

{$ENDREGION}
{$ENDIF MSWINDOWS}


{$IFDEF MSWINDOWS}
{$REGION 'TDriveInfo'}

constructor TDriveInfo.Create(const driveName: string);
var
  s: string;
begin
  s := UpperCase(driveName);
  if not (Length(s) in [1..3]) or not CharInSet(s[1], ['A'..'Z']) then
  begin
    raise EArgumentException.Create('driveName');
  end;
  case Length(s) of
    1:
    begin
      fRootDirectory := s + DriveDelim + PathDelim;
    end;
    2:
    begin
      if s[2] <> DriveDelim then
      begin
        raise EArgumentException.Create('driveName');
      end;
      fRootDirectory := s + PathDelim;
    end;
    3:
    begin
      if s[2] <> DriveDelim then
        raise EArgumentException.Create('driveName');
      if s[3] <> PathDelim then
        raise EArgumentException.Create('driveName');
      fRootDirectory := s;
    end;
    else
    begin
      Assert(False);
    end;
  end;
  Assert(Length(fRootDirectory) = 3, 'Length of fRootDirectory should be 3.');
  fDriveName := Copy(fRootDirectory, 1, 2);
  fIsInitialized := False;
end;

class function TDriveInfo.GetDrives: TArray<TDriveInfo>;
var
  drives: TStringDynArray;
  i: Integer;
begin
  drives := Environment.GetLogicalDrives;
  SetLength(Result, Length(drives));
  for i := 0 to High(drives) do
  begin
    Result[i] := TDriveInfo.Create(drives[i]);
  end;
end;

procedure TDriveInfo.CheckIsReady;
begin
  if not IsReady then
  begin
    raise EIOException.CreateResFmt(@SDriveNotReady, [fDriveName]);
  end;
end;

procedure TDriveInfo.EnsureInitialized;
begin
  if not fIsInitialized then
  begin
    fIsInitialized := True;
    CheckIsReady;
    Refresh;
  end;
end;

function TDriveInfo.GetAvailableFreeSpace: Int64;
begin
  EnsureInitialized;
  Result := fAvailableFreeSpace;
end;

function TDriveInfo.GetDriveFormat: string;
begin
  EnsureInitialized;
  Result := fFileSystemName;
end;

function TDriveInfo.GetDriveType: TDriveType;
var
  value: Cardinal;
begin
  value := Windows.GetDriveType(PChar(fRootDirectory));
  case value of
    DRIVE_NO_ROOT_DIR:  Result := dtNoRootDirectory;
    DRIVE_REMOVABLE:    Result := dtRemovable;
    DRIVE_FIXED:        Result := dtFixed;
    DRIVE_REMOTE:       Result := dtNetwork;
    DRIVE_CDROM:        Result := dtCDRom;
    DRIVE_RAMDISK:      Result := dtRam;
    else                Result := dtUnknown;  // DRIVE_UNKNOWN
  end;
end;

function TDriveInfo.GetDriveTypeString: string;
begin
  Result := DriveTypeStrings[Self.DriveType];
end;

function TDriveInfo.GetIsReady: Boolean;
begin
  Result := Length(fRootDirectory) > 0;
  Result := Result and (SysUtils.DiskSize(Ord(fRootDirectory[1]) - $40) > -1);
end;

function TDriveInfo.GetTotalFreeSpace: Int64;
begin
  EnsureInitialized;
  Result := fTotalFreeSpace;
end;

function TDriveInfo.GetTotalSize: Int64;
begin
  EnsureInitialized;
  Result := fTotalSize;
end;

function TDriveInfo.GetVolumeLabel: string;
begin
  EnsureInitialized;
  Result := fVolumeName;
end;

procedure TDriveInfo.Refresh;
begin
  if IsReady then
  begin
    Win32Check(SysUtils.GetDiskFreeSpaceEx(
      PChar(fRootDirectory),
      fAvailableFreeSpace,
      fTotalSize,
      @fTotalFreeSpace
    ));
    Win32Check(Windows.GetVolumeInformation(
      PChar(fRootDirectory),
      fVolumeName,
      Length(fVolumeName),
      @fSerialNumber,
      fMaximumComponentLength,
      fFileSystemFlags,
      fFileSystemName,
      Length(fFileSystemName)
    ));
  end;
end;

procedure TDriveInfo.SetVolumeLabel(const value: string);
begin
  CheckIsReady;
  Win32Check(Windows.SetVolumeLabel(PChar(fRootDirectory), PChar(value)));
end;

{$ENDREGION}
{$ENDIF MSWINDOWS}


{$REGION 'TSizeUnit'}

constructor TSizeUnit.Create(const name: string; const size: Int64);
begin
  fName := name;
  fSize := size;
end;

class constructor TSizeUnit.Create;
begin
  fBytes := TSizeUnit.Create(SBytesDescription, 1);
  fKB := TSizeUnit.Create(SKBDescription, COneKB);
  fMB := TSizeUnit.Create(SMBDescription, COneMB);
  fGB := TSizeUnit.Create(SGBDescription, COneGB);
  fTB := TSizeUnit.Create(STBDescription, COneTB);
end;

class function TSizeUnit.From(const size: Int64): TSizeUnit;
begin
  if size >= COneTB then
    Result := TSizeUnit.TB
  else if size >= COneGB then
    Result := TSizeUnit.GB
  else if size >= COneMB then
    Result := TSizeUnit.MB
  else if size >= COneKB then
    Result := TSizeUnit.KB
  else
    Result := TSizeUnit.Bytes;
end;

function TSizeUnit.FormatSize(const size: Int64): string;
var
  number: Double;
  numberFormat: TFloatFormat;
  numberString: string;
  precision: Integer;
  digits: Integer;
begin
  number := size / Self.Size;
  if number < 1 then
  begin
    precision := 2;
    digits := 2;
    numberFormat := ffGeneral;
  end
  else if number < 1000 then
  begin
    precision := 3;
    digits := 0;
    numberFormat := ffGeneral;
  end
  else
  begin
    precision := 15;
    digits := 0;
    numberFormat := ffNumber;
  end;
  numberString := FloatToStrF(number, numberFormat, precision, digits);
  Result := Format(SSizeStringFormat, [numberString, Name]);
end;

{$ENDREGION}


{$REGION 'TSize'}

constructor TSize.Create(const value: Int64);
begin
  fValue := value;
end;

function TSize.ToString: string;
var
  sizeUnit: TSizeUnit;
begin
  sizeUnit := TSizeUnit.From(fValue);
  Result := sizeUnit.FormatSize(fValue);
end;

function TSize.ToString(const sizeUnit: TSizeUnit): string;
begin
  Result := sizeUnit.FormatSize(fValue);
end;

class operator TSize.Explicit(const size: TSize): Int64;
begin
  Result := size.Value;
end;

class operator TSize.Explicit(const value: Int64): TSize;
begin
  Result.fValue := value;
end;

class operator TSize.Implicit(const size: TSize): Int64;
begin
  Result := size.Value;
end;

class operator TSize.Implicit(const value: Int64): TSize;
begin
  Result.fValue := value;
end;

class operator TSize.Equal(const left, right: TSize): Boolean;
begin
  Result := left.Value = right.Value;
end;

class operator TSize.NotEqual(const left, right: TSize): Boolean;
begin
  Result := left.Value <> right.Value;
end;

class operator TSize.GreaterThan(const left, right: TSize): Boolean;
begin
  Result := left.Value > right.Value;
end;

class operator TSize.GreaterThanOrEqual(const left, right: TSize): Boolean;
begin
  Result := left.Value >= right.Value;
end;

class operator TSize.LessThan(const left, right: TSize): Boolean;
begin
  Result := left.Value < right.Value;
end;

class operator TSize.LessThanOrEqual(const left, right: TSize): Boolean;
begin
  Result := left.Value <= right.Value;
end;

{$ENDREGION}


{$IFDEF MSWINDOWS}
{$REGION 'TFileSystemEntry'}

constructor TFileSystemEntry.Create(const fileName: string);
var
  handle: THandle;
  data: TWin32FindData;
  path: string;
begin
  path := ExtractFileDir(fileName);
  handle := FindFirstFile(PChar(fileName), data);
  if handle <> INVALID_HANDLE_VALUE then
  begin
    try
      Create(path, data);
    finally
      Windows.FindClose(handle);
    end;
  end
  else
  begin
    Finalize(Self);
    ZeroMemory(@Self, SizeOf(Self));
    fLocation := path;
    fName := ExtractFileName(fileName);
  end;
end;

constructor TFileSystemEntry.Create(const location: string;
  const data: TWin32FindData);
begin
  fLocation := ExcludeTrailingPathDelimiter(location);   // TEMP
  fName := data.cFileName;
  fSize := data.nFileSizeLow or Int64(data.nFileSizeHigh) shl 32;
  fCreationTime := data.ftCreationTime;
  fLastWriteTime := data.ftLastWriteTime;
  fLastAccessTime := data.ftLastAccessTime;
  fAttributeFlags := data.dwFileAttributes;
  fExists := fAttributeFlags <> INVALID_FILE_ATTRIBUTES;
end;

constructor TFileSystemEntry.Create(const location: string;
  const searchRec: TSearchRec);
begin
  Create(location, searchRec.FindData);
end;

function TFileSystemEntry.DoGetEntries(scope: TEntryScope;
  const searchPattern: string; includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>;
var
  attributes: Cardinal;
const
  ScopeAttributes: array[TEntryScope] of Cardinal = (
    faAnyFile,
    faDirectory,
    faAnyFile and not faDirectory
  );
begin
  if IsDirectory then
  begin
    attributes := ScopeAttributes[scope];
    Result := TFileEnumerable.Create(FullName, searchPattern, attributes, includeSubfolders);
  end
  else
  begin
    Result := TEmptyEnumerable<TFileSystemEntry>.Create;
  end;
end;

procedure TFileSystemEntry.Refresh;
begin
  Self := TFileSystemEntry.Create(FullName);
end;

function TFileSystemEntry.GetDirectories: IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esDirectories, '*', False);
end;

function TFileSystemEntry.GetDirectories(
  const searchPattern: string): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esDirectories, searchPattern, False);
end;

function TFileSystemEntry.GetDirectories(const searchPattern: string;
  includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esDirectories, searchPattern, includeSubfolders);
end;

function TFileSystemEntry.GetFiles: IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esFiles, '*', False);
end;

function TFileSystemEntry.GetFiles(
  const searchPattern: string): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esFiles, searchPattern, False);
end;

function TFileSystemEntry.GetFiles(const searchPattern: string;
  includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esFiles, searchPattern, includeSubfolders);
end;

function TFileSystemEntry.GetEntries: IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esAllEntries, '*', False);
end;

function TFileSystemEntry.GetEntries(
  const searchPattern: string): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esAllEntries, searchPattern, False);
end;

function TFileSystemEntry.GetEntries(const searchPattern: string;
  includeSubfolders: Boolean): IEnumerable<TFileSystemEntry>;
begin
  Result := DoGetEntries(esAllEntries, searchPattern, includeSubfolders);
end;

function TFileSystemEntry.GetFullName: string;
begin
  Result := IncludeTrailingPathDelimiter(Location) + Name;
end;

function TFileSystemEntry.GetExtension: string;
begin
  if IsFile then
    Result := ExtractFileExt(Name)
  else
    Result := '';
end;

function TFileSystemEntry.GetIsFile: Boolean;
begin
  Result := fAttributeFlags and faDirectory = 0;
end;

function TFileSystemEntry.GetIsEmpty: Boolean;
begin
  if IsFile then
    Result := Size = 0
  else
    Result := GetEntries.IsEmpty;
end;

function TFileSystemEntry.GetHasAttribute(attribute: Integer): Boolean;
begin
  Result := fAttributeFlags and attribute <> 0;
end;

function TFileSystemEntry.GetCreationTime: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fCreationTime, True);
end;

function TFileSystemEntry.GetCreationTimeUtc: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fCreationTime, False);
end;

function TFileSystemEntry.GetLastAccessTime: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fLastAccessTime, True);
end;

function TFileSystemEntry.GetLastAccessTimeUtc: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fLastAccessTime, False);
end;

function TFileSystemEntry.GetLastWriteTime: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fLastWriteTime, True);
end;

function TFileSystemEntry.GetLastWriteTimeUtc: TDateTime;
begin
  Result := ConvertFileTimeToDateTime(fLastWriteTime, False);
end;

function TFileSystemEntry.GetOwner: string;
var
  size: Cardinal;
  descriptor: PSecurityDescriptor;
  ownerSid: PSID;
  ownerDefaulted: LongBool;
  ownerType: SID_NAME_USE;
  name: string;
  domainName: string;
  nameLength: Cardinal;
  domainNameLength: Cardinal;
  sidString: PChar;
begin
  if not GetFileSecurity(PChar(FullName), OWNER_SECURITY_INFORMATION, nil, 0, size) and
    (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
  begin
    Exit('');
  end;
  descriptor := AllocMem(size);
  try
    if not GetFileSecurity(PChar(FullName), OWNER_SECURITY_INFORMATION, descriptor, size, size) or
      not GetSecurityDescriptorOwner(descriptor, ownerSid, ownerDefaulted) then
    begin
      Exit('');
    end;
    nameLength := 0;
    domainNameLength := 0;
    if not LookupAccountSid(nil, ownerSid, nil, nameLength, nil, domainNameLength, ownerType) and
      (GetLastError <> ERROR_INSUFFICIENT_BUFFER) then
    begin
      if ConvertSidToStringSid(ownerSid, sidString) then
      begin
        Result := sidString;
        LocalFree(Cardinal(sidString));
        Exit;
      end
      else
      begin
        Exit('');
      end;
    end;
    SetLength(name, nameLength - 1);
    SetLength(domainName, domainNameLength - 1);
    if not LookupAccountSID(nil, ownerSid, PChar(name), nameLength, PChar(domainName), domainNameLength, ownerType) then
    begin
      Exit('');
    end;
    Result := name;
  finally
    FreeMem(descriptor);
  end;
end;

function TFileSystemEntry.GetTypeString: string;
var
  fileInfo: TSHFileInfo;
begin
  SHGetFileInfo(PChar(FullName), 0, fileInfo, SizeOf(fileInfo), SHGFI_TYPENAME);
  Result := fileInfo.szTypeName;
end;

class operator TFileSystemEntry.Implicit(const entry: TFileSystemEntry): string;
begin
  Result := entry.FullName;
end;

{$ENDREGION}
{$ENDIF MSWINDOWS}


{$REGION 'TSearchPatternMatcher'}

constructor TSearchPatternMatcher.Create(const searchPattern: string);
begin
  inherited Create;
  fSearchPattern := searchPattern;
  fPredicate := CreatePredicate(fSearchPattern);
end;

destructor TSearchPatternMatcher.Destroy;
begin
  fMask.Free;
  inherited Destroy;
end;

class constructor TSearchPatternMatcher.Create;
begin
  fAll := TSearchPatternMatcher.Create('*');
end;

function TSearchPatternMatcher.GetSearchPattern: string;
begin
  Result := fSearchPattern;
end;

function TSearchPatternMatcher.GetPatternCount: Integer;
begin
  if fMasks <> nil then
    Result := fMasks.Count
  else
    Result := 1;
end;

function TSearchPatternMatcher.CreatePredicate(
  const patterns: TStrings): TFileNamePredicate;
var
  pattern: string;
begin
  if patterns.Count = 1 then
  begin
    pattern := patterns[0];
    fMask := TMask.Create(pattern);
    Result :=
      function (const fileName: string): Boolean
      begin
        Result := fMask.Matches(fileName);
      end;
  end
  else
  begin
    fMasks := TCollections.CreateList<TMask>(True);
    for pattern in patterns do
    begin
      fMasks.Add(TMask.Create(pattern));
    end;
    Result :=
      function (const fileName: string): Boolean
      var
        pattern: TMask;
      begin
        Result := False;
        for pattern in fMasks do
        begin
          if pattern.Matches(fileName) then
          begin
            Exit(True);
          end;
        end;
      end;
  end;
end;

function TSearchPatternMatcher.CreatePredicate(const searchPattern: string): TFileNamePredicate;
var
  patterns: TStrings;
begin
  if (searchPattern = '') or (searchPattern = '*') or (searchPattern = '*.*') then
  begin
    Result :=
      function (const fileName: string): Boolean
      begin
        Result := True;
      end;
  end
  else
  begin
    patterns := TStringList.Create;
    try
      ExtractStrings([',', ';', #9], [' '], PChar(searchPattern), patterns);
      Result := CreatePredicate(patterns);
    finally
      patterns.Free;
    end;
  end;
end;

function TSearchPatternMatcher.Matches(const fileName: string): Boolean;
begin
  Result := fPredicate(fileName);
end;

{$ENDREGION}


{$IFDEF MSWINDOWS}
{$REGION 'TFileEnumerable'}

constructor TFileEnumerable.Create(const path, searchPattern: string;
  attributes: Cardinal; includeSubfolders: Boolean);
begin
  inherited Create;
  fPath := IncludeTrailingPathDelimiter(path);
  fSearchPattern := searchPattern;
  fAttributes := attributes;
  fIncludeSubfolders := includeSubfolders;
end;

function TFileEnumerable.GetEnumerator: IEnumerator<TFileSystemEntry>;
begin
  Result := TFileEnumerator.Create(fPath, fSearchPattern, fAttributes, fIncludeSubfolders);
end;

{$ENDREGION}


{$REGION 'TFileEnumerator.TSearchContext'}

constructor TFileEnumerator.TSearchContext.Create(const path,
  searchPattern: string; attributes: Cardinal);
var
  matcher: ISearchPatternMatcher;
  fileName: string;
begin
  matcher := TSearchPatternMatcher.Create(searchPattern);
  if matcher.PatternCount = 1 then
  begin
    fileName := TPath.Combine(path, searchPattern);
  end
  else
  begin
    fileName := TPath.Combine(path, '*.*');
  end;
  Create(path, fileName, matcher, attributes);
end;

constructor TFileEnumerator.TSearchContext.Create(const path, fileName: string;
  const matcher: ISearchPatternMatcher; attributes: Cardinal);
begin
  fPath := path;
  fFileName := fileName;
  fMatcher := matcher;
  fAttributes := attributes;
  fSearchHandle := INVALID_HANDLE_VALUE;
  fIsFirstFind := True;
end;

procedure TFileEnumerator.TSearchContext.Close;
begin
  CloseSearchHandle;
end;

procedure TFileEnumerator.TSearchContext.CloseSearchHandle;
begin
  if fSearchHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(fSearchHandle);
    fSearchHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TFileEnumerator.TSearchContext.Accept(
  const data: TWin32FindData): Boolean;
var
  fileName: string;
begin
  fileName := data.cFileName;
  Result := (fileName <> fCCurrentDirName) and
    (fileName <> fCParentDirName) and
    (data.dwFileAttributes and fAttributes <> 0) and
    fMatcher.Matches(fileName);
end;

function TFileEnumerator.TSearchContext.MoveNext: Boolean;
begin
  if not fIsFirstFind then
  begin
    Result := Windows.FindNextFile(fSearchHandle, fFindData);
  end
  else
  begin
    fSearchHandle := Windows.FindFirstFile(PChar(fFileName), fFindData);
    Result := fSearchHandle <> INVALID_HANDLE_VALUE;
    fIsFirstFind := False;
  end;
  if not Result then
  begin
    Exit(False);
  end;
  repeat
    Result := Accept(fFindData);
    if Result or not Windows.FindNextFile(fSearchHandle, fFindData) then
    begin
      Exit;
    end;
  until IsTerminated;
end;

function TFileEnumerator.TSearchContext.GetIsTerminated: Boolean;
begin
  Result := Assigned(fInspector) and fInspector.IsTerminated;
end;

{$ENDREGION}


{$REGION 'TFileEnumerator'}

constructor TFileEnumerator.Create(const path, searchPattern: string;
  attributes: Cardinal; includeSubfolders: Boolean);
begin
  inherited Create;
  fRootPath := path;
  fSearchPatten := searchPattern;
  fAttributes := attributes;
  fIncludeSubfolders := includeSubfolders;
  fMatcher := TSearchPatternMatcher.Create(searchPattern);
  if fIncludeSubfolders then
  begin
    fStacks := TStack<TSearchContext>.Create;
  end;
  fCurrentContext := CreateSearchContext(fRootPath);
end;

destructor TFileEnumerator.Destroy;
begin
  FreeContexts;
  fStacks.Free;
  inherited Destroy;
end;

procedure TFileEnumerator.FreeContexts;
begin
  fCurrentContext.Close;
  if fStacks = nil then
  begin
    Exit;
  end;
  while fStacks.Count > 0 do
  begin
    fCurrentContext := fStacks.Pop;
    fCurrentContext.Close;
  end;
end;

procedure TFileEnumerator.Initialize(const inspector: IFileEnumeratorInspector);
begin
  inherited Initialize(inspector);
  fCurrentContext.Inspector := inspector;
end;

function TFileEnumerator.CreateSearchContext(const path: string): TSearchContext;
var
  fileName: string;
begin
  if not fIncludeSubfolders then
  begin
    Result := TSearchContext.Create(path, fSearchPatten, fAttributes);
    Result.Inspector := Self.Inspector;
  end
  else
  begin
    fileName := IncludeTrailingPathDelimiter(path) + '*.*';
    Result := TSearchContext.Create(path, fileName, TSearchPatternMatcher.All,
      fAttributes or faDirectory);
    Result.Inspector := Self.Inspector;
  end;
end;

procedure TFileEnumerator.DoDirectoryFound(var context: TSearchContext;
  const entry: TFileSystemEntry);
var
  location: string;
begin
  Assert(fStacks <> nil, 'fStacks should not be nil.');
  fStacks.Push(context);
  location := entry.FullName;
  context := CreateSearchContext(location);
  NotifyLocationChanged(location);
end;

function TFileEnumerator.GetCurrent: TFileSystemEntry;
begin
  Result := fCurrentEntry;
end;

function TFileEnumerator.Accept(const entry: TFileSystemEntry): Boolean;
begin
  Result := (entry.AttributeFlags and fAttributes <> 0) and
    fMatcher.Matches(entry.Name);
end;

function TFileEnumerator.MoveNext: Boolean;
begin
  repeat
    Result := fCurrentContext.MoveNext;
    if Result then
    begin
      fCurrentEntry := TFileSystemEntry.Create(fCurrentContext.Path, fCurrentContext.Current);
      if fCurrentEntry.IsDirectory and fIncludeSubfolders then
      begin
        DoDirectoryFound(fCurrentContext, fCurrentEntry);
      end;
      Result := Accept(fCurrentEntry);
    end
    else
    begin
      if fIncludeSubfolders and (fStacks.Count > 0) then
      begin
        fCurrentContext.Close;
        fCurrentContext := fStacks.Pop;
      end
      else
      begin
        Break;
      end;
    end;
  until (Result or IsTerminated);
end;

procedure TFileEnumerator.Reset;
begin
  FreeContexts;
  fCurrentContext := CreateSearchContext(fRootPath);
end;

{$ENDREGION}


{$REGION 'TFileListEnumerable'}

constructor TFileListEnumerable.Create(files: TStrings);
begin
  inherited Create;
  fFiles := files;
end;

function TFileListEnumerable.GetEnumerator: IEnumerator<TFileSystemEntry>;
begin
  Result := TFileListEnumerator.Create(fFiles);
end;

{$ENDREGION}


{$REGION 'TDroppedFilesEnumerable'}

constructor TDroppedFilesEnumerable.Create(dropHandle: THandle);
begin
  inherited Create;
  fFiles := TStringList.Create;
  GetDroppedFiles(dropHandle, fFiles);
end;

constructor TDroppedFilesEnumerable.Create(const dataObject: IDataObject);
begin
  inherited Create;
  fFiles := TStringList.Create;
  GetDroppedFiles(dataObject, fFiles);
end;

destructor TDroppedFilesEnumerable.Destroy;
begin
  fFiles.Free;
  inherited Destroy;
end;

function TDroppedFilesEnumerable.GetEnumerator: IEnumerator<TFileSystemEntry>;
begin
  Result := TFileListEnumerator.Create(fFiles);
end;

{$ENDREGION}


{$REGION 'TFileListEnumerator'}

constructor TFileListEnumerator.Create(files: TStrings);
begin
  inherited Create;
  fFiles := files;
  fIndex := -1;
end;

function TFileListEnumerator.GetCurrent: TFileSystemEntry;
var
  fileName: string;
begin
  if fIndex < 0 then
    raise EInvalidOperationException.Create(SEnumNotStarted);
  if fIndex > fFiles.Count - 1 then
    raise EInvalidOperationException.Create(SEnumEnded);
  fileName := fFiles[fIndex];
  GetFileSystemEntry(fileName, fEntry);
  Result := fEntry;
end;

procedure TFileListEnumerator.GetFileSystemEntry(const path: string;
  out entry: TFileSystemEntry);
begin
  entry := TFileSystemEntry.Create(path);
end;

function TFileListEnumerator.MoveNext: Boolean;
begin
  Result := fIndex < fFiles.Count - 1;
  if Result then
  begin
    Inc(fIndex);
  end;
end;

procedure TFileListEnumerator.Reset;
begin
  fIndex := -1;
end;

{$ENDREGION}


{$REGION 'TFileSearchWorker'}

procedure TFileSearchWorker.Initialize(const collections: IEnumerable<IFileEnumerable>);
begin
//  Guard.CheckNotNull(collections, 'collections');
  fCollections := collections;
end;

procedure TFileSearchWorker.LocationChanged(const location: string);
begin
  if Assigned(fOnLocationChanged) then
  begin
    fOnLocationChanged(Self, location);
  end;
end;

procedure TFileSearchWorker.Execute;
var
  collection: IFileEnumerable;
  enumerator: IEnumerator<TFileSystemEntry>;
  intf: ISupportFileEnumeratorInspector;
  entry: TFileSystemEntry;
begin
  if (fCollections = nil) or Terminated then
  begin
    Exit;
  end;
  BeginSearch;
  try
    for collection in fCollections do
    begin
      enumerator := collection.GetEnumerator;
      if Supports(enumerator, ISupportFileEnumeratorInspector, intf) then
      begin
        intf.Initialize(Self);
      end;
      while not Terminated and enumerator.MoveNext do
      begin
        entry := enumerator.Current;
        ProcessEntry(entry);
      end;
      if Terminated then Exit;
    end;
  finally
    EndSearch;
  end;
end;

function TFileSearchWorker.GetIsTerminated: Boolean;
begin
  Result := Self.Terminated;
end;

procedure TFileSearchWorker.ProcessEntry(const entry: TFileSystemEntry);
begin
  if Accept(entry) then
  begin
    RaiseOnProgress(entry);
  end;
end;

function TFileSearchWorker.Accept(const entry: TFileSystemEntry): Boolean;
begin
  Result := not Assigned(fOnFilter) or fOnFilter(Self, entry);
end;

procedure TFileSearchWorker.RaiseOnProgress(const entry: TFileSystemEntry);
begin
  if Assigned(fOnProgress) then
  begin
    fOnProgress(Self, entry);
  end;
end;

procedure TFileSearchWorker.BeginSearch;
begin
  if Assigned(fOnSearchBegin) then
  begin
    fOnSearchBegin(Self);
  end;
end;

procedure TFileSearchWorker.EndSearch;
begin
  if Assigned(fOnSearchEnd) then
  begin
    fOnSearchEnd(Self);
  end;
end;

{$ENDREGION}


{$REGION 'TFileSearcherBase'}

constructor TFileSearcherBase.Create;
begin
  inherited Create;
  fSync := TObject.Create;
  fStatus := ssReady;
end;

destructor TFileSearcherBase.Destroy;
begin
  fSync.Free;
  inherited Destroy;
end;

procedure TFileSearcherBase.DoStart;
begin
end;

procedure TFileSearcherBase.DoStop;
begin
end;

procedure TFileSearcherBase.DoPause;
begin
end;

procedure TFileSearcherBase.DoResume;
begin
end;

procedure TFileSearcherBase.Start;
begin
  if CanStart then
  begin
    DoStart;
    ChangeStatus(ssSearching);
  end;
end;

procedure TFileSearcherBase.Stop;
begin
  if CanStop then
  begin
    DoStop;
    ChangeStatus(ssStopped);
  end;
end;

procedure TFileSearcherBase.Pause;
begin
  if CanPause then
  begin
    DoPause;
    ChangeStatus(ssPaused);
  end;
end;

procedure TFileSearcherBase.Resume;
begin
  if CanResume then
  begin
    DoResume;
    ChangeStatus(ssSearching);
  end;
end;

function TFileSearcherBase.CreateWorker: TFileSearchWorker;
begin
  Result := TFileSearchWorker.Create(True);
  Result.OnSearchBegin := DoWorkerSearchBegin;
  Result.OnSearchEnd := DoWorkerSearchEnd;
  Result.OnFilter := DoWorkerFilter;
  Result.OnProgress := DoWorkerProgress;
  Result.OnLocationChanged := DoWorkerLocationChanged;
end;

procedure TFileSearcherBase.DoWorkerSearchBegin(sender: TObject);
begin
  Queue(RaiseOnSearchBegin);
end;

procedure TFileSearcherBase.DoWorkerSearchEnd(sender: TObject);
begin
  Queue(
    procedure
    begin
      Stop;
      RaiseOnSearchEnd;
    end
  );
end;

function TFileSearcherBase.DoWorkerFilter(sender: TObject;
  const entry: TFileSystemEntry): Boolean;
begin
  MonitorEnter(fSync);
  try
    Result := not Assigned(fOnFilter) or fOnFilter(Self, entry);
  finally
    MonitorExit(fSync);
  end;
end;

procedure TFileSearcherBase.DoWorkerProgress(sender: TObject;
  const entry: TFileSystemEntry);
var
  e: TFileSystemEntry;
begin
  e := entry;
  Queue(
    procedure
    begin
      RaiseOnProgress(e);
    end
  );
end;

procedure TFileSearcherBase.DoWorkerLocationChanged(sender: TObject;
  const location: string);
begin
  Queue(
    procedure
    begin
      RaiseOnLocationChanged(location);
    end
  );
end;

function TFileSearcherBase.GetCanStart: Boolean;
begin
  Result := Status in [ssReady, ssStopped];
end;

function TFileSearcherBase.GetCanStop: Boolean;
begin
  Result := Status in [ssSearching, ssPaused];
end;

function TFileSearcherBase.GetCanPause: Boolean;
begin
  Result := Status = ssSearching;
end;

function TFileSearcherBase.GetCanResume: Boolean;
begin
  Result := Status = ssPaused;
end;

procedure TFileSearcherBase.ChangeStatus(newStatus: TFileSearchStatus);
var
  oldStatus: TFileSearchStatus;
begin
  if fStatus <> newStatus then
  begin
    oldStatus := fStatus;
    if (oldStatus = ssReady) and (newStatus = ssSearching) then
    begin
      RaiseOnSearchBegin;
    end
    else if newStatus = ssStopped then
    begin
      RaiseOnSearchEnd;
    end;
    fStatus := newStatus;
    RaiseOnStatusChanged;
  end;
end;

procedure TFileSearcherBase.RaiseOnSearchBegin;
begin
  if Assigned(fOnSearchBegin) then
  begin
    fOnSearchBegin(Self);
  end;
end;

procedure TFileSearcherBase.RaiseOnSearchEnd;
begin
  if Assigned(fOnSearchEnd) then
  begin
    fOnSearchEnd(Self);
  end;
end;

procedure TFileSearcherBase.RaiseOnProgress(const entry: TFileSystemEntry);
begin
  if Assigned(fOnProgress) then
  begin
    fOnProgress(Self, entry);
  end;
end;

procedure TFileSearcherBase.RaiseOnLocationChanged(const location: string);
begin
  if Assigned(fOnLocationChanged) then
  begin
    fOnLocationChanged(Self, location);
  end;
end;

procedure TFileSearcherBase.RaiseOnStatusChanged;
begin
  if Assigned(fOnStatusChanged) then
  begin
    fOnStatusChanged(Self);
  end;
end;

procedure TFileSearcherBase.SetOnFilter(const value: TFileSearchFilterEvent);
begin
  MonitorEnter(fSync);
  try
    fOnFilter := value;
  finally
    MonitorExit(fSync);
  end;
end;

{$ENDREGION}


{$REGION 'TFileSearcher'}

constructor TFileSearcher.Create;
begin
  inherited Create;
  fLocations := TStringList.Create;
  fFileTypes := TStringList.Create;
  fStatistics := TFileSearchStatistics.Create;
  fSearchScope := ssDirectoriesAndFiles;
  fIncludeSubfolders := True;
end;

destructor TFileSearcher.Destroy;
begin
  Stop;
  fWorker.Free;
  fLocations.Free;
  fFileTypes.Free;
  fStatistics.Free;
  inherited Destroy;
end;

function TFileSearcher.CreateFileEnumerable(const path, searchPattern: string): IFileEnumerable;
var
  attributes: Cardinal;
begin
  case fSearchScope of
    ssDirectoriesAndFiles:
    begin
      attributes := faAnyFile;
    end;
    ssFiles:
    begin
      attributes := faAnyFile and not faDirectory;
    end;
    ssDirectories:
    begin
      attributes := faDirectory;
    end;
    else
    begin
      attributes := faAnyFile;
    end;
  end;
  Result := TFileEnumerable.Create(path, searchPattern, attributes, fIncludeSubfolders);
end;

function TFileSearcher.CreateStatistics: TFileSearchStatistics;
begin
  Result := TFileSearchStatistics.Create;
end;

procedure TFileSearcher.DoStart;
var
  collections: IList<IFileEnumerable>;
  collection: IFileEnumerable;
  path: string;
  searchPattern: string;
begin
  fStatistics.StartNew;
  searchPattern := fFileTypes.DelimitedText;
  collections := TCollections.CreateList<IFileEnumerable>;
  for path in fLocations do
  begin
    collection := CreateFileEnumerable(path, searchPattern);
    collections.Add(collection);
  end;
  FreeAndNil(fWorker);
  fWorker := CreateWorker;
  fWorker.Initialize(collections);
  fWorker.Start;
end;

procedure TFileSearcher.DoStop;
begin
  fStatistics.Stop;
  fWorker.Terminate;
  fWorker.Suspended := False;
end;

procedure TFileSearcher.DoWorkerProgress(sender: TObject;
  const entry: TFileSystemEntry);
begin
  fStatistics.Notify(entry);
  inherited DoWorkerProgress(sender, entry);
  Sleep(fCDefaultSleepTime);
end;

procedure TFileSearcher.DoPause;
begin
  fWorker.Suspended := True;
  fStatistics.Stop;
end;

procedure TFileSearcher.DoResume;
begin
  fWorker.Suspended := False;
  fStatistics.Start;
end;

procedure TFileSearcher.SetLocations(const value: TStrings);
begin
  fLocations.Assign(value);
end;

procedure TFileSearcher.SetFileTypes(const value: TStrings);
begin
  fFileTypes.Assign(value);
end;

{$ENDREGION}


{$REGION 'TFileSearchStatistics'}

constructor TFileSearchStatistics.Create;
begin
  inherited Create;
  fStopwatch := TStopwatch.Create;
end;

procedure TFileSearchStatistics.Notify(const entry: TFileSystemEntry);
begin
  Inc(fTotalCount);
  Inc(fTotalSize, entry.Size);
  if entry.IsFile then
  begin
    Inc(fFileCount);
  end
  else if entry.IsDirectory then
  begin
    Inc(fFolderCount);
  end;
end;

procedure TFileSearchStatistics.Start;
begin
  fStopwatch.Start;
end;

procedure TFileSearchStatistics.StartNew;
begin
  Reset;
  Start;
end;

procedure TFileSearchStatistics.Stop;
begin
  fStopwatch.Stop;
end;

procedure TFileSearchStatistics.Reset;
begin
  fTotalCount := 0;
  fTotalSize := 0;
  fFileCount := 0;
  fFolderCount := 0;
  fStopwatch.Reset;
end;

function TFileSearchStatistics.GetElapsed: TTimeSpan;
begin
  Result := fStopwatch.Elapsed;
end;

{$ENDREGION}


{$REGION 'TFileEnumeratorBase'}

procedure TFileEnumeratorBase.Initialize(
  const inspector: IFileEnumeratorInspector);
begin
  fInspector := inspector;
end;

procedure TFileEnumeratorBase.NotifyLocationChanged(const location: string);
begin
  if Assigned(fInspector) then
  begin
    fInspector.LocationChanged(location);
  end;
end;

function TFileEnumeratorBase.GetIsTerminated: Boolean;
begin
  Result := (fInspector <> nil) and fInspector.IsTerminated;
end;

{$ENDREGION}
{$ENDIF MSWINDOWS}

end.
