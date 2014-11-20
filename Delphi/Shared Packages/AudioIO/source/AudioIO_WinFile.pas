unit AudioIO_WinFile;

interface

// NOTE:
// The TWinFile constructor can throw an exception if it fails. I think it needs some research because I'm
// not sure how that should be handled. Perhaps TWinFile needs to be rethought.

uses
  Windows, Classes;

const
  // dwDesireAccess  (Info of variables - http://msdn.microsoft.com/en-us/library/aa446632(VS.85).aspx )
  kGenericAll     = Windows.GENERIC_ALL;      //Read, write, and execute access
  kGenericExecute = Windows.GENERIC_EXECUTE;  //execute access
  kGenericRead    = Windows.GENERIC_READ;     //read access
  kGenericWrite   = Windows.GENERIC_WRITE;    //write access

  // dwShare Mode
  kNotShared       = 0;    //Prevents other processes from opening a file or device if they request delete, read, or write access.
  kFileShareDelete = Windows.FILE_SHARE_DELETE;   //Enables subsequent open operations on a file or device to request delete access.
  kFileShareRead   = Windows.FILE_SHARE_READ;     //Enables subsequent open operations on a file or device to request read access.
  kFileShareWrite  = Windows.FILE_SHARE_WRITE;    //Enables subsequent open operations on a file or device to request write access.

  // dwCreationDisposition
  // - Usually set to kOpenExisting
  // - Parameter must be one of the following and cannot be combined.
  kCreateAlways     = Windows.CREATE_ALWAYS;
  kCreateNew        = Windows.CREATE_NEW;
  kOpenAlways       = Windows.OPEN_ALWAYS;
  kOpenExisting     = Windows.OPEN_EXISTING;
  kTruncateExisting = Windows.TRUNCATE_EXISTING;

  // dwFlagsAndAttributes
  kFileAttributeArchive   = Windows.FILE_ATTRIBUTE_ARCHIVE;
  kFileAttributeEncrypted = Windows.FILE_ATTRIBUTE_ENCRYPTED;
  KFileAttributeHidden    = Windows.FILE_ATTRIBUTE_HIDDEN;
  kFileAttributeNormal    = Windows.FILE_ATTRIBUTE_NORMAL;  //The most common default value for files.
  kFileAttributeOffline   = Windows.FILE_ATTRIBUTE_OFFLINE;
  kFileAttributeReadOnly  = Windows.FILE_ATTRIBUTE_OFFLINE;
  kFileAttributeSystem    = Windows.FILE_ATTRIBUTE_SYSTEM;
  kFileAttributeTemporary = Windows.FILE_ATTRIBUTE_TEMPORARY;



type
  TWinFile = class
  private
    fPosition: Int64;
  protected
    FileHandle:cardinal;
    Overlapped:TOverlapped;
    function GetSize:DWORD;
  public
    constructor Create(lpFileName:string;
                       dwDesiredAccess,
                       dwShareMode:DWORD;
                       lpSecurityAttributes: PSecurityAttributes;
                       dwCreationDisposition,
                       dwFlagsAndAttributes:DWORD); overload;

    constructor Create(lpFileName:string;
                       dwDesiredAccess,
                       dwShareMode:DWORD;
                       dwCreationDisposition:DWORD); overload;


	  destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;

    property Position:Int64 read fPosition write fPosition;
    property Size:DWORD read GetSize;
  end;

implementation

{ TWinFile }


// The constructor functions primarily as a wrapper around CreateFile.
// Info on create file can be found here - http://msdn.microsoft.com/en-us/library/aa363858(VS.85).aspx

{
lpFileName [in]
    The name of the file or device to be created or opened. For more info see MSDN http://msdn.microsoft.com/en-us/library/aa363858(VS.85).aspx


dwDesiredAccess [in]
    The requested access to the file or device, which can be summarized as read, write, both or neither (zero).

    The most commonly used values are GENERIC_READ, GENERIC_WRITE, or both (GENERIC_READ | GENERIC_WRITE).
    For more information, see Generic Access Rights and File Security and Access Rights.

    If this parameter is zero, the application can query certain metadata such as file, directory, or device attributes
    without accessing that file or device, even if GENERIC_READ access would have been denied.

    You cannot request an access mode that conflicts with the sharing mode that is specified by the dwShareMode parameter
    in an open request that already has an open handle.

    For more information, see the Remarks section of this topic and Creating and Opening Files.


dwShareMode [in]
    The requested sharing mode of the file or device, which can be read, write, both, delete, all of these, or none
    (refer to the following table). Access requests to attributes or extended attributes are not affected by this flag.

    If this parameter is zero and CreateFile succeeds, the file or device cannot be shared and cannot be opened again
    until the handle to the file or device is closed. For more information, see the Remarks section.

    You cannot request a sharing mode that conflicts with the access mode that is specified in an existing request
    that has an open handle. CreateFile would fail and the GetLastError function would return ERROR_SHARING_VIOLATION.

    To enable a process to share a file or device while another process has the file or device open, use a compatible
    combination of one or more of the following values. For more information about valid combinations of this
    parameter with the dwDesiredAccess parameter, see Creating and Opening Files.

    Note  The sharing options for each open handle remain in effect until that handle is closed, regardless of
    process context.


lpSecurityAttributes [in, optional]
    A pointer to a SECURITY_ATTRIBUTES structure that contains two separate but related data members: an optional
    security descriptor, and a Boolean value that determines whether the returned handle can be inherited by
    child processes.

    This parameter can be NULL.

    If this parameter is NULL, the handle returned by CreateFile cannot be inherited by any child processes the
    application may create and the file or device associated with the returned handle gets a default security
    descriptor.

    The lpSecurityDescriptor member of the structure specifies a SECURITY_DESCRIPTOR for a file or device. If
    this member is NULL, the file or device associated with the returned handle is assigned a default security
    descriptor.

    CreateFile ignores the lpSecurityDescriptor member when opening an existing file or device, but continues to
    use the bInheritHandle member.

    The bInheritHandle member of the structure specifies whether the returned handle can be inherited.

    For more information, see the Remarks section.

dwCreationDisposition [in]
    An action to take on a file or device that exists or does not exist.

    For devices other than files, this parameter is usually set to OPEN_EXISTING.


dwFlagsAndAttributes
    FILE_ATTRIBUTE_NORMAL is the most common default value for files.


}

constructor TWinFile.Create(lpFileName: string; dwDesiredAccess, dwShareMode: DWORD; lpSecurityAttributes: PSecurityAttributes;
                            dwCreationDisposition,  dwFlagsAndAttributes: DWORD);
begin
  // NOTE: CreateFile will throw an exception if it doesn't succeed.
  {$IFDEF VER230}
  FileHandle := CreateFile(PWideChar(lpFileName), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, 0);
  {$ELSE}
  FileHandle := CreateFile(PAnsiChar(lpFileName), dwDesiredAccess, dwShareMode, lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes, 0);
  {$ENDIF}


  Overlapped.Internal     := 0;
  Overlapped.InternalHigh := 0;
  Overlapped.Offset       := 0;
  Overlapped.OffsetHigh   := 0;
  Overlapped.hEvent       := 0;

  Position := 0;
end;

constructor TWinFile.Create(lpFileName: string; dwDesiredAccess, dwShareMode: DWORD; dwCreationDisposition:DWORD);
begin
  // NOTE: CreateFile will throw an exception if it doesn't succeed.
  {$IFDEF VER230}
    FileHandle := CreateFile(PWideChar(lpFileName), dwDesiredAccess, dwShareMode, nil, dwCreationDisposition, kFileAttributeNormal, 0);
  {$ELSE}
    FileHandle := CreateFile(PAnsiChar(lpFileName), dwDesiredAccess, dwShareMode, nil, dwCreationDisposition, kFileAttributeNormal, 0);
  {$ENDIF}


  Overlapped.Internal     := 0;
  Overlapped.InternalHigh := 0;
  Overlapped.Offset       := 0;
  Overlapped.OffsetHigh   := 0;
  Overlapped.hEvent       := 0;

  Position := 0;
end;

destructor TWinFile.Destroy;
begin
  CloseHandle(FileHandle);
  inherited;
end;

function TWinFile.GetSize: DWORD;
begin
  result := GetFileSize(FileHandle, nil);
end;

function TWinFile.Read(var Buffer; Count: Integer): Longint;
var
  BytesRead:DWORD;
begin
  Overlapped.Offset := Position;

  ReadFile(FileHandle, Buffer, Count, BytesRead, @Overlapped);
  result := BytesRead;

  Position := Position + BytesRead;

end;



function TWinFile.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: Position := Offset;
    soCurrent:   Position := Position + Offset;
    soEnd:       Position := GetSize  + Offset;
  end;

  result := Position;
end;

function TWinFile.Write(const Buffer; Count: Integer): Longint;
var
  BytesRead:DWORD;
begin
  Overlapped.Offset := Position;

  WriteFile(FileHandle, Buffer, Count, BytesRead, @Overlapped);
  result := BytesRead;

  Position := Position + BytesRead;
end;

end.
