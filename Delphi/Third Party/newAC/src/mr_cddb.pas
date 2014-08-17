(*
  This file is a part of New Audio Components package v. 1.9
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* -----------------------------------------------------------------------------------------------

  This file uses New Audio Components package and CDRip.dll
  Copyright (c) 2008, Michael Reisch. All rights reserved.
  Thanks to Philippe Watel for his improvements and his help to unicode                    

  Connect TCDDBInfo to a TCDIn component and
  creating the CDDB_Id and CDDB_Trackinfo to
  search for Album Info in the FreeDB Database

  Send your comments and questions to

  info(at)audiolib.de

  //---------------------------//
  // How to use this component //
  //---------------------------//

  CDDBInfo.CDIn := CDIn1;                          // Assigning the CDIn Component
  if CDDBInfo.QueryAlbums > 0 then                 // Querying Albums on FreeDB
  begin
    for i := 0 to CDDBInfo.AlbumCount - 1 do       // Filling the Album Titles in a Listbox
      ListBox.Add(CDDBInfo.Album[Index]);
  end;
  ....

  // ListBox Change event

  CDDBInfo.QueryAlbumTracks(ListBox.ItemIndex);    // Reading the Tracks for the Album on FreeDB

  // Read the Tracks
  CDDBInfo.Album.Title[Index];                     // Iterating the Album Infos

*)

(*  Sample xmcd result file will be read by CDDBQuery object

210 folk 8309a80a CD database entry follows (until terminating `.')
# xmcd
#
# Track frame offsets:
#     150
#     15094
#     33264
#     56071
#     65515
#     80900
#     104437
#     129474
#     143140
#     174593
#
# Disc length: 2474
#
# Revision: 4
# Processed by: cddbd v1.5.2PL0 Copyright (c) Steve Scherf et al.
# Submitted via: BonkEnc v1.0beta3
#
DISCID=8309a80a
DTITLE=Loreena McKennitt / Elemental (Limited Edition with Bonus DVD)
DYEAR=2004
DGENRE=Celtic
TTITLE0=Blacksmith
TTITLE1=She Moved Through The Fair
TTITLE2=Stolen Child
TTITLE3=The Lark In The Clear Air
TTITLE4=Carrighfergus
TTITLE5=Kellswater
TTITLE6=Banks Of Claudy
TTITLE7=Come By The Hills
TTITLE8=Lullaby
TTITLE9=Enhanced CD Data
EXTD=Bonus DVD\nQuinlan Road Limited  QRCD101Dred: 2004\nLimited Edition with
EXTT0=
EXTT1=
EXTT2=
EXTT3=
EXTT4=
EXTT5=
EXTT6=
EXTT7=
EXTT8=
EXTT9=
PLAYORDER=
.

--------------------------------------------------------------------------------------------------*)

(* $Id: mr_cddb.pas 1256 2010-11-11 11:26:34Z andrei.borovsky $ *)

unit mr_cddb;

(* Title: mr_cddb
   Delphi unit to read CD Album and Track Informations from http://Freedb.Freedb.org using the
   CDRip122.dll and the NewAC Audio Components *)

interface

uses Windows, classes, SysUtils, WinInet, ACS_Classes, _CDRip, ACS_CDROM;

type
  TCDDBInfo = class;

  (* Structure: TCDDBAlbum
     Record was filled with the CD and Track information after calling
     TCDDBInfo.<QueryAlbumTracks>. Artist, Title and TrackExt is an array
     from 0 .. TrackCount - 1 *)

  TCDDBAlbum = record
    Artist: array of string;
    Title: array of string;
    TrackExt: array of string;
    DiscExt: string;
    Album: string;
    AlbumArtist: string;
    Year: string;
    Genre: string;
    Comment: string;
    Generated: string;
    DiscLength: string;
    TrackCount: integer;
  end;

  (* was internally used to to read data from freedb *)

  TCDDBQuery = class(Tobject)
  private
    LogStr: TStringList;
    FOwner: TComponent;

    FCDDB_ID: string;
    FCDDB_TrackInfo: string;

    procedure SplitAlbumStr(aAlbumStr: string; aAlbumTokens: TStrings);
    function XMCD_ExtractAlbums(aWEBResult: string): integer;
    procedure XMCD_ExtractTrackList(aWEBResult: string);
    function GetTrackCount: Integer;

    function CDDBInfo: TCDDBInfo;
    function BuildCommandStr(aCommand, aDiscID: string): string;
    function HTTPRead(aURL: string; var aWEBResult: string): boolean;
  public
    AlbumStr: TStringList;
    AlbumRec: TCDDBAlbum;

    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;

    function QueryAlbums(aCDDB_ID, aCDDB_TrackInfo: string): integer;
    function QueryAlbumTracks(aAlbumIndex: integer): integer;
  end;

  (* Class: TCDDBInfo
      This component reads data from CD using CDIn component to build the CDDB_ID.
      It is suitable for searching Album and Track Info from freedb.org Database.
      Requires CDRip(122).dll and <CDIn>. *)

  TCDDBInfo = class(TComponent)
  private
    FCDIn: TCDIn;
    FOpened: Integer;
    FCDDBQuery: TCDDBQuery;

    FServer: string;
    FEmail: string;
    FApplicationName: string;
    FApplicationVersion: string;

    FCDDB_ID: string;
    FCDDB_TrackInfo: string;
    FLastResult: integer;

    procedure OpenCD;
    procedure CloseCD;
    function GetAlbums(Index: integer): string;
    function GetAlbumCount: integer;
    function GetAlbum: TCDDBAlbum;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    (*Function: GetCDDB_ID
      Returning the calculated cddb_id for searching AlbumInfo on freedb.org *)
    function GetCDDB_ID : string;

    (*Function: GetCDDB_TrackInfo
      Returning the second part of the search value to find AlbumInfo on freedb.org *)
    function GetCDDB_TrackInfo : string;

    (*Function: QueryAlbums
      Call this function to search Album information from freedb.org.

      Returns:

          CDDB_FAILED           - Failed
          CDDB_NO_INTERNET      - No Internet Connection
          CDDB_EXACT_MATCH      - Found exact match
          CDDB_NO_MATCH         - No match found
          CDDB_ENTRY_OK         - OK, CDDB database entry follows (until terminating marker)
          CDDB_INEXACT_MATCH    - Found inexact matches, list follows (until terminating marker)
          CDDB_DB_ENTRY_CORRUPT - Database entry is corrupt
          CDDB_ENTRY_NOT_FOUND  - Specified CDDB entry not found.
          CDDB_SERVER_ERROR     - Server error.
          CDDB_NO_HANDSHAKE     - No handshake
          CDDB_COMMAND_ERROR    - Command Syntax Error *)
    function QueryAlbums : integer;

    (*Function: QueryAlbumTracks
      Call this function after successful return of function <QueryAlbums> to read
      the Album Track Information of specified aAlbumIndex.

      Parameters:
         aAlbumIndex - a index between 0 and <AlbumCount> -1

      Returns:
          CDDB_FAILED           - Failed
          CDDB_SUCCESS          - Successful
    *)
    function QueryAlbumTracks(aAlbumIndex : integer) : integer;

    (*Property: AlbumCount
      After calling <QueryAlbums> you will find the number of Albums in AlbumCount.
      you can iterate from 0 to AlbumCount - 1 to retrieve the Album Informations *)
    property AlbumCount              : integer read GetAlbumCount;
    property Albums[Index : integer] : string read GetAlbums;
    property Album                   : TCDDBAlbum read GetAlbum;
    property LastResult              : integer read FLastResult;
  published
    (*Property: ApplicationName
      Setting the application name that was used in connection string to freedb.org.
      If you wish you can here assign your applications name.
      Default value *NewAC* *)
    property ApplicationName    : string read FApplicationName write FApplicationName;
    (*Property: ApplicationVersion
      Setting the application version that was used in connection string to freedb.org.
      If you wish you can here assign your applications version number.
      Default value *v1.0* *)
    property ApplicationVersion : string read FApplicationVersion write FApplicationVersion;
    (*Property: CDIn
      Connect to <TCDIn> component to retrieve data from Audio CD *)
    property CDIn               : TCDIn read FCDIn write FCDIn;
    (*Property: EMail
      Setting the EMail address that was used in connection string to freedb.org.
      If you wish you can assign your Email address here.
      Default value *mymail@myhost.net* *)
    property Email              : string read FEmail write FEmail;
    (*Property: Server
      Setting the Server Address to connect to freedb.org.
      Default value *http://freedb.freedb.org* *)
    property Server             : string read FServer write FServer;
  end;

const
  CDDB_CommandTemplate = '{S}/~cddb/cddb.cgi?cmd=cddb+{C}+{D}&hello={E}+' + #34
    + '{N}' + #34 + '{V}&proto=5';
  CDDB_FAILED = -2;
  CDDB_NO_INTERNET = -1;
  CDDB_EXACT_MATCH = 200; //	Found exact match
  CDDB_NO_MATCH	= 202; //	No match found
  CDDB_ENTRY_OK = 210;  //	OK, CDDB database entry follows (until terminating marker)
  CDDB_INEXACT_MATCH = 211;  //	Found inexact matches, list follows (until terminating marker)
  CDDB_DB_ENTRY_CORRUPT = 403; //	Database entry is corrupt
  CDDB_ENTRY_NOT_FOUND = 401; //	Specified CDDB entry not found.
  CDDB_SERVER_ERROR = 402; //	Server error.
  CDDB_NO_HANDSHAKE = 409; //	No handshake
  CDDB_COMMAND_ERROR = 500; // Command Syntax Error

  XMCD_DISCID = 'DISCID';
  XMCD_DTITLE = 'DTITLE';
  XMCD_DYEAR = 'DYEAR';
  XMCD_DGENRE = 'DGENRE';
  XMCD_TTITLE = 'TTITLE';
  XMCD_EXTD = 'EXTD';
  XMCD_EXTT = 'EXTT';
  XMCD_PLAYORDER = 'PLAYORDER';
  XMCDH_Generated = '# Generated';
  XMCDH_DiscLength = '# Disc length';
  XMCD_NO_INDEX = -1;

function XMCD_CompareString(aLine, aTag: string): boolean;
function XMCD_ExtractValue(aLine, aTag: string; aIndex: integer): string;
function CDDB_FoundState(const aState: integer): boolean;
function CDDB_ResultStateToString(const aState : integer) : string;

{$R CDDBInfo.res}

implementation

uses dialogs;

resourcestring
  rs_CDDB_FAILED           = 'Fehlgeschlagen!';
  rs_CDDB_NO_INTERNET      = 'Internetverbindung fehlgeschlagen!';
  rs_CDDB_EXACT_MATCH      = 'Exakte Übereinstimmung gefunden!';
  rs_CDDB_NO_MATCH	       = 'Keinen Eintrag in der Datenbank gefunden!';
  rs_CDDB_ENTRY_OK         = 'Eintrag Ok';
  rs_CDDB_INEXACT_MATCH    = 'Mehrere Übereinstimmungen gefunden!';
  rs_CDDB_DB_ENTRY_CORRUPT = 'Datenbank Eintrag ist defekt!';
  rs_CDDB_ENTRY_NOT_FOUND  = 'Keinen Eintrag gefunden';
  rs_CDDB_SERVER_ERROR     = 'freeDB Server Fehler!';
  rs_CDDB_NO_HANDSHAKE     = 'Keinen Handshake!';
  rs_CDDB_COMMAND_ERROR    = 'Kommando Syntax Fehler!';


//==============================================================================

function CDDB_FoundState(const aState: integer): boolean;
//==============================================================================
begin
  Result := (aState = CDDB_EXACT_MATCH) or (aState = CDDB_ENTRY_OK) or (aState =
    CDDB_INEXACT_MATCH);
end;
//==============================================================================


//==============================================================================

function CDDB_ResultStateToString(const aState : integer) : string;
//==============================================================================
begin
  case aState  of
    CDDB_FAILED           : Result := rs_CDDB_FAILED;
    CDDB_NO_INTERNET      : Result := rs_CDDB_NO_INTERNET;
    CDDB_EXACT_MATCH      : Result := rs_CDDB_EXACT_MATCH;
    CDDB_NO_MATCH         : Result := rs_CDDB_NO_MATCH;
    CDDB_ENTRY_OK         : Result := rs_CDDB_ENTRY_OK;
    CDDB_INEXACT_MATCH    : Result := rs_CDDB_INEXACT_MATCH;
    CDDB_DB_ENTRY_CORRUPT : Result := rs_CDDB_DB_ENTRY_CORRUPT;
    CDDB_ENTRY_NOT_FOUND  : Result := rs_CDDB_ENTRY_NOT_FOUND;
    CDDB_SERVER_ERROR     : Result := rs_CDDB_SERVER_ERROR;
    CDDB_NO_HANDSHAKE     : Result := rs_CDDB_NO_HANDSHAKE;
    CDDB_COMMAND_ERROR    : Result := rs_CDDB_COMMAND_ERROR;
  end;
end;
//==============================================================================


function XMCD_CompareString(aLine, aTag: string): boolean;
//==============================================================================
begin
  Result := (pos(aTag, aLine) = 1);
end;
//==============================================================================

function XMCD_ExtractValue(aLine, aTag: string; aIndex: integer): string;
//==============================================================================
begin
  Result := '';
  if (aIndex > -1) then
    aTag := aTag + IntToStr(aIndex);
  if XMCD_CompareString(aLine, aTag) then
    Result := Trim(copy(aLine, Length(aTag) + 2, Length(aLine)));
  Result := StringReplace(Result, '\n', #13#10, [rfReplaceAll]);
end;
//==============================================================================

constructor TCDDBQuery.create(AOwner: TComponent);
//==============================================================================
begin
  FOwner := aOwner;
  LogStr := TStringList.Create;
  AlbumStr := TStringList.Create;
end;
//==============================================================================

destructor TCDDBQuery.Destroy;
//==============================================================================
begin
  LogStr.Clear;
  FreeAndNil(LogStr);
  AlbumStr.Clear;
  FreeAndNil(AlbumStr);
  AlbumRec.Artist := nil;
  AlbumRec.Title := nil;
  AlbumRec.TrackExt := nil;
  inherited;
end;

//==============================================================================

function TCDDBQuery.BuildCommandStr(aCommand, aDiscID: string): string;
//==============================================================================
begin
  Result := StringReplace(CDDB_CommandTemplate, '{S}', CDDBInfo.Server, []);
  Result := StringReplace(Result, '{C}', aCommand, []);
  Result := StringReplace(Result, '{D}', StringReplace(aDiscID, ' ', '+',
    [rfReplaceAll]), []);
  Result := StringReplace(Result, '{E}', StringReplace(CDDBInfo.EMail, '@', '+',
    [rfReplaceAll]), []);
  Result := StringReplace(Result, '{N}', StringReplace(CDDBInfo.ApplicationName,
    ' ', '+', [rfReplaceAll]), []);
  Result := StringReplace(Result, '{V}', CDDBInfo.ApplicationVersion, []);
end;

//==============================================================================

function TCDDBQuery.XMCD_ExtractAlbums(aWEBResult: string): integer;
//==============================================================================
var
  i, j: integer;
  ServerResponseCode, tempStr: string;
begin
  Result := CDDB_FAILED;
  AlbumStr.Clear;
  LogStr.Text := aWEBResult;

  for i := 0 to LogStr.Count - 1 do
  begin
    ServerResponseCode := Copy(LogStr.Strings[i], 1, 3);
    Result := StrToIntDef(ServerResponseCode, CDDB_COMMAND_ERROR);
    // Exact match
    if Result = CDDB_EXACT_MATCH then
    begin
      tempStr := LogStr.Strings[i];
      delete(tempStr, 1, Length(ServerResponseCode) + 1);
      AlbumStr.Add(tempStr);
    end
      // Multiple matches
    else if (Result = CDDB_ENTRY_OK) or (Result = CDDB_INEXACT_MATCH) then
    begin
      j := i + 1;
      repeat
        AlbumStr.Add(LogStr.Strings[j]);
        inc(j);
      until LogStr.Strings[j][1] = '.';
      Break;
    end;
  end;
end;
//==============================================================================

function TCDDBQuery.GetTrackCount: Integer;
//==============================================================================
var
  i: integer;
begin
  Result := 0;
  for i := 0 to LogStr.Count - 1 do
  begin
    if XMCD_CompareString(LogStr.Strings[i], XMCD_TTITLE) then
      Result := Result + 1;
  end;
end;
//==============================================================================

procedure TCDDBQuery.XMCD_ExtractTrackList(aWEBResult: string);
//==============================================================================
var
  i, TrackIdx, ExtIdx, TrackCount: integer;
  s: string;
  //********************************************************
  function DivideString(Info: string; Part: Integer): string;
    //********************************************************
  var
    indexPart: Integer;
  begin
    indexPart := Pos('/', Info);
    if Pos('/', Info) > 0 then
    begin
      case Part of
        1: Result := Copy(Info, 1, indexPart - 2);
        2: Result := Copy(Info, Pos('/', Info) + 2, length(Info) - indexPart +
            2);
      end;
    end
    else
    begin
      case Part of
        1: Result := '';
        2: Result := Info;
      end;
    end;
  end;
  //********************************************************
begin
  LogStr.Text := aWEBResult;
//  showmessage(LogStr.text);

  AlbumRec.Artist   := nil;
  AlbumRec.Title    := nil;
  AlbumRec.TrackExt := nil;
  TrackCount := GetTrackCount;
  SetLength(AlbumRec.Artist, TrackCount);
  SetLength(AlbumRec.Title, TrackCount);
  SetLength(AlbumRec.TrackExt, TrackCount);
  AlbumRec.TrackCount := TrackCount;

  TrackIdx := 0;
  ExtIdx := 0;

  for i := 0 to LogStr.Count - 1 do
  begin
    s := LogStr.Strings[i];

    if XMCD_CompareString(s, XMCDH_Generated) then
      AlbumRec.Generated := XMCD_ExtractValue(s, XMCDH_Generated,
        XMCD_NO_INDEX);

    if XMCD_CompareString(s, XMCDH_DiscLength) then
      AlbumRec.DiscLength := XMCD_ExtractValue(s, XMCDH_DiscLength,
        XMCD_NO_INDEX);

    if XMCD_CompareString(s, XMCD_DTITLE) then
    begin
      AlbumRec.AlbumArtist := DivideString(XMCD_ExtractValue(s, XMCD_DTITLE,
        XMCD_NO_INDEX), 1);
      AlbumRec.Album := DivideString(XMCD_ExtractValue(s, XMCD_DTITLE,
        XMCD_NO_INDEX), 2);
    end;

    if XMCD_CompareString(s, XMCD_DYEAR) then
      AlbumRec.Year := XMCD_ExtractValue(s, XMCD_DYEAR, XMCD_NO_INDEX);

    if XMCD_CompareString(s, XMCD_DGENRE) then
      AlbumRec.Genre := XMCD_ExtractValue(s, XMCD_DGENRE, XMCD_NO_INDEX);

    if XMCD_CompareString(s, XMCD_TTITLE) then
    begin
      if Pos('/', s) > 0 then
      begin
        AlbumRec.Artist[TrackIdx] := DivideString(XMCD_ExtractValue(s,
          XMCD_TTITLE, TrackIdx), 1);
        AlbumRec.Title[TrackIdx] := DivideString(XMCD_ExtractValue(s,
          XMCD_TTITLE, TrackIdx), 2);
      end
      else
      begin
        AlbumRec.Artist[TrackIdx] := AlbumRec.AlbumArtist;
        AlbumRec.Title[TrackIdx] := XMCD_ExtractValue(s, XMCD_TTITLE, TrackIdx);
      end;
      inc(TrackIdx);
    end;

    if XMCD_CompareString(s, XMCD_EXTD) then
      AlbumRec.DiscExt := AlbumRec.DiscExt + XMCD_ExtractValue(s, XMCD_EXTD,
        XMCD_NO_INDEX);

    if XMCD_CompareString(s, XMCD_EXTT) then
    begin
      AlbumRec.TrackExt[ExtIdx] := XMCD_ExtractValue(s, XMCD_EXTT, ExtIdx);
      inc(ExtIdx);
    end;
  end;
end;
//==============================================================================

procedure TCDDBQuery.SplitAlbumStr(aAlbumStr: string;
  aAlbumTokens: TStrings);
//==============================================================================
begin
  aAlbumTokens.Clear;
  aAlbumTokens.Add(copy(aAlbumStr, 1, Pos(' ', aAlbumStr) - 1)); // Genre

  Delete(aAlbumStr, 1, Pos(' ', aAlbumStr));
  aAlbumTokens.Add(copy(aAlbumStr, 1, Pos(' ', aAlbumStr) - 1)); // Discid
  Delete(aAlbumStr, 1, Pos(' ', aAlbumStr));
  aAlbumTokens.Add(aAlbumStr); // Artist and Album
end;

//==============================================================================

function TCDDBQuery.QueryAlbums(aCDDB_ID, aCDDB_TrackInfo: string): integer;
//==============================================================================
var
  aWEBResult: string;
begin
  Result := CDDB_NO_INTERNET;

  FCDDB_ID := aCDDB_ID;
  FCDDB_TrackInfo := aCDDB_TrackInfo;

  if HTTPRead(BuildCommandStr('query', aCDDB_ID + ' ' + aCDDB_TrackInfo),
    aWEBResult) then
  begin
    Result := XMCD_ExtractAlbums(aWEBResult);
    if CDDB_FoundState(Result) and (AlbumStr.Count > 0) then
      QueryAlbumTracks(0);
  end;
end;
//==============================================================================

function ExtractCharSetFromPageContent(fBuffer: string): string;
//==============================================================================
{*------------------------------------------------------------------------------
  simple parser of the return from the page content query
  text/plain; charset=ISO-8859-1 ;
  should return  ISO-8859-1
-------------------------------------------------------------------------------}
const
  c_DELIM = ';';
  c_charset = 'charset=';
var
  CutBuffer: TStringList;
  posDelim: Integer;
  strEnum: string;
begin
  result := 'windows-1252';
  CutBuffer := TStringList.create;
  try
    // cut the response into lines inside cutbuffer string list
    while (fBuffer <> '') do
    begin
      posDelim := Pos(C_delim, fBuffer);
      if (posDelim > 0) then
      begin
        CutBuffer.Add(Trim(copy(fBuffer, 1, posDelim - 1)));
        Delete(fBuffer, 1, posDelim);
      end
      else
      begin
        CutBuffer.Add(Trim(fBuffer));
        fbuffer := '';
      end;
    end;
    // look for a line with charset
    for strEnum in CutBuffer do
      if (Pos(c_charset, strEnum) > 0) then
      begin
        Result := Copy(strEnum,
          Pos('=', strEnum) + 1,
          Length(strEnum));
      end;
  finally
    FreeAndNil(CutBuffer);
  end;
end;
//==============================================================================

function FromCharSetToCodePage(const fCharSet: string): Word;
//==============================================================================
{*-----------------------------------------------------------------------------
  this is a simple look up function from char set to code page number
  can be expanded of course
  -----------------------------------------------------------------------------}
begin
  Result := 20127; // US-ASCII by default
  if SameText(fCharSet, 'ISO-8859-1') then
    Result := 28591
  else if SameText(fCharSet, '') then
    Result := 0;
end;

//==============================================================================

function TCDDBQuery.HTTPRead(aURL: string; var aWEBResult: string): boolean;
//==============================================================================
const
  BufferSize = 16384;
var
  hSession,
    hURL: HInternet;
  Buffer: array[0..Pred(BufferSize)] of char;
  sAppName: string;
  {$IFDEF UNICODE}
  CodePage : DWORD;
  {$ENDIF}
  BufferLen: DWORD;
  strPageContent: TStringStream;

  {$IFDEF UNICODE}
  //***********************************************************************
  function GetContentTypeCodePage(const fhURL: HInternet): Word;
    //***********************************************************************

  var
    strContentType: string;
    Dummy: DWORD;
  begin
    Result := 20127; // US-ASCII by default
    BufferLen := 0;
    if not HttpQueryInfo(fhURL, HTTP_QUERY_CONTENT_TYPE, @buffer, BufferLen,
      Dummy) then
    begin
      if GetLastError() <> ERROR_INSUFFICIENT_BUFFER then
        Exit;
    end;
    SetLength(strContentType, BufferLen div SizeOf(Char));
    if not HttpQueryInfo(fhURL, HTTP_QUERY_CONTENT_TYPE,
      Pointer(strContentType),
      BufferLen, dummy) then
      Exit;
    SetLength(strContentType, BufferLen div SizeOf(Char));
    // parse out "charset=..." portion and translate its string name value into a
    // numeric codepage ID as needed.  Unfortunately, neither WinInet nor
    // Windows have built-in functionality for that lookup, so you have to do
    // it manually...
    strContentType := ExtractCharSetFromPageContent(strContentType);
    Result := FromCharSetToCodePage(strContentType);
  end;
  //***********************************************************************
  {$ENDIF}
begin
  Result := False;
  aWEBResult := '';

  sAppName := CDDBInfo.ApplicationName;
  hSession := InternetOpen(PChar(sAppName),
    INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  try
    hURL := InternetOpenURL(hSession, PChar(aURL), nil, 0, 0, 0);
    try
      if (hURL <> nil) then
      begin
        {$IFDEF UNICODE}
        CodePage := GetContentTypeCodePage(hURL);
        strPageContent := TStringStream.Create('', CodePage);
        {$ELSE}
        strPageContent := TStringStream.Create('');
        {$ENDIF}

        try
          repeat
            if not InternetReadFile(hURL, @Buffer[0],
              SizeOf(Buffer), BufferLen) then
              Exit;
            if (BufferLen = 0) then
              Break;
            strPageContent.WriteBuffer(Buffer[0], BufferLen);
          until False;
          aWEBResult := strPageContent.DataString;
          Result := True;
        finally
          FreeAndNil(strPageContent);
        end;
      end;
    finally
      InternetCloseHandle(hURL)
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;
//==============================================================================

function TCDDBQuery.CDDBInfo: TCDDBInfo;
//==============================================================================
begin
  Result := TCDDBInfo(Self.FOwner);
end;
//==============================================================================

function TCDDBQuery.QueryAlbumTracks(aAlbumIndex: integer): integer;
//==============================================================================
var
  sl: TStringList;
  aWEBResult: string;
begin
  Result := CDDB_FAILED;

  if (AlbumStr.Count > 0) and (aAlbumIndex in [0..AlbumStr.Count - 1]) then
  begin
    sl := TStringList.Create;
    try
      SplitAlbumStr(AlbumStr.Strings[aAlbumIndex], sl);
      if HTTPRead(BuildCommandStr('read', sl.Strings[0] + ' ' + sl.Strings[1]),
        aWEBResult) then
      begin
        Result := CDDB_ENTRY_OK;
        XMCD_ExtractTrackList(aWEBResult);
      end;
    finally
      sl.Clear;
      sl.Free;
    end;
  end;
end;

{ TCDDBInfo }
//==============================================================================

function TCDDBInfo.GetCDDB_ID: string;
//==============================================================================
var
  i, en, DiskID, TrackID: integer;
  TE: TTOCENTRY;
  PreTrack1, PreTrack2: Integer;
  PreHex: DWord;
  //***************************************************
  function LBA2PreCDDB(LBA: Integer): Integer;
    //***************************************************
  var
    M, S, Start: Integer;
  begin
    Start := 150 + LBA;
    S := (Start div 75) mod 60;
    M := (Start div 75) div 60;
    Result := ((M * 60) + S);
  end;
  //***************************************************
  function CDDB_Sum(N: Integer): Integer;
    //***************************************************
  var
    Ret: Integer;
  begin
    Ret := 0;
    while (N > 0) do
    begin
      Ret := Ret + (N mod 10);
      N := N div 10;
    end;
    Result := Ret;
  end;
  //***************************************************
begin
  Result := 'ffffffff';

  if not Assigned(FCDIn) then
    raise EAuException.Create('CDIn is not assigned');

  if not (csDesigning in ComponentState) then
  begin
    OpenCD;
    if CR_IsMediaLoaded(en) = RES_OK then
    begin
      if en <> 0 then
      begin
        Exit;
      end;
    end;
    if CR_ReadToc <> RES_OK then
      Exit;

    en := CR_GetNumTocEntries;
    TrackID := 0;
    for i := 0 to en - 1 do
    begin
      TE := GetTOCEntry(i);
      TrackID := TrackID + CDDB_Sum(LBA2PreCDDB(TE.dwStartSector));
    end;

    //size of the disc in Frames
    TE := GetTOCEntry(en); // Leadout Track
    PreTrack1 := LBA2PreCDDB(TE.dwStartSector);
    TE := GetTOCEntry(0);
    PreTrack2 := LBA2PreCDDB(TE.dwStartSector);
    DiskID := (PreTrack1 - PreTrack2);

    // Create CDDB ID
    TrackID := (TrackID mod $FF);
    TrackID := TrackID shl 24;
    DiskID := DiskID shl 8;
    PreHex := TrackID or DiskID or (en);
    Result := LowerCase(IntToHex(PreHex, 8)); //a70ce90d

    CloseCD;
  end;
end;
//==============================================================================

function TCDDBInfo.GetCDDB_TrackInfo: string;
//==============================================================================
var
  i, en: integer;
  TE: TTOCENTRY;
  PreTrack1, PreTrack2: Integer;
begin
  Result := '';

  if not Assigned(FCDIn) then
    raise EAuException.Create('CDIn is not assigned');

  if not (csDesigning in ComponentState) then
  begin
    OpenCD;
    if CR_IsMediaLoaded(en) = RES_OK then
    begin
      if en <> 0 then
      begin
        Exit;
      end;
    end;
    if CR_ReadToc <> RES_OK then
      Exit;

    en := CR_GetNumTocEntries;

    // Number of Tracks
    Result := IntToStr(en);
    for i := 0 to en - 1 do
    begin
      TE := GetTOCEntry(i);
      // StartSector of each Track
      Result := Result + ' ' + IntToStr(TE.dwStartSector);
    end;

    //size of the disc in Frames
    TE := GetTOCEntry(en); // Leadout Track
    PreTrack1 := TE.dwStartSector;
    TE := GetTOCEntry(0);
    PreTrack2 := TE.dwStartSector;

    // size of the disc in seconds (75 Frames per Second)
    Result := Result + ' ' + IntToStr((PreTrack1 - PreTrack2) div 75);

    CloseCD;
  end;
end;
//==============================================================================

procedure TCDDBInfo.CloseCD;
//==============================================================================
begin
  if FOpened > 0 then
    Dec(FOpened);
end;
//==============================================================================

constructor TCDDBInfo.Create(AOwner: TComponent);
//==============================================================================
begin
  inherited Create(AOwner);

  FCDIn := nil;
  FCDDBQuery := TCDDBQuery.Create(Self);

  FServer := 'http://Freedb.Freedb.org';
  FEmail := 'mymail@myhost.net';
  FApplicationName := 'NewAC';
  FApplicationVersion := 'v1.0';
end;
//==============================================================================

destructor TCDDBInfo.Destroy;
//==============================================================================
begin
  FCDDBQuery.Free;
  inherited Destroy;
end;
//==============================================================================

procedure TCDDBInfo.OpenCD;
//==============================================================================
begin
  if FOpened = 0 then
  begin
    CR_SetActiveCDROM(FCDIn.CurrentDrive);
  end;
  Inc(FOpened);
end;
//==============================================================================

function TCDDBInfo.QueryAlbums: integer;
//==============================================================================
begin
  FCDDB_ID := GetCDDB_ID;
  FCDDB_TrackInfo := GetCDDB_TrackInfo;

  Result := FCDDBQuery.QueryAlbums(FCDDB_ID, FCDDB_TrackInfo);
  FLastResult := Result;
end;
//==============================================================================

function TCDDBInfo.GetAlbums(Index: integer): string;
//==============================================================================
begin
  Result := '';
  if (GetAlbumCount > 0) and (Index in [0..GetAlbumCount - 1]) then
    Result := FCDDBQuery.AlbumStr.Strings[Index];
end;
//==============================================================================

function TCDDBInfo.GetAlbumCount: integer;
//==============================================================================
begin
  Result := FCDDBQuery.AlbumStr.Count;
end;
//==============================================================================

function TCDDBInfo.QueryAlbumTracks(aAlbumIndex: integer): integer;
//==============================================================================
begin
  if GetAlbumCount = 0 then
    QueryAlbums;

  Result := FCDDBQuery.QueryAlbumTracks(aAlbumIndex);
  FLastResult := Result;
end;
//==============================================================================

function TCDDBInfo.GetAlbum: TCDDBAlbum;
//==============================================================================
begin
  Result := FCDDBQuery.AlbumRec;
end;

end.

