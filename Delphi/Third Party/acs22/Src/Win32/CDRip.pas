(*
  Delphi header for CDRip.dll.
  Translated from CDRip.h header by Andrei Borovsky, kylixportal@narod.ru
  The original C/C++ header and library
  Copyright (C) 1999 - 2002 Albert Faber

  Note by A.B.:
  Special thanks to Thomas Grelle <grelle@online.de> for improving this Pascal unit.
*)

unit CDRip;

interface

uses
  Windows, sysutils, INIFiles, Registry;

const
  CDRipPath = 'cdrip.dll';

  RES_OK = $0;
  RES_ERROR = $1;
  RES_FILEOPEN_ERROR = $2;
  RES_JITTER_ERROR	= $3;
  RES_RIPPING_DONE = $4;
  RES_RIPPING_INPROGRESS = $5;
  RES_FILEWRITE_ERROR = $6;
  RES_OUTOFMEMORY = $7;
  RES_NOCDROMDEVICES = $8;
  RES_FAILEDTOLOADASPIDRIVERS = $9;
  RES_NATIVEEASPINOTSUPPORTED = $a;
  RES_FAILEDTOGETASPISTATUS = $b;

  HASTAT_OK = $0;               // Host adapter did not detect an error.
  HASTAT_TIMEOUT = $9;          // The time allocated for a bus transaction ran out.
  HASTAT_COMMAND_TIMEOUT = $b;  // SRB expired while waiting to be processed.
  HASTAT_MESSAGE_REJECT	= $d;   // MESSAGE REJECT received while processing SRB.
  HASTAT_BUS_RESET = $e;        // A bus reset was detected.
  HASTAT_PARITY_ERROR = $f;     // A parity error was detected.
  HASTAT_REQUEST_SENSE_FAILED = $10;     // The adapter failed in issuing a Request Sense after a check condition was reported by the target device.
  HASTAT_SEL_TO	= $11;          // Selection of target timed out.
  HASTAT_DO_DU = $12;   	// Data overrun.
  HASTAT_BUS_FREE = $13;        // Unexpected Bus Free.
  HASTAT_PHASE_ERR = $14;       // Target Bus phase sequence failure.

  STATUS_GOOD =	$0;             // No target status.
  STATUS_CHKCOND = $2;          // Check status (sense data is in SenseArea).
  STATUS_BUSY = $8;             // Specified Target/LUN is busy.
  STATUS_RESCONF = $18;         // Reservation conflict.

  TRANSPLAYER_ASPI = 0;
  TRANSPLAYER_NTSCSI = 1;

  CDROMDATAFLAG  = $4;
  AUDIOTRKFLAG = $10;

  CR_RIPPING_MODE_NORMAL = 0;
  CR_RIPPING_MODE_PARANOIA = 1;

  CDMEDIA_PRESENT = 0;
  CDMEDIA_NOT_PRESENT = 1;
  CDMEDIA_NOT_PRESENT_TRAY_OPEN = 2;
  CDMEDIA_NOT_PRESENT_TRAY_CLOSED = 3;

type

  TOSVer = (WIN31, WIN9x, WINNT);

  TRES_ERR = Integer;

  PCDSTATUSINFO = ^TCDSTATUSINFO;
  TCDSTATUSINFO = record
    sk : Byte;
    asc : Byte;
    ascq : Byte;
    ha_stat : Byte;
    target_stat : Byte;
  end;

  PDRIVETABLE = ^TDRIVETABLE;
  TDRIVETABLE = record
    DriveType : Integer; //TDRIVETYPE;
    ReadMethod : Integer; //TREADMETHOD;
    SetSpeed : Integer; //TSETSPEED;
    Endian : Integer; //TENDIAN;
    EnableMode : Integer; //TENABLEMODE;
    nDensity : Integer;
    bAtapi : BOOL;
  end;

  TOUTPUTFORMAT = (STEREO44100,	MONO44100, STEREO22050, MONO22050, STEREO11025,
	           MONO11025, NUMOUTPUTFORMATS);

  PCDROMPARAMS = ^TCDROMPARAMS;
  TCDROMPARAMS = record
    lpszCDROMID : array [1..255] of Char;	// CD-ROM ID, must be unique to index settings in INI file
    nNumReadSectors : Integer;           	// Number of sector to read per burst
    nNumOverlapSectors : Integer;        	// Number of overlap sectors for jitter correction
    nNumCompareSectors : Integer;        	// Number of sector to compare for jitter correction
    nOffsetStart : Integer;                     // Fudge factor at start of ripping in sectors
    nOffsetEnd : Integer;       	     	// Fudge factor at the end of ripping in sectors
    nSpeed : Integer;   	       		// CD-ROM speed factor 0 .. 32 x
    nSpinUpTime : Integer;			// CD-ROM spin up time in seconds
    bJitterCorrection : BOOL;	        	// Boolean indicates whether to use Jitter Correction
    bSwapLefRightChannel : BOOL;     	        // Swap left and right channel ?
    DriveTable : TDRIVETABLE;			// Drive specific parameters

    btTargetID : Byte;				// SCSI target ID
    btAdapterID : Byte; 			// SCSI Adapter ID
    btLunID : Byte;				// SCSI LUN ID

    bAspiPosting : BOOL;			// When set ASPI posting is used, otherwhiese ASPI polling is used
    nAspiRetries : Integer;
    nAspiTimeOut : Integer;

    bEnableMultiRead : BOOL;                 // Enables Multiple Read Verify Feature
    bMultiReadFirstOnly : BOOL;              // Only do the multiple reads on the first block
    nMultiReadCount : Integer;               // Number of times to reread and compare

    bLockDuringRead :  BOOL;

    nRippingMode : Integer;
    nParanoiaMode : Integer;

    bUseCDText : BOOL;			// Read CD Text info?
  end;


// Table of contents structure

  PTOCENTRY  = ^TTOCENTRY;
  TTOCENTRY = record
     dwStartSector : Integer;		// Start sector of the track
     btFlag : Byte;			// Track flags (i.e. data or audio track)
     btTrackNumber : Byte;		// Track number
   end;

// Function pointers

  // Call init before anything else
  CR_Init_t = function(strIniFname : PChar) : TRES_ERR; stdcall;

  // Call DeIni when ripping library is no longer needed
  CR_DeInit_t = function : TRES_ERR; stdcall;

  // Get the DLL version number
  CR_GetCDRipVersion_t = function : Integer; stdcall;

  // Get the number of detected CD-ROM drives
  CR_GetNumCDROM_t = function : Integer; stdcall;

  // Get the active CDROM drive index (0..GetNumCDROM()-1 )
  CR_GetActiveCDROM_t = function : Integer; stdcall;

  // Set the active CDROM drive (0..GetNumCDROM()-1 )
  CR_SetActiveCDROM_t = procedure(nActiveDrive : Integer); stdcall;

  // Setlect the DRIVETYPE of the active drive
  CR_SelectCDROMType_t = function(cdType : Integer) : TRES_ERR; stdcall;

  // Get the Selected CDROM type
  CR_GetCDROMType_t = function : Integer; stdcall;

  // Get the CDROM parameters of the active drive
  CR_GetCDROMParameters_t = function(pParam : PCDROMPARAMS) : TRES_ERR; stdcall;

  // Set the CDROM parameters of the active drive
  CR_SetCDROMParameters_t = function(pParam : PCDROMPARAMS) : TRES_ERR; stdcall;

  (* Start ripping section, output is fetched to WriteBufferFunc
     Data is extracted from dwStartSector to dwEndSector *)
  CR_OpenRipper_t = function(plBufferSize : PInteger; dwStartSector, dwEndSector : Integer) : TRES_ERR; stdcall;

  (* Close the ripper, has to be called when the ripping process is completed (i.e 100%)
     Or it can be called to abort the current ripping section *)
  CR_CloseRipper_t = function : TRES_ERR; stdcall;

  (* Indicates how far the ripping process is right now
     Returns 100% when the ripping is completed *)
  CR_GetPercentCompleted_t = function : Integer; stdcall;

  // Returns the peak value of the ripped section (0..2^15)
  CR_GetPeakValue_t = function : Integer; stdcall;

  (* Get number of Jitter Errors that have occured during the ripping
     This function must be called before CloseRipper is called ! *)
  CR_GetNumberOfJitterErrors_t = function : Integer; stdcall;

  // Get the jitter position of the extracted track
  CR_GetJitterPosition_t = function : Integer; stdcall;

  (* Rip a chunk from the CD, pbtStream contains the ripped data, pNumBytes the
     number of bytes that have been ripped and corrected for jitter (if enabled) *)
  CR_RipChunk_t = function(pbtStream : PByte; pNumBytes : PInteger;
                  var bAbort  : BOOL) : TRES_ERR; stdcall;

  // Load the CD-ROM settings from the file
  CR_LoadSettings_t = function : TRES_ERR; stdcall;

  // Save the settings to a INI file
  CR_SaveSettings_t = function : TRES_ERR; stdcall;

  // Normalize the stream (i.e. multiply by dScaleFactor)
  CR_NormalizeChunk_t = procedure(pbsStream : PShortInt; nNumSamples :Integer; dScaleFactor : Double); stdcall;

  // Read the table of contents
  CR_ReadToc_t = function : TRES_ERR; stdcall;

  // Read CD Text entry
  CR_ReadCDText_t = function(pbtBuffer : PByte; nBufferSize : Integer; var pnCDTextSize : Integer) : TRES_ERR; stdcall;

  // Get the number of TOC entries, including the lead out
  CR_GetNumTocEntries_t = function : Integer; stdcall;

  // Get the TOC entry
  CR_GetTocEntry_t = function(nTocEntry : Integer) : Int64 {TTOCENTRY}; stdcall;

  // Checks if the unit is ready (i.e. is the CD media present)
  CR_IsUnitReady_t = function : BOOL; stdcall;

  // Checks if the Media is loaded
  CR_IsMediaLoaded_t = function(var IsMediaLoaded : Integer) : TRES_ERR; stdcall;

  // Eject the CD, bEject=TRUE=> the CD will be ejected, bEject=FALSE=> the CD will be loaded
  CR_EjectCD_t = function(bEject : BOOL) : BOOL; stdcall;

  // Check if the CD is playing
  CR_IsAudioPlaying_t = function : BOOL; stdcall;

  // Play track
  CR_PlayTrack_t = function(nTrack : Integer) : TRES_ERR; stdcall;

  // Stop Play track
  CR_StopPlayTrack_t = function : TRES_ERR; stdcall;

  // Pause Play track
  CR_PauseCD_t = function(bPause : BOOL) : TRES_ERR; stdcall;

  // Get debug information
  CR_GetCDStatusInfo_t = function : TCDSTATUSINFO; stdcall;

  // Lock/unlock the CD Tray
  CR_LockCD_t = procedure(bLock : BOOL); stdcall;

  CR_GetSubChannelTrackInfo_t = procedure(nReadIndex, nReadTrack : PInteger;
                                          dwReadPos : PLongInt); stdcall;

  // Get status of audio playing
  CR_GetPlayPosition_t = function(dwRelPos, dwAbsPos : PLongInt) : TRES_ERR; stdcall;

  // Set the audio play position
  CR_SetPlayPosition_t = function(dwAbsPos : LongInt) : TRES_ERR; stdcall;

  CR_PlaySection_t = function(lStartSector, lEndSector : Integer) : TRES_ERR; stdcall;

  CR_GetLastJitterErrorPosition_t = procedure(dwStartSector, dwEndSector : PLongInt); stdcall;

  CR_GetCurrentRipSector_t = function : Integer; stdcall;

  (* Change transport layer, DLL has to be re-initialzed when changing the transport layer!
   0 = ASPI drivers
   1 = Native NT scsi drivers *)

  CR_SetTransportLayer_t = procedure(nTransportLayer : Integer); stdcall;
  CR_GetTransportLayer_t = function : Integer; stdcall;

  CR_ScanForC2Errors_t = function(dwStartSector, dwNumSectors : LongInt;
                                  dwErrors, pdwErrorSectors : PLongInt) : TRES_ERR; stdcall;

var

  CDRIni:TIniFile;
  CDRipLoaded : Boolean = False;

  CR_Init : CR_Init_t;
  CR_DeInit : CR_DeInit_t;
  CR_GetCDRipVersion : CR_GetCDRipVersion_t;
  CR_GetNumCDROM : CR_GetNumCDROM_t;
  CR_GetActiveCDROM : CR_GetActiveCDROM_t;
  CR_SetActiveCDROM : CR_SetActiveCDROM_t;
  CR_SelectCDROMType : CR_SelectCDROMType_t;
  CR_GetCDROMType : CR_GetCDROMType_t;
  CR_GetCDROMParameters : CR_GetCDROMParameters_t;
  CR_SetCDROMParameters : CR_SetCDROMParameters_t;
  CR_OpenRipper : CR_OpenRipper_t;
  CR_CloseRipper : CR_CloseRipper_t;
  CR_GetPercentCompleted : CR_GetPercentCompleted_t;
  CR_GetPeakValue : CR_GetPeakValue_t;
  CR_GetNumberOfJitterErrors : CR_GetNumberOfJitterErrors_t;
  CR_GetJitterPosition : CR_GetJitterPosition_t;
  CR_RipChunk : CR_RipChunk_t;
  CR_LoadSettings : CR_LoadSettings_t;
  CR_SaveSettings : CR_SaveSettings_t;
  CR_NormalizeChunk : CR_NormalizeChunk_t;
  CR_ReadToc : CR_ReadToc_t;
  CR_ReadCDText : CR_ReadCDText_t;
  CR_GetNumTocEntries : CR_GetNumTocEntries_t;
  CR_GetTocEntry : CR_GetTocEntry_t;
  CR_IsUnitReady : CR_IsUnitReady_t;
  CR_IsMediaLoaded : CR_IsMediaLoaded_t;
  CR_EjectCD : CR_EjectCD_t;
  CR_IsAudioPlaying : CR_IsAudioPlaying_t;
  CR_PlayTrack : CR_PlayTrack_t;
  CR_StopPlayTrack : CR_StopPlayTrack_t;
  CR_PauseCD : CR_PauseCD_t;
  CR_GetCDStatusInfo : CR_GetCDStatusInfo_t;
  CR_LockCD : CR_LockCD_t;
  CR_GetSubChannelTrackInfo : CR_GetSubChannelTrackInfo_t;
  CR_GetPlayPosition : CR_GetPlayPosition_t;
  CR_SetPlayPosition : CR_SetPlayPosition_t;
  CR_PlaySection : CR_PlaySection_t;
  CR_GetLastJitterErrorPosition : CR_GetLastJitterErrorPosition_t;
  CR_GetCurrentRipSector : CR_GetCurrentRipSector_t;
  CR_SetTransportLayer : CR_SetTransportLayer_t;
  CR_GetTransportLayer : CR_GetTransportLayer_t;
  CR_ScanForC2Errors : CR_ScanForC2Errors_t;

  (* Note by A.B.: this function is a Delphi wrapper around a CDRip function
    CR_GetTocEntry *)
  function GetTOCEntry(n : Integer) : TTOCENTRY;

  // Note by A.B.: this function is modified by Thomas Grelle
  procedure CDRIPInit(FilePath:String);

  // Note by A.B.: this function is added by Thomas Grelle
  function GetWindowsVersion: TOSVer;

implementation

var
  Libhandle : HMODULE;

function GetWindowsVersion: TOSVer;
var
  VerInfo: TOsversionInfo;
  PlatformId, VersionNumber: string;
  Reg: TRegistry;
begin
  VerInfo.dwOSVersionInfoSize := SizeOf(VerInfo);
  GetVersionEx(VerInfo);
  Reg := TRegistry.Create;
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  case VerInfo.dwPlatformId of
    VER_PLATFORM_WIN32s : Result := WIN31;
    VER_PLATFORM_WIN32_WINDOWS : Result := WIN9x;
    VER_PLATFORM_WIN32_NT : Result := WINNT;
  end;
  Reg.Free;
end;


  function GetTOCEntry(n : Integer) : TTOCENTRY;
  var
    r : int64;
    t : Word;
  begin
    r := CR_GetTocEntry(n);
    Result.dwStartSector := Integer(r);
    t := r shr 32;
    Result.btFlag := Lo(t);
    Result.btTrackNumber := Hi(t);
  end;

  procedure CDRIPInit(FilePath:String);
  begin
    Libhandle := LoadLibraryEx(CDRipPath, 0, 0);
    if Libhandle <> 0 then
    begin
      CDRipLoaded := True;

      CR_Init := GetProcAddress(Libhandle, 'CR_Init');
      CR_DeInit := GetProcAddress(Libhandle, 'CR_DeInit');
      CR_GetCDRipVersion := GetProcAddress(Libhandle, 'CR_GetCDRipVersion');
      CR_GetNumCDROM := GetProcAddress(Libhandle, 'CR_GetNumCDROM');
      CR_GetActiveCDROM := GetProcAddress(Libhandle, 'CR_GetActiveCDROM');
      CR_SetActiveCDROM := GetProcAddress(Libhandle, 'CR_SetActiveCDROM');
      CR_SelectCDROMType := GetProcAddress(Libhandle, 'CR_SelectCDROMType');
      CR_GetCDROMType := GetProcAddress(Libhandle, 'CR_GetCDROMType');
      CR_GetCDROMParameters := GetProcAddress(Libhandle, 'CR_GetCDROMParameters');
      CR_SetCDROMParameters := GetProcAddress(Libhandle, 'CR_SetCDROMParameters');
      CR_OpenRipper := GetProcAddress(Libhandle, 'CR_OpenRipper');
      CR_CloseRipper := GetProcAddress(Libhandle, 'CR_CloseRipper');
      CR_GetPercentCompleted := GetProcAddress(Libhandle, 'CR_GetPercentCompleted');
      CR_GetPeakValue := GetProcAddress(Libhandle, 'CR_GetPeakValue');
      CR_GetNumberOfJitterErrors := GetProcAddress(Libhandle, 'CR_GetNumberOfJitterErrors');
      CR_GetJitterPosition := GetProcAddress(Libhandle, 'CR_GetJitterPosition');
      CR_RipChunk := GetProcAddress(Libhandle, 'CR_RipChunk');
      CR_LoadSettings := GetProcAddress(Libhandle, 'CR_LoadSettings');
      CR_SaveSettings := GetProcAddress(Libhandle, 'CR_SaveSettings');
      CR_NormalizeChunk := GetProcAddress(Libhandle, 'CR_NormalizeChunk');
      CR_ReadToc := GetProcAddress(Libhandle, 'CR_ReadToc');
      CR_ReadCDText := GetProcAddress(Libhandle, 'CR_ReadCDText');
      CR_GetNumTocEntries := GetProcAddress(Libhandle, 'CR_GetNumTocEntries');
      CR_GetTocEntry := GetProcAddress(Libhandle, 'CR_GetTocEntry');
      CR_IsUnitReady := GetProcAddress(Libhandle, 'CR_IsUnitReady');
      CR_IsMediaLoaded := GetProcAddress(Libhandle, 'CR_IsMediaLoaded');
      CR_EjectCD := GetProcAddress(Libhandle, 'CR_EjectCD');
      CR_IsAudioPlaying := GetProcAddress(Libhandle, 'CR_IsAudioPlaying');
      CR_PlayTrack := GetProcAddress(Libhandle, 'CR_PlayTrack');
      CR_StopPlayTrack := GetProcAddress(Libhandle, 'CR_StopPlayTrack');
      CR_PauseCD := GetProcAddress(Libhandle, 'CR_PauseCD');
      CR_GetCDStatusInfo := GetProcAddress(Libhandle, 'CR_GetCDStatusInfo');
      CR_LockCD := GetProcAddress(Libhandle, 'CR_LockCD');
      CR_GetSubChannelTrackInfo := GetProcAddress(Libhandle, 'CR_GetSubChannelTrackInfo');
      CR_GetPlayPosition := GetProcAddress(Libhandle, 'CR_GetPlayPosition');
      CR_SetPlayPosition := GetProcAddress(Libhandle, 'CR_SetPlayPosition');
      CR_PlaySection := GetProcAddress(Libhandle, 'CR_PlaySection');
      CR_GetLastJitterErrorPosition := GetProcAddress(Libhandle, 'CR_GetLastJitterErrorPosition');
      CR_GetCurrentRipSector := GetProcAddress(Libhandle, 'CR_GetCurrentRipSector');
      CR_SetTransportLayer := GetProcAddress(Libhandle, 'CR_SetTransportLayer');
      CR_GetTransportLayer := GetProcAddress(Libhandle, 'CR_GetTransportLayer');
      CR_ScanForC2Errors := GetProcAddress(Libhandle, 'CR_ScanForC2Errors');

      CDRIni := TINIFile.Create(FilePath+'cdr.ini');
      CDRIni.WriteString('CD-ROM','nActive','0');
      case GetWindowsVersion of
        WIN9x : CDRIni.WriteInteger('CD-ROM','nTransportLayer',0);
        WINNT : CDRIni.WriteInteger('CD-ROM','nTransportLayer',1);
      end;
      CDRIni.Free;
      CR_Init(PChar(FilePath+'cdr.ini'));
    end;
  end;

initialization

finalization

  if Libhandle <> 0 then
  begin
    CR_DeInit;
    FreeLibrary(Libhandle);
  end;


end.
