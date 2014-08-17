unit lame;

interface

uses
    Windows;

const

  LAME_PATH = 'lame_enc.dll';

  BE_MP3_MODE_STEREO = 0;
  BE_MP3_MODE_JSTEREO	= 1;
  BE_MP3_MODE_DUALCHANNEL	= 2;
  BE_MP3_MODE_MONO = 3;

type

  BOOL = LongBool;

  BE_CONFIG = packed record
      dwConfig : LongWord;       // 0
      case Integer of
       0: (dwSampleRate : LongWord;		// 48000, 44100 and 32000 allowed
  			byMode : BYTE;			// BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
	  		wBitrate : WORD;		// 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256 and 320 allowed
		  	bPrivate : BOOL;
		    bCRC : BOOL;
  			bCopyright : BOOL;
	  		bOriginal : BOOL);
      1: (
      			dwStructVersion : LongWord;
			  		dwStructSize : LongWord;
			// BASIC ENCODER SETTINGS
			  		dwSampleRate1 : LongWord;		// SAMPLERATE OF INPUT FILE
			  		dwReSampleRate : LongWord;		// DOWNSAMPLERATE, 0=ENCODER DECIDES
			      nMode : LongWord;				// BE_MP3_MODE_STEREO, BE_MP3_MODE_DUALCHANNEL, BE_MP3_MODE_MONO
	 	  		  dwBitrate : LongWord;			// CBR bitrate, VBR min bitrate
			 			dwMaxBitrate : LongWord;		// CBR ignored, VBR Max bitrate
						nPreset : LongWord;			// Quality preset, use one of the settings of the LAME_QUALITY_PRESET enum
			 			dwMpegVersion  : LongWord;		// FUTURE USE, MPEG-1 OR MPEG-2
			 			dwPsyModel : LongWord;			// FUTURE USE, SET TO 0
			 			dwEmphasis : LongWord;			// FUTURE USE, SET TO 0

			// BIT STREAM SETTINGS
  					bPrivate1 : BOOL;			// Set Private Bit (TRUE/FALSE)
	  		    bCRC1 : BOOL;				// Insert CRC (TRUE/FALSE)
		  			bCopyright1 : BOOL;			// Set Copyright Bit (TRUE/FALSE)
			  		bOriginal1 : BOOL;			// Set Original Bit (TRUE/FALSE)

			// VBR STUFF
     				bWriteVBRHeader : BOOL;	// WRITE XING VBR HEADER (TRUE/FALSE)
						bEnableVBR : BOOL;			// USE VBR ENCODING (TRUE/FALSE)
						nVBRQuality : LongWord;		// VBR QUALITY 0..9
			  		dwVbrAbr_bps : LongWord;		// Use ABR in stead of nVBRQuality
      			nVbrMethod : Integer;
						bNoRes : BOOL;				// Disable Bit resorvoir (TRUE/FALSE)

			// MISC SETTINGS
			      bStrictIso : BOOL;			// Use strict ISO encoding rules (TRUE/FALSE)
						nQuality : WORD;			// Quality Setting, HIGH BYTE should be NOT LOW byte, otherwhise quality=5

					  btReserved : array[0..236] of Byte;

			);
	 end;

   PBE_CONFIG = ^BE_CONFIG;

   HBE_STREAM = LongWord;

  beInitStream_t = function(pbeConfig : PBE_CONFIG; var dwSamples, dwBufferSize : LongWord; var phbeStream : HBE_STREAM) : Integer; cdecl;
  beEncodeChunk_t = function(hbeStream : HBE_STREAM; nSamples : LongWord; pSamples : PSMallInt; pOutput : PBYTE; var pdwOutput : LongWord) : Integer; cdecl;
  beDeinitStream_t = function(hbeStream : HBE_STREAM; pOutput : PBYTE; var pdwOutput : LongWord) : Integer; cdecl;
  beCloseStream_t = function(hbeStream : HBE_STREAM) : Integer; cdecl;
  beWriteVBRHeader_t = function(const lpszFileName : PAnsiChar) : Integer; cdecl;
  beFlushNoGap_t = function(hbeStream : HBE_STREAM; pOutput : PBYTE; var pdwOutput : LongWord) : Integer; cdecl;
  beWriteInfoTag_t = function(hbeStream : HBE_STREAM; const lpszFileName : PChar) : Integer; cdecl;

var
  LAMELoaded : Boolean = False;
  beInitStream : beInitStream_t;
  beEncodeChunk : beEncodeChunk_t;
  beDeinitStream : beDeinitStream_t;
  beCloseStream : beCloseStream_t;
  beWriteVBRHeader : beWriteVBRHeader_t;
  beFlushNoGap : beFlushNoGap_t;
  beWriteInfoTag : beWriteInfoTag_t;

  procedure LoadLAME;
  procedure UnloadLAME;

implementation
var
 Libhandle : HMODULE = 0;

procedure LoadLAME;
begin
  if LAMELoaded then Exit;
  Libhandle := LoadLibraryEx(LAME_PATH, 0, 0);
  if Libhandle <> 0 then
  begin
    LAMELoaded := True;
    beInitStream := GetProcAddress(Libhandle, 'beInitStream');
    beEncodeChunk := GetProcAddress(Libhandle, 'beEncodeChunk');
    beDeinitStream := GetProcAddress(Libhandle, 'beDeinitStream');
    beCloseStream := GetProcAddress(Libhandle, 'beCloseStream');
    beWriteVBRHeader := GetProcAddress(Libhandle, 'beWriteVBRHeader');
    beFlushNoGap := GetProcAddress(Libhandle, 'beFlushNoGap');
    beWriteInfoTag := GetProcAddress(Libhandle, 'beWriteInfoTag');
  end;
end;


procedure UnloadLAME;
begin
  if not LAMELoaded then Exit;
  LAMELoaded := False;
  if Libhandle <> 0 then FreeLibrary(Libhandle);
end;


end.
