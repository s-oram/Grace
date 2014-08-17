(*
  This unit contains fragmets from Windows Media SDK header files
  relevant to encoding/decoding audio.
*)

unit wmfintf;

(* Unit: wmfintf.pas
    Delphi headers for Windows Media related stuff. Contains the bits from the 
    Windows Media SDK header files relevant to encoding/decoding data. *)

interface
uses
  Windows, ActiveX, _DirectSound, MMSystem;

const

  IID_IWMReader                 : TGUID = '{96406bd6-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMReader}
  IID_IWMSyncReader             : TGUID = '{9397f121-7705-4dc9-b049-98b698188414}';
  {$EXTERNALSYM IID_IWMSyncReader}
  IID_IWMOutputMediaProps       : TGUID = '{96406bd7-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMOutputMediaProps}
  IID_IWMHeaderInfo             : TGUID = '{96406bda-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMHeaderInfo}
  IID_IWMWriterFileSink         : TGUID = '{96406be5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterFileSink}
  IID_IWMWriter                 : TGUID = '{96406bd4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriter}
  IID_IWMWriterSink             : TGUID = '{96406be4-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterSink}
  IID_IWMWriterAdvanced         : TGUID = '{96406be3-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterAdvanced}
  IID_IWMProfile                : TGUID = '{96406bdb-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMProfile}
  IID_IWMProfileManager         : TGUID = '{d16679f2-6ca0-472d-8d31-2f5d55aee155}';
  {$EXTERNALSYM IID_IWMProfileManager}
  IID_IWMInputMediaProps        : TGUID = '{96406bd5-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMInputMediaProps}
  IID_IWMCodecInfo              : TGUID = '{a970f41e-34de-4a98-b3ba-e4b3ca7528f0}';
  {$EXTERNALSYM IID_IWMCodecInfo}
  IID_IWMCodecInfo2             : TGUID = '{aa65e273-b686-4056-91ec-dd768d4df710}';
  {$EXTERNALSYM IID_IWMCodecInfo2}
  IID_IWMCodecInfo3             : TGUID = '{7e51f487-4d93-4f98-8ab4-27d0565adc51}';
  {$EXTERNALSYM IID_IWMCodecInfo3}
  IID_IWMWriterNetworkSink      : TGUID = '{96406be7-2b2b-11d3-b36b-00c04f6108ff}';
  {$EXTERNALSYM IID_IWMWriterNetworkSink}
  IID_IWMClientConnections      : TGUID = '{73c66010-a299-41df-b1f0-ccf03b09c1c6}';
  {$EXTERNALSYM IID_IWMClientConnections}
  IID_IWMStatusCallback         : TGUID = '{6d7cdc70-9888-11d3-8edc-00c04f6109cf}';
  {$EXTERNALSYM IID_IWMStatusCallback}
  IID_IWMRegisterCallback       : TGUID = '{cf4b1f99-4de2-4e49-a363-252740d99bc1}';
  {$EXTERNALSYM IID_IWMRegisterCallback}
  IID_IWMClientConnections2     : TGUID = '{4091571e-4701-4593-bb3d-d5f5f0c74246}';
  {$EXTERNALSYM IID_IWMClientConnections2}
    IID_IWMWriterPreprocess       : TGUID = '{fc54a285-38c4-45b5-aa23-85b9f7cb424b}';
  {$EXTERNALSYM IID_IWMWriterPreprocess}

  WMMEDIASUBTYPE_WMAudioV9        : TGUID = '{00000162-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV9}
  WMMEDIASUBTYPE_WMAudio_Lossless : TGUID = '{00000163-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudio_Lossless}
  WMMEDIASUBTYPE_WMSP1            : TGUID = '{0000000A-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMSP1}
  WMMEDIASUBTYPE_WMAudioV8 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV8}
  WMMEDIASUBTYPE_WMAudioV7 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV7}
  WMMEDIASUBTYPE_WMAudioV2 : TGUID = '{00000161-0000-0010-8000-00AA00389B71}';
  {$EXTERNALSYM WMMEDIASUBTYPE_WMAudioV2}



  WMProfile_V70_6VoiceAudio            : TGUID = '{EABA9FBF-B64F-49b3-AA0C-73FBDD150AD0}';
  {$EXTERNALSYM WMProfile_V70_6VoiceAudio}
  WMProfile_V70_64Audio                : TGUID = '{B29CFFC6-F131-41db-B5E8-99D8B0B945F4}';
  {$EXTERNALSYM WMProfile_V70_64Audio}
  WMProfile_V70_96Audio                : TGUID = '{A9D4B819-16CC-4a59-9F37-693DBB0302D6}';
  {$EXTERNALSYM WMProfile_V70_96Audio}
  WMProfile_V70_128Audio               : TGUID = '{C64CF5DA-DF45-40d3-8027-DE698D68DC66}';
  {$EXTERNALSYM WMProfile_V70_128Audio}

  WMMEDIATYPE_Audio               : TGUID = '{73647561-0000-0010-8000-00AA00389B71}'; // 'auds'
  {$EXTERNALSYM WMMEDIATYPE_Audio}
  WMFORMAT_WaveFormatEx    : TGUID = '{05589f81-c356-11ce-bf01-00aa0055595a}';
  {$EXTERNALSYM WMFORMAT_WaveFormatEx}

  WM_START_CURRENTPOSITION = Int64(-1);
  {$EXTERNALSYM WM_START_CURRENTPOSITION}

  g_dwWMSpecialAttributes       = LongWord(20);
  {$EXTERNALSYM g_dwWMSpecialAttributes}
  g_wszWMDuration               = WideString('Duration');
  {$EXTERNALSYM g_wszWMDuration}
  g_wszWMBitrate                = WideString('Bitrate');
  {$EXTERNALSYM g_wszWMBitrate}
  g_wszWMSeekable               = WideString('Seekable');
  {$EXTERNALSYM g_wszWMSeekable}
  g_wszWMStridable              = WideString('Stridable');
  {$EXTERNALSYM g_wszWMStridable}
  g_wszWMBroadcast              = WideString('Broadcast');
  {$EXTERNALSYM g_wszWMBroadcast}
  g_wszWMProtected              = WideString('Is_Protected');
  {$EXTERNALSYM g_wszWMProtected}
  g_wszWMTrusted                = WideString('Is_Trusted');
  {$EXTERNALSYM g_wszWMTrusted}
  g_wszWMSignature_Name         = WideString('Signature_Name');
  {$EXTERNALSYM g_wszWMSignature_Name}
  g_wszWMHasAudio               = WideString('HasAudio');
  {$EXTERNALSYM g_wszWMHasAudio}
  g_wszWMHasImage               = WideString('HasImage');
  {$EXTERNALSYM g_wszWMHasImage}
  g_wszWMHasScript              = WideString('HasScript');
  {$EXTERNALSYM g_wszWMHasScript}
  g_wszWMHasVideo               = WideString('HasVideo');
  {$EXTERNALSYM g_wszWMHasVideo}
  g_wszWMCurrentBitrate         = WideString('CurrentBitrate');
  {$EXTERNALSYM g_wszWMCurrentBitrate}
  g_wszWMOptimalBitrate         = WideString('OptimalBitrate');
  {$EXTERNALSYM g_wszWMOptimalBitrate}
  g_wszWMHasAttachedImages      = WideString('HasAttachedImages');
  {$EXTERNALSYM g_wszWMHasAttachedImages}
  g_wszWMSkipBackward           = WideString('Can_Skip_Backward');
  {$EXTERNALSYM g_wszWMSkipBackward}
  g_wszWMSkipForward            = WideString('Can_Skip_Forward');
  {$EXTERNALSYM g_wszWMSkipForward}
  g_wszWMNumberOfFrames         = WideString('NumberOfFrames');
  {$EXTERNALSYM g_wszWMNumberOfFrames}
  g_wszWMFileSize               = WideString('FileSize');
  {$EXTERNALSYM g_wszWMFileSize}
  g_wszWMHasArbitraryDataStream = WideString('HasArbitraryDataStream');
  {$EXTERNALSYM g_wszWMHasArbitraryDataStream}
  g_wszWMHasFileTransferStream  = WideString('HasFileTransferStream');
  {$EXTERNALSYM g_wszWMHasFileTransferStream}
  g_wszWMContainerFormat        = WideString('WM/ContainerFormat');
  {$EXTERNALSYM g_wszWMContainerFormat}
  g_wszEnableDiscreteOutput    = WideString('EnableDiscreteOutput');
  {$EXTERNALSYM g_wszEnableDiscreteOutput}
  g_wszSpeakerConfig           = WideString('SpeakerConfig');
  {$EXTERNALSYM g_wszSpeakerConfig}

////////////////////////////////////////////////////////////////
//
// The content description object supports 5 basic attributes.
//

  g_dwWMContentAttributes = LongWord(5);
  {$EXTERNALSYM g_dwWMContentAttributes}
  g_wszWMTitle        = WideString('Title');
  {$EXTERNALSYM g_wszWMTitle}
  g_wszWMAuthor       = WideString('Author');
  {$EXTERNALSYM g_wszWMAuthor}
  g_wszWMDescription  = WideString('Description');
  {$EXTERNALSYM g_wszWMDescription}
  g_wszWMRating       = WideString('Rating');
  {$EXTERNALSYM g_wszWMRating}
  g_wszWMCopyright    = WideString('Copyright');
  {$EXTERNALSYM g_wszWMCopyright}

////////////////////////////////////////////////////////////////
//
// These attributes are used to configure and query DRM settings in the reader and writer.
//

  g_wszWMUse_DRM                   = WideString('Use_DRM');
  {$EXTERNALSYM g_wszWMUse_DRM}
  g_wszWMDRM_Flags                 = WideString('DRM_Flags');
  {$EXTERNALSYM g_wszWMDRM_Flags}
  g_wszWMDRM_Level                 = WideString('DRM_Level');
  {$EXTERNALSYM g_wszWMDRM_Level}
  g_wszWMUse_Advanced_DRM          = WideString('Use_Advanced_DRM');
  {$EXTERNALSYM g_wszWMUse_Advanced_DRM}
  g_wszWMDRM_KeySeed               = WideString('DRM_KeySeed');
  {$EXTERNALSYM g_wszWMDRM_KeySeed}
  g_wszWMDRM_KeyID                 = WideString('DRM_KeyID');
  {$EXTERNALSYM g_wszWMDRM_KeyID}
  g_wszWMDRM_ContentID             = WideString('DRM_ContentID');
  {$EXTERNALSYM g_wszWMDRM_ContentID}
  g_wszWMDRM_IndividualizedVersion = WideString('DRM_IndividualizedVersion');
  {$EXTERNALSYM g_wszWMDRM_IndividualizedVersion}
  g_wszWMDRM_LicenseAcqURL         = WideString('DRM_LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_LicenseAcqURL}
  g_wszWMDRM_V1LicenseAcqURL       = WideString('DRM_V1LicenseAcqURL');
  {$EXTERNALSYM g_wszWMDRM_V1LicenseAcqURL}
  g_wszWMDRM_HeaderSignPrivKey     = WideString('DRM_HeaderSignPrivKey');
  {$EXTERNALSYM g_wszWMDRM_HeaderSignPrivKey}
  g_wszWMDRM_LASignaturePrivKey    = WideString('DRM_LASignaturePrivKey');
  {$EXTERNALSYM g_wszWMDRM_LASignaturePrivKey}
  g_wszWMDRM_LASignatureCert       = WideString('DRM_LASignatureCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureCert}
  g_wszWMDRM_LASignatureLicSrvCert = WideString('DRM_LASignatureLicSrvCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureLicSrvCert}
  g_wszWMDRM_LASignatureRootCert   = WideString('DRM_LASignatureRootCert');
  {$EXTERNALSYM g_wszWMDRM_LASignatureRootCert}

////////////////////////////////////////////////////////////////
//
// These are the additional attributes defined in the WM attribute
// namespace that give information about the content.
//

  g_wszWMAlbumTitle    = WideString('WM/AlbumTitle');
  {$EXTERNALSYM g_wszWMAlbumTitle}
  g_wszWMTrack         = WideString('WM/Track');
  {$EXTERNALSYM g_wszWMTrack}
  g_wszWMPromotionURL  = WideString('WM/PromotionURL');
  {$EXTERNALSYM g_wszWMPromotionURL}
  g_wszWMAlbumCoverURL = WideString('WM/AlbumCoverURL');
  {$EXTERNALSYM g_wszWMAlbumCoverURL}
  g_wszWMGenre         = WideString('WM/Genre');
  {$EXTERNALSYM g_wszWMGenre}
  g_wszWMYear          = WideString('WM/Year');
  {$EXTERNALSYM g_wszWMYear}
  g_wszWMGenreID       = WideString('WM/GenreID');
  {$EXTERNALSYM g_wszWMGenreID}
  g_wszWMMCDI          = WideString('WM/MCDI');
  {$EXTERNALSYM g_wszWMMCDI}
  g_wszWMComposer      = WideString('WM/Composer');
  {$EXTERNALSYM g_wszWMComposer}
  g_wszWMLyrics        = WideString('WM/Lyrics');
  {$EXTERNALSYM g_wszWMLyrics}
  g_wszWMTrackNumber   = WideString('WM/TrackNumber');
  {$EXTERNALSYM g_wszWMTrackNumber}
  g_wszWMToolName      = WideString('WM/ToolName');
  {$EXTERNALSYM g_wszWMToolName}
  g_wszWMToolVersion   = WideString('WM/ToolVersion');
  {$EXTERNALSYM g_wszWMToolVersion}
  g_wszWMIsVBR         = WideString('IsVBR');
  {$EXTERNALSYM g_wszWMIsVBR}

//
// WM/AlbumArtist is a potentially different value than Author
//
  g_wszWMAlbumArtist = WideString('WM/AlbumArtist');
  {$EXTERNALSYM g_wszWMAlbumArtist}

////////////////////////////////////////////////////////////////
//
// These optional attributes may be used to give information
// about the branding of the content.
//

  g_wszWMBannerImageType = WideString('BannerImageType');
  {$EXTERNALSYM g_wszWMBannerImageType}
  g_wszWMBannerImageData = WideString('BannerImageData');
  {$EXTERNALSYM g_wszWMBannerImageData}
  g_wszWMBannerImageURL  = WideString('BannerImageURL');
  {$EXTERNALSYM g_wszWMBannerImageURL}
  g_wszWMCopyrightURL    = WideString('CopyrightURL');
  {$EXTERNALSYM g_wszWMCopyrightURL}

  g_wszComplexityMax     = WideString('_COMPLEXITYEXMAX');
  {$EXTERNALSYM g_wszComplexityMax}
  g_wszComplexityOffline = WideString('_COMPLEXITYEXOFFLINE');
  {$EXTERNALSYM g_wszComplexityOffline}
  g_wszComplexityLive    = WideString('_COMPLEXITYEXLIVE');
  {$EXTERNALSYM g_wszComplexityLive}
  g_wszIsVBRSupported    = WideString('_ISVBRSUPPORTED');
  {$EXTERNALSYM g_wszIsVBRSupported}
  g_wszVBREnabled         = WideString('_VBRENABLED');
  g_wszVBRBitrateMax      = WideString('_RMAX');
  g_wszVBRBufferWindowMax = WideString('_BMAX');
  {$EXTERNALSYM g_wszVBREnabled}
  g_wszVBRQuality         = WideString('_VBRQUALITY');
  g_wszNumPasses = WideString('_PASSESUSED');
  {$EXTERNALSYM g_wszNumPasses}

    WMT_VER_4_0 = $00040000;
    {$EXTERNALSYM WMT_VER_4_0}
    WMT_VER_7_0 = $00070000;
    {$EXTERNALSYM WMT_VER_7_0}
    WMT_VER_8_0 = $00080000;
    {$EXTERNALSYM WMT_VER_8_0}
    WMT_VER_9_0 = $00090000;
    {$EXTERNALSYM WMT_VER_9_0}


  NS_E_PROTECTED_CONTENT           = HRESULT($C00D0BBD);

  SPEAKER_FRONT_LEFT = $1;
  SPEAKER_FRONT_RIGHT = $2;
  SPEAKER_FRONT_CENTER = $4;
  SPEAKER_LOW_FREQUENCY = $8;
  SPEAKER_BACK_LEFT = $10;
  SPEAKER_BACK_RIGHT = $20;
  SPEAKER_FRONT_LEFT_OF_CENTER = $40;
  SPEAKER_FRONT_RIGHT_OF_CENTER = $80;
  SPEAKER_BACK_CENTER = $100;
  SPEAKER_SIDE_LEFT = $200;
  SPEAKER_SIDE_RIGHT = $400;
  SPEAKER_TOP_CENTER = $800;
  SPEAKER_TOP_FRONT_LEFT = $1000;
  SPEAKER_TOP_FRONT_CENTER = $2000;
  SPEAKER_TOP_FRONT_RIGHT = $4000;
  SPEAKER_TOP_BACK_LEFT = $8000;
  SPEAKER_TOP_BACK_CENTER = $10000;
  SPEAKER_TOP_BACK_RIGHT = $20000;



type
  WMT_STATUS = (
  WMT_ERROR                       = 0,
  WMT_OPENED                      = 1,
  WMT_BUFFERING_START             = 2,
  WMT_BUFFERING_STOP              = 3,
  WMT_EOF                         = 4,
  WMT_END_OF_FILE                 = 4,
  WMT_END_OF_SEGMENT              = 5,
  WMT_END_OF_STREAMING            = 6,
  WMT_LOCATING                    = 7,
  WMT_CONNECTING                  = 8,
  WMT_NO_RIGHTS                   = 9,
  WMT_MISSING_CODEC               = 10,
  WMT_STARTED                     = 11,
  WMT_STOPPED                     = 12,
  WMT_CLOSED                      = 13,
  WMT_STRIDING                    = 14,
  WMT_TIMER                       = 15,
  WMT_INDEX_PROGRESS              = 16,
  WMT_SAVEAS_START                = 17,
  WMT_SAVEAS_STOP                = 18,
  WMT_NEW_SOURCEFLAGS             = 19,
  WMT_NEW_METADATA                = 20,
  WMT_BACKUPRESTORE_BEGIN         = 21,
  WMT_SOURCE_SWITCH               = 22,
  WMT_ACQUIRE_LICENSE             = 23,
  WMT_INDIVIDUALIZE               = 24,
  WMT_NEEDS_INDIVIDUALIZATION     = 25,
  WMT_NO_RIGHTS_EX                = 26,
  WMT_BACKUPRESTORE_END           = 27,
  WMT_BACKUPRESTORE_CONNECTING    = 28,
  WMT_BACKUPRESTORE_DISCONNECTING = 29,
  WMT_ERROR_WITHURL               = 30,
  WMT_RESTRICTED_LICENSE          = 31,
  WMT_CLIENT_CONNECT              = 32,
  WMT_CLIENT_DISCONNECT           = 33,
  WMT_NATIVE_OUTPUT_PROPS_CHANGED = 34,
  WMT_RECONNECT_START             = 35,
  WMT_RECONNECT_END               = 36,
  WMT_CLIENT_CONNECT_EX           = 37,
  WMT_CLIENT_DISCONNECT_EX        = 38,
  WMT_SET_FEC_SPAN                = 39,
  WMT_PREROLL_READY               = 40,
  WMT_PREROLL_COMPLETE            = 41,
  WMT_CLIENT_PROPERTIES           = 42,
  WMT_LICENSEURL_SIGNATURE_STATE  = 43,
  WMT_INIT_PLAYLIST_BURN          = 44,
  WMT_TRANSCRYPTOR_INIT           = 45,
  WMT_TRANSCRYPTOR_SEEKED         = 46,
  WMT_TRANSCRYPTOR_READ           = 47,
  WMT_TRANSCRYPTOR_CLOSED         = 48,
  WMT_PROXIMITY_RESULT            = 49,
  WMT_PROXIMITY_COMPLETED         = 50
  );
  {$EXTERNALSYM WMT_STATUS}
  TWMTStatus = WMT_STATUS;

 WAVEFORMATEXTENSIBLE = record
   Format : TWAVEFORMATEX;
   Samples : Word;
   dwChannelMask : LongWord;
   SubFormat : TGUID;
 end;

 PWAVEFORMATEXTENSIBLE = ^WAVEFORMATEXTENSIBLE;

  WMT_VERSION = {$IFDEF TYPE_IDENTITY}type {$ENDIF} LongWord;

  PWMTStreamSelection = ^TWMTStreamSelection;
  WMT_STREAM_SELECTION = (
    WMT_OFF,
    WMT_CLEANPOINT_ONLY,
    WMT_ON
  );
  {$EXTERNALSYM WMT_STREAM_SELECTION}
  TWMTStreamSelection = WMT_STREAM_SELECTION;

  WMT_ATTR_DATATYPE = (
    WMT_TYPE_DWORD,
    WMT_TYPE_STRING,
    WMT_TYPE_BINARY,
    WMT_TYPE_BOOL,
    WMT_TYPE_QWORD,
    WMT_TYPE_WORD,
    WMT_TYPE_GUID
  );
  {$EXTERNALSYM WMT_ATTR_DATATYPE}
  TWMTAttrDataType = WMT_ATTR_DATATYPE;

  PWMMediaType = ^TWMMediaType;
  _WMMediaType = packed record
    majortype            : TGUID;
    subtype              : TGUID;
    bFixedSizeSamples    : BOOL;
    bTemporalCompression : BOOL;
    lSampleSize          : ULONG;
    formattype           : TGUID;
    pUnk                 : IUnknown;
    cbFormat             : ULONG;
    pbFormat             : PBYTE; // size_is(cbFormat)
  end;




  {$EXTERNALSYM _WMMediaType}
  WM_MEDIA_TYPE = _WMMediaType;
  {$EXTERNALSYM WM_MEDIA_TYPE}
  TWMMediaType = _WMMediaType;

  PWMWriterStatistics = ^TWMWriterStatistics;
  _WMWriterStatistics = packed record
    qwSampleCount        : Int64;
    qwByteCount          : Int64;

    qwDroppedSampleCount : Int64;
    qwDroppedByteCount   : Int64;

    dwCurrentBitrate     : LongWord;
    dwAverageBitrate     : LongWord;
    dwExpectedBitrate    : LongWord;

    //
    // Sample rates are given as 1000 * (samples / second).
    //
    dwCurrentSampleRate  : LongWord;
    dwAverageSampleRate  : LongWord;
    dwExpectedSampleRate : LongWord;
  end;
  {$EXTERNALSYM _WMWriterStatistics}
  WM_WRITER_STATISTICS = _WMWriterStatistics;
  {$EXTERNALSYM WM_WRITER_STATISTICS}
  TWMWriterStatistics = _WMWriterStatistics;

  INSSBuffer = interface(IUnknown)
  ['{E1CD3524-03D7-11d2-9EED-006097D2D7CF}']
  (*** INSSBuffer methods ***)
    function GetLength(out pdwLength: LongWord): HRESULT; stdcall;
    function SetLength(dwLength: LongWord): HRESULT; stdcall;
    function GetMaxLength(out pdwLength: LongWord): HRESULT; stdcall;
    function GetBuffer(out ppdwBuffer: PBYTE): HRESULT; stdcall;
    function GetBufferAndLength(out ppdwBuffer: PBYTE; out pdwLength: LongWord): HRESULT; stdcall;
  end;

  IWMHeaderInfo = interface(IUnknown)
  ['{96406BDA-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMHeaderInfo methods ***)
    // For attributes, the stream number passed in means:
    // -1 (0xffff) to specifies "any or no stream".
    // 0 specifies "no stream".
    // Any other value indicates the stream number.
    //
    // Windows Media version 4 and earlier does not support per stream
    // attributes, so any stream number other than 0 will fail.
    //
    function GetAttributeCount(wStreamNum: Word; out pcAttributes: Word): HRESULT; stdcall;

    function GetAttributeByIndex(wIndex: Word; var pwStreamNum: Word;
      {out} pwszName: PWideChar; var pcchNameLen: Word;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function GetAttributeByName(var pwStreamNum: Word; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    function SetAttribute(wStreamNum: Word; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE;
      cbLength: Word): HRESULT; stdcall;

    // Marker methods.
    function GetMarkerCount(out pcMarkers: Word): HRESULT; stdcall;

    function GetMarker(wIndex: Word; {out} pwszMarkerName: PWideChar;
      var pcchMarkerNameLen: Word; out pcnsMarkerTime: Int64): HRESULT; stdcall;

    function AddMarker(pwszMarkerName: PWideChar; cnsMarkerTime: Int64): HRESULT; stdcall;

    function RemoveMarker(wIndex: Word): HRESULT; stdcall;

    // Script command methods.
    function GetScriptCount(out pcScripts: Word): HRESULT; stdcall;

    function GetScript(wIndex: Word; {out} pwszType: PWideChar;
      var pcchTypeLen: Word; {out} pwszCommand: PWideChar;
      var pcchCommandLen: Word; out pcnsScriptTime: Int64): HRESULT; stdcall;

    function AddScript(pwszType, pwszCommand: PWideChar;
      cnsScriptTime: Int64): HRESULT; stdcall;

    function RemoveScript(wIndex: Word): HRESULT; stdcall;
  end;


  IWMMediaProps = interface(IUnknown)
  ['{96406BCE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMediaProps methods ***)
    //
    // GetType is provided for convenience; it returns the same as the
    // majortype of the WM_MEDIA_TYPE.
    //
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function GetMediaType(pType: PWMMediaType;
                          var pcbType: LongWord): HRESULT; stdcall;
    function SetMediaType(pType: PWMMediaType): HRESULT; stdcall;
  end;

  IWMOutputMediaProps = interface(IWMMediaProps)
  ['{96406BD7-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMOutputMediaProps methods ***)
    //
    // A Stream Group and type together uniquely identify each output. (The
    // type is on IWMMediaProps).
    //
    function GetStreamGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;

 IWMSyncReader = interface(IUnknown)
  ['{9397F121-7705-4dc9-B049-98B698188414}']
  (*** IWMSyncReader methods ***)
    //
    // This interface can be QI'ed for IWMProfile, IWMHeaderInfo and
    // IWMReaderTimecode interfaces
    //

    //
    // Open is an synchronous call. We do not support streaming at this time.
    // All open with http:// etc would fail. We do support MP3 or Windows
    // media files from both local file or UNC path.
    //
    function Open(pwszFilename: PWideChar): HRESULT; stdcall;

    function Close: HRESULT; stdcall;

    //
    // SetRange is basically a seek method, you can only set same range
    // for all streams. Use cnsDuration=0 to specify reading to end of file.
    //
    function SetRange(cnsStartTime, cnsDuration: Int64): HRESULT; stdcall;

    //
    // SetRangeByFrame is frame-based access, the file has to be frame
    // indexed to succeeded this call. If the call is successful, all
    // streams are synchronized to the same position based on time.
    // Also use cFramesToRead=0 to specify reading to end of file.
    //
    function SetRangeByFrame(wStreamNum: Word; qwFrameNumber, cFramesToRead: Int64 ): HRESULT; stdcall;

    //
    // If a valid stream number is specified, next sample from that stream
    // will be returned, pwStreamNum can be NULL in this case. Otherwise,
    // GetNextSample returns the next sample in time line (regardless of
    // which stream). The sample's stream number will be returned.
    // Time line is presentation time if no output setting is specified.
    // To get early delivery for some stream, use SetOutputSetting
    //
    function GetNextSample(wStreamNum: Word; out ppSample: INSSBuffer;
      out pcnsSampleTime: Int64; out pcnsDuration: Int64;
      out pdwFlags: LongWord; out pdwOutputNum: LongWord;
      out pwStreamNum: Word): HRESULT; stdcall;

    //
    // Stream selection methods are the same as asynchronous interface
    //
    function SetStreamsSelected(cStreamCount: Word; pwStreamNumbers: PWORD;
      pSelections: PWMTStreamSelection): HRESULT; stdcall;

    function GetStreamSelected(wStreamNum: Word; out pSelection: TWMTStreamSelection): HRESULT; stdcall;

    function SetReadStreamSamples(wStreamNum: Word; fCompressed: BOOL): HRESULT; stdcall;

    function GetReadStreamSamples(wStreamNum: Word; out pfCompressed: BOOL): HRESULT; stdcall;

    //
    // The following two methods are the same as the ones in
    // IWMReaderAdvanced2 interface. We don't support JustInTimeDecode
    // in this interface
    //
    function GetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      out pType: TWMTAttrDataType; {out} pValue: PBYTE;
      var pcbLength: Word): HRESULT; stdcall;

    //
    // Sets a named setting for a particular output
    //
    function SetOutputSetting(dwOutputNum: LongWord; pszName: PWideChar;
      Type_: TWMTAttrDataType; {in} pValue: PBYTE; cbLength: Word): HRESULT; stdcall;

    //
    // The following methods are important for receiving uncompressed samples,
    // they are identical to methods in asynchronous Reader interface.
    //
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;

    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;

    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // output on the reader.
    //
    function GetOutputFormatCount(dwOutputNum: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetOutputFormat(dwOutputNum, dwFormatNum: LongWord; out ppProps: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Methods provided to relate output numbers with stream numbers
    //
    function GetOutputNumberForStream(wStreamNum: Word; out pdwOutputNum: LongWord): HRESULT; stdcall;

    function GetStreamNumberForOutput(dwOutputNum: LongWord; out pwStreamNum: Word): HRESULT; stdcall;

    function GetMaxOutputSampleSize(dwOutput: LongWord; out pcbMax: LongWord): HRESULT; stdcall;

    function GetMaxStreamSampleSize(wStream: Word; out pcbMax: LongWord): HRESULT; stdcall;

    //
    // Same as IWMSyncReader::Open but takes an IStream interface pointer
    // instead of an file name to be opened. This method is typically
    // used for custom source.
    //
    function OpenStream(pStream: IStream): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterSink> _di_IWMWriterSink;'}
  {$EXTERNALSYM IWMWriterSink}
  IWMWriterSink = interface(IUnknown)
    ['{96406BE4-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterSink methods ***)
    function OnHeader(pHeader: INSSBuffer): HResult; stdcall;
    // Some sinks require that data be fed to them in real-time.
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function AllocateDataUnit(cbDataUnit: LongWord; out ppDataUnit: INSSBuffer): HResult; stdcall;
    function OnDataUnit(pDataUnit: INSSBuffer): HResult; stdcall;
    // This function is called when the writer is done sending data.
    function OnEndWriting: HResult; stdcall;
  end;


  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterFileSink> _di_IWMWriterFileSink;'}
  {$EXTERNALSYM IWMWriterFileSink}
    IWMWriterFileSink = interface(IWMWriterSink)
    ['{96406BE5-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterFileSink methods ***)
    function Open(pwszFilename: PWideChar): HResult; stdcall;
  end;

   {$HPPEMIT 'typedef System::DelphiInterface<IWMWriter> _di_IWMWriter;'}
  {$EXTERNALSYM IWMWriter}


  IWMStreamConfig = interface(IUnknown)
  ['{96406BDC-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamConfig methods ***)
    // This interface QI's for IWMMediaProps and one of it's inheritors.
    // (IWMVideoMediaProps, for instance).
    function GetStreamType(out pguidStreamType: TGUID): HRESULT; stdcall;
    function GetStreamNumber(out pwStreamNum: Word): HRESULT; stdcall;
    function SetStreamNumber(wStreamNum: Word): HRESULT; stdcall;
    function GetStreamName({out} pwszStreamName: PWideChar; var pcchStreamName: Word): HRESULT; stdcall;
    function SetStreamName(pwszStreamName: PWideChar): HRESULT; stdcall;
    function GetConnectionName({out} pwszInputName: PWideChar; var pcchInputName: Word): HRESULT; stdcall;
    function SetConnectionName(pwszInputName: PWideChar): HRESULT; stdcall;
    function GetBitrate(out pdwBitrate: LongWord): HRESULT; stdcall;
    function SetBitrate(pdwBitrate: LongWord): HRESULT; stdcall;

    //
    // A buffer window of -1 (0xffffffff) indicates that the buffer window
    // is unknown. On the writer side, this means the writer can use whatever
    // buffer window it chooses.
    //
    function GetBufferWindow(out pmsBufferWindow: LongWord): HRESULT; stdcall;
    function SetBufferWindow(msBufferWindow: LongWord): HRESULT; stdcall;
  end;

  IWMStreamList = interface(IUnknown)
  ['{96406BDD-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMStreamList methods ***)
    function GetStreams({out} pwStreamNumArray: PWORD; var pcStreams: PWORD): HRESULT; stdcall;
    function AddStream(wStreamNum: Word): HRESULT; stdcall;
    function RemoveStream(wStreamNum: Word): HRESULT; stdcall;
  end;

    {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo> _di_IWMCodecInfo;'}
  {$EXTERNALSYM IWMCodecInfo}
  IWMCodecInfo = interface(IUnknown)
    ['{A970F41E-34DE-4A98-B3BA-E4B3CA7528F0}']
    (*** IWMCodecInfo methods ***)
    function GetCodecInfoCount(const guidType: TGUID; out pcCodecs: LongWord): HResult; stdcall;
    function GetCodecFormatCount(const guidType: TGUID; dwCodecIndex: LongWord; out pcFormat: LongWord): HResult; stdcall;
    function GetCodecFormat(const guidType: TGUID; dwCodecIndex: LongWord; dwFormatIndex: LongWord;
                            out ppIStreamConfig: IWMStreamConfig): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo2> _di_IWMCodecInfo2;'}
  {$EXTERNALSYM IWMCodecInfo2}
  IWMCodecInfo2 = interface(IWMCodecInfo)
    ['{AA65E273-B686-4056-91EC-DD768D4DF710}']
    (*** IWMCodecInfo2 methods ***)
    function GetCodecName(const guidType: TGUID; dwCodecIndex: LongWord; {out} wszName: PWideChar;
                          var pcchName: LongWord): HResult; stdcall;
    function GetCodecFormatDesc(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; out ppIStreamConfig: IWMStreamConfig;
                                {out} wszDesc: PWideChar; var pcchDesc: LongWord): HResult; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMCodecInfo3> _di_IWMCodecInfo3;'}
  {$EXTERNALSYM IWMCodecInfo3}
  IWMCodecInfo3 = interface(IWMCodecInfo2)
    ['{7E51F487-4D93-4F98-8AB4-27D0565ADC51}']
    (*** IWMCodecInfo3 methods ***)
    function GetCodecFormatProp(const guidType: TGUID; dwCodecIndex: LongWord;
                                dwFormatIndex: LongWord; pszName: PWideChar;
                                out pType: TWMTAttrDataType; {out} pValue: PByte;
                                var pdwSize: LongWord): HResult; stdcall;
    function GetCodecProp(const guidType: TGUID; dwCodecIndex: LongWord; pszName: PWideChar;
                          out pType: TWMTAttrDataType; {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
    function SetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; Type_: TWMTAttrDataType;
                                        {in} pValue: PByte; dwSize: LongWord): HResult; stdcall;
    function GetCodecEnumerationSetting(const guidType: TGUID; dwCodecIndex: LongWord;
                                        pszName: PWideChar; out pType: TWMTAttrDataType;
                                        {out} pValue: PByte; var pdwSize: LongWord): HResult; stdcall;
  end;


  IWMMutualExclusion = interface(IWMStreamList)
  ['{96406BDE-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMMutualExclusion methods ***)
    // The possible types of mutual exclusion are defined in the ASF
    // header.
    function GetType(out pguidType: TGUID): HRESULT; stdcall;
    function SetType(guidType: TGUID): HRESULT; stdcall;
  end;


  {$EXTERNALSYM IWMProfile}
  IWMProfile = interface(IUnknown)
  ['{96406BDB-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMProfile methods ***)
    // By default, when the user creates a profile, it will use the latest
    // version of Windows Media. To create a backward-compatible profile,
    // call IWMProfileManager::CreateEmptyProfile with the appropriate version
    // number.
    function GetVersion(out pdwVersion: WMT_VERSION): HRESULT; stdcall;

    // Profiles have names and descriptions, for use when displaying lists
    // of profiles, etc.
    function GetName({out} pwszName: PWideChar; var pcchName: LongWord): HRESULT; stdcall;
    function SetName(pwszName: PWideChar): HRESULT; stdcall;

    function GetDescription({out} pwszDescription: PWideChar;
      var pcchDescription: LongWord): HRESULT; stdcall;

    function SetDescription(pwszDescription: PWideChar): HRESULT; stdcall;

    // Methods for enumerating the streams. Note that updating the
    // returned IWMStreamConfig has no effect on the profile until you
    // call ReconfigStream().
    function GetStreamCount(out pcStreams: LongWord): HRESULT; stdcall;

    function GetStream(dwStreamIndex: LongWord; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    function GetStreamByNumber(wStreamNum: Word; out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Remove a stream.
    function RemoveStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function RemoveStreamByNumber(wStreamNum: Word): HRESULT; stdcall;

    // Adding a stream copies the config into the profile.
    function AddStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    function ReconfigStream(pConfig: IWMStreamConfig): HRESULT; stdcall;

    // Create a new stream config object (avoiding the need to CoCreate).
    // This will still need to be added to the profile using the AddStream()
    // call (but only after it has been configured).
    function CreateNewStream(const guidStreamType: TGUID;
      out ppConfig: IWMStreamConfig): HRESULT; stdcall;

    // Mutual Exclusion. As above, only Add and Remove actual change the
    // profile.
    function GetMutualExclusionCount(out pcME: LongWord): HRESULT; stdcall;

    function GetMutualExclusion(dwMEIndex: LongWord; out ppME: IWMMutualExclusion): HRESULT; stdcall;

    function RemoveMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function AddMutualExclusion(pME: IWMMutualExclusion): HRESULT; stdcall;

    function CreateNewMutualExclusion(out ppME: IWMMutualExclusion): HRESULT; stdcall;
  end;

   {$EXTERNALSYM IWMProfileManager}
  IWMProfileManager = interface(IUnknown)
  ['{d16679f2-6ca0-472d-8d31-2f5d55aee155}']
  (*** IWMProfileManager methods ***)
    // Create a profile with nothing in it.
    function CreateEmptyProfile(dwVersion: WMT_VERSION; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a system profile given its ID.
    function LoadProfileByID(const guidProfile: TGUID; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Load a profile from a stored string.
    function LoadProfileByData(pwszProfile: PWideChar; out ppProfile: IWMProfile): HRESULT; stdcall;

    // Save profile specified by its string.
    function SaveProfile(pIWMProfile: IWMProfile; pwszProfile: PWideChar;
      var pdwLength: LongWord): HRESULT; stdcall;

    // Iterate through the system profiles.
    function GetSystemProfileCount(out pcProfiles: LongWord): HRESULT; stdcall;

    function LoadSystemProfile(dwProfileIndex: LongWord; out ppProfile: IWMProfile): HRESULT; stdcall;
  end;

  {$EXTERNALSYM IWMInputMediaProps}
  IWMInputMediaProps = interface(IWMMediaProps)
  ['{96406BD5-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMInputMediaProps methods ***)
    function GetConnectionName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
    function GetGroupName({out} pwszName: PWideChar; var pcchName: Word): HRESULT; stdcall;
  end;


  IWMWriter = interface(IUnknown)
  ['{96406BD4-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMWriter methods ***)
    // This interface QI's for IWMHeaderInfo, and IWMWriterAdvanced.

    //
    // There are 3 options for setting the profile on the writer. Note that
    // setting the profile means that the profile is copied into the writer.
    // Further editing of that profile object will have no effect, unless
    // SetProfile() is called again.
    //
    // Calling SetProfile() removes any previously set header attribute info
    //
    function SetProfileByID(const guidProfile: TGUID): HRESULT; stdcall;

    function SetProfile(pProfile: IWMProfile): HRESULT; stdcall;

    //
    // The easiest way to use the writer is just to write to file.
    //
    function SetOutputFilename(pwszFilename: PWideChar): HRESULT; stdcall;

    //
    // The user can enumerate through the various inputs, and get the input
    // format. Note that these are not ASF streams; one input stream may map
    // to multiple ASF streams in a MEB scenario.
    //
    // Manipulating the IWMInputMediaProps has no effect on the writer, unless
    // the user calls SetInputProps to configure the input.
    //
    function GetInputCount(out pcInputs: LongWord): HRESULT; stdcall;

    function GetInputProps(dwInputNum: LongWord; out ppInput: IWMInputMediaProps): HRESULT; stdcall;

    function SetInputProps(dwInputNum: LongWord; pInput: IWMInputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // input on the writer.
    //
    function GetInputFormatCount(dwInputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetInputFormat(dwInputNumber, dwFormatNumber: LongWord;
       out pProps: IWMInputMediaProps): HRESULT; stdcall;

    //
    // You must call BeginWriting before sending any samples, and
    // you must call EndWriting when you're done sending samples.
    //
    function BeginWriting: HRESULT; stdcall;

    //
    // EndWriting flushes everything, updates indices and headers,
    // and closes the file.
    //
    function EndWriting: HRESULT; stdcall;

    //
    // Allocate a sample. This is optional; the user is welcome to allocate
    // their own buffer class.
    //
    function AllocateSample(dwSampleSize: LongWord; out ppSample: INSSBuffer): HRESULT; stdcall;

    function WriteSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord;
      pSample: INSSBuffer): HRESULT; stdcall;

    //
    // Flush() will flush the writer, but leaves the writer prepared to run
    // again, when WriteSample() is called again.
    // Flush() also causes an updated header to be sent to the sink.
    //
    function Flush: HRESULT; stdcall;
  end;

  // The writer can be QI'd for this interface, which provides advanced writing
  // functionality.
  {$HPPEMIT 'typedef System::DelphiInterface<IWMWriterAdvanced> _di_IWMWriterAdvanced;'}
  {$EXTERNALSYM IWMWriterAdvanced}
  IWMWriterAdvanced = interface(IUnknown)
    ['{96406BE3-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterAdvanced methods ***)
    // Sinks are where the output ASF data goes.
    function GetSinkCount(out pcSinks: LongWord): HResult; stdcall;
    function GetSink(dwSinkNum: LongWord; out ppSink: IWMWriterSink): HResult; stdcall;
    function AddSink(pSink: IWMWriterSink): HResult; stdcall;
    function RemoveSink(pSink: IWMWriterSink): HResult; stdcall;
    // By default, the user provides samples to an input on the
    // IWMWriter interface, and the samples may be compressed, put
    // into a MEB stream, etc. However, the user can use this interface to
    // put the samples directly into the ASF, with no compression etc.
    function WriteStreamSample(wStreamNum: Word; cnsSampleTime: Int64;
                               msSampleSendTime: LongWord; cnsSampleDuration: Int64;
                               dwFlags: LongWord; pSample: INSSBuffer): HResult; stdcall;
    // The writer may be running in real-time. If so, it's interesting to
    // get the current time from the writer.
    function SetLiveSource(fIsLiveSource: BOOL): HResult; stdcall;
    function IsRealTime(out pfRealTime: BOOL): HResult; stdcall;
    function GetWriterTime(out pcnsCurrentTime: Int64): HResult; stdcall;
    // To get statistics, pass in a WM_WRITER_STATISTICS structure, which
    // will be filled out by the GetStatistics() call with the requested
    // stats.
    //
    // Pass in a stream number to get statistics for a specific stream, or
    // pass 0 to get statistics for the entire ASF file.
    function GetStatistics(wStreamNum: Word; out pStats: TWMWriterStatistics): HResult; stdcall;
    // Sync tolerance determines how far out of sync the inputs will be allowed
    // to get before samples are thrown away.  Default is 3000 ms.
    function SetSyncTolerance(msWindow: LongWord): HResult; stdcall;
    function GetSyncTolerance(out pmsWindow: LongWord): HResult; stdcall;
  end;

  IWMStatusCallback = interface(IUnknown)
  ['{6d7cdc70-9888-11d3-8edc-00c04f6109cf}']
  (*** IWMStatusCallback methods ***)
    // The contents of pValue depends on the Status.
    function OnStatus(Status: TWMTStatus; hr: HRESULT; dwType: TWMTAttrDataType;
      pValue: PBYTE; pvContext: Pointer): HRESULT; stdcall;
  end;

  {$HPPEMIT 'typedef System::DelphiInterface<IWMReaderCallback> _di_IWMReaderCallback;'}
  {$EXTERNALSYM IWMReaderCallback}
  IWMReaderCallback = interface(IWMStatusCallback)
  ['{96406BD8-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMReaderCallback methods ***)
    // cnsSampleDuration will be 0 for most media types.
    function OnSample(dwOutputNum: LongWord; cnsSampleTime, cnsSampleDuration: Int64;
      dwFlags: LongWord; pSample: INSSBuffer; pvContext: Pointer): HRESULT; stdcall;
  end;

    {$EXTERNALSYM IWMReader}
  IWMReader = interface(IUnknown)
  ['{96406BD6-2B2B-11d3-B36B-00C04F6108FF}']
  (*** IWMReader methods ***)
    //
    // This interface QI's for IWMHeaderInfo, IWMProfile, IWMReaderAdvanced,
    // IWMReaderAdvanced2, and IWMReaderAdvanced3.
    //

    //
    // Open is an asynch call; it returns almost immediately (if the URL
    // is valid), and the user should wait for appropriate OnStatus calls to
    // be sent to the callback.
    //
    function Open(pwszURL: PWideChar; pCallback: IWMReaderCallback;
      pvContext: Pointer): HRESULT; stdcall;

    function Close: HRESULT; stdcall;

    //
    // The user can enumerate through the various outputs, and get the
    // output format for that data.
    //
    // Manipulating the IWMOutputMediaProps has no effect on the output, unless
    // the user also calls SetOutputProps.
    //
    function GetOutputCount(out pcOutputs: LongWord): HRESULT; stdcall;

    function GetOutputProps(dwOutputNum: LongWord; out ppOutput: IWMOutputMediaProps): HRESULT; stdcall;

    function SetOutputProps(dwOutputNum: LongWord; pOutput: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // Used for determining all possible format types supported by this
    // output on the reader.
    //
    function GetOutputFormatCount(dwOutputNumber: LongWord; out pcFormats: LongWord): HRESULT; stdcall;

    function GetOutputFormat(dwOutputNumber, dwFormatNumber: LongWord;
      out ppProps: IWMOutputMediaProps): HRESULT; stdcall;

    //
    // If duration is 0, play to the end of the file.
    // If msStart is set to WM_START_CURRENTPOSITION then don't perform a seek
    // operation.  A good use for this is when you want to change the rate but
    // not the current file position.
    //
    // Note that any call to start while Paused will be treated as a seek.
    // Even calls to Start( WM_START_CURRENTPOSITION, ... ).  If your intention
    // is to seek (which will incur the buffering penalty from network files)
    // then you can go ahead and call Start.  However, if your intention was
    // to continue playing from where the user paused, you should call Resume
    // instead.
    //
    function Start(cnsStart, cnsDuration: Int64; fRate: Single; pvContext: Pointer): HRESULT; stdcall;
    function Stop: HRESULT; stdcall;
    function Pause: HRESULT; stdcall;
    function Resume: HRESULT; stdcall;
  end;

  PWMPortNumberRange = ^TWMPortNumberRange;
  _WMPortNumberRange = packed record
    wPortBegin : Word;
    wPortEnd   : Word;
  end;
  {$EXTERNALSYM _WMPortNumberRange}
  WM_PORT_NUMBER_RANGE = _WMPortNumberRange;
  {$EXTERNALSYM WM_PORT_NUMBER_RANGE}
  TWMPortNumberRange = _WMPortNumberRange;

    WMT_PROXY_SETTINGS = (
    WMT_PROXY_SETTING_NONE,
    WMT_PROXY_SETTING_MANUAL,
    WMT_PROXY_SETTING_AUTO,
    WMT_PROXY_SETTING_BROWSER,       // Only valid for HTTP
    WMT_PROXY_SETTING_MAX
  );
  {$EXTERNALSYM WMT_PROXY_SETTINGS}
  TWMTProxySettings = WMT_PROXY_SETTINGS;


    IWMReaderNetworkConfig = interface(IUnknown)
    ['{96406BEC-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMReaderNetworkConfig methods ***)
    // Get and set the amount of time the network source will buffer
    // data before rendering it.
    function GetBufferingTime(out pcnsBufferingTime: Int64): HResult; stdcall;
    function SetBufferingTime(cnsBufferingTime: Int64): HResult; stdcall;
    // Returns the UDP port number ranges that will be used for receiving
    // data.  If no ranges are available, random UDP port numbers will be used.
    function GetUDPPortRanges({out} pRangeArray: PWMPortNumberRange; var pcRanges: LongWord): HResult; stdcall;
    // Sets the UDP port number ranges that can be used for receiving data.
    // If no ranges are specified, random UDP port numbers will be used.
    function SetUDPPortRanges({in} pRangeArray: PWMPortNumberRange; cRanges: LongWord): HResult; stdcall;
    // Proxy settings: Manual proxy, Autodetect, UseBrowser (only for HTTP), or No Proxy.
    function GetProxySettings(pwszProtocol: PWideChar; out pProxySetting: TWMTProxySettings): HResult; stdcall;
    function SetProxySettings(pwszProtocol: PWideChar; ProxySetting: TWMTProxySettings): HResult; stdcall;
    // The host to use as the proxy.
    function GetProxyHostName(pwszProtocol: PWideChar; {out} pwszHostName: PWideChar;
                              var pcchHostName: LongWord): HResult; stdcall;
    function SetProxyHostName(pwszProtocol: PWideChar; pwszHostName: PWideChar): HResult; stdcall;
    // The port to use as the proxy.
    function GetProxyPort(pwszProtocol: PWideChar; out pdwPort: LongWord): HResult; stdcall;
    function SetProxyPort(pwszProtocol: PWideChar; dwPort: LongWord): HResult; stdcall;
    // Get and set the proxy exception list.
    function GetProxyExceptionList(pwszProtocol: PWideChar; {out} pwszExceptionList: PWideChar;
                                   var pcchExceptionList: LongWord): HResult; stdcall;
    function SetProxyExceptionList(pwszProtocol: PWideChar; pwszExceptionList: PWideChar): HResult; stdcall;
    // Whether or not to bypass proxy for local hosts
    function GetProxyBypassForLocal(pwszProtocol: PWideChar; out pfBypassForLocal: BOOL): HResult; stdcall;
    function SetProxyBypassForLocal(pwszProtocol: PWideChar; fBypassForLocal: BOOL): HResult; stdcall;
    // Whether to force a wpad discovery on the next run
    function GetForceRerunAutoProxyDetection(out pfForceRerunDetection: BOOL): HResult; stdcall;
    function SetForceRerunAutoProxyDetection(fForceRerunDetection: BOOL): HResult; stdcall;
    // Whether or not to use multicast, http, tcp, or udp
    function GetEnableMulticast(out pfEnableMulticast: BOOL): HResult; stdcall;
    function SetEnableMulticast(fEnableMulticast: BOOL): HResult; stdcall;
    function GetEnableHTTP(out pfEnableHTTP: BOOL): HResult; stdcall;
    function SetEnableHTTP(fEnableHTTP: BOOL): HResult; stdcall;
    function GetEnableUDP(out pfEnableUDP: BOOL): HResult; stdcall;
    function SetEnableUDP(fEnableUDP: BOOL): HResult; stdcall;
    function GetEnableTCP(out pfEnableTCP: BOOL): HResult; stdcall;
    function SetEnableTCP(fEnableTCP: BOOL): HResult; stdcall;
    // Forgets automatic protocol detection settings and redetects next time.
    function ResetProtocolRollover: HResult; stdcall;
    // Return or set the client's link bandwidth in bps.  This is an optional
    // setting.  By default, the SDK will automatically detect its connection
    // bandwidth to the streaming media server.
    function GetConnectionBandwidth(out pdwConnectionBandwidth: LongWord): HResult; stdcall;
    function SetConnectionBandwidth(dwConnectionBandwidth: LongWord): HResult; stdcall;
    // Iterate through the network protocols supported by this reader
    function GetNumProtocolsSupported(out pcProtocols: LongWord): HResult; stdcall;
    function GetSupportedProtocolName(dwProtocolNum: LongWord; {out} pwszProtocolName: PWideChar;
                                      var pcchProtocolName: LongWord): HResult; stdcall;
    // Adds the specified pszUrl to the list of URL's to recieve logging data.
    // This list is in addition to the origin server.
    function AddLoggingUrl(pwszURL: PWideChar): HResult; stdcall;
    // Fills the buffer with the URL corresponding to index dwIndex.
    function GetLoggingUrl(dwIndex: LongWord; {out} pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
    // Returns the number of URLs in the current list of logging URLs.
    function GetLoggingUrlCount(out pdwUrlCount: LongWord): HResult; stdcall;
    // Clears the list of logging URLs
    function ResetLoggingUrlList: HResult; stdcall;
  end;

  IWMPropertyVault = interface(IUnknown)
  ['{72995A79-5090-42a4-9C8C-D9D0B6D34BE5}']
  (*** IWMPropertyVault methods ***)
    function GetPropertyCount(out pdwCount: LongWord): HRESULT; stdcall;
    function GetPropertyByName(pszName: PWideChar; out pType: TWMTAttrDataType;
      {out} pValue: PBYTE;  var pdwSize: LongWord): HRESULT; stdcall;
    function SetProperty(pszName: PWideChar; pType: TWMTAttrDataType;
      pValue: PBYTE; dwSize: LongWord): HRESULT; stdcall;
    function GetPropertyByIndex(dwIndex: LongWord; {out} pszName: PWideChar;
      var pdwNameLen: LongWord; out pType: TWMTAttrDataType;
      {out} pValue: PBYTE; var pdwSize: LongWord): HRESULT; stdcall;
    function CopyPropertiesFrom(pIWMPropertyVault: IWMPropertyVault): HRESULT; stdcall;
    function Clear: HRESULT; stdcall;
  end;

 WMT_NET_PROTOCOL = (
    WMT_PROTOCOL_HTTP
  );
  {$EXTERNALSYM WMT_NET_PROTOCOL}
  TWMTNetProtocol = WMT_NET_PROTOCOL;

  IWMWriterNetworkSink = interface(IWMWriterSink)
    ['{96406BE7-2B2B-11D3-B36B-00C04F6108FF}']
    (*** IWMWriterNetworkSink methods ***)
    // Determine the maximum number of clients that can connect to this sink.
    // Default is 5.
    function SetMaximumClients(dwMaxClients: LongWord): HResult; stdcall;
    function GetMaximumClients(out pdwMaxClients: LongWord): HResult; stdcall;
    // The network protocol that the network sink will use.
    function SetNetworkProtocol(protocol: TWMTNetProtocol): HResult; stdcall;
    function GetNetworkProtocol(out pProtocol: TWMTNetProtocol): HResult; stdcall;
    // Find out the name of the URL on which we're broadcasting
    function GetHostURL({out} pwszURL: PWideChar; var pcchURL: LongWord): HResult; stdcall;
    // The method claims the network port number. Close the sink to release
    // the port.
    //
    // Specify 0 for the port number and the sink will select a port for
    // the user.
    function Open(var pdwPortNum: LongWord): HResult; stdcall;
    // Disconnect all connected clients.
    function Disconnect: HResult; stdcall;
    // Close and release the open port.
    function Close: HResult; stdcall;
  end;

    IWMWriterPreprocess = interface(IUnknown)
    ['{FC54A285-38C4-45B5-AA23-85B9F7CB424B}']
    (*** IWMWriterPreprocess methods ***)
    function GetMaxPreprocessingPasses(dwInputNum: LongWord; dwFlags: LongWord;
      out pdwMaxNumPasses: LongWord): HResult; stdcall;
    function SetNumPreprocessingPasses(dwInputNum: LongWord; dwFlags: LongWord;
      dwNumPasses: LongWord): HResult; stdcall;
    function BeginPreprocessingPass(dwInputNum: LongWord; dwFlags: LongWord): HResult; stdcall;
    function PreprocessSample(dwInputNum: LongWord; cnsSampleTime: Int64; dwFlags: LongWord;
      pSample: INSSBuffer): HResult; stdcall;
    function EndPreprocessingPass(dwInputNum: LongWord; dwFlags: LongWord): HResult; stdcall;
  end;

  PWMClientProperties = ^TWMClientProperties;
  _WMClientProperties = packed record
    dwIPAddress : LongWord;
    dwPort      : LongWord;
  end;
  {$EXTERNALSYM _WMClientProperties}
  WM_CLIENT_PROPERTIES = _WMClientProperties;
  {$EXTERNALSYM WM_CLIENT_PROPERTIES}
  TWMClientProperties = _WMClientProperties;

  IWMClientConnections = interface(IUnknown)
    ['{73C66010-A299-41DF-B1F0-CCF03B09C1C6}']
    (*** IWMClientConnections methods ***)
    // Determine the number of connected clients
    function GetClientCount(out pcClients: LongWord): HResult; stdcall;
    // Get information about a connected client
    function GetClientProperties(dwClientNum: LongWord; out pClientProperties: TWMClientProperties): HResult; stdcall;
  end;

  {$EXTERNALSYM IWMClientConnections2}
  IWMClientConnections2 = interface(IWMClientConnections)
    ['{4091571E-4701-4593-BB3D-D5F5F0C74246}']
    (*** IWMClientConnections2 methods ***)
    // Get information about a connected client
    function GetClientInfo(dwClientNum: LongWord; {out} pwszNetworkAddress: PWideChar;
      var pcchNetworkAddress: LongWord; {out} pwszPort: PWideChar;
      var pcchPort: LongWord; {out} pwszDNSName: PWideChar;
      var pcchDNSName: LongWord): HResult; stdcall;
  end;

  IWMRegisterCallback = interface(IUnknown)
    ['{CF4B1F99-4DE2-4E49-A363-252740D99BC1}']
    (*** IWMRegisterCallback methods ***)
    function Advise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
    function Unadvise(pCallback: IWMStatusCallback; pvContext: Pointer): HResult; stdcall;
  end;


  WMCreateSyncReader_t = function (pUnkCert: IUnknown; dwRights: LongWord; out ppSyncReader: IWMSyncReader): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateSyncReader}
  WMCreateWriter_t = function (pUnkCert: IUnknown; out ppWriter: IWMWriter): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriter}
  WMCreateWriterFileSink_t = function (out ppSink: IWMWriterFileSink ): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterFileSink}
  WMCreateProfileManager_t = function (out ppProfileManager: IWMProfileManager): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateProfileManager}
  WMCreateReader_t = function (pUnkCert: IUnknown; dwRights: LongWord; out ppReader: IWMReader): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateReader}
  WMCreateWriterNetworkSink_t = function (out ppSink: IWMWriterNetworkSink): HRESULT; stdcall;
  {$EXTERNALSYM WMCreateWriterNetworkSink}

var
  WMVCoreLoaded : Boolean = False;
  WMCreateSyncReader : WMCreateSyncReader_t;
  WMCreateWriter : WMCreateWriter_t;
  WMCreateWriterFileSink : WMCreateWriterFileSink_t;
  WMCreateProfileManager : WMCreateProfileManager_t;
  WMCreateReader : WMCreateReader_t;
  WMCreateWriterNetworkSink : WMCreateWriterNetworkSink_t;


  procedure LoadWMVCore;
  procedure UnloadWMVCore;

implementation

const
  WMVCORE = 'WMVCORE.DLL';

var
  WMVHandle : HMODULE = 0;

procedure LoadWMVCore;
begin
  if WMVCoreLoaded then Exit;
  WMVHandle := LoadLibrary(WMVCORE);
  if WMVHandle <> 0 then
  begin
    WMVCoreLoaded := True;
    WMCreateSyncReader := GetProcAddress(WMVHandle, 'WMCreateSyncReader');
    WMCreateWriter := GetProcAddress(WMVHandle, 'WMCreateWriter');
    WMCreateWriterFileSink := GetProcAddress(WMVHandle, 'WMCreateWriterFileSink');
    WMCreateProfileManager := GetProcAddress(WMVHandle, 'WMCreateProfileManager');
    WMCreateReader := GetProcAddress(WMVHandle, 'WMCreateReader');
    WMCreateWriterNetworkSink := GetProcAddress(WMVHandle, 'WMCreateWriterNetworkSink');
  end;
end;


procedure UnloadWMVCore;
begin
  if not WMVCoreLoaded then Exit;
  WMVCoreLoaded := False;
  if WMVHandle <> 0 then FreeLibrary(WMVHandle);
end;

initialization
  LoadWMVCore;

finalization
  UnloadWMVCore;

end.
