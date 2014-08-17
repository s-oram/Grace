(*
  This file is a part of New Audio Components package v. 2.5
  Copyright (c) 2002-2010, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_WinMedia.pas 1160 2010-01-30 14:53:09Z andrei.borovsky $ *)

unit ACS_WinMedia;

(* Title: ACS_WinMedia
    Delphi interface for Windows Media Audio (WMA) using Windows' built-in
    codec. *)

interface

uses
  Windows, Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Tags, libwma1;

const
   wmfDefault = -2;

type

  TStreamedAudioEvent = procedure(Sender : TComponent) of object;

  TConnectionInfo = record
    IP : array[0..15] of Char;
    Port : LongWord;
  end;

  TChannelsNumber = (cnMaxAvailable, cnMonoOrStereo, cn5dot1, cn7dot1);

  TWMAFormatSpec = record
    BitsPerSample : LongWord;
    Channels : LongWord;
    SampleRate : LongWord;
  end;

   (* Class: TWMIn
      Windows Media Audio file/stream decoder. This component can read not
      only WMA files but sound tracks from WMV files as well. It can also read
      mp3 files. Descends from <TAuTaggedFileIn> .*)

  TWMIn = class(TAuTaggedFileIn)
  private
    reader : wma_sync_reader;
    FDuration : LongWord;
    FHighPrecision : Boolean;
    FOutputChannels : TChannelsNumber;
    FFormat : Integer;
    procedure SetHighPrecision(Value : Boolean);
    procedure SetOutputChannels(Value : TChannelsNumber);
    function GetHasAudio : Boolean;
    function GetProtected : Boolean;
    function GetBitrate : LongWord;
    function GetId3v2Tags : TId3v2Tags;
    function GetIsVBR : Boolean;
    function CNToShortInt : ShortInt;
    function GetFormatsCount : LongWord;
    function GetFormatSpec(Index : Integer) : TWMAFormatSpec;
    procedure SetFormat(AFormat : Integer);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    (* Property: FormatsCount
     WMA decoders allow the program to read input data in several formats (each format is a set
     of channels, bits per sample and sample rate numbers. Read this property to get the total number of formats available.

     Note: the number of formats provided by the encoder depends on the <HighPrecision> value (formats with 24 bps and more than 2 channels will be available only if HighPrecision is set to True). *)
    property FormatsCount : LongWord read GetFormatsCount;
    (* Property: FormatSpec[Index : Integer]
     Read this property to get the parameters of the format specified by its Index.
     Valid Index values range from 0 to <FormatsCount> - 1. *)
    property FormatSpec[Index : Integer] : TWMAFormatSpec read GetFormatSpec;
    (* Property: FormatSelected
     Use this property to set the desired format for the decoder's output.
     Valid values range from 0 to <FormatsCount> - 1. There is also a special constant - wmfDefault.
     If you set the property value to wmfDefault the output audio format will be the "main format", the one that was used
     when the file was encoded.
     This property affects the <Channels>, <BitsPerSample>, <SampleRate>, <Size>, and <TotalSamples> properties.
     Setting this property value overrides <OutputChannels>'s value.
     Set this property value before you call Run for the corresponding output component. *)
    property FormatSelected : Integer read FFormat write SetFormat;

    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       The False value indicates that either an audio stream is missing (in
       WMV file) or the input file is corrupt.

       Note:
       Windows Media files may contain several audio streams. In the current
       version TWMIn reads data only from the first audio stream it finds. *)
    property HasAudio : Boolean read GetHasAudio;
    (* Property: IsProtected
       If the value of this property is True, the file is DRM-protected and
       hence not supported. This property has no meaning for mp3 files. *)
    property IsProtected : Boolean read GetProtected;
    (* Property: Bitrate
       Read this property to get the file's bitrate.

       Note:
       For video and multi-streamed files the total bitrate is returned. *)
     property Bitrate : LongWord read GetBitrate;
    (* Property: Id3v2Tags
       This property contains file's tags in Id3v2 format. *)
    property Id3v2Tags : TId3v2Tags read GetId3v2Tags;
    (* Property: IsVBR
       This property's value is True if the input file is VBR-encoded and False otherwise. *)
    property IsVBR : Boolean read GetIsVBR;
  published
    property EndSample;
    property StartSample;
    (* Property: HighPrecision
      Use HighPrecision to set the high precision decoding mode on or off.
      In normal precision mode the decoder will produce only 16-bit 1 or 2 channel output, even if the input file is 24-bit multi-channel.
      If you want to obtain 24-bit sound or more than two channels, set HighPrecision to True.
      This property is set to True automatically if you assign to the <OutputChannels> property some value other than cnMonoOrStereo. *)
    property HighPrecision : Boolean read FHighPrecision write SetHighPrecision;
    (* Property: OutputChannels
      Sets the number of channels for decoder output.

      Possible values are:
      - cnMaxAvailable - decoder produces all the channels present in the input file.
      - cnMonoOrStereo - decoder produces mono audio stream for mono input file and stereo for everything else.
      - cn5dot1 - decoder will produce 5.1 multi-channel output if the input file has enough channels in it.
      - cn7dot1 - decoder will produce 7.1 multi-channel output if the input file has enough channels in it.

      Use cnMaxAvailable value if you don't know how many channels the input file contains and want to play all of them.
      Note however that cnMaxAvailable upsamples audio ooutput (usually to 48000 Hz) which is not always desirable.
      Use cnMonoOrStereo if you want your output to be always mono or stereo (this mode is useful if you play sound on the hardware that doesn't support multiple channels).
      If the input file is multi-channel and cnMonoOrStereo mode is selected, the channels will be mixed into stereo.
      Use cn5dot1 if you are sure that the inpt file contains at least 6 channels. Forsing 6 channels ouput on the file that doesn't have enough channels may cause crash.
      Use cn7dot1 if you are sure that the inpt file contains at least 8 channels. Forsing 8 channels on the file that doesn't have them may cause crash.
      See also the <HighPrecision> property. *)
      property OutputChannels : TChannelsNumber read FOutputChannels write SetOutputChannels;
  end;

   (* Class: TWMAOut
      Windows Media Audio file/stream encoder. This component supports
      CBR/VBR, lossy/lossless encoding. There are two mutually exclusive
      groups of properties affecting the output audio quality. One group
      allows you to set output file bitrate or quality and let the component
      select the most appropriate codec. The other group allows you to specify
      the codec and format directly. This component descends from
      <TAuTaggedFileOut>. *)

  TOutputErrorEvent = procedure(Sender : TComponent; Reason : LongWord) of object;

  TWMAOut = class(TAuTaggedFileOut)
  private
    FCodecs : TStringList;
    FFormats : TStringList;
    FCodecIndex, FFormatIndex : Integer;
    EndOfStream : Boolean;
    Buf : Pointer;
    BufSize : Integer;
    FBitrate : LongWord;
    FLossless, FVBR : Boolean;
    FVBRQuality : Byte;
    function GetCodecs : TStringList;
    function GetCodecsCount : Word;
    function GetCodecName(Index : Word) : String;
    function GetFormats(Index : Word) : TStringList;
    function GetFormatsCount(Index : Word) : Word;
  protected
    ErrorFlag : Boolean;
    Writer : wma_writer;
    FPort : LongWord;
    FMaxClients : LongWord;
    FUseNetwork : Boolean;
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: GetFormatDesc
        This method returns a format description based on the CodecIndex and
        FormatIndex parameters. *)
    function GetFormatDesc(CodecIndex, FormatIndex : Word) : String;
    (* Property: Codecs
        Returns the names of all the WMA codecs installed in the system. *)
    property Codecs : TStringList read GetCodecs;
    (* Property: CodecsCount
        Returns the total number of the WMA codecs available in the system. *)
    property CodecsCount : Word read GetCodecsCount;
    (* Property: CodecIndex
        Use this property to set the index number of the codec to use when
        encoding. The valid values for this property range from -1 to
        <CodecsCount> - 1. If this property's value is set to -1 (the default
        setting), the <FormatIndex> property is ignored and the codec is
        selected automatically by the component depending on <DesiredBitrate>
        and <VBRQuality> values. If this property's value is greater than -1,
        the <DesiredBitrate> and <VBRQuality> properties are ignored and
        CodecIndex along with <FormatIndex> specify how audio data will be
        encoded. Wav2WMA-2 demo uses this method. *)
    property CodecIndex : Integer read FCodecIndex write FCodecIndex;
    (* Property: CodecName
        Returns the name of the WMA codec specified by its index.
        The valid indices range from 0 to <CodecsCount> -1. *)
    property CodecName[Index : Word] : String read GetCodecName;
    (* Property: FormatIndex
        Use this property to set the index of the format to encode data. Valid
         values range from 0 to <FormatsCount> -1. This property has an effect
         only if <CodecIndex> is greater than -1. *)
    property FormatIndex : Integer read FFormatIndex write FFormatIndex;
    (* Property: Formats
        Returns the names of all formats supported by the codec, specified by
        the Index and the current encoding mode. See the Wav2WMA2 demo for an
        example. See also <A Note on Windows Media Formats>. *)
    property Formats[Index : Word] : TStringList read GetFormats;
    (* Property: FormatsCount
        Returns the total number of formats supported by the codec, specified
        by its index, and the current encoding mode. The valid indices range
        from 0 to <CodecsCount> -1. Please see <A Note on Windows Media Formats>.
    *)
    property FormatsCount[index : Word] : Word read GetFormatsCount;
  published
    (* Property: Id3v2Tags
        Set an output file's tags in Id3v2 format. *)
    property Id3v2Tags;
    (* Property: DesiredBitrate
       Set the desired bitrate for an output file (in the constant bitrate
       lossy mode). The component will search for the best configuration
       matching your parameters, so the actual bitrate may be less than this
       value. *)
    property DesiredBitrate : LongWord read FBitrate write FBitrate;
    (* Property: Lossless
       Use this property to switch between the lossless and lossy compression modes.
       In the lossless mode the <DesiredBitrate> and <VBRQuality> values are ignored.
       Lossless encoding is always VBR. *)
    property Lossless : Boolean read FLossless write FLossless;
    (* Property: VBR
       Use this property to switch between constant bitrate and variable
       bitrate lossy encoding modes. In VBR mode <DesiredBitrate> value is
       ignored. The quality of the output sound is defined by the <VBRQuality>
       property. If you encode data by directly selecting the codec and
       format, note that the VBR setting affects <Formats> and <FormatsCount>
       values for every codec. In the lossless mode the this property's value
       is ignored. *)
    property VBR : Boolean read FVBR write FVBR;
    (* Property: VBRQuality
       Use this property to set the output audio quality in VBR mode. The
       valid values range from 1 to 99. This property only has an effect if
       <VBR> is set to True and <Lossless> to False. *)
    property VBRQuality : Byte read FVBRQuality write FVBRQuality;
  end;

  (* Topic: A Note on Windows Media Formats
        The <VBR> and <Lossless> settings affect the values of <Formats> and <FormatsCount> properies. For example, Windows Media Audio Lossless codec (which is VBR) will show
        any formats only if <VBR> or <Lossless> property is set to True. Conversely, Windows Media Voice codec exposes only CBR
        formats. It will expose any formats only if <VBR> and <Lossless> are both False.  You should  re-read the format-related properties each time you change either the
        <VBR> or <Lossless> values. *)

  (* Class: TWMATap
      Descends from <TAudioTap>. This is one of the "audio tap" components
      that sit between input and output in the audio chain and optionally
      record the audio data passing through them into a file. This component
      records data into WMA files. *)

  TWMATap = class(TAudioTap)
  private
    FTags : TId3v2Tags;
    FBitrate : LongWord;
    FLossless, FVBR : Boolean;
    FVBRQuality : Byte;
    Writer : wma_writer;
    FOnOutputError : TOutputErrorEvent;
    procedure SetId3v2Tags(Value: TId3v2Tags);
  protected
    procedure StartRecordInternal; override;
    procedure StopRecordInternal; override;
    procedure WriteDataInternal(Buffer : Pointer; BufferLength : LongWord); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Id3v2Tags
        Set an output file's tags in Id3v2 format. *)
    property Id3v2Tags : TId3v2Tags read FTags write SetId3v2Tags;
    (* Property: DesiredBitrate
       Set the desired bitrate for an output file (in the constant bitrate
       lossy mode). The component will search for the best configuration
       matching your parameters, so the actual bitrate may be less than this
       value. *)
    property DesiredBitrate : LongWord read FBitrate write FBitrate;
    (* Property: Lossless
       Use this property to switch between the lossless and lossy compression
       modes. In the lossless mode the <DesiredBitrate> and <VBRQuality>
       values are ignored. Lossless encoding is always VBR. *)
    property Lossless : Boolean read FLossless write FLossless;
    (* Property: VBR
       Use this property to switch between constant bitrate and variable
       bitrate lossy encoding modes. In VBR mode <DesiredBitrate> value is
       ignored. The quality of the output sound is defined by the <VBRQuality>
       property. *)
    property VBR : Boolean read FVBR write FVBR;
    (* Property: VBRQuality
       Use this property to set the output audio quality in VBR mode. The
       valid values range from 1 to 99. This property has any effect only if
       <VBR> is set to True and <Lossless> to False. *)
    property VBRQuality : Byte read FVBRQuality write FVBRQuality;
    (* Property: OnOutputError
       This event is raised if an error occurs during output. If the event handler is not set, error is ignored.
       The best way to handle the eent is to call
       > Sender.Stop(True);
       from the handler.
       When this event is called ExcaptionMessage property returns 'Windows Media output error.' string. *)
    property OnOutputError : TOutputErrorEvent read FOnOutputError write FOnOutputError;
  end;


   (* Class: TWMStreamedIn
      Streamable WMA/MP3 decoder component. This component can do three things:
       - decode wma and mp3 files stored on your hard drive.
       - decode wma and mp3 files stored at some network (http) location. The component
         starts downloading the file and decoding it simultaneously.
       - decode wma and mp3 audio streamed from "live" audio servers such as Internet radio servers.

       You should assign file or stream URL to the component's <FileName> property.

       Important note: 
       Most links to Internet streamed media that you can find on Web sites
       point not to media itself but to wax, asx, or m3u shortcuts instead.
       These shortcuts contain information about the content, and, among other
       things, a direct link to a streaming audio server. Although it is
       relatively easy to parse wax, asx, and m3u shortcuts and extract
       required links from them, it is not TWMStreamedIn's job. The component
       expects a direct link to a wma/mp3 file or a network stream to be
       assigned to its <Filename> property. See also the <LoggingURL>
       property.

       If you write a live audio player, you have to take care about http
       links traversal and shortcuts parsing. I-Radio demo that accompanies
       NewAC, uses preset links to several live audio servers.

       This decoder is not seekable.

       Descends from <TAuTaggedFileIn>. *)

  TWMStreamedIn = class(TAuTaggedFileIn)
  private
    reader : wma_async_reader;
    FBitrate : LongWord;
    FStretchFactor : Single;
    FBufferingTime : Word;
    FEnableHTTP : Boolean;
    FEnableTCP : Boolean;
    FEnableUDP : Boolean;
    FMaxWait : LongWord;
    FProxyProtocol, FProxyHost : String;
    FProxyPort : LongWord;
    FLoggingURL : String;
    FOnStreamOpened : TStreamedAudioEvent;
    FOnStartedPlaying : TStreamedAudioEvent;
    FFirstTime : Boolean;
    function GetHasAudio : Boolean;
    function GetId3v2Tags : TId3v2Tags;
    function GetTimedOut : Boolean;
    procedure SetBufferingTime(value : Word);
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    function SeekInternal(var SampleNum : Int64) : Boolean; override;
    function GetTotalTime : LongWord; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ResetStretch(NewStretch : Single);
    (* Property: HasAudio
       Read this property to determine if the input file has an audio stream.
       The False value indicates that either an audio stream is missing (in
       WMV file) or the input file is corrupt.

       Note:
       Windows Media files may contain several audio streams. In the current
       version TWMIn reads data only from the first audio stream it finds. *)
    property HasAudio : Boolean read GetHasAudio;
    procedure _Pause; override;
    procedure _Resume; override;
    (* Property: Bitrate
       Read this property to get the file's bitrate.
       Note: for video and multi-streamed files the total bitrate is returned. *)
     property Bitrate : LongWord read FBitrate;
    (* Property: Id3v2Tags
       This property contains file's tags in Id3v2 format. *)
    property Id3v2Tags : TId3v2Tags read GetId3v2Tags;
    (* Property: TimedOut
       This property indicates if some network operation has timed out. If the
       timeout has occurred, the component reports the end of data. You can
       change the time the component waits before timeout in the
       <MaxWaitMilliseconds> property. *)
    property TimedOut : Boolean read GetTimedOut;
  published
  (* Property: BufferingTime
       This property allows you to set the size of internal buffer in terms of
       playback duration. The value of this property should be buffer's
       playback time in seconds raging from 1 to 60. Larger buffering time
       imposes some delay at the beginning of the playback, but helps to
       provide smoother playback later. *)
    property BufferingTime : Word read FBufferingTime write SetBufferingTime;
  (* Property: EnableHTTP
       Use this property to enable or disable HTTP support. This property's
       value matters only if the URL supplied to <TAuFileIn.FileName> has no protocol
       prefix, such as "http:", "mms:" or "rtsp:". *)
    property EnableHTTP : Boolean read FEnableHTTP write FEnableHTTP;
  (* Property: EnableTCP
       Use this property to enable or disable TCP support. This property's
       value matters only if the URL supplied to <TAuFileIn.FileName> has no protocol
       prefix, such as "http:", "mms:" or "rtsp:". *)
    property EnableTCP : Boolean read FEnableTCP write FEnableTCP;
  (* Property: EnableUDP
       Use this property to enable or disable UDP support. This property's
       value matters only if the URL supplied to <TAuFileIn.FileName> has no protocol
       prefix, such as "http:", "mms:" or "rtsp:". *)
    property EnableUDP : Boolean read FEnableUDP write FEnableUDP;
  (* Property: LoggingURL
       Use this property to set a logging URL, if you have one. Logging URLs
       may be obtained from wax, asx, or m3u shortcuts. *)
    property LoggingURL : String read FLoggingURL write FLoggingURL;
  (* Property: MaxWaitMilliseconds
       This property allows you to set the maximum waiting time for some
       network operations to complete. The time is set in milliseconds. If
       this time is exceeded, the <TimedOut> property is set to True and the
       component stops its operation. The default value of this property is
       10000 (10 seconds). Setting this property's value too low will result
       in too many premature timeouts. Setting it too high will mean that you
       will have to wait too long just to learn that the remote server could
       not be reached. *)
    property MaxWaitMilliseconds : LongWord read FMaxWait write FMaxWait;
  (* Property: ProxyProtocol
       If your application requires a proxy to connect to Internet, use this
       property to set the proxy protocol. *)
    property ProxyProtocol : String read FProxyProtocol write FProxyProtocol;
  (* Property: ProxyHost
       If your application requires a proxy to connect to Internet, use this
       property to set the proxy host name. *)
    property ProxyHost : String read FProxyHost write FProxyHost;
  (* Property: ProxyPort
       If your application requires a proxy to connect to Internet, use this
       property to set the proxy port value. *)
    property ProxyPort : LongWord read FProxyPort write FProxyPort;
  (* Property: StretchFactor
       Use this property to change the speed at with content is delivered to
       the component. The default value is 1.0. Possible values range from 1.0
       to 10.0 and from -10.0 to -1.0. This property has no effect when
       handling live audio. *)
    property StretchFactor : Single read FStretchFactor write FStretchFactor;
  (* Property: OnStreamOpened
       This event informs you that the audio stream has been opened
       successfully. *)
    property OnStreamOpened : TStreamedAudioEvent read FOnStreamOpened write FOnStreamOpened;
  (* Property: OnStartedPlaying
       This event informs you that the decoder has decoded the first chunk of
       audio data. *)
    property OnStartedPlaying : TStreamedAudioEvent read FOnStartedPlaying write FOnStartedPlaying;
  end;

  (* Class: TWMStreamedOut
        This component can stream Windows Media audio over HTTP in local or global networks.
        You can listen to the streamed audio using such players as Windows Media Player, WinAmp, or NewAC demo i-Radio.
        Audio stream is not a file. You cannot download it to your hard drive (but you can record it).
        When connecting to the audio-streaming host from a player, don't forget to use http: prefix or the player will not know what protocol to use. *)

  TWMStreamedOut = class(TWMAOut)
  private
    FOnClientConnected, FOnClientDisconnected : TStreamedAudioEvent;
    function GetConnectionsCount : LongWord;
    function GetConnectionInfo(Index : Integer) : TConnectionInfo;
    function GetURL : String;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    (* Property: ConnectionsCount
      The number of incoming connections. *)
    property ConnectionsCount : LongWord read GetConnectionsCount;
    (* Property: Connections[Index : Integer]
      Use this property to get an information about an incoming connection.
      Valid Index values range from 0 to <ConnectionsCount> - 1. *)
    property Connections[Index : Integer] : TConnectionInfo read GetConnectionInfo;
    (* Property: URL
      Use this property to get the transmitting host URL.
      This URL usually takes form http://your_host_name:<Port>.
      The URL value becomes available only after you call Run and not at once. *)
    property URL : String read GetURL;
  published
    (* Property: MaxClients
      The maximum number of incoming connctions allowed.
      If this property is set to 0 (the default value) the number of incoming conections is not limited. *)
    property MaxClients : LongWord read FMaxClients write FMaxClients;
    (* Property: Port
      The number of port on which the component will listen for incoming connections. *)
    property Port : LongWord read FPort write FPort;
    (* Property: OnClientConnected
      The OnClientConnected event is raised when a new client connects to the transmitter. *)
    property OnClientConnected : TStreamedAudioEvent read FOnClientConnected write FOnClientConnected;
    (* Property: OnClientDisconnected
      The OnClientDisconnected event is raised when a client disconnects from the transmitter. *)
    property OnClientDisconnected : TStreamedAudioEvent read FOnClientDisconnected write FOnClientDisconnected;
  end;

  (* Class: TWMADualPassOut
      This component implements Windows Media Audio 2-pass encoder. TWMADualPassOut supports
      CBR/VBR lossy encoding in formats available for the 2-pass encoder.
      In order o perform 2-pass encoding TWMADualPassOut reads audio data from its input component twice.
      This puts some constraints on what TWMADualPassOut's input may be (it should be a TAuFileIn-descending component).
      This component descends from <TAuTaggedFileOut>. *)

  TWMADualPassOut = class(TAuTaggedFileOut)
  private
    FCodecs : TStringList;
    FFormats : TStringList;
    FCodecIndex, FFormatIndex : Integer;
    EndOfStream : Boolean;
    Buf : Pointer;
    BufSize : Integer;
    FVBR : Boolean;
    Writer : wma_writer;
    FErrorFlag : Boolean;
    function GetCodecs : TStringList;
    function GetCodecsCount : Word;
    function GetCodecName(Index : Word) : String;
    function GetFormats(Index : Word) : TStringList;
    function GetFormatsCount(Index : Word) : Word;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Function: GetFormatDesc
        This method returns a format description based on the CodecIndex and
        FormatIndex parameters. *)
    function GetFormatDesc(CodecIndex, FormatIndex : Word) : String;
    (* Property: Codecs
        Returns the names of all the WMA codecs installed in the system. *)
    property Codecs : TStringList read GetCodecs;
    (* Property: CodecsCount
        Returns the total number of the WMA codecs available in the system. *)
    property CodecsCount : Word read GetCodecsCount;
    (* Property: CodecIndex
        Use this property to set the index number of the codec to use when
        encoding. The valid values for this property range from 0 to
        <CodecsCount> - 1. *)
    property CodecIndex : Integer read FCodecIndex write FCodecIndex;
    (* Property: CodecName
        Returns the name of the WMA codec specified by its index.
        The valid indices range from 0 to <CodecsCount> -1. *)
    property CodecName[Index : Word] : String read GetCodecName;
    (* Property: FormatIndex
        Use this property to set the index of the format to encode data. Valid
         values range from 0 to <FormatsCount> -1. This property has an effect
         only if <CodecIndex> is greater than -1. *)
    property FormatIndex : Integer read FFormatIndex write FFormatIndex;
    (* Property: Formats
        Returns the names of all formats supported by the codec, specified by
        the Index and the current encoding mode. See the Wav2WMA2 demo for an
        example. *)
    property Formats[Index : Word] : TStringList read GetFormats;
    (* Property: FormatsCount
        Returns the total number of formats supported by the codec, specified
        by its index, and the current encoding mode. The valid indices range
        from 0 to <CodecsCount> -1.
    *)
    property FormatsCount[index : Word] : Word read GetFormatsCount;
  published
    (* Property: Id3v2Tags
        Set an output file's tags in Id3v2 format. *)
    property Id3v2Tags;
    (* Property: VBR
       Use this property to switch between constant bitrate and variable
       bitrate encoding modes. *)
    property VBR : Boolean read FVBR write FVBR;
  end;


implementation

  procedure CallOnConnected(Dest : TComponent);
  begin
    if Assigned((Dest as TWMStreamedOut).FOnClientConnected)  then
      EventHandler.PostGenericEvent(Dest, (Dest as TWMStreamedOut).FOnClientConnected);
  end;

  procedure CallOnDisconnected(Dest : TComponent);
  begin
    if Assigned((Dest as TWMStreamedOut).FOnClientDisconnected)  then
      EventHandler.PostGenericEvent(Dest, (Dest as TWMStreamedOut).FOnClientDisconnected);
  end;

  procedure CallOnError(Dest : TComponent; Reason : LongWord);
  begin
    (Dest as TWMAOut).FExceptionMessage := 'Windows Media output error.';
    (Dest as TWMAOut).ErrorFlag := True;
//    if Assigned((Dest as TWMAOut).FOnOutputError) then
 //      (Dest as TWMAOut).FOnOutputError(Dest, Reason);
  end;

  procedure CallOnATError(Dest : TComponent; Reason : LongWord);
  begin
    if Assigned((Dest as TWMATap).FOnOutputError) then
       (Dest as TWMATap).FOnOutputError(Dest, Reason);
  end;

  procedure CallOnDPError(Dest : TComponent; Reason : LongWord);
  begin
   (Dest as TWMADualPassOut).FExceptionMessage := 'Windows Media output error.';
   (Dest as TWMADualPassOut).FErrorFlag := True;
//    if Assigned((Dest as TWMADualPassOut).FOnOutputError) then
//       (Dest as TWMADualPassOut).FOnOutputError(Dest, Reason);
  end;


  constructor TWMIn.Create;
  begin
    inherited Create(AOwner);
    FOutputChannels := cnMonoOrStereo;
  end;

  destructor TWMIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWMIn.OpenFile;
  var
//    Tag : TID3Tag;
    ch, bps : Word;
    sr : LongWord;
//    str : WideString;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if (not FStreamAssigned) and (FWideFileName = '') then
      raise EAuException.Create('File name is not assigned');
      if not FStreamAssigned then FStream := TAuFileStream.Create(FWideFileName, fmOpenRead or fmShareDenyWrite);
      lwma_reader_init(reader, FStream, FHighPrecision, CNToShortInt);
      FValid := reader.has_audio;
      if reader.reader = nil then
        Exit;
      if reader._protected then
        raise EAuException.Create('File is protected');
      FDuration := lwma_reader_get_duration(reader);
      lwma_reader_get_audio_properties(reader, ch, bps, sr);
      FChan := ch;
      FBPS := bps;
      FSR := sr;
      FTotalSamples := Trunc((FDuration/100)*sr);
      FSize := FTotalSamples*ch*(bps shr 3);
      FSeekable := True;
//      SetLength(Str, 256);
  //    lwma_reader_get_author(reader, Str);
      _Id3v2Tags.Clear;
      _CommonTags.Clear;
      {$WARNINGS OFF}
      _Id3v2Tags.Artist := lwma_reader_get_author(reader);
      _CommonTags.Artist := lwma_reader_get_author(reader);
      _Id3v2Tags.Title := lwma_reader_get_title(reader);
      _CommonTags.Title := lwma_reader_get_title(reader);
      _Id3v2Tags.Album := lwma_reader_get_album(reader);
      _CommonTags.Album := lwma_reader_get_album(reader);
      _Id3v2Tags.Genre := lwma_reader_get_genre(reader);
      _CommonTags.Genre := lwma_reader_get_genre(reader);
      _Id3v2Tags.Track := lwma_reader_get_track(reader);
      _CommonTags.Track := lwma_reader_get_track(reader);
      _Id3v2Tags.Year := lwma_reader_get_year(reader);
      _CommonTags.Year := lwma_reader_get_year(reader);
      _Id3v2Tags.Comment := lwma_reader_get_copyright(reader);
     {$WARNINGS ON}
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      lwma_reader_free(reader);
      FOpened := 0;
      if not FStreamAssigned then FStream.Free
      else FStream.Seek(0, soFromBeginning);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  begin
    if (FSize > 0) and (FSize - FPosition < Bytes) then
      Bytes := FSize - FPosition;
    lwma_reader_get_data(reader, Buffer, Bytes);
  end;

  function TWMIn.SeekInternal(var SampleNum : Int64) : Boolean;
  var
    Offset : LongWord;
  begin
    Result := False;
    if Busy then
    begin
      Offset := Round(SampleNum/FTotalSamples*FDuration);
      lwma_reader_seek(reader, Offset);
      Result := True;
    end;
  end;

  function TWMIn.GetHasAudio;
  begin
    OpenFile;
    Result := reader.has_audio;
  end;

  function TWMIn.GetProtected;
  begin
    OpenFile;
    Result := reader._protected;
  end;

  function TWMIn.GetBitrate;
  begin
    OpenFile;
    Result := lwma_reader_get_bitrate(reader);
  end;

  function TWMIn.GetId3v2Tags;
  begin
    OpenFile;
    Result := _Id3v2Tags;
  end;

  function TWMIn.GetIsVBR;
  begin
    OpenFile;
    Result := lwma_reader_get_is_vbr(reader);
  end;

  procedure TWMIn.SetHighPrecision(Value: Boolean);
  begin
    if Busy then Exit;
    if FOutputChannels = cnMonoOrStereo then
    begin
      FHighPrecision := Value;
      if FOpened > 0 then
      begin
        CloseFile;
        OpenFile;
      end;
    end;
  end;

  procedure TWMIn.SetOutputChannels;
  begin
    if Busy then Exit;
    if Value <> cnMonoOrStereo then
       FHighPrecision := True;
    FOutputChannels := Value;
    if FOpened > 0 then
    begin
      CloseFile;
      OpenFile;
    end;
  end;

  constructor TWMAOut.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
      FCodecs := TStringList.Create;
      lwma_enumerate_codecs(FCodecs);
      FFormats := TStringList.Create;
    end;
    FCodecIndex := -1;
  end;

  destructor TWMAOut.Destroy;
  begin
    if not (csDesigning in ComponentState) then
    begin
      FCodecs.Free;
      FFormats.Free;
    end;
    inherited Destroy;
  end;

  procedure TWMAOut.Prepare;
  begin
    if not FUseNetwork then
      if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
    FInput.Init;
    if FUseNetwork then
      lwma_network_writer_init(Writer, FPort, FMaxClients, Self, CallOnConnected, CallOnDisconnected, CallOnError)
    else
      lwma_writer_init(Writer, PWideChar(FWideFileName), Self, CallOnError);
    if not Writer.Initialized then
      raise Exception.Create('Cannot create file');
    if FCodecIndex < 0 then
    begin
      if not FVBR then
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FBitrate)
      else
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FVBRQuality);
    end
    else
      lwma_writer_set_audio_properties2(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FVBR or FLossless, FCodecIndex, FFormatIndex);
    BufSize := FInput.Channels*FInput.BitsPerSample*FInput.SampleRate div 100;
    GetMem(Buf, BufSize);
    {$WARNINGS OFF}
    if Id3v2Tags.Artist <> '' then
      lwma_writer_set_author(Writer, Id3v2Tags.Artist);
    if Id3v2Tags.Album <> '' then
      lwma_writer_set_album(Writer, Id3v2Tags.Album);
    if Id3v2Tags.Genre <> '' then
      lwma_writer_set_genre(Writer, Id3v2Tags.Genre);
    if Id3v2Tags.Year <> '' then
      lwma_writer_set_year(Writer, Id3v2Tags.Year);
    if Id3v2Tags.Track <> '' then
      lwma_writer_set_track(Writer, Id3v2Tags.Track);
    if Id3v2Tags.Title <> '' then
      lwma_writer_set_title(Writer, Id3v2Tags.Title);
    if Id3v2Tags.Comment <> '' then
      lwma_writer_set_copyright(Writer, Id3v2Tags.Comment);
    {$WARNINGS ON}
    lwma_writer_begin(Writer);
    EndOfStream := false;
    ErrorFlag := False;
  end;

  function TWMAOut.DoOutput;
  var
    l : Integer;
  begin
    if ErrorFlag then
     raise EAuException.Create('Windows Media output error');
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfStream then
    begin
      Result := False;
      Exit;
    end;
    l := Finput.FillBuffer(Buf, BufSize, EndOfStream);
    lwma_writer_write(Writer, Buf, l);
  end;

  procedure TWMAOut.Done;
  begin
    FInput.Flush;
    if FUseNetwork then
      lwma_network_writer_free(Writer)
    else
      lwma_writer_free(Writer);
    FreeMem(Buf);
  end;


  constructor TWMStreamedIn.Create;
  begin
    inherited Create(AOwner);
    FStretchFactor := 1.0;
    FMaxWait := 10000;
    BufferingTime := 2;
  end;

  destructor TWMStreamedIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWMStreamedIn.OpenFile;
  begin
    OpenCS.Enter;
    try
    if FOpened = 0 then
    begin
      if FWideFileName = '' then
      raise EAuException.Create('File name or URL is not assigned');
      lwma_async_reader_init(reader);
      if FProxyHost <> '' then
        lwma_async_reader_set_proxy(reader, FProxyProtocol, FProxyHost, FProxyPort);
      if FLoggingURL <> '' then
        lwma_async_reader_add_logging_url(reader, FLoggingURL);
      reader.StretchFactor := FStretchFactor;
      reader.MaxWaitMilliseconds := FMaxWait;
      reader.TimedOut := False;
      reader.EnableTCP := FEnableTCP;
      reader.EnableHTTP := FEnableHTTP;
      reader.EnableUDP := FEnableUDP;
      reader.BufferingTime := FBufferingTime * 10000000;
      lwma_async_reader_open(reader, FWideFileName);
      if Assigned(FOnStreamOpened) then
        EventHandler.PostGenericEvent(Self, FOnStreamOpened);
      FValid := reader.has_audio;
      if reader.reader = nil then
        raise EAuException.Create('');
      FChan := reader.channels;
      FBPS := reader.BitsPerSample;
      FSR := reader.SampleRate;
      FBitrate := reader.Bitrate;
      FTotalSamples := reader.duration*FSR;
      if FTotalSamples = 0 then
      begin
        FSize := -1;
        FTotalSamples := -1;
      end else
        FSize := FTotalSamples*FChan*(FBPS shr 3);
      FSeekable := False;
      {$WARNINGS OFF}
      //      SetLength(Str, 256);
      //lwma_async_reader_get_author(reader, Str);
      _Id3v2Tags.Artist := lwma_async_reader_get_author(reader);
//      lwma_async_reader_get_title(reader, Str);
      _Id3v2Tags.Title := lwma_async_reader_get_title(reader);
      //lwma_async_reader_get_album(reader, Str);
      _Id3v2Tags.Album := lwma_async_reader_get_album(reader);
//      lwma_async_reader_get_genre(reader, Str);
      _Id3v2Tags.Genre := lwma_async_reader_get_genre(reader);
//      lwma_async_reader_get_track(reader, Str);
      _Id3v2Tags.Track := lwma_async_reader_get_track(reader);
//      lwma_async_reader_get_year(reader, Str);
      _Id3v2Tags.Year := lwma_async_reader_get_year(reader);
//      lwma_async_reader_get_copyright(reader, Str);
      _Id3v2Tags.Comment := lwma_async_reader_get_copyright(reader);
      {$WARNINGS ON}
      FFirstTime := True;
      Inc(FOpened);
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMStreamedIn.CloseFile;
  begin
    OpenCS.Enter;
    try
    if FOpened > 0 then
    begin
      reader.MaxWaitMilliseconds := 2000;
      lwma_async_reader_clear_logging_urls(reader);
      lwma_async_reader_free(reader);
      FOpened := 0;
    end;
    finally
      OpenCS.Leave;
    end;
  end;

  procedure TWMStreamedIn.GetDataInternal(var Buffer : Pointer; var Bytes : LongWord);
  begin
    lwma_async_reader_get_data(reader, Buffer, Bytes);
    if FFirstTime then
    begin
      if Assigned(FOnStartedPlaying) then
          EventHandler.PostGenericEvent(Self, FOnStartedPlaying);
      FFirstTime := False;    
    end;
  end;

  function TWMStreamedIn.SeekInternal(var SampleNum : Int64) : Boolean;
//  var
//    Offset : LongWord;
  begin
    Result := False;
(*    if Busy then
    begin
      Offset := Round(SampleNum/FTotalSamples*FDuration);
      lwma_reader_seek(reader, Offset);
      Result := True;
    end; *)
  end;

  function TWMStreamedIn.GetHasAudio;
  begin
    OpenFile;
    Result := reader.has_audio;
  end;

  function TWMStreamedIn.GetId3v2Tags;
  begin
    Result := _Id3v2Tags;
  end;

  function TWMStreamedIn.GetTotalTime;
  begin
    Result := reader.duration;
  end;

  procedure TWMStreamedIn.ResetStretch;
  begin
    FStretchFactor := NewStretch;
    lwma_async_reader_reset_stretch(reader, NewStretch);
  end;

  procedure TWMStreamedIn._Pause;
  begin
    _Lock;
    lwma_async_reader_pause(reader);
    _Unlock;
  end;

  procedure TWMStreamedIn._Resume;
  begin
    lwma_async_reader_resume(reader);
  end;

  procedure TWMStreamedIn.SetBufferingTime;
  begin
    if value in [1..60] then
      FBufferingTime := value;
  end;

  function TWMStreamedIn.GetTimedOut;
  begin
    Result := reader.TimedOut;
  end;

  function TWMAOut.GetCodecs;
  begin
    Result := FCodecs;
  end;

  function TWMAOut.GetCodecsCount;
  begin
    Result := FCodecs.Count;
  end;

  function TWMAOut.GetCodecName;
  begin
    if Index < FCodecs.Count then
      Result := FCodecs.Strings[Index];
  end;

  function TWMAOut.GetFormats;
  begin
    FFormats.Clear;
    if Index < FCodecs.Count then
      lwma_enumerate_codec_formats(Index, FVBR or FLossless, FFormats);
    Result := FFormats;
  end;

  function TWMAOut.GetFormatsCount;
  begin
    GetFormats(Index);
    Result := FFormats.Count;
  end;

  function TWMAOut.GetFormatDesc;
  begin
      GetFormats(CodecIndex);
      if FormatIndex >= FFormats.Count then Result := ''
      else
        Result := FFormats.Strings[FormatIndex];
  end;

  constructor TWMATap.Create;
  begin
    inherited Create(AOwner);
    FTags := TId3v2Tags.Create;
  end;

  destructor TWMATap.Destroy;
  begin
    FTags.Free;
    inherited Destroy;
  end;

  procedure TWMATap.SetId3v2Tags(Value: TId3v2Tags);
  begin
    FTags.Assign(Value);
  end;

  procedure TWMATap.StartRecordInternal;
  begin
    if FWideFileName = '' then raise EAuException.Create('File name is not assigned.');
    lwma_writer_init(Writer, PWideChar(FWideFileName), Self, CallOnATError);
    if not Writer.Initialized then
      raise Exception.Create('Cannot create file');
      if not FVBR then
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FBitrate)
      else
        lwma_writer_set_audio_properties(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FLossless, FVBR, FVBRQuality);
    {$WARNINGS OFF}
    if FTags.Artist <> '' then
      lwma_writer_set_author(Writer, FTags.Artist);
    if FTags.Album <> '' then
      lwma_writer_set_album(Writer, FTags.Album);
    if FTags.Genre <> '' then
      lwma_writer_set_genre(Writer, FTags.Genre);
    if FTags.Year <> '' then
      lwma_writer_set_year(Writer, FTags.Year);
    if FTags.Track <> '' then
      lwma_writer_set_track(Writer, FTags.Track);
    if FTags.Title <> '' then
      lwma_writer_set_title(Writer, Id3v2Tags.Title);
    if FTags.Comment <> '' then
      lwma_writer_set_copyright(Writer, FTags.Comment);
    {$WARNINGS ON}
    lwma_writer_begin(Writer);
  end;

  procedure TWMATap.StopRecordInternal;
  begin
    lwma_writer_free(Writer);
  end;

  procedure TWMATap.WriteDataInternal;
  begin
    lwma_writer_write(Writer, Buffer, BufferLength);
  end;

  function TWMin.CNToShortInt;
  begin
    Result := 0;
    case OutputChannels of
      cnMaxAvailable : Result := -1;
      cnMonoOrStereo : Result := 0;
      cn5dot1 : Result := 6;
      cn7dot1 : Result := 8;
    end;
  end;

  procedure TWMin.SetFormat;
  begin
    if Busy then Exit;
    OpenFile;
    if AFormat = wmfDefault then
    begin
      lwma_reader_set_format(reader, True, 0);
      FFormat := 0;
    end;
    if AFormat >= 0 then
    begin
      lwma_reader_set_format(reader, FHighPrecision, LongWord(AFormat));
      FFormat := AFormat;
    end;
    FDuration := lwma_reader_get_duration(reader);
    FChan := reader.channels;
    FBPS := reader.BitsPerSample;
    FSR := reader.SampleRate;
    FTotalSamples := Trunc((FDuration/100)*FSR);
    FSize := FTotalSamples*FChan*(FBPS shr 3);
  end;

  function TWMin.GetFormatsCount;
  begin
    Result := 0;
    if Busy then Exit;
    if (WideFileName <> '') or Assigned(FStream) then
    begin
      OpenFile;
      Result := lwma_reader_get_format_count(reader, FHighPrecision);
    end;
  end;

  function TWMin.GetFormatSpec;
  begin
    Result.BitsPerSample := 0;
    Result.Channels := 0;
    Result.SampleRate := 0;
    if Busy then Exit;
    if (WideFileName <> '') or Assigned(FStream) then
    begin
      OpenFile;
      if Index >= 0 then
        lwma_reader_get_format(reader, FHighPrecision, LongWord(Index), Result.Channels, Result.BitsPerSample, Result.SampleRate);
    end;    
  end;


  constructor TWMStreamedOut.Create;
  begin
    inherited Create(AOwner);
    FUseNetwork := True;
  end;

  destructor TWMStreamedOut.Destroy;
  begin
    inherited Destroy;
  end;

  function TWMStreamedOut.GetConnectionsCount;
  begin
    Result := lwma_network_writer_get_connections_count(Writer);
  end;

  function TWMStreamedOut.GetConnectionInfo;
  var
    IP : LongWord;
    S : String;
  begin
    lwma_network_writer_get_connection_info(Writer, Index, IP, Result.Port);
    FillChar(Result.IP, 16, 0);
    S := Format('%d.%d.%d.%d', [(IP shr 24) and $FF, (IP shr 16) and $FF, (IP shr 8) and $FF, IP and $FF]);
    Move(S[1], Result.IP[0], Length(S));
  end;

  function TWMStreamedOut.GetURL;
  begin
    Result := lwm_network_writer_get_url(Writer);
  end;

  constructor TWMADualPassOut.Create;
  begin
    inherited Create(AOwner);
    if not (csDesigning in ComponentState) then
    begin
      FCodecs := TStringList.Create;
      lwma_enumerate_codecs(FCodecs);
      FFormats := TStringList.Create;
    end;
  end;

  destructor TWMADualPassOut.Destroy;
  begin
    if not (csDesigning in ComponentState) then
    begin
      FCodecs.Free;
      FFormats.Free;
    end;
    inherited Destroy;
  end;

  function TWMADualPassOut.GetCodecName;
  begin
    if Index < FCodecs.Count then
      Result := FCodecs.Strings[Index];
  end;

  function TWMADualPassOut.GetFormats;
  begin
    FFormats.Clear;
    if Index < FCodecs.Count then
      lwma_enumerate_codec_formats2(Index, FVBR, FFormats);
    Result := FFormats;
  end;

  function TWMADualPassOut.GetCodecs;
  begin
    Result := FCodecs;
  end;

  function TWMADualPassOut.GetCodecsCount;
  begin
    Result := FCodecs.Count;
  end;

  function TWMADualPassOut.GetFormatsCount;
  begin
    GetFormats(Index);
    Result := FFormats.Count;
  end;

  procedure TWMADualPassOut.Prepare;
  var
    Preprocessor : wma_preprocessor;
    l : Integer;
  begin
    if not (Finput is TAuFileIn) then
      raise EAuException.Create('Only files can be encoded in dual passes');
    FInput.Init;
    lwma_writer_init(Writer, PWideChar(FWideFileName), Self, CallOnDPError);
    if not Writer.Initialized then
      raise Exception.Create('Cannot create file');
    lwma_writer_set_audio_properties3(Writer, FInput.Channels, FInput.BitsPerSample, FInput.SampleRate, FVBR, FCodecIndex, FFormatIndex);
    BufSize := FInput.Channels*FInput.BitsPerSample*FInput.SampleRate div 100;
    GetMem(Buf, BufSize);
   {$WARNINGS OFF}
    if Id3v2Tags.Artist <> '' then
      lwma_writer_set_author(Writer, Id3v2Tags.Artist);
    if Id3v2Tags.Album <> '' then
      lwma_writer_set_album(Writer, Id3v2Tags.Album);
    if Id3v2Tags.Genre <> '' then
      lwma_writer_set_genre(Writer, Id3v2Tags.Genre);
    if Id3v2Tags.Year <> '' then
      lwma_writer_set_year(Writer, Id3v2Tags.Year);
    if Id3v2Tags.Track <> '' then
      lwma_writer_set_track(Writer, Id3v2Tags.Track);
    if Id3v2Tags.Title <> '' then
      lwma_writer_set_title(Writer, Id3v2Tags.Title);
    if Id3v2Tags.Comment <> '' then
      lwma_writer_set_copyright(Writer, Id3v2Tags.Comment);
   {$WARNINGS ON}
    EndOfStream := false;
    lwma_create_preprocessor(Writer, Preprocessor);
    lwma_writer_begin_preprocess(Writer, Preprocessor);
    while not EndOfStream do
    begin
      l := Finput.FillBuffer(Buf, BufSize, EndOfStream);
      lwma_writer_preprocess(Writer, Preprocessor, Buf, l);
    end;
    lwma_preprocessor_free(Writer, Preprocessor);
    FInput.Flush;
    FInput.Init;
    EndOfStream := false;
    FErrorFlag := False;
  end;

  function TWMADualPassOut.DoOutput;
  var
    l : Integer;
  begin
    if FErrorFlag then
     raise EAuException.Create('Windows Media output error');
    Result := True;
    if not CanOutput then Exit;
    if Abort or EndOfStream then
    begin
      Result := False;
      Exit;
    end;
    l := Finput.FillBuffer(Buf, BufSize, EndOfStream);
    lwma_writer_write(Writer, Buf, l);
  end;

  procedure TWMADualPassOut.Done;
  begin
    FInput.Flush;
    lwma_writer_free(Writer);
    FreeMem(Buf);
  end;

  function TWMADualPassOut.GetFormatDesc;
  begin
      GetFormats(CodecIndex);
      if FormatIndex >= FFormats.Count then Result := ''
      else
        Result := FFormats.Strings[FormatIndex];
  end;


end.
