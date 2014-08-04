unit Soundcard;

interface


  // Supported card ID numbers (OBSOLETE. NOT USED ANY MORE)

const

  SNDCARD_ADLIB = 1;
  SNDCARD_SB = 2;
  SNDCARD_PAS = 3;
  SNDCARD_GUS = 4;
  SNDCARD_MPU401 = 5;
  SNDCARD_SB16 = 6;
  SNDCARD_SB16MIDI = 7;
  SNDCARD_UART6850 = 8;
  SNDCARD_GUS16 = 9;
  SNDCARD_MSS = 10;
  SNDCARD_PSS = 11;
  SNDCARD_SSCAPE = 12;
  SNDCARD_PSS_MPU = 13;
  SNDCARD_PSS_MSS = 14;
  SNDCARD_SSCAPE_MSS = 15;
  SNDCARD_TRXPRO = 16;
  SNDCARD_TRXPRO_SB = 17;
  SNDCARD_TRXPRO_MPU = 18;
  SNDCARD_MAD16 = 19;
  SNDCARD_MAD16_MPU = 20;
  SNDCARD_CS4232 = 21;
  SNDCARD_CS4232_MPU = 22;
  SNDCARD_MAUI = 23;
  SNDCARD_PSEUDO_MSS = 24;
  SNDCARD_GUSPNP = 25;
  SNDCARD_UART401 = 26;

  // Sound card numbers 27 to N are reserved. Don't add more numbers here

 //**********************************

 // IOCTL commands for /dev/dsp and /dev/audio

const

  SNDCTL_DSP_RESET = $5000;
  SNDCTL_DSP_SPEED = $c0045002;
  SNDCTL_DSP_STEREO = $c0045003;
  SNDCTL_DSP_GETBLKSIZE = $c0045004;
  SNDCTL_DSP_SAMPLESIZE = $c0045005;
  SNDCTL_DSP_CHANNELS = $c0045006;
  SOUND_PCM_WRITE_CHANNELS = $c0045006;
  SOUND_PCM_WRITE_FILTER = $c0045007;
  SNDCTL_DSP_POST = $5008;
  SNDCTL_DSP_SUBDIVIDE = $c0045009;
  SNDCTL_DSP_SETFRAGMENT = $c004500a;

// Audio data formats (Note! U8=8 and S16_LE=16 for compatibility)*/ }

  SNDCTL_DSP_GETFMTS = $8004500b;
  SNDCTL_DSP_SETFMT = $c0045005;

  AFMT_QUERY = 0;
  AFMT_MU_LAW = 1;
  AFMT_A_LAW  = 2;
  AFMT_IMA_ADPCM = 4;
  AFMT_U8 = 8;
  AFMT_S16_LE = $10;
  AFMT_S16_BE = $20;
  AFMT_S8 = $40;
  AFMT_U16_LE = $80;
  AFMT_U16_BE = $100;
  AFMT_MPEG = $200;

  // 32 bit formats (MSB aligned) formats

  AFMT_S32_LE = $1000;
  AFMT_S32_BE = $2000;

  // AC3 _compressed_ bitstreams (See Programmer's Guide for details).

  AFMT_AC3 = $4000;

  // 24 bit formats (LSB aligned in 32 bit word) formats*/ }
  AFMT_S24_LE = $8000;
  AFMT_S24_BE  = $10000;

(* S/PDIF raw format. In this format the S/PDIF frames (including all
 control and user bits) are included in the data stream. Each sample
 is stored in a 32 bit frame (see IEC-958 for more info). This format
 is supported by very few devices and it's only usable for purposes
 where full access to the control/user bits is required (real time control).
*)

  AFMT_SPDIF_RAW = $20000;

type
  audio_buf_info = record
    fragments: Integer;  // of available fragments (partially usend ones not counted)
    fragstotal: Integer; // Total # of fragments allocated
    fragsize: Integer;   // Size of a fragment in bytes
    bytes: Integer;      // Available space in bytes (includes partially used fragments)
    // Note! 'bytes' could be more than fragments*fragsize*/
  end;

const

  SNDCTL_DSP_GETOSPACE = $8010500c;
  SNDCTL_DSP_GETISPACE = $8010500d;
  SNDCTL_DSP_NONBLOCK = $500e;
  SNDCTL_DSP_GETCAPS = $8004500f;

  DSP_CAP_REVISION = $ff;  // Bits for revision level (0 to 255)
  DSP_CAP_DUPLEX = $100;   // Full duplex record/playback
  DSP_CAP_REALTIME = $200; // Not in use
  DSP_CAP_BATCH = $400;    (* Device has some kind of
                            internal buffers which may
                            cause some delays and
                            decrease precision of timing *)

  DSP_CAP_COPROC = $800;  // Has a coprocessor
  (* Sometimes it's a DSP
     but usually not *)

  DSP_CAP_TRIGGER = $1000;  // Supports SETTRIGGER
  DSP_CAP_MMAP = $2000;     // Supports mmap()
  DSP_CAP_MULTI = $4000;    // Supports multiple open
  DSP_CAP_BIND = $8000;     // Supports binding to front/rear/center/lfe
  DSP_CAP_INPUT = $10000;   // Supports recording
  DSP_CAP_OUTPUT = $20000;  // Supports playback
  DSP_CAP_VIRTUAL = $40000; // Virtuial device
  // Analog/digital control capabilities
  DSP_CAP_ANALOGOUT = $100000;
  DSP_CAP_ANALOGIN = $200000;
  DSP_CAP_DIGITALOUT = $400000;
  DSP_CAP_DIGITALIN = $800000;
  DSP_CAP_ADMASK = $f00000;
(* NOTE! (capabilities & DSP_CAP_ADMASK)==0 means just that the
 digital/analog interface control features are not supported by the
 device/driver. However the device still supports analog, digital or
 both inputs/outputs (depending on the device). See the OSS Programmer's
 Guide for full details. *)

  SNDCTL_DSP_GETTRIGGER = $80045010;
  SNDCTL_DSP_SETTRIGGER = $40045010;

  PCM_ENABLE_INPUT = 1;
  PCM_ENABLE_OUTPUT = 2;

type
  count_info = record
    bytes: Integer;   // Total # of bytes processed
    blocks : Integer; // # of fragment transitions since last time
    ptr: Integer;     // Current DMA pointer value }
  end;

const
  SNDCTL_DSP_GETIPTR = $800c5011;
  SNDCTL_DSP_GETOPTR = $800c5012;

type
  buffmem_desc = record
    buffer: PWord;
    size: Integer;
  end;

const
  SNDCTL_DSP_MAPINBUF = $80085013;
  SNDCTL_DSP_MAPOUTBUF = $80085014;
  SNDCTL_DSP_SETSYNCRO = $5015;
  SNDCTL_DSP_SETDUPLEX = $5016;

// Application's profile defines the way how playback underrun situations should be handled.

(* APF_NORMAL (the default) and APF_NETWORK make the driver to cleanup the
 playback buffer whenever an underrun occurs. This consumes some time
 prevents looping the existing buffer.
 APF_CPUINTENS is intended to be set by CPU intensive applications which
 are likely to run out of time occasionally. In this mode the buffer cleanup is
 disabled which saves CPU time but also let's the previous buffer content to
 be played during the "pause" after the underrun. *)

const
  SNDCTL_DSP_PROFILE = $40045017;

  APF_NORMAL = 0;   // Normal applications
  APF_NETWORK = 1; //  Underruns probably caused by an 'external' delay
  APF_CPUINTENS = 2; // Underruns probably caused by 'overheating' the CPU

  SNDCTL_DSP_GETODELAY = $80045017;


type
  audio_errinfo = record
    play_underruns: Integer;
    rec_overruns: Integer;
    play_ptradjust: Word;
    rec_ptradjust: Word;
    play_errorcount: Integer;
    rec_errorcount: Integer;
    play_lasterror: Integer;
    rec_lasterror: Integer;
    play_errorparm: LongInt;
    rec_errorparm: LongInt;
    filler: array[0..15] of Integer;
  end;


type
  oss_digital_control = record
    caps: LongWord;       // To be defined }
    valid : LongWord;
    cbitin: array[0..23] of Byte;
    ubitin: array[0..23] of Byte;
    cbitout: array[0..23] of Byte;
    ubitout: array[0..23] of Byte;
    outsel: LongWord;
    in_data: Integer;     // Audio/data if autodetectable by the receiver
    in_locked: Integer;	  // Receiver locked
    in_quality: Integer;  // Input signal quality
    in_vbit, out_vbit: Integer;	// V bits
    in_errors: LongWord;  // Various input errro conditions
  end;

const

  DIG_CBITIN_LIMITED = $00000001;
  DIG_CBITIN_DATA = $00000002;
  DIG_CBITIN_BYTE0 = $00000004;
  DIG_CBITIN_FULL = $00000008;
  DIG_CBITIN_MASK = $0000000;
  DIG_CBITOUT_LIMITED = $00000010;
  DIG_CBITOUT_BYTE0 = $00000020;
  DIG_CBITOUT_FULL = $00000040;
  DIG_CBITOUT_DATA = $00000080;
  DIG_CBITOUT_MASK = $000000f0;
  DIG_UBITIN = $00000100;
  DIG_UBITOUT = $00000200;

  VAL_CBITIN = $01;
  VAL_UBITIN = $02;
  VAL_CBITOUT = $04;
  VAL_UBITOUT = $08;
  VAL_ISTATUS = $10;

  OUTSEL_DIGITAL = 1;
  OUTSEL_ANALOG = 2;
  OUTSEL_BOTH = (OUTSEL_DIGITAL or OUTSEL_ANALOG);

  IND_UNKNOWN = 0;
  IND_AUDIO = 1;
  IND_DATA = 2;

  LOCK_NOT_INDICATED = 0;
  LOCK_UNLOCKED = 1;
  LOCK_LOCKED = 2;
  IN_QUAL_NOT_INDICATED = 0;
  IN_QUAL_POOR = 1;
  IN_QUAL_GOOD = 2;

  VBIT_NOT_INDICATED = 0;
  VBIT_OFF = 1;
  VBIT_ON = 2;

  INERR_CRC = $0001;
  INERR_QCODE_CRC = $0002;
  INERR_PARITY = $0004;
  INERR_BIPHASE = $0008;

type
  oss_syncgroup = record
    id: Integer;
    mode: Integer;
  end;

const

  SNDCTL_DSP_GETCHANNELMASK = $c0045040;
  SNDCTL_DSP_BIND_CHANNEL = $c0045041;

  DSP_BIND_QUERY = 0;
  DSP_BIND_FRONT = 1;
  DSP_BIND_SURR = 2;
  DSP_BIND_CENTER_LFE = 4;
  DSP_BIND_HANDSET = 8;
  DSP_BIND_MIC = $10;
  DSP_BIND_MODEM1 = $20;
  DSP_BIND_MODEM2 = $40;
  DSP_BIND_I2S = $80;
  DSP_BIND_SPDIF = $100;

//  Mixer devices

(* There can be up to 20 different analog mixer channels. The }
   SOUND_MIXER_NRDEVICES gives the currently supported maximum.
   The SOUND_MIXER_READ_DEVMASK returns a bitmask which tells
  the devices supported by the particular mixer.
*)

const

  SOUND_MIXER_NRDEVICES = 28;
  SOUND_MIXER_VOLUME = 0;
  SOUND_MIXER_BASS = 1;
  SOUND_MIXER_TREBLE = 2;
  SOUND_MIXER_SYNTH = 3;
  SOUND_MIXER_PCM = 4;
  SOUND_MIXER_SPEAKER = 5;
  SOUND_MIXER_LINE = 6;
  SOUND_MIXER_MIC = 7;
  SOUND_MIXER_CD = 8;
  SOUND_MIXER_IMIX = 9;     // Recording monitor
  SOUND_MIXER_ALTPCM = 10;
  SOUND_MIXER_RECLEV = 11;  // Recording level
  SOUND_MIXER_IGAIN = 12;   // Input gain
  SOUND_MIXER_OGAIN = 13;   // Output gain

(* The AD1848 codec and compatibles have three line level inputs
 (line, aux1 and aux2). Since each card manufacturer have assigned
 different meanings to these inputs, it's inpractical to assign
 specific meanings (line, cd, synth etc.) to them. *)

  SOUND_MIXER_LINE1 = 14;     // Input source 1 (aux1)
  SOUND_MIXER_LINE2 = 15;     // Input source 2 (aux2)
  SOUND_MIXER_LINE3 = 16;     // Input source 3 (line)
  SOUND_MIXER_DIGITAL1 = 17;  // Digital (input)
  SOUND_MIXER_DIGITAL2 = 18;  // Digital (input) 2
  SOUND_MIXER_DIGITAL3 = 19;  // Digital (input) 3
  SOUND_MIXER_PHONEIN = 20;   // Phone input
  SOUND_MIXER_PHONEOUT = 21;  // Phone output
  SOUND_MIXER_VIDEO = 22;     // Video/TV (audio) in
  SOUND_MIXER_RADIO = 23;     // Radio in
  SOUND_MIXER_MONITOR = 24;   // Monitor (usually mic) volume
  SOUND_MIXER_DEPTH = 25;     // 3D 'depth'/'space' parameter
  SOUND_MIXER_CENTER = 26;    // 3D 'center' parameter
  SOUND_MIXER_MIDI = 27;      // Alternative for 'synth'

(* Some on/off settings (SOUND_SPECIAL_MIN - SOUND_SPECIAL_MAX)
 Not counted to SOUND_MIXER_NRDEVICES, but use the same number space *)

  SOUND_ONOFF_MIN = 28;
  SOUND_ONOFF_MAX = 30;

// Note! Number 31 cannot be used since the sign bit is reserved

  SOUND_MIXER_NONE = 31;

(* The following unsupported macros are no longer functional.
 Use SOUND_MIXER_PRIVATE# macros in future. *)

  SOUND_MIXER_ENHANCE = SOUND_MIXER_NONE;
  SOUND_MIXER_MUTE = SOUND_MIXER_NONE;
  SOUND_MIXER_LOUD = SOUND_MIXER_NONE;



  SOUND_DEVICE_LABELS : array[0..27] of PChar = ('Vol ', 'Bass ', 'Trebl', 'Synth', 'Pcm ', 'Spkr ', 'Line ',
                          	 'Mic  ', 'CD   ', 'Mix  ', 'Pcm2 ', 'Rec  ', 'IGain', 'OGain',
                                 'Line1', 'Line2', 'Line3', 'Digital1', 'Digital2', 'Digital3',
				 'PhoneIn', 'PhoneOut', 'Video', 'Radio', 'Monitor',
				 'Depth', 'Center', 'MIDI');


  SOUND_DEVICE_NAMES : array[0..27] of PChar = ('vol', 'bass', 'treble', 'synth', 'pcm', 'speaker', 'line',
				 'mic', 'cd', 'mix', 'pcm2', 'rec', 'igain', 'ogain',
				 'line1', 'line2', 'line3', 'dig1', 'dig2', 'dig3',
				 'phin', 'phout', 'video', 'radio', 'monitor',
				 'depth', 'center', 'midi');

// Device bitmask identifiers

  SOUND_MIXER_RECSRC = $ff;   // Arg contains a bit for each recording source
  SOUND_MIXER_DEVMASK = $fe;  // Arg contains a bit for each supported device
  SOUND_MIXER_RECMASK = $fd;  // Arg contains a bit for each supported recording source
  SOUND_MIXER_CAPS = $fc;
  SOUND_CAP_EXCL_INPUT = $1;   // Only one recording source at a time

  SOUND_MIXER_STEREODEVS = $fb;  // Mixer channels supporting stereo

// OSS/Free ONLY
  SOUND_MIXER_OUTSRC = $fa;   // Arg contains a bit for each input source to output
  SOUND_MIXER_OUTMASK = $f9;  // Arg contains a bit for each supported input source to output
// OSS/Free ONLY

// Device mask bits

  SOUND_MASK_VOLUME = 1 shl SOUND_MIXER_VOLUME;
  SOUND_MASK_BASS = 1 shl SOUND_MIXER_BASS;
  SOUND_MASK_TREBLE = 1 shl SOUND_MIXER_TREBLE;
  SOUND_MASK_SYNTH =  1 shl SOUND_MIXER_SYNTH;
  SOUND_MASK_PCM = 1 shl SOUND_MIXER_PCM;
  SOUND_MASK_SPEAKER = 1 shl SOUND_MIXER_SPEAKER;
  SOUND_MASK_LINE = 1 shl SOUND_MIXER_LINE;
  SOUND_MASK_MIC = 1 shl SOUND_MIXER_MIC;
  SOUND_MASK_CD = 1 shl SOUND_MIXER_CD;
  SOUND_MASK_IMIX = 1 shl SOUND_MIXER_IMIX;
  SOUND_MASK_ALTPCM = 1 shl SOUND_MIXER_ALTPCM;
  SOUND_MASK_RECLEV = 1 shl SOUND_MIXER_RECLEV;
  SOUND_MASK_IGAIN = 1 shl SOUND_MIXER_IGAIN;
  SOUND_MASK_OGAIN = 1 shl SOUND_MIXER_OGAIN;
  SOUND_MASK_LINE1 = 1 shl SOUND_MIXER_LINE1;
  SOUND_MASK_LINE2 = 1 shl SOUND_MIXER_LINE2;
  SOUND_MASK_LINE3 = 1 shl SOUND_MIXER_LINE3;
  SOUND_MASK_DIGITAL1 = 1 shl SOUND_MIXER_DIGITAL1;
  SOUND_MASK_DIGITAL2 = 1 shl SOUND_MIXER_DIGITAL2;
  SOUND_MASK_DIGITAL3 = 1 shl SOUND_MIXER_DIGITAL3;
  SOUND_MASK_PHONEIN = 1 shl SOUND_MIXER_PHONEIN;
  SOUND_MASK_PHONEOUT = 1 shl SOUND_MIXER_PHONEOUT;
  SOUND_MASK_RADIO = 1 shl SOUND_MIXER_RADIO;
  SOUND_MASK_VIDEO = 1 shl SOUND_MIXER_VIDEO;
  SOUND_MASK_MONITOR = 1 shl SOUND_MIXER_MONITOR;
  SOUND_MASK_DEPTH = 1 shl SOUND_MIXER_DEPTH;
  SOUND_MASK_CENTER = 1 shl SOUND_MIXER_CENTER;
  SOUND_MASK_MIDI = 1 shl SOUND_MIXER_MIDI;

  SOUND_MIXER_READ_VOLUME = $80044d00;
  SOUND_MIXER_READ_BASS = $80044d01;
  SOUND_MIXER_READ_TREBLE = $80044d02;
  SOUND_MIXER_READ_SYNTH = $80044d03;
  SOUND_MIXER_READ_PCM = $80044d04;
  SOUND_MIXER_READ_SPEAKER = $80044d05;
  SOUND_MIXER_READ_LINE = $80044d06;
  SOUND_MIXER_READ_MIC = $80044d07;
  SOUND_MIXER_READ_CD = $80044d08;
  SOUND_MIXER_READ_IMIX = $80044d09;
  SOUND_MIXER_READ_ALTPCM = $80044d0a;
  SOUND_MIXER_READ_RECLEV = $80044d0b;
  SOUND_MIXER_READ_IGAIN = $80044d0c;
  SOUND_MIXER_READ_OGAIN = $80044d0d;
  SOUND_MIXER_READ_LINE1 = $80044d0e;
  SOUND_MIXER_READ_LINE2 = $80044d0f;
  SOUND_MIXER_READ_LINE3 = $80044d10;
  SOUND_MIXER_READ_RECSRC = $80044dff;
  SOUND_MIXER_READ_RECMASK = $80044dfd;
  SOUND_MIXER_READ_DEVMASK = $80044dfe;
  SOUND_MIXER_READ_STEREODEVS = $80044dfb;
  SOUND_MIXER_READ_CAPS = $80044dfc;

  SOUND_MIXER_WRITE_VOLUME = $c0044d00;
  SOUND_MIXER_WRITE_BASS = $c0044d01;
  SOUND_MIXER_WRITE_TREBLE = $c0044d02;
  SOUND_MIXER_WRITE_SYNTH = $c0044d03;
  SOUND_MIXER_WRITE_PCM = $c0044d04;
  SOUND_MIXER_WRITE_SPEAKER = $c0044d05;
  SOUND_MIXER_WRITE_LINE = $c0044d06;
  SOUND_MIXER_WRITE_MIC = $c0044d07;
  SOUND_MIXER_WRITE_CD = $c0044d08;
  SOUND_MIXER_WRITE_IMIX = $c0044d09;
  SOUND_MIXER_WRITE_ALTPCM = $c0044d0a;
  SOUND_MIXER_WRITE_RECLEV = $c0044d0b;
  SOUND_MIXER_WRITE_IGAIN = $c0044d0c;
  SOUND_MIXER_WRITE_OGAIN = $c0044d0d;
  SOUND_MIXER_WRITE_LINE1 = $c0044d0e;
  SOUND_MIXER_WRITE_LINE2 = $c0044d0f;
  SOUND_MIXER_WRITE_LINE3 = $c0044d10;
  SOUND_MIXER_WRITE_RECSRC = $c0044dff;
  SOUND_MIXER_INFO = $805c4d65;

type
  mixer_info = record
    id: array[0..15] of Char;
    name: array[0..31] of Char;
    modify_counter: Integer;
    fillers: array[0..9] of Integer;
  end;

const
  SOUND_MIXER_ACCESS = $c0804d66;
  SOUND_MIXER_GETLEVELS = $c0a44d74;
  SOUND_MIXER_SETLEVELS = $c0a44d75;

(* SOUND_MIXER_GETLEVELS and SOUND_MIXER_SETLEVELS calls can be used
 for querying current mixer settings from the driver and for loading
 default volume settings _prior_ activating the mixer (loading
 doesn't affect current state of the mixer hardware). These calls
 are for internal use only. *)

type
  mixer_vol_table = record
    num: Integer; // Index to volume table
    name: array[0..31] of Char;
    levels: array[0..31] of Integer;
  end;

implementation
end.
