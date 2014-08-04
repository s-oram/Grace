(* CD_ROM Kylix unit
  Translated from cdrom.h by Andrei Borovsky.

  This unit contains declarations translated
  from the huge cdrom.h header file. In fact
  cdrom.h covers a lot of stuff concerning
  CDs, DVDs, writable CDs, etc. Of all these
  there are only the declarations needed for
  reading/playing CDs in this unit. *)

unit cd_rom;

interface

uses
  Libc;

type
  __U8 = Byte;
  request_sense = Byte;
  (* request_sense is a bitmask : veeeeeee
    v - valid
    eeeeeee - error code
  *)
  PRequest_sense = ^request_sense;

const
  EDRIVE_CANT_DO_THIS = EOPNOTSUPP;

 (*
  The CD-ROM IOCTL commands -- these should be supported by }
  all the various cdrom drivers. For the CD-ROM ioctls, we }
  will commandeer byte 0x53, or 'S'. *)

const

  CDROMPAUSE = $5301;        // Pause Audio Operation
  CDROMRESUME = $5302;       // Resume paused Audio Operation
  CDROMPLAYMSF = $5303;      // Play Audio MSF (struct cdrom_msf)
  CDROMPLAYTRKIND = $5304;   // Play Audio Track/index
  CDROMREADTOCHDR = $5305;   // Read TOC header
  CDROMREADTOCENTRY = $5306; // Read TOC entry
  CDROMSTOP =  $5307;        // Stop the cdrom drive
  CDROMSTART = $5308;        // Start the cdrom drive
  CDROMEJECT = $5309;        // Ejects the cdrom media
  CDROMVOLCTRL = $530a;      // Control output volume
  CDROMSUBCHNL = $530b;      // Read subchannel data
  CDROMREADMODE2 = $530c;    // Read CDROM mode 2 data (2336 Bytes)
  CDROMREADMODE1 = $530d;    // Read CDROM mode 1 data (2048 Bytes)
  CDROMREADAUDIO = $530e;    // (struct cdrom_read_audio)
  CDROMEJECT_SW = $530;      // enable(1)/disable(0) auto-ejecting
  CDROMMULTISESSION = $5310; // Obtain the start-of-last-session
  CDROM_GET_MCN = $5311;     // Obtain the "Universal Product Code"
  CDROM_GET_UPC = CDROM_GET_MCN;  // depricated
  CDROMRESET = $5312;        // hard-reset the drive
  CDROMVOLREAD = $5313;      // Get the drive's volume setting
  CDROMREADRAW = $5314;      // read data in raw mode (2352 Bytes)

  { These ioctls are used only used in aztcd.c and optcd.c }

  CDROMREADCOOKED = $5315;   // read data in cooked mode
  CDROMSEEK = $5316;         // seek msf address

  { This ioctl is only used by the scsi-cd driver. }
  { It is for playing audio in logical block addressing mode. }

  CDROMPLAYBLK = $5317;       // (struct cdrom_blk)

  { These ioctls are only used in optcd.c }

  CDROMREADALL = $5318;       // read all 2646 bytes

(* These ioctls are (now) only in ide-cd.c for controlling
  drive spindown time. They should be implemented in the
  Uniform driver, via generic packet commands, GPCMD_MODE_SELECT_10,
 GPCMD_MODE_SENSE_10 and the GPMODE_POWER_PAGE...
 -Erik *)

const

  CDROMGETSPINDOWN = $531d;
  CDROMSETSPINDOWN = $531e;

  (* These ioctls are implemented through the uniform CD-ROM driver
   They _will_ be adopted by all CD-ROM drivers, when all the CD-ROM
   drivers are eventually ported to the uniform CD-ROM driver interface. *)

const

  CDROMCLOSETRAY = $5319;          // pendant of CDROMEJECT
  CDROM_SET_OPTIONS = $5320;       // Set behavior options
  CDROM_CLEAR_OPTIONS = $5321;     // Clear behavior options
  CDROM_SELECT_SPEED = $5322;      // Set the CD-ROM speed
  CDROM_SELECT_DISC = $5323;       // Select disc (for juke-boxes)
  CDROM_MEDIA_CHANGED = $5325;     // Check is media changed
  CDROM_DRIVE_STATUS = $5326;      // Get tray position, etc.
  CDROM_DISC_STATUS = $5327;       // Get disc type, etc.
  CDROM_CHANGER_NSLOTS = $5328;    // Get number of slots
  CDROM_LOCKDOOR = $5329;          // lock or unlock door
  CDROM_DEBUG = $5330;             // Turn debug messages on/off
  CDROM_GET_CAPABILITY = $5331;    // get capabilities

// This ioctl is only used by sbpcd at the moment

  CDROMAUDIOBUFSIZ = $5382; // set the audio buffer size
  CDROM_SEND_PACKET = $5393; // send a packet to the drive
  CDROM_NEXT_WRITABLE = $5394; // get next writable block
  CDROM_LAST_WRITTEN = $5395; // get last block written on disc

//******************************************************}
// CDROM IOCTL structures
//******************************************************}

type

// Address in MSF format
  cdrom_msf0 = record
    minute: __U8;
    second: __U8;
    frame: __U8;
  end;

// Address in either MSF or logical format
  cdrom_addr = record
    case Word of
     1: (msf: cdrom_msf0;);
     2: (lba: Integer;);
  end;



(* cdrom_msf renamed to cdrom_msf_t since there is
   also a CDROM_MSF constant in this unit *)

// This struct is used by the CDROMPLAYMSF ioctl
  cdrom_msf_t = record
    cdmsf_min0: __U8;   // start minute
    cdmsf_sec0: __U8;   // start second
    cdmsf_frame0: __U8; // start frame
    cdmsf_min1: __U8;   // end minute
    cdmsf_sec1: __U8;   // end second
    cdmsf_frame1: __U8; // end frame
  end;

// This struct is used by the CDROMPLAYTRKIND ioctl
  cdrom_ti = record
    cdti_trk0: __U8;    // start track
    cdti_ind0: __U8;    // start index
    cdti_trk1: __U8;    // end track
    cdti_ind1: __U8;    // end index
  end;

// This struct is used by the CDROMREADTOCHDR ioctl
  cdrom_tochdr = record
    cdth_trk0: __U8;    // start track
    cdth_trk1: __U8;    // end track
  end;

// This struct is used by the CDROMVOLCTRL and CDROMVOLREAD ioctls
  cdrom_volctrl = record
    channel0: __U8;
    channel1: __U8;
    channel2: __U8;
    channel3: __U8;
  end;

// This struct is used by the CDROMSUBCHNL ioctl
  cdrom_subchnl = record
    cdsc_format: __U8;
    cdsc_audiostatus: __U8;
    CDSC_ADR_CTRL : __U8; // 4 bits - ADR, 4 bits - CTRL
    cdsc_trk: __U8;
    cdsc_ind: __U8;
    cdsc_absaddr: cdrom_addr;
    cdsc_reladdr: cdrom_addr;
  end;


// This struct is used by the CDROMREADTOCENTRY ioctl
  cdrom_tocentry = record
    cdte_track: __U8;
    cdte_adr_ctrl: __U8;
    cdte_format: __U8;
    cdte_addr: cdrom_addr;
    cdte_datamode: __U8;
  end;

// This struct is used by the CDROMREADMODE1, and CDROMREADMODE2 ioctls
  cdrom_read = record
    cdread_lba: Integer;
    cdread_bufaddr: PChar;
    cdread_buflen: Integer;
  end;

// This struct is used by the CDROMREADAUDIO ioctl
  cdrom_read_audio = record
    addr: cdrom_addr; // frame address
    addr_format: __U8; // CDROM_LBA or CDROM_MSF
    nframes: Integer;  // number of 2352-byte-frames to read at once
    buf: PChar;  // frame buffer (size: nframes*2352 bytes)
  end;

// This struct is used with the CDROMMULTISESSION ioctl
  cdrom_multisession = record
    addr: cdrom_addr;
    (* frame address: start-of-last-session
    (not the new "frame 16"!). Only valid
    if the "xa_flag" is true. *)
    xa_flag: __U8; // 1: "is XA disk"
    addr_format: __U8; // CDROM_LBA or CDROM_MSF }
  end;

(* This struct is used with the CDROM_GET_MCN ioctl.
 Very few audio discs actually have Universal Product Code information,
 which should just be the Medium Catalog Number on the box. Also note
 that the way the codeis written on CD is _not_ uniform across all discs! *)

  cdrom_mcn = record
    medium_catalog_number: array[0..13] of __U8;
   { 13 ASCII digits, null-terminated }
  end;

// This is used by the CDROMPLAYBLK ioctl
type
  cdrom_blk = record
    from: Word;
    len: Word;
  end;

const
  CDROM_PACKET_SIZE = 12;
  CGC_DATA_UNKNOWN = 0;
  CGC_DATA_WRITE = 1;
  CGC_DATA_READ = 2;
  CGC_DATA_NONE = 3;

// for CDROM_PACKET_COMMAND ioctl
type
  cdrom_generic_command = record
    cmd: array[0..CDROM_PACKET_SIZE-1] of Byte;
    buffer: PByte;
    buflen: Word;
    stat: Integer;
    sense: PREQUEST_SENSE;
    data_direction: Byte;
    quiet: Integer;
    timeout: Integer;
    reserved: array[0..0] of Pointer;
  end;

// Some generally useful CD-ROM information -- mostly based on the above
const

  CD_MINS = 74; // max. minutes per CD, not really a limit
  CD_SECS = 60; // seconds per minute
  CD_FRAMES = 75; // frames per second
  CD_SYNC_SIZE = 12; // 12 sync bytes per raw data frame
  CD_MSF_OFFSET = 150; // MSF numbering offset of first frame
  CD_CHUNK_SIZE = 24; // lowest-level 'data bytes piece'
  CD_NUM_OF_CHUNKS = 98; // chunks per frame
  CD_FRAMESIZE_SUB = 96; // subchannel data 'frame' size
  CD_HEAD_SIZE = 4; // header (address) bytes per raw data frame
  CD_SUBHEAD_SIZE = 8; // subheader bytes per raw XA data frame
  CD_EDC_SIZE = 4; // bytes EDC per most raw data frame types
  CD_ZERO_SIZE = 8; // bytes zero per yellow book mode 1 frame
  CD_ECC_SIZE = 276; // bytes ECC per most raw data frame types
  CD_FRAMESIZE = 2048; // bytes per frame, 'cooked' mode
  CD_FRAMESIZE_RAW = 2352; // bytes per frame, 'raw' mode
  CD_FRAMESIZE_RAWER = 2646; // The maximum possible returned bytes

 // most drives don't deliver everything:

  CD_FRAMESIZE_RAW1 = (CD_FRAMESIZE_RAW-CD_SYNC_SIZE); //2340
  CD_FRAMESIZE_RAW0 = (CD_FRAMESIZE_RAW-CD_SYNC_SIZE-CD_HEAD_SIZE); //2336
  CD_XA_HEAD = (CD_HEAD_SIZE+CD_SUBHEAD_SIZE); // 'before data' part of raw XA frame
  CD_XA_TAIL = (CD_EDC_SIZE+CD_ECC_SIZE); // 'after data' part of raw XA frame
  CD_XA_SYNC_HEAD = (CD_SYNC_SIZE+CD_XA_HEAD); // sync bytes + header of XA frame

// CD-ROM address types (cdrom_tocentry.cdte_format)

  CDROM_LBA = $01; // 'logical block': first frame is #0
  CDROM_MSF = $02; // 'minute-second-frame': binary, not bcd here!

//  bit to tell whether track is data or audio (cdrom_tocentry.cdte_ctrl)

  CDROM_DATA_TRACK = $40;

// The leadout track is always 0xAA, regardless of # of tracks on disc

  CDROM_LEADOUT = $AA;

// audio states (from SCSI-2, but seen with other drives, too)

  CDROM_AUDIO_INVALID = $00; // audio status not supported
  CDROM_AUDIO_PLAY = $11; // audio play operation in progress
  CDROM_AUDIO_PAUSED = $12; // audio play operation paused
  CDROM_AUDIO_COMPLETED = $13; // audio play successfully completed
  CDROM_AUDIO_ERROR = $14; // audio play stopped due to error
  CDROM_AUDIO_NO_STATUS = $15; // no current audio status to return

// capability flags used with the uniform CD-ROM driver

  CDC_CLOSE_TRAY = $1; // caddy systems _can't_ close
  CDC_OPEN_TRAY = $2; // but _can_ eject.
  CDC_LOCK = $4; // disable manual eject
  CDC_SELECT_SPEED = $8; // programmable speed
  CDC_SELECT_DISC = $10; // select disc from juke-box
  CDC_MULTI_SESSION = $20; // read sessions>1
  CDC_MCN = $40; // Medium Catalog Number
  CDC_MEDIA_CHANGED = $80; // media changed
  CDC_PLAY_AUDIO = $100; // audio functions
  CDC_RESET = $200; // hard reset device
  CDC_IOCTLS = $400; // driver has non-standard ioctls
  CDC_DRIVE_STATUS = $800; // driver implements drive status
  CDC_GENERIC_PACKET = $1000; // driver implements generic packets
  CDC_CD_R = $2000; // drive is a CD-R
  CDC_CD_RW = $4000; // drive is a CD-RW
  CDC_DVD = $8000; // drive is a DVD
  CDC_DVD_R = $10000; // drive can write DVD-R
  CDC_DVD_RAM = $20000; // drive can write DVD-RAM

// drive status possibilities returned by CDROM_DRIVE_STATUS ioctl

  CDS_NO_INFO = 0; // if not implemented
  CDS_NO_DISC = 1;
  CDS_TRAY_OPEN = 2;
  CDS_DRIVE_NOT_READY = 3;
  CDS_DISC_OK = 4;

(* return values for the CDROM_DISC_STATUS ioctl
  can also return CDS_NO_[INFO|DISC], from above *)

  CDS_AUDIO = 100;
  CDS_DATA_1 = 101;
  CDS_DATA_2 = 102;
  CDS_XA_2_1 = 103;
  CDS_XA_2_2 = 104;
  CDS_MIXED = 105;

// User-configurable behavior options for the uniform CD-ROM driver

  CDO_AUTO_CLOSE = $1; // close tray on first open()
  CDO_AUTO_EJECT = $2; // open tray on last release()
  CDO_USE_FFLAGS = $4; // use O_NONBLOCK information on open
  CDO_LOCK = $8; // lock tray on open files
  CDO_CHECK_TYPE = $10; // check type on open for data

// Special codes used when specifying changer slots.

  CDSL_NONE = $7FFFFFFE;
  CDSL_CURRENT = $7FFFFFFF;

(* For partition based multisession access. IDE can handle 64 partitions
  per drive - SCSI CD-ROM's use minors to differentiate between the
  various drives, so we can't do multisessions the same way there.
  Use the -o session=x option to mount on them. *)

  CD_PART_MAX = 64;
  CD_PART_MASK = (CD_PART_MAX - 1);



implementation

end.
