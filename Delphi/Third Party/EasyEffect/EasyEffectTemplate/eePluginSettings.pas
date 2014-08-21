unit eePluginSettings;

interface

{$INCLUDE Defines.inc}

type
  // TProcessBufferType - Controls how the ProcessReplacing audio block is split.
  // - pbSplit: the buffer is split at midi events.
  // - pbSingle: the buffer is processed in one block.
  // - pbMidiOnly: audio isn't processed. Midi is processed in a single block.

  TProcessBufferType = (pbSplit, pbSingle, pbMidiOnly);


  TeePluginSettings = class
  private
    fPluginName: string;
    fPluginVendor: string;
    fPluginVersion: string;
    fNumberOfPrograms: integer;
    fVstPlugCatagory: longint;
    fVstUniqueId: AnsiString;
    fHasMidiIn: boolean;
    fHasMidiOut: boolean;
    fUseHostGui: boolean;
    fPresetsAreChunks: boolean;
    fIsSynth: boolean;
    fInitialInputCount: integer;
    fIntiialOutputcount: integer;
    fMaxBlockSize: integer;
    fProcessBufferType: TProcessBufferType;
    fPluginReleaseVersion: string;
    fInitialGuiWidth: integer;
    fInitialGuiHeight: integer;
    fSoftBypass: boolean;
    fIsOverSamplingEnabled: boolean;
    fOverSampleFactor: integer;
    fFastControlRateDivision: integer;
    fSlowControlRateDivision: integer;
  public
    constructor Create;
	  destructor Destroy; override;

    property PluginName            : string  read fPluginName           write fPluginName;
    property PluginVendor          : string  read fPluginVendor         write fPluginVendor;
    property PluginVersion         : string  read fPluginVersion        write fPluginVersion;   //ie Version 1, Version 2
    property PluginReleaseVersion  : string  read fPluginReleaseVersion write fPluginReleaseVersion;  // 1.0.10

    property NumberOfPrograms      : integer read fNumberOfPrograms     write fNumberOfPrograms;

    property VstPlugCatagory       : longint    read fVstPlugCatagory   write fVstPlugCatagory;
    property VstUniqueId           : Ansistring read fVstUniqueId       write fVstUniqueId;

    property HasMidiIn             : boolean read fHasMidiIn            write fHasMidiIn;
    property HasMidiOut            : boolean read fHasMidiOut           write fHasMidiOut;

    property UseHostGui            : boolean read fUseHostGui           write fUseHostGui;
    property PresetsAreChunks      : boolean read fPresetsAreChunks     write fPresetsAreChunks;
    property IsSynth               : boolean read fIsSynth              write fIsSynth;
    property SoftBypass            : boolean read fSoftBypass           write fSoftBypass;

    property InitialInputCount     : integer read fInitialInputCount    write fInitialInputCount;
    property InitialOutputCount    : integer read fIntiialOutputcount   write fIntiialOutputcount;
    property InitialGuiWidth       : integer read fInitialGuiWidth      write fInitialGuiWidth;
    property InitialGuiHeight      : integer read fInitialGuiHeight     write fInitialGuiHeight;

    property MaxBlockSize          : integer read fMaxBlockSize         write fMaxBlockSize;  //Larger audio blocks will be split into smaller pieces.

    property FastControlRateDivision : integer read fFastControlRateDivision  write fFastControlRateDivision;
    property SlowControlRateDivision : integer read fSlowControlRateDivision  write fSlowControlRateDivision;

    property ProcessBufferType     : TProcessBufferType read fProcessBufferType write fProcessBufferType;

    property IsOverSamplingEnabled : boolean read fIsOverSamplingEnabled write fIsOverSamplingEnabled;
    property OverSampleFactor      : integer read fOverSampleFactor      write fOverSampleFactor;
  end;



implementation

uses
  eePluginDataDir;

{ TeePluginSettings }

constructor TeePluginSettings.Create;
begin
  PluginName            := 'Easy Effect Template';
  PluginVendor          := 'One Small Clue';
  PluginVersion         := 'Version 1';
  PluginReleaseVersion  := '0.1';

  NumberOfPrograms      := 64;

  VstPlugCatagory       := 0;
  VstUniqueId           := 'Plug';

  HasMidiIn             := true;
  HasMidiOut            := false;

  UseHostGui            := true;
  PresetsAreChunks      := false;
  IsSynth               := false;
  SoftBypass            := false;

  InitialInputCount     := 0;
  InitialOutputCount    := 0;
  InitialGuiWidth       := 200;
  InitialGuiHeight      := 200;

  MaxBlockSize          := 128;

  ProcessBufferType     := pbSplit;

  IsOverSamplingEnabled   := true;
  OverSampleFactor        := 2;
  FastControlRateDivision := 8;
  SlowControlRateDivision := 16;
end;

destructor TeePluginSettings.Destroy;
begin

  inherited;
end;

end.
