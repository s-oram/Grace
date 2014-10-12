unit Lucidity.StateManager.DataClasses;

interface

{$INCLUDE Defines.inc}

{$M+}

uses
  Lucidity.Enums;

type
  TRegionLoadInfo = class
  strict private
    fHighNote: integer;
    fLoopEnd: integer;
    fSampleStart: integer;
    fSampleBeats: integer;
    fSampleVolume: single;
    fSampleFrames: integer;
    fRootNote: integer;
    fLowVelocity: integer;
    fSampleEnd: integer;
    fSampleFileName: string;
    fLowNote: integer;
    fLoopStart: integer;
    fSamplePan: single;
    fHighVelocity: integer;
    fSampleTune: integer;
    fSampleFine: integer;
  private
  public
    procedure ResetToDefaultValues;
    procedure SanitiseData; // call after reading data from a save file.
  published
    property LowNote        : integer read fLowNote        write fLowNote;
    property HighNote       : integer read fHighNote       write fHighNote;
    property LowVelocity    : integer read fLowVelocity    write fLowVelocity;
    property HighVelocity   : integer read fHighVelocity   write fHighVelocity;
    property RootNote       : integer read fRootNote       write fRootNote;
    property SampleStart    : integer read fSampleStart    write fSampleStart;
    property SampleEnd      : integer read fSampleEnd      write fSampleEnd;
    property LoopStart      : integer read fLoopStart      write fLoopStart;
    property LoopEnd        : integer read fLoopEnd        write fLoopEnd;
    property SampleBeats    : integer read fSampleBeats    write fSampleBeats;
    property SampleVolume   : single  read fSampleVolume   write fSampleVolume;
    property SamplePan      : single  read fSamplePan      write fSamplePan;
    property SampleTune     : integer read fSampleTune     write fSampleTune;
    property SampleFine     : integer read fSampleFine     write fSampleFine;
    property SampleFileName : string  read fSampleFileName write fSampleFileName;
    property SampleFrames   : integer read fSampleFrames   write fSampleFrames;
  end;


  TKeyGroupStateInfo = class
  private
    fFilter2Type: TFilterType;
    fSamplerLoopBounds: TSamplerLoopBounds;
    fSamplerTriggerMode: TKeyGroupTriggerMode;
    fFilter2KeyFollow: single;
    fFilter1Type: TFilterType;
    fAmpVelocityDepth: TEnvVelocityDepth;
    fFilter1KeyFollow: single;
    fSeq2Clock: TSequencerClock;
    fStepSeq2Length: TStepSequencerLength;
    fSeq1Clock: TSequencerClock;
    fModVelocityDepth: TEnvVelocityDepth;
    fLfoShape2: TLfoShape;
    fStepSeq1Length: TStepSequencerLength;
    fLfoShape1: TLfoShape;
    fPitchTracking: TPitchTracking;
    fSeq2Direction: TStepSequencerDirection;
    fLfoFreqMode2: TLfoFreqMode;
    fFilterRouting: TFilterRouting;
    fSeq1Direction: TStepSequencerDirection;
    fSampleReset: TClockSource;
    fLfoFreqMode1: TLfoFreqMode;
    fModEnvSnap: TEnvSnap;
    fAmpEnvSnap: TEnvSnap;
  public
    procedure ResetToDefaultValues;
    procedure SanitiseData; // call after reading data from a save file.
  published
    property PitchTracking            : TPitchTracking                     read fPitchTracking           write fPitchTracking;
    property SampleReset              : TClockSource                       read fSampleReset             write fSampleReset;
    property SamplerLoopBounds        : TSamplerLoopBounds                 read fSamplerLoopBounds       write fSamplerLoopBounds;
    property SamplerTriggerMode       : TKeyGroupTriggerMode               read fSamplerTriggerMode      write fSamplerTriggerMode;
    property FilterRouting            : TFilterRouting                     read fFilterRouting           write fFilterRouting;
    property Filter1Type              : TFilterType                        read fFilter1Type             write fFilter1Type;
    property Filter2Type              : TFilterType                        read fFilter2Type             write fFilter2Type;
    property Filter1KeyFollow         : single                             read fFilter1KeyFollow        write fFilter1KeyFollow; //range 0..1
    property Filter2KeyFollow         : single                             read fFilter2KeyFollow        write fFilter2KeyFollow; //range 0..1
    property AmpVelocityDepth         : TEnvVelocityDepth                  read fAmpVelocityDepth        write fAmpVelocityDepth;
    property ModVelocityDepth         : TEnvVelocityDepth                  read fModVelocityDepth        write fModVelocityDepth;
    property AmpEnvSnap               : TEnvSnap                           read fAmpEnvSnap              write fAmpEnvSnap;
    property ModEnvSnap               : TEnvSnap                           read fModEnvSnap              write fModEnvSnap;
    property LfoShape1                : TLfoShape                          read fLfoShape1               write fLfoShape1;
    property LfoShape2                : TLfoShape                          read fLfoShape2               write fLfoShape2;
    property LfoFreqMode1             : TLfoFreqMode                       read fLfoFreqMode1            write fLfoFreqMode1;
    property LfoFreqMode2             : TLfoFreqMode                       read fLfoFreqMode2            write fLfoFreqMode2;
    property Seq1Clock                : TSequencerClock                    read fSeq1Clock               write fSeq1Clock;
    property Seq1Direction            : TStepSequencerDirection            read fSeq1Direction           write fSeq1Direction;
    property StepSeq1Length           : TStepSequencerLength               read fStepSeq1Length          write fStepSeq1Length;
    property Seq2Clock                : TSequencerClock                    read fSeq2Clock               write fSeq2Clock;
    property Seq2Direction            : TStepSequencerDirection            read fSeq2Direction           write fSeq2Direction;
    property StepSeq2Length           : TStepSequencerLength               read fStepSeq2Length          write fStepSeq2Length;
  end;

implementation

uses
  VamLib.Utils;


{ TRegionLoadInfo }

procedure TRegionLoadInfo.ResetToDefaultValues;
begin
  LowNote        := 0;
  HighNote       := 127;
  LowVelocity    := 0;
  HighVelocity   := 127;
  RootNote       := 60; // Default root note is 60, same as SFZ format.
  SampleStart    := -1;
  SampleEnd      := -1; // TODO:HIGH - When using SFZ Format, -1 is a valid value. I should change lucidity so that it matches.
  LoopStart      := -1;
  LoopEnd        := -1;
  SampleBeats    := 0;
  SampleVolume   := 0;
  SamplePan      := 0;
  SampleTune     := 0;
  SampleFine     := 0;
  SampleFileName := '';
  SampleFrames   := -1;
end;

procedure TRegionLoadInfo.SanitiseData;
begin
  Sanitise(fLowNote, 0, 127);
  Sanitise(fHighNote, 0, 127);
  Sanitise(fLowVelocity, 0, 127);
  Sanitise(fHighVelocity, 0, 127);
  Sanitise(fRootNote, 0, 127);


  Sanitise(fSampleVolume, -48, 36);
  Sanitise(fSamplePan, -100, 100);

  Sanitise(fSampleTune, -48, 48);
  Sanitise(fSampleFine, -100, 100);
end;

{ TKeyGroupLoadInfo }

procedure TKeyGroupStateInfo.ResetToDefaultValues;
begin
  SamplerTriggerMode := TKeyGroupTriggerMode.LoopOff;
  SamplerLoopBounds  := TSamplerLoopBounds.LoopPoints;
  PitchTracking := TPitchTracking.Note;
  SampleReset := TClockSource.None;
  AmpEnvSnap := TEnvSnap.SnapOff;
  ModEnvSnap := TEnvSnap.SnapOff;

  FilterRouting            := TFilterRouting.Serial;
  Filter1Type              := TFilterType.ftNone;
  Filter2Type              := TFilterType.ftNone;
  Filter1KeyFollow         := 0.5;
  Filter2KeyFollow         := 0.5;
  AmpVelocityDepth         := TEnvVelocityDepth.Vel80;
  ModVelocityDepth         := TEnvVelocityDepth.Vel80;
  LfoShape1                := TLfoShape.Triangle;
  LfoShape2                := TLfoShape.Triangle;
  LfoFreqMode1             := TLfoFreqMode.Fixed100Millisecond;
  LfoFreqMode2             := TLfoFreqMode.Fixed100Millisecond;
  Seq1Clock                := TSequencerClock.Div_4;
  Seq1Direction            := TStepSequencerDirection.Forwards;
  StepSeq1Length           := TStepSequencerLength.Sixteen;
  Seq2Clock                := TSequencerClock.Div_4;
  Seq2Direction            := TStepSequencerDirection.Forwards;
  StepSeq2Length           := TStepSequencerLength.Sixteen;
end;

procedure TKeyGroupStateInfo.SanitiseData;
begin

end;

end.
