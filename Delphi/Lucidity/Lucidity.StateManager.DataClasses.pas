unit Lucidity.StateManager.DataClasses;

interface

{$INCLUDE Defines.inc}

{$M+}

uses
  uLucidityEnums;

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
  public
    procedure ResetToDefaultValues;
    procedure SanitiseData; // call after reading data from a save file.
  published
    // PitchTracking
    // SampleReset
    // GrainLoop
    // GrainLength
    // GrainRate
    // GrainPosition
    // SamplerLoopBounds
    // SamplerTriggerMode
    // OscShape
    // OscPulseWidth
    // FilterRouting
    // Filter1Type
    // Filter2Type
    // Filter1KeyFollow
    // Filter2KeyFollow
    // AmpVelocityDepth
    // ModVelocityDepth
    // LfoShape1
    // LfoShape2
    // LfoFreqMode1
    // LfoFreqMode2
    // Seq1Clock
    // Seq1Direction
    // StepSeq1Length
    // Seq2Clock
    // Seq2Direction
    // StepSeq2Length
  end;

implementation

uses
  VamLib.Utils;


{ TRegionLoadInfo }

procedure TRegionLoadInfo.ResetToDefaultValues;
begin
  // TODO:HIGH These are default values.
  // Some of these values won't be appropitate. I should
  // change the way they work so when a new region is created
  // I knob what values have been ---
  // Maybe that can be done as part of the sanitisation stage.

  // TODO:HIGH I wonder if other classes will need to have this
  // same default load state applied.


  LowNote        := 0;
  HighNote       := 127;
  LowVelocity    := 0;
  HighVelocity   := 127;
  RootNote       := -1;
  SampleStart    := -1;
  SampleEnd      := -1;
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
  if RootNote = -1 then
  begin
     if (LowNote = 0) and (HighNote = 127)
       then RootNote := 60
       else RootNote := LowNote;
  end;

  Sanitise(fLowNote, 0, 127);
  Sanitise(fHighNote, 0, 127);
  Sanitise(fLowVelocity, 0, 127);
  Sanitise(fHighVelocity, 0, 127);
  Sanitise(fRootNote, 0, 127);
end;

{ TKeyGroupLoadInfo }

procedure TKeyGroupStateInfo.ResetToDefaultValues;
begin

end;

procedure TKeyGroupStateInfo.SanitiseData;
begin

end;

end.
