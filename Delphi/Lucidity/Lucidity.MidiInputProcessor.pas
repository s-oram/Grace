unit Lucidity.MidiInputProcessor;

interface

uses
  //VamLib.CpuOverloadWatcher,
  eeGlobals,
  VamLib.ZeroObject,
  VamLib.Utils,
  B2.Filter.CriticallyDampedLowpass,
  soNoteStack,
  uConstants,
  Lucidity.Types,
  Lucidity.Enums;

const
  kMidiNoteCount = 128;

type
  TSuspendedNoteData = record
  public
    IsNoteOffSuspended : array[0..kMidiNoteCount-1] of boolean;
    procedure Reset;
  end;

  TMidiInputProcessor = class(TZeroObject)
  private
    fVoiceGlide: single;
    fVoiceMode: TVoiceMode;
    FIsSustainPedal: boolean;
    procedure SetVoiceGlide(const Value: single);
    procedure SetVoiceMode(const Value: TVoiceMode);
    procedure SetIsSustainPedal(const Value: boolean);
  protected
    Globals             : TGlobals;
    NoteStack           : TNoteStack;
    SuspendedNoteData   : TSuspendedNoteData;

    MidiNote_Filter : TCriticallyDampedLowpass;
    MidiNote_Current : double;
    MidiNote_Target  : integer;

    PitchBend_Filter : TCriticallyDampedLowpass;
    PitchBend_Current : double;
    PitchBend_Target  : double;

    ModWheel_Filter : TCriticallyDampedLowpass;
    ModWheel_Current : double;
    ModWheel_Target  : double;

    GlobalModPoints : PGlobalModulationPoints;


    procedure ProcessNoteOff(const Data1, Data2 : byte);
  public
    constructor Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    procedure NoteOn(const Data1, Data2 : byte);
    procedure NoteOff(const Data1, Data2 : byte);
    procedure PitchBend(const PitchBendAmount : single);
    procedure Modwheel(const Value : single);

    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;

    property VoiceMode    : TVoiceMode read fVoiceMode   write SetVoiceMode;
    property VoiceGlide   : single     read fVoiceGlide  write SetVoiceGlide; //range 0..1

    property IsSustainPedal : boolean read FIsSustainPedal write SetIsSustainPedal; // Set to true if sustain pedal is pressed.
  end;

implementation

uses
  SysUtils;



const
  kMinGlideTime = 5; //milliseconds.

function CalcPitchTransitionTime(const GlideTime : single): single;
begin
  result := StaggeredExpand(GlideTime * GlideTime, kMinGlideTime, 750, 1500, 4000);
end;

{ TKeyBoardProcessor }

constructor TMidiInputProcessor.Create(const aGlobalModPoints: PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  Globals := aGlobals;

  GlobalModPoints := aGlobalModPoints;

  NoteStack := TNoteStack.Create;

  SuspendedNoteData.Reset;

  MidiNote_Filter := TCriticallyDampedLowpass.Create;
  MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);

  MidiNote_Current := 64;
  MidiNote_Target  := 64;

  PitchBend_Filter := TCriticallyDampedLowpass.Create;
  PitchBend_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);

  PitchBend_Current := 0;
  PitchBend_Target  := 0;

  ModWheel_Filter := TCriticallyDampedLowpass.Create;
  ModWheel_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);

  ModWheel_Current := 0;
  ModWheel_Target  := 0;
end;

destructor TMidiInputProcessor.Destroy;
begin
  NoteStack.Free;
  MidiNote_Filter.Free;
  PitchBend_Filter.Free;
  ModWheel_Filter.Free;
  inherited;
end;

procedure TMidiInputProcessor.SetIsSustainPedal(const Value: boolean);
const
  kSuspendedNoteOffVelocity = 64;
var
  c1: Integer;
  DoReleaseCurrentMono : boolean;
begin
  // NOTE: Ordering is important here.

  // 1) Release notes.
  if (FIsSustainPedal <> false) and (Value = false) then
  begin
    case VoiceMode of
      TVoiceMode.Poly:
      begin
        for c1 := (kMidiNoteCount-1) downto 0 do
        begin
          if SuspendedNoteData.IsNoteOffSuspended[c1] then
          begin
            SuspendedNoteData.IsNoteOffSuspended[c1] := false;
            ProcessNoteOff(c1, kSuspendedNoteOffVelocity);
          end;
        end;
      end;

      TVoiceMode.Mono,
      TVoiceMode.Legato:
      begin
        DoReleaseCurrentMono := SuspendedNoteData.IsNoteOffSuspended[MidiNote_Target];
        for c1 := (kMidiNoteCount-1) downto 0 do
        begin
          if (SuspendedNoteData.IsNoteOffSuspended[c1]) and (c1 <> MidiNote_Target) then NoteStack.RemoveNote(c1);
        end;
        if DoReleaseCurrentMono
          then ProcessNoteOff(MidiNote_Target, kSuspendedNoteOffVelocity);
      end;

      TVoiceMode.Latch:
      begin
        // Do nothing for latch mode. The suspend pedal isn't supported in latch mode.
      end;

    else
      raise Exception.Create('Type not handled.');
    end;
  end;

  // 2) Set sustain pedal state after releasing notes.
  FIsSustainPedal := Value;
end;

procedure TMidiInputProcessor.SetVoiceGlide(const Value: single);
begin
  fVoiceGlide := Value;
end;

procedure TMidiInputProcessor.SetVoiceMode(const Value: TVoiceMode);
begin
  fVoiceMode := Value;
end;

procedure TMidiInputProcessor.PitchBend(const PitchBendAmount: single);
begin
  assert(InRange(PitchBendAmount,-1,1));
  PitchBend_Target := PitchBendAmount;
end;

procedure TMidiInputProcessor.Modwheel(const Value: single);
begin
  // TODO: This might need to be smoothed.
  assert(InRange(Value, 0,1));
  GlobalModPoints^.Source_MidiModWheel_Unipolar := Value;
  GlobalModPoints^.Source_MidiModWheel_Bipolar  := Value * 2 - 1;
end;

procedure TMidiInputProcessor.NoteOn(const Data1, Data2: byte);
const
  ksf = 26;
  ksr = 44100;
var
  NoteMsgData : TMsgData_NoteEvent;
begin
  case VoiceMode of
    TVoiceMode.Poly:
    begin
      MidiNote_Current := Data1;
      MidiNote_Target  := Data1;
      MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);
      GlobalModPoints.Source_MonophonicMidiNote := Data1;

      NoteMsgData.Data1 := Data1;
      NoteMsgData.Data2 := Data2;
      Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_PolyNoteTrigger, @NoteMsgData);
    end;

    TVoiceMode.Mono:
    begin
      MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime(VoiceGlide), Globals.FastControlRate);
      MidiNote_Target  := Data1;

      NoteStack.AddNote(Data1, Data2);

      NoteMsgData.Data1 := Data1;
      NoteMsgData.Data2 := Data2;
      Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_MonoNoteTrigger, @NoteMsgData);
    end;

    TVoiceMode.Legato:
    begin
      if NoteStack.Count = 0 then
      begin
        //setup behaviour for new note.
        MidiNote_Target  := Data1;
        MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);

        NoteMsgData.Data1 := Data1;
        NoteMsgData.Data2 := Data2;
        Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_LegatoNoteTrigger, @NoteMsgData);
      end else
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime(VoiceGlide), Globals.FastControlRate);
        MidiNote_Target  := Data1;
      end;

      NoteStack.AddNote(Data1, Data2);
    end;

    TVoiceMode.Latch:
    begin
      NoteStack.AddNote(Data1, Data2);
      NoteMsgData.Data1 := Data1;
      NoteMsgData.Data2 := Data2;
      NoteMsgData.NoteStackCount := NoteStack.Count;
      Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_LatchNoteTrigger, @NoteMsgData);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

end;

procedure TMidiInputProcessor.NoteOff(const Data1, Data2: byte);
begin
  if (IsSustainPedal) and (VoiceMode <> TVoiceMode.Latch) then
  begin
    assert( (Data1 >= 0) and (Data1 <= 127) );
    SuspendedNoteData.IsNoteOffSuspended[Data1] := true;
  end else
  begin
    ProcessNoteOff(Data1, Data2);
  end;
end;

procedure TMidiInputProcessor.ProcessNoteOff(const Data1, Data2: byte);
var
  ActiveNoteChanged : boolean;
  NoteMsgData : TMsgData_NoteEvent;
begin
  case VoiceMode of
    TVoiceMode.Poly:
    begin
      NoteMsgData.Data1 := Data1;
      NoteMsgData.Data2 := Data2;
      Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_PolyNoteRelease, @NoteMsgData);
    end;

    TVoiceMode.Mono:
    begin
      ActiveNoteChanged := NoteStack.RemoveNote(Data1);
      if ActiveNoteChanged then
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime(VoiceGlide), Globals.FastControlRate);

        if NoteStack.Count > 0 then
        begin
          MidiNote_Target  := NoteStack.LastNote^.Data1;

          NoteMsgData.Data1 := NoteStack.LastNote^.Data1;
          NoteMsgData.Data2 := NoteStack.LastNote^.Data2;
          Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_MonoNoteTrigger, @NoteMsgData);
        end else
        begin
          NoteMsgData.Data1 := Data1;
          NoteMsgData.Data2 := Data2;
          Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_MonoNoteRelease, @NoteMsgData);
        end;
      end;
    end;

    TVoiceMode.Legato:
    begin
      ActiveNoteChanged := NoteStack.RemoveNote(Data1);
      if ActiveNoteChanged then
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime(VoiceGlide), Globals.FastControlRate);

        if NoteStack.Count > 0 then
        begin
          MidiNote_Target  := NoteStack.LastNote^.Data1;
        end else
        begin
          NoteMsgData.Data1 := Data1;
          NoteMsgData.Data2 := Data2;
          Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_LegatoNoteRelease, @NoteMsgData);
        end;
      end;
    end;

    TVoiceMode.Latch:
    begin
      NoteStack.RemoveNote(Data1);
      NoteMsgData.Data1 := Data1;
      NoteMsgData.Data2 := Data2;
      NoteMsgData.NoteStackCount := NoteStack.Count;
      Globals.MotherShip.MsgNonVisual(TLucidMsgID.Audio_LatchNoteRelease, @NoteMsgData);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;



procedure TMidiInputProcessor.FastControlProcess;
begin
  if MidiNote_Current <> MidiNote_Target then
  begin
    MidiNote_Current := MidiNote_Filter.Step(MidiNote_Target);
    GlobalModPoints.Source_MonophonicMidiNote := MidiNote_Current;
  end;

  if PitchBend_Current <> PitchBend_Target then
  begin
    PitchBend_Current := PitchBend_Filter.Step(PitchBend_Target);
    assert(PitchBend_Current >= -1);
    assert(PitchBend_Current <= 1);

    GlobalModPoints.Source_MidiPitchBendST := PitchBend_Current  * 12; // Multiple by 12 to have +/-12 semitones pitch shift.
    GlobalModPoints.Source_MidiPitchbend_Unipolar := PitchBend_Current * 0.5 + 0.5; //convert to standard 0..1 parmodulation input range.
    GlobalModPoints.Source_MidiPitchbend_Bipolar  := PitchBend_Current;

    assert(InRange(GlobalModPoints.Source_MidiPitchbend_Unipolar, 0, 1));
  end;
end;

procedure TMidiInputProcessor.SlowControlProcess;
begin

end;

{ TSuspendedNoteData }

procedure TSuspendedNoteData.Reset;
var
  c1: Integer;
begin
  for c1 := 0 to kMidiNoteCount-1 do
  begin
    self.IsNoteOffSuspended[c1] := false;
  end;
end;

end.
