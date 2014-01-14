unit uLucidityVoiceController;

interface

{$INCLUDE Defines.inc}

uses
  Math,
  VamLib.MoreTypes, eeVoiceLogic, soLucidityVoice, eeGlobals,
  uConstants, Lucidity.SampleMap, soLucidityWaveOsc, uLucidityLfo,
  FilterCore.SimperSVF,
  eeSampleFloat,
  uLucidityEnums,
  uLucidityPanner,
  soLucidityFilter,
  uLucidityVCA,
  Lucidity.Osc.GrainStretch,
  soFivePointEnvelope,
  Lucidity.Osc.OneShotSampler,
  Lucidity.Osc.OneShotSampler.SubOsc,
  soADSR,
  B2.Filter.CriticallyDampedLowpass,
  soFilter.LowpassA,
  soFilter.BandPassA,
  soFilter.HighPassA,
  soSawSquareOsc,
  soModMatrix,
  soNoteStack,
  SampleOscUtils,
  soGateEnvelope,
  uLucidityClock,
  uLucidityCustomSampleOsc,
  soDynamicWaveTableOsc,
  soGrainStretchSubOsc,
  uLucidityStepSequencer,
  uLucidityKeyGroup, uLucidityKeyGroupInterface;

const
  kSoftVoiceLimit = 8;

type
  TFirstNoteInfo = record
    Data1 : byte; // Note
    Data2 : byte; // Velocity.
    KeyGroup : IKeyGroup;
    Region   : IRegion;
  end;
  TVoiceReleaseScore = record
    VoiceIndex   : integer;
    ReleaseScore : integer;
  end;

  PLucidityVoiceController = ^TLucidityVoiceController;
  TLucidityVoiceController = class
  private
    fVoiceMode: TVoiceMode;
    fVoiceGlide: single;
    procedure SetVoiceMode(const Value: TVoiceMode);
    procedure SetVoiceGlide(const Value: single);
  protected
    Globals             : TGlobals;
    Voices              : TArrayOfLucidityVoice;
    VoiceControl        : TVoiceControl;

    NoteStack           : TNoteStack;
    FirstNoteInfo       : TFirstNoteInfo;
    LastNoteInfo        : TFirstNoteInfo;

    ActiveVoices        : TLucidityVoiceList;
    TriggeredVoiceStack : TLucidityVoiceList; // Keeps track of last triggerered voice.
    ReleasedVoices      : TLucidityVoiceList; // voices go here when released.

    ReleaseScores : array[0..kMaxVoiceCount] of TVoiceReleaseScore;

    MidiNote_Filter : TCriticallyDampedLowpass;
    MidiNote_Current : double;
    MidiNote_Target  : double;

    PitchBend_Filter : TCriticallyDampedLowpass;
    PitchBend_Current : double;
    PitchBend_Target  : double;

    ModWheel_Filter : TCriticallyDampedLowpass;
    ModWheel_Current : double;
    ModWheel_Target  : double;



    GlobalModPoints : PGlobalModulationPoints;

    procedure CullVoice(const TriggerNote: byte);

    procedure EventHandle_VoiceFinished(Sender : TObject);
    procedure EventHandle_SampleRateChanged(Sender:TObject);
    procedure EventHandle_TempoChanged(Sender:TObject);

    procedure RegionTriggerCheck(const Data1, Data2 : byte; const SampleMap : TSampleMap; const MonoVoicesOnly : boolean);

    function CalcPitchTransitionTime : single;
  public
    constructor Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    function GetActiveVoiceCount : integer;

    function GetLastTriggeredVoice : TLucidityVoice;

    function GetVoiceArray : PArrayOfLucidityVoice;

    function FindVoiceToTrigger : TLucidityVoice;

    procedure NoteOn(const Data1, Data2 : byte; const SampleMap : TSampleMap);
    procedure NoteOff(const Data1, Data2 : byte; const SampleMap: TSampleMap);
    procedure PitchBend(const PitchBendAmount : single);
    procedure Modwheel(const Value : single);

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;

    property VoiceMode    : TVoiceMode read fVoiceMode   write SetVoiceMode;
    property VoiceGlide   : single     read fVoiceGlide  write SetVoiceGlide; //range 0..1
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  eeProfilerV2,
  eeFunctions,
  eeCustomGlobals,
  SysUtils;


const
  kMinGlideTime = 5; //milliseconds.


{ TLucidityVoiceController }

function TLucidityVoiceController.CalcPitchTransitionTime: single;
begin
  //TODO: We need better glide time scaling.
  //result := kMinGlideTime + VoiceGlide * 5000;
  result := StaggeredExpand(fVoiceGlide * fVoiceGlide, kMinGlideTime, 750, 1500, 4000);
end;

constructor TLucidityVoiceController.Create(const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
var
  c1 : integer;
begin
  Globals := aGlobals;
  Globals.AddEventListener(TPluginEvent.SampleRateChanged, EventHandle_SampleRateChanged);
  Globals.AddEventListener(TPluginEvent.TempoChanged,      EventHandle_TempoChanged);

  GlobalModPoints := aGlobalModPoints;

  NoteStack := TNoteStack.Create;

  ActiveVoices := TLucidityVoiceList.Create;
  ActiveVoices.OwnsObjects := false;

  TriggeredVoiceStack := TLucidityVoiceList.Create;
  TriggeredVoiceStack.OwnsObjects := false;

  ReleasedVoices   := TLucidityVoiceList.Create;
  ReleasedVoices.OwnsObjects := false;

  //== Create the voices ==
  SetLength(Voices, kMaxVoiceCount);
  VoiceControl := TVoiceControl.Create;
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    Voices[c1] := TLucidityVoice.Create('VoiceClass', aGlobalModPoints, aGlobals);
    Voices[c1].VoiceID := c1;
    Voices[c1].OnFinish := EventHandle_VoiceFinished;
    VoiceControl.AddVoice(Voices[c1]);
  end;

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

destructor TLucidityVoiceController.Destroy;
var
  c1 : integer;
begin
  ActiveVoices.Free;
  TriggeredVoiceStack.Free;
  ReleasedVoices.Free;

  //Important: Free the voice control before freeing all voices.
  FreeAndNil(VoiceControl);
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    Voices[c1].Free;
  end;
  //========================
  SetLength(Voices, 0);

  NoteStack.Free;

  MidiNote_Filter.Free;
  PitchBend_Filter.Free;
  ModWheel_Filter.Free;

  inherited;
end;

procedure TLucidityVoiceController.EventHandle_SampleRateChanged(Sender: TObject);
begin
  //TODO: I don't think kMinGlideTime should be used here for smoothing the MIDI input of these sources.
  MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);
  PitchBend_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);
  ModWheel_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);
end;

procedure TLucidityVoiceController.EventHandle_TempoChanged(Sender: TObject);
begin

end;

procedure TLucidityVoiceController.EventHandle_VoiceFinished(Sender: TObject);
var
  Index : integer;
  aVoice : TLucidityVoice;
  c1 : integer;
begin
  aVoice := Sender as TLucidityVoice;

  Index := ActiveVoices.IndexOf(aVoice);
  assert(Index <> -1);
  ActiveVoices.Delete(Index);

  Index := TriggeredVoiceStack.IndexOf(aVoice);
  if Index <> -1 then TriggeredVoiceStack.Delete(Index);


  // remove any inactive voices from the released voices list
  for c1 := ReleasedVoices.Count-1 downto 0 do
  begin
    if ReleasedVoices[c1].IsActive = false then
    begin
      ReleasedVoices.Delete(c1);
    end;
  end;



end;

function TLucidityVoiceController.FindVoiceToTrigger: TLucidityVoice;
begin
  result := VoiceControl.GetInactiveVoice as TLucidityVoice;
end;

function TLucidityVoiceController.GetLastTriggeredVoice: TLucidityVoice;
var
  c1: Integer;
begin
  for c1 := TriggeredVoiceStack.Count-1 downto 0 do
  begin
    if TriggeredVoiceStack[c1].IsActive then
    begin
      result := TriggeredVoiceStack[c1];
      exit;
    end;
  end;

  // if we've made it this var, no voice has been found.
  result := nil;
end;

function TLucidityVoiceController.GetVoiceArray: PArrayOfLucidityVoice;
begin
  result := @Voices;
end;

procedure TLucidityVoiceController.Modwheel(const Value: single);
begin
  assert(InRange(Value, 0,1));
  GlobalModPoints^.Source_MidiModWheel := Value;
end;

function TLucidityVoiceController.GetActiveVoiceCount: integer;
begin
  result := ActiveVoices.Count;
end;

procedure TLucidityVoiceController.CullVoice(const TriggerNote: byte);
var
  c1: Integer;
  AgeModifier : integer;
  DistanceModifier : integer;
  LowNote : integer;
  HighNote : integer;
  LowIndex : integer;
  HighIndex : integer;

  LowScore      : integer;
  LowScoreIndex : integer;
begin
  if ReleasedVoices.Count = 1 then
  begin
    ReleasedVoices[0].QuickRelease;
    ReleasedVoices.Delete(0);
    exit;//===============================>> exit >>=================>>
  end;

  for c1 := 0 to ReleasedVoices.Count-1 do
  begin
    AgeModifier := c1;
    DistanceModifier := abs(ReleasedVoices[c1].TriggerNote - TriggerNote);

    ReleaseScores[c1].VoiceIndex := c1;
    ReleaseScores[c1].ReleaseScore := AgeModifier + DistanceModifier;
  end;


  LowNote  := ReleasedVoices[0].TriggerNote;
  HighNote := ReleasedVoices[0].TriggerNote;
  LowIndex := 0;
  HighIndex := 0;

  for c1 := 1 to ReleasedVoices.Count-1 do
  begin
    if ReleasedVoices[c1].TriggerNote < LowNote then
    begin
      LowNote := ReleasedVoices[c1].TriggerNote;
      LowIndex := c1;
    end;

    if ReleasedVoices[c1].TriggerNote > HighNote then
    begin
      HighNote := ReleasedVoices[c1].TriggerNote;
      HighIndex := c1;
    end;
  end;

  ReleaseScores[LowIndex].ReleaseScore  := ReleaseScores[LowIndex].ReleaseScore + 24;
  ReleaseScores[HighIndex].ReleaseScore := ReleaseScores[HighIndex].ReleaseScore + 24;

  LowScore := ReleaseScores[0].ReleaseScore;
  LowScoreIndex := 0;

  for c1 := 1 to ReleasedVoices.Count-1 do
  begin
    if ReleaseScores[c1].ReleaseScore < LowScore then
    begin
      LowScore := ReleaseScores[c1].ReleaseScore;
      LowScoreIndex := c1;
    end;
  end;

  ReleasedVoices[LowScoreIndex].QuickRelease;
  ReleasedVoices.Delete(LowScoreIndex);
end;

procedure TLucidityVoiceController.SetVoiceGlide(const Value: single);
begin
  fVoiceGlide := Value;
end;

procedure TLucidityVoiceController.SetVoiceMode(const Value: TVoiceMode);
var
  c1 : integer;
begin
  if Value <> fVoiceMode then
  begin
    fVoiceMode := Value;

    for c1 := ActiveVoices.Count-1 downto 0 do
    begin
      ActiveVoices[c1].QuickRelease;
    end;

    NoteStack.Clear;
  end;
end;


procedure TLucidityVoiceController.NoteOn(const Data1, Data2: byte; const SampleMap: TSampleMap);
var
  AVC : integer;
  c1: Integer;
  aRegion : IRegion;
  aVoice : TLucidityVoice;
  KG : IKeyGroup;
  UseGlide : boolean;
  IsTriggered: boolean;
begin
  // TODO: Mono and Legato voices always track the monophonic pitch.
  // With very short glide times there is a very slight click when new
  // notes are triggered. I think the very short glide time could
  // becausing discontinuities in both the old note being faded out
  // and the new note being triggered.
  // Possibly it would be better to disable glide altogether when
  // using the minimum glide time.
  // Current each voice class tracks the pitch depending on the voice mode.
  // I think if the above idea is followed, the tracking should disassociated
  // with the voice mode. pitch tracking should be turned on or off with
  // a dedicated property, linked to the glide time. IE for very short glide times
  // pitch tracking will be disabled.

  // TODO:
  // When triggering the same note very quickly with very short amp attack times,
  // the new voice has phase interference with the previous voices that are fading
  // out (even with quick release times).
  // One possible fix would be to not trigger a new voice if an existing voice
  // has the same trigger note, and merely re-trigger the current existing voice.
  // This would be good for Poly and Mono voice modes. (Legato voice doesn't have
  // this problem I think.)

  case VoiceMode of
    TVoiceMode.Poly:
    begin
      MidiNote_Current := Data1;
      MidiNote_Target  := Data1;
      MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);
      GlobalModPoints.Source_MonophonicMidiNote := Data1;

      //look for regions to trigger...
      RegionTriggerCheck(Data1, Data2, SampleMap, false);

      // some extra voice logic...
      AVC := ActiveVoices.Count;
      if (AVC > kSoftVoiceLimit) and (ReleasedVoices.Count > 0) then
      begin
        CullVoice(Data1);
      end;
    end;

    TVoiceMode.Mono:
    begin
      MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime, Globals.FastControlRate);
      MidiNote_Target  := Data1;

      // look for voices to release.
      for c1 := ActiveVoices.Count-1 downto 0 do
      begin
        aVoice := ActiveVoices[c1];
        if (aVoice.TriggerNote <> Data1) then aVoice.QuickRelease;
      end;

      //look for regions to trigger...
      RegionTriggerCheck(Data1, Data2, SampleMap, false);

      NoteStack.AddNote(Data1, Data2);
    end;

    TVoiceMode.Legato:
    begin
      if NoteStack.Count = 0 then
      begin
        // QuickRelease an active voices...
        for c1 := ActiveVoices.Count-1 downto 0 do ActiveVoices[c1].QuickRelease;

        //setup behaviour for new note.
        MidiNote_Target  := Data1;
        MidiNote_Filter.SetTransitionTime(kMinGlideTime, Globals.FastControlRate);

        //look for regions to trigger...
        RegionTriggerCheck(Data1, Data2, SampleMap, false);
      end else
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime, Globals.FastControlRate);
        MidiNote_Target  := Data1;
      end;

      NoteStack.AddNote(Data1, Data2);
    end;
  else
    raise Exception.Create('Type not handled.');
  end;

end;

procedure TLucidityVoiceController.NoteOff(const Data1, Data2: byte; const SampleMap: TSampleMap);
var
  c1 : integer;
  IsActive, HasBeenReleased, HasBeenQuickReleased : boolean;
  Amp : single;
  aVoice : TLucidityVoice;
  ActiveNoteChanged : boolean;
  LastNoteData : PNoteData;
  UseQuickRelease : boolean;
begin
  case VoiceMode of
    TVoiceMode.Poly:
    begin
      for c1 := 0 to ActiveVoices.Count-1 do
      begin
        aVoice := ActiveVoices[c1];
        aVoice.GetVoiceState(IsActive, HasBeenReleased, HasBeenQuickReleased, Amp);
        if (aVoice.TriggerNote = Data1) and (HasBeenReleased = false) then
        begin
          if ReleasedVoices.IndexOf(aVoice) = -1 then ReleasedVoices.Add(aVoice);
          aVoice.Release;
        end;
      end;
    end;

    TVoiceMode.Mono:
    begin
      ActiveNoteChanged := NoteStack.RemoveNote(Data1);
      if ActiveNoteChanged then
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime, Globals.FastControlRate);
        if NoteStack.Count > 0 then
        begin
          for c1 := 0 to ActiveVoices.Count-1 do ActiveVoices[c1].QuickRelease;
          RegionTriggerCheck(NoteStack.LastNote^.Data1, NoteStack.LastNote^.Data2, SampleMap, true);
          MidiNote_Target  := NoteStack.LastNote^.Data1;
        end else
        begin
          for c1 := 0 to ActiveVoices.Count-1 do ActiveVoices[c1].Release;
        end;
      end;
    end;

    TVoiceMode.Legato:
    begin
      ActiveNoteChanged := NoteStack.RemoveNote(Data1);
      if ActiveNoteChanged then
      begin
        MidiNote_Filter.SetTransitionTime(CalcPitchTransitionTime, Globals.FastControlRate);
        if NoteStack.Count > 0 then
        begin
          MidiNote_Target  := NoteStack.LastNote^.Data1;
        end else
        begin
          for c1 := 0 to ActiveVoices.Count-1 do ActiveVoices[c1].Release;
        end;
      end;
    end;

  else
    raise Exception.Create('Type not handled.');
  end;

end;

procedure TLucidityVoiceController.PitchBend(const PitchBendAmount: single);
begin
  assert(InRange(PitchBendAmount,-1,1));
  PitchBend_Target := PitchBendAmount;
end;



procedure TLucidityVoiceController.RegionTriggerCheck(const Data1, Data2: byte; const SampleMap: TSampleMap; const MonoVoicesOnly : boolean);
var
  AVC : integer;
  c1: Integer;
  aRegion : IRegion;
  aVoice : TLucidityVoice;
  KG : IKeyGroup;
  UseGlide : boolean;
  IsTriggered: boolean;
begin
  for c1 := 0 to SampleMap.RegionCount-1 do
  begin
    aRegion := SampleMap.Regions[c1];
    if (aRegion.GetProperties^.SampleDataLoaded) and (IsNoteInsideRegion(aRegion, Data1, Data2)) then
    begin
      aVoice := FindVoiceToTrigger;
      if assigned(aVoice) then
      begin
        KG := aRegion.GetKeyGroup;
        if not assigned(KG) then raise Exception.Create('KeyGroup not assigned.');

        if (VoiceMode = TVoiceMode.Poly) and (MonoVoicesOnly = false) then
        begin
          IsTriggered := true;
          (KG.GetObject as TKeyGroup).VoiceParameters.ApplyParametersToVoice(aVoice);
          aVoice.VoiceMode  := VoiceMode;
          aVoice.Trigger(Data1, Data2, aRegion.GetKeyGroup, aRegion, KG.GetModConnections_OLD);
        end else
        if (VoiceMode = TVoiceMode.Mono) then
        begin
          IsTriggered := true;
          (KG.GetObject as TKeyGroup).VoiceParameters.ApplyParametersToVoice(aVoice);
          aVoice.VoiceMode  := VoiceMode;
          aVoice.Trigger(Data1, Data2, aRegion.GetKeyGroup, aRegion, KG.GetModConnections_OLD);
        end else
        if (VoiceMode = TVoiceMode.Legato) then
        begin
          IsTriggered := true;
          (KG.GetObject as TKeyGroup).VoiceParameters.ApplyParametersToVoice(aVoice);
          aVoice.VoiceMode  := VoiceMode;
          aVoice.Trigger(Data1, Data2, aRegion.GetKeyGroup, aRegion, KG.GetModConnections_OLD);
        end else
        begin
          IsTriggered := false;
        end;

        if IsTriggered then
        begin
          if TriggeredVoiceStack.IndexOf(aVoice) <> -1 then TriggeredVoiceStack.Extract(aVoice);
          TriggeredVoiceStack.Add(aVoice);

          if ActiveVoices.IndexOf(aVoice) = -1
            then ActiveVoices.Add(aVoice)
            else raise Exception.Create('Voice is alread in ActiveVoices list.');

          //Important: Increment the groups triggered not count after the voice Trigger() method has been called.
          KG.IncTriggeredNoteCount;
        end;
      end;
    end;
  end;
end;





procedure TLucidityVoiceController.FastControlProcess;
var
  c1: Integer;
begin
  if MidiNote_Current <> MidiNote_Target then
  begin
    MidiNote_Current := MidiNote_Filter.Step(MidiNote_Target);
    GlobalModPoints.Source_MonophonicMidiNote := MidiNote_Current;
  end;

  if PitchBend_Current <> PitchBend_Target then
  begin
    PitchBend_Current := PitchBend_Filter.Step(PitchBend_Target);
    GlobalModPoints.Source_MidiPitchBendST := PitchBend_Current  * 12; // Multiple by 12 to have +/-12 semitones pitch shift.
    GlobalModPoints.Source_MidiPitchbend   := PitchBend_Current * 0.5 + 0.5; //convert to standard 0..1 parmodulation input range.
    assert(InRange(GlobalModPoints.Source_MidiPitchbend, 0, 1));
  end;

  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].FastControlProcess;
  end;
end;

procedure TLucidityVoiceController.SlowControlProcess;
var
  c1: Integer;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].SlowControlProcess;
  end;
end;




procedure TLucidityVoiceController.AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].AudioProcess(Outputs, SampleFrames);
  end;


end;






end.

