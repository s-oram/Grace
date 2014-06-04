unit Lucidity.VoiceController;

interface

{$INCLUDE Defines.inc}

uses
  Lucidity.Interfaces,
  VamLib.CpuOverloadWatcher,
  Contnrs,
  Classes,
  uConstants,
  VamLib.ZeroObject,
  soLucidityVoice,
  uLucidityEnums,
  eeGlobals;

type
  TVoiceController = class(TZeroObject)
  private
  protected
    OverloadWatch : TCpuOverloadWatcher;

    Globals : TGlobals;
    Voices  : PArrayOfLucidityVoice;

    FirstNoteLatch_TriggerRequired : boolean;
    FirstNoteLatch_Data1 : byte;
    FirstNoteLatch_Data2 : byte;

    Latch_ReleaseAllOnNoteUp : boolean;

    // TODO: it might be better to use list classes that don't assign/free memory as objects
    // are added and removed.
    InactiveVoices      : TLucidityVoiceList;
    ActiveVoices        : TLucidityVoiceList;
    ReleasedVoices      : TLucidityVoiceList; // voices go here when released.
    TriggeredVoiceStack : TLucidityVoiceList; // Keeps track of last triggerered voice.


    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;

    procedure PolyTrigger(const Data1, Data2 : byte);
    procedure PolyRelease(const Data1, Data2 : byte);

    procedure MonoTrigger(const Data1, Data2 : byte);
    procedure MonoRelease(const Data1, Data2 : byte);

    procedure LatchTrigger(const Data1, Data2 : byte; const NoteStackCount : integer);
    procedure LatchRelease(const Data1, Data2 : byte; const NoteStackCount : integer);

    procedure ProcessTriggerQueue(const TriggerQueue : TObjectList; const MidiData1, MidiData2 : byte; const TriggerVoiceMode : TVoiceMode);
    procedure ProcessTrigger(const KeyGroup : IKeyGroup; Region : IRegion; const MidiData1, MidiData2 : byte; const TriggerVoiceMode : TVoiceMode);

    function FindVoiceToTrigger:TLucidityVoice;
  public
    constructor Create(const aGlobals: TGlobals; const aVoices:PArrayOfLucidityVoice);
    destructor Destroy; override;

    function GetActiveVoiceCount : integer;
    function GetLastTriggeredVoice : TLucidityVoice;
  end;


  TRegionTriggerItem = class
  public
    KeyGroup   : IInterface;
    RegionIntf : IInterface;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  VamLib.Utils,
  Lucidity.KeyGroupManager,
  Lucidity.Types,
  Lucidity.KeyGroup,
  Lucidity.SampleMap;

{ TVoiceController }

constructor TVoiceController.Create(const aGlobals: TGlobals; const aVoices: PArrayOfLucidityVoice);
var
  c1 : integer;
begin
  Globals := aGlobals;
  Voices := aVoices;

  InactiveVoices := TLucidityVoiceList.Create;
  InactiveVoices.OwnsObjects := false;

  ActiveVoices := TLucidityVoiceList.Create;
  ActiveVoices.OwnsObjects := false;

  TriggeredVoiceStack := TLucidityVoiceList.Create;
  TriggeredVoiceStack.OwnsObjects := false;

  ReleasedVoices   := TLucidityVoiceList.Create;
  ReleasedVoices.OwnsObjects := false;


  // add voice objects to inactive voice list.
  for c1 := 0 to kMaxVoiceCount-1 do
  begin
    InactiveVoices.Add(aVoices^[c1])
  end;

  Latch_ReleaseAllOnNoteUp := false;
  FirstNoteLatch_TriggerRequired := false;

  OverloadWatch := TCpuOverloadWatcher.Create;
end;

destructor TVoiceController.Destroy;
begin
  InactiveVoices.Free;
  ActiveVoices.Free;
  ReleasedVoices.Free;
  TriggeredVoiceStack.Free;
  OverloadWatch.Free;
  inherited;
end;

function TVoiceController.GetActiveVoiceCount: integer;
begin
  result := ActiveVoices.Count + ReleasedVoices.Count;
end;

function TVoiceController.GetLastTriggeredVoice: TLucidityVoice;
begin
  if TriggeredVoiceStack.Count > 0
    then result := TriggeredVoiceStack.Last
    else result := nil;
end;





procedure TVoiceController.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  pVoice : PLucidityVoice;
  c1: Integer;
begin
  inherited;

  // route the events to their handlers....

  if MsgID = TLucidMsgID.Audio_PolyNoteTrigger then
  begin
    PolyTrigger(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_PolyNoteRelease then
  begin
    PolyRelease(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_MonoNoteTrigger then
  begin
    MonoTrigger(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_MonoNoteRelease then
  begin
    MonoRelease(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_LegatoNoteTrigger then
  begin
    MonoTrigger(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_LegatoNoteRelease then
  begin
    MonoRelease(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2);
  end;

  if MsgID = TLucidMsgID.Audio_LatchNoteTrigger then
  begin
    LatchTrigger(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2, PMsgData_NoteEvent(Data)^.NoteStackCount);
  end;

  if MsgID = TLucidMsgID.Audio_LatchNoteRelease then
  begin
    LatchRelease(PMsgData_NoteEvent(Data)^.Data1, PMsgData_NoteEvent(Data)^.Data2, PMsgData_NoteEvent(Data)^.NoteStackCount);
  end;

  if MsgID = TLucidMsgID.AudioCommand_QuickReleaseAllNotes then
  begin
    for c1 := ReleasedVoices.Count-1 downto 0 do
    begin
      ReleasedVoices[c1].QuickRelease;
    end;

    for c1 := ActiveVoices.Count-1 downto 0 do
    begin
      ActiveVoices[c1].QuickRelease;
      // WARNING:
      // On first glance it would make sense to add the released voices to the
      // the ReleasedVoices list, but the  AudioCommand_QuickReleaseAllNotes
      // can be sent from the GUI thread.
      // By leaving the voice in the current list, we can postpone the lists
      // being modified until the voice 'clean-up' method is fired when the voice
      // is finished.
    end;
  end;


  if MsgID = TLucidMsgID.Audio_VoiceFinished then
  begin
    pVoice := Data;

    if ActiveVoices.IndexOf(pVoice^) <> -1
      then ActiveVoices.Remove(pVoice^);

    if ReleasedVoices.IndexOf(pVoice^) <> -1
      then ReleasedVoices.Remove(pVoice^);

    TriggeredVoiceStack.Remove(pVoice^);

    // return the voice to the inactive list.
    if InactiveVoices.IndexOf(pVoice^) = -1
      then InactiveVoices.Add(pVoice^);
  end;
end;


procedure TVoiceController.LatchTrigger(const Data1, Data2: byte; const NoteStackCount : integer);
var
  c1: Integer;
  IsLatchedVoiceActive : boolean;
  TriggerQueue : TObjectList;
  TriggerItem  : TRegionTriggerItem;
  kg : IKeyGroup;
  rg : IRegion;
  SampleMap : TSampleMap;
  aVoice : TLucidityVoice;
begin
  SampleMap := (Globals.SampleMapReference as TSampleMap);

  TriggerQueue := TObjectList.Create;
  TriggerQueue.OwnsObjects := true;
  AutoFree(@TriggerQueue);

  if NoteStackCount = 1 then
  begin
    IsLatchedVoiceActive := false;

    // Find if the triggered voice is already active...
    for c1 := ActiveVoices.Count-1 downto 0 do
    begin
      if ActiveVoices[c1].TriggerNote = Data1 then
      begin
        IsLatchedVoiceActive := true;
        Break; //========>>
      end;
    end;
    //========================================================

    if IsLatchedVoiceActive = true then
    begin
      Latch_ReleaseAllOnNoteUp := true;
      FirstNoteLatch_TriggerRequired := true;
      FirstNoteLatch_Data1 := Data1;
      FirstNoteLatch_Data2 := Data2;
    end else
    // if IsLatchedVoiceActive = false then
    begin
      Latch_ReleaseAllOnNoteUp := false;
      FirstNoteLatch_TriggerRequired := false;

      // release all voices now.
      for c1 := ActiveVoices.Count-1 downto 0 do
      begin
        aVoice := ActiveVoices[c1];
        ActiveVoices.Remove(aVoice);
        ReleasedVoices.Add(aVoice);
        aVoice.Release;
      end;

      // Trigger new voices now.
      for c1 := SampleMap.RegionCount-1 downto 0 do
      begin
        rg := SampleMap.Regions[c1];
        kg := rg.GetKeyGroup;
        if IsNoteInsideRegion(rg, Data1, Data2) then
        begin
          // Add this region to the region trigger queue.
          TriggerItem := TRegionTriggerItem.Create;
          TriggerItem.RegionIntf := rg;
          TriggerItem.KeyGroup   := kg;
          TriggerQueue.Add(TriggerItem);
        end;
        if TriggerQueue.Count > 0 then
        begin
          ProcessTriggerQueue(TriggerQueue, Data1, Data2, TVoiceMode.Latch);
          TriggerQueue.Clear;
        end;
      end;
    end;
  end;

  if NoteStackCount > 1 then
  begin
    if (FirstNoteLatch_TriggerRequired) then
    begin
      Latch_ReleaseAllOnNoteUp := false;
      FirstNoteLatch_TriggerRequired := false;

      // release all voices now.
      for c1 := ActiveVoices.Count-1 downto 0 do
      begin
        aVoice := ActiveVoices[c1];
        ActiveVoices.Remove(aVoice);
        ReleasedVoices.Add(aVoice);
        aVoice.Release;
      end;

      // trigger the first note.
      for c1 := SampleMap.RegionCount-1 downto 0 do
      begin
        rg := SampleMap.Regions[c1];
        kg := rg.GetKeyGroup;
        if IsNoteInsideRegion(rg, FirstNoteLatch_Data1, FirstNoteLatch_Data2) then
        begin
          // Add this region to the region trigger queue.
          TriggerItem := TRegionTriggerItem.Create;
          TriggerItem.RegionIntf := rg;
          TriggerItem.KeyGroup   := kg;
          TriggerQueue.Add(TriggerItem);
        end;
      end;
      if TriggerQueue.Count > 0 then
      begin
        ProcessTriggerQueue(TriggerQueue, FirstNoteLatch_Data1, FirstNoteLatch_Data1, TVoiceMode.Latch);
        TriggerQueue.Clear;
      end;
    end;

    // Trigger additional latched notes here.
    for c1 := SampleMap.RegionCount-1 downto 0 do
    begin
      rg := SampleMap.Regions[c1];
      kg := rg.GetKeyGroup;
      if IsNoteInsideRegion(rg, Data1, Data2) then
      begin
        // Add this region to the region trigger queue.
        TriggerItem := TRegionTriggerItem.Create;
        TriggerItem.RegionIntf := rg;
        TriggerItem.KeyGroup   := kg;
        TriggerQueue.Add(TriggerItem);
      end;
      if TriggerQueue.Count > 0 then
      begin
        ProcessTriggerQueue(TriggerQueue, Data1, Data2, TVoiceMode.Latch);
        TriggerQueue.Clear;
      end;
    end;

  end;




end;

procedure TVoiceController.LatchRelease(const Data1, Data2: byte; const NoteStackCount : integer);
var
  c1: Integer;
  aVoice : TLucidityVoice;
begin
  if (NoteStackCount = 0) and (Latch_ReleaseAllOnNoteUp) then
  begin
    Latch_ReleaseAllOnNoteUp := false;

    // release all voices now.
    for c1 := ActiveVoices.Count-1 downto 0 do
    begin
      aVoice := ActiveVoices[c1];
      ActiveVoices.Remove(aVoice);
      ReleasedVoices.Add(aVoice);
      aVoice.Release;
    end;
  end;
end;


procedure TVoiceController.PolyTrigger(const Data1, Data2: byte);
const
  ksf = 23;
  ksr = 44100;
var
  c1, c2: Integer;
  SampleMap : TSampleMap;
  KeyGroups : TKeyGroupManager;
  KeyGroupList : TInterfaceList;
  kg : IKeyGroup;
  rg : IRegion;
  RegionList : TRegionInterfaceList;
  TriggerQueue : TObjectList;
  TriggerItem  : TRegionTriggerItem;
begin
  SampleMap := (Globals.SampleMapReference as TSampleMap);
  KeyGroups := (Globals.KeyGroupsReference as TKeyGroupManager);

  for c1 := 0 to SampleMap.RegionCount-1 do
  begin
    rg := SampleMap.Regions[c1];

    if IsNoteInsideRegion(rg, Data1, Data2) then
    begin
      ProcessTrigger(rg.GetKeyGroup, rg, Data1, Data2, TVoiceMode.Poly);
    end;
  end;
end;

procedure TVoiceController.PolyRelease(const Data1, Data2: byte);
var
  c1 : integer;
  aVoice : TLucidityVoice;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    aVoice := ActiveVoices[c1];

    if (aVoice.TriggerNote = Data1) and (aVoice.LoopMode <> TKeyGroupTriggerMode.OneShot) then
    begin
      ActiveVoices.Remove(aVoice);
      ReleasedVoices.Add(aVoice);
      aVoice.Release;
    end;
  end;
end;

procedure TVoiceController.MonoTrigger(const Data1, Data2: byte);
var
  c1: Integer;
  c2: Integer;
  SampleMap : TSampleMap;
  KeyGroups : TKeyGroupManager;
  KeyGroupList : TInterfaceList;
  kg : IKeyGroup;
  rg : IRegion;
  RegionList : TRegionInterfaceList;
  //KeyGroupLoopMode :  TSamplerLoopMode;
  TriggerQueue : TObjectList;
  TriggerItem  : TRegionTriggerItem;
begin
  // quick release any current voices.
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].QuickRelease;
  end;

  for c1 := ReleasedVoices.Count-1 downto 0 do
  begin
    ReleasedVoices[c1].QuickRelease;
  end;


  for c1 := 0 to SampleMap.RegionCount-1 do
  begin
    rg := SampleMap.Regions[c1];

    if IsNoteInsideRegion(rg, Data1, Data2) then
    begin
      ProcessTrigger(rg.GetKeyGroup, rg, Data1, Data2, TVoiceMode.Mono);
    end;
  end;
end;

procedure TVoiceController.MonoRelease(const Data1, Data2: byte);
var
  c1 : integer;
  aVoice : TLucidityVoice;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    aVoice := ActiveVoices[c1];

    if (aVoice.LoopMode <> TKeyGroupTriggerMode.OneShot) then
    begin
      ActiveVoices.Remove(aVoice);
      ReleasedVoices.Add(aVoice);
      aVoice.Release;
    end;
  end;
end;




procedure TVoiceController.ProcessTriggerQueue(const TriggerQueue: TObjectList; const MidiData1, MidiData2 : byte; const TriggerVoiceMode : TVoiceMode);
const
  ksf = 23;
  ksr = 44100;
var
  TriggerItem  : TRegionTriggerItem;
  c1: Integer;
  aVoice : TLucidityVoice;
  rg : IRegion;
  kg : IKeyGroup;
  kgID : TKeyGroupID;
  TriggerMsg : TMsgData_Audio_VoiceTriggered;
begin
  for c1 := 0 to TriggerQueue.Count-1 do
  begin
    aVoice := FindVoiceToTrigger;
    if assigned(aVoice) then
    begin
      ActiveVoices.Remove(aVoice);
      ReleasedVoices.Remove(aVoice);
      InactiveVoices.Remove(aVoice);
    end else
    begin
      exit; //======================>> exit >>=======>>
    end;

    TriggerItem := TriggerQueue[c1] as TRegionTriggerItem;

    // NOTE: The regions owning key group must be the same as the supplied key group.
    rg := TriggerItem.RegionIntf as IRegion;
    kg := TriggerItem.KeyGroup as IKeyGroup;



    //==== Trigger the Voice ====
    // triggers the voice's envelopes etc.
    (KG.GetObject as TKeyGroup).VoiceParameters.ApplyParametersToVoice(aVoice);
    aVoice.VoiceMode := TriggerVoiceMode;
    aVoice.Trigger(MidiData1, MidiData2, kg, rg);


    //=== send the triggered voice message ====
    // This notfies the audio engine of the voice, the audio will add the voice to
    // it's playback lists...
    kgID := kg.GetID;
    TriggerMsg.Voice      := @aVoice;
    TriggerMsg.KeyGroupID := @kgID;
    Globals.MotherShip.MsgAudio(TLucidMsgID.Audio_VoiceTriggered, @TriggerMsg);

    //==== internal voice list management =====
    if TriggeredVoiceStack.IndexOf(aVoice) <> -1 then TriggeredVoiceStack.Extract(aVoice);
    TriggeredVoiceStack.Add(aVoice);

    if ActiveVoices.IndexOf(aVoice) = -1
      then ActiveVoices.Add(aVoice)
      else raise Exception.Create('Voice is alread in ActiveVoices list.');

    //Important: Increment the groups triggered not count after the voice Trigger() method has been called.
    KG.IncTriggeredNoteCount;
  end;


end;


procedure TVoiceController.ProcessTrigger(const KeyGroup: IKeyGroup; Region: IRegion; const MidiData1, MidiData2: byte; const TriggerVoiceMode: TVoiceMode);
var
  c1: Integer;
  aVoice : TLucidityVoice;
  rg : IRegion;
  kg : IKeyGroup;
  kgID : TKeyGroupID;
  TriggerMsg : TMsgData_Audio_VoiceTriggered;
begin
  aVoice := FindVoiceToTrigger;
  if assigned(aVoice) then
  begin
    ActiveVoices.Remove(aVoice);
    ReleasedVoices.Remove(aVoice);
    InactiveVoices.Remove(aVoice);
  end else
  begin
    exit; //======================>> exit >>=======>>
  end;



  kg := KeyGroup;
  rg := Region;


  //==== Trigger the Voice ====
  // triggers the voice's envelopes etc.
  (KG.GetObject as TKeyGroup).VoiceParameters.ApplyParametersToVoice(aVoice);
  aVoice.VoiceMode := TriggerVoiceMode;
  aVoice.Trigger(MidiData1, MidiData2, kg, rg);


  //=== send the triggered voice message ====
  // This notfies the audio engine of the voice, the audio will add the voice to
  // it's playback lists...
  kgID := kg.GetID;
  TriggerMsg.Voice      := @aVoice;
  TriggerMsg.KeyGroupID := @kgID;
  Globals.MotherShip.MsgAudio(TLucidMsgID.Audio_VoiceTriggered, @TriggerMsg);

  //==== internal voice list management =====
  if TriggeredVoiceStack.IndexOf(aVoice) <> -1 then TriggeredVoiceStack.Extract(aVoice);
  TriggeredVoiceStack.Add(aVoice);

  if ActiveVoices.IndexOf(aVoice) = -1
    then ActiveVoices.Add(aVoice)
    else raise Exception.Create('Voice is alread in ActiveVoices list.');

  //Important: Increment the groups triggered not count after the voice Trigger() method has been called.
  KG.IncTriggeredNoteCount;
end;



function TVoiceController.FindVoiceToTrigger: TLucidityVoice;
var
  c1 : integer;
  aVoice : TLucidityVoice;
  TestVoice : TLucidityVoice;
  CullVoiceNeeded : boolean;
begin
  aVoice := nil;

  if InactiveVoices.Count > 0 then
  begin
    aVoice := InactiveVoices[0];
    InactiveVoices.Remove(aVoice);
  end else
  begin
    if ReleasedVoices.Count > 0 then
    begin
      aVoice := ReleasedVoices[0];
      ReleasedVoices.Extract(aVoice);
      aVoice.Kill;
    end;
  end;

  result := aVoice;



  //Check if we need to release any voices.
  if (ActiveVoices.Count + ReleasedVoices.Count) > kMaxActiveVoices then
  begin
    CullVoiceNeeded := true;

    for c1 := 0 to ReleasedVoices.Count-1 do
    begin
      TestVoice := ReleasedVoices[c1];
      if TestVoice.IsActive and TestVoice.HasBeenQuickReleased = false then
      begin
        CullVoiceNeeded := false;
        TestVoice.QuickRelease;
        break;
      end;
    end;

    if CullVoiceNeeded then
    begin
      for c1 := 0 to ActiveVoices.Count-1 do
      begin
        TestVoice := ActiveVoices[c1];
        if TestVoice.IsActive and TestVoice.HasBeenQuickReleased = false then
        begin
          TestVoice.QuickRelease;
          break;
        end;
      end;
    end;
  end;




end;






{ TRegionTriggerItem }

destructor TRegionTriggerItem.Destroy;
begin
  self.RegionIntf := nil;
  inherited;
end;

end.
