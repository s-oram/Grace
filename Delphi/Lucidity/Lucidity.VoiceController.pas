unit Lucidity.VoiceController;

interface

{$INCLUDE Defines.inc}

uses
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
    Globals : TGlobals;
    Voices  : PArrayOfLucidityVoice;

    // TODO: it might be better to use list classes that don't assign/free memory as objects
    // are added and removed.
    InactiveVoices      : TLucidityVoiceList;
    ActiveVoices        : TLucidityVoiceList;
    ReleasedVoices      : TLucidityVoiceList; // voices go here when released.
    TriggeredVoiceStack : TLucidityVoiceList; // Keeps track of last triggerered voice.


    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;

    // the voice modes have slightly different trigger methods.
    // I should probably try to combine these.
    procedure PolyTrigger(const Data1, Data2 : byte);
    procedure PolyRelease(const Data1, Data2 : byte);

    procedure MonoTrigger(const Data1, Data2 : byte);
    procedure MonoRelease(const Data1, Data2 : byte);
    //procedure LegatoTrigger(const Data1, Data2 : byte);
    // All voice modes share the same release code.

    procedure ProcessTriggerQueue(const TriggerQueue : TObjectList; const MidiData1, MidiData2 : byte; const TriggerVoiceMode : TVoiceMode);

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
  uKeyGroupManager,
  Lucidity.Types,
  Lucidity.Interfaces,
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

end;

destructor TVoiceController.Destroy;
begin
  InactiveVoices.Free;
  ActiveVoices.Free;
  ReleasedVoices.Free;
  TriggeredVoiceStack.Free;
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

procedure TVoiceController.PolyTrigger(const Data1, Data2: byte);
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
  SampleMap := (Globals.SampleMapReference as TSampleMap);
  KeyGroups := (Globals.KeyGroupsReference as TKeyGroupManager);

  KeyGroupList := TInterfaceList.Create;
  AutoFree(@KeyGroupList);

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);

  TriggerQueue := TObjectList.Create;
  TriggerQueue.OwnsObjects := true;
  AutoFree(@TriggerQueue);

  KeyGroups.FindKeyGroups(KeyGroupList);

  if KeyGroupList.Count = 0 then exit;

  for c1 := 0 to KeyGroupList.Count-1 do
  begin
    kg := KeyGroupList[c1] as IKeyGroup;

    // TODO: need to add latching code here.
    //KeyGroupLoopMode := (kg.GetObject as TKeyGroup).VoiceParameters.SamplerLoopMode;
    //if KeyGroupLoopMode = TSamplerLoopMode.Latch then


    RegionList.Clear;
    SampleMap.FindRegionsByKeyGroup(kg.GetName, RegionList);

    for c2 := 0 to RegionList.Count-1 do
    begin
      rg := RegionList[c2];
      if IsNoteInsideRegion(rg, Data1, Data2) then
      begin
        // Add this region to the region trigger queue.
        TriggerItem := TRegionTriggerItem.Create;
        TriggerItem.RegionIntf := rg;
        TriggerItem.KeyGroup   := kg;
        TriggerQueue.Add(TriggerItem);
      end;
    end;
  end;

  ProcessTriggerQueue(TriggerQueue, Data1, Data2, TVoiceMode.Poly);
end;

procedure TVoiceController.PolyRelease(const Data1, Data2: byte);
var
  c1 : integer;
  aVoice : TLucidityVoice;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    aVoice := ActiveVoices[c1];

    if (aVoice.TriggerNote = Data1) and (aVoice.LoopMode <> TSamplerLoopMode.OneShot) then
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


  SampleMap := (Globals.SampleMapReference as TSampleMap);
  KeyGroups := (Globals.KeyGroupsReference as TKeyGroupManager);

  KeyGroupList := TInterfaceList.Create;
  AutoFree(@KeyGroupList);

  RegionList := TRegionInterfaceList.Create;
  AutoFree(@RegionList);

  TriggerQueue := TObjectList.Create;
  TriggerQueue.OwnsObjects := true;
  AutoFree(@TriggerQueue);

  KeyGroups.FindKeyGroups(KeyGroupList);

  if KeyGroupList.Count = 0 then exit;

  for c1 := 0 to KeyGroupList.Count-1 do
  begin
    kg := KeyGroupList[c1] as IKeyGroup;

    // TODO: need to add latching code here.
    //KeyGroupLoopMode := (kg.GetObject as TKeyGroup).VoiceParameters.SamplerLoopMode;
    //if KeyGroupLoopMode = TSamplerLoopMode.Latch then


    RegionList.Clear;
    SampleMap.FindRegionsByKeyGroup(kg.GetName, RegionList);

    for c2 := 0 to RegionList.Count-1 do
    begin
      rg := RegionList[c2];
      if IsNoteInsideRegion(rg, Data1, Data2) then
      begin
        // Add this region to the region trigger queue.
        TriggerItem := TRegionTriggerItem.Create;
        TriggerItem.RegionIntf := rg;
        TriggerItem.KeyGroup   := kg;
        TriggerQueue.Add(TriggerItem);
      end;
    end;
  end;


  // NOTE: At the moment both the Legato and Mono voice modes are using the same
  // trigger voice code. Both are triggering voices with TVoiceMode.Mono.
  // This doesn't matter as long as TVoiceMode isn't "Poly".
  ProcessTriggerQueue(TriggerQueue, Data1, Data2, TVoiceMode.Mono);
end;

procedure TVoiceController.MonoRelease(const Data1, Data2: byte);
var
  c1 : integer;
  aVoice : TLucidityVoice;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    aVoice := ActiveVoices[c1];

    if (aVoice.LoopMode <> TSamplerLoopMode.OneShot) then
    begin
      ActiveVoices.Remove(aVoice);
      ReleasedVoices.Add(aVoice);
      aVoice.Release;
    end;
  end;
end;




procedure TVoiceController.ProcessTriggerQueue(const TriggerQueue: TObjectList; const MidiData1, MidiData2 : byte; const TriggerVoiceMode : TVoiceMode);
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
    if not assigned(aVoice) then exit;

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

function TVoiceController.FindVoiceToTrigger: TLucidityVoice;
var
  Index : integer;
begin
  //TODO: Check the inactive voice count, if 0, cull an existing voice.
  Index := InactiveVoices.Count-1;
  result := InactiveVoices[Index] as TLucidityVoice;
  InactiveVoices.Delete(Index);
end;






{ TRegionTriggerItem }

destructor TRegionTriggerItem.Destroy;
begin
  self.RegionIntf := nil;
  inherited;
end;

end.
