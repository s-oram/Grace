unit Lucidity.KeyGroup;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.Utils,
  VamLib.ZeroObject,
  eeParSmoother,
  VamGuiControlInterfaces,
  Lucidity.Types,
  Lucidity.SequencerDataObject,
  LucidityGui.VectorSequence,
  Lucidity.Interfaces, LucidityModConnections,
  VamLib.MoreTypes, eeGlobals,
  uLucidityEnums,
  eeVoiceLogic,
  eePatchObject,
  uConstants,
  Lucidity.SampleMap,
  soLevelMeter,
  soLucidityVoice, soLucidityVoiceParameterWrapper,
  uModularConnectionManager,
  uLucidityStepSequencer,
  uGuiFeedbackData;

type
  // redeclare ISampleGroup for convenience.
  IKeyGroup = Lucidity.Interfaces.IKeyGroup;

type
  TKeyGroup = class(TRefCountedZeroObject, IKeyGroup, ILevelMonitor)
  private
    fTriggeredNoteCount : cardinal;
    fName     : string;
    fSampleMap: TSampleMap;
    fVoiceParameters: TLucidityVoiceParameterWrapper;
    fModConnections: TModConnections;
    fLevelMonitor: TLevelMonitor;

    function GetObject : TObject;
    function GetTriggeredNoteCount:cardinal;
    procedure IncTriggeredNoteCount;
    function GetModConnections:TModConnections;
    function GetModConnectionsPointer : PModConnections;
    function GetModulatedParameters : PModulatedPars;

    function GetSequenceData(SeqIndex : integer):IStepSequenceDataObject;

    procedure SampleRateChanged(Sender:TObject);
    procedure BlockSizeChanged(Sender:TObject);

    procedure GetDbLevel(out Ch1, Ch2 : single);
    property LevelMonitor : TLevelMonitor read fLevelMonitor write fLevelMonitor;

    function GetTriggerMode : TKeyGroupTriggerMode;
  protected
    DebugTag : string;
    ActiveVoices : TLucidityVoiceList;
    KeyGroupID : TKeyGroupID;
    VoiceBufferA : array of single;
    VoiceBufferB : array of single;
    function GetID:TKeyGroupID;
    procedure SetID(ID:TKeyGroupID);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;

    procedure ApplyAllParSmoothingNow;
  protected
    FSeq1Data : TSequencerDataObject;
    FSeq2Data : TSequencerDataObject;
    Globals         : TGlobals;
    GlobalModPoints : PGlobalModulationPoints;

    ModulatedParameters: TModulatedPars;
    ParSmoother : TParSmoother;

    function GetName:string;
    procedure SetName(Value : string);

    //TODO: Delete this.
    property SampleMap : TSampleMap read fSampleMap write fSampleMap;

    procedure Handle_ModConnectionsChanged(Sender : TObject);
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals; const aDebugTag : string = 'Unnamed');
    destructor Destroy; override;
    procedure AssignFrom(const Source : TKeyGroup);

    procedure SetPatch(var Data:TPatchNode);
    procedure GetPatch(var Data:TPatchNode);

    function GetModParValue(const ModParIndex : integer):single;
    procedure SetModParValue(const ModParIndex : integer; const Value:single; const SmoothingRequired : boolean = true);
    procedure SetModParModAmount(const ModParIndex, ModSlot : integer; const Value:single);
    function GetModParModAmount(const ModParIndex, ModSlot : integer):single;
    procedure GetModParModMinMax(const ModParIndex : integer; out ModMin, ModMax:single);

    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);

    // NOTE: Now that key groups maintain a list of active voices, it might be
    // possible to get rid of the Voice Parameters wrapper and apply changes
    // directly to said voices...
    property VoiceParameters : TLucidityVoiceParameterWrapper read fVoiceParameters;
    property ModConnections  : TModConnections                read fModConnections;

    property Name : string read fName;

    property Seq1Data : TSequencerDataObject read FSeq1Data;
    property Seq2Data : TSequencerDataObject read FSeq1Data;



    // TODO: Asking the Key Groups to process the relevent voices added about 5-7% cpu.
    // Perhaps there are ways to reduce that by being smarter about how the voices
    // and key groups are processed.
    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); //inline;
    procedure FastControlProcess; //inline;
    procedure SlowControlProcess; //inline;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  uLucidityExtra,
  VamLib.LoggingProxy,
  eeAudioBufferUtils,
  SysUtils, eeCustomGlobals;


{ TLucidityEngine }

constructor TKeyGroup.Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals; const aDebugTag : string);
var
  c1 : integer;
begin
  KeyGroupID.Init;

  DebugTag := aDebugTag;

  Log.LogMessage('KeyGroup Created ID = (' + DebugTag + ')');

  ParSmoother := TParSmoother.Create;

  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    ModulatedParameters[c1].Reset;
  end;

  // TODO: The key group shouldn't know about "aVoices". But as my code is currently written the voiceParameter wrapper class is owned by
  // the key group and the voice parameter wrapper does need to know about the voices.

  GlobalModPoints := aGlobalModPoints;

  Globals := aGlobals;
  Globals.AddEventListener(TPluginEvent.SampleRateChanged, SampleRateChanged);
  Globals.AddEventListener(TPluginEvent.BlockSizeChanged, BlockSizeChanged);

  fModConnections := TModConnections.Create;
  fModConnections.OnChanged := Handle_ModConnectionsChanged;

  fVoiceParameters := TLucidityVoiceParameterWrapper.Create(aVoices, self);

  FSeq1Data := TSequencerDataObject.Create;
  FSeq2Data := TSequencerDataObject.Create;

  fTriggeredNoteCount := 0;

  fLevelMonitor := TLevelMonitor.Create;

  SampleRateChanged(self);
  BlockSizeChanged(self);

  ActiveVoices := TLucidityVoiceList.Create(false);
end;

destructor TKeyGroup.Destroy;
begin
  Globals.RemoveEventListener(TPluginEvent.SampleRateChanged, SampleRateChanged);
  Globals.RemoveEventListener(TPluginEvent.BlockSizeChanged, BlockSizeChanged);

  Log.LogMessage('KeyGroup Destroyed ID =  (' + DebugTag + ')');
  LogStackTrace;

  FSeq1Data.Free;
  FSeq2Data.Free;
  fVoiceParameters.Free;
  fModConnections.Free;
  fLevelMonitor.Free;

  SetLength(VoiceBufferA, 0);
  SetLength(VoiceBufferB, 0);

  ActiveVoices.Free;
  ParSmoother.Free;

  inherited;
end;

procedure TKeyGroup.AssignFrom(const Source: TKeyGroup);
begin
  self.VoiceParameters.AssignFrom(Source.VoiceParameters);
end;

procedure TKeyGroup.GetPatch(var Data: TPatchNode);
//var
//  ChildModule : TPatchNode;
begin
  //ChildModule := Data.NewChildNode('SampleMap');
  //SampleMap.GetPatchData(ChildModule);
end;

function TKeyGroup.GetTriggeredNoteCount: cardinal;
begin
  result := fTriggeredNoteCount;
end;

function TKeyGroup.GetTriggerMode: TKeyGroupTriggerMode;
begin
  result := VoiceParameters.SamplerLoopMode;
end;

function TKeyGroup.GetSequenceData(SeqIndex: integer): IStepSequenceDataObject;
begin
  case SeqIndex of
    0: result := self.FSeq1Data;
    1: result := self.FSeq2Data;
  else
    raise Exception.Create('SeqIndex not handled.');
  end;
end;


procedure TKeyGroup.IncTriggeredNoteCount;
begin
  inc(fTriggeredNoteCount);
end;

procedure TKeyGroup.SetPatch(var Data: TPatchNode);
//var
//  ChildModule : TPatchNode;
begin
  //ChildModule := Data.FindChildNode('SampleMap');
  //if assigned(ChildModule) then SampleMap.SetPatchData(ChildModule);
end;



procedure TKeyGroup.GetDbLevel(out Ch1, Ch2: single);
begin
  LevelMonitor.GetDbLevel(Ch1, Ch2);
end;

procedure TKeyGroup.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
end;

function TKeyGroup.GetID: TKeyGroupID;
begin
  result := KeyGroupID;
end;

function TKeyGroup.GetModConnections: TModConnections;
begin
  result := fModConnections;
end;

function TKeyGroup.GetModConnectionsPointer: PModConnections;
begin
  result := @fModConnections;
end;

function TKeyGroup.GetModParValue(const ModParIndex: integer): single;
begin
  result := ModulatedParameters[ModParIndex].ParValue;
end;

function TKeyGroup.GetModulatedParameters: PModulatedPars;
begin
  result := @self.ModulatedParameters;
end;

procedure TKeyGroup.SetModParValue(const ModParIndex: integer; const Value: single; const SmoothingRequired : boolean);
var
  CurrentValue : single;
begin
  if (SmoothingRequired = false) or (ActiveVoices.Count = 0) then
  begin
    ModulatedParameters[ModParIndex].ParValue := Value;
    ModulatedParameters[ModParIndex].SmoothedParValue := Value;
    ModulatedParameters[ModParIndex].IsSmoothingActive := false;
  end else
  if (ModulatedParameters[ModParIndex].ParValue <> Value) then
  begin
    if not (ModulatedParameters[ModParIndex].IsSmoothingActive) then
    begin
      CurrentValue := ModulatedParameters[ModParIndex].ParValue;
      ModulatedParameters[ModParIndex].ParSmootherState.Reset(CurrentValue);
      ModulatedParameters[ModParIndex].IsSmoothingActive := true;
    end;
    ModulatedParameters[ModParIndex].ParValue := Value;
  end;
end;

procedure TKeyGroup.ApplyAllParSmoothingNow;
var
  c1 : integer;
  TargetValue : single;
begin
  // NOTE: This method is intended to be called when all voices have finished playing.
  // this will ensure any outstanding parameter smoothing is applied and not left till
  // the next time a voice is triggered. Parameter smoothing is only applied while
  // a key group is actively processing voices.
  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    if ModulatedParameters[c1].IsSmoothingActive then
    begin
      TargetValue := ModulatedParameters[c1].ParValue;
      ModulatedParameters[c1].SmoothedParValue := TargetValue;
      ModulatedParameters[c1].IsSmoothingActive := false;
    end;
  end;
end;



procedure TKeyGroup.SampleRateChanged(Sender: TObject);
begin
  LevelMonitor.SampleRate := Globals.SampleRate;
  ParSmoother.SetTransitionTime(30, Globals.SlowControlRate);
end;

procedure TKeyGroup.BlockSizeChanged(Sender: TObject);
begin
  SetLength(VoiceBufferA, Globals.BlockSize);
  SetLength(VoiceBufferB, Globals.BlockSize);
end;

procedure TKeyGroup.SetID(ID: TKeyGroupID);
begin
  KeyGroupID := ID;
end;

procedure TKeyGroup.SetModParModAmount(const ModParIndex, ModSlot: integer; const Value: single);
var
  aMin, aMax : single;
  c1: Integer;
  tx : single;
begin
  ModulatedParameters[ModParIndex].ModAmount[ModSlot] := Value;
  if assigned(fVoiceParameters)
    then fVoiceParameters.UpdateModConnections;

  //==== Calculate the min/max modulation amounts =======
  aMin := 0;
  aMax := 0;
  for c1 := 0 to kModSlotCount-1 do
  begin
    tx := ModulatedParameters[ModParIndex].ModAmount[c1];
    if aMin > tx then aMin := tx;
    if aMax < tx then aMax := tx;
  end;

  ModulatedParameters[ModParIndex].ModMin := aMin;
  ModulatedParameters[ModParIndex].ModMax := aMax;
  //=====================================================
end;

function TKeyGroup.GetModParModAmount(const ModParIndex, ModSlot: integer): single;
begin
  result := ModulatedParameters[ModParIndex].ModAmount[ModSlot];
end;

procedure TKeyGroup.GetModParModMinMax(const ModParIndex : integer;  out ModMin, ModMax:single);
begin
  ModMin := ModulatedParameters[ModParIndex].ModMin;
  ModMax := ModulatedParameters[ModParIndex].ModMax;
end;

procedure TKeyGroup.SetName(Value: string);
begin
  fName := Value;
end;

function TKeyGroup.GetName: string;
begin
  result := fName;
end;

function TKeyGroup.GetObject: TObject;
begin
  result := self;
end;

procedure TKeyGroup.Handle_ModConnectionsChanged(Sender: TObject);
begin
  if assigned(fVoiceParameters)
    then fVoiceParameters.UpdateModConnections;
end;


procedure TKeyGroup.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  ptr  : pointer;
  kgID : TKeyGroupID;
  pVoice : PLucidityVoice;
begin
  inherited;

  if MsgID = TLucidMsgID.Audio_VoiceTriggered then
  begin
    pVoice := TMsgData_Audio_VoiceTriggered(Data^).Voice;

    ptr  := TMsgData_Audio_VoiceTriggered(Data^).KeyGroupID;
    kgID := TKeyGroupID(ptr^);

    if kgID = KeyGroupID then
    begin
      ActiveVoices.Add(pVoice^)
    end;
  end;


  if MsgID = TLucidMsgID.Audio_VoiceFinished then
  begin
    pVoice := Data;
    if ActiveVoices.IndexOf(pVoice^) <> -1
      then ActiveVoices.Remove(pVoice^);
    if ActiveVoices.Count = 0
      then ApplyAllParSmoothingNow;
  end;
end;


procedure TKeyGroup.FastControlProcess;
var
  c1 : integer;
begin
  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].FastControlProcess;
  end;
end;

procedure TKeyGroup.SlowControlProcess;
var
  c1 : integer;
  TargetValue  : single;
  CurrentValue : single;
  ParSmootherState : PParSmootherState;
begin
  //TODO:HIGH Current all parameter smoothing is checked for via polling.
  // It would be better to add the required smoothing states to a list.
  // That way the parameter smoothing step could pontential be resolved with
  // one boolean check. (Ie. If no parameters require smoothing)
  // NOTE: This implementation as current adds a slight performance disadvantage.
  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    if ModulatedParameters[c1].IsSmoothingActive then
    begin
      TargetValue := ModulatedParameters[c1].ParValue;
      CurrentValue := ModulatedParameters[c1].SmoothedParValue;
      ParSmootherState := @ModulatedParameters[c1].ParSmootherState;

      ParSmoother.Step_DirectForm1(TargetValue, CurrentValue, ParSmootherState);

      ModulatedParameters[c1].SmoothedParValue := Clamp(CurrentValue, 0, 1);

      if CurrentValue = TargetValue
        then ModulatedParameters[c1].IsSmoothingActive := false;
    end;
  end;

  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].SlowControlProcess;
  end;
end;

procedure TKeyGroup.AudioProcess(const Outputs: TArrayOfPSingle; const SampleFrames: integer);
var
  c1 : integer;
  pxA, pxB : PSingle;
  pOutA, pOutB : PSingle;
  dbA, dbB : single;
  pKG : pointer;
begin
  pxA := @VoiceBufferA[0];
  pxB := @VoiceBufferB[0];

  //TODO:LOW: clearing both buffers at once could be an optimisation...
  ClearBuffer(pxA, SampleFrames);
  ClearBuffer(pxB, SampleFrames);

  for c1 := ActiveVoices.Count-1 downto 0 do
  begin
    ActiveVoices[c1].AudioProcess(pxA, pxB, SampleFrames);
  end;

  LevelMonitor.Process(pxA, pxB, SampleFrames);

  pOutA := Outputs[0];
  pOutB := Outputs[1];

  for c1 := 0 to SampleFrames-1 do
  begin
    pOutA^ := pOutA^ + VoiceBufferA[c1];
    pOutB^ := pOutB^ + VoiceBufferB[c1];

    inc(pOutA);
    inc(pOutB);
  end;


  if (ActiveVoices.Count = 0) then
  begin
    LevelMonitor.GetDbLevel(dbA, dbB);
    if (dbA < -35) and (dbB < -35) then
    begin
      pKG := Pointer(IKeyGroup(self));
      Globals.MotherShip.MsgAudio(TLucidMsgID.Audio_KeyGroupInactive, pKG);
    end;
  end;

end;



end.
