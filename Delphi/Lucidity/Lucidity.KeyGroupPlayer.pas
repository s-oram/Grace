unit Lucidity.KeyGroupPlayer;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  VamLib.Types,
  eeGlobals,
  eeCustomGlobals,
  soLevelMeter,
  eeAudioBufferUtils,
  VamLib.MoreTypes,
  VamLib.ZeroObject,
  Lucidity.Types,
  Lucidity.KeyGroup,
  soLucidityVoice;

type
  TKeyGroupPlayer = class(TZeroObject)
  private
    ActiveRegions : TInterfaceList;
    ActiveRegionsLock : TFakeCriticalSection;
  protected
    Globals : TGlobals;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(const aGlobals : TGlobals);
    destructor Destroy; override;

    procedure Clear;

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); //inline;
    procedure FastControlProcess; //inline;
    procedure SlowControlProcess; //inline;
  end;

implementation

uses
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}

  Lucidity.Interfaces,
  uConstants;

{ TKeyGroupPlayer }

constructor TKeyGroupPlayer.Create(const aGlobals : TGlobals);
begin
  Globals := aGlobals;
  ActiveRegions := TInterfaceList.Create;
  ActiveRegionsLock := TFakeCriticalSection.Create;
end;

destructor TKeyGroupPlayer.Destroy;
begin
  ActiveRegionsLock.Free;
  ActiveRegions.Free;
  inherited;
end;

procedure TKeyGroupPlayer.Clear;
begin
  ActiveRegionsLock.Acquire;
  try
    ActiveRegions.Clear;
  finally
    ActiveRegionsLock.Release;
  end;
end;

procedure TKeyGroupPlayer.ProcessZeroObjectMessage(MsgID: cardinal;  Data: Pointer);
var
  c1 : integer;
  pKG : pointer;
  kg : IKeyGroup;
  ptr  : pointer;
  kgID : TKeyGroupID;
begin
  inherited;

  if MsgID = TLucidMsgID.Audio_VoiceTriggered then
  begin
    LogMain.EnterMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Triggered');
    LogSpecial.Active := true;

    ptr  := TMsgData_Audio_VoiceTriggered(Data^).KeyGroupID;
    kgID := TKeyGroupID(ptr^);

    kg := Globals.KeyGroupLifeTimeManager.Request(kgID);

    ActiveRegionsLock.Acquire;
    try
      if (assigned(kg)) and (ActiveRegions.IndexOf(kg) = -1) then
      begin
        ActiveRegions.Add(kg);
      end;
    finally
      ActiveRegionsLock.Release;
    end;

    kg := nil;

    LogMain.LogMessage('Active Region Count = ' + IntToStr(ActiveRegions.Count));

    LogSpecial.Active := false;
    LogMain.LeaveMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Triggered');
  end;

  if MsgID = TLucidMsgID.Audio_KeyGroupInactive then
  begin
    LogMain.EnterMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Inactive');
    LogSpecial.Active := true;

    pKG := Data;
    kg := IKeyGroup(pKG);

    ActiveRegionsLock.Acquire;
    try
      if ActiveRegions.IndexOf(kg) <> -1 then
      begin
        ActiveRegions.Remove(kg);
      end;
    finally
      ActiveRegionsLock.Release;
    end;
    kg := nil;

    LogMain.LogMessage('Active Region Count = ' + IntToStr(ActiveRegions.Count));

    LogSpecial.Active := false;
    LogMain.LeaveMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Inactive');
  end;


  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    kgID := TKeyGroupID(Data^);

    ActiveRegionsLock.Acquire;
    try
      for c1 := ActiveRegions.Count-1 downto 0 do
      begin
        if (ActiveRegions[c1] as IKeyGroup).GetID = kgID then
        begin
          ActiveRegions.Delete(c1);
        end;
      end;
    finally
      ActiveRegionsLock.Release;
    end;
  end;
end;


procedure TKeyGroupPlayer.FastControlProcess;
var
  c1: Integer;
  kg : IKeyGroup;
begin
  ActiveRegionsLock.Acquire;
  try
    for c1 := ActiveRegions.Count-1 downto 0 do
    begin
      kg := (ActiveRegions[c1] as IKeyGroup);
      (kg.GetObject as TKeyGroup).FastControlProcess;
    end;
  finally
    ActiveRegionsLock.Release;
  end;
end;


procedure TKeyGroupPlayer.SlowControlProcess;
var
  c1: Integer;
  kg : IKeyGroup;
begin
  ActiveRegionsLock.Acquire;
  try
    for c1 := ActiveRegions.Count-1 downto 0 do
    begin
      kg := (ActiveRegions[c1] as IKeyGroup);
      (kg.GetObject as TKeyGroup).SlowControlProcess;
    end;
  finally
    ActiveRegionsLock.Release;
  end;
end;

procedure TKeyGroupPlayer.AudioProcess(const Outputs: TArrayOfPSingle; const SampleFrames: integer);
var
  c1: Integer;
  kg : IKeyGroup;
begin
  ActiveRegionsLock.Acquire;
  try
    for c1 := ActiveRegions.Count-1 downto 0 do
    begin
      kg := (ActiveRegions[c1] as IKeyGroup);
      (kg.GetObject as TKeyGroup).AudioProcess(Outputs, SampleFrames);
    end;
  finally
    ActiveRegionsLock.Release;
  end;
end;



end.
