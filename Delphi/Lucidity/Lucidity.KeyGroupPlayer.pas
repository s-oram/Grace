unit Lucidity.KeyGroupPlayer;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  eeGlobals,
  eeCustomGlobals,
  soLevelMeter,
  eeAudioBufferUtils,
  VamLib.MoreTypes,
  VamLib.ZeroObject,
  Lucidity.KeyGroup,
  soLucidityVoice;

type
  TKeyGroupPlayer = class(TZeroObject)
  private
    ActiveRegions : TInterfaceList;
  protected
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;
  end;

implementation

uses
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  Lucidity.Interfaces,
  uConstants;

{ TKeyGroupPlayer }

constructor TKeyGroupPlayer.Create;
begin
  ActiveRegions := TInterfaceList.Create;
end;

destructor TKeyGroupPlayer.Destroy;
begin

  ActiveRegions.Free;
  inherited;
end;

procedure TKeyGroupPlayer.Clear;
begin
  ActiveRegions.Clear;
end;

procedure TKeyGroupPlayer.ProcessZeroObjectMessage(MsgID: cardinal;  Data: Pointer);
var
  pKG : pointer;
  kg : IKeyGroup;
begin
  inherited;

  if MsgID = TLucidMsgID.Audio_VoiceTriggered then
  begin
    LogMain.EnterMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Triggered');
    LogSpecial.Active := true;

    pKG := TMsgData_Audio_VoiceTriggered(Data^).KeyGroup;
    kg := IKeyGroup(pKG);
    if ActiveRegions.IndexOf(kg) = -1 then
    begin
      ActiveRegions.Add(kg);
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
    if ActiveRegions.IndexOf(kg) <> -1 then
    begin
      ActiveRegions.Remove(kg);
    end;
    kg := nil;

    LogMain.LogMessage('Active Region Count = ' + IntToStr(ActiveRegions.Count));

    LogSpecial.Active := false;
    LogMain.LeaveMethod('TKeyGroupPlayer.ProcessZeroObjectMessage.Inactive');
  end;
end;


procedure TKeyGroupPlayer.FastControlProcess;
var
  c1: Integer;
  kg : IKeyGroup;
begin
  for c1 := 0 to ActiveRegions.Count-1 do
  begin
    kg := (ActiveRegions[c1] as IKeyGroup);
    (kg.GetObject as TKeyGroup).FastControlProcess;
  end;
end;


procedure TKeyGroupPlayer.SlowControlProcess;
var
  c1: Integer;
  kg : IKeyGroup;
begin
  for c1 := 0 to ActiveRegions.Count-1 do
  begin
    kg := (ActiveRegions[c1] as IKeyGroup);
    (kg.GetObject as TKeyGroup).SlowControlProcess;
  end;
end;

procedure TKeyGroupPlayer.AudioProcess(const Outputs: TArrayOfPSingle; const SampleFrames: integer);
var
  c1: Integer;
  kg : IKeyGroup;
begin
  for c1 := 0 to ActiveRegions.Count-1 do
  begin
    kg := (ActiveRegions[c1] as IKeyGroup);
    (kg.GetObject as TKeyGroup).AudioProcess(Outputs, SampleFrames);
  end;
end;



end.
