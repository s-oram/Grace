unit Lucidity.KeyGroupPlayer;

interface

{$INCLUDE Defines.inc}

uses
  Classes,
  //VamLib.CpuOverloadWatcher,
  VamLib.Types,
  eeGlobals,
  eeCustomGlobals,
  soLevelMeter,
  eeAudioBufferUtils,
  VamLib.MoreTypes,
  VamLib.ZeroObject,
  Lucidity.Types,
  Lucidity.KeyGroup,
  soLucidityVoice,
  Lucidity.KeyGroupManager;

type
  TKeyGroupPlayer = class(TZeroObject)
  private
    ActiveRegions : TInterfaceList;
  protected
    Globals : TGlobals;
    KeyGroupManager : TKeyGroupManager;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData);  override;
  public
    constructor Create(const aGlobals : TGlobals; const aKeyGroupManager : TKeyGroupManager);
    destructor Destroy; override;

    procedure Clear;

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); //inline;
    procedure FastControlProcess; //inline;
    procedure SlowControlProcess; //inline;
  end;

implementation

uses
  VamLib.Utils,
  SysUtils,
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  Lucidity.Interfaces,
  uConstants;

{ TKeyGroupPlayer }

constructor TKeyGroupPlayer.Create(const aGlobals : TGlobals; const aKeyGroupManager : TKeyGroupManager);
begin
  Globals := aGlobals;
  ActiveRegions := TInterfaceList.Create;
  KeyGroupManager := aKeyGroupManager;
end;

destructor TKeyGroupPlayer.Destroy;
begin
  KeyGroupManager := nil;
  ActiveRegions.Free;
  inherited;
end;

procedure TKeyGroupPlayer.Clear;
begin
  ActiveRegions.Clear;
end;

procedure TKeyGroupPlayer.ProcessZeroObjectMessage(MsgID: cardinal;  Data: Pointer; DataB:IZeroMessageData);
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
    ptr  := TMsgData_Audio_VoiceTriggered(Data^).KeyGroupID;
    kgID := TKeyGroupID(ptr^);

    kg := KeyGroupManager.Request(kgID);

    if (assigned(kg)) and (ActiveRegions.IndexOf(kg) = -1) then
    begin
      ActiveRegions.Add(kg);
    end;

    kg := nil;
  end;


  if MsgID = TLucidMsgID.Audio_KeyGroupInactive then
  begin
    pKG := Data;
    kg := IKeyGroup(pKG);

    if ActiveRegions.IndexOf(kg) <> -1 then
    begin
      ActiveRegions.Remove(kg);
    end;

    kg := nil;
  end;


  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    kgID := TKeyGroupID(Data^);

    for c1 := ActiveRegions.Count-1 downto 0 do
    begin
      if (ActiveRegions[c1] as IKeyGroup).GetID = kgID then
      begin
        ActiveRegions.Delete(c1);
      end;
    end;
  end;
end;


procedure TKeyGroupPlayer.FastControlProcess;
var
  c1: Integer;
  kg : IKeyGroup;
begin
  for c1 := ActiveRegions.Count-1 downto 0 do
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
  for c1 := ActiveRegions.Count-1 downto 0 do
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
  for c1 := ActiveRegions.Count-1 downto 0 do
  begin
    kg := (ActiveRegions[c1] as IKeyGroup);
    (kg.GetObject as TKeyGroup).AudioProcess(Outputs, SampleFrames);
  end;
end;



end.
