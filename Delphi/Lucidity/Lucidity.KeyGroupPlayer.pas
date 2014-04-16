unit Lucidity.KeyGroupPlayer;

interface

uses
  Classes,
  VamLib.MoreTypes,
  VamLib.ZeroObject;

type
  TKeyGroupPlayer = class(TZeroObject)
  private
    ActiveRegions : TInterfaceList;

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;
  end;

implementation

uses
  Lucidity.KeyGroup,
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

procedure TKeyGroupPlayer.ProcessZeroObjectMessage(MsgID: cardinal;  Data: Pointer);
var
  pKG : pointer;
begin
  inherited;

  if MsgID = TLucidMsgID.Audio_VoiceTriggered then
  begin
    pKG := TMsgData_Audio_VoiceTriggered(Data^).KeyGroup;
    if ActiveRegions.IndexOf(IKeyGroup(pKG)) = -1 then
      ActiveRegions.Add(IKeyGroup(pKG));
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
