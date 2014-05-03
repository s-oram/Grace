unit Lucidity.KeyGroupLifeTimeManager;

interface

uses
  Classes,
  VamLib.Types,
  Lucidity.Types,
  Lucidity.Interfaces,
  VamLib.ZeroObject;

type
  TKeyGroupLifeTimeManager = class(TZeroObject)
  private
    KeyGroupList : TInterfaceList;
    ListLock : TFixedCriticalSection;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(var KeyGroup : IKeyGroup);
    function Request(const KeyGroupID : TKeyGroupID):IKeyGroup;

    procedure Dispose(const KeyGroupID : TKeyGroupID);
  end;

implementation

uses
  uConstants;

{ TKeyGroupLifeTimeManager }

constructor TKeyGroupLifeTimeManager.Create;
begin
  KeyGroupList := TInterfaceList.Create;
  ListLock := TFixedCriticalSection.Create;
end;

destructor TKeyGroupLifeTimeManager.Destroy;
begin
  KeyGroupList.Free;
  ListLock.Free;
  inherited;
end;

procedure TKeyGroupLifeTimeManager.Add(var KeyGroup: IKeyGroup);
begin
  ListLock.Acquire;
  try
    KeyGroupList.Add(KeyGroup);
  finally
    ListLock.Release;
  end;
end;

function TKeyGroupLifeTimeManager.Request(const KeyGroupID: TKeyGroupID): IKeyGroup;
var
  c1: Integer;
begin
  ListLock.Acquire;
  try
    for c1 := 0 to KeyGroupList.Count-1 do
    begin
      if (KeyGroupList[c1] as IKeyGroup).GetID = KeyGroupID
        then exit(KeyGroupList[c1] as IKeyGroup);
    end;
  finally
    ListLock.Release;
  end;
  //== no match found ==
  result := nil;
end;

procedure TKeyGroupLifeTimeManager.Dispose(const KeyGroupID: TKeyGroupID);
var
  c1: Integer;
begin
  ListLock.Acquire;
  try
    for c1 := 0 to KeyGroupList.Count-1 do
    begin
      if (KeyGroupList[c1] as IKeyGroup).GetID = KeyGroupID then
      begin
        KeyGroupList[c1] := nil;
      end;
    end;
  finally
    ListLock.Release;
  end;
end;



procedure TKeyGroupLifeTimeManager.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
var
  kgID : TKeyGroupID;
begin
  inherited;

  // ignore this dispose method.
  {
  if MsgID = TLucidMsgID.Command_DisposeKeyGroup then
  begin
    kgID := TKeyGroupID(Data^);
    Dispose(kgID);
  end;
  }
end;

end.
