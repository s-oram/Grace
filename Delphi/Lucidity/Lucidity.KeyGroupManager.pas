unit Lucidity.KeyGroupManager;

interface

{$INCLUDE Defines.inc}

uses
  eeAudioBufferUtils,
  soLevelMeter,
  VamLib.Types,
  VamLib.ZeroObject,
  VamLib.MoreTypes, soLucidityVoice,
  Lucidity.Interfaces,
  Lucidity.Types,
  Classes, Contnrs, uConstants, Lucidity.KeyGroup,
  eeGlobals, SyncObjs;

type
  //=== Forward Declarations ==========
  TKeyGroupsInfo = class;
  IKeyGroupsInfo = interface;
  //===================================


  // HACK: WARNING: TODO: I think there is a problem with how key groups are deleted.
  // If a key group is deleted and active voices for that group aren't currently killed.
  // Nor will the active voices continue to sound. It would be better for
  // key groups to be added to an 'active' key group list. That way key groups
  // could be deleted and active voices would continue to play out.


  TKeyGroupManager = class(TZeroObject)
  private
    function GetSampleGroupCount: integer;
    function GetKeyGroup(Index: integer): IKeyGroup;
  protected
    // Instead of using the one list lock it might be possible
    // to use two locks, one would be a "write" lock to prevent changes,
    // the other might be a lock to signal read states. The goal of two
    // locks would be to allow multiple sections of code to read items from
    // the list, but only allow one section of code at a time to write
    // changes to the list.
    // TODO:It might be better to get rid of this critical section and go for
    // a lock free approach. or one of the multi-reader locks.
    ListLock : TFixedCriticalSection;

    fList : TInterfaceList;

    GlobalModPoints : PGlobalModulationPoints;
    Globals : TGlobals;

    Voices : PArrayOfLucidityVoice;

    KeyGroupIDCount : cardinal;

    function SampleGroup(const Name:string):IKeyGroup; overload;
    function SampleGroup(const Index:integer):IKeyGroup; overload;
    property SampleGroupCount : integer read GetSampleGroupCount;

    function IsKeyGroupNameUnique(const aName : string) : boolean;
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    function GetInfo:IKeyGroupsInfo;
    function Count : integer;

    // NOTE: Clear isn't thread-safe.
    procedure Clear;

    function Request(const KeyGroupID: TKeyGroupID): IKeyGroup;

    function NewKeyGroup:IKeyGroup; overload;
    function NewKeyGroup(aName : string):IKeyGroup; overload;
    function FindFirstKeyGroup:IKeyGroup;
    function FindSampleGroup(aName : string):IKeyGroup; overload;
    procedure DeleteKeyGroup(aName : string);
    procedure RenameKeyGroup(const kg : IKeyGroup; const NewName : string);

    procedure FindKeyGroups(var DestList : TInterfaceList);

    property KeyGroup[Index : integer]:IKeyGroup read GetKeyGroup; default;

    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;
    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
  end;


  // TODO : Key Groups info maintains a list of references, lets change
  // that to a list of key group ids.
  IKeyGroupsInfo = interface
    ['{E1C9A4CE-11D3-4A19-8EC4-9FB50C1C93DD}']
    function GetKeyGroup(Index: integer): IKeyGroup;
    function GetKeyGroupCount: integer;
  end;

  TKeyGroupsInfo = class(TInterfacedObject, IKeyGroupsInfo)
  strict private
    GroupsList : TInterfaceList;
    fFocusedGroup: IKeyGroup;
    function GetKeyGroup(Index: integer): IKeyGroup;
    function GetKeyGroupCount: integer;
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Source : TKeyGroupManager);

    property KeyGroupCount : integer read GetKeyGroupCount;
    property KeyGroups[Index : integer] : IKeyGroup read GetKeyGroup;

    property FocusedGroup : IKeyGroup read fFocusedGroup;
  end;


implementation

uses
  SysUtils,
  VamLib.Utils,
  Lucidity.PluginParameterController;

{ TSampleGroupManager }

function TKeyGroupManager.Count: integer;
begin
  result := fList.Count;
end;

constructor TKeyGroupManager.Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  fList := TInterfaceList.Create;

  Globals         := aGlobals;

  GlobalModPoints := aGlobalModPoints;

  Voices := aVoices;

  ListLock := TFixedCriticalSection.Create;

  KeyGroupIDCount := 1;
end;

destructor TKeyGroupManager.Destroy;
begin
  Clear;

  fList.Free;
  ListLock.Free;
  inherited;
end;

function TKeyGroupManager.GetInfo: IKeyGroupsInfo;
var
  aInfo : TKeyGroupsInfo;
begin
  aInfo := TKeyGroupsInfo.Create;

  ListLock.Enter;
  try
    aInfo.AssignFrom(self);
  finally
    ListLock.Leave;
  end;

  result := aInfo;
end;

function TKeyGroupManager.GetKeyGroup(Index: integer): IKeyGroup;
begin
  result := fList[Index] as IKeyGroup;
end;

function TKeyGroupManager.GetSampleGroupCount: integer;
begin
  result := fList.Count;
end;

function TKeyGroupManager.SampleGroup(const Index: integer): IKeyGroup;
begin
  result := fList[Index] as IKeyGroup;
end;


function TKeyGroupManager.SampleGroup(const Name: string): IKeyGroup;
var
  c1: Integer;
  sg : IKeyGroup;
begin
  ListLock.Enter;
  try
    result := nil;

    for c1 := fList.Count-1 downto 0 do
    begin
      sg := (fList[c1] as IKeyGroup);
      if sg.GetName = Name then result := sg;
    end;

  finally
    ListLock.Leave;
  end;
end;

procedure TKeyGroupManager.Clear;
var
  c1: Integer;
  kgName : string;
begin
  for c1 := fList.Count-1 downto 0 do
  begin
    kgName := (fList[c1] as IKeyGroup).GetName;
    DeleteKeyGroup(kgName);
  end;
end;

function TKeyGroupManager.IsKeyGroupNameUnique(const aName: string): boolean;
var
  c1 : integer;
begin
  for c1 := 0 to SampleGroupCount-1 do
  begin
    if SameText(aName, SampleGroup(c1).GetName)
      then exit(false);
  end;

  //== no match has been found ==
  result := true;
end;

function TKeyGroupManager.NewKeyGroup: IKeyGroup;
begin
  result := NewKeyGroup('');
end;

function TKeyGroupManager.NewKeyGroup(aName: string): IKeyGroup;
var
  kg : IKeyGroup;
  zo : IZeroObject;
  UniqueName : string;
begin
  ListLock.Enter;
  try
    if aName <> ''
      then UniqueName := aName
      else UniqueName := 'Group 1';

    while IsKeyGroupNameUnique(UniqueName) = false
      do UniqueName := IncrementName(UniqueName, 1);

    kg := TKeyGroup.Create(Voices, GlobalModPoints, Globals, 'New KG - ' + aName + ' ' + RandomString(4));
    TPluginParameterController.ResetKeyGroupParameters(kg);

    kg.SetName(UniqueName);
    fList.Add(kg);

    if supports(kg, IZeroObject, zo) then
    begin
      Globals.MotherShip.RegisterZeroObject(zo, TZeroObjectRank.NonVisual);
    end;

    result := kg;
  finally
    ListLock.Leave;
  end;
end;

procedure TKeyGroupManager.RenameKeyGroup(const kg: IKeyGroup; const NewName: string);
var
  TestName : string;
  TestIndex : integer;
begin
  TestIndex := 1;
  TestName := NewName;
  while IsKeyGroupNameUnique(TestName) = false do
  begin
    TestName := NewName + ' (' + IntToStr(TestIndex) + ')';
    inc(TestIndex);
  end;

  kg.SetName(TestName);
end;

function TKeyGroupManager.Request(const KeyGroupID: TKeyGroupID): IKeyGroup;
var
  c1: Integer;
begin
  ListLock.Enter;
  try
    for c1 := 0 to fList.Count-1 do
    begin
      if (fList[c1] as IKeyGroup).GetID = KeyGroupID
        then exit(fList[c1] as IKeyGroup);
    end;
  finally
    ListLock.Leave;
  end;
  //== no match found ==
  result := nil;
end;

procedure TKeyGroupManager.DeleteKeyGroup(aName: string);
var
  c1: Integer;
  kg : IKeyGroup;
  kgID : TKeyGroupID;
begin
  ListLock.Enter;
  try
    for c1 := fList.Count-1 downto 0 do
    begin
      kg := (fList[c1] as IKeyGroup);

      if kg.GetName = aName then
      begin
        fList.Remove(kg);
        kgID := kg.GetID;

        // Send out the dispose method to give other modules a chance to release
        // there references.
        Globals.MotherShip.MsgNonVisual(TLucidMsgID.Command_DisposeKeyGroup, @kgID);
        Globals.MsgVclTS(TLucidMsgID.Command_DisposeKeyGroup, nil);

        kg := nil;
      end;
    end;
  finally
    ListLock.Leave;
  end;
end;

function TKeyGroupManager.FindFirstKeyGroup: IKeyGroup;
begin
  ListLock.Enter;
  try
    if fList.Count > 0
      then result := fList[0] as IKeyGroup
      else result := nil;

  finally
    ListLock.Leave;
  end;
end;

function TKeyGroupManager.FindSampleGroup(aName: string): IKeyGroup;
var
  c1: Integer;
  sg : IKeyGroup;
begin
  ListLock.Enter;
  try
    for c1 := 0 to fList.Count-1 do
    begin
      sg := (fList[c1] as IKeyGroup);
      if sg.GetName = aName then exit(sg); //=====>>exit>>==========>>
    end;

    //If we've made it this far, no sample group is found.
    result := nil;
  finally
    ListLock.Leave;
  end;
end;

procedure TKeyGroupManager.FastControlProcess;
var
  c1: Integer;
  sg : IKeyGroup;
begin
  for c1 := 0 to fList.Count-1 do
  begin
    sg := (fList[c1] as IKeyGroup);
    (sg.GetObject as TKeyGroup).FastControlProcess;
  end;
end;

procedure TKeyGroupManager.SlowControlProcess;
var
  c1: Integer;
  sg : IKeyGroup;
begin
  for c1 := 0 to fList.Count-1 do
  begin
    sg := (fList[c1] as IKeyGroup);
    (sg.GetObject as TKeyGroup).SlowControlProcess;
  end;
end;

procedure TKeyGroupManager.AudioProcess(const Outputs: TArrayOfPSingle; const SampleFrames: integer);
var
  c1: Integer;
  sg : IKeyGroup;
begin
  for c1 := 0 to fList.Count-1 do
  begin
    sg := (fList[c1] as IKeyGroup);
    (sg.GetObject as TKeyGroup).AudioProcess(Outputs, SampleFrames);
  end;
end;



{ TSampleGroupsInfo }

constructor TKeyGroupsInfo.Create;
begin
  GroupsList := TInterfaceList.Create;
end;

destructor TKeyGroupsInfo.Destroy;
begin
  GroupsList.Free;
  inherited;
end;

procedure TKeyGroupsInfo.AssignFrom(Source: TKeyGroupManager);
var
  c1: Integer;
begin
  GroupsList.Clear;
  for c1 := 0 to Source.SampleGroupCount-1 do
  begin
    GroupsList.Add(Source.SampleGroup(c1));
  end;
end;

function TKeyGroupsInfo.GetKeyGroup(Index: integer): IKeyGroup;
begin
  result := GroupsList[Index] as IKeyGroup;
end;

function TKeyGroupsInfo.GetKeyGroupCount: integer;
begin
  result := GroupsList.Count;
end;

procedure TKeyGroupManager.FindKeyGroups(var DestList: TInterfaceList);
var
  c1: Integer;
begin
  assert(assigned(DestList));

  ListLock.Enter;
  try
    for c1 := fList.Count-1 downto 0 do
    begin
      DestList.Add(fList[c1]);
    end;
  finally
    ListLock.Leave;
  end;
end;





end.
