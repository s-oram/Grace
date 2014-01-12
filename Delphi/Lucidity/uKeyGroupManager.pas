unit uKeyGroupManager;

interface

uses
  VamLib.MoreTypes, soLucidityVoice, uLucidityVoiceController, uLucidityKeyGroupInterface,
  Classes, Contnrs, uConstants, uLucidityKeyGroup,
  eeGlobals, SyncObjs;

type
  //=== Forward Declarations ==========
  TKeyGroupsInfo = class;
  IKeyGroupsInfo = interface;
  //===================================


  TKeyGroupManager = class
  private
    function GetSampleGroupCount: integer;
  protected
    // Instead of using the one list lock it might be possible
    // to use two locks, one would be a "write" lock to prevent changes,
    // the other might be a lock to signal read states. The goal of two
    // locks would be to allow multiple sections of code to read items from
    // the list, but only allow one section of code at a time to write
    // changes to the list.
    ListLock : TMutex;

    //TODO: Compare the difference between using TCriticalSection and TMutux.
    // TFixedCriticalSection.
    // http://delphitools.info/2013/06/06/tmonitor-vs-trtlcriticalsection/?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed%3A+DelphiTools+%28DelphiTools.info%29
    // TCriticalSection vs TMutex
    // https://forums.embarcadero.com/thread.jspa?threadID=61392
    fList : TInterfaceList;

    GlobalModPoints : PGlobalModulationPoints;
    Globals : TGlobals;

    Voices : PArrayOfLucidityVoice;
    VoiceController:PLucidityVoiceController;

    SGCreateCount : cardinal;

    InitReference : IKeyGroup;

    function SampleGroup(const Name:string):IKeyGroup; overload;
    function SampleGroup(const Index:integer):IKeyGroup; overload;
    property SampleGroupCount : integer read GetSampleGroupCount;
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aVoiceController:PLucidityVoiceController; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    procedure UpdateInitReference;

    function GetInfo:IKeyGroupsInfo;
    function Count : integer;

    procedure Clear;

    function NewKeyGroup:IKeyGroup; overload;
    function NewKeyGroup(aName : string):IKeyGroup; overload;
    function FindFirstKeyGroup:IKeyGroup;
    function FindSampleGroup(aName : string):IKeyGroup; overload;
    procedure DeleteKeyGroup(aName : string);
  end;



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
  SysUtils;

{ TSampleGroupManager }

function TKeyGroupManager.Count: integer;
begin
  result := fList.Count;
end;

constructor TKeyGroupManager.Create(const aVoices:PArrayOfLucidityVoice; const aVoiceController:PLucidityVoiceController; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  fList := TInterfaceList.Create;

  Globals         := aGlobals;
  GlobalModPoints := aGlobalModPoints;

  Voices := aVoices;
  VoiceController := aVoiceController;

  ListLock := TMutex.Create;

  SGCreateCount := 0;


  InitReference := TKeyGroup.Create(Voices, GlobalModPoints, Globals);

end;

destructor TKeyGroupManager.Destroy;
begin
  InitReference := nil;
  fList.Free;
  ListLock.Free;
  inherited;
end;

function TKeyGroupManager.GetInfo: IKeyGroupsInfo;
var
  aInfo : TKeyGroupsInfo;
begin
  aInfo := TKeyGroupsInfo.Create;

  ListLock.Acquire;
  try
    aInfo.AssignFrom(self);
  finally
    ListLock.Release;
  end;

  result := aInfo;
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
  ListLock.Acquire;
  try
    result := nil;

    for c1 := fList.Count-1 downto 0 do
    begin
      sg := (fList[c1] as IKeyGroup);
      if sg.GetName = Name then result := sg;
    end;

  finally
    ListLock.Release;
  end;
end;

procedure TKeyGroupManager.Clear;
begin
  //Blocking the audio thread here seems bad....
  ListLock.Acquire;
  try
    fList.Clear;
    SGCreateCount := 0;
  finally
    ListLock.Release;
  end;
end;

function TKeyGroupManager.NewKeyGroup(aName: string): IKeyGroup;
var
  sg : IKeyGroup;
begin
  if aName = '' then raise Exception.Create('Can not create SampleGroup with an empty name.');


  // check if a group of the same name already exists, if so, return it and
  // don't make a new group...
  sg := FindSampleGroup(aName);
  if sg <> nil then exit(sg);
  //====================================


  ListLock.Acquire;
  try
    inc(SGCreateCount);

    sg := TKeyGroup.Create(Voices, GlobalModPoints, Globals);

    (sg.GetObject as TKeyGroup).AssignFrom((InitReference.GetObject as TKeyGroup));

    sg.SetName(aName);
    fList.Add(sg);

    result := sg;
  finally
    ListLock.Release;
  end;
end;

function TKeyGroupManager.NewKeyGroup: IKeyGroup;
var
  sg : IKeyGroup;
begin
  ListLock.Acquire;
  try
    inc(SGCreateCount);

    sg := TKeyGroup.Create(Voices, GlobalModPoints, Globals);

    (sg.GetObject as TKeyGroup).AssignFrom((InitReference.GetObject as TKeyGroup));

    sg.SetName('Group ' + IntToStr(SGCreateCount));
    fList.Add(sg);

    result := sg;
  finally
    ListLock.Release;
  end;
end;






procedure TKeyGroupManager.DeleteKeyGroup(aName: string);
var
  c1: Integer;
  kg : IKeyGroup;
begin
  ListLock.Acquire;
  try
    for c1 := fList.Count-1 downto 0 do
    begin
      kg := (fList[c1] as IKeyGroup);
      if kg.GetName = aName then fList.Remove(kg);
      kg := nil;
    end;
  finally
    ListLock.Release;
  end;
end;

function TKeyGroupManager.FindFirstKeyGroup: IKeyGroup;
begin
  ListLock.Acquire;
  try
    if fList.Count > 0
      then result := fList[0] as IKeyGroup
      else result := nil;

  finally
    ListLock.Release;
  end;
end;

function TKeyGroupManager.FindSampleGroup(aName: string): IKeyGroup;
var
  c1: Integer;
  sg : IKeyGroup;
begin
  ListLock.Acquire;
  try
    for c1 := 0 to fList.Count-1 do
    begin
      sg := (fList[c1] as IKeyGroup);
      if sg.GetName = aName then exit(sg); //=====>>exit>>==========>>
    end;

    //If we've made it this far, no sample group is found.
    result := nil;
  finally
    ListLock.Release;
  end;
end;



procedure TKeyGroupManager.UpdateInitReference;
var
  sg : IKeyGroup;
begin
  sg := SampleGroup(0);
  (InitReference.GetObject as TKeyGroup).AssignFrom((sg.GetObject as TKeyGroup));
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

end.
