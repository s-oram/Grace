unit uKeyGroupManager;

interface

{$INCLUDE Defines.inc}

uses
  eeAudioBufferUtils,
  soLevelMeter,
  VamLib.ZeroObject,
  VamLib.MoreTypes, soLucidityVoice, Lucidity.Interfaces,
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

    SGCreateCount : cardinal;

    KeyGroupIDCount : cardinal;

    function SampleGroup(const Name:string):IKeyGroup; overload;
    function SampleGroup(const Index:integer):IKeyGroup; overload;
    property SampleGroupCount : integer read GetSampleGroupCount;

    function IsKeyGroupNameUnique(const aName : string) : boolean;
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aVoiceController:IVoiceController; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    function GetInfo:IKeyGroupsInfo;
    function Count : integer;

    procedure Clear;

    function NewKeyGroup:IKeyGroup; overload;
    function NewKeyGroup(aName : string):IKeyGroup; overload;
    function FindFirstKeyGroup:IKeyGroup;
    function FindSampleGroup(aName : string):IKeyGroup; overload;
    procedure DeleteKeyGroup(aName : string);


    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;
    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
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
  SysUtils,
  VamLib.Utils,
  Lucidity.Types;

{ TSampleGroupManager }

function TKeyGroupManager.Count: integer;
begin
  result := fList.Count;
end;

constructor TKeyGroupManager.Create(const aVoices:PArrayOfLucidityVoice; const aVoiceController:IVoiceController; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
begin
  fList := TInterfaceList.Create;

  Globals         := aGlobals;
  GlobalModPoints := aGlobalModPoints;

  Voices := aVoices;

  ListLock := TMutex.Create;

  SGCreateCount := 0;

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
var
  c1: Integer;
begin
  //Blocking the audio thread here seems bad....
  ListLock.Acquire;
  try
    for c1 := 0 to fList.Count-1 do
    begin
      fList[c1] := nil;
    end;

    fList.Clear;
    SGCreateCount := 0;
  finally
    ListLock.Release;
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



function TKeyGroupManager.NewKeyGroup(aName: string): IKeyGroup;
var
  kg : IKeyGroup;
  zo : IZeroObject;
  UniqueName : string;
begin
  ListLock.Acquire;
  try
    inc(SGCreateCount);

    if aName <> ''
      then UniqueName := aName
      else UniqueName := 'Group ' + IntToStr(SGCreateCount);

    while IsKeyGroupNameUnique(UniqueName) = false
      do UniqueName := IncrementName(UniqueName, 1);

    kg := TKeyGroup.Create(Voices, GlobalModPoints, Globals, 'New KG - ' + aName + ' ' + RandomString(4));



    kg.SetName(UniqueName);
    fList.Add(kg);

    if supports(kg, IZeroObject, zo) then
    begin
      Globals.MotherShip.RegisterZeroObject(zo, TZeroObjectRank.Audio);
    end;

    Globals.KeyGroupLifeTimeManager.Add(kg);

    result := kg;
  finally
    ListLock.Release;
  end;
end;

function TKeyGroupManager.NewKeyGroup: IKeyGroup;
begin
 result := NewKeyGroup('');
end;






procedure TKeyGroupManager.DeleteKeyGroup(aName: string);
var
  c1: Integer;
  kg : IKeyGroup;
  kgID : TKeyGroupID;
begin
  ListLock.Acquire;
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
        Globals.MotherShip.MsgAudio(TLucidMsgID.Command_DisposeKeyGroup, @kgID);
        Globals.MotherShip.MsgMain(TLucidMsgID.Command_DisposeKeyGroup, @kgID);
        Globals.MotherShip.MsgVclTS(TLucidMsgID.Command_DisposeKeyGroup);

        // Dispose the reference in the global key group manager.
        Globals.KeyGroupLifeTimeManager.Dispose(kgID);

        // NOTE: I don't really think the above two step process to free the
        // key groups should be required. After all the key group is implemented
        // as a reference counted interface. But I'm having grief with AV errors
        // after deleting key groups so I'll put this in here for now.

        kg := nil;
      end;
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



end.
