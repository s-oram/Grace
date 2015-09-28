unit Lucidity.SampleMap;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.ZeroObject,
  VamLib.MoreTypes, Generics.Collections,
  VamSamplePeakBuffer,
  VamSampleDisplay,
  Lucidity.Types,
  Lucidity.Interfaces,
  uConstants,
  Classes, eeSampleFloat, eePatchObject,
  uSampleZeroCrossings;

type
  //=== Forward Declarations ==========
  TRegion = class;

  TSampleMapInfo = class;
  ISampleMapInfo = interface;
  //===================================

  TRegionInterfaceList = class(TList<IRegion>);

  TRegion = class(TInterfacedObject, IRegion)
  strict private
    fSampleFloat   : TSampleFloat;
    fProperties    : TRegionProperties;
    fPropertiesPtr : PRegionProperties;
    fKeyGroup      : IKeyGroup;
    fZeroCrossings : TSampleZeroCrossings;
    fSampleImage   : ISampleImageBuffer;
    fPeakBuffer    : IPeakBuffer;
    function GetObject        : TObject;
    function GetProperties    : PRegionProperties;
    function GetSample        : PSampleFloat;
    function GetKeyGroup      : IKeyGroup;
    function GetZeroCrossings : TSampleZeroCrossings;
    function GetSampleImage   : ISampleImageBuffer;
    function GetPeakBuffer    : IPeakBuffer;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadSample(const SampleFileName : string):boolean;

    // TODO:HIGH i think this ReplaceSample() method needs to be removed. The whole entire region
    // should be replaced. Not just replacing the sample.
    function ReplaceSample(const SampleFileName : string):boolean;

    function GetDbLevelAt(SamplePoint:integer):single;

    procedure UpdateSampleImage;

    property Sample        : TSampleFloat         read fSampleFloat    write fSampleFloat;
    property KeyGroup      : IKeyGroup            read fKeyGroup       write fKeyGroup;
    property Properties    : PRegionProperties    read fPropertiesPtr;
    property ZeroCrossings : TSampleZeroCrossings read fZeroCrossings;
    property SampleImage   : ISampleImageBuffer   read fSampleImage;
    property PeakBuffer    : IPeakBuffer          read fPeakBuffer;
  end;

  // TODO:MED TRegionCreateInfo contains much of the same data as TRegionProperties in Lucidity.Types.pas
  // I wonder if there is a way to at least put these type declarations in the same unit
  // if they can't be shared.
  TRegionCreateInfo = record
    //Required
    KeyGroup      : IKeyGroup;
    AudioFileName : string;
    LowNote       : integer;
    HighNote      : integer;
    LowVelocity   : integer;
    HighVelocity  : integer;
    RootNote      : integer;

    //== Sample ==
    SampleVolume  : single;  //For values ranges see TRegionProperties.
    SamplePan     : single;  //For values ranges see TRegionProperties.
    SampleTune    : integer; //For values ranges see TRegionProperties.
    SampleFine    : integer; //For values ranges see TRegionProperties.
    SampleBeats   : integer; //For values ranges see TRegionProperties.

    procedure Init;
    procedure AssignFrom(Source : TRegion); overload;
    procedure AssignFrom(Source : IRegion); overload;
  end;

  TSampleMap = class(TZeroObject)
  private
    function GetRegionCount: integer;
    function GetRegion(Index: integer): IRegion;
  protected
    RegionList : TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetInfo:ISampleMapInfo;

    procedure SetPatchData(var Data:TPatchNode);
    procedure GetPatchData(var Data:TPatchNode);

    //====== GUI Interaction Methods ===================
    function FindRegionByKeyGroup(const KeyGroupName : string):IRegion;
    procedure FindRegionsByKeyGroup(const KeyGroupName : string; const aList : TRegionInterfaceList);
    function FindRegionByUniqueID(UniqueID : TGUID):IRegion;
    function FindFocusedRegion : IRegion;

    function FocusRegion(UniqueID : TGUID):IRegion;
    procedure ClearFocus;

    procedure SelectRegion(UniqueID : TGUID);
    procedure DeselectRegion(UniqueID : TGUID);
    procedure DeselectOtherRegions(UniqueID : TGUID);
    procedure DeselectAllRegions;

    procedure MoveAllRegionsToKeygroup(const aKeyGroup : IKeyGroup);
    procedure MoveSelectedRegionsToKeyGoup(aKeyGroup : IKeyGroup);
    procedure DuplicateSelectedRegions;
    procedure DeleteSelectedRegions;

    procedure AddRegion(aRegion : IRegion);
    function NewRegion(CreateInfo : TRegionCreateInfo):IRegion;

    procedure DeleteRegion(Index : integer); overload;
    procedure DeleteRegion(const aRegion : IRegion); overload;
    procedure DeleteRegion(UniqueID : TGUID); overload;
    procedure DeleteRegionsInKeyGroup(aKeyGroupName : string);

    function LoadSample(const AudioFileName : string; OwningSampleGroup : IKeyGroup): IRegion;
    function ReplaceSample(const AudioFileName : string; const TargetRegion : IRegion): IRegion;

    // TODO:MED It might make sense to add a lock
    // so that only one of these methods will make changes to
    // the sample map at once.
    //===================================================

    function IsSampleFormatSupported(FileName:string):boolean;

    property RegionCount : integer read GetRegionCount;
    property Regions[Index : integer] : IRegion read GetRegion;

    function SelectedRegionCount : integer;
  end;


  ISampleMapInfo = interface
    ['{599130BB-8141-4959-AB00-E2C8DCBFD4E0}']
    function GetRegion(Index: integer): IRegion;
    function GetRegionCount: integer;
  end;

  TSampleMapInfo = class(TInterfacedObject, ISampleMapInfo)
  strict private
    RegionList : TInterfaceList;
  private
    function GetRegion(Index: integer): IRegion;
    function GetRegionCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Source : TSampleMap);

    property RegionCount : integer read GetRegionCount;
    property Regions[Index : integer] : IRegion read GetRegion;
  end;


function IsNoteInsideRegion(const aSampleRegion: IRegion; const MidiNoteData1, MidiNoteData2: byte): boolean;

procedure GenerateRegionPeaks(const aSampleRegion : IRegion; const aSampleImageWidth : integer; var Peaks : IPeakBuffer); overload;
procedure GenerateRegionPeaks(const aSampleRegion : IRegion; const SampleIndexA, SampleIndexB : integer; const aSampleImageWidth : integer; var Peaks : IPeakBuffer); overload;


implementation

uses
  //eeCustomSample,
  VamLib.Utils,
  Math,
  eeFunctions, eeDsp,
  SysUtils, AudioIO;

var
  EmptySampleFrames : integer;
  EmptySampleData : array of single;


function IsNoteInsideRegion(const aSampleRegion: IRegion; const MidiNoteData1, MidiNoteData2: byte): boolean;
var
  props : PRegionProperties;
begin
  props := aSampleRegion.GetProperties;
  if (MidiNoteData1 >= props^.LowNote)
    and (MidiNoteData1 <= props^.HighNote)
    and (MidiNoteData2 >= props^.LowVelocity)
    and (MidiNoteData2 <= props^.HighVelocity)
    then result := true
    else result := false;
end;

procedure GenerateRegionPeaks(const aSampleRegion : IRegion; const aSampleImageWidth : integer; var Peaks : IPeakBuffer);
var
  NewPeaks : IPeakBuffer;
  aSample : PSampleFloat;
begin
  NewPeaks := TPeakBuffer.Create;

  aSample := aSampleRegion.GetSample;

  if (aSample^.Properties.IsValid) and (aSample^.Properties.SampleFrames > aSampleImageWidth) then
  begin
    if aSample^.Properties.ChannelCount = 1
      then NewPeaks.GeneratePeaks(aSample^.Properties.Ch1, aSample^.Properties.SampleFrames, aSampleImageWidth);

    if aSample^.Properties.ChannelCount = 2
      then NewPeaks.GeneratePeaks(aSample^.Properties.Ch1, aSample^.Properties.Ch2, aSample^.Properties.SampleFrames, aSampleImageWidth);

    Peaks := NewPeaks;
  end else
  begin
    Peaks := nil;
  end;
end;

procedure GenerateRegionPeaks(const aSampleRegion : IRegion; const SampleIndexA, SampleIndexB : integer; const aSampleImageWidth : integer; var Peaks : IPeakBuffer); overload;
begin

end;


{ TSampleMapItem }

constructor TRegion.Create;
begin
  fPropertiesPtr := @fProperties;
  Sample         := TSampleFloat.Create;
  fZeroCrossings := TSampleZeroCrossings.Create;
  fSampleImage   := TSampleImageBuffer.Create;
  fPeakBuffer    := TPeakBuffer.Create;
end;

destructor TRegion.Destroy;
begin
  fKeyGroup      := nil;
  fPropertiesPtr := nil;
  fSampleImage   := nil;
  fPeakBuffer    := nil;
  Sample.Free;
  fZeroCrossings.Free;
  inherited;
end;

function TRegion.GetObject: TObject;
begin
  result := self;
end;

function TRegion.GetPeakBuffer: IPeakBuffer;
begin
  result := fPeakBuffer;
end;

function TRegion.GetProperties: PRegionProperties;
begin
  result := fPropertiesPtr;
end;

function TRegion.GetSample: PSampleFloat;
begin
  result := @fSampleFloat;
end;

function TRegion.GetSampleImage: ISampleImageBuffer;
begin
  result := fSampleImage;
end;

function TRegion.GetDbLevelAt(SamplePoint: integer): single;
var
  smp : PSampleFloat;
  x : single;
begin
  smp := self.GetSample;

  if assigned(smp) and (smp^.Properties.IsValid) and (SamplePoint >= 0) and (SamplePoint < smp^.Properties.SampleFrames) then
  begin
    if smp^.Properties.ChannelCount = 1 then
    begin
      x := abs(smp^.Ch1[SamplePoint]);
      result := LinearToDecibels(x);
    end else
    begin
      x := Max(abs(smp^.Ch1[SamplePoint]), abs(smp^.Ch2[SamplePoint]));
      result := LinearToDecibels(x);
    end;
  end else
  begin
    result := -120;
  end;
end;

function TRegion.GetKeyGroup: IKeyGroup;
begin
  result := fKeyGroup;
end;

function TRegion.GetZeroCrossings: TSampleZeroCrossings;
begin
  result := fZeroCrossings;
end;

function TRegion.LoadSample(const SampleFileName: string): boolean;
var
  Info : TAudioFileInfo;
  LoopStart, LoopEnd : integer;
  LoopDataFound : boolean;
begin
  result := false;
  Properties^.SampleDataLoaded := false;

  if FileExists(SampleFileName) = false then
  begin
    self.Properties^.SampleDataLoaded := false;
    self.Properties^.IsSampleError := true;
    self.Properties^.SampleErrorType := TSampleError.FileNotFound;
    self.Properties^.ErrorMessage     := 'Audio file not found.';
  end else
  //if FileExists(SampleFileName) then
  begin
    GetAudioFileInfoEx(SampleFileName, Info);

    if (Info.IsValid) and (Info.IsSupported) then
    begin
      if self.Sample.ReserveSampleMemory(Info.Channels, Info.SampleFrames) then
      begin
        //TODO:HIGH where should SampleStart/End values be set?
        self.Properties^.SampleStart := 0;
        self.Properties^.SampleEnd   := Info.SampleFrames-1;

        LoopDataFound := ReadLoopPoints(SampleFileName, LoopStart, LoopEnd);
        if LoopDataFound then
        begin
          self.Properties^.RefLoopStart   := LoopStart;
          self.Properties^.RefLoopEnd     := LoopEnd;
        end else
        begin
          self.Properties^.RefLoopStart   := -1;
          self.Properties^.RefLoopEnd     := -1;
        end;

        if self.Sample.LoadFromFile(SampleFileName) = true then
        begin
          result := true;

          self.ZeroCrossings.CalcZeroCrossingData(self.Sample);
          self.UpdateSampleImage;

          self.Properties^.SampleDataLoaded := true;
          self.Properties^.IsSampleError    := false;
          self.Properties^.SampleErrorType  := TSampleError.None;
          self.Properties^.ErrorMessage     := '';
        end else
        begin
          self.Properties^.SampleDataLoaded := false;
          self.Properties^.IsSampleError    := true;
          self.Properties^.SampleErrorType  := TSampleError.ErrorLoadingData;
          self.Properties^.ErrorMessage     := Sample.LastErrorMessage;
        end;
      end else
      begin
        self.Properties^.SampleDataLoaded := false;
        self.Properties^.IsSampleError    := true;
        self.Properties^.SampleErrorType  := TSampleError.ErrorLoadingData;
        self.Properties^.ErrorMessage     := 'Error reserving memory.';
      end;
    end else
    if (Info.IsSupported) = false then
    begin
      self.Properties^.SampleDataLoaded := false;
      self.Properties^.IsSampleError    := true;
      self.Properties^.SampleErrorType  := TSampleError.ErrorLoadingData;
      self.Properties^.ErrorMessage     := 'Unsupported File Format.';
    end else
    begin
      self.Properties^.SampleDataLoaded := false;
      self.Properties^.IsSampleError    := true;
      self.Properties^.SampleErrorType  := TSampleError.ErrorLoadingData;
      self.Properties^.ErrorMessage     := Info.ErrorMessage;
    end;

    {
    if self.Sample.LastError = seOutOfMemory then
    begin

    end;
    }

  end;
end;

function TRegion.ReplaceSample(const SampleFileName: string): boolean;
begin
  result := LoadSample(SampleFileName);
end;

procedure TRegion.UpdateSampleImage;
var
  SDI : TSampleDisplayInfo;
begin
  //============================================================================
  //            Sample Image
  //============================================================================
  SampleImage.GetObject.Resize(kSampleImageWidth, kSampleImageHeight);
  SampleImage.GetObject.LineColor := kColor_SampleDisplayLine;
  SampleImage.GetObject.Zoom      := 0;
  SampleImage.GetObject.Offset    := 0;

  if Sample.Properties.IsValid then
  begin
    SDI.IsValid      := true;
    SDI.ChannelCount := Sample.Properties.ChannelCount;
    SDI.SampleFrames := Sample.Properties.SampleFrames;
    SDI.Ch1          := Sample.Properties.Ch1;
    SDI.Ch2          := Sample.Properties.Ch2;

    SampleImage.GetObject.DrawSample(SDI);
  end else
  begin
    SDI.IsValid      := true;
    SDI.ChannelCount := 1;
    SDI.SampleFrames := EmptySampleFrames;
    SDI.Ch1          := @EmptySampleData[0];
    SDI.Ch2          := @EmptySampleData[0];

    SampleImage.GetObject.DrawSample(SDI);
  end;


  //============================================================================
  //            Peak Buffer
  //============================================================================
  GenerateRegionPeaks(self, kSampleImageWidth, fPeakBuffer);
end;

{ TSampleMap }

constructor TSampleMap.Create;
begin
  RegionList := TInterfaceList.Create;
end;

destructor TSampleMap.Destroy;
begin
  RegionList.Free;
  inherited;
end;

function TSampleMap.FindFocusedRegion: IRegion;
var
  c1: Integer;
begin
  for c1 := 0 to RegionList.Count-1 do
  begin
    if (RegionList[c1] as IRegion).GetProperties^.IsFocused then
    begin
      result := (RegionList[c1] as IRegion);
      exit; //====================================>> exit >>=====>>
    end;
  end;
  // If we've made it this far, no focused region has been found.
  result := nil;
end;

function TSampleMap.FindRegionByKeyGroup(const KeyGroupName: string): IRegion;
var
  c1: Integer;
begin
  assert(KeyGroupName <> '');

  result := nil;
  for c1 := 0 to RegionList.Count-1 do
  begin
    if (RegionList[c1] as IRegion).GetKeyGroup.GetName = KeyGroupName then
    begin
      result := (RegionList[c1] as IRegion);
      exit; //====================================>> exit >>=====>>
    end;
  end;
end;

function TSampleMap.FindRegionByUniqueID(UniqueID: TGUID): IRegion;
var
  c1: Integer;
begin
  result := nil;
  for c1 := 0 to RegionList.Count-1 do
  begin
    if (RegionList[c1] as IRegion).GetProperties^.UniqueID = UniqueID then
    begin
      result := (RegionList[c1] as IRegion);
      exit; //====================================>> exit >>=====>>
    end;
  end;
end;

procedure TSampleMap.FindRegionsByKeyGroup(const KeyGroupName: string; const aList: TRegionInterfaceList);
var
  c1: Integer;
begin
  for c1 := 0 to RegionCount-1 do
  begin
    if (Regions[c1].GetKeyGroup.GetName = KeyGroupName) then
    begin
      aList.Add(Regions[c1]);
    end;
  end;
end;

function TSampleMap.GetRegion(Index: integer): IRegion;
begin
  result := RegionList[Index] as IRegion;
end;

function TSampleMap.GetRegionCount: integer;
begin
  result := RegionList.Count;
end;

function TSampleMap.IsSampleFormatSupported(FileName: string): boolean;
var
  Info : TAudioFileInfo;
begin
  GetAudioFileInfoEx(FileName, Info);
  result := Info.IsSupported;
end;

function TSampleMap.LoadSample(const AudioFileName: string; OwningSampleGroup: IKeyGroup): IRegion;
var
  CreateInfo: TRegionCreateInfo;
begin
  if not assigned(OwningSampleGroup) then raise Exception.Create('OwningSampleGroup is not assigned.');

  CreateInfo.KeyGroup      := OwningSampleGroup;
  CreateInfo.AudioFileName := AudioFileName;
  CreateInfo.LowNote       := 0;
  CreateInfo.HighNote      := 127;
  CreateInfo.LowVelocity   := 0;
  CreateInfo.HighVelocity  := 127;
  CreateInfo.RootNote      := 60; //MIDI c4.

  result := NewRegion(CreateInfo);
end;

procedure TSampleMap.AddRegion(aRegion: IRegion);
begin
  aRegion.GetProperties^.UniqueID := CreateGuidEx;
  aRegion.UpdateSampleImage;
  RegionList.Add(aRegion);
end;

function TSampleMap.ReplaceSample(const AudioFileName: string; const TargetRegion: IRegion): IRegion;
var
  CreateInfo: TRegionCreateInfo;
  ar : IRegion;
begin
  // NOTE: ReplaceSample() replaces a sample by duplicating an existing region
  // with a new sample and deleting the 'replaced' region. For a brief period
  // both regions will 'alive' and on the sample map.
  if not assigned(TargetRegion) then raise Exception.Create('TargetRegion is not assigned.');

  CreateInfo.AudioFileName := AudioFileName;
  CreateInfo.KeyGroup   := TargetRegion.GetKeyGroup;
  CreateInfo.LowNote       := TargetRegion.GetProperties^.LowNote;
  CreateInfo.HighNote      := TargetRegion.GetProperties^.HighNote;
  CreateInfo.LowVelocity   := TargetRegion.GetProperties^.LowVelocity;
  CreateInfo.HighVelocity  := TargetRegion.GetProperties^.HighVelocity;
  CreateInfo.RootNote      := TargetRegion.GetProperties^.RootNote;

  ar := NewRegion(CreateInfo);

  if assigned(ar) then
  begin
    DeleteRegion(TargetRegion);
    result := ar;
  end else
  begin
    result := nil;
  end;

end;

function TSampleMap.NewRegion(CreateInfo: TRegionCreateInfo): IRegion;
var
  rx : TRegion;
  aRegion : IRegion;
begin
  result := nil;
  rx := TRegion.Create;
  aRegion := rx;

  assert(assigned(CreateInfo.KeyGroup));

  //====================
  rx.Properties^.SampleDataLoaded := false;
  rx.Properties^.IsSampleError    := false;
  rx.Properties^.ErrorMessage     := '';
  rx.Properties^.UniqueID         := CreateGuidEx;
  rx.Properties^.SampleFileName   := CreateInfo.AudioFileName;
  rx.Properties^.LowNote          := CreateInfo.LowNote;
  rx.Properties^.HighNote         := CreateInfo.HighNote;
  rx.Properties^.LowVelocity      := CreateInfo.LowVelocity;
  rx.Properties^.HighVelocity     := CreateInfo.HighVelocity;
  rx.Properties^.RootNote         := CreateInfo.RootNote;
  rx.Properties^.RefLoopStart     := -1;
  rx.Properties^.RefLoopEnd       := -1;
  rx.Properties^.UserLoopStart    := -1;
  rx.Properties^.UserLoopEnd      := -1;
  rx.Properties^.SampleVolume     := CreateInfo.SampleVolume;
  rx.Properties^.SamplePan        := CreateInfo.SamplePan;
  rx.Properties^.SampleTune       := CreateInfo.SampleTune;
  rx.Properties^.SampleFine       := CreateInfo.SampleFine;
  rx.Properties^.SampleBeats      := CreateInfo.SampleBeats;

  rx.KeyGroup := CreateInfo.KeyGroup;
  //====================

  rx.LoadSample(CreateInfo.AudioFileName);

  // TODO:HIGH Regions are always added. What happens if I try loading an
  // unsupported file format? ie. .FLAC or something else.
  //Add the region...
  RegionList.Add(aRegion);
  result := aRegion;
end;

procedure TSampleMap.DeselectAllRegions;
var
  c1: Integer;
begin
  for c1 := 0 to RegionCount-1 do
  begin
    Regions[c1].GetProperties^.IsSelected := false;
    Regions[c1].GetProperties^.IsFocused  := false;
  end;
end;

procedure TSampleMap.DeselectOtherRegions(UniqueID: TGUID);
var
  c1: Integer;
begin
  for c1 := 0 to RegionCount-1 do
  begin
    if Regions[c1].GetProperties^.UniqueID <> UniqueID then
    begin
      Regions[c1].GetProperties^.IsSelected := false;
      Regions[c1].GetProperties^.IsFocused  := false;
    end;
  end;
end;

procedure TSampleMap.DeselectRegion(UniqueID: TGUID);
begin
  FindRegionByUniqueID(UniqueID).GetProperties^.IsSelected := false;
  FindRegionByUniqueID(UniqueID).GetProperties^.IsFocused  := false;
end;

function TSampleMap.SelectedRegionCount: integer;
var
  c1 : integer;
  Count : integer;
begin
  Count := 0;

  for c1 := 0 to RegionCount-1 do
  begin
    if Regions[c1].GetProperties^.IsSelected then
    begin
      inc(count);
    end;
  end;

  result := Count;
end;

procedure TSampleMap.SelectRegion(UniqueID: TGUID);
begin
  FindRegionByUniqueID(UniqueID).GetProperties^.IsSelected := true;
end;

function TSampleMap.FocusRegion(UniqueID: TGUID):IRegion;
var
  c1: Integer;
  DelselectOthers : boolean;
begin
  result := nil;
  DelselectOthers := false;

  for c1 := 0 to RegionCount-1 do
  begin
    if (Regions[c1].GetProperties^.UniqueID = UniqueID) then
    begin
      if (Regions[c1].GetProperties^.IsFocused) then
      begin
        // Region is already focused, nothing else needs to be done,
        exit; //=====================>> exit >>========================>>
      end;

      // If the region is selected, but not focused, we need to deselect the other regions.
      if (Regions[c1].GetProperties^.IsSelected = false)
        then DelselectOthers := true
        else DelselectOthers := false;

      Regions[c1].GetProperties^.IsFocused  := true;
      Regions[c1].GetProperties^.IsSelected := true;
      result := Regions[c1];
    end;
  end;

  // un-focus and un-select other regions.
  for c1 := 0 to RegionCount-1 do
  begin
    if (Regions[c1].GetProperties^.UniqueID <> UniqueID) then
    begin
      Regions[c1].GetProperties^.IsFocused := false;
      if DelselectOthers then Regions[c1].GetProperties^.IsSelected := false;
    end;
  end;
end;

procedure TSampleMap.Clear;
begin
  RegionList.Clear;
end;

procedure TSampleMap.ClearFocus;
var
  c1: Integer;
begin
  for c1 := 0 to RegionCount-1 do
  begin
    Regions[c1].GetProperties^.IsFocused := false;
  end;
end;

procedure TSampleMap.DeleteRegion(Index: integer);
begin
  RegionList.Delete(Index);
end;

procedure TSampleMap.DeleteRegion(const aRegion: IRegion);
var
  c1: Integer;
begin
  if assigned(aRegion) then
  begin
    for c1 := RegionCount-1 downto 0 do
    begin
      if Regions[c1] = aRegion then DeleteRegion(c1);
    end;
  end;
end;

procedure TSampleMap.DeleteRegion(UniqueID: TGUID);
var
  c1: Integer;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    if Regions[c1].GetProperties^.UniqueID = UniqueID
      then DeleteRegion(c1);
  end;
end;

procedure TSampleMap.DeleteRegionsInKeyGroup(aKeyGroupName: string);
var
  c1: Integer;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    if Regions[c1].GetKeyGroup.GetName = aKeyGroupName then
    begin
      DeleteRegion(c1);
    end;
  end;
end;

procedure TSampleMap.DuplicateSelectedRegions;
var
  c1: Integer;
  RegionCreateInfo : TRegionCreateInfo;
  rg : IRegion;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    if Regions[c1].GetProperties^.IsSelected then
    begin
      RegionCreateInfo.AssignFrom(Regions[c1]);

      rg := NewRegion(RegionCreateInfo);
      rg.GetProperties^.IsSelected := true;

      Regions[c1].GetProperties^.IsSelected := false;
      Regions[c1].GetProperties^.IsFocused  := false;
    end;
  end;
end;

procedure TSampleMap.DeleteSelectedRegions;
var
  c1: Integer;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    if Regions[c1].GetProperties^.IsSelected then
    begin
      DeleteRegion(c1);
    end;
  end;
end;

procedure TSampleMap.MoveAllRegionsToKeygroup(const aKeyGroup: IKeyGroup);
var
  c1: Integer;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    (Regions[c1].GetObject as TRegion).KeyGroup := aKeyGroup;
  end;
end;

procedure TSampleMap.MoveSelectedRegionsToKeyGoup(aKeyGroup: IKeyGroup);
var
  c1: Integer;
begin
  for c1 := RegionCount-1 downto 0 do
  begin
    if Regions[c1].GetProperties^.IsSelected then
    begin
      (Regions[c1].GetObject as TRegion).KeyGroup := aKeyGroup;
    end;
  end;
end;





function TSampleMap.GetInfo: ISampleMapInfo;
var
  aInfo : TSampleMapInfo;
begin
  aInfo := TSampleMapInfo.Create;
  aInfo.AssignFrom(self);
  result := aInfo;
end;

procedure TSampleMap.GetPatchData(var Data: TPatchNode);
var
  c1: Integer;
  ChildNode : TPatchNode;
  Props : PRegionProperties;
begin
  assert(false, 'Need to save SampleGroup field for each region.');

  for c1 := 0 to RegionCount-1 do
  begin
    ChildNode := Data.NewChildNode('Region');

    Props := Regions[c1].GetProperties;

    ChildNode.AddValue('SampleFileName', Props^.SampleFileName);
    ChildNode.AddValue('LowNote', Props^.LowNote);
    ChildNode.AddValue('HighNote', Props^.HighNote);
    ChildNode.AddValue('LowVelocity', Props^.LowVelocity);
    ChildNode.AddValue('HighVelocity', Props^.HighVelocity);
    ChildNode.AddValue('RootNote', Props^.RootNote);
    ChildNode.AddValue('SampleStart', Props^.SampleStart);
    ChildNode.AddValue('SampleEnd', Props^.SampleEnd);
    ChildNode.AddValue('LoopStart', Props^.RefLoopStart);
    ChildNode.AddValue('LoopEnd', Props^.RefLoopEnd);
  end;
end;

procedure TSampleMap.SetPatchData(var Data: TPatchNode);
var
  c1: Integer;
  ChildNode : TPatchNode;
  //Props : PRegionProperties;
  CreateInfo : TRegionCreateInfo;
  aSampleRegion : IRegion;
begin
  assert(false, 'Need to save SampleGroup field for each region.');

  for c1 := 0 to Data.ChildNodeCount-1 do
  begin
    ChildNode := Data.ChildNode[c1];
    if ChildNode.NodeName = 'Region' then
    begin
      CreateInfo.AudioFileName := ChildNode.GetValue('SampleFileName', '');
      CreateInfo.LowNote       := ChildNode.GetValue('LowNote', 0);
      CreateInfo.HighNote      := ChildNode.GetValue('HighNote', 127);
      CreateInfo.LowVelocity  := ChildNode.GetValue('LowVelocity', 0);
      CreateInfo.HighVelocity := ChildNode.GetValue('HighVelocity', 127);
      CreateInfo.RootNote     := ChildNode.GetValue('RootNote', 64);

      aSampleRegion := NewRegion(CreateInfo);

      if assigned(aSampleRegion) then
      begin
        aSampleRegion.GetProperties^.SampleStart := ChildNode.GetValue('LoopStart', -1);
        aSampleRegion.GetProperties^.SampleEnd   := ChildNode.GetValue('LoopStart', -1);
        aSampleRegion.GetProperties^.RefLoopStart   := ChildNode.GetValue('LoopStart', -1);
        aSampleRegion.GetProperties^.RefLoopEnd     := ChildNode.GetValue('LoopStart', -1);
      end;
    end;
  end;
end;





{ TSampleMapInfo }

constructor TSampleMapInfo.Create;
begin
  RegionList := TInterfaceList.Create;
end;

destructor TSampleMapInfo.Destroy;
begin
  RegionList.Free;
  inherited;
end;

procedure TSampleMapInfo.AssignFrom(Source: TSampleMap);
var
  c1 : integer;
begin
  self.RegionList.Clear;

  for c1  := 0 to source.RegionCount-1 do
  begin
    self.RegionList.Add(Source.RegionList[c1]);
  end;
end;

function TSampleMapInfo.GetRegion(Index: integer): IRegion;
begin
  result := self.RegionList[Index] as IRegion;
end;

function TSampleMapInfo.GetRegionCount: integer;
begin
  result := self.RegionList.Count;
end;



{ TRegionCreateInfo }

procedure TRegionCreateInfo.Init;
begin
  self.KeyGroup      := nil;
  self.AudioFileName := '';
  self.LowNote       := 0;
  self.HighNote      := 127;
  self.LowVelocity   := 0;
  self.HighVelocity  := 127;
  self.RootNote      := 60; //who knows what MIDI note that is?.... //TODO:MED find out!

  self.SampleVolume  := 0;
  self.SamplePan     := 0;
  self.SampleTune    := 0;
  self.SampleFine    := 0;
  self.SampleBeats   := 4;
end;

procedure TRegionCreateInfo.AssignFrom(Source: TRegion);
begin
  self.KeyGroup      := source.KeyGroup;
  self.AudioFileName := source.Properties^.SampleFileName;
  self.LowNote       := source.Properties^.LowNote;
  self.HighNote      := source.Properties^.HighNote;
  self.LowVelocity   := source.Properties^.LowVelocity;
  self.HighVelocity  := source.Properties^.HighVelocity;
  self.RootNote      := source.Properties^.RootNote;

  self.SampleVolume  := source.Properties^.SampleVolume;
  self.SamplePan     := source.Properties^.SamplePan;
  self.SampleTune    := source.Properties^.SampleTune;
  self.SampleFine    := source.Properties^.SampleFine;
  self.SampleBeats   := source.Properties^.SampleBeats;
end;

procedure TRegionCreateInfo.AssignFrom(Source: IRegion);
begin
  self.KeyGroup      := source.GetKeyGroup;
  self.AudioFileName := source.GetProperties^.SampleFileName;
  self.LowNote       := source.GetProperties^.LowNote;
  self.HighNote      := source.GetProperties^.HighNote;
  self.LowVelocity   := source.GetProperties^.LowVelocity;
  self.HighVelocity  := source.GetProperties^.HighVelocity;
  self.RootNote      := source.GetProperties^.RootNote;

  self.SampleVolume  := source.GetProperties^.SampleVolume;
  self.SamplePan     := source.GetProperties^.SamplePan;
  self.SampleTune    := source.GetProperties^.SampleTune;
  self.SampleFine    := source.GetProperties^.SampleFine;
  self.SampleBeats   := source.GetProperties^.SampleBeats;
end;




initialization
  EmptySampleFrames := 200;
  SetLength(EmptySampleData, EmptySampleFrames);
finalization
  SetLength(EmptySampleData, 0);

end.




