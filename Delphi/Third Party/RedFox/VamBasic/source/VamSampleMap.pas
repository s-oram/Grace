unit VamSampleMap;

interface

uses
  Types, Classes, Controls, Generics.Collections, AggColor,
  RedFox, RedFoxColor,
  VamVisibleControl, VamWinControl;

type
  //===== Forward Declarations =====
  TVamSampleRegion = class;
  TVamSampleRegionList = class;
  //================================

  TBooleanEvent = procedure(Sender : TObject; Value : boolean) of  object;
  TVamDragRegionCountEvent = procedure(Sender : TObject; const Data:IVamDragData; var DragRegionCount : integer) of object;
  TVamNewRegionsEvent = procedure(Sender : TObject; const aRegions : TVamSampleRegionList; const Data:IVamDragData) of object;
  TVamNewCopiedRegionsEvent = procedure(Sender : TObject; const aRegions : TVamSampleRegionList) of object;
  TVamReplaceRegionEvent = procedure(Sender : TObject; const NewRegion, OldRegion : TVamSampleRegion; const Data:IVamDragData) of object;

  TVamSampleRegionMouseDownEvent = procedure(const Sender:TObject; const Button: TMouseButton; const Shift: TShiftState; const aRegion:TVamSampleRegion) of object;
  TVamSampleRegionMouseUpEvent   = procedure(const Sender:TObject; const Button: TMouseButton; const Shift: TShiftState; const aRegion:TVamSampleRegion) of object;

  TVamSampleRegionEvent = procedure(const Sender:TObject; aRegion:TVamSampleRegion) of object;

  TRegionHandleID = (rhNone, rhTopLeft, rhTopRight, rhBottomRight, rhBottomLeft, rhTop, rhRight, rhBottom, rhLeft);

  TVamSampleMapKeyZone = record
    Bounds     : TRectF;
  end;

  TVamSampleMapDisplayInfo = record
    IsValid         : boolean;
    FileName        : string;  // for selected region
    RootNote        : byte;    // for selected region
    LowKey          : byte;    // for selected region
    HighKey         : byte;    // for selected region
    LowVelocity     : byte;    // for selected region
    HighVelocity    : byte;    // for selected region
  end;

  TVamSampleRegion = class
  strict private
    fLowKey: byte;
    fLowVelocity: byte;
    fHighKey: byte;
    fHighVelocity: byte;
    fRootNote: byte;
    fIsSelected: boolean;
    fIsFocused: boolean;
    fFileName: string;
    fUniqueID: TGUID;
    fMovedLowKey: integer;
    fMovedLowVelocity: integer;
    fMovedHighKey: integer;
    fMovedHighVelocity: integer;
    fIsSampleError: boolean;
    fIsMoving: boolean;
    fMovedRootNote: integer;
    fIsDragSelected: boolean;
    fIsInOtherKeyGroup: boolean;
    fIsVisible: boolean;
    procedure SetMovedHighKey(Value: integer);
    procedure SetMovedHighVelocity(Value: integer);
    procedure SetMovedLowKey(Value: integer);
    procedure SetMovedLowVelocity(Value: integer);
    procedure SetMovedRootNote(Value: integer);
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure AssignFrom(Source : TVamSampleRegion);

    property UniqueID          : TGUID   read fUniqueID          write fUniqueID;
    property FileName          : string  read fFileName          write fFileName;
    property IsSampleError     : boolean read fIsSampleError     write fIsSampleError; //Is there a problem loading the sample data?
    property IsInOtherKeyGroup : boolean read fIsInOtherKeyGroup write fIsInOtherKeyGroup;
    property IsVisible         : boolean read fIsVisible         write fIsVisible; // generally only regions in other key groups will be made invisible.
    property IsSelected        : boolean read fIsSelected        write fIsSelected;
    property IsDragSelected    : boolean read fIsDragSelected    write fIsDragSelected;
    property IsFocused         : boolean read fIsFocused         write fIsFocused;
    property IsMoving          : boolean read fIsMoving          write fIsMoving;
    property RootNote          : byte    read fRootNote          write fRootNote;
    property LowKey            : byte    read fLowKey            write fLowKey;
    property HighKey           : byte    read fHighKey           write fHighKey;
    property LowVelocity       : byte    read fLowVelocity       write fLowVelocity;
    property HighVelocity      : byte    read fHighVelocity      write fHighVelocity;
    property MovedLowKey       : integer read fMovedLowKey       write SetMovedLowKey;
    property MovedHighKey      : integer read fMovedHighKey      write SetMovedHighKey;
    property MovedLowVelocity  : integer read fMovedLowVelocity  write SetMovedLowVelocity;
    property MovedHighVelocity : integer read fMovedHighVelocity write SetMovedHighVelocity;
    property MovedRootNote     : integer read fMovedRootNote     write SetMovedRootNote;
  end;

  TVamSampleRegionList = class(TObjectList<TVamSampleRegion>);

  TVamSampleMap = class(TVamWinControl)
  strict private
    fOffset: single;
    fZoom: single;
    fSampleRegions: TVamSampleRegionList;
    fProposedSampleRegions: TVamSampleRegionList;
    procedure SetOffset(const Value: single);
    procedure SetZoom(const Value: single);
  private
    fOnDeselectOtherRegions: TVamSampleRegionEvent;
    fOnSelectRegion: TVamSampleRegionEvent;
    fOnDeselectAllRegions: TNotifyEvent;
    fOnFocusRegion: TVamSampleRegionEvent;
    fOnDeselectRegion: TVamSampleRegionEvent;
    fOnShowRegionContextMenu: TVamSampleRegionEvent;
    fOnRegionMoved: TVamSampleRegionEvent;
    fOnRegionInfoChanged: TNotifyEvent;
    fOnMouseOverRegionChanged: TVamSampleRegionEvent;
    fOnDragSelectionChanged: TNotifyEvent;
    fOnDragSelectStart: TNotifyEvent;
    fOnDragSelectEnd: TNotifyEvent;
    fOnGetDragRegionCount: TVamDragRegionCountEvent;
    fOnNewRegions: TVamNewRegionsEvent;
    fOnNewCopiedRegions: TVamNewCopiedRegionsEvent;
    fOnReplaceRegion: TVamReplaceRegionEvent;
    fOnShowReplaceRegionMessage: TBooleanEvent;
    IsReplaceRegionMessageShowing : boolean;

    function GetColors(const Index: Integer): TRedFoxColorString;
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
  protected

    MouseOverRegion       : TVamSampleRegion;
    MouseOverRegionHandle : TRegionHandleID;

    fColors : array[0..10] of TRedFoxColorString;

    IsCopyRegionActive : boolean;
    IsDragSelectActive : boolean;
    LastDragSelectIndex : integer;
    DragSelectRect     : TRect;
    IsGrabbedByLeft : boolean;
    IsGrabbedByRight : boolean;
    WatchForDrag : boolean;
    IsDragActive : boolean;
    DeselectOthersOnMouseUp : boolean;
    MouseDownPos : TPoint;
    MouseDownRegion       : TVamSampleRegion;
    MouseDownRegionHandle : TRegionHandleID;
    OriginKey        : integer;
    OriginVelocity   : integer;
    LastDistKey      : integer;
    LastDistVelocity : integer;

    NumberOfKeysToShow : integer;
    KeyPixelWidth      : integer;
    KeyBedPixelWidth   : integer;
    KeyBedPixelOffset  : integer;

    KeyZone : array [0..127] of TVamSampleMapKeyZone;

    ProposedMapInfo : record
      IsFullKeyboardSpread : boolean;
    end;

    procedure ZoomOffsetChanged;
    procedure RegionInfoChanged;
    procedure MouseOverRegionChanged(aRegion:TVamSampleRegion);

    function PixelToSampleMapPos(Pixel : TPoint):TPoint;

    function CalcSampleRegionBounds(aSampleRegion : TVamSampleRegion; const CalcMovingBounds : boolean = false):TRectF;
    function CalcRegionHandleBounds(const aRegion : TVamSampleRegion; const HandleIndex : TRegionHandleID):TRectF;

    function CalcProposedSampleRegionKeyWidth(aPoint : TPoint):integer;

    //UpdateProposedRegions is called when dragging and dropping.
    procedure UpdateProposedRegions(DropPoint : TPoint; NewRegionCount:integer);
    procedure UpdateProposedRegions_RegionReplace(DropPoint : TPoint);
    procedure UpdateProposedRegions_MultiFileDrop(DropPoint : TPoint; NewRegionCount:integer);
    procedure UpdateProposedRegions_SingleFileDrop(DropPoint : TPoint; NewRegionCount:integer);

    procedure DrawSampleRegion(const aRegion:TVamSampleRegion; aColor:TRedFoxColor; const UseMovingBounds : boolean);
    procedure DrawSampleRegionResizeHandles(const aRegion:TVamSampleRegion; const aColor:TRedFoxColor; const UseMovingBounds : boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    function GetRegionAt(PixelPosX, PixelPosY:integer):TVamSampleRegion;
    function GetRegionHandleAt(PixelPosX, PixelPosY:integer):TRegionHandleID;

    //procedure ResizeSelectedRegions(KeyOffset, VelocityOffset : integer; Handle : TRegionHandleID; const Snapping : boolean);
    procedure Paint; override;

    procedure SortSampleRegionList;

    function GetDragRegionCount(const Data : IVamDragData):integer;

    procedure UpdateCursorIcon(Shift: TShiftState; X, Y: Integer);

    procedure ShowReplaceRegionMessage(Show : boolean);

    function FindRegionAbove(LowKey, HighKey, LowVelocity, HighVelocity : integer):TVamSampleRegion;
    function FindRegionBelow(LowKey, HighKey, LowVelocity, HighVelocity : integer):TVamSampleRegion;
    function FindRegionLeft(LowKey, HighKey, LowVelocity, HighVelocity : integer):TVamSampleRegion;
    function FindRegionRight(LowKey, HighKey, LowVelocity, HighVelocity : integer):TVamSampleRegion;

    procedure DeselectOtherRegions(const TargetRegion : TVamSampleRegion);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); override;
    procedure OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); override;
    procedure OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); override;
    procedure OleDragLeave(Sender: TObject); override;

    property SampleRegions         : TVamSampleRegionList read fSampleRegions         write fSampleRegions;
    property ProposedSampleRegions : TVamSampleRegionList read fProposedSampleRegions write fProposedSampleRegions;

    function FindRegionByUniqueID(UniqueID : TGUID):TVamSampleRegion;

    procedure MoveRegionToFront(aRegion : TVamSampleRegion);

    function GetDisplayInfo : TVamSampleMapDisplayInfo;

    function GetDragSelectCount : integer;
    function GetSelectedCount   : integer;
    function GetMouseOverRegionInfo : TVamSampleMapDisplayInfo;


  published
    property Color_Background            : TRedFoxColorString index 0 read GetColors write SetColors;
    property Color_BackgroundLines       : TRedFoxColorString index 1 read GetColors write SetColors;
    property Color_Region                : TRedFoxColorString index 2 read GetColors write SetColors;
    property Color_RegionFocused         : TRedFoxColorString index 3 read GetColors write SetColors;
    property Color_RegionMouseOver       : TRedFoxColorString index 4 read GetColors write SetColors;
    property Color_ProposedRegions       : TRedFoxColorString index 5 read GetColors write SetColors;
    property Color_SelectionRect         : TRedFoxColorString index 6 read GetColors write SetColors;
    property Color_RegionError           : TRedFoxColorString index 7 read GetColors write SetColors;
    property Color_OtherKeyGroup         : TRedFoxColorString index 8 read GetColors write SetColors;
    property Color_OtherKeyGroupSelected : TRedFoxColorString index 9 read GetColors write SetColors;


    property Zoom   : single read fZoom   write SetZoom;   //Range 0..1. 0=Show all keys, 1=Show minimum keys.
    property Offset : single read fOffset write SetOffset; //Range 0..1  0=Show lowest keys, 1=show highest keys.

    property OnFocusRegion          : TVamSampleRegionEvent read fOnFocusRegion          write fOnFocusRegion;
    property OnSelectRegion         : TVamSampleRegionEvent read fOnSelectRegion         write fOnSelectRegion;
    property OnDeselectRegion       : TVamSampleRegionEvent read fOnDeselectRegion       write fOnDeselectRegion;
    property OnDeselectOtherRegions : TVamSampleRegionEvent read fOnDeselectOtherRegions write fOnDeselectOtherRegions;
    property OnDeselectAllRegions   : TNotifyEvent          read fOnDeselectAllRegions   write fOnDeselectAllRegions;
    property OnRegionMoved          : TVamSampleRegionEvent read fOnRegionMoved          write fOnRegionMoved;

    property OnShowRegionContextMenu : TVamSampleRegionEvent read fOnShowRegionContextMenu write fOnShowRegionContextMenu;

    property OnDragSelectStart      : TNotifyEvent read fOnDragSelectStart      write fOnDragSelectStart;
    property OnDragSelectEnd        : TNotifyEvent read fOnDragSelectEnd        write fOnDragSelectEnd;
    property OnDragSelectionChanged : TNotifyEvent read fOnDragSelectionChanged write fOnDragSelectionChanged;

    property OnGetDragRegionCount : TVamDragRegionCountEvent  read fOnGetDragRegionCount write fOnGetDragRegionCount;
    property OnNewRegions         : TVamNewRegionsEvent       read fOnNewRegions         write fOnNewRegions;
    property OnNewCopiedRegions   : TVamNewCopiedRegionsEvent read fOnNewCopiedRegions   write fOnNewCopiedRegions;
    property OnReplaceRegion      : TVamReplaceRegionEvent    read fOnReplaceRegion      write fOnReplaceRegion;

    property OnShowReplaceRegionMessage : TBooleanEvent read fOnShowReplaceRegionMessage write fOnShowReplaceRegionMessage;

    // OnMouseOverRegionChanged is fired whenever the region underneath the mouse changes. It is also called when the mouse
    // cursor leaves the control.
    property OnMouseOverRegionChanged : TVamSampleRegionEvent read fOnMouseOverRegionChanged write fOnMouseOverRegionChanged;

    // OnRegionInfoChanged is fired when 'in front' region changes. Different regions will
    // have 'in front' priority at different times. Generally the priority is
    // 1: New sample regions (while being dragged and dropped on the sample map prior to loading.)
    // 2: The region under the mouse cursor. (so the user can move the mouse and see region details.)
    // 3: The Focused region. (is called while the focused region is being moved or resized.)
    property OnRegionInfoChanged : TNotifyEvent read fOnRegionInfoChanged write fOnRegionInfoChanged;
    {$INCLUDE TControlProperties.inc}
  end;





implementation

uses
  SysUtils,
  VamLib.WinUtils,
  VamLib.Utils,
  VamSampleMap.Sorting,
  VamSampleMap.Movement,
  Math, Graphics, AggPixelFormat;

const
  kMinimumKeys = 24;
  kMaximumKeys = 128;

procedure CorrectOrientation(var aRect : TRect);
var
  tx : integer;
begin
  if aRect.Left > aRect.Right then
  begin
    tx := aRect.Left;
    aRect.Left  := aRect.Right;
    aRect.Right := tx;
  end;

  if aRect.Top > aRect.Bottom then
  begin
    tx := aRect.Top;
    aRect.Top := aRect.Bottom;
    aRect.Bottom := tx;
  end;
end;

function Intersects(RectA, RectB: TRect):boolean;
begin
  CorrectOrientation(RectA);
  CorrectOrientation(RectB);

  result := RectA.IntersectsWith(RectB);
end;

procedure CorrectMovingRegionBounds(aRegion : TVamSampleRegion);
begin
  if aRegion.MovedLowKey < 0   then aRegion.MovedLowKey := 0;
  if aRegion.MovedLowKey > 127 then aRegion.MovedLowKey := 127;

  if aRegion.MovedHighKey < 0   then aRegion.MovedHighKey := 0;
  if aRegion.MovedHighKey > 127 then aRegion.MovedHighKey := 127;

  if aRegion.MovedRootNote < 0   then aRegion.MovedRootNote := 0;
  if aRegion.MovedRootNote > 127 then aRegion.MovedRootNote := 127;

  if aRegion.MovedLowVelocity < 0   then aRegion.MovedLowVelocity := 0;
  if aRegion.MovedLowVelocity > 127 then aRegion.MovedLowVelocity := 127;

  if aRegion.MovedHighVelocity < 0   then aRegion.MovedHighVelocity := 0;
  if aRegion.MovedHighVelocity > 127 then aRegion.MovedHighVelocity := 127;
end;






procedure CommitRegionMovement(Regions : TVamSampleRegionList);
var
  c1: Integer;
begin
  for c1 := 0 to Regions.Count-1 do
  begin
    if Regions[c1].IsMoving then
    begin
      Regions[c1].IsMoving     := false;
      Regions[c1].LowKey       := Regions[c1].MovedLowKey;
      Regions[c1].HighKey      := Regions[c1].MovedHighKey;
      Regions[c1].RootNote     := Regions[c1].MovedRootNote;
      Regions[c1].LowVelocity  := Regions[c1].MovedLowVelocity;
      Regions[c1].HighVelocity := Regions[c1].MovedHighVelocity;
    end;
  end;
end;


procedure CopySampleRegions(Dest, Source : TVamSampleRegionList);
var
  aRegion : TVamSampleRegion;
  c1: Integer;
begin
  Dest.Clear;

  for c1 := 0 to Source.Count-1 do
  begin
    aRegion := TVamSampleRegion.Create;
    aRegion.AssignFrom(Source[c1]);
    Dest.Add(aRegion);
  end;
end;


procedure PrepareCopiedRegions(const Regions, ProposedRegions : TVamSampleRegionList);
var
  c1 : integer;
  aRegion : TVamSampleRegion;
begin
  for c1 := 0 to Regions.Count-1 do
  begin
    if Regions[c1].IsSelected then
    begin
      aRegion := TVamSampleRegion.Create;
      aRegion.AssignFrom(Regions[c1]);
      ProposedRegions.Add(aRegion);

      Regions[c1].IsSelected := false;
    end;
  end;
end;


{ TRedFoxSampleRegion }


constructor TVamSampleRegion.Create;
begin
  IsMoving := false;
end;

destructor TVamSampleRegion.Destroy;
begin

  inherited;
end;

procedure TVamSampleRegion.AssignFrom(Source: TVamSampleRegion);
begin
  Self.UniqueID          := Source.UniqueID;
  Self.FileName          := Source.FileName;
  Self.IsSampleError     := Source.IsSampleError;
  Self.IsInOtherKeyGroup := Source.IsInOtherKeyGroup;
  Self.IsVisible         := Source.IsVisible;
  Self.IsSelected        := Source.IsSelected;
  Self.IsDragSelected    := Source.IsDragSelected;
  Self.IsFocused         := Source.IsFocused;
  Self.IsMoving          := Source.IsMoving;
  Self.RootNote          := Source.RootNote;
  Self.LowKey            := Source.LowKey;
  Self.HighKey           := Source.HighKey;
  Self.LowVelocity       := Source.LowVelocity;
  Self.HighVelocity      := Source.HighVelocity;
  Self.MovedLowKey       := Source.MovedLowKey;
  Self.MovedHighKey      := Source.MovedHighKey;
  Self.MovedLowVelocity  := Source.MovedLowVelocity;
  Self.MovedHighVelocity := Source.MovedHighVelocity;
  Self.MovedRootNote     := Source.MovedRootNote;
end;

procedure TVamSampleRegion.SetMovedHighKey(Value: integer);
begin
  //if Value > 127 then Value := 127;
  //if Value < 0   then Value := 0;
  fMovedHighKey := Value;
end;

procedure TVamSampleRegion.SetMovedHighVelocity(Value: integer);
begin
  //if Value > 127 then Value := 127;
  //if Value < 0   then Value := 0;
  fMovedHighVelocity := Value;
end;

procedure TVamSampleRegion.SetMovedLowKey(Value: integer);
begin
  //if Value > 127 then Value := 127;
  //if Value < 0   then Value := 0;
  fMovedLowKey := Value;
end;

procedure TVamSampleRegion.SetMovedLowVelocity(Value: integer);
begin
  //if Value > 127 then Value := 127;
  //if Value < 0   then Value := 0;
  fMovedLowVelocity := Value;
end;

procedure TVamSampleRegion.SetMovedRootNote(Value: integer);
begin
  //if Value > 127 then Value := 127;
  //if Value < 0   then Value := 0;
  fMovedRootNote := Value;
end;

{ TRedFoxSampleMap }

constructor TVamSampleMap.Create(AOwner: TComponent);
begin
  inherited;
  MouseOverRegion := nil;
  MouseOverRegionHandle := rhNone;

  fZoom := 0;
  fOffset := 0;
  ZoomOffsetChanged;

  SampleRegions := TVamSampleRegionList.Create(true);
  ProposedSampleRegions := TVamSampleRegionList.Create(true);

  Color_Background            := '$FF242B39';
  Color_BackgroundLines       := '$FF64718D';
  Color_Region                := '$dd226CEA';
  Color_RegionFocused         := '$dd226CEA';
  Color_RegionMouseOver       := '$ffffffff';
  Color_ProposedRegions       := '$a0EAEDD3';
  Color_SelectionRect         := '$40639DFF';
  Color_RegionError           := '$66FF0000';
  Color_OtherKeyGroup         := '$55FFD455';
  Color_OtherKeyGroupSelected := '$bbFFD455';
end;

procedure TVamSampleMap.DeselectOtherRegions(const TargetRegion: TVamSampleRegion);
var
  c1: Integer;
begin
  for c1 := 0 to SampleRegions.Count-1 do
  begin
    if SampleRegions[c1] <> TargetRegion then
    begin
      SampleRegions[c1].IsSelected := false;
    end;

  end;
end;

destructor TVamSampleMap.Destroy;
begin
  SampleRegions.Free;
  ProposedSampleRegions.Free;
  inherited;
end;

procedure TVamSampleMap.RegionInfoChanged;
begin
  if assigned(OnRegionInfoChanged) then OnRegionInfoChanged(self);
end;

procedure TVamSampleMap.MouseOverRegionChanged(aRegion: TVamSampleRegion);
begin
  if assigned(OnMouseOverRegionChanged) then OnMouseOverRegionChanged(self, aRegion);
end;



procedure TVamSampleMap.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (aWidth <> 0) and (aHeight <> 0) then
  begin
    ZoomOffsetChanged;
  end;
end;

procedure TVamSampleMap.SetColors(const Index: Integer; const Value: TRedFoxColorString);
begin
  if Value <> fColors[Index] then
  begin
    fColors[Index] := Value;
    Invalidate;
  end;
end;

function TVamSampleMap.GetColors(const Index: Integer): TRedFoxColorString;
begin
  result := fColors[Index];
end;

procedure TVamSampleMap.SetOffset(const Value: single);
begin
  if Value <> fOffset then
  begin
    fOffset := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TVamSampleMap.SetZoom(const Value: single);
begin
  if Value <> fZoom then
  begin
    fZoom := Value;
    ZoomOffsetChanged;
  end;
end;

procedure TVamSampleMap.ShowReplaceRegionMessage(Show: boolean);
begin
  if Show <> IsReplaceRegionMessageShowing then
  begin
    IsReplaceRegionMessageShowing := Show;
    if assigned(OnShowReplaceRegionMessage) then OnShowReplaceRegionMessage(self, Show);
  end;


end;

procedure TVamSampleMap.ZoomOffsetChanged;
var
  c1 : integer;
begin
  if (Width = 0) or (Height = 0) then exit;


  NumberOfKeysToShow := round((kMaximumKeys * (1 - fZoom)) + (kMinimumKeys * fZoom));

  //== Calculate the key left & right bounds ==
  if fZoom = 0 then
  begin
    KeyBedPixelWidth  := self.Width;
    KeyPixelWidth     := ceil(KeyBedPixelWidth / kMaximumKeys);
    KeyBedPixelOffset := 0;

    for c1 := 0 to kMaximumKeys-1 do
    begin
      KeyZone[c1].Bounds.Left  := (c1 / kMaximumKeys) * KeyBedPixelWidth;
      KeyZone[c1].Bounds.Right := (c1 / kMaximumKeys) * KeyBedPixelWidth + KeyPixelWidth;
    end;

  end else
  begin
    KeyPixelWidth     := ceil(self.Width / NumberOfKeysToShow);
    KeyBedPixelWidth  := KeyPixelWidth * kMaximumKeys;
    KeyBedPixelOffset := round((KeyBedPixelWidth - self.Width) * Offset);

    for c1 := 0 to kMaximumKeys-1 do
    begin
      KeyZone[c1].Bounds.Left  := c1       * KeyPixelWidth - KeyBedPixelOffset;
      KeyZone[c1].Bounds.Right := (c1 + 1) * KeyPixelWidth - KeyBedPixelOffset;
    end;
  end;


  //=== set key top and bottom bounds ===
  for c1 := 0 to kMaximumKeys-1 do
  begin
    KeyZone[c1].Bounds.Top    := 0;
    KeyZone[c1].Bounds.Bottom := Height;
  end;



  //========================
  // Adjust keys for pixel drawing reasons
  for c1 := 0 to kMaximumKeys-1 do
  begin
    KeyZone[c1].Bounds.Left := floor(KeyZone[c1].Bounds.Left) + 0.5;
  end;

  for c1 := 0 to kMaximumKeys-2 do
  begin
    KeyZone[c1].Bounds.Right := KeyZone[c1+1].Bounds.Left - 1;
  end;

  if fZoom = 0 then
  begin
    KeyZone[kMaximumKeys-1].Bounds.Right := Width - 0.5;
  end else
  begin
    KeyZone[kMaximumKeys-1].Bounds.Right := floor(KeyZone[kMaximumKeys-1].Bounds.Right) - 0.5;
  end;




  Invalidate;
end;

procedure TVamSampleMap.UpdateProposedRegions(DropPoint: TPoint; NewRegionCount:integer);
var
  RegionAtDropPoint : TVamSampleRegion;
begin
  if NewRegionCount = 0 then
  begin
    ProposedSampleRegions.Clear;
    exit; //===========================>> exit >>=============>>
  end;

  RegionAtDropPoint := GetRegionAt(DropPoint.X, DropPoint.Y);

  if not assigned(RegionAtDropPoint) then
  begin
    if NewRegionCount > 1
      then UpdateProposedRegions_MultiFileDrop(DropPoint, NewRegionCount)
      else UpdateProposedRegions_SingleFileDrop(DropPoint, NewRegionCount);

    ShowReplaceRegionMessage(false);
    MouseOverRegion := nil;
    MouseOverRegionHandle := rhNone;
    MouseOverRegionChanged(nil);
  end else
  begin
    UpdateProposedRegions_RegionReplace(DropPoint);

    ShowReplaceRegionMessage(true);
    MouseOverRegion := RegionAtDropPoint;
    MouseOverRegionHandle := rhNone;
    MouseOverRegionChanged(MouseOverRegion);
  end;
end;

procedure TVamSampleMap.UpdateProposedRegions_SingleFileDrop(DropPoint: TPoint; NewRegionCount: integer);
var
  sr : TVamSampleRegion;
  kp : TPoint;
  LowKey, HighKey, LowVelocity, HighVelocity : integer;
  AboveRegion : TVamSampleRegion;
  BelowRegion : TVamSampleRegion;
  LeftRegion, RightRegion : TVamSampleRegion;

  CurrentRegion : TVamSampleRegion;
begin
  assert(NewRegionCount = 1);

  //The samples are being dropped on an existing region. Replace that region instead.
  if ProposedSampleRegions.Count <> 1 then
  begin
    ProposedSampleRegions.Clear;
    sr := TVamSampleRegion.Create;
    ProposedSampleRegions.Add(sr);
  end;

  UpdateProposedRegions_MultiFileDrop(DropPoint, NewRegionCount);

  CurrentRegion := ProposedSampleRegions[0];


  kp := PixelToSampleMapPos(DropPoint);

  LowKey       := kp.X;
  HighKey      := kp.X;
  LowVelocity  := kp.Y;
  HighVelocity := kp.Y;

  AboveRegion := FindRegionAbove(LowKey, HighKey, LowVelocity, HighVelocity);
  BelowRegion := FindRegionBelow(LowKey, HighKey, LowVelocity, HighVelocity);


  if (assigned(AboveRegion)) and (not assigned(BelowRegion)) then
  begin
    CurrentRegion.LowKey       := AboveRegion.LowKey;
    CurrentRegion.HighKey      := AboveRegion.HighKey;
    CurrentRegion.LowVelocity  := 0;
    CurrentRegion.HighVelocity := AboveRegion.LowVelocity - 1;
  end else
  if (not assigned(AboveRegion)) and (assigned(BelowRegion)) then
  begin
    CurrentRegion.LowKey       := BelowRegion.LowKey;
    CurrentRegion.HighKey      := BelowRegion.HighKey;
    CurrentRegion.LowVelocity  := BelowRegion.HighVelocity + 1;
    CurrentRegion.HighVelocity := 127;
  end else
  if (assigned(AboveRegion)) and (assigned(BelowRegion)) then
  begin
    CurrentRegion.LowKey  := Max(BelowRegion.LowKey, AboveRegion.LowKey);
    CurrentRegion.HighKey := Min(BelowRegion.HighKey, AboveRegion.HighKey);
    CurrentRegion.LowVelocity  := BelowRegion.HighVelocity + 1;
    CurrentRegion.HighVelocity := AboveRegion.LowVelocity  - 1;
  end;


  LowKey       := kp.X;
  HighKey      := kp.X;
  LowVelocity  := CurrentRegion.LowVelocity;
  HighVelocity := CurrentRegion.HighVelocity;


  LeftRegion  := FindRegionLeft(LowKey, HighKey, LowVelocity, HighVelocity);
  RightRegion := FindRegionRight(LowKey, HighKey, LowVelocity, HighVelocity);

  if assigned(LeftRegion) then
  begin
    if CurrentRegion.LowKey <= LeftRegion.HighKey
      then CurrentRegion.LowKey := LeftRegion.HighKey + 1;
  end;

  if assigned(RightRegion) then
  begin
    if CurrentRegion.HighKey >= RightRegion.LowKey
      then CurrentRegion.HighKey := RightRegion.LowKey - 1;
  end;


end;

procedure TVamSampleMap.UpdateProposedRegions_MultiFileDrop(DropPoint: TPoint; NewRegionCount: integer);
var
  c1:integer;
  sr : TVamSampleRegion;
  kp : TPoint;
  KeyWidth : integer;
  NextLowVelocity : integer;
begin
  // Check if the number of sample regions needs to be updated.
  if ProposedSampleRegions.Count <> NewRegionCount then
  begin
    ProposedSampleRegions.Clear;
    for c1 := 0 to NewRegionCount-1 do
    begin
      sr := TVamSampleRegion.Create;
      ProposedSampleRegions.Add(sr);
    end;
  end;

  // Update the sample region positions.
  kp := PixelToSampleMapPos(DropPoint);
  KeyWidth := CalcProposedSampleRegionKeyWidth(DropPoint);

  if KeyWidth = 128 then
  begin
    ProposedMapInfo.IsFullKeyboardSpread := true;

    for c1 := 0 to NewRegionCount-1 do
    begin
      sr := ProposedSampleRegions[c1];

      sr.LowVelocity := 0;
      sr.HighVelocity := 127;
      sr.LowKey := 0;
      sr.HighKey := 127;
      sr.RootNote := 60; //MIDI c4.
    end;
  end;

  if KeyWidth = 0 then
  begin
    ProposedMapInfo.IsFullKeyboardSpread := false;

    NextLowVelocity := 0;
    for c1 := 0 to NewRegionCount-1 do
    begin
      sr := ProposedSampleRegions[c1];

      sr.LowVelocity  := NextLowVelocity;
      sr.HighVelocity := round(128 / NewRegionCount * (c1 + 1));
      if sr.HighVelocity > 127 then sr.HighVelocity := 127;

      sr.LowKey   := kp.X;
      sr.HighKey  := kp.X;
      sr.RootNote := kp.X;

      NextLowVelocity := sr.HighVelocity + 1;
    end;
  end else
  if (KeyWidth >= 1) and (KeyWidth <= 12) then
  begin
    ProposedMapInfo.IsFullKeyboardSpread := false;

    kp.X := floor(kp.X / KeyWidth) * KeyWidth;

    for c1 := 0 to NewRegionCount-1 do
    begin
      sr := ProposedSampleRegions[c1];

      sr.LowVelocity  := 0;
      sr.HighVelocity := 127;
      sr.LowKey       := kp.X;
      sr.HighKey      := kp.X + (KeyWidth - 1);
      sr.RootNote     := kp.X;

      if sr.LowKey  > 127 then sr.LowKey  := 127;
      if sr.HighKey > 127 then sr.HighKey := 127;

      kp.X := kp.X + KeyWidth;


    end;
  end;
end;

procedure TVamSampleMap.UpdateProposedRegions_RegionReplace(DropPoint: TPoint);
var
  sr : TVamSampleRegion;
  RegionAtDropPoint : TVamSampleRegion;
begin
  RegionAtDropPoint := GetRegionAt(DropPoint.X, DropPoint.Y);
  assert(assigned(RegionAtDropPoint));

  //The samples are being dropped on an existing region. Replace that region instead.
  if ProposedSampleRegions.Count <> 1 then
  begin
    ProposedSampleRegions.Clear;
    sr := TVamSampleRegion.Create;
    ProposedSampleRegions.Add(sr);
  end;

  sr := ProposedSampleRegions[0];
  sr.RootNote     := RegionAtDropPoint.RootNote;
  sr.LowKey       := RegionAtDropPoint.LowKey;
  sr.HighKey      := RegionAtDropPoint.HighKey;
  sr.LowVelocity  := RegionAtDropPoint.LowVelocity;
  sr.HighVelocity := RegionAtDropPoint.HighVelocity;



end;

function TVamSampleMap.CalcProposedSampleRegionKeyWidth(aPoint: TPoint): integer;
var
  Index : integer;
begin
  Index := floor((aPoint.Y / Height) * 9);
  if Index < 0 then Index := 0;
  if Index > 8 then Index := 8;

  case Index of
    0: result := 128;
    1: result := 128;
    2: result := 12;
    3: result := 12;
    4: result := 12;
    5: result := 6;
    6: result := 3;
    7: result := 1; //special result to say 'Velocity Stack' the new key zones.
    8: result := 0; //special result to say 'Velocity Stack' the new key zones.
  else
    result := 1;
  end;
end;




function TVamSampleMap.CalcSampleRegionBounds(aSampleRegion: TVamSampleRegion; const CalcMovingBounds : boolean = false): TRectF;
var
  x1, y1, x2, y2 : single;
  LowKey, HighKey, LowVelocity, HighVelocity : integer;
begin
  if CalcMovingBounds = false then
  begin
    LowKey       := aSampleRegion.LowKey;
    HighKey      := aSampleRegion.HighKey;
    LowVelocity  := aSampleRegion.LowVelocity;
    HighVelocity := aSampleRegion.HighVelocity;
  end else
  begin
    LowKey       := aSampleRegion.MovedLowKey;
    HighKey      := aSampleRegion.MovedHighKey;
    LowVelocity  := aSampleRegion.MovedLowVelocity;
    HighVelocity := aSampleRegion.MovedHighVelocity;
  end;

  x1 := KeyZone[LowKey].Bounds.Left;
  x2 := KeyZone[HighKey].Bounds.Right;

  y1 := (1 - ((HighVelocity + 1) / 128)) * (Height);
  y2 := (1 - (LowVelocity        / 128)) * (Height);
  y1 := round(y1) + 0.5;
  y2 := round(y2) - 0.5;

  result := RectF(x1, y1, x2, y2);
end;

function TVamSampleMap.PixelToSampleMapPos(Pixel: TPoint): TPoint;
var
  KeyIndex, VelocityIndex : integer;
begin
  KeyIndex := floor((Pixel.X + KeyBedPixelOffset) / KeyBedPixelWidth * 128);
  if KeyIndex > 127 then KeyIndex := 127;
  //if KeyIndex < 0   then KeyIndex := 0;

  VelocityIndex := floor((1 - (Pixel.Y / Height)) * 128);
  if VelocityIndex > 127 then VelocityIndex := 127;
  //if VelocityIndex < 0   then VelocityIndex := 0;

  result := Point(KeyIndex, VelocityIndex);
end;

procedure TVamSampleMap.OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  NewRegionCount : integer;
begin
  NewRegionCount := GetDragRegionCount(Data);
  UpdateProposedRegions(aPoint, NewRegionCount);
  Invalidate;
  RegionInfoChanged;

  if assigned(OnOleDragEnter)
    then OnOleDragEnter(Sender, ShiftState, aPoint, Effect, Data);
end;

procedure TVamSampleMap.OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  NewRegionCount : integer;
begin
  NewRegionCount := GetDragRegionCount(Data);
  UpdateProposedRegions(aPoint, NewRegionCount);
  Invalidate;
  RegionInfoChanged;
end;

procedure TVamSampleMap.OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  NewRegionCount : integer;
  RegionAtDropPoint : TVamSampleRegion;
begin
  NewRegionCount := GetDragRegionCount(Data);
  UpdateProposedRegions(aPoint, NewRegionCount);

  if (ProposedSampleRegions.Count > 0) then
  begin
    RegionAtDropPoint := GetRegionAt(aPoint.X, aPoint.Y);

    //TODO: The OnNewRegions and OnReplaceRegions should send a file listing here, not the Data variable.
    if (not assigned(RegionAtDropPoint)) then
    begin
      if assigned(OnNewRegions)
        then OnNewRegions(self, ProposedSampleRegions, Data);
    end else
    begin
      if assigned(OnReplaceRegion)
        then OnReplaceRegion(self, ProposedSampleRegions[0], RegionAtDropPoint, Data);
    end;

    ProposedSampleRegions.Clear;
  end;

  Invalidate;
  RegionInfoChanged;
  ShowReplaceRegionMessage(false);
end;

procedure TVamSampleMap.OleDragLeave(Sender: TObject);
begin
  if ProposedSampleRegions.Count > 0 then ProposedSampleRegions.Clear;
  Invalidate;
  RegionInfoChanged;
  ShowReplaceRegionMessage(false);
end;

function TVamSampleMap.FindRegionAbove(LowKey, HighKey, LowVelocity, HighVelocity: integer): TVamSampleRegion;
var
  c1 : integer;
  TempRegionList : TVamSampleRegionList;
  aRegion : TVamSampleRegion;
begin
  TempRegionList := TVamSampleRegionList.Create(false);
  AutoFree(@TempRegionList);

  for c1 := 0 to SampleRegions.Count-1 do
  begin
    aRegion := SampleRegions[c1];
    if (aRegion.LowKey <= HighKey) and (aRegion.HighKey >= LowKey) and (aRegion.LowVelocity > HighVelocity)
      then TempRegionList.Add(aRegion);
  end;

  if TempRegionList.Count = 0 then exit(nil);
  if TempRegionList.Count = 1 then exit(TempRegionList[0]);

  aRegion := TempRegionList[0];
  for c1 := 0 to TempRegionList.Count-1 do
  begin
    if aRegion.LowVelocity > TempRegionList[c1].LowVelocity
      then aRegion := TempRegionList[c1];
  end;

  result := aRegion;
end;

function TVamSampleMap.FindRegionBelow(LowKey, HighKey, LowVelocity, HighVelocity: integer): TVamSampleRegion;
var
  c1 : integer;
  TempRegionList : TVamSampleRegionList;
  aRegion : TVamSampleRegion;
begin
  TempRegionList := TVamSampleRegionList.Create(false);
  AutoFree(@TempRegionList);

  for c1 := 0 to SampleRegions.Count-1 do
  begin
    aRegion := SampleRegions[c1];
    if (aRegion.LowKey <= HighKey) and (aRegion.HighKey >= LowKey) and (aRegion.HighVelocity < LowVelocity)
      then TempRegionList.Add(aRegion);
  end;

  if TempRegionList.Count = 0 then exit(nil);
  if TempRegionList.Count = 1 then exit(TempRegionList[0]);

  aRegion := TempRegionList[0];
  for c1 := 0 to TempRegionList.Count-1 do
  begin
    if aRegion.HighVelocity < TempRegionList[c1].HighVelocity
      then aRegion := TempRegionList[c1];
  end;

  result := aRegion;
end;


function TVamSampleMap.FindRegionLeft(LowKey, HighKey, LowVelocity, HighVelocity: integer): TVamSampleRegion;
var
  c1 : integer;
  TempRegionList : TVamSampleRegionList;
  aRegion : TVamSampleRegion;
begin
  TempRegionList := TVamSampleRegionList.Create(false);
  AutoFree(@TempRegionList);

  for c1 := 0 to SampleRegions.Count-1 do
  begin
    aRegion := SampleRegions[c1];
    if (aRegion.LowVelocity <= HighVelocity) and (aRegion.HighVelocity >= LowVelocity) and (aRegion.HighKey < LowKey)
      then TempRegionList.Add(aRegion);
  end;

  if TempRegionList.Count = 0 then exit(nil);
  if TempRegionList.Count = 1 then exit(TempRegionList[0]);

  aRegion := TempRegionList[0];
  for c1 := 0 to TempRegionList.Count-1 do
  begin
    if aRegion.HighKey < TempRegionList[c1].HighKey
      then aRegion := TempRegionList[c1];
  end;

  result := aRegion;
end;


function TVamSampleMap.FindRegionRight(LowKey, HighKey, LowVelocity, HighVelocity: integer): TVamSampleRegion;
var
  c1 : integer;
  TempRegionList : TVamSampleRegionList;
  aRegion : TVamSampleRegion;
begin
  TempRegionList := TVamSampleRegionList.Create(false);
  AutoFree(@TempRegionList);

  for c1 := 0 to SampleRegions.Count-1 do
  begin
    aRegion := SampleRegions[c1];
    if (aRegion.LowVelocity <= HighVelocity) and (aRegion.HighVelocity >= LowVelocity) and (aRegion.LowKey > HighKey)
      then TempRegionList.Add(aRegion);
  end;

  if TempRegionList.Count = 0 then exit(nil);
  if TempRegionList.Count = 1 then exit(TempRegionList[0]);

  aRegion := TempRegionList[0];
  for c1 := 0 to TempRegionList.Count-1 do
  begin
    if aRegion.LowKey > TempRegionList[c1].LowKey
      then aRegion := TempRegionList[c1];
  end;

  result := aRegion;
end;



function TVamSampleMap.FindRegionByUniqueID(UniqueID: TGUID): TVamSampleRegion;
var
  c1: Integer;
begin
  result := nil;
  for c1 := 0 to SampleRegions.Count-1 do
  begin
    if (SampleRegions[c1].UniqueID = UniqueID) then
    begin
      result := SampleRegions[c1];
      exit; //============================>> exit >>======>>
    end;
  end;
end;

function TVamSampleMap.GetRegionHandleAt(PixelPosX, PixelPosY: integer): TRegionHandleID;
const
  HandleSize = 10;
var
  aRegion : TVamSampleRegion;
  SampleRegionBounds : TRectF;

  Points     : array[0..7] of TPointF;
  PointDistX : array[0..7] of single;
  PointDistY : array[0..7] of single;
  w2, h2 : single;
  c1: Integer;

  CurIndex  : integer;
  CurDistX  : single;
  CurDistY  : single;

  IsSelectable : boolean;
begin
  result := rhNone;

  aRegion := GetRegionAt(PixelPosX, PixelPosY);


  if assigned(aRegion) then
  begin
    SampleRegionBounds := CalcSampleRegionBounds(aRegion);

    w2 := SampleRegionBounds.Width  / 2;
    h2 := SampleRegionBounds.Height / 2;

    Points[0] := PointF(SampleRegionBounds.Left,      SampleRegionBounds.Top);
    Points[1] := PointF(SampleRegionBounds.Left + w2, SampleRegionBounds.Top);
    Points[2] := PointF(SampleRegionBounds.Right,     SampleRegionBounds.Top);
    Points[3] := PointF(SampleRegionBounds.Left,      SampleRegionBounds.Top + h2);
    Points[4] := PointF(SampleRegionBounds.Right,     SampleRegionBounds.Top + h2);
    Points[5] := PointF(SampleRegionBounds.Left,      SampleRegionBounds.Bottom);
    Points[6] := PointF(SampleRegionBounds.Left + w2, SampleRegionBounds.Bottom);
    Points[7] := PointF(SampleRegionBounds.Right,     SampleRegionBounds.Bottom);

    for c1 := 0 to 7 do
    begin
      PointDistX[c1] := abs(Points[c1].X - PixelPosX);
      PointDistY[c1] := abs(Points[c1].Y - PixelPosY);
    end;

    CurIndex := 0;
    CurDistX  := PointDistX[0];
    CurDistY  := PointDistY[0];

    for c1 := 0 to 7 do
    begin
      IsSelectable := true;
      if (c1 = 1) and (SampleRegionBounds.Width <= 10) then IsSelectable := false;
      if (c1 = 6) and (SampleRegionBounds.Width <= 10) then IsSelectable := false;

      if (IsSelectable) and (CurDistX >= PointDistX[c1]) and (CurDistY >= PointDistY[c1]) then
      begin
        CurIndex := c1;
        CurDistX := PointDistX[CurIndex];
        CurDistY := PointDistY[CurIndex];
      end;

    end;

    if (PointDistX[CurIndex] > HandleSize) or (PointDistY[CurIndex] > HandleSize) then
    begin
      result := rhNone;
    end else
    begin
      case CurIndex of
      0 : result := TRegionHandleID.rhTopLeft;
      1 : result := TRegionHandleID.rhTop;
      2 : result := TRegionHandleID.rhTopRight;
      3 : result := TRegionHandleID.rhLeft;
      4 : result := TRegionHandleID.rhRight;
      5 : result := TRegionHandleID.rhBottomLeft;
      6 : result := TRegionHandleID.rhBottom;
      7 : result := TRegionHandleID.rhBottomRight;
      else
        result := rhNone;
      end;
    end;
  end;
end;

function TVamSampleMap.GetRegionAt(PixelPosX, PixelPosY: integer): TVamSampleRegion;
var
  c1: Integer;
  SampleRegionBounds : TRectF;
begin
  result := nil;
  for c1 := 0 to SampleRegions.Count-1 do
  begin
    SampleRegionBounds := CalcSampleRegionBounds(SampleRegions[c1]);
    if (SampleRegions[c1].IsVisible) and (InRect(PixelPosX, PixelPosY, SampleRegionBounds)) then
    begin
      result := SampleRegions[c1];
      exit; //=========================>> exit >>============>>
    end;
  end;
end;


procedure TVamSampleMap.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  c1 : integer;
begin
  inherited;

  IsGrabbedByLeft    := false;
  IsGrabbedByRight   := false;
  IsDragSelectActive := false;
  IsCopyRegionActive := false;

  if (Button = mbLeft) then
  begin
    LastDragSelectIndex := -1;
    IsGrabbedByLeft   := true;
    MouseDownPos      := Point(x, y);
    WatchForDrag      := true;
    IsDragActive      := false;
    DeselectOthersOnMouseUp := false;
    MouseDownRegion  := GetRegionAt(X, Y);
    MouseDownRegionHandle := GetRegionHandleAt(X, Y);
    OriginKey      := PixelToSampleMapPos(Point(x, y)).X;
    OriginVelocity := PixelToSampleMapPos(Point(x, y)).Y;
    LastDistKey      := 0;
    LastDistVelocity := 0;

    if assigned(MouseDownRegion) then
    begin
      //Move the clicked region to the front
      MoveRegionToFront(MouseDownRegion);


      //=== Logic for mouse down on a region with no modifier keys ===
      if not((ssCtrl in Shift) {or (ssAlt in Shift)} or (ssShift in Shift)) then
      begin
        if (MouseDownRegion.IsSelected) and (MouseDownRegion.IsFocused) and (MouseDownRegionHandle <> rhNone) then
        begin
          DeselectOthersOnMouseUp := true;
        end
        else if (MouseDownRegion.IsSelected) then
        begin
          DeselectOthersOnMouseUp := true;
          if assigned(OnFocusRegion) then OnFocusRegion(self, MouseDownRegion);
        end
        else if (MouseDownRegion.IsSelected = false) then
        begin
          if assigned(OnFocusRegion)  then OnFocusRegion(self, MouseDownRegion);
          if assigned(OnSelectRegion) then OnSelectRegion(self, MouseDownRegion);
          if assigned(OnDeselectOtherRegions) then OnDeselectOtherRegions(self, MouseDownRegion);
        end;
      end;


      //=== Logic for mouse down on a region with Ctrl key held ===
      if (ssCtrl in Shift)
        and (not(ssAlt   in Shift))
        and (not(ssShift in Shift)) then
      begin
        if MouseDownRegion.IsSelected then
        begin
          if assigned(OnDeselectRegion) then OnDeselectRegion(self, MouseDownRegion)
        end else
        begin
          if assigned(OnSelectRegion) then OnSelectRegion(self, MouseDownRegion);
        end;
      end;

      //=== logic for copying a selected region (with the [ALT] key held) ===
      if (ssAlt in Shift)
        and (MouseDownRegion.IsSelected = true)
        and (not(ssCtrl  in Shift))
        and (not(ssShift in Shift)) then
      begin
        IsCopyRegionActive := true;
      end;


    end;

    if not assigned(MouseDownRegion) then
    begin
      // Logic for clicking an area with no sample regions.
      if not((ssCtrl in Shift) or (ssAlt in Shift) or (ssShift in Shift)) then
      begin
        IsDragSelectActive := true;
        DragSelectRect.Left   := X;
        DragSelectRect.Right  := X;
        DragSelectRect.Top    := Y;
        DragSelectRect.Bottom := Y;

        for c1 := 0 to SampleRegions.Count-1 do
        begin
          SampleRegions[c1].IsDragSelected := false;
        end;


        if assigned(OnFocusRegion)        then OnFocusRegion(self, nil);
        if assigned(OnDeselectAllRegions) then OnDeselectAllRegions(self);
        if assigned(OnDragSelectStart)    then OnDragSelectStart(self);

      end;
    end;
  end;

  if (Button = mbRight) then
  begin
    IsGrabbedByRight := true;
    MouseDownRegion  := GetRegionAt(X, Y);
    if assigned(MouseDownRegion) then
    begin
      MoveRegionToFront(MouseDownRegion);

      if (MouseDownRegion.IsSelected) and (MouseDownRegion.IsFocused = false) then
      begin
        if assigned(OnFocusRegion) then OnFocusRegion(self, MouseDownRegion);
      end else
      if (MouseDownRegion.IsSelected = false) then
      begin
        if assigned(OnFocusRegion)  then OnFocusRegion(self, MouseDownRegion);
        if assigned(OnSelectRegion) then OnSelectRegion(self, MouseDownRegion);
        if assigned(OnDeselectOtherRegions) then OnDeselectOtherRegions(self, MouseDownRegion);
      end;
    end;
  end;

  RegionInfoChanged;
end;



procedure TVamSampleMap.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  IsSnapping : boolean;
  MapDragSelect:TRect;
  SampleRect : TRect;
  aRegion : TVamSampleRegion;
  aHandle : TRegionHandleID;
  CurKey, CurVelocity : integer;
  DistKey, DistVelocity : integer;
  c1: Integer;
  HasDragSelectionChanged : boolean;
begin
  inherited;

  if ssShift in Shift
    then IsSnapping := false
    else IsSnapping := true;

  if (DeselectOthersOnMouseUp) and (MouseDownPos.X <> X) and (MouseDownPos.Y <> Y) then
  begin
    DeselectOthersOnMouseUp := false;
  end;


  if (IsGrabbedByLeft) and (assigned(MouseDownRegion)) and (MouseDownRegionHandle = rhNone) then
  begin
    CurKey      := PixelToSampleMapPos(Point(x, y)).X;
    CurVelocity := PixelToSampleMapPos(Point(x, y)).Y;

    DistKey      := CurKey - OriginKey;
    DistVelocity := CurVelocity - OriginVelocity;

    if (abs(DistKey) > abs(DistVelocity)) and (ssCtrl in Shift) then DistVelocity := 0;
    if (abs(DistVelocity) > abs(DistKey)) and (ssCtrl in Shift) then DistKey      := 0;

    if (DistKey <> LastDistKey) or (DistVelocity <> LastDistVelocity) then
    begin
      LastDistKey := DistKey;
      LastDistVelocity := DistVelocity;

      MoveSelectedRegions(MouseDownRegion, SampleRegions, DistKey, DistVelocity, IsSnapping);

      Invalidate;
      RegionInfoChanged;
    end;

  end;


  if (IsGrabbedByLeft) and (assigned(MouseDownRegion)) and (MouseDownRegionHandle <> rhNone) then
  begin
    CurKey      := PixelToSampleMapPos(Point(x, y)).X;
    CurVelocity := PixelToSampleMapPos(Point(x, y)).Y;

    DistKey      := CurKey - OriginKey;
    DistVelocity := CurVelocity - OriginVelocity;

    if (abs(DistKey) > abs(DistVelocity)) and (ssCtrl in Shift) then DistVelocity := 0;
    if (abs(DistVelocity) > abs(DistKey)) and (ssCtrl in Shift) then DistKey      := 0;

    if (DistKey <> LastDistKey) or (DistVelocity <> LastDistVelocity) then
    begin
      LastDistKey := DistKey;
      LastDistVelocity := DistVelocity;

      ResizeSelectedRegions(MouseDownRegion, SampleRegions, DistKey, DistVelocity, MouseDownRegionHandle, IsSnapping);

      Invalidate;
      RegionInfoChanged;
    end;
  end;


  if (IsGrabbedByLeft) and (IsDragSelectActive) then
  begin
    if MouseDownPos.X < X then
    begin
      DragSelectRect.Left  := MouseDownPos.X;
      DragSelectRect.Right := X;
    end else
    begin
      DragSelectRect.Left  := X;
      DragSelectRect.Right := MouseDownPos.X;
    end;

    if MouseDownPos.Y < Y then
    begin
      DragSelectRect.Top    := Y;
      DragSelectRect.Bottom := MouseDownPos.Y;
    end else
    begin
      DragSelectRect.Top    := MouseDownPos.Y;
      DragSelectRect.Bottom := Y;
    end;

    HasDragSelectionChanged := false;

    for c1 := 0 to SampleRegions.Count-1 do
    begin
      MapDragSelect.TopLeft     := PixelToSampleMapPos(DragSelectRect.TopLeft);
      MapDragSelect.BottomRight := PixelToSampleMapPos(DragSelectRect.BottomRight);

      SampleRect.Left   := SampleRegions[c1].LowKey;
      SampleRect.Top    := SampleRegions[c1].HighVelocity;
      SampleRect.Right  := SampleRegions[c1].HighKey;
      SampleRect.Bottom := SampleRegions[c1].LowVelocity;

      if (SampleRegions[c1].IsVisible) and (Intersects(MapDragSelect, SampleRect)) then
      begin
        if SampleRegions[c1].IsDragSelected = false
          then LastDragSelectIndex := c1;

        if SampleRegions[c1].IsDragSelected <> true then
        begin
          SampleRegions[c1].IsDragSelected := true;
          HasDragSelectionChanged := true;
        end;
      end else
      begin
        if SampleRegions[c1].IsDragSelected <> false then
        begin
          SampleRegions[c1].IsDragSelected := false;
          HasDragSelectionChanged := true;
        end;
      end;
    end;

    Invalidate;

    if (HasDragSelectionChanged) and (assigned(OnDragSelectionChanged)) then
    begin
      OnDragSelectionChanged(Self);
    end;
  end;




  if not(IsGrabbedByLeft or IsGrabbedByRight) then
  begin
    aRegion := GetRegionAt(X, Y);
    if aRegion <> MouseOverRegion then
    begin
      MouseOverRegion := aRegion;
      Invalidate;
      RegionInfoChanged;
      MouseOverRegionChanged(MouseOverRegion);
    end;

    aHandle := GetRegionHandleAt(X, Y);
    if aHandle <> MouseOverRegionHandle then
    begin
      MouseOverRegionHandle := aHandle;
      Invalidate;
    end;

    UpdateCursorIcon(Shift, X, Y);
  end;


end;

procedure TVamSampleMap.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MouseUpRegion : TVamSampleRegion;
  aRegion : TVamSampleRegion;
  c1: Integer;
  FocusIndex : integer;
begin
  inherited;


  if (Button = mbLeft) and (IsDragSelectActive) then
  begin
    FocusIndex := -1;

    if (LastDragSelectIndex <> -1) and (SampleRegions[LastDragSelectIndex].IsDragSelected) then
    begin
      FocusIndex := LastDragSelectIndex;
    end;

    if FocusIndex = -1 then
    begin
      MouseUpRegion  := GetRegionAt(X, Y);
      if assigned(MouseUpRegion) then
      begin
        FocusIndex := SampleRegions.IndexOf(MouseUpRegion);
      end;
    end;

    if FocusIndex = -1 then
    begin
      for c1 := 0 to SampleRegions.Count-1 do
      begin
        if SampleRegions[c1].IsDragSelected then
        begin
          FocusIndex := c1;
          break;
        end;
      end;
    end;

    for c1 := 0 to SampleRegions.Count-1 do
    begin
      if SampleRegions[c1].IsDragSelected then
      begin
        SampleRegions[c1].IsDragSelected := false;
        if assigned(OnSelectRegion) then OnSelectRegion(self, SampleRegions[c1]);
      end;
    end;

    if FocusIndex <> -1 then
    begin
      if assigned(OnFocusRegion)  then OnFocusRegion(self, SampleRegions[FocusIndex]);
    end;

    IsDragSelectActive := false;
    if assigned(OnDragSelectEnd)    then OnDragSelectEnd(self);
  end;


  if (Button = mbLeft) and (IsGrabbedByLeft) then
  begin
    IsGrabbedByLeft  := false;
    IsGrabbedByRight := false;

    //Finalise and moved regions.
    if not IsCopyRegionActive then
    begin
      for c1 := 0 to SampleRegions.Count-1 do
      begin
        if SampleRegions[c1].IsMoving then
        begin
          SampleRegions[c1].IsMoving := false;
          if assigned(OnRegionMoved) then OnRegionMoved(self, SampleRegions[c1]);
        end;
      end;
    end;

    //Finalise copied regions..
    if IsCopyRegionActive then
    begin
      PrepareCopiedRegions(SampleRegions, ProposedSampleRegions);

      if assigned(OnNewCopiedRegions) then OnNewCopiedRegions(self, ProposedSampleRegions);
      ProposedSampleRegions.Clear;
    end;


    if DeselectOthersOnMouseUp then
    begin
      DeselectOtherRegions(MouseDownRegion);
      if assigned(OnDeselectOtherRegions) then OnDeselectOtherRegions(self, MouseDownRegion);
    end;

    IsCopyRegionActive := false;
  end;


  if (Button = mbRight) and (IsGrabbedByRight) then
  begin
    IsGrabbedByLeft  := false;
    IsGrabbedByRight := false;

    aRegion  := GetRegionAt(X, Y);

    if assigned(aRegion) then
    begin
      if assigned(OnShowRegionContextMenu) then OnShowRegionContextMenu(self, aRegion);
    end;
  end;



  if (Button = mbLeft) and (IsGrabbedByLeft) then
  begin
    MouseDownRegion := nil;

    // Cancel region movement for existing regions.
    for c1 := 0 to SampleRegions.Count-1 do
    begin
      if SampleRegions[c1].IsMoving then
      begin
        SampleRegions[c1].IsMoving := false;
      end;
    end;
  end;

  if (Button = mbRight) and (IsGrabbedByRight) then
  begin
    MouseDownRegion := nil;
  end;

  if (IsGrabbedByLeft = false) and (IsGrabbedByRight = false) then
  begin
    UpdateCursorIcon(Shift, X, Y);
  end;

  Invalidate;
  RegionInfoChanged;

  SortSampleRegionList;
end;

procedure TVamSampleMap.MouseEnter;
begin
  inherited;
  RegionInfoChanged;
end;

procedure TVamSampleMap.MouseLeave;
var
  c1 : integer;
begin
  inherited;
  IsGrabbedByLeft  := false;
  IsGrabbedByRight := false;

  for c1 := SampleRegions.Count-1 downto 0 do
  begin
    SampleRegions[c1].IsMoving := false;
  end;

  if MouseOverRegion <> nil then
  begin
    MouseOverRegion := nil;
    MouseOverRegionHandle := rhNone;
    MouseOverRegionChanged(nil);
  end;

  Invalidate;
  RegionInfoChanged;

  Cursor := crDefault;
end;


procedure TVamSampleMap.MoveRegionToFront(aRegion: TVamSampleRegion);
var
  Index1 : integer;
begin
  Index1 := SampleRegions.IndexOf(aRegion);
  if Index1 > 0 then
  begin
    SampleRegions.Move(Index1, 0);
    Invalidate;
  end;
end;



procedure TVamSampleMap.Paint;
const
  kNumberOfHorizontalLines = 9;
  ShowOtherRegions = false;
var
  c1: Integer;
  x1,y1:single;
  Opacity : byte;
  frac : single;
  //SampleRegionBounds : TRectF;
  HandleBounds : TRectF;
  aColor : TRedFoxColor;
  ErrorColor : TRedFoxColor;
begin
  inherited;

  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  BackBuffer.BufferInterface.ClearAll(TRedFoxColor(Color_Background));

  //Draw octave divisions.
  frac := (1 - NumberOfKeysToShow / kMaximumKeys);
  Opacity := round(120 * frac) + 80;
  BackBuffer.BufferInterface.LineColor := TRedFoxColor(Color_BackgroundLines).WithAlpha(Opacity);
  BackBuffer.BufferInterface.LineWidth := 1;
  for c1 := 0 to kMaximumKeys-1 do
  begin
    if (c1 mod 12 = 0) then
    begin
      x1 := KeyZone[c1].Bounds.Left;
      BackBuffer.BufferInterface.Line(x1, 0, x1, Height);
    end;
  end;

  //draw inbetween keys
  frac := (1 - NumberOfKeysToShow / kMaximumKeys);
  Opacity := round(70 * frac);
  BackBuffer.BufferInterface.LineColor := TRedFoxColor(Color_BackgroundLines).WithAlpha(Opacity);
  BackBuffer.BufferInterface.LineWidth := 1;
  for c1 := 0 to kMaximumKeys-1 do
  begin
    if (c1 mod 12 <> 0) then
    begin
      x1 := KeyZone[c1].Bounds.Left;
      BackBuffer.BufferInterface.Line(x1, 0, x1, Height);
    end;
  end;


  //draw horizontal lines.
  frac := (1 - NumberOfKeysToShow / kMaximumKeys);
  Opacity := round(10 * frac) + 42;
  BackBuffer.BufferInterface.LineColor := TRedFoxColor(Color_BackgroundLines).WithAlpha(Opacity);
  BackBuffer.BufferInterface.LineWidth := 1;
  for c1 := 0 to kNumberOfHorizontalLines-1 do
  begin
    y1 := round((c1 / (kNumberOfHorizontalLines-1)) * (height-1)) + 0.5;
    BackBuffer.BufferInterface.Line(0,y1, Width, y1);
  end;



  //==== draw sample regions ==============
  for c1 := SampleRegions.Count-1 downto 0 do
  begin
    if ((SampleRegions[c1].IsVisible) or (ShowOtherRegions)) and (SampleRegions[c1].IsMoving = false)then
    begin
      if SampleRegions[c1].IsInOtherKeyGroup = false
        then aColor := Color_Region
        else aColor := Color_OtherKeyGroup;

      if (SampleRegions[c1].IsDragSelected)
        or (SampleRegions[c1].IsFocused)
        or (SampleRegions[c1].IsSelected)
        or (SampleRegions[c1] = MouseOverRegion)
        then aColor := Color_RegionFocused;

      if SampleRegions[c1].IsSampleError then
      begin
        ErrorColor := Color_RegionError;
        ErrorColor := ColorFadeF(aColor, ErrorColor, 0.3);
        ErrorColor.A := aColor.A;
        aColor := ErrorColor;
      end;

      DrawSampleRegion(SampleRegions[c1], aColor, false);
    end;
  end;


  // Draw the regions being copied at their original position.
  if IsCopyRegionActive then
  begin
    for c1 := SampleRegions.Count-1 downto 0 do
    begin
      if ((SampleRegions[c1].IsVisible) or (ShowOtherRegions)) and (SampleRegions[c1].IsMoving = true)then
      begin
        if SampleRegions[c1].IsInOtherKeyGroup = false
          then aColor := Color_Region
          else aColor := Color_OtherKeyGroup;

        if SampleRegions[c1].IsSampleError then
        begin
          ErrorColor := Color_RegionError;
          ErrorColor := ColorFadeF(aColor, ErrorColor, 0.3);
          ErrorColor.A := aColor.A;
          aColor := ErrorColor;
        end;

        DrawSampleRegion(SampleRegions[c1], aColor, false);
      end;
    end;
  end;




  // Draw moving regions... (includes copied regions destination position)
  for c1 := SampleRegions.Count-1 downto 0 do
  begin
    if ((SampleRegions[c1].IsVisible) or (ShowOtherRegions)) and (SampleRegions[c1].IsMoving = true)then
    begin
      if SampleRegions[c1].IsInOtherKeyGroup = false
        then aColor := Color_RegionFocused
        else aColor := Color_OtherKeyGroupSelected;

      if IsCopyRegionActive
        then aColor := Color_RegionFocused;

      if SampleRegions[c1].IsSampleError then
      begin
        ErrorColor := Color_RegionError;
        ErrorColor := ColorFadeF(aColor, ErrorColor, 0.3);
        ErrorColor.A := aColor.A;
        aColor := ErrorColor;
      end;

      DrawSampleRegion(SampleRegions[c1], aColor, true);
    end;
  end;





  {
  for c1 := SampleRegions.Count-1 downto 0 do
  begin
    if (SampleRegions[c1].IsDragSelected = false)
      and (SampleRegions[c1].IsFocused = false)
      and (SampleRegions[c1].IsSelected = false)
      and (SampleRegions[c1] <> MouseOverRegion)
      then
    begin
      if SampleRegions[c1].IsInOtherKeyGroup = false
        then aColor := Color_Region
        else aColor := Color_OtherKeyGroup;

      if SampleRegions[c1].IsSampleError then
      begin
        ErrorColor := Color_RegionError;
        ErrorColor := ColorFadeF(aColor, ErrorColor, 0.3);
        ErrorColor.A := aColor.A;
        aColor := ErrorColor;
      end;

      if (SampleRegions[c1].IsVisible) or (ShowOtherRegions) then
      begin
        DrawSampleRegion(SampleRegions[c1], aColor);
      end;
    end;
  end;

  for c1 := SampleRegions.Count-1 downto 0 do
  begin
    if ((SampleRegions[c1].IsDragSelected = true) or (SampleRegions[c1].IsFocused = true) or (SampleRegions[c1].IsSelected = true))
      and (SampleRegions[c1] <> MouseOverRegion)
      then
    begin
      if SampleRegions[c1].IsInOtherKeyGroup = false
        then aColor := Color_RegionFocused
        else aColor := Color_OtherKeyGroupSelected;

      if SampleRegions[c1].IsSampleError then
      begin
        ErrorColor := Color_RegionError;
        ErrorColor := ColorFadeF(aColor, ErrorColor, 0.7);
        ErrorColor.A := aColor.A;
        aColor := ErrorColor;
      end;

      if (IsCopyRegionActive) and (SampleRegions[c1].IsMoving)
        then aColor := Color_ProposedRegions;

      if (SampleRegions[c1].IsVisible) or (ShowOtherRegions) then
      begin
        DrawSampleRegion(SampleRegions[c1], aColor);
        DrawSampleRegionResizeHandles(SampleRegions[c1], aColor);
      end;
    end;
  end;
  }

  if assigned(MouseOverRegion) then
  begin
    if (MouseOverRegion.IsVisible) or (ShowOtherRegions) then
    begin
      aColor := Color_RegionFocused;

      if MouseOverRegion.IsMoving
        then DrawSampleRegionResizeHandles(MouseOverRegion, aColor, true)
        else DrawSampleRegionResizeHandles(MouseOverRegion, aColor, false);

      if (MouseOverRegionHandle <> rhNone) then
      begin
        BackBuffer.BufferInterface.FillColor := GetRedFoxColor('$FFFFFFFF').AsAggRgba8;
        BackBuffer.BufferInterface.NoLine;
        HandleBounds := CalcRegionHandleBounds(MouseOverRegion, MouseOverRegionHandle);
        BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);
      end;
    end;


    {
    if MouseOverRegion.IsInOtherKeyGroup = false
        then aColor := Color_RegionMouseOver
        else aColor := Color_OtherKeyGroupSelected;

    if MouseOverRegion.IsSampleError then
    begin
      ErrorColor := Color_RegionError;
      ErrorColor := ColorFadeF(aColor, ErrorColor, 0.8);
      ErrorColor.A := aColor.A;
      aColor := ErrorColor;
    end;

    if (MouseOverRegion.IsVisible) or (ShowOtherRegions) then
    begin
      DrawSampleRegion(MouseOverRegion, aColor, false);
      DrawSampleRegionResizeHandles(MouseOverRegion, aColor, false);

      if (MouseOverRegionHandle <> rhNone) then
      begin
        BackBuffer.BufferInterface.FillColor := GetRedFoxColor('$FFFFFFFF').AsAggRgba8;
        BackBuffer.BufferInterface.NoLine;
        HandleBounds := CalcRegionHandleBounds(MouseOverRegion, MouseOverRegionHandle);
        BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);
      end;
    end;
    }
  end;




  //==== draw the proposed sample regions ==============
  aColor := Color_ProposedRegions;
  if (ProposedSampleRegions.Count > 0) and (ProposedMapInfo.IsFullKeyboardSpread = false) then
  begin
    for c1 := 0 to ProposedSampleRegions.Count-1 do
    begin
      DrawSampleRegion(ProposedSampleRegions[c1], aColor, false);
    end;
  end else
  if (ProposedSampleRegions.Count > 0) and (ProposedMapInfo.IsFullKeyboardSpread = true) then
  begin
    DrawSampleRegion(ProposedSampleRegions.First, aColor, false);
  end;


  if IsDragSelectActive then
  begin
    BackBuffer.BufferInterface.FillColor := TRedFoxColor(Color_SelectionRect);
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.Rectangle(DragSelectRect.Left, DragSelectRect.Top, DragSelectRect.Right, DragSelectRect.Bottom);
  end;



end;

procedure TVamSampleMap.DrawSampleRegion(const aRegion: TVamSampleRegion; aColor: TRedFoxColor; const UseMovingBounds : boolean);
var
  SampleRegionBounds : TRectF;
begin
  BackBuffer.BufferInterface.FillColor := aColor.WithAlphaBlend(200).AsAggRgba8;
  BackBuffer.BufferInterface.LineColor := aColor.WithAlphaBlend(255).AsAggRgba8;
  BackBuffer.BufferInterface.LineWidth := 1;

  if UseMovingBounds
    then SampleRegionBounds := CalcSampleRegionBounds(aRegion, true)
    else SampleRegionBounds := CalcSampleRegionBounds(aRegion, false);

  //SampleRegionBounds.Inflate(-0.5,-0.5);

  BackBuffer.BufferInterface.Rectangle(SampleRegionBounds.Left, SampleRegionBounds.Top, SampleRegionBounds.Right, SampleRegionBounds.Bottom);
end;

procedure TVamSampleMap.DrawSampleRegionResizeHandles(const aRegion: TVamSampleRegion; const aColor:TRedFoxColor; const UseMovingBounds : boolean);
const
  HandleSize = 4;
  HandleSizeDiv2 = 4;
var
  SampleRegionBounds : TRectF;
  HandleBounds : TRectF;
begin
  //====== Draw the resize outline =====
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineColor := aColor;
  BackBuffer.BufferInterface.LineWidth := 1;

  if UseMovingBounds
    then SampleRegionBounds := CalcSampleRegionBounds(aRegion, true)
    else SampleRegionBounds := CalcSampleRegionBounds(aRegion, false);

  //SampleRegionBounds.Inflate(-0.5,-0.5);

  BackBuffer.BufferInterface.Rectangle(SampleRegionBounds.Left, SampleRegionBounds.Top, SampleRegionBounds.Right, SampleRegionBounds.Bottom);


  //====== Draw the handles =====
  BackBuffer.BufferInterface.FillColor := aColor;
  BackBuffer.BufferInterface.NoLine;

  //Top Left
  HandleBounds := CalcRegionHandleBounds(aRegion, rhTopLeft);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Top Right
  HandleBounds := CalcRegionHandleBounds(aRegion, rhTopRight);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Bottom Right
  HandleBounds := CalcRegionHandleBounds(aRegion, rhBottomRight);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Bottom Left
  HandleBounds := CalcRegionHandleBounds(aRegion, rhBottomLeft);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Top
  HandleBounds := CalcRegionHandleBounds(aRegion, rhTop);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Right
  HandleBounds := CalcRegionHandleBounds(aRegion, rhRight);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Bottom
  HandleBounds := CalcRegionHandleBounds(aRegion, rhBottom);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

  //Left
  HandleBounds := CalcRegionHandleBounds(aRegion, rhLeft);
  HandleBounds.Intersect(SampleRegionBounds);
  BackBuffer.BufferInterface.Rectangle(HandleBounds.Left, HandleBounds.Top, HandleBounds.Right, HandleBounds.Bottom);

end;

function TVamSampleMap.CalcRegionHandleBounds(const aRegion: TVamSampleRegion; const HandleIndex: TRegionHandleID): TRectF;
const
  HandleSize = 5;
  HandleSizeDiv2 = 4;
var
  SampleRegionBounds : TRectF;
  HandleBounds : TRectF;
begin
  if aRegion.IsMoving
    then SampleRegionBounds := CalcSampleRegionBounds(aRegion, true)
    else SampleRegionBounds := CalcSampleRegionBounds(aRegion, false);

  SampleRegionBounds.Inflate(0.5,0.5);

  case HandleIndex of
    rhNone:
    begin
      HandleBounds := RectF(0,0,0,0);
    end;

    rhTopLeft:
    begin
      HandleBounds.Left   := SampleRegionBounds.Left;
      HandleBounds.Top    := SampleRegionBounds.Top;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhTopRight:
    begin
      HandleBounds.Left   := SampleRegionBounds.Right - HandleSize;
      HandleBounds.Top    := SampleRegionBounds.Top;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhBottomRight:
    begin
      HandleBounds.Left   := SampleRegionBounds.Right - HandleSize;
      HandleBounds.Top    := SampleRegionBounds.Bottom - HandleSize;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhBottomLeft:
    begin
      HandleBounds.Left   := SampleRegionBounds.Left;
      HandleBounds.Top    := SampleRegionBounds.Bottom - HandleSize;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhTop:
    begin
      HandleBounds.Left   := SampleRegionBounds.Left + (SampleRegionBounds.Width - HandleSize) / 2;
      HandleBounds.Top    := SampleRegionBounds.Top;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhRight:
    begin
      HandleBounds.Left   := SampleRegionBounds.Right - HandleSize;
      HandleBounds.Top    := SampleRegionBounds.Top + (SampleRegionBounds.Height-HandleSize) / 2;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhBottom:
    begin
      HandleBounds.Left   := SampleRegionBounds.Left + (SampleRegionBounds.Width - HandleSize) / 2;
      HandleBounds.Top    := SampleRegionBounds.Bottom - HandleSize;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;

    rhLeft:
    begin
      HandleBounds.Left   := SampleRegionBounds.Left;
      HandleBounds.Top    := SampleRegionBounds.Top + (SampleRegionBounds.Height-HandleSize) / 2;
      HandleBounds.Right  := HandleBounds.Left + HandleSize;
      HandleBounds.Bottom := HandleBounds.Top  + HandleSize;
    end;
  else
    HandleBounds := RectF(0,0,0,0);
  end;

  result := HandleBounds;
end;

function TVamSampleMap.GetDisplayInfo: TVamSampleMapDisplayInfo;
var
  c1: Integer;
  aRegion : TVamSampleRegion;
  s : string;
begin
  // This method returns information about the current state of the sample map display.
  // The GUI can use this information to sync other GUI components to reflect the current
  // state of the sample map.

  aRegion := nil;

  if (not assigned(aRegion)) and (ProposedSampleRegions.Count > 0) then
  begin
    aRegion := ProposedSampleRegions[0];
  end;

  if (not assigned(aRegion)) and (not(IsGrabbedByLeft or IsGrabbedByRight)) and (assigned(MouseOverRegion)) then
  begin
    aRegion := MouseOverRegion;
  end;

  if (not assigned(aRegion)) and (assigned(MouseDownRegion)) then
  begin
    aRegion := MouseDownRegion;
  end;

  if (not assigned(aRegion)) then
  begin
    for c1 := 0 to SampleRegions.Count-1 do
    begin
      if SampleRegions[c1].IsFocused then
      begin
        aRegion := SampleRegions[c1];
        break;
      end;
    end;
  end;

  if assigned(aRegion) then
  begin
    result.IsValid  := true;

    s := aRegion.FileName; //sometimes the region will not be assigned here for some reason...
    result.FileName := s;

    if aRegion.IsMoving = false then
    begin
      result.RootNote     := aRegion.RootNote;
      result.LowKey       := aRegion.LowKey;
      result.HighKey      := aRegion.HighKey;
      result.LowVelocity  := aRegion.LowVelocity;
      result.HighVelocity := aRegion.HighVelocity;
    end else
    begin
      result.RootNote     := aRegion.MovedRootNote;
      result.LowKey       := aRegion.MovedLowKey;
      result.HighKey      := aRegion.MovedHighKey;
      result.LowVelocity  := aRegion.MovedLowVelocity;
      result.HighVelocity := aRegion.MovedHighVelocity;
    end;
  end else
  begin
    result.IsValid := false;
    result.FileName := '';
    result.RootNote := 0;
    result.LowKey   := 0;
    result.HighKey  := 0;
    result.LowVelocity := 0;
    result.HighVelocity := 0;
  end;
end;



function TVamSampleMap.GetDragRegionCount(const Data: IVamDragData): integer;
var
  xCount : integer;
begin
  xCount := Data.GetFiles.Count;

  if assigned(OnGetDragRegionCount) then OnGetDragRegionCount(self, Data, xCount);

  result := xCount;
end;

function TVamSampleMap.GetDragSelectCount: integer;
var
  Count : integer;
  c1 : integer;
begin
  //NOTE: function returns -1 when a "drag select" action is not in progress.

  if IsDragSelectActive then
  begin
    Count := 0;
    for c1 := 0 to SampleRegions.Count-1 do
    begin
      if SampleRegions[c1].IsDragSelected then inc(Count);
    end;
    result := Count;
  end else
  begin
    result := -1;
  end;
end;

function TVamSampleMap.GetMouseOverRegionInfo: TVamSampleMapDisplayInfo;
begin
  if not assigned(MouseOverRegion) then
  begin
    result.IsValid := false;
  end else
  begin
    result.IsValid := true;

    result.FileName     := MouseOverRegion.FileName;

    if MouseOverRegion.IsMoving = false then
    begin
      result.RootNote     := MouseOverRegion.RootNote;
      result.LowKey       := MouseOverRegion.LowKey;
      result.HighKey      := MouseOverRegion.HighKey;
      result.LowVelocity  := MouseOverRegion.LowVelocity;
      result.HighVelocity := MouseOverRegion.HighVelocity;
    end else
    begin
      result.RootNote     := MouseOverRegion.MovedRootNote;
      result.LowKey       := MouseOverRegion.MovedLowKey;
      result.HighKey      := MouseOverRegion.MovedHighKey;
      result.LowVelocity  := MouseOverRegion.MovedLowVelocity;
      result.HighVelocity := MouseOverRegion.MovedHighVelocity;
    end;
  end;
end;

function TVamSampleMap.GetSelectedCount: integer;
var
  Count : integer;
  c1 : integer;
begin
  Count := 0;
  for c1 := 0 to SampleRegions.Count-1 do
  begin
    if SampleRegions[c1].IsSelected then inc(Count);
  end;
  result := Count;
end;

procedure TVamSampleMap.SortSampleRegionList;
begin
  VamSampleMap.Sorting.SortToAvoidUnclickableElements(SampleRegions);
end;

procedure TVamSampleMap.UpdateCursorIcon(Shift: TShiftState; X, Y: Integer);
var
  aHandle : TRegionHandleID;
begin
  aHandle := GetRegionHandleAt(X, Y);
  case aHandle of
    rhNone:        self.Cursor := crDefault;
    rhTopLeft:     self.Cursor := crSizeNWSE;
    rhTopRight:    self.Cursor := crSizeNESW;
    rhBottomRight: self.Cursor := crSizeNWSE;
    rhBottomLeft:  self.Cursor := crSizeNESW;
    rhTop:         self.Cursor := crSizeNS;
    rhRight:       self.Cursor := crSizeWE;
    rhBottom:      self.Cursor := crSizeNS;
    rhLeft:        self.Cursor := crSizeWE;
  end;

end;





end.
