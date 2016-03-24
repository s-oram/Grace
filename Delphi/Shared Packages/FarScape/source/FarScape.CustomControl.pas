unit FarScape.CustomControl;

interface

uses
  Vcl.Controls,
  Classes,
  Windows,
  Graphics,
  Types,
  SysUtils,
  Contnrs,
  Agg2d,
  FarScape.PureInterfacedObject,
  FarScape.Event,
  FarScape.Events,
  FarScape.EventDispatcher,
  FarScape.Color;

type
  EFarScapeException = class(Exception);

  IFarScapeUserInteraction = interface
  end;

  TFarScapeCustomControl = class;
  TFarScapeContainer = class;
  TFarScapeControl = class;
  TFarScapeAbstractRoot = class;

  TMargins = record
    Left, Top, Right, Bottom : integer;
  end;

  TPadding = record
    Left, Top, Right, Bottom : integer;
  end;

  PRootDrawInfo = ^TRootDrawInfo;
  TRootDrawInfo = record
    BackBuffer : TBitmap;
    Agg2d : TAgg2d;
  end;

  TOnInvalidateRootRegion = procedure(Region : TRect) of object;

  THitTest = (
    htNone,    // Hit checking always fails.
    htPartial, // Hit checking will succeed for some parts of the control.
    htAlways   // Hit checking always passes.
  );

  TControlAlignment = (
    caNone,   // No alignment. (duh!)
    caTop,
    caBottom,
    caLeft,
    caRight,
    caClient, // Control is sized to fill the parent bounds with respect to padding and margins.
    caCenter, // Control is placed in the center of the parent.
    caGrid    // Control is aligned to a grid. //TODO:HIGH IMPORTANT: When using caGrid alignment, the TControlPosition units must puGrid.
  );

  TControlPositionUnits = (
    puPixels,
    puPercentage,
    puGrid
  );

  TControlPosition = record
    Alignment  : TControlAlignment;
    Units      : TControlPositionUnits;
    Left       : integer;
    Top        : integer;
    Width      : integer;
    Height     : integer;
    GridWidth  : integer;
    GridHeight : integer;
    procedure Zero;
    procedure AssignFrom(const Source : TControlPosition);
  end;

  TControlState = set of (csIsDestroying);

  TFarScapeDrawMethod = (dmOldSchool, dmNewSchool);

  TFarScapeCustomControl = class(TPureInterfacedObject)
  strict private
    FComputedLeft   : integer;
    FComputedTop    : integer;
    FComputedWidth  : integer;
    FComputedHeight : integer;
    FPadding: TPadding;
    FMargins: TMargins;
    FAlign: TControlAlignment;
    FName: string;
    FIsOwnedByParent: boolean;
    procedure SetAlign(const Value: TControlAlignment);
    procedure SetParent(const c : TFarScapeControl);
    procedure SetName(const Value: string);
  private
    FParent : TFarScapeControl;
    FRoot   : TFarScapeAbstractRoot;
    FVisible: boolean;
    FGridTop: integer;
    FGridHeight: integer;
    FGridLeft: integer;
    FGridWidth: integer;
    FCursor: TCursor;
    FHitTest: THitTest;
    FGridXDivisions: integer;
    FGridYDivisions: integer;

    FEventDispatcher : TEventDispatcher;
    FControlState: TControlState;
    FDrawMethod: TFarScapeDrawMethod;
    FPosition: TControlPosition;
    procedure SetVisible(const Value: boolean);
    procedure SetCursor(const Value: TCursor);
    procedure SetControlPosition(const Value: TControlPosition);

    procedure ControlPositionChanged;
    function GetNaturalHeight: integer;
    function GetNaturalLeft: integer;
    function GetNaturalTop: integer;
    function GetNaturalWidth: integer;
  strict protected
    function FindTopMostControl : TFarScapeControl;
  protected


    procedure SetComputedBounds(const aLeft, aTop, aWidth, aHeight : integer);

    // Descendent components can override ControlBoundsChanged() to react to control size changes.
    // It's a good place to resize buffers or other dependent components.
    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); virtual;

    procedure FarScapeEvent(const ev : TEventData; const ReleaseEvent : boolean = true); virtual;

    property ControlState : TControlState read FControlState write FControlState;
    function FindRoot : TFarScapeAbstractRoot;


  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Name : string read FName write SetName;

    // Trigger a control repaint.
    procedure Invalidate; virtual;

    procedure AddEventListener(const EventTypes : array of TEventClass; const Handler : TEventHandler; const Duration   : TListenerDuration = ldAll);
    procedure RemoveEventListener(const Handler    : TEventHandler);


    property HitTest : THitTest read FHitTest write FHitTest;
    function IsHit(const HitPointX, HitPointY : integer):boolean; virtual;

    // Descendent controls need to override this method to implement custom alignment methods.
    //procedure PerformCustomAlignment; virtual; abstract;

    // GetAbsoluteRect() returns the control bounds with respect to top most parent control.
    function GetAbsoluteRect:TRect;
    function GetNamePath : string;

    property Parent : TFarScapeControl read FParent write SetParent; // The parent control.
    property Root   : TFarScapeAbstractRoot read FRoot;              // The top-most root control.

    property Position : TControlPosition read FPosition write SetControlPosition;

    //====== TODO:HIGH remove these methods ===================
    procedure SetSize(const aWidth, aHeight : integer);
    procedure SetPosition(const aLeft, aTop : integer); //TODO:MED Maybe delete this method?
    procedure SetBounds(const aLeft, aTop, aWidth, aHeight : integer); overload;
    procedure SetBounds(const aBounds : TRect); overload;
    procedure SetGridBounds(const aLeft, aTop, aWidth, aHeight : integer);
    //=========================================================

    property Left   : integer read FComputedLeft;
    property Top    : integer read FComputedTop;
    property Width  : integer read FComputedWidth;
    property Height : integer read FComputedHeight;

    // Natural dimensions are the baseline dimensions before alignment calculations are applied.
    property NaturalLeft   : integer read GetNaturalLeft;
    property NaturalTop    : integer read GetNaturalTop;
    property NaturalWidth  : integer read GetNaturalWidth;
    property NaturalHeight : integer read GetNaturalHeight;

    // TODO:HIGH - remove the grid alignment mode. Let the user perform independent grid alignment.
    // Use the grid properties in conjunction with the Grid alignment mode to create resizable GUI layouts.
    property GridLeft   : integer read FGridLeft;
    property GridTop    : integer read FGridTop;
    property GridWidth  : integer read FGridWidth;
    property GridHeight : integer read FGridHeight;

    // The grid divisions specifies the number of positions in the grid.
    property GridXDivisions : integer read FGridXDivisions;
    property GridYDivisions : integer read FGridYDivisions;
    procedure SetGridDivisions(const XDiv, YDiv : integer);

    // Margins and Padding will be used with some alignment modes.
    property Margins : TMargins read FMargins;
    property Padding : TPadding read FPadding;
    procedure SetMargins(const aLeft, aTop, aRight, aBottom : integer);
    procedure SetPadding(const aLeft, aTop, aRight, aBottom : integer);

    property Align : TControlAlignment read FAlign write SetAlign;

    property Visible : boolean read FVisible write SetVisible;

    property IsOwnedByParent : boolean read FIsOwnedByParent write FIsOwnedByParent;

    property Cursor: TCursor read FCursor write SetCursor default crDefault;

    property DrawMethod : TFarScapeDrawMethod read FDrawMethod write FDrawMethod;
  end;

  TFarScapeContainer = class(TFarScapeCustomControl)
  strict private
    FControlList : TObjectList;
  private
    procedure InsertControl(const aControl : TFarScapeControl);
    procedure RemoveControl(const aControl : TFarScapeControl);
    function GetControlCount: integer;
    function GetControl(Index: integer): TFarScapeControl; // Removes the child but doesn't free it.
  strict protected
    property ControlList : TObjectList read fControlList;
  protected
    procedure AlignChildControls;

    // Request alignment of all child controls to be updated.
    procedure RequestControlAlignment;

    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // ReleaseAllChildControls() removes all child controls. Owned controls will be freed.
    procedure ReleaseAllChildControls;

    // == child controls ==
    function ContainsControl(const aControl : TFarScapeControl):boolean;

    // TODO:MED Should this be rename to ChildControl[]?
    property Control[Index : integer] : TFarScapeControl read GetControl;
    property ControlCount : integer read GetControlCount;

    // TODO:HIGH NOTE: Currently, FarScape allows multple controls to share the same name.
    // This should be changed. Additionally, I'm thinking about storing a list of all
    // controls in the root control.
    function FindControlByNamePath(const NamePath : string):TFarScapeControl;
  end;

  TFarScapeControl = class(TFarScapeContainer)
  protected
    //==== Mouse Events ====
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  public
    destructor Destroy; override;

    function FindControlAndConfirmClassType(const Name : string; const ClassType : TClass):TFarScapeControl;
    function FindControlByName(const Name : string):TFarScapeControl;

    procedure PaintToBackBuffer(const DrawInfo : PRootDrawInfo); virtual;
    procedure PaintToDc(DC: HDC); virtual;

    procedure RemoveAllEventListeners(const RemoveForChildren : boolean);

    function SetPropertyValue(const PropertyName : string; const StrArr : array of string; const IntArr : array of integer; const FloatArr : array of single; const BoolArr : array of boolean):boolean; virtual;
  end;

  TFarScapeAbstractRoot = class(TFarScapeControl)
  private
    fOnInvalidateRootRegion: TOnInvalidateRootRegion;
  protected
    procedure FarScapeEvent(const ev : TEventData; const ReleaseEvent : boolean = true); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InvalidateRootRegion(r : TRect); virtual;

    property OnInvalidateRootRegion : TOnInvalidateRootRegion read fOnInvalidateRootRegion write fOnInvalidateRootRegion;
  end;

  PFarScapeControl = ^TFarScapeControl;
  TFarScapeControlClass = class of TFarScapeControl;
  TFarScapeControlProc = reference to procedure(const c : TFarScapeControl);

function FindOrCreateControl(const Root : TFarScapeAbstractRoot; const ControlClass : TFarScapeControlClass; const Name : string):TFarScapeControl; overload;

implementation

uses
  VamLib.Utils,
  FarScape.Assistant.AlignControls,
  FarScape.ControlHelper,
  FarScape.SupportFunctions;

// ForAllChildControls() is a way to apply changes to all child controls. It's an alternative
// to having a 'propagate down' event system. Currently Events propagate up.
procedure ForAllChildControls(const Control : TFarScapeControl; const Proc : TFarScapeControlProc);
var
  c1: Integer;
begin
  // Apply the procedure. Proc().
  for c1 := Control.ControlCount-1 downto 0 do
  begin
    Proc(Control.Control[c1]);
  end;

  // recursivly call function for all child controls.
  for c1 := Control.ControlCount-1 downto 0 do
  begin
    ForAllChildControls(Control.Control[c1], Proc);
  end;
end;


procedure SetRoot(const Control : TFarScapeControl; const aRoot : TFarScapeAbstractRoot);
var
  c1: Integer;
begin
  Control.FRoot := aRoot;
  for c1 := 0 to Control.ControlCount-1 do
  begin
    SetRoot(Control.Control[c1], aRoot);
  end;
end;

function Recursive_FindControlByName(const Control : TFarScapeControl; const Name : string):TFarScapeControl;
var
  c1: Integer;
  c : TFarScapeControl;
begin
  assert(assigned(Control));
  if SameText(Control.Name, Name) then exit(Control);

  for c1 := 0 to Control.ControlCount-1 do
  begin
    c := Recursive_FindControlByName(Control.Control[c1], Name);
    if assigned(c) then exit(c);
  end;

  // if we make it this far, the control hasn't been found.
  result := nil;
end;

function FindOrCreateControl(const Root : TFarScapeAbstractRoot; const ControlClass : TFarScapeControlClass; const Name : string):TFarScapeControl; overload;
var
  fsc : TFarScapeControl;
begin
  fsc := Root.FindControlByName(Name);
  if assigned(fsc) then
  begin
    if fsc.ClassType <> ControlClass then raise EFarScapeException.Create('Object exists but isn''t of same type.');
    result := fsc;
    exit; //============>> exit >>=========>>
  end;

  fsc := TFarScapeControlClass.Create;
  fsc.Name := Name;
  fsc.Visible := true;
  result := fsc;
end;


{ TFarScapeCustomControl }

constructor TFarScapeCustomControl.Create;
begin
  FGridXDivisions := 24;
  FGridYDivisions := 24;
  FHitTest := htAlways;
  FVisible := true;
  FParent := nil;
  FRoot := nil;
  FName := '';
  FAlign := TControlAlignment.caNone;

  FComputedLeft   := 0;
  FComputedTop    := 0;
  FComputedWidth  := 0;
  FComputedHeight := 0;

  FMargins.Left   := 0;
  FMargins.Top    := 0;
  FMargins.Right  := 0;
  FMargins.Bottom := 0;

  FPadding.Left   := 0;
  FPadding.Top    := 0;
  FPadding.Right  := 0;
  FPadding.Bottom := 0;

  FGridTop    := 0;
  FGridLeft   := 0;
  FGridWidth  := 1;
  FGridHeight := 1;

  FIsOwnedByParent := true;
end;

destructor TFarScapeCustomControl.Destroy;
begin
  if assigned(fParent) then fParent.RemoveControl((self as TFarScapeControl));
  if assigned(FEventDispatcher) then FreeAndNil(FEventDispatcher);
  inherited;
end;

procedure TFarScapeCustomControl.AddEventListener(const EventTypes: array of TEventClass; const Handler: TEventHandler; const Duration: TListenerDuration);
begin
  if not assigned(FEventDispatcher) then FEventDispatcher := TEventDispatcher.Create;
  FEventDispatcher.AddListener(EventTypes, Handler, Duration);
end;

procedure TFarScapeCustomControl.RemoveEventListener(const Handler: TEventHandler);
begin
  if assigned(FEventDispatcher) then FEventDispatcher.RemoveListener(Handler);
end;

procedure TFarScapeCustomControl.FarScapeEvent(const ev: TEventData; const ReleaseEvent : boolean);
begin
  if assigned(FEventDispatcher) then FEventDispatcher.Broadcast(ev, false);

  if (ev.Propagate) and (assigned(Parent))
    then Parent.FarScapeEvent(ev, false);

  if ReleaseEvent then ev.Release;
end;



function TFarScapeCustomControl.FindRoot: TFarScapeAbstractRoot;
var
  fsc : TFarScapeControl;
begin
  fsc := self as TFarScapeControl;
  while assigned(fsc.Parent) do
  begin
    fsc := fsc.Parent;
  end;

  if fsc is TFarScapeAbstractRoot
    then result := fsc as TFarScapeAbstractRoot
    else result := nil;
end;

function TFarScapeCustomControl.FindTopMostControl: TFarScapeControl;
var
  fsc : TFarScapeControl;
begin
  fsc := self as TFarScapeControl;
  while assigned(fsc.Parent) do
  begin
    fsc := fsc.Parent;
  end;
  result := fsc;
end;

function TFarScapeCustomControl.GetAbsoluteRect: TRect;
var
  fsc : TFarScapeControl;
begin
  fsc := FindTopMostControl;
  result := (self as TFarScapeControl).GetBoundsInReferenceTo(fsc);
end;

procedure TFarScapeCustomControl.SetPosition(const aLeft, aTop: integer);
begin
  SetBounds(aLeft, aTop, Width, Height);
end;

procedure TFarScapeCustomControl.SetSize(const aWidth, aHeight: integer);
begin
  SetBounds(Left, Top, aWidth, aHeight);
end;

procedure TFarScapeCustomControl.SetVisible(const Value: boolean);
var
  fsc : TFarScapeControl;
begin
  if Value <> fVisible then
  begin
    fVisible := Value;

    // 1) Request alignment if needed...
    if (assigned(self.Parent)) and (Align <> TControlAlignment.caNone) then
    begin
      Parent.RequestControlAlignment;
    end;

    // 2) Align children.
    fsc := (self as TFarScapeControl);
    if (Visible) and (fsc.ControlCount > 0) then fsc.AlignChildControls;

    // 3) Notify the root...
    // TODO:HIGH might need to notify the root that a control has change visibility here.
  end;
end;

procedure TFarScapeCustomControl.SetAlign(const Value: TControlAlignment);
begin
  if (fAlign <> Value) then
  begin
    fAlign := Value;
    if assigned(Parent) then Parent.RequestControlAlignment;
  end;
end;

procedure TFarScapeCustomControl.SetBounds(const aBounds: TRect);
begin
  SetBounds(aBounds.Left, aBounds.Top, aBounds.Width, aBounds.Height);
end;

procedure TFarScapeCustomControl.SetBounds(const aLeft, aTop, aWidth, aHeight: integer);
begin
  FPosition.Left := aLeft;
  FPosition.Top  := aTop;
  FPosition.Width := aWidth;
  FPosition.Height := aHeight;
  FPosition.Units := puPixels;
  ControlPositionChanged;
end;

procedure TFarScapeCustomControl.SetGridBounds(const aLeft, aTop, aWidth, aHeight: integer);
begin
  FPosition.Left := aLeft;
  FPosition.Top  := aTop;
  FPosition.Width := aWidth;
  FPosition.Height := aHeight;
  FPosition.Units := puGrid;
  ControlPositionChanged;
end;

procedure TFarScapeCustomControl.SetGridDivisions(const XDiv, YDiv: integer);
begin
  FPosition.GridWidth := XDiv;
  FPosition.GridHeight := YDiv;
  ControlPositionChanged;
end;

procedure TFarScapeCustomControl.SetComputedBounds(const aLeft, aTop, aWidth, aHeight: integer);
var
  OriginalBounds : TRect;
  NewBounds : TRect;
begin
  if assigned(Root) then
  begin
    // 1) Get current client rect.
    OriginalBounds := (self as TFarScapeControl).GetBounds;
    OriginalBounds.Offset( (self as TFarScapeControl).GetAbsoluteOffset );
  end else
  begin
    OriginalBounds := Rect(0,0,0,0);
  end;

  // 2) Update control boundariers
  fComputedLeft   := aLeft;
  fComputedTop    := aTop;

  if aWidth > 0
    then fComputedWidth := aWidth
    else fComputedWidth := 0;
  if aHeight > 0
    then fComputedHeight := aHeight
    else fComputedHeight := 0;

  // 3) Provide chance for descendcent controls to respond to changes.
  ControlBoundsChanged(aLeft, aTop, aWidth, aHeight);

  // 4) Important: The root needs to know that a child control has changed location.
  FarScapeEvent(  TControlBoundsChangedEvent.Create(self)  );

  if assigned(Root) then
  begin
    // 5) Get new client rect.
    NewBounds := (self as TFarScapeControl).GetBounds;
    NewBounds.Offset( (self as TFarScapeControl).GetAbsoluteOffset );

    // 6) Invalidate both client rect regions.
    Root.InvalidateRootRegion(OriginalBounds);
    Root.InvalidateRootRegion(NewBounds);
  end;
end;

procedure TFarScapeCustomControl.SetControlPosition(const Value: TControlPosition);
begin
  FPosition := Value;
  ControlPositionChanged;
end;

procedure TFarScapeCustomControl.ControlPositionChanged;
begin
  if assigned(Parent)
    then Parent.RequestControlAlignment
    else self.SetComputedBounds(0,0, Position.Width, Position.Height);
end;

procedure TFarScapeCustomControl.SetCursor(const Value: TCursor);
begin
  FCursor := Value;
end;

procedure TFarScapeCustomControl.ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight: integer);
begin
  // Empty. Provided for descendent controls.
end;

procedure TFarScapeCustomControl.SetPadding(const aLeft, aTop, aRight, aBottom: integer);
var
  c : TFarScapeControl;
begin
  fPadding.Left   := aLeft;
  fPadding.Top    := aTop;
  fPadding.Right  := aRight;
  fPadding.Bottom := aBottom;

  c := (self as TFarScapeControl);
  if c.ControlCount > 0 then c.AlignChildControls;
end;

procedure TFarScapeCustomControl.SetParent(const c: TFarScapeControl);
begin
  if self.Parent = c then exit;

  if assigned(self.Parent) then
  begin
    self.Parent.RemoveControl((self as TFarScapeControl));
  end;

  if assigned(c) then
  begin
    c.InsertControl((self as TFarScapeControl));
  end;
end;

procedure TFarScapeCustomControl.SetMargins(const aLeft, aTop, aRight, aBottom: integer);
begin
  fMargins.Left   := aLeft;
  fMargins.Top    := aTop;
  fMargins.Right  := aRight;
  fMargins.Bottom := aBottom;

  // TODO:HIGH check what the alignment mode is, then ask the parent control to realign if needed.
  // Actually, instead of checking, possibly just trigger a relayout and let the parent check,
  // the parent may ignore the alignment property and enforce a custom alignment.

  if (Align <> TControlAlignment.caNone) and (assigned(self.Parent)) then
  begin
    Parent.RequestControlAlignment;
  end;
end;

procedure TFarScapeCustomControl.SetName(const Value: string);
var
  OldName : string;
begin
  if (Pos('.', Value) > 0) then raise EFarScapeException.Create('FarScapeControl name must not contain period characters. (.)');

  OldName := fName;
  fName := Value; //TODO:MED Trim white space from name.

  FarScapeEvent( TControlNameChangedEvent.Create(self, @OldName, @FName)  );
end;

procedure TFarScapeCustomControl.Invalidate;
begin
  if assigned(Root) then Root.InvalidateRootRegion(self.GetAbsoluteRect);
end;


function TFarScapeCustomControl.IsHit(const HitPointX, HitPointY: integer): boolean;
begin
  case HitTest of
    htNone:   result := false;
    htAlways: result := true;
    htPartial: raise EFarScapeException.Create('Not implemented.');
  else
    raise EFarScapeException.Create('Unexpected type.');
  end;
end;


function TFarScapeCustomControl.GetNamePath: string;
var
  fsc : TFarScapeControl;
  NamePath : string;
begin
  if self.Name = '' then
  begin
    result := ''
  end else
  begin
    NamePath := self.Name;
    fsc := self as TFarScapeControl;
    while assigned(fsc.Parent) do
    begin
      fsc := fsc.Parent;
      if fsc.Name <> '' then NamePath := fsc.Name + '.' + NamePath;
    end;
    result := NamePath;
  end;
end;

function TFarScapeCustomControl.GetNaturalHeight: integer;
begin
  if assigned(Parent) then
  begin
    case Position.Units of
      puGrid:       result := Position.Height;
      puPixels:     result := Position.Height;
      puPercentage: result := round(Parent.Height * Position.Height / 100);
    else
      assert(false, 'Unexpected units value.');
      result := 0;
    end;
  end else
  begin
    result := Position.Height;
  end;
end;

function TFarScapeCustomControl.GetNaturalTop: integer;
begin
  if assigned(Parent) then
  begin
    case Position.Units of
      puGrid:       result := Position.Top;
      puPixels:     result := Position.Top;
      puPercentage: result := round(Parent.Height * Position.Top / 100);
    else
      assert(false, 'Unexpected units value.');
      result := 0;
    end;
  end else
  begin
    result := Position.Top;
  end;
end;

function TFarScapeCustomControl.GetNaturalLeft: integer;
begin
  if assigned(Parent) then
  begin
    case Position.Units of
      puGrid:       result := Position.Left;
      puPixels:     result := Position.Left;
      puPercentage: result := round(Parent.Width * Position.Left / 100);
    else
      assert(false, 'Unexpected units value.');
      result := 0;
    end;
  end else
  begin
    result := Position.Left;
  end;
end;

function TFarScapeCustomControl.GetNaturalWidth: integer;
begin
    if assigned(Parent) then
  begin
    case Position.Units of
      puGrid:       result := Position.Width;
      puPixels:     result := Position.Width;
      puPercentage: result := round(Parent.Width * Position.Width / 100);
    else
      assert(false, 'Unexpected units value.');
      result := 0;
    end;
  end else
  begin
    result := Position.Width;
  end;
end;

{ TFarScapeContainerControl }

function TFarScapeContainer.ContainsControl(const aControl: TFarScapeControl): boolean;
var
  c1: Integer;
  c : TObject;
begin
  // TODO:HIGH The implementation of this method should be flipped. Instead
  // check if the target control has this control as a parent. Probably far less controls to check.
  // Better yet. It's probably possible to move this method to the helper class.

  // First check for immediate children...
  for c1 := 0 to ControlList.Count-1 do
  begin
    if ControlList[c1] = aControl then exit(true); //=================== exit =====================>>
  end;

  // ...then check if control is a child of.
  for c1 := 0 to ControlList.Count-1 do
  begin
    c := ControlList[c1];
    // TODO:MED Checking if the control is a FarScapeContainer is now redundant. All controls will be TFarScapeControl
    if (c is TFarScapeControl) then
    begin
      if (c as TFarScapeControl).ContainsControl(aControl) then exit(true); //=========== exit =========>>
    end;
  end;

  // If we make it this far, the control is not contained.
  result := false;
end;

procedure TFarScapeContainer.ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited;
  if ControlCount > 0 then AlignChildControls;
end;

constructor TFarScapeContainer.Create;
begin
  inherited;
  fControlList := TObjectList.Create;
  fControlList.OwnsObjects := false;
end;

destructor TFarScapeContainer.Destroy;
begin
  // NOTE: Should the child controls be owned by their parents? Should it be optional?
  ReleaseAllChildControls;

  fControlList.Free;
  inherited;
end;

function TFarScapeContainer.FindControlByNamePath(const NamePath: string): TFarScapeControl;
var
  c1 : integer;
  NameParts : TStringList;
  PartIndex : integer;
  CurTarget : TFarScapeContainer;
  NextTarget : TFarScapeContainer;
begin
  // TODO:MED need to write a unit test for this method.


  // TODO:LOW this method could be much more efficient.
  NameParts := TStringList.Create;
  AutoFree(@NameParts);

  ExplodeString('.', NamePath, NameParts);

  if NameParts.Count = 0 then exit(nil);

  PartIndex := 0;
  CurTarget := self;
  while PartIndex < NameParts.Count do
  begin
    NextTarget := nil;

    for c1 := 0 to CurTarget.ControlCount-1 do
    begin
      if SameText(CurTarget.Control[c1].Name, NameParts[PartIndex]) then
      begin
        NextTarget := CurTarget.Control[c1];
        inc(PartIndex);
        break;
      end;
    end;

    if assigned(NextTarget)
      then CurTarget := NextTarget
      else exit(nil);
  end;

  assert(CurTarget <> self);

  result := CurTarget as TFarScapeControl;
end;

function TFarScapeContainer.GetControl(Index: integer): TFarScapeControl;
var
  obj : TObject;
begin
  assert(assigned(self.ControlList));
  assert(Index < ControlList.Count);
  obj := ControlList[Index];
  assert(assigned(obj));
  if not(obj is TFarScapeControl)
    then result := nil
    else result := obj as TFarScapeControl;

  //result := ControlList[Index] as TFarScapeControl;
end;

function TFarScapeContainer.GetControlCount: integer;
begin
  result := ControlList.Count;
end;

procedure TFarScapeContainer.ReleaseAllChildControls;
var
  c1: Integer;
  c : TFarScapeControl;
begin
  // This method does one of two things for each child control.
  // If it's owned, it gets deleted, if not, the control is removed.
  for c1 := ControlList.Count-1 downto 0 do
  begin
    if assigned(ControlList[c1]) and (ControlList[c1] is TFarScapeControl) then
    begin
      c := ControlList[c1] as TFarScapeControl;
      RemoveControl(c);
      if c.IsOwnedByParent then c.Free;
    end;
  end;
end;

procedure TFarScapeContainer.InsertControl(const aControl: TFarScapeControl);
var
  rc : TFarScapeAbstractRoot;
begin
  // 1) Add the control.
  if assigned(aControl.Parent) then raise EFarScapeException.Create('Control already has a parent.');
  ControlList.Add(aControl);
  aControl.FParent := self as TFarScapeControl;

  // 2) align new siblings...
  if aControl.Align <> TControlAlignment.caNone then self.RequestControlAlignment;

  // 3) Update root control for all controls.
  rc := FindRoot;
  SetRoot(aControl, rc);

  // 4) Generate a ChildAddedEvent.
  FarScapeEvent(  TChildAddedEvent.Create(aControl)  );
end;

procedure TFarScapeContainer.RemoveControl(const aControl: TFarScapeControl);
begin
  if aControl.Parent <> self then raise EFarScapeException.Create('Control is not a child of this container control.');

  FarScapeEvent(  TChildRemovingEvent.Create(aControl)  );

  // 1) Remove the control.
  ControlList.Remove(aControl);
  aControl.FParent := nil;


  // 2) Align old siblings...
  if (not (csIsDestroying in ControlState)) and (aControl.Align <> TControlAlignment.caNone)
    then self.RequestControlAlignment;

  // 3) Remove the root for all children..
  assert(aControl is TFarScapeControl);
  SetRoot(aControl, nil);

  // Finally, trigger a child removed event.
  FarScapeEvent(  TChildRemovedEvent.Create(aControl)  );

end;

procedure TFarScapeContainer.RequestControlAlignment;
begin
  if ControlCount > 0 then AlignChildControls;
end;

procedure TFarScapeContainer.AlignChildControls;
begin
  if ControlCount > 0 then ControlAlignmentAssistant.AlignChildControls(self as TFarScapeControl);
end;



{ TFarScapeControl }

procedure TFarScapeControl.MouseEnter;
begin
  FarScapeEvent(  TMouseEnterEvent.Create(self)  );
end;

procedure TFarScapeControl.MouseLeave;
begin
  FarScapeEvent(  TMouseLeaveEvent.Create(self)  );
end;

destructor TFarScapeControl.Destroy;
begin
  ControlState := ControlState + [csIsDestroying];
  inherited;
end;

function TFarScapeControl.FindControlAndConfirmClassType(const Name: string; const ClassType: TClass): TFarScapeControl;
var
  c : TFarScapeControl;
begin
  c := Recursive_FindControlByName(self, Name);
  if (assigned(c)) and (not(c is ClassType))
    then raise EFarScapeException.Create('Object exists but fails type check.');
  result := c;
end;

function TFarScapeControl.FindControlByName(const Name: string): TFarScapeControl;
begin
  result := Recursive_FindControlByName(self, Name);
end;

procedure TFarScapeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FarScapeEvent(  TMouseDownEvent.Create(self, Button, Shift, X, Y)  );
end;

procedure TFarScapeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  // Don't generate mouse move events by default. That will be a lot of events.
end;

procedure TFarScapeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FarScapeEvent(  TMouseUpEvent.Create(self, Button, Shift, X, Y)  );
end;

procedure TFarScapeControl.PaintToBackBuffer(const DrawInfo: PRootDrawInfo);
begin

end;

procedure TFarScapeControl.PaintToDc(DC: HDC);
begin

end;

procedure TFarScapeControl.RemoveAllEventListeners(const RemoveForChildren: boolean);
var
  c1: Integer;
begin
  if assigned(FEventDispatcher)
    then FEventDispatcher.RemoveAllListeners;

  if RemoveForChildren then
  begin
    for c1 := self.ControlCount-1 downto 0 do
    begin
      self.Control[c1].RemoveAllEventListeners(true);
    end;
  end;
end;

function TFarScapeControl.SetPropertyValue(const PropertyName: string; const StrArr: array of string; const IntArr: array of integer; const FloatArr: array of single; const BoolArr: array of boolean): boolean;
var
  pn : string;
begin
  // TODO:MED Add tests to ensure all properties are being set correctly.

  pn := Uppercase(PropertyName);
  if pn = 'VISIBLE'       then begin self.Visible := BoolArr[0];                                        exit(true); end;
  if pn = 'SIZE'          then begin self.SetSize(IntArr[0], IntArr[1]);                                exit(true); end;
  if pn = 'POSITION'      then begin self.SetPosition(IntArr[0], IntArr[1]);                            exit(true); end;
  if pn = 'BOUNDS'        then begin self.SetBounds(IntArr[0], IntArr[1], IntArr[2], IntArr[3]);        exit(true); end;
  if pn = 'GRIDBOUNDS'    then begin self.SetGridBounds(IntArr[0], IntArr[1], IntArr[2], IntArr[3]);    exit(true); end;
  if pn = 'GRIDDIVISIONS' then begin self.SetGridDivisions(IntArr[0], IntArr[1]);                       exit(true); end;
  if pn = 'MARGINS'       then begin self.SetMargins(IntArr[0], IntArr[1], IntArr[2], IntArr[3]);       exit(true); end;
  if pn = 'PADDING'       then begin self.SetPadding(IntArr[0], IntArr[1], IntArr[2], IntArr[3]);       exit(true); end;
  if pn = 'ALIGN'         then begin self.Align   := StrToAlign(StrArr[0]);                             exit(true); end;
  if pn = 'HITTEST'       then begin self.HitTest := StrToHitTest(StrArr[0]);                           exit(true); end;

  // if we make it this far, no supported property match has been found.
  result := false;
end;

{ TFarScapeAbstractRoot }

constructor TFarScapeAbstractRoot.Create;
begin
  inherited;
  FRoot := self;
end;

destructor TFarScapeAbstractRoot.Destroy;
begin
  inherited;
end;

procedure TFarScapeAbstractRoot.FarScapeEvent(const ev: TEventData; const ReleaseEvent: boolean);
begin
  inherited;
end;

procedure TFarScapeAbstractRoot.InvalidateRootRegion(r: TRect);
begin
  //assert(assigned(Root));
  //if Root <> self then raise EFarScapeException.Create('InvalidateRegion() can only be called on root elements.');

  if assigned(OnInvalidateRootRegion) then OnInvalidateRootRegion(r);
end;





{ TControlPosition }

procedure TControlPosition.AssignFrom(const Source: TControlPosition);
begin
  self.Alignment  := source.Alignment;
  self.Units      := source.Units;
  self.Left       := source.Left;
  self.Top        := source.Top;
  self.Width      := source.Width;
  self.Height     := source.Height;
  self.GridWidth  := source.GridWidth;
  self.GridHeight := source.GridHeight;
end;

procedure TControlPosition.Zero;
begin
  self.Alignment  := TControlAlignment.caNone;
  self.Units      := puPixels;
  self.Left       := 0;
  self.Top        := 0;
  self.Width      := 0;
  self.Height     := 0;
  self.GridWidth  := 100;
  self.GridHeight := 100;
end;

end.
