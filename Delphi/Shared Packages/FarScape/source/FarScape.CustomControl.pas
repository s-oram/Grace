unit FarScape.CustomControl;

interface

uses
  Vcl.Controls,
  Classes,
  Windows,
  Types,
  SysUtils,
  Contnrs;

type
  EFarScapeException = class(Exception);

  IFarScapeScene = interface
    // RebuildScene must be called after adding or removing an element from the scene root.
    procedure RebuildScene;

    // Call when:
    // - a control changes position.
    procedure UpdateScene;
  end;

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

  TOnInvalidateRootRegion = procedure(Region : TRect) of object;

  TControlAlignment = (
    caNone,   // No alignment. (duh!)
    caTop,
    caBottom,
    caLeft,
    caRight,
    caClient, // Control is sized to fill the parent bounds with respect to padding and margins.
    caCenter, // Control is placed in the center of the parent.
    caGrid,   // Control is aligned to a grid.
    caCustom  // The control implements a custom alignment method to align itself.
  );

  TFarScapeCustomControl = class
  strict private
    fNaturalLeft   : integer;
    fNaturalTop    : integer;
    fNaturalWidth  : integer;
    fNaturalHeight : integer;
    fComputedLeft   : integer;
    fComputedTop    : integer;
    fComputedWidth  : integer;
    fComputedHeight : integer;
    fPadding: TPadding;
    fMargins: TMargins;
    fAlign: TControlAlignment;
    fName: string;
    procedure SetAlign(const Value: TControlAlignment);
    procedure SetParent(const c : TFarScapeControl);
    procedure SetName(const Value: string);
  private
    fParent : TFarScapeControl;
    fRoot   : TFarScapeAbstractRoot;
    fVisible: boolean;
    fGridTop: integer;
    fGridHeight: integer;
    fGridLeft: integer;
    fGridWidth: integer;
    procedure SetVisible(const Value: boolean);
  strict protected
    function FindTopMostControl : TFarScapeControl;
  protected
    procedure SetComputedBounds(const aLeft, aTop, aWidth, aHeight : integer);

    // Descendent components can override ControlBoundsChanged() to react to control size changes.
    // It's a good place to resize buffers or other dependent components.
    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); virtual;


  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Name : string read fName write SetName;

    // Trigger a control repaint.
    procedure Invalidate;

    // Descendent controls need to override this method to implement custom alignment methods.
    //procedure PerformCustomAlignment; virtual; abstract;

    // GetAbsoluteRect() returns the control bounds with respect to top most parent control.
    function GetAbsoluteRect:TRect;

    property Parent : TFarScapeControl read fParent write SetParent; // The parent control.
    property Root   : TFarScapeAbstractRoot read fRoot;              // The top-most root control.

    procedure SetSize(const aWidth, aHeight : integer);
    procedure SetPosition(const aLeft, aTop : integer); //TODO:MED Maybe delete this method?
    procedure SetBounds(const aLeft, aTop, aWidth, aHeight : integer);
    procedure SetGridBounds(const aLeft, aTop, aWidth, aHeight : integer);

    property Left   : integer read fComputedLeft;
    property Top    : integer read fComputedTop;
    property Width  : integer read fComputedWidth;
    property Height : integer read fComputedHeight;

    // Natural dimensions are the baseline dimensions before alignment calculations are applied.
    property NaturalLeft   : integer read fNaturalLeft;
    property NaturalTop    : integer read fNaturalTop;
    property NaturalWidth  : integer read fNaturalWidth;
    property NaturalHeight : integer read fNaturalHeight;

    // Use the grid properties in conjunction with the Grid alignment mode to create resizable GUI layouts.
    property GridLeft   : integer read fGridLeft;
    property GridTop    : integer read fGridTop;
    property GridWidth  : integer read fGridWidth;
    property GridHeight : integer read fGridHeight;

    // Margins and Padding will be used with some alignment modes.
    property Margins : TMargins read fMargins;
    property Padding : TPadding read fPadding;
    procedure SetMargins(const aLeft, aTop, aRight, aBottom : integer);
    procedure SetPadding(const aLeft, aTop, aRight, aBottom : integer);

    property Align : TControlAlignment read fAlign write SetAlign;

    property Visible : boolean read fVisible write SetVisible;
  end;

  TFarScapeContainer = class(TFarScapeCustomControl)
  strict private
    fControlList : TObjectList;
    procedure RemoveAllChildControls;
  private
    fGridRowCount: integer;
    fGridColumnCount: integer;
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

    procedure SetAlignmentGridSize(const aRowCount, aColumnCount : integer);

    function ContainsControl(const aControl : TFarScapeControl):boolean;

    property Control[Index : integer] : TFarScapeControl read GetControl;
    property ControlCount : integer read GetControlCount;

    property AlignmentGridRowCount    : integer read fGridRowCount;
    property AlignmentGridColumnCount : integer read fGridColumnCount;
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
    procedure PaintToDc(DC: HDC); virtual;
  end;

  TFarScapeAbstractRoot = class(TFarScapeControl)
  private
    fOnInvalidateRootRegion: TOnInvalidateRootRegion;
  protected
    // ObjectHierarachChanged() must be called when a control is added, removed or child ordering changed.
    procedure ObjectHierarchyChanged; virtual;

    // ObjectLayoutChanged() must be called when a control position is changed.
    procedure ObjectLayoutChanged; virtual;

    // IMPORTANT: Descendent classes must implement SceneInterface() and UserInteraction() functions.
    function GetSceneInterface : IFarScapeScene; virtual; abstract;
    function GetUserInteractionInterface : IFarScapeUserInteraction; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure InvalidateRootRegion(r : TRect); virtual;

    property OnInvalidateRootRegion : TOnInvalidateRootRegion read fOnInvalidateRootRegion write fOnInvalidateRootRegion;
  end;



implementation

uses
  FarScape.Assistant.AlignControls,
  FarScape.ControlHelper;


procedure UpdateRootForAllControls(const Control : TFarScapeControl; const Root : TFarScapeAbstractRoot);
var
  c1: Integer;
begin
  Control.fRoot := Root;
  for c1 := 0 to Control.ControlCount-1 do
  begin
    UpdateRootForAllControls(Control.Control[c1], Root);
  end;
end;

{ TFarScapeCustomControl }

constructor TFarScapeCustomControl.Create;
begin
  fVisible := true;
  fParent := nil;
  fRoot := nil;
  fAlign := TControlAlignment.caNone;

  fComputedLeft   := 0;
  fComputedTop    := 0;
  fComputedWidth  := 0;
  fComputedHeight := 0;

  fMargins.Left   := 0;
  fMargins.Top    := 0;
  fMargins.Right  := 0;
  fMargins.Bottom := 0;

  fPadding.Left   := 0;
  fPadding.Top    := 0;
  fPadding.Right  := 0;
  fPadding.Bottom := 0;

  fGridTop    := 0;
  fGridLeft   := 0;
  fGridWidth  := 1;
  fGridHeight := 1;
end;

destructor TFarScapeCustomControl.Destroy;
begin
  if assigned(fParent) then fParent.RemoveControl((self as TFarScapeControl));
  inherited;
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
    //if assigned(Root) then Root.ObjectHierarchyChanged;
    // TODO:HIGH instead of ObjectHierarchyChange(). Might need to add a new method. ObjectVisibilityChanged(). Or ObjectLayoutChanged().
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

procedure TFarScapeCustomControl.SetBounds(const aLeft, aTop, aWidth, aHeight: integer);
begin
  fNaturalLeft   := aLeft;
  fNaturalTop    := aTop;

  if aWidth > 0
    then fNaturalWidth := aWidth
    else fNaturalWidth := 0;

  if aHeight > 0
    then fNaturalHeight := aHeight
    else fNaturalHeight := 0;

  if (Align = TControlAlignment.caNone) or (not assigned(self.Parent)) then
  begin
    SetComputedBounds(aLeft, aTop, aWidth, aHeight);
  end else
  begin
    self.Parent.RequestControlAlignment;
  end;
end;

procedure TFarScapeCustomControl.SetGridBounds(const aLeft, aTop, aWidth, aHeight: integer);
begin
  fGridLeft   := aLeft;
  fGridTop    := aTop;
  fGridWidth  := aWidth;
  fGridHeight := aHeight;

  if (Align = TControlAlignment.caGrid) and (assigned(self.Parent))
    then self.Parent.RequestControlAlignment;
end;

procedure TFarScapeCustomControl.SetComputedBounds(const aLeft, aTop, aWidth, aHeight: integer);
var
  OriginalBounds : TRect;
  NewBounds : TRect;
begin
  if assigned(Root) then
  begin
    // 1) Get current client rect.
    OriginalBounds := (self as TFarScapeControl).GetBoundsWithChildren;
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

  if assigned(Root) then
  begin
    // 3) Important: The root needs to know that a child control has changed location.
    Root.ObjectLayoutChanged;

    // 4) Get new client rect.
    NewBounds := (self as TFarScapeControl).GetBoundsWithChildren;
    NewBounds.Offset( (self as TFarScapeControl).GetAbsoluteOffset );

    // 5) Invalidate both client rect regions.
    Root.InvalidateRootRegion(OriginalBounds);
    Root.InvalidateRootRegion(NewBounds);
  end;
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
begin
  if (Pos('.', Value) > 0) then raise EFarScapeException.Create('FarScapeControl name must not contain period characters. (.)');
  fName := Value;
end;

procedure TFarScapeCustomControl.Invalidate;
begin
  if assigned(Root) then Root.InvalidateRootRegion(self.GetAbsoluteRect);
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
  fGridRowCount := 24;
  fGridColumnCount := 24;
end;

destructor TFarScapeContainer.Destroy;
begin
  // NOTE: Should the child controls be owned by their parents? Should it be optional?
  RemoveAllChildControls;

  fControlList.Free;
  inherited;
end;

function TFarScapeContainer.GetControl(Index: integer): TFarScapeControl;
begin
  result := ControlList[Index] as TFarScapeControl;
end;

function TFarScapeContainer.GetControlCount: integer;
begin
  result := ControlList.Count;
end;

procedure TFarScapeContainer.RemoveAllChildControls;
var
  c1: Integer;
  c : TFarScapeControl;
begin
  for c1 := ControlList.Count-1 downto 0 do
  begin
    c := ControlList[c1] as TFarScapeControl;
    RemoveControl(c);
  end;
end;

procedure TFarScapeContainer.InsertControl(const aControl: TFarScapeControl);
begin
  // IMPORTANT: Ordering is important!

  // 1) Add the control.
  if assigned(aControl.Parent) then raise EFarScapeException.Create('Control already has a parent.');
  ControlList.Add(aControl);
  aControl.FParent := self as TFarScapeControl;

  // 2) align new siblings...
  if aControl.Align <> TControlAlignment.caNone then self.RequestControlAlignment;

  // 3) Notify the root...
  if assigned(Root) then Root.ObjectHierarchyChanged;
end;

procedure TFarScapeContainer.RemoveControl(const aControl: TFarScapeControl);
begin
  // IMPORTANT: Ordering is important!

  // 1) Remove the control.
  if aControl.Parent <> self then raise EFarScapeException.Create('Control is not a child of this container control.');
  ControlList.Remove(aControl);
  aControl.fParent := nil;

  // 2) Align old siblings...
  if aControl.Align <> TControlAlignment.caNone then self.RequestControlAlignment;

  // 3) Remove the root for all children..
  UpdateRootForAllControls(aControl, nil);

  // 4) Notify the root...
  if assigned(Root) then Root.ObjectHierarchyChanged;
end;

procedure TFarScapeContainer.RequestControlAlignment;
begin
  if ControlCount > 0 then AlignChildControls;
end;

procedure TFarScapeContainer.SetAlignmentGridSize(const aRowCount, aColumnCount: integer);
begin
  assert(aRowCount >= 1);
  assert(aColumnCount >= 1);

  if (fGridRowCount <> aRowCount) or (fGridColumnCount <> aColumnCount) then
  begin
    fGridRowCount    := aRowCount;
    fGridColumnCount := aColumnCount;

    if ControlCount > 0 then AlignChildControls;
  end;
end;

procedure TFarScapeContainer.AlignChildControls;
begin
  ControlAlignmentAssistant.AlignChildControls(self as TFarScapeControl);
end;

{ TFarScapeControl }

procedure TFarScapeControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TFarScapeControl.MouseEnter;
begin

end;

procedure TFarScapeControl.MouseLeave;
begin

end;

procedure TFarScapeControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TFarScapeControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TFarScapeControl.PaintToDc(DC: HDC);
begin

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

procedure TFarScapeAbstractRoot.ObjectHierarchyChanged;
begin
  UpdateRootForAllControls(self, self);
  GetSceneInterface.RebuildScene;
end;

procedure TFarScapeAbstractRoot.ObjectLayoutChanged;
begin
  GetSceneInterface.UpdateScene;
end;

procedure TFarScapeAbstractRoot.InvalidateRootRegion(r: TRect);
begin
  //assert(assigned(Root));
  //if Root <> self then raise EFarScapeException.Create('InvalidateRegion() can only be called on root elements.');

  if assigned(OnInvalidateRootRegion) then OnInvalidateRootRegion(r);
end;



end.
