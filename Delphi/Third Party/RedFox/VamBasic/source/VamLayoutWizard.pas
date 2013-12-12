unit VamLayoutWizard;

interface

uses
  Classes, Controls, RedFox;

{$SCOPEDENUMS ON}

type
  TControlFeature = (LeftEdge, RightEdge, TopEdge, BottomEdge);

  TVamLayoutWizard = class;

  IVamLayoutWizard = interface
    ['{B109A63A-8EBB-425C-9CDF-550A5BF62433}']

    function Anchor(aAnchorControl : TControl):IVamLayoutWizard;
    function ClearAnchor:IVamLayoutWizard;

    function Move(OffsetX, OffsetY : integer):IVamLayoutWizard;

    function MatchWidth:IVamLayoutWizard;
    function MatchHeight:IVamLayoutWizard;

    function SnapToEdge(const SnapPoint : TControlFeature):IVamLayoutWizard;
    function SnapToParentEdge(const SnapPoint : TControlFeature):IVamLayoutWizard;

    function AlignEdge(const Edge : TControlFeature):IVamLayoutWizard;

    function AdjustBounds(const Left, Top, Right, Bottom:integer):IVamLayoutWizard;

    function SetSize(const aWidth, aHeight : integer):IVamLayoutWizard;
    function SetPos(const aLeft, aTop : integer):IVamLayoutWizard;
  end;

  // TVamLayoutWizard provides methods to help position controls at run time.
  TVamLayoutWizard = class(TPureInterfacedObject, IVamLayoutWizard)
  private
  protected
    fTarget : TControl;
    fAnchor : TControl;
  public
    constructor Create(aTargetControl : TControl);
    destructor Destroy; override;

    // Anchor() sets the control that is to be used as a reference point
    // which is required by a couple methods.
    function Anchor(aAnchorControl : TControl):IVamLayoutWizard;
    function ClearAnchor:IVamLayoutWizard;

    // Move the target control by offset amount.
    function Move(OffsetX, OffsetY : integer):IVamLayoutWizard;
    function MoveChildren(OffsetX, OffsetY : integer):IVamLayoutWizard;

    // Adjusts the target control to match the dimensions of the anchor control.
    function MatchWidth:IVamLayoutWizard;
    function MatchHeight:IVamLayoutWizard;

    // Moves the target control to butt against the edge of the anchor control.
    function SnapToEdge(const SnapPoint : TControlFeature):IVamLayoutWizard;
    function SnapToParentEdge(const SnapPoint : TControlFeature):IVamLayoutWizard;

    // Align's the control edge to match the anchor.
    function AlignEdge(const Edge : TControlFeature):IVamLayoutWizard;


    // AdjustBounds() changes the size of the control. Positive values will grow the control in
    // that direction. Negative values will shrink the control size. (all distances are in Pixels).
    function AdjustBounds(const Left, Top, Right, Bottom:integer):IVamLayoutWizard;

    function SetSize(const aWidth, aHeight : integer):IVamLayoutWizard;
    function SetPos(const aLeft, aTop : integer):IVamLayoutWizard;
  end;


function WidthOfControls(const ControlA, ControlB : TControl):integer;

implementation

uses
  SysUtils, Math;


function WidthOfControls(const ControlA, ControlB : TControl):integer;
var
  MinX, MaxX : integer;
begin
  MinX := Min(ControlA.Left, ControlB.Left);
  MaxX := Max(ControlA.Left + ControlA.Width, ControlB.Left + ControlB.Width);

  result := MaxX - MinX;
end;



{ TVamLayoutWizard }

constructor TVamLayoutWizard.Create(aTargetControl: TControl);
begin
  assert(assigned(aTargetControl));
  fTarget := aTargetControl;
end;

destructor TVamLayoutWizard.Destroy;
begin

  inherited;
end;

function TVamLayoutWizard.Anchor(aAnchorControl: TControl): IVamLayoutWizard;
begin
  fAnchor := aAnchorControl;
  result := self;
end;

function TVamLayoutWizard.ClearAnchor: IVamLayoutWizard;
begin
  fAnchor := nil;
  result := self;
end;

function TVamLayoutWizard.MatchHeight: IVamLayoutWizard;
begin
  assert(assigned(fAnchor));
  fTarget.Height := fAnchor.Height;
  result := self;
end;

function TVamLayoutWizard.MatchWidth: IVamLayoutWizard;
begin
  assert(assigned(fAnchor));
  fTarget.Width := fAnchor.Width;
  result := self;
end;

function TVamLayoutWizard.Move(OffsetX, OffsetY: integer): IVamLayoutWizard;
begin
  fTarget.Left := fTarget.Left + OffsetX;
  fTarget.Top  := fTarget.Top  + OffsetY;
  result := self;
end;

function TVamLayoutWizard.MoveChildren(OffsetX, OffsetY: integer): IVamLayoutWizard;
var
  aWinControl : TWinControl;
  c1: Integer;
begin
  if not(fTarget is TWinControl) then exit;

  aWinControl := (fTarget as TWinControl);

  for c1 := 0 to aWinControl.ControlCount-1 do
  begin
    aWinControl.Controls[c1].Top  := aWinControl.Controls[c1].Top  + OffsetY;
    aWinControl.Controls[c1].Left := aWinControl.Controls[c1].Left + OffsetX;
  end;

  result := self;
end;

function TVamLayoutWizard.SnapToEdge(const SnapPoint: TControlFeature): IVamLayoutWizard;
var
  PosX, PosY : integer;
begin
  assert(assigned(fAnchor));
  assert(assigned(fTarget));

  // Controls must have same parent container.
  assert(fAnchor.Parent = fTarget.Parent);

  case SnapPoint of
    TControlFeature.LeftEdge:
    begin
      PosX := (fAnchor.Left - fTarget.Width);
      PosY := fAnchor.Top + ((fAnchor.Height - fTarget.Height) div 2);
    end;

    TControlFeature.RightEdge:
    begin
      PosX := (fAnchor.Left + fAnchor.Width);
      PosY := fAnchor.Top + ((fAnchor.Height - fTarget.Height) div 2);
    end;

    TControlFeature.TopEdge:
    begin
      PosX := fAnchor.Left + ((fAnchor.Width - fTarget.Width) div 2);
      PosY := (fAnchor.Top  - fTarget.Height);
    end;

    TControlFeature.BottomEdge:
    begin
      PosX := fAnchor.Left + ((fAnchor.Width - fTarget.Width) div 2);
      PosY := (fAnchor.Top  + fAnchor.Height);
    end;

  else
    raise Exception.Create('Snap Point type not handled.');
  end;

  fTarget.Left := PosX;
  fTarget.Top  := PosY;

  result := self;
end;

function TVamLayoutWizard.SnapToParentEdge(const SnapPoint: TControlFeature): IVamLayoutWizard;
var
  PosX, PosY : integer;
begin
  case SnapPoint of
    TControlFeature.LeftEdge:
    begin
      fTarget.Left := 0
    end;

    TControlFeature.RightEdge:
    begin
      fTarget.Left := fTarget.Parent.ClientWidth - fTarget.Width;
    end;

    TControlFeature.TopEdge:
    begin
      fTarget.Top := 0;
    end;

    TControlFeature.BottomEdge:
    begin
      fTarget.Top := fTarget.Parent.ClientHeight - fTarget.Height;
    end;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

function TVamLayoutWizard.AlignEdge(const Edge: TControlFeature): IVamLayoutWizard;
var
  PosX, PosY : integer;
begin
  assert(assigned(fAnchor));
  assert(assigned(fTarget));

  PosX := fTarget.Left;
  PosY := fTarget.Top;

  case Edge of
    TControlFeature.LeftEdge:   PosX := fAnchor.Left;
    TControlFeature.RightEdge:  PosX := fAnchor.Left + fAnchor.Width - fTarget.Width;
    TControlFeature.TopEdge:    PosY := fAnchor.Top;
    TControlFeature.BottomEdge: PosY := fAnchor.Top + fAnchor.Height - fTarget.Height;
  end;

  fTarget.Left := PosX;
  fTarget.Top  := PosY;

  result := self;

end;



function TVamLayoutWizard.AdjustBounds(const Left, Top, Right, Bottom: integer): IVamLayoutWizard;
begin
  fTarget.Left := fTarget.Left - Left;
  fTarget.Top  := fTarget.Top  - Top;

  fTarget.Width  := fTarget.Width  + Left + Right;
  fTarget.Height := fTarget.Height + Top  + Bottom;

  result := self;
end;

function TVamLayoutWizard.SetSize(const aWidth, aHeight: integer): IVamLayoutWizard;
begin
  fTarget.Width  := aWidth;
  fTarget.Height := aHeight;
  result := self;
end;

function TVamLayoutWizard.SetPos(const aLeft, aTop: integer): IVamLayoutWizard;
begin
  fTarget.Left := aLeft;
  fTarget.Top  := aTop;
  result := self;
end;







end.
