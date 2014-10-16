unit VamLib.VclLayout;

interface

uses
  Controls;

type
  IVclLayoutWizard = interface;
  IVclMultiLayoutWizard = interface;

  IVclLayoutWizard = interface
    ['{61A58D4D-3A21-4D48-9B7D-727E3DD5AC2B}']

    function SnapToLeftEdge:IVclLayoutWizard;
    function SnapToRightEdge:IVclLayoutWizard;
    function SnapToTopEdge:IVclLayoutWizard;
    function SnapToBottomEdge:IVclLayoutWizard;

    function Move(const dX, dY : integer):IVclLayoutWizard;
  end;

  IVclMultiLayoutWizard = interface
    ['{1AB4FC31-5002-40C4-A5B2-9FA206EB64E5}']


    // resizes and spreads all controls to fit the width of the parent control.
    function FitToParentWidth(const Spacing : integer):IVclMultiLayoutWizard;
  end;



function VclLayout(const Target : TControl):IVclLayoutWizard; overload;
function VclLayout(const Target, Anchor : TControl):IVclLayoutWizard; overload;


function VclLayout:IVclLayoutWizard overload;
function VclLayout(const Targets : array of TControl):IVclMultiLayoutWizard; overload;


//=== private use only ====

type
  TVclLayoutWizard = class(TInterfacedObject, IVclLayoutWizard)
  private
  protected
    fTarget, fAnchor : TControl;
  public
    constructor Create(const aTarget : TControl);

    function SnapToLeftEdge:IVclLayoutWizard;
    function SnapToRightEdge:IVclLayoutWizard;
    function SnapToTopEdge:IVclLayoutWizard;
    function SnapToBottomEdge:IVclLayoutWizard;

    function Move(const dX, dY : integer):IVclLayoutWizard;
  end;

  TVclMultiLayoutWizard = class(TInterfacedObject, IVclMultiLayoutWizard)
  private
    fTargets : array of TControl;
    fTargetCount : integer;
  public
    constructor Create(const aTargets : array of TControl);
    destructor Destroy; override;

    function FitToParentWidth(const Spacing : integer):IVclMultiLayoutWizard;
  end;

implementation

uses
  Math;

function VclLayout(const Target : TControl):IVclLayoutWizard;
begin
  result := TVclLayoutWizard.Create(Target);
end;

function VclLayout(const Target, Anchor : TControl):IVclLayoutWizard;
var
  wiz : TVclLayoutWizard;
begin
  wiz := TVclLayoutWizard.Create(Target);
  wiz.fAnchor := Anchor;
  result := wiz;
end;

function VclLayout:IVclLayoutWizard overload;
begin
  result := TVclLayoutWizard.Create(nil);
end;

function VclLayout(const Targets : array of TControl):IVclMultiLayoutWizard; overload;
var
  wiz : TVclMultiLayoutWizard;
begin
  wiz := TVclMultiLayoutWizard.Create(Targets);
  result := wiz;
end;

{ TVclLayoutWizard }

constructor TVclLayoutWizard.Create(const aTarget: TControl);
begin
  fAnchor := nil;
  fTarget := aTarget;
end;

function TVclLayoutWizard.SnapToLeftEdge: IVclLayoutWizard;
begin
  assert(assigned(fAnchor), 'Anchor not assigned.');
  assert(fAnchor.Parent = fTarget.Parent, 'Controls must share parent.');

  assert(false, 'TODO');
  result := self;
end;

function TVclLayoutWizard.SnapToRightEdge: IVclLayoutWizard;
begin
  assert(assigned(fAnchor), 'Anchor not assigned.');
  assert(fAnchor.Parent = fTarget.Parent, 'Controls must share parent.');

  assert(false, 'TODO');
  result := self;
end;

function TVclLayoutWizard.SnapToTopEdge: IVclLayoutWizard;
begin
  assert(assigned(fAnchor), 'Anchor not assigned.');
  assert(fAnchor.Parent = fTarget.Parent, 'Controls must share parent.');

  assert(false, 'TODO');
  result := self;
end;

function TVclLayoutWizard.Move(const dX, dY: integer): IVclLayoutWizard;
begin
  fTarget.Left := fTarget.Left + dx;
  fTarget.Top  := fTarget.Top  + dy;
end;

function TVclLayoutWizard.SnapToBottomEdge: IVclLayoutWizard;
begin
  assert(assigned(fAnchor), 'Anchor not assigned.');
  assert(fAnchor.Parent = fTarget.Parent, 'Controls must share parent.');

  fTarget.Top := fAnchor.Top + fAnchor.Height;

  result := self;
end;



{ TVclMultiLayoutWizard }

constructor TVclMultiLayoutWizard.Create(const aTargets: array of TControl);
var
  c1: Integer;
begin
  assert(Length(aTargets) > 0);
  fTargetCount := Length(aTargets);
  SetLength(fTargets, fTargetCount);
  for c1 := 0 to fTargetCount-1 do
  begin
    fTargets[c1] := aTargets[c1];
  end;
end;

destructor TVclMultiLayoutWizard.Destroy;
begin
  SetLength(fTargets, 0);
  inherited;
end;

function TVclMultiLayoutWizard.FitToParentWidth(const Spacing: integer): IVclMultiLayoutWizard;
var
  TotalSpace : integer;
  WidthOfControl : integer;
  Parent : TWinControl;
  c1: Integer;
  LastControl : TControl;
  TotalWidth : integer;
begin
  assert(fTargetCount > 0);

  // assume all controls share the same parent.
  Parent := fTargets[0].Parent;
  assert(assigned(Parent));

  TotalSpace := Spacing * (fTargetCount - 1);
  //TotalSpace := 0;
  TotalWidth := Parent.Width - Parent.Padding.Left - Parent.Padding.Right;
  WidthOfControl := Floor((TotalWidth - TotalSpace) / fTargetCount);

  for c1 := 0 to fTargetCount-1 do
  begin
    fTargets[c1].Width := WidthOfControl;
    fTargets[c1].Left  := Parent.Padding.Left + round(c1 / fTargetCount * TotalWidth) + round(c1 / (fTargetCount-1) * Spacing);
  end;

  // set the position of the last control relative to the parent edge. This
  // is needed in case the spacing between the controls is uneven.
  LastControl := fTargets[fTargetCount-1];
  LastControl.Width := WidthOfControl;
  LastControl.Left := Parent.Width - LastControl.Width - Parent.Padding.Right;

  // TODO:LOW if wanting to be really precise, I could locate the difference between
  // the two methods of postioning the last control (snapping to the right boundary
  // or calculating as  a fraction of the total width in the loop above.)
  // That difference would be an error factor. That error factor could then
  // be fed into the loop that positions the other controls.
  // The error is probably not noticable in most situations and is probably never
  // more than one or two pixels anyway.

  result := self;
end;

end.
