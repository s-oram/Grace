unit FarScape.ControlHelper;

interface

uses
  FarScape.CustomControl,
  Types;

type
  TFarScapeControlHelper = class helper for TFarScapeControl
  public
    function IsChildOf(const PossibleParent : TFarScapeControl):boolean;
    function GetBounds : TRect;
    function GetBoundsInReferenceTo(const Target : TFarScapeControl):TRect;
    function GetAbsoluteOffset:TPoint; overload;
    function GetAbsoluteOffset(const aParent : TFarScapeControl):TPoint; overload;

    // Is the control and all parents visible?
    function IsShowing:boolean;
  end;

implementation

{ TFarScapeControlHelper }


function TFarScapeControlHelper.IsChildOf(const PossibleParent: TFarScapeControl): boolean;
var
  c : TFarScapecontrol;
begin
  if (self = PossibleParent) then exit(false);

  c := self;

  while assigned(c.Parent) do
  begin
    if c.Parent = PossibleParent
      then exit(true)
      else c := c.Parent;
  end;

  // if we make it this far...
  result := false;
end;


function TFarScapeControlHelper.IsShowing: boolean;
var
  c : TFarScapeControl;
begin
  c := self;
  while assigned(c) do
  begin
    if not c.Visible then exit(false); //======= exit =====>>
    c := c.Parent;
  end;

  // If we make it this far, the control is showing.
  result := true;
end;

function TFarScapeControlHelper.GetAbsoluteOffset: TPoint;
begin
  result := GetAbsoluteOffset(self.Root);
end;

function TFarScapeControlHelper.GetAbsoluteOffset(const aParent: TFarScapeControl): TPoint;
var
  c : TFarScapeControl;
  OffsetX, OffsetY : integer;
begin
  if not assigned(aParent) then exit(Point(0,0));
  if self = aParent then exit(Point(0,0));

  assert(self.IsChildOf(aParent));

  c := self;
  OffsetX := 0;
  OffsetY := 0;

  while assigned(c.Parent) and (c <> aParent) do
  begin
    OffsetX := OffsetX + c.Left;
    OffsetY := OffsetY + c.Top;
    c := c.Parent;
  end;

  result.X := OffsetX;
  result.Y := OffsetY;
end;

function TFarScapeControlHelper.GetBounds: TRect;
begin
  result := Rect(0,0, self.Width, self.Height);
end;

function TFarScapeControlHelper.GetBoundsInReferenceTo(const Target: TFarScapeControl): TRect;
var
  OffsetX, OffsetY : integer;
  c : TFarScapeControl;
begin
  assert(assigned(Target));
  assert(assigned(self));

  if self = Target then exit(Rect(0, 0, self.Width, self.Height));

  assert(self.IsChildOf(Target));

  c := self;

  OffSetX := 0;
  OffsetY := 0;

  while (assigned(c.Parent)) and (c <> Target) do
  begin
    OffSetX := OffsetX + c.Left;
    OffsetY := OffsetY + c.Top;
    c := c.Parent;
  end;

  result := Rect(OffsetX, OffsetY, OffsetX + self.Width, OffsetY + self.Height);
end;



end.
