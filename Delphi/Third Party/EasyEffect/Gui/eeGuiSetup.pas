{
  eeGuiSetup contains a bunch of routines for positioning GUI controls.
}

unit eeGuiSetup;

interface

uses
  Controls;

type
  TControlFeature = (cfLeftEdge, cfRightEdge, cfTopEdge, cfBottomEdge);

  TControlErector = record
    Anchor : TControl;  // Anchor is never modified.
    Target : TControl;  // Target is modified using Anchor as a reference point.

    MarginLeft   : integer;
    MarginTop    : integer;
    MarginRight  : integer;
    MarginBottom : integer;

    PaddingLeft   : integer;
    PaddingTop    : integer;
    PaddingRight  : integer;
    PaddingBottom : integer;

    function Init(const aAnchorControl, aTargetControl : TControl):TControlErector;

    // Moves the target control to butt against the edge of the anchor control.
    function SnapToEdge(const SnapPoint : TControlFeature):TControlErector;

    // Adjusts the target control to match the dimensions of the anchor control.
    function MatchWidth:TControlErector;
    function MatchHeight:TControlErector;

    // Move the target control by offset amount.
    function Move(OffsetX, OffsetY : integer):TControlErector;

  end;




implementation

uses
  SysUtils;

{ TControlErector }

function TControlErector.Init(const aAnchorControl, aTargetControl: TControl): TControlErector;
begin
  MarginLeft   := 0;
  MarginTop    := 0;
  MarginRight  := 0;
  MarginBottom := 0;

  PaddingLeft   := 0;
  PaddingTop    := 0;
  PaddingRight  := 0;
  PaddingBottom := 0;

  Anchor := aAnchorControl;
  Target := aTargetControl;
  result := self;
end;

function TControlErector.SnapToEdge(const SnapPoint: TControlFeature): TControlErector;
var
  PosX, PosY : integer;
begin
  assert(assigned(Anchor));
  assert(assigned(Target));
  result := self;

  // Controls must have same parent container.
  assert(Anchor.Parent = Target.Parent);

  case SnapPoint of
    cfLeftEdge:
    begin
      PosX := (Anchor.Left - Target.Width) - MarginRight;
      PosY := Anchor.Top + ((Anchor.Height - Target.Height) div 2);
    end;

    cfRightEdge:
    begin
      PosX := (Anchor.Left + Anchor.Width) + MarginLeft;
      PosY := Anchor.Top + ((Anchor.Height - Target.Height) div 2);
    end;

    cfTopEdge:
    begin
      PosX := Anchor.Left + ((Anchor.Width - Target.Width) div 2);
      PosY := (Anchor.Top  - Target.Height) - MarginBottom;
    end;

    cfBottomEdge:
    begin
      PosX := Anchor.Left + ((Anchor.Width - Target.Width) div 2);
      PosY := (Anchor.Top  + Anchor.Height) + MarginTop;
    end;

  else
    raise Exception.Create('Snap Point type not handled.');
  end;

  Target.Left := PosX;
  Target.Top  := PosY;
end;

function TControlErector.MatchHeight: TControlErector;
begin
  assert(assigned(Anchor));
  assert(assigned(Target));
  result := self;

  Target.Height := Anchor.Height;
end;

function TControlErector.MatchWidth: TControlErector;
begin
  assert(assigned(Anchor));
  assert(assigned(Target));
  result := self;

  Target.Width := Anchor.Width;
end;

function TControlErector.Move(OffsetX, OffsetY: integer): TControlErector;
begin
  assert(assigned(Anchor));
  assert(assigned(Target));
  result := self;

  Target.Left := Target.Left + OffsetX;
  Target.Top  := Target.Top  + OffsetY;
end;

end.




