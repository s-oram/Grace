unit RedFoxCustomMoveableControl;

interface

uses
  Types, Controls, Classes, RedFoxCustomControl;

type
  TRedFoxCustomMoveableControl = class(TRedFoxCustomControl)
  private
  protected
    IsGrabbed : boolean;
    OriginalPosition : TPoint;
    MouseDownScreenPos : TPoint;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TRedFoxCustomMoveableControl }

constructor TRedFoxCustomMoveableControl.Create(AOwner: TComponent);
begin
  inherited;
  IsGrabbed := false;
end;

destructor TRedFoxCustomMoveableControl.Destroy;
begin

  inherited;
end;

procedure TRedFoxCustomMoveableControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  IsGrabbed := true;

  OriginalPosition.X := Left;
  OriginalPosition.Y := Top;

  MouseDownScreenPos := Mouse.CursorPos;
end;

procedure TRedFoxCustomMoveableControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPos : TPoint;
  CurrentMousePos : TPoint;
begin
  inherited;

  if IsGrabbed then
  begin
    CurrentMousePos := Mouse.CursorPos;

    NewPos.X :=  OriginalPosition.X + (CurrentMousePos.X - MouseDownScreenPos.X);
    NewPos.Y :=  OriginalPosition.Y + (CurrentMousePos.Y - MouseDownScreenPos.Y);

    if (NewPos.X <> Left) or (NewPos.Y <> Top) then
    begin
      Left := NewPos.X;
      Top  := NewPos.Y;
    end;
  end;

end;

procedure TRedFoxCustomMoveableControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  BringToFront;
  IsGrabbed := false;
end;

end.
