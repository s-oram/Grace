unit RedFoxControl;

interface

uses
  Messages, Classes, Vcl.Controls, RedFoxImageBuffer, Types;

type
  TRedFoxControl = class(TControl)
  private
    fBackBuffer: TRedFoxImageBuffer;
    procedure WMPAINT(var Message: TWMPaint); message WM_Paint;
  protected
    IsGrabbed : boolean;
    OriginalPosition : TPoint;
    MouseDownScreenPos : TPoint;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure DrawDesignBoundary;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; virtual;

    procedure DoPaintBuffer; virtual;
    property BackBuffer:TRedFoxImageBuffer read fBackBuffer write fBackBuffer;
  published
  end;

implementation

uses
  Agg2d, AggWin32Bmp, Graphics,
  SysUtils, RedFox, RedFoxContainer;

{ TRedFoxControl }

constructor TRedFoxControl.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csOpaque];

  //if (assigned(AOwner)) and ((AOwner is TRedFoxContainer) = false) and ((AOwner is TRedFoxControl) = false) then raise Exception.Create('Owner must be a RedFox control.');

  BackBuffer := TRedFoxImageBuffer.Create;

  IsGrabbed := false;
end;

destructor TRedFoxControl.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TRedFoxControl.Paint;
begin
  if Parent is TRedFoxContainer then
  begin
    //(Parent as TRedFoxContainer).Invalidate;
  end;
end;

procedure TRedFoxControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    BackBuffer.Width := AWidth;
    BackBuffer.Height := AHeight;
  end;
end;

procedure TRedFoxControl.WMPAINT(var Message: TWMPaint);
begin
  Paint;
end;

procedure TRedFoxControl.DoPaintBuffer;
begin
  //Override this method to draw the control. The control should
  // be drawn to the backbuffer.
end;

procedure TRedFoxControl.DrawDesignBoundary;
begin
  BackBuffer.BufferInterface.SetLineColor(0,0,0,64);
  BackBuffer.BufferInterface.NoFill;
  BackBuffer.BufferInterface.LineWidth := 1;
  BackBuffer.BufferInterface.Rectangle(0.5,0.5,Width-0.5,Height-0.5);
end;

procedure TRedFoxControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  IsGrabbed := true;

  OriginalPosition.X := Left;
  OriginalPosition.Y := Top;

  MouseDownScreenPos := Mouse.CursorPos;
end;

procedure TRedFoxControl.MouseMove(Shift: TShiftState; X, Y: Integer);
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

procedure TRedFoxControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  BringToFront;
  IsGrabbed := false;
end;

end.
