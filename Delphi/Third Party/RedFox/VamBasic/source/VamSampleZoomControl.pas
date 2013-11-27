{
  TVamSampleZoomControl is a special purpose control to designed for
  zooming/scrolling sample displays, similar in function to a scroll bar. The
  user can set both the zoom factor and scroll position of the sample display
  with one control.

}


unit VamSampleZoomControl;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TGrabType = (gtNone, gtSetNewArea, gtMoveArea, gtMoveIndexA, gtMoveIndexB);

  TVamSampleZoomControl = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fIndexB: single;
    fIndexA: single;
    procedure SetIndexA(const Value: single);
    procedure SetIndexB(const Value: single);
  protected
    IsGrabbed : boolean;
    GrabbedMode : TGrabType;
    HasMoved : boolean;

    WasDoubleClicked : boolean;

    MouseDownPos : TPoint;

    GrabIndexA, GrabIndexB : single;

    ReferenceIndexA, ReferenceIndexB : single;
    ReferenceDist : single;


    ReferencePoint   : TPoint;
    ReferencePos     : single;
    IsFineAdjustment : boolean;


    procedure DblClick; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure Changed;


    function IsMouseOverIndexA(const PixelX, ControlWidth : integer):boolean;
    function IsMouseOverIndexB(const PixelX, ControlWidth : integer):boolean;
    procedure Paint; override;


    function IsFullZoomOut : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published



    property IndexA : single read fIndexA write SetIndexA; //range 0..1
    property IndexB : single read fIndexB write SetIndexB; //range 0..1

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  AggPixelFormat, Math;

{ TVamSampleZoomControl }

constructor TVamSampleZoomControl.Create(AOwner: TComponent);
begin
  inherited;

  fIndexA := 0;
  fIndexB := 1;

end;

destructor TVamSampleZoomControl.Destroy;
begin

  inherited;
end;

function TVamSampleZoomControl.IsFullZoomOut: boolean;
begin
  if ((IndexA = 0) and (IndexB = 1)) or ((IndexA = 1) and (IndexB = 0))
    then result := true
    else result := false;

end;

function TVamSampleZoomControl.IsMouseOverIndexA(const PixelX, ControlWidth: integer): boolean;
begin
  if abs((IndexA * ControlWidth) - PixelX) < 5
    then result := true
    else result := false;
end;

function TVamSampleZoomControl.IsMouseOverIndexB(const PixelX, ControlWidth: integer): boolean;
begin
  if abs((IndexB * ControlWidth) - PixelX) < 5
    then result := true
    else result := false;
end;

procedure TVamSampleZoomControl.SetIndexA(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));

  if Value <> fIndexA then
  begin
    fIndexA := Value;
    Invalidate;
  end;
end;

procedure TVamSampleZoomControl.SetIndexB(const Value: single);
begin
  assert((Value >= 0) and (Value <= 1));

  if Value <> fIndexB then
  begin
    fIndexB := Value;
    Invalidate;
  end;
end;

procedure TVamSampleZoomControl.DblClick;
begin
  inherited;
  WasDoubleClicked := true;
end;



procedure TVamSampleZoomControl.MouseEnter;
begin
  inherited;

end;

procedure TVamSampleZoomControl.MouseLeave;
begin
  inherited;
  Cursor := crDefault;
end;

procedure TVamSampleZoomControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  PosX1, PosX2 : single;
  gx, dx1, dx2 : single;
begin
  inherited;

  //=== Invoke special behaviour if the control was double clicked. ===
  if WasDoubleClicked then
  begin
    WasDoubleClicked := false;

    if IndexA < IndexB then
    begin
      PosX1 := IndexA;
      PosX2 := IndexB;
    end else
    begin
      PosX1 := IndexB;
      PosX2 := IndexA;
    end;

    if ((PosX1 = 0) and (PosX2 = 1)) = false then
    begin
      fIndexA := 0;
      fIndexB := 1;
    end else
    begin
      gx := x / (Width-1);
      if gx > 1 then gx := 1;
      if gx < 0 then gx := 0;

      dx1 := (gx - 0.125);
      dx2 := (gx + 0.125);

      if dx1 < 0 then
      begin
        dx1 := 0;
        dx2 := 0.25;
      end;

      if dx2 > 1 then
      begin
        dx1 := 0.75;
        dx2 := 1;
      end;

      fIndexA := dx1;
      fIndexB := dx2;
    end;

    Invalidate;
    Changed;

    exit; //===========================>> exit >>============>>
  end;
  //=== END special double click behaviour ===





  if (Button = mbLeft) and (IsFullZoomOut = false) then
  begin
    IsGrabbed := true;
    GrabbedMode := gtNone;

    GrabIndexA := x / (Width-1);
    if GrabIndexA > 1 then GrabIndexA := 1;
    if GrabIndexA < 0 then GrabIndexA := 0;

    if IndexA < IndexB then
    begin
      PosX1 := IndexA;
      PosX2 := IndexB;
    end else
    begin
      PosX1 := IndexB;
      PosX2 := IndexA;
    end;

    ReferenceIndexA := PosX1;
    ReferenceIndexB := PosX2;
    ReferenceDist   := PosX2 - PosX1;


    // Detect what type of move operation is required depending on where the
    // control is clicked.
    if IsMouseOverIndexA(X, Width) then
    begin
      GrabbedMode := gtMoveIndexA;
      ReferenceIndexA := IndexA;
      ReferenceIndexB := IndexB;
    end else
    if IsMouseOverIndexB(X, Width) then
    begin
      GrabbedMode := gtMoveIndexB;
      ReferenceIndexA := IndexA;
      ReferenceIndexB := IndexB;
    end else
    if (PosX1 = 0) and (PosX2 = 1) then
    begin
      GrabbedMode := gtSetNewArea;
      HasMoved := false;
    end else
    if ((GrabIndexA < PosX1) or (GrabIndexA > PosX2)) and (ssShift in Shift) then
    begin
      GrabbedMode := gtSetNewArea;
      HasMoved := false;
    end else
    if ((GrabIndexA < PosX1) or (GrabIndexA > PosX2)) and (not(ssShift in Shift)) then
    begin
      GrabbedMode := gtMoveArea;
      HasMoved := true;

      PosX1 := GrabIndexA - (ReferenceDist) * 0.5;
      PosX2 := GrabIndexA + (ReferenceDist) * 0.5;

      if PosX1 < 0 then
      begin
        PosX1 := 0;
        PosX2 := ReferenceDist;
      end;

      if PosX2 > 1 then
      begin
        PosX2 := 1;
        PosX1 := 1 - ReferenceDist;
      end;

      ReferenceIndexA := PosX1;
      ReferenceIndexB := PosX2;
      ReferenceDist   := PosX2 - PosX1;

      IndexA := PosX1;
      IndexB := PosX2;

      Invalidate;
      Changed;

      MouseDownPos := Point(x, y);
    end else
    if (GrabIndexA >= PosX1) and (GrabIndexA <= PosX2) then
    begin
      GrabbedMode := gtMoveArea;
      MouseDownPos := Point(x, y);
    end;

  end;


  if (Button = mbLeft) and (IsFullZoomOut = true) then
  begin
    IsGrabbed := true;
    GrabbedMode := gtNone;

    GrabIndexA := x / (Width-1);
    if GrabIndexA > 1 then GrabIndexA := 1;
    if GrabIndexA < 0 then GrabIndexA := 0;

    if IndexA < IndexB then
    begin
      PosX1 := IndexA;
      PosX2 := IndexB;
    end else
    begin
      PosX1 := IndexB;
      PosX2 := IndexA;
    end;

    ReferenceIndexA := PosX1;
    ReferenceIndexB := PosX2;
    ReferenceDist   := PosX2 - PosX1;

    GrabbedMode := gtSetNewArea;
    HasMoved := false;
  end;


end;

procedure TVamSampleZoomControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  gx : single;
  gOffset : single;
  PosX1, PosX2 : single;
begin
  inherited;

  if (IsGrabbed = false) then
  begin
    if IsMouseOverIndexA(X, Width) then
    begin
      if Cursor <> crSizeWE then Cursor := crSizeWE;
    end else
    if IsMouseOverIndexB(X, Width) then
    begin
      if Cursor <> crSizeWE then Cursor := crSizeWE;
    end else
    begin
      if Cursor <> crDefault then Cursor := crDefault;
    end;
  end;


  if (IsGrabbed) and (GrabbedMode = gtSetNewArea) then
  begin
    HasMoved := true;

    GrabIndexB := x / (Width-1);
    if GrabIndexB > 1 then GrabIndexB := 1;
    if GrabIndexB < 0 then GrabIndexB := 0;

    if (GrabIndexA <> fIndexA) or (GrabIndexB <> fIndexB) then
    begin
      fIndexA := GrabIndexA;
      fIndexB := GrabIndexB;
      Invalidate;
      Changed;
    end;
  end;

  if (IsGrabbed) and (GrabbedMode = gtMoveArea) then
  begin
    gx := x / (Width-1);
    gOffset := gx - GrabIndexA;
    PosX1 := ReferenceIndexA + gOffset;
    PosX2 := ReferenceIndexB + gOffset;

    if PosX1 < 0 then
    begin
      PosX1 := 0;
      PosX2 := ReferenceDist;
    end;

    if PosX2 > 1 then
    begin
      PosX1 := 1 - ReferenceDist;
      PosX2 := 1;
    end;

    if (PosX1 <> fIndexA) or (PosX2 <> fIndexB) then
    begin
      fIndexA := PosX1;
      fIndexB := PosX2;
      Invalidate;
      Changed;
    end;
  end;

  if (IsGrabbed) and (GrabbedMode = gtMoveIndexA) then
  begin
    gx := x / (Width-1);
    gOffset := gx - GrabIndexA;
    PosX1 := ReferenceIndexA + gOffset;
    if PosX1 < 0 then PosX1 := 0;
    if PosX1 > 1 then PosX1 := 1;
    fIndexA := PosX1;

    Invalidate;
    Changed;
  end;

  if (IsGrabbed) and (GrabbedMode = gtMoveIndexB) then
  begin
    gx := x / (Width-1);
    gOffset := gx - GrabIndexA;
    PosX1 := ReferenceIndexB + gOffset;
    if PosX1 < 0 then PosX1 := 0;
    if PosX1 > 1 then PosX1 := 1;
    fIndexB := PosX1;

    Invalidate;
    Changed;
  end;


end;

procedure TVamSampleZoomControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  gx, dx1, dx2 : single;
begin
  inherited;

  if (Button = mbLeft) and (IsGrabbed) and (GrabbedMode = gtSetNewArea) then
  begin
    IsGrabbed := false;
    GrabbedMode := gtNone;

    if HasMoved = true then
    begin
      GrabIndexB := x / (Width-1);
      if GrabIndexB > 1 then GrabIndexB := 1;
      if GrabIndexB < 0 then GrabIndexB := 0;

      if (GrabIndexA <> fIndexA) or (GrabIndexB <> fIndexB) then
      begin
        fIndexA := GrabIndexA;
        fIndexB := GrabIndexB;
        Invalidate;
        Changed;
      end;
    end;

    if HasMoved = false then
    begin
      gx := x / (Width-1);
      if gx > 1 then gx := 1;
      if gx < 0 then gx := 0;

      dx1 := (gx - ReferenceDist * 0.5);
      dx2 := (gx + ReferenceDist * 0.5);

      if dx1 < 0 then
      begin
        dx1 := 0;
        dx2 := ReferenceDist;
      end else
      if dx2 > 1 then
      begin
        dx1 := 1 - ReferenceDist;
        dx2 := 1;
      end;

      fIndexA := dx1;
      fIndexB := dx2;

      Invalidate;
      Changed;
    end;
  end;


  if (Button = mbLeft) and (IsGrabbed) and (GrabbedMode = gtMoveArea) then
  begin
    IsGrabbed := false;
    GrabbedMode := gtNone;
    Invalidate;
    Changed;
  end;

  if (Button = mbLeft) and (IsGrabbed) and (GrabbedMode = gtMoveIndexA) then
  begin
    IsGrabbed := false;
    GrabbedMode := gtNone;
    Invalidate;
    Changed;
  end;

  if (Button = mbLeft) and (IsGrabbed) and (GrabbedMode = gtMoveIndexB) then
  begin
    IsGrabbed := false;
    GrabbedMode := gtNone;
    Invalidate;
    Changed;
  end;





end;



procedure TVamSampleZoomControl.Changed;
begin
  if assigned(OnChanged) then OnChanged(Self);
end;

procedure TVamSampleZoomControl.Paint;
var
  PosX1, PosX2 : single;
  x1, x2 : single;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);


  if IndexA < IndexB then
  begin
    PosX1 := IndexA;
    PosX2 := IndexB;
  end else
  begin
    PosX1 := IndexB;
    PosX2 := IndexA;
  end;

  if IsFullZoomOut = false then
  begin
    //== Draw a semi-opaque fill over the entire control ==
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSource;
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.FillColor := GetAggColor(clBlack, 100);
    BackBuffer.BufferInterface.Rectangle(0,0,Width, Height);

    //== erase the index area ==
    x1 := PosX1 * (Width);
    x2 := PosX2 * (Width);
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmClear;
    BackBuffer.BufferInterface.Rectangle(x1,0,x2, Height);


    //== Draw selection endpoint lines ==
    BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSource;
    BackBuffer.BufferInterface.NoFill;
    BackBuffer.BufferInterface.LineColor := GetAggColor(clWhite, 240);
    BackBuffer.BufferInterface.LineWidth := 1;
    x1 := ceil(PosX1 * Width) - 0.5;
    if x1 < 0 then x1 := 0.5;
    x2 := floor(PosX2 * Width) + 0.5;
    if x2 > Width then x2 := Width-0.5;


    BackBuffer.BufferInterface.Line(x1, 0, x1, Height);
    BackBuffer.BufferInterface.Line(x2, 0, x2, Height);
  end;

end;



end.
