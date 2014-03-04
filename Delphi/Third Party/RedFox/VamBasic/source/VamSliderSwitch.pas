unit VamSliderSwitch;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamSliderType = (stVert, stHorz);


  TVamSliderSwitch = class(TVamWinControl)
  private
    fEnabled: boolean;
    fOnChanged: TNotifyEvent;
    fSliderType: TVamSliderType;
    fSwitchSteps: integer;
    fSwitchPos: integer;
    fBackgroundImage: TBitmap;
    fIndexImage: TBitmap;
    function GetColor(const Index: Integer): TRedFoxColorString;
    procedure SetColor(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetEnabled(const Value: boolean);
    procedure SetSliderType(const Value: TVamSliderType);
    procedure SetSwitchPos(const Value: integer);
  protected
    IsGrabbed : boolean;
    MoveReferencePoint : TPoint;
    ReferencePoint   : integer;
    ReferencePos     : single;
    IsFineAdjustment : boolean;

    ThrowDist : integer;
    IndexBounds : TRect;

    fColorBackground, fColorIndex : TRedFoxColor;

    IndexPos : single;

    function IsIndexAt(x, y : integer):boolean;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed;
    procedure Paint; override;

    property SliderType : TVamSliderType read fSliderType write SetSliderType;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property BackgroundImage : TBitmap read fBackgroundImage write fBackgroundImage;
    property IndexImage      : TBitmap read fIndexImage      write fIndexImage;
  published
    property ColorBackground : TRedFoxColorString index 0 read GetColor write SetColor;
    property ColorIndex      : TRedFoxColorString index 1 read GetColor write SetColor;

    property Enabled      : boolean read fEnabled   write SetEnabled;

    property SwitchSteps : integer      read fSwitchSteps write fSwitchSteps; //range 2+
    property SwitchPos   : integer      read fSwitchPos   write SetSwitchPos;
    property OnChanged   : TNotifyEvent read fOnChanged   write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils,
  VamLib.Utils;

{ TVamSliderSwitch }

constructor TVamSliderSwitch.Create(AOwner: TComponent);
begin
  inherited;

  fColorBackground := '$FF666666';
  fColorIndex      := '$FFccccFF';

  IndexPos := 0;

  SliderType := TVamSliderType.stHorz;

  ThrowDist  := 0;
  IndexBounds := Rect(0,0,0,0);

  SwitchSteps := 4;
end;

destructor TVamSliderSwitch.Destroy;
begin

  inherited;
end;

procedure TVamSliderSwitch.Changed;
begin
  if assigned(OnChanged) then OnChanged(Self);

end;

procedure TVamSliderSwitch.SetColor(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColor;
begin
  case Index of
  0: pc := @fColorBackground;
  1: pc := @fColorIndex;
  else
    pc := nil;
  end;

  if (assigned(pc)) and (pc^.AsString <> Value) then
  begin
    pc^ := Value;
    Invalidate;
  end;
end;

function TVamSliderSwitch.GetColor(const Index: Integer): TRedFoxColorString;
begin
  case Index of
  0: result := fColorBackground;
  1: result := fColorIndex;
  else
    raise Exception.Create('Index not handled.');
  end;
end;

procedure TVamSliderSwitch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (IsIndexAt(x, y)) and (ThrowDist <> 0) then
  begin
    IsGrabbed := true;

    case SliderType of
      stVert: ReferencePoint := Height-Y;
      stHorz: ReferencePoint := X;
    else
      raise Exception.Create('Type not handled.');
    end;

    ReferencePos := IndexPos;
  end;

end;

procedure TVamSliderSwitch.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewRefPoint : integer;
  Dist : integer;
  NewPos : single;
  NewSwitchPos : integer;
begin
  inherited;


  if IsGrabbed then
  begin
    case SliderType of
      stVert:
      begin
        NewRefPoint := Height-Y;
        Dist := Height;
      end;
      stHorz:
      begin
        NewRefPoint := X;
        Dist := Width;
      end
    else
      raise Exception.Create('Type not handled.');
    end;

    NewPos := NewRefPoint / Dist;
    NewPos := Clamp(NewPos, 0, 1);
    NewPos := ExpandFloat(NewPos, 0, (SwitchSteps-1)) / (SwitchSteps-1);

    if NewPos <> IndexPos then
    begin
      IndexPos := NewPos;
      Invalidate;
    end;

    NewSwitchPos := ExpandFloat(NewPos, 0, (SwitchSteps-1));
    if NewSwitchPos <> fSwitchPos then
    begin
      fSwitchPos := NewSwitchPos;
      Changed;
    end;



  end;

end;

procedure TVamSliderSwitch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Button = mbLeft then
  begin
    IsGrabbed := false;
  end;

end;

function TVamSliderSwitch.IsIndexAt(x, y: integer): boolean;
begin
  result := IndexBounds.Contains(Point(x,y));
end;



procedure TVamSliderSwitch.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

procedure TVamSliderSwitch.SetEnabled(const Value: boolean);
begin
  fEnabled := Value;
end;

procedure TVamSliderSwitch.SetSliderType(const Value: TVamSliderType);
begin
  fSliderType := Value;
end;

procedure TVamSliderSwitch.SetSwitchPos(const Value: integer);
begin
  if not IsGrabbed then
  begin
    fSwitchPos := Clamp(Value, 0, fSwitchSteps-1);
    IndexPos := fSwitchPos / (fSwitchSteps-1);
    Invalidate;
  end;
end;

procedure TVamSliderSwitch.Paint;
var
  x1, y1, x2, y2 : single;
  IndexWidth, IndexHeight : integer;
begin
  inherited;

  if assigned(BackgroundImage) then
  begin

  end else
  begin
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.FillColor := fColorBackground;

    x1 := 0;
    y1 := 0;
    x2 := Width;
    y2 := Height;
    BackBuffer.BufferInterface.Rectangle(x1,y1,x2,y2);
  end;


  if assigned(IndexImage) then
  begin



  end else
  begin
    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.FillColor := fColorIndex;

    case SliderType of
      stVert:
      begin
        IndexWidth := Width;
        IndexHeight := IndexWidth;
      end;

      stHorz:
      begin
        IndexHeight := Height;
        IndexWidth := IndexHeight;

        ThrowDist := Width - IndexWidth;

        x1 := ThrowDist * IndexPos;
        x2 := x1 + IndexWidth;
        y1 := 0;
        y2 := Width;

        BackBuffer.BufferInterface.Rectangle(x1,y1,x2,y2);

        IndexBounds := Rect(round(x1), round(y1), round(x2), round(y2));
      end;
    else
      raise Exception.Create('Type not handled.');
    end;
  end;


end;



end.
