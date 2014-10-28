unit VamTabs;

interface

uses
  Controls, Classes, Graphics, Types,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamTabs = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fTabs: TStrings;
    fTabIndex: integer;
    procedure SetTabs(const Value: TStrings);
    procedure SetTabIndex(const Value: integer);
    function GetTabCount: integer;

  protected
    TabDimensions : array of TRect;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed; inline;

    procedure TabsChanged(Sender : TObject);

    function TabIndexAt(PixelX, PixelY : integer):integer;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;



  published
    property TabIndex : integer  read fTabIndex    write SetTabIndex;
    property Tabs     : TStrings read fTabs        write SetTabs;
    property TabCount : integer  read GetTabCount;

    property Font;

    {$INCLUDE TControlProperties.inc}
    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

  end;

implementation

{ TVamTabs }

constructor TVamTabs.Create(AOwner: TComponent);
begin
  inherited;

  fTabIndex := -1;

  fTabs := TStringList.Create;
  TStringList(fTabs).OnChange := TabsChanged;

end;

destructor TVamTabs.Destroy;
begin
  fTabs.Free;
  SetLength(TabDimensions, 0);
  inherited;
end;

function TVamTabs.GetTabCount: integer;
begin
  result := fTabs.Count;
end;

procedure TVamTabs.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamTabs.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabUnderMouse : integer;
begin
  inherited;

  if (Button = TMouseButton.mbLeft) then
  begin
    TabUnderMouse := TabIndexAt(X, Y);
    if (TabUnderMouse <> fTabIndex) and (TabUnderMouse <> -1) then
    begin
      fTabIndex := TabUnderMouse;
      Invalidate;
      Changed;
    end;
  end;
end;

procedure TVamTabs.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TVamTabs.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

end;

procedure TVamTabs.SetTabIndex(const Value: integer);
begin
  if Value <> fTabIndex then
  begin
    fTabIndex := Value;
    Invalidate;
  end;
end;

procedure TVamTabs.SetTabs(const Value: TStrings);
begin
  fTabs.Assign(Value);
  Invalidate;
end;

function TVamTabs.TabIndexAt(PixelX, PixelY: integer): integer;
var
  c1: Integer;
begin
  for c1 := 0 to TabCount-1 do
  begin
    if InRect(Point(PixelX, PixelY), TabDimensions[c1]) then
    begin
      result := c1;
      exit; //==============>>exit>>=============>>
    end;
  end;

  //if we make it this far, there is no tab here, return -1.
  result := -1;
end;

procedure TVamTabs.TabsChanged(Sender: TObject);
begin
  if fTabIndex >= fTabs.Count then fTabIndex := fTabs.Count-1;

  SetLength(TabDimensions, fTabs.Count);

  Invalidate;
  Changed;
end;

procedure TVamTabs.Paint;
const
  kTabPadding  = 5; //padding around text inside tabs.
  kTabDistance = 4; //pixels between tabs.

  kColorTabOff = '$FFD3D3D3';
  kColorTabOn  = '$FFEEEEEE';
  kLineColor   = '$FFC0C0C0';
var
  c1: Integer;
  tw, th : single;
  OffsetX : integer;
  //OffsetY : single;
  x1,y1,x2,y2:integer;
  sx1,sy1,sx2,sy2:single;

  TextBounds : TRect;
  s : string;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  OffsetX := kTabDistance;
  //OffsetY := 0;


  for c1 := 0 to fTabs.Count-1 do
  begin
    BackBuffer.UpdateFont(self.Font);

    s := fTabs[c1];

    tw := BackBuffer.TextWidth(s);
    th := BackBuffer.TextHeight;

    //Get tab dimensions.
    x1 := OffsetX;
    x2 := OffsetX + round(tw) + (kTabPadding * 2);
    y1 := round(0);
    y2 := round(th) + (kTabPadding * 2);

    TabDimensions[c1] := Rect(x1, y1, x2, y2);

    sx1 := x1;
    sx2 := x2;
    sy1 := y1;
    sy2 := y2;


    if c1 = fTabIndex then
    begin
      BackBuffer.BufferInterface.LineWidth := 2;
      BackBuffer.BufferInterface.NoLine;
      BackBuffer.BufferInterface.FillColor := GetRedFoxColor(kColorTabOn);
      BackBuffer.BufferInterface.Rectangle(sx1, sy1, sx2, sy2);

      BackBuffer.BufferInterface.LineColor := GetRedFoxColor(kLineColor);

      sx1 := x1;
      sx2 := x2;
      sy1 := y1;
      sy2 := y2;

      BackBuffer.BufferInterface.Line(sx1, sy1, sx1, sy2);
      BackBuffer.BufferInterface.Line(sx2, sy1, sx2, sy2);
      //BackBuffer.BufferInterface.Line(sx1, sy1+1, sx2, sy1+1);
      BackBuffer.BufferInterface.Line(sx1, sy2, sx2, sy2);
    end else
    begin
      BackBuffer.BufferInterface.LineWidth := 2;
      BackBuffer.BufferInterface.NoLine;
      BackBuffer.BufferInterface.FillColor := GetRedFoxColor(kColorTabOff);
      BackBuffer.BufferInterface.Rectangle(sx1, sy1, sx2, sy2);


      BackBuffer.BufferInterface.LineColor := GetRedFoxColor(kLineColor);

      sx1 := x1;
      sx2 := x2;
      sy1 := y1;
      sy2 := y2;

      BackBuffer.BufferInterface.Line(sx1, sy1, sx1, sy2);
      BackBuffer.BufferInterface.Line(sx2, sy1, sx2, sy2);
      BackBuffer.BufferInterface.Line(sx1, sy1+1, sx2, sy1+1);
      BackBuffer.BufferInterface.Line(sx1, sy2, sx2, sy2);
    end;


    //Draw the tab text.
    TextBounds := Rect(x1,y1, x2, y2);
    BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, TextBounds);



    OffsetX := x2 + kTabDistance;
  end;



end;



end.
