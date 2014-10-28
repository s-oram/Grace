unit VamTabPanel;

interface

uses
  Types, Controls, Classes, RedFox, RedFoxGraphicControl, RedFoxColor, RedFoxImageBuffer,
  VamGraphicControl, VamWinControl;

type
  {$SCOPEDENUMS ON}
  TTabPosition = (tpAboveLeft, tpAboveRight, tbAboveCenter, tpBelowLeft, tpBelowRight, tpBelowCenter);

  TVamTabPanel = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fTabs: TStrings;
    fTabIndex: integer;
    fTabHeight: integer;
    fColor_TabOff: TRedFoxColorString;
    fColor_Background: TRedFoxColorString;
    fColor_TabOn: TRedFoxColorString;
    fTabOffset: integer;
    fTabPadding: integer;
    fTabSpace: integer;
    fTabPosition: TTabPosition;
    function GetTabCount: integer;
    procedure SetTabIndex(const Value: integer);
    procedure SetTabs(const Value: TStrings);
    procedure SetTabHeight(const Value: integer);
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    function GetCornerRadius(const Index: Integer): double;
    procedure SetCornerRadius(const Index: Integer; const Value: double);
    procedure SetTabOffset(const Value: integer);
    procedure SetTabPadding(const Value: integer);
    procedure SetTabSpace(const Value: integer);
    procedure SetTabPosition(const Value: TTabPosition);
  protected
    fCornerRadius : array[0..3] of double;
    TabDimensions : array of TRect;
    MouseOverPadIndex : integer;

    function GetClientRect:TRect; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure TabsChanged(Sender : TObject);
    procedure Changed;

    procedure Paint; override;
    procedure DrawTabs_LeftOrRight; inline;
    procedure DrawTabs_BelowCenter;
    procedure DrawTabs_AboveCenter;

    function TabIndexAt(PixelX, PixelY : integer):integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    //==========================================================================
    //        Appearence settings
    //==========================================================================
    property Color_Background : TRedFoxColorString index 0 read fColor_Background write SetColors;
    property Color_TabOff     : TRedFoxColorString index 1 read fColor_TabOff     write SetColors;
    property Color_TabOn      : TRedFoxColorString index 2 read fColor_TabOn      write SetColors;

    property CornerRadius1 : double index 0 read GetCornerRadius write SetCornerRadius;
    property CornerRadius2 : double index 1 read GetCornerRadius write SetCornerRadius;
    property CornerRadius3 : double index 2 read GetCornerRadius write SetCornerRadius;
    property CornerRadius4 : double index 3 read GetCornerRadius write SetCornerRadius;

    property TabHeight  : integer  read fTabHeight   write SetTabHeight;  // Height of tab in pixels.
    property TabOffset  : integer  read fTabOffset   write SetTabOffset;  // controls distance from the edge the first tab is drawn (pixels)
    property TabPadding : integer  read fTabPadding  write SetTabPadding; // controls padding amount inside a tab. (pixels)
    property TabSpace   : integer  read fTabSpace    write SetTabSpace;   // controls amount of space between tabs (pixels)
    property TabPosition : TTabPosition read fTabPosition write SetTabPosition;

    property Font;
    //==========================================================================


    property TabIndex  : integer  read fTabIndex    write SetTabIndex;
    property Tabs      : TStrings read fTabs        write SetTabs;
    property TabCount  : integer  read GetTabCount;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, Graphics, AggPixelFormat, AggRoundedRect, AggPathStorage;



{ TVamTabPanel }

constructor TVamTabPanel.Create(AOwner: TComponent);
begin
  inherited;

  MouseOverPadIndex := -1;

  fCornerRadius[0] := 0;
  fCornerRadius[1] := 0;
  fCornerRadius[2] := 0;
  fCornerRadius[3] := 0;

  fColor_Background := '$ff777776';
  fColor_TabOff     := '$ffC0C0C0';
  fColor_TabOn      := '$ffD3D3D3';

  fTabIndex := -1;

  fTabs := TStringList.Create;
  TStringList(fTabs).OnChange := TabsChanged;

  fTabHeight  := 24;
  fTabOffset  := 8;
  fTabPadding := 16;
  fTabSpace   := 8;
end;

destructor TVamTabPanel.Destroy;
begin
  fTabs.Free;
  SetLength(TabDimensions, 0);

  inherited;
end;

procedure TVamTabPanel.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  pc : PRedFoxColorString;
begin
  case Index of
    0 : pc := @fColor_Background;
    1 : pc := @fColor_TabOff;
    2 : pc := @fColor_TabOn;
  else
    raise Exception.Create('Index not handled.');
  end;

  if pc^ <> Value then
  begin
    pc^ := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetCornerRadius(const Index: Integer; const Value: double);
begin
  if Value <> fCornerRadius[Index] then
  begin
    fCornerRadius[Index] := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabHeight(const Value: integer);
begin
  if Value <> fTabHeight then
  begin
    fTabHeight := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabIndex(const Value: integer);
begin
  if Value <> fTabIndex then
  begin
    fTabIndex := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabOffset(const Value: integer);
begin
  if Value <> fTabOffset then
  begin
    fTabOffset := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabPadding(const Value: integer);
begin
  if Value <> fTabPadding then
  begin
    fTabPadding := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabPosition(const Value: TTabPosition);
begin
  if Value <> fTabPosition then
  begin
    fTabPosition := Value;
    Invalidate;
  end;
end;

procedure TVamTabPanel.SetTabs(const Value: TStrings);
begin
  fTabs.Assign(Value);
  Invalidate;
end;

procedure TVamTabPanel.SetTabSpace(const Value: integer);
begin
  if Value <> fTabSpace then
  begin
    fTabSpace := Value;
    Invalidate;
  end;

end;

function TVamTabPanel.TabIndexAt(PixelX, PixelY: integer): integer;
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

procedure TVamTabPanel.TabsChanged(Sender: TObject);
begin
  if fTabIndex >= fTabs.Count then fTabIndex := fTabs.Count-1;

  SetLength(TabDimensions, fTabs.Count);

  Invalidate;
  Changed;
end;

function TVamTabPanel.GetClientRect: TRect;
begin
  result := inherited GetClientRect;

  case TabPosition of
    TTabPosition.tpAboveLeft:   result.Top    := result.Top + fTabHeight;
    TTabPosition.tpAboveRight:  result.Top    := result.Top + fTabHeight;
    TTabPosition.tbAboveCenter: result.Top    := result.Top + fTabHeight;
    TTabPosition.tpBelowLeft:   result.Bottom := result.Bottom - fTabHeight;
    TTabPosition.tpBelowRight:  result.Bottom := result.Bottom - fTabHeight;
    TTabPosition.tpBelowCenter: result.Bottom := result.Bottom - fTabHeight;
  else
    raise Exception.Create('TabPosition type not handled.');
  end;

end;

function TVamTabPanel.GetCornerRadius(const Index: Integer): double;
begin
  result := fCornerRadius[Index];
end;

function TVamTabPanel.GetTabCount: integer;
begin
  result := fTabs.Count;
end;


procedure TVamTabPanel.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;



procedure TVamTabPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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


procedure TVamTabPanel.MouseEnter;
begin
  inherited;

end;

procedure TVamTabPanel.MouseLeave;
begin
  inherited;

  if MouseOverPadIndex <> -1 then
  begin
    MouseOverPadIndex := -1;
    Invalidate;
  end;

end;

procedure TVamTabPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index : integer;
begin
  inherited;


  Index := TabIndexAt(X,Y);
  if MouseOverPadIndex <> Index then
  begin
    MouseOverPadIndex := Index;
    Invalidate;
  end;

end;

procedure TVamTabPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

end;

procedure TVamTabPanel.Paint;
begin
  inherited;

  case TabPosition of
    TTabPosition.tpAboveLeft,
    TTabPosition.tpAboveRight,
    TTabPosition.tpBelowLeft,
    TTabPosition.tpBelowRight: DrawTabs_LeftOrRight;

    TTabPosition.tpBelowCenter: DrawTabs_BelowCenter;
    TTabPosition.tbAboveCenter: DrawTabs_AboveCenter;
  end;

end;


procedure TVamTabPanel.DrawTabs_AboveCenter;
begin
  //TODO:
end;

procedure TVamTabPanel.DrawTabs_BelowCenter;
var
  TabArea : TRect;
  OffsetX : integer;
  c1 : integer;
  s : string;
  x1,y1,x2,y2:integer;
  Rc   : TAggRoundedRect;
  Path : TAggPathStorage;

  TabWidth : single;
begin
  BackBuffer.UpdateFont(self.Font);

  //BackBuffer.BufferInterface.ClearAll(GetRedFoxColor(fColor_Background).WithAlpha(0));
  BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;


  TabArea.Left := 0;
  TabArea.Right := Width;
  TabArea.Top := Height-TabHeight;
  TabArea.Bottom := Height;


  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Background);
  BackBuffer.BufferInterface.NoLine;


  //== Draw the rounded rect ==
  rc := TAggRoundedRect.Create;
  Path := TAggPathStorage.Create;
  try
    rc.Rect(TabArea.Left, TabArea.Top, TabArea.Right, TabArea.Bottom);
    rc.Radius(CornerRadius1, CornerRadius1, CornerRadius2, CornerRadius2, CornerRadius3, CornerRadius3, CornerRadius4, CornerRadius4);
    Path.AddPath(rc);

    BackBuffer.BufferInterface.ResetPath;
    BackBuffer.BufferInterface.AddPath(Path);
    BackBuffer.BufferInterface.DrawPath;
  finally
    rc.Free;
    Path.Free;
  end;
  //=================================



  // === draw the tabs ===

  TabWidth := self.Width - (fTabOffset * 2) - ((fTabs.Count-1) * fTabSpace);
  TabWidth := TabWidth / fTabs.Count;

  OffsetX := fTabOffset;
  for c1 := 0 to fTabs.Count-1 do
  begin
    if (c1 = fTabIndex) or (c1 = MouseOverPadIndex)
        then BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOn)
        else BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOff);

    //Get tab dimensions.
    x1 := OffsetX;
    x2 := OffsetX + round(TabWidth);
    y1 := TabArea.Top;
    y2 := TabArea.Bottom;

    if (c1 = fTabs.Count-1) then x2 := Width - fTabOffset;

    TabDimensions[c1] := Rect(x1, y1, x2, y2);

    BackBuffer.BufferInterface.NoLine;
    BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);

    BackBuffer.UpdateFont(self.Font);
    s := fTabs[c1];
    BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, Rect(x1,y1, x2, y2));

    OffsetX := x2 + fTabSpace;
  end;

end;

procedure TVamTabPanel.DrawTabs_LeftOrRight;
var
  TabArea : TRect;
  OffsetX : integer;
  c1 : integer;
  s : string;
  x1,y1,x2,y2:integer;
  tw : single;
  Rc   : TAggRoundedRect;
  Path : TAggPathStorage;
begin
  BackBuffer.UpdateFont(self.Font);

  //BackBuffer.BufferInterface.ClearAll(GetRedFoxColor(fColor_Background).WithAlpha(0));
  BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  case TabPosition of
    TTabPosition.tpAboveLeft,
    TTabPosition.tpAboveRight:
    begin
      TabArea.Left := 0;
      TabArea.Right := Width;
      TabArea.Top := 0;
      TabArea.Bottom := TabHeight;
    end;

    TTabPosition.tpBelowLeft,
    TTabPosition.tpBelowRight:
    begin
      TabArea.Left := 0;
      TabArea.Right := Width;
      TabArea.Top := Height-TabHeight;
      TabArea.Bottom := Height;
    end;
  else
    raise Exception.Create('TabPosition not handled.');
  end;

  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Background);
  BackBuffer.BufferInterface.NoLine;

  //BackBuffer.BufferInterface.Rectangle(TabArea.Left, TabArea.Top, TabArea.Right, TabArea.Bottom);

  //== Draw the rounded rect ==
  rc := TAggRoundedRect.Create;
  Path := TAggPathStorage.Create;
  try
    rc.Rect(TabArea.Left, TabArea.Top, TabArea.Right, TabArea.Bottom);
    rc.Radius(CornerRadius1, CornerRadius1, CornerRadius2, CornerRadius2, CornerRadius3, CornerRadius3, CornerRadius4, CornerRadius4);
    Path.AddPath(rc);

    BackBuffer.BufferInterface.ResetPath;
    BackBuffer.BufferInterface.AddPath(Path);
    BackBuffer.BufferInterface.DrawPath;
  finally
    rc.Free;
    Path.Free;
  end;
  //=================================



  if (TabPosition = TTabPosition.tpAboveLeft) or (TabPosition = TTabPosition.tpBelowLeft) then
  begin
    OffsetX := fTabOffset;
    for c1 := 0 to fTabs.Count-1 do
    begin
      if (c1 = fTabIndex) or (c1 = MouseOverPadIndex)
        then BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOn)
        else BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOff);

      s := fTabs[c1];
      tw := BackBuffer.TextWidth(s);

      //Get tab dimensions.
      x1 := OffsetX;
      x2 := OffsetX + round(tw) + (fTabPadding * 2);
      y1 := TabArea.Top;
      y2 := TabArea.Bottom;

      TabDimensions[c1] := Rect(x1, y1, x2, y2);

      BackBuffer.BufferInterface.NoLine;
      BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);

      BackBuffer.UpdateFont(self.Font);
      BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, Rect(x1,y1, x2, y2));

      OffsetX := x2 + fTabSpace;
    end;
  end;


  if (TabPosition = TTabPosition.tpAboveRight) or (TabPosition = TTabPosition.tpBelowRight) then
  begin
    OffsetX := TabArea.Right - fTabOffset;
    for c1 := fTabs.Count-1 downto 0 do
    begin
      if (c1 = fTabIndex) or (c1 = MouseOverPadIndex)
        then BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOn)
        else BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_TabOff);

      s := fTabs[c1];
      tw := BackBuffer.TextWidth(s);

      x1 := OffsetX - round(tw) - (fTabPadding * 2);
      x2 := OffsetX;
      y1 := TabArea.Top;
      y2 := TabArea.Bottom;

      TabDimensions[c1] := Rect(x1, y1, x2, y2);

      BackBuffer.BufferInterface.NoLine;
      BackBuffer.BufferInterface.Rectangle(x1, y1, x2, y2);

      BackBuffer.UpdateFont(self.Font);
      BackBuffer.DrawText(s, Font, TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignCenter, Rect(x1,y1, x2, y2));

      OffsetX := x1 - fTabSpace;
    end;
  end;
end;




end.
