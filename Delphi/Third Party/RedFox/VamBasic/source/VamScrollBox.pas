unit VamScrollBox;

interface

uses
  Types, StdCtrls, Classes, RedFox, RedFoxColor, VamWinControl, VamScrollBar;

type

  // TVamScrollbox is a container component that can optionally show a vertical and/or
  // horizontal scrollbar. The scrollbars have no impact on the container's
  // child controls by default. Any functionality will need to be explicitly added by
  // handling the OnScroll event.

  TScrollEventKind = (seHorz, seVert);
  TScrollEvent = procedure(Sender:TObject; Kind:TScrollEventKind; ScrollPos:single) of object;

  TVamScrollBox = class(TVamWinControl)
  private
    fColor : TRedFoxColor;
    FScrollBars: TScrollStyle;
    fScrollBarWidth: integer;
    fOnScroll: TScrollEvent;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetScrollBarWidth(const Value: integer);
    function GetScrollXIndexSize: single;
    function GetScrollXPos: single;
    function GetScrollYPos: single;
    procedure SetScrollXIndexSize(const Value: single);
    procedure SetScrollXPos(const Value: single);
    procedure SetScrollYPos(const Value: single);
    function GetScrollYIndexSize: single;
    procedure SetScrollYIndexSize(const Value: single);
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    function GetColors(const Index: Integer): TRedFoxColorString;
  protected
    fHorzScrollBar : TVamScrollBar;
    fVertScrollBar : TVamScrollBar;
    procedure UpdateScrollBars;
    procedure EventHandler_ScrollBarChanged(Sender:TObject);

    function GetClientRect:TRect; override;

    procedure MouseWheelUp(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
    procedure MouseWheelDown(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property HorzScrollBar : TVamScrollBar read fHorzScrollBar;
    property VertScrollBar : TVamScrollBar read fVertScrollBar;
  published
    property Color_Border     : TRedFoxColorString index 0 read GetColors write SetColors;
    property Color_Background : TRedFoxColorString index 1 read GetColors write SetColors;
    property Color_Foreground : TRedFoxColorString index 2 read GetColors write SetColors;

    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property ScrollBarWidth : integer read fScrollBarWidth write SetScrollBarWidth;

    property ScrollXPos : single read GetScrollXPos write SetScrollXPos; //range 0..1
    property ScrollYPos : single read GetScrollYPos write SetScrollYPos; //range 0..1
    property ScrollXIndexSize : single read GetScrollXIndexSize write SetScrollXIndexSize; //range 0..1
    property ScrollYIndexSize : single read GetScrollYIndexSize write SetScrollYIndexSize; //range 0..1

    property OnScroll : TScrollEvent read fOnScroll write fOnScroll;

    {$INCLUDE TControlProperties.inc}
    {$INCLUDE TWinControlProperties.inc}
  end;

implementation

uses
  SysUtils;

{ TVamScrollBox }

constructor TVamScrollBox.Create(AOwner: TComponent);
begin
  inherited;

  // The scroll box is transparent. The sub-controls it contains aren't.
  Transparent := true;

  fScrollBars := ssBoth;
  fScrollBarWidth := 16;

  fHorzScrollBar := TVamScrollBar.Create(self);
  HorzScrollBar.CornerRadius[2] := 3;
  HorzScrollBar.CornerRadius[3] := 3;
  HorzScrollBar.SliderType := TVamSliderType.stHorizontal;
  HorzScrollBar.Parent := self;
  HorzScrollBar.OnChanged := EventHandler_ScrollBarChanged;


  fVertScrollBar := TVamScrollBar.Create(self);
  VertScrollBar.CornerRadius[1] := 3;
  VertScrollBar.CornerRadius[2] := 3;
  VertScrollBar.SliderType := TVamSliderType.stVertical;
  VertScrollBar.Parent := self;
  VertScrollBar.OnChanged := EventHandler_ScrollBarChanged;
end;

destructor TVamScrollBox.Destroy;
begin
  FreeAndNil(fHorzScrollBar);
  FreeAndNil(fVertScrollBar);
  inherited;
end;

procedure TVamScrollBox.MouseWheelDown(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;

end;

procedure TVamScrollBox.MouseWheelUp(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;


end;

procedure TVamScrollBox.EventHandler_ScrollBarChanged(Sender: TObject);
begin
  if (Sender = VertScrollBar) and (assigned(OnScroll)) then
  begin
    OnScroll(self, seVert, VertScrollBar.IndexPos);
  end;

  if (Sender = HorzScrollBar) and (assigned(OnScroll)) then
  begin
    OnScroll(self, seHorz, HorzScrollBar.IndexPos);
  end;
end;

function TVamScrollBox.GetClientRect: TRect;
begin
  result := inherited GetClientRect;

  case fScrollBars of
    ssHorizontal:
    begin
      result.Height := result.Height - ScrollBarWidth;
    end;

    ssVertical:
    begin
      result.Width := result.Width - ScrollBarWidth;
    end;

    ssBoth:
    begin
      result.Width := result.Width - ScrollBarWidth;
      result.Height := result.Height - ScrollBarWidth;
    end;
  end;
end;

function TVamScrollBox.GetColors(const Index: Integer): TRedFoxColorString;
begin
  case Index of
    0 : result := VertScrollBar.Color_Border;
    1 : result := VertScrollBar.Color_Background;
    2 : result := VertScrollBar.Color_Foreground;
  else
    raise Exception.Create('Index type not handled.');
  end;
end;

function TVamScrollBox.GetScrollXIndexSize: single;
begin
  result := HorzScrollBar.IndexSize;
  assert(result >= 0);
  assert(result <= 1);
end;

function TVamScrollBox.GetScrollXPos: single;
begin
  result := HorzScrollBar.IndexPos;
  assert(result >= 0);
  assert(result <= 1);
end;

function TVamScrollBox.GetScrollYIndexSize: single;
begin
  result := VertScrollBar.IndexSize;
  assert(result >= 0);
  assert(result <= 1);
end;

function TVamScrollBox.GetScrollYPos: single;
begin
  result := VertScrollBar.IndexPos;
  assert(result >= 0);
  assert(result <= 1);
end;



procedure TVamScrollBox.Paint;
begin
  inherited;
  BackBuffer.BufferInterface.ClearAll(fColor.R, fColor.G, fColor.B, fColor.A);
end;

procedure TVamScrollBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    UpdateScrollBars;
  end;

end;

procedure TVamScrollBox.SetColors(const Index: Integer; const Value: TRedFoxColorString);
begin
  case Index of
    0 :
    begin
      VertScrollBar.Color_Border     := Value;
      HorzScrollBar.Color_Border     := Value;
    end;

    1 :
    begin
      VertScrollBar.Color_Background := Value;
      HorzScrollBar.Color_Background := Value;
    end;

    2 :
    begin
      VertScrollBar.Color_Foreground := Value;
      HorzScrollBar.Color_Foreground := Value;
    end
  else
    raise Exception.Create('Index type not handled.');
  end;
end;

procedure TVamScrollBox.SetScrollBars(const Value: TScrollStyle);
begin
  if Value <> fScrollBars then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
  end;
end;

procedure TVamScrollBox.SetScrollBarWidth(const Value: integer);
begin
  if Value <> fScrollBarWidth then
  begin
    fScrollBarWidth := Value;
    UpdateScrollBars;
  end;
end;

procedure TVamScrollBox.SetScrollXIndexSize(const Value: single);
begin
  HorzScrollBar.IndexSize := Value;
end;

procedure TVamScrollBox.SetScrollXPos(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);
  HorzScrollBar.IndexPos := Value;
end;

procedure TVamScrollBox.SetScrollYIndexSize(const Value: single);
begin
  VertScrollBar.IndexSize := Value;
end;

procedure TVamScrollBox.SetScrollYPos(const Value: single);
begin
  VertScrollBar.IndexPos := Value;
end;

procedure TVamScrollBox.UpdateScrollBars;
var
  sbw : integer;
begin
  sbw := ScrollBarWidth;

  case ScrollBars of
    ssNone:
    begin
      HorzScrollBar.Visible := false;
      VertScrollBar.Visible := false;
    end;

    ssHorizontal:
    begin
      HorzScrollBar.Visible := true;
      VertScrollBar.Visible := false;

      HorzScrollBar.Width  := Width;
      HorzScrollBar.Height := sbw;
      HorzScrollBar.Top    := Height - sbw;
      HorzScrollBar.Left   := 0;
    end;

    ssVertical:
    begin
      HorzScrollBar.Visible := false;
      VertScrollBar.Visible := true;

      VertScrollBar.Width  := sbw;
      VertScrollBar.Height := Height;
      VertScrollBar.Top    := 0;
      VertScrollBar.Left   := Width - sbw;
    end;

    ssBoth:
    begin
      HorzScrollBar.Visible := true;
      VertScrollBar.Visible := true;

      HorzScrollBar.Width  := Width - sbw;
      HorzScrollBar.Height := sbw;
      HorzScrollBar.Top    := Height - sbw;
      HorzScrollBar.Left   := 0;

      VertScrollBar.Width  := sbw;
      VertScrollBar.Height := Height - sbw;
      VertScrollBar.Top    := 0;
      VertScrollBar.Left   := Width - sbw;
    end;
  else
    raise Exception.Create('Unexcepted ScrollBars value.');
  end;

  Invalidate;
end;



end.
