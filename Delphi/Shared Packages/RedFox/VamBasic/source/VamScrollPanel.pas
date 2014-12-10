unit VamScrollPanel;

interface

uses
  Types, Controls, Classes, StdCtrls, VamWinControl, VamScrollBar, VamDiv;

type
  {
    Development of TVamScrollPanel has been temporaily halted. It's not working
    as well as I would like. child controls will be drawn overlapping/underneath
    the scroll bars and non-client area.
    - one solution is to figure out if it's possible to have a sub-panel control
      that can be created if one doesn't exist. Look at TVstPage control
      for an example of creating child components owned by the main form.
  }


  TVamScrollPanel = class(TVamWinControl)
  private
    fScrollBarWidth: integer;
    FScrollBars: TScrollStyle;
    procedure SetScrollBarWidth(const Value: integer);
    procedure SetScrollBars(const Value: TScrollStyle);
  protected
    HorzScrollBar : TVamScrollBar;
    VertScrollBar : TVamScrollBar;
    procedure UpdateScrollBars;

    function GetClientRect:TRect; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetChildControlsBoundsRect:TRect;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property ScrollBarWidth : integer read fScrollBarWidth write SetScrollBarWidth;
  end;

implementation

uses
  SysUtils;

{ TVamScrollPanel }

procedure TVamScrollPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  inherited;
  UpdateScrollBars;

  VertScrollBar.BringToFront;
  HorzScrollBar.BringToFront;
end;

constructor TVamScrollPanel.Create(AOwner: TComponent);
begin
  inherited;

  fScrollBars := ssBoth;
  fScrollBarWidth := 16;

  HorzScrollBar := TVamScrollBar.Create(self);
  HorzScrollBar.SliderType := TVamSliderType.stHorizontal;
  HorzScrollBar.Parent := self;

  VertScrollBar := TVamScrollBar.Create(self);
  VertScrollBar.SliderType := TVamSliderType.stVertical;
  VertScrollBar.Parent := self;

end;

destructor TVamScrollPanel.Destroy;
begin
  FreeAndNil(HorzScrollBar);
  FreeAndNil(VertScrollBar);
  inherited;
end;

procedure TVamScrollPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    UpdateScrollBars;
  end;

end;

procedure TVamScrollPanel.SetScrollBars(const Value: TScrollStyle);
begin
  if Value <> fScrollBars then
  begin
    FScrollBars := Value;
    UpdateScrollBars;
  end;
end;

procedure TVamScrollPanel.SetScrollBarWidth(const Value: integer);
begin
  if Value <> fScrollBarWidth then
  begin
    fScrollBarWidth := Value;
    UpdateScrollBars;
  end;
end;

procedure TVamScrollPanel.UpdateScrollBars;
var
  sbw : integer;
  ClientRect, ContentRect : TRect;
begin
  //========= Update Visible Scroll Bars ==============

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

  //=============  Update Scroll Bar Values  ======================
  ClientRect := GetClientRect;
  ContentRect := GetChildControlsBoundsRect;

  if ClientRect.Height > ContentRect.Height
    then VertScrollBar.IndexSize := 1
    else VertScrollBar.IndexSize := ClientRect.Height / ContentRect.Height;
  //===============================================

  Invalidate;
end;

function TVamScrollPanel.GetClientRect: TRect;
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

function TVamScrollPanel.GetChildControlsBoundsRect: TRect;
var
  LeftMost, RightMost, TopMost, BottomMost : integer;
  Control: TControl;
  c1: Integer;
begin
  LeftMost   := 0;
  RightMost  := 0;
  TopMost    := 0;
  BottomMost := 0;

  //Get the coordinates of the first non-subcomponent control.
  for c1 := 0 to ControlCount-1 do
  begin
    Control := Controls[c1];

    if (Control <> HorzScrollBar) and (Control <> VertScrollBar) then
    begin
      LeftMost := Control.Left;
      TopMost := Control.Top;
      RightMost  := Control.Left + Control.Width;
      BottomMost := Control.Top  + Control.Height;

      Break;  //===================>> BREAK >>============>>
    end;
  end;


  //Iterate through all non-subcomponent controls and expand bounds where needed.
  for c1 := 0 to ControlCount-1 do
  begin
    Control := Controls[c1];

    if (Control <> HorzScrollBar) and (Control <> VertScrollBar) then
    begin
      if Control.Left < LeftMost then LeftMost := Control.Left;
      if Control.Top  < TopMost  then TopMost := Control.Top;
      if Control.Left + Control.Width  > RightMost  then RightMost  := Control.Left + Control.Width;
      if Control.Top  + Control.Height > BottomMost then BottomMost := Control.Top  + Control.Height;
    end;
  end;

  result := Rect(LeftMost, TopMost, RightMost, BottomMost);
end;



end.
