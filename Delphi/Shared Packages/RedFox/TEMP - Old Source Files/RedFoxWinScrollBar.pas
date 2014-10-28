unit RedFoxWinScrollBar;

interface

uses
  Messages, Vcl.StdCtrls, Vcl.Forms,
  Classes, Controls, Windows, RedFoxWinControl;

type
  TRedFoxWinScrollBar = class(TRedFoxWinControl)
  protected
    FMin: Integer;
    FMax: Integer;
    FPosition: Integer;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateParams(var Params: TCreateParams); override;
  end;

implementation

{ TRedFoxWinScrollBar }

constructor TRedFoxWinScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csAcceptsControls];

  fMin := 0;
  fMax := 100;
  fPosition := 50;
end;

destructor TRedFoxWinScrollBar.Destroy;
begin

  inherited;
end;

procedure TRedFoxWinScrollBar.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'SCROLLBAR');
end;

procedure TRedFoxWinScrollBar.CreateWnd;
var
  ScrollInfo: TScrollInfo;
  LBounds: TRect;
begin
  // Windows' does not always create the window size we ask for, so we have
  //  insist sometimes.  Setting BoundsRect will only adjust the size if needed.
  LBounds := BoundsRect;
  inherited CreateWnd;
  BoundsRect := LBounds;

  SetScrollRange(Handle, SB_CTL, FMin, FMax, False);
{$IF DEFINED(CLR)}
  ScrollInfo.cbSize := Marshal.SizeOf(TypeOf(ScrollInfo));
{$ELSE}
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
{$IFEND}
  ScrollInfo.nPage := 1;
  ScrollInfo.fMask := SIF_PAGE;
  SetScrollInfo(Handle, SB_CTL, ScrollInfo, False);
  SetScrollPos(Handle, SB_CTL, FMax - FPosition, True);

end;



end.
