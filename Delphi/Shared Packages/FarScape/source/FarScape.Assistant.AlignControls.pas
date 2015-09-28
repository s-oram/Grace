unit FarScape.Assistant.AlignControls;

interface

uses
  FarScape.CustomControl;

type
  ControlAlignmentAssistant = record
  private
    class procedure AlignToTop(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToBottom(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToLeft(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToRight(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToCenter(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToClient(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToCustom(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToGrid(const ContainerControl, TargetControl : TFarScapeControl); static;
    class procedure AlignToNone(const ContainerControl, PrevControl, TargetControl : TFarScapeControl); static;

    class procedure AlignChildControls(const ContainerControl : TFarScapeControl; AlignMode : TControlAlignment); overload; static;

  public
    class procedure AlignChildControls(const ContainerControl : TFarScapeControl); overload; static;

    class function FindNextAlignmentTarget(const ContainerControl : TFarScapeControl; const AlignmentMode : TControlAlignment; var Index : integer):TFarScapeControl; static;
    class function FindLastAlignmentTarget(const ContainerControl : TFarScapeControl; const AlignmentMode : TControlAlignment):TFarScapeControl; static;
  end;



implementation

uses
  SysUtils;

type
  TProtectedControlHack = class(TFarScapeControl);


{ AlignControls }

class procedure ControlAlignmentAssistant.AlignChildControls(const ContainerControl: TFarScapeControl);
begin
  // 1) IMPORTANT: Align the Top, Bottom, Left and Right elements first.
  AlignChildControls(ContainerControl, TControlAlignment.caTop);
  AlignChildControls(ContainerControl, TControlAlignment.caBottom);
  AlignChildControls(ContainerControl, TControlAlignment.caLeft);
  AlignChildControls(ContainerControl, TControlAlignment.caRight);

  // 2) Then align the Client and Center elements.
  AlignChildControls(ContainerControl, TControlAlignment.caClient);
  AlignChildControls(ContainerControl, TControlAlignment.caCenter);

  // 3) The remaining alignment modes can be completed in any order.
  AlignChildControls(ContainerControl, TControlAlignment.caCustom);
  AlignChildControls(ContainerControl, TControlAlignment.caGrid);
  AlignChildControls(ContainerControl, TControlAlignment.caNone);
end;

class procedure ControlAlignmentAssistant.AlignChildControls(const ContainerControl: TFarScapeControl; AlignMode: TControlAlignment);
var
  CurrentControl, PrevControl : TFarScapeControl;
  Index : integer;
begin
  Index := 0;

  PrevControl := nil;
  CurrentControl := FindNextAlignmentTarget(ContainerControl, AlignMode, Index);

  while assigned(CurrentControl) do
  begin
    case AlignMode of
      caNone:   AlignToNone(ContainerControl, PrevControl, CurrentControl);
      caTop:    AlignToTop(ContainerControl, PrevControl, CurrentControl);
      caBottom: AlignToBottom(ContainerControl, PrevControl, CurrentControl);
      caLeft:   AlignToLeft(ContainerControl, PrevControl, CurrentControl);
      caRight:  AlignToRight(ContainerControl, PrevControl, CurrentControl);
      caClient: AlignToClient(ContainerControl, PrevControl, CurrentControl);
      caCenter: AlignToCenter(ContainerControl, PrevControl, CurrentControl);
      caCustom: AlignToCustom(ContainerControl, PrevControl, CurrentControl);
      caGrid: AlignToGrid(ContainerControl, CurrentControl);
    else
      raise Exception.Create('Unexpected Align mode.');
    end;

    PrevControl := CurrentControl;
    CurrentControl := FindNextAlignmentTarget(ContainerControl, AlignMode, Index);
  end;
end;

class procedure ControlAlignmentAssistant.AlignToTop(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  xLeft, xTop, xWidth, xHeight : integer;
  xRight : integer;
begin
  xLeft   := ContainerControl.Padding.Left  + TargetControl.Margins.Left;
  xRight  := ContainerControl.Padding.Right + TargetControl.Margins.Right;
  xWidth  := ContainerControl.Width - xLeft - xRight;
  xHeight := TargetControl.NaturalHeight;

  if assigned(PrevControl) then
  begin
    xTop := PrevControl.Top + PrevControl.Height + PrevControl.Margins.Bottom + TargetControl.Margins.Top;
  end else
  begin
    xTop := ContainerControl.Padding.Top + TargetControl.Margins.Top;
  end;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToBottom(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  xLeft, xTop, xWidth, xHeight : integer;
  xRight : integer;
begin
  xLeft   := ContainerControl.Padding.Left  + TargetControl.Margins.Left;
  xRight  := ContainerControl.Padding.Right + TargetControl.Margins.Right;
  xWidth  := ContainerControl.Width - xLeft - xRight;
  xHeight := TargetControl.NaturalHeight;

  if assigned(PrevControl) then
  begin
    xTop := PrevControl.Top - PrevControl.Margins.Top - TargetControl.NaturalHeight - TargetControl.Margins.Bottom;
  end else
  begin
    xTop := ContainerControl.Height - ContainerControl.Padding.Bottom - TargetControl.Margins.Bottom - TargetControl.NaturalHeight;
  end;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToLeft(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  xLeft, xTop, xWidth, xHeight : integer;
begin
  xWidth := TargetControl.NaturalWidth;
  xHeight := ContainerControl.Height - ContainerControl.Padding.Top - ContainerControl.Padding.Bottom - TargetControl.Margins.Top - TargetControl.Margins.Bottom;
  xTop := ContainerControl.Padding.Top + TargetControl.Margins.Top;

  if assigned(PrevControl) then
  begin
    xLeft := PrevControl.Left + PrevControl.Width + PrevControl.Margins.Right + TargetControl.Margins.Left;
  end else
  begin
    xLeft := ContainerControl.Padding.Left + TargetControl.Margins.Left;
  end;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToRight(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  xLeft, xTop, xWidth, xHeight : integer;
begin
  xWidth := TargetControl.NaturalWidth;
  xHeight := ContainerControl.Height - ContainerControl.Padding.Top - ContainerControl.Padding.Bottom - TargetControl.Margins.Top - TargetControl.Margins.Bottom;
  xTop := ContainerControl.Padding.Top + TargetControl.Margins.Top;

  if assigned(PrevControl) then
  begin
    xLeft := PrevControl.Left - PrevControl.Margins.Left - TargetControl.Margins.Right - TargetControl.NaturalWidth;
  end else
  begin
    xLeft := ContainerControl.Width - ContainerControl.Padding.Right - TargetControl.Margins.Right - TargetControl.NaturalWidth;
  end;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToCenter(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  LastLeft, LastTop, LastRight, LastBottom : TFarScapeControl;
  xLeft, xTop, xWidth, xHeight : integer;
  AreaLeft,  AreaTop : integer;
  AreaRight, AreaBottom : integer;
  AreaWidth, AreaHeight : integer;

  OffsetX, OffsetY : integer;
begin
  LastLeft   := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caLeft);
  LastTop    := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caTop);
  LastRight  := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caRight);
  LastBottom := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caBottom);

  if assigned(LastLeft)
    then AreaLeft := LastLeft.Left + LastLeft.Width + LastLeft.Margins.Right
    else AreaLeft := ContainerControl.Padding.Left;

  if assigned(LastTop)
    then AreaTop := LastTop.Top + LastTop.Height + LastTop.Margins.Bottom
    else AreaTop := ContainerControl.Padding.Top;

  if assigned(LastRight)
    then AreaRight := LastRight.Left - LastRight.Margins.Left
    else AreaRight := ContainerControl.Width - ContainerControl.Padding.Right;

  if assigned(LastBottom)
    then AreaBottom := LastBottom.Top - LastBottom.Margins.Top
    else AreaBottom := ContainerControl.Height - ContainerControl.Padding.Bottom;

  AreaWidth  := AreaRight  - AreaLeft;
  AreaHeight := AreaBottom - AreaTop;

  OffsetX := (AreaWidth - TargetControl.NaturalWidth) div 2;
  OffsetY := (AreaHeight - TargetControl.NaturalHeight) div 2;

  xLeft   := AreaLeft + OffsetX;
  xTop    := AreaTop  + OffsetY;
  xWidth  := TargetControl.NaturalWidth;
  xHeight := TargetControl.NaturalHeight;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToClient(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  LastLeft, LastTop, LastRight, LastBottom : TFarScapeControl;
  xLeft, xTop, xWidth, xHeight : integer;
  xRight, xBottom : integer;
begin
  LastLeft   := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caLeft);
  LastTop    := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caTop);
  LastRight  := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caRight);
  LastBottom := FindLastAlignmentTarget(ContainerControl, TControlAlignment.caBottom);

  if assigned(LastLeft)
    then xLeft := LastLeft.Left + LastLeft.Width + LastLeft.Margins.Right + TargetControl.Margins.Left
    else xLeft := ContainerControl.Padding.Left + TargetControl.Margins.Left;

  if assigned(LastTop)
    then xTop := LastTop.Top + LastTop.Height + LastTop.Margins.Bottom + TargetControl.Margins.Top
    else xTop := ContainerControl.Padding.Top + TargetControl.Margins.Top;

  if assigned(LastRight)
    then xRight := LastRight.Left - LastRight.Margins.Left - TargetControl.Margins.Right
    else xRight := ContainerControl.Width - ContainerControl.Padding.Right - TargetControl.Margins.Right;

  if assigned(LastBottom)
    then xBottom := LastBottom.Top - LastBottom.Margins.Top - TargetControl.Margins.Bottom
    else xBottom := ContainerControl.Height - ContainerControl.Padding.Bottom - TargetControl.Margins.Bottom;

  xWidth  := xRight  - xLeft;
  xHeight := xBottom - xTop;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToCustom(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
begin
  assert(false, 'TODO:MED');
end;

class procedure ControlAlignmentAssistant.AlignToNone(const ContainerControl, PrevControl, TargetControl: TFarScapeControl);
var
  xLeft, xTop, xWidth, xHeight : integer;
begin
  xLeft := TargetControl.NaturalLeft;
  xTop  := TargetControl.NaturalTop;
  xWidth := TargetControl.NaturalWidth;
  xHeight := TargetControl.NaturalHeight;

  TProtectedControlHack(TargetControl).SetComputedBounds(xLeft, xTop, xWidth, xHeight);
end;

class procedure ControlAlignmentAssistant.AlignToGrid(const ContainerControl, TargetControl: TFarScapeControl);
var
  x1, y1, x2, y2 : single;
  Left, Top, Width, Height : integer;
  AreaWidth, AreaHeight : integer;
  OffsetX, OffsetY : integer;
begin
  assert(ContainerControl.AlignmentGridRowCount >= 1);
  assert(ContainerControl.AlignmentGridColumnCount >= 1);

  AreaWidth := ContainerControl.Width - ContainerControl.Padding.Left - ContainerControl.Padding.Right;
  AreaHeight := ContainerControl.Height - ContainerControl.Padding.Top - ContainerControl.Padding.Bottom;
  OffsetX := ContainerControl.Padding.Left;
  OffsetY := ContainerControl.Padding.Top;

  x1 := AreaWidth / ContainerControl.AlignmentGridColumnCount * TargetControl.GridLeft;
  x2 := AreaWidth / ContainerControl.AlignmentGridColumnCount * (TargetControl.GridLeft + TargetControl.GridWidth);

  x1 := x1 + OffsetX;
  x2 := x2 + OffsetX;

  y1 := AreaHeight / ContainerControl.AlignmentGridRowCount * TargetControl.GridTop;
  y2 := AreaHeight / ContainerControl.AlignmentGridRowCount * (TargetControl.GridTop + TargetControl.GridHeight);

  y1 := y1 + OffsetY;
  y2 := y2 + OffsetY;

  Left   := round(x1) + TargetControl.Margins.Left;
  Width  := round(x2) - round(x1) - TargetControl.Margins.Left - TargetControl.Margins.Right;

  Top    := round(y1) + TargetControl.Margins.Top;
  Height := round(y2) - round(y1) - TargetControl.Margins.Top - TargetControl.Margins.Bottom;

  TProtectedControlHack(TargetControl).SetComputedBounds(Left, Top, Width, Height);
end;



class function ControlAlignmentAssistant.FindNextAlignmentTarget(const ContainerControl : TFarScapeControl; const AlignmentMode: TControlAlignment; var Index: integer): TFarScapeControl;
var
  c1: Integer;
begin
  for c1 := Index to ContainerControl.ControlCount-1 do
  begin
    if (ContainerControl.Control[c1].Align = AlignmentMode) and (ContainerControl.Control[c1].Visible) then
    begin
      Index := c1 + 1;
      exit(ContainerControl.Control[c1]);
    end;
  end;

  // If we make it this far, no control is found.
  Index := 0;
  result := nil;
end;

class function ControlAlignmentAssistant.FindLastAlignmentTarget(const ContainerControl: TFarScapeControl; const AlignmentMode: TControlAlignment): TFarScapeControl;
var
  c1: Integer;
begin
  for c1 := ContainerControl.ControlCount-1 downto 0 do
  begin
    if (ContainerControl.Control[c1].Align = AlignmentMode) and (ContainerControl.Control[c1].Visible) then
    begin
      exit(ContainerControl.Control[c1]);
    end;
  end;

  // If we make it this far, no control is found.
  result := nil;
end;

end.
