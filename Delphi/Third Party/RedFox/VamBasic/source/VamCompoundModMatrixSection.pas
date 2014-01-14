unit VamCompoundModMatrixSection;

interface

uses
  Types, Controls, Classes, StdCtrls, Graphics, WinApi.Messages,
  RedFox, RedFoxColor,
  VamWinControl, VamScrollBar, VamDiv, VamPanel, VamTextBox,
  VamKnob, VamArrows, VamLabel;

type

  TControlElements = (egNone, egTextBox1, egTextBox2, egTextBox3, egKnob1);


  TVamCompoundModMatrixSection = class(TVamWinControl)
  private
    fElementMouseOver: TControlElements;
    fOnKnob1Changed: TNotifyEvent;
    fOnText1MouseDown: TNotifyEvent;
    fOnText2MouseDown: TNotifyEvent;
    fOnText3MouseDown: TNotifyEvent;
    fColor_BackgroundMouseOver: TRedFoxColorString;
    fColor_Background: TRedFoxColorString;
    fColor_SectionFocused: TRedFoxColorString;
    fElementGrabbed: TControlElements;
    fElementFocused: TControlElements;
    fOnReset: TNotifyEvent;
    procedure SetElementMouseOver(const Value: TControlElements);
    procedure SetText1(const Value: string);
    procedure SetText2(const Value: string);
    procedure SetText3(const Value: string);
    function GetKnobPos(const Index: Integer): single;
    procedure SetKnobPos(const Index: Integer; const Value: single);
    procedure WMFontChange(var Message: TMessage); message WM_FONTCHANGE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure SetColors(const Index: Integer; const Value: TRedFoxColorString);
    procedure SetElementGrabbed(const Value: TControlElements);
    procedure SetElementFocused(const Value: TControlElements);
  protected
    FBackground : TVamPanel;
    FTextBox1 : TVamTextBox;
    FTextBox2 : TVamTextBox;
    FTextBox3 : TVamTextBox;
    FModAmountTextBox : TVamTextBox;
    FKnob1    : TVamKnob;

    property ElementGrabbed   : TControlElements read fElementGrabbed   write SetElementGrabbed;
    property ElementMouseOver : TControlElements read fElementMouseOver write SetElementMouseOver;


    procedure UpdateColors;

    procedure UpdateLayout;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure SetFont(const Value: TFont); override;

    procedure EventHandler_KnobChanged(Sender : TObject);

    procedure UpdateModAmountText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ElementFocused   : TControlElements read fElementFocused   write SetElementFocused;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    function GetControlElementOffset(const CE : TControlElements):integer;

    property Background : TVamPanel   read fBackground;
    property TextBox1   : TVamTextBox read fTextBox1;
    property TextBox2   : TVamTextBox read fTextBox2;
    property TextBox3   : TVamTextBox read fTextBox3;
    property ModAmountTextBox : TVamTextBox read FModAmountTextBox;
    property Knob1      : TVamKnob    read fKnob1;

  published

    property Color_Background          : TRedFoxColorString index 0 read fColor_Background          write SetColors;
    property Color_BackgroundMouseOver : TRedFoxColorString index 1 read fColor_BackgroundMouseOver write SetColors;
    property Color_SectionFocused      : TRedFoxColorString index 2 read fColor_SectionFocused      write SetColors;

    property Font;

    property Knob1Pos : single index 0 read GetKnobPos write SetKnobPos;

    property Text1 : string write SetText1;
    property Text2 : string write SetText2;
    property Text3 : string write SetText3;

    property OnText1MouseDown : TNotifyEvent read fOnText1MouseDown write fOnText1MouseDown;
    property OnText2MouseDown : TNotifyEvent read fOnText2MouseDown write fOnText2MouseDown;
    property OnText3MouseDown : TNotifyEvent read fOnText3MouseDown write fOnText3MouseDown;

    property OnKnob1Changed : TNotifyEvent read fOnKnob1Changed write fOnKnob1Changed;
    property OnReset        : TNotifyEvent read fOnReset        write fOnReset;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils;

type
  // Use a hack to gain access to protected methods.
  TKnobHack = class(TVamKnob)
  end;

{ TVamCompoundModMatrixSection }

constructor TVamCompoundModMatrixSection.Create(AOwner: TComponent);
var
  c : TRedFoxColor;
begin
  inherited;


  //This is a compound component. the contained
  // child controls do all the drawing..
  Transparent := true;

  FBackground := TVamPanel.Create(self);
  FBackground.HitTest := false;
  FBackground.Parent := self;

  FBackground.CornerRadius1 := 3;
  FBackground.CornerRadius2 := 3;
  FBackground.CornerRadius3 := 3;
  FBackground.CornerRadius4 := 3;
  FBackground.Color := '$22FF0000';

  FTextBox1 := TVamTextBox.Create(self);
  FTextBox1.Font := self.Font;
  FTextBox1.HitTest := false;
  FTextBox1.Parent := FBackground;
  FTextBox1.Text   := 'Text';
  FTextBox1.TextAlign := TRedFoxAlign.AlignCenter;
  FTextBox1.TextVAlign := TRedFoxAlign.AlignCenter;
  FTextBox1.Color          := '$00000000';
  FTextBox1.ColorMouseOver := '$66FFFFFF';


  FTextBox2 := TVamTextBox.Create(self);
  FTextBox2.Font := self.Font;
  FTextBox2.HitTest := false;
  FTextBox2.Parent := FBackground;
  FTextBox2.Text   := 'Text';
  FTextBox2.TextAlign := TRedFoxAlign.AlignCenter;
  FTextBox2.TextVAlign := TRedFoxAlign.AlignCenter;
  FTextBox2.Color          := '$00000000';
  FTextBox2.ColorMouseOver := '$66FFFFFF';

  FTextBox3 := TVamTextBox.Create(self);
  FTextBox3.Font := self.Font;
  FTextBox3.HitTest := false;
  FTextBox3.Parent := FBackground;
  FTextBox3.Text   := 'Text';
  FTextBox3.TextAlign := TRedFoxAlign.AlignCenter;
  FTextBox3.TextVAlign := TRedFoxAlign.AlignCenter;
  FTextBox3.Color          := '$00000000';
  FTextBox3.ColorMouseOver := '$66FFFFFF';

  FModAmountTextBox := TVamTextBox.Create(self);
  FModAmountTextBox.Font := self.Font;
  FModAmountTextBox.HitTest := false;
  FModAmountTextBox.Parent := FBackground;
  FModAmountTextBox.Text   := '100%';
  FModAmountTextBox.TextAlign := TRedFoxAlign.AlignCenter;
  FModAmountTextBox.TextVAlign := TRedFoxAlign.AlignCenter;
  FModAmountTextBox.Color          := '$00000000';
  FModAmountTextBox.ColorMouseOver := '$66FFFFFF';
  fModAmountTextBox.Visible := false;

  fKnob1    := TVamKnob.Create(self);
  fKnob1.HitTest := false;
  fKnob1.Parent := FBackground;
  fKnob1.OnKnobPosChanged := self.EventHandler_KnobChanged;



  fColor_BackgroundMouseOver   := '$00000000';
  fColor_Background            := '$FF66dd66';
  fColor_SectionFocused        := '$FFFFFFFF';


  UpdateColors;

end;

destructor TVamCompoundModMatrixSection.Destroy;
begin
  fTextBox1.Free;
  fTextBox2.Free;
  fTextBox3.Free;
  fModAmountTextBox.Free;
  fKnob1.Free;
  FBackground.Free;
  inherited;
end;

procedure TVamCompoundModMatrixSection.CMFontChanged(var Message: TMessage);
begin
  inherited;

  if assigned(TextBox1)
    then FTextBox1.Font := self.Font;

  if assigned(TextBox2)
    then FTextBox2.Font := self.Font;

  if assigned(TextBox3)
    then TextBox3.Font := self.Font;

  if assigned(ModAmountTextBox)
    then ModAmountTextBox.Font := self.Font;
end;



function TVamCompoundModMatrixSection.GetControlElementOffset(const CE: TControlElements): integer;
begin
  case CE of
    egNone:     result := 0;
    egTextBox1: result := FTextBox1.Left + (FTextBox1.Width div 2);
    egTextBox2: result := FTextBox2.Left + (FTextBox2.Width div 2);
    egTextBox3: result := TextBox3.Left  + (TextBox3.Width div 2);
    egKnob1:    result := fKnob1.Left    + (fKnob1.Width div 2);
  else
    raise Exception.Create('Unexpected CE value.');
  end;
end;

function TVamCompoundModMatrixSection.GetKnobPos(const Index: Integer): single;
begin
  case Index of
    0: result := fKnob1.Pos;
  else
    raise Exception.Create('Index not handled.');
  end;
end;

procedure TVamCompoundModMatrixSection.SetKnobPos(const Index: Integer; const Value: single);
begin
  case Index of
    0:
    begin
      fKnob1.Pos := Value;
      UpdateModAmountText;
    end
  else
    raise Exception.Create('Index not handled.');
  end;
end;



procedure TVamCompoundModMatrixSection.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (assigned(self.Parent)) and (aWidth <> 0) and (aHeight <> 0) then UpdateLayout;
end;

procedure TVamCompoundModMatrixSection.SetColors(const Index: Integer; const Value: TRedFoxColorString);
var
  xColor : PRedFoxColorString;
begin
  case Index of
    0 : xColor := @fColor_Background;
    1 : xColor := @fColor_BackgroundMouseOver;
    2 : xColor := @fColor_SectionFocused;
  else
    raise Exception.Create('Index not handled');
  end;

  if xColor^ <> Value then
  begin
    xColor ^ := Value;
    UpdateColors;
  end;

end;

procedure TVamCompoundModMatrixSection.SetElementFocused(const Value: TControlElements);
begin
  if Value <> fElementFocused then
  begin
    fElementFocused := Value;
    UpdateColors;
  end;
end;

procedure TVamCompoundModMatrixSection.SetElementGrabbed(const Value: TControlElements);
begin
  if Value <> fElementGrabbed then
  begin
    fElementGrabbed := Value;
    UpdateColors;
  end;
end;

procedure TVamCompoundModMatrixSection.SetElementMouseOver(const Value: TControlElements);
begin
  if Value <> fElementMouseOver then
  begin
    fElementMouseOver := Value;
    UpdateColors;
  end;
end;

procedure TVamCompoundModMatrixSection.SetFont(const Value: TFont);
begin
  inherited;
  TextBox1.Font := Value;
  TextBox2.Font := Value;
  TextBox3.Font := Value;
  ModAmountTextBox.Font := Value;
end;

procedure TVamCompoundModMatrixSection.SetText1(const Value: string);
begin
  FTextBox1.Text := Value;
end;

procedure TVamCompoundModMatrixSection.SetText2(const Value: string);
begin
  FTextBox2.Text := Value;
end;

procedure TVamCompoundModMatrixSection.SetText3(const Value: string);
begin
  TextBox3.Text := Value;
end;

procedure TVamCompoundModMatrixSection.UpdateColors;
begin
  if (ElementMouseOver <> egTextBox1) and (ElementGrabbed <> egTextBox1) and (ElementFocused <> egTextBox1)
    then FTextBox1.Color := '$00000000'
    else FTextBox1.Color := fColor_SectionFocused;

  if (ElementMouseOver <> egTextBox2) and (ElementGrabbed <> egTextBox2) and (ElementFocused <> egTextBox2)
    then FTextBox2.Color := '$00000000'
    else FTextBox2.Color := fColor_SectionFocused;

  if (ElementMouseOver <> egTextBox3) and (ElementGrabbed <> egTextBox3) and (ElementFocused <> egTextBox3)
    then TextBox3.Color := '$00000000'
    else TextBox3.Color := fColor_SectionFocused;

  if self.MouseInClient
    then FBackground.Color := fColor_BackgroundMouseOver
    else FBackground.Color := fColor_Background;

end;

procedure TVamCompoundModMatrixSection.UpdateLayout;
var
  TBWidth : integer;
  xPos : integer;
begin
  //== set FBackground properties ==
  FBackground.Top    := 0;
  FBackground.Left   := 0;
  FBackground.Height := self.Height;
  FBackground.Width  := self.Width;

  //== Set knob height+width ==
  fKnob1.Width  := self.Height;
  fKnob1.Height := self.Height;

  ModAmountTextBox.Height := self.Height;
  ModAmountTextBox.Width  := self.Height * 2;

  //== Set text box height+width ==
  TBWidth := (self.Width - fKnob1.Width - ModAmountTextBox.Width) div 3;

  FTextBox1.Height := self.Height;
  FTextBox1.Width  := TBWidth;

  FTextBox2.Height := self.Height;
  FTextBox2.Width  := TBWidth;

  TextBox3.Height := self.Height;
  TextBox3.Width  := TBWidth;


  //== Set control positions ==
  xPos := 0;
  FTextBox1.Top    := 0;
  FTextBox1.Left   := xPos;

  xPos := xPos + FTextBox1.Width;
  fKnob1.Top  := 0;
  fKnob1.Left := xPos;

  xPos := xPos + fKnob1.Width;
  ModAmountTextBox.Top := 0;
  ModAmountTextBox.Left := xPos;

  xPos := xPos + ModAmountTextBox.Width;
  FTextBox2.Top    := 0;
  FTextBox2.Left   := xPos;

  xPos := xPos + FTextBox2.Width;
  TextBox3.Top    := 0;
  TextBox3.Left   := xPos;


  //== adjust the text box boundaries so they are not hard against each other. ==
  TextBox1.Layout.AdjustBounds(0,0,-4,0);
  TextBox2.Layout.AdjustBounds(-4,0,0,0);
  TextBox3.Layout.AdjustBounds(-5,0,1,0);

end;

procedure TVamCompoundModMatrixSection.UpdateModAmountText;
var
  s : string;
begin
  s := IntToStr(round(Knob1.Pos * 200 - 100)) + '%';
  if ModAmountTextBox.Text <> s then  ModAmountTextBox.Text := s;
end;

procedure TVamCompoundModMatrixSection.WMFontChange(var Message: TMessage);
begin
  inherited;
end;

procedure TVamCompoundModMatrixSection.MouseEnter;
begin
  inherited;
  UpdateColors;
  UpdateLayout;
  ModAmountTextBox.Visible := true;
end;

procedure TVamCompoundModMatrixSection.MouseLeave;
begin
  inherited;
  ElementMouseOver := TControlElements.egNone;
  UpdateColors;
  ModAmountTextBox.Visible := false;
end;

procedure TVamCompoundModMatrixSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ModX, ModY : integer;
begin
  inherited;

  ElementGrabbed := egNone;


  if (Button = mbLeft) and (TextBox1.BoundsRect.Contains(Point(x,y))) then
  begin
    if assigned(OnText1MouseDown) then OnText1MouseDown(self);
  end;

  if (Button = mbLeft) and (TextBox2.BoundsRect.Contains(Point(x,y))) then
  begin
    if assigned(OnText2MouseDown) then OnText2MouseDown(self);
  end;

  if (Button = mbLeft) and (TextBox3.BoundsRect.Contains(Point(x,y))) then
  begin
    if assigned(OnText3MouseDown) then OnText3MouseDown(self);
  end;

  if (Button = mbLeft) and (Knob1.BoundsRect.Contains(Point(x,y))) and (ssCtrl in Shift = false) then
  begin
    ElementGrabbed := TControlElements.egKnob1;

    ModX := x - fKnob1.Left;
    ModY := y - fKnob1.Top;
    TKnobHack(fKnob1).MouseDown(Button, Shift, ModX, ModY);
  end;

  if (Button = mbLeft) and (Knob1.BoundsRect.Contains(Point(x,y))) and (ssCtrl in Shift = true) then
  begin
    if assigned(OnReset) then OnReset(self);
  end;

end;

procedure TVamCompoundModMatrixSection.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ModX, ModY : integer;
begin
  inherited;

  if ElementGrabbed = TControlElements.egNone then
  begin
    if (FTextBox1.BoundsRect.Contains(Point(x,y))) then ElementMouseOver := TControlElements.egTextBox1;
    if (FTextBox2.BoundsRect.Contains(Point(x,y))) then ElementMouseOver := TControlElements.egTextBox2;
    if (TextBox3.BoundsRect.Contains(Point(x,y)))  then ElementMouseOver := TControlElements.egTextBox3;
    if (fKnob1.BoundsRect.Contains(Point(x,y)))    then ElementMouseOver := TControlElements.egKnob1;
  end;

  if ElementGrabbed = TControlElements.egKnob1 then
  begin
    ModX := x - fKnob1.Left;
    ModY := y - fKnob1.Top;
    TKnobHack(fKnob1).MouseMove(Shift, ModX, ModY);
  end;

end;

procedure TVamCompoundModMatrixSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ModX, ModY : integer;
begin
  inherited;

  if (Button = mbLeft) and (ElementGrabbed = TControlElements.egKnob1) then
  begin
    ModX := x - fKnob1.Left;
    ModY := y - fKnob1.Top;
    TKnobHack(fKnob1).MouseUp(Button, Shift, ModX, ModY);

    ElementGrabbed := TControlElements.egNone;
  end;

end;

procedure TVamCompoundModMatrixSection.EventHandler_KnobChanged(Sender: TObject);
begin
  UpdateModAmountText;
  if (Sender = fKnob1) and (assigned(OnKnob1Changed)) then OnKnob1Changed(self);
end;





end.
