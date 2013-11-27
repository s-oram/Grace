unit LucidityControl.ModSection;

interface

uses
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl,
  uGuiFeedbackData, VamTextBox, VamLabel, VamPanel, VamSlider;

type
  TModSectionActiveControl = (acNone, acSlider);
  TModSectionDisplayState = (dsDefault, dsModSourceSelected, dsModViaSelected);

  TModSection = class(TVamWinControl)
  private
    fDisplayState: TModSectionDisplayState;
    fOnShowModViaMenu: TNotifyEvent;
    fOnShowModSourceMenu: TNotifyEvent;
    fIsEnabled: boolean;
    procedure SetDisplayState(const Value: TModSectionDisplayState);
    procedure SetIsEnabled(const Value: boolean);
  protected
    Background : TVamPanel;
    fTextBox1,
    fTextBox3 : TVamTextBox;
    TextBoxes : TArray<TWinControl>;

    fSlider : TVamSlider;

    IsMouseOver : boolean;
    ActiveControl : TModSectionActiveControl;

    procedure UpdateDisplay;
  protected
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;


    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property TextboxA : TVamTextBox read fTextBox1;
    property Slider   : TVamSlider  read fSlider;
    property TextboxC : TVamTextBox read fTextBox3;

    procedure ResetDisplayState;

    property DisplayState : TModSectionDisplayState read fDisplayState write SetDisplayState;

    property IsEnabled : boolean read fIsEnabled write SetIsEnabled;

    property OnShowModSourceMenu : TNotifyEvent read fOnShowModSourceMenu write fOnShowModSourceMenu;
    property OnShowModViaMenu    : TNotifyEvent read fOnShowModViaMenu    write fOnShowModViaMenu;



  end;

implementation

uses
  SysUtils,
  uGuiUtils,
  uConstants;


type
  // Use a hack to gain access to protected methods.
  TSliderHack = class(TVamSlider)
  end;



{ TModSection }

constructor TModSection.Create(AOwner: TComponent);
var
  c1: Integer;

begin
  inherited;

  Background := TVamPanel.Create(nil);
  Background.Parent := self;
  Background.HitTest := false;


  Setlength(TextBoxes, 2);

  fTextBox1 := TVamTextBox.Create(nil);
  fTextBox1.Parent := self;

  fTextBox3 := TVamTextBox.Create(nil);
  fTextBox3.Parent := self;

  TextBoxes[0] := fTextBox1;
  TextBoxes[1] := fTextBox3;


  for c1 := 0 to 1 do
  begin
    (TextBoxes[c1] as TVamTextBox).HitTest := false;
    (TextBoxes[c1] as TVamTextBox).Text := 'bang';
    (TextBoxes[c1] as TVamTextBox).Font.Color := GetRedFoxColor(kColor_LcdDark5);
    (TextBoxes[c1] as TVamTextBox).Color := kColor_LcdDark1;;
    (TextBoxes[c1] as TVamTextBox).ColorMouseOver    := kColor_ButtonMouseOver;
    (TextBoxes[c1] as TVamTextBox).TextVAlign := TRedFoxAlign.AlignCenter;
    (TextBoxes[c1] as TVamTextBox).TextAlign  := TRedFoxAlign.AlignCenter;
  end;


  fSlider := TVamSlider.Create(nil);
  fSlider.Parent := self;
  fSlider.HitTest := false;
  fSlider.ColorSlider := GetRedFoxColor(kColor_LcdDark5, 230);


  // finally...
  UpdateDisplay;
end;

destructor TModSection.Destroy;
begin
  fTextBox1.Free;
  fTextBox3.Free;
  Setlength(TextBoxes, 0);

  fSlider.Free;

  Background.Free;

  inherited;
end;


procedure TModSection.AlignControls(AControl: TControl; var Rect: TRect);
var
  c1 : integer;
  xLeft  : integer;
  xWidth : integer;


begin
  inherited;

  Background.Width   := self.Width;
  Background.Height  := self.Height;
  Background.Top     := 0;
  Background.Left    := 0;
  Background.Color   := kColor_LcdDark1;
  background.Visible := true;


  xWidth := (self.Width - 16) div 3;

  for c1 := 0 to 1 do
  begin
    (TextBoxes[c1] as TVamTextBox).Width := xWidth;
    (TextBoxes[c1] as TVamTextBox).Height := 20;
    (TextBoxes[c1] as TVamTextBox).Top    := 0;
  end;

  SpreadControls_Horz(TArray<TControl>(TextBoxes), self);


  fSlider.Width := xWidth;
  fSlider.Height := 14;
  fSlider.Top    := 3;
  fSlider.Left   := fTextBox1.Left + fTextBox1.Width + 8;


end;


procedure TModSection.MouseEnter;
begin
  inherited;
  IsMouseOver := true;
  DisplayState := dsDefault;
  UpdateDisplay;
end;

procedure TModSection.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  mx, my : integer;
begin
  inherited;



  if (IsEnabled) and (Button = mbLeft) and (TextBoxA.BoundsRect.Contains(Point(x,y))) then
  begin
    DisplayState := dsModSourceSelected;
    if assigned(OnShowModSourceMenu) then OnShowModSourceMenu(self);
  end;

  if (IsEnabled) and (Button = mbLeft) and (TextBoxC.BoundsRect.Contains(Point(x,y))) then
  begin
    DisplayState := dsModViaSelected;
    if assigned(OnShowModViaMenu) then OnShowModViaMenu(self);
  end;

  if (IsEnabled) and (Button = mbLeft) and (Slider.BoundsRect.Contains(Point(x,y))) then
  begin
    ActiveControl := acSlider;
    mx := x - Slider.Left;
    my := y - Slider.Top;
    TSliderHack(Slider).MouseDown(Button, Shift, mX, mY);
  end;



end;

procedure TModSection.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  mx, my : integer;
begin
  inherited;

  if (IsEnabled) and (ActiveControl = acSlider) then
  begin
    mx := x - Slider.Left;
    my := y - Slider.Top;
    TSliderHack(Slider).MouseMove(Shift, mX, mY);
  end;


end;

procedure TModSection.MouseUp(Button: TMouseButton; Shift: TShiftState; X,  Y: Integer);
var
  mx, my : integer;
begin
  inherited;

  if (Button = mbLeft) and (ActiveControl = acSlider) then
  begin
    ActiveControl := acNone;
    mx := x - Slider.Left;
    my := y - Slider.Top;
    TSliderHack(Slider).MouseUp(Button, Shift, mX, mY);
  end;

end;


procedure TModSection.ResetDisplayState;
begin
  fDisplayState := dsDefault;
  UpdateDisplay;
end;

procedure TModSection.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  UpdateDisplay;
end;

procedure TModSection.SetDisplayState(const Value: TModSectionDisplayState);
begin
  if Value <> fDisplayState then
  begin
    fDisplayState := Value;
    UpdateDisplay;
  end;
end;

procedure TModSection.SetIsEnabled(const Value: boolean);
begin
  if fIsEnabled <> Value then
  begin
    fIsEnabled := Value;
    UpdateDisplay;
  end;
end;

procedure TModSection.UpdateDisplay;
begin
  if IsEnabled then
  begin
    Slider.Visible   := true;
    TextBoxA.Visible := true;
    TextBoxC.Visible := true;
  end else
  begin
    Slider.Visible   := false;
    TextBoxA.Visible := false;
    TextBoxC.Visible := false;
  end;


  if DisplayState = dsModSourceSelected then
  begin
    Background.Color   := kColor_LcdDark1;
    TextBoxA.Color     := kColor_ButtonMouseOver;
    TextBoxC.Color     := kColor_LcdDark1;
  end else
  if DisplayState = dsModViaSelected then
  begin
    Background.Color   := kColor_LcdDark1;
    TextBoxA.Color     := kColor_LcdDark1;
    TextBoxC.Color     := kColor_ButtonMouseOver;
  end else
  if (IsMouseOver) and (IsEnabled) then
  begin
    Background.Color   := kColor_ButtonMouseOver;
    TextBoxA.Color     := kColor_ButtonMouseOver;
    TextBoxC.Color     := kColor_ButtonMouseOver;
  end else
  begin
    Background.Color   := kColor_LcdDark1;
    TextBoxA.Color     := kColor_LcdDark1;
    TextBoxC.Color     := kColor_LcdDark1;
  end;
end;

end.
