unit VamCompoundNumericKnob;

interface

uses
  VamGuiControlInterfaces,
  Types, Controls, Classes, StdCtrls, Graphics,
  RedFox, RedFoxColor,
  VamWinControl, VamScrollBar, VamDiv,
  VamNumericKnob, VamArrows, VamLabel;

type
  TVamCompoundNumericKnob = class(TVamWinControl, IKnobControl)
  private
    //==========================================================================
    // These procedures were added to satisfy IKnobControl.
    fParameterIndex : integer;
    fModAmount : single;
    procedure SetParameterIndex(Index : integer);
    function  GetParameterIndex:integer;
    function  KnobControl_GetKnobValue : single;
    procedure KnobControl_SetKnobValue(Value : single);
    function  GetModAmountValue : single;
    procedure SetModAmountValue(Value : single);
    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);
    procedure SetOnKnobPosChanged(Handler:TNotifyEvent);
    procedure SetOnModAmountChanged(Handler:TNotifyEvent);

    procedure SetParameterName(aName:string);
    function GetParameterName:string;

    function GetKnobMode:TKnobMode;
    procedure SetKnobMode(const Value: TKnobMode);

    function  IKnobControl.GetKnobValue = KnobControl_GetKnobValue;
    procedure IKnobControl.SetKnobValue = KnobControl_SetKnobValue;
    //==========================================================================
  private
    fColor_Arrows2: TRedFoxColorString;
    fColor_Arrows1: TRedFoxColorString;
    fColor_Label: TColor;
    fColor_Numeric: TColor;
    fOnChanged: TNotifyEvent;
    fOnRotaryStepUp: TNotifyEvent;
    fOnRotaryStepDown: TNotifyEvent;
    fColor_Background: TRedFoxColorString;
    fParameterName: string;

    procedure SetColor_Arrows1(const Value: TRedFoxColorString);
    procedure SetColor_Arrows2(const Value: TRedFoxColorString);
    procedure SetColor_Label(const Value: TColor);
    procedure SetColor_Numeric(const Value: TColor);
    function GetKnobValue: double;
    procedure SetKnobValue(const Value: double);
    function GetKnobMax: integer;
    function GetKnobMin: integer;
    procedure SetKnobMax(const Value: integer);
    procedure SetKnobMin(const Value: integer);
    function GetKnobDecimalPlaces: integer;
    function GetKnobNumericStyle: TNumericStyle;
    procedure SetKnobDecimalPlaces(const Value: integer);
    procedure SetKnobNumericStyle(const Value: TNumericStyle);
    function GetText: string;
    procedure SetText(const Value: string);
    function GetUnits: string;
    procedure SetUnits(const Value: string);
    function GetCustomText: string;
    procedure SetCustomText(const Value: string);
    procedure SetColor_Background(const Value: TRedFoxColorString);

    procedure DoPaddingChange(Sender: TObject);
    function GetKnobSensitivity: single;
    procedure SetKnobSensitivity(const Value: single);




  protected
    Arrows       : TVamArrows;
    Knob         : TVamNumericKnob;
    ControlLabel : TVamLabel;

    procedure SetFont(const Value: TFont); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure Changed(Sended : TObject);
    procedure StepUp(Sender : TObject);
    procedure StepDown(Sender : TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Font;

    property Text  : string read GetText  write SetText;
    property Units : string read GetUnits write SetUnits;

    property Color_Background : TRedFoxColorString read fColor_Background write SetColor_Background;
    property Color_Label   : TColor             read fColor_Label   write SetColor_Label;
    property Color_Numeric : TColor             read fColor_Numeric write SetColor_Numeric;
    property Color_Arrows1 : TRedFoxColorString read fColor_Arrows1 write SetColor_Arrows1;
    property Color_Arrows2 : TRedFoxColorString read fColor_Arrows2 write SetColor_Arrows2;

    property KnobValue         : double        read GetKnobValue         write SetKnobValue;
    property KnobMin           : integer       read GetKnobMin           write SetKnobMin;
    property KnobMax           : integer       read GetKnobMax           write SetKnobMax;
    property KnobNumericStyle  : TNumericStyle read GetKnobNumericStyle  write SetKnobNumericStyle;
    property KnobDecimalPlaces : integer       read GetKnobDecimalPlaces write SetKnobDecimalPlaces;

    property KnobSensitivity : single read GetKnobSensitivity write SetKnobSensitivity;

    property KnobCustomText : string read GetCustomText write SetCustomText;

    // typically used to store the linked parameter name.
    property ParameterName  : string  read fParameterName  write fParameterName;

    property OnChanged        : TNotifyEvent read fOnChanged        write fOnChanged;
    property OnRotaryStepUp   : TNotifyEvent read fOnRotaryStepUp   write fOnRotaryStepUp;
    property OnRotaryStepDown : TNotifyEvent read fOnRotaryStepDown write fOnRotaryStepDown;

    property Padding;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

type
  TKnobHack = class(TVamNumericKnob)
  end;

{ TVamCompoundNumericKnob }

constructor TVamCompoundNumericKnob.Create(AOwner: TComponent);
begin
  inherited;

  Padding.OnChange := DoPaddingChange;

  fColor_Background := '$00000000';

  ControlLabel := TVamLabel.Create(self);
  ControlLabel.AutoSize := true;
  ControlLabel.Parent := self;
  ControlLabel.Width := 30;
  ControlLabel.Align := alLeft;
  ControlLabel.Top := 0;
  ControlLabel.Left := 0;
  ControlLabel.Text := 'Label';
  ControlLabel.TextAlign  := TRedFoxAlign.AlignNear;
  ControlLabel.TextVAlign := TRedFoxAlign.AlignCenter;
  ControlLabel.HitTest := false;
  ControlLabel.IsSubComponent := true;

  Arrows := TVamArrows.Create(self);
  Arrows.Parent := self;
  Arrows.Width := 8;
  Arrows.Align := alRight;
  Arrows.Visible := true;
  Arrows.HitTest := false;
  Arrows.IsSubComponent := true;

  Knob := TVamNumericKnob.Create(self);
  Knob.Parent := self;
  Knob.Height := 20;
  Knob.Width := 200;
  Knob.Left := 2;
  Knob.Top  := 0;
  Knob.Align := alClient;
  Knob.Visible := true;
  Knob.HitTest := false;
  Knob.OnChanged := Changed;
  Knob.OnRotaryStepUp   := StepUp;
  Knob.OnRotaryStepDown := StepDown;
  Knob.IsSubComponent := true;

  Color_Label   := clGray;
  Color_Numeric := clBlack;
  Color_Arrows1 := '$cc000000';
  Color_Arrows2 := '$FF000000';
end;

destructor TVamCompoundNumericKnob.Destroy;
begin
  ControlLabel.Free;
  Arrows.Free;
  Knob.Free;
  inherited;
end;

function TVamCompoundNumericKnob.KnobControl_GetKnobValue: single;
var
  KnobRange : single;
begin
  KnobRange := (KnobMax - KnobMin);
  if KnobRange <> 0
    then result := (KnobValue - KnobMin) / KnobRange
    else result := 0;
end;

procedure TVamCompoundNumericKnob.KnobControl_SetKnobValue(Value: single);
var
  KnobRange : single;
begin
  KnobRange := (KnobMax - KnobMin);
  KnobValue := (Value * KnobRange) + KnobMin;
end;

procedure TVamCompoundNumericKnob.DoPaddingChange(Sender: TObject);
begin
  Realign;
end;

function TVamCompoundNumericKnob.GetCustomText: string;
begin
  result := Knob.CustomText;
end;

function TVamCompoundNumericKnob.GetKnobDecimalPlaces: integer;
begin
  result := Knob.DecimalPlaces;
end;

function TVamCompoundNumericKnob.GetKnobMax: integer;
begin
  result := Knob.KnobMax;
end;

function TVamCompoundNumericKnob.GetKnobMin: integer;
begin
  result := Knob.KnobMin;
end;

function TVamCompoundNumericKnob.GetKnobMode: TKnobMode;
begin
  result := TKnobMode.PositionEdit;
end;

function TVamCompoundNumericKnob.GetKnobNumericStyle: TNumericStyle;
begin
  result := Knob.NumericStyle;
end;

function TVamCompoundNumericKnob.GetKnobSensitivity: single;
begin
  result := Knob.Sensitivity;
end;

function TVamCompoundNumericKnob.GetKnobValue: double;
begin
  result := Knob.KnobValue;
end;

function TVamCompoundNumericKnob.GetModAmountValue: single;
begin
  result := fModAmount;
end;

function TVamCompoundNumericKnob.GetParameterIndex: integer;
begin
  result := fParameterIndex;
end;

function TVamCompoundNumericKnob.GetParameterName: string;
begin
  result := fParameterName;
end;

function TVamCompoundNumericKnob.GetText: string;
begin
  result := ControlLabel.Text;
end;

function TVamCompoundNumericKnob.GetUnits: string;
begin
  result := Knob.Units;
end;

procedure TVamCompoundNumericKnob.Changed(Sended: TObject);
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamCompoundNumericKnob.SetBounds(ALeft, ATop, AWidth,  AHeight: Integer);
begin
  inherited;

end;

procedure TVamCompoundNumericKnob.SetColor_Arrows1(const Value: TRedFoxColorString);
begin
  if Value <> fColor_Arrows1 then
  begin
    fColor_Arrows1 := Value;
    Arrows.Color := fColor_Arrows1;
  end;
end;

procedure TVamCompoundNumericKnob.SetColor_Arrows2(const Value: TRedFoxColorString);
begin
  if Value <> fColor_Arrows2 then
  begin
    fColor_Arrows2 := Value;
  end;
end;

procedure TVamCompoundNumericKnob.SetColor_Background(const Value: TRedFoxColorString);
begin
  fColor_Background := Value;
end;

procedure TVamCompoundNumericKnob.SetColor_Label(const Value: TColor);
begin
  fColor_Label := Value;
  ControlLabel.Font.Color := fColor_Label;
end;

procedure TVamCompoundNumericKnob.SetColor_Numeric(const Value: TColor);
begin
  fColor_Numeric := Value;
  Knob.Font.Color := fColor_Numeric;
end;

procedure TVamCompoundNumericKnob.SetCustomText(const Value: string);
begin
  Knob.CustomText := Value;
end;

procedure TVamCompoundNumericKnob.SetFont(const Value: TFont);
begin
  inherited;

  ControlLabel.Font := Value;
  Knob.Font := Value;

  Knob.Font.Color         := fColor_Numeric;
  ControlLabel.Font.Color := fColor_Label;
end;

procedure TVamCompoundNumericKnob.SetKnobDecimalPlaces(const Value: integer);
begin
  Knob.DecimalPlaces := Value;
end;

procedure TVamCompoundNumericKnob.SetKnobMax(const Value: integer);
begin
  Knob.KnobMax := Value;
end;

procedure TVamCompoundNumericKnob.SetKnobMin(const Value: integer);
begin
  Knob.KnobMin := Value;
end;

procedure TVamCompoundNumericKnob.SetKnobMode(const Value: TKnobMode);
begin
  // ignore. This is required by IKnobControl but the
  // compound numeric knob doesn't allow mod depth editing.
  // Maybe in the future this will be added.
end;

procedure TVamCompoundNumericKnob.SetKnobNumericStyle(const Value: TNumericStyle);
begin
  Knob.NumericStyle := Value;
end;

procedure TVamCompoundNumericKnob.SetKnobSensitivity(const Value: single);
begin
  Knob.Sensitivity := Value;
end;

procedure TVamCompoundNumericKnob.SetKnobValue(const Value: double);
begin
  Knob.KnobValue := Value;
end;

procedure TVamCompoundNumericKnob.SetModAmountValue(Value: single);
begin
  fModAmount := Value;
end;

procedure TVamCompoundNumericKnob.SetOnKnobPosChanged(Handler: TNotifyEvent);
begin
  OnChanged := Handler;
end;

procedure TVamCompoundNumericKnob.SetOnModAmountChanged(Handler: TNotifyEvent);
begin
  //do nothing.
end;

procedure TVamCompoundNumericKnob.SetOnMouseDown(Handler: TMouseEvent);
begin
  OnMouseDown := Handler;
end;

procedure TVamCompoundNumericKnob.SetOnMouseEnter(Handler: TNotifyEvent);
begin
  OnMouseEnter := Handler;
end;

procedure TVamCompoundNumericKnob.SetOnMouseLeave(Handler: TNotifyEvent);
begin
  OnMouseLeave := Handler;
end;

procedure TVamCompoundNumericKnob.SetOnMouseUp(Handler: TMouseEvent);
begin
  OnMouseUp := Handler;
end;

procedure TVamCompoundNumericKnob.SetParameterIndex(Index: integer);
begin
  fParameterIndex := Index;
end;

procedure TVamCompoundNumericKnob.SetParameterName(aName: string);
begin
  fParameterName := aName;
end;

procedure TVamCompoundNumericKnob.SetText(const Value: string);
begin
  ControlLabel.Text := Value;
end;

procedure TVamCompoundNumericKnob.SetUnits(const Value: string);
begin
  Knob.Units := Value;
end;

procedure TVamCompoundNumericKnob.StepDown(Sender: TObject);
begin
  if assigned(OnRotaryStepDown) then OnRotaryStepDown(self);
end;

procedure TVamCompoundNumericKnob.StepUp(Sender: TObject);
begin
  if assigned(OnRotaryStepUp) then OnRotaryStepUp(self);
end;

procedure TVamCompoundNumericKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  x := x - Knob.Left;
  y := y - Knob.Top;
  TKnobHack(Knob).MouseDown(Button, Shift, X, Y);
end;

procedure TVamCompoundNumericKnob.MouseEnter;
begin
  inherited;
  Arrows.Color := Color_Arrows2;
  //Arrows.Invalidate;
  Cursor := crSizeNS;
end;

procedure TVamCompoundNumericKnob.MouseLeave;
begin
  inherited;
  Arrows.Color := Color_Arrows1;
  //Arrows.Invalidate;
  Cursor := crDefault;
end;

procedure TVamCompoundNumericKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  x := x - Knob.Left;
  y := y - Knob.Top;
  TKnobHack(Knob).MouseMove(Shift, X, Y);
end;

procedure TVamCompoundNumericKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  x := x - Knob.Left;
  y := y - Knob.Top;
  TKnobHack(Knob).MouseUp(Button, Shift, X, Y);
end;



procedure TVamCompoundNumericKnob.Paint;
begin
  inherited;

  //NOTE: clear the background becuase it's not completed in the child component.
  BackBuffer.BufferInterface.ClearAll(0,0,0,0);


  BackBuffer.BufferInterface.FillColor := GetRedFoxColor(fColor_Background);
  BackBuffer.BufferInterface.NoLine;

  BackBuffer.BufferInterface.RoundedRect(0,0,Width,Height,3);


end;






end.


