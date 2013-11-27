unit VamButton;

interface

uses
  Types, Controls, Classes, Graphics,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamGraphicControl, VamWinControl;

type
  TVamButtonState = (bsOff, bsOn);

  TVamButton = class(TVamWinControl)
  private
    fOnChanged: TNotifyEvent;
    fTextVAlign: TRedFoxAlign;
    fText: string;
    fTextAlign: TRedFoxAlign;
    fButtonState: TVamButtonState;
    fShowBorder: boolean;
    fColor_Border: TRedFoxColorString;
    fImageOn: TBitmap;
    fImageOff: TBitmap;
    procedure SetText(const Value: string);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    procedure SetButtonState(const Value: TVamButtonState);
    function GetColorOffA: TRedFoxColorString;
    function GetColorOffB: TRedFoxColorString;
    function GetColorOnA: TRedFoxColorString;
    function GetColorOnB: TRedFoxColorString;
    procedure SetColorOffA(const Value: TRedFoxColorString);
    procedure SetColorOffB(const Value: TRedFoxColorString);
    procedure SetColorOnA(const Value: TRedFoxColorString);
    procedure SetColorOnB(const Value: TRedFoxColorString);
    procedure SetColor_Border(const Value: TRedFoxColorString);
    procedure SetShowBroder(const Value: boolean);
    procedure SetImageOn(const Value: TBitmap);
    procedure SetImageOff(const Value: TBitmap);
    function GetIsOn: boolean;
    procedure SetIsOn(const Value: boolean);
  protected
    IsGrabbed : boolean;
    IsMouseOver : boolean;

    fColorOnA  : TRedFoxColor;
    fColorOnB  : TRedFoxColor;
    fColorOffA : TRedFoxColor;
    fColorOffB : TRedFoxColor;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure Changed;

    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsOn : boolean read GetIsOn write SetIsOn;

  published
    property ShowBorder : boolean read fShowBorder write SetShowBroder;
    property Color_Border : TRedFoxColorString read fColor_Border write SetColor_Border;

    property ColorOnA  : TRedFoxColorString read GetColorOnA write SetColorOnA;
    property ColorOnB  : TRedFoxColorString read GetColorOnB write SetColorOnB;
    property ColorOffA : TRedFoxColorString read GetColorOffA write SetColorOffA;
    property ColorOffB : TRedFoxColorString read GetColorOffB write SetColorOffB;

    property ButtonState : TVamButtonState read fButtonState write SetButtonState;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : string read fText write SetText;
    property Font;

    property ImageOn  : TBitmap read fImageOn  write SetImageOn;
    property ImageOff : TBitmap read fImageOff write SetImageOff;

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils;

{ TVamButton }

constructor TVamButton.Create(AOwner: TComponent);
begin
  inherited;

  fShowBorder := false;
  fColor_Border := '$FF242B39';

  fColorOffA.SetColor('$FFF99595');
  fColorOffB.SetColor('$FFF96969');
  fColorOnA.SetColor('$FF96F9D3');
  fColorOnB.SetColor('$FF59F9BC');
end;

destructor TVamButton.Destroy;
begin

  inherited;
end;

function TVamButton.GetColorOffA: TRedFoxColorString;
begin
  result := fColorOffA.AsString;
end;

function TVamButton.GetColorOffB: TRedFoxColorString;
begin
  result := fColorOffB.AsString;
end;

function TVamButton.GetColorOnA: TRedFoxColorString;
begin
  result := fColorOnA.AsString;
end;

function TVamButton.GetColorOnB: TRedFoxColorString;
begin
  result := fColorOnB.AsString;
end;

function TVamButton.GetIsOn: boolean;
begin
  case ButtonState of
    bsOff: result := false;
    bsOn:  result := true;
  else
    raise Exception.Create('Type not handled.');
  end;
end;

procedure TVamButton.Changed;
begin
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TVamButton.MouseEnter;
begin
  inherited;

  IsMouseOver := true;
  Invalidate;
end;

procedure TVamButton.MouseLeave;
begin
  inherited;

  IsMouseOver := false;
  Invalidate;
end;



procedure TVamButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  IsGrabbed   := true;
  IsMouseOver := true;
  Invalidate;
end;

procedure TVamButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (IsGrabbed) then
  begin
    if InRect(x,y, Rect(0,0,Width, Height)) then
    begin
      if IsMouseOver <> true then
      begin
        IsMouseOver := true;
        Invalidate;
      end;
    end else
    begin
      if IsMouseOver <> false then
      begin
        IsMouseOver := false;
        Invalidate;
      end;
    end;
  end;


end;

procedure TVamButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (IsGrabbed) then
  begin
    IsGrabbed := false;


    if InRect(x,y, Rect(0,0,Width, Height)) then
    begin
      case fButtonState of
        bsOff: fButtonState := bsOn;
        bsOn:  fButtonState := bsOff;
      end;

      Changed;
    end;

    Invalidate;
  end;

end;

procedure TVamButton.SetButtonState(const Value: TVamButtonState);
begin
  if Value <> fButtonState then
  begin
    fButtonState := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetColorOffA(const Value: TRedFoxColorString);
begin
  if Value <> fColorOffA.AsString then
  begin
    fColorOffA.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamButton.SetColorOffB(const Value: TRedFoxColorString);
begin
  if Value <> fColorOffB.AsString then
  begin
    fColorOffB.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamButton.SetColorOnA(const Value: TRedFoxColorString);
begin
  if Value <> fColorOnA.AsString then
  begin
    fColorOnA.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamButton.SetColorOnB(const Value: TRedFoxColorString);
begin
  if Value <> fColorOnB.AsString then
  begin
    fColorOnB.SetColor(Value);
    Invalidate;
  end;
end;

procedure TVamButton.SetColor_Border(const Value: TRedFoxColorString);
begin
  if Value <> fColor_Border then
  begin
    fColor_Border := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetImageOff(const Value: TBitmap);
begin
  if Value <> fImageOff then
  begin
    fImageOff := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetImageOn(const Value: TBitmap);
begin
  if fImageOn <> Value then
  begin
    fImageOn := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetIsOn(const Value: boolean);
begin
  if (Value = true) and (fButtonState <> bsOn)  then
  begin
    fButtonState := bsOn;
    Invalidate;
  end else
  if (Value = false) and (fButtonState <> bsOff)  then
  begin
    fButtonState := bsOff;
    Invalidate;
  end;

end;

procedure TVamButton.SetShowBroder(const Value: boolean);
begin
  if Value <> fShowBorder then
  begin
    fShowBorder := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetText(const Value: string);
begin
  if Value <> fText then
  begin
    fText := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    Invalidate;
  end;
end;

procedure TVamButton.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    Invalidate;
  end;
end;

procedure TVamButton.Paint;
var
  TextBounds : TRect;
  ButtonColor : TRedFoxColor;
  SrcRect : TRect;
  DstRect : TRect;
  aImage : TBitmap;
begin
  inherited;

  if ButtonState = bsOn then
  begin
    //== Button Is On ===
    if (IsGrabbed) then
    begin
      if IsMouseOver
        then ButtonColor := fColorOffB
        else ButtonColor := fColorOnB;
    end else
    begin
      if IsMouseOver = false
        then ButtonColor := fColorOnA
        else ButtonColor := fColorOnB;
    end;
  end else
  begin
    //== Button Is Off ===
    if (IsGrabbed) then
    begin
      if IsMouseOver
        then ButtonColor := fColorOnB
        else ButtonColor := fColorOffB;
    end else
    begin
      if IsMouseOver = false
        then ButtonColor := fColorOffA
        else ButtonColor := fColorOffB;
    end;
  end;


  BackBuffer.BufferInterface.ClearAll(255,255,255,0);

  BackBuffer.BufferInterface.NoLine;
  BackBuffer.BufferInterface.FillColor := ButtonColor.AsAggRgba8;
  BackBuffer.BufferInterface.RoundedRect(0, 0, Width, Height, 3);

  if ShowBorder then
  begin
    BackBuffer.BufferInterface.NoFill;
    BackBuffer.BufferInterface.LineColor := TRedFoxColor(Color_Border);
    BackBuffer.BufferInterface.LineWidth := 1;

    BackBuffer.BufferInterface.RoundedRect(0, 0, Width, Height, 3);
  end;

  //== draw the button text ====
  TextBounds := Rect(0,0, Width, Height);
  BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);

  if ButtonState = bsOn
    then aImage := ImageOn
    else aImage := ImageOff;

  if (assigned(aImage)) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := aImage.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := aImage.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(aImage, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;


  {
  if (ButtonState = bsOn) and (assigned(ImageOn)) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := ImageOn.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := ImageOn.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(ImageOn, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;

  if (ButtonState = bsOff) and (assigned(ImageOff)) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := ImageOff.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := ImageOff.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(ImageOff, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;
  }
end;

end.
