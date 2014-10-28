unit VamMultiLineTextBox;

interface


uses
  Controls, WinApi.Windows, RedFoxTextBuffer,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamMultiLineTextBox = class(TVamWinControl)
  private
    FTextPadding: TPadding;
    fColor: TRedFoxColorString;
    fTextVAlign: TRedFoxAlign;
    fText: TStrings;
    fTextAlign: TRedFoxAlign;
    TextBuffer : TRedFoxTextBuffer;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetText(const Value: TStrings);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextPadding(const Value: TPadding);
    procedure SetTextVAlign(const Value: TRedFoxAlign);

    procedure DoTextPaddingChange(Sender: TObject);
  protected
    TargetHandle : HDC;
    procedure Paint; override;

    procedure UpdateTextBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure SetTargetHandle(aHandle : HDC);
  published
    property Color : TRedFoxColorString read fColor write SetColor;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : TStrings read fText write SetText;
    property TextPadding: TPadding read FTextPadding write SetTextPadding;
    property Font;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses

  SysUtils,
  Graphics, Types, AggPixelFormat;

{ TVamMultiLineTextBox }

constructor TVamMultiLineTextBox.Create(AOwner: TComponent);
begin
  inherited;

  fColor := '$FF000000';

  fText := TStringList.Create;

  FTextPadding := TPadding.Create(self);
  FTextPadding.OnChange := DoTextPaddingChange;

  TextBuffer := TRedFoxTextBuffer.Create;
end;

destructor TVamMultiLineTextBox.Destroy;
begin
  fText.Free;
  FreeAndNil(FTextPadding);
  TextBuffer.Free;
  inherited;
end;

procedure TVamMultiLineTextBox.DoTextPaddingChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TVamMultiLineTextBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TVamMultiLineTextBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (aWidth > 0) and (aHeight > 0) and (assigned(TextBuffer)) then
  begin
    TextBuffer.SetSize(aWidth, aHeight);
    UpdateTextBuffer;
  end;
end;

procedure TVamMultiLineTextBox.SetColor(const Value: TRedFoxColorString);
begin
  fColor := Value;
  UpdateTextBuffer;
end;

procedure TVamMultiLineTextBox.SetTargetHandle(aHandle: HDC);
begin
  TargetHandle := aHandle;
end;

procedure TVamMultiLineTextBox.SetText(const Value: TStrings);
begin
  fText.Assign(Value);
  UpdateTextBuffer;
  Invalidate;
end;

procedure TVamMultiLineTextBox.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    UpdateTextBuffer;
    Invalidate;
  end;
end;

procedure TVamMultiLineTextBox.SetTextPadding(const Value: TPadding);
begin
  FTextPadding.Assign(Value);
  UpdateTextBuffer;
  Invalidate;
end;

procedure TVamMultiLineTextBox.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> FTextVAlign then
  begin
    fTextVAlign := Value;
    UpdateTextBuffer;
    Invalidate;
  end;
end;

procedure TVamMultiLineTextBox.UpdateTextBuffer;
begin
  TextBuffer.UpdateBuffer(self.Text, self.Font, TextAlign, TextVAlign, 0);
end;

procedure TVamMultiLineTextBox.Paint;
const
  S = 'TVamMultiLineTextBox.Paint TVamMultiLineTextBox.Paint';
var
  Bounds : TRect;
begin
  inherited;

  //BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  BackBuffer.BufferInterface.ClearAll(255,255,255,0);


  TextBuffer.DrawTo(BackBuffer, self.Color);

  //self.BackBuffer.BufferInterface.FillColor := TRedFoxColor.RandomColor(255);
  //self.BackBuffer.BufferInterface.Rectangle(0,0,100,100);


  Bounds.Left := 0;
  Bounds.Top := 0;
  Bounds.Width := 200;
  Bounds.Height := 200;

  //Format := DT_LEFT;


  if TargetHandle <> 0 then
  begin
    //windows.DrawText(self.BackBuffer.Handle, PWideChar(s), Length(s), Bounds, format)
    //windows.DrawText(TargetHandle, PWideChar(s), Length(s), Bounds, format)
    //Windows.ExtTextOut(TargetHandle,20, 20, Format, nil, s, Length(s), nil)
  end;

  {
  BackBuffer.asBitmap.Canvas.Brush.Color := clMoneyGreen;
  BackBuffer.asBitmap.Canvas.Brush.Style := TBrushStyle.bsSolid;

  BackBuffer.asBitmap.Canvas.Pen.Color := clBlack;
  BackBuffer.asBitmap.Canvas.Pen.Style := TPenStyle.psSolid;

  BackBuffer.asBitmap.Canvas.Rectangle(0,0,50,50);

  BackBuffer.asBitmap.Canvas.Brush.Style := TBrushStyle.bsClear;

  BackBuffer.RedFoxInterface.InvertAlpha;
  BackBuffer.asBitmap.Canvas.TextOut(0,0, s);
  BackBuffer.RedFoxInterface.InvertAlpha;
  }

  //BackBuffer.RedFoxInterface.SetImageAlpha(255);


end;



end.
