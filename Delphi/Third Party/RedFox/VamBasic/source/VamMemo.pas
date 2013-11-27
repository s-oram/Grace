unit VamMemo;

interface

uses
  Graphics, RedFoxTextBuffer,
  Classes, RedFox, RedFoxColor, RedFoxWinControl, VamWinControl;

type
  TVamMemo = class(TVamWinControl)
  private
    fColor: TRedFoxColorString;
    fTextVAlign: TRedFoxAlign;
    fTextAlign: TRedFoxAlign;
    fColorMouseOver: TRedFoxColorString;
    fText: TStrings;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure SetColorMouseOver(const Value: TRedFoxColorString);
    procedure SetTextAlign(const Value: TRedFoxAlign);
    procedure SetTextVAlign(const Value: TRedFoxAlign);
    procedure SetText(const Value: TStrings);
  protected
    TextBuffer : TRedFoxTextBuffer;
    RedrawText : boolean;

    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;

    procedure EventHandle_TextChanged(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property Color          : TRedFoxColorString read fColor          write SetColor;
    property ColorMouseOver : TRedFoxColorString read fColorMouseOver write SetColorMouseOver;

    property TextAlign  : TRedFoxAlign read fTextAlign  write SetTextAlign;
    property TextVAlign : TRedFoxAlign read fTextVAlign write SetTextVAlign;

    property Text : TStrings read fText write SetText;
    property Font;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  Windows,
  Types, AggPixelFormat;

{ TVamMemo }

constructor TVamMemo.Create(AOwner: TComponent);
begin
  inherited;

  fColor          := '$FF000000';
  fColorMouseOver := '$FF000000';

  Font.Color := clWhite;

  fText := TStringList.Create;
  TStringList(fText).OnChange := EventHandle_TextChanged;

  TextBuffer := TRedFoxTextBuffer.Create;

end;

destructor TVamMemo.Destroy;
begin
  TextBuffer.Free;
  inherited;
end;

procedure TVamMemo.EventHandle_TextChanged(Sender: TObject);
begin
  RedrawText := true;
  Invalidate;
end;

procedure TVamMemo.SetColor(const Value: TRedFoxColorString);
begin
  if fColor <> Value then
  begin
    fColor := Value;
    Invalidate;
  end;
end;

procedure TVamMemo.SetColorMouseOver(const Value: TRedFoxColorString);
begin
  if fColorMouseOver <> Value then
  begin
    fColorMouseOver := Value;
    Invalidate;
  end;
end;

procedure TVamMemo.SetText(const Value: TStrings);
begin
  fText.Assign(Value);
  //Invalidate;
end;

procedure TVamMemo.SetTextAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextAlign then
  begin
    fTextAlign := Value;
    RedrawText := true;
    Invalidate;
  end;
end;

procedure TVamMemo.SetTextVAlign(const Value: TRedFoxAlign);
begin
  if Value <> fTextVAlign then
  begin
    fTextVAlign := Value;
    RedrawText := true;
    Invalidate;
  end;
end;

procedure TVamMemo.MouseEnter;
begin
  inherited;

end;

procedure TVamMemo.MouseLeave;
begin
  inherited;

end;

procedure TVamMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    TextBuffer.SetSize(aWidth, aHeight);
    RedrawText := true;
    Invalidate;
  end;
end;

procedure TVamMemo.Paint;
var
  TextRect : TRect;
begin
  BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;

  if RedrawText then
  begin
    TextBuffer.UpdateBuffer(fText, Font, TextAlign, TextVAlign, DT_WordBreak);
    RedrawText := false;
  end;

  TextBuffer.DrawTo(Backbuffer, GetRedFoxColor(Color));
end;



end.
