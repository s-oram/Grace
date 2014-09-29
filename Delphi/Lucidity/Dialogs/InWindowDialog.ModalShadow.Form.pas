unit InWindowDialog.ModalShadow.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TModalShadow = class(TForm)
    procedure FormClick(Sender: TObject);
  private
    { Private declarations }
    FBmp: TBitmap;
    fOnShadowClicked: TNotifyEvent;
  protected
    procedure Paint; override;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
    procedure WMDisplayChange(var Message: TMessage); message WM_DISPLAYCHANGE;
    procedure WMMove(var Message: TMessage) ; message WM_MOVE;
  protected
    procedure DoClose(var Action: TCloseAction); override;
  public
  public
    { Public declarations }
    constructor CreateShadow(AForm: TForm);
    destructor Destroy; override;
    procedure ShowShadow;
    property OnShadowClicked : TNotifyEvent read fOnShadowClicked write fOnShadowClicked;
  end;

implementation

uses
  VamLib.LoggingProxy;

{$R *.dfm}

procedure AdjustAlpha(var source : TBitmap);
type
  T32BitPixel = record
    B, G, R, A : byte;
  end;
  P32BitPixel = ^T32BitPixel;
var
  c1, c2: Integer;
  pp : P32BitPixel;
begin
  for c1 := 0 to source.Height-1 do
  begin
    pp := Source.ScanLine[c1];

    for c2 := 0 to source.Width-1 do
    begin
      pp^.A := 255;
      pp^.R := round(pp^.R * 0.5);
      pp^.G := round(pp^.G * 0.5);
      pp^.B := round(pp^.B * 0.5);
      inc(pp);
    end;
  end;
end;


constructor TModalShadow.CreateShadow(AForm: TForm);
begin
  inherited Create(AForm);

  self.DoubleBuffered := true;

  Parent := AForm;
  PopupParent := AForm;
  FBmp := TBitmap.Create;
  FBmp.PixelFormat := pf32bit;
  FBmp.AlphaFormat := afDefined;

  AlphaBlend := true;
  AlphaBlendValue := 128;
  TransparentColor := true;
  TransparentColorValue := clFuchsia;
  BorderStyle := bsNone;
  Color := clBlack;
  OldCreateOrder := False;
  PopupMode := pmExplicit;
  Position := poDesigned;



  FBmp.Width  := AForm.Width;
  FBmp.Height := AForm.Height;

  PopupParent.PaintTo(fBmp.Canvas, 0, 0);

  AdjustAlpha(fBmp);
end;

destructor TModalShadow.Destroy;
begin
  Log.LogMessage('TModalShadow.Destroy');
  FBmp.Free;
  inherited;
end;

procedure TModalShadow.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action := TCloseAction.caFree;
end;

procedure TModalShadow.Paint;
begin
  Canvas.Draw(0, 0, FBmp);
end;

procedure TModalShadow.ShowShadow;
var
  Pt: TPoint;
  R: TRect;
begin
  if not assigned(PopupParent) then exit;

  Pt := PopupParent.ClientOrigin;
  R := PopupParent.ClientRect;

  FBmp.Width  := r.Width;
  FBmp.Height := r.Height;

  PopupParent.PaintTo(fBmp.Canvas, 0, 0);

  AdjustAlpha(fBmp);

  SetBounds(0, 0, R.Width, R.Height);
  if Showing then
    Invalidate
  else
    ShowWindow(Handle, SW_SHOWNOACTIVATE);

end;

procedure TModalShadow.WMDisplayChange(var Message: TMessage);
begin
  inherited;
  ShowShadow;
end;

procedure TModalShadow.WMMouseActivate(var Message: TWMMouseActivate);
begin
  Message.Result := MA_NOACTIVATE;
end;

procedure TModalShadow.WMMove(var Message: TMessage);
begin
  ShowShadow;
end;

procedure TModalShadow.FormClick(Sender: TObject);
begin
  if assigned(OnShadowClicked) then OnShadowClicked(self);
end;



end.
