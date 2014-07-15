unit AggControlVCL;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$IFDEF MSWindows} Windows, {$ENDIF} {$ELSE}
  Windows, {$ENDIF} Classes, Controls, Messages, Graphics, AggBasics, AggColor,
  AggWin32Bmp, AggPlatformSupport, AggPixelFormat, AggPixelFormatRgba,
  AggRenderScanLines, AggRendererBase, AggRenderingBuffer,
  AggRasterizerScanLineAA, AggRendererScanLine, AggScanLinePacked,
  AggControl, AggSliderControl, AggCheckBoxControl, AggRadioBoxControl,
  AggGsvText, AggTransAffine;

type
  TAggCustomControl = class(TCustomControl)
  private
    FBufferOversize: Integer;
    FBufferValid: Boolean;
{$IFDEF FPC}
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
{$ELSE}
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
{$ENDIF}
  protected
    FRenderingBuffer: TAggRenderingBuffer;
    FPixelFormatProcessor: TAggPixelFormatProcessor;

    FRendererBase: TAggRendererBase;
    FRasterizer: TAggRasterizerScanLineAA;

    FScanLine: TAggScanLinePacked8;
    FBuffer: TPixelMap;
    function GetViewportRect: TRect; virtual;
    procedure Paint; override;
    procedure ResizeBuffer; virtual;
    procedure PaintBuffer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Invalidate; override;
    procedure Loaded; override;
    procedure Resize; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Anchors;
    property Color;
    property ParentColor;

    {$IFNDEF FPC}
    property OnMouseActivate;
    {$ENDIF}

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

  TAggSlider = class(TAggCustomControl)
  private
    FSliderControl: TAggControlSlider;
    FTextThickness: Double;
    FCaption: AnsiString;
    FValue: Double;
    FDescending: Boolean;
    FBorderWidth: Integer;
    FSteps: Cardinal;
    FTextColor: TAggPackedRgba8;
    FTriangleColor: TAggPackedRgba8;
    FPointerColor: TAggPackedRgba8;
    FPointerPreviewColor: TAggPackedRgba8;
    FBackgroundColor: TAggPackedRgba8;
    FOnChange: TNotifyEvent;
    procedure SetCaption(const Value: AnsiString);
    procedure SetDescending(const Value: Boolean);
    procedure SetTextThickness(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetNumSteps(const Value: Cardinal);
    procedure SetBackgroundColor(const Value: TAggPackedRgba8);
    procedure SetPointerColor(const Value: TAggPackedRgba8);
    procedure SetPointerPreviewColor(const Value: TAggPackedRgba8);
    procedure SetTextColor(const Value: TAggPackedRgba8);
    procedure SetTriangleColor(const Value: TAggPackedRgba8);
    procedure UpdateSliderBounds;
  protected
    procedure ResizeBuffer; override;
    procedure PaintBuffer; override;

    procedure BackgroundColorChanged; virtual;
    procedure BorderWidthChanged; virtual;
    procedure CaptionChanged; virtual;
    procedure DescendingChanged; virtual;
    procedure PointerColorChanged; virtual;
    procedure PointerPreviewColorChanged; virtual;
    procedure StepsChanged; virtual;
    procedure TextColorChanged; virtual;
    procedure TextThicknessChanged; virtual;
    procedure TriangleColorChanged; virtual;
    procedure ValueChanged; virtual;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: AnsiString read FCaption write SetCaption;
    property TextThickness: Double read FTextThickness write SetTextThickness;
    property Descending: Boolean read FDescending write SetDescending
      default False;
    property Value: Double read FValue write SetValue;
    property Steps: Cardinal read FSteps write SetNumSteps default 0;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth
      default -1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property BackgroundColor: TAggPackedRgba8 read FBackgroundColor write SetBackgroundColor;
    property TriangleColor: TAggPackedRgba8 read FTriangleColor write SetTriangleColor;
    property TextColor: TAggPackedRgba8 read FTextColor write SetTextColor;
    property PointerPreviewColor: TAggPackedRgba8 read FPointerPreviewColor write SetPointerPreviewColor;
    property PointerColor: TAggPackedRgba8 read FPointerColor write SetPointerColor;
  end;

  TAggCheckBox = class(TAggCustomControl)
  private
    FCheckBoxControl: TAggControlCheckBox;
    FCaption: AnsiString;
    FTextColor: TAggPackedRgba8;
    FActiveColor: TAggPackedRgba8;
    FInactiveColor: TAggPackedRgba8;
    procedure SetCaption(const Value: AnsiString);
    procedure SetActiveColor(const Value: TAggPackedRgba8);
    procedure SetInactiveColor(const Value: TAggPackedRgba8);
    procedure SetTextColor(const Value: TAggPackedRgba8);
  protected
    procedure ResizeBuffer; override;
    procedure PaintBuffer; override;

    procedure CaptionChanged; virtual;
    procedure ActiveColorChanged; virtual;
    procedure InactiveColorChanged; virtual;
    procedure TextColorChanged; virtual;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Caption: AnsiString read FCaption write SetCaption;
    property TextColor: TAggPackedRgba8 read FTextColor write SetTextColor;
    property InactiveColor: TAggPackedRgba8 read FInactiveColor write SetInactiveColor;
    property ActiveColor: TAggPackedRgba8 read FActiveColor write SetActiveColor;
  end;

  TAggRadioBox = class(TAggCustomControl)
  private
    FRadioBoxControl: TAggControlRadioBox;
    FItems: TStringList;
    FBorderColor: TAggPackedRgba8;
    FTextColor: TAggPackedRgba8;
    FActiveColor: TAggPackedRgba8;
    FInactiveColor: TAggPackedRgba8;
    FBackgroundColor: TAggPackedRgba8;
    procedure SetItems(const Value: TStringList);
    procedure ItemsChangeHandler(Sender: TObject);
    procedure SetActiveColor(const Value: TAggPackedRgba8);
    procedure SetBackgroundColor(const Value: TAggPackedRgba8);
    procedure SetBorderColor(const Value: TAggPackedRgba8);
    procedure SetInactiveColor(const Value: TAggPackedRgba8);
    procedure SetTextColor(const Value: TAggPackedRgba8);
  protected
    procedure ResizeBuffer; override;
    procedure PaintBuffer; override;

    procedure ActiveColorChanged; virtual;
    procedure BackgroundColorChanged; virtual;
    procedure BorderColorChanged; virtual;
    procedure InactiveColorChanged; virtual;
    procedure TextColorChanged; virtual;

    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TStringList read FItems write SetItems;

    property BackgroundColor: TAggPackedRgba8 read FBackgroundColor write SetBackgroundColor;
    property BorderColor: TAggPackedRgba8 read FBorderColor write SetBorderColor;
    property TextColor: TAggPackedRgba8 read FTextColor write SetTextColor;
    property InactiveColor: TAggPackedRgba8 read FInactiveColor write SetInactiveColor;
    property ActiveColor: TAggPackedRgba8 read FActiveColor write SetActiveColor;
  end;

  TAggLabel = class(TAggCustomControl)
  private
    FText: AnsiString;
    FTextSize: Double;
    FGsvText: TAggGsvText;
    FTransform: TAggTransAffine;
    FTextWidth: Double;
    FTextOutline: TAggGsvTextOutline;
    FColor: TAggColor;
    FTextColor: TColor;
    FTextColorAgg: TAggColor;
    function GetLineSpace: Double;
    function GetSpace: Double;
    procedure SetLineSpace(const Value: Double);
    procedure SetSpace(const Value: Double);
    procedure SetTextSize(const Value: Double);
    procedure SetText(const Value: AnsiString);
    procedure SetTextColor(const Value: TColor);
    procedure SetTextWidth(const Value: Double);
{$IFDEF FPC}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
{$ELSE}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
{$ENDIF}
  protected
    procedure PaintBuffer; override;
    procedure TextChanged; virtual;
    procedure TextSizeChanged; virtual;
    procedure TextColorChanged; virtual;
    procedure TextWidthChanged; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property TextColor: TColor read FTextColor write SetTextColor default clBtnText;
    property TextSize: Double read FTextSize write SetTextSize;
    property Text: AnsiString read FText write SetText;
    property Space: Double read GetSpace write SetSpace;
    property LineSpace: Double read GetLineSpace write SetLineSpace;
    property TextWidth: Double read FTextWidth write SetTextWidth;
  end;

implementation


function ColorToAggColor(WinColor: TColor): TAggColor; overload;
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

  Result.Rgba8.R := WinColor and $FF;
  Result.Rgba8.G := (WinColor and $FF00) shr 8;
  Result.Rgba8.B := (WinColor and $FF0000) shr 16;
  Result.Rgba8.A := $FF;
end;


{ TAggCustomControl }

constructor TAggCustomControl.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];

  {$IFDEF FPC}
  DoubleBuffered := True;
  {$ENDIF}

  FBuffer := TPixelMap.Create;
  FBuffer.Build(Width, Height, COrgColor32);
  FBufferOversize := 0;

  FRenderingBuffer := TAggRenderingBuffer.Create;
  FRenderingBuffer.Attach(FBuffer.Buffer, FBuffer.Width, FBuffer.Height,
    -FBuffer.Stride);

  PixelFormatBgra32(FPixelFormatProcessor, FRenderingBuffer);
  FRendererBase := TAggRendererBase.Create(FPixelFormatProcessor, True);

  FScanLine := TAggScanLinePacked8.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;
end;

destructor TAggCustomControl.Destroy;
begin
  FBuffer.Free;

  FRasterizer.Free;
  FScanLine.Free;

  FRendererBase.Free;
  FRenderingBuffer.Free;
  inherited;
end;

procedure TAggCustomControl.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TAggCustomControl then
  begin
    FBuffer.Assign(TAggCustomControl(Dest).FBuffer);
    TAggCustomControl(Dest).FBufferValid := FBufferValid;
    TAggCustomControl(Dest).FBufferOversize := FBufferOversize;
  end;
end;

procedure TAggCustomControl.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomControl.Loaded;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomControl.Paint;
begin
  if not Assigned(Parent) then
    Exit;

  if not FBufferValid then
    PaintBuffer;

  FBuffer.Draw(Canvas.Handle);
end;

procedure TAggCustomControl.PaintBuffer;
begin
  FBufferValid := True;
end;

procedure TAggCustomControl.Resize;
begin
  ResizeBuffer;
  FBufferValid := False;
  inherited;
end;

function TAggCustomControl.GetViewportRect: TRect;
begin
  // returns position of the buffered area within the control bounds
  // by default, the whole control is buffered
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := Width;
  Result.Bottom := Height;
end;

procedure TAggCustomControl.ResizeBuffer;
var
  NewWidth, NewHeight, W, H: Integer;
begin
  // get the viewport parameters
  with GetViewportRect do
  begin
    NewWidth := Right - Left;
    NewHeight := Bottom - Top;
  end;
  if NewWidth < 0 then NewWidth := 0;
  if NewHeight < 0 then NewHeight := 0;

  W := FBuffer.Width;

  if NewWidth > W then
    W := NewWidth + FBufferOversize
  else if NewWidth < W - FBufferOversize then
    W := NewWidth;

  if W < 1 then W := 1;

  H := FBuffer.Height;

  if NewHeight > H then
    H := NewHeight + FBufferOversize
  else if NewHeight < H - FBufferOversize then
    H := NewHeight;

  if H < 1 then H := 1;

  if (W <> FBuffer.Width) or (H <> FBuffer.Height) then
    FBuffer.Build(W, H, COrgColor32);

  FRendererBase.Free;
  FRenderingBuffer.Attach(FBuffer.Buffer, FBuffer.Width, FBuffer.Height,
    -FBuffer.Stride);

  PixelFormatBgra32(FPixelFormatProcessor, FRenderingBuffer);
  FRendererBase := TAggRendererBase.Create(FPixelFormatProcessor, True);
end;

procedure TAggCustomControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if csDesigning in ComponentState then
    ResizeBuffer;
  FBufferValid := False;
end;

{$IFDEF FPC}
procedure TAggCustomControl.WMPaint(var Message: TLMPaint);
{$ELSE}
procedure TAggCustomControl.WMPaint(var Message: TMessage);
{$ENDIF}
begin
(*
  if CustomRepaint then
  begin
    if InvalidRectsAvailable then
      // BeginPaint deeper might set invalid clipping, so we call Paint here
      // to force repaint of our invalid rects...
      Paint
    else
      // no invalid rects available? Invalidate the whole client area
      InvalidateRect(Handle, nil, False);
  end;
*)

  Paint;

  {$IFDEF FPC}
  { On FPC we need to specify the name of the ancestor here }
  inherited WMPaint(Message);
  {$ELSE}
  inherited;
  {$ENDIF}
end;


{ TAggLabel }

constructor TAggLabel.Create(AOwner: TComponent);
begin
  inherited;
  FTextColorAgg.Black;
  FColor.White;

  Color := clBtnFace;

  FTextSize := 10;
  FText := 'Hallo Welt';
  FTextWidth := 1.5;

  FGsvText := TAggGsvText.Create;
  FGsvText.SetSize(FTextSize);
  FGsvText.Flip := True;
  FGsvText.SetText(FText);

  FTransform := TAggTransAffine.Create;
  FTextOutline := TAggGsvTextOutline.Create(FGsvText, FTransform);
  FTextOutline.Width := FTextWidth;
end;

destructor TAggLabel.Destroy;
begin
  FTextOutline.Free;
  FTransform.Free;
  FGsvText.Free;
  inherited;
end;

function TAggLabel.GetLineSpace: Double;
begin
  Result := FGsvText.LineSpace;
end;

function TAggLabel.GetSpace: Double;
begin
  Result := FGsvText.Space;
end;

procedure TAggLabel.PaintBuffer;
var
  IntTextWidth: Integer;
  RenScan: TAggRendererScanLineAASolid;
begin
  inherited;

  FRendererBase.Clear(@FColor);

  IntTextWidth := Trunc(FTextWidth - 0.5) + 1;
  FGsvText.SetStartPoint(IntTextWidth, TextSize + IntTextWidth);

  FRasterizer.AddPath(FTextOutline);

  RenScan := TAggRendererScanLineAASolid.Create(FRendererBase);
  try
    RenScan.SetColor(@FTextColorAgg);
    RenderScanLines(FRasterizer, FScanLine, RenScan);
  finally
    RenScan.Free;
  end;
end;

procedure TAggLabel.SetLineSpace(const Value: Double);
begin
  FGsvText.LineSpace := Value;
  Invalidate;
end;

procedure TAggLabel.SetSpace(const Value: Double);
begin
  FGsvText.Space := Value;
  Invalidate;
end;

procedure TAggLabel.SetText(const Value: AnsiString);
begin
  if FText <> Value then
  begin
    FText := Value;
    TextChanged;
  end;
end;

procedure TAggLabel.SetTextColor(const Value: TColor);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    TextColorChanged;
  end;
end;

procedure TAggLabel.SetTextSize(const Value: Double);
begin
  if FTextSize <> Value then
  begin
    FTextSize := Value;
    TextSizeChanged;
  end;
end;

procedure TAggLabel.SetTextWidth(const Value: Double);
begin
  if FTextWidth <> Value then
  begin
    FTextWidth := Value;
    TextWidthChanged;
  end;
end;

procedure TAggLabel.TextChanged;
begin
  FGsvText.SetText(FText);
  Invalidate;
end;

procedure TAggLabel.TextColorChanged;
begin
  FTextColorAgg := ColorToAggColor(FTextColor);
  Invalidate;
end;

procedure TAggLabel.TextSizeChanged;
begin
  FGsvText.SetSize(FTextSize);
  Invalidate;
end;

procedure TAggLabel.TextWidthChanged;
begin
  FTextOutline.Width := FTextWidth;
  Invalidate;
end;

procedure TAggLabel.CMColorChanged(var Message: TMessage);
begin
  FColor := ColorToAggColor(Color);
  Invalidate;
end;


{ TAggCheckBox }

constructor TAggCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  FCheckBoxControl := TAggControlCheckBox.Create(0, 0, 'Test', True);

  FTextColor := FCheckBoxControl.TextColor.Rgba8.ABGR;
  FActiveColor := FCheckBoxControl.ActiveColor.Rgba8.ABGR;
  FInactiveColor := FCheckBoxControl.InactiveColor.Rgba8.ABGR;
end;

destructor TAggCheckBox.Destroy;
begin
  FCheckBoxControl.Free;
  inherited;
end;

procedure TAggCheckBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCheckBoxControl.OnMouseButtonDown(X, Y) then
    Invalidate;

  inherited;
end;

procedure TAggCheckBox.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if FCheckBoxControl.OnMouseMove(X, Y, ssLeft in Shift) then
    Invalidate;

  inherited;
end;

procedure TAggCheckBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FCheckBoxControl.OnMouseButtonUp(X, Y) then
    Invalidate;

  inherited;
end;

procedure TAggCheckBox.PaintBuffer;
var
  RenScan: TAggRendererScanLineAASolid;
  AggColor: TAggColor;
begin
  AggColor := ColorToAggColor(Color);
  FRendererBase.Clear(@AggColor);

  RenScan := TAggRendererScanLineAASolid.Create(FRendererBase);
  try
    RenderControl(FRasterizer, FScanLine, RenScan, FCheckBoxControl);
  finally
    RenScan.Free;
  end;

  inherited;
end;

procedure TAggCheckBox.CaptionChanged;
begin
  Invalidate;
end;

procedure TAggCheckBox.ResizeBuffer;
begin
  inherited;
  FCheckBoxControl.SetClipBox(RectDouble(0, 0, Width, Height));
end;

procedure TAggCheckBox.ActiveColorChanged;
begin

end;

procedure TAggCheckBox.InactiveColorChanged;
begin

end;

procedure TAggCheckBox.TextColorChanged;
begin

end;

procedure TAggCheckBox.SetCaption(const Value: AnsiString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    CaptionChanged;
  end;
end;

procedure TAggCheckBox.SetActiveColor(const Value: TAggPackedRgba8);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    ActiveColorChanged;
  end;
end;

procedure TAggCheckBox.SetInactiveColor(const Value: TAggPackedRgba8);
begin
  if FInactiveColor <> Value then
  begin
    FInactiveColor := Value;
    InactiveColorChanged;
  end;
end;

procedure TAggCheckBox.SetTextColor(const Value: TAggPackedRgba8);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    TextColorChanged;
  end;
end;


{ TAggRadioBox }

constructor TAggRadioBox.Create(AOwner: TComponent);
begin
  inherited;
  FRadioBoxControl := TAggControlRadioBox.Create(0, 0, Width, Height, True);
  FItems := TStringList.Create;
  FItems.OnChange := ItemsChangeHandler;
  FItems.Add('Item 1');
  FItems.Add('Item 2');

  FBorderColor := FRadioBoxControl.BorderColor.Rgba8.ABGR;
  FTextColor := FRadioBoxControl.TextColor.Rgba8.ABGR;
  FActiveColor := FRadioBoxControl.ActiveColor.Rgba8.ABGR;
  FInactiveColor := FRadioBoxControl.InactiveColor.Rgba8.ABGR;
  FBackgroundColor := FRadioBoxControl.BackgroundColor.Rgba8.ABGR;
end;

destructor TAggRadioBox.Destroy;
begin
  FItems.Free;
  FRadioBoxControl.Free;
  inherited;
end;

procedure TAggRadioBox.ItemsChangeHandler(Sender: TObject);
var
  Index: Integer;
begin
  FRadioBoxControl.Clear;
  for Index := 0 to FItems.Count - 1 do
    FRadioBoxControl.AddItem(FItems.Strings[Index]);
end;

procedure TAggRadioBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FRadioBoxControl.OnMouseButtonDown(X, Y) then
    Invalidate;

  inherited;
end;

procedure TAggRadioBox.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  if FRadioBoxControl.OnMouseMove(X, Y, ssLeft in Shift) then
    Invalidate;

  inherited;
end;

procedure TAggRadioBox.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FRadioBoxControl.OnMouseButtonUp(X, Y) then
    Invalidate;

  inherited;
end;

procedure TAggRadioBox.ActiveColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FActiveColor));
  FRadioBoxControl.ActiveColor := AggColor;
  Invalidate;
end;

procedure TAggRadioBox.BackgroundColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FBackgroundColor));
  FRadioBoxControl.BackgroundColor := AggColor;
  Invalidate;
end;

procedure TAggRadioBox.BorderColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FBorderColor));
  FRadioBoxControl.BorderColor := AggColor;
  Invalidate;
end;

procedure TAggRadioBox.InactiveColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FInactiveColor));
  FRadioBoxControl.InactiveColor := AggColor;
  Invalidate;
end;

procedure TAggRadioBox.TextColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FInactiveColor));
  FRadioBoxControl.InactiveColor := AggColor;
  Invalidate;
end;

procedure TAggRadioBox.PaintBuffer;
var
  RenScan: TAggRendererScanLineAASolid;
  AggColor: TAggColor;
begin
  AggColor := ColorToAggColor(Color);
  FRendererBase.Clear(@AggColor);

  RenScan := TAggRendererScanLineAASolid.Create(FRendererBase);
  try
    RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxControl);
  finally
    RenScan.Free;
  end;

  inherited;
end;

procedure TAggRadioBox.ResizeBuffer;
begin
  inherited;
  FRadioBoxControl.SetClipBox(0, 0, Width, Height);
end;

procedure TAggRadioBox.SetActiveColor(const Value: TAggPackedRgba8);
begin
  if FActiveColor <> Value then
  begin
    FActiveColor := Value;
    ActiveColorChanged;
  end;
end;

procedure TAggRadioBox.SetBackgroundColor(const Value: TAggPackedRgba8);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    BackgroundColorChanged;
  end;
end;

procedure TAggRadioBox.SetBorderColor(const Value: TAggPackedRgba8);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    BorderColorChanged;
  end;
end;

procedure TAggRadioBox.SetInactiveColor(const Value: TAggPackedRgba8);
begin
  if FInactiveColor <> Value then
  begin
    FInactiveColor := Value;
    InactiveColorChanged;
  end;
end;

procedure TAggRadioBox.SetTextColor(const Value: TAggPackedRgba8);
begin
  if FTextColor  <> Value then
  begin
    FTextColor := Value;
    TextColorChanged;
  end;
end;

procedure TAggRadioBox.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);
end;


{ TAggSlider }

constructor TAggSlider.Create(AOwner: TComponent);
begin
  inherited;
  FBorderWidth := -1;
  FCaption := '';
  FValue := 0.5;
  FTextThickness := 1.5;

  FSliderControl := TAggControlSlider.Create(FBorderWidth, FBorderWidth,
    Width - FBorderWidth, Height - FBorderWidth, True);
  FSliderControl.Value := FValue;

  FBackgroundColor := FSliderControl.BackgroundColor.Rgba8.ABGR;
  FTextColor := FSliderControl.TextColor.Rgba8.ABGR;
  FTriangleColor := FSliderControl.TriangleColor.Rgba8.ABGR;
  FPointerColor := FSliderControl.PointerColor.Rgba8.ABGR;
  FPointerPreviewColor := FSliderControl.PointerPreviewColor.Rgba8.ABGR;
end;

destructor TAggSlider.Destroy;
begin
  FSliderControl.Free;
  inherited;
end;

procedure TAggSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  OldValue: Double;
begin
  OldValue := FSliderControl.Value;
  if Key in [37..40] then
  begin
    FSliderControl.OnArrowKeys(Key = 37, Key = 39, Key = 40, Key = 38);
    if (OldValue <> FSliderControl.Value) and Assigned(FOnChange) then
      FOnChange(Self);
  end;

  inherited;
end;

procedure TAggSlider.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FSliderControl.OnMouseButtonDown(X, Y) then
    Invalidate;
  inherited;
end;

procedure TAggSlider.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  OldValue: Double;
begin
  OldValue := FSliderControl.Value;
  if FSliderControl.OnMouseMove(X, Y, ssLeft in Shift) then
  begin
    if (OldValue <> FSliderControl.Value) and Assigned(FOnChange) then
      FOnChange(Self);

    Invalidate;
  end;

  inherited;
end;

procedure TAggSlider.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FSliderControl.OnMouseButtonUp(X, Y) then
    Invalidate;
  inherited;
end;

procedure TAggSlider.PaintBuffer;
var
  RenScan: TAggRendererScanLineAASolid;
begin
  RenScan := TAggRendererScanLineAASolid.Create(FRendererBase);
  try
    FSliderControl.NoTransform;
    RenderControl(FRasterizer, FScanLine, RenScan, FSliderControl);
  finally
    RenScan.Free;
  end;
  inherited;
end;

procedure TAggSlider.ResizeBuffer;
begin
  inherited;
  UpdateSliderBounds;
end;

procedure TAggSlider.UpdateSliderBounds;
var
  BW: Double;
begin
  if Assigned(FSliderControl) then
  begin
    if FBorderWidth > 0 then
      BW := FBorderWidth
    else
      BW := FBuffer.Height div 4;

    if BW > 0.5 * FBuffer.Height then
      BW :=  0.5 * FBuffer.Height;
    if BW > 0.5 * FBuffer.Width then
      BW :=  0.5 * FBuffer.Width;

    FSliderControl.SetClipBox(RectDouble(BW, BW, FBuffer.Width - BW,
      FBuffer.Height - BW));

    FSliderControl.SetBorderWidth(0, BW);
  end;
end;

procedure TAggSlider.BorderWidthChanged;
begin
  UpdateSliderBounds;
  Invalidate;
end;

procedure TAggSlider.CaptionChanged;
begin
  FSliderControl.Caption := FCaption;
  Invalidate;
end;

procedure TAggSlider.DescendingChanged;
begin
  FSliderControl.Descending := FDescending;
  Invalidate;
end;

procedure TAggSlider.StepsChanged;
begin
  FSliderControl.NumSteps := FSteps;
  Invalidate;
end;

procedure TAggSlider.TextThicknessChanged;
begin
  FSliderControl.TextThickness := FTextThickness;
  Invalidate;
end;

procedure TAggSlider.ValueChanged;
begin
  FSliderControl.Value := FValue;
  Invalidate;
end;

procedure TAggSlider.BackgroundColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FBackgroundColor));
  FSliderControl.BackgroundColor := AggColor;
  Invalidate;
end;

procedure TAggSlider.PointerColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FPointerColor));
  FSliderControl.PointerColor := AggColor;
  Invalidate;
end;

procedure TAggSlider.PointerPreviewColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FPointerPreviewColor));
  FSliderControl.PointerPreviewColor := AggColor;
  Invalidate;
end;

procedure TAggSlider.TextColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FTextColor));
  FSliderControl.TextColor := AggColor;
  Invalidate;
end;

procedure TAggSlider.TriangleColorChanged;
var
  AggColor: TAggColor;
begin
  AggColor.FromRgba8(TAggRgba8(FTriangleColor));
  FSliderControl.TriangleColor := AggColor;
  Invalidate;
end;

procedure TAggSlider.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    BorderWidthChanged;
  end;
end;

procedure TAggSlider.SetCaption(const Value: AnsiString);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    CaptionChanged;
  end;
end;

procedure TAggSlider.SetDescending(const Value: Boolean);
begin
  if FDescending <> Value then
  begin
    FDescending := Value;
    DescendingChanged;
  end;
end;

procedure TAggSlider.SetNumSteps(const Value: Cardinal);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    StepsChanged;
  end;
end;

procedure TAggSlider.SetBackgroundColor(const Value: TAggPackedRgba8);
begin
  if FBackgroundColor <> Value then
  begin
    FBackgroundColor := Value;
    BackgroundColorChanged;
  end;
end;

procedure TAggSlider.SetPointerColor(const Value: TAggPackedRgba8);
begin
  if FPointerColor <> Value then
  begin
    FPointerColor := Value;
    PointerColorChanged;
  end;
end;

procedure TAggSlider.SetPointerPreviewColor(const Value: TAggPackedRgba8);
begin
  if FPointerPreviewColor <> Value then
  begin
    FPointerPreviewColor := Value;
    PointerPreviewColorChanged;
  end;
end;

procedure TAggSlider.SetTextColor(const Value: TAggPackedRgba8);
begin
  if FTextColor <> Value then
  begin
    FTextColor := Value;
    TextColorChanged;
  end;
end;

procedure TAggSlider.SetTriangleColor(const Value: TAggPackedRgba8);
begin
  if FTriangleColor <> Value then
  begin
    FTriangleColor := Value;
    TriangleColorChanged;
  end;
end;

procedure TAggSlider.SetTextThickness(const Value: Double);
begin
  if FTextThickness <> Value then
  begin
    FTextThickness := Value;
    TextThicknessChanged;
  end;
end;

procedure TAggSlider.SetValue(const Value: Double);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    ValueChanged;
  end;
end;

end.
