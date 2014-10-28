unit Agg2DControl;

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
  Windows, {$ENDIF} Classes,
  Controls, Messages, Graphics, SysUtils, AggBasics, AggColor,
  AggWin32Bmp, AggPlatformSupport, AggPixelFormat, AggPixelFormatRgba,
  AggRenderScanLines, AggRendererBase, AggRenderingBuffer,
  AggRasterizerScanLineAA, AggRendererScanLine, AggScanLinePacked,
  AggControl, AggSliderControl, Agg2D, AggGsvText, AggConvStroke;

type
  TAgg2DControlBuffer = class(TInterfacedPersistent, IStreamPersist)
  private
    FPixelMap: TPixelMap;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
  end;


  TAggCustomAgg2DControl = class(TCustomControl)
  private
    FAgg2D: TAgg2D;
    FAggColor: TAggColor;
    FBuffer: TAgg2DControlBuffer;
    FPixelMap: TPixelMap;
    FBufferValid: Boolean;
    FMouseInControl: Boolean;

    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
{$IFDEF FPC}
    procedure CMColorChanged(var Message: TLMessage); message CM_COLORCHANGED;
{$ELSE}
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
{$ENDIF}
  protected
    procedure Paint; override;
    procedure Resize; override;
    property Buffer: TAgg2DControlBuffer read FBuffer;
    property Agg2D: TAgg2D read FAgg2D;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawTo(Handle: HDC; X, Y: Integer); overload;
    procedure DrawTo(HandleDC: HDC; DeviceRect: PRect = nil;
      BitmapRect: PRect = nil); overload;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;
  end;

  TAgg2DControl = class(TAggCustomAgg2DControl)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    property Buffer;
    property Agg2D;
  published
    property Align;
    property Anchors;
    property Color;

    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

    {$IFNDEF FPC}
    property OnCanResize;
    property OnMouseActivate;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnDockDrop;
    property OnDockOver;
    property OnUnDock;
    property OnEnter;
    property OnExit;
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

implementation

resourcestring
  RCStrStreamIsNotAValid = 'Stream is not a valid Bitmap';


function ColorToAggColor(WinColor: TColor): TAggColor; overload;
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

  Result.Rgba8.R := WinColor and $FF;
  Result.Rgba8.G := (WinColor and $FF00) shr 8;
  Result.Rgba8.B := (WinColor and $FF0000) shr 16;
  Result.Rgba8.A := $FF;
end;


{ TAgg2DControlBuffer }

constructor TAgg2DControlBuffer.Create;
begin
  inherited;
  FPixelMap := TPixelMap.Create;
end;

destructor TAgg2DControlBuffer.Destroy;
begin
  FPixelMap.Free;
  inherited;
end;

procedure TAgg2DControlBuffer.LoadFromFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

type
  TPixelMapAccess = class(TPixelMap);

procedure TAgg2DControlBuffer.LoadFromStream(Stream: TStream);
var
  BitmapFileHeader: TBitmapFileHeader;
  NewBitmapInfo: PBitmapInfo;
  BitmapSize: Cardinal;
begin
  with Stream, TPixelMapAccess(FPixelMap) do
  begin
    Read(BitmapFileHeader, SizeOf(TBitmapFileHeader));
    if BitmapFileHeader.BfType <> $4D42 then
      raise Exception.Create(RCStrStreamIsNotAValid);

    BitmapSize := BitmapFileHeader.BfSize - SizeOf(TBitmapFileHeader);

    AggGetMem(Pointer(NewBitmapInfo), BitmapSize);
    try
      if Read(NewBitmapInfo^, BitmapSize) <> BitmapSize then
        raise Exception.Create(RCStrStreamIsNotAValid);

      FreeBitmap;
      FBitsPerPixel := NewBitmapInfo^.bmiHeader.BiBitCount;
      CreateFromBitmap(NewBitmapInfo);
      FIsInternal := True;
    except
      if NewBitmapInfo <> nil then
        AggFreeMem(Pointer(NewBitmapInfo), BitmapSize);
    end;
  end;
end;

procedure TAgg2DControlBuffer.SaveToFile(FileName: TFileName);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TAgg2DControlBuffer.SaveToStream(Stream: TStream);
var
  BitmapFileHeader: TBitmapFileHeader;
begin
  with Stream, TPixelMapAccess(FPixelMap) do
  begin
    if BitmapInfo <> nil then
    begin
      BitmapFileHeader.BfType := $4D42;
      BitmapFileHeader.BfOffBits := CalculateHeaderSize(BitmapInfo) +
        SizeOf(TBitmapFileHeader);
      BitmapFileHeader.BfSize := BitmapFileHeader.BfOffBits + FImageSize;
      BitmapFileHeader.BfReserved1 := 0;
      BitmapFileHeader.BfReserved2 := 0;

      Write(BitmapFileHeader, SizeOf(BitmapFileHeader));
      Write(BitmapInfo^, FFullSize);
    end;
  end;
end;


{ TAggCustomAgg2DControl }

constructor TAggCustomAgg2DControl.Create(AOwner: TComponent);
begin
  inherited;

  ControlStyle := ControlStyle + [csOpaque];

  {$IFDEF FPC}
  DoubleBuffered := True;
  {$ENDIF}

  Width := 128;
  Height := 128;

  FBuffer := TAgg2DControlBuffer.Create;
  FPixelMap := FBuffer.FPixelMap;
  FPixelMap.Build(Width, Height, COrgColor32);

  FAgg2D := TAgg2D.Create;
  FAgg2D.Attach(FPixelMap.Buffer, FPixelMap.Width, FPixelMap.Height,
    -FPixelMap.Stride);

  FAggColor := ColorToAggColor(Color);
  FAgg2D.ClearAll(FAggColor.Rgba8);
end;

destructor TAggCustomAgg2DControl.Destroy;
begin
  FAgg2D.Free;
  FBuffer.Free;

  inherited;
end;

procedure TAggCustomAgg2DControl.DrawTo(Handle: HDC; X, Y: Integer);
begin
  FPixelMap.Draw(Handle, X, Y);
end;

procedure TAggCustomAgg2DControl.DrawTo(HandleDC: HDC; DeviceRect,
  BitmapRect: PRect);
begin
  FPixelMap.Draw(Handle, DeviceRect, BitmapRect);
end;

procedure TAggCustomAgg2DControl.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);

  if Dest is TAggCustomAgg2DControl then
  begin
    FPixelMap.Assign(TAggCustomAgg2DControl(Dest).FPixelMap);
    TAggCustomAgg2DControl(Dest).FBufferValid := FBufferValid;
    TAggCustomAgg2DControl(Dest).FOnMouseEnter := FOnMouseEnter;
    TAggCustomAgg2DControl(Dest).FOnMouseLeave := FOnMouseLeave;
  end;
end;

procedure TAggCustomAgg2DControl.CMColorChanged(var Message: TMessage);
begin
  FAggColor := ColorToAggColor(Color);
  FAgg2D.ClearAll(FAggColor.Rgba8);
end;

procedure TAggCustomAgg2DControl.Invalidate;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomAgg2DControl.Loaded;
begin
  FBufferValid := False;
  inherited;
end;

procedure TAggCustomAgg2DControl.Paint;
begin
  if not Assigned(Parent) then
    Exit;

  FPixelMap.Draw(Canvas.Handle);
end;

procedure TAggCustomAgg2DControl.Resize;
begin
  if Assigned(FPixelMap) then
  with FPixelMap do
    if (Self.Width <> Width) or (Self.Height <> Height) then
    begin
      SetSize(Self.Width, Self.Height);
      FAgg2D.Attach(Buffer, Width, Height, -Stride);

      FAgg2D.Viewport(0, 0, Width, Height, 0, 0, Width, Height, voXMinYMin);
      FAgg2D.ClearAll(FAggColor.Rgba8);
    end;

  inherited;
end;


{ TAgg2DControl }

procedure TAgg2DControl.Paint;
begin
  if Assigned(FOnPaint)
    then FOnPaint(Self);

  inherited;
end;

end.
