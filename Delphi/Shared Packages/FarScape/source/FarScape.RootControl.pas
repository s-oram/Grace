unit FarScape.RootControl;

interface

uses
  Windows,
  Graphics,
  Types,
  FarScape.Assistant.Bitmap,
  FarScape.CustomControl,
  FarScape.Scene,
  FarScape.UserInteraction;

type
  TFarScapeRootControl = class(TFarScapeAbstractRoot)
  private
    fBackBuffer: TBitmap;
    fScene : TScene;
    fUserInteraction: TUserInteraction;
  protected
    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); override;

    function GetSceneInterface : IFarScapeScene; override;
    function GetUserInteractionInterface : IFarScapeUserInteraction; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property BackBuffer : TBitmap read fBackBuffer;

    procedure PaintRegion(const ClipBox : TRect);

    property Scene : TScene read fScene;
    property UserInteraction : TUserInteraction read fUserInteraction;
  end;

implementation

uses
  SysUtils,
  VamLib.WinUtils;

{ TFarScapeRootControl }

constructor TFarScapeRootControl.Create;
begin
  inherited;

  fBackBuffer := TBitmap.Create;

  fScene := TScene.Create(self);
  fUserInteraction := TUserInteraction.Create(fScene);
end;

destructor TFarScapeRootControl.Destroy;
begin
  inherited;
  fBackBuffer.Free;
  fScene.Free;
  fUserInteraction.Free;
end;

function TFarScapeRootControl.GetSceneInterface: IFarScapeScene;
begin
  result := fScene;
end;

function TFarScapeRootControl.GetUserInteractionInterface: IFarScapeUserInteraction;
begin
  result := fUserInteraction;
end;

procedure TFarScapeRootControl.ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited;
  fBackBuffer.PixelFormat := pf32Bit;
  fBackBuffer.SetSize(aWidth, aHeight);
  //Important: Set afPreMultiplied after setting pixel data.
  fBackBuffer.AlphaFormat := afPreMultiplied;
end;

procedure TFarScapeRootControl.PaintRegion(const ClipBox: TRect);
var
  c1: Integer;
  FSC : TFarScapeControl;
  c : TColor;
begin
  SendDebugMesssage('Repaint: ' + IntToStr(ClipBox.Left) + ', '  + IntToStr(ClipBox.Top) + ', '  + IntToStr(ClipBox.Right) + ', '  + IntToStr(ClipBox.Bottom) + ', ');
  //c := RGB(Random(255),Random(255),Random(255));

  c := RGB(55,55,55);
  BackBuffer.Canvas.Brush.Color := c;
  BackBuffer.Canvas.Brush.Style := TBrushStyle.bsSolid;
  BackBuffer.Canvas.Pen.Style := TPenStyle.psClear;
  BackBuffer.Canvas.Rectangle(ClipBox.Left, ClipBox.Top, ClipBox.Right, ClipBox.Bottom);

  //

  //c := RGB(Random(255),Random(255),Random(255));
  //BitmapEx.ClearAll(BackBuffer, c,255);

  fScene.RebuildScene;



  for c1 := 0 to fScene.ElementCount-1 do
  begin
    FSC := fScene.Element[c1].Control;
    FSC.PaintToDc(BackBuffer.Canvas.Handle);
  end;

  //BackBuffer.Canvas.Brush.Style := TBrushStyle.bsClear;
  //BackBuffer.Canvas.Pen.Style := TPenStyle.psSolid;
  //BackBuffer.Canvas.Pen.Color := RGB(255,0,255);
  //BackBuffer.Canvas.Rectangle(ClipBox.Left, ClipBox.Top, ClipBox.Right, ClipBox.Bottom);

end;



end.
