unit RedFox.SampleDisplay;

interface

uses
  Graphics,  System.Types, System.Classes, RedFox, RedFox.SampleDisplay.BackBuffer, RedFoxCustomControl;

type
  TSampleDisplayInfo = RedFox.SampleDisplay.BackBuffer.TSampleDisplayInfo;

  TRedFoxSampleDisplay = class(TRedFoxCustomControl)
  private
    fOffset: double;
    fZoom: double;
    fOnDisplayChange: TNotifyEvent;
    procedure SetZoom(const Value: double);
    procedure SetOffset(const Value: double);
    function GetLineColor: TRedFoxColorString;
    procedure SetLineColor(const Value: TRedFoxColorString);
  protected
    IsBufferInvalid      : boolean;
    BackBufferController : TBackBufferController;
    procedure DoPaintBuffer; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawSample(SDI:TSampleDisplayInfo);
    procedure ClearSample(InvalidateDisplay : boolean = true);

    function SamplePosToPixelPos(x1:integer):single;
    function PixelPosToSamplePos(x1:single):integer;
  published
    property Zoom   : double read fZoom   write SetZoom;
    property Offset : double read fOffset write SetOffset;

    property LineColor : TRedFoxColorString read GetLineColor          write SetLineColor;

    property OnDisplayChange : TNotifyEvent read fOnDisplayChange write fOnDisplayChange;
  end;

implementation

{ TFxSampleDisplay }

constructor TRedFoxSampleDisplay.Create(AOwner: TComponent);
begin
  inherited;
  BackBufferController := TBackBufferController.Create;

  IsBufferInvalid := true;
end;

destructor TRedFoxSampleDisplay.Destroy;
begin
  BackBufferController.Free;
  inherited;
end;

function TRedFoxSampleDisplay.PixelPosToSamplePos(x1: single): integer;
begin
  result := BackBufferController.PixelPosToSamplePos(x1);
end;

function TRedFoxSampleDisplay.SamplePosToPixelPos(x1: integer): single;
begin
  result := BackBufferController.SamplePosToPixelPos(x1);
end;

procedure TRedFoxSampleDisplay.DrawSample(SDI: TSampleDisplayInfo);
begin
  BackBufferController.DrawSample(SDI);
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

function TRedFoxSampleDisplay.GetLineColor: TRedFoxColorString;
begin
  result := BackBufferController.LineColor;
end;

procedure TRedFoxSampleDisplay.ClearSample(InvalidateDisplay : boolean = true);
begin
  BackBufferController.ClearSample(InvalidateDisplay);
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TRedFoxSampleDisplay.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    BackBufferController.Resize(AWidth, AHeight);
    IsBufferInvalid := true;
    Invalidate;
    if assigned(OnDisplayChange) then OnDisplayChange(self);
  end;

end;

procedure TRedFoxSampleDisplay.SetLineColor(const Value: TRedFoxColorString);
begin
  BackBufferController.LineColor := Value;
end;

procedure TRedFoxSampleDisplay.SetOffset(const Value: double);
begin
  fOffset := Value;
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TRedFoxSampleDisplay.SetZoom(const Value: double);
begin
  fZoom := Value;
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TRedFoxSampleDisplay.DoPaintBuffer;
var
  x : integer;
  r : TRectF;
begin
  //=== Clear the back Buffer ====
  BackBuffer.RedFoxInterface.ClearAll(255,255,255,0);

  if IsBufferInvalid then
  begin
    IsBufferInvalid := false;
    BackBufferController.Offset := Offset;
    BackBufferController.Zoom := Zoom;
    BackBufferController.DrawSample;
  end;

  BackBuffer.BufferInterface.CopyImage(BackBufferController.BackBuffer.AsImage,0,0);
end;

end.
