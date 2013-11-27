unit VamSampleDisplay;

interface

uses
  Graphics,  System.Types, System.Classes,
  RedFox, RedFoxColor,
  VamSampleDisplayBackBuffer, VamWinControl;

type
  TSampleDisplayInfo = VamSampleDisplayBackBuffer.TSampleDisplayInfo;
  TSampleImageBuffer = VamSampleDisplayBackBuffer.TSampleImageBuffer;
  ISampleImageBuffer = VamSampleDisplayBackBuffer.ISampleImageBuffer;

  TVamSampleDisplay = class(TVamWinControl)
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
    BackBufferController : TSampleImageBuffer;
    StoredSampleImage    : ISampleImageBuffer;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure DrawSample(aSampleImage:ISampleImageBuffer); overload;
    procedure DrawSample(SDI:TSampleDisplayInfo); overload;
    procedure ClearSample(InvalidateDisplay : boolean = true);

    function SamplePosToPixelPos(x1:integer; SampleFrames:integer):single;
    function PixelPosToSamplePos(x1:single; SampleFrames:integer):integer;
  published
    property Zoom   : double read fZoom   write SetZoom;
    property Offset : double read fOffset write SetOffset;

    property LineColor : TRedFoxColorString read GetLineColor          write SetLineColor;

    property OnDisplayChange : TNotifyEvent read fOnDisplayChange write fOnDisplayChange;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

{ TFxSampleDisplay }

constructor TVamSampleDisplay.Create(AOwner: TComponent);
begin
  inherited;
  BackBufferController := TSampleImageBuffer.Create;

  IsBufferInvalid := true;

  LineColor := '$FF777776';
end;

destructor TVamSampleDisplay.Destroy;
begin
  BackBufferController.Free;
  StoredSampleImage := nil;
  inherited;
end;

function TVamSampleDisplay.PixelPosToSamplePos(x1: single; SampleFrames:integer): integer;
begin
  result := VamSampleDisplayBackBuffer.PixelPosToSamplePos(x1, SampleFrames, Width, Zoom, Offset);
end;

function TVamSampleDisplay.SamplePosToPixelPos(x1: integer; SampleFrames:integer): single;
begin
  result := VamSampleDisplayBackBuffer.SamplePosToPixelPos(x1, SampleFrames, Width, Zoom, Offset)
end;

procedure TVamSampleDisplay.DrawSample(aSampleImage:ISampleImageBuffer);
begin
  StoredSampleImage := aSampleImage;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;


procedure TVamSampleDisplay.DrawSample(SDI: TSampleDisplayInfo);
begin
  StoredSampleImage := nil;
  BackBufferController.DrawSample(SDI);
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

function TVamSampleDisplay.GetLineColor: TRedFoxColorString;
begin
  result := BackBufferController.LineColor;
end;

procedure TVamSampleDisplay.ClearSample(InvalidateDisplay : boolean = true);
begin
  StoredSampleImage := nil;
  BackBufferController.ClearSample(InvalidateDisplay);
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TVamSampleDisplay.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

procedure TVamSampleDisplay.SetLineColor(const Value: TRedFoxColorString);
begin
  BackBufferController.LineColor := Value;
end;

procedure TVamSampleDisplay.SetOffset(const Value: double);
begin
  fOffset := Value;
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TVamSampleDisplay.SetZoom(const Value: double);
begin
  fZoom := Value;
  IsBufferInvalid := true;
  Invalidate;
  if assigned(OnDisplayChange) then OnDisplayChange(self);
end;

procedure TVamSampleDisplay.Paint;
begin
  if assigned(StoredSampleImage) then
  begin
    BackBuffer.BufferInterface.CopyImage(StoredSampleImage.GetObject.BackBuffer.AsImage, 0, 0);
  end else
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
end;

end.
