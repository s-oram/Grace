unit LucidityGui.Scope.FreqAnalyzer;

interface

uses
  Types,
  RedFoxColor,
  RedFoxImageBuffer,
  VamLib.MoreTypes;

type
  IFreqAnalyzer = interface
    ['{DD43662C-FBFE-4B6D-8797-78482805DABE}']
    procedure GetAnalysisData(out MagnitudeData:PSingle; out SampleFrames : integer);
  end;

  TFreqDisplay = class
  private
    BackBuffer: TRedFoxImageBuffer;
    ResetRequired : boolean;
    ReadIndex  : integer;
    WriteIndex : integer;
    BufferSize : integer;
    DrawXIndex : integer;
    fLineColor: TRedFoxColor;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(w,h:integer);

    procedure ProcessSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IFreqAnalyzer);
    procedure DrawTo(Dest : TRedFoxImageBuffer; const DestRect:TRect);

    property LineColor : TRedFoxColor read fLineColor write fLineColor;
  end;

implementation

uses
  RedFox2D;

{ TFreqDisplay }

constructor TFreqDisplay.Create;
begin
  BackBuffer := TRedFoxImageBuffer.Create;
end;

destructor TFreqDisplay.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TFreqDisplay.SetSize(w, h: integer);
begin
  BackBuffer.SetSize(w, h);
end;

procedure TFreqDisplay.ProcessSignal(Dest: TRedFoxImageBuffer; const DestRect: TRect; const Source: IFreqAnalyzer);
var
  x1, y1, x2, y2, destX, destY : integer;
  MData : PSingle;
  MFrames : integer;
  c1: Integer;

  dx1, dx2, dy1, dy2 : single;
begin
  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  BackBuffer.BufferInterface.LineColor := LineColor;
  //BackBuffer.BufferInterface.Line(0,0, BackBuffer.Width, BackBuffer.Height);


  Source.GetAnalysisData(MData, MFrames);

  dx1 := 0;
  dy1 := DestRect.Height;

  for c1 := 0 to MFrames-1 do
  begin
    dx2 := (c1 / (MFrames-1)) * DestRect.Width;
    dy2 := (1 - MData^) * DestRect.Height;

    BackBuffer.BufferInterface.Line(dx1, dy1, dx2, dy2);

    dx1 := dx2;
    dy1 := dy2;

    inc(MData);
  end;

  x1 := 0;
  y1 := 0;
  x2 := BackBuffer.Width;
  y2 := BackBuffer.Height;
  DestX := DestRect.Left;
  DestY := DestRect.Top;

  RedFox_AlphaBlit(Dest.RedFoxInterface, BackBuffer.RedFoxInterface, x1, y1, x2, y2, destX, destY, 255);
end;

procedure TFreqDisplay.DrawTo(Dest: TRedFoxImageBuffer; const DestRect: TRect);
begin

end;



end.
