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
    fLineColor: TRedFoxColor;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetSize(w,h:integer);

    procedure ProcessSignal(Dest : TRedFoxImageBuffer; const DestRect:TRect; const Source: IFreqAnalyzer);

    property LineColor : TRedFoxColor read fLineColor write fLineColor;
  end;

implementation

uses
  VamLib.Utils,
  AggPathStorage,
  RedFox2D;

{ TFreqDisplay }

constructor TFreqDisplay.Create;
begin
end;

destructor TFreqDisplay.Destroy;
begin
  inherited;
end;

procedure TFreqDisplay.SetSize(w, h: integer);
begin
end;

procedure TFreqDisplay.ProcessSignal(Dest: TRedFoxImageBuffer; const DestRect: TRect; const Source: IFreqAnalyzer);
var
  MData : PSingle;
  MFrames : integer;
  Magnitude : single;
  c1: Integer;
  dx1, dx2, dy1, dy2 : single;
  Path: TAggPathStorage;
begin
  Path := TAggPathStorage.Create;
  AutoFree(@Path);

  Dest.BufferInterface.ClearAll(255,255,255,0);
  Dest.BufferInterface.LineColor := LineColor;
  Dest.BufferInterface.FillColor := LineColor.WithAlpha(220);
  Dest.BufferInterface.LineWidth := 1;

  Source.GetAnalysisData(MData, MFrames);





  dx1 := DestRect.Left - 5;
  dy1 := DestRect.Top + DestRect.Height;

  Path.MoveTo(dx1, dy1);

  for c1 := 0 to MFrames-1 do
  begin
    Magnitude := MData^;
    Magnitude := Clamp(Magnitude, 0, 1);
    dx2 := DestRect.Left + (c1 / (MFrames-1)) * DestRect.Width;
    dy2 := DestRect.Top  + (1 - Magnitude) * DestRect.Height;
    Path.LineTo(dx2, dy2);
    inc(MData);
  end;


  dx1 := DestRect.Right + 5;
  dy1 := DestRect.Top + DestRect.Height;

  Path.LineTo(dx1, dy1);


  Dest.BufferInterface.ResetPath;
  Dest.BufferInterface.AddPath(Path);
  Dest.BufferInterface.DrawPath;


  {
  dx1 := DestRect.Left;
  dy1 := DestRect.Height;

  for c1 := 0 to MFrames-1 do
  begin
    dx2 := DestRect.Left + (c1 / (MFrames-1)) * DestRect.Width;
    dy2 := DestRect.Top  + (1 - MData^) * DestRect.Height;

    Dest.BufferInterface.Line(dx1, dy1, dx2, dy2);

    dx1 := dx2;
    dy1 := dy2;

    inc(MData);
  end;
  }
end;



end.
