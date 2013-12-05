unit VamSamplePeakBuffer;

interface

type
  TPeakBuffer = class;
  IPeakBuffer = interface;

  TPeakBufferDataPoint = record
    MinValue : single;
    MaxValue : single;
  end;


  TPeakBufferData = TArray<TPeakBufferDataPoint>;
  PPeakBufferData = ^TPeakBufferData;


  IPeakBuffer = interface
    ['{E925DFD8-A07D-403A-A0FD-7D4AFE96C27F}']

    procedure Clear;

    procedure GeneratePeaks(InputA : PSingle; const InputSampleFrames, PeakFrames : integer); overload;
    procedure GeneratePeaks(InputA, InputB : PSingle; const InputSampleFrames, PeakFrames : integer); overload;

    function GetDataFrames : integer;
    function GetDataChannels : integer;
    function GetDataA : PPeakBufferData;
    function GetDataB : PPeakBufferData;
  end;

  TPeakBuffer = class(TInterfacedObject, IPeakBuffer)
  private
    fDataFrames: integer;
    fDataA: TPeakBufferData;
    fDataB: TPeakBufferData;
    fDataChannels: integer;

    procedure InternalGenPeaks(Input : PSingle; const InputSampleFrames, PeakFrames : integer; var Buffer : TPeakBufferData);
    procedure InternalClearPeaks(PeakFrames : integer; var Buffer : TPeakBufferData);


    function GetDataFrames : integer;
    function GetDataChannels : integer;
    function GetDataA : PPeakBufferData;
    function GetDataB : PPeakBufferData;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure GeneratePeaks(InputA : PSingle; const InputSampleFrames, PeakFrames : integer); overload;
    procedure GeneratePeaks(InputA, InputB : PSingle; const InputSampleFrames, PeakFrames : integer); overload;

    property DataFrames   : integer   read fDataFrames;   //number of points in the data buffer.
    property DataChannels : integer   read fDataChannels; //0,1 or 2
    property DataA : TPeakBufferData  read fDataA;
    property DataB : TPeakBufferData  read fDataB;
  end;

implementation

uses
  SysUtils;

const
  INF    =  (1.0 / 0.0);

{ TPeakBuffer }

procedure TPeakBuffer.Clear;
begin
  fDataFrames   := 0;
  fDataChannels := 0;
  SetLength(fDataA, 0);
  SetLength(fDataB, 0);
end;

constructor TPeakBuffer.Create;
begin
  Clear;
end;

destructor TPeakBuffer.Destroy;
begin
  Clear;
  inherited;
end;

procedure TPeakBuffer.GeneratePeaks(InputA: PSingle; const InputSampleFrames, PeakFrames: integer);
begin
  fDataFrames   := PeakFrames;
  fDataChannels := 1;
  SetLength(fDataA, PeakFrames);
  SetLength(fDataB, 0);

  if InputSampleFrames > PeakFrames then
  begin
    InternalGenPeaks(InputA, InputSampleFrames, PeakFrames, fDataA);
  end else
  begin
    InternalClearPeaks(PeakFrames, fDataA);
  end;
end;

procedure TPeakBuffer.GeneratePeaks(InputA, InputB: PSingle; const InputSampleFrames, PeakFrames: integer);
begin
  fDataFrames   := PeakFrames;
  fDataChannels := 2;
  SetLength(fDataA, PeakFrames);
  SetLength(fDataB, PeakFrames);

  if InputSampleFrames > PeakFrames then
  begin
    InternalGenPeaks(InputA, InputSampleFrames, PeakFrames, fDataA);
    InternalGenPeaks(InputB, InputSampleFrames, PeakFrames, fDataB);
  end else
  begin
    InternalClearPeaks(PeakFrames, fDataA);
    InternalClearPeaks(PeakFrames, fDataB);
  end;
end;

function TPeakBuffer.GetDataA: PPeakBufferData;
begin
  result := @fDataA;
end;

function TPeakBuffer.GetDataB: PPeakBufferData;
begin
  result := @fDataB;
end;

function TPeakBuffer.GetDataChannels: integer;
begin
  result := fDataChannels;
end;

function TPeakBuffer.GetDataFrames: integer;
begin
  result := fDataFrames;
end;

procedure TPeakBuffer.InternalClearPeaks(PeakFrames: integer; var Buffer: TPeakBufferData);
var
  c1: Integer;
begin
  for c1 := 0 to PeakFrames-1 do
  begin
    Buffer[c1].MinValue := 0;
    Buffer[c1].MaxValue := 0;
  end;
end;

procedure TPeakBuffer.InternalGenPeaks(Input: PSingle; const InputSampleFrames, PeakFrames: integer; var Buffer: TPeakBufferData);
  procedure FindMinMax(Input: PSingle; MarkA, MarkB : integer; out MinValue, MaxValue : single);
  var
    SamplePoints : array of single absolute Input;
    c2: Integer;
  begin
    MinValue := INF;
    MaxValue := -INF;
    for c2 := MarkA to MarkB do
    begin
      if SamplePoints[c2] > MaxValue then MaxValue := SamplePoints[c2];
      if SamplePoints[c2] < MinValue then MinValue := SamplePoints[c2];
    end;
  end;
var
  SamplesPerPeak : integer;
  c1: Integer;
  MarkA : integer;
  MarkB : integer;
  MinValue, MaxValue : single;
begin
  SamplesPerPeak := round(InputSampleFrames / PeakFrames) + 1;

  for c1 := 0 to PeakFrames-1 do
  begin
    MarkA := round( (c1)   * (InputSampleFrames / PeakFrames) );
    MarkB := round( (c1+1) * (InputSampleFrames / PeakFrames) );

    if MarkA < 0 then MarkA := 0;
    if MarkA >= InputSampleFrames then MarkA := InputSampleFrames-1;

    if MarkB < 0 then MarkB := 0;
    if MarkB >= InputSampleFrames then MarkB := InputSampleFrames-1;

    FindMinMax(Input, MarkA, MarkB, MinValue, MaxValue);

    Buffer[c1].MinValue := MinValue;
    Buffer[c1].MaxValue := MaxValue;
  end;
end;

end.
