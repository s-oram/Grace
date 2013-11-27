unit eeMultiChannelBuffers;

interface

{$INCLUDE Defines.inc}

uses
  MoreTypes, SysUtils;

type

  TCustomMultiBuffer = class
  private
    fSampleFrames: integer;
    fChannelCount: integer;

    procedure SetChannelCount(const Value: integer);
    procedure SetSampleFrames(const Value: integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetSize(const NewChannelCount, NewSampleFrames: integer); virtual; abstract;

    property ChannelCount : integer read fChannelCount write SetChannelCount;
    property SampleFrames : integer read fSampleFrames write SetSampleFrames;
  end;


  TMultiSingleBuffer = class(TCustomMultiBuffer)
  private
    fData: T2dArrayOfSingle;
  public
    procedure Clear;
    procedure SetSize(const NewChannelCount, NewSampleFrames: integer); override;
    property Data : T2dArrayOfSingle read fData;


  end;

  TMultiDoubleBuffer = class(TCustomMultiBuffer)
  private
    fData: T2dArrayOfDouble;
  public
    procedure Clear;
    procedure SetSize(const NewChannelCount, NewSampleFrames: integer); override;
    property Data : T2dArrayOfDouble read fData;
  end;



implementation



{ TMultiSingleBuffer }

constructor TCustomMultiBuffer.Create;
begin
  fChannelCount := 0;
  fSampleFrames := 0;
end;

destructor TCustomMultiBuffer.Destroy;
begin
  SetSize(0,0);
  inherited;
end;

procedure TCustomMultiBuffer.SetChannelCount(const Value: integer);
begin
  SetSize(Value, SampleFrames);
end;

procedure TCustomMultiBuffer.SetSampleFrames(const Value: integer);
begin
  SetSize(ChannelCount, Value);
end;


{ TMultiSingleBuffer }

procedure TMultiSingleBuffer.Clear;
var
  c1: Integer;
  c2: Integer;
begin
  for c1 := 0 to ChannelCount-1 do
  begin
    for c2 := 0 to SampleFrames-1 do
    begin
      fData[c1,c2] := 0;
    end;
  end;
end;


procedure TMultiSingleBuffer.SetSize(const NewChannelCount, NewSampleFrames: integer);
var
  c1 : integer;
  CurCh, CurSF : integer;
begin
  CurCh := fChannelCount;
  CurSF := fSampleFrames;

  if (NewChannelCount = CurCh) and (NewSampleFrames = CurSF) then exit;

  if (NewChannelCount = CurCh) then
  begin
    for c1 := 0 to CurCh-1 do
    begin
      SetLength(fData[c1], NewSampleFrames);
    end;

    fSampleFrames := NewSampleFrames;

    exit; //========================================>> exit >>=================>>
  end;


  if (NewChannelCount <> CurCh) then
  begin
    for c1 := 0 to CurCh-1 do
    begin
      SetLength(fData[c1], 0);
    end;

    SetLength(fData, NewChannelCount);

    for c1 := 0 to NewChannelCount-1 do
    begin
      SetLength(fData[c1], NewSampleFrames);
    end;

    fChannelCount := NewChannelCount;
    fSampleFrames := NewSampleFrames;

    exit; //========================================>> exit >>=================>>
  end;

end;


{ TMultiDoubleBuffer }

procedure TMultiDoubleBuffer.Clear;
var
  c1: Integer;
  c2: Integer;
begin
  for c1 := 0 to ChannelCount-1 do
  begin
    for c2 := 0 to SampleFrames-1 do
    begin
      fData[c1,c2] := 0;
    end;
  end;
end;

procedure TMultiDoubleBuffer.SetSize(const NewChannelCount, NewSampleFrames: integer);
var
  c1 : integer;
  CurCh, CurSF : integer;
begin
  CurCh := fChannelCount;
  CurSF := fSampleFrames;

  if (NewChannelCount = CurCh) and (NewSampleFrames = CurSF) then exit;

  if (NewChannelCount = CurCh) then
  begin
    for c1 := 0 to CurCh-1 do
    begin
      SetLength(fData[c1], NewSampleFrames);
    end;

    fSampleFrames := NewSampleFrames;

    exit; //========================================>> exit >>=================>>
  end;


  if (NewChannelCount <> CurCh) then
  begin
    for c1 := 0 to CurCh-1 do
    begin
      SetLength(fData[c1], 0);
    end;

    SetLength(fData, NewChannelCount);

    for c1 := 0 to NewChannelCount-1 do
    begin
      SetLength(fData[c1], NewSampleFrames);
    end;

    fChannelCount := NewChannelCount;
    fSampleFrames := NewSampleFrames;

    exit; //========================================>> exit >>=================>>
  end;

end;

end.
