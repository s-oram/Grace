unit uSampleZeroCrossings;

interface

uses
  VamLib.MoreTypes,
  eeSampleFloat;

type
  TSampleZeroCrossings = class
  private
    fIsValid: boolean;
    fDataArraySize: integer;
  protected
    ch1 : array of integer;
    ch2 : array of integer;
    chMid  : array of integer;
    chSide : array of integer;

    ZeroCrossingCount : integer;

    procedure SetDataArraySize(aSize : integer);
    property DataArraySize : integer read fDataArraySize;

    procedure InsertZeroCrossing(const ZeroCrossingIndex:integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure CalcZeroCrossingData(aSample : TSampleFloat);

    property IsValid : boolean read fIsValid;

    procedure FindNearestZeroCrossingIndex(const aSampleIndex: integer; out NextIndex, PrevIndex, NearestIndex, FarIndex : integer);


  end;

implementation

uses
  SysUtils;

{ TSampleZeroCrossings }

constructor TSampleZeroCrossings.Create;
begin
  fIsValid := false;
  fDataArraySize := 0;
end;

destructor TSampleZeroCrossings.Destroy;
begin
  SetDataArraySize(0);
  inherited;
end;

procedure TSampleZeroCrossings.InsertZeroCrossing(const ZeroCrossingIndex: integer);
begin
  assert(DataArraySize > 0);
  if ZeroCrossingCount >= DataArraySize then
  begin
    SetDataArraySize(round(DataArraySize * 1.6));
  end;

  ch1[ZeroCrossingCount] := ZeroCrossingIndex;
  inc(ZeroCrossingCount);

end;

procedure TSampleZeroCrossings.SetDataArraySize(aSize: integer);
begin
  assert(aSize >= 0);

  SetLength(ch1, aSize);
  SetLength(ch2, aSize);
  SetLength(chMid, aSize);
  SetLength(chSide, aSize);

  fDataArraySize := aSize;
end;

procedure TSampleZeroCrossings.CalcZeroCrossingData(aSample: TSampleFloat);
const
  kHysterisisAmt = 0.0000316227766; //approx -90dB
var
  LastZeroCrossingIndex : integer;
  CurrentIndex : integer;
  c1: Integer;

  InputSignalSum : single;
  SourceDataCh1 : PSingle;
  SourceDataCh2 : PSingle;
  WatchForRise : boolean;
begin
  fIsValid           := false;
  ZeroCrossingCount  := 0;
  SetDataArraySize(0);

  if (aSample.Properties.IsValid = true)  and (aSample.Properties.SampleFrames > 0) then
  begin
    ZeroCrossingCount := 0;


    // Set the data array size to some size so we're not requesting memory each
    // time a new zero crossing is found.
    SetDataArraySize(aSample.Properties.SampleFrames div 20);


    // Initialise some variables for the zero crossing search.
    if (aSample.Properties.ChannelCount = 1) then
    begin
      SourceDataCh1 := aSample.Properties.Ch1;
      SourceDataCh2 := aSample.Properties.Ch1;
    end else
    begin
      SourceDataCh1 := aSample.Properties.Ch1;
      SourceDataCh2 := aSample.Properties.Ch2;
    end;

    InputSignalSum := SourceDataCh1^ + SourceDataCh2^;

    if InputSignalSum > 0
      then WatchForRise := false
      else WatchForRise := true;

    //Insert a zero crossing point to mark the beginning of the file.
    InsertZeroCrossing(0);
    LastZeroCrossingIndex := 0;

    for c1 := 0 to aSample.Properties.SampleFrames-1 do
    begin
      InputSignalSum := SourceDataCh1^ + SourceDataCh2^;

      if (WatchForRise) and (InputSignalSum > 0) then
      begin
        //zero crossing found.
        WatchForRise := false;

        // Insert Zero crossing info.
        assert(LastZeroCrossingIndex <> c1); //it shouldn't be the same if my thinking is correct.
        InsertZeroCrossing(c1);
        LastZeroCrossingIndex := c1;
      end else
      if (WatchForRise = false) and (InputSignalSum <= -kHysterisisAmt) then
      begin
        // the source audio has dropped under the zero line. update current value
        // so that we can continue searching for zero crossings.
        WatchForRise := true;
      end;

      // increment the source data pointers.
      inc(SourceDataCh1);
      inc(SourceDataCh2);
    end;


    if LastZeroCrossingIndex < aSample.Properties.SampleFrames-1 then
    begin
      //Insert a zero crossing point to mark the end of the file.
      InsertZeroCrossing(aSample.Properties.SampleFrames-1);
      LastZeroCrossingIndex := aSample.Properties.SampleFrames-1;
    end;

  end;




  // After finding all the zero crossings, shrink the array to save memory.
  SetDataArraySize(ZeroCrossingCount);

  if ZeroCrossingCount > 0 then
  begin
    fIsValid := true;
  end;

end;

procedure TSampleZeroCrossings.FindNearestZeroCrossingIndex(const aSampleIndex: integer; out NextIndex, PrevIndex, NearestIndex, FarIndex : integer);
var
  c1 : integer;
begin
  assert(ZeroCrossingCount >= 2);
  assert(Ch1[0] = 0, 'First zero crossing marker must be at SampleIndex 0.');

  for c1 := 0 to ZeroCrossingCount-1 do
  begin
    if ch1[c1] > aSampleIndex then
    begin
      NextIndex := ch1[c1];
      PrevIndex := ch1[c1 - 1];

      if abs(NextIndex - aSampleIndex) < abs(aSampleIndex - PrevIndex) then
      begin
        NearestIndex := NextIndex;
        FarIndex     := PrevIndex;
      end else
      begin
        NearestIndex := PrevIndex;
        FarIndex     := NextIndex;
      end;

      //we've found the results so exit.
      exit; //============================>> exit >>=================>>
    end;
  end;

  PrevIndex    := Ch1[ZeroCrossingCount-2];
  NextIndex    := Ch1[ZeroCrossingCount-1];

  if abs(NextIndex - aSampleIndex) < abs(aSampleIndex - PrevIndex) then
  begin
    NearestIndex := NextIndex;
    FarIndex     := PrevIndex;
  end else
  begin
    NearestIndex := PrevIndex;
    FarIndex     := NextIndex;
  end;


end;







end.
