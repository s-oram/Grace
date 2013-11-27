unit eeMipMapLayer;

interface

uses
  MoreTypes, eeSampleInt;

type
  // TMipMapLayer
  // If the mip map layer being based on the SmallInt type becomes a problem in the future,
  // it shouldn't be too difficult to drop in an alternative data array, and set the class
  // up so that it can handle either float data or SmallInt data.

  TMipMapLayer = class
  private
    fCh2: Pointer;
    fCh1: Pointer;
    fSampleFrames: integer;
    fChannelCount: integer;
  protected
    Ch1Data_Int:TArrayOfSmallInt;
    Ch2Data_Int:TArrayOfSmallInt;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Assign(Source:TMipMapLayer);

    function Init(aChannelCount, aSampleFrames:integer):boolean;
    procedure Clear;

    procedure ImportFrom(Sample:TSampleInt); overload;
    procedure ImportFrom(MipMapLayer:TMipMapLayer); overload;

    property Ch1:Pointer read fCh1;
    property Ch2:Pointer read fCh2;
    property Ch1Data :TArrayOfSmallInt read Ch1Data_Int write Ch1Data_Int;
    property Ch2Data :TArrayOfSmallInt read Ch2Data_Int write Ch2Data_Int;

    property SampleFrames:integer read fSampleFrames;
    property ChannelCount:integer read fChannelCount;
  end;

implementation

uses
  uGeneralFunctions, SysUtils,
  eeDspSampleRateConversion;

{ TMipMapLayer }

constructor TMipMapLayer.Create;
begin

end;

destructor TMipMapLayer.Destroy;
begin
  Clear;
  inherited;
end;


function TMipMapLayer.Init(aChannelCount, aSampleFrames: integer): boolean;
begin
  assert((aChannelCount = 1) or (aChannelCount = 2));

  try
    if aChannelCount = 1 then
    begin
      SetLength(Ch1Data_Int, aSampleFrames);
      SetLength(Ch2Data_Int, 0);
      fCh1 := @Ch1Data_Int[0];
      fCh2 := @Ch1Data_Int[0];
    end else
    //if aChannelCount = 2 then
    begin
      SetLength(Ch1Data_Int, aSampleFrames);
      SetLength(Ch2Data_Int, aSampleFrames);
      fCh1 := @Ch1Data_Int[0];
      fCh2 := @Ch2Data_Int[0];
    end;

    fChannelCount := aChannelCount;
    fSampleFrames := aSampleFrames;

    //If we've made it this far, return true as the method has finished successfully.
    result := true;
  except
    on EOutOfMemory do
    begin
      Clear;
      result := false;
    end;
  end;
end;

procedure TMipMapLayer.Assign(Source: TMipMapLayer);
begin
  // TODO: This function may fail but that hasn't been taken into account. Perhaps it should raise an
  // exception here.....
  self.Clear;
  self.Init(source.ChannelCount, source.SampleFrames);
  self.ImportFrom(Source);
end;

procedure TMipMapLayer.Clear;
begin
  SetLength(Ch1Data_Int, 0);
  SetLength(Ch2Data_Int, 0);
  fCh1 := nil;
  fCh2 := nil;
  fChannelCount := 0;
  fSampleFrames := 0;
end;


procedure TMipMapLayer.ImportFrom(Sample: TSampleInt);
var
  c1: Integer;
  Src1, Src2, Dest1, Dest2:PSmallInt;
begin
  //Check data memory has been reserved..
  assert(self.ChannelCount <> 0);
  assert(self.SampleFrames <> 0);

  //To make things easier here, the channel count must match.
  assert(Sample.Properties.ChannelCount = self.ChannelCount);


  //===================================================================================================
  //If the sampleframe counts are the same, just copy the data, don't do any sample rate conversion.
  //===================================================================================================
  if Sample.Properties.SampleFrames = Self.SampleFrames then
  begin
    if (Self.ChannelCount = 1) then
    begin
      Src1  := Sample.Properties.Ch1;
      Dest1 := Self.Ch1;
      for c1 := 0 to Self.SampleFrames - 1 do
      begin
        Dest1^ := Src1^;
        inc(Src1);
        inc(Dest1);
      end;
    end;

    if (Self.ChannelCount = 2) then
    begin
      Src1  := Sample.Properties.Ch1;
      Src2  := Sample.Properties.Ch2;
      Dest1 := Self.Ch1;
      Dest2 := Self.Ch2;
      for c1 := 0 to Self.SampleFrames - 1 do
      begin
        Dest1^ := Src1^;
        Dest2^ := Src2^;
        inc(Src1);
        inc(Src2);
        inc(Dest1);
        inc(Dest2);
      end;
    end;

  end;


  //===================================================================================================
  //If the sampleframe counts are different, we need to do some sample rate conversion... 
  //===================================================================================================
  if Sample.Properties.SampleFrames <> Self.SampleFrames then
  begin
    if (Self.ChannelCount = 1) then
    begin
      Src1  := Sample.Properties.Ch1;
      Dest1 := Self.Ch1;
      RateConvert_WindowedSinc_Int(Src1, Sample.Properties.SampleFrames, Dest1, Self.SampleFrames);
    end;

    if (Self.ChannelCount = 2) then
    begin
      Src1  := Sample.Properties.Ch1;
      Src2  := Sample.Properties.Ch2;
      Dest1 := Self.Ch1;
      Dest2 := Self.Ch2;
      RateConvert_WindowedSinc_Int(Src1, Sample.Properties.SampleFrames, Dest1, Self.SampleFrames);
      RateConvert_WindowedSinc_Int(Src2, Sample.Properties.SampleFrames, Dest2, Self.SampleFrames);
    end;

  end;

end;

procedure TMipMapLayer.ImportFrom(MipMapLayer: TMipMapLayer);
var
  c1: Integer;
  Src1, Src2, Dest1, Dest2:PSmallInt;
begin
  //Check data memory has been reserved..
  assert(self.ChannelCount <> 0);
  assert(self.SampleFrames <> 0);

  //To make things easier here, the channel count must match.
  assert(MipMapLayer.ChannelCount = self.ChannelCount);


  //===================================================================================================
  //If the sampleframe counts are the same, just copy the data, don't do any sample rate conversion.
  //===================================================================================================
  if MipMapLayer.SampleFrames = Self.SampleFrames then
  begin
    if (Self.ChannelCount = 1) then
    begin
      Src1  := MipMapLayer.Ch1;
      Dest1 := Self.Ch1;
      for c1 := 0 to Self.SampleFrames - 1 do
      begin
        Dest1^ := Src1^;
        inc(Src1);
        inc(Dest1);
      end;
    end;

    if (Self.ChannelCount = 2) then
    begin
      Src1  := MipMapLayer.Ch1;
      Src2  := MipMapLayer.Ch2;
      Dest1 := Self.Ch1;
      Dest2 := Self.Ch2;
      for c1 := 0 to Self.SampleFrames - 1 do
      begin
        Dest1^ := Src1^;
        Dest2^ := Src2^;
        inc(Src1);
        inc(Src2);
        inc(Dest1);
        inc(Dest2);
      end;
    end;

  end;


  //===================================================================================================
  //If the sampleframe counts are different, we need to do some sample rate conversion...
  //===================================================================================================
  if MipMapLayer.SampleFrames <> Self.SampleFrames then
  begin
    if (Self.ChannelCount = 1) then
    begin
      Src1  := MipMapLayer.Ch1;
      Dest1 := Self.Ch1;
      RateConvert_WindowedSinc_Int(Src1, MipMapLayer.SampleFrames, Dest1, Self.SampleFrames);
    end;

    if (Self.ChannelCount = 2) then
    begin
      Src1  := MipMapLayer.Ch1;
      Src2  := MipMapLayer.Ch2;
      Dest1 := Self.Ch1;
      Dest2 := Self.Ch2;
      RateConvert_WindowedSinc_Int(Src1, MipMapLayer.SampleFrames, Dest1, Self.SampleFrames);
      RateConvert_WindowedSinc_Int(Src2, MipMapLayer.SampleFrames, Dest2, Self.SampleFrames);
    end;

  end;
end;



end.
