unit eeBeatLoopAnalyser.BeatChopperStrategy;

interface

uses
  Contnrs, eeSampleFloat, VamLib.MoreTypes,
  eeBeatLoopAnalyser.CoreTypes,
  eeBeatLoopAnalyser.GenericChopperStrategy;


type
  TBeatChopperA = class(TGenericChopper)
  private
    fRequiredSliceCount: integer;
    function FindTransientNearest(const Position, Margin: integer; const Transients:TTransientList):integer;
  public
    procedure Chop; override;
    property RequiredSliceCount : integer read fRequiredSliceCount write fRequiredSliceCount;
  end;


implementation

uses
  Math, VamLib.Utils, eeFilters.EnvFollowerA, eeDsp,
  MtxVec, MtxVecHelper, SignalUtils;



//==============================================================================

{ TBeatChopper }


// HilbertEnvelopeDetection()
// This procedure detects the amplitude envelope of the source signal
// using a Hilbert Transform.
// See "Understanding Digital Signal Processing" by Richard Lyons,
// Chapter 9.2. - Why Care About the Hilbert Transform.
procedure HilbertEnvelopeDetection(Dst, Src:TVec);
var
  tv1, tv2: TVec;
  c1: Integer;
  x1, x2 : single;
begin
  assert(Dst.Length = Src.Length);

  tv1 := TVec.Create;
  AutoFree(@tv1);

  tv2 := TVec.Create;
  AutoFree(@tv2);

  tv1.Hilbert(Src);
  tv2.ImagPart(tv1);

  for c1 := 0 to Src.Length-1 do
  begin
    x1 := Src[c1];
    x2 := tv2[c1];

    Dst[c1] := sqrt( x1*x1 + x2*x2 );
  end;
end;


// GenerateTransientDetectionEnvelopes();
//
// This function reads the raw amplitude envelope of a signal and generates two
// envelope curves using two envelope followers. The two envelope follower curves
// can be used to detect transients.
//
// The raw amplited envelope input should be generated with the HilbertEnvelopeDetection()
// procedure.
//
// Env1 and Env2 countain the results of both
procedure GenerateTransientDetectionEnvelopes_Method1(Env1, Env2:TVec; SrcEnv:TVec; SampleRate : integer);
var
  ef1 : TEnvFollowerA;
  ef2 : TEnvFollowerA;
  SampleFrames : integer;
  Offset       : single;
  c1           : integer;
begin
  assert(Env1.Length = SrcEnv.Length);
  assert(Env2.Length = SrcEnv.Length);

  ef1 := TEnvFollowerA.Create;
  AutoFree(@ef1);

  ef2 := TEnvFollowerA.Create;
  AutoFree(@ef2);

  SampleFrames := SrcEnv.Length;

  //Run the envelope followers...
  ef1.SampleRate := SampleRate;
  ef1.AttackTime := 0.1;
  ef1.ReleaseTime := 1000 / 80;

  ef2.SampleRate := SampleRate;
  ef2.AttackTime := 20;
  ef2.ReleaseTime := 1000 / 160;

  Offset := eeDsp.DecibelsToLinear(-28);
  for c1 := 0 to SampleFrames-1 do
  begin
    Env1.SData[c1] := ef1.Step(SrcEnv[c1]);
    Env2.SData[c1] := ef2.Step(Env1.SData[c1] + Offset);
  end;

end;

procedure GenerateTransientDetectionEnvelopes_Method2(Env1, Env2:TVec; SrcEnv:TVec; SampleRate : integer);
var
  ef1 : TEnvFollowerA;
  ef2 : TEnvFollowerA;
  SampleFrames : integer;
  c1           : integer;
  Offset       : single;
begin
  assert(Env1.Length = SrcEnv.Length);
  assert(Env2.Length = SrcEnv.Length);

  ef1 := TEnvFollowerA.Create;
  AutoFree(@ef1);

  ef2 := TEnvFollowerA.Create;
  AutoFree(@ef2);

  SampleFrames := SrcEnv.Length;

  //Run the envelope followers...
  ef1.SampleRate := SampleRate;
  ef1.AttackTime := 0.1;
  ef1.ReleaseTime := 1000 / 80;

  ef2.SampleRate := SampleRate;
  ef2.AttackTime := 20;
  ef2.ReleaseTime := 1000 / 80;

  Offset := eeDsp.DecibelsToLinear(-30);
  for c1 := 0 to SampleFrames-1 do
  begin
    Env1.SData[c1] := ef1.Step(SrcEnv[c1]);
    Env2.SData[c1] := ef2.Step(SrcEnv[c1]) + Offset;
  end;

end;




procedure TBeatChopperA.Chop;
var
  c1: Integer;
  aSlice : TSliceInfo;
  x1 : PSingle;
  p1, p2 : PSingle;

  v1  : TVec;
  v2  : TVec;
  v3  : TVec;
  t1, t2 : single;

  Transients : TTransientList;
  TrIndex        : integer;
  TargetPosition : integer;
  Margin         : integer;
begin
  v1 := TVec.Create;
  AutoFree(@v1);
  v2 := TVec.Create;
  AutoFree(@v2);
  v3 := TVec.Create;
  AutoFree(@v3);

  Transients := TTransientList.Create;
  AutoFree(@Transients);

  assert(self.RequiredSliceCount > 0);
  assert(Self.Data.SampleFrames > 0);

  SliceList.Clear;

  {
  //== Create all slices.
  for c1 := 0 to RequiredSliceCount-1 do
  begin
    aSlice := TSliceInfo.Create;
    aSlice.Position := floor(Data.SampleFrames * c1 / RequiredSliceCount);
    //aSlice.SampleFrames := floor(Data.SampleFrames / RequiredSliceCount);
    aSlice.SampleFrames := 100;
    SliceList.Add(aSlice);
  end;
  }
  //===============================================================================
  //   Envelope Gen
  //===============================================================================
  v1.Length := Data.SampleFrames;
  v2.Length := Data.SampleFrames;
  v3.Length := Data.SampleFrames;

  v1.CopyFromPSingle(Data.Ch1, Data.SampleFrames);

  HilbertEnvelopeDetection(v1, v1);

  GenerateTransientDetectionEnvelopes_Method2(v2, v3, v1, Data.SampleRate);

  Transients.FindTransients(v2, v3);
  //===============================================================================

  //== write out env data.
  for c1 := 0 to Data.SampleFrames do
  begin
    //Env1.Ch1[c1] := 0;

    Env1.Ch1[c1] := v2.SData[c1];
    Env2.Ch1[c1] := v3.SData[c1];
  end;


  //===============================================================================



  Margin := floor(Data.SampleFrames / RequiredSliceCount * 0.2);
  for c1 := 0 to RequiredSliceCount-1 do
  begin
    TargetPosition := floor(Data.SampleFrames / RequiredSliceCount * c1);
    TrIndex := FindTransientNearest(TargetPosition, Margin, Transients);

    if TrIndex = -1 then
    begin
      aSlice.Position := TargetPosition;
      aSlice.SampleFrames := 100;
      SliceList.Add(aSlice);
    end else
    begin
      aSlice.Position := Transients[TrIndex].Position;
      aSlice.SampleFrames := 100;
      SliceList.Add(aSlice);
    end;
  end;


  if RequiredSliceCount > 0 then
  begin
    //Set the position of the first slice to the beginning of the loop.
    aSlice := SliceList[0];
    aSlice.Position := 0;
    SliceList[0] := aSlice
  end;

  SliceList.UpdateSliceLengths(Data.SampleFrames);


end;



// FindTransientNearest() looks for the strongest transient near the target position.
function TBeatChopperA.FindTransientNearest(const Position, Margin: integer; const Transients:TTransientList): integer;
var
  c1: Integer;
  LowRange, HighRange : integer;
  ts : single;
  TrIndex: integer;
begin
  LowRange  := Position - Margin;
  HighRange := Position + Margin;

  ts      := -1;
  TrIndex := -1;

  for c1 := 0 to Transients.Count-1 do
  begin
    if (Transients[c1].Position >= LowRange) and (Transients[c1].Position <= HighRange) then
    begin
      if Transients[c1].Strength > ts then
      begin
        ts := Transients[c1].Strength;
        TrIndex := c1;
      end;
    end;
  end;

  result := TrIndex
end;

end.
