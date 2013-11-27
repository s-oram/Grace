unit soLucidityFilter;

interface

{$INCLUDE Defines.inc}

uses
  MoreTypes,
  uConstants,
  uLucidityEnums,
  Math,
  eeVirtualCV, eeFunctions,
  FilterCore.SimperSVF,
  soFilter.Test,
  soFilter.LofiA,
  soFilter.RingModA,
  soFilter.DistortionA,
  soFilter.CombA,
  soFilter.LowPassA,
  soFilter.LowPassB,
  soFilter.BandPassA,
  soFilter.HighPassA;

type
  TLucidityFilter = class
  private
    fSampleRate: single;
    fPar1: single;
    fPar2: single;
    fPar3: single;
    fFilterType: TFilterType;
    fPar4: single;
    procedure SetSampleRate(const Value: single);
    procedure SetFilterType(const Value: TFilterType);
  protected
    DistortionA : TDistortionA;
    RingModA    : TRingModA;
    LofiA       : TLofiA;
    CombA       : TCombA;
    LowPassA    : TLowPassA;
    BandPassA   : TBandPassA;
    HighPassA   : THighPassA;
    LowPassB    : TLowPassB;

    TestFilter : TTestFilter;

    Par1Mod: single;
    Par2Mod: single;
    Par3Mod: single;
    Par4Mod: single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure Reset;

    property SampleRate : single read fSampleRate write SetSampleRate;

    procedure AudioRateStep(var x1, x2 : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    //==== Parameters ====
    property FilterType : TFilterType read fFilterType write SetFilterType;

    property Par1 : single read fPar1 write fPar1; //range 0..1
    property Par2 : single read fPar2 write fPar2; //range 0..1
    property Par3 : single read fPar3 write fPar3; //range 0..1
    property Par4 : single read fPar4 write fPar4; //range 0..1
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils;

{ TLucidityFilter }

constructor TLucidityFilter.Create;
begin
  TestFilter := TTestFilter.Create;
  RingModA   := TRingModA.Create;
  DistortionA := TDistortionA.Create;
  LofiA       := TLofiA.Create;
  CombA       := TCombA.Create;
  LowPassA   := TLowPassA.Create;
  BandPassA  := TBandPassA.Create;
  HighPassA  := THighPassA.Create;
  LowPassB    := TLowPassB.Create;
end;

destructor TLucidityFilter.Destroy;
begin
  TestFilter.Free;
  LowPassA.Free;
  BandPassA.Free;
  HighPassA.Free;
  LofiA.Free;
  CombA.Free;
  RingModA.Free;
  DistortionA.Free;
  LowPassB.Free;
  inherited;
end;

procedure TLucidityFilter.Reset;
begin
  DistortionA.Reset;
  RingModA.Reset;
  LowPassA.Reset;
  LowPassB.Reset;
  BandPassA.Reset;
  HighPassA.Reset;
end;

function TLucidityFilter.GetModPointer(const Name: string): PSingle;
begin
  if Name ='Par1Mod' then exit(@Par1Mod);
  if Name ='Par2Mod' then exit(@Par2Mod);
  if Name ='Par3Mod' then exit(@Par3Mod);
  if Name ='Par4Mod' then exit(@Par4Mod);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLucidityFilter.SetFilterType(const Value: TFilterType);
begin
  fFilterType := Value;

  LofiA.Reset;
  CombA.Reset;
  LowPassA.Reset;
  LowPassB.Reset;
  BandPassA.Reset;
  HighPassA.Reset;
  RingModA.Reset;
  DistortionA.Reset;
end;

procedure TLucidityFilter.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;

  LowPassA.SampleRate := Value;
  LowPassB.SampleRate := Value;
  BandPassA.SampleRate := Value;
  HighPassA.SampleRate := Value;
  LofiA.SampleRate := Value;
  CombA.SampleRate := Value;
  RingModA.SampleRate := Value;
  DistortionA.SampleRate := Value;
end;


procedure TLucidityFilter.FastControlProcess;
const
  kBaseFilterFreq = 4.0878994578;
  kMinFreq = 0.001;
  kMaxFreq = 18000;
  kMinQ = 0;
  kMaxQ = 0.98;
var
  cFreq : single;
  cQ    : single;
  CV    : single;

  px1 : single;
  px2 : single;
  px3 : single;
begin
  case FilterType of
    ftNone:
    begin
    end;

    ftLowPassA:
    begin
      CV := (Par1 * 15) + AudioRangeToModularVoltage(Par1Mod);
      cFreq := VoltsToFreq(kBaseFilterFreq, CV);
      Clamp(cFreq, kMinFreq, kMaxFreq);

      cQ := (Par2 + Par2Mod) * 0.98;
      Clamp(cQ, kMinQ, kMaxQ);

      LowPassA.Freq := cFreq;
      LowPassA.Q    := cQ;
    end;

    ftBandPassA:
    begin
      CV := (Par1 * 15) + AudioRangeToModularVoltage(Par1Mod);
      cFreq := VoltsToFreq(kBaseFilterFreq, CV);
      Clamp(cFreq, kMinFreq, kMaxFreq);

      cQ := (Par2 + Par2Mod) * 0.98;
      Clamp(cQ, kMinQ, kMaxQ);

      BandPassA.Freq := cFreq;
      BandPassA.Q    := cQ;
    end;

    ftHighPassA:
    begin
      CV := (Par1 * 15) + AudioRangeToModularVoltage(Par1Mod);
      cFreq := VoltsToFreq(kBaseFilterFreq, CV);
      Clamp(cFreq, kMinFreq, kMaxFreq);

      cQ := (Par2 + Par2Mod) * 0.98;
      Clamp(cQ, kMinQ, kMaxQ);

      HighPassA.Freq := cFreq;
      HighPassA.Q    := cQ;
    end;

    ftLofiA:
    begin
      //==== Lofi A ====
      px1 := Par1 + Par1Mod;
      Clamp(px1, 0, 1);
      LofiA.RateReduction := px1;

      px2 := Par2 + Par2Mod;
      clamp(px2, 0, 1);
      LofiA.BitReduction := px2;

      px3 := Par3 + Par3Mod;
      clamp(px3, 0, 1);
      LofiA.BitEmphasis := px3;
    end;

    ftRingModA:
    begin
      //==== Ring Mod A ====
      CV := (Par1 * 12) + AudioRangeToModularVoltage(Par1Mod);
      cFreq := VoltsToFreq(15, CV);
      Clamp(cFreq, 15, 18000);
      RingModA.OscFreq := cFreq;

      px2 := Par2 + Par2Mod;
      clamp(px2, 0, 1);
      RingModA.Depth := px2;
    end;


    {
    ftDistA:
    begin
      //==== Distortion A ====
      px1 := Par1 + Par1Mod;
      Clamp(px1, 0, 1);
      DistortionA.Par1 := px1;

      px2 := Par2 + Par2Mod;
      clamp(px2, 0, 1);
      DistortionA.Par2 := px2;

      px3 := Par3 + Par3Mod;
      clamp(px3, 0, 1);
      DistortionA.Par3 := px3;
    end;
    }

    ftCombA:
    begin
      //==== Comb A ====
      px1 := Par1 + Par1Mod;
      Clamp(px1, 0, 1);
      CombA.Par1 := px1;

      px2 := Par2 + Par2Mod;
      clamp(px2, 0, 1);
      CombA.Par2 := px2;

      px3 := Par3 + Par3Mod;
      clamp(px3, 0, 1);
      CombA.Par3 := px3;
    end;

  end;

end;

procedure TLucidityFilter.SlowControlProcess;
begin

end;

procedure TLucidityFilter.AudioRateStep(var x1, x2: single);
begin
  case FilterType of
    ftNone: ;
    ftLowPassA:  LowPassA.Step(x1, x2);
    ftBandPassA: BandPassA.Step(x1, x2);
    ftHighPassA: HighPassA.Step(x1, x2);
    ftLofiA:     LofiA.Step(x1, x2);
    ftRingModA:  RingModA.AudioRateStep(x1, x2);
    //ftDistA:     DistortionA.AudioRateStep(x1, x2);
    ftCombA:     CombA.AudioRateStep(x1, x2);
  else
    raise Exception.Create('Unexpected filter type.');
  end;
end;

end.
