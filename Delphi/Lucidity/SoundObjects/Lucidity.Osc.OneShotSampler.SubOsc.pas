unit Lucidity.Osc.OneShotSampler.SubOsc;

interface

uses
  Lucidity.Interfaces,
  VamLib.MoreTypes, uLucidityEnums,
  Lucidity.SampleMap, soGateEnvelope,
  uConstants, eeSampleFloat,
  Math, eeDsp, eeFunctions,
  eeDsp.Interpolation,
  SampleOscUtils;

type
  TOscState = (osInactive, osPlayForward, osLoopForward, osLoopBackward, osPlayForwardFadeOut, osPlayBackwardFadeOut);
  TOscTriggerDirection = (PlayForwards, PlayBackwards);
  TOscTriggerPoint = (SampleStart, SampleEnd, LoopStart, LoopEnd);

  TUpdateSampleBounds = procedure(const Sender:TObject; const aSampleRegion:IRegion; const SampleBounds:PSampleOsc_SampleBounds) of object;

  TLoopEndEvent = procedure(const Sender : TObject; const LoopOffset:single; const CurrentOscState:TOscState) of object;

  TOneShotSamplerSubOsc = class
  private
    fSampleRate: single;
    fRate: single;
    fOnUpdateSampleBounds: TUpdateSampleBounds;
    fOnLoopEnd: TLoopEndEvent;
    fLoopBounds: TSamplerLoopBounds;
    procedure SetRate(const Value: single);
    procedure SetSampleRate(const Value: single);
  protected
    Sample : TSampleFloat;
    SampleRegion : IRegion;
    SampleBounds : TSampleOsc_SampleBounds;
    fOscState : TOscState;

    Index : single;
    RelativeRate : single;

    Env : TGateEnvelope;

    procedure UpdateSampleBounds; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetupOsc(const aSampleRegion:IRegion; const aSample:TSampleFloat);
    procedure Trigger(const Direction : TOscTriggerDirection; const TriggerPoint : TOscTriggerPoint; const SmoothAttack:boolean);
    procedure QuickRelease;
    procedure Kill;

    procedure ControlRateStep; inline;
    procedure AudioRateStep(out Out1, Out2 : Single); inline;

    property SampleRate : single read fSampleRate write SetSampleRate;

    property Rate : single read fRate write SetRate;

    property OscState : TOscState read fOscState;

    //=== For GUI Feedback ===
    function GetCurrentSampleBounds:PSampleOsc_SampleBounds;
    function GetCurrentPlaybackPos : integer;

    property LoopBounds : TSamplerLoopBounds read fLoopBounds write fLoopBounds;

    property OnLoopEnd            : TLoopEndEvent       read fOnLoopEnd            write fOnLoopEnd;
    property OnUpdateSampleBounds : TUpdateSampleBounds read fOnUpdateSampleBounds write fOnUpdateSampleBounds;
  end;

implementation

uses
  SysUtils;


{ TSampleOsc }

constructor TOneShotSamplerSubOsc.Create;
begin
  Env := TGateEnvelope.Create;
  Env.AttackTime_MS  := 5;
  Env.ReleaseTime_MS := 5;

  fSampleRate := 44100;
  fOscState := osInactive;
end;

destructor TOneShotSamplerSubOsc.Destroy;
begin
  SampleRegion := nil;
  Env.Free;
  inherited;
end;

function TOneShotSamplerSubOsc.GetCurrentPlaybackPos: integer;
begin
  result := round(Index);
end;

function TOneShotSamplerSubOsc.GetCurrentSampleBounds: PSampleOsc_SampleBounds;
begin
  result := @SampleBounds;
end;

procedure TOneShotSamplerSubOsc.SetRate(const Value: single);
begin
  fRate := Value;

  if assigned(Sample) then
  begin
    RelativeRate := fRate * (Sample.Properties.SampleRate / SampleRate);
  end else
  begin
    RelativeRate := fRate;
  end;
end;

procedure TOneShotSamplerSubOsc.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  Env.SampleRate := Value;
end;

procedure TOneShotSamplerSubOsc.SetupOsc(const aSampleRegion: IRegion; const aSample: TSampleFloat);
begin
  assert(aSample <> nil);
  Sample       := aSample;
  SampleRegion := aSampleRegion;
  fOscState := osInactive;
end;

procedure TOneShotSamplerSubOsc.Trigger(const Direction : TOscTriggerDirection; const TriggerPoint : TOscTriggerPoint; const SmoothAttack:boolean);
begin
  UpdateSampleBounds;

  //TODO: What is "LoopModeB"?
  case LoopBounds of
    {
    TSamplerLoopBounds.LoopOff:
    begin
      case Direction of
        TOscTriggerDirection.PlayForwards:  fOscState := TOscState.osPlayForward;
        TOscTriggerDirection.PlayBackwards: fOscState := TOscState.osPlayForward;
      else
        raise Exception.Create('unexpected Direction value.');
      end;
    end;
    }
    TSamplerLoopBounds.LoopSample:
    begin
      //TODO: This is copied from Loop Using Loop Points.
      case Direction of
        TOscTriggerDirection.PlayForwards:  fOscState := TOscState.osLoopForward;
        TOscTriggerDirection.PlayBackwards: fOscState := TOscState.osLoopBackward;
      else
        raise Exception.Create('unexpected Direction value.');
      end;
    end;

    TSamplerLoopBounds.LoopPoints:
    begin
      case Direction of
        TOscTriggerDirection.PlayForwards:  fOscState := TOscState.osLoopForward;
        TOscTriggerDirection.PlayBackwards: fOscState := TOscState.osLoopBackward;
      else
        raise Exception.Create('unexpected Direction value.');
      end;
    end;
  else
    raise Exception.Create('Loop type not handled.');
  end;


  case TriggerPoint of
    SampleStart: Index := SampleBounds.SampleStart;
    SampleEnd:   Index := SampleBounds.SampleEnd;
    LoopStart:   Index := SampleBounds.LoopStart;
    LoopEnd:     Index := SampleBounds.LoopEnd;
  else
    raise Exception.Create('unexpected TriggerPoint value.');
  end;


  if SmoothAttack
    then Env.AttackTime_MS := 5
    else env.AttackTime_MS := 0;

  Env.ReleaseTime_MS := 5;

  Env.Kill;
  Env.Trigger;

end;

procedure TOneShotSamplerSubOsc.QuickRelease;
begin
  case fOscState of
    osLoopForward:  fOscState := TOscState.osPlayForwardFadeOut;
    osLoopBackward: fOscState := TOscState.osPlayBackwardFadeOut;
    osInactive: ; //do nothing.
    osPlayForward: ; //do nothing.
    osPlayForwardFadeOut: ; //do nothing.
    osPlayBackwardFadeOut: ; //do nothing.
  else
    raise Exception.Create('Unexpected OscState value.');
  end;

  Env.Release;
end;

procedure TOneShotSamplerSubOsc.Kill;
begin
  SampleRegion := nil;
  fOscState := osInactive;
  Env.Kill;
end;

procedure TOneShotSamplerSubOsc.UpdateSampleBounds;
begin
  OnUpdateSampleBounds(self, SampleRegion, @SampleBounds);
end;

procedure TOneShotSamplerSubOsc.ControlRateStep;
begin

end;

procedure TOneShotSamplerSubOsc.AudioRateStep(out Out1, Out2: Single);
var
  LoopOffset : single;
  ax         : integer;
  frac       : single;
  FadeAmp    : single;
begin
  ax := floor(Index);
  frac := Index - ax;

  FadeAmp := Env.Step;

  //ReadValuesFromSample_LinearInterpolation(Sample, ax, Frac, Out1, Out2);
  ReadValuesFromSample_4x3_Optimal(Sample, ax, Frac, Out1, Out2);

  Out1 := Out1 * FadeAmp;
  Out2 := Out2 * FadeAmp;


  //== Increment phase ==
  case fOscState of
    osInactive: ;

    osPlayForward:
    begin
      Index := Index + RelativeRate;
      if (Index >= SampleBounds.SampleEnd-1) then
      begin
        Index := SampleBounds.SampleEnd-1;
        fOscState := osInactive;
      end;
    end;

    osLoopForward:
    begin
      Index := Index + RelativeRate;
      if (Index >= SampleBounds.LoopEnd-1) then
      begin
        LoopOffset := Index - (SampleBounds.LoopEnd-1);
        OnLoopEnd(self, LoopOffset, fOscState);
        Env.Release;
        fOscState := osPlayForwardFadeOut;
      end;
    end;

    osPlayForwardFadeOut:
    begin
      Index := Index + RelativeRate;
      if (Index >= SampleBounds.AbsoluteSampleEnd-1) then
      begin
        Index := SampleBounds.AbsoluteSampleEnd-1;
        fOscState := osInactive;
      end;
    end;

    osLoopBackward:
    begin
    end;

    osPlayBackwardFadeOut:
    begin
    end;
  end;
end;




end.
