unit soLevelMeter;

interface

uses
  Math,
  eeDsp,
  VamLib.MoreTypes,
  VamLib.ZeroObject,
  VamGuiControlInterfaces;

type
  TLevelMonitor = class(TZeroObject, ILevelMonitor)
  private
    LevelA, LevelB : single;
    fSampleRate: single;
    DecayCoefficient : single;
    procedure SetSampleRate(const Value: single);
  protected
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetDbLevel(out Ch1, Ch2 : single);

    procedure Process(InputA, InputB : PSingle; const SampleFrames : integer);

    property SampleRate : single read fSampleRate write SetSampleRate;
  end;

implementation

{ TLevelMonitor }

constructor TLevelMonitor.Create;
begin
  DecayCoefficient := 0;
end;

destructor TLevelMonitor.Destroy;
begin

  inherited;
end;



procedure TLevelMonitor.GetDbLevel(out Ch1, Ch2: single);
begin
  Ch1 := LinearToDecibels(LevelA);
  Ch2 := LinearToDecibels(LevelB);
end;

procedure TLevelMonitor.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;

  //https://en.wikipedia.org/wiki/Peak_programme_meter
  DecayCoefficient := CalcRcEnvelopeCoefficient(700, fSampleRate);
end;

procedure TLevelMonitor.Process(InputA, InputB: PSingle; const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    if InputA^ > LevelA
      then LevelA := InputA^
      else LevelA := LevelA * DecayCoefficient;

    if InputB^ > LevelB
      then LevelB := InputB^
      else LevelB := LevelB * DecayCoefficient;

    inc(InputA);
    inc(InputB);
  end;
end;

procedure TLevelMonitor.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB: IInterface);
begin
  inherited;

end;

end.
