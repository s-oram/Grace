unit uLucidityVoiceAmp;

interface

uses
  VamLib.Utils,
  VamLib.MoreTypes, eeFunctions;

type
  TVoiceAmpModPoints = record
    ModInput_Gain : single;
    ModInput_Pan  : single;
  end;

  TLucidityVoiceAmp = class
  private
    fPan       : single;
    fPanScaled : single;
    fGain: single;
    procedure SetPan(const Value: single);
  protected
    ModPoints : TVoiceAmpModPoints;

    GainLeft  : single;
    GainRight : single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure ControlRateStep;
    procedure AudioRateStep(var x1, x2 : single);

    property Gain : single read fGain write fGain; //range 0..2
    property Pan  : single read fPan  write SetPan;  //range 0..1
  end;

implementation

uses
  SysUtils;

{ TLucidityVoiceAmp }

constructor TLucidityVoiceAmp.Create;
begin

end;

destructor TLucidityVoiceAmp.Destroy;
begin

  inherited;
end;

function TLucidityVoiceAmp.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'ModInput_Gain' then Exit(@ModPoints.ModInput_Gain);
  if Name = 'ModInput_Pan' then Exit(@ModPoints.ModInput_Pan);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLucidityVoiceAmp.SetPan(const Value: single);
begin
  fPan := Value;
  fPanScaled := Value * 0.5 + 0.5;
  assert(fPanScaled >= 0);
  assert(fPanScaled <= 1);
end;

procedure TLucidityVoiceAmp.ControlRateStep;
var
  xLeft : single;
  PanPos : single;

begin
  xLeft := ModPoints.ModInput_Gain * ModPoints.ModInput_Gain;
  xLeft := Clamp(xLeft, 0,1);

  PanPos := fPanScaled + ModPoints.ModInput_Pan;
  PanPos := Clamp(PanPos, 0,1);

  GainLeft  := xLeft * Gain * Sqrt(1 - PanPos);
  GainRight := xLeft * Gain * Sqrt(PanPos);
end;

procedure TLucidityVoiceAmp.AudioRateStep(var x1, x2: single);
begin
  x1 := x1 * GainLeft;
  x2 := x2 * GainRight;
end;





end.
