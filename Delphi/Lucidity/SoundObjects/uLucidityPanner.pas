unit uLucidityPanner;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.Utils,
  VamLib.MoreTypes, eeFunctions;

type
  TPannerModPoints = record
    ModInput_Pan  : single;
  end;

  TLucidityPanner = class
  private
    fPan       : single;
    fPanScaled : single;
    procedure SetPan(const Value: single);
  protected
    ModPoints : TPannerModPoints;
    GainLeft  : single;
    GainRight : single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateStep(var x1, x2 : single);  {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateProcess(x1, x2 : PSingle; const SampleFrames:integer); {$IFDEF AudioInline}inline;{$ENDIF}

    property Pan  : single read fPan  write SetPan;  //range 0..1
  end;

implementation

uses
  SysUtils;

{ TLucidityVoiceAmp }

constructor TLucidityPanner.Create;
begin

end;

destructor TLucidityPanner.Destroy;
begin

  inherited;
end;

function TLucidityPanner.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'ModInput_Pan' then Exit(@ModPoints.ModInput_Pan);

  //result := nil;
  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidityPanner.SetPan(const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fPan := Value;
  fPanScaled := Value;
end;

procedure TLucidityPanner.FastControlProcess;
var
  PanPos : single;
begin
  PanPos := fPanScaled + ModPoints.ModInput_Pan;
  PanPos := Clamp(PanPos, 0,1);

  GainLeft  := Sqrt(1 - PanPos);
  GainRight := Sqrt(PanPos);
end;

procedure TLucidityPanner.AudioRateStep(var x1, x2: single);
begin
  x1 := x1 * GainLeft;
  x2 := x2 * GainRight;
end;

procedure TLucidityPanner.AudioRateProcess(x1, x2: PSingle; const SampleFrames: integer);
var
  c1 : integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    x1^ := x1^ * GainLeft;
    x2^ := x2^ * GainRight;
    inc(x1);
    inc(x2);
  end;
end;







end.
