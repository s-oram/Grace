unit uLucidityVCA;

interface

{$INCLUDE Defines.inc}

uses
  MoreTypes, eeFunctions;

type
  TVCAModPoints = record
    ModInput_Gain : single;
  end;

  TLucidityVCA = class
  private
    fGain: single;
  protected
    ModPoints : TVCAModPoints;
    GainFactor : single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure FastControlProcess(const VcaEnvLevel : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateStep(var x1, x2 : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateProcess(x1, x2 : PSingle; const SampleFrames:integer); {$IFDEF AudioInline}inline;{$ENDIF}

    property Gain : single read fGain write fGain; //range 0..2
  end;

implementation

uses
  SysUtils;

{ TLucidityVoiceAmp }

constructor TLucidityVCA.Create;
begin

end;

destructor TLucidityVCA.Destroy;
begin

  inherited;
end;

function TLucidityVCA.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'ModInput_Gain' then Exit(@ModPoints.ModInput_Gain);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
  result := nil;
end;

procedure TLucidityVCA.FastControlProcess(const VcaEnvLevel : single);
var
  xg : single;
begin
  xg := Gain + ModPoints.ModInput_Gain;
  Clamp(xg, 0,1);
  GainFactor  := xg * VcaEnvLevel;
end;

procedure TLucidityVCA.AudioRateStep(var x1, x2: single);
begin
  x1 := x1 * GainFactor;
  x2 := x2 * GainFactor;
end;

procedure TLucidityVCA.AudioRateProcess(x1, x2: PSingle; const SampleFrames: integer);
var
  c1 : integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    x1^ := x1^ * GainFactor;
    x2^ := x2^ * GainFactor;
    inc(x1);
    inc(x2);
  end;
end;




end.
