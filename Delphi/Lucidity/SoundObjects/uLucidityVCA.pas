unit uLucidityVCA;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes;

type
  TVCAModPoints = record
    ModInput_Gain : single;
  end;


  PVcaConfigData = ^TVcaConfigData;
  TVcaConfigData = record
    GainPar : PSingle;
    GainMod : PSingle;
    PanPar  : PSingle;
    PanMod  : PSingle;
  end;

  TLucidityVCA = class
  private
    function GetConfig: PVcaConfigData;
  protected
    ModPoints : TVCAModPoints;
    GainFactor : single;
    GainLeft, GainRight : single;
    fConfig : TVcaConfigData;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure FastControlProcess(const VcaEnvLevel : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateStep(var x1, x2 : single); {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateProcess(x1, x2 : PSingle; const SampleFrames:integer); {$IFDEF AudioInline}inline;{$ENDIF}

    property Config : PVcaConfigData read GetConfig;
  end;

implementation

uses
  VamLib.Utils,
  SysUtils;

{ TLucidityVoiceAmp }

constructor TLucidityVCA.Create;
begin

end;

destructor TLucidityVCA.Destroy;
begin

  inherited;
end;

function TLucidityVCA.GetConfig: PVcaConfigData;
begin
  result := @fConfig;
end;

function TLucidityVCA.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'ModInput_Gain' then Exit(@ModPoints.ModInput_Gain);

  //result := nil;
  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TLucidityVCA.FastControlProcess(const VcaEnvLevel : single);
var
  xg : single;
  PanPos : single;
  x1, x2 : single;
begin
  //xg := Gain + ModPoints.ModInput_Gain;
  xg := Config.GainPar^ + Config.GainMod^;
  xg := Clamp(xg, 0,1);
  GainFactor  := xg * VcaEnvLevel;

  x1 := Config.PanPar^;
  x2 := Config.PanMod^;

  PanPos := x1 + x2;
  PanPos := Clamp(PanPos, 0,1);

  //TODO: Optimise away this sqrt() call.
  GainLeft  := Sqrt(1 - PanPos) * GainFactor;
  GainRight := Sqrt(PanPos)     * GainFactor;
end;

procedure TLucidityVCA.AudioRateStep(var x1, x2: single);
begin
  x1 := x1 * GainLeft;
  x2 := x2 * GainRight;
end;

procedure TLucidityVCA.AudioRateProcess(x1, x2: PSingle; const SampleFrames: integer);
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
