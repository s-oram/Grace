unit SynthModule_Custom;

interface

type
  TCustomSynthModule = class;

  PSynthModuleOutput = ^TSynthModuleOutput;
  TSynthModuleOutput = record
    Value : single;
  end;

  PSynthModuleInput = ^TSynthModuleInput;
  TSynthModuleInput = record
    Value : PSingle;
    procedure ConnectTo(Output : PSynthModuleOutput);
    procedure Disconnect;
  end;

  TCustomSynthModule = class
  private
    fAudioRate: single;
    fControlRate: single;
  protected
    procedure SetAudioRate(const Value: single); virtual;
    procedure SetControlRate(const Value: single); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DisconnectAllInputs; virtual; abstract;

    property ControlRate : single read fControlRate write SetControlRate;  //ControlRate
    property AudioRate   : single read fAudioRate   write SetAudioRate;    //SampleRate



    //procedure ControlRateStep; inline;
    //procedure AudioRateStep;   inline;
  end;

implementation

uses
  SysUtils;

var
  GlobalDisconnectedValue : single;

{ TSynthModuleInput }

procedure TSynthModuleInput.ConnectTo(Output: PSynthModuleOutput);
begin
  Value := @(Output^.Value);
end;

procedure TSynthModuleInput.Disconnect;
begin
  self.Value := @GlobalDisconnectedValue;
end;

{ TCustomSynthModule }

constructor TCustomSynthModule.Create;
begin
  fAudioRate   := 44100;
  fControlRate := 44100;
end;

destructor TCustomSynthModule.Destroy;
begin

  inherited;
end;

procedure TCustomSynthModule.SetAudioRate(const Value: single);
begin
  fAudioRate := Value;
end;

procedure TCustomSynthModule.SetControlRate(const Value: single);
begin
  fControlRate := Value;
end;



initialization
  GlobalDisconnectedValue := 0;

finalization

end.
