unit Lucidity.KeyGroupPlayer;

interface

uses
  VamLib.MoreTypes,
  VamLib.ZeroObject;

type
  TKeyGroupPlayer = class(TZeroObject)
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure AudioProcess(const Outputs:TArrayOfPSingle; const SampleFrames : integer); inline;
    procedure FastControlProcess; inline;
    procedure SlowControlProcess; inline;
  end;

implementation

{ TKeyGroupPlayer }

constructor TKeyGroupPlayer.Create;
begin

end;

destructor TKeyGroupPlayer.Destroy;
begin

  inherited;
end;

procedure TKeyGroupPlayer.FastControlProcess;
begin

end;

procedure TKeyGroupPlayer.SlowControlProcess;
begin

end;

procedure TKeyGroupPlayer.AudioProcess(const Outputs: TArrayOfPSingle; const SampleFrames: integer);
begin

end;



end.
