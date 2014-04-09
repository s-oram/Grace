unit soLevelMeter;

interface

uses
  VamLib.MoreTypes,
  VamLib.ZeroObject;

type
  TLevelMonitor = class(TZeroObject)
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure Process(InputA, InputB : PSingle; const SampleFrames : integer);
  end;

implementation

{ TLevelMonitor }

constructor TLevelMonitor.Create;
begin

end;

destructor TLevelMonitor.Destroy;
begin

  inherited;
end;

procedure TLevelMonitor.Process(InputA, InputB: PSingle; const SampleFrames: integer);
begin

end;

end.
