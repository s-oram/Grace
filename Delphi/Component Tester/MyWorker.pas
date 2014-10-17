unit MyWorker;

interface

uses
  SysUtils;

type
  TWorker = class
  private
  public
    constructor Create;
    destructor Destroy; override;

    procedure Run(aTask : TProc);
  end;

implementation

{ TWorker }

constructor TWorker.Create;
begin

end;

destructor TWorker.Destroy;
begin

  inherited;
end;

procedure TWorker.Run(aTask: TProc);
begin
  aTask();
end;

end.
