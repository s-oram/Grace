unit Helm.Sync;

interface

type
  TLock = class
  private
  public
    procedure Enter;
    procedure Leave;
  end;

implementation

{ THelmLock }

procedure TLock.Enter;
begin
  TMonitor.Enter(self);
end;

procedure TLock.Leave;
begin
  TMonitor.Exit(self);
end;

end.
