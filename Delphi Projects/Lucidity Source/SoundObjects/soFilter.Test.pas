unit soFilter.Test;

interface

type
  TTestFilter = class
  private
    fVolume: single;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Step(var x1, x2 : single);

    property Volume : single read fVolume write fVolume;

  end;

implementation

{ TTestFilter }

constructor TTestFilter.Create;
begin
  fVolume := 1;
end;

destructor TTestFilter.Destroy;
begin

  inherited;
end;

procedure TTestFilter.Step(var x1, x2: single);
begin
  x1 := x1 * fVolume;
  x2 := x2 * fVolume;
end;

end.
