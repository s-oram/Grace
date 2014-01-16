unit eeGlobals;

interface

uses
  eeCustomGlobals;

type
  TGlobals = class(TCustomGlobals)
  private
  protected
  public
    constructor Create; override;
	  destructor Destroy; override;
  end;

implementation

{ TGlobals }

constructor TGlobals.Create;
begin
  inherited;

end;

destructor TGlobals.Destroy;
begin

  inherited;
end;

end.






