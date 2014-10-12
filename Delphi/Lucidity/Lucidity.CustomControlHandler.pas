unit Lucidity.CustomControlHandler;

interface

uses
  eePlugin,
  Classes,
  Contnrs,
  VamLib.ZeroObject;

type
  TCustomControlHandler = class(TZeroObject)
  private
  public
    constructor Create(const aPlugin : TeePlugin); virtual;
    destructor Destroy; override;

  end;

implementation

{ TCustomControlHandler }

constructor TCustomControlHandler.Create(const aPlugin: TeePlugin);
begin

end;

destructor TCustomControlHandler.Destroy;
begin

  inherited;
end;

end.
