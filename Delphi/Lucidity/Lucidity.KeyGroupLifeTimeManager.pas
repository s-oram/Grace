unit Lucidity.KeyGroupLifeTimeManager;

interface

uses
  Classes,
  Lucidity.Interfaces,
  VamLib.ZeroObject;

type
  TKeyGroupLifeTimeManager = class(TZeroObject)
  private
    KeyGroupList : TInterfaceList;
  public
    constructor Create;
    destructor Destroy; override;


  end;

implementation

{ TKeyGroupLifeTimeManager }

constructor TKeyGroupLifeTimeManager.Create;
begin

end;

destructor TKeyGroupLifeTimeManager.Destroy;
begin

  inherited;
end;

end.
