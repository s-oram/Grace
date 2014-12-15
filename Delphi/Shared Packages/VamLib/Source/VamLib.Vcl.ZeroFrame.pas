unit VamLib.Vcl.ZeroFrame;

interface

uses
  Controls, Classes,
  VamLib.ZeroObject,
  Vcl.Forms;

type
  TZeroFrame = class(TFrame, IZeroObject)
  protected
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TZeroFrame }

constructor TZeroFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TZeroFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  inherited;
end;

procedure TZeroFrame.ProcessZeroObjectMessage(MsgID: cardinal; DataA: Pointer; DataB: IInterface);
begin

end;

procedure TZeroFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;



end.

