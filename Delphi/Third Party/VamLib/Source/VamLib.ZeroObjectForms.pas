unit VamLib.ZeroObjectForms;

interface

uses
  Vcl.Forms,
  VamLib.ZeroObject;

type
  TZeroObjectFrame = class(TFrame, IZeroObject)
  protected
    FMotherShip : IMotherShip;
    procedure ClearMotherShipReference;
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); virtual;
  public
    destructor Destroy; override;

    procedure RegisterWithMotherShip(const Mothership:IMotherShip);
  end;

implementation

{ TZeroObjectFrame }

destructor TZeroObjectFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
  end;

  inherited;
end;

procedure TZeroObjectFrame.ClearMotherShipReference;
begin
  FMotherShip := nil;
end;

procedure TZeroObjectFrame.RegisterWithMotherShip(const Mothership: IMotherShip);
begin
  FMotherShip := MotherShip;
  FMotherShip.RegisterZeroObject(self);
end;

procedure TZeroObjectFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin

end;


end.
