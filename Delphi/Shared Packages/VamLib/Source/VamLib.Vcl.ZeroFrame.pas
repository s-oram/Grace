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
    procedure AddMotherShipReference(aMotherShip : IMothership);
    procedure RemoveMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; DataA:Pointer; DataB:IInterface); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

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

procedure TZeroFrame.AddMotherShipReference(aMotherShip: IMothership);
begin
  if assigned(FMotherShip) then raise Exception.Create('MotherShip reference already set. Cannot assign again.');
  FMotherShip := aMotherShip;
end;

procedure TZeroFrame.RemoveMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := nil;
end;

end.

