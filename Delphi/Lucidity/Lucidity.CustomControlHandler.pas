unit Lucidity.CustomControlHandler;

interface

{$INCLUDE Defines.inc}

uses
  eePlugin,
  Classes,
  Contnrs,
  VamLib.UniqueID,
  VamLib.ZeroObject;

type
  TCustomControlHandler = class(TZeroObject)
  private
  protected
    Plugin : TeePlugin;
    ControlList : TObjectList;
    ThrottleHandle : TUniqueID;
    procedure UpdateControl(const c : TObject); virtual; abstract;
  public
    constructor Create(const aPlugin : TeePlugin); virtual;
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject); virtual;
    procedure DeregisterControl(const c : TObject); virtual;
    procedure UpdateAllControls;
  end;

implementation

{ TCustomControlHandler }

constructor TCustomControlHandler.Create(const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
  ThrottleHandle.Init;
end;

destructor TCustomControlHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TCustomControlHandler.RegisterControl(const c: TObject);
begin
  if ControlList.IndexOf(c) = -1 then ControlList.Add(c);
end;

procedure TCustomControlHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TCustomControlHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;

end.
