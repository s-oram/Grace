unit uDialogDisplayArea;

interface

uses
  SysUtils, Classes, Controls;

type
  TWinControlEvent = function(Sender : TObject):TWinControl of object;

  TDialogDisplayArea = class
  private
    fOnShowDialogArea: TNotifyEvent;
    fOnHideDialogArea: TNotifyEvent;
    fOnGetDisplayArea: TWinControlEvent;

    CloseCallback : TProc;
    fAllowClose : boolean;

  public
    constructor Create;
    destructor Destroy; override;

    function GetDisplayArea : TWinControl;

    procedure Show(const aAllowClose : boolean; aCloseCallback : TProc);
    procedure Hide;

    property AllowClose : boolean read fAllowClose;

    property OnGetDisplayArea : TWinControlEvent read fOnGetDisplayArea write fOnGetDisplayArea;
    property OnShowDialogArea : TNotifyEvent     read fOnShowDialogArea write fOnShowDialogArea;
    property OnHideDialogArea : TNotifyEvent     read fOnHideDialogArea write fOnHideDialogArea;
  end;

implementation

{ TDialogDislayArea }

constructor TDialogDisplayArea.Create;
begin

end;

destructor TDialogDisplayArea.Destroy;
begin
  CloseCallback := nil;
  inherited;
end;

function TDialogDisplayArea.GetDisplayArea: TWinControl;
begin
  if assigned(OnGetDisplayArea)
    then result := OnGetDisplayArea(self)
    else result := nil;
end;

procedure TDialogDisplayArea.Show(const aAllowClose : boolean; aCloseCallback : TProc);
begin
  fAllowClose   := aAllowClose;
  CloseCallBack := aCloseCallback;

  if assigned(OnShowDialogArea)
    then OnShowDialogArea(self);
end;

procedure TDialogDisplayArea.Hide;
begin
  if assigned(OnHideDialogArea)
    then OnHideDialogArea(self);

  if assigned(CloseCallback) then
  begin
    CloseCallback;
    CloseCallback := nil;
  end;


end;



end.
