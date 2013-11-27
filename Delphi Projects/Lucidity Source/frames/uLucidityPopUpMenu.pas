unit uLucidityPopUpMenu;

interface

uses
  eePlugin, Vcl.Menus;

type
  TLucidityPopupMenu = class
  private
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer); virtual; abstract;
  end;

implementation

{ TLucidityPopupMenu }

constructor TLucidityPopupMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TLucidityPopupMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TLucidityPopupMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

end.
