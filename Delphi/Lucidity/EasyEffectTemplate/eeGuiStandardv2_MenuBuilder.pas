unit eeGuiStandardv2_MenuBuilder;

interface

{$SCOPEDENUMS ON}

uses
  Menus, Controls, Classes,
  eeGlobals,
  eeTypes, eeMidiAutomation, eeMidiMap,
  eeGuiStandard_Types,
  eeEnumHelper;

type
  TMenuItemSelectedCallback = reference to procedure(SelectedItemIndex : integer);
  TShowMenuCallback         = reference to procedure(aMenu : TMenu);

  TShowMenuState = record
    ItemSelectedCallback : TMenuItemSelectedCallback;
  end;

  TGuiMenuBuilder = class
  private
  protected
    Menu:TPopupMenu;
    ShowMenuState : TShowMenuState;
    procedure MenuItemClicked(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowMenuForVstParameter(SelectedCallback : TMenuItemSelectedCallback; const x, y:integer; const CurrentValueIndex:integer; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TShowMenuCallback);
  end;


implementation

uses
  SysUtils, TypInfo, Rtti;

{ TGuiMenuBuilder }

constructor TGuiMenuBuilder.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TGuiMenuBuilder.Destroy;
begin
  Menu.Items.Clear;
  FreeAndNil(Menu);
  inherited;
end;

procedure TGuiMenuBuilder.ShowMenuForVstParameter(SelectedCallback : TMenuItemSelectedCallback; const x, y:integer; const CurrentValueIndex:integer; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TShowMenuCallback);
var
  MenuItemCount : integer;
  c1: Integer;
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  MenuItemCount := EnumHelper.GetEnumTypeCount;

  for c1 := 0 to MenuItemCount-1 do
  begin
    mi := TMenuItem.Create(nil);

    mi.Caption := EnumHelper.ToFullGuiString(c1);
    mi.Name    := 'mi' + EnumHelper.ToUnicodeString(c1);
    mi.Tag     := c1;
    mi.OnClick := MenuItemClicked;

    if c1 = CurrentValueIndex then mi.Checked := true;

    Menu.Items.Add(mi);
  end;

  ShowMenuState.ItemSelectedCallback := SelectedCallback;

  if assigned(PopupCallback)
    then PopupCallback(Menu);

  Menu.Popup(x, y);
end;

procedure TGuiMenuBuilder.MenuItemClicked(Sender: TObject);
var
  Tag : integer;
begin
  assert((Sender is TMenuItem), 'Sender must be of TMenuItem type.');

  if assigned(ShowMenuState.ItemSelectedCallback) then
  begin
    Tag := (Sender as TMenuItem).Tag;
    ShowMenuState.ItemSelectedCallback(Tag);
    ShowMenuState.ItemSelectedCallback := nil;
  end;
end;



end.
