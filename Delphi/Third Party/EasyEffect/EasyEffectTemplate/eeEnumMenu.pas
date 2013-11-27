unit eeEnumMenu;

interface

uses
  Vcl.Menus, eeMenus, eeEnumHelper;

type
  TCustomEnumMenu = class
  private
    fOnMenuClose: TMenuCloseEvent;
  protected
    fMenu : TPopUpMenuEx;

    procedure MenuClose(Sender: TObject; Cancelled: Boolean);
    procedure MenuItemClicked(Sender : TObject); virtual; abstract;

    procedure InitMenu<TEnum>(const aEnumHelper:TCustomEnumHelperClass);
    procedure PopUp_Generic<TEnum>(X, Y : integer; CurrentValue:TEnum);

    property Menu : TPopUpMenuEx read fMenu;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function FindMenuItemByName(Name : string):TMenuItem;

    procedure PopUp(X, Y : integer); overload;

    property OnClose : TMenuCloseEvent read fOnMenuClose write fOnMenuClose;
  end;

  type
     TEnumValueEvent<TEnum> = procedure(Sender : TObject; Value:TEnum) of object;

  TEnumMenu<TEnum> = class(TCustomEnumMenu)
  private
    fOnItemSelected: TEnumValueEvent<TEnum>;
  protected
    EnumHelper : TCustomEnumHelperClass;
    procedure MenuItemClicked(Sender : TObject); override;
  public
    constructor Create(const aEnumHelper:TCustomEnumHelperClass); reintroduce;



    procedure Init;

    procedure PopUp(X, Y : integer; CurrentValue:TEnum); overload;

    property Menu;

    property OnItemSelected : TEnumValueEvent<TEnum> read fOnItemSelected write fOnItemSelected;
  end;

implementation

uses
  SysUtils;

{ TCustomEnumMenu }

constructor TCustomEnumMenu.Create;
begin
  fMenu := TPopUpMenuEx.Create(nil);
  fMenu.OnClosed := MenuClose;
end;

destructor TCustomEnumMenu.Destroy;
begin
  fMenu.Free;
  inherited;
end;

function TCustomEnumMenu.FindMenuItemByName(Name: string): TMenuItem;
var
  c1: Integer;
  Text : string;
begin
  Text := 'mi' + Name;

  for c1 := 0 to fMenu.Items.Count-1 do
  begin
    if sameText(fMenu.Items[c1].Name, Text) then
    begin
      exit(fMenu.Items[c1]);
    end;
  end;

  // if we've made it this far no item has been found.
  result := nil;
end;

procedure TCustomEnumMenu.InitMenu<TEnum>(const aEnumHelper:TCustomEnumHelperClass);
var
  c1 : integer;
  EnumTypeCount : integer;
  mi : TMenuItem;
begin
  fMenu.Items.Clear;

  EnumTypeCount := TEnumHelper<TEnum>.GetEnumTypeCount;

  for c1 := 0 to EnumTypeCount-1 do
  begin
    mi := TMenuItem.Create(fMenu);
    mi.Tag := c1;
    mi.Caption := aEnumHelper.ToFullGuiString(c1);
    mi.Name    := 'mi' + aEnumHelper.ToString(c1);
    mi.OnClick := MenuItemClicked;

    fMenu.Items.Add(mi);
  end;
end;

procedure TCustomEnumMenu.MenuClose(Sender: TObject; Cancelled: Boolean);
begin
  if assigned(OnClose) then OnClose(self, Cancelled);
end;

procedure TCustomEnumMenu.PopUp(X, Y: integer);
var
  c1: Integer;
begin
  for c1 := 0 to fMenu.Items.Count-1 do
  begin
    fMenu.Items[c1].Checked := false;
  end;

  fMenu.Popup(X, Y);
end;

procedure TCustomEnumMenu.PopUp_Generic<TEnum>(X, Y: integer; CurrentValue: TEnum);
var
  c1: Integer;
  s : string;
begin
  for c1 := 0 to fMenu.Items.Count-1 do
  begin
    fMenu.Items[c1].Checked := false;
  end;

  s := TEnumHelper<TEnum>.ToString(CurrentValue);
  s := 'mi' + s;

  for c1 := 0 to fMenu.Items.Count-1 do
  begin
    if fMenu.Items[c1].Name = s
      then fMenu.Items[c1].Checked := true;
  end;

  fMenu.Popup(X, Y);
end;







{ TEnumMenu<TEnum> }

constructor TEnumMenu<TEnum>.Create(const aEnumHelper:TCustomEnumHelperClass);
begin
  inherited Create;

  EnumHelper := aEnumHelper;

  //finally
  Init;
end;

procedure TEnumMenu<TEnum>.Init;
begin
  InitMenu<TEnum>(EnumHelper);
end;

procedure TEnumMenu<TEnum>.MenuItemClicked(Sender: TObject);
var
  aEnumValue : TEnum;
  Tag : integer;
begin
  inherited;

  Tag := (Sender as TMenuItem).Tag;

  aEnumValue := TEnumHelper<TEnum>.ToEnum(Tag);

  if assigned(OnItemSelected) then OnItemSelected(Self, aEnumValue);
end;

procedure TEnumMenu<TEnum>.PopUp(X, Y: integer; CurrentValue: TEnum);
begin
  PopUp_Generic<TEnum>(X, Y, CurrentValue);
end;

end.
