unit eeGuiStandard_MenuBuilder;

interface

{$SCOPEDENUMS ON}

uses
  Menus, Controls, Classes,
  eeGlobals,
  eeTypes, eeMidiAutomation, eeMidiMap,
  eeGuiStandard_Types,
  eeEnumHelper;

type
  TMenuCallback = procedure (aMenu : TMenu) of object;

  TShowMenuState = record
    ParIndex : integer;
    EnumHelper : TCustomEnumHelperClass;
  end;

  TGuiMenuBuilder = class
  private
    fGlobals: TGlobals;
    procedure SetGlobals(const Value: TGlobals);
  protected
    Menu:TPopupMenu;
    ShowMenuState : TShowMenuState;

    procedure MenuItemClicked(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure IncrementEnumeratedVstParameter(const ParIndex:integer; const EnumHelper:TCustomEnumHelperClass);
    procedure ShowMenuForVstParameter(const x, y:integer; const ParIndex:integer; const EnumHelper:TCustomEnumHelperClass; PopupCallBack : TMenuCallback = nil);

    //The globals property must be set so that this class can get/set parameter values and MIDI parameter mappings.
    property Globals : TGlobals read fGlobals write SetGlobals;
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

procedure TGuiMenuBuilder.SetGlobals(const Value: TGlobals);
begin
  fGlobals := Value;
end;

procedure TGuiMenuBuilder.ShowMenuForVstParameter(const x, y, ParIndex: integer; const EnumHelper: TCustomEnumHelperClass; PopupCallBack : TMenuCallback);
var
  MenuItemCount : integer;
  c1: Integer;
  mi : TMenuItem;

  CurrentParValue : single;
  CurrentParValueAsInt : integer;
begin
  CurrentParValue      := Globals.VstParameters[ParIndex].ValueVST;
  CurrentParValueAsInt := EnumHelper.ToInteger(CurrentParValue);

  Menu.Items.Clear;

  MenuItemCount := EnumHelper.GetEnumTypeCount;

  for c1 := 0 to MenuItemCount-1 do
  begin
    mi := TMenuItem.Create(nil);

    mi.Caption := EnumHelper.ToFullGuiString(c1);
    mi.Name    := 'mi' + EnumHelper.ToString(c1);
    mi.Tag     := c1;
    mi.OnClick := MenuItemClicked;

    if c1 = CurrentParValueAsInt then mi.Checked := true;

    Menu.Items.Add(mi);
  end;

  ShowMenuState.ParIndex   := ParIndex;
  ShowMenuState.EnumHelper := EnumHelper;

  if assigned(PopupCallback)
    then PopupCallback(Menu);


  Menu.Popup(x, y);
end;

procedure TGuiMenuBuilder.MenuItemClicked(Sender: TObject);
var
  mi : TMenuItem;
  ParIndex : integer;
  ParValue : integer;
  PublishedParameterIndex : integer;
begin
  assert((Sender is TMenuItem), 'Sender must be of TMenuItem type.');

  ParIndex := ShowMenuState.ParIndex;

  mi := (Sender as TMenuItem);
  ParValue := mi.Tag;

  if Globals.VstParameters[ParIndex].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters[ParIndex].PublishedVSTParameterIndex;
    Globals.VstMethods^.BeginParameterEdit(PublishedParameterIndex);
    Globals.VstMethods^.SetParameterAutomated(PublishedParameterIndex, ShowMenuState.EnumHelper.ToSingle(ParValue));
    Globals.VstMethods^.EndParameterEdit(PublishedParameterIndex);
  end else
  begin
    Globals.VstParameters[ParIndex].ValueVST := ShowMenuState.EnumHelper.ToSingle(ParValue);
  end;
end;

procedure TGuiMenuBuilder.IncrementEnumeratedVstParameter(const ParIndex: integer; const EnumHelper: TCustomEnumHelperClass);
var
  ParValueAsVstFloat : single;
  ParValueAsInt      : integer;
  PublishedParameterIndex : integer;
begin
  ParValueAsVstFloat := Globals.VstParameters[ParIndex].ValueVST;

  ParValueAsInt := EnumHelper.ToInteger(ParValueAsVstFloat);
  inc(ParValueAsInt);
  if ParValueAsInt >= EnumHelper.GetEnumTypeCount then ParValueAsInt := 0;

  ParValueAsVstFloat := EnumHelper.ToSingle(ParValueAsInt);

  if Globals.VstParameters[ParIndex].IsPublished then
  begin
    PublishedParameterIndex := Globals.VstParameters[ParIndex].PublishedVSTParameterIndex;
    Globals.VstMethods^.BeginParameterEdit(PublishedParameterIndex);
    Globals.VstMethods^.SetParameterAutomated(PublishedParameterIndex, ParValueAsVstFloat);
    Globals.VstMethods^.EndParameterEdit(PublishedParameterIndex);
  end else
  begin
    Globals.VstParameters[ParIndex].ValueVST := ParValueAsVstFloat;
  end;
end;





end.
