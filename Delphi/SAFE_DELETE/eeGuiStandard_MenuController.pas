unit eeGuiStandard_MenuController;

interface

{$TYPEINFO ON}

uses
  Classes, SysUtils, Generics.Collections, Controls, eeEnumHelper, eeGlobals,
  eeGuiStandard_MenuBuilder;

type
  EMenuControllerException = Exception;

  TShowMenu = (Always, ContextClickOnly);

  TMenuControlReference = record
    Control : TControl;
    ParIndex : integer;
    EnumHelper : TCustomEnumHelperClass;
    ShowMenu   : TShowMenu;
    PopupCallBack : TMenuCallback;
  end;

  TMenuControlReferenceList = class(TList<TMenuControlReference>)
  private
  public
    function FindReferenceTo(const aControl : TControl):integer;
  end;

  TMenuController = class
  private
    fGlobals: TGlobals;
    procedure SetGlobals(const Value: TGlobals);
  protected
    ControlReferenceList : TMenuControlReferenceList;
    MenuBuilder : TGuiMenuBuilder;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterControlAsMenuControl(const aControl : TControl; const ParIndex:integer; const EnumHelper:TCustomEnumHelperClass; const ShowMenu   : TShowMenu; PopupCallBack : TMenuCallback = nil);

    property Globals : TGlobals read fGlobals write SetGlobals;
  published

    //NOTE: do not call these methods. They are used by class for to handle control presses. Eventually
    // they will be moved to another (hidden) class so that application developers do not call them inadverntely.
    procedure HandleControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    //================
  end;

implementation

uses
  TypInfo, Dialogs;

{ TMenuControlReferenceList }

function TMenuControlReferenceList.FindReferenceTo(const aControl: TControl): integer;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if self.Items[c1].Control = aControl then
    begin
      result := c1;
      exit; //====================>>exit>>===============>>
    end;
  end;

  // No matching control found.
  result := -1;
end;

{ TMenuController }

constructor TMenuController.Create;
begin
  ControlReferenceList := TMenuControlReferenceList.Create;
  MenuBuilder := TGuiMenuBuilder.Create;
end;

destructor TMenuController.Destroy;
begin
  ControlReferenceList.Free;
  MenuBuilder.Free;
  inherited;
end;


procedure TMenuController.RegisterControlAsMenuControl(const aControl: TControl; const ParIndex: integer; const EnumHelper: TCustomEnumHelperClass; const ShowMenu : TShowMenu; PopupCallBack : TMenuCallback);
var
  ref:TMenuControlReference;
  m     : TMethod;
begin
  if ControlReferenceList.FindReferenceTo(aControl) <> -1 then raise EMenuControllerException.Create('A reference to this control has already been added.');

  ref.Control    := aControl;
  ref.ParIndex   := ParIndex;
  ref.EnumHelper := EnumHelper;
  ref.ShowMenu   := ShowMenu;
  ref.PopupCallBack := PopupCallBack;

  ControlReferenceList.Add(ref);

  aControl.Tag := ParIndex;

  // Asign mouse down event handler
  m.Data := Pointer(self);
  m.Code := Self.MethodAddress('HandleControlMouseDown');
  SetMethodProp(aControl, 'OnMouseDown', m);

  // Asign mouse up event handler
  m.Data := Pointer(self);
  m.Code := Self.MethodAddress('HandleControlMouseUp');
  SetMethodProp(aControl, 'OnMouseUp', m);

end;

procedure TMenuController.SetGlobals(const Value: TGlobals);
begin
  fGlobals := Value;

  MenuBuilder.Globals := Value;
end;

procedure TMenuController.HandleControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  refIndex : integer;
  ref : TMenuControlReference;
begin
  if (Sender is TControl) = false then raise EMenuControllerException.Create('Sender must be a TControl descendent.');

  refIndex := ControlReferenceList.FindReferenceTo((Sender as TControl));
  if refIndex = -1 then raise EMenuControllerException.Create('Reference to control not found.');

  ref := ControlReferenceList.Items[refIndex];


  //if (Button = mbLeft) and (ref.ShowMenu = TShowMenu.ContextClickOnly) then
  if (Button = mbRight) then
  begin
    MenuBuilder.IncrementEnumeratedVstParameter(ref.ParIndex, ref.EnumHelper);
  end;
end;

procedure TMenuController.HandleControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  refIndex : integer;
  ref : TMenuControlReference;
begin
  if (Sender is TControl) = false then raise EMenuControllerException.Create('Sender must be a TControl descendent.');

  refIndex := ControlReferenceList.FindReferenceTo((Sender as TControl));
  if refIndex = -1 then raise EMenuControllerException.Create('Reference to control not found.');

  ref := ControlReferenceList.Items[refIndex];

  //if (Button = mbLeft) or (ref.ShowMenu = TShowMenu.Always) then
  if (Button = mbLeft) then
  begin
    MenuBuilder.ShowMenuForVstParameter(Mouse.CursorPos.X, Mouse.CursorPos.Y, ref.ParIndex, ref.EnumHelper, ref.PopupCallBack);
  end;
end;




end.
