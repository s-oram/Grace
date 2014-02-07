unit VamQuery;

interface

uses
  Generics.Collections, Controls,
  RedFoxWinControl, RedFoxGraphicControl;

type
  TVamQuery = class;
  TControlList = class;


  IVamQuery = interface
    ['{5D588FAE-5874-4FFD-B9CA-F54AF6FB2C39}']

    function List : TControlList;
  end;

  // VamQuery will work with controls that have a "DisplayClass" property.
  // A "DisplayClass" property type is "string".
  // A control's DisplayClass can contain any number of display classes, for example
  //   - MyControl.DisplayClass = "BigKnob MainControl";
  // Each display class needs to be separated by a space.
  // It works similarly to html elements with CSS class type tag.


  // TDisplayClass wraps a DisplayClass string and provides methods for
  // working with display class strings.
  TDisplayClass = record
  private
    fDisplayClassString : string;
  public
    class operator Implicit(a : string) : TDisplayClass;
    class operator Implicit(a : TDisplayClass) : string;

    function Contains(DisplayClassName : string):boolean;
    procedure Clear;
  end;

  TControlList = class(TList<TControl>);

  TVamQuery = class(TInterfacedObject, IVamQuery)
  private
    fList: TControlList;
  public
    constructor Create;
    destructor Destroy;

    function List : TControlList;


  end;

  TControlProcedure = reference to procedure(c:TControl);

procedure ForAllChildControls(const Parent : TControl; Action : TControlProcedure);

function VamQueryRequest(const Parent : TControl; ClassType : TClass):IVamQuery; overload;
function VamQueryRequest(const Parent : TControl; DisplayClass : TDisplayClass):IVamQuery; overload;

function HasDisplayClass(const aControl : TObject; const DisplayClassName : string):boolean;

procedure AddDisplayClass(const aControl : TRedFoxWinControl; const DisplayClass : string);
procedure RemoveDisplayClass(const aControl : TRedFoxWinControl; const DisplayClass : string);


function FindControlByName(Parent : TControl; ChildName : string):TControl;


implementation

uses
  SysUtils,
  TypInfo;

procedure ForAllChildControls(const Parent : TControl; Action : TControlProcedure);
var
  c1 : integer;
  wc : TWinControl;
begin
  if (Parent is TWinControl) then
  begin
    wc := (Parent as TWinControl);
    for c1 := 0 to wc.ControlCount-1 do
    begin
      Action(wc.Controls[c1]);
      ForAllChildControls(wc.Controls[c1], Action);
    end;
  end;
end;

function VamQueryRequest(const Parent : TControl; ClassType : TClass):IVamQuery;
var
  vq : IVamQuery;
  Action : TControlProcedure;
begin
  vq := TVamQuery.Create;

  Action := procedure(c:TControl)
  begin
    if (c is ClassType)
      then vq.List.Add(c);
  end;

  ForAllChildControls(Parent, Action);

  result := vq;
end;

function VamQueryRequest(const Parent : TControl; DisplayClass : TDisplayClass):IVamQuery; overload;
var
  vq : IVamQuery;
  Action : TControlProcedure;
  ControlDisplayClass : TDisplayClass;
begin
  vq := TVamQuery.Create;

  Action := procedure(c:TControl)
  begin
    if (c is TRedFoxWinControl) or (c is TRedFoxGraphicControl) then
    begin
      ControlDisplayClass := GetPropValue(c, 'DisplayClass');
      if (ControlDisplayClass <> '') and (DisplayClass <> '') and (ControlDisplayClass.Contains(DisplayClass)) then
      begin
        vq.List.Add(c);
      end;
    end;
  end;

  ForAllChildControls(Parent, Action);

  result := vq;
end;

{ TVamQuery }

constructor TVamQuery.Create;
begin
  fList := TControlList.Create;
end;

destructor TVamQuery.Destroy;
begin
  fList.Free;
end;

function TVamQuery.List: TControlList;
begin
  result := fList;
end;

{ TDisplayClass }

class operator TDisplayClass.Implicit(a: string): TDisplayClass;
begin
  result.fDisplayClassString := a;
end;

procedure TDisplayClass.Clear;
begin
  self.fDisplayClassString := '';
end;

function TDisplayClass.Contains(DisplayClassName: string): boolean;
begin
  if (Pos(DisplayClassName, self.fDisplayClassString) <> 0 )
    then result := true
    else result := false;
end;

class operator TDisplayClass.Implicit(a: TDisplayClass): string;
begin
  result := a.fDisplayClassString;
end;

function HasDisplayClass(const aControl : TObject; const DisplayClassName : string):boolean;
var
  s : string;
begin
  result := false;
  try
    s := GetPropValue(aControl, 'DisplayClass');
    if (DisplayClassName <> '') and (s <> '') and (Pos(DisplayClassName, s) <> 0 )
      then result := true
      else result := false;
  except
    on EPropertyError do ; //will be raise if the 'DisplayClass property doens't exist.
    else raise;
  end;
end;

procedure AddDisplayClass(const aControl : TRedFoxWinControl; const DisplayClass : string);
begin
  if (Pos(DisplayClass, aControl.DisplayClass) = 0 ) then
  begin
    aControl.DisplayClass := aControl.DisplayClass + ' ' + DisplayClass;
  end;
end;

procedure RemoveDisplayClass(const aControl : TRedFoxWinControl; const DisplayClass : string);
var
  Text : string;
begin
  if (Pos(DisplayClass, aControl.DisplayClass) <> 0 ) then
  begin
    Text := aControl.DisplayClass;
    Text := StringReplace(Text, DisplayClass, '', [rfReplaceAll, rfIgnoreCase]);
    Text := StringReplace(Text, '  ', '', [rfReplaceAll, rfIgnoreCase]);
    aControl.DisplayClass := Text;
  end;
end;



function FindControlByName(Parent : TControl; ChildName : string):TControl;
var
  c1 : integer;
  wc : TWinControl;
  c : TControl;
begin
  assert(assigned(Parent));

  if SameText(ChildName, Parent.Name) then
  begin
    result := Parent;
  end else
  if (Parent is TWinControl) and ((Parent as TWinControl).ControlCount > 0) then
  begin
    wc := (Parent as TWinControl);
    for c1 := 0 to wc.ControlCount-1 do
    begin
      if SameText(ChildName, wc.Controls[c1].Name)
        then exit(wc.Controls[c1]);
    end;

    for c1 := 0 to wc.ControlCount-1 do
    begin
      c := FindControlByName(wc.Controls[c1], ChildName); // <<<----recursion here----<<<
      if assigned(c)
        then exit(c);
    end;
  end;


  // if we've made it this far no matching control has been found.
  result := nil;
end;

end.

