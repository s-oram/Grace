unit VamLib.Utils.ObjectEnumerator;

interface

// Fun with enumerators
// http://www.thedelphigeek.com/2007/03/fun-with-enumerators-part-6-generators.html

{
  I built this object enumerator. It converts an array of objects into an enumerator
  that can be used in "for-in" loop constructs. It would
  be more efficient to use the ObjectArray() method in VamLib.Utils.pas
  However I'll leave this code here as proof-of-concept. It might come in
  handy if I need to implement an enumerator for another situation.
}

type
  IObjectEnumerator = interface
    function GetEnumerator : IObjectEnumerator;
    function GetCurrent : TObject;
    function MoveNext   : boolean;
    property Current : TObject read GetCurrent;
  end;

  // Use ObjectEnumerator() to wrap an enumerator around arrays for "for-in" loops.
  function ObjectEnumerator(const objs : array of TObject):IObjectEnumerator;

//============= Private - Do not use ========================================
type
  TObjectEnumerator = class(TInterfacedObject, IObjectEnumerator)
  private
    fObjs : array of TObject;
    fObjCount : integer;
    fIndex    : integer;
  protected
    function GetCurrent : TObject;
    function MoveNext   : boolean;
    property Current : TObject read GetCurrent;
    function GetEnumerator : IObjectEnumerator;
  public
    constructor Create(const objs : array of TObject);
    destructor Destroy; override;
  end;




implementation

{ TObjectEnumerator }

constructor TObjectEnumerator.Create(const objs: array of TObject);
var
  c1: Integer;
begin
  fObjCount := Length(Objs);
  SetLength(fObjs, fObjCount);
  for c1 := 0 to fObjCount-1 do
  begin
    fObjs[c1] := objs[c1];
  end;
  fIndex := -1;
end;

destructor TObjectEnumerator.Destroy;
begin
  SetLength(fObjs, 0);
  inherited;
end;

function TObjectEnumerator.GetCurrent: TObject;
begin
  result := fObjs[fIndex];
end;

function TObjectEnumerator.GetEnumerator: IObjectEnumerator;
begin
  result := self;
end;

function TObjectEnumerator.MoveNext: boolean;
begin
  if fIndex < fObjCount-1 then
  begin
    inc(fIndex);
    result := true;
  end else
  begin
    result := false;
  end;
end;


function ObjectEnumerator(const objs : array of TObject):IObjectEnumerator;
begin
  {
  ==== Object Enumerator Example ====
  for Obj in ObjectEnumerator([MyObject1, MyObject2, MyObject3]) do
  begin
    (Obj as TControl).Height := 20;
    (Obj as TControl).Width := 100;
  end;
  }
  result := TObjectEnumerator.Create(objs);
end;



end.
