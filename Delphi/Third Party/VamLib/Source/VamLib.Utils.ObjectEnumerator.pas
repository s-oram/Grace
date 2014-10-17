unit VamLib.Utils.ObjectEnumerator;

interface

// Fun with enumerators
// http://www.thedelphigeek.com/2007/03/fun-with-enumerators-part-6-generators.html

type
  IObjectEnumerator = interface
    function GetEnumerator : IObjectEnumerator;
    function GetCurrent : TObject;
    function MoveNext   : boolean;
    property Current : TObject read GetCurrent;
  end;

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


end.
