unit eeVstParameterList;

interface

uses
  Contnrs,
  eeVstParameter;

type
  TVstParameterList = class
  private
    function GetPar(Index: integer): TVstParameter;
    function GetOwnsObjects: boolean;
    procedure SetOwnsObjects(const Value: boolean);
  protected
    fParList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function Par(aName:string):TVstParameter;

    function NewParameter(Name : string):TVstParameter;
    procedure Add(aPar : TVstParameter);
    procedure Remove(aPar : TVstParameter);
    function Count : integer;

    function FindParameterIndexByName(aName : string):integer;
    property Parameter[Index:integer]:TVstParameter read GetPar; default;

    property OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
  end;

implementation

uses
  SysUtils;

{ TVstParameterList }

constructor TVstParameterList.Create;
begin
  fParList := TObjectList.Create;
  fParList.OwnsObjects := false;
end;

destructor TVstParameterList.Destroy;
begin
  fParList.Free;
  inherited;
end;

function TVstParameterList.Count: integer;
begin
  result := fParList.Count;
end;

function TVstParameterList.FindParameterIndexByName(aName: string): integer;
var
  c1: Integer;
begin
  for c1 := 0 to fParList.Count-1 do
  begin
    if SameText((fParList[c1] as TVstParameter).Name, aName) then
    begin
      result := c1;
      exit; //==================>> exit >>=============>>
    end;
  end;

  // If we've made it this far a matching parameter hasn't been found.
  result := -1;
end;

function TVstParameterList.GetOwnsObjects: boolean;
begin
  result := fParList.OwnsObjects;
end;

function TVstParameterList.GetPar(Index: integer): TVstParameter;
begin
  result := fParList[Index] as TVstParameter;
end;

function TVstParameterList.NewParameter(Name: string): TVstParameter;
var
  aPar : TVstParameter;
begin
  aPar := TVstParameter.Create(Name);
  fParList.Add(aPar);
  result := aPar;
end;

function TVstParameterList.Par(aName: string): TVstParameter;
var
  c1: Integer;
begin
  for c1 := 0 to fParList.Count-1 do
  begin
    if SameText((fParList[c1] as TVstParameter).Name, aName) then
    begin
      result := fParList[c1] as TVstParameter;
      exit; //==================>> exit >>=============>>
    end;
  end;

  // If we've made it this far a matching parameter hasn't been found.
  raise Exception.Create('Parameter not found. (Name = ' + aName + ')');
end;

procedure TVstParameterList.Remove(aPar: TVstParameter);
begin
  fParList.Remove(aPar);
end;

procedure TVstParameterList.SetOwnsObjects(const Value: boolean);
begin
  fParList.OwnsObjects := Value;
end;

procedure TVstParameterList.Add(aPar: TVstParameter);
begin
  fParList.Add(aPar)
end;





end.
