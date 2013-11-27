unit eeVstParameter_OLD;

interface

uses
  Contnrs;

type
  TVstParameter = class
  private
    fName: string;
    fUnits: string;
    fValueText: string;
    fTag: integer;
    fShortName: string;
  public
    constructor Create;
	  destructor Destroy; override;

    property Name      :string  read fName      write fName;
    property ShortName :string  read fShortName write fShortName;
    property Units     :string  read fUnits     write fUnits;
    property ValueText :string  read fValueText write fValueText;
    property Tag       :integer read fTag       write fTag;
  end;

  TVstParameterList = class
  private
    function GetParName(Index: integer): string;
    function GetParameter(Index: integer): TVstParameter;
    procedure SetParamater(Index: integer; const Value: TVstParameter);
    function GetParUnits(Index: integer): string;
    function GetParValueText(Index: integer): string;
    function GetParTag(Index: integer): integer;
    function GetParShortName(Index: integer): string;
  protected
    ParList:TObjectList;
    property Par[Index:integer]:TVstParameter read GetParameter write SetParamater;
  public
    constructor Create;
	  destructor Destroy; override;

    function AddParameter(aPar:TVstParameter):integer;

    property ParName     [Index:integer] :string  read GetParName;
    property ParShortName[Index:integer] :string  read GetParShortName;
    property ParUnits    [Index:integer] :string  read GetParUnits;
    property ParValueText[Index:integer] :string  read GetParValueText;
    property ParTag      [Index:integer] :integer read GetParTag;
  end;

implementation

uses
  SysUtils, Classes;

{ TVstParameter }

constructor TVstParameter.Create;
begin
  Name := 'Parameter';
  Units := '';
  ValueText := '';
  Tag := 0;
end;

destructor TVstParameter.Destroy;
begin
  inherited;
end;

{ TVstParameterList }

constructor TVstParameterList.Create;
begin
  ParList := TObjectList.Create;
  ParList.OwnsObjects := true;
end;

destructor TVstParameterList.Destroy;
begin
  ParList.Free;
  inherited;
end;

function TVstParameterList.AddParameter(aPar: TVstParameter):integer;
begin
  result := ParList.Add(aPar);
end;

function TVstParameterList.GetParameter(Index: integer): TVstParameter;
begin
  assert(Index < ParList.Count);
  result := ParList[Index] as TVstParameter;
end;

procedure TVstParameterList.SetParamater(Index: integer;  const Value: TVstParameter);
begin
  assert(Index < ParList.Count);
  ParList[Index] := Value;
end;


function TVstParameterList.GetParName(Index: integer): string;
begin
  if Index >= ParList.Count
    then result := 'Par ' + IntToStr(Index)
    else result := Par[Index].Name;

end;


function TVstParameterList.GetParShortName(Index: integer): string;
begin
  if Index >= ParList.Count
    then result := 'Par ' + IntToStr(Index)
    else result := Par[Index].ShortName;

end;

function TVstParameterList.GetParTag(Index: integer): integer;
begin
  if Index >= ParList.Count
    then result := -1
    else result := Par[Index].Tag;

end;

function TVstParameterList.GetParUnits(Index: integer): string;
begin
  if Index >= ParList.Count
    then result := ''
    else result := Par[Index].Units;

end;

function TVstParameterList.GetParValueText(Index: integer): string;
begin
  if Index >= ParList.Count
    then result := ''
    else result := Par[Index].ValueText;

end;

end.
