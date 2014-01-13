unit eeVstParameterManager;

interface

uses
  Contnrs,
  eeVstParameter;

type
  TVstParameterManager = class
  private
    function GetPar(Index: integer): TVstParameter;
    function GetPublishedParameterCount: integer;
  protected type
    // TVstParameterHack class gives us access to the internals of the TVstParameter
    // class. Normally this indicates a design flaw. In this case we are making an exception
    // because PublishedVstParameterIndex should only be updated by this class, not in any other
    // part of the application. Making the field private prevents other parts of
    // the application from inadvertently change the PublishedVstParameterIndex, whilst
    // the this hack gives us the access required to update the field.
    TVstParameterHack = class(TVstParameter);

  protected var
    //fParList includes *all* parameters. Private and published.
    fParList : TObjectList;

    // Published parameters are also included in the fPublished parameter list.
    fPublishedParList : TObjectList;
  public
    constructor Create;
    destructor Destroy; override;

    function NewParameter(Name : string):TVstParameter;
    function Add(aPar : TVstParameter):TVstParameter;

    function FindParameter(aName:string):TVstParameter;
    function FindParameterIndex(aPar : TVstParameter):integer;
    function FindParameterIndexByName(aName : string):integer;
    property Parameter[Index:integer]:TVstParameter read GetPar; default;

    function Count : integer;



    // BuildPublishedParameterInfo should be called after all
    // VST Parameters are added to the manager.
    procedure BuildPublishedParameterInfo;

    property PublishedParameterCount : integer read GetPublishedParameterCount;
    function PublishedParameter(Index : integer): TVstParameter;

    // NOTE: The whole published VST parameter scheme is a little bit odd I think.
    // I think I should have another go at implementing something a bit more straight forward.
    // It's odd because the VstParameterManager does the 'publishing' but
    // a parameters 'published' state can be checked via the TVstParameter code.
    // I think it would make more sense if the TVstParameter class didn't know
    // if it was published or not (as it doesn't do the publishing). Or the VST parameter
    // should be responsible for control whether or not it is published.
  end;

implementation

uses
  SysUtils;



{ TVstParameterList }

constructor TVstParameterManager.Create;
begin
  fParList := TObjectList.Create;
  fParList.OwnsObjects := true;

  fPublishedParList := TObjectList.Create;
  fPublishedParList.OwnsObjects := false;
end;

destructor TVstParameterManager.Destroy;
begin
  fParList.Free;
  fPublishedParList.Free;
  inherited;
end;

function TVstParameterManager.Count: integer;
begin
  result := fParList.Count;
end;

function TVstParameterManager.FindParameterIndex(aPar: TVstParameter): integer;
var
  c1: Integer;
begin
  for c1 := 0 to fParList.Count-1 do
  begin
    if fParList[c1] = aPar
      then exit(c1);
  end;

  // no matching parameter found.
  result := -1;
end;

function TVstParameterManager.FindParameterIndexByName(aName: string): integer;
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

function TVstParameterManager.GetPar(Index: integer): TVstParameter;
begin
  result := fParList[Index] as TVstParameter;
end;

function TVstParameterManager.GetPublishedParameterCount: integer;
begin
   result := fPublishedParList.Count;
end;

function TVstParameterManager.NewParameter(Name: string): TVstParameter;
var
  aPar : TVstParameter;
begin
  aPar := TVstParameter.Create(Name);
  fParList.Add(aPar);
  result := aPar;
end;

function TVstParameterManager.FindParameter(aName: string): TVstParameter;
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

function TVstParameterManager.PublishedParameter(Index: integer): TVstParameter;
begin
  result := fPublishedParList[Index] as TVstParameter;
end;

function TVstParameterManager.Add(aPar: TVstParameter):TVstParameter;
begin
  fParList.Add(aPar);
  result := aPar;
end;

procedure TVstParameterManager.BuildPublishedParameterInfo;
var
  c1: Integer;
  PublishedParIndex : integer;
begin
  PublishedParIndex := 0;

  for c1 := 0 to Count-1 do
  begin
    if Parameter[c1].IsPublished then
    begin
      // add a referece to the parameter to the published parameter list.
      fPublishedParList.Add(Parameter[c1]);

      // Update the Vst Parameter with a PublishedParameterIndex.
      // NOTE: Published Par Index is the index of the parameter as seen by the VST host.
      TVstParameterHack(Parameter[c1]).fPublishedVstParameterIndex := PublishedParIndex;
      inc(PublishedParIndex);
    end;
  end;
end;


end.
