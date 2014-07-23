{
  TLifeTimeManager holds a list of objects and maintains reference counts for each. If an object is freed by
  the manager's interface, the manager will not free it until the reference count for it drops to zero.

  The LifeTimeManager class is useful in situations when a data object is owned by a single object but used by
  multiple objects. 
}

unit eeLifeTimeManager;

interface

uses
  Contnrs, SyncObjs, VamLib.Types;

type
  TObjectWrapper = class
  private
    fWrappedObject: TObject;
    fUsageCount: integer;
  public
    constructor Create;
	  destructor Destroy; override;

    property WrappedObject:TObject read fWrappedObject write fWrappedObject;
    property UsageCount   :integer read fUsageCount    write fUsageCount;
  end;


  TLifeTimeManager = class
  private
  protected
    InActiveObjects:TObjectList;
    ActiveObjects:TObjectList;
    Lock:TFixedCriticalSection;
    function FindAsActive(aObject:TObject):TObjectWrapper;
    function FindAsInActive(aObject:TObject):TObjectWrapper;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure AddObject(aObject:TObject);
    procedure FreeObject(aObject:TObject);

    //All IncUsageReference calls must be matched with a DecUsageReference call.
    procedure IncUsageReference(aObject:TObject);
    procedure DecUsageReference(aObject:TObject);
  end;

implementation

uses
  SysUtils;

{ TObjectWrapper }

constructor TObjectWrapper.Create;
begin
  UsageCount := 0;
end;

destructor TObjectWrapper.Destroy;
begin
  if assigned(fWrappedObject) then fWrappedObject.Free;
  inherited;
end;


{ TLifeTimeManager }

constructor TLifeTimeManager.Create;
begin
  ActiveObjects   := TObjectList.Create;
  ActiveObjects.OwnsObjects := true;

  InActiveObjects := TObjectList.Create;
  InActiveObjects.OwnsObjects := true;

  Lock := TFixedCriticalSection.Create;
end;

destructor TLifeTimeManager.Destroy;
begin
  ActiveObjects.Free;
  InActiveObjects.Free;
  Lock.Free;
  inherited;
end;

procedure TLifeTimeManager.AddObject(aObject: TObject);
var
  Wrapper:TObjectWrapper;
begin
  Lock.Acquire;
  try
    Wrapper := TObjectWrapper.Create;
    Wrapper.WrappedObject := aObject;
    ActiveObjects.Add(Wrapper);
  finally
    Lock.Release;
  end;
end;

procedure TLifeTimeManager.FreeObject(aObject: TObject);
var
  wobj:TObjectWrapper;
begin
  Lock.Acquire;
  try
    wobj := FindAsActive(aObject);
    if wobj = nil then raise Exception.Create('aObject not found in Active Objects list.');

    if wobj.UsageCount > 0 then
    begin
      ActiveObjects.Extract(wobj);
      InActiveObjects.Add(wobj);
    end else
    begin
      //remove from list and free the object.
      ActiveObjects.Remove(wobj);
    end;
  finally
    Lock.Release;
  end;
end;

procedure TLifeTimeManager.IncUsageReference(aObject: TObject);
var
  wobj:TObjectWrapper;
begin
  Lock.Acquire;
  try
    wobj := FindAsActive(aObject);
    if wobj = nil then raise Exception.Create('aObject not found in Active Objects list.');

    wobj.UsageCount := wobj.UsageCount + 1;
  finally
    Lock.Release;
  end;
end;

procedure TLifeTimeManager.DecUsageReference(aObject: TObject);
var
  wobj:TObjectWrapper;
begin
  Lock.Acquire;
  try
    //NOTE: This nested if-then statement looks a bit ugly doesn't it? Refactor maybe...
    wobj := FindAsActive(aObject);
    if wobj <> nil then
    begin
      wobj.UsageCount := wobj.UsageCount - 1;
      if wobj.UsageCount < 0 then raise Exception.Create('Usage count is less then zero.');
    end else
    begin
      wobj := FindAsInActive(aObject);
      if wobj <> nil then
      begin
        wobj.UsageCount := wobj.UsageCount - 1;
        if wobj.UsageCount = 0
          then InActiveObjects.Remove(wobj); //remove from list and free the object.
      end else
      begin
        Exception.Create('aObject not found.');
      end;
    end;
  finally
    Lock.Release;
  end;
end;




function TLifeTimeManager.FindAsActive(aObject: TObject): TObjectWrapper;
var
  c1:integer;
begin
  for c1 := 0 to ActiveObjects.Count - 1 do
  begin
    if (ActiveObjects[c1] as TObjectWrapper).WrappedObject = aObject then
    begin
      result := ActiveObjects[c1] as TObjectWrapper;
      exit; //========================================>>
    end;
  end;
  result := nil;
end;

function TLifeTimeManager.FindAsInActive(aObject: TObject): TObjectWrapper;
var
  c1:integer;
begin
  for c1 := 0 to InActiveObjects.Count - 1 do
  begin
    if (InActiveObjects[c1] as TObjectWrapper).WrappedObject = aObject then
    begin
      result := InActiveObjects[c1] as TObjectWrapper;
      exit; //========================================>>
    end;
  end;
  result := nil;
end;





end.
