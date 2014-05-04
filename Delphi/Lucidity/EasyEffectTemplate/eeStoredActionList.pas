unit eeStoredActionList;

interface

uses
  SysUtils,
  VamLib.Types,
  VamLib.Collections.Lists;

type
  TStoredAction = record
    ID     : cardinal;
    Action : TProc;
  end;

  TStoredActionList = class(TSimpleRecordList<TStoredAction>);

  TStoredActions = class
  private
    ListLock : TFixedCriticalSection;
    Actions : TStoredActionList;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const ID : cardinal; const Action : TProc);

    procedure Run;

    property Count : integer read GetCount;
  end;

implementation

{ TStoredActions }

constructor TStoredActions.Create;
begin
  ListLock := TFixedCriticalSection.Create;
  Actions := TStoredActionList.Create;
end;

destructor TStoredActions.Destroy;
begin
  Actions.Free;
  ListLock.Free;
  inherited;
end;

function TStoredActions.GetCount: integer;
begin
  result := Actions.Count;
end;

procedure TStoredActions.Add(const ID: cardinal; const Action: TProc);
var
  NewAction : TStoredAction;
begin
  ListLock.Acquire;
  try
    NewAction.ID := ID;
    NewAction.Action := Action;
    Actions.Add(NewAction);
  finally
    ListLock.Release;
  end;
end;

procedure TStoredActions.Run;
var
  c1: Integer;
begin
  ListLock.Acquire;
  try
    for c1 := 0 to Actions.Count-1 do
    begin
      //run the action.
      Actions[c1].Action();
    end;

    Actions.Clear;
  finally
    ListLock.Release;
  end;
end;





end.
