unit uModularConnectionManager;

//TODO:MED delete this unit.

interface

uses
  SysUtils, Classes, Contnrs, uEventList;

type
  EModularConnectionsException = class(Exception);

  TModularConnection = class
  strict private
    fOutputJack: string;
    fInputJack: string;
  private
  public
    property OutputJack : string read fOutputJack write fOutputJack;
    property InputJack  : string read fInputJack  write fInputJack;
  end;

  TModularConnectionManager = class
  strict private
    fLimitInputConnections: boolean;
    fConnectionsChangedWatchList: TEventList;
    function GetConnectionsCount: integer;
    function GetConnection(Index: integer): TModularConnection;
  private
  strict protected
    fConnections : TObjectList;
    function IsExistingConnectionsToInput(const aInputJack : string):boolean;
    property ConnectionsChangedWatchList : TEventList read fConnectionsChangedWatchList write fConnectionsChangedWatchList;
  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure RequestNotificationOfConnectionsChanged(RequestHandler : TNotifyEvent);
    procedure RemoveNotificationOfConnectionsChanged(RequestHandler : TNotifyEvent);


    // NOTE: The NewConnection() and DeleteConnection() are both quite strict. They will throw exceptions
    // if the action doesn't make sense. For example: New connections can not be made
    // if an existing identical connection already exists. Likewise non-existent connections cannot be
    // deleted.
    // It would be possible for the NewConnection() and DeleteConnection() methods to be more forgiving
    // and ignore invalid actions but here I'm choosing to go with the Fail-Fast philosophy.
    // It's likely that this modular connection manager will be used alongside other
    // connection managers. For example a GUI connection manager (TRedFoxCableOverlay). It's important that
    // all connection managers remain in sync.
    procedure NewConnection(const OutputJack, InputJack : string);
    procedure DeleteConnection(const OutputJack, InputJack : string);

    function ConnectionExists(const OutputJack, InputJack : string):boolean;
    function IndexOfConnection(const OutputJack, InputJack : string) : integer;

    property ConnectionsCount : integer read GetConnectionsCount;

    property Connections[Index : integer] : TModularConnection read GetConnection; default;

    // Limits connections to one connection per input. (IE. several connections cannot be made to the
    // same input jack.)
    property LimitInputConnections : boolean read fLimitInputConnections write fLimitInputConnections;




  end;

implementation

{ TModularConnectionManager }

constructor TModularConnectionManager.Create;
begin
  fConnections := TObjectList.Create;
  fConnections.OwnsObjects := true;

  fConnectionsChangedWatchList := TEventList.Create;

  fLimitInputConnections := true;
end;

destructor TModularConnectionManager.Destroy;
begin
  fConnections.Free;
  fConnectionsChangedWatchList.Free;
  inherited;
end;

procedure TModularConnectionManager.RequestNotificationOfConnectionsChanged(RequestHandler: TNotifyEvent);
begin
  fConnectionsChangedWatchList.Add(RequestHandler);
end;

procedure TModularConnectionManager.RemoveNotificationOfConnectionsChanged(RequestHandler: TNotifyEvent);
begin
  fConnectionsChangedWatchList.Remove(RequestHandler);
end;

function TModularConnectionManager.GetConnection(Index: integer): TModularConnection;
begin
  result := fConnections[Index] as TModularConnection;
end;

function TModularConnectionManager.GetConnectionsCount: integer;
begin
  result := fConnections.Count;
end;

function TModularConnectionManager.IndexOfConnection(const OutputJack, InputJack: string): integer;
var
  c1: Integer;
begin
  for c1 := 0 to ConnectionsCount-1 do
  begin
    if (Connections[c1].OutputJack = OutputJack) and (Connections[c1].InputJack = InputJack) then
    begin
      result := c1;
      exit; //========>> exit >>===========>>
    end;
  end;

  // If we've made it this far no matching connections have been found.
  result := -1;
end;

function TModularConnectionManager.ConnectionExists(const OutputJack, InputJack: string): boolean;
begin
  if IndexOfConnection(OutputJack, InputJack) <> -1
    then result := true
    else result := false;
end;



function TModularConnectionManager.IsExistingConnectionsToInput(const aInputJack: string): boolean;
var
  c1: Integer;
begin
  for c1 := 0 to ConnectionsCount-1 do
  begin
    if Connections[c1].InputJack = aInputJack then
    begin
      result := true;
      exit; //========>> exit >>===========>>
    end;
  end;

  // If we've made it this far no connections to this input have been found.
  result := false;
end;

procedure TModularConnectionManager.NewConnection(const OutputJack, InputJack: string);
var
  aConnection : TModularConnection;
begin
  if (LimitInputConnections) and (IsExistingConnectionsToInput(InputJack))
    then raise EModularConnectionsException.Create('A connection to this input jack already exists. Cannot connect multiple sources to a single input jack.');

  if (IndexOfConnection(OutputJack, InputJack) <> -1)
    then raise EModularConnectionsException.Create('This connection already exists. Cannot create multiple duplicate connections.');

  aConnection := TModularConnection.Create;
  aConnection.OutputJack := OutputJack;
  aConnection.InputJack  := InputJack;

  fConnections.Add(aConnection);
  fConnectionsChangedWatchList.TriggerAll(self);
end;

procedure TModularConnectionManager.DeleteConnection(const OutputJack, InputJack: string);
var
  Index : integer;
begin
  Index := IndexOfConnection(OutputJack, InputJack);

  if (Index = -1) then raise EModularConnectionsException.Create('This connection doesn''t exist. Cannot delete non-existent connection.');

  fConnections.Delete(Index);

  fConnectionsChangedWatchList.TriggerAll(self);
end;



end.
