unit uCurrentUser;

interface

type
  TUser = class
  end;

{
 So we'll implement the notion of "GetCurrentUser" as a singleton, purely for
 the purpose of simulating it inside the Spring Container using a delegated
 constructor.  How the TUser is created managed isn't important in this demo
 application.
}

function GetCurrentUser: TUser;

implementation

var
  FInternalUser: TUser;

function GetCurrentUser: TUser;
begin
  if FInternalUser = nil then
  begin
    FInternalUser := TUser.Create;
  end;
  Result := FInternalUser;
end;

end.
