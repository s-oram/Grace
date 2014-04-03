unit uDelegatedConstructor;

interface

uses
      uCurrentUser
    ;

type

  IUserUpgrader = interface
    ['{1786FBFC-3473-41F8-8CEE-EF0A5656DD61}']
    procedure UpgradeUser;
  end;

  IUserDownGrader = interface
    ['{92D94C11-E94C-4F36-967F-0B118476945B}']
    procedure DowngradeUser;
  end;

  TUserProcessor = class(TInterfacedObject, IUserUpgrader, IUserDownGrader)
  private
    FUser: TUser;
  public
    constructor Create(aUser: TUser);
    procedure UpgradeUser;
    procedure DowngradeUser;
  end;

implementation

uses
      Spring.Container
    ;

{ TUserUpgrader }

constructor TUserProcessor.Create(aUser: TUser);
begin
  inherited Create;
  FUser := aUser;
end;

procedure TUserProcessor.DowngradeUser;
begin
  Assert(FUser <> nil);
  WriteLn('User is now downgraded!!');
end;

procedure TUserProcessor.UpgradeUser;
begin
  Assert(FUser <> nil);
  WriteLn('User is now upgraded!!');
end;

initialization
  // Okay, so to create a TUserProcessor, we need to have the current user.
  // (Pretend like this is a live system, and that there might be some other administrative
  // system that uses the class that might not want the "current user".
  //
  // Thus, there is no concept of a "normal" constructor, because the concept of
  // current user is dynamic at the time of creation of TUserProcessor
  //
  // Thus, we "delegate" the construction inside the container to an anonymous method
  // that creates it based on information only available at runtime.
  //
  // The call to AsTransient (the default, actually) means that each instance will be
  // released when it goes out of scope, and that  you'll get a new, current one each
  // time you ask for it.

  GlobalContainer.RegisterType<TUserProcessor>.Implements<IUserUpgrader>.AsTransient.DelegateTo(
    function: TUserProcessor
    begin
      Result := TUserProcessor.Create(GetCurrentUser);
    end
  );

  GlobalContainer.RegisterType<TUserProcessor>.Implements<IUserDownGrader>.AsTransient.DelegateTo(
    function: TUserProcessor
    begin
      Result := TUserProcessor.Create(GetCurrentUser);
    end
  );


end.
