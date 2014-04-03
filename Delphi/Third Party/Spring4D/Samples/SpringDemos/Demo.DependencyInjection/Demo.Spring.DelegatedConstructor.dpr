program Demo.Spring.DelegatedConstructor;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Spring.Services,
  Spring.Container,
  uDelegatedConstructor in 'uDelegatedConstructor.pas',
  uCurrentUser in 'uCurrentUser.pas';

var
  UserUpgrader: IUserUpgrader;
  UserDownGrader: IUserDownGrader;

begin
  try
    GlobalContainer.Build;

    UserUpgrader := ServiceLocator.GetService<IUserUpgrader>;
    UserUpgrader.UpgradeUser;

    UserDownGrader := ServiceLocator.GetService<IUserDownGrader>;
    UserDownGrader.DowngradeUser;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  ReadLn;
end.
