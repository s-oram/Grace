unit uYeller;

interface

type

//  IYeller = interface
//    procedure Yell;
//  end;

  TYeller = class//(TInterfacedObject, IYeller)
    procedure Yell;
  end;

implementation

uses
      Spring.Container
    ;

procedure TYeller.Yell;
begin
  Writeln('See, you can put a plain old object in the Spring Container, and it works.');
end;

initialization
  GlobalContainer.RegisterType<TYeller>;


end.
