unit uFieldInjectionDemo;

interface


type
  ISomeClass = interface
  ['{BD80C4AC-42E1-4260-9532-F0C9E831FB09}']
    procedure DoSomething;
  end;

  IFieldInjectionDemo = interface
    ['{73E3709A-B525-4946-99FE-14CC8B3593B6}']
    procedure UseSomeClass;
  end;

implementation

uses
      Spring  // needed to get the attributes to work.
    , Spring.Container
    , Spring.Services
    ;

type
  TSomeClass = class(TInterfacedObject, ISomeClass)
  public
    procedure DoSomething;
  end;

  TFieldInjectionDemo = class(TInterfacedObject, IFieldInjectionDemo)
  private
    [Inject]
    FSomeClass: ISomeClass;
  public
    procedure UseSomeClass;
  end;

{ TSomeClass }

procedure TSomeClass.DoSomething;
begin
  WriteLn('Hey, there''s no Create call for me anywhere!  Huh?!?');
end;

{ TFieldInjectionDemo }

procedure TFieldInjectionDemo.UseSomeClass;
begin
  FSomeClass.DoSomething;
end;

initialization
  GlobalContainer.RegisterType<TFieldInjectionDemo>.Implements<IFieldInjectionDemo>;
  GlobalContainer.RegisterType<TSomeClass>.Implements<ISomeClass>;


end.
