unit uInjectionDemo;

interface

uses
       Classes
     , Spring
     ;

type

  TSpecialType = class
  private
    FString: String;
  public
    constructor Create(aString: string);
    property MyString: string read FString;
  end;

  IMyInterface = interface
    ['{90ADF5EC-795C-4E14-8FDD-F736C22BBEC8}']
    function GetSpecialType: TSpecialType;
    procedure SetSpecialType(aValue: TSpecialType);
    function GetSomeString: string;
    procedure SetSomeString(aValue: string);
    property SomeString: string read GetSomeString write SetSomeString;
    function GetSomeInteger: integer;
    function GetAnotherString: string;
    procedure SetAnotherString(const aValue: string);
    function GetSetterInjectionString: string;
    procedure SetSetterInjectionString(const Value: string);
    procedure SetFieldInjectionString(const Value: string);
    function GetFieldInjectionString: string;


    procedure SomeMethod(aString: string);
    procedure SetSomeInteger(const aValue: integer);
    property SomeInteger: integer read GetSomeInteger write SetSomeInteger;
    property AnotherString: string read GetAnotherString Write SetAnotherString;
    property SetterInjectionString: string read GetSetterInjectionString write SetSetterInjectionString;
    property SpecialType: TSpecialType read GetSpecialType write SetSpecialType;
    property FieldInjectionString: string read GetFieldInjectionString write SetFieldInjectionString;
  end;


procedure RegisterInjectionTestServices(aSpecialTypeString: string);

implementation

uses
       Spring.Container
     , Spring.Services
     ;

type
  TPropertyInjectionDemo = class(TInterfacedObject, IMyInterface)
  private
    FSomeString: string;
    FSomeInteger: integer;
    FAnotherString: string;
    FSetterInjectionString: string;
    FSpecialType: TSpecialType;
    [Inject]
    FFieldInjectionString: string;
    function GetSomeString: string;
    procedure SetSomeString(aValue: string);
    function GetSomeInteger: integer;
    procedure SetSomeInteger(const aValue: integer);
    function GetAnotherString: string;
    procedure SetAnotherString(const aValue: string);
    function GetSpecialType: TSpecialType;
    procedure SetSpecialType(aValue: TSpecialType);
    procedure SetFieldInjectionString(const Value: string);
    function GetFieldInjectionString: string;

  public
    function GetSetterInjectionString: string;
    // Must be public for Method Injection to work
    procedure SetSetterInjectionString(const Value: string);

    constructor Create(aString: String; aInteger: integer);
    procedure SomeMethod(aString: string);
    property SomeString: string read GetSomeString write SetSomeString;
    property SomeInteger: integer read GetSomeInteger write SetSomeInteger;
    property AnotherString: string read GetAnotherString Write SetAnotherString;
    property SetterInjectionString: string read GetSetterInjectionString write SetSetterInjectionString;
    [Inject]
    property SpecialType: TSpecialType read GetSpecialType write SetSpecialType;
    property FieldInjectionString: string read GetFieldInjectionString write SetFieldInjectionString;

  end;

procedure RegisterInjectionTestServices(aSpecialTypeString: string);
begin
  GlobalContainer.RegisterType<TPropertyInjectionDemo>
                 .Implements<IMyInterface>
                 .InjectField('FFieldInjectionString', 'Field Injection is working!')
                 .InjectMethod('SetSetterInjectionString', ['string from InjectMethod call'])
                 .InjectProperty('AnotherString', 'This value set by injection.')
                 .InjectConstructor(['Set from Spring DI', Random(100)])

    ;
  GlobalContainer.RegisterType<TSpecialType>.Implements<TSpecialType>.DelegateTo(
  function: TSpecialType
  begin
    Result := TSpecialType.Create(aSpecialTypeString);
  end);
  GlobalContainer.Build;
end;

{ TPropertyInjectionDemo }

constructor TPropertyInjectionDemo.Create(aString: String; aInteger: integer);
begin
  inherited Create;
  FSomeString := aString;
  FSomeInteger := aInteger;
end;

function TPropertyInjectionDemo.GetAnotherString: string;
begin
  Result := FAnotherString;
end;

function TPropertyInjectionDemo.GetFieldInjectionString: string;
begin
  Result := FFieldInjectionString;
end;

function TPropertyInjectionDemo.GetSetterInjectionString: string;
begin
  Result := FSetterInjectionString;
end;

function TPropertyInjectionDemo.GetSomeInteger: integer;
begin
  Result := FSomeInteger;
end;

function TPropertyInjectionDemo.GetSomeString: string;
begin
  Result := FSomeString;
end;

function TPropertyInjectionDemo.GetSpecialType: TSpecialType;
begin
  Result := FSpecialType;
end;

procedure TPropertyInjectionDemo.SetAnotherString(const aValue: string);
begin
  FAnotherString := aValue;
end;

procedure TPropertyInjectionDemo.SetFieldInjectionString(const Value: string);
begin
  FFieldInjectionString := Value;
end;

procedure TPropertyInjectionDemo.SetSetterInjectionString(const Value: string);
begin
  FSetterInjectionString := Value;
end;

procedure TPropertyInjectionDemo.SetSomeInteger(const aValue: integer);
begin
  FSomeInteger := aValue
end;

procedure TPropertyInjectionDemo.SetSomeString(aValue: string);
begin
  FSomeString := aValue;
end;

procedure TPropertyInjectionDemo.SetSpecialType(aValue: TSpecialType);
begin
  FSpecialType.Free;
  FSpecialType := aValue;
end;

procedure TPropertyInjectionDemo.SomeMethod(aString: string);
begin
  WriteLn('Method Injection from normal method: ', aString);
end;

{ TSpecialType }

constructor TSpecialType.Create(aString: string);
begin
  FString := aString;
end;

end.
