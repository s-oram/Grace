unit uCustomer;

interface

type
  TCustomer = class
  private
    FLastName: string;
    FSalary: integer;
    FFirstName: string;
    procedure SetFirstName(const Value: string);
    procedure SetLastName(const Value: string);
    procedure SetSalary(const Value: integer);
  public
    constructor Create(aFirstName: string; aLastName: string; aSalary: integer);
    property FirstName: string read FFirstName write SetFirstName;
    property LastName: string read FLastName write SetLastName;
    property Salary: integer read FSalary write SetSalary;
  end;

implementation

{ TCustomer }

constructor TCustomer.Create(aFirstName, aLastName: string; aSalary: integer);
begin
  inherited Create;
  FirstName := aFirstName;
  LastName := aLastName;
  Salary := aSalary;
end;

procedure TCustomer.SetFirstName(const Value: string);
begin
  FFirstName := Value;
end;

procedure TCustomer.SetLastName(const Value: string);
begin
  FLastName := Value;
end;

procedure TCustomer.SetSalary(const Value: integer);
begin
  FSalary := Value;
end;

end.
