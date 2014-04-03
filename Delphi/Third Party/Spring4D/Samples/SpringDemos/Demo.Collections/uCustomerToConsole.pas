unit uCustomerToConsole;

interface

uses
       uCustomer
     , Spring.Collections
     ;

procedure WriteCustomersToConsole(aList: IList<TCustomer>);

implementation

uses
      SysUtils
    ;

procedure WriteCustomersToConsole(aList: IList<TCustomer>);
var
  Customer: TCustomer;
begin
  for Customer in aList do
  begin
    WriteLn(Format('%s, %s: $%d', [Customer.LastName, Customer.FirstName, Customer.Salary]));
  end;
end;

end.
