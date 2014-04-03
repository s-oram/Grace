unit uSortCustomers;

interface

procedure UnsortedCustomers;
procedure SortCustomersBySalary;
procedure SortCustomerByLastName;

implementation

uses
       uCustomer
     , uCustomerListFiller
     , uCustomerToConsole
     , Spring.Collections
     , SysUtils;

function CompareBySalary(const Left, Right: TCustomer): Integer;
begin
  Result := Left.Salary - Right.Salary;
end;

function CompareByName(const Left, Right: TCustomer): Integer;
begin
  Result := CompareText(Left.LastName, Right.LastName);
end;

procedure UnsortedCustomers;
var
  Customers: IList<TCustomer>;
begin
  Customers := TCollections.CreateObjectList<TCustomer>(True);
  FillListWithCustomers(Customers);
  WriteCustomersToConsole(Customers);
end;

procedure SortCustomersBySalary;
var
  Customers: IList<TCustomer>;
begin
  Customers := TCollections.CreateObjectList<TCustomer>(True);
  FillListWithCustomers(Customers);
  Customers.Sort(CompareBySalary);
  WriteCustomersToConsole(Customers);
end;

procedure SortCustomerByLastName;
var
  Customers: IList<TCustomer>;
begin
  Customers := TCollections.CreateObjectList<TCustomer>(True);
  FillListWithCustomers(Customers);
  Customers.Sort(CompareByName);
  WriteCustomersToConsole(Customers);
end;

end.
