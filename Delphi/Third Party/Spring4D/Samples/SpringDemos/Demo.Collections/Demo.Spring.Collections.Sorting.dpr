program Demo.Spring.Collections.Sorting;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uCustomer in 'uCustomer.pas',
  uCustomerListFiller in 'uCustomerListFiller.pas',
  uSortCustomers in 'uSortCustomers.pas',
  uCustomerToConsole in 'uCustomerToConsole.pas';

begin
  try
    Writeln('Unsorted Customers:');
    UnsortedCustomers;
    WriteLn('--------------------------------');
    WriteLn('Sorting by Salary:');
    SortCustomersBySalary;
    WriteLn('--------------------------------');
    WriteLn('Sorting by Last Name:');
    SortCustomerByLastName
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
