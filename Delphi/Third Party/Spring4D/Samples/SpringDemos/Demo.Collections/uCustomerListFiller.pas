unit uCustomerListFiller;

interface

uses
      Spring
    , Spring.Collections
    , uCustomer
    ;

procedure FillListWithCustomers(aList: IList<TCustomer>);

implementation

procedure FillListWithCustomers(aList: IList<TCustomer>);
begin
  Guard.CheckNotNull(aList, 'aList');

  aList.Add(TCustomer.Create('Nick', 'Hodges', 1000000));
  aList.Add(TCustomer.Create('Michael', 'Miller', 500000));
  aList.Add(TCustomer.Create('Bill', 'Johnson', 67009));
  aList.Add(TCustomer.Create('Sally', 'Anderson', 33443));
  aList.Add(TCustomer.Create('George', 'Wilson', 120987));
  aList.Add(TCustomer.Create('Alice', 'Fritchman', 49887));
  aList.Add(TCustomer.Create('Henry', 'Moser', 91234));
  aList.Add(TCustomer.Create('Lisa', 'Williams', 66000));
  aList.Add(TCustomer.Create('Allen', 'Rogers', 77000));
  aList.Add(TCustomer.Create('Wendy', 'Harding', 88000));
end;

end.
