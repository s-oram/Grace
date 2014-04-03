unit uCreditCardInterface;

interface

type
  ICreditCard = interface
    ['{6490640C-0E2B-4F7D-908C-0E6A74DCC0A0}']
    function IsValid(aCreditCardNumber: string): boolean;
    function ChargeAmount(aCreditCardNumber: string; aAmount: Double): Boolean;
  end;

const
  Visa = 'VISA';
  Mastercard = 'MasterCard';
  Discover = 'Discover';
  AMEX = 'AMEX';

implementation

end.
