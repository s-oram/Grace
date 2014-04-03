unit uCreditCards;

interface

uses
     Classes
   ;

procedure RegisterCreditCards(aStrings: TStrings);

implementation

uses
       uCreditCardInterface
     , SysUtils
     , Spring.Container
     ;

type

  ECreditCardException = class(Exception);

  TCreditCard = class(TInterfacedObject, ICreditCard)
  strict private
    fStrings: TStrings;
    FName: string;
  strict protected
    constructor Create(aName: string; aStrings: TStrings); overload;
  public
    function ChargeAmount(aCreditCardNumber: string; aAmount: Double): Boolean;
    function IsValid(aCreditCardNumber: string): Boolean;
    property Name: string read FName;
  end;

  TVisa = class(TCreditCard)
  public
    constructor Create(aStrings: TStrings); overload;
  end;

  TMasterCard = class(TCreditCard)
  public
    constructor Create(aStrings: TStrings); overload;
  end;

  TDiscover = class(TCreditCard)
  public
    constructor Create(aStrings: TStrings); overload;
  end;

  TAMEX = class(TCreditCard)
  public
    constructor Create(aStrings: TStrings); overload;
  end;


procedure RegisterCreditCards(aStrings: TStrings);
begin
  GlobalContainer.RegisterType<TVisa>.Implements<ICreditCard>(VISA).DelegateTo
      (
    function: TVisa
    begin
      Result := TVisa.Create(aStrings);
    end
  );
  GlobalContainer.RegisterType<TMasterCard>.Implements<ICreditCard>(MasterCard).DelegateTo
      (
    function: TMasterCard
    begin
      Result := TMasterCard.Create(aStrings);
    end
  );;
  GlobalContainer.RegisterType<TDiscover>.Implements<ICreditCard>(Discover).DelegateTo
      (
    function: TDiscover
    begin
      Result := TDiscover.Create(aStrings);
    end
  );;
  GlobalContainer.RegisterType<TAMEX>.Implements<ICreditCard>(AMEX).DelegateTo
      (
    function: TAMEX
    begin
      Result := TAMEX.Create(aStrings);
    end
  );;
end;

{ TCreditCard }

constructor TCreditCard.Create(aName: string; aStrings: TStrings);
begin
  inherited Create;
  if aStrings = nil then
  begin
    raise ECreditCardException.Create('aStrings cannot be nil');
  end;
  if aName = '' then
  begin
    raise ECreditCardException.Create('A credit card must have a name');
  end;
  FStrings := aStrings;
  FName := aName;
end;

function TCreditCard.ChargeAmount(aCreditCardNumber: string; aAmount: Double): Boolean;
begin
  FStrings.Add(Format('This %s card: %s has been charged $%f', [Name, aCreditCardNumber, aAmount]));
  Result := True;
end;

function TCreditCard.IsValid(aCreditCardNumber: string): Boolean;
begin
  Result := True;
  FStrings.Add(Format('This %s card: %s is valid.', [Name, aCreditCardNumber]));
end;

{ TVisa }

constructor TVisa.Create(aStrings: TStrings);
begin
  inherited Create;
  Create(VISA, aStrings);
end;

{ TMasterCard }

constructor TMasterCard.Create(aStrings: TStrings);
begin
  inherited Create;
  Create(MasterCard, aStrings);
end;

{ TDiscover }

constructor TDiscover.Create(aStrings: TStrings);
begin
  inherited Create;
  Create(Discover, aStrings);
end;

{ TAMEX }

constructor TAMEX.Create(aStrings: TStrings);
begin
  inherited Create;
  Create(AMEX, aStrings);
end;



end.
