unit uOrderEntryDecorator;

interface

uses
  uOrder,
  uOrderInterfaces;

type
  TOrderEntryDecorator = class(TInterfacedObject, IOrderEntry)
  private
    fOrderEntry: IOrderEntry;
  protected
    property OrderEntry: IOrderEntry read fOrderEntry;
  public
    constructor Create(const aOrderEntry: IOrderEntry);

    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean; virtual; abstract;
  end;

  TOrderEntryLoggingDecorator = class(TOrderEntryDecorator)
  public
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean; override;
  end;

  TOrderEntryTransactionDecorator = class(TOrderEntryDecorator)
  public
    function EnterOrderIntoDatabase(aOrder: TOrder): Boolean; override;
  end;

  TOrderValidatorDecorator = class(TInterfacedObject, IOrderValidator)
  private
    fOrderValidator: IOrderValidator;
  protected
    property OrderValidator: IOrderValidator read fOrderValidator;
  public
    constructor Create(const aOrderValidator: IOrderValidator);

    function ValidateOrder(aOrder: TOrder): Boolean; virtual; abstract;
  end;

  TOrderValidatorLoggingDecorator = class(TOrderValidatorDecorator)
  public
    function ValidateOrder(aOrder: TOrder): Boolean; override;
  end;

implementation


{$REGION 'TOrderEntryDecorator'}

constructor TOrderEntryDecorator.Create(const aOrderEntry: IOrderEntry);
begin
  fOrderEntry := aOrderEntry;
end;

{$ENDREGION}


{$REGION 'TOrderEntryLoggingDecorator'}

function TOrderEntryLoggingDecorator.EnterOrderIntoDatabase(aOrder: TOrder): Boolean;
begin
  Writeln('Before entering order into the database....');
  Result := fOrderEntry.EnterOrderIntoDatabase(aOrder);
  Writeln('After entering order into the database....');
end;

{$ENDREGION}


{$REGION 'TAnotherOrderEntryDecorator'}

function TOrderEntryTransactionDecorator.EnterOrderIntoDatabase(
  aOrder: TOrder): Boolean;
begin
  Writeln('Starting transaction....');
  Result := fOrderEntry.EnterOrderIntoDatabase(aOrder);
  Writeln('Committing transaction....');
end;

{$ENDREGION}


{$REGION 'TOrderValidatorDecorator'}

constructor TOrderValidatorDecorator.Create(
  const aOrderValidator: IOrderValidator);
begin
  fOrderValidator := aOrderValidator;
end;

{$ENDREGION}


{$REGION 'TOrderValidatorLoggingDecorator'}

function TOrderValidatorLoggingDecorator.ValidateOrder(aOrder: TOrder): Boolean;
begin
  Writeln('Before validating order....');
  Result := fOrderValidator.ValidateOrder(aOrder);
  Writeln('After validating order....');
end;

{$ENDREGION}


end.
