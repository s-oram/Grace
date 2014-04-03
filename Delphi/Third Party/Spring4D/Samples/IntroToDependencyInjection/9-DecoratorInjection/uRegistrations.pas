unit uRegistrations;

interface

uses
  Spring.Container;

procedure RegisterTypes(const container: TContainer);

implementation

uses
  Spring.Container.DecoratorExtension,
  uOrderInterfaces,
  uOrderEntry,
  uOrderEntryDecorator,
  uOrderProcessor,
  uOrderValidator;

procedure RegisterTypes(const container: TContainer);
begin
  container.AddExtension<TDecoratorContainerExtension>;

  container.RegisterType<TOrderEntryTransactionDecorator>;
  container.RegisterType<TOrderEntryLoggingDecorator>;
  container.RegisterType<TOrderEntry>;
  container.RegisterType<TOrderValidatorLoggingDecorator>;
  container.RegisterType<TOrderValidator>;
  container.RegisterType<TOrderProcessor>;

  container.Build;
end;

end.
