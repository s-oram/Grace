unit uRegistrations;

interface

uses
  Spring.Container;

procedure RegisterTypes(const container: TContainer);

implementation

uses
  uOrderEntry,
  uOrderProcessor,
  uOrderValidator;

procedure RegisterTypes(const container: TContainer);
begin
  container.RegisterType<TOrderEntry>;
  container.RegisterType<TOrderProcessor>;
  container.RegisterType<TOrderValidator>;

  container.Build;
end;

end.
