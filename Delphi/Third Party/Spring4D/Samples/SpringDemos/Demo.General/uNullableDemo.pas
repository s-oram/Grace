unit uNullableDemo;

interface

uses
     Spring;

procedure RunNullableDemo;

implementation

procedure ShowThreeStates(aThreeStateBoolean: TNullableBoolean);
begin
  if aThreeStateBoolean.HasValue then
  begin
    Write('Value set to: ');
    if aThreeStateBoolean.Value then
    begin
      Writeln('True')
    end else
    begin
      Writeln('False')
    end;
  end else
  begin
    Writeln('No value set.')
  end;
end;

var
    NullableBoolean: TNullableBoolean;

procedure RunNullableDemo;
begin
  try
    WriteLn(NullableBoolean.Value);
  except on E: EInvalidOperationException do
    WriteLn('Value has not been set, EInvalidOperationException properly thrown');
  end;

  ShowThreeStates(NullableBoolean);

  NullableBoolean := True;
  ShowThreeStates(NullableBoolean);

  NullableBoolean := False;
  ShowThreeStates(NullableBoolean);
end;

end.
