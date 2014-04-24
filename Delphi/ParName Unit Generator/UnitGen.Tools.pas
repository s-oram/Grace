unit UnitGen.Tools;

interface

uses
  Classes;

type
  TUnitGen = class
  private
  public
    class procedure AddEmptyLine(var UnitText : TStringList);

    class procedure AddUnitHeader(var UnitText : TStringList; const UnitName : string);
    class procedure AddImplementationHeader(var UnitText : TStringList);
    class procedure AddUnitFooter(var UnitText : TStringList);
  end;


implementation

{ TUnitGen }

class procedure TUnitGen.AddEmptyLine(var UnitText: TStringList);
begin
  UnitText.Add('');
end;


class procedure TUnitGen.AddUnitHeader(var UnitText: TStringList; const UnitName: string);
var
  s : string;
begin
  s := 'unit ' + UnitName + ';';
  UnitText.Add(s);
  AddEmptyLine(UnitText);

  UnitText.Add('interface');
  AddEmptyLine(UnitText);
end;

class procedure TUnitGen.AddImplementationHeader(var UnitText: TStringList);
begin
  UnitText.Add('implementation');
  AddEmptyLine(UnitText);
end;

class procedure TUnitGen.AddUnitFooter(var UnitText: TStringList);
begin
  UnitText.Add('end.');
end;



end.
