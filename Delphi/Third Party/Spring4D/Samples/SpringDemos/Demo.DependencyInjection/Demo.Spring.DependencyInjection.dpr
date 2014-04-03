program Demo.Spring.DependencyInjection;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  Spring,
  Spring.Container,
  Spring.Services,
  uStringListService,
  uWackyMathService in 'uWackyMathService.pas',
  uNormalMathService in 'uNormalMathService.pas',
  uCalculator in 'uCalculator.pas',
  MathServiceConsts in 'MathServiceConsts.pas',
  uMathInterfaces in 'uMathInterfaces.pas',
  uServiceNames in 'uServiceNames.pas',
  uInjectionDemo in 'uInjectionDemo.pas',
  uIniFileService in 'uIniFileService.pas';

var
  CalculatorService: ICalculator;
  IniFile: IIniFileService;
  SLS: IStringListService;

  MyIntf: IMyInterface;
  S: string;

  procedure CheckNotEmptyBeforeWriting(aStringtoCheck: string; aName: string; aLabelString: string);
  begin
    if aStringtoCheck = '' then
    begin
      raise Exception.Create(aName + ' is empty.');
    end else
    begin
      Writeln(aLabelString, aStringToCheck);
    end;
  end;

begin
  try

    GlobalContainer.Build;

    CalculatorService := ServiceLocator.GetService<ICalculator>(CalculatorName);
    WriteLn('Using Calculator with injected field...');
    WriteLn(CalculatorService.Addition(2, 3), ', ', CalculatorService.Multiplication(3, 4));
    WriteLn;

    RegisterIniFileService('Inifile', 'blah.ini', iftFileBased);
    RegisterIniFileService('MemInifile', 'blah.ini', iftMemoryBased);

    IniFile := ServiceLocator.GetService<IIniFileService>('Inifile');
    WriteLn(IniFile.FileName);
    IniFile.WriteString('one', 'two', 'three');
    WriteLn(IniFile.ReadString('one', 'two', 'failed'));
    IniFile.WriteInteger('Blah', 'two', 42);
    WriteLn(IniFile.ReadInteger('blah', 'two', -1));
    WriteLn;

    IniFile := ServiceLocator.GetService<IIniFileService>('MemInifile');
    WriteLn(IniFile.FileName);
    IniFile.WriteString('one', 'two', 'three');
    WriteLn(IniFile.ReadString('one', 'two', 'failed'));
    IniFile.WriteInteger('Blah', 'two', 42);
    WriteLn(IniFile.ReadInteger('blah', 'two', -1));
    WriteLn;

    SLS := ServiceLocator.GetService<IStringListService>('DSL');
    SLS.Add('one');
    SLS.Add('two');
    SLS.Add('buckle my shoe');
    for S in SLS do
    begin
      Writeln(S);
    end;

    WriteLn(SLS.Count);
    WriteLn;

    SLS.Text := 'Holy Mackeral, Dependency Injection is cool';
    WriteLn(SLS.Text);
    WriteLn;

    RegisterInjectionTestServices('SpecialType.AString set in the Factory via the property injection attribute at: ' + DateTimeToStr(Now));

    MyIntf := ServiceLocator.GetService<IMyInterface>;

    CheckNotEmptyBeforeWriting(MyIntf.SomeString, 'MyIntf.SomeString', 'SomeString set in constructor: ');
    WriteLn('SomeInteger injected value: ', MyIntf.SomeInteger);
    CheckNotEmptyBeforeWriting(MyIntf.AnotherString, 'MyIntf.AnotherString', 'AnotherString injected value: ');
    CheckNotEmptyBeforeWriting(MyIntf.SetterInjectionString, 'MyIntf.SetterInjectionString', 'SetterInjectionString injected value: ');
    CheckNotEmptyBeforeWriting(MyIntf.SpecialType.MyString, 'MyIntf.SpecialType.MyString', 'SpecialType value: ');
    CheckNotEmptyBeforeWriting(MyIntf.FieldInjectionString, 'MyIntf.FieldInjectionString', 'Field set by field injection: ');

    Readln;
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;

end.
