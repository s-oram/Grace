unit SoundElement.Test;

interface

//=========== Public ================
procedure RunTests;

//=========== Private ================

procedure TestModulePins;
procedure TestModulePinEvents;
procedure TestCustomModules;
procedure TestModuleController;

implementation

uses
  SoundElement.Test.Mocks,
  SoundElement.Test.Utils,
  SysUtils,
  Contnrs,
  SoundElement.ModulePins,
  SoundElement.Modules,
  SoundElement.ModuleController;

procedure RunTests;
begin
  TestModulePins;
  TestModulePinEvents;
  TestCustomModules;
  TestModuleController;
end;

procedure TestModulePinEvents;
var
  IsDead : boolean;
  FloatInput : TFloatInput;
  EventHandleObject : TEventHandler;

begin
  FloatInput := TFloatInput.Create;
  EventHandleObject := TEventHandler.Create;

  try
    // Confirm that events can be assigned to anonymous methods.
    IsDead := false;
    FloatInput.OnChanged := procedure(Sender : TObject)
    begin
      IsDead := true;
    end;
    FloatInput.SetPinValue(120);
    assert(IsDead);

    // Confirm that events can be assigned to object methods
    EventHandleObject.IsDead := false;
    FloatInput.OnChanged := EventHandleObject.EventHandle;
    FloatInput.SetPinValue(120);
    assert(IsDead);

  finally
    FloatInput.Free;
    EventHandleObject.Free;
  end;
end;



procedure TestModulePins;
var
  FloatInput : TFloatInput;
  FloatOutput : TFloatOutput;

  IntegerInput : TIntegerInput;
  IntegerOutput : TIntegerOutput;

  StringInput : TStringInput;
  StringOutput : TStringOutput;

  Float64StreamInputA  : TFloat64StreamInput;
  Float64StreamOutputA : TFloat64StreamOutput;
  Float64StreamInputB  : TFloat64StreamInput;
  Float64StreamOutputB : TFloat64StreamOutput;
  c1: Integer;
begin
  FloatInput := TFloatInput.Create;
  FloatOutput := TFloatOutput.Create;

  IntegerInput := TIntegerInput.Create;
  IntegerOutput := TIntegerOutput.Create;

  StringInput := TStringInput.Create;
  StringOutput := TStringOutput.Create;

  Float64StreamInputA  := TFloat64StreamInput.Create;
  Float64StreamOutputA := TFloat64StreamOutput.Create;
  Float64StreamInputB  := TFloat64StreamInput.Create;
  Float64StreamOutputB := TFloat64StreamOutput.Create;

  try
    // Test Value assignment
    IntegerInput.SetPinValue(1);
    IntegerOutput.SetPinValue(1);

    FloatInput.SetPinValue(2);
    FloatInput.SetPinValue(2.3);

    FloatOutput.SetPinValue(2);
    FloatOutput.SetPinValue(2.3);

    StringInput.SetPinValue('James Brown');
    StringOutput.SetPinValue('James Brown');

    // Test float input-output
    assert(PinCommand.CanMakeConnection(FloatOutput, FloatInput) = true);
    PinCommand.MakeConnection(FloatOutput, FloatInput);

    FloatOutput.PinValue := 10.25;
    assert(FloatInput.PinValue = 10.25);

    PinCommand.BreakConnection(FloatOutput, FloatInput);
    FloatOutput.PinValue := 25;
    assert(FloatInput.PinValue <> 25);

    // Test integer input-output
    assert(PinCommand.CanMakeConnection(IntegerOutput, IntegerInput) = true);
    PinCommand.MakeConnection(IntegerOutput, IntegerInput);

    IntegerOutput.PinValue := 10;
    assert(IntegerInput.PinValue = 10);

    PinCommand.BreakConnection(IntegerOutput, IntegerInput);
    IntegerOutput.PinValue := 25;
    assert(IntegerInput.PinValue <> 25);


    // Test integer - float combo
    assert(PinCommand.CanMakeConnection(IntegerOutput, FloatInput) = true);
    assert(PinCommand.CanMakeConnection(FloatOutput, IntegerInput) = false);

    PinCommand.MakeConnection(IntegerOutput, FloatInput);
    IntegerOutput.PinValue := 42;
    assert(FloatInput.PinValue = 42);


    PinCommand.BreakAllConnections(IntegerOutput);
    PinCommand.BreakAllConnections(FloatInput);
    PinCommand.BreakAllConnections(IntegerInput);

    assert(PinCommand.GetConnectionCount(IntegerOutput) = 0);
    assert(PinCommand.GetConnectionCount(FloatInput) = 0);
    assert(PinCommand.GetConnectionCount(IntegerInput) = 0);

    PinCommand.MakeConnection(IntegerOutput, FloatInput);
    PinCommand.MakeConnection(IntegerOutput, IntegerInput);

    assert(PinCommand.GetConnectionCount(IntegerOutput) = 2);
    assert(PinCommand.GetConnectionCount(FloatInput) = 1);
    assert(PinCommand.GetConnectionCount(IntegerInput) = 1);

    IntegerOutput.PinValue := 99;
    assert(IntegerInput.PinValue = 99);
    assert(FloatInput.PinValue   = 99);


    // Test the string pins.
    assert(PinCommand.CanMakeConnection(StringOutput, StringInput)  = true);
    assert(PinCommand.CanMakeConnection(StringOutput, IntegerInput) = false);
    assert(PinCommand.CanMakeConnection(IntegerOutput, StringInput) = false);

    PinCommand.MakeConnection(StringOutput, StringInput);
    assert(PinCommand.GetConnectionCount(StringOutput) = 1);
    assert(PinCommand.GetConnectionCount(StringInput)  = 1);

    StringOutput.PinValue := 'JamesBrownIsDead';
    assert(StringInput.PinValue = 'JamesBrownIsDead');



    //=== test the Float64Stream pins ===
    assert(PinCommand.CanMakeConnection(Float64StreamOutputA, Float64StreamInputA)  = true);
    assert(PinCommand.CanMakeConnection(StringOutput, Float64StreamInputA)  = false);
    PinCommand.MakeConnection(Float64StreamOutputA, Float64StreamInputA);
    assert(PinCommand.CanMakeConnection(Float64StreamOutputB, Float64StreamInputA)  = false);
    PinCommand.BreakConnection(Float64StreamOutputA, Float64StreamInputA);
    assert(PinCommand.CanMakeConnection(Float64StreamOutputB, Float64StreamInputA)  = true);


    Float64StreamOutputB.SetMaxBufferSize(32);
    Float64StreamInputA.SetMaxBufferSize(32);
    PinCommand.MakeConnection(Float64StreamOutputB, Float64StreamInputA);

    for c1 := 0 to 32-1 do
    begin
      Float64StreamOutputB.PinData^.Data[c1] := c1;
    end;

    for c1 := 0 to 32-1 do
    begin
      assert(Float64StreamInputA.PinData^.Data[c1] = c1);
    end;

    PinCommand.BreakConnection(Float64StreamOutputB, Float64StreamInputA);

    for c1 := 0 to 32-1 do
    begin
      assert(Float64StreamInputA.PinData^.Data[c1] = 0);
    end;



  finally
    FloatInput.Free;
    FloatOutput.Free;

    IntegerInput.Free;
    IntegerOutput.Free;

    StringInput.Free;
    StringOutput.Free;

    Float64StreamInputA.Free;
    Float64StreamOutputA.Free;
    Float64StreamInputB.Free;
    Float64StreamOutputB.Free;
  end;
end;

procedure TestCustomModules;
var
  PinInputs : TModulePinList;
  PinOutputs : TModulePinList;
begin
  PinInputs  := TModulePinList.Create(TPinPolarity.Input, nil);
  PinOutputs := TModulePinList.Create(TPinPolarity.Output, nil);
  try
    PinInputs.NewPin('James', TPinType.FloatEvent);
    assert(PinInputs.Count = 1);
    assert(PinInputs['James'].Name = 'James');
    assert(PinInputs[0].Name = 'James');
  finally
    PinInputs.Free;
    PinOutputs.Free;
  end;
end;


procedure TestModuleController;
var
  ModA, ModB, ModC, ModD, ModE : TSteroModule;
  ProcessList : TObjectList;
  CorrectNames, ActualNames : string;
begin
  ProcessList := TObjectList.Create;
  ProcessList.OwnsObjects := false;
  ModA := TSteroModule.Create;
  ModB := TSteroModule.Create;
  ModC := TSteroModule.Create;
  ModD := TSteroModule.Create;
  ModE := TSteroModule.Create;
  try
    ModA.Name := 'ModA';
    ModB.Name := 'ModB';
    ModC.Name := 'ModC';
    ModD.Name := 'ModD';
    ModE.Name := 'ModE';

    //=== Test Feedback Detection ====
    PinCommand.MakeConnection(ModA.Outputs['Out1'] as TOutputPin, ModB.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModB.Outputs['Out1'] as TOutputPin, ModC.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModC.Outputs['Out1'] as TOutputPin, ModD.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModD.Outputs['Out1'] as TOutputPin, ModE.Inputs['In1'] as TInputPin);

    assert(ModuleCommand.CheckForFeedbackConnections(ModB) = false);

    PinCommand.MakeConnection(ModE.Outputs['Out1'] as TOutputPin, ModA.Inputs['In1'] as TInputPin); // <-- Feedback here!!

    assert(ModuleCommand.CheckForFeedbackConnections(ModB) = true);

    //-- Clean up -----------------------------
    ProcessList.Clear;
    ModuleCommand.BreakAllConnections(ModA);
    ModuleCommand.BreakAllConnections(ModB);
    ModuleCommand.BreakAllConnections(ModC);
    ModuleCommand.BreakAllConnections(ModD);
    ModuleCommand.BreakAllConnections(ModE);
    //-----------------------------------------


    //=== Test Processing Order 1 ====

    PinCommand.MakeConnection(ModA.Outputs['Out1'] as TOutputPin, ModB.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModB.Outputs['Out1'] as TOutputPin, ModC.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModC.Outputs['Out1'] as TOutputPin, ModD.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModD.Outputs['Out1'] as TOutputPin, ModE.Inputs['In1'] as TInputPin);

    ModuleCommand.BuildProcessGraph(ModE, ProcessList);

    CorrectNames := 'ModA|ModB|ModC|ModD|ModE';
    ActualNames  := GetModuleNames(ProcessList);

    if not SameText(CorrectNames, ActualNames) then raise Exception.Create('Module processing order is not correct.');

    //-- Clean up -----------------------------
    ProcessList.Clear;
    ModuleCommand.BreakAllConnections(ModA);
    ModuleCommand.BreakAllConnections(ModB);
    ModuleCommand.BreakAllConnections(ModC);
    ModuleCommand.BreakAllConnections(ModD);
    ModuleCommand.BreakAllConnections(ModE);
    //-----------------------------------------


    //=== Test Processing Order 2 ====

    PinCommand.MakeConnection(ModA.Outputs['Out1'] as TOutputPin, ModB.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModB.Outputs['Out1'] as TOutputPin, ModC.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModC.Outputs['Out1'] as TOutputPin, ModD.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModD.Outputs['Out1'] as TOutputPin, ModE.Inputs['In1'] as TInputPin);

    // extra connection from ModA -> ModE
    PinCommand.MakeConnection(ModA.Outputs['Out2'] as TOutputPin, ModE.Inputs['In2'] as TInputPin);

    ModuleCommand.BuildProcessGraph(ModE, ProcessList);

    CorrectNames := 'ModA|ModB|ModC|ModD|ModE';
    ActualNames  := GetModuleNames(ProcessList);

    if not SameText(CorrectNames, ActualNames) then raise Exception.Create('Module processing order is not correct.');

    //-- Clean up -----------------------------
    ProcessList.Clear;
    ModuleCommand.BreakAllConnections(ModA);
    ModuleCommand.BreakAllConnections(ModB);
    ModuleCommand.BreakAllConnections(ModC);
    ModuleCommand.BreakAllConnections(ModD);
    ModuleCommand.BreakAllConnections(ModE);
    //-----------------------------------------



    //=== Test Processing Order 3 ====

    PinCommand.MakeConnection(ModA.Outputs['Out1'] as TOutputPin, ModB.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModB.Outputs['Out1'] as TOutputPin, ModC.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModC.Outputs['Out1'] as TOutputPin, ModD.Inputs['In1'] as TInputPin);
    PinCommand.MakeConnection(ModD.Outputs['Out1'] as TOutputPin, ModE.Inputs['In1'] as TInputPin);

    // extra connections
    PinCommand.MakeConnection(ModA.Outputs['Out2'] as TOutputPin, ModC.Inputs['In2'] as TInputPin);
    PinCommand.MakeConnection(ModC.Outputs['Out2'] as TOutputPin, ModE.Inputs['In2'] as TInputPin);

    ModuleCommand.BuildProcessGraph(ModE, ProcessList);

    CorrectNames := 'ModA|ModB|ModC|ModD|ModE';
    ActualNames  := GetModuleNames(ProcessList);

    if not SameText(CorrectNames, ActualNames) then raise Exception.Create('Module processing order is not correct.');

    //-- Clean up -----------------------------
    ProcessList.Clear;
    ModuleCommand.BreakAllConnections(ModA);
    ModuleCommand.BreakAllConnections(ModB);
    ModuleCommand.BreakAllConnections(ModC);
    ModuleCommand.BreakAllConnections(ModD);
    ModuleCommand.BreakAllConnections(ModE);
    //-----------------------------------------









  finally
    ModA.Free;
    ModB.Free;
    ModC.Free;
    ModD.Free;
    ModE.Free;

    ProcessList.Free;
  end;
end;





end.
