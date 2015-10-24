unit Test.Ron.Parser;

interface

uses
  WatchTower,
  Ron.Lexer,
  Ron.Parser;

type
  TReadRonValueTest = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure SetParserValueTestsA;

    [Test]
    procedure SetParserValueTestsB;

    [Test]
    procedure ReadRonValueTest;

    [Test]
    procedure ReadRonArrayValueTest;
  end;

  TRonParserTest = class(TWatchTowerTest)
  private
    Parser : TParser;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure ParseTestA;

    [Test]
    procedure ParseTestB;

    [Test]
    procedure ParseTestC;

    [Test]
    procedure ParseTestD;

    [Test]
    procedure ParseTestWithComments;

    [Test]
    procedure ParseTestWithMultiLineComments;
  end;

implementation

uses
  WatchTower.Confirm;

type
  // Cracker to access protected methods.
  TParserValueCracker = class(TParserValue);

{ TReadRonValueTest }

procedure TReadRonValueTest.SetParserValueTestsA;
const
  kx : single = 3.14;
var
  pv : TParserValueCracker;
begin
  pv := TParserValueCracker.Create;
  try
    pv.SetValueAsInteger(42);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvInteger  );
    Confirm.IsTrue(  pv.ValueAsInteger = 42 );

    pv.SetValueAsString('data');
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvString  );
    Confirm.IsTrue(  pv.ValueAsString = 'data' );

    pv.SetValueAsFloat(kx);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvFloat  );
    Confirm.IsTrue(  pv.ValueAsFloat = kx );

    pv.SetValueAsBoolean(true);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvBoolean  );
    Confirm.IsTrue(  pv.ValueAsBoolean = true );

    pv.SetValueAsBoolean(false);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvBoolean  );
    Confirm.IsTrue(  pv.ValueAsBoolean = false );

    pv.SetValueAsUntyped('data');
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvUntyped  );
    Confirm.IsTrue(  pv.ValueAsUntyped = 'data' );
  finally
    pv.Free;
  end;
end;

procedure TReadRonValueTest.SetParserValueTestsB;
const
  kx : single = 3.14;
var
  pv : TParserValueCracker;
  vIntegerArray : array of integer;
  vFloatArray   : array of single;
  vBooleanArray : array of boolean;
  vStringArray  : array of string;
begin
  pv := TParserValueCracker.Create;
  try
    SetLength(vIntegerArray, 10);
    SetLength(vFloatArray,   10);
    SetLength(vBooleanArray, 10);
    SetLength(vStringArray,  10);

    vIntegerArray[1] := 42;
    vFloatArray[1] := kx;
    vBooleanArray[1] := true;
    vStringArray[1] := 'data';

    pv.SetValueAsIntegerArray(vIntegerArray);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvIntegerArray  );
    Confirm.IsTrue(  pv.ValueAsInteger(1) = 42 );
    Confirm.IsTrue(  pv.ArrayLength = 10 );

    pv.SetValueAsStringArray(vStringArray);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvStringArray  );
    Confirm.IsTrue(  pv.ValueAsString(1) = 'data' );
    Confirm.IsTrue(  pv.ArrayLength = 10 );

    pv.SetValueAsFloatArray(vFloatArray);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvFloatArray  );
    Confirm.IsTrue(  pv.ValueAsFloat(1) = kx);
    Confirm.IsTrue(  pv.ArrayLength = 10 );

    pv.SetValueAsBooleanArray(vBooleanArray);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvBooleanArray  );
    Confirm.IsTrue(  pv.ValueAsBoolean(1) = true );
    Confirm.IsTrue(  pv.ArrayLength = 10 );

    pv.SetValueAsUntypedArray(vStringArray);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvUntypedArray  );
    Confirm.IsTrue(  pv.ValueAsUntyped(1) = 'data' );
    Confirm.IsTrue(  pv.ArrayLength = 10 );
  finally
    pv.Free;
  end;
end;

procedure TReadRonValueTest.ReadRonValueTest;
var
  Text : string;
  pv : TParserValue;
begin
  pv := TParserValue.Create;
  try
    Text := 'Pizza: 42';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvInteger  );
    Confirm.IsTrue(  pv.ValueAsInteger = 42 );

    Text := 'Pizza: +42';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvInteger  );
    Confirm.IsTrue(  pv.ValueAsInteger = 42 );

    Text := 'Pizza: -1042';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvInteger  );
    Confirm.IsTrue(  pv.ValueAsInteger = -1042 );

    Text := 'Pizza: "TomJones"';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvString  );
    Confirm.IsTrue(  pv.ValueAsString = 'TomJones' );

    Text := 'Pizza: """TomJones"""';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvString  );
    Confirm.IsTrue(  pv.ValueAsString = 'TomJones' );

    Text := 'Pizza: ''TomJones''';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvString  );
    Confirm.IsTrue(  pv.ValueAsString = 'TomJones' );

    Text := 'Pizza: ''''''TomJones''''''';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvString  );
    Confirm.IsTrue(  pv.ValueAsString = 'TomJones' );

    Text := 'Pizza: 3.14';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvFloat  );
    Confirm.IsTrue(  pv.ValueAsFloat > 3.1 );
    Confirm.IsTrue(  pv.ValueAsFloat < 3.2 );

    Text := 'Pizza: -3.14';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvFloat  );
    Confirm.IsTrue(  pv.ValueAsFloat < -3.1 );
    Confirm.IsTrue(  pv.ValueAsFloat > -3.2 );

    Text := 'Pizza: true';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvBoolean  );
    Confirm.IsTrue(  pv.ValueAsBoolean = true );

    Text := 'Pizza: false';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvBoolean  );
    Confirm.IsTrue(  pv.ValueAsBoolean = false );

    Text := 'Pizza: ptSalami';
    TParserHelper.ReadRonValue(Text, 1, pv);
    Confirm.IsTrue(  pv.ValueType = TParserValueType.pvUntyped  );
    Confirm.IsTrue(  pv.ValueAsUntyped = 'ptSalami' );
  finally
    pv.Free;
  end;
end;

procedure TReadRonValueTest.ReadRonArrayValueTest;
var
  Text : string;
  pv : TParserValue;
begin
  pv := TParserValue.Create;
  try

    Text := 'Pizza: [Salami, Ham, Cheese]';
    TParserHelper.ReadRonValue(Text, 1, pv);
    confirm.IsTrue( pv.ValueType = TParserValueType.pvUntypedArray);
    confirm.IsTrue( pv.ArrayLength = 3 );
    confirm.IsTrue( pv.ValueAsUntyped(0) = 'Salami' );
    confirm.IsTrue( pv.ValueAsUntyped(1) = 'Ham'    );
    confirm.IsTrue( pv.ValueAsUntyped(2) = 'Cheese' );

    Text := 'Pizza: ["Salami", "Ham", "Cheese"]';
    TParserHelper.ReadRonValue(Text, 1, pv);
    confirm.IsTrue( pv.ValueType = TParserValueType.pvStringArray);
    confirm.IsTrue( pv.ArrayLength = 3 );
    confirm.IsTrue( pv.ValueAsString(0) = 'Salami' );
    confirm.IsTrue( pv.ValueAsString(1) = 'Ham'    );
    confirm.IsTrue( pv.ValueAsString(2) = 'Cheese' );

    Text := 'Pizza: [7, 42, -105]';
    TParserHelper.ReadRonValue(Text, 1, pv);
    confirm.IsTrue( pv.ValueType = TParserValueType.pvIntegerArray);
    confirm.IsTrue( pv.ArrayLength = 3 );
    confirm.IsTrue( pv.ValueAsInteger(0) = 7    );
    confirm.IsTrue( pv.ValueAsInteger(1) = 42   );
    confirm.IsTrue( pv.ValueAsInteger(2) = -105 );

    Text := 'Pizza: [3.14, 0.2222, -105.0]';
    TParserHelper.ReadRonValue(Text, 1, pv);
    confirm.IsTrue( pv.ValueType = TParserValueType.pvFloatArray);
    confirm.IsTrue( pv.ArrayLength = 3 );

    Text := 'Pizza: [true, true, false,]';
    TParserHelper.ReadRonValue(Text, 1, pv);
    confirm.IsTrue( pv.ValueType = TParserValueType.pvBooleanArray);
    confirm.IsTrue( pv.ArrayLength = 3 );
    confirm.IsTrue( pv.ValueAsBoolean(0) = true  );
    confirm.IsTrue( pv.ValueAsBoolean(1) = true  );
    confirm.IsTrue( pv.ValueAsBoolean(2) = false );

  finally
    pv.Free;
  end;
end;



{ TRonParserTest }

procedure TRonParserTest.Setup;
begin
  inherited;
  Parser := TParser.Create;
end;

procedure TRonParserTest.TearDown;
begin
  Parser.Free;
  inherited;
end;

procedure TRonParserTest.ParseTestA;
var
  TestCompleted : boolean;
  TestData : string;
begin
  TestCompleted := false;

  TestData := '     Panel {                                   ' + EndOfLine +
              '                                               ' + EndOfLine +
              '       Pizza: [Pasta, Bacon , cheese ];        ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.KeyName = 'Pizza' then
    begin
      TestCompleted := true;
      Confirm.IsTrue( ParserValue.ValueType = TParserValueType.pvUntypedArray  );
      Confirm.IsTrue( ParserValue.ArrayLength = 3   );
    end;
  end;

  Parser.ParseText(TestData);

  if not(TestCompleted) then Confirm.Fail('Test not completed.');
end;

procedure TRonParserTest.ParseTestB;
var
  TestCompleted : boolean;
  TestData : string;
begin
  TestCompleted := false;

  TestData := '     Panel {                                   ' + EndOfLine +
              '                                               ' + EndOfLine +
              '       Pizza: """Freak Of Nature""";           ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.KeyName = 'Pizza' then
    begin
      TestCompleted := true;
      Confirm.IsTrue( ParserValue.ValueType = TParserValueType.pvString  );
      Confirm.IsTrue( ParserValue.ValueAsString = 'Freak Of Nature'  );
      Confirm.IsTrue( ParserValue.ObjectPath = 'Panel' );
    end;
  end;

  Parser.ParseText(TestData);

  if not(TestCompleted) then Confirm.Fail('Test not completed.');
end;




procedure TRonParserTest.ParseTestC;
var
  TestCompleted : boolean;
  TestData : string;
begin
  TestCompleted := false;

  TestData := '     Panel::TPanel {                           ' + EndOfLine +
              '       Knob::TKnob {                           ' + EndOfLine +
              '         Pizza: 42;                            ' + EndOfLine +
              '       }                                       ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.KeyName = 'Pizza' then
    begin
      TestCompleted := true;
      Confirm.IsTrue( ParserValue.ValueType = TParserValueType.pvInteger  );
      Confirm.IsTrue( ParserValue.ValueAsInteger = 42  );
      Confirm.IsTrue( ParserValue.ObjectPath = 'Panel.Knob' );
      Confirm.IsTrue( ParserValue.ObjectType = 'TKnob' );

    end;
  end;

  Parser.ParseText(TestData);

  if not(TestCompleted) then Confirm.Fail('Test not completed.');
end;


procedure TRonParserTest.ParseTestD;
var
  TestData : string;
  PartA, PartB, PartC : boolean;
begin
  PartA := false;
  PartB := false;
  PartC := false;

  TestData := '     Master.Panel::TPanel {                     ' + EndOfLine +
              '       Knob::TKnob {                            ' + EndOfLine +
              '         Label1::TLabel,                        ' + EndOfLine +
              '         Label2::TLabel,                        ' + EndOfLine +
              '         Label3::TLabel {                       ' + EndOfLine +
              '           Pizza: 42;                          ' + EndOfLine +
              '         }                                     ' + EndOfLine +
              '       }                                       ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.ObjectPath = 'Master.Panel.Knob.Label1' then PartA := true;
    if ParserValue.ObjectPath = 'Master.Panel.Knob.Label2' then PartB := true;
    if ParserValue.ObjectPath = 'Master.Panel.Knob.Label3' then PartC := true;
  end;

  Parser.ParseText(TestData);

  if not(PartA) then Confirm.Fail('Test not completed.');
  if not(PartB) then Confirm.Fail('Test not completed.');
  if not(PartC) then Confirm.Fail('Test not completed.');
end;


procedure TRonParserTest.ParseTestWithComments;
var
  TestCompleted : boolean;
  TestData : string;
begin
  TestCompleted := false;

  TestData := ' # This is a comment! It shouldn''t cause      ' + EndOfLine +
              ' # problems.                                   ' + EndOfLine +
              ' #' + EndOfLine +
              '     Panel {                                   ' + EndOfLine +
              '       Pizza: [Pasta, Bacon , cheese ];        ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.KeyName = 'Pizza' then
    begin
      TestCompleted := true;
      Confirm.IsTrue( ParserValue.ValueType = TParserValueType.pvUntypedArray  );
      Confirm.IsTrue( ParserValue.ArrayLength = 3   );
    end;
  end;

  Parser.ParseText(TestData);

  if not(TestCompleted) then Confirm.Fail('Test not completed.');
end;

procedure TRonParserTest.ParseTestWithMultiLineComments;
var
  TestCompleted : boolean;
  TestData : string;
begin
  TestCompleted := false;

  TestData := ' # This is a comment! It shouldn''t cause      ' + EndOfLine +
              ' # problems.                                   ' + EndOfLine +
              ' #                                             ' + EndOfLine +
              '     Panel {                                   ' + EndOfLine +
              '       /*   Pizza is the best!     */          ' + EndOfLine +
              '       Pizza: [Pasta, Bacon , cheese ];        ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              ' /*                                            ' + EndOfLine +
              '   This comment shouldn''t cause any           ' + EndOfLine +
              '   drama either.                               ' + EndOfLine +
              ' */                                            ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';


  Parser.OnValue := procedure(Sender : TObject; const ParserValue : TParserValue)
  begin
    if ParserValue.KeyName = 'Pizza' then
    begin
      TestCompleted := true;
      Confirm.IsTrue( ParserValue.ValueType = TParserValueType.pvUntypedArray  );
      Confirm.IsTrue( ParserValue.ArrayLength = 3   );
    end;
  end;

  Parser.ParseText(TestData);

  if not(TestCompleted) then Confirm.Fail('Test not completed.');
end;

end.
