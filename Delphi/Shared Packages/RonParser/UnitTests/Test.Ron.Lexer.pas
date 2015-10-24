unit Test.Ron.Lexer;

interface

uses
  WatchTower,
  ron.Lexer;

type
  TRonLowLevelFunctions = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure ObjectPathTest;
    [Test]
    procedure ObjectTypeTest;
  end;

  TRonStringTest = class(TWatchTowerTest)
  public
    [Test]
    procedure UnescapeBasicStringTest;

    [Test]
    procedure UnescapeMultilineStringTest;

    [Test]
    procedure UnescapeMultilineLiteralStringTest;
  end;

  TRonHelperTest = class(TWatchTowerTest)
  public
    [Test]
    procedure TokenToString;

    [Test]
    procedure TokenToInteger;

    [Test]
    procedure TokenToFloat;

    [Test]
    procedure TokenToBoolean;

    [Test]
    procedure TokenToUntyped;
  end;

  TTokenMatcherTest = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure MatchComment;

    [Test]
    procedure MatchMultiLineComment;

    [Test]
    procedure MatchWhiteSpace;

    [Test]
    procedure MatchMultiObjectOpen;

    [Test]
    procedure MatchObjectOpen;

    [Test]
    procedure MatchObjectClose;

    [Test]
    procedure MatchObjectKey;

    [Test]
    procedure MatchIntegerValue;

    [Test]
    procedure MatchFloatValueA;

    [Test]
    procedure MatchBooleanValue;

    [Test]
    procedure MatchUntypedValue;

    [Test]
    procedure MatchBasicString;

    [Test]
    procedure MatchMultiLineString;

    [Test]
    procedure MatchLiteralString;

    [Test]
    procedure MatchMultiLineLiteralString;

    [Test]
    procedure MatchSymbol;
  end;


  TTokenizer_NextToken = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure NextTokenTestEmptyString;

    [Test]
    procedure NextTokenTestAllWhiteSpace;

    [Test]
    procedure BasicData;

    [Test]
    procedure NestedObject;

    [Test]
    procedure ObjectWithArray;

    [Test]
    procedure NextFloatToken;

    [Test]
    procedure ArrayOfInteger;

    [Test]
    procedure ArrayOfFloat;
  end;

  TSyntaxCheckerTest = class(TWatchTowerTest)
  private
    SyntaxChecker : TSyntaxChecker;
  public
    procedure Setup; override;

    [Test]
    procedure EmptyString;

    [Test]
    procedure BasicData;

    [Test]
    procedure NestedObject;

    [Test]
    procedure ObjectWithArray;
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm;

{ TRonLowLevelFunctions }

procedure TRonLowLevelFunctions.ObjectPathTest;
var
  path : string;
begin
  path := TRonHelper.GetRonObjectPath('Panel1.Knob1.Label::TVstLabel');
  Confirm.IsTrue( path = 'Panel1.Knob1.Label');

  path := TRonHelper.GetRonObjectPath('Panel1.Knob1.Label');
  Confirm.IsTrue( path = 'Panel1.Knob1.Label');

  path := TRonHelper.GetRonObjectPath('Panel1');
  Confirm.IsTrue( path = 'Panel1');

  path := TRonHelper.GetRonObjectPath('');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectPath('::TVstPanel');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectPath('  Pizza::TVstPanel');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectPath('P**izza::TVstPanel');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectPath('Pizza.::TVstPanel');
  Confirm.IsTrue( path = '');
end;

procedure TRonLowLevelFunctions.ObjectTypeTest;
var
  path : string;
begin
  path := TRonHelper.GetRonObjectType('Panel1.Knob1.Label::TVstLabel');
  Confirm.IsTrue( path = 'TVstLabel');

  path := TRonHelper.GetRonObjectType('Panel1.Knob1.Label');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectType('Panel1');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectType('');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectType('::TVstPanel');
  Confirm.IsTrue( path = 'TVstPanel');

  path := TRonHelper.GetRonObjectType('  Pizza::TVstPanel');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectType('P**izza::TVstPanel');
  Confirm.IsTrue( path = '');

  path := TRonHelper.GetRonObjectType('Pizza.::TVstPanel');
  Confirm.IsTrue( path = '');
end;



{ TTokenMatcherTest }

procedure TTokenMatcherTest.MatchComment;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_Comment('# Tom Jones is dead ' + ENDOFLINE + ' some more text ', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '# Tom Jones is dead ' );
  Confirm.IsTrue( PosIncrement = 20  );

  Confirm.IsFalse( TTokenizer.Match_Comment('this should fail # Fefef', 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchMultiLineComment;
var
  TestData : string;
  MatchedText   : string;
  PosIncrement : integer;
begin
  TestData := '  /*                                           ' + EndOfLine +
              '    This is a multi-line comment.              ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '  */                                           ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';

  Confirm.IsTrue( TTokenizer.Match_MultiLineComment(TestData, 3, MatchedText, PosIncrement) );

end;

procedure TTokenMatcherTest.MatchWhiteSpace;
var
  MatchedText   : string;
  PosIncrement : integer;
  Text : string;
begin
  Text := ' ' + EndOfLine + ' Pizza!';
  Confirm.IsTrue( TTokenizer.Match_WhiteSpace(Text, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '' );
  Confirm.IsTrue( Text[1+PosIncrement] = 'P' );
end;

procedure TTokenMatcherTest.MatchMultiObjectOpen;
var
  MatchedText   : string;
  PosIncrement : integer;
  Text : string;
begin
  Text := 'Panel1::TVst, Knob1::TKnob {';
  Confirm.IsTrue( TTokenizer.Match_MultiObjectOpen(Text, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel1::TVst' );
  Confirm.IsTrue( Text[1+PosIncrement] = 'K' );


  Text := 'Panel1::TVst, ' + EndOfLine + 'Zanel2.Label::TVst, ' + EndOfLine + 'Knob1::TKnob {';
  Confirm.IsTrue( TTokenizer.Match_MultiObjectOpen(Text, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel1::TVst' );
  Confirm.IsTrue( Text[1+PosIncrement] = 'Z' );


  Text := 'Panel1::TVst, ' + EndOfLine + 'Zanel2.Label::TVst, ' + EndOfLine + 'Knob1::TKnob,';
  Confirm.IsFalse( TTokenizer.Match_MultiObjectOpen(Text, 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchObjectOpen;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_ObjectOpen('Panel { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel' );
  Confirm.IsTrue( PosIncrement = 7 );

  Confirm.IsTrue( TTokenizer.Match_ObjectOpen('Panel.Knob { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel.Knob' );
  Confirm.IsTrue( PosIncrement = 7+5 );

  Confirm.IsTrue( TTokenizer.Match_ObjectOpen('Panel.Knob.Label { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel.Knob.Label' );
  Confirm.IsTrue( PosIncrement = 7+5+6 );

  Confirm.IsTrue( TTokenizer.Match_ObjectOpen('Panel.Knob.Label::TLabel { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'Panel.Knob.Label::TLabel' );
  Confirm.IsTrue( PosIncrement = 7+5+6+8 );


  Confirm.IsFalse( TTokenizer.Match_ObjectOpen('Panel. { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_ObjectOpen('Panel.2Test { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_ObjectOpen('Panel.Knob::TKnob::TPizza { ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchObjectClose;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_ObjectClose('}' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '' );
  Confirm.IsTrue( PosIncrement = 1 );
end;



procedure TTokenMatcherTest.MatchObjectKey;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_ObjectKey('JamesBrown: ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'JamesBrown' );
  Confirm.IsTrue( PosIncrement = 11 );

  Confirm.IsTrue( TTokenizer.Match_ObjectKey('JamesBrown: ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'JamesBrown' );
  Confirm.IsTrue( PosIncrement = 11 );

  Confirm.IsTrue( TTokenizer.Match_ObjectKey('JamesBrown: ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'JamesBrown' );
  Confirm.IsTrue( PosIncrement = 11 );

  Confirm.IsFalse( TTokenizer.Match_ObjectKey('JamesBrown ef = ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_ObjectKey('2JamesBrown = ' + ENDOFLINE, 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_ObjectKey('= ' + ENDOFLINE, 1, MatchedText, PosIncrement) );

  Confirm.IsFalse( TTokenizer.Match_ObjectKey('James Brown ' + ENDOFLINE + '=', 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchIntegerValue;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_IntegerValue('56;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '56' );
  Confirm.IsTrue( PosIncrement = 2 );

  Confirm.IsTrue( TTokenizer.Match_IntegerValue('+256,', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '+256' );
  Confirm.IsTrue( PosIncrement = 4 );

  Confirm.IsTrue( TTokenizer.Match_IntegerValue('-256)', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '-256' );
  Confirm.IsTrue( PosIncrement = 4 );
end;

procedure TTokenMatcherTest.MatchFloatValueA;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_FloatValue('56.2;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '56.2' );
  Confirm.IsTrue( PosIncrement = 4 );

  Confirm.IsTrue( TTokenizer.Match_FloatValue('+56.0,', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '+56.0' );
  Confirm.IsTrue( PosIncrement = 5 );

  Confirm.IsFalse( TTokenizer.Match_FloatValue('-256;', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_FloatValue('256;', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_FloatValue('256.;', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_FloatValue('.256;', 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchBooleanValue;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_BooleanValue('true;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'true' );
  Confirm.IsTrue( PosIncrement = 4 );

  Confirm.IsTrue( TTokenizer.Match_BooleanValue('tRue;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'tRue' );
  Confirm.IsTrue( PosIncrement = 4 );

  Confirm.IsTrue( TTokenizer.Match_BooleanValue('false ;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'false' );
  Confirm.IsTrue( PosIncrement = 5 );

  Confirm.IsTrue( TTokenizer.Match_BooleanValue('falsE ]', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'falsE' );
  Confirm.IsTrue( PosIncrement = 5 );

  Confirm.IsFalse( TTokenizer.Match_BooleanValue('f', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_BooleanValue('t', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_BooleanValue('0', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_BooleanValue('1', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_BooleanValue('y', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_BooleanValue('n', 1, MatchedText, PosIncrement) );
end;

procedure TTokenMatcherTest.MatchUntypedValue;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_UntypedValue('JamesBrown;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'JamesBrown' );
  Confirm.IsTrue( PosIncrement = 10 );

  Confirm.IsTrue( TTokenizer.Match_UntypedValue('J4mes_rown;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'J4mes_rown' );
  Confirm.IsTrue( PosIncrement = 10 );

  Confirm.IsTrue( TTokenizer.Match_UntypedValue('_4mes_rown;', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = '_4mes_rown' );
  Confirm.IsTrue( PosIncrement = 10 );

  Confirm.IsFalse( TTokenizer.Match_UntypedValue('44mes_rown', 1, MatchedText, PosIncrement) );
  Confirm.IsFalse( TTokenizer.Match_UntypedValue('44', 1, MatchedText, PosIncrement) );

end;

procedure TTokenMatcherTest.MatchBasicString;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_BasicString('"JamesBrown"', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'JamesBrown' );
  Confirm.IsTrue( PosIncrement = 12 );

  Confirm.IsTrue( TTokenizer.Match_BasicString('" JamesBrown "', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = ' JamesBrown ' );
  Confirm.IsTrue( PosIncrement = 14 );

  Confirm.IsTrue( TTokenizer.Match_BasicString('" \"JamesBrown\" "', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = ' \"JamesBrown\" ' );
  Confirm.IsTrue( PosIncrement = 18 );
end;


procedure TTokenMatcherTest.MatchMultiLineString;
var
  Text : string;
  MatchedText   : string;
  PosIncrement : integer;
begin
  Text := '""" James brown ' + EndOfLine + ' was dead. """ ';
  Confirm.IsTrue( TTokenizer.Match_MultiLineString(Text, 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = ' James brown ' + EndOfLine + ' was dead. ' );
  Confirm.IsTrue( PosIncrement = Length(MatchedText)+6 );
end;

procedure TTokenMatcherTest.MatchLiteralString;
var
  Text : string;
  MatchedText   : string;
  PosIncrement : integer;
begin
  Text := 'Pizza: ''TomJones''';
  Confirm.IsTrue( TTokenizer.Match_LiteralString(Text, 8, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'TomJones' );
  Confirm.IsTrue( PosIncrement = Length(MatchedText)+2 );
end;

procedure TTokenMatcherTest.MatchMultiLineLiteralString;
var
  Text : string;
  MatchedText   : string;
  PosIncrement : integer;
begin
  Text := 'Pizza: ''''''TomJones''''''';

  Confirm.IsTrue( TTokenizer.Match_MultiLineLiteralString(Text, 8, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = 'TomJones' );
  Confirm.IsTrue( PosIncrement = Length(MatchedText)+6 );
end;



procedure TTokenMatcherTest.MatchSymbol;
var
  MatchedText   : string;
  PosIncrement : integer;
begin
  Confirm.IsTrue( TTokenizer.Match_Symbol(';', 1, MatchedText, PosIncrement) );
  Confirm.IsTrue( MatchedText  = ';');
  Confirm.IsTrue( PosIncrement = Length(MatchedText) );

end;





{ TTokenizer_NextToken }

procedure TTokenizer_NextToken.NextTokenTestEmptyString;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 0;
  TestData := '';
  Confirm.IsTrue( TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttEndOfFile  );

  Confirm.IsFalse( TTokenizer.NextToken(TestData, IndexPos, Token)  );
end;

procedure TTokenizer_NextToken.NextTokenTestAllWhiteSpace;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 0;
  TestData := '    ' + EndOfLine + '         ';
  Confirm.IsTrue( TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttEndOfFile  );
end;

procedure TTokenizer_NextToken.BasicData;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 0;
  TestData := '     Panel.Knob1::TVstKnob,                     ' + EndOfLine +
              '     Panel.Knob2::TVstKnob,                     ' + EndOfLine +
              '     Panel.Knob3::TVstKnob {                    ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttMultiObjectOpen   );
  Confirm.IsTrue(  Token.TokenData = 'Panel.Knob1::TVstKnob'         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttMultiObjectOpen   );
  Confirm.IsTrue(  Token.TokenData = 'Panel.Knob2::TVstKnob'         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectOpen        );
  Confirm.IsTrue(  Token.TokenData = 'Panel.Knob3::TVstKnob'         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectClose       );
  Confirm.IsTrue(  Token.TokenData = '' );

  // End of file.
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(   Token.TokenType = TTokenType.ttEndOfFile         );
end;

procedure TTokenizer_NextToken.NestedObject;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 0;
  TestData := '     Panel {                                   ' + EndOfLine +
              '       Knob {                                  ' + EndOfLine +
              '         Pizza: "Bacon";                       ' + EndOfLine +
              '       }                                       ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectOpen        );
  Confirm.IsTrue(  Token.TokenData = 'Panel'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectOpen        );
  Confirm.IsTrue(  Token.TokenData = 'Knob'                         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectKey         );
  Confirm.IsTrue(  Token.TokenData = 'Pizza'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttBasicString       );
  Confirm.IsTrue(  Token.TokenData = 'Bacon'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectClose       );
  Confirm.IsTrue(  Token.TokenData = '' );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectClose       );
  Confirm.IsTrue(  Token.TokenData = '' );

  // End of file.
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(   Token.TokenType = TTokenType.ttEndOfFile         );
end;

procedure TTokenizer_NextToken.ObjectWithArray;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 0;
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

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectOpen        );
  Confirm.IsTrue(  Token.TokenData = 'Panel'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectKey         );
  Confirm.IsTrue(  Token.TokenData = 'Pizza'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = '['                            );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttUntypedValue      );
  Confirm.IsTrue(  Token.TokenData = 'Pasta'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttUntypedValue      );
  Confirm.IsTrue(  Token.TokenData = 'Bacon'                        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttUntypedValue      );
  Confirm.IsTrue(  Token.TokenData = 'cheese'                       );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ']'                            );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectClose       );
  Confirm.IsTrue(  Token.TokenData = '' );

  // End of file.
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(   Token.TokenType = TTokenType.ttEndOfFile         );
end;

procedure TTokenizer_NextToken.NextFloatToken;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 9;
  TestData := 'Pizza = 3.14';
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttFloatValue        );
end;

procedure TTokenizer_NextToken.ArrayOfInteger;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 1;
  TestData := 'Bounds: [0, 0, 100, 300];';
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectKey         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = '['                            );

  // 3
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttIntegerValue      );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 1
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttIntegerValue      );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 4
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttIntegerValue      );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 6
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttIntegerValue      );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ']'                            );
end;

procedure TTokenizer_NextToken.ArrayOfFloat;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
begin
  IndexPos := 1;
  TestData := 'Bounds: [0.0, 0.0, 100.7, 300.3];';
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttObjectKey         );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = '['                            );

  // 3
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttFloatValue        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 1
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttFloatValue        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 4
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttFloatValue        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ','                            );

  // 6
  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttFloatValue        );

  Confirm.IsTrue(  TTokenizer.NextToken(TestData, IndexPos, Token)  );
  Confirm.IsTrue(  Token.TokenType = TTokenType.ttSymbol            );
  Confirm.IsTrue(  Token.TokenData = ']'                            );
end;






{ TSyntaxCheckerTest }

procedure TSyntaxCheckerTest.Setup;
begin
  inherited;
  SyntaxChecker.ResetState;
end;


procedure TSyntaxCheckerTest.EmptyString;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
  ErrorMsg : string;
begin
  IndexPos := 0;
  TestData := '';

  while (TTokenizer.NextToken(TestData, IndexPos, Token)) do
  begin
    Confirm.IsTrue( SyntaxChecker.IsSyntaxValid(Token, ErrorMsg)  );
  end;
end;

procedure TSyntaxCheckerTest.BasicData;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
  ErrorMsg : string;
begin
  IndexPos := 0;
  TestData := '     Panel.Knob1::TVstKnob,                    ' + EndOfLine +
              '     Panel.Knob2::TVstKnob,                    ' + EndOfLine +
              '     Panel.Knob3::TVstKnob {                   ' + EndOfLine +
              '                                               ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';

  while (TTokenizer.NextToken(TestData, IndexPos, Token)) do
  begin
    Confirm.IsTrue( SyntaxChecker.IsSyntaxValid(Token, ErrorMsg)  );
  end;
end;

procedure TSyntaxCheckerTest.NestedObject;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
  ErrorMsg : string;
begin
  IndexPos := 0;
  TestData := '     Panel {                                   ' + EndOfLine +
              '       Knob {                                  ' + EndOfLine +
              '         Pizza: "Bacon";                       ' + EndOfLine +
              '       }                                       ' + EndOfLine +
              '     }                                         ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ' + EndOfLine +
              '                                               ';

  while (TTokenizer.NextToken(TestData, IndexPos, Token)) do
  begin
    Confirm.IsTrue( SyntaxChecker.IsSyntaxValid(Token, ErrorMsg)  );
  end;
end;


procedure TSyntaxCheckerTest.ObjectWithArray;
var
  TestData : string;
  Token : TToken;
  IndexPos : integer;
  ErrorMsg : string;
begin
  IndexPos := 0;
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

  while (TTokenizer.NextToken(TestData, IndexPos, Token)) do
  begin
    Confirm.IsTrue( SyntaxChecker.IsSyntaxValid(Token, ErrorMsg)  );
  end;
end;



{ TRonStringTest }

procedure TRonStringTest.UnescapeBasicStringTest;
const
  UnicodeTab = '$0009';
var
  tr : string;
  text : string;
begin
  tr := TRonString.UnescapeBasicString('James \"Brown\" is dead.');
  Confirm.IsTrue(tr = 'James "Brown" is dead.');

  tr := TRonString.UnescapeBasicString('\t James is dead.');
  text := Char(StrToInt(UnicodeTab)) + ' James is dead.';
  Confirm.IsTrue(tr = text);

  tr := TRonString.UnescapeBasicString('\T James is dead.');
  text := Char(StrToInt(UnicodeTab)) + ' James is dead.';
  Confirm.IsTrue(tr = text);

  tr := TRonString.UnescapeBasicString('\\\\ James is dead.');
  text := '\\\\ James is dead.';
  Confirm.IsTrue(tr = text);
end;

procedure TRonStringTest.UnescapeMultilineStringTest;
var
  input : string;
  tr : string;
  text : string;
begin
  Input := '   ' + EndOfLine + '   Pizza is the best.' + EndOfLine + '   Pasta is ok.';
  tr := TRonString.UnescapeMultilineString(Input);
  text := '   Pizza is the best.' + EndOfLine + '   Pasta is ok.';
  Confirm.IsTrue(tr = text);

  Input := '   ' + EndOfLine + '   Pizza is the best.\' + EndOfLine + '   Pasta is ok.';
  tr := TRonString.UnescapeMultilineString(Input);
  text := '   Pizza is the best.' + 'Pasta is ok.';
  Confirm.IsTrue(tr = text);
end;

procedure TRonStringTest.UnescapeMultilineLiteralStringTest;
var
  input : string;
  tr : string;
  text : string;
begin
  Input := '   ' + EndOfLine + '   Pizza is the best.' + EndOfLine + '   Pasta is ok.';
  tr := TRonString.UnescapeMultilineLiteralString(Input);
  text := '   Pizza is the best.' + EndOfLine + '   Pasta is ok.';
  Confirm.IsTrue(tr = text);

  Input := '   ' + EndOfLine + '   Pizza is the best.\' + EndOfLine + '   Pasta is ok.';
  tr := TRonString.UnescapeMultilineLiteralString(Input);
  text := '   Pizza is the best.\' + EndOfLine + '   Pasta is ok.';
  Confirm.IsTrue(tr = text);
end;



{ TRonHelperTest }

procedure TRonHelperTest.TokenToBoolean;
var
  Token : TToken;
  v : boolean;
begin
  Token.Position := 0;
  Token.TokenType := TTokenType.ttBooleanValue;
  Token.TokenData := 'true';
  v := TRonHelper.TokenToBoolean(Token);
  Confirm.IsTrue( v = true );

  Token.Position := 0;
  Token.TokenType := TTokenType.ttBooleanValue;
  Token.TokenData := 'f';
  v := TRonHelper.TokenToBoolean(Token);
  Confirm.IsTrue( v = false );
end;

procedure TRonHelperTest.TokenToFloat;
var
  Token : TToken;
  v : single;
begin
  Token.Position := 0;
  Token.TokenType := TTokenType.ttFloatValue;
  Token.TokenData := '3.14';
  v := TRonHelper.TokenToFloat(Token);
  Confirm.IsTrue( v > 3.1 );
  Confirm.IsTrue( v < 3.2 );
end;

procedure TRonHelperTest.TokenToInteger;
var
  Token : TToken;
  v : integer;
begin
  Token.Position := 0;
  Token.TokenType := TTokenType.ttIntegerValue;
  Token.TokenData := '42';
  v := TRonHelper.TokenToInteger(Token);
  Confirm.IsTrue( v = 42 );
end;

procedure TRonHelperTest.TokenToString;
var
  Token : TToken;
  str : string;
begin
  Token.Position := 0;
  Token.TokenType := TTokenType.ttBasicString;
  Token.TokenData := 'James Brown';
  str := TRonHelper.TokenToString(Token);
  Confirm.IsTrue( str = 'James Brown' );
end;

procedure TRonHelperTest.TokenToUntyped;
var
  Token : TToken;
  str : string;
begin
  Token.Position := 0;
  Token.TokenType := TTokenType.ttUntypedValue;
  Token.TokenData := 'vaFruit';
  str := TRonHelper.TokenToString(Token);
  Confirm.IsTrue( str = 'vaFruit' );
end;


end.
