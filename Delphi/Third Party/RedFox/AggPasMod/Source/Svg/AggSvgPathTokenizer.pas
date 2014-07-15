unit AggSvgPathTokenizer;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  SysUtils,
  AggBasics,
  AggSvgException;


const
  CSvgCommands = '+-MmZzLlHhVvCcSsQqTtAaFfPp';
  CSvgNumeric = '.Ee0123456789';
  CSvgSeparators = ' ,'#9#10#13;

type
  // SVG path tokenizer.
  // Example:
  //
  // agg::svg::TPathTokenizer tok;
  //
  // tok.set_str("M-122.304 84.285L-122.304 84.285 122.203 86.179 ");
  // while(tok.Next)
  // {
  // printf("command='%c' number=%f\n",
  // tok.LastCommand,
  // tok.LastNumber);
  // }
  //
  // The tokenizer does all the routine job of parsing the SVG paths.
  // It doesn't recognize any graphical primitives, it even doesn't know
  // anything about pairs of coordinates (X,Y). The purpose of this class
  // is to tokenize the numeric values and commands. SVG paths can
  // have single numeric values for Horizontal or Vertical LineTo commands
  // as well as more than two coordinates (4 or 6) for Bezier curves
  // depending on the semantics of the command.
  // The behaviour is as follows:
  //
  // Each call to Next returns true if there's new command or new numeric
  // value or false when the path ends. How to interpret the result
  // depends on the sematics of the command. For example, command "C"
  // (cubic Bezier curve) implies 6 floating point numbers preceded by this
  // command. If the command assumes no arguments (like z or Z) the
  // the LastNumber values won't change, that is, last_number always
  // returns the last recognized numeric value, so does LastCommand.
  TPathTokenizer = class
  private
    FSeparatorsMask, FCommandsMask, FNumericMask:
      array [0..256 div 8 - 1] of AnsiChar;

    FPath: PAnsiChar;
    FLastNumber: Double;
    FLastCommand: AnsiChar;
  public
    constructor Create;

    procedure SetPathStr(Str: PAnsiChar);

    function Next: Boolean; overload;
    function Next(Cmd: AnsiChar): Double; overload;

    function LastCommand: AnsiChar;
    function LastNumber: Double;

    procedure InitCharMask(Mask, Char_set: PAnsiChar);

    function Contains_(Mask: PAnsiChar; C: Cardinal): Boolean;
    function IsCommand(C: Cardinal): Boolean;
    function IsNumeric(C: Cardinal): Boolean;
    function IsSeparator(C: Cardinal): Boolean;
    function ParseNumber: Boolean;
  end;

implementation


{ TPathTokenizer }

constructor TPathTokenizer.Create;
begin
  FPath := nil;
  FLastCommand := #0;
  FLastNumber := 0.0;

  InitCharMask(@FCommandsMask[0], @CSvgCommands[1]);
  InitCharMask(@FNumericMask[0], @CSvgNumeric[1]);
  InitCharMask(@FSeparatorsMask[0], @CSvgSeparators[1]);
end;

procedure TPathTokenizer.SetPathStr;
begin
  FPath := Str;
  FLastCommand := #0;
  FLastNumber := 0.0;
end;

function TPathTokenizer.Next: Boolean;
var
  Buf: array [0..99] of AnsiChar;

begin
  Result := False;

  if FPath = nil then
    Result := False;

  // Skip all white spaces and other garbage
  while (FPath^ <> #0) and not IsCommand(Cardinal(FPath^)) and
    not IsNumeric(Cardinal(FPath^)) do
  begin
    if not IsSeparator(Cardinal(FPath^)) then
      raise TSvgException.Create(Format('TPathTokenizer::next : Invalid '
        + 'Character %c', [FPath^]));

    Inc(PtrComp(FPath));
  end;

  if FPath^ = #0 then
    Exit;

  if IsCommand(Cardinal(FPath^)) then
  begin
    // Check if the command is a numeric sign character
    if (FPath^ = '-') or (FPath^ = '+') then
    begin
      Result := ParseNumber;

      Exit;
    end;

    FLastCommand := FPath^;

    Inc(PtrComp(FPath));

    while (FPath^ <> #0) and IsSeparator(Cardinal(FPath^)) do
      Inc(PtrComp(FPath));

    if FPath^ = #0 then
    begin
      Result := True;

      Exit;
    end;
  end;

  Result := ParseNumber;
end;

function TPathTokenizer.Next(Cmd: AnsiChar): Double;
var
  Buf: array [0..99] of AnsiChar;

begin
  if not Next then
    raise TSvgException.Create(PAnsiChar('parse_path: Unexpected end of path'));

  if LastCommand <> Cmd then
    raise TSvgException.Create(Format('parse_path: Command %c: bad or missing '
      + 'parameters', [Cmd]));

  Result := LastNumber;
end;

function TPathTokenizer.LastCommand;
begin
  Result := FLastCommand;
end;

function TPathTokenizer.LastNumber;
begin
  Result := FLastNumber;
end;

procedure TPathTokenizer.InitCharMask;
var
  C: Cardinal;

begin
  FillChar(Mask^, 256 div 8, 0);

  while Char_set^ <> #0 do
  begin
    C := PInt8u(Char_set)^;

    PInt8u(PtrComp(Mask) + (C shr 3))^ := PInt8u(PtrComp(Mask) + (C shr 3)
      )^ or (1 shl (C and 7));

    Inc(PtrComp(Char_set));
  end;
end;

function TPathTokenizer.Contains_;
begin
  Result := (PInt8u(PtrComp(Mask) + (C shr 3) and (256 div 8 - 1))^ and
    (1 shl (C and 7))) <> 0;
end;

function TPathTokenizer.IsCommand;
begin
  Result := Contains_(@FCommandsMask[0], C);
end;

function TPathTokenizer.IsNumeric;
begin
  Result := Contains_(@FNumericMask[0], C);
end;

function TPathTokenizer.IsSeparator;
begin
  Result := Contains_(@FSeparatorsMask[0], C);
end;

function TPathTokenizer.ParseNumber;
var
  Buf: array [0..255] of AnsiChar; // Should be enough for any number
  BufPointer: PAnsiChar;
begin
  BufPointer := @Buf[0];

  // Copy all sign characters
  while (PtrComp(BufPointer) < PtrComp(@Buf[0]) + 255) and
    ((FPath^ = '-') or (FPath^ = '+')) do
  begin
    BufPointer^ := FPath^;

    Inc(PtrComp(BufPointer));
    Inc(PtrComp(FPath));
  end;

  // Copy all numeric characters
  while (PtrComp(BufPointer) < PtrComp(@Buf[0]) + 255) and
    IsNumeric(Cardinal(FPath^)) do
  begin
    BufPointer^ := FPath^;

    Inc(PtrComp(BufPointer));
    Inc(PtrComp(FPath));
  end;

  BufPointer^ := #0;

  FLastNumber := GetDouble(Pointer(PAnsiChar(@Buf[0])));

  Result := True;
end;

end.
