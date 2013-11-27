unit eeSerialTools;

interface




//Generates a random sequence of Alpha-numeric charactors suitable for a serial number.
function GenerateSerialCore(CoreLength:integer):AnsiString;

function CalcCheckSum(Text:AnsiString):cardinal;

function AnsiCharToInteger(Text:AnsiChar):integer;
function IntegerToAnsiChar(x:integer):string;


implementation



// NOTE: The manual conversion between an integer and ansichar looks pretty crude.
// But I figure a simple hardcoded approach like this will be more resilient when
// upgrading to a unicode system. And perhaps dealing with locale differences if
// trying using a charactor's "charactor code".
                                               
function AnsiCharToInteger(Text:AnsiChar):integer;
begin
  result := 0;
  if Text = 'a' then result := 1;
  if Text = 'b' then result := 2;
  if Text = 'c' then result := 3;
  if Text = 'd' then result := 4;
  if Text = 'e' then result := 5;
  if Text = 'f' then result := 6;
  if Text = 'g' then result := 7;
  if Text = 'h' then result := 8;
  if Text = 'i' then result := 9;
  if Text = 'j' then result := 10;
  if Text = 'k' then result := 11;
  if Text = 'l' then result := 12;
  if Text = 'm' then result := 13;
  if Text = 'n' then result := 14;
  if Text = 'o' then result := 15;
  if Text = 'p' then result := 16;
  if Text = 'q' then result := 17;
  if Text = 'r' then result := 18;
  if Text = 's' then result := 19;
  if Text = 't' then result := 20;
  if Text = 'u' then result := 21;
  if Text = 'v' then result := 22;
  if Text = 'w' then result := 23;
  if Text = 'x' then result := 24;
  if Text = 'y' then result := 25;
  if Text = 'z' then result := 26;
  if Text = 'A' then result := 27;
  if Text = 'B' then result := 28;
  if Text = 'C' then result := 29;
  if Text = 'D' then result := 30;
  if Text = 'E' then result := 31;
  if Text = 'F' then result := 32;
  if Text = 'G' then result := 33;
  if Text = 'H' then result := 34;
  if Text = 'I' then result := 35;
  if Text = 'J' then result := 36;
  if Text = 'K' then result := 37;
  if Text = 'L' then result := 38;
  if Text = 'M' then result := 39;
  if Text = 'N' then result := 40;
  if Text = 'O' then result := 41;
  if Text = 'P' then result := 42;
  if Text = 'Q' then result := 43;
  if Text = 'R' then result := 44;
  if Text = 'S' then result := 45;
  if Text = 'T' then result := 46;
  if Text = 'U' then result := 47;
  if Text = 'V' then result := 48;
  if Text = 'W' then result := 49;
  if Text = 'X' then result := 50;
  if Text = 'Y' then result := 51;
  if Text = 'Z' then result := 52;
  if Text = '1' then result := 53;
  if Text = '2' then result := 54;
  if Text = '3' then result := 55;
  if Text = '4' then result := 56;
  if Text = '5' then result := 57;
  if Text = '6' then result := 58;
  if Text = '7' then result := 59;
  if Text = '8' then result := 60;
  if Text = '9' then result := 61;
  if Text = '0' then result := 62;
end;

function IntegerToAnsiChar(x:integer):string;
begin
  result := 0;
  if x = 1  then result := 'a';
  if x = 2  then result := 'b';
  if x = 3  then result := 'c';
  if x = 4  then result := 'd';
  if x = 5  then result := 'e';
  if x = 6  then result := 'f';
  if x = 7  then result := 'g';
  if x = 8  then result := 'h';
  if x = 9  then result := 'i';
  if x = 10 then result := 'j';
  if x = 11 then result := 'k';
  if x = 12 then result := 'l';
  if x = 13 then result := 'm';
  if x = 14 then result := 'n';
  if x = 15 then result := 'o';
  if x = 16 then result := 'p';
  if x = 17 then result := 'q';
  if x = 18 then result := 'r';
  if x = 19 then result := 's';
  if x = 20 then result := 't';
  if x = 21 then result := 'u';
  if x = 22 then result := 'v';
  if x = 23 then result := 'w';
  if x = 24 then result := 'x';
  if x = 25 then result := 'y';
  if x = 26 then result := 'z';
  if x = 27 then result := 'A';
  if x = 28 then result := 'B';
  if x = 29 then result := 'C';
  if x = 30 then result := 'D';
  if x = 31 then result := 'E';
  if x = 32 then result := 'F';
  if x = 33 then result := 'G';
  if x = 34 then result := 'H';
  if x = 35 then result := 'I';
  if x = 36 then result := 'J';
  if x = 37 then result := 'K';
  if x = 38 then result := 'L';
  if x = 39 then result := 'M';
  if x = 40 then result := 'N';
  if x = 41 then result := 'O';
  if x = 42 then result := 'P';
  if x = 43 then result := 'Q';
  if x = 44 then result := 'R';
  if x = 45 then result := 'S';
  if x = 46 then result := 'T';
  if x = 47 then result := 'U';
  if x = 48 then result := 'V';
  if x = 49 then result := 'W';
  if x = 50 then result := 'X';
  if x = 51 then result := 'Y';
  if x = 52 then result := 'Z';
  if x = 53 then result := '1';
  if x = 54 then result := '2';
  if x = 55 then result := '3';
  if x = 56 then result := '4';
  if x = 57 then result := '5';
  if x = 58 then result := '6';
  if x = 59 then result := '7';
  if x = 60 then result := '8';
  if x = 61 then result := '9';
  if x = 62 then result := '0';
end;

function GenerateSerialCore(CoreLength:integer):AnsiString;
var
  s:AnsiString;
  c1, x: Integer;
begin
  Randomize;

  s := '';

  for c1 := 0 to CoreLength - 1 do
  begin
    //generate a random number to pick alpha-numeric charactor.
    x := random(35);
    // inc x to the first block of valid charactors (numerals)
    x := x + 48;
    // if necessary, inc x to 2nd block of valid charactors (uppercase letters)
    if x > 57 then x := x + 8;

    //Some exceptions
    if x = 48 then x := 65; //Don't allow 0's, convert to A.
    if x = 49 then x := 66; //Don't allow 1's, convert to B.
    if x = 73 then x := 67; //Don't allow I's, convert to C.
    if x = 79 then x := 68; //Don't allow O's, convert to D.

    s := s + AnsiChar(x);
  end;

  result := s;
end;


function CalcCheckSum(Text:AnsiString):cardinal;
var
  x  : cardinal;
  c1 : integer;
begin
  x := 0;
  for c1 := 0 to Length(Text) - 1 do
  begin
    x := x + AnsiCharToInteger(Text[c1+1]);
  end;
  result := x;
end;

end.
