program Demo.Spring.Collections.Stack;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND},
  uIsPalindrome in 'uIsPalindrome.pas';

procedure TellIfPalindrome(const aString: string);
begin
  if IsPalindrome(aString) then
  begin
    Writeln('"', aString, '"', ' is a palindrome');
  end else
  begin
    Writeln('"', aString, '"', ' is NOT a palindrome');
  end;
end;

var
  S: string;
begin
  try
    S := 'rAceCar';
    TellIfPalindrome(S);

    S := 'racecar';
    TellIfPalindrome(S);

    S := 'RACECAR';
    TellIfPalindrome(S);

    S := 'Madam, I''m adam';
    TellIfPalindrome(S);

    S := 'A Toyota! Race fast, safe car! A Toyota!';
    TellIfPalindrome(S);

    S := 'A man, a plan, a canal: Panama';
    TellIfPalindrome(S);

    S := 'No sir -- away! A papaya war is on!';
    TellIfPalindrome(S);

    S := 'To be or not to be, that is the question.';
    TellIfPalindrome(S);

    S := 'No way is this a palindrome';
    TellIfPalindrome(S);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
  Readln;
end.
