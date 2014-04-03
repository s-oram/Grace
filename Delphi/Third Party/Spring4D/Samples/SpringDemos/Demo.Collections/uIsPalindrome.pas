unit uIsPalindrome;

interface

function IsPalindrome(const aString: string): Boolean;

implementation

uses
        Spring.Collections
      , {$IF CompilerVersion >= 23.0}System.SysUtils{$ELSE}SysUtils{$IFEND}
      , Character
      ;

function IsPalindrome(const aString: string): Boolean;
var
  Stack: IStack<Char>;
  C: Char;
begin
  stack := TCollections.CreateStack<Char>;
  for C in aString do
  begin
    if TCharacter.IsLetter(C) then
      Stack.Push(TCharacter.ToLower(C));
  end;
  Result := Stack.EqualsTo(Stack.Reversed);
end;

end.
