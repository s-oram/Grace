unit Punycode;

interface

uses
  SysUtils, StrUtils;

type
  //This PunyCode implemenation was copied from:
  //  https://github.com/oulan/XMPP-Delphi

  TPunycode=class
  const
    TMIN:Integer=1;
    TMAX:Integer=26;
    BASE:Integer = 36;
		INITIAL_N:Integer = 128;
		INITIAL_BIAS:Integer = 72;
		DAMP:Integer = 700;
		SKEW:Integer = 38;
		DELIMITER:char = '-';
  public
    class function Encode(input:string):string;
    class function Decode(input:string):string;
    class function Adapt(delta,numpoints:Integer;first:Boolean):integer;
    class function IsBasic(c:Char):Boolean;
    class function Digit2Codepoint(d:Integer):integer;
    class function Codepoint2Digit(c:Integer):integer;
  end;

  TPunycodeException=class(Exception)
  public
    constructor Create(msg:string);overload;
  const
    OVERFLOW:string = 'Overflow.';
    BAD_INPUT:string = 'Bad input.';
  end;

implementation

{ TPunycodeException }

constructor TPunycodeException.Create(msg: string);
begin
  inherited Create(msg);
end;

{ TPunycode }

class function TPunycode.Adapt(delta, numpoints: Integer; first: Boolean): integer;
var
  k:Integer;
begin
  if first then
    delta:=delta div damp
  else
    delta:=delta div 2;
  delta:=delta+(delta div numpoints);
  k:=0;
  while(delta>((BASE-TMIN)*TMAX) div 2) do
  begin
    delta:=delta div (BASE-TMIN);
    k:=k+BASE;
  end;
  Result:=k+((BASE-TMIN+1)*delta) div (delta+SKEW);
end;

class function TPunycode.Codepoint2Digit(c: Integer): integer;
begin
  if c-ord('0')<10 then
    Result:=c-ord('0')+26
  else if c-ord('a')<26 then
    Result:=c-ord('a')
  else
    raise TPunycodeException.Create(TPunycodeException.BAD_INPUT);
end;

class function TPunycode.Decode(input: string): string;
var
  n,i,j,bias,d,oldi,w,k,digit,t:integer;
  output:TStringBuilder;
  c:char;
begin
  n:=INITIAL_N;
  i:=0;
  bias:=INITIAL_BIAS;
  output:=TStringBuilder.Create;
  d:=LastDelimiter(DELIMITER,input);
  if d>1 then
  begin
    for j := 1 to d-1 do
    begin
      c:=input[j];
      if not IsBasic(c) then
        raise TPunycodeException.Create(TPunycodeException.BAD_INPUT);
      output.Append(c);
    end;
    inc(d);
  end
  else
    d:=0;
  while d<Length(input) do
  begin
    oldi:=i;
    w:=1;
    k:=BASE;
    while True do
    begin
      if d=Length(input)+1 then
        raise TPunycodeException.Create(TPunycodeException.BAD_INPUT);
      c:=input[d];
      Inc(d);
      digit:=Codepoint2Digit(Ord(c));
          if digit>(MAXINT-i) div w then
        raise TPunycodeException.Create(TPunycodeException.OVERFLOW);
      i:=i+digit*w;
      t:=0;
      if k<=bias then
        t:=TMIN
      else if k>=bias+TMAX then
        t:=TMAX
      else
        t:=k-bias;
      if digit<t then
        Break;
      w:=w*(BASE-t);

      Inc(k,BASE);
    end;
    bias:=Adapt(i-oldi,output.Length+1,oldi=0);
    if i div (output.Length+1)>MAXINT-n then
      raise TPunycodeException.Create(TPunycodeException.OVERFLOW);
    n:=n+i div (output.Length+1);
    i:=i mod (output.Length+1);
    output.Insert(i,Chr(n));
    Inc(i);
  end;
  Result:=output.ToString;
end;

class function TPunycode.Digit2Codepoint(d: Integer): integer;
begin
  if d<26 then
    Result:=d+ord('a')
  else if d<36 then
    Result:=d-26+ord('0')
  else
    raise TPunycodeException.Create(TPunycodeException.BAD_INPUT);
end;

class function TPunycode.Encode(input: string): string;
var
  n,b,delta,bias,m,i,h,j,q,k,t:integer;
  output:TStringBuilder;
  c:char;
begin
  n := initial_n;
  delta := 0;
  bias := INITIAL_BIAS;
  output := TStringBuilder.create;
  b := 0;
  for i := 1 to Length(input) do
  begin
    c:=input[i];
    if IsBasic(c) then
    begin
      output.Append(c);
      Inc(b);
    end;
  end;
  if b>0 then
    output.Append(DELIMITER);
  h:=b;
  while h<Length(input) do
  begin
    m:=MaxInt;
    for i := 1 to Length(input) do
    begin
      c:=input[i];
      if (Ord(c)>=n) and (Ord(c)<m) then
        m:=Ord(c);
    end;
    if m-n>(MaxInt-delta) div (h+1) then
      raise TPunycodeException.Create(TPunycodeException.OVERFLOW);
    delta:=delta+(m-n)*(h+1);
    n:=m;
    for j := 1 to Length(input) do
    begin
      c:=input[j];
      if Ord(c)<n then
      begin
        Inc(delta);
        if delta=0 then
          raise TPunycodeException.Create(TPunycodeException.OVERFLOW);
      end;
      if Ord(c)=n then
      begin
        q:=delta;
        k:=BASE;
        while True do
        begin
          t:=0;
          if k<=bias then
            t:=TMIN
          else if k>=bias+Tmax then
            t:=TMAX
          else
            t:=k-bias;
          if q<t then
            Break;
          output.Append(Chr(Digit2Codepoint(t+(q-t)mod (BASE-t))));
          q:=(q-t) div (BASE-t);
          Inc(k,BASE);
        end;
        output.Append(Chr(Digit2Codepoint(q)));
        bias:=Adapt(delta,h+1,h=b);
        delta:=0;
        Inc(h);
      end;
    end;
    Inc(delta);
    Inc(n);
  end;
  Result:=output.ToString;
end;

class function TPunycode.IsBasic(c: Char): Boolean;
begin
  Result:=Ord(c)<$80;
end;

end.

