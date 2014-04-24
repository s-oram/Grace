unit eeHashes;

interface



// Function Murmur2()
// Source: http://stackoverflow.com/a/3690639/395461
// Notes:
// - Currently untested (19-March-2013)
function Murmur2(const S: AnsiString; const Seed: LongWord=$9747b28c): LongWord;

function Md5(const Value: AnsiString): string; overload;
function Md5(const Value: UnicodeString): string; overload;

implementation


uses
  MessageDigest_5; //Part of the delphi RTL.


function Md5(const Value: AnsiString): string; overload;
var
  hash: MessageDigest_5.IMD5;
begin
  hash := MessageDigest_5.GetMD5();
  hash.Update(String(Value));
  Result := hash.AsString();
end;

function Md5(const Value: UnicodeString): string; overload;
var
  hash: MessageDigest_5.IMD5;
begin
  hash := MessageDigest_5.GetMD5();
  hash.Update(Value);
  Result := hash.AsString();
end;


function Murmur2(const S: AnsiString; const Seed: LongWord=$9747b28c): LongWord;
var
    h: LongWord;
    len: LongWord;
    k: LongWord;
    data: Integer;
const
    // 'm' and 'r' are mixing constants generated offline.
    // They're not really 'magic', they just happen to work well.
    m = $5bd1e995;
    r = 24;
begin
    len := Length(S);

    //The default seed, $9747b28c, is from the original C library

    // Initialize the hash to a 'random' value
    h := seed xor len;

    // Mix 4 bytes at a time into the hash
    data := 1;

    while(len >= 4) do
    begin
        k := PLongWord(@S[data])^;

        k := k*m;
        k := k xor (k shr r);
        k := k* m;

        h := h*m;
        h := h xor k;

        data := data+4;
        len := len-4;
    end;

    {   Handle the last few bytes of the input array
            S: ... $69 $18 $2f
    }
    Assert(len <= 3);
    if len = 3 then
        h := h xor (LongWord(s[data+2]) shl 16);
    if len >= 2 then
        h := h xor (LongWord(s[data+1]) shl 8);
    if len >= 1 then
    begin
        h := h xor (LongWord(s[data]));
        h := h * m;
    end;

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    h := h xor (h shr 13);
    h := h * m;
    h := h xor (h shr 15);

    Result := h;
end;

end.

