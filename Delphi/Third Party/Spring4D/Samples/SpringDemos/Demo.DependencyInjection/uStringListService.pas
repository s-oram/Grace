unit uStringListService;

interface

uses
         Classes
       , SysUtils
       ;

type
  IStringListService = interface
    ['{E9E04CBA-6ADC-4917-BE18-B407D109D12B}']
    // Getters/Setters
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(aValue: TDuplicates);
    function GetSorted: Boolean;
    procedure SetSorted(aValue: Boolean);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(aValue: Boolean);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(aValue: TNotifyEvent);
    function GetOnChanging: TNotifyEvent;
    procedure SetOnChanging(aValue: TNotifyEvent);
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(aValue: Boolean);
    function GetCapacity: integer;
    procedure SetCapacity(const aValue: integer);
    function GetCommaText: string;
    procedure SetCommaText(const aValue: string);
    function GetCount: integer;
{$IF COMPILERVERSION > 21}
    function GetDefaultEncoding: TEncoding;
    procedure SetDefaultEncoding(const aValue: TEncoding);
{$IFEND}
    function GetDelimiter: Char;
    procedure SetDelimiter(const aValue: Char);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const aValue: string);
{$IF COMPILERVERSION > 21}
    function GetEncoding: TEncoding;
{$IFEND}
    function GetLineBreak: string;
    procedure SetLineBreak(const aValue: string);
    function GetName(Index: integer): string;
    function GetObject(Index: integer): TObject;
    procedure PutObject(Index: integer; aObject: TObject);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const aValue: Char);
    function GetValue(Index: string): string;
    procedure SetValue(Index: string; aValue: string);
    function GetValueFromIndex(Index: integer): string;
    procedure SetValueFromIndex(Index: integer; aValue: string);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const aValue: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const aValue: Boolean);
    function Get(Index: integer): string;
    procedure Put(Index: integer; aValue: string);
    function GetTextStr: string;
    procedure SetTextStr(const aValue: string);
    function GetStringsAdapter: IStringsAdapter;
    procedure SetStringsAdapter(const aValue: IStringsAdapter);
{$IF COMPILERVERSION > 21}
    function GetWriteBOM: Boolean;
    procedure SetWriteBOM(const aValue: Boolean);
{$IFEND}

    // actual stuff
    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); overload;
{$IF COMPILERVERSION > 21}
    procedure AddStrings(const Strings: TArray<string>); overload;
    procedure AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>); overload;
{$IFEND}
    procedure BeginUpdate;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean;
    function GetEnumerator: TStringsEnumerator;
    function GetText: PChar;
    function IndexOfName(const Name: string): integer;
    function IndexOfObject(aObject: TObject): integer;
    procedure LoadFromFile(const FileName: string); overload;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload;
    procedure LoadFromStream(Stream: TStream); overload;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload;
    procedure Move(CurIndex, NewIndex: integer);
    procedure SaveToFile(const FileName: string); overload;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload;
    procedure SaveToStream(Stream: TStream); overload;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload;
    procedure SetText(Text: PChar);
{$IF COMPILERVERSION > 21}
    function ToStringArray: TArray<string>;
    function ToObjectArray: TArray<TObject>;
{$IFEND}

    function Add(const S: string): integer;
    function AddObject(const S: string; aObject: TObject): integer;
    procedure Assign(Source: TPersistent);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Exchange(Index1, Index2: integer);
    function Find(const S: string; var Index: integer): Boolean;
    function IndexOf(const S: string): integer;
    procedure Insert(Index: integer; const S: string);
    procedure InsertObject(Index: integer; const S: string; aObject: TObject);
    procedure Sort;
    procedure CustomSort(Compare: TStringListSortCompare);
    property Duplicates: TDuplicates read GetDuplicates write SetDuplicates;
    property Sorted: Boolean read GetSorted write SetSorted;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnChanging: TNotifyEvent read GetOnChanging write SetOnChanging;
    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    property Capacity: integer read GetCapacity write SetCapacity;
    property CommaText: string read GetCommaText write SetCommaText;
    property Count: integer read GetCount;
{$IF COMPILERVERSION > 21}
    property DefaultEncoding: TEncoding read GetDefaultEncoding write SetDefaultEncoding;
{$IFEND}
    property Delimiter: Char read GetDelimiter write SetDelimiter;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
{$IF COMPILERVERSION > 21}
    property Encoding: TEncoding read GetEncoding;
{$IFEND}
    property LineBreak: string read GetLineBreak write SetLineBreak;
    property Names[Index: integer]: string read GetName;
    property Objects[Index: integer]: TObject read GetObject write PutObject;
    property QuoteChar: Char read GetQuoteChar write SetQuoteChar;
    property Values[Name: string]: string read GetValue write SetValue;
    property ValueFromIndex[Index: integer]: string read GetValueFromIndex write SetValueFromIndex;
    property NameValueSeparator: Char read GetNameValueSeparator write SetNameValueSeparator;
    property StrictDelimiter: Boolean read GetStrictDelimiter write SetStrictDelimiter;
    property Strings[Index: integer]: string read Get write Put; default;
    property Text: string read GetTextStr write SetTextStr;
    property StringsAdapter: IStringsAdapter read GetStringsAdapter write SetStringsAdapter;
{$IF COMPILERVERSION > 21}
    property WriteBOM: Boolean read GetWriteBOM write SetWriteBOM;
{$IFEND}

  end;

procedure RegisterStringListService(aName: string);

implementation

uses
        Spring.Container
      ;

type
  TStringListImpl = class(TInterfacedObject, IStringListService)
  strict private
    FStringList: TStringList;
    function GetDuplicates: TDuplicates;
    procedure SetDuplicates(aValue: TDuplicates);
    function GetSorted: Boolean;
    procedure SetSorted(aValue: Boolean);
    function GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(aValue: Boolean);
    function GetOnChange: TNotifyEvent;
    procedure SetOnChange(aValue: TNotifyEvent);
    function GetOnChanging: TNotifyEvent;
    procedure SetOnChanging(aValue: TNotifyEvent);
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(aValue: Boolean);
    function GetCapacity: integer;
    procedure SetCapacity(const aValue: integer);
    function GetCommaText: string;
    procedure SetCommaText(const aValue: string);
    function GetCount: integer;
{$IF COMPILERVERSION > 21}
    function GetDefaultEncoding: TEncoding;
    procedure SetDefaultEncoding(const aValue: TEncoding);
{$IFEND}
    function GetDelimiter: Char;
    procedure SetDelimiter(const aValue: Char);
    function GetDelimitedText: string;
    procedure SetDelimitedText(const aValue: string);
{$IF COMPILERVERSION > 21}
    function GetEncoding: TEncoding;
{$IFEND}
    function GetLineBreak: string;
    procedure SetLineBreak(const aValue: string);
    function GetName(Index: integer): string;
    function GetObject(Index: integer): TObject;
    procedure PutObject(Index: integer; aObject: TObject);
    function GetQuoteChar: Char;
    procedure SetQuoteChar(const aValue: Char);
    function GetValue(Index: string): string;
    procedure SetValue(Index: string; aValue: string);
    function GetValueFromIndex(Index: integer): string;
    procedure SetValueFromIndex(Index: integer; aValue: string);
    function GetNameValueSeparator: Char;
    procedure SetNameValueSeparator(const aValue: Char);
    function GetStrictDelimiter: Boolean;
    procedure SetStrictDelimiter(const aValue: Boolean);
    function Get(Index: integer): string;
    procedure Put(Index: integer; aValue: string);
    function GetTextStr: string;
    procedure SetTextStr(const aValue: string);
    function GetStringsAdapter: IStringsAdapter;
    procedure SetStringsAdapter(const aValue: IStringsAdapter);
{$IF COMPILERVERSION > 21}
    function GetWriteBOM: Boolean;
    procedure SetWriteBOM(const aValue: Boolean);
{$IFEND}

  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer;
    function AddObject(const S: string; aObject: TObject): integer;
    procedure Assign(Source: TPersistent);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Exchange(Index1, Index2: integer);
    function Find(const S: string; var Index: integer): Boolean;
    function IndexOf(const S: string): integer;
    procedure Insert(Index: integer; const S: string);
    procedure InsertObject(Index: integer; const S: string; aObject: TObject);
    procedure Sort;
    procedure CustomSort(Compare: TStringListSortCompare);
    procedure Append(const S: string);
    procedure AddStrings(Strings: TStrings); overload;
{$IF COMPILERVERSION > 21}
    procedure AddStrings(const Strings: TArray<string>); overload;
    procedure AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>); overload;
{$IFEND}
    procedure BeginUpdate;
    procedure EndUpdate;
    function Equals(Strings: TStrings): Boolean; reintroduce;
    function GetEnumerator: TStringsEnumerator;
    function GetText: PChar;
    function IndexOfName(const Name: string): integer;
    function IndexOfObject(aObject: TObject): integer;
    procedure LoadFromFile(const FileName: string); overload;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); overload;
    procedure LoadFromStream(Stream: TStream); overload;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); overload;
    procedure Move(CurIndex, NewIndex: integer);
    procedure SaveToFile(const FileName: string); overload;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); overload;
    procedure SaveToStream(Stream: TStream); overload;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); overload;
    procedure SetText(Text: PChar);
{$IF COMPILERVERSION > 21}
    function ToStringArray: TArray<string>;
    function ToObjectArray: TArray<TObject>;
{$IFEND}
  end;

  { TStringListImpl }

function TStringListImpl.Add(const S: string): integer;
begin
  Result := FStringList.Add(S);
end;

function TStringListImpl.AddObject(const S: string; aObject: TObject): integer;
begin
  Result := FStringList.AddObject(S, aObject);
end;

{$IF COMPILERVERSION > 21}
procedure TStringListImpl.AddStrings(const Strings: TArray<string>);
begin
  FStringList.AddStrings(Strings);
end;

procedure TStringListImpl.AddStrings(const Strings: TArray<string>; const Objects: TArray<TObject>);
begin
  FStringList.AddStrings(Strings, Objects);
end;
{$IFEND}

procedure TStringListImpl.AddStrings(Strings: TStrings);
begin
  FStringList.AddStrings(Strings);
end;

procedure TStringListImpl.Append(const S: string);
begin
  FStringList.Append(S);
end;

procedure TStringListImpl.Assign(Source: TPersistent);
begin
  FStringList.Assign(Source);
end;

procedure TStringListImpl.BeginUpdate;
begin
  FStringList.BeginUpdate;
end;

procedure TStringListImpl.Clear;
begin
  FStringList.Clear;
end;

constructor TStringListImpl.Create;
begin
  inherited Create;
  FStringList := TStringList.Create;
end;

procedure TStringListImpl.CustomSort(Compare: TStringListSortCompare);
begin
  FStringList.CustomSort(Compare);
end;

procedure TStringListImpl.Delete(Index: integer);
begin
  FStringList.Delete(Index);
end;

destructor TStringListImpl.Destroy;
begin
  FStringList.Free;
  inherited;
end;

procedure TStringListImpl.EndUpdate;
begin
  FStringList.EndUpdate;
end;

function TStringListImpl.Equals(Strings: TStrings): Boolean;
begin
  Result := FStringList.Equals(Strings);
end;

procedure TStringListImpl.Exchange(Index1, Index2: integer);
begin
  FStringList.Exchange(Index1, Index2);
end;

function TStringListImpl.Find(const S: string; var Index: integer): Boolean;
begin
  Result := FStringList.Find(S, Index);
end;

function TStringListImpl.Get(Index: integer): string;
begin
  Result := FStringList[Index];
end;

function TStringListImpl.GetCapacity: integer;
begin
  Result := FStringList.Capacity;
end;

function TStringListImpl.GetCaseSensitive: Boolean;
begin
  Result := FStringList.CaseSensitive;
end;

function TStringListImpl.GetCommaText: string;
begin
  Result := FStringList.CommaText;
end;

function TStringListImpl.GetCount: integer;
begin
  Result := FStringList.Count;
end;

{$IF COMPILERVERSION > 21}
function TStringListImpl.GetDefaultEncoding: TEncoding;
begin
  Result := FStringList.DefaultEncoding;
end;
{$IFEND}

function TStringListImpl.GetDelimitedText: string;
begin
  Result := FStringList.DelimitedText;
end;

function TStringListImpl.GetDelimiter: Char;
begin
  Result := FStringList.Delimiter;
end;

function TStringListImpl.GetDuplicates: TDuplicates;
begin
  Result := FStringList.Duplicates;
end;

{$IF COMPILERVERSION > 21}
function TStringListImpl.GetEncoding: TEncoding;
begin
  Result := FStringList.Encoding;
end;
{$IFEND}

function TStringListImpl.GetEnumerator: TStringsEnumerator;
begin
  Result := FStringList.GetEnumerator;
end;

function TStringListImpl.GetLineBreak: string;
begin
  Result := FStringList.LineBreak;
end;

function TStringListImpl.GetName(Index: integer): string;
begin
  Result := FStringList.Names[Index];
end;

function TStringListImpl.GetNameValueSeparator: Char;
begin
  Result := FStringList.NameValueSeparator;
end;

function TStringListImpl.GetObject(Index: integer): TObject;
begin
  Result := FStringList.Objects[Index];
end;

function TStringListImpl.GetOnChange: TNotifyEvent;
begin
  Result := FStringList.OnChange;
end;

function TStringListImpl.GetOnChanging: TNotifyEvent;
begin
  Result := FStringList.OnChanging;
end;

function TStringListImpl.GetOwnsObjects: Boolean;
begin
  Result := FStringList.OwnsObjects;
end;

function TStringListImpl.GetQuoteChar: Char;
begin
  Result := FStringList.QuoteChar;
end;

function TStringListImpl.GetSorted: Boolean;
begin
  Result := FStringList.Sorted;
end;

function TStringListImpl.GetStrictDelimiter: Boolean;
begin
  Result := FStringList.StrictDelimiter;
end;

function TStringListImpl.GetStringsAdapter: IStringsAdapter;
begin
  Result := FStringList.StringsAdapter
end;

function TStringListImpl.GetText: PChar;
begin
  Result := PChar(FStringList.Text);
end;

function TStringListImpl.GetTextStr: string;
begin
  Result := FStringList.Text;
end;

function TStringListImpl.GetValue(Index: string): string;
begin
  Result := FStringList.Values[Index];
end;

function TStringListImpl.GetValueFromIndex(Index: integer): string;
begin
  Result := FStringList.ValueFromIndex[Index];
end;

{$IF COMPILERVERSION > 21}
function TStringListImpl.GetWriteBOM: Boolean;
begin
  Result := FStringList.WriteBOM;
end;
{$IFEND}

function TStringListImpl.IndexOf(const S: string): integer;
begin
  Result := FStringList.IndexOf(S);
end;

function TStringListImpl.IndexOfName(const Name: string): integer;
begin
  Result := FStringList.IndexOfName(Name);
end;

function TStringListImpl.IndexOfObject(aObject: TObject): integer;
begin
  Result := FStringList.IndexOfObject(aObject);
end;

procedure TStringListImpl.Insert(Index: integer; const S: string);
begin
  FStringList.Insert(Index, S);
end;

procedure TStringListImpl.InsertObject(Index: integer; const S: string; aObject: TObject);
begin
  FStringList.InsertObject(Index, S, aObject);
end;

procedure TStringListImpl.LoadFromFile(const FileName: string; Encoding: TEncoding);
begin
  FStringList.LoadFromFile(FileName, Encoding);
end;

procedure TStringListImpl.LoadFromFile(const FileName: string);
begin
  FStringList.LoadFromFile(FileName);
end;

procedure TStringListImpl.LoadFromStream(Stream: TStream);
begin
  FStringList.LoadFromStream(Stream);
end;

procedure TStringListImpl.LoadFromStream(Stream: TStream; Encoding: TEncoding);
begin
  FStringList.LoadFromStream(Stream, Encoding);
end;

procedure TStringListImpl.Move(CurIndex, NewIndex: integer);
begin
  FStringList.Move(CurIndex, NewIndex);
end;

procedure TStringListImpl.Put(Index: integer; aValue: string);
begin
  FStringList[Index] := aValue;
end;

procedure TStringListImpl.PutObject(Index: integer; aObject: TObject);
begin
  FStringList.Objects[Index] := aObject;
end;

procedure TStringListImpl.SaveToFile(const FileName: string; Encoding: TEncoding);
begin
  FStringList.SaveToFile(FileName, Encoding);
end;

procedure TStringListImpl.SaveToFile(const FileName: string);
begin
  FStringList.SaveToFile(FileName);
end;

procedure TStringListImpl.SaveToStream(Stream: TStream; Encoding: TEncoding);
begin
  FStringList.SaveToStream(Stream, Encoding);
end;

procedure TStringListImpl.SaveToStream(Stream: TStream);
begin
  FStringList.SaveToStream(Stream);
end;

procedure TStringListImpl.SetCapacity(const aValue: integer);
begin
  FStringList.Capacity := aValue;
end;

procedure TStringListImpl.SetCaseSensitive(aValue: Boolean);
begin
  FStringList.CaseSensitive := aValue;
end;

procedure TStringListImpl.SetCommaText(const aValue: string);
begin
  FStringList.CommaText := aValue;
end;

{$IF COMPILERVERSION > 21}
procedure TStringListImpl.SetDefaultEncoding(const aValue: TEncoding);
begin
  FStringList.DefaultEncoding := aValue;
end;
{$IFEND}

procedure TStringListImpl.SetDelimitedText(const aValue: string);
begin
  FStringList.DelimitedText := aValue;
end;

procedure TStringListImpl.SetDelimiter(const aValue: Char);
begin
  FStringList.Delimiter := aValue;
end;

procedure TStringListImpl.SetDuplicates(aValue: TDuplicates);
begin
  FStringList.Duplicates := aValue;
end;

procedure TStringListImpl.SetLineBreak(const aValue: string);
begin
  FStringList.LineBreak := aValue;
end;

procedure TStringListImpl.SetNameValueSeparator(const aValue: Char);
begin
  FStringList.NameValueSeparator := aValue;
end;

procedure TStringListImpl.SetOnChange(aValue: TNotifyEvent);
begin
  FStringList.OnChange := aValue;
end;

procedure TStringListImpl.SetOnChanging(aValue: TNotifyEvent);
begin
  FStringList.OnChanging := aValue;
end;

procedure TStringListImpl.SetOwnsObjects(aValue: Boolean);
begin
  FStringList.OwnsObjects := aValue;
end;

procedure TStringListImpl.SetQuoteChar(const aValue: Char);
begin
  FStringList.QuoteChar := aValue;
end;

procedure TStringListImpl.SetSorted(aValue: Boolean);
begin
  FStringList.Sorted := aValue
end;

procedure TStringListImpl.SetStrictDelimiter(const aValue: Boolean);
begin
  FStringList.StrictDelimiter := aValue;
end;

procedure TStringListImpl.SetStringsAdapter(const aValue: IStringsAdapter);
begin
  FStringList.StringsAdapter := aValue;
end;

procedure TStringListImpl.SetText(Text: PChar);
begin
  FStringList.SetText(Text);
end;

procedure TStringListImpl.SetTextStr(const aValue: string);
begin
  FStringList.Text := aValue;
end;

procedure TStringListImpl.SetValue(Index: string; aValue: string);
begin
  FStringList.Values[Index] := aValue;
end;

procedure TStringListImpl.SetValueFromIndex(Index: integer; aValue: string);
begin
  FStringList.ValueFromIndex[Index] := aValue;
end;

{$IF COMPILERVERSION > 21}
procedure TStringListImpl.SetWriteBOM(const aValue: Boolean);
begin
  FStringList.WriteBOM := aValue;
end;
{$IFEND}

procedure TStringListImpl.Sort;
begin
  FStringList.Sort;
end;

{$IF COMPILERVERSION > 21}
function TStringListImpl.ToObjectArray: TArray<TObject>;
begin
  Result := FStringList.ToObjectArray;
end;

function TStringListImpl.ToStringArray: TArray<string>;
begin
  Result := FStringList.ToStringArray;
end;
{$IFEND}

procedure RegisterStringListService(aName: string);
begin
  GlobalContainer.RegisterType<TStringListImpl>.Implements<IStringListService>(aName);
  GlobalContainer.Build;
end;

initialization
  RegisterStringListService('DSL');

end.
