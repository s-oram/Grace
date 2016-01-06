unit FarScape.SupportFunctions;

interface

uses
  FarScape.CustomControl;

function StrToAlign(const Value : string):TControlAlignment;
function StrToHitTest(const Value : string):THitTest;


type
  TBitAccess = record
  private
    Data : integer;
  public
    function GetBit(const Index : integer):boolean;
    procedure SetBit(const Index : integer; const Value : boolean);
  end;

implementation

uses
  SysUtils;

function StrToAlign(const Value : string):TControlAlignment;
begin
  if SameText(Value, 'None')   then exit(caNone);
  if SameText(Value, 'Top')    then exit(caTop);
  if SameText(Value, 'Bottom') then exit(caBottom);
  if SameText(Value, 'Left')   then exit(caLeft);
  if SameText(Value, 'Right')  then exit(caRight);
  if SameText(Value, 'Client') then exit(caClient);
  if SameText(Value, 'Center') then exit(caCenter);
  if SameText(Value, 'Grid')   then exit(caGrid);

  raise Exception.Create('Unexpected value.');
end;


function StrToHitTest(const Value : string):THitTest;
begin
  if SameText(Value, 'None')    then exit(htNone);
  if SameText(Value, 'Partial') then exit(htPartial);
  if SameText(Value, 'Always')  then exit(htAlways);

  raise Exception.Create('Unexpected value.');
end;


{ TBitAccess }

function TBitAccess.GetBit(const Index: integer): boolean;
begin
  assert(Index >= 0);
  assert(Index <= 31);
  Result := (Data and (1 shl Index)) <> 0;
end;

procedure TBitAccess.SetBit(const Index: integer; const Value: boolean);
begin
  assert(Index >= 0);
  assert(Index <= 31);
  if Value
    then Data := Data or (1 shl Index)
    else Data := Data and not (1 shl Index);
end;

end.
