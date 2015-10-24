unit FarScape.SupportFunctions;

interface

uses
  FarScape.CustomControl;

function StrToAlign(const Value : string):TControlAlignment;
function StrToHitTest(const Value : string):THitTest;



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
  if SameText(Value, 'Custom') then exit(caCustom);

  raise Exception.Create('Unexpected value.');
end;


function StrToHitTest(const Value : string):THitTest;
begin
  if SameText(Value, 'None')    then exit(htNone);
  if SameText(Value, 'Partial') then exit(htPartial);
  if SameText(Value, 'Always')  then exit(htAlways);

  raise Exception.Create('Unexpected value.');
end;


end.
