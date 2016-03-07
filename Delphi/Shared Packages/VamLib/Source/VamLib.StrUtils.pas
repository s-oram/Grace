unit VamLib.StrUtils;

interface

function AddQuotes(const Text : string):string;

function IsInteger(const Text : string):boolean;
function IsFloat(const Text : string):boolean;

implementation

uses
  SysUtils;

function AddQuotes(const Text : string):string;
begin
  result := '''' + Text + '''';
end;

function IsInteger(const Text : string):boolean;
begin
  try
    StrToInt(Text);
  except
     on EConvertError do exit(false);
  end;

  // if we make it this far...
  result := true;
end;

function IsFloat(const Text : string):boolean;
begin
  try
    StrToFloat(Text);
  except
     on EConvertError do exit(false);
  end;

  // if we make it this far...
  result := true;
end;

end.
