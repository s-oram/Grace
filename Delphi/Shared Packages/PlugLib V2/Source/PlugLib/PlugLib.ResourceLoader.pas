unit PlugLib.ResourceLoader;

interface

uses
  Windows;

type
  ResourceLoader = record
    class function LoadText(const ResourceName : string):string; static;
  end;

implementation

uses
  Classes;

{ ResourceLoader }

class function ResourceLoader.LoadText(const ResourceName: string): string;
var
  RS: TResourceStream;
  Text : TStringList;
begin
  // NOTE: EResNotFound exception will be raised if the resource doesn't exist.
  RS := TResourceStream.Create(hInstance, ResourceName, RT_RCDATA);
  Text := TStringList.Create;
  try
    Text.LoadFromStream(rs);
    result := Text.Text;
  finally
    RS.Free;
    Text.Free;
  end;
end;

end.
