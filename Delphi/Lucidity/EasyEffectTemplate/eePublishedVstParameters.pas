unit eePublishedVstParameters;

interface

uses
  VamLib.Collections.Lists;

type
  TVstParameter = record
  public
    PluginParameterName : string;
  end;

  TVstParameterList = class(TSimpleList<TVstParameter>)
  end;

  TPublishedVstParameterController = class
  private
    VstParameterList : TVstParameterList;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParameter(const PluginParameterName : string);

    function FindParameterIndex(const PluginParameterName : string) : integer;

    property Count : integer read GetCount;
  end;


implementation

{ TPublishedVstParameterController }

constructor TPublishedVstParameterController.Create;
begin
  VstParameterList := TVstParameterList.Create;

end;

destructor TPublishedVstParameterController.Destroy;
begin
  VstParameterList.Free;
  inherited;
end;

function TPublishedVstParameterController.GetCount: integer;
begin
  result := VstParameterList.Count;
end;

function TPublishedVstParameterController.FindParameterIndex(const PluginParameterName: string): integer;
var
  c1: Integer;
begin
  for c1 := 0 to VstParameterList.Count-1 do
  begin
    if PluginParameterName = VstParameterList[c1].PluginParameterName
      then exit(c1);
  end;

  //== no match has been found if we've made it this far ==
  result := -1;
end;

procedure TPublishedVstParameterController.AddParameter(const PluginParameterName: string);
var
  VstParameter : TVstParameter;
begin
  VstParameter.PluginParameterName := PluginParameterName;
end;



end.
