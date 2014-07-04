unit eePublishedVstParameters;

interface

uses
  Contnrs,
  VamLib.Collections.Lists;

type
  TVstParameterInfo = record
    Name : string;
    ShortName : string;
    Display : string;
    Units   : string;
  end;

  TVstParameter = class
  public
    PluginParameterName : string;
    // NOTE: PluginParmaterID should be TPluginParameterID, not an integer.
    // But I would need a common base unit to define TPluginParameterID in.
    PluginParameterID   : integer;
  end;

  TPublishedVstParameterController = class
  private
    VstParameterList : TObjectList;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParameter(const PluginParameterName : string; const PluginParID : integer);

    function FindParameterIndex(const PluginParameterName : string) : integer;
    function FindParameterName(const Index : integer) : string;

    property Count : integer read GetCount;
  end;


implementation

{ TPublishedVstParameterController }

constructor TPublishedVstParameterController.Create;
begin
  VstParameterList := TObjectList.Create;
  VstParameterList.OwnsObjects := true;

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
  VstPar : TVstParameter;
begin
  for c1 := 0 to VstParameterList.Count-1 do
  begin
    VstPar := VstParameterList[c1] as TVstParameter;
    if PluginParameterName = VstPar.PluginParameterName
      then exit(c1);
  end;

  //== no match has been found if we've made it this far ==
  result := -1;
end;

function TPublishedVstParameterController.FindParameterName(const Index: integer): string;
begin
  result := (VstParameterList[Index] as TVstParameter).PluginParameterName;
end;

procedure TPublishedVstParameterController.AddParameter(const PluginParameterName: string; const PluginParID : integer);
var
  VstPar : TVstParameter;
begin
  VstPar := TVstParameter.Create;
  VstPar.PluginParameterName := PluginParameterName;
  VstPar.PluginParameterID   := PluginParID;
  VstParameterList.Add(VstPar);
end;



end.
