unit eePublishedVstParameters;

interface

uses
  Contnrs,
  VamLib.Collections.Lists,
  eeTypes;

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
    PluginParameterID   : TPluginParameterID;
  end;

  TPublishedVstParameterController = class
  private
    VstParameterList : TObjectList;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddParameter(const PluginParameterName : string; const PluginParID : TPluginParameterID);

    function FindVstParameterIndex(const PluginParameterName : string) : integer; //TODO:LOW is this method even used anywhere?
    function FindParameterName(const VstParameterIndex : integer) : string; //TODO:MED this method should be removed. It's been superceded by using a parameterID
    function FindParameterID(const VstParameterIndex : integer) : TPluginParameterID;

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

function TPublishedVstParameterController.FindVstParameterIndex(const PluginParameterName: string): integer;
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

function TPublishedVstParameterController.FindParameterID(const VstParameterIndex: integer): TPluginParameterID;
begin
  result := (VstParameterList[VstParameterIndex] as TVstParameter).PluginParameterID;
end;

function TPublishedVstParameterController.FindParameterName(const VstParameterIndex: integer): string;
begin
  result := (VstParameterList[VstParameterIndex] as TVstParameter).PluginParameterName;
end;

procedure TPublishedVstParameterController.AddParameter(const PluginParameterName: string; const PluginParID : TPluginParameterID);
var
  VstPar : TVstParameter;
begin
  VstPar := TVstParameter.Create;
  VstPar.PluginParameterName := PluginParameterName;
  VstPar.PluginParameterID   := PluginParID;
  VstParameterList.Add(VstPar);
end;



end.
