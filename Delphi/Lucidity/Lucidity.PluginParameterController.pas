unit Lucidity.PluginParameterController;

interface

uses
  Lucidity.PluginParameters,
  Lucidity.Types;


type
  TPluginParameterController = class
  private
  public
    class procedure SetPluginParameter(const aPlugin : TObject; const Scope : TParChangeScope; const KeyGroupName : string; const ParName : string; const Value : single); static; inline;
    class function GetPluginParameter(const aPlugin : TObject; const ParName : string):single; static; inline;
  end;

  // TODO: This class will contain all the code required to set/get the parameter values
  // to the internal audio engine.

implementation

uses

  SysUtils,
  soLucidityVoiceParameterWrapper,
  Lucidity.KeyGroup,
  Lucidity.Interfaces,
  uLucidityEnums,
  eePlugin;

{ TPluginParameterController }

class function TPluginParameterController.GetPluginParameter(const aPlugin : TObject; const ParName: string): single;
begin
  result := 0;
end;

class procedure TPluginParameterController.SetPluginParameter(
    const aPlugin : TObject;
    const Scope: TParChangeScope;
    const KeyGroupName: string;
    const ParName: string;
    const Value: single);
var
  Plugin : TeePlugin;
  Par : TPluginParameter;
  kg : IKeyGroup;
  VoicePar : TLucidityVoiceParameterWrapper;
begin
  assert(aPlugin is TeePlugin);
  Plugin := aPlugin as TeePlugin;

  kg :=  Plugin.ActiveKeyGroup;

  VoicePar := (kg.GetObject as TKeyGroup).VoiceParameters;

  PluginParFromName(ParName);


end;

end.
