unit Lucidity.PluginParameterController;

interface

uses
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
begin

end;

end.
