unit AudioPlugin.Vst2PluginInfo;

interface

uses
  VamLib.MoreTypes,
  AudioPlugin;

type
  TVst2PluginInfo = class;
  TVst2PluginInfoClass = class of TVst2PluginInfo;

  TVst2PluginInfo = class
  private
    FPlug: TAudioPlugin;
  protected
    property Plug : TAudioPlugin read FPlug;
  public
    constructor Create(const aPlug : TAudioPlugin); virtual;
    destructor Destroy; override;

    function GetNumberOfPrograms:integer; virtual; abstract;

    function GetNumberOfAudioInputs : integer; virtual; abstract;
    function GetNumberOfAudioOutputs : integer; virtual; abstract;


    function GetParameterLabel(const Index : integer):string; virtual; abstract;
    function GetParameterDisplay(const Index : integer):string; virtual; abstract;
    function GetParameterName(const Index : integer):string; virtual; abstract;

    function GetIsSynth : boolean; virtual; abstract;
  end;

implementation

{ TVst2PluginInfo }

constructor TVst2PluginInfo.Create(const aPlug : TAudioPlugin);
begin
  FPlug := aPlug;
end;

destructor TVst2PluginInfo.Destroy;
begin

  inherited;
end;


end.
