unit Test.LucidityBug.SetGuiParameter;

interface

uses
  WatchTower,
  eeGlobals,
  eePlugin;

type
  TLucidityBug_SetGUIPar = class(TWatchTowerTest)
  private
  protected
    Globals : TGlobals;
    Plug : TeePlugin;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure AttempToSetGUIParameter;


  end;

implementation

uses
  SysUtils,
  VamLib.MoreTypes,
  WatchTower.Confirm,
  Lucidity.PluginParameters,
  eeTypes;

{ TLucidity_SetGUIPar }

procedure TLucidityBug_SetGUIPar.Setup;
begin
  inherited;
  Globals := TGlobals.Create;
  Plug := TeePlugin.Create(Globals);

end;

procedure TLucidityBug_SetGUIPar.TearDown;
begin
  inherited;
  Plug.Free;
  Globals.Free;
end;

procedure TLucidityBug_SetGUIPar.AttempToSetGUIParameter;
var
  ParID    : TPluginParameterID;
  ParValue : single;
  RetrievedValue : single;
begin
  // BUG:
  // Grace is not setting parameters from the GUI correctly.
  // Steps to reproduce:
  //   a) Create a grace instance in DAW.
  //   b) Create two groups. try to adjust attack parameter for both groups. Notice
  //      that both groups always have the same attack parameter. The groups should
  //      be able to have different attack parameters.

  Plug.KeyGroups.NewKeyGroup('Group1');
  Plug.KeyGroups.NewKeyGroup('Group2');

  ParID := PluginParNameToID(PluginParToName(TPluginParameter.AmpAttack));

  Plug.FocusKeyGroup('Group1');
  ParValue := 0.3;
  Plug.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);


  Plug.FocusKeyGroup('Group2');
  ParValue := 0.7;
  Plug.SetPluginParameter(ParID, ParValue, TParChangeScope.psFocused);



  Plug.FocusKeyGroup('Group1');
  ParValue := 0.3;
  RetrievedValue := Plug.GetPluginParameter(ParID);
  Confirm.IsTrue(RetrievedValue = ParValue);



  Plug.FocusKeyGroup('Group2');
  ParValue := 0.7;
  RetrievedValue := Plug.GetPluginParameter(ParID);
  Confirm.IsTrue(RetrievedValue = ParValue);



  //Plug.SaveProgramToFileWithoutSamples('C:\Users\vam\Desktop\Default.lpg');
end;



end.
