unit eeVstEditorAdapter;

interface

{$INCLUDE Defines.inc}

uses
  {$IFDEF VER230}
  Winapi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  eePluginGui, eePlugin, eePluginGuiMeta,
  DVstUtils, DAEffect, DAEffectX, DAudioEffect, DAudioEffectX,
  Messages;

const
  WM_EDITOROPEN = WM_USER + 961;

type
  TVstEditor = class(AEffEditor)
  private
    r             : ERect;
    useCount      : Longint;
    Plugin        : TeePlugin;
    PluginGUI     : TPluginGui;
    PluginGuiMeta : TPluginGuiMeta;
    systemWindow : HWnd;
  public
    constructor Create(effect: AudioEffect; aPlugin:TeePlugin); reintroduce;
    destructor Destroy; override;
    function GetRect(var rect: PERect): Longint; override;
    function Open(ptr: Pointer): Longint; override;
    procedure Close; override;
    procedure Idle; override;

    function OnKeyDown(var KeyCode: VstKeyCode): boolean; override;
    function OnKeyUp(var KeyCode: VstKeyCode): boolean; override;
  end;

implementation

uses
  {$IFDEF FmxGui}
    FMX.Platform.Win,
    InitFMXHack,
    {$DEFINE GdipRequired}
  {$ENDIF}

  {$IFDEF VGSceneGui}
    eeVGSceneHack,
    {$DEFINE GdipRequired}
  {$ENDIF}

  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  SysUtils,
  VamLib.ZeroObject,
  uConstants;

var
  GlobalGuiCount:integer; //Counts number of visible GUI's for all plugin instances.


{ AEditor }

constructor TVstEditor.Create(effect: AudioEffect; aPlugin:TeePlugin);
begin
  inherited Create(effect);
  Plugin := aPlugin;
  PluginGUI    := nil;
  useCount := 0;

  // NOTE: GDI+ needs to be initialised manually if used in a DLL. VGScene and FireMonkey both use GDI+ on Windows.
  //======== VGScene / FireMonkey =================
  if GlobalGuiCount = 0 then
  begin
    {$IFDEF GdipRequired}
    InitGDIP;
    {$ENDIF}
  end;
  inc(GlobalGuiCount);
  //==================================
end;

destructor TVstEditor.Destroy;
begin
  if (UseCount > 0) then Close;

  //======== VGScene / FireMonkey =================
  dec(GlobalGuiCount);
  if GlobalGuiCount = 0 then
  begin
    {$IFDEF GdipRequired}
    OutputDebugString(PWideChar('Enter: eeVstEditorAdapter.pas FreeGDIP finalization'));
    FreeGDIP;
    OutputDebugString(PWideChar('Leave: eeVstEditorAdapter.pas FreeGDIP finalization'));
    {$ENDIF}
  end;
  //==================================

  inherited;
end;

function TVstEditor.GetRect(var rect: PERect): Longint;
begin
  r.top  := 0;
  r.left := 0;

  if assigned(PluginGui) then
  begin
    //r.right  := PluginGUI.Width;
    //r.bottom := PluginGUI.Height;
    r.right  := PluginInfo.InitialGuiWidth;
    r.bottom := PluginInfo.InitialGuiHeight;
  end else
  begin
    r.right  := PluginInfo.InitialGuiWidth;
    r.bottom := PluginInfo.InitialGuiHeight;
  end;

  rect   := @r;
  Result := 1;
end;

procedure TVstEditor.Idle;
begin
  inherited;


  if (UseCount > 0) then
  begin
    if PluginGUI.Visible = false then
    begin
      //=== VCL GUI =======
      //PluginGUI.PostCreate(SystemWindow);
      PluginGUI.UpdateGui(nil);
      PluginGUI.Visible := true;

      Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.OnPostCreateFinished);
      Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.Command_UpdateGUI);
    end else
    begin
      PluginGUI.UpdateGui(nil);
    end;
  end;
end;

function TVstEditor.Open(ptr: Pointer): Longint;
begin
  systemWindow := hwnd(ptr);

  Plugin.Globals.TopLevelWindow := hwnd(ptr);

  if (UseCount = 0) then
  begin
    try
      PluginGuiMeta := TPluginGuiMeta.Create(Plugin, SystemWindow);

      PluginGUI := TPluginGui.CreateParented(SystemWindow);
      PluginGUI.Width  := PluginInfo.InitialGuiWidth;
      PluginGUI.Height := PluginInfo.InitialGuiHeight;
      PluginGUI.Plugin := self.Plugin;

      Plugin.Globals.TopLevelForm := PluginGUI;

      PluginGui.PostCreate(SystemWindow);
      PluginGuiMeta.PostCreate(PluginGui);

      // finally...
      Plugin.Globals.IsGuiOpen := true;
    finally
      UseCount := 1;
    end;
  end;
  result := 1;
end;

procedure TVstEditor.Close;
begin
  // AFAIK: GUI Open/Close calls can be mismatched in hosts, so the GUI open/close code needs to check if the GUI has
  // been opened/is still opened etc.
  if (UseCount > 0) then
  begin
    Plugin.Globals.IsGuiOpen := false;
    systemWindow := 0;
    UseCount := 0;

    if assigned(PluginGuiMeta) then FreeAndNil(PluginGuiMeta);
    if assigned(PluginGui)     then FreeAndNil(PluginGui);

    //Plugin.Globals.MotherShip.LogMainObjects;
    //Plugin.Globals.MotherShip.LogAudioObjects;
  end;
end;

function TVstEditor.OnKeyDown(var KeyCode: VstKeyCode): boolean;
begin
  if assigned(PluginGui) then
  begin
    result := PluginGUI.PluginHotkeys.KeyDown(KeyCode);
  end else
  begin
    result := false;
  end;
end;

function TVstEditor.OnKeyUp(var KeyCode: VstKeyCode): boolean;
begin
  if assigned(PluginGui) then
  begin
    result := PluginGUI.PluginHotkeys.KeyUp(KeyCode);
  end else
  begin
    result := false;
  end;
end;



initialization
  GlobalGuiCount := 0;
finalization
  OutputDebugString(PWideChar('Finalize eeVstEditorAdapter.pas'));
end.
