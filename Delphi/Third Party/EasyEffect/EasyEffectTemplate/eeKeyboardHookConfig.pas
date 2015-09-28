unit eeKeyboardHookConfig;

interface

uses
  Windows;


function GetHandleForKeyHook(aHostName: string; aHostVersion: integer; aEditorWindow: hwnd; aConfigFileName: string; Out ConfigID:string; Out UseKeyHook:boolean):hwnd;

implementation

uses
  SysUtils,
  Classes,
  Contnrs,
  VamLib.Win.Shell,
  NativeXML,
  eeTypes,
  eeFunctions,
  eeDetectHost,
  VamLib.Utils;


//== forward function declarations ===
function GetParentHandle(aHandle:hwnd; Level:integer):hwnd; forward;


type
  TMatchElements = (meHostName, meHostVersion, meWindowClass, meWindowText);
  TMatchElementSet = set of TMatchElements;
  TMatchType = (mtUnknown, mtHostName, mtHostName_HostVersion, mtHostName_WindowClass, mtWindowClass, mtWindowClass_WindowText);

  THostConfig = class
  private
    function GetMatchType    : TMatchType;
    function UsesHostName    : boolean;
    function UsesHostVersion : boolean;
    function UsesWindowClass : boolean;
    function UsesWindowText  : boolean;
  public
    //Match elements
    HostName    : string;
    HostVersion : integer;
    WindowClass : string;
    WindowText  : string;

    //Config properties
    ConfigID    : string;
    UseKeyHook  : boolean;
    HookLevel   : integer;
    MatchElements : TMatchElementSet;
    constructor Create;
    property MatchType : TMatchType read GetMatchType;
  end;

  TKeyboardHookConfig = class
  private
    function GetHostConfigCount: integer;
    function GetHostConfigs(Index: integer): THostConfig;
  protected
    HostConfigList : TObjectList;

    function FindConfigA(HostName : string):THostConfig;
    function FindConfigB(HostName : string; HostVersion : integer):THostConfig;
    function FindConfigE(HostName, WindowClass : string):THostConfig;
    function FindConfigC(WindowClass : string): THostConfig;
    function FindConfigD(WindowClass, WindowText : string): THostConfig;

    procedure LoadConfigs(Filename:string);

    property HostConfigCount : integer read GetHostConfigCount;
    property HostConfigs[Index:integer]:THostConfig read GetHostConfigs;

  public
    constructor Create;
    destructor Destroy; override;

    function GetTargetHandle(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName : string; Out ConfigID:string; Out UseKeyHook:boolean): hwnd;
  end;





{ TKeyboardHookConfig }

constructor TKeyboardHookConfig.Create;
begin
  HostConfigList := TObjectList.Create;
  HostConfigList.OwnsObjects := true;
end;

destructor TKeyboardHookConfig.Destroy;
begin
  HostConfigList.Free;
  inherited;
end;

function TKeyboardHookConfig.FindConfigA(HostName: string): THostConfig;
var
  c1: Integer;
  cx : THostConfig;
begin
  for c1 := 0 to HostConfigCount-1 do
  begin
    cx := HostConfigs[c1];
    if (cx.MatchType = mtHostName) and (cx.HostName = HostName) then
    begin
      result := cx;
      exit; //==================================>> exit >>=======>>
    end;
  end;
  result := nil;
end;

function TKeyboardHookConfig.FindConfigB(HostName: string; HostVersion: integer): THostConfig;
var
  c1: Integer;
  cx : THostConfig;
begin
  for c1 := 0 to HostConfigCount-1 do
  begin
    cx := HostConfigs[c1];
    if (cx.MatchType = mtHostName_HostVersion) and (cx.HostName = HostName) and (cx.HostVersion = HostVersion) then
    begin
      result := cx;
      exit; //==================================>> exit >>=======>>
    end;
  end;
  result := nil;
end;

function TKeyboardHookConfig.FindConfigC(WindowClass: string): THostConfig;
var
  c1: Integer;
  cx : THostConfig;
begin
  for c1 := 0 to HostConfigCount-1 do
  begin
    cx := HostConfigs[c1];
    if (cx.MatchType = mtWindowClass) and (cx.WindowClass = WindowClass) then
    begin
      result := cx;
      exit; //==================================>> exit >>=======>>
    end;
  end;
  result := nil;
end;


function TKeyboardHookConfig.FindConfigD(WindowClass, WindowText: string): THostConfig;
var
  c1: Integer;
  cx : THostConfig;
begin
  for c1 := 0 to HostConfigCount-1 do
  begin
    cx := HostConfigs[c1];
    if (cx.MatchType = mtWindowClass_WindowText) and (cx.WindowClass = WindowClass) and (cx.WindowText = WindowText) then
    begin
      result := cx;
      exit; //==================================>> exit >>=======>>
    end;
  end;
  result := nil;
end;


function TKeyboardHookConfig.FindConfigE(HostName,  WindowClass: string): THostConfig;
var
  c1: Integer;
  cx : THostConfig;
begin
  for c1 := 0 to HostConfigCount-1 do
  begin
    cx := HostConfigs[c1];
    if (cx.MatchType = mtHostName_WindowClass) and (cx.HostName = HostName) and (cx.WindowClass = WindowClass) then
    begin
      result := cx;
      exit; //==================================>> exit >>=======>>
    end;
  end;
  result := nil;
end;


function TKeyboardHookConfig.GetHostConfigCount: integer;
begin
  result := HostConfigList.Count;
end;

function TKeyboardHookConfig.GetHostConfigs(Index: integer): THostConfig;
begin
  result := HostConfigList[Index] as THostConfig;
end;

procedure TKeyboardHookConfig.LoadConfigs(Filename: string);
var
  c1       : integer;
  xml      : TNativeXml;
  NodeList : TsdNodeList;
  aConfig  : THostConfig;
  Name     : UTF8String;
  Value    : UTF8String;
begin
  xml := TNativeXml.Create(nil);
  AutoFree(@xml);

  NodeList := TsdNodeList.Create;
  AutoFree(@NodeList);

  xml.LoadFromFile(FileName);

  if assigned(xml.Root) then
  begin
    xml.Root.FindNodes('HostConfig', NodeList);

    for c1 := 0 to NodeList.Count-1 do
    begin
      aConfig := THostConfig.Create;
      HostConfigList.Add(aConfig);
      //------------------------------
      //-- Get data from XML node ----
      //------------------------------


      //--- Config Match Elements ----

      Name := 'HostName';
      if NodeList[c1].HasAttribute(Name) then
      begin
        Value := NodeList[c1].AttributeValueByName[Name];
        aConfig.HostName := String(Value);
        aConfig.MatchElements := aConfig.MatchElements + [meHostName];
      end;

      Name := 'HostVersion';
      if NodeList[c1].HasAttribute(Name) then
      begin
        Value := NodeList[c1].AttributeValueByName[Name];
        aConfig.HostVersion := DataIO_StrToInt(string(Value), 0);
        aConfig.MatchElements := aConfig.MatchElements + [meHostVersion];
      end;

      Name := 'WindowClass';
      if NodeList[c1].HasAttribute(Name) then
      begin
        Value := NodeList[c1].AttributeValueByName[Name];
        aConfig.WindowClass := string(Value);
        aConfig.MatchElements := aConfig.MatchElements + [meWindowClass];
      end;


      Name := 'WindowText';
      if NodeList[c1].HasAttribute(Name) then
      begin
        Value := NodeList[c1].AttributeValueByName[Name];
        aConfig.WindowText := string(Value);
        aConfig.MatchElements := aConfig.MatchElements + [meWindowText];
      end;

      //--- General properties -----

      Name := 'ConfigID';
      if NodeList[c1].HasAttribute(Name) then
      begin
        Value := NodeList[c1].AttributeValueByName[Name];
        aConfig.ConfigID := string(Value);
      end;

      Name := 'UseKeyHook';
      if NodeList[c1].HasAttribute(Name)
        then Value := NodeList[c1].AttributeValueByName[Name]
        else Value := 'true';
      aConfig.UseKeyHook := DataIO_StrToBool(string(Value), false);

      Name := 'HookLevel';
      if NodeList[c1].HasAttribute(Name)
        then Value := NodeList[c1].AttributeValueByName[Name]
        else Value := '0';
      aConfig.HookLevel := DataIO_StrToInt(string(Value), 0);
      //----------------------------------------------
      //-- Finished gettting data from xml node ------
      //----------------------------------------------
    end;
  end;
end;


function TKeyboardHookConfig.GetTargetHandle(aHostName:string; aHostVersion:integer; aEditorWindow:hwnd; aConfigFileName : string; Out ConfigID:string; Out UseKeyHook:boolean): hwnd;
var
  cx : THostConfig;
  ParentHandle : hwnd;
  WindowClass : string;
  WindowText : string;
begin
  cx := nil;

  try
    LoadConfigs(aConfigFileName);

    ParentHandle := GetParent(aEditorWindow);
    WindowClass  := eeGetClassName(ParentHandle);
    WindowText   := eeGetWindowText(ParentHandle);

    if (cx = nil) then cx := FindConfigD(WindowClass, WindowText);
    if (cx = nil) then cx := FindConfigC(WindowClass);
    if (cx = nil) then cx := FindConfigB(aHostName, aHostVersion);
    if (cx = nil) then cx := FindConfigE(aHostName, WindowClass);
    if (cx = nil) then cx := FindConfigA(aHostName);

    if cx <> nil then
    begin
      // NOTE: Here we reduce the hook level by one.
      // The keyboard hook config was originally made for Poise, which
      // uses the Poise GUI form handle as it's reference handle.
      // Lucidity (and possibly more recent plugins) use the window handle
      // that is supplied to the VST editor Open() method.
      // That way the plugin GUI form can be contained within any number
      // of child windows and still use the same reference handle.
      cx.HookLevel := cx.HookLevel-1;
    end;


    if cx <> nil then
    begin
      if cx.UseKeyHook then
      begin
        UseKeyHook := cx.UseKeyHook;
        ConfigID   := cx.ConfigID;
        result     := GetParentHandle(aEditorWindow, cx.HookLevel);
      end else
      begin
        UseKeyHook := false;
        ConfigID   := cx.ConfigID;
        result     := 0;
      end;
    end else
    begin
      UseKeyHook := false;
      ConfigID   := 'No Config Match';
      result     := 0;
    end;
  except
    UseKeyHook := false;
    ConfigID := 'Error';
    result := 0;
  end;
end;

{ THostConfig }

constructor THostConfig.Create;
begin

  HostName    := '';
  HostVersion := 0;
  WindowClass := '';
  WindowText  := '';
  ConfigID    := '';
  UseKeyHook  := true;
  HookLevel   := 0;
  MatchElements := [];
end;

function THostConfig.UsesHostName: boolean;
begin
  if meHostName in MatchElements
    then result := true
    else result := false;
end;

function THostConfig.UsesHostVersion: boolean;
begin
  if meHostVersion in MatchElements
    then result := true
    else result := false;
end;

function THostConfig.UsesWindowClass: boolean;
begin
  if meWindowClass in MatchElements
    then result := true
    else result := false;
end;

function THostConfig.UsesWindowText: boolean;
begin
  if meWindowText in MatchElements
    then result := true
    else result := false;
end;

function THostConfig.GetMatchType: TMatchType;
begin
  //TMatchType = (mtUnknown, mtHostName, mtHostName_HostVersion, mtHostName_WindowClass, mtWindowClass, mtWindowClass_WindowText);

  if  (UsesHostName    = true)
  and (UsesHostVersion = false)
  and (UsesWindowClass = false)
  and (UsesWindowText  = false)  then
  begin
    result := mtHostName;
    exit;
  end;

  if  (UsesHostName    = true)
  and (UsesHostVersion = true)
  and (UsesWindowClass = false)
  and (UsesWindowText  = false) then
  begin
    result := mtHostName_HostVersion;
    exit;
  end;

  if  (UsesHostName    = true)
  and (UsesHostVersion = false)
  and (UsesWindowClass = true)
  and (UsesWindowText  = false) then
  begin
    result := mtHostName_WindowClass;
    exit;
  end;

  if  (UsesHostName    = false)
  and (UsesHostVersion = false)
  and (UsesWindowClass = true)
  and (UsesWindowText  = false) then
  begin
    result := mtWindowClass;
    exit;
  end;

  if  (UsesHostName    = false)
  and (UsesHostVersion = false)
  and (UsesWindowClass = true)
  and (UsesWindowText  = true) then
  begin
    result := mtWindowClass_WindowText;
    exit;
  end;



   //if we've made it this far, no match type has been found.
   result := mtUnknown;
end;




//==============================================================================================================================
//==============================================================================================================================
//==============================================================================================================================

function GetParentHandle(aHandle:hwnd; Level:integer):hwnd;
var
  c1:integer;
  h:hwnd;
begin
  h := aHandle;
  for c1 := 0 to Level - 1 do
  begin
    h := GetParent(h);
  end;
  result := h;
end;

function GetHandleForHotkeyHook(EditorHandle: hwnd; VstHost: TVstHost): hwnd;
begin
  // NOTE: VST GUIs are usully placed inside one or more container controls/windows. Often one
  // of these outer container windows will have 'focus' when the VST GUI is in front.
  // This function returns the correct handle the keyboard hook routine should watch depending
  // on what host is being used.

  case VstHost of
    vhUnKnown:          result := GetParentHandle(EditorHandle, 2);
    vhEnergyXT:         result := GetParentHandle(EditorHandle, 2);
    vhEnergyXT2:        result := GetParentHandle(EditorHandle, 2);
    vhCubaseSX2:        result := GetParentHandle(EditorHandle, 2);
    vhCubase5:          result := GetParentHandle(EditorHandle, 3);
    vhReaper:           result := GetParentHandle(EditorHandle, 2);
    vhReaperBridge32:   result := GetParentHandle(EditorHandle, 1);
    vhTracktion3:       result := GetParentHandle(EditorHandle, 0);
    vhFLStudio:         result := GetParentHandle(EditorHandle, 3);
    vhPodium:           result := GetParentHandle(EditorHandle, 1);
    vhAbletonLive:      result := GetParentHandle(EditorHandle, 1);
    vhAcidPro6:         result := GetParentHandle(EditorHandle, 4);
    vhSonar:            result := GetParentHandle(EditorHandle, 4);
    vhMultiTrackStudio: result := GetParentHandle(EditorHandle, 3);
    vhSamplitude11:     result := GetParentHandle(EditorHandle, 2);
    vhStudioOne:        result := GetParentHandle(EditorHandle, 2);
    vhJBridge:          result := GetParentHandle(EditorHandle, 4);
  else
    result := GetParentHandle(EditorHandle, 2);
  end;


end;


function GetHandleForKeyHook(aHostName: string; aHostVersion: integer; aEditorWindow: hwnd; aConfigFileName: string; Out ConfigID:string; Out UseKeyHook:boolean):hwnd;
var
  Config  : TKeyboardHookConfig;
begin
  if FileExists(aConfigFileName) = false then
  begin
    ConfigID   := 'Error: No Config File';
    UseKeyHook := false;
    result     := 0;
    //VstHost := DetectVstHostFromWindow(aEditorWindow);
    //result  := GetHandleForHotkeyHook(aEditorWindow, VstHost);
  end else
  begin
    Config := TKeyboardHookConfig.Create;
    AutoFree(@Config);
    result := Config.GetTargetHandle(aHostName, aHostVersion, aEditorWindow, aConfigFileName, ConfigID, UseKeyHook);
  end;
end;




end.
