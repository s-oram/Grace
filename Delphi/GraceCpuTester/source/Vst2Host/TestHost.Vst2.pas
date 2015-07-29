unit TestHost.Vst2;

interface

uses
  TestHost.Vst2.DAEffect,
  TestHost.Vst2.DAEffectX;

type
  PVst2HostInfo = ^TVst2HostInfo;
  TVst2HostInfo = record
    SampleRate : integer;
    MaxBufferSize : integer;
  end;

  TVst2Plugin = class
  private
  protected
    HostInfo : PVst2HostInfo;
  public
    constructor Create(const aHostInfo : PVst2HostInfo);
    destructor Destroy; override;

    function LoadPlugin(const FileName : string):boolean;
  end;

implementation

uses
  WinApi.Windows;

{ TVst2Plugin }

constructor TVst2Plugin.Create(const aHostInfo : PVst2HostInfo);
begin
  HostInfo := aHostInfo;
end;

destructor TVst2Plugin.Destroy;
begin

  inherited;
end;


function TVst2Plugin.LoadPlugin(const FileName: string): boolean;
var
  VstDllHandle:THandle;
  Main:TMainProc;
begin
  VstDllHandle := LoadLibrary(@FileName[1]);
  if VstDllHandle <> 0 then
  begin
    @Main := GetProcAddress(VstDllHandle,'main');
    //TODO: Check main has been found.


  end;


  //WriteLn('VST Plugin has been created.');
end;

end.
