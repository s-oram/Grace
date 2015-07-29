unit PluginHost.Vst2;

interface

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
begin

end;

end.
