unit Mocks.AudioPlugin;

interface

uses
  AudioPlugin;

type
  TMockAudioPlugin = class(TAudioPlugin)
  private
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TMockAudioPlugin }

constructor TMockAudioPlugin.Create;
begin
  inherited;
  self.SetVstParameterCount(10);
end;

destructor TMockAudioPlugin.Destroy;
begin

  inherited;
end;

end.
