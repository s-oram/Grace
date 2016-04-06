unit Mocks.AudioPlugin;

interface

uses
  AudioPlugin.PlugMain,
  VamLib.MoreTypes,
  VamLib.FlexValue,
  AudioPlugin.Globals,
  AudioPlugin.RunTimeInfo;

type
  TMockAudioPlugin = class(TAudioPlug)
  private
  public
    AudioProcessHistory : TFlexList;

    constructor Create(const GlobalsPtr : Pointer); override;
    destructor Destroy; override;

    procedure Resume(const RunTimeInfo : TRunTimeInfo); override;

    procedure ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer); override;
  end;

implementation

{ TMockAudioPlugin }

constructor TMockAudioPlugin.Create(const GlobalsPtr : Pointer);
begin
  inherited;
  AudioProcessHistory := TFlexList.Create;
end;

destructor TMockAudioPlugin.Destroy;
begin
  AudioProcessHistory.Free;
  inherited;
end;

procedure TMockAudioPlugin.Resume(const RunTimeInfo : TRunTimeInfo);
begin
  inherited;

  AudioProcessHistory.Clear;
end;

procedure TMockAudioPlugin.ProcessAudio(Inputs, Outputs: PPSingle; SampleFrames: integer);
begin
  inherited;
  AudioProcessHistory.Add(Flex(SampleFrames));
end;



end.
