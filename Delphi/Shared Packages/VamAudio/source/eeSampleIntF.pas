unit eeSampleIntF;

interface

uses
  Classes, Types, eeCustomSample;

type
  ISample = interface
    ['{5CF7274A-1789-46E0-B2E3-00F2DD7299E3}']
    function GetObject : TObject;
    function ReserveSampleMemory(ChannelCount, SampleFrames:integer):boolean;
    function LoadFromFile(FileName:string):boolean;
    function SaveToFile(FileName:string):boolean;
    function GetSampleProperties:PSampleProperties;
  end;

implementation

end.
