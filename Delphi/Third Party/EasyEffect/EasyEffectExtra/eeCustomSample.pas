unit eeCustomSample;

interface

type
  TSampleDataFormat = (dfFloat, dfInt);

  PSampleProperties = ^TSampleProperties;
  TSampleProperties = record
    IsValid          :boolean; //Is the sample data loaded correctly.
    ChannelCount     :integer;
    SampleFrames     :integer;
    SampleRate       :integer;
    SourceBitDepth   :integer;
    Ch1              :Pointer;
    Ch2              :Pointer;
    SampleDataFormat :TSampleDataFormat;
  end;

  TCustomSample = class(TInterfacedObject)
  private
  protected
    fProperties: TSampleProperties;
    function GetObject : TObject;
    function GetSampleProperties:PSampleProperties;
    function GetSampleMem(ChannelCount, SampleFrames:integer):boolean; virtual; abstract;
  public
    constructor Create; virtual;
	  destructor Destroy; override;

    function ReserveSampleMemory(ChannelCount, SampleFrames:integer):boolean;
    function Init(Channels, SampleFrames, SampleRate, SourceBitDepth:integer):boolean; virtual; abstract;
    procedure Clear; virtual; abstract;

    function LoadFromFile(FileName:string):boolean; virtual; abstract;
    function SaveToFile(FileName:string):boolean; virtual; abstract;

    property Properties:TSampleProperties read fProperties write fProperties;
  end;

implementation

{ TCustomSample }

constructor TCustomSample.Create;
begin

end;

destructor TCustomSample.Destroy;
begin

  inherited;
end;

function TCustomSample.GetObject: TObject;
begin
  result := self;
end;

function TCustomSample.GetSampleProperties: PSampleProperties;
begin
  result:= @fProperties;
end;

function TCustomSample.ReserveSampleMemory(ChannelCount, SampleFrames: integer): boolean;
begin
  result := GetSampleMem(ChannelCount, SampleFrames);
end;

end.
