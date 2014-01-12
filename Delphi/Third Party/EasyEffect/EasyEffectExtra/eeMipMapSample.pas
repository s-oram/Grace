unit eeMipMapSample;

interface

uses
  VamLib.MoreTypes, eeMipMapLayer, Contnrs;

const
  kBundleLayerCount = 6;
type

  //MonoSumMode - controls how a stereo signal is mixed down to mono.
  // msCenter is as in Mid/Side processing. Taking the mid channel.
  // msSumAndScale. Add both channels and rescales to maximum peak level.
  //TMonoSumMode = (msLeftOnly, msRightOnly, msCenter, msSumAndScale);

  TLoadParameters = record
    //ForceMono         :boolean;
    //MonoSumMode       :TMonoSumMode;
    //InitialBufferSize :integer; //set to 0 for same size as loaded sample. (Initial as in the first mip map layer)
    ForceStereo       :boolean;
    InitialSampleRate :integer; //set to 0 for same samplerate as loaded sample.
  end;

  TMipMapBundle = array[0..kBundleLayerCount-1] of TMipMapLayer;

  PMipMapProperties = ^TMipMapProperties;
  TMipMapProperties = record
    IsValid:boolean;
    FileName:string;
    RootSampleFrames :integer;
    RootSampleRate   :integer;
    MipMapLayerCount :integer;
    ChannelCount     :integer;
    procedure Assign(Source:TMipMapProperties);
  end;

  TMipMapSample = class
  private
    fMipMaps    :TMipMapBundle;
    fProperties :TMipMapProperties;
  protected
    function SetMipMapSizes(const ChannelCount, SampleFrames:integer):boolean;
    procedure ClearData;
  public
    constructor Create;
	  destructor Destroy; override;

    function LoadFromFile(const FileName:string; const Par:TLoadParameters):boolean;

    procedure Clear;
    procedure Assign(Source:TMipMapSample);

    //TODO: MipMaps should be made into a list or a default property...
    property MipMaps:TMipMapBundle read fMipMaps write fMipMaps;

    property Properties:TMipMapProperties read fProperties;
  end;




  TMipMapSampleList = class
  private
    function GetCount: integer;
    function GetMipMapSample(Index: integer): TMipMapSample;
  protected
    List:TObjectList;
  public
    constructor Create;
	  destructor Destroy; override;

    function Add(aMipMapSample:TMipMapSample):integer;
    procedure Delete(Index:integer); overload;
    procedure Delete(aMipMapSample:TMipMapSample); overload;

    function FindBy(FileName:string):TMipMapSample;

    property Count:integer read GetCount;
    property MipMapSamples[Index:integer]:TMipMapSample read GetMipMapSample; default;
  end;

implementation

uses
  uGeneralFunctions, SysUtils, AudioIO, uAutoFree, Math,
  eeDspSampleRateConversion, eeSampleInt, eeSampleIntFunctions;

{ TMipMapSampleInt }

constructor TMipMapSample.Create;
begin
  fProperties.IsValid := false;
end;

destructor TMipMapSample.Destroy;
begin
  ClearData;
  inherited;
end;

function TMipMapSample.SetMipMapSizes(const ChannelCount, SampleFrames: integer): boolean;
var
  c1:integer;
  sfc:integer;
begin
  sfc := SampleFrames;

  for c1 := 0 to kBundleLayerCount - 1 do
  begin
    if not assigned(MipMaps[c1]) then fMipMaps[c1] := TMipMapLayer.Create;

    if MipMaps[c1].Init(ChannelCount, sfc) = false then
    begin
      ClearData;
      result := false;
      exit;
    end;

    sfc := sfc div 2;
  end;

  //if the method has made it this far, return true.
  Result := true;
end;


procedure TMipMapSample.Assign(Source: TMipMapSample);
var
  c1: Integer;
begin
  Properties.Assign(Source.Properties);

  for c1 := 0 to kBundleLayerCount - 1 do
  begin
    if assigned(Source.MipMaps[c1]) then
    begin
      if not assigned(Self.MipMaps[c1]) then fMipMaps[c1] := TMipMapLayer.Create;
      Self.MipMaps[c1].Assign(source.MipMaps[c1]);
    end else
    begin
      if assigned(Self.MipMaps[c1]) then self.fMipMaps[c1].Free;      
    end;
  end;
end;

procedure TMipMapSample.Clear;
begin
  ClearData;
end;

procedure TMipMapSample.ClearData;
var
  c1:integer;
begin
  for c1 := 0 to kBundleLayerCount - 1 do
  begin
    if assigned(MipMaps[c1]) then FreeAndNil(fMipMaps[c1]);
  end;
  fProperties.IsValid := false;
end;



function TMipMapSample.LoadFromFile(const FileName: string; const Par:TLoadParameters): boolean;
const
  MinSampleFrames = 1024;
var
  SourceSample     :TSampleInt;
  sfc              :integer;
  ch               :integer;
  c1               :Integer;
  DestSampleFrames :integer;
begin
  Clear;

  fProperties.IsValid := false;

  SourceSample := TSampleInt.Create;
  AutoFree(@SourceSample);


  //Load file
  if SourceSample.LoadFromFile(FileName) = false then
  begin
    result := false;
    exit; //===============================>>
  end;

  // calculate the correct destination sampleframe count (for sample rate conversion)
  // pad to even number of sample frames
  if (Par.InitialSampleRate <> 0) and (Par.InitialSampleRate <> SourceSample.Properties.SampleRate) then
  begin
    DestSampleFrames := round(Par.InitialSampleRate / SourceSample.Properties.SampleRate * SourceSample.Properties.SampleFrames);
  end else
  begin
    DestSampleFrames := SourceSample.Properties.SampleFrames;
  end;

  //Convert to stereo if needed.
  if (SourceSample.Properties.ChannelCount = 1) and (Par.ForceStereo) then
  begin
    SourceSample := MakeStereo(SourceSample);
    if not assigned(SourceSample) then
    begin
      //Something went wrong during the conversion to stereo.
      result := false;
      exit; //===================================>>
    end;
  end;

  //The initial samle needs to be long enough that it can be divided by 2, six times and still be a meaningful length.
  if DestSampleFrames < MinSampleFrames then
  begin
    result := false;
    exit; //===============================>>
  end;


  //Set size for all mip map layers..
  ch  := SourceSample.Properties.ChannelCount;
  sfc := DestSampleFrames;
  if SetMipMapSizes(ch, sfc) = false then
  begin
    result := false;
    exit; //==============================>>
  end;


  //import data
  MipMaps[0].ImportFrom(SourceSample);
  for c1 := 1 to kBundleLayerCount - 1 do
  begin
    MipMaps[c1].ImportFrom(MipMaps[c1-1]);
  end;


  //If we've made it this far, we must be doing ok.
  result := true;

  fProperties.IsValid          := true;
  fProperties.FileName         := FileName;
  fProperties.ChannelCount     := SourceSample.Properties.ChannelCount;
  fProperties.MipMapLayerCount := kBundleLayerCount;
  fProperties.RootSampleFrames := DestSampleFrames;
  
  if (Par.InitialSampleRate <> 0)
    then fProperties.RootSampleRate := Par.InitialSampleRate
    else fProperties.RootSampleRate := SourceSample.Properties.SampleRate;




end;




{ TMipMapList }

constructor TMipMapSampleList.Create;
begin
  List := TObjectList.Create;
  List.OwnsObjects := true;
end;

destructor TMipMapSampleList.Destroy;
begin
  List.Free;
  inherited;
end;

function TMipMapSampleList.GetCount: integer;
begin
  result := List.Count;
end;

function TMipMapSampleList.GetMipMapSample(Index: integer): TMipMapSample;
begin
  result := List[Index] as TMipMapSample;
end;

function TMipMapSampleList.Add(aMipMapSample: TMipMapSample): integer;
begin
  result := List.Add(aMipMapSample);
end;

procedure TMipMapSampleList.Delete(Index: integer);
begin
  List.Delete(Index);
end;

procedure TMipMapSampleList.Delete(aMipMapSample: TMipMapSample);
var
  Index:integer;
begin
  Index := List.IndexOf(aMipMapSample);
  if Index <> -1 then Delete(Index);
end;



function TMipMapSampleList.FindBy(FileName: string): TMipMapSample;
var
  c1: Integer;
begin
  for c1 := 0 to List.Count - 1 do
  begin
    if FileName = (List[c1] as TMipMapSample).Properties.FileName then
    begin
      result := (List[c1] as TMipMapSample);
      exit; //============================================================>>
    end;
  end;

  //if we've made it this far, the mip map sample has not been found, so return nil.
  result := nil;
end;




{ TMipMapProperties }

procedure TMipMapProperties.Assign(Source: TMipMapProperties);
begin
  self.IsValid          := Source.IsValid;
  self.FileName         := Source.FileName;
  self.RootSampleFrames := Source.RootSampleFrames;
  self.RootSampleRate   := source.RootSampleRate;
  self.MipMapLayerCount := source.MipMapLayerCount;
  self.ChannelCount     := source.ChannelCount;
end;

end.
