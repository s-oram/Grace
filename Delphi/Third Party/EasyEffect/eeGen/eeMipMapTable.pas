{
  MipMapTable is designed to be used by the MipMapOsc class only.
}

unit eeMipMapTable;

interface

uses
  BlockProcess.DoubleLowPass,
  VamLib.MoreTypes;

type
  TMipMap = array of array of Single;

  TMipMapTable = class
  private
    fMipMap: TMipMap;
    fBaseTableSize: integer;
    fTableCount: integer;
    fUsingDecimatedTables: boolean;
  public
    constructor Create(aBaseTableSize : integer = 2048; aTableCount : integer = 8; aUseDecimatedTables : boolean = true);
    destructor Destroy; override;

    procedure ImportWaveData(p:PSingle);

    property MipMap : TMipMap read fMipMap write fMipMap;

    property TableCount           : integer read fTableCount;
    property BaseTableSize        : integer read fBaseTableSize;
    property UsingDecimatedTables : boolean read fUsingDecimatedTables;
  end;

implementation

uses
  Math, uAutoFree, DewResearchSampleRateConversion;

{ TMipMapTableData }

constructor TMipMapTable.Create(aBaseTableSize : integer = 2048; aTableCount : integer = 8; aUseDecimatedTables: boolean = true);
var
  c1 : integer;
  ts : integer;
begin
  //== Set mip map table properties ===
  fTableCount := aTableCount;
  fBaseTableSize  := aBaseTableSize;
  fUsingDecimatedTables := aUseDecimatedTables;


  SetLength(fMipMap, TableCount);


  if UsingDecimatedTables then
  begin
    // When using decimated tables, reduce the table size by 2 for each successive table.
    ts := BaseTableSize;
    for c1 := 0 to TableCount-1 do
    begin
      assert(ts > 1);
      setLength(fMipMap[c1], ts);
      ts := ts div 2;
    end;
  end else
  begin
    for c1 := 0 to TableCount-1 do
    begin
      setLength(fMipMap[c1], BaseTableSize);
    end;
  end;

end;

destructor TMipMapTable.Destroy;
var
  c1: Integer;
begin
  for c1 := 0 to TableCount-1 do
  begin
    SetLength(fMipMap[c1], 0);
  end;
  SetLength(fMipMap, 0);
  inherited;
end;

procedure TMipMapTable.ImportWaveData(p: PSingle);
var
  c1: Integer;
  DownSampler : TDownsampler;
  SourceIndex : integer;
  DestIndex   : integer;
  SrcWaveData : PSingle;
  Src, Dst    : PSingle;
  SrcSampleFrames : integer;
  c2: Integer;
  LowPass : TBpDoubleLowpass;
  Freq : double;
begin
  LowPass := TBpDoubleLowpass.Create;
  AutoFree(@LowPass);

  if UsingDecimatedTables then
  begin
    SrcWaveData := p;
    for c1 := 0 to BaseTableSize-1 do
    begin
      MipMap[0, c1] := SrcWaveData^;
      inc(SrcWaveData);
    end;

    DownSampler := TDownSampler.Create;
    AutoFree(@DownSampler);

    for c1 := 1 to TableCount-1 do
    begin
      Src := @MipMap[0, 0];
      Dst := @MipMap[c1, 0];
      SrcSampleFrames := Length(MipMap[0]);

      DownSampler.DecimationFactor := round(Power(2, c1));
      DownSampler.Reset;
      DownSampler.Process(Src, Dst, SrcSampleFrames);
    end;
  end else
  begin
    Freq := 0.9;
    for c1 := 0 to TableCount-1 do
    begin
      SrcWaveData := p;
      for c2 := 0 to BaseTableSize-1 do
      begin
        MipMap[c1, c2] := SrcWaveData^;
        inc(SrcWaveData);
      end;

      LowPass.Frequency := Freq;
      Freq := Freq * 0.5;

      LowPass.Process(@MipMap[c1,0], @MipMap[c1,0], BaseTableSize);
    end;
  end;

  //=== Attempt two ====
  // This code works without errors but the results are not perfect.
  // The final three wavetables are mal-formed.
  {
  for c1 := 0 to 2048-1 do
  begin
    MipMap[0, c1] := p^;
    inc(p);
  end;

  DownSampler := TDownSampler.Create;
  AutoFree(@DownSampler);


  for c1 := 1 to 7 do
  begin
    Src := @MipMap[c1-1, 0];
    Dst := @MipMap[c1, 0];
    SrcSampleFrames := Length(MipMap[c1-1]);

    DownSampler.DecimationFactor := 2;
    DownSampler.Reset;
    DownSampler.Process(Src, Dst, SrcSampleFrames);
  end;
  }


  //=== OLD IMPORT code which wasn't working.... ====
  {
  SrcSampleFrames := 2048;

  Src:= @MipMap[0, 0];

  for c1 := 1 to 7 do
  begin
    DestIndex   := c1;
    Dst         := @MipMap[DestIndex, 0];

    DownSampler.DecimationFactor := round(Power(2, c1));
    DownSampler.Reset;
    DownSampler.Process(Src, Dst, SrcSampleFrames);

    SrcSampleFrames := SrcSampleFrames div 2;
  end;
  }
end;



end.
