unit ACS_Indicator;

interface

uses
   Classes, ACS_Types, ACS_Classes, ACS_Procs;

type

  TSoundIndicator = class(TACSConverter)
  private
    Lock : Boolean;
    Window : array[0..1023] of Double;
    FValues : array[0..31] of Double;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure GetValues(var Values : array of Double);
    procedure Init; override;
    procedure Flush; override;
  end;

implementation

  constructor TSoundIndicator.Create;
  begin
    inherited Create(AOwner);
    HannWindow(@Window, 1024, True);
  end;

  destructor TSoundIndicator.Destroy;
  begin
    inherited Destroy;
  end;

  function TSoundIndicator.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    Result := FInput.BitsPerSample;
  end;

  function TSoundIndicator.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    Result := FInput.Channels;
  end;

  function TSoundIndicator.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    Result := FInput.SampleRate;
  end;

  procedure TSoundIndicator.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    Buisy := True;
    FInput.Init;
    FSize := FInput.Size;
    FillChar(FValues[0], SizeOf(Double)*32, 0);
    Lock := False;
    FPosition := 0;
  end;

  procedure TSoundIndicator.Flush;
  begin
    FInput.Flush;
    Buisy := False;
    Lock := False;
  end;

  function TSoundIndicator.GetData;
  var
    i, j, k, NumSamples : Integer;
    P : Pointer;
    P8 : PBuffer8;
    P16 : PBuffer16;
    PS8 : PStereoBuffer8;
    PS16 : PStereoBuffer16;
    DA : array[0..63] of Double;
    C1 : array[0..63] of TComplex;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    while InputLock do;
    InputLock := True;
    Result := FInput.GetData(Buffer, BufferSize);
    InputLock := False;
    FPosition := Finput.Position;
    if Result = 0 then Exit;
    if Lock then Exit;
    Lock := True;
    k := Result;
    GetMem(P, k);
    Move(Buffer^, P^, k);
    if FInput.BitsPerSample = 8 then
    begin
      if FInput.Channels = 1 then NumSamples := k
      else NumSamples := k shr 1;
    end else
    begin
      if FInput.Channels = 1 then NumSamples := k shr 1
      else NumSamples := k shr 2;
    end;
    for i := 0 to (NumSamples div 64) - 1 do
    begin
      if FInput.BitsPerSample = 8 then
      begin
        if FInput.Channels = 1 then
        begin
          P8 := P;
          for j := 0 to 63 do DA[j] := P8[i*64+j];
        end else
        begin
          PS8 := P;
          for j := 0 to 63 do DA[j] := (PS8[i*64+j].Left+PS8[i*64+j].Right)/2;
        end
      end else
      begin
        if FInput.Channels = 1 then
        begin
          P16 := P;
          for j := 0 to 63 do DA[j] := P16[i*64+j];
        end else
        begin
          PS16 := P;
          for j := 0 to 63 do DA[j] := (PS16[i*64+j].Left+PS16[i*64+j].Right)/2;
        end
      end;
      MultDoubleArrays(@Window[0], @DA[0], 64);
      for j := 0 to 63 do
      begin
        C1[j].Re := DA[j];
        C1[j].Im := 0;
      end;
      ComplexFFT(@C1, 64, 1);
      LgMagnitude(@C1[0], @DA[0], 64, 0);
      try
        for j := 0 to 31 do FValues[j]:=FValues[j]+DA[j];
      except
        for j := 0 to 31 do FValues[j] := 0;
      end;
    end;
    for j := 0 to 31 do FValues[j]:=FValues[j]/(NumSamples div 64);
    FreeMem(P);
    Lock := False;
  end;

  procedure TSoundIndicator.GetValues;
  var
    i : Integer;
  begin
    while Lock do;
    Lock := True;
    for i := 0 to 31 do Values[i] := FValues[i]*20; //ValCount;
    for i := 0 to 31 do FValues[i] := 0;
    Lock := False;
  end;

end.
