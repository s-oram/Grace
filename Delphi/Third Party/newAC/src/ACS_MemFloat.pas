unit ACS_MemFloat;

interface

uses
  Classes,
  ACS_Classes;

const
  _BUFFERSIZE = 4096;

type


  TMemFloatOut = class(TAuOutput)
  private
    TempBuffer : array[0.._BUFFERSIZE-1] of byte;
    BytesPerSample : integer;
    SampleFrameOffset : integer;
    fChannels: integer;
    fSampleFrames: integer;
  protected
    procedure ClearSampleData;
    procedure ReserveSampleMemory(const Channels, SampleFrames : integer);

    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Done; override; // Calls FInput.Flush
    procedure Prepare; override; // Calls FInput.init
  public
    SampleData : array of array of single;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;


    property SampleFrames : integer read fSampleFrames;
    property Channels     : integer read fChannels;
  end;

implementation

uses
  SysUtils;


function Read_8BitPCMToFloat(var Ptr : Pointer):single;
begin
  result := Byte(ptr^) / High(Byte) * 2 - 1;
  inc(PByte(Ptr), 1);
end;

function Read_16BitPCMToFloat(var Ptr : Pointer):single;
begin
  result := SmallInt(ptr^) / 32767;
  inc(PByte(Ptr), 2);
end;

function Read_24BitPCMToFloat(var Ptr : Pointer):single;
begin
  inc(PByte(Ptr), 3);
end;


{ TMemFloatOutput }

constructor TMemFloatOut.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMemFloatOut.Destroy;
begin
  ClearSampleData;
  inherited;
end;

procedure TMemFloatOut.ReserveSampleMemory(const Channels, SampleFrames: integer);
var
  c1: Integer;
begin
  ClearSampleData;

  SetLength(SampleData, Channels);

  for c1 := 0 to Length(SampleData)-1 do
  begin
    SetLength(SampleData[c1], SampleFrames);
  end;
end;



procedure TMemFloatOut.ClearSampleData;
var
  c1: Integer;
begin
  for c1 := 0 to Length(SampleData)-1 do
  begin
    SetLength(SampleData[c1], 0);
  end;
  SetLength(SampleData, 0);
end;


procedure TMemFloatOut.Prepare;
begin
  inherited;

  Input.Init;
  BytesPerSample := Input.BitsPerSample div 8;

  ReserveSampleMemory(Input.Channels, Input.TotalSamples);
  SampleFrameOffset := 0;

  fChannels     := Input.Channels;
  fSampleFrames := Input.TotalSamples;
end;

procedure TMemFloatOut.Done;
begin
  inherited;
  Input.Flush;
end;

function TMemFloatOut.DoOutput(Abort: Boolean): Boolean;
var
  Ptr : Pointer;
  Len : LongWord;
  ByteCount : integer;
  SampleFrames : integer;
  c1: Integer;
  c2: Integer;
begin
  if Abort then
  begin
    (* We don't close file here to avoid exceptions
      if output component's Stop method is called *)
    Result := False;
    Exit;
  end;

  Len := _BUFFERSIZE;
  Input.GetData(Ptr, Len);
  if Len > 0
    then result := true
    else result := false;

  if Len > 0 then
  begin
    // copy the data into the mem float output.
    // TODO:HIGH work on adding input conversion here.

    ByteCount := Len;

    SampleFrames := Len div BytesPerSample div Input.Channels;

    if SampleFrames + SampleFrameOffset > Input.TotalSamples
      then exit;


    case BytesPerSample of
      1:
      begin
        for c1 := 0 to SampleFrames-1 do
        begin
          for c2 := 0 to Input.Channels-1 do
          begin
            SampleData[c2, SampleFrameOffset + c1] := Read_8BitPCMToFloat(ptr);
          end;
        end;
      end;

      2:
      begin
        for c1 := 0 to SampleFrames-1 do
        begin
          for c2 := 0 to Input.Channels-1 do
          begin
            SampleData[c2, SampleFrameOffset + c1] := Read_16BitPCMToFloat(ptr);
          end;
        end;
      end;

      3:
      begin
        for c1 := 0 to SampleFrames-1 do
        begin
          for c2 := 0 to Input.Channels-1 do
          begin
            SampleData[c2, SampleFrameOffset + c1] := Read_24BitPCMToFloat(ptr);
          end;
        end;
      end;
    else
      raise Exception.Create('Unexpected Value.');
    end;

    Inc(SampleFrameOffset, SampleFrames);
  end;

end;








end.
