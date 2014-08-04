(*
  This file is a part of Audio Components Suite v 2.2.
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)


unit ACS_Streams;


interface

uses
  Classes, SysUtils, ACS_Classes;

const

  OUTBUF_SIZE = $4000;


type

  TStreamOut = class(TACSStreamedOutput)
  private
    function GetSR : Integer;
    function GetBPS : Integer;
    function GetCh : Integer;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OutSampleRate : Integer read GetSR;
    property OutBitsPerSample : Integer read GetBPS;
    property OutChannles : Integer read GetCh;
  end;

  TStreamIn = class(TACSStreamedInput)
  private
    FBPS, FChan, FFreq : Integer;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property InBitsPerSample : Integer read FBPS write FBPS;
    property InChannels : Integer read FChan write FChan;
    property InSampleRate : Integer read FFreq write FFreq;
  end;


implementation

procedure TStreamOut.Prepare;
begin
  if not FStreamAssigned then
  raise EACSException.Create('Stream is not assigned.');
  FInput.Init;
end;

procedure TStreamOut.Done;
begin
  FInput.Flush;
end;

function TStreamOut.DoOutput;
var
  Len : Integer;
  P : Pointer;
begin
  // No exceptions Here
  Result := True;
  if not Buisy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  GetMem(P, OUTBUF_SIZE);
  while InputLock do;
  InputLock := True;
  Len := Finput.GetData(P, OUTBUF_SIZE);
  InputLock := False;
  if Len > 0 then
  begin
    Result := True;
    FStream.WriteBuffer(P^, Len);
  end
  else Result := False;
  if Assigned(FOnProgress) then
  begin
    if FInput.Size > 0 then
    if CurProgr <> GetProgress then
    begin
      CurProgr := GetProgress;
      FOnProgress(Self);
    end;
  end;
  FreeMem(P);
end;

constructor TStreamOut.Create;
begin
  inherited Create(AOwner);
end;

destructor TStreamOut.Destroy;
begin
  inherited Destroy;
end;

constructor TStreamIn.Create;
begin
  inherited Create(AOwner);
  FBPS := 8;
  FChan := 1;
  FFreq := 8000;
  FSize := -1;
end;

destructor TStreamIn.Destroy;
begin
  inherited Destroy;
end;

procedure TStreamIn.Init;
begin
  if Buisy then raise EACSException.Create('The component is buisy');
  if not Assigned(FStream) then raise EACSException.Create('Stream object not assigned');
  FPosition := FStream.Position;
  Buisy := True;
  FSize := FStream.Size;
end;

procedure TStreamIn.Flush;
begin
//  FStream.Position := 0;
  Buisy := False;
end;

function TStreamIn.GetData;
begin
  Result := FStream.Read(Buffer^, BufferSize);
  FPosition := FStream.Position;
  //  Inc(FPosition, Result);
  if FPosition >= FSize then
  Result := 0;
end;

function TStreamOut.GetSR;
begin
  if not Assigned(Input) then
  raise EACSException.Create('Input is not assigned.');
  Result := FInput.SampleRate;
end;

function TStreamOut.GetBPS;
begin
  if not Assigned(Input) then
  raise EACSException.Create('Input is not assigned.');
  Result := FInput.BitsPerSample;
end;

function TStreamOut.GetCh;
begin
  if not Assigned(Input) then
  raise EACSException.Create('Input is not assigned.');
  Result := FInput.Channels;
end;

function TStreamIn.GetBPS;
begin
  Result := FBPS
end;

function TStreamIn.GetCh;
begin
  Result := FChan;
end;

function TStreamIn.GetSR;
begin
  Result := Self.FFreq;
end;


end.
