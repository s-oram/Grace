(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2008, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
  *************************************************************

   TTAKIn component is written by Sergei Borisov, <jr_ross@mail.ru>
*)

unit ACS_TAK;

(* $Id: ACS_TAK.pas 1117 2010-01-22 09:28:59Z andrei.borovsky $ *)

interface

uses
  Classes,
  tak_decoder,
  ACS_Classes,
  ACS_Tags;

type

  (* class TTAKIn *)

  TTAKIn = class(TAuTaggedFileIn)
  private
    FDecoder: TTAKDecoder;
    FBuffer: array of Byte;

    function GetAPEv2Tags : TAPEv2Tags;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    function SeekInternal(var Sample: Int64): Boolean; override;
    procedure GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal); override;
  public
    constructor Create(AOwner: TComponent); override;

    property APEv2Tags : TAPEv2Tags read GetAPEv2Tags;
  published
    property Loop;
    property StartSample;
    property EndSample;
  end;


implementation

uses
  SysUtils;

{ class TTAKIn }

constructor TTAKIn.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) and not TAKDLL_Loaded then
    raise EAuException.Create(TAKDLL_Name + ' library could not be loaded.');
end;

function TTAKIn.GetAPEv2Tags: TAPEv2Tags;
begin
  OpenFile();

  Result := _APEv2Tags;
end;

procedure TTAKIn.OpenFile;
var
  i: Integer;
  tag_id: String;
begin
  OpenCS.Enter();
  try
    if FOpened = 0 then begin
      FValid := False;

      if not FStreamAssigned then
        if FWideFileName = '' then
          raise EAuException.Create('File name is not assigned')
        else
          FStream := TAuFileStream.Create(
            FWideFileName, fmOpenRead or fmShareDenyWrite);

      FDecoder := TTAKDecoder.Create(FStream);

      FValid := True;
      FSeekable := True;

      FBPS := FDecoder.SampleBits;
      FSR := FDecoder.SampleRate;
      FChan := FDecoder.ChannelNum;
      FTotalSamples := FDecoder.SampleNum;

      FPosition := 0;
      if FTotalSamples >= 0 then
        FSize := FTotalSamples * ((FBPS + 7) shr 3) * FChan
      else
        FSize := - 1;

      _APEv2Tags.Clear();
      for i := 0 to _APEv2Tags.IdCount - 1 do begin
        {$WARNINGS OFF}
        tag_id := _APEv2Tags.Ids[i];
        {$WARNINGS ON}
        if FDecoder.TagPresents[tag_id] then
        {$WARNINGS OFF}
          _APEv2Tags[tag_id] := FDecoder.TagValue[tag_id];
        {$WARNINGS ON}
      end;

      Inc(FOpened);
    end;
  finally
    OpenCS.Leave();
  end;
end;

procedure TTAKIn.CloseFile;
begin
  OpenCS.Enter();
  try
    if FOpened > 0 then begin
      Dec(FOpened);

      if FOpened = 0 then begin
        try
          FDecoder.Free();
        finally
          FDecoder := nil;
        end;

        if not FStreamAssigned then
          FreeAndNil(FStream);

        SetLength(FBuffer, 0);
      end;
    end;
  finally
    OpenCS.Leave;
  end;
end;

function TTAKIn.SeekInternal(var Sample: Int64): Boolean;
begin
  Result := FDecoder.Seek(Sample);
end;

procedure TTAKIn.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  new_bytes, new_buffer_size,
  sample_size,
  samples, samples_read: Cardinal;
begin
  if not Busy then
    raise EAuException.Create('The stream is not opened');

  if Bytes > 0 then begin
    if FSize >= 0 then begin
      new_bytes := FSize - FPosition;
      if Bytes > new_bytes then
        Bytes := new_bytes;
    end;

    sample_size := ((FBPS + 7) shr 3) * FChan;
    samples := Bytes div sample_size;
    Bytes := samples * sample_size;

    new_buffer_size := samples * sample_size;
    if Cardinal(Length(FBuffer)) < new_buffer_size then
      SetLength(FBuffer, new_buffer_size);

    Buffer := @(FBuffer[0]);

    samples_read := FDecoder.Read(Buffer, samples);

    if samples_read < samples then
      if samples_read = 0 then begin
        Buffer := nil;
        Bytes := 0;
      end
      else
        Bytes := samples_read * sample_size;
  end;
end;

end.

