unit AggSvgException;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  SysUtils,
  AggBasics;

type
  PSvgException = ^TSvgException;

  TSvgException = class(Exception)
  private
    FMessage: PAnsiChar;
  public
    constructor Create; overload;
    constructor Create(const Text: AnsiString); overload;
    constructor Create(Exc: PSvgException); overload;

    constructor CreateFmt(const Text: AnsiString; const Args: array of const);

    procedure Free;
    function GetMessage: PAnsiChar;
  end;

function GetDouble(Ptr: PAnsiChar): Double;

implementation


{ TSvgException }

constructor TSvgException.Create;
begin
  FMessage := nil;
end;

constructor TSvgException.Create(const Text: AnsiString);
var
  Max: Integer;
begin
  FMessage := nil;

  if AggGetMem(Pointer(FMessage), 4096) then
  begin
    Max := Length(Text);

    if Max > 4095 then
      Max := 4095;

    Move(Text[1], FMessage^, Max);

    PInt8(PtrComp(FMessage) + Max)^ := 0;
  end;
end;

constructor TSvgException.Create(Exc: PSvgException);
var
  Max: Integer;
begin
  FMessage := nil;

  if (Exc <> nil) and (Exc.FMessage <> nil) then
    if AggGetMem(Pointer(FMessage), 4096) then
    begin
      Max := StrLen(Exc.FMessage);

      if Max > 4095 then
        Max := 4095;

      Move(Exc.FMessage^, FMessage^, Max);

      PInt8(PtrComp(FMessage) + Max)^ := 0;
    end;
end;

constructor TSvgException.CreateFmt(const Text: AnsiString;
  const Args: array of const);
begin
  Create(Format(Text, Args));
end;

procedure TSvgException.Free;
begin
  if FMessage <> nil then
    AggFreeMem(Pointer(FMessage), 4096);
end;

function TSvgException.GetMessage;
begin
  Result := PAnsiChar(FMessage);
end;

function GetDouble;
var
  Buf     : array [0..49] of AnsiChar;
  Dst, Max: PAnsiChar;
  Err     : Integer;
begin
  Dst := @Buf[0];
  Max := @Buf[48];

  while Ptr^ <> #0 do
  begin
    case Ptr^ of
      '-', '.', '0'..'9':
        if Dst <> Max then
        begin
          Dst^ := Ptr^;

          Inc(PtrComp(Dst));

        end
        else
          Break;
    else
      Break;
    end;

    Inc(PtrComp(Ptr));
  end;

  Dst^ := #0;

  Val(PAnsiChar(@Buf[0]), Result, Err);
end;

end.
