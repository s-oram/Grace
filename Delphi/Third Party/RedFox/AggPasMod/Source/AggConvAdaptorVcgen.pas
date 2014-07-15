unit AggConvAdaptorVcgen;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
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
  AggBasics,
  AggVertexSource;

type
  TAggNullMarkers = class(TAggVertexSource)
  private
    FMarkers: TAggVertexSource;
    procedure SetMarkers(Value: TAggVertexSource);
  public
    constructor Create;

    procedure RemoveAll; override;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); override;
    procedure PrepareSource;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Markers: TAggVertexSource read FMarkers write SetMarkers;
  end;

  TAggInternalStatus = (siInitial, siAccumulate, siGenerate);

  TAggConvAdaptorVcgen = class(TAggVertexSource)
  private
    FSource: TAggCustomVertexSource;
    FGenerator: TAggVertexSource;
    FMarkers: TAggNullMarkers;
    FStatus: TAggInternalStatus;
    FLastCmd: Cardinal;
    FStart: TPointDouble;
    function GetMarkers: TAggVertexSource;
    procedure SetMarkers(Value: TAggVertexSource);
    procedure SetSource(Source: TAggCustomVertexSource);
  public
    constructor Create(Source: TAggCustomVertexSource;
      Generator: TAggVertexSource);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Markers: TAggVertexSource read GetMarkers write SetMarkers;

    property Source: TAggCustomVertexSource read FSource write SetSource;
    property Generator: TAggVertexSource read FGenerator;
  end;


implementation


{ TAggNullMarkers }

constructor TAggNullMarkers.Create;
begin
  inherited Create;

  FMarkers := nil;
end;

procedure TAggNullMarkers.RemoveAll;
begin
  if FMarkers <> nil then
    FMarkers.RemoveAll;
end;

procedure TAggNullMarkers.AddVertex(X, Y: Double; Cmd: Cardinal);
begin
  if FMarkers <> nil then
    FMarkers.AddVertex(X, Y, Cmd);
end;

procedure TAggNullMarkers.PrepareSource;
begin
end;

procedure TAggNullMarkers.Rewind(PathID: Cardinal);
begin
  if FMarkers <> nil then
    FMarkers.Rewind(PathID);
end;

function TAggNullMarkers.Vertex(X, Y: PDouble): Cardinal;
begin
  if FMarkers <> nil then
    Result := FMarkers.Vertex(X, Y)
  else
    Result := CAggPathCmdStop;
end;

procedure TAggNullMarkers.SetMarkers(Value: TAggVertexSource);
begin
  FMarkers := Value;
end;


{ TAggConvAdaptorVcgen }

constructor TAggConvAdaptorVcgen.Create(Source: TAggCustomVertexSource;
  Generator: TAggVertexSource);
begin
  inherited Create;

  FSource := Source;
  FStatus := siInitial;

  FGenerator := Generator;

  FMarkers := TAggNullMarkers.Create;

  FLastCmd := 0;
  FStart.X := 0;
  FStart.Y := 0;
end;

destructor TAggConvAdaptorVcgen.Destroy;
begin
  FMarkers.Free;
  inherited;
end;

procedure TAggConvAdaptorVcgen.SetSource;
begin
  FSource := Source;
end;

procedure TAggConvAdaptorVcgen.SetMarkers(Value: TAggVertexSource);
begin
  FMarkers.SetMarkers(Value);
end;

function TAggConvAdaptorVcgen.GetMarkers: TAggVertexSource;
begin
  if FMarkers.FMarkers <> nil then
    Result := FMarkers.FMarkers
  else
    Result := FMarkers;
end;

procedure TAggConvAdaptorVcgen.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);

  FStatus := siInitial;
end;

function TAggConvAdaptorVcgen.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd : Cardinal;
  Done: Boolean;
label
  _acc, _gen, _end;

begin
  Cmd := CAggPathCmdStop;
  Done := False;

  while not Done do
  begin
    case FStatus of
      siInitial:
        begin
          FMarkers.RemoveAll;

          FLastCmd := FSource.Vertex(@FStart.X, @FStart.Y);
          FStatus := siAccumulate;

          goto _acc;
        end;

      siAccumulate:
        begin
        _acc:
          if IsStop(FLastCmd) then
          begin
            Result := CAggPathCmdStop;

            Exit;
          end;

          FGenerator.RemoveAll;
          FGenerator.AddVertex(FStart.X, FStart.Y, CAggPathCmdMoveTo);
          FMarkers.AddVertex(FStart.X, FStart.Y, CAggPathCmdMoveTo);

          repeat
            Cmd := FSource.Vertex(X, Y);

            if IsVertex(Cmd) then
            begin
              FLastCmd := Cmd;

              if IsMoveTo(Cmd) then
              begin
                FStart.X := X^;
                FStart.Y := Y^;

                Break;
              end;

              FGenerator.AddVertex(X^, Y^, Cmd);
              FMarkers.AddVertex(X^, Y^, CAggPathCmdLineTo);
            end
            else
            begin
              if IsStop(Cmd) then
              begin
                FLastCmd := CAggPathCmdStop;

                Break;
              end;

              if IsEndPoly(Cmd) then
              begin
                FGenerator.AddVertex(X^, Y^, Cmd);

                Break;
              end;
            end;

          until False;

          FGenerator.Rewind(0);

          FStatus := siGenerate;

          goto _gen;
        end;

      siGenerate:
        begin
        _gen:
          Cmd := FGenerator.Vertex(X, Y);

          if IsStop(Cmd) then
          begin
            FStatus := siAccumulate;

            goto _end;
          end;

          Done := True;
        end;
    end;

  _end:
  end;

  Result := Cmd;
end;

end.
