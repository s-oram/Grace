unit AggConvMarker;

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
  Math,
  AggBasics,
  AggTransAffine,
  AggVertexSource;

type
  TAggInternalStatus = (siInitial, siMarkers, siPolygon, siStop);

  TAggConvMarker = class(TAggVertexSource)
  private
    FMarkerLocator, FMarkerShapes: TAggVertexSource;

    FTransform, FMatrix: TAggTransAffine;

    FStatus: TAggInternalStatus;
    FMarker, FNumMarkers: Cardinal;
  public
    constructor Create(MarkerLocator, MarkerShapes: TAggVertexSource);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Transform: TAggTransAffine read FTransform;
  end;

implementation


{ TAggConvMarker }

constructor TAggConvMarker.Create(MarkerLocator, MarkerShapes: TAggVertexSource);
begin
  FTransform := TAggTransAffine.Create;
  FMatrix := TAggTransAffine.Create;

  FMarkerLocator := MarkerLocator;
  FMarkerShapes := MarkerShapes;

  FStatus := siInitial;
  FMarker := 0;

  FNumMarkers := 1;
end;

destructor TAggConvMarker.Destroy;
begin
  FTransform.Free;
  FMatrix.Free;

  inherited;
end;

procedure TAggConvMarker.Rewind(PathID: Cardinal);
begin
  FStatus := siInitial;
  FMarker := 0;

  FNumMarkers := 1;
end;

function TAggConvMarker.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
  X1, Y1, X2, Y2: Double;

label
  _markers, _polygon, _stop;

begin
  Cmd := CAggPathCmdMoveTo;

  while not IsStop(Cmd) do
    case FStatus of
      siInitial:
        begin
          if FNumMarkers = 0 then
          begin
            Cmd := CAggPathCmdStop;
            Continue;
          end;

          FMarkerLocator.Rewind(FMarker);

          Inc(FMarker);

          FNumMarkers := 0;
          FStatus := siMarkers;

          goto _markers;
        end;

      siMarkers:
      _markers:
        begin
          if IsStop(FMarkerLocator.Vertex(@X1, @Y1)) or
            IsStop(FMarkerLocator.Vertex(@X2, @Y2)) then
          begin
            FStatus := siInitial;
            Continue;
          end;

          Inc(FNumMarkers);

          FMatrix.Assign(FTransform);
          FMatrix.Rotate(ArcTan2(Y2 - Y1, X2 - X1));
          FMatrix.Translate(X1, Y1);

          FMarkerShapes.Rewind(FMarker - 1);

          FStatus := siPolygon;

          goto _polygon;
        end;

      siPolygon:
      _polygon:
        begin
          Cmd := FMarkerShapes.Vertex(X, Y);

          if IsStop(Cmd) then
          begin
            Cmd := CAggPathCmdMoveTo;
            FStatus := siMarkers;
            Continue;
          end;

          FMatrix.Transform(FMatrix, X, Y);

          Break;
        end;

      siStop:
        begin
          Cmd := CAggPathCmdStop;
          Continue;
        end;
    end;

  Result := Cmd;
end;

end.
