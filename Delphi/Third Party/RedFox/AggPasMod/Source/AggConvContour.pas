unit AggConvContour;

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
  AggVcgenContour,
  AggConvAdaptorVcgen,
  AggVertexSource;

type
  TAggConvContour = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenContour;

    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetInnerJoin(Value: TAggInnerJoin);

    procedure SetWidth(Value: Double);

    procedure SetMiterLimit(Value: Double);
    procedure SetMiterLimitTheta(Value: Double);
    procedure SetInnerMiterLimit(Value: Double);
    procedure SetApproximationScale(Value: Double);

    procedure SetAutoDetectOrientation(Value: Boolean);

    function GetLineJoin: TAggLineJoin;
    function GetInnerJoin: TAggInnerJoin;
    function GetWidth: Double;
    function GetMiterLimit: Double;
    function GetInnerMiterLimit: Double;
    function GetApproximationScale: Double;
    function GetAutoDetectOrientation: Boolean;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property LineJoin: TAggLineJoin read GetLineJoin write SetLineJoin;
    property InnerJoin: TAggInnerJoin read GetInnerJoin write SetInnerJoin;

    property Width: Double read GetWidth write SetWidth;
    property MiterLimit: Double read GetMiterLimit write SetMiterLimit;
    property InnerMiterLimit: Double read GetInnerMiterLimit write SetInnerMiterLimit;
    property ApproximationScale: Double read GetApproximationScale write SetApproximationScale;
    property AutoDetectOrientation: Boolean read GetAutoDetectOrientation write SetAutoDetectOrientation;
  end;

implementation


{ TAggConvContour }

constructor TAggConvContour.Create;
begin
  FGenerator := TAggVcgenContour.Create;

  inherited Create(VertexSource, FGenerator);
end;

destructor TAggConvContour.Destroy;
begin
  FGenerator.Free;

  inherited;
end;

procedure TAggConvContour.SetLineJoin;
begin
  FGenerator.LineJoin := Value;
end;

procedure TAggConvContour.SetInnerJoin;
begin
  FGenerator.InnerJoin := Value;
end;

procedure TAggConvContour.SetWidth;
begin
  FGenerator.Width := Value;
end;

procedure TAggConvContour.SetMiterLimit;
begin
  FGenerator.MiterLimit := Value;
end;

procedure TAggConvContour.SetMiterLimitTheta;
begin
  FGenerator.SetMiterLimitTheta(Value);
end;

procedure TAggConvContour.SetInnerMiterLimit;
begin
  FGenerator.InnerMiterLimit := Value;
end;

procedure TAggConvContour.SetApproximationScale;
begin
  FGenerator.ApproximationScale := Value;
end;

procedure TAggConvContour.SetAutoDetectOrientation;
begin
  FGenerator.AutoDetectOrientation := Value;
end;

function TAggConvContour.GetLineJoin: TAggLineJoin;
begin
  Result := FGenerator.LineJoin;
end;

function TAggConvContour.GetInnerJoin: TAggInnerJoin;
begin
  Result := FGenerator.InnerJoin;
end;

function TAggConvContour.GetWidth;
begin
  Result := FGenerator.Width;
end;

function TAggConvContour.GetMiterLimit;
begin
  Result := FGenerator.MiterLimit;
end;

function TAggConvContour.GetInnerMiterLimit;
begin
  Result := FGenerator.InnerMiterLimit;
end;

function TAggConvContour.GetApproximationScale;
begin
  Result := FGenerator.ApproximationScale;
end;

function TAggConvContour.GetAutoDetectOrientation;
begin
  Result := FGenerator.AutoDetectOrientation;
end;

end.
