unit AggConvDash;

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
  AggVertexSource,
  AggConvAdaptorVcgen,
  AggVcgenDash;

type
  TAggConvDash = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenDash;

    function GetDashStart: Double;
    function GetShorten: Double;
    procedure SetDashStart(Value: Double);
    procedure SetShorten(Value: Double);
  public
    constructor Create(VertexSourtce: TAggCustomVertexSource);
    destructor Destroy; override;

    procedure RemoveAllDashes;
    procedure AddDash(DashLength, GapLength: Double);

    property DashStart: Double read GetDashStart write SetDashStart;
    property Shorten: Double read GetShorten write SetShorten;
  end;

implementation


{ TAggConvDash }

constructor TAggConvDash.Create(VertexSourtce: TAggCustomVertexSource);
begin
  FGenerator := TAggVcgenDash.Create;

  inherited Create(VertexSourtce, FGenerator);
end;

destructor TAggConvDash.Destroy;
begin
  FGenerator.Free;

  inherited;
end;

procedure TAggConvDash.RemoveAllDashes;
begin
  TAggVcgenDash(Generator).RemoveAllDashes;
end;

procedure TAggConvDash.AddDash(DashLength, GapLength: Double);
begin
  TAggVcgenDash(Generator).AddDash(DashLength, GapLength);
end;

procedure TAggConvDash.SetDashStart(Value: Double);
begin
  TAggVcgenDash(Generator).DashStart := Value;
end;

procedure TAggConvDash.SetShorten(Value: Double);
begin
  TAggVcgenDash(Generator).Shorten := Value;
end;

function TAggConvDash.GetShorten: Double;
begin
  Result := TAggVcgenDash(Generator).Shorten;
end;

function TAggConvDash.GetDashStart: Double;
begin
  Result := TAggVcgenDash(Generator).DashStart;
end;

end.
