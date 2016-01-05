unit AggDesignTimeRegistration;

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
  Classes, TypInfo,
{$IFDEF FPC}
  LCLIntf, LResources, LazIDEIntf, PropEdits, ComponentEditors
{$ELSE}
  DesignIntf
{$ENDIF};

procedure Register;

implementation

uses
  AggColor,
  Agg2D,
  AggDesignTimeColor,
  Agg2DControl,
  AggControlVCL;

{ Registration }
procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TAggColorRgba8), nil, '', TAggRgba8Property);
  RegisterPropertyEditor(TypeInfo(TAggRgba8), nil, '', TAggRgba8Property);
  RegisterPropertyEditor(TypeInfo(TAggPackedRgba8), nil, '', TAggRgba8Property);

  RegisterComponents('AggPas', [TAggLabel, TAggCheckBox, TAggRadioBox,
    TAggSlider, TAgg2DControl, TAggSVG]);
end;

end.
