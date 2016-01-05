unit AggDesignTimeMisc;

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
  {$IFDEF FPC} LCLIntf, LazIDEIntf, PropEdits,{$ELSE}
  DesignIntf, DesignEditors,{$ENDIF}
  Classes, TypInfo;

type
  TAggCustomClassProperty = class(TClassProperty)
  private
    function HasSubProperties: Boolean;
  protected
    class function GetClassList: TClassList; virtual;
    procedure SetClassName(const CustomClass: string); virtual; {$IFNDEF BCB} abstract; {$ENDIF}
    function GetObject: TObject; virtual; {$IFNDEF BCB} abstract; {$ENDIF}
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;
  end;

implementation

{ TAggCustomClassProperty }

function TAggCustomClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paReadOnly] +
    [paValueList, paRevertable, paVolatileSubProperties];
  if not HasSubProperties then Exclude(Result, paSubProperties);
end;

class function TAggCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

function TAggCustomClassProperty.GetValue: string;
begin
  if PropCount > 0 then
    Result := GetObject.ClassName
  else
    Result := '';
end;

procedure TAggCustomClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) then
    for I := 0 to L.Count - 1 do
      Proc(L.Items[I].ClassName);
end;

function TAggCustomClassProperty.HasSubProperties: Boolean;
begin
  if PropCount > 0 then
    Result := GetTypeData(GetObject.ClassInfo)^.PropCount > 0
  else
    Result := False;
end;

procedure TAggCustomClassProperty.SetValue(const Value: string);
var
  L: TClassList;
begin
  L := GetClassList;
  if Assigned(L) and Assigned(L.Find(Value)) then
    SetClassName(Value)
  else SetStrValue('');
  Modified;
end;

{$IFDEF BCB}
class function TAggCustomClassProperty.GetClassList: TClassList;
begin
  Result := nil;
end;

procedure TAggCustomClassProperty.SetClassName(const CustomClass: string);
begin
end;

function TAggCustomClassProperty.GetObject: TObject;
begin
  Result := nil;
end;
{$ENDIF}

end.
