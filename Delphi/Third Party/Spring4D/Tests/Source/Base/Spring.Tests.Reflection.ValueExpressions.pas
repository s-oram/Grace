{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (C) 2009-2010 DevJet                                  }
{                                                                           }
{           http://www.DevJet.net                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Tests.Reflection.ValueExpressions;

interface

uses
  TestFramework,
  TestExtensions;

type
  TPoint = record
    X, Y: Integer;
  end;
  TPoints = array[0..9] of TPoint;

  TTestValueExpression = class(TTestCase)
  strict private
    type
      IInnerObject = interface
        ['{A045E5C6-3499-43EA-8F83-681C61BE0874}']
        procedure SetName(const value: string);
        function GetName: string;
        function GetPoint: TPoint;
        property Name: string read GetName write SetName;
        property Point: TPoint read GetPoint;
      end;

      TInnerObject = class(TInterfacedObject, IInnerObject)
      private
        fName: string;
        fPoint: TPoint;
        procedure SetName(const value: string);
        function GetName: string;
        function GetPoint: TPoint;
      public
        constructor Create;
        property Name: string read GetName write SetName;
        property Point: TPoint read GetPoint;
      end;

      TOuterObject = class
      private
        fName: string;
        fNumber: Integer;
        fInner: IInnerObject;
        fCoords: TPoints;
        procedure SetName(const value: string);
        function GetName: string;
        procedure SetNumber(const value: Integer);
        function GetNumber: Integer;
        function GetCoords: TPoints;
        function GetInner: IInnerObject;
      public
        constructor Create;
        destructor Destroy; override;
        property Name: string read GetName write SetName;
        property Number: Integer read GetNumber write SetNumber;
        property Coords: TPoints read GetCoords;
        property Inner: IInnerObject read GetInner;
      end;

    // Ignored Tests
//    procedure TestSetPropertyRecordType;
//    procedure TestSetPropertyArray;
  published
    procedure TestGetPropertyRecordType;
    procedure TestGetPropertyNativeType;
    procedure TestSetPropertyNativeType;
    procedure TestGetPropertyDrillDownNativeType;
    procedure TestSetPropertyDrillDownNativeType;
    procedure TestGetPropertyArray;
    procedure TestGetPropertyObject;
    procedure TestGetFieldNativeType;
    procedure TestSetFieldNativeType;
    procedure TestGetFieldDrillDownNativeType;
    procedure TestSetFieldDrillDownNativeType;
    procedure TestGetFieldArray;
    procedure TestGetFieldObject;
  end;

implementation

uses
  Rtti,
  Spring.Reflection.ValueExpression;


{$REGION 'TTestValueExpression.TOuterObject'}

constructor TTestValueExpression.TOuterObject.Create;
var
  i: Integer;
begin
  fInner := TInnerObject.Create;
  fName := 'Outer Object';
  fNumber := 15;
  for i := Low(fCoords) to High(fCoords) do
  begin
    fCoords[i].X := i * i;
    fCoords[i].Y := i + i;
  end;
end;

destructor TTestValueExpression.TOuterObject.Destroy;
begin
  //fInner.Free;
end;

function TTestValueExpression.TOuterObject.GetCoords: TPoints;
begin
  Result := fCoords;
end;

function TTestValueExpression.TOuterObject.GetInner: IInnerObject;
begin
  Result := fInner;
end;

function TTestValueExpression.TOuterObject.GetName: string;
begin
  Result := fName;
end;

function TTestValueExpression.TOuterObject.GetNumber: Integer;
begin
  Result := fNumber;
end;

procedure TTestValueExpression.TOuterObject.SetName(const value: string);
begin
  if value <> fName then
    fName := value;
end;

procedure TTestValueExpression.TOuterObject.SetNumber(const value: Integer);
begin
  if value <> fNumber then
    fNumber := value;
end;

{$ENDREGION}


{$REGION 'TTestValueExpression.TInnerObject'}

constructor TTestValueExpression.TInnerObject.Create;
begin
  fName := 'Inner Object';
  fPoint.X := 15;
  fPoint.Y := 20;
end;

function TTestValueExpression.TInnerObject.GetName: string;
begin
  Result := fName;
end;

function TTestValueExpression.TInnerObject.GetPoint: TPoint;
begin
  Result := fPoint;
end;

procedure TTestValueExpression.TInnerObject.SetName(const value: string);
begin
  if value <> fName then
    fName := value;
end;

{$ENDREGION}


{$REGION 'TTestValueExpression'}

procedure TTestValueExpression.TestGetFieldArray;
var
  obj: TOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckTrue(expression.Follow('fCoords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
  obj.Free;
end;

procedure TTestValueExpression.TestGetFieldNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckEquals(expression.Follow('').Value.ToString, expression.Value.ToString);
  CheckEquals('Outer Object', expression.Follow('fName').Value.ToString);
  CheckEquals(15, expression.Follow('fNumber').Value.AsInteger);
  obj.Free;
end;

procedure TTestValueExpression.TestSetFieldNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckEquals('Outer Object', expression.Follow('fName').Value.ToString);
  expression.Follow('fName').SetValue('Test Outer Object');
  CheckEquals('Test Outer Object', expression.Follow('fName').Value.ToString);
  CheckEquals(15, expression.Follow('fNumber').Value.AsInteger);
  expression.Follow('fNumber').SetValue(18);
  CheckEquals(18, expression.Follow('fNumber').Value.AsInteger);
  obj.Free;
end;

procedure TTestValueExpression.TestGetFieldDrillDownNativeType;
var
  obj: TOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.Create(obj, '');
  expression := root.Follow('fInner.fName');
  CheckEquals('Inner Object', expression.Value.AsString);
  obj.Free;
end;

procedure TTestValueExpression.TestSetFieldDrillDownNativeType;
var
  obj: TOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.Create(obj, '');
  expression := root.Follow('fInner.fName');
  CheckEquals('Inner Object', expression.Value.AsString);
  expression.SetValue('Test Inner Object');
  CheckEquals('Test Inner Object', expression.Value.AsString);
  obj.Free;
end;

procedure TTestValueExpression.TestGetFieldObject;
var
  outerObj: TOuterObject;
  obj: IInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.Create(outerObj, '');
  expression := root.Follow('fInner');
  CheckTrue(expression.Value.TryAsType<IInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
  outerObj.Free;
end;

procedure TTestValueExpression.TestGetPropertyNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckEquals('Outer Object', expression.Follow('Name').Value.ToString);
  CheckEquals(15, expression.Follow('Number').Value.AsInteger);
  obj.Free;
end;

procedure TTestValueExpression.TestGetPropertyDrillDownNativeType;
var
  obj: TOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.Create(obj, '');
  expression := root.Follow('Inner.Name');
  CheckEquals('Inner Object', expression.Value.AsString);
  obj.Free;
end;

procedure TTestValueExpression.TestSetPropertyDrillDownNativeType;
var
  obj: TOuterObject;
  root, expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  root := TValueExpression.Create(obj, '');
  expression := root.Follow('Inner.Name');
  CheckEquals('Inner Object', expression.Value.AsString);
  expression.SetValue('Test Inner Object');
  CheckEquals('Test Inner Object', expression.Value.AsString);
  obj.Free;
end;

procedure TTestValueExpression.TestGetPropertyArray;
var
  obj: TOuterObject;
  coord: TPoint;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckTrue(expression.Follow('Coords[2]').Value.TryAsType<TPoint>(coord));
  CheckEquals(4, coord.X);
  obj.Free;
end;

procedure TTestValueExpression.TestGetPropertyObject;
var
  outerObj: TOuterObject;
  obj: IInnerObject;
  root, expression: IValueExpression;
begin
  outerObj := TOuterObject.Create;
  root := TValueExpression.Create(outerObj, '');
  expression := root.Follow('Inner');
  CheckTrue(expression.Value.TryAsType<IInnerObject>(obj));
  CheckEquals('Inner Object', obj.Name);
  outerObj.Free;
end;

procedure TTestValueExpression.TestGetPropertyRecordType;
var
  obj: TInnerObject;
  expression: IValueExpression;
begin
  obj := TInnerObject.Create;
  expression := TValueExpression.Create(obj, '');
  CheckEquals(15, expression.Follow('Point.X').Value.AsInteger);
  obj.Free;
end;

(*
procedure TTestValueExpression.TestSetPropertyRecordType;
var
  obj: IInnerObject;
  expression: IValueExpression;
begin
  obj := TInnerObject.Create;
  expression := TValueExpression.Create(TValue.From<IInnerObject>(obj));
  CheckEquals(15, expression.Follow('.Point.X').Value.AsInteger);
  expression.Follow('.Point.X').SetValue(22);
  CheckEquals(22, expression.Follow('.Point.X').Value.AsInteger);
end;

procedure TTestValueExpression.TestSetPropertyArray;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(TValue.From<TOuterObject>(obj));
  CheckEquals(4, expression.Follow('.Coords[2].X').Value.AsInteger);
  expression.Follow('.Coords[2].X').SetValue(8);
  try
    CheckEquals(8, expression.Follow('.Coords[2].X').Value.AsInteger);
  finally
    obj.Free;
  end;
end;
//*)

procedure TTestValueExpression.TestSetPropertyNativeType;
var
  obj: TOuterObject;
  expression: IValueExpression;
begin
  obj := TOuterObject.Create;
  expression := TValueExpression.Create(obj, '');

  CheckEquals('Outer Object', expression.Follow('Name').Value.ToString);

  expression.Follow('Name').SetValue('Set Native Type Test');
  CheckEquals('Set Native Type Test', expression.Follow('Name').Value.ToString);

  CheckEquals(15, expression.Follow('Number').Value.AsInteger);

  expression.Follow('Number').SetValue(18);
  CheckEquals(18, expression.Follow('Number').Value.AsInteger);
  obj.Free;
end;

{$ENDREGION}


end.
