unit AggSvgPathRenderer;

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

{$I ../AggCompiler.inc}

uses
  SysUtils,
  AggBasics,
  AggPathStorage,
  AggConvTransform,
  AggConvStroke,
  AggConvContour,
  AggConvCurve,
  AggColor,
  AggArray,
  AggBoundingRect,
  AggRasterizerScanLine,
  AggRasterizerScanLineAA,
  AggVertexSource,
  AggSvgPathTokenizer,
  AggSvgException,
  AggTransAffine,
  AggMathStroke,
  AggScanLine,
  AggRendererScanLine,
  AggRenderScanLines;

{$IFDEF FPC}
  {$DEFINE FPC_RECORD_CONSTRUCTOR}
{$ENDIF}

type
  TAggConvCount = class(TAggVertexSource)
  private
    FSource: TAggVertexSource;
    FCount: Cardinal;
    procedure SetCount(N: Cardinal);
  public
    constructor Create(Vs: TAggVertexSource);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Count: Cardinal read FCount write SetCount;
  end;

  // Basic path attributes
  PPathAttributesRecord = ^TPathAttributesRecord;
  TPathAttributesRecord = {$IFDEF FPC_RECORD_CONSTRUCTOR} record {$ELSE} object {$ENDIF}
  private
    Index: Cardinal;

    FFillColor, FStrokeColor: TAggColor;
    FFillFlag, FStrokeFlag, FEvenOddFlag: Boolean;

    FLineJoin: TAggLineJoin;
    FLineCap: TAggLineCap;
    FMiterLimit: Double;
    FStrokeWidth: Double;

    FTransform: TAggTransAffine;
  public
    // Copy constructor
    constructor Create(Attr: PPathAttributesRecord); overload;

    // Copy constructor with new index value
    constructor Create(Attr: PPathAttributesRecord; Idx: Cardinal); overload;

    procedure Assign(Attr: PPathAttributesRecord); overload;
    procedure Assign(Attr: PPathAttributesRecord; Idx: Cardinal); overload;

    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property StrokeWidth: Double read FStrokeWidth write FStrokeWidth;

    property LineCap: TAggLineCap read FLineCap write FLineCap;
    property LineJoin: TAggLineJoin read FLineJoin write FLineJoin;

    property FillFlag: Boolean read FFillFlag write FFillFlag;
    property StrokeFlag: Boolean read FStrokeFlag write FStrokeFlag;
    property EvenOddFlag: Boolean read FEvenOddFlag write FEvenOddFlag;
  end;

  // Path container and Renderer.
  TPathRenderer = class(TCardinalList)
  private
    FStorage: TAggPathStorage;
    FAttrStorage, FAttrStack: TAggPodDeque;
    FTransform: TAggTransAffine;

    FCurved: TAggConvCurve;
    FCurvedCount: TAggConvCount;

    FCurvedStroked: TAggConvStroke;
    FCurvedStrokedTrans: TAggConvTransform;

    FCurvedTrans: TAggConvTransform;
    FCurvedTransContour: TAggConvContour;

    // Private
    function GetCurrentAttributes: PPathAttributesRecord;
    function GetTransform: TAggTransAffine;
    function GetVertexCount: Cardinal;

    procedure SetFillOpacity(Value: Double);
    procedure SetStrokeOpacity(Value: Double);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetMiterLimit(Value: Double);
    procedure SetEvenOdd(Value: Boolean);
    procedure SetStrokeWidth(Value: Double);
  protected
    function GetItem(Index: Cardinal): Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAll;

    // Use these functions as follows:
    // BeginPath when the XML tag <path> comes ("start_element" handler)
    // ParsePath on "d=" tag attribute
    // EndPath when parsing of the entire tag is done.
    procedure BeginPath;
    procedure ParsePath(Tok: TPathTokenizer);
    procedure EndPath;

    // The following functions are essentially a "reflection" of
    // the respective SVG path commands.
    procedure MoveTo(X, Y: Double; Rel: Boolean = False); // M, m
    procedure LineTo(X, Y: Double; Rel: Boolean = False); // L, l
    procedure HorizontalLineTo(X: Double; Rel: Boolean = False); // H, h
    procedure VerticalLineTo(Y: Double; Rel: Boolean = False); // V, v

    procedure Curve3(X1, Y1, X, Y: Double; Rel: Boolean = False); overload;
    // Q, q
    procedure Curve3(X, Y: Double; Rel: Boolean = False); overload; // T, t
    procedure Curve4(X1, Y1, X2, Y2, X, Y: Double; Rel: Boolean = False);
      overload; // C, c
    procedure Curve4(X2, Y2, X, Y: Double; Rel: Boolean = False); overload;
    // S, s

    procedure CloseSubpath; // Z, z

    procedure AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0;
      SolidPath: Boolean = True);

    // Call these functions on <g> tag (start_element, end_element respectively)
    procedure PushAttribute;
    procedure PopAttribute;

    // Attribute setting functions
    procedure SetFillColor(Value: PAggColor);
    procedure SetStrokeColor(Value: PAggColor);

    procedure FillNone;
    procedure StrokeNone;

    // Make all polygons CCW-oriented
    procedure ArrangeOrientations;

    // Expand all polygons
    procedure Expand(Value: Double);

    procedure BoundingRect(X1, Y1, X2, Y2: PDouble);

    // Rendering. One can specify two additional parameters:
    // TAggTransAffine and opacity. They can be used to transform the whole
    // image and/or to make it translucent.
    procedure Render(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
      Ren: TAggCustomRendererScanLineSolid; Mtx: TAggTransAffine;
      const Cb: TRectInteger; Opacity: Double = 1.0);

    property Transform: TAggTransAffine read GetTransform;
    property VertexCount: Cardinal read GetVertexCount;

    property FillOpacity: Double write SetFillOpacity;
    property StrokeOpacity: Double write SetFillOpacity;
    property LineJoin: TAggLineJoin write SetLineJoin;
    property LineCap: TAggLineCap write SetLineCap;
    property MiterLimit: Double write SetMiterLimit;
    property EvenOdd: Boolean write SetEvenOdd;
    property StrokeWidth: Double write SetStrokeWidth;
  end;

implementation

resourcestring
  RCStrParsePathNotImplemented = 'ParsePath: Command A: NOT IMPLEMENTED YET';
  RCStrParsePathInvalidCommand = 'ParsePath: Invalid Command %c';
  RCStrEndPathNotStarted = 'EndPath: The path was not begun';
  RCStrPopAttributeEmptyStack = 'PopAttribute: Attribute stack is empty';
  RCStrGetCurrentAttributesEmpty = 'GetCurrentAttributes: Attribute stack is empty';

{ TAggConvCount }

constructor TAggConvCount.Create;
begin
  FSource := Vs;
  FCount := 0;
end;

procedure TAggConvCount.SetCount;
begin
  FCount := N;
end;

procedure TAggConvCount.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);
end;

function TAggConvCount.Vertex(X, Y: PDouble): Cardinal;
begin
  Inc(FCount);

  Result := FSource.Vertex(X, Y);
end;


{ TPathAttributesRecord }

constructor TPathAttributesRecord.Create(Attr: PPathAttributesRecord);
begin
  FTransform := TAggTransAffine.Create;
  Assign(Attr);
end;

constructor TPathAttributesRecord.Create(Attr: PPathAttributesRecord; Idx: Cardinal);
begin
  FTransform := TAggTransAffine.Create;
  Assign(Attr, Idx);
end;

procedure TPathAttributesRecord.Assign(Attr: PPathAttributesRecord);
begin
  Index := Attr^.Index;

  FFillColor := Attr^.FFillColor;
  FStrokeColor := Attr^.FStrokeColor;

  FillFlag := Attr^.FillFlag;
  FStrokeFlag := Attr^.StrokeFlag;
  FEvenOddFlag := Attr^.EvenOddFlag;
  FLineJoin := Attr^.FLineJoin;
  FLineCap := Attr^.FLineCap;
  FMiterLimit := Attr^.FMiterLimit;
  FStrokeWidth := Attr^.FStrokeWidth;
  FTransform.AssignAll(Attr^.FTransform);
end;

procedure TPathAttributesRecord.Assign(Attr: PPathAttributesRecord; Idx: Cardinal);
begin
  Index := Idx;

  FFillColor := Attr^.FFillColor;
  FStrokeColor := Attr^.FStrokeColor;

  FillFlag := Attr^.FillFlag;
  FStrokeFlag := Attr^.StrokeFlag;
  FEvenOddFlag := Attr^.EvenOddFlag;
  FLineJoin := Attr^.FLineJoin;
  FLineCap := Attr^.FLineCap;
  FMiterLimit := Attr^.FMiterLimit;
  FStrokeWidth := Attr^.FStrokeWidth;
  FTransform.AssignAll(Attr^.FTransform);
end;


{ TPathRenderer }

constructor TPathRenderer.Create;
begin
  FStorage := TAggPathStorage.Create;
  FAttrStorage := TAggPodDeque.Create(SizeOf(TPathAttributesRecord));
  FAttrStack := TAggPodDeque.Create(SizeOf(TPathAttributesRecord));
  FTransform := TAggTransAffine.Create;

  FCurved := TAggConvCurve.Create(FStorage);
  FCurvedCount := TAggConvCount.Create(FCurved);

  FCurvedStroked := TAggConvStroke.Create(FCurvedCount);
  FCurvedStrokedTrans := TAggConvTransform.Create(FCurvedStroked, FTransform);

  FCurvedTrans := TAggConvTransform.Create(FCurvedCount, FTransform);
  FCurvedTransContour := TAggConvContour.Create(FCurvedTrans);
end;

destructor TPathRenderer.Destroy;
var
  Index: Integer;
  PathAttributesRecordPtr: PPathAttributesRecord;
begin
  FCurved.Free;
  FCurvedCount.Free;
  FCurvedStroked.Free;
  FCurvedStrokedTrans.Free;
  FCurvedTrans.Free;
  FCurvedTransContour.Free;

  if FAttrStorage.Size > 0 then
    for Index := 0 to FAttrStorage.Size - 1 do
      if Assigned(FAttrStorage.ItemPointer[Index]) then
      begin
        PathAttributesRecordPtr := PPathAttributesRecord(FAttrStorage.ItemPointer[Index]);
        if Assigned(PathAttributesRecordPtr^.FTransform) then
          PathAttributesRecordPtr^.FTransform.Free;
      end;

  FAttrStack.Free;
  FAttrStorage.Free;
  FStorage.Free;
  FTransform.Free;

  inherited;
end;

procedure TPathRenderer.RemoveAll;
begin
  FStorage.RemoveAll;
  FAttrStorage.RemoveAll;
  FAttrStack.RemoveAll;
  FTransform.Reset;
end;

procedure TPathRenderer.BeginPath;
var
  Idx : Cardinal;
  Attr: TPathAttributesRecord;
begin
  PushAttribute;

  Idx := FStorage.StartNewPath;

  // create attribute and add to storage list
  Attr.Create(GetCurrentAttributes, Idx);
  FAttrStorage.Add(@Attr);
end;

procedure TPathRenderer.ParsePath(Tok: TPathTokenizer);
var
  Arg: array [0..9] of Double;
  I  : Cardinal;
  Cmd: AnsiChar;
  Buf: array [0..99] of AnsiChar;
begin
  while Tok.Next do
  begin
    Cmd := Tok.LastCommand;

    case Cmd of
      'M', 'm':
        begin
          Arg[0] := Tok.LastNumber;
          Arg[1] := Tok.Next(Cmd);

          MoveTo(Arg[0], Arg[1], Cmd = 'm');
        end;

      'L', 'l':
        begin
          Arg[0] := Tok.LastNumber;
          Arg[1] := Tok.Next(Cmd);

          LineTo(Arg[0], Arg[1], Cmd = 'l');
        end;

      'V', 'v':
        VerticalLineTo(Tok.LastNumber, Cmd = 'v');

      'H', 'h':
        HorizontalLineTo(Tok.LastNumber, Cmd = 'h');

      'Q', 'q':
        begin
          Arg[0] := Tok.LastNumber;

          for I := 1 to 3 do
            Arg[I] := Tok.Next(Cmd);

          Curve3(Arg[0], Arg[1], Arg[2], Arg[3], Cmd = 'q');
        end;

      'T', 't':
        begin
          Arg[0] := Tok.LastNumber;
          Arg[1] := Tok.Next(Cmd);

          Curve3(Arg[0], Arg[1], Cmd = 't');
        end;

      'C', 'c':
        begin
          Arg[0] := Tok.LastNumber;

          for I := 1 to 5 do
            Arg[I] := Tok.Next(Cmd);

          Curve4(Arg[0], Arg[1], Arg[2], Arg[3], Arg[4], Arg[5], Cmd = 'c');
        end;

      'S', 's':
        begin
          Arg[0] := Tok.LastNumber;

          for I := 1 to 3 do
            Arg[I] := Tok.Next(Cmd);

          Curve4(Arg[0], Arg[1], Arg[2], Arg[3], Cmd = 's');
        end;

      'A', 'a':
        raise TSvgException.Create(PAnsiChar(RCStrParsePathNotImplemented));

      'Z', 'z':
        CloseSubpath;

    else
      raise TSvgException.Create(Format(RCStrParsePathInvalidCommand, [Cmd]));
    end;
  end;
end;

procedure TPathRenderer.EndPath;
var
  Idx : Cardinal;
  Attr: PPathAttributesRecord;
begin
  if FAttrStorage.Size = 0 then
    raise TSvgException.Create(PAnsiChar(RCStrEndPathNotStarted));

  Attr := PPathAttributesRecord(FAttrStorage[FAttrStorage.Size - 1]);
  Idx := Attr.Index;
  Attr.Assign(GetCurrentAttributes);
  Attr.Index := Idx;

  PopAttribute;
end;

procedure TPathRenderer.MoveTo(X, Y: Double; Rel: Boolean = False);
begin
  if Rel then
    FStorage.RelativeToAbsolute(@X, @Y);

  FStorage.MoveTo(X, Y);
end;

procedure TPathRenderer.LineTo(X, Y: Double; Rel: Boolean = False);
begin
  if Rel then
    FStorage.RelativeToAbsolute(@X, @Y);

  FStorage.LineTo(X, Y);
end;

procedure TPathRenderer.HorizontalLineTo(X: Double; Rel: Boolean = False);
var
  X2, Y2: Double;
begin
  X2 := 0.0;
  Y2 := 0.0;

  if FStorage.TotalVertices <> 0 then
  begin
    FStorage.SetVertex(FStorage.TotalVertices - 1, @X2, @Y2);

    if Rel then
      X := X + X2;

    FStorage.LineTo(X, Y2);
  end;
end;

procedure TPathRenderer.VerticalLineTo(Y: Double; Rel: Boolean = False);
var
  X2, Y2: Double;
begin
  X2 := 0.0;
  Y2 := 0.0;

  if FStorage.TotalVertices <> 0 then
  begin
    FStorage.SetVertex(FStorage.TotalVertices - 1, @X2, @Y2);

    if Rel then
      Y := Y + Y2;

    FStorage.LineTo(X2, Y);
  end;
end;

procedure TPathRenderer.Curve3(X1, Y1, X, Y: Double; Rel: Boolean = False);
begin
  if Rel then
  begin
    FStorage.RelativeToAbsolute(@X1, @Y1);
    FStorage.RelativeToAbsolute(@X, @Y);
  end;

  FStorage.Curve3(X1, Y1, X, Y);
end;

procedure TPathRenderer.Curve3(X, Y: Double; Rel: Boolean = False);
begin
  if Rel then
    FStorage.Curve3Relative(X, Y)
  else
    FStorage.Curve3(X, Y);
end;

procedure TPathRenderer.Curve4(X1, Y1, X2, Y2, X, Y: Double;
  Rel: Boolean = False);
begin
  if Rel then
  begin
    FStorage.RelativeToAbsolute(@X1, @Y1);
    FStorage.RelativeToAbsolute(@X2, @Y2);
    FStorage.RelativeToAbsolute(@X, @Y);
  end;

  FStorage.Curve4(X1, Y1, X2, Y2, X, Y);
end;

procedure TPathRenderer.Curve4(X2, Y2, X, Y: Double; Rel: Boolean = False);
begin
  if Rel then
    FStorage.Curve4Relative(X2, Y2, X, Y)
  else
    FStorage.Curve4(X2, Y2, X, Y);
end;

procedure TPathRenderer.CloseSubpath;
begin
  FStorage.EndPoly(CAggPathFlagsClose);
end;

procedure TPathRenderer.AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0;
  SolidPath: Boolean = True);
begin
  FStorage.AddPath(Vs, PathID, SolidPath);
end;

function TPathRenderer.GetVertexCount: Cardinal;
begin
  Result := FCurvedCount.Count;
end;

procedure TPathRenderer.PushAttribute;
var
  Attr: TPathAttributesRecord;
begin
  if FAttrStack.Size <> 0 then
    FAttrStack.Add(FAttrStack[FAttrStack.Size - 1])
  else
  begin
    with Attr do
    begin
      Index := 0;

      FFillColor.Black;
      FStrokeColor.Black;

      FillFlag := True;
      StrokeFlag := False;
      EvenOddFlag := False;
      LineJoin := ljMiter;
      LineCap := lcButt;
      MiterLimit := 4.0;
      StrokeWidth := 1.0;

      FTransform := TAggTransAffine.Create;
    end;

    FAttrStack.Add(@Attr);
  end;
end;

procedure TPathRenderer.PopAttribute;
begin
  if FAttrStack.Size = 0 then
    raise TSvgException.Create(PAnsiChar(RCStrPopAttributeEmptyStack));

  if FAttrStack.Size = 1 then
    PPathAttributesRecord(FAttrStack.Last).FTransform.Free;

  FAttrStack.RemoveLast;
end;

procedure TPathRenderer.SetFillColor(Value: PAggColor);
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.FFillColor := Value^;
  Attr.FillFlag := True;
end;

procedure TPathRenderer.SetStrokeColor(Value: PAggColor);
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.FStrokeColor := Value^;
  Attr.StrokeFlag := True;
end;

procedure TPathRenderer.SetEvenOdd(Value: Boolean);
begin
  GetCurrentAttributes.EvenOddFlag := Value;
end;

procedure TPathRenderer.SetStrokeWidth(Value: Double);
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.StrokeWidth := Value;
  Attr.StrokeFlag := True;
end;

procedure TPathRenderer.FillNone;
begin
  GetCurrentAttributes.FillFlag := False;
end;

procedure TPathRenderer.StrokeNone;
begin
  GetCurrentAttributes.StrokeFlag := False;
end;

procedure TPathRenderer.SetFillOpacity(Value: Double);
begin
  GetCurrentAttributes.FFillColor.Opacity := Value;
end;

procedure TPathRenderer.SetStrokeOpacity(Value: Double);
begin
  GetCurrentAttributes.FStrokeColor.Opacity := Value;
end;

procedure TPathRenderer.SetLineJoin(Value: TAggLineJoin);
begin
  GetCurrentAttributes.FLineJoin := Value;
end;

procedure TPathRenderer.SetLineCap(Value: TAggLineCap);
begin
  GetCurrentAttributes.FLineCap := Value;
end;

procedure TPathRenderer.SetMiterLimit(Value: Double);
begin
  GetCurrentAttributes.FMiterLimit := Value;
end;

function TPathRenderer.GetTransform: TAggTransAffine;
begin
  Result := GetCurrentAttributes.FTransform;
end;

procedure TPathRenderer.ArrangeOrientations;
begin
  FStorage.ArrangeOrientationsAllPaths(CAggPathFlagsCcw);
end;

procedure TPathRenderer.Expand(Value: Double);
begin
  FCurvedTransContour.Width := Value;
end;

function TPathRenderer.GetItem(Index: Cardinal): Cardinal;
begin
  FTransform.AssignAll(PPathAttributesRecord(FAttrStorage[Index]).FTransform);
  Result := PPathAttributesRecord(FAttrStorage[Index]).Index;
end;

procedure TPathRenderer.BoundingRect(X1, Y1, X2, Y2: PDouble);
var
  Trans: TAggConvTransform;
begin
  Trans := TAggConvTransform.Create(FStorage, FTransform);
  try
    BoundingRectInteger(Trans, Self, 0, FAttrStorage.Size, X1, Y1, X2, Y2);
  finally
    Trans.Free;
  end;
end;

procedure TPathRenderer.Render(Ras: TAggRasterizerScanLine;
  Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLineSolid;
  Mtx: TAggTransAffine; const Cb: TRectInteger; Opacity: Double = 1.0);
var
  I: Cardinal;
  Scl: Double;
  Attr: PPathAttributesRecord;
  Color: TAggColor;
begin
  Ras.SetClipBox(Cb.X1, Cb.Y1, Cb.X2, Cb.Y2);
  FCurvedCount.SetCount(0);

  I := 0;

  while I < FAttrStorage.Size do
  begin
    Attr := PPathAttributesRecord(FAttrStorage[I]);

    FTransform.AssignAll(Attr.FTransform);
    FTransform.Multiply(Mtx);

    Scl := FTransform.GetScale;

    // FCurved.ApproximationMethod := curveInc;

    FCurved.ApproximationScale := Scl;
    FCurved.AngleTolerance := 0.0;

    if Attr.FillFlag then
    begin
      Ras.Reset;

      if Attr.EvenOddFlag then
        Ras.FillingRule := frEvenOdd
      else
        Ras.FillingRule := frNonZero;

      if Abs(FCurvedTransContour.Width) < 0.0001 then
        Ras.AddPath(FCurvedTrans, Attr.Index)
      else
      begin
        FCurvedTransContour.MiterLimit := Attr.FMiterLimit;

        Ras.AddPath(FCurvedTransContour, Attr.Index);
      end;

      Color := Attr.FFillColor;

      Color.Opacity := Color.Opacity * Opacity;
      Ren.SetColor(@Color);
      RenderScanLines(Ras, Sl, Ren);
    end;

    if Attr.StrokeFlag then
    begin
      FCurvedStroked.Width := Attr.StrokeWidth;

      (*
      if Attr.LineJoin = ljMiter then
        FCurvedStroked.LineJoin := ljMiterRound
      else
      *)

      FCurvedStroked.LineJoin := Attr.FLineJoin;
      FCurvedStroked.LineCap := Attr.FLineCap;
      FCurvedStroked.MiterLimit := Attr.FMiterLimit;
      FCurvedStroked.ApproximationScale := Scl;

      // If the *visual* line width is considerable we
      // turn on processing of curve cusps.
      // ---------------------
      if Attr.StrokeWidth * Scl > 1.0 then
        FCurved.AngleTolerance := 0.2;

      Ras.Reset;
      Ras.FillingRule := frNonZero;
      Ras.AddPath(FCurvedStrokedTrans, Attr.Index);

      Color := Attr.FStrokeColor;

      Color.Opacity := Color.Opacity * Opacity;
      Ren.SetColor(@Color);
      RenderScanLines(Ras, Sl, Ren);
    end;

    Inc(I);
  end;
end;

function TPathRenderer.GetCurrentAttributes: PPathAttributesRecord;
begin
  if FAttrStack.Size = 0 then
    raise TSvgException.Create(PAnsiChar(RCStrGetCurrentAttributesEmpty));

  Result := PPathAttributesRecord(FAttrStack[FAttrStack.Size - 1]);
end;

end.
