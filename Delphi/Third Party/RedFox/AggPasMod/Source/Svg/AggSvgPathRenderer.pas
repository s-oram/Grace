unit AggSvgPathRenderer;

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
  TPathAttributesRecord = record
  private
    Index: Cardinal;

    FillColor, StrokeColor: TAggColor;
    FillFlag, StrokeFlag, EvenOddFlag: Boolean;

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

    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property StrokeWidth: Double read FStrokeWidth write FStrokeWidth;
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
    function GetVertexCount: Cardinal;

    // Call these functions on <g> tag (start_element, end_element respectively)
    procedure PushAttribute;
    procedure PopAttribute;

    // Attribute setting functions
    procedure SetFillColor(F: PAggColor);
    procedure SetStrokeColor(S: PAggColor);
    procedure EvenOdd(Flag: Boolean);
    procedure SetStrokeWidth(W: Double);

    procedure FillNone;
    procedure StrokeNone;

    procedure SetFillOpacity(Op: Double);
    procedure SetStrokeOpacity(Op: Double);
    procedure SetLineJoin(Value: TAggLineJoin);
    procedure SetLineCap(Value: TAggLineCap);
    procedure SetMiterLimit(Ml: Double);

    // Make all polygons CCW-oriented
    procedure ArrangeOrientations;

    // Expand all polygons
    procedure Expand(Value: Double);

    function ArrayOperator(Idx: Cardinal): Cardinal; virtual;
    procedure BoundingRect(X1, Y1, X2, Y2: PDouble);

    // Rendering. One can specify two additional parameters:
    // TAggTransAffine and opacity. They can be used to transform the whole
    // image and/or to make it translucent.
    procedure Render(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
      Ren: TAggCustomRendererScanLineSolid; Mtx: TAggTransAffine;
      const Cb: TRectInteger; Opacity: Double = 1.0);

    property Transform: TAggTransAffine read GetTransform;
  end;

implementation


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
  Index := Attr.Index;

  FillColor := Attr.FillColor;
  StrokeColor := Attr.StrokeColor;

  FillFlag := Attr.FillFlag;
  StrokeFlag := Attr.StrokeFlag;
  EvenOddFlag := Attr.EvenOddFlag;
  FLineJoin := Attr.FLineJoin;
  FLineCap := Attr.FLineCap;
  FMiterLimit := Attr.FMiterLimit;
  FStrokeWidth := Attr.FStrokeWidth;

  FTransform := TAggTransAffine.Create;
  FTransform.AssignAll(Attr.FTransform);
end;

constructor TPathAttributesRecord.Create(Attr: PPathAttributesRecord; Idx: Cardinal);
begin
  Index := Idx;

  FillColor := Attr.FillColor;
  StrokeColor := Attr.StrokeColor;

  FillFlag := Attr.FillFlag;
  StrokeFlag := Attr.StrokeFlag;
  EvenOddFlag := Attr.EvenOddFlag;
  FLineJoin := Attr.FLineJoin;
  FLineCap := Attr.FLineCap;
  FMiterLimit := Attr.FMiterLimit;
  FStrokeWidth := Attr.FStrokeWidth;

  FTransform := TAggTransAffine.Create;
  FTransform.AssignAll(Attr.FTransform);
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
begin
  FCurved.Free;
  FCurvedCount.Free;
  FCurvedStroked.Free;
  FCurvedStrokedTrans.Free;
  FCurvedTrans.Free;
  FCurvedTransContour.Free;

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

  Attr.Create(GetCurrentAttributes, Idx);

  FAttrStorage.Add(@Attr);
end;

procedure TPathRenderer.ParsePath;
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
        raise TSvgException.Create(PAnsiChar('parse_path: Command A: NOT IMPLEMENTED YET'));

      'Z', 'z':
        CloseSubpath;

    else
      begin
        raise TSvgException.Create(Format('parse_path: Invalid Command %c',
          [Cmd]));
      end;
    end;
  end;
end;

procedure TPathRenderer.EndPath;
var
  Idx : Cardinal;
  Attr: TPathAttributesRecord;
begin
  if FAttrStorage.Size = 0 then
    raise TSvgException.Create(PAnsiChar('end_path : The path was not begun'));

  Attr.Create(GetCurrentAttributes);

  Idx := PPathAttributesRecord(FAttrStorage[FAttrStorage.Size - 1]).Index;
  Attr.Index := Idx;

  Move(Pointer(@Attr)^, PPathAttributesRecord(FAttrStorage[
    FAttrStorage.Size - 1])^, SizeOf(TPathAttributesRecord));

  PopAttribute;
end;

procedure TPathRenderer.MoveTo;
begin
  if Rel then
    FStorage.RelativeToAbsolute(@X, @Y);

  FStorage.MoveTo(X, Y);
end;

procedure TPathRenderer.LineTo;
begin
  if Rel then
    FStorage.RelativeToAbsolute(@X, @Y);

  FStorage.LineTo(X, Y);
end;

procedure TPathRenderer.HorizontalLineTo;
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

procedure TPathRenderer.VerticalLineTo;
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
  // raise exception("Curve3(x, y) : NOT IMPLEMENTED YET");
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
  // throw exception("Curve4(x2, y2, x, y) : NOT IMPLEMENTED YET");
  if Rel then
    FStorage.Curve4Relative(X2, Y2, X, Y)
  else
    FStorage.Curve4(X2, Y2, X, Y);
end;

procedure TPathRenderer.CloseSubpath;
begin
  FStorage.EndPoly(CAggPathFlagsClose);
end;

procedure TPathRenderer.AddPath;
begin
  FStorage.AddPath(Vs, PathID, SolidPath);
end;

function TPathRenderer.GetVertexCount;
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

      FillColor.Black;
      StrokeColor.Black;

      FillFlag := True;
      StrokeFlag := False;
      EvenOddFlag := False;
      FLineJoin := ljMiter;
      FLineCap := lcButt;
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
    raise TSvgException.Create(PAnsiChar('pop_attr : Attribute stack is empty'));

  FAttrStack.RemoveLast;
end;

procedure TPathRenderer.SetFillColor;
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.FillColor := F^;
  Attr.FillFlag := True;
end;

procedure TPathRenderer.SetStrokeColor;
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.StrokeColor := S^;
  Attr.StrokeFlag := True;
end;

procedure TPathRenderer.EvenOdd;
begin
  GetCurrentAttributes.EvenOddFlag := Flag;
end;

procedure TPathRenderer.SetStrokeWidth;
var
  Attr: PPathAttributesRecord;
begin
  Attr := GetCurrentAttributes;

  Attr.StrokeWidth := W;
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

procedure TPathRenderer.SetFillOpacity;
begin
  GetCurrentAttributes.FillColor.Opacity := Op;
end;

procedure TPathRenderer.SetStrokeOpacity;
begin
  GetCurrentAttributes.StrokeColor.Opacity := Op;
end;

procedure TPathRenderer.SetLineJoin(Value: TAggLineJoin);
begin
  GetCurrentAttributes.FLineJoin := Value;
end;

procedure TPathRenderer.SetLineCap(Value: TAggLineCap);
begin
  GetCurrentAttributes.FLineCap := Value;
end;

procedure TPathRenderer.SetMiterLimit;
begin
  GetCurrentAttributes.FMiterLimit := Ml;
end;

function TPathRenderer.GetTransform;
begin
  Result := GetCurrentAttributes.FTransform;
end;

procedure TPathRenderer.ArrangeOrientations;
begin
  FStorage.ArrangeOrientationsAllPaths(CAggPathFlagsCcw);
end;

procedure TPathRenderer.Expand;
begin
  FCurvedTransContour.Width := Value;
end;

function TPathRenderer.ArrayOperator;
begin
  FTransform.AssignAll(@PPathAttributesRecord(FAttrStorage[Idx]).FTransform);

  Result := PPathAttributesRecord(FAttrStorage[Idx]).Index;
end;

procedure TPathRenderer.BoundingRect;
var
  Trans: TAggConvTransform;
begin
  Trans := TAggConvTransform.Create(FStorage, FTransform);

  BoundingRectInteger(Trans, @Self, 0, FAttrStorage.Size, X1, Y1, X2, Y2);
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

    // FCurved.approximation_method(curveInc );

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

      Color := Attr.FillColor;

      Color.Opacity := Color.Opacity * Opacity;
      Ren.SetColor(@Color);
      RenderScanLines(Ras, Sl, Ren);
    end;

    if Attr.StrokeFlag then
    begin
      FCurvedStroked.Width := Attr.StrokeWidth;

      // FCurvedStroked.FLineJoin((attr.FLineJoin == MiterJoin) ? MiterJoin_round : attr.FLineJoin);

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

      Color := Attr.StrokeColor;

      Color.Opacity := Color.Opacity * Opacity;
      Ren.SetColor(@Color);
      RenderScanLines(Ras, Sl, Ren);
    end;

    Inc(I);
  end;
end;

function TPathRenderer.GetCurrentAttributes;
begin
  if FAttrStack.Size = 0 then
    raise TSvgException.Create(PAnsiChar('cur_attr : Attribute stack is empty'));

  Result := PPathAttributesRecord(FAttrStack[FAttrStack.Size - 1]);
end;

end.
