unit RedFox;

interface

uses
  Windows,
  Graphics,
  Types,
  Agg2D,
  RedFoxColor;

type
  TRedFoxAlign = (AlignNear, AlignCenter, AlignFar);

  P32BitPixel = ^T32BitPixel;
  T32BitPixel = packed record
    B : byte;
    G : byte;
    R : byte;
    A : byte;
  end;


  //I should depreciated TSingleRect for TRectF
  TSingleRect = record
    x1 : single;
    x2 : single;
    y1 : single;
    y2 : single;
    function Height : single;
    function Width  : single;
  end;

function ToPAnsiChar(Text:string):PAnsiChar;
function DistanceBetweenPoints(const PointA,PointB:TPointF):single; inline; overload;
function DistanceBetweenPoints(const X1,Y1,X2,Y2:single):single; inline; overload;

function InRect(px,py:single; Rect:TRectF):boolean; overload;
function InRect(px,py:single; Rect:TSingleRect):boolean; overload;
function InRect(Point:TPoint; Rect:TRect):boolean; overload;
function InRect(px,py:integer; Rect:TRect):boolean; overload;


// RefineRectEdges() rounds the rectangle edges to fall the on nearest pixel. This
// helps when drawing rectangles with one pixel wide borders. Often, anti-aliasing
// will cause the single pixel edge to be 2 pixels wide and partially transparent,
// giving the impression of rectangles having fuzzy edges.
function RefineRectEdge(Rect:TRectF; SnapToHalfPixel : boolean = false):TRectF;



//TODO: add a bitblit function. Clean these two functions up. Ensure it is as neat
// as possible.
function WinAlphaBlend(hdcDest: HDC; xoriginDest, yoriginDest, wDest, hDest: integer;
  hdcSrc: HDC; xoriginSrc, yoriginSrc, wSrc, hSrc: integer; ftn: BLENDFUNCTION): LongBool;
  stdcall; external 'Msimg32.dll' name 'AlphaBlend';


procedure AlphaBlendBitmapToDC(DeviceContext:HDC; DcWidth, DcHeight: integer; SourceBitmap : TBitmap);


type
  // A non-reference-counted IInterface implementation.
  TPureInterfacedObject = class(TObject, IInterface)
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;





implementation

uses
  SysUtils;

function ToPAnsiChar(Text:string):PAnsiChar;
// http://stackoverflow.com/a/614720/395461
begin
  result := PAnsiChar(AnsiString(Text));
end;

function DistanceBetweenPoints(const PointA,PointB:TPointF):single;
var
  xd,yd:single;
begin
  xd := abs(PointA.X - PointB.X);
  yd := abs(PointA.Y - PointB.Y);
  if xd > yd
    then result := xd
    else result := yd;
end;

function DistanceBetweenPoints(const X1,Y1,X2,Y2:single):single;
var
  xd,yd:single;
begin
  xd := abs(x1 - x2);
  yd := abs(y1 - y2);
  if xd > yd
    then result := xd
    else result := yd;
end;

{ TSingleRect }

function TSingleRect.Height: single;
begin
  result := abs(y2 - y1);
end;

function TSingleRect.Width: single;
begin
  result := abs(x2 - x1);
end;


function InRect(px,py:single; Rect:TRectF):boolean; overload;
var
  x1,x2, y1, y2 : single;
begin

  if Rect.Left < Rect.Right then
  begin
    x1 := Rect.Left;
    x2 := Rect.Right;
  end else
  begin
    x2 := Rect.Left;
    x1 := Rect.Right;
  end;

  if (Rect.Top < Rect.Bottom) then
  begin
    y1 := Rect.Top;
    y2 := Rect.Bottom;
  end else
  begin
    y2 := Rect.Top;
    y1 := Rect.Bottom;
  end;

  if (px >= x1) and (px <= x2) and (py >= y1) and (py <= y2)
    then result := true
    else result := false;
end;

function InRect(px,py:single; Rect:TSingleRect):boolean;
begin
  if (px >= Rect.x1) and (px <= Rect.x2) and (py >= Rect.y1) and (py <= Rect.y2)
    then result := true
    else result := false;
end;

function InRect(Point:TPoint; Rect:TRect):boolean; overload;
begin
  if (Point.X >= Rect.Left) and (Point.X <= Rect.Right) and (Point.Y >= Rect.Top) and (Point.Y <= Rect.Bottom)
    then result := true
    else result := false;
end;

function InRect(px,py:integer; Rect:TRect):boolean; overload;
begin
  if Rect.Top < Rect.Bottom then
  begin
    if (pX >= Rect.Left) and (pX <= Rect.Right) and (pY >= Rect.Top) and (pY <= Rect.Bottom)
      then result := true
      else result := false;
  end else
  begin
    if (pX >= Rect.Left) and (pX <= Rect.Right) and (pY >= Rect.Bottom) and (pY <= Rect.Top)
      then result := true
      else result := false;
  end;


end;


function RefineRectEdge(Rect:TRectF; SnapToHalfPixel : boolean = false):TRectF;
begin
  if SnapToHalfPixel = false then
  begin
    Rect.Left   := round(Rect.Left);
    Rect.Right  := round(Rect.Right);
    Rect.Top    := round(Rect.Top);
    Rect.Bottom := round(Rect.Bottom);
  end else
  begin
    Rect.Left   := round(Rect.Left)   + 0.5;
    Rect.Right  := round(Rect.Right)  + 0.5;
    Rect.Top    := round(Rect.Top)    - 0.5;
    Rect.Bottom := round(Rect.Bottom) - 0.5;
  end;

  result := Rect;
end;

procedure AlphaBlendBitmapToDC(DeviceContext:HDC; DcWidth, DcHeight: integer; SourceBitmap : TBitmap);
var
  BlendFunction: TBlendFunction;
begin
  if SourceBitmap.PixelFormat <> pf32Bit then raise Exception.Create('Pixel format is not correct.');
  if SourceBitmap.AlphaFormat <> afPremultiplied then raise Exception.Create('Alpha must be premultiplied.');

  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 255;
  BlendFunction.AlphaFormat := AC_SRC_ALPHA;

  AlphaBlend(DeviceContext, 0, 0, DcWidth, DcHeight, SourceBitmap.Canvas.Handle, 0, 0, SourceBitmap.Width, SourceBitmap.Height, BlendFunction);
end;

{ TPureInterfacedObject }

function TPureInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TPureInterfacedObject._AddRef: Integer;
begin
  Result := -1;
end;

function TPureInterfacedObject._Release: Integer;
begin
  Result := -1;
end;

end.
