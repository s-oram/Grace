unit AggRendererMultiClip;

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
  AggArray,
  AggColor,
  AggRenderingBuffer,
  AggRendererBase,
  AggPixelFormat;

type
  TAggRendererMultiClip = class(TAggRendererBase)
  private
    FClip: TAggPodDeque;
    FCurrentClibBoxIndex: Cardinal;
    FBounds: TRectInteger;
  protected
    function GetBoundingXMin: Integer; override;
    function GetBoundingYMin: Integer; override;
    function GetBoundingXMax: Integer; override;
    function GetBoundingYMax: Integer; override;
  public
    constructor Create(PixelFormatProcessor: TAggPixelFormatProcessor;
      OwnPixelFormatProcessor: Boolean = False); override;
    destructor Destroy; override;

    function GetBoundingClipBox: PRectInteger; virtual;

    procedure FirstClipBox; override;
    function NextClipBox: Boolean; override;

    procedure ResetClipping(Visibility: Boolean); override;

    procedure AddClipBox(X1, Y1, X2, Y2: Integer); overload;
    procedure AddClipBox(Rect: TRectInteger); overload;

    procedure CopyPixel(X, Y: Integer; C: PAggColor); override;
    procedure BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u); override;
    function Pixel(X, Y: Integer): TAggColor; override;

    procedure CopyHorizontalLine(X1, Y, X2: Integer; C: PAggColor); override;
    procedure CopyVerticalLine(X, Y1, Y2: Integer; C: PAggColor); override;

    procedure BlendHorizontalLine(X1, Y, X2: Integer; C: PAggColor; Cover: Int8u); override;
    procedure BlendVerticalLine(X, Y1, Y2: Integer; C: PAggColor; Cover: Int8u); override;

    procedure CopyBar(X1, Y1, X2, Y2: Integer; C: PAggColor); override;
    procedure BlendBar(X1, Y1, X2, Y2: Integer; C: PAggColor;
      Cover: Int8u); override;

    procedure BlendSolidHSpan(X, Y, Len: Integer; C: PAggColor;
      Covers: PInt8u); override;
    procedure BlendSolidVSpan(X, Y, Len: Integer; C: PAggColor;
      Covers: PInt8u); override;

    procedure CopyColorHSpan(X, Y, Len: Integer; Colors: PAggColor); override;
    procedure BlendColorHSpan(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;
    procedure BlendColorVSpan(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull); override;

    procedure CopyFrom(From: TAggRenderingBuffer; Rc: PRectInteger = nil;
      ToX: Integer = 0; ToY: Integer = 0); override;
  end;

implementation


{ TAggRendererMultiClip }

constructor TAggRendererMultiClip.Create(PixelFormatProcessor: TAggPixelFormatProcessor;
  OwnPixelFormatProcessor: Boolean = False);
begin
  inherited Create(PixelFormatProcessor, OwnPixelFormatProcessor);

  FClip := TAggPodDeque.Create(SizeOf(TRectInteger), 4);
  FBounds := RectInteger(GetXMin, GetYMin, GetXMax, GetYMax);

  FCurrentClibBoxIndex := 0;
end;

destructor TAggRendererMultiClip.Destroy;
begin
  FClip.Free;
  inherited;
end;

function TAggRendererMultiClip.GetBoundingClipBox;
begin
  Result := @FBounds;
end;

function TAggRendererMultiClip.GetBoundingXMin;
begin
  Result := FBounds.X1;
end;

function TAggRendererMultiClip.GetBoundingYMin;
begin
  Result := FBounds.Y1;
end;

function TAggRendererMultiClip.GetBoundingXMax;
begin
  Result := FBounds.X2;
end;

function TAggRendererMultiClip.GetBoundingYMax;
begin
  Result := FBounds.Y2;
end;

procedure TAggRendererMultiClip.FirstClipBox;
var
  Cb: PRectInteger;
begin
  FCurrentClibBoxIndex := 0;

  if FClip.Size <> 0 then
  begin
    Cb := FClip[0];

    ClipBoxNaked(Cb.X1, Cb.Y1, Cb.X2, Cb.Y2);
  end;
end;

function TAggRendererMultiClip.NextClipBox;
var
  Cb: PRectInteger;
begin
  Inc(FCurrentClibBoxIndex);

  if FCurrentClibBoxIndex < FClip.Size then
  begin
    Cb := FClip[FCurrentClibBoxIndex];

    ClipBoxNaked(Cb.X1, Cb.Y1, Cb.X2, Cb.Y2);

    Result := True;

    Exit;
  end;

  Result := False;
end;

procedure TAggRendererMultiClip.ResetClipping;
begin
  inherited ResetClipping(Visibility);

  FClip.RemoveAll;

  FCurrentClibBoxIndex := 0;

  FBounds := GetClipBox^;
end;

procedure TAggRendererMultiClip.AddClipBox(X1, Y1, X2, Y2: Integer);
begin
  AddClipBox(RectInteger(X1, Y1, X2, Y2));
end;

procedure TAggRendererMultiClip.AddClipBox(Rect: TRectInteger);
var
  Rc: TRectInteger;
begin
  Rect.Normalize;
  Rc := RectInteger(0, 0, Width - 1, Height - 1);

  if Rect.Clip(Rc) then
  begin
    FClip.Add(@Rect);

    if Rect.X1 < FBounds.X1 then
      FBounds.X1 := Rect.X1;

    if Rect.Y1 < FBounds.Y1 then
      FBounds.Y1 := Rect.Y1;

    if Rect.X2 > FBounds.X2 then
      FBounds.X2 := Rect.X2;

    if Rect.Y2 > FBounds.Y2 then
      FBounds.Y2 := Rect.Y2;
  end;
end;

procedure TAggRendererMultiClip.CopyPixel(X, Y: Integer; C: PAggColor);
begin
  FirstClipBox;

  repeat
    if Inbox(X, Y) then
    begin
      FPixelFormatProcessor.CopyPixel(FPixelFormatProcessor, X, Y, C);

      Break;
    end;
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
    if Inbox(X, Y) then
    begin
      FPixelFormatProcessor.BlendPixel(FPixelFormatProcessor, X, Y, C, Cover);

      Break;
    end;
  until not NextClipBox;
end;

function TAggRendererMultiClip.Pixel;
begin
  FirstClipBox;

  repeat
    if Inbox(X, Y) then
    begin
      Result := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, X, Y);

      Exit;
    end;
  until not NextClipBox;

  Result.Clear;
end;

procedure TAggRendererMultiClip.CopyHorizontalLine(X1, Y, X2: Integer;
  C: PAggColor);
begin
  FirstClipBox;

  repeat
    inherited CopyHorizontalLine(X1, Y, X2, C);
  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyVerticalLine(X, Y1, Y2: Integer;
  C: PAggColor);
begin
  FirstClipBox;

  repeat
    inherited CopyVerticalLine(X, Y1, Y2, C);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendHorizontalLine(X1, Y, X2: Integer;
  C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
    inherited BlendHorizontalLine(X1, Y, X2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendVerticalLine(X, Y1, Y2: Integer;
  C: PAggColor; Cover: Int8u);
begin
  FirstClipBox;

  repeat
    inherited BlendVerticalLine(X, Y1, Y2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyBar(X1, Y1, X2, Y2: Integer; C: PAggColor);
begin
  FirstClipBox;

  repeat
    inherited CopyBar(X1, Y1, X2, Y2, C);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendBar(X1, Y1, X2, Y2: Integer; C: PAggColor;
  Cover: Int8u);
begin
  FirstClipBox;

  repeat
    inherited Blendbar(X1, Y1, X2, Y2, C, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidHSpan(X, Y, Len: Integer;
  C: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
    inherited BlendSolidHSpan(X, Y, Len, C, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendSolidVSpan(X, Y, Len: Integer;
  C: PAggColor; Covers: PInt8u);
begin
  FirstClipBox;

  repeat
    inherited BlendSolidVSpan(X, Y, Len, C, Covers);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyColorHSpan(X, Y, Len: Integer;
  Colors: PAggColor);
begin
  FirstClipBox;

  repeat
    inherited CopyColorHSpan(X, Y, Len, Colors);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorHSpan(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
    inherited BlendColorHSpan(X, Y, Len, Colors, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.BlendColorVSpan(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FirstClipBox;

  repeat
    inherited BlendColorVSpan(X, Y, Len, Colors, Covers, Cover);

  until not NextClipBox;
end;

procedure TAggRendererMultiClip.CopyFrom(From: TAggRenderingBuffer;
  Rc: PRectInteger = nil; ToX: Integer = 0; ToY: Integer = 0);
begin
  FirstClipBox;

  repeat
    inherited CopyFrom(From, Rc, ToX, ToY);

  until not NextClipBox;
end;

end.
