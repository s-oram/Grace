unit VamImage;

interface

uses
  Types, Controls, Classes, Graphics, AggColor,
  RedFox, RedFoxGraphicControl, RedFoxColor,
  VamWinControl;

type
  TVamImage = class(TVamWinControl)
  private
    fBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Bitmap:TBitmap read fBitmap write SetBitmap;

  published

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils,
  AggPixelFormat;



{ TVamImage }

constructor TVamImage.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVamImage.Destroy;
begin

  inherited;
end;

procedure TVamImage.SetBitmap(const Value: TBitmap);
begin
  if Value <> fBitmap then
  begin
    fBitmap := Value;
    Invalidate;
  end;
end;

procedure TVamImage.Paint;
var
  SrcRect : TRect;
  DstRect : TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(0,0,0,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;


  if assigned(Bitmap) then
  begin
    SrcRect.Left   := 0;
    SrcRect.Width  := Bitmap.Width;
    SrcRect.Top    := 0;
    SrcRect.Bottom := Bitmap.Height;

    DstRect.Left   := (Width - SrcRect.Width)   div 2;
    DstRect.Right  := DstRect.Left + SrcRect.Width;
    DstRect.Top    := (Height - SrcRect.Height) div 2;
    DstRect.Bottom := DstRect.Top + SrcRect.Height;

    BackBuffer.TransformImage(Bitmap, SrcRect.Left, SrcRect.Top, SrcRect.Right, SrcRect.Bottom, DstRect.Left, DstRect.Top);
  end;

end;



end.
