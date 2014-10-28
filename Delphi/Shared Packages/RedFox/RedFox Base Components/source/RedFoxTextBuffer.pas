unit RedFoxTextBuffer;

interface

uses
  Windows, Graphics, Types,
  Classes, RedFox, RedFoxColor, RedFoxImageBuffer;

const
  // DrawTextOptions
  DT_TOP                       = Windows.DT_TOP;
  DT_LEFT                      = Windows.DT_LEFT;
  DT_CENTER                    = Windows.DT_CENTER;
  DT_RIGHT                     = Windows.DT_RIGHT;
  DT_VCENTER                   = Windows.DT_VCENTER;
  DT_BOTTOM                    = Windows.DT_BOTTOM;
  DT_WORDBREAK                 = Windows.DT_WORDBREAK;
  DT_SINGLELINE                = Windows.DT_SINGLELINE;
  DT_EXPANDTABS                = Windows.DT_EXPANDTABS;
  DT_TABSTOP                   = Windows.DT_TABSTOP;
  DT_NOCLIP                    = Windows.DT_NOCLIP;
  DT_EXTERNALLEADING           = Windows.DT_EXTERNALLEADING;
  DT_CALCRECT                  = Windows.DT_CALCRECT;
  DT_NOPREFIX                  = Windows.DT_NOPREFIX;
  DT_INTERNAL                  = Windows.DT_INTERNAL;
  DT_EDITCONTROL               = Windows.DT_EDITCONTROL;
  DT_PATH_ELLIPSIS             = Windows.DT_PATH_ELLIPSIS;
  DT_END_ELLIPSIS              = Windows.DT_END_ELLIPSIS;
  DT_MODIFYSTRING              = Windows.DT_MODIFYSTRING;
  DT_RTLREADING                = Windows.DT_RTLREADING;
  DT_WORD_ELLIPSIS             = Windows.DT_WORD_ELLIPSIS;
  DT_NOFULLWIDTHCHARBREAK      = Windows.DT_NOFULLWIDTHCHARBREAK;
  DT_HIDEPREFIX                = Windows.DT_HIDEPREFIX;
  DT_PREFIXONLY                = Windows.DT_PREFIXONLY;

type
  TRedFoxTextBuffer = class
  private
    fWidth: integer;
    fHeight: integer;
    Buffer : TBitmap;
    TextRect : TRect;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateBuffer(aText : TStrings; aFont : TFont; HorzAlign, VertAlign : TRedFoxAlign; DrawTextOptions : integer);

    procedure DrawTo(Dest:TRedFoxImageBuffer; aColor:TRedFoxColor);

    procedure SetSize(aWidth, aHeight : integer);

    property Width  : integer read fWidth;
    property Height : integer read fHeight;
  end;

implementation

uses
  RedFoxBlend;

{ TRedFoxTextBuffer }

constructor TRedFoxTextBuffer.Create;
begin
  Buffer := Graphics.TBitmap.Create;
end;

destructor TRedFoxTextBuffer.Destroy;
begin
  Buffer.Free;
  inherited;
end;

procedure TRedFoxTextBuffer.SetSize(aWidth, aHeight: integer);
begin
  fWidth  := aWidth;
  fHeight := aHeight;
  Buffer.SetSize(aWidth, aHeight);
  TextRect := Rect(0,0, aWidth, aHeight);
end;

procedure TRedFoxTextBuffer.UpdateBuffer(aText: TStrings; aFont: TFont; HorzAlign, VertAlign : TRedFoxAlign; DrawTextOptions : integer);
var
  s : string;
  FormatOptions : integer;
begin
  Buffer.PixelFormat := TPixelFormat.pf32bit;
  Buffer.AlphaFormat := TAlphaFormat.afIgnored;

  Buffer.Canvas.Font.Assign(aFont);

  Buffer.Canvas.Brush.Style := TBrushStyle.bsSolid;
  Buffer.Canvas.Brush.Color := clBlack;
  Buffer.Canvas.Rectangle(0,0,Width,Height);

  Buffer.Canvas.Brush.Color := clBlack;
  Buffer.Canvas.Pen.Color   := clBlack;
  Buffer.Canvas.Brush.Style := TBrushStyle.bsSolid;
  Buffer.Canvas.Pen.Style   := TPenStyle.psSolid;

  //Buffer.Canvas.TextOut(20,20, 'Button6');
  //Windows.DrawText(Buffer.Canvas.Handle, 'Button6', 7, TextRect, DT_Left);

  FormatOptions := DrawTextOptions;

  case HorzAlign of
    AlignNear:    FormatOptions := FormatOptions or DT_Left;
    AlignCenter:  FormatOptions := FormatOptions or DT_CENTER;
    AlignFar:     FormatOptions := FormatOptions or DT_Right;
  end;

  case VertAlign of
    AlignNear:    FormatOptions := FormatOptions or DT_Top;
    AlignCenter:  FormatOptions := FormatOptions or DT_VCENTER;
    AlignFar:     FormatOptions := FormatOptions or DT_Bottom;
  end;

  s := aText.Text;
  Windows.DrawText(Buffer.Canvas.Handle, s, Length(s), TextRect, FormatOptions)
end;

procedure TRedFoxTextBuffer.DrawTo(Dest: TRedFoxImageBuffer; aColor: TRedFoxColor);
var
  BlitSrc  : TBlitSrcInfo;
  BlitDest : TBlitDestInfo;
  BlendFunc : TPixelBlendFunction;
begin
  // HACK: NOTE: Currently the text is drawn with ClearType settings (i think).
  // This function then uses the 'green' pixel as a alpha value for
  // anti-aliasing. It might be possible to improve this function
  // further yet to better approximate how clearType fonts are rendered.
  // Alternatively, rendering with true anti-alising might be better
  // than bodging it.
  BlendFunc := function(const BasePixel, TopPixel : T32BitPixel):T32BitPixel
  var
    Alpha : byte;
    MinA2, MaxA2, A2 : byte;
  begin
    Alpha    := TopPixel.G;
    if Alpha = 0 then
    begin
      result.A := 0;
      exit;
    end;
    MinA2 := 255 - BasePixel.A;
    MaxA2 := 255;
    a2 := ByteCrossFade(MinA2, MaxA2, Alpha);

    result.R := ByteCrossFade(BasePixel.R, aColor.R, A2);
    result.G := ByteCrossFade(BasePixel.G, aColor.G, A2);
    result.B := ByteCrossFade(BasePixel.B, aColor.B, A2);
    result.A := ByteCrossFade(BasePixel.A,Alpha, Alpha);
  end;

  BlitSrc.Buffer       := Buffer.ScanLine[Buffer.Height-1];
  BlitSrc.BufferWidth  := Width;
  BlitSrc.BufferHeight := Height;
  BlitSrc.x1           := 0;
  BlitSrc.y1           := 0;
  BlitSrc.x2           := Width;
  BlitSrc.y2           := Height;

  BlitDest.Buffer       := Dest.RedFoxInterface.Buffer;
  BlitDest.BufferWidth  := Dest.Width;
  BlitDest.BufferHeight := Dest.Height;
  BlitDest.x1           := 0;
  BlitDest.y1           := 0;

  BlitImage(BlitDest, BlitSrc, BlendFunc);



end;



end.
