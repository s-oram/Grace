unit VamLib.Graphics;

interface

uses
  Types, Windows,
  Vcl.Graphics, VamLib.Types, VamLib.Utils;


type
  IInterfacedBitmap = interface
    ['{366D818B-9F33-4584-9C2A-93922B2F486E}']
    function Bitmap : TBitmap;
  end;

  TInterfacedBitmap = class(TVamInterfacedObject, IInterfacedBitmap)
  private
    fBitmap: TBitmap;

    function IInterfacedBitmap.Bitmap = GetBitmap;
    function GetBitmap : TBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    property Bitmap : TBitmap read fBitmap;
  end;


// Calculate how large a canvas/control needs to be to display a multi-line block of text.
// Text can use EndOfLine charactors for multi-line strings.
function CalcRequiredTextRect(const CanvasHandle : HDC; const Text : string; const MaxWidth : integer):TRect;

implementation

{ TInterfacedBitmap }

constructor TInterfacedBitmap.Create;
begin
  fBitmap := TBitmap.Create;
end;

destructor TInterfacedBitmap.Destroy;
begin
  fBitmap.Free;
  inherited;
end;

function TInterfacedBitmap.GetBitmap: TBitmap;
begin
  result := fBitmap;
end;


function CalcRequiredTextRect(const CanvasHandle : HDC; const Text : string; const MaxWidth : integer):TRect;
var
  TextWidth, TextHeight : integer;
  TextRect : TRect;
  Format: TDrawTextFlags;
begin
  TextRect.Left   := 0;
  TextRect.Right  := 0;
  TextRect.Top    := 0;
  TextRect.Bottom := 0;

  Format := TTextFormatFlags([tfCalcRect]);
  DrawTextEx(CanvasHandle, PChar(Text), Length(Text), TextRect, Format, nil);

  TextWidth := TextRect.Width;

  if TextWidth > MaxWidth then TextWidth := MaxWidth;

  TextRect.Left   := 0;
  TextRect.Right  := TextWidth;
  TextRect.Top    := 0;
  TextRect.Bottom := 0;

  Format := TTextFormatFlags([tfCalcRect, tfWordBreak]);
  DrawTextEx(CanvasHandle, PChar(Text), Length(Text), TextRect, Format, nil);

  TextHeight := TextRect.Height;

  result := Rect(0,0,TextWidth,TextHeight);
end;



end.
