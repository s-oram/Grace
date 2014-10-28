unit GR32_Agg2D;

interface

uses
  GR32, AggBasics, AggColor, AggPixelFormat, AggTransAffine, Agg2D;

type
  TGr32Agg2DImage = class(TAgg2DImage)
  public
    constructor Create(Bitmap32: TBitmap32); overload;
    procedure Attach(Bitmap32: TBitmap32); overload;
  end;

  TGr32Agg2D = class(TAgg2D)
  private
    function GetFillColor: TColor32;
    function GetImageBlendColor: TColor32;
    function GetLineColor: TColor32;
    procedure SetFillColor(const Value: TColor32);
    procedure SetImageBlendColor(const Value: TColor32);
    procedure SetLineColor(const Value: TColor32);
  public
    procedure Attach(Bitmap32: TBitmap32); overload;

    procedure FillLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TColor32;
      Profile: Double = 1);
    procedure LineLinearGradient(X1, Y1, X2, Y2: Double; C1, C2: TColor32;
      Profile: Double = 1);

    procedure FillRadialGradient(X, Y, R: Double; C1, C2: TColor32;
      Profile: Double = 1); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2: TColor32;
      Profile: Double = 1); overload;

    procedure FillRadialGradient(X, Y, R: Double; C1, C2, C3: TColor32); overload;
    procedure LineRadialGradient(X, Y, R: Double; C1, C2, C3: TColor32); overload;

    property ImageBlendColor: TColor32 read GetImageBlendColor write SetImageBlendColor;
    property FillColor: TColor32 read GetFillColor write SetFillColor;
    property LineColor: TColor32 read GetLineColor write SetLineColor;
  end;

implementation

{ TGr32Agg2DImage }

constructor TGr32Agg2DImage.Create(Bitmap32: TBitmap32);
begin
  inherited Create(PInt8U(Bitmap32.ScanLine[0]), Bitmap32.Width,
    Bitmap32.Height, -4 * Bitmap32.Width);
end;

procedure TGr32Agg2DImage.Attach(Bitmap32: TBitmap32);
begin
  inherited Attach(PInt8U(Bitmap32.ScanLine[0]), Bitmap32.Width,
    Bitmap32.Height, -4 * Bitmap32.Width);
end;


function Color32ToAggColorRgba8(Value: TColor32): TAggColorRgba8;
var
  Color32: TColor32Entry absolute Value;
begin
  Result.A := Color32.A;
  Result.B := Color32.B;
  Result.G := Color32.G;
  Result.R := Color32.R;
end;

function AggColorRgba8ToColor32(Value: TAggColorRgba8): TColor32;
var
  Color32: TColor32Entry absolute Result;
begin
  Color32.A := Value.A;
  Color32.B := Value.B;
  Color32.G := Value.G;
  Color32.R := Value.R;
end;


{ TGr32Agg2D }

procedure TGr32Agg2D.Attach(Bitmap32: TBitmap32);
begin
  inherited Attach(PInt8U(Bitmap32.ScanLine[0]), Bitmap32.Width,
    Bitmap32.Height, -4 * Bitmap32.Width);
end;

procedure TGr32Agg2D.FillLinearGradient(X1, Y1, X2, Y2: Double; C1,
  C2: TColor32; Profile: Double);
begin
  inherited FillLinearGradient(X1, Y1, X2, Y2, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Profile);
end;

procedure TGr32Agg2D.FillRadialGradient(X, Y, R: Double; C1, C2: TColor32;
  Profile: Double);
begin
  inherited FillRadialGradient(X, Y, R, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Profile);
end;

procedure TGr32Agg2D.FillRadialGradient(X, Y, R: Double; C1, C2, C3: TColor32);
begin
  inherited FillRadialGradient(X, Y, R, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Color32ToAggColorRgba8(C3));
end;

function TGr32Agg2D.GetFillColor: TColor32;
begin
  Result := AggColorRgba8ToColor32(FFillColor);
end;

function TGr32Agg2D.GetImageBlendColor: TColor32;
begin
  Result := AggColorRgba8ToColor32(FImageBlendColor);
end;

function TGr32Agg2D.GetLineColor: TColor32;
begin
  Result := AggColorRgba8ToColor32(FLineColor);
end;

procedure TGr32Agg2D.LineLinearGradient(X1, Y1, X2, Y2: Double; C1,
  C2: TColor32; Profile: Double);
begin
  inherited LineLinearGradient(X1, Y1, X2, Y2, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Profile);
end;

procedure TGr32Agg2D.LineRadialGradient(X, Y, R: Double; C1, C2: TColor32;
  Profile: Double);
begin
  inherited LineRadialGradient(X, Y, R, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Profile);
end;

procedure TGr32Agg2D.LineRadialGradient(X, Y, R: Double; C1, C2, C3: TColor32);
begin
  inherited LineRadialGradient(X, Y, R, Color32ToAggColorRgba8(C1),
    Color32ToAggColorRgba8(C2), Color32ToAggColorRgba8(C3));
end;

procedure TGr32Agg2D.SetFillColor(const Value: TColor32);
begin
  inherited FillColor := Color32ToAggColorRgba8(Value);
end;

procedure TGr32Agg2D.SetImageBlendColor(const Value: TColor32);
begin
  inherited ImageBlendColor := Color32ToAggColorRgba8(Value);
end;

procedure TGr32Agg2D.SetLineColor(const Value: TColor32);
begin
  inherited LineColor := Color32ToAggColorRgba8(Value);
end;

end.
