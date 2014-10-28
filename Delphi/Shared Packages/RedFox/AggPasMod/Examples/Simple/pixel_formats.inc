, AggPixelFormat, AggColor

{$IFDEF AGG_GRAY8}
  , AggPixelFormatGray;

const
  CPixFormat = pfGray8;

  CPixelFormat: DefinePixelFormat = PixelFormatGray8;
  CPixelFormatPre: DefinePixelFormat = PixelFormatGray8_pre;

{$ELSE}
{$IFDEF AGG_BGR24}
  , AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas';

const
  CPixFormat = pfBgr24;

  CPixelFormat: DefinePixelFormat = PixelFormatBgr24;
  CPixelFormatPre: DefinePixelFormat = PixelFormatBgr24Pre;
  CPixelFormatGamma: DefinePixelFormatGamma = PixelFormatBgr24Gamma;

  CComponentOrder: TAggOrder = (R: 2; G: 1; B: 0; A: 3);

{$ELSE}
{$IFDEF AGG_Rgb24}
  , AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas';

const
  CPixFormat = pfRgb24;
  CPixelFormat: DefinePixelFormat = PixelFormatRgb24;
  CPixelFormatPre: DefinePixelFormat = PixelFormatRgb24_pre;
  CPixelFormatGamma: DefinePixelFormatGamma = PixelFormatRgb24Gamma;
  CComponentOrder: TAggOrder = (R: 0; G: 1; B: 2; A: 3);

{$ELSE}
{$IFDEF AGG_BGRA32}
  , AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas';

const
  CPixFormat = pfBgra32;

  CPixelFormat: DefinePixelFormat = PixelFormatBgra32;
  CPixelFormatPre: DefinePixelFormat = PixelFormatBgra32Pre;

  CComponentOrder: TAggOrder = (R: 2; G: 1; B: 0; A: 3);

{$ELSE}
{$IFDEF AGG_RgbA32}
  , AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas';

const
  CPixFormat = pfRgba32;

  CPixelFormat: DefinePixelFormat = PixelFormatRgba32;
  CPixelFormatPre: DefinePixelFormat = PixelFormatRgba32_pre;

  CComponentOrder: TAggOrder = (R: 0; G: 1; B: 2; A: 3);

{$ELSE}
{$IFDEF AGG_ARGB32}
  , AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas';

const
  CPixFormat = pfArgb32;

  CPixelFormat: DefinePixelFormat = PixelFormatArgb32;
  CPixelFormatPre: DefinePixelFormat = PixelFormatArgb32_pre;

  CComponentOrder: TAggOrder = (R: 1; G: 2; B: 3; A: 0);

{$ELSE}
{$IFDEF AGGABGR32}
  , AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas';

const
  CPixFormat = Pix_formatABGR32;

  CPixelFormat: DefinePixelFormat = PixelFormatAbgr32;
  CPixelFormatPre: DefinePixelFormat = PixelFormatAbgr32_pre;

  CComponentOrder: TAggOrder = (R: 3; G: 2; B: 1; A: 0);

{$ELSE}
{$IFDEF AGG_Rgb555}
  , AggPixelFormatRgb_packed in '..\..\Source\AggPixelFormatRgb_packed.pas';

const
  CPixFormat = pfRgb555;

  CPixelFormat: DefinePixelFormat = PixelFormatRgb555;
  CPixelFormatPre: DefinePixelFormat = PixelFormatRgb555_pre;
  CPixelFormatGamma: DefinePixelFormatGamma = PixelFormatRgb555Gamma;

  CComponentOrder: TAggOrder = (R: 0; G: 1; B: 2; A: 3);

{$ELSE}
{$IFDEF AGG_Rgb565}
  , AggPixelFormatRgb_packed in '..\..\Source\AggPixelFormatRgb_packed.pas';

const
  CPixFormat = pfRgb565;

  CPixelFormat: DefinePixelFormat = PixelFormatRgb565;
  CPixelFormatPre: DefinePixelFormat = PixelFormatRgb565_pre;
  CPixelFormatGamma: DefinePixelFormatGamma = PixelFormatRgb565Gamma;

  CComponentOrder: TAggOrder = (R: 0; G: 1; B: 2; A: 3);

{$ELSE}
  ;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
//
// Please define pixel format: AGG_GRAY8, AGG_BGR24, AGG_Rgb24, etc. See this file above
//
