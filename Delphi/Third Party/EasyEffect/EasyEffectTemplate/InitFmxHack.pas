unit InitFmxHack;

interface

procedure InitGDIP; // Call before using FMX.
procedure FreeGDIP; // Call after using FMX.

// NOTE:
// InitGDIP() must be called before instantiating a FireMonkey form.
// FreeGDIP() must be called after all FireMonkey forms are destroyed.
//
// InitGDIP/FreeGDIP can not be called from the initalization or finalization sections,
// or any method called from these sections.
//


implementation

uses
  System.SysUtils,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  FMX.Types;

var
  NeedToShutdownGDIPlus: Boolean;
  GDIPlusInput: TGDIPlusStartupInput;
  gdiplusToken: Cardinal;
  TempRgn: GpRegion;

type
  TBitmapAccess = class(TBitmap);

procedure InitGDIP;
begin
  NeedToShutdownGDIPlus := False;
  case GdipCreateRegion(TempRgn) of
    Ok: GdipDeleteRegion(TempRgn);
    GdiplusNotInitialized:
    begin
      GDIPlusInput.GdiplusVersion := 1;
      GDIPlusInput.DebugEventCallback := nil;
      GDIPlusInput.SuppressBackgroundThread := False;
      GDIPlusInput.SuppressExternalCodecs := False;
      GdiplusStartup(GDIPlusToken, @GDIPlusInput, nil);
      NeedToShutdownGDIPlus := True;
    end;
  end;

end;

procedure FreeGDIP;
begin
  // HACK: Need to forcibly release a GDI+ object held in a global variable.
  FreeAndNil(TBitmapAccess(GetMeasureBitmap).FCanvas);

  // These lines have been copied from Winapi.GDIPOBJ. I'm not 100% sure
  // if there needed but it's probably safer to include them as they are part of
  // the standard FireMonkey shutdown sequence. Similar code is also found in
  // the VGScene library.
  if Assigned(GenericSansSerifFontFamily)           then FreeAndNil(GenericSansSerifFontFamily);
  if Assigned(GenericSerifFontFamily)               then FreeAndNil(GenericSerifFontFamily);
  if Assigned(GenericMonospaceFontFamily)           then FreeAndNil(GenericMonospaceFontFamily);
  if Assigned(GenericTypographicStringFormatBuffer) then FreeAndNil(GenericTypographicStringFormatBuffer);
  if Assigned(GenericDefaultStringFormatBuffer)     then FreeAndNil(GenericDefaultStringFormatBuffer);

  // Finalise GDI+ here if needed...
  if NeedToShutdownGDIPlus then GdiplusShutdown(GDIPlusToken);
end;

end.
