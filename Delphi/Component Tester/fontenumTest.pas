unit fontenumTest;

interface

uses
  Classes;


procedure CollectFonts(FontList: TStringList);

implementation

uses
  {$IFDEF MACOS}
  MacApi.Appkit,Macapi.CoreFoundation, Macapi.Foundation,
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Winapi.Messages, Winapi.Windows,
  {$ENDIF}
  Types, SysUtils;

{$IFDEF MSWINDOWS}
function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
  FontType: Integer; Data: Pointer): Integer; stdcall;
var
  S: TStrings;
  Temp: string;
begin
  S := TStrings(Data);
  Temp := LogFont.lfFaceName;
  if (S.Count = 0) or (AnsiCompareText(S[S.Count-1], Temp) <> 0) then
    S.Add(Temp);
  Result := 1;
end;
{$ENDIF}

procedure CollectFonts(FontList: TStringList);
var
{$IFDEF MACOS}
  fManager: NsFontManager;
  list:NSArray;
  lItem:NSString;
{$ENDIF}
{$IFDEF MSWINDOWS}
  DC: HDC;
  LFont: TLogFont;
{$ENDIF}
  i: Integer;
begin

  {$IFDEF MACOS}
    fManager := TNsFontManager.Wrap(TNsFontManager.OCClass.sharedFontManager);
    list := fManager.availableFontFamilies;
    if (List <> nil) and (List.count > 0) then
    begin
      for i := 0 to List.Count-1 do
      begin
        lItem := TNSString.Wrap(List.objectAtIndex(i));
        FontList.Add(String(lItem.UTF8String))
      end;
    end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    DC := GetDC(0);
    FillChar(LFont, sizeof(LFont), 0);
    LFont.lfCharset := DEFAULT_CHARSET;
    EnumFontFamiliesEx(DC, LFont, @EnumFontsProc, Winapi.Windows.LPARAM(FontList), 0);
    ReleaseDC(0, DC);
  {$ENDIF}
end;

end.
