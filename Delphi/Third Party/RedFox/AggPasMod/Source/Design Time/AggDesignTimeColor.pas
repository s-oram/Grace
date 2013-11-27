unit AggDesignTimeColor;

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
  Classes, SysUtils,
{$IFDEF FPC}
  RTLConsts, LazIDEIntf, PropEdits, Graphics, Dialogs, Forms,
  {$IFDEF Windows}
    Windows, Registry,
  {$ENDIF}
{$ELSE}
  Consts,
  DesignIntf, DesignEditors, VCLEditors,
  Windows, Registry, Graphics, Dialogs, Forms,
{$ENDIF}
  AggColor, Agg2D, AggWin32Bmp;

type
  { TAggColorManager }
  PColorEntry = ^TColorEntry;
  TColorEntry = record
    Name: string[31];
    Color: TAggRgba8;
  end;

  TAggColorManager = class(TList)
  public
    destructor Destroy; override;

    procedure AddColor(const AName: string; AColor: TAggRgba8);
    procedure EnumColors(Proc: TGetStrProc);
    function  FindColor(const AName: string): TAggRgba8;
    function  GetColor(const AName: string): TAggRgba8;
    function  GetColorName(AColor: TAggRgba8): string;
    procedure RegisterDefaultColors;
    procedure RemoveColor(const AName: string);
  end;

  { TAggRgba8Property }
  TAggRgba8Property = class(TIntegerProperty{$IFNDEF FPC},
    ICustomPropertyListDrawing, ICustomPropertyDrawing,
    ICustomPropertyDrawing80{$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    procedure Edit; override;

{$IFDEF FPC}
    procedure ListDrawValue(const Value: ansistring; Index: Integer;
      ACanvas:TCanvas; const ARect: TRect; AState: TPropEditDrawState);
      override;
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      AState: TPropEditDrawState); override;

{$ELSE}

    { ICustomPropertyListDrawing }
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);

    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
{$ENDIF}
  end;

procedure RegisterColor(const AName: string; AColor: TAggRgba8);
procedure UnregisterColor(const AName: string);

var
  AggColorManager: TAggColorManager;

implementation


function WinColor(AggRgba8: TAggRgba8): TColor;
{$IFDEF PUREPASCAL}
begin
  Result := ((AggRgba8 and $000000FF) shl 16) or
             (AggRgba8 and $0000FF00) or
            ((AggRgba8 and $00FF0000) shr 16);
{$ELSE}
asm

{$IFDEF TARGET_x64}
        MOV     EAX, ECX
{$ENDIF}
        // the alpha channel byte is set to zero!
        ROL     EAX, 8  // ABGR  ->  BGRA
        XOR     AL, AL  // BGRA  ->  BGR0
        BSWAP   EAX     // BGR0  ->  0RGB
{$ENDIF}
end;

function ColorToAggRgba8(WinColor: TColor): TAggRgba8; overload;
begin
  if WinColor < 0 then WinColor := GetSysColor(WinColor and $000000FF);

  Result.R := WinColor and $FF;
  Result.G := (WinColor and $FF00) shr 8;
  Result.B := (WinColor and $FF0000) shr 16;
  Result.A := $FF;
end;


{ TAggColorManager }

destructor TAggColorManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I], SizeOf(TColorEntry));
  inherited;
end;

procedure TAggColorManager.AddColor(const AName: string; AColor: TAggRgba8);
var
  NewEntry: PColorEntry;
begin
  New(NewEntry);
  if NewEntry = nil then
    raise Exception.Create('Could not allocate memory for color registration!');
  with NewEntry^ do
  begin
    Name := ShortString(AName);
    Color := AColor;
  end;
  Add(NewEntry);
end;

procedure TAggColorManager.EnumColors(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Proc(string(TColorEntry(Items[I]^).Name));
end;

function TAggColorManager.FindColor(const AName: string): TAggRgba8;
var
  I: Integer;
begin
  Result := CRgba8Black;
  for I := 0 to Count - 1 do
    with TColorEntry(Items[I]^) do
      if string(Name) = AName then
      begin
        Result := Color;
        Exit;
      end;
end;

function TAggColorManager.GetColor(const AName: string): TAggRgba8;
var
  S: string;

  function HexToClr(const HexStr: string): Cardinal;
  var
    I: Integer;
    C: Char;
  begin
    Result := 0;
    for I := 1 to Length(HexStr) do
    begin
      C := HexStr[I];
      case C of
        '0'..'9': Result := Int64(16) * Result + (Ord(C) - $30);
        'A'..'F': Result := Int64(16) * Result + (Ord(C) - $37);
        'a'..'f': Result := Int64(16) * Result + (Ord(C) - $57);
      else
        raise EConvertError.Create('Illegal character in hex string');
      end;
    end;
  end;

begin
  S := Trim(AName);
  if S[1] = '$' then S := Copy(S, 2, Length(S) - 1);
  if (S[1] = 'C') and (S[2] = 'R') then
    Result := FindColor(S)
  else
  try
    Result := TAggRgba8(HexToClr(S));
  except
    Result := CRgba8Black;
  end;
end;

function TAggColorManager.GetColorName(AColor: TAggRgba8): string;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    with TColorEntry(Items[Index]^) do
      if Color.ABGR = AColor.ABGR then
      begin
        Result := string(TColorEntry(Items[Index]^).Name);
        Exit;
      end;
  Result := '$' + IntToHex(AColor.ABGR, 8);
end;

procedure TAggColorManager.RegisterDefaultColors;
begin
  Capacity := 50;
  AddColor('CRgba8Black',       CRgba8Black);
  AddColor('CRgba8DarkGray',    CRgba8DarkGray);
  AddColor('CRgba8Gray',        CRgba8Gray);
  AddColor('CRgba8LightGray',   CRgba8LightGray);
  AddColor('CRgba8White',       CRgba8White);
  AddColor('CRgba8Maroon',      CRgba8Maroon);
  AddColor('CRgba8Green',       CRgba8Green);
  AddColor('CRgba8Olive',       CRgba8Olive);
  AddColor('CRgba8Navy',        CRgba8Navy);
  AddColor('CRgba8Purple',      CRgba8Purple);
  AddColor('CRgba8Teal',        CRgba8Teal);
  AddColor('CRgba8Red',         CRgba8Red);
  AddColor('CRgba8Lime',        CRgba8Lime);
  AddColor('CRgba8Yellow',      CRgba8Yellow);
  AddColor('CRgba8Blue',        CRgba8Blue);
  AddColor('CRgba8Fuchsia',     CRgba8Fuchsia);
  AddColor('CRgba8Aqua',        CRgba8Aqua);

  AddColor('CRgba8SemiWhite',   CRgba8SemiWhite);
  AddColor('CRgba8SemiBlack',   CRgba8SemiBlack);
  AddColor('CRgba8SemiRed',     CRgba8SemiRed);
  AddColor('CRgba8SemiGreen',   CRgba8SemiGreen);
  AddColor('CRgba8SemiBlue',    CRgba8SemiBlue);
  AddColor('CRgba8SemiMaroon',  CRgba8SemiMaroon);
  AddColor('CRgba8SemiOlive',   CRgba8SemiOlive);
  AddColor('CRgba8SemiNavy',    CRgba8SemiNavy);
  AddColor('CRgba8SemiPurple',  CRgba8SemiPurple);
  AddColor('CRgba8SemiTeal',    CRgba8SemiTeal);
  AddColor('CRgba8SemiLime',    CRgba8SemiLime);
  AddColor('CRgba8SemiFuchsia', CRgba8SemiFuchsia);
  AddColor('CRgba8SemiAqua',    CRgba8SemiAqua);
end;

procedure TAggColorManager.RemoveColor(const AName: string);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    if CompareText(string(TColorEntry(Items[Index]^).Name), AName) = 0 then
    begin
      Delete(Index);
      Break;
    end;
end;

procedure RegisterColor(const AName: string; AColor: TAggRgba8);
begin
  AggColorManager.AddColor(AName, AColor);
end;

procedure UnregisterColor(const AName: string);
begin
  AggColorManager.RemoveColor(AName);
end;


{ TAggRgba8Property }

procedure TAggRgba8Property.Edit;
var
  ColorDialog: TColorDialog;
  IniFile: TRegIniFile;

{$IFNDEF FPC}
  procedure GetCustomColors;
  begin
    if BaseRegistryKey = '' then Exit;
    IniFile := TRegIniFile.Create(BaseRegistryKey);
    try
      IniFile.ReadSectionValues(SCustomColors, ColorDialog.CustomColors);
    except
      { Ignore errors while reading values }
    end;
  end;

  procedure SaveCustomColors;
  var
    I, P: Integer;
    S: string;
  begin
    if IniFile <> nil then
      with ColorDialog do
        for I := 0 to CustomColors.Count - 1 do
        begin
          S := CustomColors.Strings[I];
          P := Pos('=', S);
          if P <> 0 then
          begin
            S := Copy(S, 1, P - 1);
            IniFile.WriteString(SCustomColors, S, CustomColors.Values[S]);
          end;
        end;
  end;
{$ENDIF}

begin
  IniFile := nil;
  ColorDialog := TColorDialog.Create(Application);
  try
{$IFNDEF FPC}
    GetCustomColors;
    ColorDialog.Options := [cdShowHelp];
{$ENDIF}
    ColorDialog.Color := WinColor(Rgb8Packed(GetOrdValue));
    ColorDialog.HelpContext := 25010;
    if ColorDialog.Execute then
      SetOrdValue(ColorToAggRgba8(ColorDialog.Color).ABGR);
{$IFNDEF FPC}
    SaveCustomColors;
{$ENDIF}
  finally
    IniFile.Free;
    ColorDialog.Free;
  end;
end;

function TAggRgba8Property.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paDialog, paValueList,
  paRevertable];
end;

procedure TAggRgba8Property.GetValues(Proc: TGetStrProc);
begin
  try
    AggColorManager.EnumColors(Proc);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

function TAggRgba8Property.GetValue: string;
var
  ColorRgba8: TAggRgba8;
begin
  try
    ColorRgba8.ABGR := GetOrdValue;
    Result := AggColorManager.GetColorName(ColorRgba8);
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

procedure TAggRgba8Property.SetValue(const Value: string);
begin
  try
    SetOrdValue(Cardinal(AggColorManager.GetColor(Value)));
    Modified;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{$IFNDEF FPC}
procedure TAggRgba8Property.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TAggRgba8Property.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  // implementation dummie to satisfy interface. Don't change default value.
end;

procedure TAggRgba8Property.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
{$ELSE}
procedure TAggRgba8Property.ListDrawValue(const Value: ansistring;
  Index: Integer; ACanvas:TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
{$ENDIF}
var
  Right: Integer;
  C: TAggRgba8;
  i, j: Integer;
  W, H: Integer;
  PixelMap: TPixelMap;
  Buffer: PAggColorRgba8;
  Agg2D: TAgg2D;
begin
  try
    Right := (ARect.Bottom - ARect.Top) + ARect.Left;
    {$IFDEF FPC}
    ACanvas.FillRect(ARect.Left, ARect.Top, Right, ARect.Bottom);
    {$ELSE}
    ACanvas.FillRect(Rect(ARect.Left, ARect.Top, Right, ARect.Bottom));
    {$ENDIF}
    PixelMap := TPixelMap.Create;
    try
      W := Right - ARect.Left - 2;
      H := ARect.Bottom - ARect.Top - 2;
      PixelMap.Build(W, H, COrgColor32);
      if Assigned(AggColorManager) then
        C := AggColorManager.GetColor(Value)
      else
        C := CRgba8White;

      if (W > 8) and (H > 8) then
      begin
        with TAgg2D.Create do
        try
          Attach(PixelMap.Buffer, W, H, -PixelMap.Stride);

          ClearAll(CRgba8White);
          for j := 0 to H - 1 do
          begin
            Buffer := PAggColorRgba8(Row[j]);
            for i := 0 to W - 1 do
            begin
              if Odd(i div 3) = Odd(j div 3) then
                Buffer^ := CRgba8DarkGray
              else
                Buffer^ := CRgba8White;
              Inc(Buffer);
            end;
          end;
          FillColor := C;
          C.A := $FF;
          LineColor := C;
          LineWidth := 3;
          RoundedRect(0, 0, W, H, 4);
        finally
          Free;
        end;

      end;
      PixelMap.Draw(ACanvas.Handle, ARect.Left + 1, ARect.Top + 1);
    finally
      PixelMap.Free;

      {$IFDEF FPC}
      inherited ListDrawValue(Value, Index, ACanvas, ARect, AState);
      {$ELSE}
      DefaultPropertyListDrawValue(Value, ACanvas,
        Rect(Right, ARect.Top, ARect.Right, ARect.Bottom), ASelected);
      {$ENDIF}
    end;
  except
    on E: Exception do ShowMessage(E.Message);
  end;
end;

{$IFNDEF FPC}
procedure TAggRgba8Property.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  if GetVisualValue <> '' then
    ListDrawValue(GetVisualValue, ACanvas, ARect, True{ASelected})
  else
    DefaultPropertyDrawValue(Self, ACanvas, ARect);
end;

procedure TAggRgba8Property.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

function TAggRgba8Property.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TAggRgba8Property.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left,
    ARect.Bottom);
end;

{$ELSE}

procedure TAggRgba8Property.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
begin
  inherited PropDrawName(ACanvas, ARect, AState);
end;

procedure TAggRgba8Property.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  AState: TPropEditDrawState);
begin
  inherited PropDrawValue(ACanvas, ARect, AState);
end;

{$ENDIF}

initialization
  AggColorManager := TAggColorManager.Create;
  AggColorManager.RegisterDefaultColors;

finalization
  AggColorManager.Free;

end.
