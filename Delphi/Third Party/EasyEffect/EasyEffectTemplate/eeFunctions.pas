unit eeFunctions;

interface

uses
  Windows, eeTypes, eeDetectHost, VamLib.MoreTypes;


procedure ZeroBufferX1(In1:PSingle; SampleFrames:integer); inline;
procedure ZeroBufferX2(In1,In2:PSingle; SampleFrames:integer); inline;

procedure ZeroBufferX2_DenormalNoise(In1,In2:PSingle; SampleFrames:integer); inline;


//TODO: These SizeWindow functions should be moved into another unit. Need to do that when working on Poise

  //SizeWindow_AllHosts will attempt to resize the VST whether or not the host offically supports window resizing.
function SizeWindow_AllHosts(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;

  // The host specific SizeWindow functions shouldn't be called directly. Use SizeWindow_AllHosts
function SizeWindow_CubaseSX2(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_Cubase5_A(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_Cubase5_B(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_Reaper(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_Reaper32BitBridge(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_Sonar(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
function SizeWindow_JBridge(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;



  //Makes sample count human readable.
function SamplesToTime(SampleFrames, SampleRate:single):string;

function FloatToBoolean(Value:single):boolean; inline;
function BooleanToFloat(Value:boolean):single; inline;
function ValueSplit(IndexValue:single; SplitCount:integer):integer;

function eeIntToStr(Int:integer; MinDigits:integer):string; inline;
function eeFloatToStr(Value:single; DecimalDigits:integer):string; inline;
function eeStrToFloat(Text:string):single; inline;

function RoundFloatToStr(Value : single):string;

function IntToUTF8Str(Value: Integer): UTF8String; inline;

function StrToBool_Safe(Text:string; FallBack:boolean):boolean; inline;
function StrToInt_Safe(Text:string; FallBack:integer):integer; inline;

//==============================================================
// Use the DataIO functions to convert values to and from strings for
// storing into save files. The DataIO functions ensure values are valid
// when converting back from strings.
function DataIO_BoolToStr(Value:boolean):string;
function DataIO_FloatToStr(Value:single):string;
function DataIO_IntToStr(Value:integer):string;

function DataIO_StrToBool(Value:string; FallbackValue:boolean):boolean;
function DataIO_StrToFloat(Value:string; FallbackValue:single):single;
function DataIO_StrToInt(Value:string; FallbackValue:integer):integer;
//function DataIO_UTF8StrToInt(Value:UTF8String; FallbackValue:integer):integer;

//==============================================================


function SetBit(const aByte:byte; const BitIndex:integer; const Value:boolean):byte;
function SetBits(const aByte:byte; const BitMask:AnsiString):byte;

function GetBit(const Value:byte; const BitIndex:integer):boolean;

function BinToInt(Value: string): Integer; //Binary to integer.


//==============================================================
//    String Functions
//==============================================================

function RemoveFileExt(FileName:string):string;
function AddQuotes(Text:string):string;

//==============================================================
//    String Conversion
//==============================================================
function ToPAnsiChar(stringVar : string) : PAnsiChar;
//==============================================================

function CreateGuidEx:TGUID;
function GUIDAsString : string;




//==============================================================
//    File Functions
//==============================================================

function MoveFile(ExistingFileName, NewFileName:string):longBool;
function CopyFile(ExistingFileName, NewFileName:string):longBool;


//==============================================================
//    Variable Handling
//==============================================================


 // x := StaggeredExpand(0.5, 100,200,400,800);
 // InputValue range is 0..1.
 // The x1..x4 values are the min-max scaling values.
function StaggeredExpand(InputValue, x1, x2, x3, x4 : single):single;


//==============================================================
//    Other
//==============================================================

{
type
  TObjectFunction = reference to function:TObject;

procedure ResizeObjectArray(var ObjectArray; FactoryFunc : TObjectFunction; const NewLength : integer);
var
  c1 : integer;
  Objects : array of TObject absolute ObjectArray;
begin
  for c1 := 0 to Length(Objects)-1 do
  begin
    Objects[c1].Free;
  end;

  SetLength(Objects, NewLength);

  for c1 := 0 to Length(Objects)-1 do
  begin
    Objects[c1] := FactoryFunc;
  end;
end;
}


implementation

uses
  {$IFDEF VER230}
    Vcl.Dialogs,
  {$ELSE}
    Dialogs,
  {$ENDIF}
  Math, SysUtils, uGeneralFunctions;


procedure ZeroBufferX1(In1:PSingle; SampleFrames:integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := 0;
    inc(In1);
  end;
end;

procedure ZeroBufferX2(In1,In2:PSingle; SampleFrames:integer); inline;
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := 0;
    In2^ := 0;
    inc(In1);
    inc(In2);
  end;
end;

procedure ZeroBufferX2_DenormalNoise(In1,In2:PSingle; SampleFrames:integer); inline;
const
  kDenormal = 1.0e-24;
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := kDenormal;
    In2^ := kDenormal;
    inc(In1);
    inc(In2);
  end;
end;



// function SmartVstWindowResize(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
//
// Width: New window width
// Height: New window height
// HostName + Host Version: As reported by the VST GetHostName and GetHostVendor version methods.
// EditorHandle: Handle of the editor window.
// NativeSizeWindow: Method point to the VST SizeWindow method.
//
// Depending on what host the VST is being used in, the size window method works differently. Some hosts do not support
// window resizing. For those, the VST window is resized manually. (ie brute force).
//
// Unfortunately not all hosts report their name and version correctly, therefore we need to deduce what host is being
// used by examing the GUI window parents.

function SizeWindow_AllHosts(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  DetectedHost:TVstHost;
begin
  DetectedHost := DetectVstHostFromWindow(EditorHandle);

  case DetectedHost of
    vhEnergyXT:         result := NativeSizeWindow(Width, Height);
    vhEnergyXT2:        result := NativeSizeWindow(Width, Height);
    vhTracktion3:       result := NativeSizeWindow(Width, Height);
    vhFLStudio:         result := NativeSizeWindow(Width, Height);
    vhPodium:           result := NativeSizeWindow(Width, Height);
    vhAbletonLive:      result := NativeSizeWindow(Width, Height);
    vhAcidPro6:         result := NativeSizeWindow(Width, Height);
    vhSamplitude11:     result := NativeSizeWindow(Width, Height);
    vhMultiTrackStudio: result := NativeSizeWindow(Width, Height);
    vhReaper:           result := SizeWindow_Reaper(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow);
    vhReaperBridge32:   result := SizeWindow_Reaper32BitBridge(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow);
    vhJBridge:          result := SizeWindow_JBridge(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow);
    vhCubaseSX2:        result := SizeWindow_CubaseSX2(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow);
    vhCubase5:
    begin
      //NOTE HostVersion 8200 corrosponds to Cubase 5.5 and up.
      if HostVersion < 8200
        then result := SizeWindow_Cubase5_A(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow)
        else result := SizeWindow_Cubase5_B(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow)
    end;
  else
    if HostName =  'Cakewalk VST Wizard 4.5' then
    begin
      result := SizeWindow_Sonar(Width, Height, HostName, HostVersion, EditorHandle, NativeSizeWindow);
    end else
    if HostName = 'Samplitude'  then
    begin
      //Samplitude supports window resizing but will not return TRUE, so act if it has
      // returned true anyway.
      result := true;
    end else
    begin
      result := NativeSizeWindow(Width, Height);
    end;
  end;




end;


function SizeWindow_CubaseSX2(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h, MdiClientHandle:HWND;
  xPadding:integer;
  aPoint:TPoint;
  x2,y2,cx,cy:integer;

  Flags:cardinal;
  InsertAfter:cardinal;

  EditorBounds:TRect;
  ContainerBounds:TRect;
begin

  //Get bounds of editor window.
  if GetWindowRect(EditorHandle, EditorBounds) = false then
  begin
    result := false;
    exit; //===============================================================>
  end;

  //Get bounds of the VST GUI container window.
  h := GetParent(EditorHandle);
  h := GetParent(h);

  if GetWindowRect(h, ContainerBounds) = false then
  begin
    result := false;
    exit; //===============================================================>
  end;


  //Find out how much padding there is around the editor window.
  xPadding := (ContainerBounds.Right - ContainerBounds.Left) - (EditorBounds.Right - EditorBounds.Left);


  // Calculate info for positioning the container window.
  MdiClientHandle := GetParent(h);

  aPoint.X := 0;
  aPoint.Y := 0;
  Windows.ClientToScreen(MdiClientHandle, aPoint);




  //x2 and y2 are calculated to keep the window in one position.
  x2 := ContainerBounds.Left - aPoint.X;
  y2 := ContainerBounds.Top - aPoint.Y;
  cx := Width + xPadding;
  cy := ContainerBounds.Bottom - ContainerBounds.Top;

  InsertAfter := HWND_TOP;
  Flags := SWP_NOCOPYBITS;

  SetWindowPos(h, InsertAfter, x2, y2, cx, cy, Flags);  // <-- Use this method I think.
  //MoveWindow(h, x2,y2,cx,cy,true);
  result := true;



  //== The old method. ======================

  {
  h := GetParent(EditorHandle);
  h := GetParent(h);
  MdiClientHandle := GetParent(h);

  //Get the boundary rect of the vst container window.
  r := GetWindowRect(h, aRect);
  if r = false then
  begin
    xPadding := 0;
    //yPadding := 0;
  end else
  begin
    aWidth   := aRect.Right - aRect.Left;
    //aHeight  := aRect.Bottom - aRect.Top;
    xPadding := aWidth - MasterEffect^.WindowWidth;
    //yPadding := aHeight - MasterEffect^.WindowHeight;
  end;

  //Some info for positioning the container window.
  aPoint.X := 0;
  aPoint.Y := 0;
  Windows.ClientToScreen(MdiClientHandle, aPoint);


  //Resize ourselves/make appropiate things visible.
  ToggleBrowserVisibility;


  //Attempt to change the Vst container window size.
  r := GetWindowRect(h, aRect);
  if r = false then
  begin
    ShowMessage('Could not resize VST window.');
  end else
  begin
    //x2 and y2 are calculated to keep the window in one position.
    x2 := aRect.Left - aPoint.X;
    y2 := aRect.Top - aPoint.Y;
    cx := MasterEffect^.WindowWidth + xPadding;
    cy := aRect.Bottom - aRect.Top;

    //InsertAfter := HWND_TOP;
    //Flags := SWP_NOCOPYBITS;

    //SetWindowPos(h, InsertAfter, x2, y2, cx, cy, Flags);
    MoveWindow(h, x2,y2,cx,cy,true);
  end;
  }
end;


// SizeWindow_Cubase5_A() is used with versions of Cubase 5 prior to the 5.5 update.
function SizeWindow_Cubase5_A(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h, MdiClientHandle:HWND;
  xPadding, yPadding:integer;
  aPoint:TPoint;
  x2,y2,cx,cy:integer;
  EditorBounds:TRect;
  ContainerBounds:TRect;
begin
  h := GetParent(EditorHandle);
  h := GetParent(h);
  MdiClientHandle := GetParent(h);


  //Get boundry of editor.
  if GetWindowRect(EditorHandle, EditorBounds) = false then
  begin
    result := false;
    exit; //=================================================================>
  end;

  //Get the boundary of vst container window.
  if GetWindowRect(h, ContainerBounds) = false then
  begin
    result := false;
    exit; //=================================================================>
  end;

  xPadding := (ContainerBounds.Right  - ContainerBounds.Left) - (EditorBounds.Right  - EditorBounds.Left);
  yPadding := (ContainerBounds.Bottom - ContainerBounds.Top)  - (EditorBounds.Bottom - EditorBounds.Top);


  //Some info for positioning the container window.
  aPoint.X := 0;
  aPoint.Y := 0;
  Windows.ClientToScreen(MdiClientHandle, aPoint);

  //x2 and y2 are calculated to keep the window in one position.
  //x2 := aRect.Left - aPoint.X;
  //y2 := aRect.Top - aPoint.Y;
  x2 := ContainerBounds.Left;
  y2 := ContainerBounds.Top;
  cx := Width  + xPadding;
  cy := Height + yPadding;

  //InsertAfter := HWND_TOP;
  //Flags := SWP_NOCOPYBITS;

  //SetWindowPos(h, InsertAfter, x2, y2, cx, cy, Flags);
  MoveWindow(h, x2,y2,cx,cy,true);
  result := true;
end;


// SizeWindow_Cubase5_B() is used with Cubase 5.5.
function SizeWindow_Cubase5_B(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h : HWND;
  xPadding, yPadding:integer;
  x2,y2,cx,cy:integer;
  EditorBounds:TRect;
  ContainerBounds:TRect;
begin
  // NOTE: This method resizes the outermost VST window. As such it needs to take into account the
  // window chrome that cubase wraps around the VST GUI. Currently the extra space is calcuated by
  // comparing the current GUI size with the intended GUI size. This causes problem if the current
  // GUI size is not already correct for some reason. Instead of calculating window chrome from
  // the current GUI size, it may be worthwhile stepping up one window level to use a reference while
  // calculating window chrome size.

  h := GetParent(EditorHandle);
  h := GetParent(h);

  //Get boundry of editor.
  if GetWindowRect(EditorHandle, EditorBounds) = false then
  begin
    result := false;
    exit; //=================================================================>
  end;

  //Get the boundary of vst container window.
  if GetWindowRect(h, ContainerBounds) = false then
  begin
    result := false;
    exit; //=================================================================>
  end;

  xPadding := (ContainerBounds.Right  - ContainerBounds.Left) - (EditorBounds.Right  - EditorBounds.Left);
  yPadding := (ContainerBounds.Bottom - ContainerBounds.Top)  - (EditorBounds.Bottom - EditorBounds.Top);

  //x2 and y2 are calculated to keep the window in one position.
  x2 := ContainerBounds.Left;
  y2 := ContainerBounds.Top;
  cx := Width  + xPadding;
  cy := Height + yPadding;

  MoveWindow(h, x2,y2,cx,cy,true);
  result := true;
end;

function SizeWindow_Reaper(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h:HWND;
  aRect:TRect;
  x2,y2,cx,cy:integer;
  xDelta,yDelta:integer;
begin
  if HostVersion >= 3000 then
  begin
    //result := NativeSizeWindow(Width, Height);
    NativeSizeWindow(Width, Height);

    //NOTE: Override the result to return TRUE. Even though Reaper reports it doesn't support changing window
    //size, it does. :/  this fits in with Reapers philsophy that it should support the bare minimum of the VST spec.
    result := true;
    exit; //========================================================>
  end;

  if HostVersion < 3000 then
  begin
    //h := self.Handle;
    h := GetParent(EditorHandle);
    h := GetParent(h);

    if GetWindowRect(h, aRect) then
    begin
      xDelta := (aRect.Right - aRect.Left) - Width;
      yDelta := (aRect.Bottom - aRect.Top) - Height;

      x2 := aRect.Left;
      y2 := aRect.Top;
      cx := aRect.Right + xDelta -x2;
      cy := aRect.Bottom + yDelta -y2;

      MoveWindow(h,x2,y2,cx,cy,true);

      result := true;
    end else
    begin
      result := false;
    end;

    exit; //========================================================>
  end;

  result := false;
end;

function SizeWindow_Reaper32BitBridge(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h:HWND;
  wRect:TRect; //window rect
  cRect:TRect; //client area rect.
  x2,y2,cx,cy:integer;
  BoundingWidth,BoundingHeight:integer;
begin
    h := GetParent(EditorHandle);
    //h := GetParent(h);
    if (GetWindowRect(h, wRect)) and (GetClientRect(h, cRect)) then
    begin
      BoundingWidth  := (wRect.Right  - wRect.Left) - (cRect.Right  - cRect.Left);
      BoundingHeight := (wRect.Bottom - wRect.Top)  - (cRect.Bottom - cRect.Top);

      x2 := wRect.Left;
      y2 := wRect.Top;
      cx := Width  + BoundingWidth;
      cy := Height + BoundingHeight;

      MoveWindow(h,x2,y2,cx,cy,true);

      result := true;
    end else
    begin
      result := false;
    end;
end;

function SizeWindow_Sonar(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h:HWND;
  wRect:TRect; //window rect
  cRect:TRect; //client area rect.
  x2,y2,cx,cy:integer;
  BoundingWidth,BoundingHeight:integer;
begin
  //===================================================================
  // Resize the outer container
  //===================================================================
  h := GetParent(EditorHandle);
  h := GetParent(h);
  h := GetParent(h);
  h := GetParent(h);

  if (h <> 0) and (GetWindowRect(h, wRect)) and (GetClientRect(h, cRect)) then
  begin
    BoundingWidth  := (wRect.Right  - wRect.Left) - (cRect.Right  - cRect.Left);
    BoundingHeight := (wRect.Bottom - wRect.Top)  - (cRect.Bottom - cRect.Top);

    x2 := wRect.Left;
    y2 := wRect.Top;
    cx := Width  + BoundingWidth;
    cy := Height + BoundingHeight;

    MoveWindow(h,x2,y2,cx,cy,true);

    result := true;
  end else
  begin
    result := false;
  end;


  //===================================================================
  // Resize the inner container
  //===================================================================
  h := GetParent(EditorHandle);
  if (h <> 0) then
  begin
    x2 := 0;
    y2 := 0;
    cx := Width;
    cy := Height;

    MoveWindow(h,x2,y2,cx,cy,true);

    result := true;
  end

end;

function SizeWindow_JBridge(Width, Height:LongInt; HostName:string; HostVersion:integer; EditorHandle:HWND; NativeSizeWindow:TSizeWindowMethod):boolean;
var
  h:HWND;
  wRect:TRect; //window rect
  cRect:TRect; //client area rect.
  //x2,y2,cx,cy:integer;
begin
    h := GetParent(EditorHandle);
    h := GetParent(h);
    h := GetParent(h);
    if (GetWindowRect(h, wRect)) and (GetClientRect(h, cRect)) then
    begin
      //x2 := 0;
      //y2 := 0;
      //cx := Width;
      //cy := Height;

      //Currently disabled because it doesn't work properly.
      //MoveWindow(h,x2,y2,cx,cy,true);

      result := true;
    end else
    begin
      result := false;
    end;
end;



function SamplesToTime(SampleFrames, SampleRate:single):string;
var
  x:integer;
begin
  x := round(SampleFrames / SampleRate * 1000);

  //================================================================
  if x < 1000
    then result := IntToStr(x) + ' ms'
  else
  if (x >= 1000) and (x < 10000)
    then result := FloatToStrF(x/1000, ffNumber, 3, 2) + ' sec'
  else
  if (x >= 10000) and (x < 60000)
    then result := IntToStr(round(x / 1000)) + ' sec'
  else
    result := IntToStr(Floor(x / 60000)) + ':' + IntToStrB(round((x mod 60000) / 1000),2) + ' min'
  //================================================================
end;


function FloatToBoolean(Value:single):boolean; inline;
begin
  result := (Value >= 0.5);  
end;

function BooleanToFloat(Value:boolean):single; inline;
begin
  if Value
    then result := 1
    else result := 0;
end;

function ValueSplit(IndexValue:single; SplitCount:integer):integer;
var
  x:integer;
begin
  assert(IndexValue >= 0);
  assert(IndexValue <= 1);
  x := floor(IndexValue * SplitCount);
  if x = SplitCount then x := x-1;
  result := x;  
end;

function eeFloatToStr(Value:single; DecimalDigits:integer):string; inline;
var
  x1,x2:integer;
begin
  if DecimalDigits = 0 then
  begin
    result := IntToStr(Round(Value));
  end else
  begin
    x1 := floor(abs(Value));
    x2 := floor(abs((Power(10, DecimalDigits) * (abs(Value)-x1))));
    if Value >= 0
      then result :=       IntToStr(x1) + '.' + eeIntToStr(x2, DecimalDigits)
      else result := '-' + IntToStr(x1) + '.' + eeIntToStr(x2, DecimalDigits)
  end;
end;

function IntToUTF8Str(Value: Integer): UTF8String; inline;
begin
  result := UTF8String(IntToStr(Value));
end;

function RoundFloatToStr(Value : single):string;
begin
  result := IntToStr(Round(Value));
end;

function eeStrToFloat(Text:string):single; inline;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  result := StrToFloat(Text, fs);
end;

function eeIntToStr(Int:integer; MinDigits:integer):string;
begin
  result := IntToStr(Int);
  while Length(result) < MinDigits do result := '0' + result;
end;

function StrToBool_Safe(Text:string; FallBack:boolean):boolean; inline;
begin
  try
    result := StrToBool(Text);
  except
    result := FallBack;
  end;
end;

function StrToInt_Safe(Text:string; FallBack:integer):integer; inline;
begin
  try
    result := StrToInt(Text);
  except
    result := FallBack;
  end;
end;



function DataIO_BoolToStr(Value:boolean):string;
begin
  if Value
    then result := 'true'
    else result := 'false';
end;

function DataIO_FloatToStr(Value:single):string;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  result := FloatToStr(Value, fs);
end;

function DataIO_IntToStr(Value:integer):string;
begin
  result := IntToStr(Value);
end;





function DataIO_StrToBool(Value:string; FallbackValue:boolean):boolean;
begin
  Value := Trim(Value);
  try
    if SameText(Value, '1')     then exit(true);
    if SameText(Value, 'true')  then exit(true);
    if SameText(Value, 'T')     then exit(true);
    if SameText(Value, 'Y')     then exit(true);
    if SameText(Value, 'Yes')   then exit(true);

    if SameText(Value, '0')     then exit(false);
    if SameText(Value, 'false') then exit(false);
    if SameText(Value, 'F')     then exit(false);
    if SameText(Value, 'N')     then exit(false);
    if SameText(Value, 'No')    then exit(false);

    if Value = '' then exit(false);

    //if we've made it this far, exit with the fall back value.
    result := FallBackValue;
  except
    result := FallBackValue;
  end;
end;

function DataIO_StrToFloat(Value:string; FallbackValue:single):single;
var
  fs:TFormatSettings;
begin
  fs.ThousandSeparator := ',';
  fs.DecimalSeparator  := '.';
  try
    result := StrToFloat(Value, fs)
  except
    result := FallBackValue;
  end;
end;

function DataIO_StrToInt(Value:string; FallbackValue:integer):integer;
begin
  try
    result := StrToInt(Value)
  except
    result := FallBackValue;
  end;
end;

{
function DataIO_UTF8StrToInt(Value:UTF8String; FallbackValue:integer):integer;
begin
  try
    result := StrToInt(String(Value))
  except
    result := FallBackValue;
  end;
end;
}

function SetBit(const aByte:byte; const BitIndex:integer; const Value:boolean):byte;
begin
  if Value
    then result := aByte or (1 shr BitIndex)
    else result := aByte and not (1 shr BitIndex);
end;

function GetBit(const Value:byte; const BitIndex:integer):boolean;
begin
  Result := (Value and (1 shl BitIndex)) <> 0;
end;


function SetBits(const aByte:byte; const BitMask:AnsiString):byte;
var
  s: AnsiString;
  StringLength : integer;
  c1: Integer;
  x : byte;
  Mask : byte;
begin
  StringLength := Length(BitMask);
  assert(StringLength = 8);

  x := aByte;

  for c1 := 0 to StringLength-1 do
  begin
    s := BitMask[8-c1];
    if s = '1' then
    begin
      Mask := (1 shl c1);
      x := x or Mask;
    end;

    if s = '0' then
    begin
      Mask := (1 shl c1);
      Mask := not Mask;
      x := x and Mask;
    end;
    //All other letters are ignored.
  end;

  result := x;
end;

function BinToInt(Value: string): Integer;
var
  i, iValueSize: Integer;
begin
  Result := 0;
  iValueSize := Length(Value);
  for i := iValueSize downto 1 do
    if Value[i] = '1' then Result := Result + (1 shl (iValueSize - i));
end;

function RemoveFileExt(FileName:string):string;
var
  Ext:string;
  Index:integer;
  cc:integer;
begin
  FileName := ExtractFileName(FileName); //Remove path information, if there is any.

  Ext := ExtractFileExt(FileName);

  if Ext ='' then
  begin
    //There is no extension.
    result := FileName;
    exit; //==================================================>
  end;

  cc := Length(Ext);
  Index := Pos(Ext,FileName);

  Delete(FileName,Index,cc);

  result := FileName;
end;

function AddQuotes(Text:string):string;
begin
  result := '"' + Text + '"';
end;


function ToPAnsiChar(stringVar : string) : PAnsiChar;
// This function converts a string to a PAnsiChar
// If the output is not the same, an exception is raised
// Author: nogabel@hotmail.com
// http://stackoverflow.com/a/614720/395461
var
  AnsString : AnsiString;
  InternalError : Boolean;
begin
  InternalError := false;
  result := '';
  try
    if stringVar <> '' Then
    begin
       AnsString := AnsiString(StringVar);
       result := PAnsiChar(PAnsiString(AnsString));
    end;
  except
    InternalError := true;
  end;
  if InternalError or (string(result) <> stringVar) then
  begin
    raise Exception.Create('Conversion from string to PAnsiChar failed!');
  end;
end;


function CreateGuidEx:TGUID;
begin
  if CreateGuid(result) <> S_OK then raise Exception.Create('Couldn''t create GUID.');
end;

function GUIDAsString : string;
var
  aGUID : TGUID;
begin
  aGUID := CreateGuidEx;
  result := GuidToString(aGuid);
end;

function MoveFile(ExistingFileName, NewFileName:string):longBool;
begin
  result := Windows.MoveFile(@(ExistingFileName[1]), @(NewFileName[1]));
end;

function CopyFile(ExistingFileName, NewFileName:string):longBool;
begin
  if ExistingFileName = NewFileName then
  begin
    result := true;
    exit;
  end;

  result := Windows.CopyFile(@(ExistingFileName[1]), @(NewFileName[1]), false);
end;

function StaggeredExpand(InputValue, x1, x2, x3, x4 : single):single;
var
  Index : integer;
begin
  assert(InputValue >= 0);
  assert(InputValue <= 1);

  Index := floor(InputValue * 3);

  case Index of
  0: result := x1 + (InputValue - 0/3) * 3 * (x2 - x1);
  1: result := x2 + (InputValue - 1/3) * 3 * (x3 - x2);
  2: result := x3 + (InputValue - 2/3) * 3 * (x4 - x3);
  3: result := x4;
  else
    raise Exception.Create('Unexpected Index Value.');
  end;

  assert(result >= x1);
  assert(result <= x4);
end;


end.


