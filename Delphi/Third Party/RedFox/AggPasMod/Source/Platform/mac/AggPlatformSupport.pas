unit AggPlatformSupport;

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
//                                                                            //
//  Note:                                                                     //
//  I tried to retain the original structure for the Win32 platform as far    //
//  as possible. Currently, not all features are implemented but the          //
//  examples should work properly.                                            //
//  HB                                                                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  class TPlatformSupport                                                    //
//                                                                            //
//  It's not a part of the AGG library, it's just a helper class to create    //
//  interactive demo examples. Since the examples should not be too complex   //
//  this class is provided to support some very basic interactive graphical   //
//  funtionality, such as putting the rendered image to the window, simple    //
//  keyboard and mouse input, window resizing, setting the window title,      //
//  and catching the "idle" events.                                           //
//                                                                            //
//  The most popular platforms are:                                           //
//                                                                            //
//  Windows-32 API                                                            //
//  X-Window API                                                              //
//  SDL library (see http://www.libsdl.org/)                                  //
//  MacOS C/C++ API                                                           //
//                                                                            //
//  All the system dependent stuff sits in the TPlatformSpecific class.       //
//  The PlatformSupport class has just a pointer to it and it's               //
//  the responsibility of the implementation to create/delete it.             //
//  This class being defined in the implementation file can have              //
//  any platform dependent stuff such as HWND, X11 Window and so on.          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  Carbon,
  AggBasics,
  AggControl,
  AggRenderingBuffer,
  AggTransAffine,
  AggTransViewport,
  AggMacPixelMap,
  AggColorConversion;


const
  // These are flags used in method init. Not all of them are
  // applicable on different platforms, for example the win32_api
  // cannot use a hardware buffer (window_hw_buffer).
  // The implementation should simply ignore unsupported flags.
  TWindowFlag = (wfResize, wfHardwareBuffer, wfKeepAspectRatio,
    wfProcessAllKeys);
  TWindowFlags = set of TWindowFlag;

type
  // -----------------------------------------------------------pix_format_e
  // Possible formats of the rendering buffer. Initially I thought that it's
  // reasonable to create the buffer and the rendering functions in
  // accordance with the native pixel format of the system because it
  // would have no overhead for pixel format conersion.
  // But eventually I came to a conclusion that having a possibility to
  // convert pixel formats on demand is a good idea. First, it was X11 where
  // there lots of different formats and visuals and it would be great to
  // render everything in, say, RGB-24 and display it automatically without
  // any additional efforts. The second reason is to have a possibility to
  // debug Renderers for different pixel formats and colorspaces having only
  // one computer and one system.
  //
  // This stuff is not included into the basic AGG functionality because the
  // number of supported pixel formats (and/or colorspaces) can be great and
  // if one needs to add new format it would be good only to add new
  // rendering files without having to modify any existing ones (a general
  // principle of incapsulation and isolation).
  //
  // Using a particular pixel format doesn't obligatory mean the necessity
  // of software conversion. For example, win32 API can natively display
  // gray8, 15-bit RGB, 24-bit BGR, and 32-bit BGRA formats.
  // This list can be (and will be!) extended in future.
  TPixelFormat = (
    pfUndefined, // By default. No conversions are applied
    pfbw, // 1 bit per color B/W
    pfGray8, // Simple 256 level grayscale
    pfGray16, // Simple 65535 level grayscale
    pfRgb555, // 15 bit rgb. Depends on the byte ordering!
    pfRgb565, // 16 bit rgb. Depends on the byte ordering!
    pfRgbAAA, // 30 bit rgb. Depends on the byte ordering!
    pfRgbBBA, // 32 bit rgb. Depends on the byte ordering!
    pfBgrAAA, // 30 bit bgr. Depends on the byte ordering!
    pfBgrABB, // 32 bit bgr. Depends on the byte ordering!
    pfRgb24, // R-G-B, one byte per color component
    pfBgr24, // B-G-R, native win32 BMP format.
    pfRgba32, // R-G-B-A, one byte per color component
    pfArgb32, // A-R-G-B, native MAC format
    pfAbgr32, // A-B-G-R, one byte per color component
    pfBgra32, // B-G-R-A, native win32 BMP format
    pfRgb48, // R-G-B, 16 bits per color component
    pfBgr48, // B-G-R, native win32 BMP format.
    pfRgba64, // R-G-B-A, 16 bits byte per color component
    pfArgb64, // A-R-G-B, native MAC format
    pfAbgr64, // A-B-G-R, one byte per color component
    pfBgra64, // B-G-R-A, native win32 BMP format
  );

const
  // -------------------------------------------------------------input_flag_e
  // Mouse and keyboard flags. They can be different on different platforms
  // and the ways they are obtained are also different. But in any case
  // the system dependent flags should be mapped into these ones. The meaning
  // of that is as follows. For example, if kbd_ctrl is set it means that the
  // ctrl key is pressed and being held at the moment. They are also used in
  // the overridden methods such as on_mouse_move, OnMouseButtonDown,
  // on_mouse_button_dbl_click(), on_mouse_ButtonUp(), OnKey().
  // In the method on_mouse_ButtonUp() the mouse flags have different
  // meaning. They mean that the respective button is being released, but
  // the meaning of the keyboard flags remains the same.
  // There's absolut minimal set of flags is used because they'll be most
  // probably supported on different platforms. Even the mouse_right flag
  // is restricted because Mac's mice have only one button, but AFAIK
  // it can be simulated with holding a special key on the keydoard.
  TMouseKeyboardFlag = (mkfMouseLeft, mkfMouseRight, mkfShift, mkfCtrl);
  TMouseKeyboardFlags = set of TMouseKeyboardFlag;

  // --------------------------------------------------------------key_code_e
  // Keyboard codes. There's also a restricted set of codes that are most
  // probably supported on different platforms. Any platform dependent codes
  // should be converted into these ones. There're only those codes are
  // defined that cannot be represented as printable ASCII-characters.
  // All printable ASCII-set can be used in a regilar C/C++ manner:
  // ' ', 'A', '0' '+' and so on.
  // Since the clasas is used for creating very simple demo-applications
  // we don't need very rich possibilities here, just basic ones.
  // Actually the numeric key codes are taken from the SDL library, so,
  // the implementation of the SDL support does not require any mapping.
  // ASCII set. Should be supported everywhere
  Cardinal(kcBackspace = 8;
  Cardinal(kcTab = 9;
  Cardinal(kcClear = 12;
  Cardinal(kcReturn = 13;
  Cardinal(kcPause = 19;
  Cardinal(kcEscape = 27;

  // Keypad
  Cardinal(kcDelete = 127;
  Cardinal(kcPad0 = 256;
  Cardinal(kcPad1 = 257;
  Cardinal(kcPad2 = 258;
  Cardinal(kcPad3 = 259;
  Cardinal(kcPad4 = 260;
  Cardinal(kcPad5 = 261;
  Cardinal(kcPad6 = 262;
  Cardinal(kcPad7 = 263;
  Cardinal(kcPad8 = 264;
  Cardinal(kcPad9 = 265;
  Cardinal(kcPadPeriod = 266;
  Cardinal(kcPadDivide = 267;
  Cardinal(kcPadMultiply = 268;
  Cardinal(kcPadMinus = 269;
  Cardinal(kcPadPlus = 270;
  Cardinal(kcPadEnter = 271;
  Cardinal(kcPadEquals = 272;

  // Arrow-keys and stuff
  Cardinal(kcUp = 273;
  Cardinal(kcDown = 274;
  Cardinal(kcRight = 275;
  Cardinal(kcLeft = 276;
  Cardinal(kcInsert = 277;
  Cardinal(kcHome = 278;
  Cardinal(kcEnd = 279;
  Cardinal(kcPageUp = 280;
  Cardinal(kcPageDown = 281;

  // Functional keys. You'd better avoid using
  // f11...f15 in your applications if you want
  // the applications to be portable
  Cardinal(kcF1 = 282;
  Cardinal(kcF2 = 283;
  Cardinal(kcF3 = 284;
  Cardinal(kcF4 = 285;
  Cardinal(kcF5 = 286;
  Cardinal(kcF6 = 287;
  Cardinal(kcF7 = 288;
  Cardinal(kcF8 = 289;
  Cardinal(kcF9 = 290;
  Cardinal(kcF10 = 291;
  Cardinal(kcF11 = 292;
  Cardinal(kcF12 = 293;
  Cardinal(kcF13 = 294;
  Cardinal(kcF14 = 295;
  Cardinal(kcF15 = 296;

  // The possibility of using these keys is
  // very restricted. Actually it's guaranteed
  // only in win32_api and win32_sdl implementations
  Cardinal(kcNumlock = 300;
  Cardinal(kcCapslock = 301;
  Cardinal(kcScrollLock = 302;

  CMaxControl = 128;

type
  // A helper class that contains pointers to a number of controls.
  // This class is used to ease the event handling with controls.
  // The implementation should simply call the appropriate methods
  // of this class when appropriate events occure.
  TControlContainer = class
  private
    FControls: array [0..CMaxControl - 1] of TAggCustomAggControl;

    FNumControls: Cardinal;
    FCurControl: Integer;
  public
    constructor Create;

    procedure Add(C: TAggCustomAggControl);

    function InRect(X, Y: Double): Boolean;

    function OnMouseButtonDown(X, Y: Double): Boolean;
    function OnMouseButtonUp(X, Y: Double): Boolean;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;

    function SetCurrent(X, Y: Double): Boolean;

  end;

  // This class is a base one to the apllication classes. It can be used
  // as follows:
  //
  // TAggApplication = class(TPlatformSupport)
  //
  // constructor Create(bpp : Cardinal; flip_y : boolean );
  // . . .
  //
  // //override stuff . . .
  // procedure OnInit; virtual;
  // procedure OnDraw; virtual;
  // procedure OnResize(sx ,sy : int ); virtual;
  // // . . . and so on, see virtual functions
  //
  // //any your own stuff . . .
  // };
  //
  // VAR
  // app : TAggApplication;
  //
  // BEGIN
  // app.Create(pix_formatRgb24 ,true );
  // app.caption  ("AGG Example. Lion" );
  //
  // if app.init(500 ,400 ,wfResize ) then
  // app.run;
  //
  // app.Free;
  //
  // END.
  //
const
  CMaxImages = 16;

type
  TPlatformSpecific = class
  private
    FPixelFormat, FSystemPixelFormat: TPixelFormat;

    FFlipY: Boolean;
    FBitsPerPixel, FSystemBitsPerPixel: Cardinal;
    FWindow: WindowRef;

    FPixelMapWindow: TPixelMap;
    FPixelMapImage: array [0..CMaxImages - 1] of TPixelMap;

    FKeymap: array [0..255] of Cardinal;

    FLastTranslatedKey: Cardinal;

    FCurrentX, FCurrentY: Integer;

    FInputFlags: TMouseKeyboardFlags;
    FRedrawFlag: Boolean;

    FSwFreq, FSwStart: CardinalWide;
  public
    constructor Create(Format: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure CreatePixelMap(Width, Height: Cardinal; Wnd: TAggRenderingBuffer);
    procedure DisplayPixelMap(Window: WindowRef; Src: TAggRenderingBuffer);

    function Load_pmap(Fn: ShortString; Index: Cardinal;
      Dst: TAggRenderingBuffer): Boolean;
    function Save_pmap(Fn: ShortString; Index: Cardinal;
      Src: TAggRenderingBuffer): Boolean;

    function Translate(Keycode: Cardinal): Cardinal;
  end;

  TPlatformSupport = class
  private
    FSpecific: TPlatformSpecific;
    FControls: TControlContainer;

    FPixelFormat: TPixelFormat;

    FBitsPerPixel: Cardinal;

    FRenderingBufferWindow: TAggRenderingBuffer;
    FRenderingBufferImage: array [0..CMaxImages - 1] of TAggRenderingBuffer;

    FWindowFlags: TWindowFlags;
    FWaitMode, FFlipY: Boolean;
    // flip_y - true if you want to have the Y-axis flipped vertically
    FCaption: ShortString;
    FResizeMatrix: TAggTransAffine;

    FInitialWidth, FInitialHeight: Integer;

    // The following provides a very simple mechanism of doing someting
    // in background. It's not multitheading. When whait_mode is true
    // the class waits for the events and it does not ever call OnIdle().
    // When it's false it calls OnIdle() when the event queue is empty.
    // The mode can be changed anytime. This mechanism is satisfactory
    // for creation very simple animations.
    function GetWaitMode: Boolean;
    procedure SetWaitMode(WaitMode: Boolean);
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    // Setting the windows caption (title). Should be able
    // to be called at least before calling init().
    // It's perfect if they can be called anytime.
    procedure SetCaption(Cap: ShortString);

    // These 3 menthods handle working with images. The image
    // formats are the simplest ones, such as .BMP in Windows or
    // .ppm in Linux. In the applications the names of the files
    // should not have any file extensions. Method LoadImage() can
    // be called before init(), so, the application could be able
    // to determine the initial size of the window depending on
    // the size of the loaded image.
    // The argument "idx" is the number of the image 0...CMaxImages-1
    function LoadImage(Index: Cardinal; FileName: ShortString): Boolean;
    function SaveImage(Index: Cardinal; FileName: ShortString): Boolean;
    function CreateImage(Index: Cardinal; AWidth: Cardinal = 0;
      AHeight: Cardinal = 0): Boolean;

    // init() and run(). See description before the class for details.
    // The necessity of calling init() after creation is that it's
    // impossible to call the overridden virtual function (OnInit())
    // from the constructor. On the other hand it's very useful to have
    // some OnInit() event handler when the window is created but
    // not yet displayed. The RenderingBufferWindow() method (see below) is
    // accessible from OnInit().
    function Init(AWidth, AHeight, Flags: TWindowFlags): Boolean;
    function Run: Integer;
    procedure Quit;

    // The very same parameters that were used in the constructor
    function GetFormat: TPixelFormat;
    function GetFlipY: Boolean;
    function GetBitsPerPixel: Cardinal;

    // These two functions control updating of the window.
    // force_redraw() is an analog of the Win32 InvalidateRect() function.
    // Being called it sets a flag (or sends a message) which results
    // in calling OnDraw() and updating the content of the window
    // when the next event cycle comes.
    // update_window() results in just putting immediately the content
    // of the currently rendered buffer to the window without calling
    // OnDraw().
    procedure ForceRedraw;
    procedure UpdateWindow;

    // So, finally, how to draw anythig with AGG? Very simple.
    // RenderingBufferWindow() returns a reference to the main rendering
    // buffer which can be attached to any rendering class.
    // RenderingBufferImage() returns a reference to the previously created
    // or loaded image buffer (see LoadImage()). The image buffers
    // are not displayed directly, they should be copied to or
    // combined somehow with the RenderingBufferWindow(). RenderingBufferWindow() is
    // the only buffer that can be actually displayed.
    function RenderingBufferWindow: TAggRenderingBuffer;
    function RenderingBufferImage(Index: Cardinal): TAggRenderingBuffer;

    // Returns file extension used in the implemenation for the particular
    // system.
    function GetImageExtension: ShortString;

    //
    procedure CopyImageToWindow(Index: Cardinal);
    procedure CopyWindowToImage(Index: Cardinal);
    procedure CopyImageToImage(Idx_to, Idx_from: Cardinal);

    // Event handlers. They are not pure functions, so you don't have
    // to override them all.
    // In my demo applications these functions are defined inside
    // the TAggApplication class
    procedure OnInit; virtual;
    procedure OnResize(Width, Height: Integer); virtual;
    procedure OnIdle; virtual;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); virtual;

    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags); virtual;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags); virtual;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); virtual;
    procedure OnControlChange; virtual;
    procedure OnDraw; virtual;
    procedure OnPostDraw(RawHandler: Pointer); virtual;

    // Adding control elements. A control element once added will be
    // working and reacting to the mouse and keyboard events. Still, you
    // will have to render them in the OnDraw() using function
    // RenderControl() because TPlatformSupport doesn't know anything about
    // Renderers you use. The controls will be also scaled automatically
    // if they provide a proper scaling mechanism (all the controls
    // included into the basic AGG package do).
    // If you don't need a particular control to be scaled automatically
    // call Control.NoTransform() after adding.
    procedure AddControl(C: TAggCustomAggControl);

    // Auxiliary functions. SetTransAffineResizing() modifier sets up the resizing
    // matrix on the basis of the given width and height and the initial
    // width and height of the window. The implementation should simply
    // call this function every time when it catches the resizing event
    // passing in the new values of width and height of the window.
    // Nothing prevents you from "cheating" the scaling matrix if you
    // call this function from somewhere with wrong arguments.
    // SetTransAffineResizing() accessor simply returns current resizing matrix
    // which can be used to apply additional scaling of any of your
    // stuff when the window is being resized.
    // width(), height(), GetInitialWidth(), and GetInitialHeight() must be
    // clear to understand with no comments :-)
    procedure SetTransAffineResizing(AWidth, AHeight: Integer);
    function GetTransAffineResizing: TAggTransAffine;

    function GetWidth: Double;
    function GetHeight: Double;
    function GetInitialWidth: Double;
    function GetInitialHeight: Double;
    function GetWindowFlags: TWindowFlags;

    // Get raw display handler depending on the system.
    // For win32 its an HDC, for other systems it can be a pointer to some
    // structure. See the implementation files for detals.
    // It's provided "as is", so, first you should check if it's not null.
    // If it's null the raw_display_handler is not supported. Also, there's
    // no guarantee that this function is implemented, so, in some
    // implementations you may have simply an unresolved symbol when linking.
    function GetRawDisplayHandler: Pointer;

    // display message box or print the message to the console
    // (depending on implementation)
    procedure DisplayMessage(Msg: PAnsiChar);

    // Stopwatch functions. Function GetElapsedTime() returns time elapsed
    // since the latest start_timer() invocation in millisecods.
    // The resolutoin depends on the implementation.
    // In Win32 it uses QueryPerformanceFrequency() / QueryPerformanceCounter().
    procedure StartTimer;
    function GetElapsedTime: Double;

    // Get the full file name. In most cases it simply returns
    // file_name. As it's appropriate in many systems if you open
    // a file by its name without specifying the path, it tries to
    // open it in the current directory. The demos usually expect
    // all the supplementary files to be placed in the current
    // directory, that is usually coincides with the directory where
    // the the executable is. However, in some systems (BeOS) it's not so.
    // For those kinds of systems FullFileName() can help access files
    // preserving commonly used policy.
    // So, it's a good idea to use in the demos the following:
    // FILE* fd = fopen(FullFileName("some.file"), "r");
    // instead of
    // FILE* fd = fopen("some.file", "r");
    function FullFileName(File_name: ShortString): ShortString;
    function FileSource(Path, Fname: ShortString): ShortString;

    property WaitMode:Boolean read GetWaitMode write SetWaitMode;
  end;

implementation


{ TControlContainer }

constructor TControlContainer.Create;
begin
  FNumControls := 0;
  FCurControl := -1;
end;

procedure TControlContainer.Add(C: TAggCustomAggControl);
begin
  if FNumControls < CMaxControl then
  begin
    FControls[FNumControls] := C;

    Inc(FNumControls);
  end;
end;

function TControlContainer.InRect(X, Y: Double): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControls[I].InRect(X, Y) then
      begin
        Result := True;

        Exit;
      end;
end;

function TControlContainer.OnMouseButtonDown(X, Y: Double): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControls[I].OnMouseButtonDown(X, Y) then
      begin
        Result := True;

        Exit;
      end;
end;

function TControlContainer.OnMouseButtonUp(X, Y: Double): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControls[I].OnMouseButtonUp(X, Y) then
      begin
        Result := True;

        Exit;
      end;
end;

function TControlContainer.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControls[I].OnMouseMove(X, Y, ButtonFlag) then
      begin
        Result := True;

        Exit;
      end;
end;

function TControlContainer.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
begin
  Result := False;

  if FCurControl >= 0 then
    Result := FControls[FCurControl].OnArrowKeys(Left, Right, Down, Up);
end;

function TControlContainer.SetCurrent(X, Y: Double): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControls[I].InRect(X, Y) then
      begin
        if FCurControl <> I then
        begin
          FCurControl := I;

          Result := True;
        end;

        Exit;
      end;

  if FCurControl <> -1 then
  begin
    FCurControl := -1;

    Result := True;
  end;
end;


{ TPlatformSpecific }

constructor TPlatformSpecific.Create(Format: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
begin
  FPixelMapWindow := TPixelMap.Create;

  for I := 0 to CMaxImages - 1 do
    FPixelMapImage[I] := TPixelMap.Create;

  FPixelFormat := Format;
  FSystemPixelFormat := pfUndefined;

  FFlipY := FlipY;
  FBitsPerPixel := 0;
  FSystemBitsPerPixel := 0;
  FWindow := nil;

  FLastTranslatedKey := 0;

  FCurrentX := 0;
  FCurrentY := 0;

  FInputFlags := 0;
  FRedrawFlag := True;

  FillChar(FKeymap[0], SizeOf(FKeymap), 0);

  // Keyboard input is not yet fully supported nor tested
  // FKeymap[VK_PAUSE ]:=CKeyPause;
  FKeymap[KClearCharCode] := Cardinal(kcClear;

  // FKeymap[VK_NUMPAD0 ] :=key_kp0;
  // FKeymap[VK_NUMPAD1 ] :=key_kp1;
  // FKeymap[VK_NUMPAD2 ] :=key_kp2;
  // FKeymap[VK_NUMPAD3 ] :=key_kp3;
  // FKeymap[VK_NUMPAD4 ] :=key_kp4;
  // FKeymap[VK_NUMPAD5 ] :=key_kp5;
  // FKeymap[VK_NUMPAD6 ] :=key_kp6;
  // FKeymap[VK_NUMPAD7 ] :=key_kp7;
  // FKeymap[VK_NUMPAD8 ] :=key_kp8;
  // FKeymap[VK_NUMPAD9 ] :=key_kp9;
  // FKeymap[VK_DECIMAL ] :=key_kp_period;
  // FKeymap[VK_DIVIDE ]  :=key_kp_divide;
  // FKeymap[VK_MULTIPLY ]:=key_kp_multiply;
  // FKeymap[VK_SUBTRACT ]:=key_kp_minus;
  // FKeymap[VK_ADD ]     :=key_kp_plus;

  FKeymap[KUpArrowCharCode] := Cardinal(kcUp;
  FKeymap[KDownArrowCharCode] := Cardinal(kcDown;
  FKeymap[KRightArrowCharCode] := Cardinal(kcRight;
  FKeymap[KLeftArrowCharCode] := Cardinal(kcLeft;
  // FKeymap[VK_INSERT ]:=CKeyInsert;
  FKeymap[KDeleteCharCode] := Cardinal(kcDelete;
  FKeymap[KHomeCharCode] := Cardinal(kcHome;
  FKeymap[KEndCharCode] := Cardinal(kcEnd;
  FKeymap[KPageUpCharCode] := Cardinal(kcPageUp;
  FKeymap[KPageDownCharCode] := Cardinal(kcPageDown;

  // FKeymap[VK_F1 ] :=CKeyF1;
  // FKeymap[VK_F2 ] :=CKeyF2;
  // FKeymap[VK_F3 ] :=CKeyF3;
  // FKeymap[VK_F4 ] :=CKeyF4;
  // FKeymap[VK_F5 ] :=CKeyF5;
  // FKeymap[VK_F6 ] :=CKeyF6;
  // FKeymap[VK_F7 ] :=CKeyF7;
  // FKeymap[VK_F8 ] :=CKeyF8;
  // FKeymap[VK_F9 ] :=CKeyF9;
  // FKeymap[VK_F10 ]:=CKeyF10;
  // FKeymap[VK_F11 ]:=CKeyF11;
  // FKeymap[VK_F12 ]:=CKeyF12;
  // FKeymap[VK_F13 ]:=CKeyF13;
  // FKeymap[VK_F14 ]:=CKeyF14;
  // FKeymap[VK_F15 ]:=CKeyF15;

  // FKeymap[VK_NUMLOCK ]:=CKeyNumlock;
  // FKeymap[VK_CAPITAL ]:=CKeyCapslock;
  // FKeymap[VK_SCROLL ] :=CKeyScrollLock;

  case FPixelFormat of
    pfGray8:
      begin
        FSystemPixelFormat := pfRgb24;
        FBitsPerPixel := 8;
        FSystemBitsPerPixel := 24;
      end;

    pfRgb565, pfRgb555:
      begin
        FSystemPixelFormat := pfRgb555;
        FBitsPerPixel := 16;
        FSystemBitsPerPixel := 16;
      end;

    pfRgb24, pfBgr24:
      begin
        FSystemPixelFormat := pfRgb24;
        FBitsPerPixel := 24;
        FSystemBitsPerPixel := 24;
      end;

    pfBgra32, pfAbgr32, pfArgb32, pfRgba32:
      begin
        FSystemPixelFormat := pfArgb32;
        FBitsPerPixel := 32;
        FSystemBitsPerPixel := 32;
      end;
  end;

  Microseconds(FSwFreq);
  Microseconds(FSwStart);
end;

destructor TPlatformSpecific.Destroy;
var
  Index: Cardinal;
begin
  FPixelMapWindow.Free;

  for Index := 0 to CMaxImages - 1 do
    FPixelMapImage[Index].Free;

  inherited;
end;

procedure TPlatformSpecific.CreatePixelMap;
begin
  FPixelMapWindow.Create(Width, Height, FBitsPerPixel);

  if FFlipY then
    Wnd.Attach(FPixelMapWindow.GetBuffer, FPixelMapWindow.GetWidth, FPixelMapWindow.GetHeight,
      -FPixelMapWindow._row_bytes)
  else
    Wnd.Attach(FPixelMapWindow.GetBuffer, FPixelMapWindow.GetWidth, FPixelMapWindow.GetHeight,
      FPixelMapWindow._row_bytes)
end;

procedure Convert_pmap(Dst, Src: TAggRenderingBuffer; Format: TPixelFormat);
begin
  case Format of
    pfGray8:
      ColorConversion(Dst, Src, ColorConversionGray8ToRgb24);

    pfRgb565:
      ColorConversion(Dst, Src, ColorConversionRgb565ToRgb555);

    pfBgr24:
      ColorConversion(Dst, Src, ColorConversionBgr24ToRgb24);

    pfAbgr32:
      ColorConversion(Dst, Src, ColorConversionAbgr32ToArgb32);

    pfBgra32:
      ColorConversion(Dst, Src, ColorConversionBgra32ToArgb32);

    pfRgba32:
      ColorConversion(Dst, Src, ColorConversionRgba32ToArgb32);
  end;
end;

procedure TPlatformSpecific.DisplayPixelMap;
var
  Pmap_tmp: TPixelMap;
  Rbuf_tmp: TAggRenderingBuffer;
begin
  if FSystemPixelFormat = FPixelFormat then
    FPixelMapWindow.Draw(Window)

  else
  begin
    Pmap_tmp.Create;
    Pmap_tmp.Create(FPixelMapWindow.GetWidth, FPixelMapWindow.GetHeight, FSystemBitsPerPixel);

    Rbuf_tmp.Create;

    if FFlipY then
      Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
        -Pmap_tmp._row_bytes)
    else
      Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
        Pmap_tmp._row_bytes);

    Convert_pmap(@Rbuf_tmp, Src, FPixelFormat);
    Pmap_tmp.Draw(Window);

    Rbuf_tmp.Free;
    Pmap_tmp.Free;
  end;
end;

function TPlatformSpecific.Load_pmap;
var
  Pmap_tmp: TPixelMap;
  Rbuf_tmp: TAggRenderingBuffer;
begin
  Pmap_tmp.Create;

  if not Pmap_tmp.Load_from_qt(Fn) then
  begin
    Result := False;

    Pmap_tmp.Free;
    Exit;
  end;

  Rbuf_tmp.Create;

  if FFlipY then
    Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
      -Pmap_tmp._row_bytes)
  else
    Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
      Pmap_tmp._row_bytes);

  FPixelMapImage[Index].Create(Pmap_tmp.GetWidth, Pmap_tmp.GetHeight, FBitsPerPixel, 0);

  if FFlipY then
    Dst.Attach(FPixelMapImage[Index].GetBuffer, FPixelMapImage[Index].GetWidth,
      FPixelMapImage[Index].GetHeight, -FPixelMapImage[Index]._row_bytes)
  else
    Dst.Attach(FPixelMapImage[Index].GetBuffer, FPixelMapImage[Index].GetWidth,
      FPixelMapImage[Index].GetHeight, FPixelMapImage[Index]._row_bytes);

  case FPixelFormat of
    pfRgb555:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToRgb555);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToRgb555);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToRgb555);
      end;

    pfRgb565:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToRgb565);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToRgb565);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToRgb565);
      end;

    pfRgb24:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToRgb24);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToRgb24);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToRgb24);
      end;

    pfBgr24:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToBgr24);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToBgr24);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToBgr24);
      end;

    pfAbgr32:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToAbgr32);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToAbgr32);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToAbgr32);
      end;

    pfArgb32:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToArgb32);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToArgb32);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToArgb32);
      end;

    pfBgra32:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToBgra32);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToBgra32);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToBgra32);
      end;

    pfRgba32:
      case Pmap_tmp.GetBitsPerPixel of
        16:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionRgb555ToRgba32);
        24:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgr24ToRgba32);
        32:
          ColorConversion(Dst, @Rbuf_tmp, ColorConversionBgra32ToRgba32);
      end;
  end;

  Pmap_tmp.Free;
  Rbuf_tmp.Free;

  Result := True;
end;

function TPlatformSpecific.Save_pmap;
var
  Pmap_tmp: TPixelMap;
  Rbuf_tmp: TAggRenderingBuffer;
begin
  if FSystemPixelFormat = FPixelFormat then
  begin
    Result := FPixelMapImage[Index].Save_as_qt(Fn);

    Exit;
  end;

  Pmap_tmp.Create;
  Pmap_tmp.Create(FPixelMapImage[Index].GetWidth, FPixelMapImage[Index].GetHeight, FSystemBitsPerPixel);

  Rbuf_tmp.Create;

  if FFlipY then
    Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
      -Pmap_tmp._row_bytes)
  else
    Rbuf_tmp.Attach(Pmap_tmp.GetBuffer, Pmap_tmp.GetWidth, Pmap_tmp.GetHeight,
      Pmap_tmp._row_bytes);

  Convert_pmap(@Rbuf_tmp, Src, FPixelFormat);

  Result := Pmap_tmp.Save_as_qt(Fn);

  Rbuf_tmp.Free;
  Pmap_tmp.Free;
end;

function TPlatformSpecific.Translate;
begin
  if Keycode > 255 then
    FLastTranslatedKey := 0
  else
    FLastTranslatedKey := FKeymap[Keycode];
end;


{ TPlatformSupport }

constructor TPlatformSupport.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
begin
  FSpecific := TPlatformSpecific.Create(PixelFormat, FlipY));

  FControls := TControlContainer.Create;
  FRenderingBufferWindow.Create;

  for I := 0 to CMaxImages - 1 do
    FRenderingBufferImage[I].Create;

  FResizeMatrix.Create;

  FPixelFormat := PixelFormat;

  FBitsPerPixel := FSpecific.FBitsPerPixel;

  FWindowFlags := [];
  FWaitMode := True;
  FFlipY := FlipY;

  FInitialWidth := 10;
  FInitialHeight := 10;

  FCaption := 'Anti-Grain Geometry Application'#0;
end;

destructor TPlatformSupport.Destroy;
var
  I: Cardinal;
begin
  FSpecific.Free

  Controls.Free;
  FRenderingBufferWindow.Free;

  for I := 0 to CMaxImages - 1 do
    FRenderingBufferImage[I].Free;

  inherited;
end;

procedure TPlatformSupport.SetCaption(Cap: ShortString);
begin
  FCaption := Cap + #0;

  Dec(Byte(FCaption[0]));

  if FSpecific.FWindow <> nil then
    SetWindowTitleWithCFString(FSpecific.FWindow,
      CFStringCreateWithPascalStringNoCopy(nil, Cap,
      KCFStringEncodingASCII, nil));
  inherited;
end;

function TPlatformSupport.LoadImage(Index: Cardinal; FileName: ShortString): Boolean;
begin
  if Index < CMaxImages then
  begin
    FileName := FileName + GetImageExtension;
    Result := FSpecific.Load_pmap(FileName, Index, @FRenderingBufferImage[Index]);
  end
  else
    Result := True;
end;

function TPlatformSupport.SaveImage(Index: Cardinal; FileName: ShortString): Boolean;
begin
  if Index < CMaxImages then
    Result := FSpecific.Save_pmap(FileName, Index, @FRenderingBufferImage[Index])
  else
    Result := True;
end;

function TPlatformSupport.Create_img;
begin
  if Index < CMaxImages then
  begin
    if AWidth = 0 then
      AWidth := FSpecific.FPixelMapWindow.GetWidth;

    if AHeight = 0 then
      AHeight := FSpecific.FPixelMapWindow.GetHeight;

    FSpecific.FPixelMapImage[Index].Create(AWidth, AHeight, FSpecific.FBitsPerPixel);

    if FFlipY then
      FRenderingBufferImage[Index].Attach(FSpecific.FPixelMapImage[Index].GetBuffer,
        FSpecific.FPixelMapImage[Index].GetWidth, FSpecific.FPixelMapImage[Index].GetHeight,
        -FSpecific.FPixelMapImage[Index]._row_bytes)
    else
      FRenderingBufferImage[Index].Attach(FSpecific.FPixelMapImage[Index].GetBuffer,
        FSpecific.FPixelMapImage[Index].GetWidth, FSpecific.FPixelMapImage[Index].GetHeight,
        FSpecific.FPixelMapImage[Index]._row_bytes);

    Result := True;
  end
  else
    Result := False;
end;

function GetKeyFlags(Wflags: Integer): TMouseKeyboardFlags;
begin
  Result := [];

  if Wflags and ShiftKey <> 0 then
    Result := Result + [mkfShift];

  if Wflags and MK_CONTROL <> 0 then
    Result := Result + [mkfCtrl];
end;

function DoWindowClose(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
begin
  QuitApplicationEventLoop;

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoWindowDrawContent(NextHandler: EventHandlerCallRef;
  TheEvent: EventRef; UserData: Pointer): OSStatus;
var
  App: TPlatformSpecific;
begin
  App := TPlatformSpecific(UserData);

  if App <> nil then
  begin
    if App.FSpecific.FRedrawFlag then
    begin
      App.OnDraw;

      App.FSpecific.FRedrawFlag := False;
    end;

    App.FSpecific.DisplayPixelMap(App.FSpecific.FWindow, App.RenderingBufferWindow);
  end;

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoWindowResize(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  App : PTPlatformSupport;
  Rect: Carbon.Rect;

  Width, Height: Cardinal;
begin
  App := PTPlatformSupport(UserData);

  GetWindowBounds(App.FSpecific.FWindow, KWindowContentRgn, Rect);

  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;

  if (Width <> App.RenderingBufferWindow.Width) or (Height <> App.RenderingBufferWindow.Height)
  then
  begin
    App.FSpecific.CreatePixelMap(Width, Height, App.RenderingBufferWindow);
    App.SetTransAffineResizing(Width, Height);

    App.OnResize(Width, Height);
  end;

  App.ForceRedraw;

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoAppQuit(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
begin
  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoMouseDown(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  WheresMyMouse: Carbon.Point;

  Modifier: UInt32;
  Button  : EventMouseButton;

  Sz : UInt32;
  App: PTPlatformSupport;
  Ept: EventParamType;
begin
  Ept := 0;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseLocation[1])
    ^), LongWord(PInt32(@TypeQDPoint[1])^), Ept, SizeOf(Carbon.Point), Sz,
    @WheresMyMouse);

  GlobalToLocal(WheresMyMouse);
  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyModifiers[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Modifier);

  App := PTPlatformSupport(UserData);

  App.FSpecific.FCurrentX := WheresMyMouse.H;

  if App.GetFlipY then
    App.FSpecific.FCurrentY := App.RenderingBufferWindow.Height - WheresMyMouse.V
  else
    App.FSpecific.FCurrentY := WheresMyMouse.V;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseButton[1])^),
    LongWord(PInt32(@TypeMouseButton[1])^), Ept, SizeOf(EventMouseButton),
    Sz, @Button);

  case Button of
    KEventMouseButtonSecondary:
      App.FSpecific.FInputFlags := Mouse_right or GetKeyFlags(Modifier);
  else
    App.FSpecific.FInputFlags := Mouse_left or GetKeyFlags(Modifier);
  end;

  App.Controls.SetCurrent(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY);

  if App.Controls.OnMouseButtonDown(App.FSpecific.FCurrentX,
    App.FSpecific.FCurrentY) then
  begin
    App.OnControlChange;
    App.ForceRedraw;

  end
  else if App.Controls.InRect(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY)
  then
    if App.Controls.SetCurrent(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY) then
    begin
      App.OnControlChange;
      App.ForceRedraw;

    end
    else
  else
    App.OnMouseButtonDown(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY,
      App.FSpecific.FInputFlags);

  Result := CallNextEventHandler(NextHandler, TheEvent);

end;

function DoMouseUp(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  WheresMyMouse: Carbon.Point;

  Modifier: UInt32;
  Button  : EventMouseButton;

  Sz : UInt32;
  App: PTPlatformSupport;
  Ept: EventParamType;

begin
  Ept := 0;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseLocation[1])
    ^), LongWord(PInt32(@TypeQDPoint[1])^), Ept, SizeOf(Carbon.Point), Sz,
    @WheresMyMouse);

  GlobalToLocal(WheresMyMouse);

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyModifiers[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Modifier);

  App := PTPlatformSupport(UserData);

  App.FSpecific.FCurrentX := WheresMyMouse.H;

  if App.GetFlipY then
    App.FSpecific.FCurrentY := App.RenderingBufferWindow.Height - WheresMyMouse.V
  else
    App.FSpecific.FCurrentY := WheresMyMouse.V;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseButton[1])^),
    LongWord(PInt32(@TypeMouseButton[1])^), Ept, SizeOf(EventMouseButton),
    Sz, @Button);

  case Button of
    KEventMouseButtonSecondary:
      App.FSpecific.FInputFlags := Mouse_right or GetKeyFlags(Modifier);
  else
    App.FSpecific.FInputFlags := Mouse_left or GetKeyFlags(Modifier);
  end;

  if App.Controls.OnMouseButtonUp(App.FSpecific.FCurrentX,
    App.FSpecific.FCurrentY) then
  begin
    App.OnControlChange;
    App.ForceRedraw;
  end;

  App.OnMouseButtonUp(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY,
    App.FSpecific.FInputFlags);

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoMouseDragged(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  WheresMyMouse: Carbon.Point;

  Modifier: UInt32;
  Button  : EventMouseButton;

  Sz : UInt32;
  App: PTPlatformSupport;
  Ept: EventParamType;
begin
  Ept := 0;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseLocation[1])
    ^), LongWord(PInt32(@TypeQDPoint[1])^), Ept, SizeOf(Carbon.Point), Sz,
    @WheresMyMouse);

  GlobalToLocal(WheresMyMouse);
  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyModifiers[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Modifier);

  App := PTPlatformSupport(UserData);

  App.FSpecific.FCurrentX := WheresMyMouse.H;

  if App.GetFlipY then
    App.FSpecific.FCurrentY := App.RenderingBufferWindow.Height - WheresMyMouse.V
  else
    App.FSpecific.FCurrentY := WheresMyMouse.V;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamMouseButton[1])^),
    LongWord(PInt32(@TypeMouseButton[1])^), Ept, SizeOf(EventMouseButton),
    Sz, @Button);

  case Button of
    KEventMouseButtonSecondary:
      App.FSpecific.FInputFlags := Mouse_right or GetKeyFlags(Modifier);

  else
    App.FSpecific.FInputFlags := Mouse_left or GetKeyFlags(Modifier);

  end;

  if App.Controls.OnMouseMove(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY,
    App.FSpecific.FInputmkfMouseLeft in Flags) then
  begin
    App.OnControlChange;
    App.ForceRedraw;

  end
  else
    App.OnMouseMove(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY,
      App.FSpecific.FInputFlags);

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

function DoKeyDown(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  Key_char          : Byte;
  Key_code, Modifier: UInt32;

  Sz : UInt32;
  App: PTPlatformSupport;
  Ept: EventParamType;

  Left, Up, Right, Down: Boolean;

begin
  Ept := 0;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyMacCharCodes[1])
    ^), LongWord(PInt32(@TypeChar[1])^), Ept, SizeOf(Byte), Sz, @Key_char);

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyCode[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Key_code);

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyModifiers[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Modifier);

  App := PTPlatformSupport(UserData);

  App.FSpecific.FLastTranslatedKey := 0;

  case Modifier of
    ControlKey:
      App.FSpecific.FInputFlags := App.FSpecific.FInputFlags or Kbd_ctrl;

    ShiftKey:
      App.FSpecific.FInputFlags := App.FSpecific.FInputFlags or Kbd_shift;
  else
    App.FSpecific.Translate(Key_char);
  end;

  case Key_char of
    KFunctionKeyCharCode:
      case Key_code of
        122:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF1;
        120:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF2;
        99:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF3;
        118:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF4;
        96:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF5;
        97:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF6;
        98:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF7;
        100:
          App.FSpecific.FLastTranslatedKey := Cardinal(kcF8;
      end;
  end;

  if (App.FSpecific.FLastTranslatedKey = 0) and (Key_char > 31) then
    App.FSpecific.FLastTranslatedKey := Key_char;

  if App.FSpecific.FLastTranslatedKey <> 0 then
  begin
    Left := False;
    Up := False;
    Right := False;
    Down := False;

    case App.FSpecific.FLastTranslatedKey of
      Cardinal(kcLeft:
        Left := True;
      Cardinal(kcUp:
        Up := True;
      Cardinal(kcRight:
        Right := True;
      Cardinal(kcDown:
        Down := True;

      // On a Mac, screenshots are handled by the system.
      Cardinal(kcF2:
        begin
          App.CopyWindowToImage(CMaxImages - 1);
          App.SaveImage(CMaxImages - 1, 'screenshot.png');
        end;

      Cardinal(kcF4:
        if Modifier = OptionKey then
          App.Quit;

    end;

    if App.Controls.OnArrowKeys(Left, Right, Down, Up) then
    begin
      App.OnControlChange;
      App.ForceRedraw;

    end
    else
      App.OnKey(App.FSpecific.FCurrentX, App.FSpecific.FCurrentY,
        App.FSpecific.FLastTranslatedKey, App.FSpecific.FInputFlags);

  end;

  Result := CallNextEventHandler(NextHandler, TheEvent);

end;

function DoKeyUp(NextHandler: EventHandlerCallRef; TheEvent: EventRef;
  UserData: Pointer): OSStatus;
var
  Key_code: Byte;
  Modifier: UInt32;

  Sz : UInt32;
  App: PTPlatformSupport;
  Ept: EventParamType;

begin
  Ept := 0;

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyMacCharCodes[1])
    ^), LongWord(PInt32(@TypeChar[1])^), Ept, SizeOf(Byte), Sz, @Key_code);

  GetEventParameter(TheEvent, LongWord(PInt32(@KEventParamKeyModifiers[1])^),
    LongWord(PInt32(@TypeUInt32[1])^), Ept, SizeOf(UInt32), Sz, @Modifier);

  App := PTPlatformSupport(UserData);

  App.FSpecific.FLastTranslatedKey := 0;

  case Modifier of
    ControlKey:
      App.FSpecific.FInputFlags := App.FSpecific.FInputFlags and
        not Kbd_ctrl;

    ShiftKey:
      App.FSpecific.FInputFlags := App.FSpecific.FInputFlags and
        not Kbd_shift;
  end;

  Result := CallNextEventHandler(NextHandler, TheEvent);
end;

procedure DoPeriodicTask(TheTimer: EventLoopTimerRef; UserData: Pointer);
var
  App: PTPlatformSupport;
begin
  App := PTPlatformSupport(UserData);

  if not App.GetWaitMode then
    App.OnIdle;
end;

function TPlatformSupport.Init(AWidth, AHeight, Flags: TWindowFlags): Boolean;
var
  EventType  : EventTypeSpec;
  HandlerUPP : EventHandlerUPP;
  TheTarget  : CFStringRef;
  WindowAttrs: WindowAttributes;
  Bounds     : Carbon.Rect;
  MainLoop   : EventLoopRef;
  TimerUPP   : EventLoopTimerUPP;
  TheTimer   : EventLoopTimerRef;
begin
  if FSpecific.FSystemPixelFormat = pfUndefined then
  begin
    Result := False;

    Exit;
  end;

  FWindowFlags := Flags;

  // application
  TheTarget := GetApplicationEventTarget;

  EventType.EventClass := LongWord(PInt32(@KEventClassApplication[1])^);
  EventType.EventKind := KEventAppQuit;

  HandlerUPP := NewEventHandlerUPP(@DoAppQuit);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, nil, nil);

  EventType.EventClass := LongWord(PInt32(@KEventClassMouse[1])^);
  EventType.EventKind := KEventMouseDown;

  HandlerUPP := NewEventHandlerUPP(@DoMouseDown);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventMouseUp;

  HandlerUPP := NewEventHandlerUPP(@DoMouseUp);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventMouseDragged;

  HandlerUPP := NewEventHandlerUPP(@DoMouseDragged);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventClass := LongWord(PInt32(@KEventClassKeyboard[1])^);
  EventType.EventKind := KEventRawKeyDown;

  HandlerUPP := NewEventHandlerUPP(@DoKeyDown);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventRawKeyUp;

  HandlerUPP := NewEventHandlerUPP(@DoKeyUp);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventRawKeyRepeat;

  HandlerUPP := NewEventHandlerUPP(@DoKeyDown);
  // 'key repeat' is translated to 'key down'

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  // window
  WindowAttrs := KWindowCloseBoxAttribute or KWindowCollapseBoxAttribute or
    KWindowStandardHandlerAttribute;

  if wfResize if Flags then
    WindowAttrs := WindowAttrs or KWindowResizableAttribute or
      KWindowFullZoomAttribute or KWindowLiveResizeAttribute;

  SetRect(Bounds, 0, 0, AWidth, AHeight);
  OffsetRect(Bounds, 100, 100);

  CreateNewWindow(KDocumentWindowClass, WindowAttrs, Bounds,
    FSpecific.FWindow);

  if FSpecific.FWindow = nil then
  begin
    Result := False;

    Exit;
  end;

  // I assume the text is ASCII.
  // Change to kCFStringEncodingMacRoman, kCFStringEncodingISOLatin1, kCFStringEncodingUTF8 or what else you need.
  SetWindowTitleWithCFString(FSpecific.FWindow,
    CFStringCreateWithPascalStringNoCopy(nil, FCaption,
    KCFStringEncodingASCII, nil));

  TheTarget := GetWindowEventTarget(FSpecific.FWindow);

  EventType.EventClass := LongWord(PInt32(@KEventClassWindow[1])^);
  EventType.EventKind := KEventWindowClose;

  HandlerUPP := NewEventHandlerUPP(@DoWindowClose);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventWindowDrawContent;

  HandlerUPP := NewEventHandlerUPP(@DoWindowDrawContent);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  EventType.EventKind := KEventWindowBoundsChanged;

  HandlerUPP := NewEventHandlerUPP(@DoWindowResize);

  InstallEventHandler(TheTarget, HandlerUPP, 1, EventType, @Self, nil);

  // Periodic task
  // Instead of an idle function I use the Carbon event timer.
  // You may decide to change the wait value which is currently 50 milliseconds.
  MainLoop := GetMainEventLoop;
  TimerUPP := NewEventLoopTimerUPP(@DoPeriodicTask);

  InstallEventLoopTimer(MainLoop, 0, 50 * KEventDurationMillisecond, TimerUPP,
    @Self, TheTimer);

  FSpecific.CreatePixelMap(AWidth, AHeight, @FRenderingBufferWindow);

  FInitialWidth := AWidth;
  FInitialHeight := AHeight;

  OnInit;
  OnResize(AWidth, AHeight);

  FSpecific.FRedrawFlag := True;

  ShowWindow(FSpecific.FWindow);
  SetPortWindowPort(FSpecific.FWindow);

  Result := True;
end;

function TPlatformSupport.Run;
begin
  RunApplicationEventLoop;

  Result := 1;
end;

procedure TPlatformSupport.Quit;
begin
  QuitApplicationEventLoop;
end;

function TPlatformSupport.GetFormat;
begin
  Result := FPixelFormat;
end;

function TPlatformSupport.GetFlipY;
begin
  Result := FFlipY;
end;

function TPlatformSupport.GetBitsPerPixel;
begin
  Result := FBitsPerPixel;
end;

function TPlatformSupport.GetWaitMode;
begin
  Result := FWaitMode;
end;

procedure TPlatformSupport.SetWaitMode;
begin
  FWaitMode := WaitMode;
end;

procedure TPlatformSupport.ForceRedraw;
var
  Bounds: Carbon.Rect;
begin
  FSpecific.FRedrawFlag := True;

  // OnControlChange
  OnDraw;

  SetRect(Bounds, 0, 0, FRenderingBufferWindow.Width, FRenderingBufferWindow.Height);
  InvalWindowRect(FSpecific.FWindow, Bounds);
end;

procedure TPlatformSupport.UpdateWindow;
begin
  FSpecific.DisplayPixelMap(FSpecific.FWindow, @FRenderingBufferWindow);
end;

function TPlatformSupport.RenderingBufferWindow;
begin
  Result := @FRenderingBufferWindow;
end;

function TPlatformSupport.RenderingBufferImage;
begin
  Result := @FRenderingBufferImage[Index];
end;

function TPlatformSupport.GetImageExtension;
begin
  Result := '.bmp';
end;

procedure TPlatformSupport.CopyImageToWindow;
begin
  if (Index < CMaxImages) and (RenderingBufferImage(Index).GetBuffer <> nil) then
    RenderingBufferWindow.CopyFrom(RenderingBufferImage(Index));
end;

procedure TPlatformSupport.CopyWindowToImage;
begin
  if Index < CMaxImages then
  begin
    CreateImage(Index, RenderingBufferWindow.Width, RenderingBufferWindow.Height);
    RenderingBufferImage(Index).CopyFrom(RenderingBufferWindow);
  end;
end;

procedure TPlatformSupport.CopyImageToImage;
begin
  if (Idx_from < CMaxImages) and (Idx_to < CMaxImages) and
    (RenderingBufferImage(Idx_from).GetBuffer <> nil) then
  begin
    CreateImage(Idx_to, RenderingBufferImage(Idx_from).GetWidth, RenderingBufferImage(Idx_from).GetHeight);

    RenderingBufferImage(Idx_to).CopyFrom(RenderingBufferImage(Idx_from));
  end;
end;

procedure TPlatformSupport.OnInit;
begin
end;

procedure TPlatformSupport.OnResize(Width, Height: Integer);
begin
end;

procedure TPlatformSupport.OnIdle;
begin
end;

procedure TPlatformSupport.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
begin
end;

procedure TPlatformSupport.OnMouseButtonDown(X, Y: Double): Boolean;
begin
end;

procedure TPlatformSupport.OnMouseButtonUp(X, Y: Double): Boolean;
begin
end;

procedure TPlatformSupport.OnKey;
begin
end;

procedure TPlatformSupport.OnControlChange;
begin
end;

procedure TPlatformSupport.OnDraw;
begin
end;

procedure TPlatformSupport.OnPostDraw;
begin
end;

procedure TPlatformSupport.Add_ctrl;
begin
  Controls.Add(C);

  C.Transform(@FResizeMatrix);
end;

procedure TPlatformSupport.SetTransAffineResizing;
var
  Vp: TAggTransViewport;
  Ts: TAggTransAffineScaling;
begin
  if wfKeepAspectRatio in FWindowFlags then
  begin
    // double sx = double(width) / double(FInitialWidth);
    // double sy = double(height) / double(FInitialHeight);
    // if(sy < sx) sx = sy;
    // FResizeMatrix = TransAffineScaling(sx, sx);

    Vp.Create;
    Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

    Vp.DeviceViewport(0, 0, AWidth, AHeight);
    Vp.WorldViewport(0, 0, FInitialWidth, FInitialHeight);

    Vp.ToAffine(@FResizeMatrix);
  end
  else
  begin
    Ts.Create(AWidth / FInitialWidth, AHeight / FInitialHeight);

    FResizeMatrix.Assign(@Ts);
  end;
end;

function TPlatformSupport.GetTransAffineResizing;
begin
  Result := @FResizeMatrix;
end;

function TPlatformSupport.GetWidth;
begin
  Result := FRenderingBufferWindow.Width;
end;

function TPlatformSupport.GetHeight;
begin
  Result := FRenderingBufferWindow.Height;
end;

function TPlatformSupport.GetInitialWidth;
begin
  Result := FInitialWidth;
end;

function TPlatformSupport.GetInitialHeight;
begin
  Result := FInitialHeight;
end;

function TPlatformSupport.GetWindowFlags: TWindowFlags;
begin
  Result := FWindowFlags;
end;

function TPlatformSupport.GetRawDisplayHandler;
begin
end;

procedure TPlatformSupport.DisplayMessage;
var
  Dlg: DialogRef;
  Itm: DialogItemIndex;
begin
  CreateStandardAlert(KAlertPlainAlert, CFStringCreateWithCStringNoCopy(nil,
    'AGG Message', KCFStringEncodingASCII, nil),
    CFStringCreateWithCStringNoCopy(nil, Msg, KCFStringEncodingASCII, nil),
    nil, Dlg);

  RunStandardAlert(Dlg, nil, Itm);
end;

procedure TPlatformSupport.StartTimer;
begin
  Microseconds(FSpecific.FSwStart);
end;

function TPlatformSupport.ElapsedTime;
var
  Stop: CardinalWide;
begin
  Microseconds(Stop);

  Result := (Stop.Lo - FSpecific.FSwStart.Lo) * 1E6 /
    FSpecific.FSwFreq.Lo;
end;

function TPlatformSupport.FullFileName;
begin
  Result := File_name;
end;

function TPlatformSupport.FileSource;
begin
  Result := Fname;
end;

end.
