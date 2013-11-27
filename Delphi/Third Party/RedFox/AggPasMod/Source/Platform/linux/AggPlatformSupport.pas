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
// class TPlatformSupport                                                     //
//                                                                            //
// It's not a part of the AGG library, it's just a helper class to create     //
// interactive demo examples. Since the examples should not be too complex    //
// this class is provided to support some very basic interactive graphical    //
// funtionality, such as putting the rendered image to the window, simple     //
// keyboard and mouse input, window resizing, setting the window title,       //
// and catching the "idle" events.                                            //
//                                                                            //
// The most popular platforms are:                                            //
//                                                                            //
// Windows-32 API                                                             //
// X-Window API                                                               //
// SDL library (see http://www.libsdl.org/)                                   //
// MacOS C/C++ API                                                            //
//                                                                            //
// All the system dependent stuff sits in the TPlatformSpecific class.        //
// The TPlatformSupport class has just a pointer to it and it's               //
// the responsibility of the implementation to create/delete it.              //
// This class being defined in the implementation file can have               //
// any platform dependent stuff such as HWND, X11 Window and so on.           //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$I- }

uses
  X, Xlib, Xutil, Xatom, Keysym, Libc, CTypes, SysUtils,
  AggBasics,
  AggControl,
  AggRenderingBuffer,
  AggTransAffine,
  AggTransViewport,
  AggColorConversion,
  AggFileUtils;

type
  // -----------------------------------------------------------------------
  // These are flags used in method init. Not all of them are
  // applicable on different platforms, for example the win32_api
  // cannot use a hardware buffer (window_hw_buffer).
  // The implementation should simply ignore unsupported flags.
  TWindowFlag = (wfResize, wfHardwareBuffer, wfKeepAspectRatio,
    wfProcessAllKeys)
  TWindowFlags = set of TWindowFlag;

  // -----------------------------------------------------------------------
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
  // the overridden methods such as on_mouse_move(), on_mouse_ButtonDown(),
  // on_mouse_button_dbl_click(), on_mouse_ButtonUp(), OnKey().
  // In the method on_mouse_ButtonUp() the mouse flags have different
  // meaning. They mean that the respective button is being released, but
  // the meaning of the keyboard flags remains the same.
  // There's absolut minimal set of flags is used because they'll be most
  // probably supported on different platforms. Even the mouse_right flag
  // is restricted because Mac's mice have only one button, but AFAIK
  // it can be simulated with holding a special key on the keydoard.
  Mouse_left = 1;
  Mouse_right = 2;
  Kbd_shift = 4;
  Kbd_ctrl = 8;

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
    FControl: array [0..CMaxControl - 1] of TAggCustomAggControl;

    FNumControls: Cardinal;
    FCurrentControl: Integer;
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
  // constructor Create(bpp : Cardinal; FlipY : boolean );
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
    FPixelFormat, FSystemFormat: TPixelFormat;
    FByteOrder: Integer;

    FFlipY: Boolean;
    FBitsPerPixel, FSystemBitsPerPixel: Cardinal;
    FDisplay: PDisplay;
    FScreen, FDepth: Integer;
    FVisual: PVisual;
    FWindow: TWindow;
    FGraphicContext: TGC;

    FWindowAttributes: TXSetWindowAttributes;

    FXImageWindow: PXImage;
    FCloseAtom: TAtom;
    FBufferWindow: Pointer;
    FBufferAlloc: Cardinal;
    FBufferImage: array [0..CMaxImages - 1] of Pointer;
    FImageAlloc: array [0..CMaxImages - 1] of Cardinal;

    FKeymap: array [0..255] of Cardinal;

    FUpdateFlag, FResizeFlag, FInitialized: Boolean;

    // FWaitMode : boolean;
    FSwStart: Clock_t;
  public
    constructor Create(Format: TPixelFormat; FlipY: Boolean);

    procedure SetCaption(Capt: PAnsiChar);
    procedure Put_image(Src: TAggRenderingBuffer);
  end;

  TPlatformSupport = class
  private
    FSpecific: TPlatformSpecific;
    FControls: TControlContainer;

    FPixelFormat: TPixelFormat;

    FBitsPerPixel: Cardinal;

    FRenderingBufferWindow: TAggRenderingBuffer;
    FRenderingBufferImage: array [0..CMaxImages - 1] of TAggRenderingBuffer;

    FWindowFlags: TWindowFlag;
    FWaitMode, FFlipY: Boolean;
    // FlipY - true if you want to have the Y-axis flipped vertically
    FCaption: ShortString;
    FResizeMatrix: TAggTransAffine;

    FInitialWidth, GetInitialHeight: Integer;

    FQuit: Boolean;
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
    function LoadImage(Index: Cardinal; File_: ShortString): Boolean;
    function SaveImage(Index: Cardinal; File_: ShortString): Boolean;
    function CreateImage(Index: Cardinal; AWidth: Cardinal = 0;
      AHeight: Cardinal = 0): Boolean;

    // init() and run(). See description before the class for details.
    // The necessity of calling init() after creation is that it's
    // impossible to call the overridden virtual function (OnInit())
    // from the constructor. On the other hand it's very useful to have
    // some OnInit() event handler when the window is created but
    // not yet displayed. The RenderingBufferWindow() method (see below) is
    // accessible from OnInit().
    function Init(AWidth, AHeight: Cardinal; Flags: TWindowFlag): Boolean;
    function Run: Integer;
    procedure Quit;

    // The very same parameters that were used in the constructor
    function GetPixelFormat: TPixelFormat;
    function GetFlipY: Boolean;
    function GetBitsPerPixel: Cardinal;

    // The following provides a very simple mechanism of doing someting
    // in background. It's not multitheading. When whait_mode is true
    // the class waits for the events and it does not ever call OnIdle().
    // When it's false it calls OnIdle() when the event queue is empty.
    // The mode can be changed anytime. This mechanism is satisfactory
    // for creation very simple animations.
    function GetWaitMode: Boolean;
    procedure SetWaitMode(WaitMode: Boolean);

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
    procedure CopyImageToImage(IndexTo, IndexFrom: Cardinal);

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
    // call Control.NoTransform after adding.
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
    // width(), height(), initial_width(), and initial_height() must be
    // clear to understand with no comments :-)
    procedure SetTransAffineResizing(AWidth, AHeight: Integer);
    function GetTransAffineResizing: TAggTransAffine;

    function GetWidth: Double;
    function GetHeight: Double;
    function GetInitialWidth: Double;
    function GetInitialHeight: Double;
    function GetWindowFlags: Cardinal;

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
    // FileName. As it's appropriate in many systems if you open
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
    function FullFileName(FileName: ShortString): ShortString;
    function FileSource(Path, FName: ShortString): ShortString;

  end;

implementation

{ TControlContainer }

constructor TControlContainer.Create;
begin
  FNumControls := 0;
  FCurrentControl := -1;
end;

procedure TControlContainer.Add;
begin
  if FNumControls < CMaxControl then
  begin
    FControl[FNumControls] := C;

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
      if FControl[I].InRect(X, Y) then
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
      if FControl[I].OnMouseButtonDown(X, Y) then
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
      if FControl[I].OnMouseButtonUp(X, Y) then
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
      if FControl[I].OnMouseMove(X, Y, ButtonFlag) then
      begin
        Result := True;

        Exit;
      end;
end;

function TControlContainer.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
begin
  Result := False;

  if FCurrentControl >= 0 then
    Result := FControl[FCurrentControl].OnArrowKeys(Left, Right, Down, Up);
end;

function TControlContainer.SetCurrent;
var
  I: Cardinal;
begin
  Result := False;

  if FNumControls > 0 then
    for I := 0 to FNumControls - 1 do
      if FControl[I].InRect(X, Y) then
      begin
        if FCurrentControl <> I then
        begin
          FCurrentControl := I;

          Result := True;
        end;

        Exit;
      end;

  if FCurrentControl <> -1 then
  begin
    FCurrentControl := -1;

    Result := True;
  end;
end;


{ TPlatformSpecific }

constructor TPlatformSpecific.Create(Format: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
begin
  FPixelFormat := Format;
  FSystemFormat := pfUndefined;
  FByteOrder := LSBFirst;
  FFlipY := FlipY;

  FBitsPerPixel := 0;
  FSystemBitsPerPixel := 0;
  FDisplay := nil;
  FScreen := 0;
  FDepth := 0;
  FVisual := nil;
  FWindow := 0;
  FGraphicContext := nil;

  FXImageWindow := nil;
  FCloseAtom := 0;
  FBufferWindow := nil;
  FBufferAlloc := 0;

  FUpdateFlag := True;
  FResizeFlag := True;
  FInitialized := False;
  // FWaitMode:=true;

  FillChar(FBufferImage[0], SizeOf(FBufferImage), 0);

  for I := 0 to 255 do
    FKeymap[I] := I;

  FKeymap[XK_Pause and $FF] := Cardinal(kcPause;
  FKeymap[XK_Clear and $FF] := Cardinal(kcClear;

  FKeymap[XK_KP_0 and $FF] := Cardinal(kcPad0;
  FKeymap[XK_KP_1 and $FF] := Cardinal(kcPad1;
  FKeymap[XK_KP_2 and $FF] := Cardinal(kcPad2;
  FKeymap[XK_KP_3 and $FF] := Cardinal(kcPad3;
  FKeymap[XK_KP_4 and $FF] := Cardinal(kcPad4;
  FKeymap[XK_KP_5 and $FF] := Cardinal(kcPad5;
  FKeymap[XK_KP_6 and $FF] := Cardinal(kcPad6;
  FKeymap[XK_KP_7 and $FF] := Cardinal(kcPad7;
  FKeymap[XK_KP_8 and $FF] := Cardinal(kcPad8;
  FKeymap[XK_KP_9 and $FF] := Cardinal(kcPad9;

  FKeymap[XK_KP_Insert and $FF] := Cardinal(kcPad0;
  FKeymap[XK_KP_End and $FF] := Cardinal(kcPad1;
  FKeymap[XK_KP_Down and $FF] := Cardinal(kcPad2;
  FKeymap[XK_KP_Page_Down and $FF] := Cardinal(kcPad3;
  FKeymap[XK_KP_Left and $FF] := Cardinal(kcPad4;
  FKeymap[XK_KP_Begin and $FF] := Cardinal(kcPad5;
  FKeymap[XK_KP_Right and $FF] := Cardinal(kcPad6;
  FKeymap[XK_KP_Home and $FF] := Cardinal(kcPad7;
  FKeymap[XK_KP_Up and $FF] := Cardinal(kcPad8;
  FKeymap[XK_KP_Page_Up and $FF] := Cardinal(kcPad9;
  FKeymap[XK_KP_Delete and $FF] := Cardinal(kcPadPeriod;
  FKeymap[XK_KP_Decimal and $FF] := Cardinal(kcPadPeriod;
  FKeymap[XK_KP_Divide and $FF] := Cardinal(kcPadDivide;
  FKeymap[XK_KP_Multiply and $FF] := Cardinal(kcPadMultiply;
  FKeymap[XK_KP_Subtract and $FF] := Cardinal(kcPadMinus;
  FKeymap[XK_KP_Add and $FF] := Cardinal(kcPadPlus;
  FKeymap[XK_KP_Enter and $FF] := Cardinal(kcPadEnter;
  FKeymap[XK_KP_Equal and $FF] := Cardinal(kcPadEquals;

  FKeymap[XK_Up and $FF] := Cardinal(kcUp;
  FKeymap[XK_Down and $FF] := Cardinal(kcDown;
  FKeymap[XK_Right and $FF] := Cardinal(kcRight;
  FKeymap[XK_Left and $FF] := Cardinal(kcLeft;
  FKeymap[XK_Insert and $FF] := Cardinal(kcInsert;
  FKeymap[XK_Home and $FF] := Cardinal(kcDelete;
  FKeymap[XK_End and $FF] := Cardinal(kcEnd;
  FKeymap[XK_Page_Up and $FF] := Cardinal(kcPageUp;
  FKeymap[XK_Page_Down and $FF] := Cardinal(kcPageDown;

  FKeymap[XK_F1 and $FF] := Cardinal(kcF1;
  FKeymap[XK_F2 and $FF] := Cardinal(kcF2;
  FKeymap[XK_F3 and $FF] := Cardinal(kcF3;
  FKeymap[XK_F4 and $FF] := Cardinal(kcF4;
  FKeymap[XK_F5 and $FF] := Cardinal(kcF5;
  FKeymap[XK_F6 and $FF] := Cardinal(kcF6;
  FKeymap[XK_F7 and $FF] := Cardinal(kcF7;
  FKeymap[XK_F8 and $FF] := Cardinal(kcF8;
  FKeymap[XK_F9 and $FF] := Cardinal(kcF9;
  FKeymap[XK_F10 and $FF] := Cardinal(kcF10;
  FKeymap[XK_F11 and $FF] := Cardinal(kcF11;
  FKeymap[XK_F12 and $FF] := Cardinal(kcF12;
  FKeymap[XK_F13 and $FF] := Cardinal(kcF13;
  FKeymap[XK_F14 and $FF] := Cardinal(kcF14;
  FKeymap[XK_F15 and $FF] := Cardinal(kcF15;

  FKeymap[XK_Num_Lock and $FF] := Cardinal(kcNumlock;
  FKeymap[XK_Caps_Lock and $FF] := Cardinal(kcCapslock;
  FKeymap[XK_Scroll_Lock and $FF] := Cardinal(kcScrollLock;

  case FPixelFormat of
    pfGray8:
      FBitsPerPixel := 8;

    pfRgb565, pfRgb555:
      FBitsPerPixel := 16;

    pfRgb24, pfBgr24:
      FBitsPerPixel := 24;

    pfBgra32, pfAbgr32, pfArgb32, pfRgba32:
      FBitsPerPixel := 32;

  end;

  FSwStart := Clock;
end;

procedure TPlatformSpecific.SetCaption;
var
  Tp: TXTextProperty;
begin
  Tp.Value := PCUChar(@Capt[1]);
  Tp.Encoding := XA_WM_NAME;
  Tp.Format := 8;
  Tp.Nitems := Strlen(Capt);

  XSetWMName(FDisplay, FWindow, @Tp);
  XStoreName(FDisplay, FWindow, Capt);
  XSetIconName(FDisplay, FWindow, Capt);
  XSetWMIconName(FDisplay, FWindow, @Tp);
end;

procedure TPlatformSpecific.Put_Image;
var
  RowLength: Integer;
  TempBuffer: Pointer;

  TempRenderingBuffer: TAggRenderingBuffer;
begin
  if FXImageWindow = nil then
    Exit;

  FXImageWindow.Data := FBufferWindow;

  if FPixelFormat = FSystemFormat then
    XPut_Image(FDisplay, FWindow, FGraphicContext, FXImageWindow, 0, 0, 0, 0, 
      Src.GetWidth, Src.GetHeight)
  else
  begin
    RowLength := Src.GetWidth * FSystemBitsPerPixel div 8;

    AggGetMem(TempBuffer, RowLength * Src.GetHeight);

    TempRenderingBuffer.Create;

    if FFlipY then
      TempRenderingBuffer.Attach(TempBuffer, Src.GetWidth, Src.GetHeight, -RowLength)
    else
      TempRenderingBuffer.Attach(TempBuffer, Src.GetWidth, Src.GetHeight, RowLength);

    case FSystemFormat of
      pfRgb555:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToRgb555);
          pfRgb565:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb565ToRgb555);
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24ToRgb555 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToRgb555);
          // pix_formatRgba32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgba32ToRgb555 );
          // pix_format_argb32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionArgb32ToRgb555 );
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToRgb555);
          // pix_format_abgr32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionAbgr32ToRgb555 );
        end;

      pfRgb565:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToRgb565);
          // pix_formatRgb565 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb565ToRgb565 );
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24ToRgb565 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToRgb565);
          // pix_formatRgba32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgba32ToRgb565 );
          // pix_format_argb32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionArgb32ToRgb565 );
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToRgb565);
          // pix_format_abgr32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionAbgr32ToRgb565 );
        end;

      pfRgba32:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToRgba32);
          // pix_formatRgb565 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb565ToRgba32 );
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24ToRgba32 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToRgba32);
          // pix_formatRgba32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgba32ToRgba32 );
          // pix_format_argb32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionArgb32ToRgba32 );
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToRgba32);
          // pix_format_abgr32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionAbgr32ToRgba32 );
        end;

      pfAbgr32:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToAbgr32);
          // pix_formatRgb565 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb565To_abgr32 );
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24To_abgr32 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToAbgr32);
          // pix_format_abgr32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionAbgr32To_abgr32 );
          // pix_formatRgba32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgba32To_abgr32 );
          // pix_format_argb32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionArgb32To_abgr32 );
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToAbgr32);
        end;

      pfArgb32:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToArgb32);
          // pix_formatRgb565 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb565To_argb32 );
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24To_argb32 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToArgb32);
          pfRgba32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgba32ToArgb32);
          // pix_format_argb32 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionArgb32To_argb32 );
          pfAbgr32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionAbgr32ToArgb32);
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToArgb32);
        end;

      pfBgra32:
        case FPixelFormat of
          pfRgb555:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgb555ToBgra32);
          // pix_formatRgb565 : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb565To_bgra32 );
          // pix_formatRgb24  : ColorConversion(@TempRenderingBuffer ,src ,ColorConversionRgb24ToBgra32 );
          pfBgr24:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgr24ToBgra32);
          pfRgba32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionRgba32ToBgra32);
          pfArgb32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionArgb32ToBgra32);
          pfAbgr32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionAbgr32ToBgra32);
          pfBgra32:
            ColorConversion(@TempRenderingBuffer, Src, ColorConversionBgra32ToBgra32);
        end;
    end;

    FXImageWindow.Data := TempBuffer;

    XPut_Image(FDisplay, FWindow, FGraphicContext, FXImageWindow, 0, 0, 0, 0, 
      Src.GetWidth, Src.GetHeight);

    AggFreeMem(TempBuffer, RowLength * Src.GetHeight);

    TempRenderingBuffer.Free;
  end;
end;


{ TPlatformSupport }

constructor TPlatformSupport.Create;
var
  I: Cardinal;
  P, N, X: ShortString;
begin
  New(FSpecific, Create(Format_, FlipY));

  FControls := TControlContainer.Create;
  FRenderingBufferWindow.Create;

  for I := 0 to CMaxImages - 1 do
    FRenderingBufferImage[I].Create;

  FResizeMatrix.Create;

  FPixelFormat := Format_;

  FBitsPerPixel := FSpecific.FBitsPerPixel;

  FWindowFlags := 0;
  FWaitMode := True;
  FFlipY := FlipY;

  FInitialWidth := 10;
  GetInitialHeight := 10;

  FCaption := 'Anti-Grain Geometry Application'#0;

  // Change working dir to the application one
  SpreadName(ParamStr(0), P, N, X);

  P := P + #0;

  Libc.__chdir(PAnsiChar(@P[1]));

end;

destructor TPlatformSupport.Destroy;
var
  I: Cardinal;
begin
  FSpecific.Free;

  FControls.Free;
  FRenderingBufferWindow.Free;

  for I := 0 to CMaxImages - 1 do
    FRenderingBufferImage[I].Free;

  inherited;
end;

procedure TPlatformSupport.SetCaption;
begin
  FCaption := Cap + #0;

  Dec(Byte(FCaption[0]));

  if FSpecific.FInitialized then
    FSpecific.SetCaption(PAnsiChar(@FCaption[1]));
end;

function IsDigit(C: AnsiChar): Boolean;
begin
  case C of
    '0'..'9':
      Result := True;
  else
    Result := False;
  end;
end;

{ atoi }
function Atoi(C: PAnsiChar): Integer;
var
  S: ShortString;
  E: Integer;
begin
  S := '';

  repeat
    case C^ of
      '0'..'9':
        S := S + C^;

    else
      Break;

    end;

    Inc(PtrComp(C));

  until False;

  Val(S, Result, E);

end;

function TPlatformSupport.LoadImage;
var
  Fd : file;
  Buf: array [0..1023] of AnsiChar;
  Len: Integer;
  Ptr: PAnsiChar;
  Ret: Boolean;

  Width, Height: Cardinal;

  Buf_img  : Pointer;
  RenderingBufferImage_: TAggRenderingBuffer;
begin
  Result := False;

  if Index < CMaxImages then
  begin
    File_ := File_ + GetImageExtension;

    if not FileExists(File_) then
      File_ := 'ppm/' + File_;

    AssignFile(Fd, File_);
    Reset(Fd, 1);

    if IOResult <> 0 then
      Exit;

    Blockread(Fd, Buf, 1022, Len);

    if Len = 0 then
    begin
      Close(Fd);
      Exit;
    end;

    Buf[Len] := #0;

    if (Buf[0] <> 'P') and (Buf[1] <> '6') then
    begin
      Close(Fd);
      Exit;
    end;

    Ptr := @Buf[2];

    while (Ptr^ <> #0) and not IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    if Ptr^ = #0 then
    begin
      Close(Fd);
      Exit;
    end;

    Width := Atoi(Ptr);

    if (Width = 0) or (Width > 4096) then
    begin
      Close(Fd);
      Exit;
    end;

    while (Ptr^ <> #0) and IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    while (Ptr^ <> #0) and not IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    if Ptr^ = #0 then
    begin
      Close(Fd);
      Exit;
    end;

    Height := Atoi(Ptr);

    if (Height = 0) or (Height > 4096) then
    begin
      Close(Fd);
      Exit;
    end;

    while (Ptr^ <> #0) and IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    while (Ptr^ <> #0) and not IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    if Atoi(Ptr) <> 255 then
    begin
      Close(Fd);
      Exit;
    end;

    while (Ptr^ <> #0) and IsDigit(Ptr^) do
      Inc(PtrComp(Ptr));

    if Ptr^ = #0 then
    begin
      Close(Fd);
      Exit;
    end;

    Inc(PtrComp(Ptr));
    Seek(Fd, PtrComp(Ptr) - PtrComp(@Buf));
    CreateImage(Index, Width, Height);

    Ret := True;

    if FPixelFormat = pfRgb24 then
      Blockread(Fd, FSpecific.FBufferImage[Index]^, Width * Height * 3)
    else
    begin
      AggGetMem(Buf_img, Width * Height * 3);

      RenderingBufferImage_.Create;

      if FFlipY then
        RenderingBufferImage_.Attach(Buf_img, Width, Height, -Width * 3)
      else
        RenderingBufferImage_.Attach(Buf_img, Width, Height, Width * 3);

      Blockread(Fd, Buf_img^, Width * Height * 3);

      case FPixelFormat of
        // pix_formatRgb555 : ColorConversion(@m_RenderingBufferImage[Index ] ,@RenderingBufferImage_ ,ColorConversionRgb24ToRgb555 );
        // pix_formatRgb565 : ColorConversion(@m_RenderingBufferImage[Index ] ,@RenderingBufferImage_ ,ColorConversionRgb24ToRgb565 );
        pfBgr24:
          ColorConversion(@FRenderingBufferImage[Index], @RenderingBufferImage_, ColorConversionRgb24ToBgr24);
        // pix_formatRgba32 : ColorConversion(@m_RenderingBufferImage[Index ] ,@RenderingBufferImage_ ,ColorConversionRgb24ToRgba32 );
        // pix_format_argb32 : ColorConversion(@m_RenderingBufferImage[Index ] ,@RenderingBufferImage_ ,ColorConversionRgb24To_argb32 );
        pfBgra32:
          ColorConversion(@FRenderingBufferImage[Index], @RenderingBufferImage_, ColorConversionRgb24ToBgra32);
        // pix_format_abgr32 : ColorConversion(@m_RenderingBufferImage[Index ] ,@RenderingBufferImage_ ,ColorConversionRgb24To_abgr32 );
      else
        Ret := False;

      end;

      AggFreeMem(Buf_img, Width * Height * 3);

      RenderingBufferImage_.Free;
    end;

    Close(Fd);

    Result := Ret;
  end;
end;

function TPlatformSupport.SaveImage(Index: Cardinal; File_: ShortString): Boolean;
var
  Fd: file;

  S, C: ShortString;

  W, H, Y: Cardinal;

  Tmp_buf, Src: Pointer;
begin
  Result := False;

  if (Index < CMaxImages) and (RenderingBufferImage(Index).GetBuffer <> nil) then
  begin
    AssignFile(Fd, File_);
    Rewrite(Fd, 1);

    if IOResult <> 0 then
      Exit;

    W := RenderingBufferImage(Index).GetWidth;
    H := RenderingBufferImage(Index).GetHeight;

    Str(W, C);

    S := 'P6'#13 + C + ' ';

    Str(H, C);

    S := S + C + #13'255'#13;

    Blockwrite(Fd, S[1], Length(S));

    AggGetMem(Tmp_buf, W * 3);

    Y := 0;

    while Y < RenderingBufferImage(Index).GetHeight do
    begin
      if FFlipY then
        Src := RenderingBufferImage(Index).Row(H - 1 - Y)
      else
        Src := RenderingBufferImage(Index).Row(Y);

      case FPixelFormat of
        pfRgb555:
          ColorConversionRgb555ToRgb24(Tmp_buf, Src, W);
        // pix_formatRgb565 : ColorConversionRgb565ToRgb24(tmp_buf ,src ,w );
        pfBgr24:
          ColorConversionBgr24ToRgb24(Tmp_buf, Src, W);
        // pix_formatRgb24  : ColorConversionRgb24ToRgb24 (tmp_buf ,src ,w );
        // pix_formatRgba32 : ColorConversionRgba32ToRgb24(tmp_buf ,src ,w );
        // pix_format_argb32 : ColorConversionArgb32ToRgb24(tmp_buf ,src ,w );
        pfBgra32:
          ColorConversionBgra32ToRgb24(Tmp_buf, Src, W);
        // pix_format_abgr32 : ColorConversionAbgr32ToRgb24(tmp_buf ,src ,w );
      end;

      Blockwrite(Fd, Tmp_buf^, W * 3);
      Inc(Y);
    end;

    AggGetMem(Tmp_buf, W * 3);
    Close(Fd);

    Result := True;
  end;
end;

function TPlatformSupport.CreateImage;
begin
  Result := False;

  if Index < CMaxImages then
  begin
    if AWidth = 0 then
      AWidth := Trunc(RenderingBufferWindow.Width);

    if AHeight = 0 then
      AHeight := Trunc(RenderingBufferWindow.Height);

    AggFreeMem(FSpecific.FBufferImage[Index], FSpecific.FImageAlloc[Index]);

    FSpecific.FImageAlloc[Index] := AWidth * AHeight * (FBitsPerPixel div 8);

    AggGetMem(FSpecific.FBufferImage[Index], FSpecific.FImageAlloc[Index]);

    if FFlipY then
      FRenderingBufferImage[Index].Attach(FSpecific.FBufferImage[Index], AWidth, AHeight,
        -AWidth * (FBitsPerPixel div 8))
    else
      FRenderingBufferImage[Index].Attach(FSpecific.FBufferImage[Index], AWidth, AHeight,
        AWidth * (FBitsPerPixel div 8));

    Result := True;
  end;
end;

function TPlatformSupport.Init;
const
  Xevent_mask = PointerMotionMask or ButtonPressMask or ButtonReleaseMask or
    ExposureMask or KeyPressMask or StructureNotifyMask;
var
  R_mask, G_mask, B_mask, Window_mask: Cardinal;
  T, Hw_byte_order: Integer;
  Hints: PXSizeHints;
begin
  FWindowFlags := Flags;

  FSpecific.FDisplay := XOpenDisplay(nil);

  if FSpecific.FDisplay = nil then
  begin
    Writeln(Stderr, 'Unable to open DISPLAY!');

    Result := False;

    Exit;
  end;

  FSpecific.FScreen := XDefaultScreen(FSpecific.FDisplay);
  FSpecific.FDepth := XDefaultDepth(FSpecific.FDisplay,
    FSpecific.FScreen);
  FSpecific.FVisual := XDefaultVisual(FSpecific.FDisplay,
    FSpecific.FScreen);

  R_mask := FSpecific.FVisual.Red_mask;
  G_mask := FSpecific.FVisual.Green_mask;
  B_mask := FSpecific.FVisual.Blue_mask;

  if (FSpecific.FDepth < 15) or (R_mask = 0) or (G_mask = 0) or (B_mask = 0)
  then
  begin
    Writeln(Stderr,
      'There''s no Visual compatible with minimal AGG requirements:');
    Writeln(Stderr,
      'At least 15-bit color depth and True- or DirectColor class.');
    Writeln(Stderr);

    XCloseDisplay(FSpecific.FDisplay);

    Result := False;

    Exit;

  end;

  T := 1;

  Hw_byte_order := LSBFirst;

  if Byte(Pointer(@T)^) = 0 then
    Hw_byte_order := MSBFirst;

  // Perceive SYS-format by mask
  case FSpecific.FDepth of
    15:
      begin
        FSpecific.FSystemBitsPerPixel := 16;

        if (R_mask = $7C00) and (G_mask = $3E0) and (B_mask = $1F) then
        begin
          FSpecific.FSystemFormat := pfRgb555;
          FSpecific.FByteOrder := Hw_byte_order;

        end;

      end;

    16:
      begin
        FSpecific.FSystemBitsPerPixel := 16;

        if (R_mask = $F800) and (G_mask = $7E0) and (B_mask = $1F) then
        begin
          FSpecific.FSystemFormat := pfRgb565;
          FSpecific.FByteOrder := Hw_byte_order;

        end;

      end;

    24, 32:
      begin
        FSpecific.FSystemBitsPerPixel := 32;

        if G_mask = $FF00 then
        begin
          if (R_mask = $FF) and (B_mask = $FF0000) then
            case FSpecific.FPixelFormat of
              pfRgba32:
                begin
                  FSpecific.FSystemFormat := pfRgba32;
                  FSpecific.FByteOrder := LSBFirst;
                end;

              pfAbgr32:
                begin
                  FSpecific.FSystemFormat := pfAbgr32;
                  FSpecific.FByteOrder := MSBFirst;
                end;

            else
              begin
                FSpecific.FByteOrder := Hw_byte_order;

                if Hw_byte_order = LSBFirst then
                  FSpecific.FSystemFormat := pfRgba32
                else
                  FSpecific.FSystemFormat := pfAbgr32;
              end;

            end;

          if (R_mask = $FF0000) and (B_mask = $FF) then
            case FSpecific.FPixelFormat of
              pfArgb32:
                begin
                  FSpecific.FSystemFormat := pfArgb32;
                  FSpecific.FByteOrder := MSBFirst;
                end;

              pfBgra32:
                begin
                  FSpecific.FSystemFormat := pfBgra32;
                  FSpecific.FByteOrder := LSBFirst;
                end;

            else
              begin
                FSpecific.FByteOrder := Hw_byte_order;

                if Hw_byte_order = MSBFirst then
                  FSpecific.FSystemFormat := pfArgb32
                else
                  FSpecific.FSystemFormat := pfBgra32;
              end;
            end;
        end;
      end;
  end;

  if FSpecific.FSystemFormat = pfUndefined then
  begin
    Writeln(Stderr, 'RGB masks are not compatible with AGG pixel formats:');
    write(Stderr, 'R=', R_mask, 'G=', G_mask, 'B=', B_mask);

    XCloseDisplay(FSpecific.FDisplay);

    Result := False;

    Exit;

  end;

  FillChar(FSpecific.FWindowAttributes,
    SizeOf(FSpecific.FWindowAttributes), 0);

  FSpecific.FWindowAttributes.Border_pixel :=
    XBlackPixel(FSpecific.FDisplay, FSpecific.FScreen);

  FSpecific.FWindowAttributes.Background_pixel :=
    XWhitePixel(FSpecific.FDisplay, FSpecific.FScreen);

  FSpecific.FWindowAttributes.Override_redirect := False;

  Window_mask := CWBackPixel or CWBorderPixel;

  FSpecific.FWindow := XCreateWindow(FSpecific.FDisplay,
    XDefaultRootWindow(FSpecific.FDisplay), 0, 0, AWidth, AHeight, 0,
    FSpecific.FDepth, InputOutput, CopyFromParent, Window_mask,
    @FSpecific.FWindowAttributes);

  FSpecific.FGraphicContext := XCreateGC(FSpecific.FDisplay, FSpecific.FWindow, 0, 0);

  FSpecific.FBufferAlloc := AWidth * AHeight * (FBitsPerPixel div 8);

  AggGetMem(FSpecific.FBufferWindow, FSpecific.FBufferAlloc);
  FillChar(FSpecific.FBufferWindow^, FSpecific.FBufferAlloc, 255);

  if FFlipY then
    FRenderingBufferWindow.Attach(FSpecific.FBufferWindow, AWidth, AHeight,
      -AWidth * (FBitsPerPixel div 8))
  else
    FRenderingBufferWindow.Attach(FSpecific.FBufferWindow, AWidth, AHeight,
      AWidth * (FBitsPerPixel div 8));

  FSpecific.FXImageWindow := XCreateImage(FSpecific.FDisplay,
    FSpecific.FVisual, // CopyFromParent,
    FSpecific.FDepth, ZPixmap, 0, FSpecific.FBufferWindow, AWidth, AHeight,
    FSpecific.FSystemBitsPerPixel, AWidth * (FSpecific.FSystemBitsPerPixel div 8));

  FSpecific.FXImageWindow.Byte_order := FSpecific.FByteOrder;

  FSpecific.SetCaption(PAnsiChar(@FCaption[1]));

  FInitialWidth := AWidth;
  GetInitialHeight := AHeight;

  if not FSpecific.FInitialized then
  begin
    OnInit;

    FSpecific.FInitialized := True;

  end;

  SetTransAffineResizing(AWidth, AHeight);

  OnResize(AWidth, AHeight);

  FSpecific.FUpdateFlag := True;

  Hints := XAllocSizeHints;

  if Hints <> nil then
  begin
    if Flags and wfResize <> 0 then
    begin
      Hints.Min_width := 32;
      Hints.Min_height := 32;
      Hints.Max_width := 4096;
      Hints.Max_height := 4096;

    end
    else
    begin
      Hints.Min_width := AWidth;
      Hints.Min_height := AHeight;
      Hints.Max_width := AWidth;
      Hints.Max_height := AHeight;

    end;

    Hints.Flags := PMaxSize or PMinSize;

    XSetWMNormalHints(FSpecific.FDisplay, FSpecific.FWindow, Hints);
    XFree(Hints);

  end;

  XMapWindow(FSpecific.FDisplay, FSpecific.FWindow);
  XSelectInput(FSpecific.FDisplay, FSpecific.FWindow, Xevent_mask);

  FSpecific.FCloseAtom := XInternAtom(FSpecific.FDisplay,
    'WM_DELETE_WINDOW', False);

  XSetWMProtocols(FSpecific.FDisplay, FSpecific.FWindow,
    @FSpecific.FCloseAtom, 1);

  Result := True;

end;

function TPlatformSupport.Run;
var
  Flags, I: Cardinal;
  Cur_x, Cur_y, Width, Height: Integer;
  X_event, Te: TXEvent;
  Key: TKeySym;
  Left, Up, Right, Down: Boolean;

begin
  XFlush(FSpecific.FDisplay);

  FQuit := False;

  while not FQuit do
  begin
    if FSpecific.FUpdateFlag then
    begin
      OnDraw;
      UpdateWindow;

      FSpecific.FUpdateFlag := False;
    end;

    if not FWaitMode then
      if XPending(FSpecific.FDisplay) = 0 then
      begin
        OnIdle;
        Continue;
      end;

    XNextEvent(FSpecific.FDisplay, @X_event);

    // In the Idle mode discard all intermediate MotionNotify events
    if not FWaitMode and (X_event._type = MotionNotify) then
    begin
      Te := X_event;

      repeat
        if XPending(FSpecific.FDisplay) = 0 then
          Break;

        XNextEvent(FSpecific.FDisplay, @Te);

        if Te._type <> MotionNotify then
          Break;

      until False;

      X_event := Te;
    end;

    case X_event._type of
      ConfigureNotify:
        if (X_event.Xconfigure.Width <> Trunc(FRenderingBufferWindow.Width)) or
          (X_event.Xconfigure.Height <> Trunc(FRenderingBufferWindow.Height)) then
        begin
          Width := X_event.Xconfigure.Width;
          Height := X_event.Xconfigure.Height;

          AggFreeMem(FSpecific.FBufferWindow, FSpecific.FBufferAlloc);

          FSpecific.FXImageWindow.Data := 0;

          XDestroyImage(FSpecific.FXImageWindow);

          FSpecific.FBufferAlloc := Width * Height * (FBitsPerPixel div 8);

          AggGetMem(FSpecific.FBufferWindow, FSpecific.FBufferAlloc);

          if FFlipY then
            FRenderingBufferWindow.Attach(FSpecific.FBufferWindow, Width, Height,
              -Width * (FBitsPerPixel div 8))
          else
            FRenderingBufferWindow.Attach(FSpecific.FBufferWindow, Width, Height,
              Width * (FBitsPerPixel div 8));

          FSpecific.FXImageWindow := XCreateImage(FSpecific.FDisplay,
            FSpecific.FVisual, // CopyFromParent,
            FSpecific.FDepth, ZPixmap, 0, FSpecific.FBufferWindow, Width,
            Height, FSpecific.FSystemBitsPerPixel, Width * (FSpecific.FSystemBitsPerPixel div 8));

          FSpecific.FXImageWindow.Byte_order := FSpecific.FByteOrder;

          SetTransAffineResizing(Width, Height);

          OnResize(Width, Height);
          OnDraw;
          UpdateWindow;
        end;

      Expose:
        begin
          FSpecific.Put_image(@FRenderingBufferWindow);

          XFlush(FSpecific.FDisplay);
          XSync(FSpecific.FDisplay, False);
        end;

      KeyPress:
        begin
          Key := XLookupKeysym(@X_event.Xkey, 0);
          Flags := 0;

          if X_event.Xkey.State and Button1Mask <> 0 then
            Flags := Flags or Mouse_left;

          if X_event.Xkey.State and Button3Mask <> 0 then
            Flags := Flags or Mouse_right;

          if X_event.Xkey.State and ShiftMask <> 0 then
            Flags := Flags or Kbd_shift;

          if X_event.Xkey.State and ControlMask <> 0 then
            Flags := Flags or Kbd_ctrl;

          Left := False;
          Up := False;
          Right := False;
          Down := False;

          case FSpecific.FKeymap[Key and $FF] of
            Cardinal(kcLeft:
              Left := True;
            Cardinal(kcUp:
              Up := True;
            Cardinal(kcRight:
              Right := True;
            Cardinal(kcDown:
              Down := True;

            Cardinal(kcF2:
              begin
                CopyWindowToImage(CMaxImages - 1);
                SaveImage(CMaxImages - 1, 'screenshot.ppm');
              end;
          end;

          if FControls.OnArrowKeys(Left, Right, Down, Up) then
          begin
            OnControlChange;
            ForceRedraw;
          end
          else if FFlipY then
            OnKey(X_event.Xkey.X, Trunc(FRenderingBufferWindow.Height) -
              X_event.Xkey.Y, FSpecific.FKeymap[Key and $FF], Flags)
          else
            OnKey(X_event.Xkey.X, X_event.Xkey.Y,
              FSpecific.FKeymap[Key and $FF], Flags)
        end;

      ButtonPress:
        begin
          Flags := 0;

          if X_event.Xbutton.State and ShiftMask <> 0 then
            Flags := Flags or Kbd_shift;

          if X_event.Xbutton.State and ControlMask <> 0 then
            Flags := Flags or Kbd_ctrl;

          if X_event.Xbutton.Button = Button1 then
            Flags := Flags or Mouse_left;

          if X_event.Xbutton.Button = Button3 then
            Flags := Flags or Mouse_right;

          Cur_x := X_event.Xbutton.X;

          if FFlipY then
            Cur_y := Trunc(FRenderingBufferWindow.Height) - X_event.Xbutton.Y
          else
            Cur_y := X_event.Xbutton.Y;

          if mkfMouseLeft in Flags then
            if FControls.OnMouseButtonDown(Cur_x, Cur_y) then
            begin
              FControls.SetCurrent(Cur_x, Cur_y);
              OnControlChange;
              ForceRedraw;
            end
            else if FControls.InRect(Cur_x, Cur_y) then
              if FControls.SetCurrent(Cur_x, Cur_y) then
              begin
                OnControlChange;
                ForceRedraw;
              end
              else
            else
              OnMouseButtonDown(Cur_x, Cur_y, Flags);

          if mkfMouseRight in Flags then
            OnMouseButtonDown(Cur_x, Cur_y, Flags);

          // FSpecific.FWaitMode:=FWaitMode;
          // FWaitMode           :=true;
        end;

      MotionNotify:
        begin
          Flags := 0;

          if X_event.Xmotion.State and Button1Mask <> 0 then
            Flags := Flags or Mouse_left;

          if X_event.Xmotion.State and Button3Mask <> 0 then
            Flags := Flags or Mouse_right;

          if X_event.Xmotion.State and ShiftMask <> 0 then
            Flags := Flags or Kbd_shift;

          if X_event.Xmotion.State and ControlMask <> 0 then
            Flags := Flags or Kbd_ctrl;

          Cur_x := X_event.Xbutton.X;

          if FFlipY then
            Cur_y := Trunc(FRenderingBufferWindow.Height) - X_event.Xbutton.Y
          else
            Cur_y := X_event.Xbutton.Y;

          if FControls.OnMouseMove(Cur_x, Cur_y, mkfMouseLeft in Flags) then
          begin
            OnControlChange;
            ForceRedraw;
          end
          else if not FControls.InRect(Cur_x, Cur_y) then
            OnMouseMove(Cur_x, Cur_y, Flags);
        end;

      ButtonRelease:
        begin
          Flags := 0;

          if X_event.Xbutton.State and ShiftMask <> 0 then
            Flags := Flags or Kbd_shift;

          if X_event.Xbutton.State and ControlMask <> 0 then
            Flags := Flags or Kbd_ctrl;

          if X_event.Xbutton.Button = Button1 then
            Flags := Flags or Mouse_left;

          if X_event.Xbutton.Button = Button3 then
            Flags := Flags or Mouse_right;

          Cur_x := X_event.Xbutton.X;

          if FFlipY then
            Cur_y := Trunc(FRenderingBufferWindow.Height) - X_event.Xbutton.Y
          else
            Cur_y := X_event.Xbutton.Y;

          if mkfMouseLeft in Flags then
            if FControls.OnMouseButtonUp(Cur_x, Cur_y) then
            begin
              OnControlChange;
              ForceRedraw;
            end;

          if Flags and (Mouse_left or Mouse_right) <> 0 then
            OnMouseButtonUp(Cur_x, Cur_y, Flags);

          // FWaitMode:=FSpecific.FWaitMode;
        end;

      ClientMessage:
        if (X_event.Xclient.Format = 32) and
          (X_event.Xclient.Data.L[0] = Integer(FSpecific.FCloseAtom)) then
          FQuit := True;
    end;
  end;

  I := CMaxImages;

  while I <> 0 do
  begin
    Dec(I);

    if FSpecific.FBufferImage[I] <> nil then
      AggFreeMem(FSpecific.FBufferImage[I], FSpecific.FImageAlloc[I]);
  end;

  AggFreeMem(FSpecific.FBufferWindow, FSpecific.FBufferAlloc);

  FSpecific.FXImageWindow.Data := nil;

  XDestroyImage(FSpecific.FXImageWindow);
  XFreeGC(FSpecific.FDisplay, FSpecific.FGraphicContext);
  XDestroyWindow(FSpecific.FDisplay, FSpecific.FWindow);
  XCloseDisplay(FSpecific.FDisplay);

  Result := 0;
end;

procedure TPlatformSupport.Quit;
begin
  FQuit := True;
end;

function TPlatformSupport.GetPixelFormat;
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
begin
  FSpecific.FUpdateFlag := True;
end;

procedure TPlatformSupport.UpdateWindow;
begin
  FSpecific.Put_image(@FRenderingBufferWindow);

  // When FWaitMode is true we can discard all the events
  // came while the image is being drawn. In this case
  // the X server does not accumulate mouse motion events.
  // When FWaitMode is false, i.e. we have some idle drawing
  // we cannot afford to miss any events
  XSync(FSpecific.FDisplay, FWaitMode);
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
  Result := '.ppm';
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
  if (IndexFrom < CMaxImages) and (IndexTo < CMaxImages) and
    (RenderingBufferImage(IndexFrom).GetBuffer <> nil) then
  begin
    CreateImage(IndexTo, RenderingBufferImage(IndexFrom).GetWidth, RenderingBufferImage(IndexFrom).GetHeight);

    RenderingBufferImage(IndexTo).CopyFrom(RenderingBufferImage(IndexFrom));
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
  FControls.Add(C);

  C.Transform(@FResizeMatrix);
end;

procedure TPlatformSupport.SetTransAffineResizing;
var
  Vp: TAggTransViewport;
  Ts: TAggTransAffineScaling;
begin
  if FWindowFlags and wfKeepAspectRatio <> 0 then
  begin
    Vp.Create;
    Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

    Vp.DeviceViewport(0, 0, AWidth, AHeight);
    Vp.WorldViewport(0, 0, FInitialWidth, GetInitialHeight);

    Vp.ToAffine(@FResizeMatrix);
  end
  else
  begin
    Ts.Create(AWidth / FInitialWidth, AHeight / GetInitialHeight);

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

function TPlatformSupport.Initial_width;
begin
  Result := FInitialWidth;
end;

function TPlatformSupport.Initial_height;
begin
  Result := GetInitialHeight;
end;

function TPlatformSupport.GetWindowFlags;
begin
  Result := FWindowFlags;
end;

function TPlatformSupport.GetRawDisplayHandler;
begin
end;

procedure TPlatformSupport.DisplayMessage;
const
  X_event_mask = ExposureMask or KeyPressMask;

  Capt = '  PRESS ANY KEY TO CONTINUE THE AGGPAS DEMO ...';
  Plus = 4;
var
  X_display: PDisplay;
  X_window : TWindow;
  X_event  : TXEvent;
  X_close  : TAtom;
  X_changes: TXWindowChanges;
  X_hints  : PXSizeHints;

  X_gc: TGC;
  X_tp: TXTextProperty;
  X_tx: TXTextItem;

  Str, Cur: PAnsiChar;

  Y, Len, Cnt, Max, X_dx, X_dy: Cardinal;

  Font_dir, Font_ascent, Font_descent: Integer;

  Font_str: TXCharStruct;

  procedure Draw_text;
  begin
    X_dx := 0;
    X_dy := 0;

    Y := 20;
    Cur := PAnsiChar(@Msg[0]);
    Max := Strlen(Msg);
    Len := 0;
    Cnt := 0;

    while Cnt < Max do
    begin
      if Len = 0 then
        Str := Cur;

      case Cur^ of
        #13:
          begin
            XDrawString(X_display, X_window, X_gc, 10, Y, Str, Len);
            XQueryTextExtents(X_display, XGContextFromGC(X_gc), Str, Len,
              @Font_dir, @Font_ascent, @Font_descent, @Font_str);

            Inc(Y, Font_str.Ascent + Font_str.Descent + Plus);
            Inc(X_dy, Font_str.Ascent + Font_str.Descent + Plus);

            if Font_str.Width > X_dx then
              X_dx := Font_str.Width;

            Len := 0;

          end;

      else
        Inc(Len);

      end;

      Inc(PtrComp(Cur));
      Inc(Cnt);

    end;

    if Len > 0 then
    begin
      XDrawString(X_display, X_window, X_gc, 10, Y, Str, Len);
      XQueryTextExtents(X_display, XGContextFromGC(X_gc), Str, Len, @Font_dir,
        @Font_ascent, @Font_descent, @Font_str);

      Inc(X_dy, Font_str.Ascent + Font_str.Descent + Plus);

      if Font_str.Width > X_dx then
        X_dx := Font_str.Width;

    end;

  end;

begin
  X_display := XOpenDisplay(nil);

  if X_display <> nil then
  begin
    X_window := XCreateSimpleWindow(X_display, XDefaultRootWindow(X_display),
      50, 50, 550, 300, 0, 0, 255 + (255 shl 8) + (255 shl 16));

    X_gc := XCreateGC(X_display, X_window, 0, 0);

    Draw_text;
    XResizeWindow(X_display, X_window, X_dx + 20, X_dy + 40);

    X_hints := XAllocSizeHints;

    if X_hints <> nil then
    begin
      X_hints.Min_width := X_dx + 20;
      X_hints.Min_height := X_dy + 40;
      X_hints.Max_width := X_dx + 20;
      X_hints.Max_height := X_dy + 40;

      X_hints.Flags := PMaxSize or PMinSize;

      XSetWMNormalHints(X_display, X_window, X_hints);
      XFree(X_hints);

    end;

    X_tp.Value := PCUChar(@Capt[1]);
    X_tp.Encoding := XA_WM_NAME;
    X_tp.Format := 8;
    X_tp.Nitems := Strlen(Capt);

    XSetWMName(X_display, X_window, @X_tp);
    XStoreName(X_display, X_window, Capt);
    XSetIconName(X_display, X_window, Capt);
    XSetWMIconName(X_display, X_window, @X_tp);

    XMapWindow(X_display, X_window);
    XSelectInput(X_display, X_window, X_event_mask);

    X_close := XInternAtom(X_display, 'WM_DELETE_WINDOW', False);

    XSetWMProtocols(X_display, X_window, @X_close, 1);

    XFlush(X_display);

    repeat
      XNextEvent(X_display, @X_event);

      XFlush(X_display);
      XSync(X_display, True);

      case X_event._type of
        Expose:
          Draw_text;

        KeyPress:
          Break;

        ClientMessage:
          if (X_event.Xclient.Format = 32) and
            (X_event.Xclient.Data.L[0] = Integer(X_close)) then
            Break;

      end;

    until False;

    while XPending(X_display) > 0 do
    begin
      XNextEvent(X_display, @X_event);

      XFlush(X_display);
      XSync(X_display, True);

    end;

    XFreeGC(X_display, X_gc);
    XDestroyWindow(X_display, X_window);
    XCloseDisplay(X_display);

  end
  else
    Writeln(Stderr, Msg);
end;

procedure TPlatformSupport.StartTimer;
begin
  FSpecific.FSwStart := Clock;
end;

function TPlatformSupport.Elapsed_time;
var
  Stop: Clock_t;
begin
  Stop := Clock;

  Result := (Stop - FSpecific.FSwStart) * 1000.0 / CLOCKS_PER_SEC;
end;

function TPlatformSupport.Full_file_name;
begin
  Result := FileName;
end;

function TPlatformSupport.FileSource;
var
  F: file;
  E: Integer;
begin
  Result := FName;

  E := Ioresult;

  AssignFile(F, Result);
  Reset(F, 1);

  if Ioresult <> 0 then
    Result := Path + '/' + FName;

  Close(F);

  E := Ioresult;
end;

end.
