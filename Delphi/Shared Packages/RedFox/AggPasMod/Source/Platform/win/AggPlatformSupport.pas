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
//  The TPlatformSupport class has just a pointer to it and it's              //
//  the responsibility of the implementation to create/delete it.             //
//  This class being defined in the implementation file can have              //
//  any platform dependent stuff such as HWND, X11 Window and so on.          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$I- }

uses
  Windows, Messages,

  AggBasics,
  AggControl,
  AggRenderingBuffer,
  AggTransAffine,
  AggTransViewport,
  AggWin32Bmp,
  AggColorConversion;

type
  // These are flags used in method init. Not all of them are
  // applicable on different platforms, for example the win32_api
  // cannot use a hardware buffer (window_hw_buffer).
  // The implementation should simply ignore unsupported flags.
  TWindowFlag = (wfResize, wfHardwareBuffer, wfKeepAspectRatio,
    wfProcessAllKeys);
  TWindowFlags = set of TWindowFlag;


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
  // This list can be extended in future.
  TPixelFormat = (
    pfUndefined, // By default. No conversions are applied
    pfBW,        // 1 bit per color B/W
    pfGray8,     // Simple 256 level grayscale
    pfGray16,    // Simple 65535 level grayscale
    pfRgb555,    // 15 bit rgb. Depends on the byte ordering!
    pfRgb565,    // 16 bit rgb. Depends on the byte ordering!
    pfRgbAAA,    // 30 bit rgb. Depends on the byte ordering!
    pfRgbBBA,    // 32 bit rgb. Depends on the byte ordering!
    pfBgrAAA,    // 30 bit bgr. Depends on the byte ordering!
    pfBgrABB,    // 32 bit bgr. Depends on the byte ordering!
    pfRgb24,     // R-G-B, one byte per color component
    pfBgr24,     // B-G-R, native win32 BMP format.
    pfRgba32,    // R-G-B-A, one byte per color component
    pfArgb32,    // A-R-G-B, native MAC format
    pfAbgr32,    // A-B-G-R, one byte per color component
    pfBgra32,    // B-G-R-A, native win32 BMP format
    pfRgb48,     // R-G-B, 16 bits per color component
    pfBgr48,     // B-G-R, native win32 BMP format.
    pfRgba64,    // R-G-B-A, 16 bits byte per color component
    pfArgb64,    // A-R-G-B, native MAC format
    pfAbgr64,    // A-B-G-R, one byte per color component
    pfBgra64     // B-G-R-A, native win32 BMP format
  );

  // Mouse and keyboard flags. They can be different on different platforms
  // and the ways they are obtained are also different. But in any case
  // the system dependent flags should be mapped into these ones. The meaning
  // of that is as follows. For example, if kbd_ctrl is set it means that the
  // ctrl key is pressed and being held at the moment. They are also used in
  // the overridden methods such as OnMouseMove, OnMouseButtonDown,
  // OnMouseButtonDoubleClick, OnMouseButtonUp, OnKey.
  // In the method OnMouseButtonUp the mouse flags have different
  // meaning. They mean that the respective button is being released, but
  // the meaning of the keyboard flags remains the same.
  // There's absolut minimal set of flags is used because they'll be most
  // probably supported on different platforms. Even the mouse_right flag
  // is restricted because Mac's mice have only one button, but AFAIK
  // it can be simulated with holding a special key on the keydoard.
  TMouseKeyboardFlag = (mkfMouseLeft, mkfMouseRight, mkfShift, mkfCtrl);
  TMouseKeyboardFlags = set of TMouseKeyboardFlag;

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
  TKeyCode = (
    kcNone = 0,
    kcBackspace = 8,
    kcTab = 9,
    kcClear = 12,
    kcReturn = 13,
    kcPause = 19,
    kcEscape = 27,

    // Keypad
    kcDelete = 127,
    kcPad0 = 256,
    kcPad1 = 257,
    kcPad2 = 258,
    kcPad3 = 259,
    kcPad4 = 260,
    kcPad5 = 261,
    kcPad6 = 262,
    kcPad7 = 263,
    kcPad8 = 264,
    kcPad9 = 265,
    kcPadPeriod = 266,
    kcPadDivide = 267,
    kcPadMultiply = 268,
    kcPadMinus = 269,
    kcPadPlus = 270,
    kcPadEnter = 271,
    kcPadEquals = 272,

    // Arrow-keys and stuff
    kcUp = 273,
    kcDown = 274,
    kcRight = 275,
    kcLeft = 276,
    kcInsert = 277,
    kcHome = 278,
    kcEnd = 279,
    kcPageUp = 280,
    kcPageDown = 281,

    // Functional keys. You'd better avoid using
    // f11...f15 in your applications if you want
    // the applications to be portable
    kcF1 = 282,
    kcF2 = 283,
    kcF3 = 284,
    kcF4 = 285,
    kcF5 = 286,
    kcF6 = 287,
    kcF7 = 288,
    kcF8 = 289,
    kcF9 = 290,
    kcF10 = 291,
    kcF11 = 292,
    kcF12 = 293,
    kcF13 = 294,
    kcF14 = 295,
    kcF15 = 296,

    // The possibility of using these keys is
    // very restricted. Actually it's guaranteed
    // only in win32_api and win32_sdl implementations
    kcNumlock = 300,
    kcCapslock = 301,
    kcScrollLock = 302
  );

const
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

    procedure Clear;

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
  // TAggApplication = object(TPlatformSupport)
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
  MaxImages = 16;

var
  // Hmmm, I had to rip the fields below out of the TPlatformSpecific class,
  // because being them the part of that object/class, the corresponding
  // Windows API calls QueryPerformanceXXX are working NOT!.
  // Anyway, since we use usually only one instance of TPlatformSpecific in
  // our agg demos, it's okay to do that this way. See {hack}.
  GSwFreq, GSwStart: Int64;

type
  TPlatformSpecific = class
  private
    FPixelFormat, FSystemPixelFormat: TPixelFormat;

    FFlipY: Boolean;
    FBitsPerPixel, FSysBitsPerPixel: Cardinal;
    FHwnd: HWND;

    FPixelMapWindow: TPixelMap;
    FPixelMapImage: array [0..MaxImages - 1] of TPixelMap;

    FKeymap: array [0..255] of TKeyCode;

    FLastTranslatedKey: TKeyCode;

    FCurX, FCurY: Integer;

    FInputFlags: TMouseKeyboardFlags;
    FRedrawFlag: Boolean;
    FCurrentDC: HDC;

    // GSwFreq  ,
    // GSwStart : int64;{hack}
 public
    constructor Create(Format: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure CreatePixelMap(Width, Height: Cardinal;
      RenderingBuffer: TAggRenderingBuffer); virtual;
    procedure DisplayPixelMap(Dc: HDC; Src: TAggRenderingBuffer); virtual;

    function LoadPixelMap(Fn: ShortString; Index: Cardinal;
      Dst: TAggRenderingBuffer): Boolean;
    function SavePixelMap(Fn: ShortString; Index: Cardinal;
      Src: TAggRenderingBuffer): Boolean;

    function Translate(Keycode: Cardinal): Cardinal;

    property WindowHandle: HWND read FHwnd;
  end;

  TPlatformSupport = class
  private
    FSpecific: TPlatformSpecific;

    FPixelFormat: TPixelFormat;

    FBitsPerPixel: Cardinal;

    FRenderingBufferWindow: TAggRenderingBuffer;
    FRenderingBufferImages: array [0..MaxImages - 1] of TAggRenderingBuffer;

    FWindowFlags: TWindowFlags;
    FWaitMode, FFlipY: Boolean;
    // FlipY - true if you want to have the Y-axis flipped vertically
    FCaption: string;
    FResizeMatrix: TAggTransAffine;

    FCtrls: TControlContainer;

    // The following provides a very simple mechanism of doing someting
    // in background. It's not multitheading. When whait_mode is true
    // the class waits for the events and it does not ever call OnIdle().
    // When it's false it calls OnIdle() when the event queue is empty.
    // The mode can be changed anytime. This mechanism is satisfactory
    // for creation very simple animations.
    function GetWaitMode: Boolean;
    procedure SetWaitMode(Value: Boolean);
    procedure SetCaption(Value: string);


    // So, finally, how to draw anythig with AGG? Very simple.
    // RenderingBufferWindow() returns a reference to the main rendering
    // buffer which can be attached to any rendering class.
    // RenderingBufferImage() returns a reference to the previously created
    // or loaded image buffer (see LoadImage()). The image buffers
    // are not displayed directly, they should be copied to or
    // combined somehow with the RenderingBufferWindow(). RenderingBufferWindow() is
    // the only buffer that can be actually displayed.
    function GetRenderingBufferImage(Index: Cardinal): TAggRenderingBuffer;

    // Returns file extension used in the implemenation for the particular
    // system.
    function GetImageExtension: ShortString;
    function GetWindowHandle: HWND;
  protected
    FInitialWidth, FInitialHeight: Integer;
    function GetWidth: Double;
    function GetHeight: Double;
    property Specific: TPlatformSpecific read FSpecific;
    property WindowHandle: HWND read GetWindowHandle;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean;
      Specific: TPlatformSpecific = nil);
    destructor Destroy; override;

    // These 3 menthods handle working with images. The image
    // formats are the simplest ones, such as .BMP in Windows or
    // .ppm in Linux. In the applications the names of the files
    // should not have any file extensions. Method LoadImage() can
    // be called before init(), so, the application could be able
    // to determine the initial size of the window depending on
    // the size of the loaded image.
    // The argument "idx" is the number of the image 0...MaxImages-1
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
    function Init(AWidth, AHeight: Cardinal; Flags: TWindowFlags): Boolean;
    function Run: Integer;
    procedure Quit;

    // These two functions control updating of the window.
    // ForceRedraw() is an analog of the Win32 InvalidateRect() function.
    // Being called it sets a flag (or sends a message) which results
    // in calling OnDraw() and updating the content of the window
    // when the next event cycle comes.
    // UpdateWindow() results in just putting immediately the content
    // of the currently rendered buffer to the window without calling
    // OnDraw().
    procedure ForceRedraw;
    procedure UpdateWindow;

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
    // width(), height(), initial_width(), and initial_height() must be
    // clear to understand with no comments :-)
    procedure SetTransAffineResizing(AWidth, AHeight: Integer);
    function GetTransAffineResizing: TAggTransAffine;

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
    procedure DisplayMessage(Msg: PAnsiChar); overload;
    procedure DisplayMessage(Msg: AnsiString); overload;
    {$IFDEF SupportsUnicode}
    procedure DisplayMessage(Msg: string); overload;
    {$ENDIF}

    // Stopwatch functions. Function GetElapsedTime() returns time elapsed
    // since the latest StartTimer() invocation in millisecods.
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
    function FullFileName(FileName: ShortString): ShortString;
    function FileSource(Path, FileName: ShortString): ShortString;

    property Caption: string read FCaption write SetCaption;
    property WaitMode: Boolean read GetWaitMode write SetWaitMode;
    property RenderingBufferWindow: TAggRenderingBuffer read FRenderingBufferWindow;
    property RenderingBufferImage[Index: Cardinal]: TAggRenderingBuffer read
      GetRenderingBufferImage;

    property ImageExtension: ShortString read GetImageExtension;

    property ControlContainer: TControlContainer read FCtrls;

    // The very same parameters that were used in the constructor
    property PixelFormat: TPixelFormat read FPixelFormat;
    property FlipY: Boolean read FFlipY;
    property BitsPerPixel: Cardinal read FBitsPerPixel;
    property WindowFlags: TWindowFlags read FWindowFlags;

    property Width: Double read GetWidth;
    property Height: Double read GetHeight;
    property InitialWidth: Integer read FInitialWidth;
    property InitialHeight: Integer read FInitialHeight;
  end;

implementation


{ TControlContainer }

constructor TControlContainer.Create;
begin
  FNumControls := 0;
  FCurrentControl := -1;
  inherited;
end;

procedure TControlContainer.Add(C: TAggCustomAggControl);
begin
  if FNumControls < CMaxControl then
  begin
    FControl[FNumControls] := C;

    Inc(FNumControls);
  end;
end;

procedure TControlContainer.Clear;
begin
  FNumControls := 0;
  FCurrentControl := -1;
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

function TControlContainer.SetCurrent(X, Y: Double): Boolean;
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
  FPixelMapWindow := TPixelMap.Create;

  for I := 0 to MaxImages - 1 do
    FPixelMapImage[I] := TPixelMap.Create;

  FPixelFormat := Format;
  FSystemPixelFormat := pfUndefined;

  FFlipY := FlipY;
  FBitsPerPixel := 0;
  FSysBitsPerPixel := 0;
  FHwnd := 0;

  FLastTranslatedKey := kcNone;

  FCurX := 0;
  FCurY := 0;

  FInputFlags := [];
  FRedrawFlag := True;
  FCurrentDC := 0;

  FillChar(FKeymap[0], SizeOf(FKeymap), 0);

  FKeymap[VK_PAUSE] := kcPause;
  FKeymap[VK_CLEAR] := kcClear;

  FKeymap[VK_NUMPAD0] := kcPad0;
  FKeymap[VK_NUMPAD1] := kcPad1;
  FKeymap[VK_NUMPAD2] := kcPad2;
  FKeymap[VK_NUMPAD3] := kcPad3;
  FKeymap[VK_NUMPAD4] := kcPad4;
  FKeymap[VK_NUMPAD5] := kcPad5;
  FKeymap[VK_NUMPAD6] := kcPad6;
  FKeymap[VK_NUMPAD7] := kcPad7;
  FKeymap[VK_NUMPAD8] := kcPad8;
  FKeymap[VK_NUMPAD9] := kcPad9;
  FKeymap[VK_DECIMAL] := kcPadPeriod;
  FKeymap[VK_DIVIDE] := kcPadDivide;
  FKeymap[VK_MULTIPLY] := kcPadMultiply;
  FKeymap[VK_SUBTRACT] := kcPadMinus;
  FKeymap[VK_ADD] := kcPadPlus;

  FKeymap[VK_UP] := kcUp;
  FKeymap[VK_DOWN] := kcDown;
  FKeymap[VK_RIGHT] := kcRight;
  FKeymap[VK_LEFT] := kcLeft;
  FKeymap[VK_INSERT] := kcInsert;
  FKeymap[VK_DELETE] := kcDelete;
  FKeymap[VK_HOME] := kcHome;
  FKeymap[VK_END] := kcEnd;
  FKeymap[VK_PRIOR] := kcPageUp;
  FKeymap[VK_NEXT] := kcPageDown;

  FKeymap[VK_F1] := kcF1;
  FKeymap[VK_F2] := kcF2;
  FKeymap[VK_F3] := kcF3;
  FKeymap[VK_F4] := kcF4;
  FKeymap[VK_F5] := kcF5;
  FKeymap[VK_F6] := kcF6;
  FKeymap[VK_F7] := kcF7;
  FKeymap[VK_F8] := kcF8;
  FKeymap[VK_F9] := kcF9;
  FKeymap[VK_F10] := kcF10;
  FKeymap[VK_F11] := kcF11;
  FKeymap[VK_F12] := kcF12;
  FKeymap[VK_F13] := kcF13;
  FKeymap[VK_F14] := kcF14;
  FKeymap[VK_F15] := kcF15;

  FKeymap[VK_NUMLOCK] := kcNumlock;
  FKeymap[VK_CAPITAL] := kcCapslock;
  FKeymap[VK_SCROLL] := kcScrollLock;

  case FPixelFormat of
    pfbw:
      begin
        FSystemPixelFormat := pfbw;
        FBitsPerPixel := 1;
        FSysBitsPerPixel := 1;
      end;

    pfGray8:
      begin
        FSystemPixelFormat := pfBgr24; // pix_format_gray8;{hack}
        FBitsPerPixel := 8;
        FSysBitsPerPixel := 24; // 8;
      end;

    pfGray16:
      begin
        FSystemPixelFormat := pfGray8;
        FBitsPerPixel := 16;
        FSysBitsPerPixel := 8;
      end;

    pfRgb565, pfRgb555:
      begin
        FSystemPixelFormat := pfRgb555;
        FBitsPerPixel := 16;
        FSysBitsPerPixel := 16;
      end;

    pfRgbAAA, pfBgrAAA, pfRgbBBA, pfBgrABB:
      begin
        FSystemPixelFormat := pfBgr24;
        FBitsPerPixel := 32;
        FSysBitsPerPixel := 24;
      end;

    pfRgb24, pfBgr24:
      begin
        FSystemPixelFormat := pfBgr24;
        FBitsPerPixel := 24;
        FSysBitsPerPixel := 24;
      end;

    pfRgb48, pfBgr48:
      begin
        FSystemPixelFormat := pfBgr24;
        FBitsPerPixel := 48;
        FSysBitsPerPixel := 24;
      end;

    pfBgra32, pfAbgr32, pfArgb32, pfRgba32:
      begin
        FSystemPixelFormat := pfBgra32;
        FBitsPerPixel := 32;
        FSysBitsPerPixel := 32;
      end;

    pfBgra64, pfAbgr64, pfArgb64, pfRgba64:
      begin
        FSystemPixelFormat := pfBgra32;
        FBitsPerPixel := 64;
        FSysBitsPerPixel := 32;
      end;
  end;

  QueryPerformanceFrequency(GSwFreq); { hack }
  QueryPerformanceCounter(GSwStart);
end;

destructor TPlatformSpecific.Destroy;
var
  Index: Cardinal;
begin
  FPixelMapWindow.Free;

  for Index := 0 to MaxImages - 1 do
    FPixelMapImage[Index].Free;

  inherited;
end;

procedure TPlatformSpecific.CreatePixelMap(Width, Height: Cardinal;
  RenderingBuffer: TAggRenderingBuffer);
begin
  FPixelMapWindow.Build(Width, Height, FBitsPerPixel);

  if FFlipY then
    RenderingBuffer.Attach(FPixelMapWindow.Buffer, FPixelMapWindow.Width,
      FPixelMapWindow.Height, FPixelMapWindow.Stride)
  else
    RenderingBuffer.Attach(FPixelMapWindow.Buffer, FPixelMapWindow.Width,
      FPixelMapWindow.Height, -FPixelMapWindow.Stride)
end;

procedure ConvertPixelMap(Dst, Src: TAggRenderingBuffer; Format: TPixelFormat);
begin
  case Format of
    pfGray8:
      ColorConversion(Dst, Src, ColorConversionGray8ToBgr24);

    pfGray16:
      ColorConversion(Dst, Src, ColorConversionGray16ToGray8);

    pfRgb565:
      ColorConversion(Dst, Src, ColorConversionRgb565ToRgb555);

    pfRgbAAA:
      ColorConversion(Dst, Src, ColorConversionRgbAAAToBgr24);

    pfBgrAAA:
      ColorConversion(Dst, Src, ColorConversionBgrAAAToBgr24);

    pfRgbBBA:
      ColorConversion(Dst, Src, ColorConversionRgbBBAToBgr24);

    pfBgrABB:
      ColorConversion(Dst, Src, ColorConversionBgrABBToBgr24);

    pfRgb24:
      ColorConversion(Dst, Src, ColorConversionRgb24ToBgr24);

    pfRgb48:
      ColorConversion(Dst, Src, ColorConversionRgb48ToBgr24);

    pfBgr48:
      ColorConversion(Dst, Src, ColorConversionBgr48ToBgr24);

    pfAbgr32:
      ColorConversion(Dst, Src, ColorConversionAbgr32ToBgra32);

    pfArgb32:
      ColorConversion(Dst, Src, ColorConversionArgb32ToBgra32);

    pfRgba32:
      ColorConversion(Dst, Src, ColorConversionRgba32ToBgra32);

    pfBgra64:
      ColorConversion(Dst, Src, ColorConversionBgra64ToBgra32);

    pfAbgr64:
      ColorConversion(Dst, Src, ColorConversionAbgr64ToBgra32);

    pfArgb64:
      ColorConversion(Dst, Src, ColorConversionArgb64ToBgra32);

    pfRgba64:
      ColorConversion(Dst, Src, ColorConversionRgba64ToBgra32);
  end;
end;

procedure TPlatformSpecific.DisplayPixelMap(Dc: HDC; Src: TAggRenderingBuffer);
var
  TempPixelMap: TPixelMap;
  TempRenderingBuffer: TAggRenderingBuffer;
begin
  if FSystemPixelFormat = FPixelFormat then
    FPixelMapWindow.Draw(Dc)
  else
  begin
    TempPixelMap := TPixelMap.Create;
    try
      TempPixelMap.Build(FPixelMapWindow.Width, FPixelMapWindow.Height,
        FSysBitsPerPixel);

      TempRenderingBuffer := TAggRenderingBuffer.Create;

      if FFlipY then
        TempRenderingBuffer.Attach(TempPixelMap.Buffer, TempPixelMap.Width,
          TempPixelMap.Height, TempPixelMap.Stride)
      else
        TempRenderingBuffer.Attach(TempPixelMap.Buffer, TempPixelMap.Width,
          TempPixelMap.Height, -TempPixelMap.Stride);

      ConvertPixelMap(TempRenderingBuffer, Src, FPixelFormat);
      TempPixelMap.Draw(Dc);

      TempRenderingBuffer.Free;
    finally
      TempPixelMap.Free;
    end;
  end;
end;

function TPlatformSpecific.LoadPixelMap(Fn: ShortString; Index: Cardinal;
  Dst: TAggRenderingBuffer): Boolean;
var
  TempPixelMap: TPixelMap;
  TempRenderingBuffer: TAggRenderingBuffer;
begin
  Result := False;

  TempPixelMap := TPixelMap.Create;
  try
    if not TempPixelMap.LoadFromBitmap(Fn) then
      Exit;

    TempRenderingBuffer := TAggRenderingBuffer.Create;

    if FFlipY then
      TempRenderingBuffer.Attach(TempPixelMap.Buffer,
        TempPixelMap.Width, TempPixelMap.Height, TempPixelMap.Stride)
    else
      TempRenderingBuffer.Attach(TempPixelMap.Buffer, TempPixelMap.Width,
        TempPixelMap.Height, -TempPixelMap.Stride);

    FPixelMapImage[Index].Build(TempPixelMap.Width, TempPixelMap.Height,
      FBitsPerPixel, 0);

    if FFlipY then
      Dst.Attach(FPixelMapImage[Index].Buffer,
        FPixelMapImage[Index].Width, FPixelMapImage[Index].Height,
        FPixelMapImage[Index].Stride)
    else
      Dst.Attach(FPixelMapImage[Index].Buffer,
        FPixelMapImage[Index].Width, FPixelMapImage[Index].Height,
        -FPixelMapImage[Index].Stride);

    case FPixelFormat of
      pfGray8:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToGray8);
        end;

      pfGray16:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToGray16);
        end;

      pfRgb555:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToRgb555);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgb555);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToRgb555);
        end;

      pfRgb565:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToRgb565);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgb565);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToRgb565);
        end;

      pfRgb24:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToRgb24);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgb24);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToRgb24);
        end;

      pfBgr24:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToBgr24);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToBgr24);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToBgr24);
        end;

      pfRgb48:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgb48);
        end;

      pfBgr48:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToBgr48);
        end;

      pfAbgr32:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToAbgr32);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToAbgr32);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToAbgr32);
        end;

      pfArgb32:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToArgb32);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToArgb32);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToArgb32);
        end;

      pfBgra32:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToBgra32);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToBgra32);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToBgra32);
        end;

      pfRgba32:
        case TempPixelMap.BitsPerPixel of
          16:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionRgb555ToRgba32);
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgba32);
          32:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgra32ToRgba32);
        end;

      pfAbgr64:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToAbgr64);
        end;

      pfArgb64:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToArgb64);
        end;

      pfBgra64:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToBgra64);
        end;

      pfRgba64:
        case TempPixelMap.BitsPerPixel of
          24:
            ColorConversion(Dst, TempRenderingBuffer,
              ColorConversionBgr24ToRgba64);
        end;
    end;

    TempRenderingBuffer.Free;

    Result := True;
  finally
    TempPixelMap.Free;
  end;
end;

function TPlatformSpecific.SavePixelMap(Fn: ShortString; Index: Cardinal;
  Src: TAggRenderingBuffer): Boolean;
var
  TempPixelMap: TPixelMap;
  TempRenderingBuffer: TAggRenderingBuffer;
begin
  if FSystemPixelFormat = FPixelFormat then
  begin
    Result := FPixelMapImage[Index].SaveAsBitmap(Fn);

    Exit;
  end;

  TempPixelMap := TPixelMap.Create;
  try
    TempPixelMap.Build(FPixelMapImage[Index].Width,
      FPixelMapImage[Index].Height, FSysBitsPerPixel);

    TempRenderingBuffer.Create;

    if FFlipY then
      TempRenderingBuffer.Attach(TempPixelMap.Buffer, TempPixelMap.Width,
        TempPixelMap.Height, TempPixelMap.Stride)
    else
      TempRenderingBuffer.Attach(TempPixelMap.Buffer, TempPixelMap.Width,
        TempPixelMap.Height, -TempPixelMap.Stride);

    ConvertPixelMap(TempRenderingBuffer, Src, FPixelFormat);

    Result := TempPixelMap.SaveAsBitmap(Fn);

    TempRenderingBuffer.Free;
  finally
    TempPixelMap.Free;
  end;
end;

function TPlatformSpecific.Translate(Keycode: Cardinal): Cardinal;
begin
  if Keycode > 255 then
    FLastTranslatedKey := kcNone
  else
    FLastTranslatedKey := FKeymap[Keycode];
end;


{ TPlatformSupport }

constructor TPlatformSupport.Create(PixelFormat: TPixelFormat;
  FlipY: Boolean; Specific: TPlatformSpecific = nil);
var
  I: Cardinal;
begin
  if Specific <> nil then
    FSpecific := Specific
  else
    FSpecific := TPlatformSpecific.Create(PixelFormat, FlipY);

  FCtrls := TControlContainer.Create;
  FRenderingBufferWindow := TAggRenderingBuffer.Create;

  for I := 0 to MaxImages - 1 do
    FRenderingBufferImages[I] := TAggRenderingBuffer.Create;

  FResizeMatrix := TAggTransAffine.Create;

  FPixelFormat := PixelFormat;

  FBitsPerPixel := FSpecific.FBitsPerPixel;

  FWindowFlags := [];
  FWaitMode := True;
  FFlipY := FlipY;

  FInitialWidth := 10;
  FInitialHeight := 10;

  FCaption := 'Anti-Grain Geometry Application';
end;

destructor TPlatformSupport.Destroy;
var
  I: Cardinal;
begin
  FSpecific.Free;
  FCtrls.Free;
  FResizeMatrix.Free;

  FRenderingBufferWindow.Free;

  for I := 0 to MaxImages - 1 do
    FRenderingBufferImages[I].Free;

  inherited;
end;

procedure TPlatformSupport.SetCaption(Value: string);
begin
  FCaption := Value;

  if WindowHandle <> 0 then
{$IFDEF FPC}
    SetWindowText(WindowHandle, @FCaption[1]);
{$ELSE}
    SetWindowText(WindowHandle, FCaption[1]);
{$ENDIF}
end;

function TPlatformSupport.LoadImage(Index: Cardinal; File_: ShortString): Boolean;
var
  F: file;
begin
  if Index < MaxImages then
  begin
    File_ := File_ + GetImageExtension;

    AssignFile(F, File_);
    Reset(F, 1);

    if Ioresult <> 0 then
      File_ := 'bmp\' + File_;

    Close(F);

    Result := FSpecific.LoadPixelMap(File_, Index, FRenderingBufferImages[Index]);
  end
  else
    Result := True;
end;

function TPlatformSupport.SaveImage(Index: Cardinal; File_: ShortString): Boolean;
begin
  if Index < MaxImages then
    Result := FSpecific.SavePixelMap(File_, Index, FRenderingBufferImages[Index])
  else
    Result := True;
end;

function TPlatformSupport.CreateImage(Index: Cardinal; AWidth: Cardinal = 0;
  AHeight: Cardinal = 0): Boolean;
begin
  if Index < MaxImages then
    with FSpecific do
    begin
      if AWidth = 0 then
        AWidth := FPixelMapWindow.Width;

      if AHeight = 0 then
        AHeight := FPixelMapWindow.Height;

      FPixelMapImage[Index].Build(AWidth, AHeight, FSpecific.FBitsPerPixel);

      if FFlipY then
        FRenderingBufferImages[Index].Attach(FPixelMapImage[Index].Buffer,
          FPixelMapImage[Index].Width, FPixelMapImage[Index].Height,
          FPixelMapImage[Index].Stride)
      else
        FRenderingBufferImages[Index].Attach(FPixelMapImage[Index].Buffer,
          FPixelMapImage[Index].Width, FPixelMapImage[Index].Height,
          -FPixelMapImage[Index].Stride);

      Result := True;

    end
    else
      Result := False;
end;

function GetKeyFlags(Wflags: Integer): TMouseKeyboardFlags;
begin
  Result := [];

  if Wflags and MK_LBUTTON <> 0 then
    Result := Result + [mkfMouseLeft];

  if Wflags and MK_RBUTTON <> 0 then
    Result := Result + [mkfMouseRight];

  if Wflags and MK_SHIFT <> 0 then
    Result := Result + [mkfShift];

  if Wflags and MK_CONTROL <> 0 then
    Result := Result + [mkfCtrl];
end;

function Window_proc(Wnd: HWND; Msg: UINT; WPar: WParam; LPar: LParam)
  : LResult; stdcall;
var
  Ps : TPaintStruct;
  App: TPlatformSupport;
  Ret: LResult;
  Dc, PaintDC: HDC;
  Left, Up, Right, Down: Boolean;
  LPar32: TInt32uAccess;
begin
  App := TPlatformSupport(GetWindowLong(Wnd, GWL_USERDATA));

  if App = nil then
  begin
    if Msg = WM_DESTROY then
    begin
      PostQuitMessage(0);
      Result := 0;
      Exit;
    end;

    Result := DefWindowProc(Wnd, Msg, WPar, LPar);
    Exit;
  end;

  Dc := GetDC(App.WindowHandle);
  App.Specific.FCurrentDC := Dc;
  Ret := 0;
{$IFDEF CPU64}
  LPar32 := TInt64uAccess(LPar).Low;
{$ELSE}
  LPar32 := TInt32uAccess(LPar);
{$ENDIF}

  case Msg of
    WM_CREATE:
      NoP;

    WM_SIZE:
      begin
        App.Specific.CreatePixelMap(Int16(LPar32.Low), Int16(LPar32.High),
          App.RenderingBufferWindow);
        App.SetTransAffineResizing(Int16(LPar32.Low), Int16(LPar32.High));
        App.OnResize(Int16(LPar32.Low), Int16(LPar32.High));
        App.ForceRedraw;
      end;

    WM_ERASEBKGND:
      NoP;

    WM_LBUTTONDOWN:
      begin
        SetCapture(App.WindowHandle);

        App.Specific.FCurX := Int16(LPar32.Low);

        if App.FlipY then
          App.Specific.FCurY := Integer(App.RenderingBufferWindow.Height) -
            Int16(LPar32.High)
        else
          App.Specific.FCurY := Int16(LPar32.High);

        App.Specific.FInputFlags := GetKeyFlags(WPar) + [mkfMouseLeft];
        App.FCtrls.SetCurrent(App.Specific.FCurX, App.Specific.FCurY);

        if App.FCtrls.OnMouseButtonDown(App.Specific.FCurX,
          App.Specific.FCurY) then
        begin
          App.OnControlChange;
          App.ForceRedraw;

        end
        else if App.FCtrls.InRect(App.Specific.FCurX,
          App.Specific.FCurY) then
          if App.FCtrls.SetCurrent(App.Specific.FCurX, App.Specific.FCurY)
          then
          begin
            App.OnControlChange;
            App.ForceRedraw;

          end
          else
        else
          App.OnMouseButtonDown(App.Specific.FCurX,
            App.Specific.FCurY, App.Specific.FInputFlags);
      end;

    WM_LBUTTONUP:
      begin
        ReleaseCapture;

        App.Specific.FCurX := Int16(LPar32.Low);

        if App.FlipY then
          App.Specific.FCurY := Integer(App.RenderingBufferWindow.Height) -
            Int16(LPar32.High)
        else
          App.Specific.FCurY := Int16(LPar32.High);

        App.Specific.FInputFlags := GetKeyFlags(WPar) + [mkfMouseLeft];

        if App.FCtrls.OnMouseButtonUp(App.Specific.FCurX,
          App.Specific.FCurY) then
        begin
          App.OnControlChange;
          App.ForceRedraw;
        end;

        App.OnMouseButtonUp(App.Specific.FCurX, App.Specific.FCurY,
          App.Specific.FInputFlags);
      end;

    WM_RBUTTONDOWN:
      begin
        SetCapture(App.WindowHandle);

        App.Specific.FCurX := Int16(LPar32.Low);

        if App.FlipY then
          App.Specific.FCurY := Integer(App.RenderingBufferWindow.Height) -
            Int16(LPar32.High)
        else
          App.Specific.FCurY := Int16(LPar32.High);

        App.Specific.FInputFlags := GetKeyFlags(WPar) + [mkfMouseRight];

        App.OnMouseButtonDown(App.Specific.FCurX, App.Specific.FCurY,
          App.Specific.FInputFlags);
      end;

    WM_RBUTTONUP:
      begin
        ReleaseCapture;

        App.Specific.FCurX := Int16(LPar32.Low);

        if App.FlipY then
          App.Specific.FCurY := Integer(App.RenderingBufferWindow.Height) -
            Int16(LPar32.High)
        else
          App.Specific.FCurY := Int16(LPar32.High);

        App.Specific.FInputFlags := GetKeyFlags(WPar) + [mkfMouseRight];

        App.OnMouseButtonUp(App.Specific.FCurX, App.Specific.FCurY,
          App.Specific.FInputFlags);
      end;

    WM_MOUSEMOVE:
      begin
        App.Specific.FCurX := Int16(LPar32.Low);

        if App.FlipY then
          App.Specific.FCurY := Integer(App.RenderingBufferWindow.Height) -
            Int16(LPar32.High)
        else
          App.Specific.FCurY := Int16(LPar32.High);
        App.Specific.FInputFlags := GetKeyFlags(WPar);

        if App.FCtrls.OnMouseMove(App.Specific.FCurX,
          App.Specific.FCurY, mkfMouseLeft in App.Specific.FInputFlags) then
        begin
          App.OnControlChange;
          App.ForceRedraw;

        end
        else if not App.FCtrls.InRect(App.Specific.FCurX,
          App.Specific.FCurY) then
          App.OnMouseMove(App.Specific.FCurX, App.Specific.FCurY,
            App.Specific.FInputFlags);
      end;
    WM_SYSKEYDOWN, WM_KEYDOWN:
      begin
        App.Specific.FLastTranslatedKey := kcNone;

        case WPar of
          VK_CONTROL:
            App.Specific.FInputFlags := App.Specific.FInputFlags + [mkfCtrl];

          VK_SHIFT:
            App.Specific.FInputFlags := App.Specific.FInputFlags + [mkfShift];

          VK_F4:
            if LPar and $20000000 <> 0 then
              App.Quit
            else
              App.Specific.Translate(WPar);

        else
          App.Specific.Translate(WPar);
        end;

        if App.Specific.FLastTranslatedKey <> kcNone then
        begin
          Left := False;
          Up := False;
          Right := False;
          Down := False;

          case App.Specific.FLastTranslatedKey of
            kcLeft:
              Left := True;

            kcUp:
              Up := True;

            kcRight:
              Right := True;

            kcDown:
              Down := True;

            kcF2:
              begin
                App.CopyWindowToImage(MaxImages - 1);
                App.SaveImage(MaxImages - 1, 'screenshot.bmp');
              end;
          end;

          if wfProcessAllKeys in App.WindowFlags then
            App.OnKey(App.Specific.FCurX, App.Specific.FCurY,
              Cardinal(App.Specific.FLastTranslatedKey),
              App.Specific.FInputFlags)

          else if App.FCtrls.OnArrowKeys(Left, Right, Down, Up) then
          begin
            App.OnControlChange;
            App.ForceRedraw;
          end
          else
            App.OnKey(App.Specific.FCurX, App.Specific.FCurY,
              Cardinal(App.Specific.FLastTranslatedKey),
              App.Specific.FInputFlags);
        end;
      end;

    WM_SYSKEYUP, WM_KEYUP:
      begin
        App.Specific.FLastTranslatedKey := kcNone;

        case WPar of
          VK_CONTROL:
            App.Specific.FInputFlags := App.Specific.FInputFlags - [mkfCtrl];

          VK_SHIFT:
            App.Specific.FInputFlags := App.Specific.FInputFlags - [mkfShift];
        end;
      end;

    WM_CHAR, WM_SYSCHAR:
      if App.Specific.FLastTranslatedKey = kcNone then
        App.OnKey(App.Specific.FCurX, App.Specific.FCurY, WPar,
          App.Specific.FInputFlags);

    WM_PAINT:
      begin
        PaintDC := BeginPaint(Wnd, Ps);

        App.Specific.FCurrentDC := PaintDC;

        if App.Specific.FRedrawFlag then
        begin
          App.OnDraw;

          App.Specific.FRedrawFlag := False;
        end;

        App.Specific.DisplayPixelMap(PaintDC, App.RenderingBufferWindow);
        App.OnPostDraw(Pointer(@PaintDC));

        App.Specific.FCurrentDC := 0;

        EndPaint(Wnd, Ps);
      end;

    WM_COMMAND:
      NoP;

    WM_DESTROY:
      PostQuitMessage(0);

  else
    Ret := DefWindowProc(Wnd, Msg, WPar, LPar);
  end;

  App.Specific.FCurrentDC := 0;

  ReleaseDC(App.WindowHandle, Dc);

  Result := Ret;
end;

function TPlatformSupport.Init(AWidth, AHeight: Cardinal;
  Flags: TWindowFlags): Boolean;
var
  Wc : WNDCLASS;
  Rct: TRect;
  Wflags: Integer;
begin
  Result := False;

  if FSpecific.FSystemPixelFormat = pfUndefined then
    Exit;

  FWindowFlags := Flags;

  Wflags := CS_OWNDC or CS_VREDRAW or CS_HREDRAW;

  Wc.LpszClassName := 'AGGAppClass';
  Wc.LpfnWndProc := @Window_proc;
  Wc.Style := Wflags;
  Wc.HInstance := HInstance;
  Wc.HIcon := LoadIcon(0, IDI_APPLICATION);
  Wc.HCursor := LoadCursor(0, IDC_ARROW);
  Wc.HbrBackground := COLOR_WINDOW + 1;
  Wc.LpszMenuName := 'AGGAppMenu';
  Wc.CbClsExtra := 0;
  Wc.CbWndExtra := 0;

  RegisterClass(Wc);

  Wflags := WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or WS_MINIMIZEBOX;

  if wfResize in FWindowFlags then
    Wflags := Wflags or WS_THICKFRAME or WS_MAXIMIZEBOX;

  FSpecific.FHwnd := CreateWindow('AGGAppClass', @FCaption[1], Wflags, 10,
    10, AWidth, AHeight, 0, 0, HInstance, 0);

  if FSpecific.FHwnd = 0 then
    Exit;

  GetClientRect(FSpecific.FHwnd, Rct);

  MoveWindow(FSpecific.FHwnd, // handle to window
    10, // horizontal position
    10, // vertical position
    AWidth + (AWidth - (Rct.Right - Rct.Left)),
    AHeight + (AHeight - (Rct.Bottom - Rct.Top)), False);

  SetWindowLong(FSpecific.FHwnd, GWL_USERDATA, PtrComp(Self));

  FSpecific.CreatePixelMap(AWidth, AHeight, FRenderingBufferWindow);

  FInitialWidth := AWidth;
  FInitialHeight := AHeight;

  OnInit;

  FSpecific.FRedrawFlag := True;

  ShowWindow(FSpecific.FHwnd, SW_SHOW);

  Result := True;
end;

function TPlatformSupport.Run: Integer;
var
  Msg: TMsg;
begin
  repeat
    if FWaitMode then
    begin
      if not GetMessage(Msg, 0, 0, 0) then
        Break;

      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end
    else if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
    begin
      TranslateMessage(Msg);

      if Msg.Message = WM_QUIT then
        Break;

      DispatchMessage(Msg);
    end
    else
      OnIdle;
  until False;

  Result := Msg.WParam;
end;

procedure TPlatformSupport.Quit;
begin
  if FSpecific.FHwnd <> 0 then
    DestroyWindow(FSpecific.FHwnd);

  PostQuitMessage(0);
end;

function TPlatformSupport.GetWaitMode: Boolean;
begin
  Result := FWaitMode;
end;

procedure TPlatformSupport.SetWaitMode(Value: Boolean);
begin
  FWaitMode := Value;
end;

procedure TPlatformSupport.ForceRedraw;
begin
  FSpecific.FRedrawFlag := True;

  InvalidateRect(FSpecific.FHwnd, 0, False);
end;

procedure TPlatformSupport.UpdateWindow;
var
  Dc: HDC;
begin
  Dc := GetDC(FSpecific.FHwnd);

  FSpecific.DisplayPixelMap(Dc, FRenderingBufferWindow);

  ReleaseDC(FSpecific.FHwnd, Dc);
end;

function TPlatformSupport.GetRenderingBufferImage(Index: Cardinal): TAggRenderingBuffer;
begin
  Result := FRenderingBufferImages[Index];
end;

function TPlatformSupport.GetImageExtension;
begin
  Result := '.bmp';
end;

procedure TPlatformSupport.CopyImageToWindow(Index: Cardinal);
begin
  if (Index < MaxImages) and (RenderingBufferImage[Index].Buffer <> nil) then
    RenderingBufferWindow.CopyFrom(RenderingBufferImage[Index]);
end;

procedure TPlatformSupport.CopyWindowToImage(Index: Cardinal);
begin
  if Index < MaxImages then
  begin
    CreateImage(Index, RenderingBufferWindow.Width,
      RenderingBufferWindow.Height);
    RenderingBufferImage[Index].CopyFrom(RenderingBufferWindow);
  end;
end;

procedure TPlatformSupport.CopyImageToImage(IndexTo, IndexFrom: Cardinal);
begin
  if (IndexFrom < MaxImages) and (IndexTo < MaxImages) and
    (RenderingBufferImage[IndexFrom].Buffer <> nil) then
  begin
    CreateImage(IndexTo, RenderingBufferImage[IndexFrom].Width,
      RenderingBufferImage[IndexFrom].Height);

    RenderingBufferImage[IndexTo].CopyFrom(RenderingBufferImage[IndexFrom]);
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

procedure TPlatformSupport.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
end;

procedure TPlatformSupport.OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
end;

procedure TPlatformSupport.OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
end;

procedure TPlatformSupport.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
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

procedure TPlatformSupport.AddControl;
begin
  FCtrls.Add(C);

  C.Transform(FResizeMatrix);
end;

procedure TPlatformSupport.SetTransAffineResizing;
var
  Vp: TAggTransViewport;
  Ts: TAggTransAffineScaling;
begin
  if wfKeepAspectRatio in FWindowFlags then
  begin
    // sx := Double(Width) / Double(FInitialWidth);
    // sy := Double(Height) / Double(FInitialHeight);
    // if (sy < sx) then
    //  sx = sy;
    // FResizeMatrix = TransAffineScaling(sx, sx);

    Vp := TAggTransViewport.Create;
    try
      Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

      Vp.DeviceViewport(0, 0, AWidth, AHeight);
      Vp.WorldViewport(0, 0, FInitialWidth, FInitialHeight);

      Vp.ToAffine(FResizeMatrix);
    finally
      Vp.Free;
    end;
  end
  else
  begin
    Ts := TAggTransAffineScaling.Create(AWidth / FInitialWidth,
      AHeight / FInitialHeight);
    try
      FResizeMatrix.Assign(Ts);
    finally
      Ts.Free;
    end;
  end;
end;

function TPlatformSupport.GetTransAffineResizing;
begin
  Result := FResizeMatrix;
end;

function TPlatformSupport.GetWidth: Double;
begin
  Result := FRenderingBufferWindow.Width;
end;

function TPlatformSupport.GetWindowHandle: HWND;
begin
  Result := FSpecific.WindowHandle;
end;

function TPlatformSupport.GetHeight: Double;
begin
  Result := FRenderingBufferWindow.Height;
end;

function TPlatformSupport.GetRawDisplayHandler;
begin
  Result := @FSpecific.FCurrentDC;
end;

procedure TPlatformSupport.DisplayMessage(Msg: PAnsiChar);
begin
  MessageBoxA(FSpecific.FHwnd, Msg, 'AGG Message', MB_OK);
end;

procedure TPlatformSupport.DisplayMessage(Msg: AnsiString);
begin
  MessageBoxA(FSpecific.FHwnd, PAnsiChar(Msg), 'AGG Message', MB_OK);
end;

{$IFDEF SupportsUnicode}
procedure TPlatformSupport.DisplayMessage(Msg: string);
begin
  MessageBoxW(FSpecific.FHwnd, PWideChar(Msg), 'AGG Message', MB_OK);
end;
{$ENDIF}

procedure TPlatformSupport.StartTimer;
begin
  QueryPerformanceCounter( { FSpecific. } GSwStart); { hack }
end;

function TPlatformSupport.GetElapsedTime: Double;
var
  Stop: TLargeInteger;

begin
  QueryPerformanceCounter(Stop);

  Result := (Stop - { FSpecific. } GSwStart) * 1000.0 /
  { FSpecific. } GSwFreq; { hack }
end;

function TPlatformSupport.FullFileName(FileName: ShortString): ShortString;
begin
  Result := FileName;
end;

function TPlatformSupport.FileSource(Path, FileName: ShortString): ShortString;
var
  F: file;
  E: Integer;
begin
  Result := FileName;

  E := Ioresult;

  AssignFile(F, Result);
  Reset(F, 1);

  if Ioresult <> 0 then
    Result := Path + '\' + FileName;

  Close(F);

  E := Ioresult;
end;

end.
