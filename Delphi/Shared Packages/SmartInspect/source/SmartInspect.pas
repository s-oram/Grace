//
// <!-- Copyright (C) 2003-2011 Gurock Software GmbH. All rights reserved. -->
//
// <summary>
//   Contains the definitions and implementations for all types of the
//   SmartInspect Delphi library. To get started, please see the two most
//   important classes of the SmartInspect concept: TSmartInspect and
//   TSiSession.
// </summary>

unit SmartInspect;

interface

{$I 'SmartInspect.inc'}

uses
  Classes,
  SysUtils,
  DateUtils,
  {$IFNDEF SI_DISABLE_DB} DB, {$ENDIF}
  {$IFNDEF SI_DISABLE_RTTI} TypInfo, {$ENDIF}
  SyncObjs,
  Windows,
  Graphics,
  Contnrs,
  {$IFNDEF SI_DISABLE_GRAPHIC} Jpeg, {$ENDIF}
  {$IFNDEF SI_DISABLE_ENCRYPT} SiEncryption, {$ENDIF}
  SiWinSock2;

type

  { Declare UnicodeString as WideString for Delphi versions older than
    Delphi 2009 in order to be able to use the now-standard UnicodeString
    type in older versions as well. SmartInspect has supported Unicode
    since SmartInspect v2.0 by using the WideString type and UTF8 for the
    internal packet format. What has changed since then is that the
    SmartInspect library now uses UnicodeString instead of WideString
    (or String) with the exception of a few places where

    a) Delphi only allows the usage of String (e.g. in the Message
       property of custom exceptions). In Delphi 2009 or newer, this
       automatically uses the new UnicodeString as well, however, in
       older versions we are restricted to AnsiString in those cases.

    b) The underlying data semantics only support the usage of
       AnsiString, e.g. as type for the host name in the TSiTcpClient
       class.

    c) Log methods are designed to log a value of a specific data type,
       e.g. LogString and LogWideString still accept a String and
       WideString value parameter, respectively. }

{$IFNDEF DELPHI2009_OR_HIGHER}
  UnicodeString = type WideString;
  PUnicodeString = type PWideString;
{$ENDIF}

  TSiUTF8String = type AnsiString;

  // <summary>
  //   Used internally to report any kind of error.
  // </summary>
  // <remarks>
  //   This is the base class for several exceptions which are mainly
  //   used for internal error reporting. However, it can be useful
  //   to have a look at its derived classes ESiLoadConnectionsError,
  //   ESiLoadConfigurationError and ESiProtocolError, which provide
  //   additional information about occurred errors besides the normal
  //   exception message.
  //
  //   This can be useful if you need to obtain more information about
  //   a particular error in the TSmartInspect.OnError event.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  ESmartInspectError = class(Exception);

  // <summary>
  //   Used to report errors concerning the TSmartInspect.LoadConnections
  //   method.
  // </summary>
  // <remarks>
  //   This exception is used to report errors concerning the
  //   TSmartInspect.LoadConnections method. This method is able to load a
  //   <link TSmartInspect.Connections, connections string> from a file.
  //   Therefore errors can occur when trying to load a connections string
  //   from an inexistent file or when the file can not be opened for reading,
  //   for example.
  //
  //   If such an error occurs, an instance of this class will be passed to
  //   the TSmartInspect.OnError event. Please note, that, if a connections
  //   string can be read correctly, but is found to be invalid then this
  //   exception type will not be used. The TSmartInspect.LoadConnections
  //   method will use the ESiInvalidConnectionsError in this case.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>
  // <example>
  // <code>
  // program OnError;
  // {$APPTYPE CONSOLE}
  //
  // uses
  //   SysUtils, SiAuto, SmartInspect;
  //
  // type
  //   TEventTest = class(TObject)
  //   public
  //     procedure OnError(ASender: TSmartInspect; AException: Exception);
  //   end;
  //
  // procedure TEventTest.OnError(ASender: TSmartInspect;
  //   AException: Exception);
  // var
  //   LException: ESiLoadConnectionsError;
  // begin
  //   WriteLn(AException.Message);
  //
  //   if AException is ESiLoadConnectionsError then
  //   begin
  //     LException := ESiLoadConnectionsError(AException);
  //
  //     // A ESiLoadConnectionsError provides additional information
  //     // about the occurred error besides the normal exception message.
  //     // It contains the name of the file which caused the exception
  //     // while trying to read the connections string from it.
  //
  //     WriteLn(LException.FileName);
  //   end;
  // end;
  //
  // var
  //   EventTest: TEventTest;
  //
  // begin
  //   EventTest := TEventTest.Create;
  //   try
  //     // Register our event handler for the error event.
  //     Si.OnError := EventTest.OnError;
  //
  //     // Force an error event by passing a name of a file
  //     // which does not exist to the LoadConnections method.
  //     Si.LoadConnections('Inexistent.sic');
  //   finally
  //     EventTest.Free;
  //   end;
  // end.
  // </code>
  // </example>

  ESiLoadConnectionsError = class(ESmartInspectError)
  private
    FFileName: UnicodeString;
  public

    // <summary>
    //   Creates and initializes an ESiLoadConnectionsError instance.
    // </summary>
    // <param name="AFileName">
    //   The name of the file which caused this exception.
    // </param>
    // <param name="AMessage">
    //   The error message which describes the exception.
    // </param>

    constructor Create(const AFileName: UnicodeString; const AMessage: String);

    // <summary>
    //   Specifies the name of the file which caused this exception
    //   while trying to load the connections string from it.
    // </summary>

    property FileName: UnicodeString read FFileName write FFileName;
  end;

  // <summary>
  //   Used to report errors concerning the TSmartInspect.LoadConfiguration
  //   method.
  // </summary>
  // <remarks>
  //   This exception is used to report errors concerning the
  //   TSmartInspect.LoadConfiguration method. This method is able to load a
  //   SmartInspect configuration from a file. Therefore errors can occur
  //   when trying to load a configuration from an inexistent file or when
  //   the file can not be opened for reading, for example.
  //
  //   If such an error occurs, an instance of this class will be passed to
  //   the TSmartInspect.OnError event. Please note, that, if a connections
  //   string can be read correctly, but is found to be invalid then this
  //   exception type will not be used. The TSmartInspect.LoadConfiguration
  //   method will use the ESiInvalidConnectionsError in this case.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>
  // <example>
  // <code>
  // program OnError;
  // {$APPTYPE CONSOLE}
  //
  // uses
  //   SysUtils, SiAuto, SmartInspect;
  //
  // type
  //   TEventTest = class(TObject)
  //   public
  //     procedure OnError(ASender: TSmartInspect; AException: Exception);
  //   end;
  //
  // procedure TEventTest.OnError(ASender: TSmartInspect;
  //   AException: Exception);
  // var
  //   LException: ESiLoadConfigurationError;
  // begin
  //   WriteLn(AException.Message);
  //
  //   if AException is ESiLoadConfigurationError then
  //   begin
  //     LException := ESiLoadConfigurationError(AException);
  //
  //     // A ESiLoadConfigurationError provides additional information
  //     // about the occurred error besides the normal exception message.
  //     // It contains the name of the file which caused the exception
  //     // while trying to read the configuration from it.
  //
  //     WriteLn(LException.FileName);
  //   end;
  // end;
  //
  // var
  //   EventTest: TEventTest;
  //
  // begin
  //   EventTest := TEventTest.Create;
  //   try
  //     // Register our event handler for the error event.
  //     Si.OnError := EventTest.OnError;
  //
  //     // Force an error event by passing a name of a file
  //     // which does not exist to the LoadConfiguration method.
  //     Si.LoadConfiguration('Inexistent.sic');
  //   finally
  //     EventTest.Free;
  //   end;
  // end.
  // </code>
  // </example>

  ESiLoadConfigurationError = class(ESmartInspectError)
  private
    FFileName: UnicodeString;
  public

    // <summary>
    //   Creates and initializes an ESiLoadConfigurationError instance.
    // </summary>
    // <param name="AFileName">
    //   The name of the file which caused this exception.
    // </param>
    // <param name="AMessage">
    //   The error message which describes the exception.
    // </param>

    constructor Create(const AFileName: UnicodeString; const AMessage: String);

    // <summary>
    //   Specifies the name of the file which caused this exception
    //   while trying to load the configuration from it.
    // </summary>

    property FileName: UnicodeString read FFileName write FFileName;
  end;

  // <summary>
  //   Used to report any errors concerning the protocol classes.
  // </summary>
  // <remarks>
  //   This exception can be thrown by several Protocol methods,
  //   like the TSiProtocol.Connect, TSiProtocol.Disconnect or
  //   TSiProtocol.WritePacket methods when an error has occurred.
  //
  //   See below for an example on how to obtain detailed information
  //   in the TSmartInspect.OnError event about the protocol which
  //   caused the error.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>
  // <example>
  // <code>
  // program OnError;
  // {$APPTYPE CONSOLE}
  //
  // uses
  //   SysUtils, SiAuto, SmartInspect;
  //
  // type
  //   TEventTest = class(TObject)
  //   public
  //     procedure OnError(ASender: TSmartInspect; AException: Exception);
  //   end;
  //
  // procedure TEventTest.OnError(ASender: TSmartInspect;
  //   AException: Exception);
  // var
  //   LException: ESiProtocolError;
  // begin
  //   WriteLn(AException.Message);
  //
  //   if AException is ESiProtocolError then
  //   begin
  //     LException := ESiProtocolError(AException);
  //
  //     // A ESiProtocolError provides additional information
  //     // about the occurred error besides the normal exception
  //     // message, like, for example, the name of the protocol
  //     // which caused this error.
  //
  //     WriteLn(LException.ProtocolName);
  //     WriteLn(LException.ProtocolOptions);
  //   end;
  // end;
  //
  // var
  //   EventTest: TEventTest;
  //
  // begin
  //   EventTest := TEventTest.Create;
  //   try
  //     // Register our event handler for the error event.
  //     Si.OnError := EventTest.OnError;
  //
  //     // And force a connection error.
  //     Si.Connections := 'file(filename=c:\)';
  //     Si.Enabled := true;
  //   finally
  //     EventTest.Free;
  //   end;
  // end.
  // </code>
  // </example>

  ESiProtocolError = class(ESmartInspectError)
  private
    FProtocolName: UnicodeString;
    FProtocolOptions: UnicodeString;
  public

    // <summary>
    //   Represents the name of the protocol which threw this exception.
    //   A possible value would be 'tcp'.
    // </summary>
    // <remarks>
    //   This property can return the name of the built-in protocols as
    //   well as names for custom protocols. For a complete list of
    //   built-in protocols please refer to the documentation of the
    //   TSiProtocol class.
    // </remarks>

    property ProtocolName: UnicodeString read FProtocolName write
      FProtocolName;

    // <summary>
    //   Represents the current options of the protocol which threw this
    //   exception. Can be empty if not set.
    // </summary>

    property ProtocolOptions: UnicodeString read FProtocolOptions write
      FProtocolOptions;
  end;

  // <summary>
  //   Used to report errors concerning the connections string in the
  //   TSmartInspect class.
  // </summary>
  // <remarks>
  //   An invalid syntax, unknown protocols or inexistent options in the
  //   <link TSmartInspect.Connections, connections string> will result
  //   in an ESiInvalidConnectionsError exception. This exception type is
  //   used by the Connections property of the TSmartInspect class.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  ESiInvalidConnectionsError = class(Exception);

  TSiProtocol = class;
  TSiSchedulerQueue = class;

  // <summary>
  //   Represents a scheduler action to execute when a protocol is
  //   operating in asynchronous mode. For general information about
  //   the asynchronous protocol mode, please refer to
  //   TSiProtocol.IsValidOption.
  // </summary>

  TSiSchedulerAction =
  (
    // <summary>
    //   Represents a connect protocol operation. This action is
    //   enqueued when the TSiProtocol.Connect method is called and
    //   the protocol is operating in asynchronous mode.
    // </summary>

    saConnect,

    // <summary>
    //   Represents a write protocol operation. This action is
    //   enqueued when the TSiProtocol.WritePacket method is called
    //   and the protocol is operating in asynchronous mode.
    // </summary>

    saWritePacket,

    // <summary>
    //   Represents a disconnect protocol operation. This action
    //   is enqueued when the TSiProtocol.Disconnect method is called
    //   and the protocol is operating in asynchronous mode.
    // </summary>

    saDisconnect,

    // <summary>
    //   Represents a dispatch protocol operation. This action is
    //   enqueued when the TSiProtocol.Dispatch method is called and
    //   the protocol is operating in asynchronous mode.
    // </summary>

    saDispatch
  );

  // <summary>
  //   Represents a scheduler command as used by the TSiScheduler class
  //   and the asynchronous protocol mode.
  // </summary>
  // <remarks>
  //   This class is used by the TSiScheduler class to enqueue protocol
  //   operations for later execution when operating in asynchronous
  //   mode. For detailed information about the asynchronous protocol
  //   mode, please refer to TSiProtocol.IsValidOption.
  //
  //   Instances of TSiSchedulerCommand are reference counted, that
  //   means they will be automatically freed when there are no
  //   references pointing to it anymore. To increment and decrement
  //   the reference counter, use the AddRef and Release methods.
  //   After an instance has been created with the Create constructor,
  //   the reference counter is always set to 1.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiSchedulerCommand = class(TObject)
  private
    FState: TObject;
    FAction: TSiSchedulerAction;
    FRefCount: Integer;
    function GetSize: Integer;
  public

    // <summary>
    //   Creates and initializes a new TSiSchedulerCommand instance and
    //   sets the reference counter to the initial value of 1.
    // </summary>
    // <remarks>
    //   Subclasses must always call this constructor to initialize the
    //   reference counter correctly. <b>Please note</b>: Since scheduler
    //   commands are reference counted, to not call Free directly, use
    //   the Release method instead.
    // </remarks>

    constructor Create;

    // <summary>
    //   Increments the reference counter by one.
    // </summary>
    // <remarks>
    //   This method increments the reference counter by one. Every call
    //   to this method must have a matching call to the corresponding
    //   Release method.
    // </remarks>

    procedure AddRef;

    // <summary>
    //   Decrements the reference counter by one.
    // </summary>
    // <remarks>
    //   This method decrements the reference counter by one. If the
    //   reference counter reaches 0, this object is freed. Every call
    //   to this method must have a matching call to the corresponding
    //   AddRef or Create method.
    // </remarks>

    procedure Release;

    // <summary>
    //   Represents the scheduler action to execute. Please refer
    //   to the documentation of the TSiSchedulerAction enum for more
    //   information about possible values.
    // </summary>

    property Action: TSiSchedulerAction read FAction write FAction;

    // <summary>
    //   Represents the optional scheduler command state object which
    //   provides additional information about the scheduler command.
    //   This property can be nil.
    // </summary>

    property State: TObject read FState write FState;

    // <summary>
    //   Calculates and returns the total memory size occupied by
    //   this scheduler command.
    // </summary>
    // <remarks>
    //   This read-only property returns the total occupied memory
    //   size of this scheduler command. This functionality is used by
    //   the <link TSiProtocol.IsValidOption, asynchronous protocol mode>
    //   to track the total size of scheduler commands.
    // </remarks>

    property Size: Integer read GetSize;
  end;

  PSiSchedulerQueueItem = ^TSiSchedulerQueueItem;

  TSiSchedulerQueueItem = record
    Command: TSiSchedulerCommand;
    Next: PSiSchedulerQueueItem;
    Previous: PSiSchedulerQueueItem;
  end;

  // <summary>
  //   This is the event handler type for the TSiSchedulerQueue.OnDelete
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="ACommand">
  //   The scheduler command which is removed from the queue.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, an ACommand argument will be
  //   passed to the event handlers which offers the possibility of doing
  //   some cleanup work like releasing the packet, for example.
  // </remarks>

  TSiSchedulerQueueEvent = procedure (ASender: TSiSchedulerQueue;
    ACommand: TSiSchedulerCommand) of object;

  // <summary>
  //   Manages a queue of scheduler commands.
  // </summary>
  // <remarks>
  //   This class is responsible for managing a queue of scheduler
  //   commands. This functionality is needed by the
  //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>
  //   and the TSiScheduler class. New commands can be added with the
  //   Enqueue method. Commands can be dequeued with Dequeue. This
  //   queue does not have a maximum size or count.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiSchedulerQueue = class(TObject)
  private
    FHead: PSiSchedulerQueueItem;
    FTail: PSiSchedulerQueueItem;
    FSize: Int64;
    FCount: Integer;
    FOnDelete: TSiSchedulerQueueEvent;
    procedure Add(const AItem: PSiSchedulerQueueItem);
    procedure Remove(const AItem: PSiSchedulerQueueItem);
    procedure DoDelete(const ACommand: TSiSchedulerCommand);
  public

    // <summary>
    //   Adds a new scheduler command to the queue.
    // </summary>
    // <param name="ACommand">The command to add.</param>
    // <remarks>
    //   This method adds the supplied scheduler command to the
    //   queue. The Size of the queue is incremented by the size of
    //   the supplied command (plus some internal management overhead).
    //   This queue does not have a maximum size or count.
    // </remarks>

    procedure Enqueue(const ACommand: TSiSchedulerCommand);

    // <summary>
    //   Returns a scheduler command and removes it from the queue.
    // </summary>
    // <returns>
    //   The removed scheduler command or nil if the queue does not
    //   contain any packets.
    // </returns>
    // <remarks>
    //   If the queue is not empty, this method removes the oldest
    //   scheduler command from the queue (also known as FIFO) and
    //   returns it. The total Size of the queue is decremented by
    //   the size of the returned command (plus some internal
    //   management overhead).
    // </remarks>

    function Dequeue: TSiSchedulerCommand;

    // <summary>
    //   Removes all scheduler commands from this queue.
    // </summary>
    // <remarks>
    //   Removing all scheduler commands of the queue is done by
    //   calling the Dequeue method and the OnDelete event for each
    //   command in the current queue.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Tries to skip and remove scheduler commands from this queue.
    // </summary>
    // <param name="ASize">
    //   The minimum amount of bytes to remove from this queue.
    // </param>
    // <returns>
    //   True if enough scheduler commands could be removed and false
    //   otherwise.
    // </returns>
    // <remarks>
    //   This method removes the next WritePacket scheduler commands
    //   from this queue until the specified minimum amount of bytes
    //   has been removed. Administrative scheduler commands (connect,
    //   disconnect or dispatch) are not removed. If the queue is
    //   currently empty or does not contain enough WritePacket
    //   commands to achieve the specified minimum amount of bytes,
    //   this method returns false.
    // </remarks>

    function Trim(const ASize: Integer): Boolean;

    // <summary>
    //   Returns the current amount of scheduler commands in this
    //   queue.
    // </summary>
    // <remarks>
    //   For each added scheduler command this counter is incremented
    //   by one and for each removed command (with Dequeue) this
    //   counter is decremented by one. If the queue is empty, this
    //   property returns 0.
    // </remarks>

    property Count: Integer read FCount;

    // <summary>
    //   Returns the current size of this queue in bytes.
    // </summary>
    // <remarks>
    //   For each added scheduler command this counter is incremented
    //   by the size of the command (plus some internal management
    //   overhead) and for each removed command (with Dequeue) this
    //   counter is then decremented again. If the queue is empty,
    //   this property returns 0.
    // </remarks>

    property Size: Int64 read FSize;

    // <summary>
    //   Occurs when a scheduler command has been automatically removed
    //   from the queue.
    // </summary>
    // <seealso cref="TSiSchedulerQueueEvent"/>
    // <remarks>
    //   This event is fired for every scheduler packet which is
    //   automatically removed from the queue. Automatically removed in
    //   this context means that the packet is removed during the Clear
    //   method. This event is never called when calling the Dequeue
    //   method.
    // </remarks>

    property OnDelete: TSiSchedulerQueueEvent read FOnDelete write FOnDelete;
  end;

  // <summary>
  //   Responsible for scheduling protocol operations and executing
  //   them asynchronously in a different thread of control.
  // </summary>
  // <remarks>
  //   This class is used by the <link TSiProtocol.IsValidOption,
  //   asynchronous protocol mode> to asynchronously execute protocol
  //   operations. New commands can be scheduled for execution with
  //   the Schedule method. The scheduler can be started and stopped
  //   with the Start and Stop methods. The scheduler uses a size
  //   limited queue to buffer scheduler commands. The maximum size of
  //   this queue can be set with the Threshold property. To influence
  //   the behavior of the scheduler if new commands are enqueued and
  //   the queue is currently considered full, you can specify the
  //   Throttle mode.
  // </remarks>
  // <threadsafety>
  //   This class is guaranteed to be threadsafe.
  // </threadsafety>

  TSiScheduler = class(TThread)
  private
    FLock: TCriticalSection;
    FNotFull: THandle;
    FNotEmpty: THandle;
    FBuffer: array of TSiSchedulerCommand;
    FQueue: TSiSchedulerQueue;
    FThrottle: Boolean;
    FThreshold: Int64;
    FStarted: Boolean;
    FStopped: Boolean;
    FProtocol: TSiProtocol;
    procedure Run;
    function RunCommands(const ACount: Integer): Boolean;
    procedure RunCommand(const ACommand: TSiSchedulerCommand);
    function Enqueue(const ACommand: TSiSchedulerCommand): Boolean;
    function Dequeue: Integer;
    procedure ReleaseCommand(const ACommand: TSiSchedulerCommand);
    procedure DeleteCommand(ASender: TSiSchedulerQueue;
      ACommand: TSiSchedulerCommand);
  protected
    procedure Execute; override;
  public

    // <summary>
    //   Creates and initializes a new TSiScheduler instance.
    // </summary>
    // <param name="AProtocol">
    //   The protocol on which to execute the actual operations like
    //   connect, disconnect, write or dispatch.
    // </param>

    constructor Create(const AProtocol: TSiProtocol);

    // <summary>
    //   Overridden. Releases all resources used by this TSiScheduler
    //   instance and stops the internal scheduler thread.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Removes all scheduler commands from this scheduler.
    // </summary>
    // <remarks>
    //   This method clears the current queue of scheduler commands.
    //   If the Stop method is called after calling Clear and no new
    //   commands are stored between these two calls, the internal
    //   scheduler thread will exit as soon as possible (after the
    //   current command, if any, has been processed).
    // </remarks>

    procedure Clear;

    // <summary>
    //   Starts this scheduler and the internal scheduler thread.
    // </summary>
    // <remarks>
    //   This method must be called before scheduling new commands
    //   with the Schedule method. Call Stop to stop the internal
    //   thread when the scheduler is no longer needed. Note that
    //   this method starts the internal scheduler thread only once.
    //   This means that subsequent calls to this method have no
    //   effect.
    // </remarks>

    procedure Start;

    // <summary>
    //   Stops this scheduler and the internal scheduler thread.
    // </summary>
    // <remarks>
    //   This is the matching method for Start. After calling this
    //   method, new commands will no longer be accepted by Schedule
    //   and are ignored. This method blocks until the internal
    //   thread has processed the current content of the queue.
    //   Call Clear before calling Stop to exit the internal thread
    //   as soon as possible.
    // </remarks>

    procedure Stop;

    // <summary>
    //   Schedules a new command for asynchronous execution.
    // </summary>
    // <param name="ACommand">The command to schedule.</param>
    // <returns>
    //   True if the command could be scheduled for asynchronous
    //   execution and false otherwise.
    // </returns>
    // <remarks>
    //   This method adds the passed command to the internal queue
    //   of scheduler commands. The command is eventually executed
    //   by the internal scheduler thread. This method can block the
    //   caller if the scheduler operates in Throttle mode and the
    //   internal queue is currently considered full (see Threshold).
    // </remarks>

    function Schedule(const ACommand: TSiSchedulerCommand): Boolean;

    // <summary>
    //   Specifies the maximum size of the scheduler command queue.
    // </summary>
    // <remarks>
    //   To influence the behavior of the scheduler if new commands
    //   are enqueued and the queue is currently considered full,
    //   you can specify the Throttle mode.
    // </remarks>

    property Threshold: Int64 read FThreshold write FThreshold;

    // <summary>
    //   Specifies if the scheduler should automatically throttle
    //   threads that enqueue new scheduler commands.
    // </summary>
    // <remarks>
    //   If this property is true and the queue is considered full
    //   when enqueuing new commands, the enqueuing thread is
    //   automatically throttled until there is room in the queue
    //   for the new command. In non-throttle mode, the thread is
    //   not blocked but older commands are removed from the queue.
    // </remarks>

    property Throttle: Boolean read FThrottle write FThrottle;
  end;

  // <summary>
  //   Represents the timestamp resolution mode for the TSiClock class.
  // </summary>
  // <remarks>
  //   SmartInspect currently supports two different kinds of timestamp
  //   resolutions. The standard resolution is the default timestamp
  //   behavior of the SmartInspect Delphi library and usually provides
  //   a maximum resolution of 10-55 milliseconds (depending on the
  //   Windows version). This is the recommended option for production
  //   systems. High-resolution timestamps, on the other hand, can provide
  //   a microseconds resolution but are only intended to be used on
  //   development machines.
  //
  //   Please see TSmartInspect.Resolution for details.
  // </remarks>

  TSiClockResolution =
  (
    // <summary>
    //   Represents the standard timestamp resolution. This is the
    //   default timestamp behavior of the SmartInspect Delphi library
    //   and the recommended option for production systems.
    // </summary>

    crStandard,

    // <summary>
    //   Represents timestamps with a very high resolution (microseconds).
    //   This option is not intended to be used on production systems. See
    //   TSmartInspect.Resolution for details.
    // </summary>

    crHigh
  );

  // <summary>
  //   Provides access to the current date and time, optionally with a
  //   high resolution.
  // </summary>
  // <seealso cref="TSiClockResolution"/>
  // <remarks>
  //   See Now for a method which returns the current date and time,
  //   optionally with a very high resolution. See Calibrate for a
  //   method which can synchronize the high-resolution timer with the
  //   system clock.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiClock = class
  private
    class procedure SortArray(var AArray: array of TDateTime);
    class function GetMedian(var AArray: array of TDateTime): TDateTime;  
    class function GetOffset: TDateTime;
    class function GetTicks: TDateTime;
    class function DoCalibrate: TDateTime;
  public

    // <summary>
    //   Returns the current date and time, optionally with a high
    //   resolution.
    // </summary>
    // <param name="AResolution">
    //   Specifies the desired resolution mode for the returned timestamp.
    // </param>
    // <seealso cref="TSiClockResolution"/>
    // <returns>The current date and time as TDateTime value.</returns>
    // <remarks>
    //   If crHigh is passed as value for the AResolution argument, this
    //   method tries to return a timestamp with a microsecond resolution.
    //
    //   High-resolution timestamps are only available if the
    //   QueryPerformanceCounter and QueryPerformanceFrequency functions
    //   indicate a successfully working high-resolution performance
    //   counter. Please see the Windows Platform SDK documentation for
    //   details.
    //
    //   Additionally, high-resolution timestamps are not intended to be
    //   used on production systems. It is recommended to use them only
    //   during development and debugging. See TSmartInspect.Resolution
    //   for details.
    //
    //   If high-resolution support is not available, this method simply
    //   returns SysUtils.Now.
    // </remarks>

    class function Now(const AResolution: TSiClockResolution): TDateTime;

    // <summary>
    //   Calibrates the high-resolution timer and synchronizes it
    //   with the system clock.
    // </summary>
    // <remarks>
    //   Use this method to calibrate the high-resolution timer and
    //   to improve the timer synchronization with the system clock.
    //
    //   Background: Without calling this method before calling Now
    //   in high-resolution mode, Now returns a value which is only
    //   loosely synchronized with the system clock. The returned
    //   value might differ by a few milliseconds. This can usually
    //   safely be ignored for a single process application, but may
    //   be an issue for distributed interacting applications with
    //   multiple processes. In this case, calling Calibrate once on
    //   application startup might be necessary to improve the system
    //   clock synchronization of each process in order to get
    //   comparable timestamps across all processes.
    //
    //   Note that calling this method is quite costly, it can easily
    //   take 50 milliseconds, depending on the system clock timer
    //   resolution of the underlying operating system. Also note that
    //   the general limitations (see TSmartInspect.Resolution) of
    //   high-resolution timestamps still apply after calling this
    //   method.
    // </remarks>

    class procedure Calibrate;
  end;

  // <summary>
  //   Represents the type of the callback for the TSiTimer class.
  // </summary>
  // <param name="AState">
  //   The state object which has been passed to the constructor of the
  //   TSiTimer class. Can be nil.
  // </param>
  // <remarks>
  //   This type is used by the TSiTimer class. Callbacks of this type
  //   are periodically executed in a configurable interval. The AState
  //   object can be used to pass a custom object to the callback.
  // </remarks>

  TSiTimerCallback = procedure(AState: TObject) of object;

  TSiTimerThread = class(TThread)
  private
    FCallback: TSiTimerCallback;
    FState: TObject;
    FPeriod: Integer;
    FEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(const ACallback: TSiTimerCallback;
      const AState: TObject; const APeriod: Integer);
    destructor Destroy; override;
    procedure Cancel;
  end;

  // <summary>
  //   Provides a configurable timer class which periodically executes
  //   a custom action.
  // </summary>
  // <remarks>
  //   This class can come in handy if you need to periodically execute
  //   some code. The underlying implementation uses threads instead of
  //   window messages and it is thus safe to be used in situations when
  //   window messages are not available. Please note that the custom
  //   action is executed asynchronously, i.e. not in the main thread of
  //   the application.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiTimer = class(TObject)
  private
    FThread: TSiTimerThread;
  public

    // <summary>
    //   Creates and initializes a new TSiTimer instance with a specified
    //   interval and custom action.
    // </summary>
    // <param name="ACallback">
    //   The custom action to execute periodically.
    // </param>
    // <param name="AState">
    //   A state object which is passed to the ACallback callback. Can be
    //   nil if not needed.
    // </param>
    // <param name="APeriod">
    //   The milliseconds interval in which to execute the callback.
    // </param>

    constructor Create(const ACallback: TSiTimerCallback;
      const AState: TObject; const APeriod: Integer);

    // <summary>
    //   Overridden. Releases all resources used by this TSiTimer instance
    //   and stops executing the custom action.
    // </summary>
    // <remarks>
    //   This destructor can block if the callback is currently running.
    //   In this case, this destructor will wait until the callback is
    //   done.
    // </remarks>

    destructor Destroy; override;
  end;

  // <summary>
  //   Represents the type of a packet. In the SmartInspect concept, there
  //   are multiple packet types each serving a special purpose. For a good
  //   starting point on packets, please have a look at the documentation
  //   of the TSiPacket class.
  // </summary>

  TSiPacketType =
  (
    // <summary>
    //   Identifies a packet as Log Entry. Please have a look at the
    //   documentation of the TSiLogEntry class for information about
    //   this packet type.
    // </summary>

    ptLogEntry,

    // <summary>
    //   Identifies a packet as Control Command. Please have a look at
    //   the documentation of the TSiControlCommand class for information
    //   about this packet type.
    // </summary>

    ptControlCommand,

    // <summary>
    //   Identifies a packet as Watch. Please have a look at the
    //   documentation of the TSiWatch class for information about
    //   this packet type.
    // </summary>

    ptWatch,

    // <summary>
    //   Identifies a packet as Process Flow entry. Please have a look
    //   at the documentation of the TSiProcessFlow class for information
    //   about this packet type.
    // </summary>

    ptProcessFlow,

    // <summary>
    //   Identifies a packet as Log Header. Please have a look at the
    //   documentation of the TSiLogHeader class for information about
    //   this packet type.
    // </summary>

    ptLogHeader
  );

  // <summary>
  //   Represents the type of a TSiLogEntry packet. Instructs the Console
  //   to choose the correct icon and to perform additional actions, like,
  //   for example, enter a new method or draw a separator.
  // </summary>

  TSiLogEntryType =
  (
    // <summary>
    //   Instructs the Console to draw a separator.
    // </summary>

    ltSeparator,

    // <summary>
    //   Instructs the Console to enter a new method.
    // </summary>

    ltEnterMethod,

    // <summary>
    //   Instructs the Console to leave a method.
    // </summary>

    ltLeaveMethod,

    // <summary>
    //   Instructs the Console to reset the current call stack.
    // </summary>

    ltResetCallstack,

    // <summary>
    //   Instructs the Console to treat a Log Entry as simple
    //   message.
    // </summary>

    ltMessage,

    // <summary>
    //   Instructs the Console to treat a Log Entry as warning
    //   message.
    // </summary>

    ltWarning,

    // <summary>
    //   Instructs the Console to treat a Log Entry as error
    //   message.
    // </summary>

    ltError,

    // <summary>
    //   Instructs the Console to treat a Log Entry as internal
    //   error.
    // </summary>

    ltInternalError,

    // <summary>
    //   Instructs the Console to treat a Log Entry as comment.
    // </summary>

    ltComment,

    // <summary>
    //   Instructs the Console to treat a Log Entry as a variable
    //   value.
    // </summary>

    ltVariableValue,

    // <summary>
    //   Instructs the Console to treat a Log Entry as checkpoint.
    // </summary>

    ltCheckpoint,

    // <summary>
    //   Instructs the Console to treat a Log Entry as debug
    //   message.
    // </summary>

    ltDebug,

    // <summary>
    //   Instructs the Console to treat a Log Entry as verbose
    //   message.
    // </summary>

    ltVerbose,

    // <summary>
    //   Instructs the Console to treat a Log Entry as fatal error
    //   message.
    // </summary>

    ltFatal,

    // <summary>
    //   Instructs the Console to treat a Log Entry as conditional
    //   message.
    // </summary>

    ltConditional,

    // <summary>
    //   Instructs the Console to treat a Log Entry as assert message.
    // </summary>

    ltAssert,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with text.
    // </summary>

    ltText,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with binary data.
    // </summary>

    ltBinary,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with a picture as data.
    // </summary>

    ltGraphic,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with source code data.
    // </summary>

    ltSource,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with object data.
    // </summary>

    ltObject,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with web data.
    // </summary>

    ltWebContent,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with system information.
    // </summary>

    ltSystem,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with memory statistics.
    // </summary>

    ltMemoryStatistic,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with a database result.
    // </summary>

    ltDatabaseResult,

    // <summary>
    //   Instructs the Console to treat the Log Entry as Log Entry
    //   with a database structure.
    // </summary>

    ltDatabaseStructure
  );

  // <summary>
  //   Specifies the viewer for displaying the title or data of a Log
  //   Entry in the Console.
  // </summary>
  // <remarks>
  //   There are many viewers available for displaying the data of a
  //   Log Entry in different ways. For example, there are viewers that
  //   can display lists, tables, binary dumps of data or even websites.
  //
  //   Every viewer in the Console has a corresponding so called viewer
  //   context in this library which can be used to send custom logging
  //   information. To get started, please see the documentation of the
  //   TSiSession.LogCustomContext method and TSiViewerContext class.
  // </remarks>

  TSiViewerId =
  (
    // <summary>
    //   Instructs the Console to use no viewer at all.
    // </summary>

    viNone,

    // <summary>
    //   Instructs the Console to display the title of a Log Entry
    //   in a read-only text field.
    // </summary>

    viTitle,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   in a read-only text field.
    // </summary>

    viData,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a list.
    // </summary>

    viList,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a key/value list.
    // </summary>

    viValueList,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   using an object inspector.
    // </summary>

    viInspector,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a table.
    // </summary>

    viTable,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a website.
    // </summary>

    viWeb,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a binary dump using a read-only hex editor.
    // </summary>

    viBinary,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as HTML source with syntax highlighting.
    // </summary>

    viHtmlSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as Java Script source with syntax highlighting.
    // </summary>

    viJavaScriptSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as VBScript source with syntax highlighting.
    // </summary>

    viVbScriptSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as Perl source with syntax highlighting.
    // </summary>

    viPerlSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as SQL source with syntax highlighting.
    // </summary>

    viSqlSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as INI source with syntax highlighting.
    // </summary>

    viIniSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as Python source with syntax highlighting.
    // </summary>

    viPythonSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as XML source with syntax highlighting.
    // </summary>

    viXmlSource,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as bitmap image.
    // </summary>

    viBitmap,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as JPEG image.
    // </summary>

    viJpeg,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as a Windows icon.
    // </summary>

    viIcon,

    // <summary>
    //   Instructs the Console to display the data of a Log Entry
    //   as Windows Metafile image.
    // </summary>

    viMetafile
  );

  // <summary>
  //   Used in the LogSource methods of the TSiSession class to specify
  //   the type of source code.
  // </summary>

  TSiSourceId =
  (
    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for HTML.
    // </summary>

    siHtml,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for Java Script.
    // </summary>

    siJavaScript,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for VBScript.
    // </summary>

    siVbScript,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for Perl.
    // </summary>

    siPerl,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for SQL.
    // </summary>

    siSql,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for INI files.
    // </summary>

    siIni,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for Python.
    // </summary>

    siPython,

    // <summary>
    //   Instructs the TSiSession.LogSource methods to use syntax
    //   highlighting for XML.
    // </summary>

    siXml
  );

  // <summary>
  //   Used by the TSiGraphicViewerContext class to specify the desired
  //   picture type.
  // </summary>

  TSiGraphicId =
  (
    // <summary>
    //   Instructs the TSiGraphicViewerContext class to treat the data
    //   as bitmap image.
    // </summary>

    giBitmap,

    // <summary>
    //   Instructs the TSiGraphicViewerContext class to treat the data
    //   as JPEG image.
    // </summary>

    giJpeg,

    // <summary>
    //   Instructs the TSiGraphicViewerContext class to treat the data
    //   as Window icon.
    // </summary>

    giIcon,

    // <summary>
    //   Instructs the TSiGraphicViewerContext class to treat the data
    //   as Window Metafile image.
    // </summary>

    giMetafile
  );

  // <summary>
  //   Represents the type of a TSiControlCommand packet. The type of
  //   a Control Commmand influences the way the Console interprets the
  //   packet.
  // </summary>
  // <remarks>
  //   For example, if a Control Command packet has a type of ccClearAll,
  //   the entire Console is reset when this packet arrives. Also have a
  //   look at the corresponding TSiSession.ClearAll method.
  // </remarks>

  TSiControlCommandType =
  (
    // <summary>
    //   Instructs the Console to clear all Log Entries.
    // </summary>

    ccClearLog,

    // <summary>
    //   Instructs the Console to clear all Watches.
    // </summary>

    ccClearWatches,

    // <summary>
    //   Instructs the Console to clear all AutoViews.
    // </summary>

    ccClearAutoViews,

    // <summary>
    //   Instructs the Console to reset the whole Console.
    // </summary>

    ccClearAll,

    // <summary>
    //   Instructs the Console to clear all Process Flow entries.
    // </summary>

    ccClearProcessFlow
  );

  // <summary>
  //   Represents the type of a TSiWatch packet. The type of a Watch
  //   specifies its variable type. 
  // </summary>
  // <remarks>
  //   For example, if a Watch packet has a type of wtString, the
  //   represented variable is treated as string in the Console.
  // </remarks>

  TSiWatchType =
  (
    // <summary>
    //   Instructs the Console to treat a Watch value as char.
    // </summary>

    wtChar,

    // <summary>
    //   Instructs the Console to treat a Watch value as string.
    // </summary>

    wtString,

    // <summary>
    //   Instructs the Console to treat a Watch value as integer.
    // </summary>

    wtInteger,

    // <summary>
    //   Instructs the Console to treat a Watch value as float.
    // </summary>

    wtFloat,

    // <summary>
    //   Instructs the Console to treat a Watch value as boolean.
    // </summary>

    wtBoolean,

    // <summary>
    //   Instructs the Console to treat a Watch value as address.
    // </summary>

    wtAddress,

    // <summary>
    //   Instructs the Console to treat a Watch value as timestamp.
    // </summary>

    wtTimestamp,

    // <summary>
    //   Instructs the Console to treat a Watch value as object.
    // </summary>

    wtObject
  );

  // <summary>
  //   Represents the type of a TSiProcessFlow packet. The type of a
  //   Process Flow entry specifies the way the Console interprets this
  //   packet.
  // </summary>
  // <remarks>
  //   For example, if a Process Flow entry has a type of pEnterThread,
  //   the Console interprets this packet as information about a new thread
  //   of your application.
  // </remarks>

  TSiProcessFlowType =
  (
    // <summary>
    //   Instructs the Console to enter a new method.
    // </summary>

    pfEnterMethod,

    // <summary>
    //   Instructs the Console to leave a method.
    // </summary>

    pfLeaveMethod,

    // <summary>
    //   Instructs the Console to enter a new thread.
    // </summary>

    pfEnterThread,

    // <summary>
    //   Instructs the Console to leave a thread.
    // </summary>

    pfLeaveThread,

    // <summary>
    //   Instructs the Console to enter a new process.
    // </summary>

    pfEnterProcess,

    // <summary>
    //   Instructs the Console to leave a process.
    // </summary>

    pfLeaveProcess
  );

  // <summary>
  //   Represents the log level of a packet in the SmartInspect Delphi
  //   library.
  // </summary>
  // <remarks>
  //   Please see the TSmartInspect.Level and TSmartInspect.DefaultLevel
  //   properties for detailed examples and more information on how to
  //   use the TSiLevel enum.
  // </remarks>

  TSiLevel =
  (
    // <summary>
    //   Represents the Debug log level. This log level is mostly intended
    //   to be used in the debug and development process.
    // </summary>

    lvDebug,

    // <summary>
    //   Represents the Verbose log level. This log level is intended
    //   to track the general progress of applications at a fine-grained
    //   level.
    // </summary>

    lvVerbose,

    // <summary>
    //   Represents the Message log level. This log level is intended to
    //   track the general progress of applications at a coarse-grained
    //   level.
    // </summary>

    lvMessage,

    // <summary>
    //   Represents the Warning log level. This log level designates
    //   potentially harmful events or situations.
    // </summary>

    lvWarning,

    // <summary>
    //   Represents the Error log level. This log level designates error
    //   events or situations which are not critical to the entire system.
    //   This log level thus describes recoverable or less important
    //   errors.
    // </summary>

    lvError,

    // <summary>
    //   Represents the Fatal log level. This log level designates errors
    //   which are not recoverable and eventually stop the system or
    //   application from working.
    // </summary>

    lvFatal,

    // <summary>
    //   This log level represents a special log level which is only used
    //   by <link TSiControlCommand, Control Commands> and is not intended
    //   to be used directly.
    // </summary>

    lvControl
  );

  // <summary>
  //   Specifies the log rotate mode for the <link TSiFileProtocol,
  //   file protocol> and derived classes.
  // </summary>
  // <remarks>
  //   For information about the rotate functionality, please have a look
  //   at the documentation of the TSiFileProtocol.IsValidOption method.
  // </remarks>

  TSiFileRotate =
  (
    // <summary>
    //   Completely disables the log rotate functionality.
    // </summary>

    frNone,

    // <summary>
    //   Instructs the file protocol to rotate log files hourly.
    // </summary>

    frHourly,

    // <summary>
    //   Instructs the file protocol to rotate log files daily.
    // </summary>

    frDaily,

    // <summary>
    //   Instructs the file protocol to rotate log files weekly.
    // </summary>

    frWeekly,

    // <summary>
    //   Instructs the file protocol to rotate log files monthly.
    // </summary>

    frMonthly
  );

  // <summary>
  //   Represents a collection of Unicode strings.
  // </summary>
  // <remarks>
  //   This class is similar to the well-known TStringList class of Delphi
  //   except that it supports Unicode strings even in Delphi versions older
  //   than Delphi 2009. Besides always operating with Unicode strings,
  //   this class is capable of auto-detecting the string encoding of files
  //   and streams. Please see the LoadFromStream method for details.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiStringList = class
  private
    FLength: Integer;
    FCount: Integer;
    FItems: array of UnicodeString;
    function Get(AIndex: Integer): UnicodeString;
    function GetCount: Integer;
    procedure Put(AIndex: Integer; const AValue: UnicodeString);
    procedure Grow;
    procedure Exchange(const I, J: Integer);
    function Partition(const P, R: Integer): Integer;
    procedure QuickSort(const P, R: Integer);
    function GetText: UnicodeString;
    procedure SetText(const AValue: UnicodeString);
    function ReadAnsi(const AStream: TStream): UnicodeString;
    function ReadUtf8(const AStream: TStream): UnicodeString;
    function ReadUcs2Le(const AStream: TStream): UnicodeString;
    function ReadUcs2Be(const AStream: TStream): UnicodeString;
  public

    // <summary>
    //   Overridden. Releases any resources used by this object by calling
    //   the Clear method.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Adds a new string to this collection.
    // </summary>
    // <param name="AString">The string to add.</param>
    // <remarks>
    //   This method adds the passed string to this collection. After this
    //   method returns, the Count property is incremented by one and the
    //   string can be retrieved with the Items property.
    // </remarks>

    procedure Add(const AString: UnicodeString);

    // <summary>
    //   Removes a previously added string from this collection.
    // </summary>
    // <param name="AString">The string to remove.</param>
    // <remarks>
    //   This method removes the passed string from this collection. If
    //   the string cannot be found, this method has no effect. If the
    //   string exists more than once, the first occurrence is removed.
    // </remarks>

    procedure Remove(const AString: UnicodeString);

    // <summary>
    //   Deletes a string from this collection.
    // </summary>
    // <param name="AIndex">The index of the string to delete.</param>
    // <remarks>
    //   This method deletes the string pointed to by the AIndex argument
    //   from this collection. If the passed AIndex argument is less than
    //   0 or greater than or equal to the amount of strings in this list,
    //   an exception is thrown.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   ESmartInspectError       Index out of bounds. AIndex is less than 0
    //                              or greater than or equal to the amount of
    //                              strings in this list.
    // </table>
    // </exception>

    procedure Delete(const AIndex: Integer);

    // <summary>
    //   Loads the string items from a file.
    // </summary>
    // <param name="AFileName">
    //   The name of the file to load the string items from.
    // </param>
    // <remarks>
    //   This method does nothing more than to create a new TSiFileStream
    //   object with the passed file name in read-only mode and then calls
    //   the LoadFromStream method. Please see the LoadFromStream method
    //   for more information, especially for the string encoding detection.
    // <remarks>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        The desired file could not be opened.
    //
    //   EStreamError              An I/O error occurred while trying
    //                               to load the string items from the
    //                               specified file.
    // </table>
    // </exception>

    procedure LoadFromFile(const AFileName: UnicodeString);

    // <summary>
    //   Loads the string items from a stream.
    // </summary>
    // <param name="AStream">
    //   The stream to load the string items from.
    // </param>
    // <remarks>
    //   This method loads the string items from the supplied stream. This
    //   method is capable of auto-detecting the string encoding when a
    //   BOM (Byte Order Mark) is given at the start of the stream. The
    //   following table lists the supported encodings and the corresponding
    //   BOM identifiers.
    //
    //   <table>
    //     Encoding                BOM identifier
    //     -                       -
    //     UTF8                    $EF, $BB, $BF
    //     Unicode                 $FF, $FE
    //     Unicode big-endian      $FE, $FF
    //   </table>
    //
    //   If no BOM is given, the text is assumed to be in the ANSI format.
    //   After detecting the encoding and reading the text, string items
    //   are added line per line (using the combination of the carriage
    //   return and linefeed characters as separator).
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   EStreamError              An I/O error occurred while trying
    //                               to load the string items from the
    //                               specified stream.
    // </table>
    // </exception>

    procedure LoadFromStream(const AStream: TStream);

    // <summary>
    //   Returns the index in this collection for a string.
    // </summary>
    // <param name="AString">
    //   The string whose index in this collection should be returned.
    // </param>
    // <returns>
    //   The index of the passed string or -1 if the string is unknown.
    // </returns>
    // <remarks>
    //   This method returns the index in this collection for the specified
    //   string. If the passed string is unknown, -1 is returned. If the
    //   string exists more than once, the index of the first occurrence
    //   is returned.
    // </remarks>

    function IndexOf(const AString: UnicodeString): Integer;

    // <summary>
    //   Removes all string items from this collection.
    // </summary>
    // <remarks>
    //   After this method has been called, the Count property returns 0 and
    //   the Text property an empty string.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Sorts the items in this collection.
    // </summary>
    // <remarks>
    //   After this method returns, the items in this collection are sorted
    //   alphabetically. If the collection is empty or contains only a single
    //   element, this method has no effect at all.
    // </remarks>

    procedure Sort;

    // <summary>
    //   Returns the amount of items currently held in this collections.
    // </summary>
    // <remarks>
    //   For each added string, this property is incremented by one and for
    //   each removed item, this property is decremented by one. If this
    //   collection is empty, 0 is returned.
    // </remarks>

    property Count: Integer read GetCount;

    // <summary>
    //   Gets or sets the string items in this collection.
    // </summary>
    // <remarks>
    //   With this property you can set or get the string items contained in
    //   this collection. In read-mode this property concatenates the items
    //   of this collection to a single string with the carriage return and
    //   line feed characters as separators. In write-mode this property
    //   clears the current list of items, parses the supplied string and
    //   then adds new strings line per line. Assigning an empty string is
    //   therefore practically the same as clearing the list.
    // </remarks>

    property Text: UnicodeString read GetText write SetText;

    // <summary>
    //   Gets or overrides the string for a specified index.
    // </summary>
    // <param name="AIndex">
    //   The index of the string to override or get.
    // </param>
    // <remarks>
    //   In read-mode this method returns the string pointed to by the AIndex
    //   argument. In write-mode this method overrides the existing string at
    //   index AIndex with the passed string. Note that an exception is thrown
    //   if AIndex is less than 0 or greater than or equal to the amount of
    //   string items in this list.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   ESmartInspectError       Index out of bounds. AIndex is less than 0
    //                              or greater than or equal to the amount of
    //                              strings in this list.
    // </table>
    // </exception>

    property Items[AIndex: Integer]: UnicodeString read Get write Put; default;
  end;

  // <summary>
  //   Responsible for concatenating a Unicode string.
  // </summary>
  // <remarks>
  //   This class provides an efficient way to concatenate a Unicode string.
  //   Instead of relying on the standard Delphi way for building a string
  //   with the plus operator, you can use this class.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiStringBuilder = class
  private
    FCount: Integer;
    FCapacity: Integer;
    FBuffer: PWideChar;
    procedure Reset(const ACapacity: Integer);
    function GetText: UnicodeString;
    procedure Grow;
  public

    // <summary>
    //   Creates and initializes a new TSiStringBuilder instance.
    // </summary>
    // <param name="ACapacity">
    //   The initial capacity of the string buffer. This argument is optional
    //   and defaults to $100 = 256 byte.
    // </param>

    constructor Create(const ACapacity: Integer = $100);

    // <summary>
    //   Overridden. Release any resources used by this object.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Appends a new string to the end of the buffer.
    // </summary>
    // <param name="AString">The string to append.</param>
    // <remarks>
    //   This methods appends the supplied string to the end of the internal
    //   buffer. After this method call, the Text property returns the value
    //   it had before this method call concatenated with the supplied
    //   AString argument.
    // </remarks>

    procedure Append(const AString: UnicodeString);

    // <summary>
    //   Clears the internal string buffer.
    // </summary>
    // <remarks>
    //   This method clears the internal string buffer. After this method
    //   call, the Text property returns an empty string. It is safe to
    //   reuse the object after this method has been called.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Returns the concatenated string.
    // </summary>
    // <remarks>
    //   This method does nothing more than to return the previously
    //   concatenated string. It does not change the internal buffer so
    //   that this object can safely be reused after calling this method.
    //   In contrast to the TSiStringList class, this method returns the
    //   text as concatenated and does not add any separators.
    // </remarks>

    property Text: UnicodeString read GetText;

    // <summary>
    //   Returns the amount of characters currently stored in this string
    //   builder. Returns 0 after calling the Clear method.
    // </summary>

    property Count: Integer read FCount;
  end;

  PPSiHashItem = ^PSiHashItem;
  PSiHashItem = ^TSiHashItem;
  PSiHashItems = array of PSiHashItem;

  TSiHashItem = record
    Key: UnicodeString;
    Value: Pointer;
    Hash: Cardinal;
    Next: PSiHashItem;
  end;

  // <summary>
  //   Abstract base implementation of a Unicode enabled hash table.
  // </summary>
  // <remarks>
  //   This is the base class for TSiStringHash, TSiObjectHash and
  //   TSiIntegerHash. It provides the necessary methods to store,
  //   retrieve, remove and iterate hashed items. The key is always
  //   of type UnicodeString and the type of the hash value depends on
  //   the subclass implementation. TSiStringHash provides hashing
  //   of string values, TSiObjectHash hashing of objects and
  //   TSiIntegerHash hashing of integers.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiHash = class
  private
    FCurrentIndex: Integer;
    FCurrent: PSiHashItem;
    FPrimeIndex: Integer;
    FLoad: Integer;
    FCount: Integer;
    FLoadFactor: Single;
    FItems: PSiHashItems;
    function FindEx(const AKey: UnicodeString; var AIndex: Cardinal):
      PPSiHashItem;
    function Find(const AKey: UnicodeString): PPSiHashItem;
    function HashOf(const AKey: UnicodeString): Cardinal;
    procedure Resize;
    procedure Grow(const ACapacity: Cardinal);
    function GetCurrent: Pointer;
    function GetCurrentKey: UnicodeString;
  protected

    // <summary>
    //   Returns the value for the current item in an iteration loop.
    // </summary>
    // <returns>
    //   The value of the current item in an iteration loop or nil if the
    //   iteration loop has already ended.
    // </returns>
    // <remarks>
    //   An iteration loop is initialized with the Restart method. Then,
    //   to iterator over the items in this hash table, call Next in a
    //   loop until it returns False.
    // </remarks>

    property Current: Pointer read GetCurrent;

    // <summary>
    //   Intended to free used resources for a value of an item.
    // </summary>
    // <param name="AValue">
    //   The value whose resources are supposed to be freed.
    // </param>
    // <remarks>
    //   This abstract method is always called when an existing item is
    //   removed from the hash table. The value of the item as added with
    //   the InternalPut method is passed to allow subclasses to cleanup
    //   any used resources.
    // </remarks>

    procedure DoDispose(const AValue: Pointer); virtual; abstract;

    // <summary>
    //   Returns the value of a specified item.
    // </summary>
    // <param name="AKey">
    //   The key of the item whose value should be returned.
    // </param>
    // <returns>
    //   The previously added value for the specified key if the requested
    //   item exists in the hash table or nil otherwise.
    // </returns>
    // <remarks>
    //   This method returns the value for a specified key which has
    //   previously been added with the InternalPut method. If the key is
    //   unknown, this method returns nil.
    // </remarks>

    function InternalGet(const AKey: UnicodeString): Pointer;

    // <summary>
    //   Adds a new item with the specified key and value to this hash
    //   table.
    // </summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method does not take care of duplicate keys. The subclasses
    //   are responsible for handling duplicate keys and should never call
    //   this method with a key which already exists in the hash table.
    //
    //   To retrieve an added value for a specified key, you can call the
    //   InternalGet method.
    // </remarks>

    procedure InternalPut(const AKey: UnicodeString; const AValue: Pointer);
  public

    // <summary>
    //   Creates and initializes and new TSiHash instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Destroys any used resources of this TSiHash instance.
    // </summary>

    destructor Destroy; override;

    // <summary>Clears this hash table.</summary>
    // <remarks>
    //   After this method returns, the hash table is empty and the Next
    //   and Contains methods always return False until new items are added.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Removes a previously added item from this hash table.
    // </summary>
    // <param name="AKey">The key of the item to remove.</param>
    // <remarks>
    //   This method removes the item which can be identified by the passed
    //   key from this hash table. After this method returns, the Contains
    //   method returns False when called with the same AKey argument. If
    //   the specified key is unknown, this method does nothing.
    // </remarks>

    procedure Remove(const AKey: UnicodeString);

    // <summary>
    //   Indicates if this hash table contains a specified item.
    // </summary>
    // <param name="AKey">The key of the item to check for.</param>
    // <returns>
    //   True if the item with the specified key is contained in the hash
    //   table and False otherwise.
    // </returns>

    function Contains(const AKey: UnicodeString): Boolean;

    // <summary>
    //   Initializes an iteration loop over the items in this hash table.
    // </summary>
    // <remarks>
    //   To iterate over the items contained in this hash table, call this
    //   method first and then call the Next method in a loop until it
    //   returns False. Note that it is not supported to remove an existing
    //   or adding a new item during an iteration loop. Please see the Next
    //   method for an example.
    // </remarks>

    procedure Restart;

    // <summary>
    //   Advances the iteration loop pointer by one and indicates if the
    //   loop has ended.
    // </summary>
    // <returns>
    //   True if there are items left to iterate over and False otherwise.
    // </returns>
    // <remarks>
    //   To iterate over the items in this hash table, you first need to
    //   initialize the iteration loop by calling the Restart method and can
    //   then call this Next method in a loop until it returns False. Please
    //   see the example section below.
    // </remarks>
    // <example>
    // <code>
    //   LHash.Restart;
    //   while LHash.Next do
    //   begin
    //     ShowMessage(LHash.CurrentKey);
    //   end;
    // </code>
    // </example>

    function Next: Boolean;

    // <summary>
    //   Returns the key of the current item in an iteration loop.
    // </summary>
    // <returns>
    //   Either the key of the current item in an iteration loop if the loop
    //   has not ended already or an empty string otherwise.
    // </returns>
    // <remarks>
    //   You can call this property to retrieve the key of the current item
    //   in an iteration loop. To initialize a new iteration loop, call the
    //   Restart method first and then call the Next method in a loop until
    //   it returns false. Please see the Next method for an example.
    // </remarks>

    property CurrentKey: UnicodeString read GetCurrentKey;

    // <summary>
    //   Returns the number of elements in this hash table.
    // </summary>

    property Count: Integer read FCount;
  end;

  // <summary>
  //   Provides a hash table for string keys and values.
  // </summary>
  // <remarks>
  //   This concrete implementation of a hash table is capable of storing
  //   values of type UnicodeString. For a hash table which can store objects,
  //   please refer to the documentation of the TSiObjectHash class. Also,
  //   for a hash table which can store integers, please refer to
  //   TSiIntegerHash.
  // </remarks>

  TSiStringHash = class(TSiHash)
  private
    function GetCurrentValue: UnicodeString;
  protected
    procedure DoDispose(const AValue: Pointer); override;
  public

    // <summary>
    //   Adds a new or overrides an existing item with a specified key and
    //   value.
    //</summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the key already exists, the value
    //   of the related item is overridden with the supplied AValue argument.
    //   See Add for a method which does not override an existing item and
    //   adds an item only if does not already exist in the hash table.
    // </remarks>

    procedure Put(const AKey, AValue: UnicodeString);

    // <summary>
    //   Adds a new item with a specified key and value to the hash table.
    // </summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the specified key already exists,
    //   this method does nothing. See Put for a method which can override
    //   the value of an existing item.
    // </remarks>

    procedure Add(const AKey, AValue: UnicodeString);

    // <summary>
    //   Returns the value of a previously added item.
    // <summary>
    // <param name="AKey">
    //   The key of the item whose value should be returned.
    // </param>
    // <returns>
    //   The value of a previously added item or an empty string if the
    //   passed key is unknown.
    // </returns>
    // <remarks>
    //   This method returns the value of an item which has previously
    //   been added with the Put or Add methods. If the requested item is
    //   not known (i.e. already removed from this hash table or never
    //   added), this method returns an empty string.
    // </remarks>

    function Get(const AKey: UnicodeString): UnicodeString;

    // <summary>
    //   Returns the value of the current item in an iteration loop.
    // </summary>
    // <returns>
    //   The value of the current item in an iteration loop or an empty
    //   string if the loop has already ended.
    // </returns>
    // <remarks>
    //   To initialize a new iteration loop, call Restart. To then iterate
    //   over the items in this hash table, call the Next method in a loop
    //   until it returns False. Please see the Next method for an example.
    // </remarks>

    property CurrentValue: UnicodeString read GetCurrentValue;

    // <summary>
    //   Gets or sets the value for a specified item.
    // </summary>
    // <returns>
    //   The value of a previously added item or an empty string if the
    //   passed key is unknown.
    // </returns>
    // <remarks>
    //   This property behaves like the Get and Put methods. When accessed
    //   in write mode, this property adds a new item with the specified
    //   key and value to this hash table. If the key already exists, the
    //   value of the related item is overridden. In read mode, this property
    //   returns the value of an existing item. If the requested item is not
    //   known (i.e. already removed from this hash table or never added),
    //   this property returns an empty string.
    // </remarks>

    property Items[const AKey: UnicodeString]: UnicodeString read Get
      write Put; default;
  end;

  // <summary>
  //   Provides a hash table for string keys and object values.
  // </summary>
  // <remarks>
  //   This concrete implementation of a hash table is capable of storing
  //   objects. For a hash table which can store strings, please refer to
  //   the documentation of the TSiStringHash class. Also, for a hash table
  //   which can store integers, please refer to TSiIntegerHash.
  // </remarks>

  TSiObjectHash = class(TSiHash)
  private
    FOwnsObjects: Boolean;
    function GetCurrentValue: TObject;
  protected
    procedure DoDispose(const AValue: Pointer); override;
  public

    // <summary>
    //   Creates and initializes a new TSiObjectHash instance.
    // </summary>
    // <param name="AOwnsObjects">
    //   Specifies if objects should be automatically freed when removed
    //   from this hash table. This parameter is optional and defaults to
    //   False.
    // </param>

    constructor Create(const AOwnsObjects: Boolean = False);

    // <summary>
    //   Adds a new or overrides an existing item with a specified key and
    //   value.
    //</summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the key already exists, the value
    //   of the related item is overridden with the supplied AValue argument.
    //   See Add for a method which does not override an existing item and
    //   adds an item only if does not already exist in the hash table.
    // </remarks>

    procedure Put(const AKey: UnicodeString; const AValue: TObject);

    // <summary>
    //   Adds a new item with a specified key and value to the hash table.
    // </summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the specified key already exists,
    //   this method does nothing. See Put for a method which can override
    //   the value of an existing item.
    // </remarks>

    procedure Add(const AKey: UnicodeString; const AValue: TObject);

    // <summary>
    //   Returns the value of a previously added item.
    // <summary>
    // <param name="AKey">
    //   The key of the item whose value should be returned.
    // </param>
    // <returns>
    //   The value of a previously added item or nil if the passed key is
    //   unknown.
    // </returns>
    // <remarks>
    //   This method returns the value of an item which has previously
    //   been added with the Put or Add methods. If the requested item is
    //   not known (i.e. already removed from this hash table or never
    //   added), this method returns nil.
    // </remarks>

    function Get(const AKey: UnicodeString): TObject;

    // <summary>
    //   Returns the value of the current item in an iteration loop.
    // </summary>
    // <returns>
    //   The value of the current item in an iteration loop or nil if the
    //   loop has already ended.
    // </returns>
    // <remarks>
    //   To initialize a new iteration loop, call Restart. To then iterate
    //   over the items in this hash table, call the Next method in a loop
    //   until it returns False. Please see the Next method for an example.
    // </remarks>

    property CurrentValue: TObject read GetCurrentValue;

    // <summary>
    //   Gets or sets the value for a specified item.
    // </summary>
    // <returns>
    //   The value of a previously added item or nil if the passed key is
    //   unknown.
    // </returns>
    // <remarks>
    //   This property behaves like the Get and Put methods. When accessed
    //   in write mode, this property adds a new item with the specified
    //   key and value to this hash table. If the key already exists, the
    //   value of the related item is overridden. In read mode, this property
    //   returns the value of an existing item. If the requested item is not
    //   known (i.e. already removed from this hash table or never added),
    //   this property returns nil.
    // </remarks>

    property Items[const AKey: UnicodeString]: TObject read Get write Put;
      default;
  end;

  // <summary>
  //   Provides a hash table for string keys and integer values.
  // </summary>
  // <remarks>
  //   This concrete implementation of a hash table is capable of storing
  //   integers. For a hash table which can store strings, please refer to
  //   the documentation of the TSiStringHash class. Also, for a hash table
  //   which can store objects, please refer to TSiObjectHash.
  // </remarks>

  TSiIntegerHash = class(TSiHash)
  private
    function GetCurrentValue: Integer;
  protected
    procedure DoDispose(const APointer: Pointer); override;
  public

    // <summary>
    //   Increments the value of an existing item by one or adds a new
    //   item.
    // </summary>
    // <param name="AKey">
    //   The key of the item whose value should be incremented by one.
    // </param>
    // <returns>
    //   The new integer value of the item. If the item was unknown before
    //   calling this method, 1 is returned. Otherwise this method returns
    //   the old value of the item plus one.
    // </returns>
    // <remarks>
    //   This method increments the integer value of the item identified
    //   by the passed AKey argument by one. If the passed key is unknown,
    //   then this method adds a new item with the supplied AKey argument
    //   as key and 1 as value.
    //
    //   See Dec for a method which can decrement the integer value of an
    //   item.
    // </remarks>

    function Inc(const AKey: UnicodeString): Integer;

    // <summary>
    //   Decrements the value of an item by one or removes the item.
    // </summary>
    // <param name="AKey">
    //   The key of the item whose value should be decremented by one.
    // </param>
    // <returns>
    //   The new integer value of the item. If the item was unknown before
    //   calling this method, -1 is returned. Otherwise this method returns
    //   the old value of the item minus one.
    // </returns>
    // <remarks>
    //   This method decrements the integer value of the item identified
    //   by the passed AKey argument by one. If the passed key is unknown,
    //   then this method adds a new item with the supplied AKey argument
    //   as key and -1 as value.
    //
    //   See Inc for a method which can increment the integer value of an
    //   item.
    // </remarks>

    function Dec(const AKey: UnicodeString): Integer;

    // <summary>
    //   Adds a new or overrides an existing item with a specified key and
    //   value.
    //</summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the key already exists, the value
    //   of the related item is overridden with the supplied AValue argument.
    //   See Add for a method which does not override an existing item and
    //   adds an item only if does not already exist in the hash table.
    // </remarks>

    procedure Put(const AKey: UnicodeString; const AValue: Integer);

    // <summary>
    //   Adds a new item with a specified key and value to the hash table.
    // </summary>
    // <param name="AKey">The key of the item to add.</param>
    // <param name="AValue">The value of the item to add.</param>
    // <remarks>
    //   This method adds a new item with the specified key and value
    //   arguments to this hash table. If the specified key already exists,
    //   this method does nothing. See Put for a method which can override
    //   the value of an existing item.
    // </remarks>

    procedure Add(const AKey: UnicodeString; const AValue: Integer);

    // <summary>
    //   Returns the value of a previously added item.
    // <summary>
    // <param name="AKey">
    //   The key of the item whose value should be returned.
    // </param>
    // <returns>
    //   The value of a previously added item or -1 if the passed key is
    //   unknown.
    // </returns>
    // <remarks>
    //   This method returns the value of an item which has previously
    //   been added with the Put or Add methods. If the requested item is
    //   not known (i.e. already removed from this hash table or never
    //   added), this method returns -1.
    // </remarks>

    function Get(const AKey: UnicodeString): Integer;

    // <summary>
    //   Returns the value of the current item in an iteration loop.
    // </summary>
    // <returns>
    //   The value of the current item in an iteration loop or -1 if the
    //   loop has already ended.
    // </returns>
    // <remarks>
    //   To initialize a new iteration loop, call Restart. To then iterate
    //   over the items in this hash table, call the Next method in a loop
    //   until it returns False. Please see the Next method for an example.
    // </remarks>

    property CurrentValue: Integer read GetCurrentValue;

    // <summary>
    //   Gets or sets the value for a specified item.
    // </summary>
    // <returns>
    //   The value of a previously added item or -1 if the passed key is
    //   unknown.
    // </returns>
    // <remarks>
    //   This property behaves like the Get and Put methods. When accessed
    //   in write mode, this property adds a new item with the specified
    //   key and value to this hash table. If the key already exists, the
    //   value of the related item is overridden. In read mode, this property
    //   returns the value of an existing item. If the requested item is not
    //   known (i.e. already removed from this hash table or never added),
    //   this property returns -1.
    // </remarks>

    property Items[const AKey: UnicodeString]: Integer read Get write Put;
      default;
  end;

  TSiBytes = array of Byte;

  // <summary>
  //   Represents a simple collection of key/value pairs.
  // </summary>
  // <remarks>
  //   The TSiLookupTable class is responsible for storing and returning
  //   values which are organized by keys. Values can be added with the Put
  //   method. To query a string value for a given key, the GetStringValue
  //   method can be used. To query and automatically convert values to
  //   types other than string, please have a look at the Get method
  //   family.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiLookupTable = class(TObject)
  private
    FItems: TSiStringHash;
    class function IsValidInteger(const AValue: UnicodeString): Boolean;
    class function IsValidSizeUnit(const AUnit: UnicodeString): Boolean;
    class function IsValidTimespanUnit(const AUnit: UnicodeString): Boolean;
    class function IsValidHex(const AValue: UnicodeString): Boolean;
    class function ConvertHexValue(const AValue: UnicodeString): TSiBytes;
    class function ConvertHexString(const AValue: UnicodeString): TSiBytes;
    class function ConvertUnicodeValue(const AValue: UnicodeString): TSiBytes;
    function GetCount: Integer;
  public

    // <summary>
    //   Creates and initializes a TSiLookupTable instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases any resources.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Adds or updates an element with a specified key and value to
    //   the TSiLookupTable.
    // </summary>
    // <param name="AKey">The key of the element.</param>
    // <param name="AValue">The value of the element.</param>
    // <remarks>
    //   This method adds a new element with a given key and value to
    //   the collection of key/value pairs. If an element for the
    //   given key already exists, the original element's value is
    //   updated.
    // </remarks>

    procedure Put(const AKey, AValue: UnicodeString);

    // <summary>
    //   Adds a new element with a specified key and value to the
    //   TSiLookupTable.
    // </summary>
    // <param name="AKey">The key of the element.</param>
    // <param name="AValue">The value of the element.</param>
    // <remarks>
    //   This method adds a new element with a given key and value to
    //   the collection of key/value pairs. If an element for the
    //   given key already exists, the original element's value is
    //   not updated.
    // </remarks>

    procedure Add(const AKey, AValue: UnicodeString);

    // <summary>
    //   Removes an existing element with a given key from this lookup
    //   table.
    // </summary>
    // <param name="AKey">The key of the element to remove.</param>
    // <remarks>
    //   This method removes the element with the given key from the
    //   internal list. Nothing happens if no element with the given
    //   key can be found.
    // </remarks>

    procedure Remove(const AKey: UnicodeString);

    // <summary>
    //   Removes all key/value pairs of the collection.
    // </summary>

    procedure Clear;

    // <summary>
    //   Tests if the collection contains a value for a given key.
    // </summary>
    // <param name="AKey">The key to test for.</param>
    // <returns>
    //   True if a value exists for the given key and false otherwise.
    // </returns>

    function Contains(const AKey: UnicodeString): Boolean;

    // <summary>
    //   Returns a value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value for a given key if an element with the given
    //   key exists or ADefaultValue otherwise.
    // </returns>

    function GetStringValue(const AKey, ADefaultValue: UnicodeString):
      UnicodeString;

    // <summary>
    //   Returns a value of an element converted to an Integer for a given
    //   key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to an Integer for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Integer or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Integer.
    //   Only non-negative Integer values are recognized as valid.
    // </remarks>

    function GetIntegerValue(const AKey: UnicodeString;
      const ADefaultValue: Integer): Integer;

    // <summary>
    //   Returns a value of an element converted to a Boolean for a
    //   given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a Boolean for the given key if an
    //   element with the given key exists or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns a Boolean value of true if the found value
    //   of the given key matches either "true", "1" or "yes" and false
    //   otherwise. If the supplied key is unknown, the ADefaultValue
    //   argument is returned.
    // </remarks>

    function GetBooleanValue(const AKey: UnicodeString;
      const ADefaultValue: Boolean): Boolean;

    // <summary>
    //   Returns a value of an element converted to a TSiLevel value
    //   for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to the corresponding TSiLevel value for
    //   the given key if an element with the given key exists and the
    //   found value is a valid TSiLevel value or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid TSiLevel
    //   value. Please see the TSiLevel enum for more information on the
    //   available values.
    // </remarks>

    function GetLevelValue(const AKey: UnicodeString;
      const ADefaultValue: TSiLevel): TSiLevel;

    // <summary>
    //   Returns a value of an element converted to an Integer for a given
    //   key. The Integer value is interpreted as a byte size and it is
    //   supported to specify byte units.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to an Integer for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Integer or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Integer
    //   or ends with an unknown byte unit. Only non-negative Integer
    //   values are recognized as valid.
    //
    //   It is possible to specify a size unit at the end of the value.
    //   If a known unit is found, this function multiplies the
    //   resulting value with the corresponding factor. For example, if
    //   the value of the element is "1KB", the return value of this
    //   function would be 1024.
    //
    //   The following table lists the available units together with a
    //   short description and the corresponding factor.
    //
    //   <table>
    //   Unit Name  Description  Factor
    //   -          -            -
    //   KB         Kilo Byte    1024
    //   MB         Mega Byte    1024^2
    //   GB         Giga Byte    1024^3
    //   </table>
    //
    //   If no unit is specified, this function defaults to the KB unit.
    // </remarks>

    function GetSizeValue(const AKey: UnicodeString;
      const ADefaultValue: Int64): Int64;

    // <summary>
    //   Returns a value of an element converted to a TSiFileRotate
    //   value for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a TSiFileRotate value for the
    //   given key if an element with the given key exists and the found
    //   value is a valid TSiFileRotate or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid
    //   TSiFileRotate value. For a complete list of available values,
    //   please have a look the TSiFileRotate enum.
    // </remarks>

    function GetRotateValue(const AKey: UnicodeString;
      const ADefaultValue: TSiFileRotate): TSiFileRotate;

    // <summary>
    //   Returns a byte array value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ASize">
    //   The desired size in bytes of the returned byte array. If
    //   the element value does not have the expected size, it is
    //   shortened or padded automatically.
    // </param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown or if the
    //   found value has an invalid format.
    // </param>
    // <returns>
    //   Either the value converted to a byte array for the given key
    //   if an element with the given key exists and the found value
    //   has a valid format or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   The returned byte array always has the desired length as
    //   specified by the ASize argument. If the element value does
    //   not have the required size after conversion, it is shortened
    //   or padded (with zeros) automatically. This method returns
    //   the ADefaultValue argument if either the supplied key is
    //   unknown or the found value does not have a valid format
    //   (e.g. invalid characters when using hexadecimal strings).
    // </remarks>

    function GetBytesValue(const AKey: UnicodeString; const ASize: Integer;
      const ADefaultValue: TSiBytes): TSiBytes;

    // <summary>
    //   Returns a TColor value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown or if the
    //   found value has an invalid format.
    // </param>
    // <returns>
    //   Either the value converted to a TColor value for the given key
    //   if an element with the given key exists and the found value
    //   has a valid format or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   The element value must be specified as hexadecimal string.
    //   To indicate that the element value represents a hexadecimal
    //   string, the element value must begin with "0x", "&H" or "$".
    //   A '0' nibble is appended if the hexadecimal string has an odd
    //   length.
    //
    //   The hexadecimal value must represent a three or four byte
    //   integer value. The hexadecimal value is handled as follows.
    //
    //   <table>
    //   Bytes          Format
    //   -              -
    //   3              RRGGBB
    //   4              AARRGGBB
    //   Other          Ignored
    //   </table>
    //
    //   A stands for the alpha channel and R, G and B represent the
    //   red, green and blue channels, respectively. If the value is not
    //   given as hexadecimal value with a length of 6 or 8 characters
    //   excluding the hexadecimal prefix identifier or if the value
    //   does not have a valid hexadecimal format, this method returns
    //   ADefaultValue.
    //
    //   Please note that the result value of this methd does not include
    //   an alpha channel value if a valid color can be found since Delphi
    //   does not support alpha channels.
    // </remarks>

    function GetColorValue(const AKey: UnicodeString;
      const ADefaultValue: TColor): TColor;

    // <summary>
    //   Returns a value of an element converted to a Cardinal (unsigned
    //   integer) for a given key. The Cardinal value is interpreted as a
    //   time span and it is supported to specify time span units.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a Cardinal for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Cardinal or ADefaultValue otherwise. The value is returned
    //   in milliseconds.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Cardinal
    //   or ends with an unknown time span unit.
    //
    //   It is possible to specify a time span unit at the end of the
    //   value. If a known unit is found, this function multiplies the
    //   resulting value with the corresponding factor. For example, if
    //   the value of the element is "1s", the return value of this
    //   function would be 1000.
    //
    //   The following table lists the available units together with a
    //   short description and the corresponding factor.
    //
    //   <table>
    //   Unit Name  Description  Factor
    //   -          -            -
    //   s          Seconds      1000
    //   m          Minutes      60*s
    //   h          Hours        60*m
    //   d          Days         24*h
    //   </table>
    //
    //   If no unit is specified, this function defaults to the Seconds
    //   unit. Please note that the value is always returned in
    //   milliseconds.
    // </remarks>

    function GetTimespanValue(const AKey: UnicodeString;
      const ADefaultValue: Cardinal): Cardinal;

    // <summary>
    //   Returns the number of key/value pairs of this collection.
    // </summary>

    property Count: Integer read GetCount;
  end;

  // <summary>
  //   Manages connection variables.
  // </summary>
  // <remarks>
  //   This class manages a list of connection variables. Connection
  //   variables are placeholders for strings in the
  //   <link TSmartInspect.Connections, connections string> of the
  //   TSmartInspect class. Please see TSmartInspect.SetVariable for
  //   more information.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiProtocolVariables = class
  private
    FItems: TSiStringHash;
    FLock: TCriticalSection;
    function GetCount: Integer;
  public

    // <summary>
    //   Creates and initializes a TSiProtocolVariables instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases any resources.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Adds or updates an element with a specified key and value to
    //   the set of connection variables.
    // </summary>
    // <param name="AKey">The key of the element.</param>
    // <param name="AValue">The value of the element.</param>
    // <remarks>
    //   This method adds a new element with a given key and value to
    //   the set of connection variables. If an element for the given
    //   key already exists, the original element's value is updated.
    // </remarks>

    procedure Put(const AKey, AValue: UnicodeString);

    // <summary>
    //   Adds a new element with a specified key and value to the
    //   set of connection variables.
    // </summary>
    // <param name="AKey">The key of the element.</param>
    // <param name="AValue">The value of the element.</param>
    // <remarks>
    //   This method adds a new element with a given key and value to
    //   the set of connection variables. If an element for the given
    //   key already exists, the original element's value is not
    //   updated.
    // </remarks>

    procedure Add(const AKey, AValue: UnicodeString);

    // <summary>
    //   Removes an existing element with a given key from this set
    //   of connection variables.
    // </summary>
    // <param name="AKey">The key of the element to remove.</param>
    // <remarks>
    //   This method removes the element with the given key from the
    //   internal set of connection variables. Nothing happens if no
    //   element with the given key can be found.
    // </remarks>

    procedure Remove(const AKey: UnicodeString);

    // <summary>
    //   Removes all key/value pairs of the collection.
    // </summary>

    procedure Clear;

    // <summary>
    //   Tests if the collection contains a value for a given key.
    // </summary>
    // <param name="AKey">The key to test for.</param>
    // <returns>
    //   True if a value exists for the given key and false otherwise.
    // </returns>

    function Contains(const AKey: UnicodeString): Boolean;

    // <summary>
    //   Returns a value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value for a given key if an element with the given
    //   key exists or ADefaultValue otherwise.
    // </returns>

    function Get(const AKey: UnicodeString): UnicodeString;

    // <summary>
    //   Expands and returns a connections string.
    // </summary>
    // <param name="connections">
    //	  The connections string to expand and return.
    // </param>
    // <returns>The expanded connections string.</returns>
    // <remarks>
    //  This method replaces all variables which have previously
    //  been added to this collection (with Add or Put) in the
    //  given connections string with their respective values and
    //  then returns it. Variables in the connections string must
    //  have the following form: $variable$.
    // </remarks>

    function Expand(const AConnections: UnicodeString): UnicodeString;

    // <summary>
    //   Returns the number of key/value pairs of this collection.
    // </summary>

    property Count: Integer read GetCount;
  end;

  // <summary>
  //   Assists in building a SmartInspect connections string.
  // </summary>
  // <remarks>
  //   The TSiConnectionsBuilder class assists in creating connections
  //   strings as used by the TSmartInspect.Connections property. To get
  //   started, please have a look at the following example. For general
  //   information about connections strings, please refer to the
  //   TSmartInspect.Connections property.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>
  // <example>
  // <code>
  //  LBuilder := TSiConnectionsBuilder.Create;
  //  try
  //    LBuilder.BeginProtocol('file');
  //    LBuilder.AddOption('filename', 'log.sil');
  //    LBuilder.AddOption('append', True);
  //    LBuilder.EndProtocol;
  //    Si.Connections := LBuilder.Connections;
  //  finally
  //    LBuilder.Free;
  //  end;
  // </code>
  // </example>

  TSiConnectionsBuilder = class
  private
    FBuilder: TSiStringBuilder;
    FHasOptions: Boolean;
    function Escape(const AValue: UnicodeString): UnicodeString;
    procedure Append(const AValue: UnicodeString);
    function GetConnections: UnicodeString;
  public

    // <summary>
    //   Creates and initializes a new TSiConnectionsBuilder instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overriden. Releases the resources of this TSiConnectionsBuilder
    //   object.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Clears this TSiConnectionsBuilder instance by removing all
    //   protocols and their options.
    // </summary>
    // <remarks>
    //   After this method has been called, the Connections property
    //   returns an empty string.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Begins a new protocol section.
    // </summary>
    // <param name="AProtocol">The name of the new protocol.</param>
    // <remarks>
    //   This method begins a new protocol with the supplied name.
    //   All subsequent protocol options are added to this protocol
    //   until the new protocol section is closed by calling the
    //   EndProtocol method.
    // </remarks>

    procedure BeginProtocol(const AProtocol: UnicodeString);

    // <summary>
    //   Ends the current protocol section.
    // </summary>
    // <remarks>
    //   This method ends the current protocol. To begin a new protocol
    //   section, use the BeginProtocol method.
    // </remarks>

    procedure EndProtocol;

    // <summary>
    //   Overloaded. Adds a new string option to the current protocol
    //   section.
    // </summary>
    // <param name="AKey">The key of the new option.</param>
    // <param name="AValue">The value of the new option.</param>
    // <remarks>
    //   This method adds a new string option to the current protocol
    //   section. The supplied AValue argument is properly escaped if
    //   necessary.
    // </remarks>

    procedure AddOption(const AKey, AValue: UnicodeString); overload;

    // <summary>
    //   Overloaded. Adds a new Boolean option to the current protocol
    //   section.
    // </summary>
    // <param name="AKey">The key of the new option.</param>
    // <param name="AValue">The value of the new option.</param>
    // <remarks>
    //   This method adds a new Boolean option to the current protocol
    //   section.
    // </remarks>

    procedure AddOption(const AKey: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Adds a new Integer option to the current protocol
    //   section.
    // </summary>
    // <param name="AKey">The key of the new option.</param>
    // <param name="AValue">The value of the new option.</param>
    // <remarks>
    //   This method adds a new Integer option to the current protocol
    //   section.
    // </remarks>

    procedure AddOption(const AKey: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Adds a new TSiLevel option to the current protocol
    //   section.
    // </summary>
    // <param name="AKey">The key of the new option.</param>
    // <param name="AValue">The value of the new option.</param>
    // <remarks>
    //   This method adds a new TSiLevel option to the current protocol
    //   section.
    // </remarks>

    procedure AddOption(const AKey: UnicodeString;
      const AValue: TSiLevel); overload;

    // <summary>
    //   Overloaded. Adds a new TSiFileRotate option to the current
    //   protocol section.
    // </summary>
    // <param name="AKey">The key of the new option.</param>
    // <param name="AValue">The value of the new option.</param>
    // <remarks>
    //   This method adds a new TSiFileRotate option to the current
    //   protocol section.
    // </remarks>

    procedure AddOption(const AKey: UnicodeString;
      const AValue: TSiFileRotate); overload;

    // <summary>
    //   Returns the built connections string.
    // </summary>
    // <remarks>
    //   This read-only property returns the connections string which
    //   has previously been built with the BeginProtocol, AddOption
    //   and EndProtocol methods.
    // </remarks>

    property Connections: UnicodeString read GetConnections;
  end;

  TSiConnectionsParser = class;

  // <summary>
  //   This is the callback type for the TSiConnectionsParser.Parse
  //   method.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AProtocol">The protocol which has been found.</param>
  // <param name="AOptions">The options of the new protocol.</param>
  // <remarks>
  //   In addition to the ASender parameter, the AProtocol and AOptions
  //   arguments will be passed to the event handlers which offer the
  //   possibility of retrieving information about the found protocol.
  // </remarks>

  TSiConnectionsParserEvent = procedure (ASender: TSiConnectionsParser;
    AProtocol, AOptions: UnicodeString) of object;

  // <summary>
  //   Responsible for parsing a SmartInspect connections string.
  // </summary>
  // <remarks>
  //   This class offers a single method only, called Parse, which is
  //   responsible for parsing a connections string. This method informs
  //   the caller about found protocols and options with a supplied
  //   callback.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiConnectionsParser = class
  private
    procedure InternalParse(const AConnections: UnicodeString;
      const ACallback: TSiConnectionsParserEvent);
    procedure DoProtocol(const ACallback: TSiConnectionsParserEvent;
      const AProtocol, AOptions: UnicodeString);
  public

    // <summary>
    //   Parses a connections string.
    // </summary>
    // <param name="AConnections">The connections string to parse.</param>
    // <param name="ACallback">
    //   The callback which should be informed about found protocols and
    //   their options.
    // </param>
    // <remarks>
    //   This method parses the supplied connections string and informs
    //   the caller about found protocols and options with the supplied
    //   callback.
    //
    //   For information about the correct syntax, please refer to the
    //   documentation of the TSmartInspect.Connections property.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   ESmartInspectError       Invalid connections string syntax.
    // </table>
    // </exception>

    procedure Parse(const AConnections: UnicodeString;
      const ACallback: TSiConnectionsParserEvent);
  end;

  TSiOptionsParser = class;

  // <summary>
  //   This is the callback type for the TSiOptionsParser.Parse method.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AProtocol">The protocol of the new option.</param>
  // <param name="AKey">The key of the new option.</param>
  // <param name="AValue">The value of the new option.</param>
  // <remarks>
  //   In addition to the ASender parameter, the AProtocol, AKey and
  //   AValue arguments will be passed to the event handlers which
  //   offer the possibility of retrieving information about the found
  //   option.
  // </remarks>

  TSiOptionsParserEvent = procedure (ASender: TSiOptionsParser;
    AProtocol, AKey, AValue: UnicodeString) of object;

  // <summary>
  //   Responsible for parsing the options part of a SmartInspect
  //   connections string.
  // </summary>
  // <remarks>
  //   This class offers a single method only, called Parse, which is
  //   responsible for parsing the options part of a connections
  //   string. This method informs the caller about found options with
  //   a supplied callback.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiOptionsParser = class
  private
    procedure InternalParse(const AProtocol, AOptions: UnicodeString;
      const ACallback: TSiOptionsParserEvent);
    procedure DoOption(const ACallback: TSiOptionsParserEvent;
      const AProtocol, AKey, AValue: UnicodeString);
  public

    // <summary>
    //   Parses the options part of a connections string.
    // </summary>
    // <param name="AProtocol">The related protocol.</param>
    // <param name="AOptions">The options to parse.</param>
    // <param name="ACallback">
    //   The callback which should be informed about found options.
    // </param>
    // <remarks>
    //   This method parses the supplied options part of a connections
    //   string and informs the caller about found options with the
    //   supplied callback.
    //
    //   For information about the correct syntax of the options,
    //   please refer to the documentation of the TSiProtocol.Options
    //   property.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   SmartInspectException    Invalid options string syntax.
    // </table>
    // </exception>

    procedure Parse(const AProtocol, AOptions: UnicodeString;
      const ACallback: TSiOptionsParserEvent);
  end;

  // <summary>
  //   Responsible for handling the SmartInspect configuration and loading
  //   it from a file.
  // </summary>
  // <remarks>
  //   This class is responsible for loading and reading values from a
  //   SmartInspect configuration file. For more information, please refer
  //   to the TSmartInspect.LoadConfiguration method.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiConfiguration = class(TObject)
  private
    FItems: TSiLookupTable;
    FKeys: TSiStringList;
    procedure Parse(const APair: UnicodeString);
    function GetCount: Integer;
  public

    // <summary>
    //   Creates and initializes a new TSiConfiguration instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases any resources.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Removes all key/value pairs of the configuration.
    // </summary>

    procedure Clear;

    // <summary>
    //   Loads the configuration from a file.
    // </summary>
    // <param name="AFileName">
    //   The name of the file to load the configuration from.
    // </param>
    // <remarks>
    //   This method loads key/value pairs separated with a '='
    //   character from a file. Empty, unrecognized lines or lines
    //   beginning with a ';' character are ignored.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type          Condition
    //   -                       -
    //   EStreamError            An I/O error occurred while trying
    //                             to load the configuration or if the
    //                             specified file does not exist.
    // </table>
    // </exception>

    procedure LoadFromFile(const AFileName: UnicodeString);

    // <summary>
    //   Tests if the configuration contains a value for a given key.
    // </summary>
    // <param name="AKey">The key to test for.</param>
    // <returns>
    //   True if a value exists for the given key and false otherwise.
    // </returns>

    function Contains(const AKey: UnicodeString): Boolean;

    // <summary>
    //   Returns a value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value for a given key if an element with the given
    //   key exists or ADefaultValue otherwise.
    // </returns>

    function ReadString(const AKey, ADefaultValue: UnicodeString):
      UnicodeString;

    // <summary>
    //   Returns a value of an element converted to an Integer for a given
    //   key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to an Integer for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Integer or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Integer.
    //   Only non-negative Integer values are recognized as valid.
    // </remarks>

    function ReadInteger(const AKey: UnicodeString;
      const ADefaultValue: Integer): Integer;

    // <summary>
    //   Returns a value of an element converted to a Boolean for a
    //   given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a Boolean for the given key if an
    //   element with the given key exists or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns a Boolean value of true if the found value
    //   of the given key matches either "true", "1" or "yes" and false
    //   otherwise. If the supplied key is unknown, the ADefaultValue
    //   argument is returned.
    // </remarks>

    function ReadBoolean(const AKey: UnicodeString;
      const ADefaultValue: Boolean): Boolean;

    // <summary>
    //   Returns a value of an element converted to a TSiLevel value
    //   for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to the corresponding TSiLevel value for
    //   the given key if an element with the given key exists and the
    //   found value is a valid TSiLevel value or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid TSiLevel
    //   value. Please see the TSiLevel enum for more information on the
    //   available values.
    // </remarks>

    function ReadLevel(const AKey: UnicodeString;
      const ADefaultValue: TSiLevel): TSiLevel;

    // <summary>
    //   Returns a TColor value of an element for a given key.
    // </summary>
    // <param name="AKey">The key whose value to return.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown or if the
    //   found value has an invalid format.
    // </param>
    // <returns>
    //   Either the value converted to a TColor value for the given key
    //   if an element with the given key exists and the found value
    //   has a valid format or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   The element value must be specified as hexadecimal string.
    //   To indicate that the element value represents a hexadecimal
    //   string, the element value must begin with "0x", "&H" or "$".
    //   A '0' nibble is appended if the hexadecimal string has an odd
    //   length.
    //
    //   The hexadecimal value must represent a three or four byte
    //   integer value. The hexadecimal value is handled as follows.
    //
    //   <table>
    //   Bytes          Format
    //   -              -
    //   3              RRGGBB
    //   4              AARRGGBB
    //   Other          Ignored
    //   </table>
    //
    //   A stands for the alpha channel and R, G and B represent the
    //   red, green and blue channels, respectively. If the value is not
    //   given as hexadecimal value with a length of 6 or 8 characters
    //   excluding the hexadecimal prefix identifier or if the value
    //   does not have a valid hexadecimal format, this method returns
    //   ADefaultValue.
    //
    //   Please note that the result value of this methd does not include
    //   an alpha channel value if a valid color can be found since Delphi
    //   does not support alpha channels.
    // </remarks>

    function ReadColor(const AKey: UnicodeString;
      const ADefaultValue: TColor): TColor;

    // <summary>
    //   Returns a key of this SmartInspect configuration for a
    //   given index.
    // </summary>
    // <param name="AIndex">
    //   The index in this SmartInspect configuration.
    // </param>
    // <returns>
    //   A key of this SmartInspect configuration for the given index.
    // </returns>
    // <remarks>
    //   To find out the total number of key/value pairs in this
    //   SmartInspect configuration, use Count. To get the value for
    //   a given key, use ReadString.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   ESmartInspectError       The AIndex argument is not a valid
    //                             index of this SmartInspect
    //                             configuration.
    // </table>
    // </exception>

    function ReadKey(const AIndex: Integer): UnicodeString;

    // <summary>
    //   Returns the number of key/value pairs of this SmartInspect
    //   configuration.
    // </summary>

    property Count: Integer read GetCount;
  end;

  // <summary>
  //   Is the abstract base class for all packets in the SmartInspect
  //   Delphi library.
  // </summary>
  // <remarks>
  //   This class is the base class for all packets in the SmartInspect
  //   Delphi library. The following table lists the available packets
  //   together with a short description.
  //
  //   <table>
  //   Packet                 Description
  //   -                      -
  //   TSiControlCommand      Responsible for administrative tasks like
  //                            clearing the Console.
  //
  //   TSiLogEntry            Represents the most important packet in the
  //                            entire SmartInspect concept. Is used for
  //                            the majority of logging methods in the
  //                            TSiSession class.
  //
  //   TSiLogHeader           Responsible for storing and transferring
  //                            log metadata. Used by the TSiPipeProtocol
  //                            and TSiTcpProtocol classes to support the
  //                            filter and trigger functionality of the
  //                            SmartInspect Router service application.
  //
  //   TSiProcessFlow         Responsible for managing thread and process
  //                            information about your application.
  //
  //   TSiWatch               Responsible for handling variable watches.
  //   </table>
  //
  //   Instances of TSiPacket are reference counted, that means they
  //   will be automatically freed when there are no references pointing
  //   to it anymore. To increment and decrement the reference counter,
  //   use the AddRef and Release methods. After an instance has been
  //   created with the Create constructor, the reference counter is
  //   always set to 1.
  // </remarks>
  // <threadsafety>
  //   This class and sub-classes are not guaranteed to be threadsafe.
  //   To ensure thread-safety, use ThreadSafe as well as the Lock and
  //   Unlock methods.
  // </threadsafety>

  TSiPacket = class(TObject)
  private
    FThreadSafe: Boolean;
    FRefCount: Integer;
    FLevel: TSiLevel;
    FBytes: Integer;
    FCriticalSection: TRTLCriticalSection;
    procedure SetThreadSafe(const AValue: Boolean);
  protected

    // <summary>
    //   Calculates and returns the total memory size occupied by a
    //   packet.
    // </summary>
    // <returns>
    //   The total memory size occupied by a packet.
    // </returns>
    // <remarks>
    //   Subclasses are intended to return the total occupied memory
    //   size of a packet here. This functionality is needed by the
    //   <link TSiProtocol.IsValidOption, backlog> protocol feature
    //   to calculate the total backlog queue size.
    // </remarks>

    function GetSize: Integer; virtual; abstract;

    // <summary>Returns the packet type.</summary>
    // <returns>The type of this packet.</returns>
    // <remarks>
    //   Real packet implementations are intended to return their
    //   packet type here. Please see TSiPacketType for a list of
    //   available packet types.
    // </remarks>

    function GetPacketType: TSiPacketType; virtual; abstract;
  public

    // <summary>
    //   Creates and initializes a TSiPacket object by setting the
    //   reference counter to the initial value of 1. The default log
    //   level of a packet is lvMessage.
    // </summary>
    // <remarks>
    //   Subclasses must always call this constructor to initialize
    //   the reference counter correctly. <b>Please note</b>: Since
    //   packets are reference counted, to not call Free directly, use
    //   the Release method instead.
    // </remarks>

    constructor Create;

    // <summary>
    //   Overriden. Destroys this TSiPacket object and releases any
    //   resources.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Increments the reference counter by one.
    // </summary>
    // <remarks>
    //   This method increments the reference counter by one. Every call
    //   to this method must have a matching call to the corresponding
    //   Release method.
    // </remarks>

    procedure AddRef;

    // <summary>
    //   Decrements the reference counter by one.
    // </summary>
    // <remarks>
    //   This method decrements the reference counter by one. If the
    //   reference counter reaches 0, this object is freed. Every call
    //   to this method must have a matching call to the corresponding
    //   AddRef or Create method.
    // </remarks>

    procedure Release;

    // <summary>
    //   Locks this packet for safe multi-threaded packet processing
    //   if this packet is operating in thread-safe mode.
    // </summary>
    // <remarks>
    //   Call this method before reading or changing properties of a
    //   packet when using this packet from multiple threads at the
    //   same time. This is needed, for example, when one or more
    //   <link TSmartInspect.Connections, connections> of a TSmartInspect
    //   object are told to operate in <link TSiProtocol.IsValidOption,
    //   asynchronous protocol mode>. Each Lock call must be
    //   matched by a call to Unlock.
    //
    //   Before using Lock and Unlock in a multi-threaded environment
    //   you must indicate that this packet should operate in
    //   thread-safe mode by setting the ThreadSafe property to true.
    //   Otherwise, the Lock and Unlock methods do nothing. Note
    //   that setting the ThreadSafe property is done automatically
    //   if this packet has been created by the TSiSession class and
    //   is processed by a related TSmartInspect object which has one
    //   or more connections which operate in asynchronous protocol
    //   mode.
    // </remarks>

    procedure Lock;

    // <summary>
    //   Unlocks a previously locked packet.
    // </summary>
    // <remarks>
    //   Call this method after reading or changing properties of a
    //   packet when using this packet from multiple threads at the
    //   same time. This is needed, for example, when one or more
    //   <link TSmartInspect.Connections, connections> of a TSmartInspect
    //   object are told to operate in <link TSiProtocol.IsValidOption,
    //   asynchronous protocol mode>. Each Unlock call must be
    //   matched by a previous call to Lock.
    //
    //   Before using Lock and Unlock in a multi-threaded environment
    //   you must indicate that this packet should operate in
    //   thread-safe mode by setting the ThreadSafe property to true.
    //   Otherwise, the Lock and Unlock methods do nothing. Note
    //   that setting the ThreadSafe property is done automatically
    //   if this packet has been created by the TSiSession class and
    //   is processed by a related TSmartInspect object which has one
    //   or more connections which operate in asynchronous protocol
    //   mode.
    // </remarks>

    procedure Unlock;

    // <summary>
    //   Represents the log level of this packet.
    // </summary>
    // <remarks>
    //   Every packet can have a certain log level value. Log levels
    //   describe the severity of a packet. Please see the TSiLevel
    //   enum for more information about log levels and their usage.
    // </remarks>

    property Level: TSiLevel read FLevel write FLevel;

    // <summary>
    //   Represents the amount of bytes needed for storing this packet
    //   in the standard SmartInspect binary log file format as
    //   represented by TSiBinaryFormatter.
    // </summary>
    // <remarks>
    //   Please note that this property is only intended to be used by
    //   a possible Delphi implementation of the SmartInspect SDK. The
    //   SmartInspect SDK is a small library for reading SmartInspect
    //   binary log files and is available for download on the Gurock
    //   Software website.
    // </remarks>

    property Bytes: Integer read FBytes write FBytes;

    // <summary>Returns the packet type.</summary>
    // <remarks>
    //   This read-only property returns the type of this packet.
    //   Please see the TSiPacketType enum for a list of available
    //   packet types.
    // </remarks>

    property PacketType: TSiPacketType read GetPacketType;

    // <summary>
    //   Represents the total occupied memory size of this packet.
    // </summary>
    // <remarks>
    //   This read-only property returns the total occupied memory
    //   size of this packet. This functionality is used by the
    //   <link TSiProtocol.IsValidOption, backlog> protocol feature
    //   to calculate the total backlog queue size.
    // </remarks>

    property Size: Integer read GetSize;

    // <summary>
    //   Indicates if this packet is used in a multi-threaded
    //   SmartInspect environment.
    // </summary>
    // <remarks>
    //   Set this property to true before calling Lock and Unlock
    //   in a multi-threaded environment. Otherwise, the Lock and
    //   Unlock methods do nothing. Note that setting this property
    //   is done automatically if this packet has been created by
    //   the TSiSession class and is processed by a related
    //   TSmartInspect object which has one or more connections which
    //   operate in asynchronous protocol mode.
    //
    //   Setting this property must be done before using this packet
    //   from multiple threads simultaneously.
    // </remarks>

    property ThreadSafe: Boolean read FThreadSafe write SetThreadSafe;
  end;

  PSiPacketQueueItem = ^TSiPacketQueueItem;

  TSiPacketQueueItem = record
    Packet: TSiPacket;
    Next: PSiPacketQueueItem;
    Previous: PSiPacketQueueItem;
  end;

  TSiPacketQueue = class;

  // <summary>
  //   This is the event handler type for the TSiPacketQueue.OnDelete
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="APacket">
  //   The packet which is removed from the queue.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, an APacket argument will be
  //   passed to the event handlers which offers the possibility of doing
  //   some cleanup work like releasing the packet, for example.
  // </remarks>

  TSiPacketQueueEvent = procedure (ASender: TSiPacketQueue;
    APacket: TSiPacket) of object;

  // <summary>
  //   Manages a memory size limited queue of packets.
  // </summary>
  // <remarks>
  //   This class is responsible for managing a size limited queue of
  //   packets. This functionality is needed by the protocol
  //   <link TSiProtocol.IsValidOption, backlog> feature. The maximum
  //   total memory size of the queue can be set with the Backlog
  //   property. New packets can be added with the Push method. Packets
  //   which are no longer needed can be retrieved and removed from the
  //   queue with the Pop method.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiPacketQueue = class
  private
    FHead: PSiPacketQueueItem;
    FTail: PSiPacketQueueItem;
    FSize: Int64;
    FBacklog: Int64;
    FOnDelete: TSiPacketQueueEvent;
    FCount: Integer;
    procedure Resize;
    procedure SetBacklog(const AValue: Int64);
    procedure DoDelete(const APacket: TSiPacket);
  public

    // <summary>
    //   Adds a new packet to the queue.
    // </summary>
    // <param name="APacket">The packet to add.</param>
    // <remarks>
    //   This method adds the supplied packet to the queue. The size of
    //   the queue is incremented by the size of the supplied packet
    //   (plus some internal management overhead). If the total occupied
    //   memory size of this queue exceeds the Backlog limit after
    //   adding the new packet, then already added packets will be
    //   removed from this queue until the Backlog size limit is reached
    //   again.
    // </remarks>

    procedure Push(const APacket: TSiPacket);

    // <summary>
    //   Returns a packet and removes it from the queue.
    // </summary>
    // <returns>
    //   The removed packet or nil if the queue does not contain any
    //   packets.
    // </returns>
    // <remarks>
    //   If the queue is not empty, this method removes the oldest packet
    //   from the queue (also known as FIFO) and returns it. The total
    //   size of the queue is decremented by the size of the returned
    //   packet (plus some internal management overhead).
    // </remarks>

    function Pop: TSiPacket;

    // <summary>
    //   Removes all packets from the queue.
    // </summary>
    // <remarks>
    //   Removing all packets of the queue is done by calling the Pop
    //   method and the OnDelete event for every packet in the current
    //   queue.
    // </remarks>

    procedure Clear;

    // <summary>
    //   Specifies the total maximum memory size of this queue in
    //   bytes.
    // </summary>
    // <remarks>
    //   Each time a new packet is added with the Push method, it will
    //   be verified that the total occupied memory size of the queue
    //   still falls below the supplied Backlog limit. To satisfy this
    //   constraint, old packets are removed from the queue if necessary.
    //   For each automatically removed packet, the OnDelete event is
    //   called.
    // </remarks>

    property Backlog: Int64 read FBacklog write SetBacklog;

    // <summary>
    //   Occurs when a packet has been automatically removed from the
    //   queue.
    // </summary>
    // <seealso cref="TSiPacketQueueEvent"/>
    // <remarks>
    //   This event is fired for every packet which is automatically
    //   removed from the queue. Automatically removed in this context
    //   means that the packet is removed during the Clear method or
    //   when doing a queue resizing. A queue resizing can happen when
    //   the Backlog limit is reduced or when a new packet has been
    //   added with the Push method and old packets needs be purged.
    //   This event is never called when calling the Pop method.
    // </remarks>

    property OnDelete: TSiPacketQueueEvent read FOnDelete write FOnDelete;

    // <summary>
    //   Returns the current amount of packets in this queue.
    // </summary>
    // <remarks>
    //   For each added packet this counter is incremented by one and for
    //   each removed packet (either with the Pop method or automatically
    //   while resizing the queue) this counter is decremented by one. If
    //   the queue is empty, this property returns 0.
    // </remarks>

    property Count: Integer read FCount;
  end;

  // <summary>
  //   Represents the Log Header packet type which is used for storing
  //   and transferring log metadata.
  // </summary>
  // <remarks>
  //   The TSiLogHeader class is used to store and transfer log metadata.
  //   After the TSiPipeProtocol or TSiTcpProtocol has established a
  //   connection, a Log Header packet with the metadata of the current
  //   logging context is created and written. Log Header packets are
  //   used by the SmartInspect Router application for its filter and
  //   trigger functionality.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe. However, instances
  //   of this class will normally only be used in the context of a single
  //   thread.
  // </threadsafety>

  TSiLogHeader = class(TSiPacket)
  private
    FHostName: UnicodeString;
    FAppName: UnicodeString;
    function GetContent: UnicodeString;
  protected

    // <summary>
    //   Overridden. Returns the total occupied memory size of this Log
    //   Header packet.
    // </summary>
    // <returns>
    //   The total memory size of this Log Header.
    // </returns>
    // <remarks>
    //   The total occupied memory size of this Log Header is the size
    //   of memory occupied by all strings and any internal data
    //   structures of this Log Header.
    // </remarks>

    function GetSize: Integer; override;

    // <summary>
    //   Simply returns the ptLogHeader packet type.
    // </summary>
    // <returns>
    //   The type of this packet, namely ptLogHeader.
    // </returns>
    // <remarks>
    //   For a complete list of available packet types, please have
    //   a look at the documentation of the TSiPacketType enum.
    // </remarks>

    function GetPacketType: TSiPacketType; override;
  public

    // <summary>
    //   Represents the application name of this Log Header.
    // </summary>
    // <remarks>
    //   The application name of a Log Header is usually set to the
    //   name of the application this Log Header is created in.
    // </remarks>

    property AppName: UnicodeString read FAppName write FAppName;

    // <summary>
    //   Represents the hostname of this Log Header.
    // </summary>
    // <remarks>
    //   The hostname of a Log Header is usually set to the name
    //   of the machine this Log Header is sent from.
    // </remarks>

    property HostName: UnicodeString read FHostName write FHostName;

    // <summary>
    //   Represents the entire content of this Log Header packet.
    // </summary>
    // <remarks>
    //   The content of a Log Header packet is a key-value (syntax:
    //   key=value) list of the properties of this Log Header packet
    //   (currently only the AppName and the HostName strings).
    //   Key-value pairs are separated by carriage return and newline
    //   characters.
    // </remarks>

    property Content: UnicodeString read GetContent;
  end;

  // <summary>
  //   Represents the Log Entry packet type which is used for nearly
  //   all logging methods in the TSiSession class.
  // </summary>
  // <remarks>
  //   TSiLogEntry is the most important packet type available in the
  //   SmartInspect concept. It is used for almost all logging methods
  //   in the TSiSession class, like, for example, TSiSession.LogMessage,
  //   TSiSession.LogObject or TSiSession.LogSql.
  //
  //   A Log Entry has several properties which describe its creation
  //   context (like a thread ID, timestamp or hostname) and other
  //   properties which specify the way the Console interprets this packet
  //   (like the viewer ID or the background color). Furthermore a Log
  //   Entry contains the actual data which will be displayed in the
  //   Console.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe. However, instances
  //   of this class will normally only be used in the context of a single
  //   thread.
  // </threadsafety>

  TSiLogEntry = class(TSiPacket)
  private
    FTimestamp: TDateTime;
    FViewerId: TSiViewerId;
    FLogEntryType: TSiLogEntryType;
    FAppName: UnicodeString;
    FHostName: UnicodeString;
    FSessionName: UnicodeString;
    FTitle: UnicodeString;
    FColor: TColor;
    FData: TMemoryStream;
    FThreadId: Cardinal;
    FProcessId: Cardinal;
    function GetHasData: Boolean;
    function GetData: TStream;
    procedure SetData(const AValue: TStream);
  protected

    // <summary>
    //   Overridden. Returns the total occupied memory size of this Log
    //   Entry packet.
    // </summary>
    // <returns>
    //   The total memory size of this Log Entry.
    // </returns>
    // <remarks>
    //   The total occupied memory size of this Log Entry is the size
    //   of memory occupied by all strings, the optional Data stream and
    //   any internal data structures of this Log Entry.
    // </remarks>

    function GetSize: Integer; override;

    // <summary>
    //   Simply returns the ptLogEntry packet type.
    // </summary>
    // <returns>
    //   The type of this packet, namely ptLogEntry.
    // </returns>
    // <remarks>
    //   For a complete list of available packet types, please have
    //   a look at the documentation of the TSiPacketType enum.
    // </remarks>

    function GetPacketType: TSiPacketType; override;

    // <summary>
    //   Indicates if this Log Entry contains optional data or not.
    // </summary>
    // <remarks>
    //   Returns true if this Log Entry packet contains optional data
    //   and false otherwise.
    // </remarks>

    property HasData: Boolean read GetHasData;
  public

    // <summary>
    //   Overloaded. Creates a TSiLogEntry instance and initializes its
    //   reference counter with a value of 1.
    // </summary>

    constructor Create; overload;

    // <summary>
    //   Overloaded. Creates a TSiLogEntry instance with a custom log
    //   entry type and custom viewer ID and initializes its reference
    //   counter with a value of 1.
    // </summary>
    // <param name="ALogEntryType">
    //   The type of the new Log Entry describes the way the Console
    //   interprets this packet. Please see TSiLogEntryType for more
    //   information.
    // </param>
    // <param name="AViewerId">
    //   The viewer ID of the new Log Entry describes which viewer
    //   should be used in the Console when displaying the data of
    //   this Log Entry. Please see TSiViewerId for more information.
    // </param>
    // <remarks>
    //   TSiLogEntry objects are reference counted. Instead of using
    //   the Free method directly to free this object, use Release
    //   instead. Please see the documentation of the AddRef and the
    //   Release method for more information.
    // </remarks>

    constructor Create(const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overridden. Releases all resources of this TSiLogEntry object.
    // </summary>
    // <remarks>
    //   If this Log Entry contains any optional data, this data
    //   stream is freed in this destructor.
    // </remarks>

    destructor Destroy; override;

    // <summary>
    //   Represents the session name of this Log Entry.
    // </summary>
    // <remarks>
    //   The session name of a Log Entry is normally set to the name
    //   of the session which sent this Log Entry.
    // </remarks>

    property SessionName: UnicodeString read FSessionName write FSessionName;

    // <summary>
    //   Represents the title of the Log Entry.
    // </summary>

    property Title: UnicodeString read FTitle write FTitle;

    // <summary>
    //   Represents the application name of this Log Entry.
    // </summary>
    // <remarks>
    //   The application name of a Log Entry is usually set to the
    //   name of the application this Log Entry is created in.
    // </remarks>

    property AppName: UnicodeString read FAppName write FAppName;

    // <summary>
    //   Represents the hostname of this Log Entry.
    // </summary>
    // <remarks>
    //   The hostname of a Log Entry is usually set to the name
    //   of the machine this Log Entry is sent from.
    // </remarks>

    property HostName: UnicodeString read FHostName write FHostName;

    // <summary>
    //   Represents the type of this Log Entry.
    // </summary>
    // <remarks>
    //   The type of this Log Entry describes the way the Console
    //   interprets this packet. Please see the TSiLogEntryType enum for
    //   more information.
    // </remarks>

    property LogEntryType: TSiLogEntryType read FLogEntryType
      write FLogEntryType;

    // <summary>
    //   Represents the viewer ID of this Log Entry.
    // </summary>
    // <remarks>
    //   The viewer ID of the Log Entry describes which viewer should
    //   be used in the Console when displaying the data of this Log
    //   Entry. Please see the TSiViewerId enum for more information.
    // </remarks>

    property ViewerId: TSiViewerId read FViewerId write FViewerId;

    // <summary>
    //   Represents the background color of this Log Entry.
    // </summary>
    // <remarks>
    //   The background color of a Log Entry is normally set to the
    //   color of the session which sent this Log Entry.
    // </remarks>

    property Color: TColor read FColor write FColor;

    // <summary>
    //   Represents the optional data stream of this Log Entry.
    // </summary>
    // <remarks>
    //   This property might be nil if this Log Entry does not
    //   contain any optional data.
    //
    //   By setting the optional data stream of a Log Entry, the
    //   data of the supplied AStream instance are copied into
    //   the internal stream of this Log Entry.
    //
    //   <b>Important:</b> Treat this stream as read-only. This
    //   means, modifying this stream in any way is not supported.
    //   Additionally, only pass streams which support seeking.
    //   Streams which do not support seeking cannot be used by
    //   this class.
    // </remarks>

    property Data: TStream read GetData write SetData;

    // <summary>
    //   Represents the timestamp of this Log Entry.
    // </summary>
    // <remarks>
    //   This property returns the creation time of this TSiLogEntry
    //   object. If specified, the SmartInspect Delphi library tries to
    //   use high-resolution timestamps. See TSmartInspect.Resolution
    //   for more information on timestamps.
    // </remarks>

    property Timestamp: TDateTime read FTimestamp write FTimestamp;

    // <summary>
    //   Represents the thread ID of this Log Entry.
    // </summary>
    // <remarks>
    //   This property represents the ID of the thread this Log Entry
    //   object was created in.
    // </remarks>

    property ThreadId: Cardinal read FThreadId write FThreadId;

    // <summary>
    //   Represents the process ID of this Log Entry.
    // </summary>
    // <remarks>
    //   This property represents the ID of the process this Log Entry
    //   object was created in.
    // </remarks>

    property ProcessId: Cardinal read FProcessId write FProcessId;
  end;

  // <summary>
  //   Represents the Control Command packet type which is used for
  //   administrative tasks like resetting or clearing the Console.
  // </summary>
  // <remarks>
  //   A Control Command is used for several administrative Console tasks.
  //   Among other things, this packet type allows you to
  //   <link TSiSession.ClearAll, reset the Console>.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe. However, instances
  //   of this class will normally only be used in the context of a single
  //   thread.
  // </threadsafety>

  TSiControlCommand = class(TSiPacket)
  private
    FControlCommandType: TSiControlCommandType;
    FData: TMemoryStream;
    function GetData: TStream;
    function GetHasData: Boolean;
    procedure SetData(const AValue: TStream);
  protected

    // <summary>
    //   Overridden. Returns the total occupied memory size of this Control
    //   Command packet.
    // </summary>
    // <returns>
    //   The total memory size of this Control Command.
    // </returns>
    // <remarks>
    //   The total occupied memory size of this Control Command is
    //   the size of memory occupied the optional Data stream and any
    //   internal data structures of this Control Command.
    // </remarks>

    function GetSize: Integer; override;

    // <summary>
    //   Overridden. Simply returns the ptControlCommand packet type.
    // </summary>
    // <returns>
    //   The type of this packet, namely ptControlCommand.
    // </returns>
    // <remarks>
    //   For a complete list of available packet types, please have a
    //   look at the documentation of the TSiPacketType enum.
    // </remarks>

    function GetPacketType: TSiPacketType; override;

    // <summary>
    //   Indicates if this Control Command contains optional data or
    //   not.
    // </summary>
    // <remarks>
    //   Returns true if this Control Command packet contains optional
    //   data and false otherwise.
    // </remarks>

    property HasData: Boolean read GetHasData;
  public

    // <summary>
    //   Overloaded. Creates a TSiControlCommand instance and initializes
    //   its reference counter with a value of 1.
    // </summary>

    constructor Create; overload;

    // <summary>
    //   Overloaded. Creates a TSiControlCommand instance with a custom
    //   control command type and initializes its reference counter with a
    //   value of 1.
    // </summary>
    // <param name="AControlCommandType">
    //   The type of the new Control Command describes the way the Console
    //   interprets this packet. Please see the TSiControlCommandType enum
    //   for more information.
    // </param>
    // <remarks>
    //   TSiControlCommand objects are reference counted. Instead of
    //   using the Free method directly to free this object, use Release
    //   instead. Please see the documentation of the AddRef and the
    //   Release method for more information.
    // </remarks>

    constructor Create(const AControlCommandType: TSiControlCommandType);
      overload;

    // <summary>
    //   Overridden. Releases all resources of this TSiControlCommand
    //   object.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Represents the type of this Control Command.
    // </summary>
    // <remarks>
    //   The type of the Control Command describes the way the Console
    //   interprets this packet. Please see the TSiControlCommandType
    //   enum for more information.
    // </remarks>

    property ControlCommandType: TSiControlCommandType
      read FControlCommandType write FControlCommandType;

    // <summary>
    //   Represents the optional data stream of the Control Command.
    // </summary>
    // <remarks>
    //   This property might be nil if this Control Command does not
    //   contain any optional data.
    //
    //   By setting the optional data stream of a Control Command,
    //   the data of the supplied AStream instance are copied into
    //   the internal stream of this Control Command.
    //
    //   <b>Important:</b> Treat this stream as read-only. This means,
    //   modifying this stream in any way is not supported. Additionally,
    //   only pass streams which support seeking. Streams which do not
    //   support seeking cannot be used by this class.
    // </remarks>

    property Data: TStream read GetData write SetData;
  end;

  // <summary>
  //   Represents the Watch packet type which is used in the Watch
  //   methods in the TSiSession classes.
  // </summary>
  // <remarks>
  //   A Watch is responsible for sending variables and their values
  //   to the Console. These key/value pairs will be displayed in the
  //   Watches toolbox. If a Watch with the same name is sent twice,
  //   the old value is overwritten and the Watches toolbox displays
  //   the most current value.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe. However, instances
  //   of this class will normally only be used in the context of a single
  //   thread.
  // </threadsafety>

  TSiWatch = class(TSiPacket)
  private
    FWatchType: TSiWatchType;
    FValue: UnicodeString;
    FName: UnicodeString;
    FTimestamp: TDateTime;
  protected

    // <summary>
    //   Overridden. Returns the total occupied memory size of this Watch
    //   packet.
    // </summary>
    // <returns>
    //   The total memory size of this Watch.
    // </returns>
    // <remarks>
    //   The total occupied memory size of this Watch is the size of
    //   memory occupied by all strings and any internal data structures
    //   of this Watch.
    // </remarks>

    function GetSize: Integer; override;

    // <summary>
    //   Simply returns the ptWatch packet type.
    // </summary>
    // <returns>
    //   The type of this packet, namely ptWatch.
    // </returns>
    // <remarks>
    //   For a complete list of available packet types, please have a
    //   look at the documentation of the TSiPacketType enum.
    // </remarks>

    function GetPacketType: TSiPacketType; override;
  public 

    // <summary>
    //   Overloaded. Creates a TSiWatch instance and initializes its
    //   reference counter with a value of 1.
    // </summary>

    constructor Create; overload;

    // <summary>
    //   Overloaded. Creates a TSiWatch instance with a custom watch
    //   type and initializes its reference counter with a value of 1.
    // </summary>
    // <param name="AWatchType">
    //   The type of the new Watch describes the variable type (String,
    //   Integer and so on). Please see the TSiWatchType enum for more
    //   information.
    // </param>
    // <remarks>
    //   TSiWatch objects are reference counted. Instead of using the Free
    //   method directly to free this object, use Release instead. Please
    //   see the documentation of the AddRef and the Release method for more
    //   information.
    // </remarks>

    constructor Create(const AWatchType: TSiWatchType); overload;

    // <summary>
    //   Represents the name of this Watch.
    // </summary>
    // <remarks>
    //   If a Watch with the same name is sent twice, the old value is
    //   overwritten and the Watches toolbox displays the most current
    //   value.
    // </remarks>

    property Name: UnicodeString read FName write FName;

    // <summary>
    //   Represents the value of this Watch.
    // </summary>
    // <remarks>
    //   The value of a Watch is always sent as String. To view
    //   the type of this variable Watch, please have a look at the
    //   WatchType property.
    // </remarks>

    property Value: UnicodeString read FValue write FValue;

    // <summary>
    //   Represents the type of this Watch.
    // </summary>
    // <remarks>
    //   The type of this Watch describes the variable type (String,
    //   Integer and so on). Please see the TSiWatchType enum for
    //   more information.
    // </remarks>

    property WatchType: TSiWatchType read FWatchType write FWatchType;

    // <summary>
    //   Represents the timestamp of this Watch.
    // </summary>
    // <remarks>
    //   This property returns the creation time of this TSiWatch
    //   object. If specified, the SmartInspect Delphi library tries to
    //   use high-resolution timestamps. See TSmartInspect.Resolution
    //   for more information on timestamps.
    // </remarks>

    property Timestamp: TDateTime read FTimestamp write FTimestamp;
  end;

  // <summary>
  //   Represents the Process Flow packet type which is used in the
  //   Enter-/Leave methods in the TSiSession class.
  // </summary>
  // <remarks>
  //   A Process Flow entry is responsible for illustrated process
  //   and thread information.
  //
  //   It has several properties which describe its creation context
  //   (like a thread ID, timestamp or hostname) and other properties
  //   which specify the way the Console interprets this packet (like
  //   the process flow ID). Furthermore a Process Flow entry contains
  //   the actual data, namely the title, which will be displayed in
  //   the Console.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe. However, instances
  //   of this class will normally only be used in the context of a single
  //   thread.
  // </threadsafety>

  TSiProcessFlow = class(TSiPacket)
  private
    FHostName: UnicodeString;
    FProcessFlowType: TSiProcessFlowType;
    FTitle: UnicodeString;
    FTimestamp: TDateTime;
    FThreadId: Cardinal;
    FProcessId: Cardinal;
  protected

    // <summary>
    //   Overridden. Returns the total occupied memory size of this Process
    //   Flow packet.
    // </summary>
    // <returns>
    //   The total memory size of this Process Flow.
    // </returns>
    // <remarks>
    //   The total occupied memory size of this Process Flow entry is the
    //   size of memory occupied by all strings and any internal data
    //   structures of this Process Flow entry.
    // </remarks>

    function GetSize: Integer; override;

    // <summary>
    //   Simply returns the ptProcessFlow packet type.
    // </summary>
    // <returns>
    //   The type of this packet, namely ptProcessFlow.
    // </returns>
    // <remarks>
    //   For a complete list of available packet types, please have
    //   a look at the documentation of the TSiPacketType enum.
    // </remarks>

    function GetPacketType: TSiPacketType; override;
  public

    // <summary>
    //   Overloaded. Creates a TSiProcessFlow instance and initializes its
    //   reference counter with a value of 1.
    // </summary>

    constructor Create; overload;

    // <summary>
    //   Overloaded. Creates a TSiProcessFlow instance with a custom process
    //   flow type and initializes its reference counter with a value of 1.
    // </summary>
    // <param name="AProcessFlowType">
    //   The type of the new Process Flow entry describes the way the
    //   Console interprets this packet. Please see the TSiProcessFlowType
    //   enum for more information.
    // </param>
    // <remarks>
    //   TSiProcessFlow objects are reference counted. Instead of using the
    //   Free method directly to free this object, use Release instead.
    //   Please see the documentation of the AddRef and the Release method
    //   for more information.
    // </remarks>

    constructor Create(const AProcessFlowType: TSiProcessFlowType); overload;

    // <summary>
    //   Represents the title of this Process Flow entry.
    // </summary>

    property Title: UnicodeString read FTitle write FTitle;

    // <summary>
    //   Represents the hostname of this Process Flow entry.
    // </summary>
    // <remarks>
    //   The hostname of this Process Flow entry is usually set to the
    //   name of the machine this Process Flow entry is sent from.
    // </remarks>

    property HostName: UnicodeString read FHostName write FHostName;

    // <summary>
    //   Represents the type of this Process Flow entry.
    // </summary>
    // <remarks>
    //   The type of the Process Flow entry describes the way the
    //   Console interprets this packet. Please see the TSiProcessFlowType
    //   enum for more information.
    // </remarks>

    property ProcessFlowType: TSiProcessFlowType read FProcessFlowType
      write FProcessFlowType;

    // <summary>
    //   Represents the timestamp of this Process Flow entry.
    // </summary>
    // <remarks>
    //   This property returns the creation time of this TSiProcessFlow
    //   object. If specified, the SmartInspect Delphi library tries to
    //   use high-resolution timestamps. See TSmartInspect.Resolution
    //   for more information on timestamps.
    // </remarks>

    property Timestamp: TDateTime read FTimestamp write FTimestamp;

    // <summary>
    //   Represents the thread ID of this Process Flow entry.
    // </summary>
    // <remarks>
    //   This property represents the ID of the thread this Process Flow
    //   object was created in.
    // </remarks>

    property ThreadId: Cardinal read FThreadId write FThreadId;

    // <summary>
    //   Represents the process ID of this Process Flow entry.
    // </summary>
    // <remarks>
    //   This property represents the ID of the process this Process Flow
    //   object was created in.
    // </remarks>

    property ProcessId: Cardinal read FProcessId write FProcessId;
  end;

  // <summary>
  //   Responsible for formatting and writing a packet.
  // </summary>
  // <remarks>
  //   This abstract class defines several methods which are intended
  //   to preprocess a packet and subsequently write it to a stream.
  //   The process of preprocessing (or compiling) and writing a packet
  //   can either be executed with a single step by calling the Format
  //   method or with two steps by calls to Compile and Write.
  // </remarks>
  // <threadsafety>
  //   This class and subclasses thereof are not guaranteed to be
  //   threadsafe.
  // </threadsafety>

  TSiFormatter = class
  public

    // <summary>
    //   Preprocesses (or compiles) a packet and returns the required
    //   size for the compiled result.
    // </summary>
    // <param name="APacket">The packet to compile.</param>
    // <returns>
    //   The size for the compiled result.
    // </returns>
    // <remarks>
    //   To write a previously compiled packet, call the Write method.
    //   Derived classes are intended to compile the supplied packet
    //   and return the required size for the compiled result.
    // </remarks>

    function Compile(const APacket: TSiPacket): Integer; virtual; abstract;

    // <summary>
    //   Writes a previously compiled packet to the supplied stream.
    // </summary>
    // <param name="AStream">The stream to write the packet to.</param>
    // <remarks>
    //   This method is intended to write a previously compiled packet
    //   (see Compile) to the supplied AStream object. If the return
    //   value of the Compile method was 0, nothing is written.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type          Condition
    //   -                       -
    //   EStreamError            An I/O error occurred while trying
    //                             to write the compiled packet.
    // </table>
    // </exception>

    procedure Write(const AStream: TStream); virtual; abstract;

    // <summary>
    //   Compiles a packet and writes it to a stream.
    // </summary>
    // <param name="APacket">The packet to compile.</param>
    // <param name="AStream">The stream to write the packet to.</param>
    // <remarks>
    //   This non-abstract method simply calls the Compile method with
    //   the supplied APacket object and then the Write method with
    //   the supplied AStream object.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type          Condition
    //   -                       -
    //   EStreamError            An I/O error occurred while trying
    //                             to write the compiled packet.
    // </table>
    // </exception>

    procedure Format(const APacket: TSiPacket; const AStream: TStream);
  end;

  // <summary>
  //   Responsible for formatting and writing a packet in the standard
  //   SmartInspect binary format.
  // </summary>
  // <remarks>
  //   This class formats and writes a packet in the standard binary
  //   format which can be read by the SmartInspect Console. The
  //   Compile method preprocesses a packet and computes the required
  //   size of the packet. The Write method writes the preprocessed
  //   packet to the supplied stream.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiBinaryFormatter = class(TSiFormatter)
  private
    FSize: Integer;
    FStream: TMemoryStream;
    FPacket: TSiPacket;
    procedure WriteString(const AString: TSiUTF8String; const AStream: TStream);
{$IFDEF DELPHI2005_OR_HIGHER}
      inline;
{$ENDIF}
    procedure CompileLogHeader;
    procedure CompileLogEntry;
    procedure CompileControlCommand;
    procedure CompileProcessFlow;
    procedure CompileWatch;
    procedure ResetStream;
  public

    // <summary>
    //   Creates and initializes a TSiBinaryFormatter instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases any resources of this TSiBinaryFormatter
    //   object.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Overridden. Preprocesses (or compiles) a packet and returns the
    //   required size for the compiled result.
    // </summary>
    // <param name="APacket">The packet to compile.</param>
    // <returns>
    //   The size for the compiled result.
    // </returns>
    // <remarks>
    //   This method preprocesses the supplied packet and computes the
    //   required binary format size. To write this compiled packet,
    //   call the Write method.
    // </remarks>

    function Compile(const APacket: TSiPacket): Integer; override;

    // <summary>
    //   Overridden. Writes a previously compiled packet to the supplied
    //   stream.
    // </summary>
    // <param name="AStream">The stream to write the packet to.</param>
    // <remarks>
    //   This method writes the previously compiled packet (see Compile)
    //   to the supplied AStream object. If the return value of the Compile
    //   method was 0, nothing is written.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type          Condition
    //   -                       -
    //   EStreamError            An I/O error occurred while trying
    //                             to write the compiled packet.
    // </table>
    // </exception>

    procedure Write(const AStream: TStream); override;
  end;

  // <summary>
  //   Represents a token in the pattern string of the TSiTextProtocol
  //   protocol.
  // </summary>
  // <remarks>
  //   This is the abstract base class for all available tokens. Derived
  //   classes are not documented for clarity reasons. To create a
  //   suitable token object for a given token string, you can use the
  //   TSiTokenFactory class.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiToken = class
  private
    FOptions: UnicodeString;
    FValue: UnicodeString;
    FIndent: Boolean;
    FWidth: Integer;
  public

    // <summary>
    //   Creates and initializes a new TSiToken instance.
    // </summary>

    constructor Create; virtual;

    // <summary>
    //   Creates a string representation of a variable or literal token.
    // </summary>
    // <param name="ALogEntry">
    //   The Log Entry to use to create the string representation.
    // </param>
    // <remarks>
    //   With the help of the supplied Log Entry, this token is expanded
    //   into a string. For example, if this token represents the %session%
    //   variable of a pattern string, this Expand method simply returns
    //   the session name of the supplied Log Entry.
    //
    //   For a literal token, the supplied Log Entry argument is ignored
    //   and the Value property is returned.
    // </remarks>
    // <returns>
    //   The text representation of this token for the supplied TSiLogEntry
    //   object.
    // </returns>

    function Expand(const ALogEntry: TSiLogEntry): UnicodeString;
      virtual; abstract;

    // <summary>
    //   Represents the raw string value of the parsed pattern string for
    //   this token.
    // </summary>
    // <remarks>
    //   This property represents the raw string of this token as found
    //   in the parsed pattern string. For a variable, this property is
    //   set to the variable name surrounded with '%' characters and an
    //   optional options string like this: %name{options}%. For a literal,
    //   this property can have any value.
    // </remarks>

    property Value: UnicodeString read FValue write FValue;

    // <summary>
    //   Represents the optional options string for this token.
    // </summary>
    // <remarks>
    //   A variable token can have an optional options string. In the
    //   raw string representation of a token, an options string can be
    //   specified in curly braces after the variable name like this:
    //   %name{options}%. For a literal, this property is always set to
    //   an empty string.
    // </remarks>

    property Options: UnicodeString read FOptions write FOptions;

    // <summary>
    //   Indicates if this token supports indenting.
    // </summary>
    // <remarks>
    //   This property always returns false unless this token represents
    //   the title token of a pattern string. This property is used by
    //   the TSiPatternParser.Expand method to determine if a token
    //   allows indenting.
    // </remarks>

    property Indent: Boolean read FIndent;

    // <summary>
    //   Represents the minimum width of this token.
    // </summary>
    // <remarks>
    //   A variable token can have an optional width modifier. In the
    //   raw string representation of a token, a width modifier can be
    //   specified after the variable name like this: %name,width%.
    //   Width must be a valid positive or negative integer.
    //
    //   If the width is greater than 0, formatted values will be
    //   right-aligned. If the width is less than 0, they will be
    //   left-aligned.
    //
    //   For a literal, this property is always set to 0.
    // </remarks>

    property Width: Integer read FWidth write FWidth;
  end;

  // <summary>
  //   Creates instances of TSiToken subclasses.
  // </summary>
  // <remarks>
  //   This class has only one public method called GetToken, which
  //   is capable of creating TSiToken objects depending on the given
  //   argument.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiTokenFactory = class
  private
    class function CreateLiteral(const AValue: UnicodeString): TSiToken;
    class function ParseWidth(const AValue: UnicodeString): Integer;
  public

    // <summary>
    //   Creates instance of TSiToken subclasses.
    // </summary>
    // <param name="AValue">
    //   The original string representation of the token.
    // </param>
    // <remarks>
    //   This method analyses and parses the supplied representation of
    //   a token and creates an appropriate TSiToken object. For example,
    //   if the AValue argument is set to '%session%', a TSiToken object
    //   is created and returned which is responsible for expanding the
    //   %session% variable. For a list of available tokens and a detailed
    //   description, please have a look at the TSiPatternParser class,
    //   especially the TSiPatternParser.Pattern property.
    // </remarks>
    // <returns>
    //   An appropriate TSiToken object for the given string representation
    //   of a token.
    // </returns>

    class function GetToken(const AValue: UnicodeString): TSiToken;
  end;

  // <summary>
  //   Capable of parsing and expanding a pattern string as used in the
  //   TSiTextProtocol and TSiTextFormatter classes.
  // </summary>
  // <remarks>
  //   The TSiPatternParser class is capable of creating a text
  //   representation of a TSiLogEntry object (see Expand). The string
  //   representation can be influenced by setting a pattern string.
  //   Please see the Pattern property for a description.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiPatternParser = class
  private
    FBuilder: TSiStringBuilder;
    FTokens: TObjectList;
    FPosition: Integer;
    FPattern: UnicodeString;
    FIndent: Boolean;
    FIndentLevel: Integer;
    function Next: TSiToken;
    procedure Parse;
    procedure SetPattern(const AValue: UnicodeString);
  public

    // <summary>
    //   Creates and initializes a TSiPatternParser instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases all resources used by this TSiPatternParser
    //   instance.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Creates a text representation of a Log Entry by applying a
    //   user-specified Pattern string.
    // </summary>
    // <param name="ALogEntry">
    //   The Log Entry whose text representation should be computed by
    //   applying the current Pattern string. All recognized variables
    //   in the pattern string are replaced with the actual values of
    //   this Log Entry.
    // </param>
    // <returns>
    //   The text representation for the supplied TSiLogEntry object.
    // </returns>

    function Expand(const ALogEntry: TSiLogEntry): UnicodeString;

    // <summary>
    //   Represents the pattern string for this TSiPatternParser object.
    // </summary>
    // <remarks>
    //   The pattern string influences the way a text representation of
    //   a TSiLogEntry object is created. A pattern string consists of a
    //   list of so called variable and literal tokens. When a string
    //   representation of a TSiLogEntry object is created, the variables
    //   are replaced with the actual values of the TSiLogEntry object.
    //
    //   Variables have a unique name, are surrounded with '%' characters
    //   and can have an optional options string enclosed in curly braces
    //   like this: %name{options}%.
    //
    //   You can also specify the minimum width of a value like this:
    //   %name,width%. Width must be a valid positive or negative
    //   integer. If the width is greater than 0, formatted values will
    //   be right-aligned. If the width is less than 0, they will be
    //   left-aligned.
    //
    //   The following table lists the available variables together with
    //   the corresponding TSiLogEntry property.
    //
    //   <table>
    //   Variable        Corresponding Property
    //   -               -
    //   %appname%       TSiLogEntry.AppName
    //   %color%         TSiLogEntry.Color
    //   %hostname%      TSiLogEntry.HostName
    //   %level%         TSiPacket.Level
    //   %logentrytype%  TSiLogEntry.LogEntryType
    //   %process%       TSiLogEntry.ProcessId
    //   %session%       TSiLogEntry.SessionName
    //   %thread%        TSiLogEntry.ThreadId
    //   %timestamp%     TSiLogEntry.Timestamp
    //   %title%         TSiLogEntry.Title
    //   %viewerid%      TSiLogEntry.ViewerId
    //   </table>
    //
    //   For the timestamp token, you can use the options string to
    //   pass a custom date/time format string. This can look as
    //   follows:
    //
    //   %timestamp{hh:nn:ss.zzz}%
    //
    //   The format string must be a valid Delphi TDateTime format
    //   string. The default format string used by the timestamp token
    //   is "yyyy-mm-dd hh:nn:ss.zzz".
    //
    //   Literals are preserved as specified in the pattern string. When
    //   a specified variable is unknown, it is handled as literal.
    // </remarks>
    // <example>
    // <code>
    // '[%timestamp%] %level,8%: %title%'
    // '[%timestamp%] %session%: %title% (Level: %level%)'
    // </code>
    // </example>

    property Pattern: UnicodeString read FPattern write SetPattern;

    // <summary>
    //   Indicates if the Expand method should automatically intend log
    //   packets like in the Views of the SmartInspect Console.
    // </summary>
    // <remarks>
    //   Log Entry packets of type EnterMethod increase the indentation
    //   and packets of type LeaveMethod decrease it.
    // </remarks>

    property Indent: Boolean read FIndent write FIndent;
  end;

  // <summary>
  //   Responsible for creating a text representation of a packet and
  //   writing it to a stream.
  // </summary>
  // <remarks>
  //   This class creates a text representation of a packet and writes
  //   it to a stream. The representation can be influenced with the
  //   Pattern property. The Compile method preprocesses a packet and
  //   computes the required size of the packet. The Write method writes
  //   the preprocessed packet to the supplied stream.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiTextFormatter = class(TSiFormatter)
  private
    FLine: TSiUTF8String;
    FParser: TSiPatternParser;
    procedure SetPattern(const AValue: UnicodeString);
    function GetPattern: UnicodeString;
    function GetIndent: Boolean;
    procedure SetIndent(const AValue: Boolean);
  public

    // <summary>
    //   Creates and initializes a TSiTextFormatter instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overrides. Releases any resources used by this TSiTextFormatter
    //   instance.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Overridden. Preprocesses (or compiles) a packet and returns the
    //   required size for the compiled result.
    // </summary>
    // <param name="APacket">The packet to compile.</param>
    // <returns>
    //   The size for the compiled result.
    // </returns>
    // <remarks>
    //   This method creates a text representation of the supplied packet
    //   and computes the required size. The resulting representation can
    //   be influenced with the Pattern property. To write a compiled
    //   packet, call the Write method. Please note that this method only
    //   supports TSiLogEntry objects and ignores any other packet. This
    //   means, for packets other than TSiLogEntry, this method always
    //   returns 0.
    // </remarks>

    function Compile(const APacket: TSiPacket): Integer; override;

    // <summary>
    //   Overridden. Writes a previously compiled packet to the supplied
    //   stream.
    // </summary>
    // <param name="AStream">The stream to write the packet to.</param>
    // <remarks>
    //   This method writes the previously computed text representation
    //   of a packet (see Compile) to the supplied AStream object.
    //   If the return value of the Compile method was 0, nothing is
    //   written.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type          Condition
    //   -                       -
    //   EStreamError            An I/O error occurred while trying
    //                             to write the compiled packet.
    // </table>
    // </exception>

    procedure Write(const AStream: TStream); override;

    // <summary>
    //   Represents the pattern used to create a text representation
    //   of a packet.
    // </summary>
    // <remarks>
    //   For detailed information of how a pattern string can look like,
    //   please have a look at the documentation of the TSiPatternParser
    //   class, especially the TSiPatternParser.Pattern property.
    // </remarks>

    property Pattern: UnicodeString read GetPattern write SetPattern;

    // <summary>
    //   Indicates if this formatter should automatically intend log
    //   packets like in the Views of the SmartInspect Console.
    // </summary>
    // <remarks>
    //   Log Entry packets of type EnterMethod increase the indentation
    //   and packets of type LeaveMethod decrease it.
    // </remarks>

    property Indent: Boolean read GetIndent write SetIndent;
  end;

  // <summary>Acts as a TCP socket client.</summary>
  // <remarks>
  //   This class offers methods to create and close a socket connection
  //   as well as to send and receive data. This class is used in the
  //   TSiTcpProtocol class to handle the low level TCP socket connection.
  // </remarks>
  // <threadsafety>
  //   The class is not threadsafe.
  // </threadsafety>

  TSiTcpClient = class(TObject)
  private
    FConnected: Boolean;
    FSocket: TSocket;
    FTimeout: Integer;
    FHost: AnsiString;
    FPort: Word;
    procedure Reset;
    procedure RaiseLastSocketError;
    function BuildSocket: TSocket;
    function BuildSocketAddress(const AHostName: AnsiString;
      const APort: Word): TSockAddrIn;
    procedure InternalConnect(const ASocket: TSocket; const ATo: PSockAddrIn;
      const ATimeout: Integer);
    procedure ChangeBlockingMode(const ASocket: TSocket;
      const ABlocking: Boolean);
    procedure WaitForConnect(const ASocket: TSocket; const ATimeout: Integer);
    procedure SetTimeout(const AValue: Integer);
  public

    // <summary>
    //   Creates and initializes a TSiTcpClient instance.
    // </summary>
    // <param name="AHost">The hostname of the remote server.</param>
    // <param name="APort">The destination port of the remote server.</param>

    constructor Create(const AHost: AnsiString; const APort: Word);

    // <summary>
    //   Overridden. Releases any resources.
    // </summary>
    // <remarks>
    //   This method closes the underlying socket if the socket is still
    //   connected to a remote server.
    // </remarks>

    destructor Destroy; override;

    // <summary>
    //   Connects to a remote server.
    // </summary>
    // <param name="ATimeout">
    //   The maximum time in milliseconds to wait for a successful connect.
    // </param>
    // <remarks>
    //   This method tries to connect to the hostname/port pair specified
    //   in the constructor. Furthermore, it offers the possibility to use
    //   a timeout for this operation. If the timeout has been reached before
    //   the connection has been established successfully, this method will
    //   cancel the connection try and raise an exception. A value of 0
    //   disables the timeout feature.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  Connecting to the destination failed. This can
    //                         either mean, that the timeout has been reached
    //                         before the connection has been established or
    //                         a normal socket error occurred. Furthermore,
    //                         this exception can be thrown if the socket is
    //                         already connected.
    // </table>
    // </exception>

    procedure Connect(const ATimeout: Integer);

    // <summary>
    //   Disconnects from the remote server.
    // </summary>
    // <remarks>
    //   This method tries to disconnect from the remote server.
    // </remarks>

    procedure Disconnect;

    // <summary>
    //   Reads a line from the remote server.
    // </summary>
    // <returns>
    //   The line received from the server without the trailing line
    //   delimiter characters.
    // </returns>
    // <remarks>
    //   This method reads characters from the socket up to a newline
    //   (#10) character. The line to return does not include the
    //   trailing line delimiter characters like #13 or #10.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the socket has not
    //                         been connected previously.
    // </table>
    // </exception>

    function ReceiveLn: AnsiString;

    // <summary>
    //   Reads from the underlying socket.
    // </summary>
    // <param name="ABuffer">The buffer to write the bytes to.</param>
    // <param name="ALength">Specifies the amount of bytes to read.</param>
    // <returns>
    //   The amount of bytes actually read.
    // </returns>
    // <remarks>
    //   This method reads from the underlying socket up to ALength
    //   bytes and writes these data to the ABuffer argument. It ensures
    //   that the desired amount of bytes will really be read. If the
    //   return value is less than ALength, then we've got an EOF while
    //   reading from the socket.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the socket has not
    //                         been connected previously.
    // </table>
    // </exception>

    function Receive(var ABuffer; ALength: Longint): Integer;

    // <summary>
    //   Writes to the underlying socket.
    // </summary>
    // <param name="ABuffer">The buffer to read the bytes from.</param>
    // <param name="ALength">Specifies the amount of bytes to write.</param>
    // <returns>
    //   The amount of bytes actually written.
    // </returns>
    // <remarks>
    //   This method tries to write ALength bytes of ABuffer to the
    //   underlying socket. It ensures that the desired amount of bytes
    //   will really be written.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the socket has not
    //                         been connected previously.
    // </table>
    // </exception>

    function Send(const ABuffer; ALength: Longint): Integer;

    // <summary>
    //   Writes a line to the underlying socket.
    // </summary>
    // <param name="ALine">The line to write.</param>
    // <returns>
    //   The amount of bytes actually written.
    // </returns>
    // <remarks>
    //   This method appends a newline character (#10) to the supplied
    //   string and writes it to the underlying socket using the Send
    //   method.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the socket has not
    //                         been connected previously.
    // </table>
    // </exception>

    function SendLn(const ALine: AnsiString): Integer;

    // <summary>
    //   Represents the receive/send timeout of the underlying
    //   socket in milliseconds.
    // </summary>
    // <remarks>
    //   After the underlying socket has been created and connected
    //   using the Connect method you can set the send and receive timeout
    //   by changing this property.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  Occurs while setting this property if
    //                         the underlying socket has not been created
    //                         and connected previously.
    // </table>
    // </exception>

    property Timeout: Integer read FTimeout write SetTimeout;

    // <summary>
    //   Represents the status of the underlying socket connection.
    // </summary>

    property Connected: Boolean read FConnected;
  end;

  // <summary>
  //   A simple TSiTcpClient wrapper which derives from TStream.
  // </summary>
  // <threadsafety>
  //   The class is not threadsafe.
  // </threadsafety>

  TSiTcpClientStream = class(TStream)
  private
    FTcpClient: TSiTcpClient;
  public

    // <summary>
    //   Creates and initializes a TSiTcpClientStream instance.
    // </summary>
    // <param name="ATcpClient">The TCP client to encapsulate.</param>

    constructor Create(const ATcpClient: TSiTcpClient);

    // <summary>
    //   Reads a line from the TCP client.
    // </summary>
    // <returns>
    //   The line received from the TCP client.
    // </returns>
    // <remarks>
    //   This method is just a wrapper for the <link TSiTcpClient.ReceiveLn,
    //   ReceiveLn> method of the TSiTcpClient class.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the TCP client has
    //                         not been connected previously.
    // </table>
    // </exception>

    function ReadLn: AnsiString;

    // <summary>
    //   Overridden. Reads from the underlying TCP client.
    // </summary>
    // <param name="ABuffer">The buffer to write the bytes to.</param>
    // <param name="ACount">Specifies the amount of bytes to read.</param>
    // <returns>
    //   The amount of bytes actually read.
    // </returns>
    // <remarks>
    //   This method is just a wrapper for the <link TSiTcpClient.Receive,
    //   Receive> method of the TSiTcpClient class.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the TCP client has
    //                         not been connected previously.
    // </table>
    // </exception>

    function Read(var ABuffer; ACount: Longint): Longint; override;

    // <summary>
    //   Writes a line to the underlying TCP client.
    // </summary>
    // <param name="ALine">The line to write.</param>
    // <returns>
    //   The amount of bytes actually written.
    // </returns>
    // <remarks>
    //   This method is just a wrapper for the <link TSiTcpClient.SendLn,
    //   SendLn> method of the TSiTcpClient class.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the TCP client has
    //                         not been connected previously.
    // </table>
    // </exception>

    function WriteLn(const ALine: AnsiString): Integer;

    // <summary>
    //   Overridden. Writes to the underlying TCP client.
    // </summary>
    // <param name="ABuffer">The buffer to read the bytes from.</param>
    // <param name="ACount">Specifies the amount of bytes to write.</param>
    // <returns>
    //   The amount of bytes actually written.
    // </returns>
    // <remarks>
    //   This method is just a wrapper for the <link TSiTcpClient.Send,
    //   Send> method of the TSiTcpClient class.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   ESmartInspectError  An I/O error occurred or the TCP client has
    //                         not been connected previously.
    // </table>
    // </exception>

    function Write(const ABuffer; ACount: Longint): Longint; override;

    // <summary>
    //   Overridden. Moves to a specified position in the TCP client
    //   stream.
    // </summary>
    // <param name="AOffset">
    //   The amount of bytes to move from the position specified by the
    //   AOrigin parameter.
    // </param>
    // <param name="AOrigin">
    //   Specifies the way how to interpret the AOffset parameter.
    // </param>
    // <returns>
    //   This function always returns -1.
    // </returns>
    // <remarks>
    //   Moving to a specified position in a TCP stream is not supported,
    //   therefore this function always returns -1.
    // </remarks>

    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;
  end;

  // <summary>
  //   Offers the functionality to read from and write to a file.
  // </summary>
  // <remarks>
  //   This class offers the functionality to read from and write to a
  //   file while the Console is still able to open the file for reading
  //   at the same time.
  // </remarks>
  // <threadsafety>
  //   This class is not threadsafe.
  // </threadsafety>

  TSiFileStream = class(THandleStream)
  public

    // <summary>
    //   Creates and initializes a TSiFileStream instance.
    // </summary>
    // <param name="AFileName">
    //   The name of the file to read from and write to.
    // </param>
    // <param name="AAccess">
    //   Specifies the access to the file as described in the Windows
    //   API documentation of the CreateFile function.
    // </param>
    // <param name="ACreation">
    //   Specifies the action to take on files that exist as described
    //   in the documentation for the Windows API function CreateFile.
    // </param>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESmartInspectError     The desired file could not be opened.
    // </table>
    // </exception>

    constructor Create(const AFileName: UnicodeString; const AAccess: DWord;
      const ACreation: DWord);

    // <summary>
    //   Overridden. Releases all resources by closing the file.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   Responsible for the log file rotate management as used by the
  //   TSiFileProtocol class.
  // </summary>
  // <seealso cref="TSiFileRotaterEvent"/>
  // <remarks>
  //   This class implements a flexible log file rotate management system.
  //   For a detailed description of how to use this class, please refer
  //   to the documentation of the Initialize and Update methods and the
  //   Mode property.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiFileRotater = class
  private
    FMode: TSiFileRotate;
    FTimeValue: Integer;
    function GetTimeValue(const ANow: TDateTime): Integer;
  public

    // <summary>
    //   Creates a new TSiFileRotater instance with a default mode of
    //   frNone. Please refer to the Update and Initialize methods for
    //   additional information about this class.
    // </summary>

    constructor Create;

    // <summary>
    //   Initializes this TSiFileRotater object with a user-supplied
    //   timestamp.
    // </summary>
    // <param name="ANow">
    //   The user-specified timestamp to use to initialize this object.
    // </param>
    // <remarks>
    //   Always call this method after creating a new TSiFileRotater
    //   object and before calling the Update method the first time.
    //   For additional information please refer to the Update method.
    // </remarks>

    procedure Initialize(const ANow: TDateTime);

    // <summary>
    //   Updates the date of this TSiFileRotater object and returns
    //   whether the rotate state has changed since the last call to
    //   this method or to Initialize.
    // </summary>
    // <returns>
    //   True if the rotate state has changed since the last call to
    //   this method or to Initialize and false otherwise.
    // </returns>
    // <param name="ANow">The timestamp to update this object.</param>
    // <remarks>
    //   This method updates the internal date of this TSiFileRotater
    //   object and returns whether the rotate state has changed since
    //   the last call to this method or to Initialize. Before calling
    //   this method, always call the Initialize method.
    // </remarks>

    function Update(const ANow: TDateTime): Boolean;

    // <summary>
    //   Represents the TSiFileRotate mode of this TSiFileRotater object.
    // </summary>
    // <remarks>
    //   Always call the Initialize method after changing this property
    //   to reinitialize this TSiFileRotater object. For a complete list
    //   of available property values, please refer to the documentation
    //   of the TSiFileRotate enum.
    // </remarks>

    property Mode: TSiFileRotate read FMode write FMode;
  end;

  // <summary>
  //   Buffers write calls of a TStream instance.
  // </summary>
  // <remarks>
  //   To achieve a higher I/O performance, the protocols use this class
  //   as a buffer for their output streams. It functions as a wrapper
  //   for the well known TStream class with the ability of buffering the
  //   write calls.
  // </remarks>
  // <threadsafety>
  //   This class is not threadsafe.
  // </threadsafety>

  TSiBufferedStream = class(TStream)
  private
    FStream: TStream;
    FBuffer: Pointer;
    FCapacity: Integer;
    FCount: Integer;
  public

    // <summary>
    //   Creates and initializes a TSiBufferedStream instance.
    // </summary>
    // <param name="AStream">The TStream instance to buffer.</param>
    // <param name="ACapacity">The capacity of the internal buffer.</param>

    constructor Create(const AStream: TStream; const ACapacity: Integer);

    // <summary>
    //   Overridden. Releases all resources.
    // </summary>
    // <remarks>
    //   This destructor flushes and frees the internal buffer.
    // </remarks>

    destructor Destroy; override;

    // <summary>
    //   Overridden. Reads from the underlying stream.
    // </summary>
    // <param name="ABuffer">The buffer to write the bytes to.</param>
    // <param name="ACount">Specifies the amount of bytes to read.</param>
    // <returns>
    //   The amount of bytes actually read.
    // </returns>
    // <remarks>
    //   This method reads directly from the underlying stream up to
    //   ACount bytes and writes these data to the ABuffer argument. That
    //   means, no buffering occurs here.
    // </remarks>

    function Read(var ABuffer; ACount: Longint): Longint; override;

    // <summary>
    //   Overridden. Writes to the underlying stream.
    // </summary>
    // <param name="ABuffer">The buffer to read the bytes from.</param>
    // <param name="ACount">Specifies the amount of bytes to write.</param>
    // <returns>
    //   The amount of bytes actually written.
    // </returns>
    // <remarks>
    //   This method tries to write ACount bytes of ABuffer to the internal
    //   buffer. If ACount exceeds the internal capacity, then the specified
    //   amount of bytes of ABuffer will be written directly to the stream.
    // </remarks>

    function Write(const ABuffer; ACount: Longint): Longint; override;

    // <summary>
    //   Overridden. Moves to a specified position in the underlying stream.
    // </summary>
    // <param name="AOffset">
    //   The amount of bytes to move from the position specified by the
    //   AOrigin parameter.
    // </param>
    // <param name="AOrigin">
    //   Specifies the way how to interpret the AOffset parameter.
    // </param>
    // <returns>
    //   The return value of the Seek method of the underlying stream.
    // </returns>
    // <remarks>
    //   This function <link TSiBufferedStream.Flush, flushes> the internal
    //   data buffer and then calls the Seek method of the underlying stream.
    // </remarks>

    function Seek(AOffset: Longint; AOrigin: Word): Longint; override;

    // <summary>
    //   Flushes the internal buffer.
    // </summary>
    // <remarks>
    //   This method writes the internal buffer to the underlying
    //   stream.
    // </remarks>

    procedure Flush;
  end;

  // <summary>
  //   Represents a custom protocol action command as used by the
  //   TSiProtocol.Dispatch method.
  // </summary>
  // <remarks>
  //   This class is used by custom protocol actions. For detailed
  //   information about custom protocol actions, please refer to the
  //   TSiProtocol.Dispatch and TSmartInspect.Dispatch methods.
  //
  //   Instances of TSiProtocolCommand are reference counted, that
  //   means they will be automatically freed when there are no
  //   references pointing to it anymore. To increment and decrement
  //   the reference counter, use the AddRef and Release methods.
  //   After an instance has been created with the Create constructor,
  //   the reference counter is always set to 1.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiProtocolCommand = class
  private
    FRefCount: Integer;
    FState: TObject;
    FAction: Integer;
  public

    // <summary>
    //   Creates and initializes a new TSiProtocolCommand instance and
    //   sets the reference counter to the initial value of 1.
    // </summary>
    // <param name="AAction">The custom protocol action to execute.</param>
    // <param name="AState">
    //   Optional object which provides additional information about the
    //   custom protocol action.
    // </param>
    // <remarks>
    //   Subclasses must always call this constructor to initialize
    //   the reference counter correctly. <b>Please note</b>: Since
    //   protocol commands are reference counted, to not call Free
    //   directly, use the Release method instead.
    // </remarks>

    constructor Create(const AAction: Integer; const AState: TObject);

    // <summary>
    //   Increments the reference counter by one.
    // </summary>
    // <remarks>
    //   This method increments the reference counter by one. Every call
    //   to this method must have a matching call to the corresponding
    //   Release method.
    // </remarks>

    procedure AddRef;

    // <summary>
    //   Decrements the reference counter by one.
    // </summary>
    // <remarks>
    //   This method decrements the reference counter by one. If the
    //   reference counter reaches 0, this object is freed. Every call
    //   to this method must have a matching call to the corresponding
    //   AddRef or Create method.
    // </remarks>

    procedure Release;

    // <summary>
    //   Returns the custom protocol action to execute. The value of this
    //   property is protocol specific.
    // </summary>

    property Action: Integer read FAction;

    // <summary>
    //   Returns the optional protocol command object which provides
    //   additional information about the custom protocol action. This
    //   property can be nil.
    // </summary>

    property State: TObject read FState;
  end;

  // <summary>
  //   This is the event handler type for the TProtocol.OnError
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AException">The occurred exception.</param>
  // <remarks>
  //   In addition to the ASender parameter, an Exception
  //   argument will be passed to the event handlers which offers the
  //   possibility of retrieving information about the occurred error.
  // </remarks>

  TSiProtocolErrorEvent = procedure (ASender: TSiProtocol;
    AException: Exception) of object;

  // <summary>
  //   Is the abstract base class for a protocol. A protocol is responsible
  //   for transporting packets.
  // </summary>
  // <remarks>
  //   A protocol is responsible for the transport of packets. This base
  //   class offers all necessary methods to handle the protocol options
  //   and it declares several abstract protocol specific methods for
  //   handling protocol destinations like connecting or writing packets.
  //
  //   The following table lists the available protocols together with
  //   their identifier in the <link TSmartInspect.Connections,
  //   connections string> and a short description.
  //
  //   <table>
  //   Protocol           Identifier    Description
  //   -                  -             -
  //   TSiFileProtocol    'file'        Used for writing log files in the
  //                                     standard SmartInspect binary log
  //                                     file format which can be loaded
  //                                     into the Console.
  //
  //   TSiMemoryProtocol  'mem'         Used for writing log data to memory
  //                                     and saving it to a stream on
  //                                     request.
  //
  //   TSiPipeProtocol    'pipe'        Used for sending packets over a
  //                                     local named pipe directly to the
  //                                     Console.
  //
  //   TSiTcpProtocol     'tcp'         Used for sending packets over a TCP
  //                                     connection directly to the Console.
  //
  //   TSiTextProtocol    'text'        Used for writing log files in a
  //                                     customizable text format. Best
  //                                     suited for end-user notification
  //                                     purposes.
  //   </table>
  //
  //   There are several options which are <link IsValidOption,
  //   common to all protocols> and beyond that each protocol has its own
  //   set of additional options. For those protocol specific options,
  //   please refer to the documentation of the corresponding protocol
  //   class. Protocol options can be set with Initialize derived classes
  //   can query option values using the Get methods.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiProtocol = class(TObject)
  private
    FReconnect: Boolean;
    FReconnectInterval: Cardinal;
    FReconnectTickCount: Cardinal;
    FLevel: TSiLevel;
    FCaption: UnicodeString;
    FBacklogEnabled: Boolean;
    FBacklogQueue: Int64;
    FBacklogFlushOn: TSiLevel;
    FBacklogKeepOpen: Boolean;
    FAsyncEnabled: Boolean;
    FAsyncThrottle: Boolean;
    FAsyncClearOnDisconnect: Boolean;
    FAsyncQueue: Int64;
    FKeepOpen: Boolean;
    FFailed: Boolean;
    FConnected: Boolean;
    FInitialized: Boolean;
    FCriticalSection: TCriticalSection;
    FEventLock: TCriticalSection;
    FOptions: TSiLookupTable;
    FQueue: TSiPacketQueue;
    FScheduler: TSiScheduler;
    FOnError: TSiProtocolErrorEvent;
    FHostName: UnicodeString;
    FAppName: UnicodeString;
    procedure AddOption(ASender: TSiOptionsParser;
      AProtocol, AKey, AValue: UnicodeString);
    procedure CreateOptions(const AOptions: UnicodeString);
    procedure RemoveOptions;
    function GetOptions: UnicodeString;
    procedure DeletePacket(ASender: TSiPacketQueue; APacket: TSiPacket);
    procedure ForwardPacket(const APacket: TSiPacket;
      const ADisconnect: Boolean);
    procedure Reconnect;
    function MapOption(const AKey, AValue: UnicodeString): Boolean;
    procedure StartScheduler;
    procedure StopScheduler;
    procedure ScheduleConnect;
    procedure ScheduleWritePacket(const APacket: TSiPacket);
    procedure ScheduleDisconnect;
    procedure ScheduleDispatch(const ACommand: TSiProtocolCommand);
    procedure ImplConnect;
    procedure ImplWritePacket(const APacket: TSiPacket);
    procedure ImplDisconnect;
    procedure ImplDispatch(const ACommand: TSiProtocolCommand);
    function GetOnError: TSiProtocolErrorEvent;
    procedure SetOnError(const AValue: TSiProtocolErrorEvent);
    procedure InternalWriteLogHeader;
  protected

    // <summary>
    //   Invokes the TSiProtocol.OnError event handler.
    // </summary>
    // <param name="AException">The occurred exception.</param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSiProtocol.OnError event. Note that the OnError event is
    //   only used in combination with asynchronous logging (please
    //   see IsValidOption for more information). In normal blocking
    //   mode, exceptions are reported by throwing.
    // </remarks>

    procedure DoError(const AException: Exception);

    // <summary>
    //   Gets the string value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the key does not exist.
    // </param>
    // <returns>
    //   Either the related value if the key exists or ADefaultValue
    //   otherwise.
    // </returns>

    function GetStringOption(const AName: UnicodeString;
      const ADefaultValue: UnicodeString): UnicodeString;

    // <summary>
    //   Gets the Boolean value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the key does not exist.
    // </param>
    // <returns>
    //   Either the Boolean value if the key exists or ADefaultValue
    //   otherwise.
    // </returns>
    // <remarks>
    //   A Boolean value will be treated as true if the value of
    //   the key matches either 'true', 'yes' or '1' and as false
    //   otherwise.
    // </remarks>

    function GetBooleanOption(const AName: UnicodeString;
      const ADefaultValue: Boolean): Boolean;

    // <summary>
    //   Gets an Integer value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the key does not exist.
    // </param>
    // <returns>
    //   Either the value converted to an Integer for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Integer or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   Please note that if a value could be found but is not a
    //   valid Integer, the supplied default value will be returned.
    //   Only non-negative Integers will be recognized as valid
    //   values.
    // </remarks>

    function GetIntegerOption(const AName: UnicodeString;
      const ADefaultValue: Integer): Integer;

    // <summary>
    //   Gets a TSiLevel value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to the corresponding TSiLevel value
    //   for the given key if an element with the given key exists and
    //   the found value is a valid TSiLevel value or ADefaultValue
    //   otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid
    //   TSiLevel value. Please see the TSiLevel enum for more information
    //   on the available values.
    // </remarks>

    function GetLevelOption(const AName: UnicodeString;
      const ADefaultValue: TSiLevel): TSiLevel;

    // <summary>
    //   Gets an Integer value of a key. The Integer value is interpreted
    //   as a byte size value and it is supported to specify byte units.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to an Integer for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Integer or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Integer
    //   or ends with an unknown byte unit. Only non-negative Integer
    //   values are recognized as valid.
    //
    //   It is possible to specify a size unit at the end of the value.
    //   If a known unit is found, this function multiplies the
    //   resulting value with the corresponding factor. For example, if
    //   the value of the element is "1KB", the return value of this
    //   function would be 1024.
    //
    //   The following table lists the available units together with a
    //   short description and the corresponding factor.
    //
    //   <table>
    //   Unit Name  Description  Factor
    //   -          -            -
    //   KB         Kilo Byte    1024
    //   MB         Mega Byte    1024^2
    //   GB         Giga Byte    1024^3
    //   </table>
    //
    //   If no unit is specified, this function defaults to the KB unit.
    // </remarks>

    function GetSizeOption(const AName: UnicodeString;
      const ADefaultValue: Int64): Int64;

    // <summary>
    //   Gets a TSiFileRotate value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a TSiFileRotate value for the
    //   given key if an element with the given key exists and the found
    //   value is a valid TSiFileRotate or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid
    //   TSiFileRotate value. For a complete list of available values,
    //   please have a look the TSiFileRotate enum.
    // </remarks>

    function GetRotateOption(const AName: UnicodeString;
      const ADefaultValue: TSiFileRotate): TSiFileRotate;

    // <summary>
    //   Gets a byte array value of a key.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ASize">
    //   The desired size in bytes of the returned byte array. If
    //   the element value does not have the expected size, it is
    //   shortened or padded automatically.
    // </param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown or if the
    //   found value has an invalid format.
    // </param>
    // <returns>
    //   Either the value converted to a byte array for the given key
    //   if an element with the given key exists and the found value
    //   has a valid format or ADefaultValue otherwise.
    // </returns>
    // <remarks>
    //   The returned byte array always has the desired length as
    //   specified by the ASize argument. If the element value does
    //   not have the required size after conversion, it is shortened
    //   or padded (with zeros) automatically. This method returns
    //   the ADefaultValue argument if either the supplied key is
    //   unknown or the found value does not have a valid format
    //   (e.g. invalid characters when using hexadecimal strings).
    // </remarks>

    function GetBytesOption(const AName: UnicodeString;
      const ASize: Integer; const ADefaultValue: TSiBytes): TSiBytes;

    // <summary>
    //   Gets a Cardinal value of a key. The Cardinal value is interpreted
    //   as a time span and it is supported to specify time span units.
    // </summary>
    // <param name="AName">The option name to search for.</param>
    // <param name="ADefaultValue">
    //   The value to return if the given key is unknown.
    // </param>
    // <returns>
    //   Either the value converted to a Cardinal for the given key if
    //   an element with the given key exists and the found value is a
    //   valid Cardinal or ADefaultValue otherwise. The value is returned
    //   in milliseconds.
    // </returns>
    // <remarks>
    //   This method returns the ADefaultValue argument if either the
    //   supplied key is unknown or the found value is not a valid Cardinal
    //   or ends with an unknown time span unit.
    //
    //   It is possible to specify a time span unit at the end of the
    //   value. If a known unit is found, this function multiplies the
    //   resulting value with the corresponding factor. For example, if
    //   the value of the element is "1s", the return value of this
    //   function would be 1000.
    //
    //   The following table lists the available units together with a
    //   short description and the corresponding factor.
    //
    //   <table>
    //   Unit Name  Description  Factor
    //   -          -            -
    //   s          Seconds      1000
    //   m          Minutes      60*s
    //   h          Hours        60*m
    //   d          Days         24*h
    //   </table>
    //
    //   If no unit is specified, this function defaults to the Seconds
    //   unit. Please note that the value is always returned in
    //   milliseconds.
    // </remarks>

    function GetTimespanOption(const AName: UnicodeString;
      const ADefaultValue: Cardinal): Cardinal;

    // <summary>
    //   Specifies the name of a real protocol implementation.
    // </summary>
    // <returns>
    //   The name of a real protocol implementation.
    // </returns>
    // <remarks>
    //   Real implementations should return a meaningful name which
    //   represents the protocol. For example, the TSiFileProtocol returns
    //   'file', the TSiTcpProtocol 'tcp' and the TSiTextProtocol 'text'.
    // </remarks>

    function GetName: UnicodeString; virtual; abstract;

    // <summary>
    //   Handles a protocol exception.
    // </summary>
    // <param name="AMessage">The exception message.</param>
    // <remarks>
    //   This method handles an occurred protocol exception. It first
    //   sets the Failed flag and creates a ESiProtocolError object
    //   with the name and options of this protocol. In normal blocking
    //   mode (see IsValidOption), it then throws this exception. When
    //   operating in asynchronous mode, it invokes the OnError event
    //   handlers instead and does not throw an exception.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESiProtocolError       Always in normal blocking mode.
    //                           Never in asynchronous mode.
    // </table>
    // </exception>

    procedure HandleException(const AMessage: String);

    // <summary>
    //   Fills a TSiConnectionsBuilder instance with the options currently
    //   used by this protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>
    // <remarks>
    //   The filled options string consists of key, value option pairs
    //   separated by commas.
    //
    //   This function takes care of the options <link IsValidOption,
    //   common to all protocols>. To include protocol specific options,
    //   override this function.
    // </remarks>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); virtual;

    // <summary>
    //   Loads and inspects protocol specific options.
    // </summary>
    // <remarks>
    //   This method is intended to give real protocol implementations
    //   an opportunity to load and inspect options. This method will be
    //   called automatically when the options have been changed. The
    //   default implementation of this method takes care of the options
    //   <link IsValidOption, common to all protocols> and should thus
    //   always be called by derived classes which override this method.
    // </remarks>

    procedure LoadOptions; virtual;
    
    // <summary>
    //   Validates if a option is supported by this protocol.
    // </summary>
    // <param name="AName">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default
    //   values and descriptions common to all protocols. See below
    //   for explanations.
    //
    //   <table>
    //   Option Name              Default   Description
    //   +                        +         +
    //   level                    debug     Specifies the log level of
    //                                       this protocol.
    //
    //   reconnect                false     Specifies if a reconnect
    //                                       should be initiated when a
    //                                       connection gets dropped.
    //
    //   reconnect.interval       0         If reconnecting is enabled,
    //                                       specifies the minimum time
    //                                       in seconds between two
    //                                       successive reconnect
    //                                       attempts. If 0 is specified,
    //                                       a reconnect attempt is
    //                                       initiated for each log packet
    //                                       if needed. It is possible to
    //                                       specify time span units like
    //                                       this: "1s". Supported units
    //                                       are "s" (seconds), "m"
    //                                       (minutes), "h" (hours) and
    //                                       "d" (days).
    //
    //   caption                  [name]    Specifies the caption of
    //                                       this protocol as used by
    //                                       TSmartInspect.Dispatch.
    //                                       By default, it is set to
    //                                       the protocol identifier
    //                                       (e.g., "file" or "mem").
    //
    //   async.enabled            false     Specifies if this protocol
    //                                       should operate in
    //                                       asynchronous instead of the
    //                                       default blocking mode.
    //
    //   async.queue              2048      Specifies the maximum size
    //                                       of the asynchronous queue in
    //                                       kilobytes. It is possible
    //                                       to specify size units like
    //                                       this: "1 MB". Supported
    //                                       units are "KB", "MB" and
    //                                       "GB".
    //
    //   async.throttle           true      Specifies if the application
    //                                       should be automatically
    //                                       throttled in asynchronous
    //                                       mode when more data is
    //                                       logged than the queue can
    //                                       handle.
    //
    //   async.clearondisconnect  false     Specifies if the current
    //                                       content of the asynchronous
    //                                       queue should be discarded
    //                                       before disconnecting. Useful
    //                                       if an application must not
    //                                       wait for the logging to
    //                                       complete before exiting.
    //
    //   backlog.enabled          false     Enables the backlog feature
    //                                       (see below).
    //
    //   backlog.queue            2048      Specifies the maximum size
    //                                       of the backlog queue in
    //                                       kilobytes. It is possible
    //                                       to specify size units like
    //                                       this: "1 MB". Supported
    //                                       units are "KB", "MB" and
    //                                       "GB".
    //
    //   backlog.flushon          error     Specifies the flush level for
    //                                       the backlog functionality.
    //
    //   backlog.keepopen         false     Specifies if the connection
    //                                       should be kept open between
    //                                       two successive writes when
    //                                       the backlog feature is used.
    //   </table>
    //
    //   With the log level of a protocol you can limit the amount of
    //   data being logged by excluding packets which don't have a
    //   certain minimum log level. For example, if you set the level
    //   to "message", all packets with a log level of "debug" or
    //   "verbose" are ignored. For a complete list of available log
    //   level values, please see the documentation of the Level enum.
    //
    //   The caption option specifies the caption for this protocol
    //   as used by the TSmartInspect.Dispatch method. This method
    //   can send and initiate custom protocol actions and the caption
    //   is used to lookup the requested connection. By default, the
    //   caption is set to the identifier of a protocol (e.g., "file"
    //   or "mem"). For more information about the dispatching of
    //   custom protocol actions, please refer to the documentation of
    //   the Dispatch and TSmartInspect.Dispatch methods.
    //
    //   If the backlog option is enabled, all packets whose log level
    //   is less than the flushon level and equal to or higher than the
    //   general log level of a protocol, will be written to a queue
    //   rather than directly to the protocol specific destination. When
    //   a packet arrives with a log level of at least the same value
    //   as the flushon option, the current content of the queue is
    //   written. The total amount of memory occupied by this queue
    //   can be set with the queue option. If the packet queue has
    //   been filled up with packets and a new packet is about to be
    //   stored, old packets are discarded.
    //
    //   As an example, if the backlog queue is set to "2 MB" and the
    //   flushon level to "error", all packets with a log level less
    //   than error are written to a queue first. By specifying a queue
    //   option of "2 MB", the baclog queue is set to a maximum memory
    //   size of 2 megabyte. Now, when a packet with a log level of
    //   error arrives, the current content of the queue and then the
    //   error itself are written.
    //
    //   With the keepopen option of the backlog feature you can specify
    //   if a connection should be kept open between two successive
    //   writes. When keepopen is set to false, a connection is only
    //   available during the actual write / flush. A connection is
    //   thus only created when absolutely necessary.
    //
    //   A protocol can either operate in normal blocking (the default)
    //   or in asynchronous mode. In blocking mode, the operations of
    //   this protocol (Connect, Disconnect, Dispatch and WritePacket)
    //   are executed synchronously and block the caller until they are
    //   done. In asynchronous mode, these operations are not executed
    //   directly but scheduled for execution in a different thread
    //   and return immediately. Asynchronous logging can increase the
    //   logging performance and reduce the blocking of applications.
    //
    //   When operating in asynchronous mode, this protocol uses a
    //   queue to buffer the logging data. The total amount of memory
    //   occupied by this queue can be set with the queue option. The
    //   throttle option specifies if an application should be
    //   automatically throttled in asynchronous mode when more data
    //   is logged / generated than the queue can handle. If this
    //   option is disabled and the queue is currently full, old
    //   packets are discarded when new data is logged. The throttle
    //   option ensures that no logging data is lost but can be
    //   disabled if logging performance is critical.
    //
    //   With the clearondisconnect option, you can specify if the
    //   current content of the asynchronous queue should be discarded
    //   before disconnecting. This can be useful if an application
    //   must not wait for the logging to complete before exiting.
    //
    //   The reconnect option allows a protocol to reconnect
    //   automatically before a packet is being written. A reconnect
    //   might be necessary if a working connection has been unexpectedly
    //   disconnected or could not be established in the first place.
    //   Possible errors during a reconnect attempt will silently be
    //   ignored and not reported.
    //
    //   Please note that the reconnect functionality causes a protocol
    //   by default to initiate a connection attempt for each packet
    //   until a connection has been successfully (re-) established.
    //   This can be a very time consuming process, especially when using
    //   a protocol which requires a complex connection process like
    //   <link TSiTcpProtocol, TCP>, for example. This can slow down the
    //   logging performance. When using the reconnect option, it is thus
    //   recommended to also enable asynchronous logging to not block the
    //   application or to specify a reconnect interval to minimize the
    //   reconnect attempts.
    // </remarks>

    function IsValidOption(const AOption: UnicodeString): Boolean; virtual;

    // <summary>
    //   Resets the protocol and brings it into a consistent state.
    // </summary>
    // <remarks>
    //   This method resets the current protocol state by clearing the
    //   internal queue of packets, setting the connected status to
    //   false and calling the abstract InternalDisconnect method of a
    //   real protocol implementation to cleanup any protocol specific
    //   resources.
    // </remarks>

    procedure Reset;

    // <summary>
    //   Connects to the protocol specific destination.
    // </summary>
    // <remarks>
    //   This method initiates a protocol specific connection attempt.
    //   The behavior of real implementations of this method can often
    //   be changed by setting protocol options with the Initialize
    //   method. This method is always called in a threadsafe and
    //   exception-safe context.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type       Condition
    //   -                    -
    //   Exception            Connecting to the destination failed.
    // </table>
    // </exception>

    procedure InternalConnect; virtual; abstract;

    // <summary>
    //   Reconnects to the protocol specific destination.
    // </summary>
    // <returns>
    //   True if the reconnect attempt has been successful and false
    //   otherwise.
    // </returns>
    // <remarks>
    //   This method initiates a protocol specific reconnect attempt.
    //   The behavior of real method implementations can often be
    //   changed by setting protocol options with Initialize. This
    //   method is always called in a threadsafe and exception-safe
    //   context.
    //
    //   The default implementation simply calls the protocol specific
    //   InternalConnect method. Derived classes can change this
    //   behavior by overriding this method.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type       Condition
    //   -                    -
    //   Exception            Reconnecting to the destination failed.
    // </table>
    // </exception>

    function InternalReconnect: Boolean; virtual;

    // <summary>
    //   Disconnects from the protocol specific destination.
    // </summary>
    // <exception>
    // <remarks>
    //   This method is intended for real protocol implementations to
    //   disconnect from the protocol specific source. This could be
    //   closing a file or disconnecting a TCP socket, for example. This
    //   method is always called in a threadsafe and exception-safe
    //   context.
    // </remarks>
    // <table>
    //   Exception Type       Condition
    //   -                    -
    //   Exception            Disconnecting from the destination failed.
    // </table>
    // </exception>

    procedure InternalDisconnect; virtual; abstract;

    // <summary>
    //   Writes a packet to the protocol specific destination.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   This method is intended for real protocol implementations to
    //   write the supplied packet to the protocol specific destination.
    //   This method is always called in a threadsafe and exception-safe
    //   context.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type       Condition
    //   -                    -
    //   Exception            Writing the packet to the destination failed.
    // </table>
    // </exception>

    procedure InternalWritePacket(const APacket: TSiPacket);
      virtual; abstract;

    // <summary>
    //   Executes a protocol specific custom action.
    // </summary>
    // <param name="ACommand">
    //   The protocol command which provides protocol specific information
    //   about the custom action. Can be nil.
    // </param>
    // <seealso cref="TSmartInspect.Dispatch"/>
    // <remarks>
    //   The default implementation does nothing. Derived protocol
    //   implementations can override this method to add custom actions.
    //   Please see the TSiMemoryProtocol.InternalDispatch method for an
    //   example.
    //
    //   This method is always called in a threadsafe and exception-safe
    //   way.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   Exception              Executing the custom action failed.
    // </table>
    // </exception>

    procedure InternalDispatch(const ACommand: TSiProtocolCommand); virtual;
  public

    // <summary>
    //   Creates and initializes a TSiProtocol subclass instance. For a
    //   list of protocol options common to all protocols, please refer
    //   to the IsValidOption method.
    // </summary>

    constructor Create; virtual;

    // <summary>
    //   Overridden. Releases all resources.
    // </summary>
    // <remarks>
    //   This method removes all protocol options and disconnects from
    //   the protocol specific destination. No exception can be thrown.
    // </remarks>

    destructor Destroy; override;

    // <summary>
    //   Sets and initializes the options of this protocol.
    // </summary>
    // <remarks>
    //   This property expects an options string which consists
    //   of key, value pairs separated by commas like this:
    //   "filename=log.sil, append=true". To use a comma in a value,
    //   you can use quotation marks like in the following example:
    //   "filename=\\"log.sil\\", append=true".
    //
    //   Please note that a ESmartInspectError exception is thrown
    //   if an incorrect options string is assigned. An incorrect
    //   options string could use an invalid syntax or contain one or
    //   more unknown option keys. This method can be called only once.
    //   Further calls have no effect. Pass an empty string to use the
    //   default options of a particular protocol.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESmartInspectError     Invalid options syntax or an unknown
    //                            option key.
    // </table>
    // </exception>

    procedure Initialize(const AOptions: UnicodeString);

    // <summary>
    //   Connects to the protocol destination.
    // </summary>
    // <remarks>
    //   In normal blocking mode (see IsValidOption), this method
    //   does nothing more than to verify that the protocol is not
    //   already connected and does not use the <link IsValidOption,
    //   keepopen backlog feature> and then calls the abstract
    //   protocol specific InternalConnect method in a threadsafe
    //   and exception-safe context.
    //
    //   When operating in asynchronous mode instead, this method
    //   schedules a connect operation for asynchronous execution
    //   and returns immediately. Please note that possible
    //   exceptions which occur during the eventually executed
    //   connect are not thrown directly but reported with the
    //   OnError event.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESiProtocolError       Connecting to the destination failed.
    //                           Can only occur when operating in
    //                           normal blocking mode. In asynchronous
    //                           mode, the OnError event is used for
    //                           reporting exceptions instead.
    // </table>
    // </exception>

    procedure Connect;

    // <summary>
    //   Disconnects from the protocol destination.
    // </summary>
    // <remarks>
    //   In normal blocking mode (see IsValidOption), this method
    //   checks if this protocol has a working connection and then
    //   calls the protocol specific InternalDisconnect method in a
    //   threadsafe and exception-safe context.
    //
    //   When operating in asynchronous mode instead, this method
    //   schedules a disconnect operation for asynchronous execution
    //   and then blocks until the internal protocol thread is done.
    //   Please note that possible exceptions which occur during
    //   the eventually executed disconnect are not thrown directly
    //   but reported with the OnError event.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESiProtocolError       Disconnecting from the destination
    //                           failed. Can only occur when operating
    //                           in normal blocking mode. In asynchronous
    //                           mode, the OnError event is used for
    //                           reporting exceptions instead.
    // </table>
    // </exception>

    procedure Disconnect;

    // <summary>
    //   Writes a packet to the protocol destination.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   This method first checks if the log level of the supplied
    //   packet is sufficient to be logged. If this is not the
    //   case, this method returns immediately.
    //
    //   Otherwise, in normal blocking mode (see IsValidOption),
    //   this method verifies that this protocol is successfully
    //   connected and then writes the supplied packet to the
    //   <link IsValidOption, backlog queue> or passes it directly
    //   to the protocol specific destination by calling the
    //   InternalWritePacket method. Calling InternalWritePacket
    //   is always done in a threadsafe and exception-safe way.
    //
    //   When operating in asynchronous mode instead, this method
    //   schedules a write operation for asynchronous execution and
    //   returns immediately. Please note that possible exceptions
    //   which occur during the eventually executed write are not
    //   thrown directly but reported with the OnError event.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESiProtocolError       Writing the packet to the destination
    //                           failed. Can only occur when operating
    //                           in normal blocking mode. In asynchronous
    //                           mode, the OnError event is used for
    //                           reporting exceptions instead.
    // </table>
    // </exception>

    procedure WritePacket(const APacket: TSiPacket);

    // <summary>
    //   Dispatches a custom action to a concrete implementation of a
    //   protocol.
    // </summary>
    // <param name="ACommand">
    //   The protocol command object which provides protocol specific
    //   information about the custom action. Can be nil.
    // </param>
    // <seealso cref="TSmartInspect.Dispatch"/>
    // <remarks>
    //   In normal blocking mode (see IsValidOption), this method
    //   does nothing more than to call the protocol specific
    //   InternalDispatch method with the supplied command argument
    //   in a threadsafe and exception-safe way. Please note that
    //   this method dispatches the custom action only if the protocol
    //   is currently connected.
    //
    //   When operating in asynchronous mode instead, this method
    //   schedules a dispatch operation for asynchronous execution
    //   and returns immediately. Please note that possible
    //   exceptions which occur during the eventually executed
    //   dispatch are not thrown directly but reported with the
    //   OnError event.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESiProtocolError       An exception occurred in the custom
    //                           action. Can only occur when operating
    //                           in normal blocking mode. In asynchronous
    //                           mode, the OnError event is used for
    //                           reporting exceptions instead.
    // </table>
    // </exception>

    procedure Dispatch(const ACommand: TSiProtocolCommand); reintroduce;

    // <summary>
    //   Returns the caption of this protocol.
    // </summary>
    // <remarks>
    //   The caption is used in the TSmartInspect.Dispatch method to
    //   lookup the requested connection. The caption can be set with the
    //   Options property. If you use only one connection at once or does
    //   not use the TSmartInspect.Dispatch method, the caption option can
    //   safely be ignored.
    //
    //   For more information, please refer to the documentation of the
    //   Dispatch and TSmartInspect.Dispatch methods.
    // </remarks>

    property Caption: UnicodeString read FCaption write FCaption;

    // <summary>
    //   Returns if the last executed connection-related operation of
    //   this protocol has failed. Indicates if the next operation is
    //   likely to block.
    // </summary>

    property Failed: Boolean read FFailed;

    // <summary>
    //   Indicates if this protocol is operating in asynchronous
    //   protocol mode.
    // </summary>
    // <remarks>
    //   If this property returns true, this protocol is operating
    //   in asynchronous protocol mode. Otherwise, it returns false.
    //   Asynchronous protocol mode can be enabled with the
    //   Initialize method. Also see IsValidOption for information
    //   on asynchronous logging and how to enable it.
    // </remarks>

    property Asynchronous: Boolean read FAsyncEnabled;

    // <summary>
    //   Represents the hostname of this protocol.
    // </summary>
    // <remarks>
    //   The hostname of a protocol is usually set to the name of
    //   the machine this protocol is created in. The hostname can
    //   be used to write TSiLogHeader packets after a successful
    //   protocol connect.
    // </remarks>

    property AppName: UnicodeString read FAppName write FAppName;

    // <summary>
    //   Represents the application name of this protocol.
    // </summary>
    // <remarks>
    //   The application name of a protocol is usually set to the name
    //   of the machine this protocol is created in. The application
    //   name can be used to write TSiLogHeader packets after a
    //   successful protocol connect.
    // </remarks>

    property HostName: UnicodeString read FHostName write FHostName;

    // <summary>
    //   This event is fired after an error occurred when operating
    //   in asynchronous mode.
    // </summary>
    // <seealso cref="TSiProtocolErrorEvent"/>
    // <remarks>
    //   This event is fired when an error occurs in asynchronous mode
    //   (see IsValidOption). Instead of throwing exceptions when an
    //   operation has failed like in normal blocking mode, the
    //   asynchronous mode uses this OnError event for error reporting.
    //
    //   <b>Please note</b>: Keep in mind that adding code to the
    //   event handlers which can lead to the error event can cause a
    //   presumably undesired recursive behavior.
    // </remarks>

    property OnError: TSiProtocolErrorEvent read GetOnError write SetOnError;
  end;

  // <summary>
  //   Used for writing log data to memory and saving it to a stream
  //   or another protocol object on request.
  // </summary>
  // <remarks>
  //   This class is used for writing log data to memory. On request
  //   this data can be saved to a stream or another protocol object. To
  //   initiate such a request, use the InternalDispatch method.
  //
  //   This class is used when the 'mem' protocol is specified in the
  //   <link TSmartInspect.Connections, connections string>. Please see
  //   the IsValidOption method for a list of available options for this
  //   protocol.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiMemoryProtocol = class(TSiProtocol)
  private
    FFormatter: TSiFormatter;
    FAsText: Boolean;
    FIndent: Boolean;
    FPattern: UnicodeString;
    FMaxSize: Int64;
    FQueue: TSiPacketQueue;
    procedure InitializeFormatter;
    procedure DeletePacket(ASender: TSiPacketQueue; APacket: TSiPacket);
    procedure FlushToStream(const AStream: TStream);
    procedure FlushToProtocol(const AProtocol: TSiProtocol);
  protected

    // <summary>
    //   Overridden. Returns 'mem'.
    // </summary>
    // <returns>
    //   Just 'mem'. Derived classes can change this behavior by
    //   overriding this method.
    // </returns>

    function GetName: UnicodeString; override;

    // <summary>
    //   Overridden. Creates and initializes the packet queue.
    // </summary>
    // <remarks>
    //   This method creates and initializes a new packet queue with a
    //   maximum size as specified by the Initialize method. For other
    //   valid options which might affect the behavior of this method
    //   and protocol, please see the IsValidOption method.
    // </remarks>

    procedure InternalConnect; override;

    // <summary>
    //   Overridden. Clears the internal queue of packets.
    // </summary>
    // <remarks>
    //   This method does nothing more than to clear the internal queue of
    //   packets. After this method has been called, the InternalDispatch
    //   method writes an empty log unless new packets are queued in the
    //   meantime.
    // </remarks>

    procedure InternalDisconnect; override;

    // <summary>
    //   Overridden. Writes a packet to the packet queue.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   This method writes the supplied packet to the internal queue of
    //   packets. If the size of the queue exceeds the maximum size as
    //   specified by the Options property, the queue is automatically
    //   resized and older packets are discarded.
    // </remarks>

    procedure InternalWritePacket(const APacket: TSiPacket); override;

    // <summary>
    //   Overridden. Implements a custom action for saving the current
    //   queue of packets of this memory protocol to a stream or
    //   protocol object.
    // </summary>
    // <param name="ACommand">
    //   The protocol command which is expected to provide the stream
    //   or protocol object.
    // </param>
    // <seealso cref="TSmartInspect.Dispatch"/>
    // <seealso cref="TSiProtocol.Dispatch"/>
    // <remarks>
    //   Depending on the supplied ACommand argument, this method does
    //   the following.
    //
    //   If the supplied State object of the protocol command is of
    //   type TStream, then this method uses this stream to write the
    //   entire content of the internal queue of packets. The necessary
    //   header is written first and then the actual packets are
    //   appended.
    //
    //   The header and packet output format can be influenced with
    //   the "astext" protocol option (see IsValidOption). If the
    //   "astext" option is true, the header is a UTF8 Byte Order
    //   Mark and the packets are written in plain text format. If
    //   the "astext" option is false, the header is the standard
    //   header for SmartInspect log files and the packets are
    //   written in the default binary mode. In the latter case, the
    //   resulting log files can be loaded by the SmartInspect
    //   Console.
    //
    //   If the supplied State object of the protocol command is of
    //   type TSiProtocol instead, then this method uses this
    //   protocol object to call its WritePacket method for each
    //   packet in the internal packet queue.
    //
    //   The Action property of the ACommand argument should currently
    //   always be set to 0. If the State object is not a stream or
    //   a protocol object or if the ACommand argument is nil, then
    //   this method does nothing.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Writing the internal queue of packets to the
    //                         supplied stream or protocol object failed.
    // </table>
    // </exception>

    procedure InternalDispatch(const ACommand: TSiProtocolCommand); override;

    // <summary>
    //   Overridden. Loads and inspects memory specific options.
    // </summary>
    // <remarks>
    //   This method loads all relevant options and ensures their
    //   correctness. See IsValidOption for a list of options which
    //   are recognized by the memory protocol.
    // </remarks>

    procedure LoadOptions; override;

    // <summary>
    //   Overridden. Fills a TSiConnectionsBuilder instance with the
    //   options currently used by this memory protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); override;

    // <summary>
    //   Overridden. Validates if a protocol option is supported.
    // </summary>
    // <param name="AOption">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default values
    //   and descriptions for this memory protocol. For a list of options
    //   common to all protocols, please have a look at the
    //   <link TSiProtocol.IsValidOption, IsValidOption> method of the
    //   parent class.
    //
    //   <table>
    //   Valid Options  Default Value                       Description
    //   -              -                                   -
    //   astext         false                               Specifies if
    //                                                       logging data
    //                                                       should be
    //                                                       written as
    //                                                       text instead
    //                                                       of binary.
    //
    //   indent         false                               Indicates if the
    //                                                       logging output
    //                                                       should
    //                                                       automatically be
    //                                                       indented like in
    //                                                       the Console if
    //                                                       'astext' is set
    //                                                       to true.
    //
    //
    //   maxsize        2048                                Specifies the
    //                                                       maximum size
    //                                                       of the packet
    //                                                       queue of this
    //                                                       protocol in
    //                                                       kilobytes.
    //                                                       Specify size
    //                                                       units like
    //                                                       this: "1 MB".
    //                                                       Supported
    //                                                       units are
    //                                                       "KB", "MB" and
    //                                                       "GB".
    //
    //   pattern        "[%timestamp%] %level%: %title%"    Specifies the
    //                                                        pattern used
    //                                                        to create a
    //                                                        text
    //                                                        representation
    //                                                        of a packet.
    //   </table>
    //
    //   If the "astext" option is used for creating a textual output
    //   instead of the default binary, the "pattern" string specifies
    //   the textual representation of a log packet. For detailed
    //   information of how a pattern string can look like, please
    //   have a look at the documentation of the TSiPatternParser class,
    //   especially the TSiPatternParser.Pattern property.
    // </remarks>
    // <example>
    // <code>
    // Si.Connections := 'mem()';
    // Si.Connections := 'mem(maxsize="8MB")';
    // Si.Connections := 'mem(astext=true)';
    // </code>
    // </example>

    function IsValidOption(const AOption: UnicodeString): Boolean; override;
  public

    // <summary>
    //   Overridden. Creates and initializes a TSiMemoryProtocol instance.
    //   For a list of available memory protocol options, please refer to
    //   the IsValidOption method.
    // </summary>

    constructor Create; override;

    // <summary>
    //   Overridden. Releases any resources used by an TSiMemoryProtocol
    //   object.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   Used for sending packets to a local SmartInspect Console over a
  //   named pipe connection.
  // </summary>
  // <remarks>
  //   This class is used for sending packets through a local named pipe
  //   to the SmartInspect Console. It is used when the 'pipe' protocol
  //   is specified in the <link TSmartInspect.Connections,
  //   connections string>. Please see the IsValidOption method for a
  //   list of available protocol options. Please note that this protocol
  //   can only be used for local connections. For remote connections to
  //   other machines, please use TSiTcpProtocol.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiPipeProtocol = class(TSiProtocol)
  private
    FFormatter: TSiFormatter;
    FHandle: THandle;
    FStream: TStream;
    FBuffer: TSiBufferedStream;
    FPipeName: UnicodeString;
    function CreateHandle(const APipeName: UnicodeString;
      var AErrorCode: Cardinal): Boolean;
    class procedure DoHandShake(const AStream: TStream);
  protected

    // <summary>
    //   Overridden. Returns 'pipe'.
    // </summary>

    function GetName: UnicodeString; override;

    // <summary>
    //   Overridden. Validates if a protocol option is supported.
    // </summary>
    // <param name="AOption">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default
    //   values and descriptions for the pipe protocol.
    //
    //   <table>
    //   Valid Options  Default Value   Description
    //   -              -               -
    //   pipename       'smartinspect'  Specifies the named pipe
    //                                   for sending log packets to
    //                                   the SmartInspect Console.
    //   </table>
    //
    //   For further options which affect the behavior of this
    //   protocol, please have a look at the documentation of the
    //   <link TSiProtocol.IsValidOption, IsValidOption> method of the
    //   parent class.
    // </remarks>
    // <example>
    // <code>
    // Si.Connections := 'pipe()';
    // Si.Connections := 'pipe(pipename="logging")';
    // </code>
    // </example>

    function IsValidOption(const AOption: UnicodeString): Boolean; override;

    // <summary>
    //   Overridden. Fills a TSiConnectionsBuilder instance with the
    //   options currently used by this pipe protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); override;

    // <summary>
    //   Overridden. Loads and inspects pipe specific options.
    // </summary>
    // <remarks>
    //   This method loads all relevant options and ensures their
    //   correctness. See IsValidOption for a list of options which
    //   are recognized by the pipe protocol.
    // </remarks>

    procedure LoadOptions; override;

    // <summary>
    //   Overridden. Connects to the specified local named pipe.
    // </summary>
    // <remarks>
    //   This method tries to establish a connection to a local named
    //   pipe of a SmartInspect Console. The name of the pipe can be
    //   specified by passing the 'pipename' option to the Initialize
    //   method.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Establishing the named pipe connection
    //                        failed.
    // </table>
    // </exception>

    procedure InternalConnect; override;

    // <summary>
    //   Overriden. Tries to reconnect to the specified local named
    //   pipe.
    // </summary>
    // <returns>
    //   True if the reconnect attempt has been successful and false
    //   otherwise.
    // </returns>
    // <remarks>
    //   This method tries to (re-)establish a connection to the local
    //   named pipe of a SmartInspect Console. The name of the pipe can
    //   be specified by passing the 'pipename' option to the Initialize
    //   method.
    // </remarks>

    function InternalReconnect: Boolean; override;

    // <summary>
    //   Overridden. Sends a packet to the Console.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   This method sends the supplied packet to the SmartInspect
    //   Console over the previously established named pipe connection.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Sending the packet to the Console failed.
    // </table>
    // </exception>

    procedure InternalWritePacket(const APacket: TSiPacket); override;

    // <summary>
    //   Overridden. Closes the connection to the specified local
    //   named pipe.
    // </summary>
    // <remarks>
    //   This method closes the named pipe handle if previously created
    //   and disposes any supplemental objects.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   Exception              Closing the named pipe handle failed.
    // </table>
    // </exception>

    procedure InternalDisconnect; override;
  public

    // <summary>
    //   Creates and initializes a TSiPipeProtocol instance. For a
    //   list of available pipe protocol options, please refer to the
    //   IsValidOption method.
    // </summary>

    constructor Create; override;

    // <summary>
    //   Overridden. Releases any resources used by this TSiPipeProtocol
    //   instance.
    // </summary>

    destructor Destroy; override;
  end;

  TSiFileHelper = class
  private
    class function IsValidFile(const ABaseName: UnicodeString;
      const APath: UnicodeString): Boolean;
    class procedure GetFiles(const ABaseName: UnicodeString;
      const AFiles: TSiStringList);
    class function FindFileName(const ABaseName: UnicodeString):
      UnicodeString;
    class function ExpandFileName(const ABaseName: UnicodeString):
      UnicodeString;
    class function TryGetFileDate(const ABaseName: UnicodeString;
      const APath: UnicodeString; var AFileDate: TDateTime): Boolean;
    class function TryParseFileDate(const AFileDate: UnicodeString;
      var ADateTime: TDateTime): Boolean;
  public
    class function GetFileName(const ABaseName: UnicodeString;
      const AAppend: Boolean): UnicodeString;
    class function GetFileDate(const ABaseName: UnicodeString;
      const APath: UnicodeString): TDateTime;
    class procedure DeleteFiles(const ABaseName: UnicodeString;
      const AMaxParts: Integer);
  end;

  // <summary>
  //   The standard SmartInspect protocol for writing log packets to a log
  //   file.
  // </summary>
  // <remarks>
  //   TSiFileProtocol is the base class for all protocol classes which
  //   deal with log files. By default, it uses the binary log file format
  //   which is compatible to the Console. Derived classes can change this
  //   behavior. For example, for a simple protocol which is capable of
  //   creating plain text files, see the TSiTextProtocol class.
  //
  //   The file protocol supports a variety of options, such as log
  //   rotation (by size and date), encryption and I/O buffers. For a
  //   complete list of available protocol options, please have a look at
  //   the IsValidOption method.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiFileProtocol = class(TSiProtocol)
  private
{$IFNDEF SI_DISABLE_ENCRYPT}
    FCipher: TCipher;
    FCipherStream: TEncryptStream;
{$ENDIF}
    FRotate: TSiFileRotate;
    FRotater: TSiFileRotater;
    FFormatter: TSiFormatter;
    FStream: TStream;
    FFileStream: TStream;
    FBuffer: TSiBufferedStream;
    FIOBuffer: Int64;
    FIOBufferCounter: Int64;
    FFileSize, FMaxSize: Int64;
    FMaxParts: Integer;
    FAppend: Boolean;
    FFileName: UnicodeString;
{$IFNDEF SI_DISABLE_ENCRYPT}
    FKey: TSiBytes;
    FEncrypt: Boolean;
    procedure RaiseException(const AMessage: UnicodeString);
    function GetIVector: OctetString;
{$ENDIF}
    procedure Rotate;
    function IsRotating: Boolean;
    procedure InternalBeforeConnect;
    procedure InternalDoConnect(const AAppend: Boolean);
    procedure InternalAfterConnect(const AFileName: UnicodeString);
  protected

    // <summary>
    //   Overridden. Returns 'file'.
    // </summary>
    // <returns>
    //   Just 'file'. Derived classes can change this behavior by
    //   overriding this method.
    // </returns>

    function GetName: UnicodeString; override;

    // <summary>
    //   Returns the formatter for this log file protocol.
    // </summary>
    // <returns>
    //   The formatter for this log file protocol.
    // </returns>
    // <remarks>
    //   The standard implementation of this method returns an instance
    //   of the TSiBinaryFormatter class. Derived classes can change this
    //   behavior by overriding this method.
    // </remarks>

    function GetFormatter: TSiFormatter; virtual;

    // <summary>
    //   Returns the default filename for this log file protocol.
    // </summary>
    // <returns>
    //   The default filename for this log file protocol.
    // </returns>
    // <remarks>
    //   The standard implementation of this method returns the string
    //   'log.sil' here. Derived classes can change this behavior by
    //   overriding this method.
    // </remarks>

    function GetDefaultFileName: UnicodeString; virtual;

    // <summary>
    //   Intended to provide a wrapper stream for the underlying
    //   file stream.
    // </summary>
    // <param name="AStream">The underlying file stream.</param>
    // <returns>The wrapper stream.</returns>
    // <remarks>
    //   This method can be used by custom protocol implementers
    //   to wrap the underlying file stream into a filter stream.
    //   Such filter streams could include streams for encrypting
    //   or compressing log files, for example. Please note that
    //   it is not necessary to free the returned object. The
    //   returned filter stream is freed automatically when this
    //   protocol object is closed or destroyed.
    //
    //   By default, this method simply returns the passed AStream
    //   argument.
    // </remarks>

    function GetStream(const AStream: TStream): TStream; virtual;

    // <summary>
    //   Overridden. Validates if a protocol option is supported.
    // </summary>
    // <param name="AOption">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default values
    //   and descriptions for the file protocol.
    //
    //   <table>
    //   Valid Options  Default Value  Description
    //   -              -              -
    //   append         false          Specifies if new packets should be
    //                                   appended to the destination file
    //                                   instead of overwriting the file.
    //
    //   buffer         0              Specifies the I/O buffer size in
    //                                   kilobytes. It is possible to
    //                                   specify size units like this:
    //                                   "1 MB". Supported units are "KB",
    //                                   "MB" and "GB". A value of 0
    //                                   disables this feature. Enabling
    //                                   the I/O buffering greatly improves
    //                                   the logging performance but has
    //                                   the disadvantage that log packets
    //                                   are temporarily stored in memory
    //                                   and are not immediately written
    //                                   to disk.
    //
    //   encrypt        false          Specifies if the resulting log
    //                                   file should be encrypted. Note
    //                                   that the 'append' option cannot
    //                                   be used with encryption enabled.
    //                                   If encryption is enabled the
    //                                   'append' option has no effect.
    //
    //   filename       [varies]       Specifies the filename of the log.
    //
    //   key            [empty]        Specifies the secret encryption
    //                                   key as string if the 'encrypt'
    //                                   option is enabled.
    //
    //   maxparts       [varies]       Specifies the maximum amount of
    //                                   log files at any given time when
    //                                   log rotating is enabled or the
    //                                   maxsize option is set. Specify
    //                                   0 for no limit. See below for
    //                                   information on the default
    //                                   value for this option.
    //
    //   maxsize        0              Specifies the maximum size of a
    //                                   log file in kilobytes. When this
    //                                   size is reached, the current log
    //                                   file is closed and a new file
    //                                   is opened. The maximum amount
    //                                   of log files can be set with
    //                                   the maxparts option. It is
    //                                   possible to specify size units
    //                                   like this: "1 MB". Supported
    //                                   units are "KB", "MB" and "GB".
    //                                   A value of 0 disables this
    //                                   feature.
    //
    //   rotate         none           Specifies the rotate mode for log
    //                                   files. Please see below for a list
    //                                   of available values. A value of
    //                                   'none' disables this feature.
    //                                   The maximum amount of log files
    //                                   can be set with the maxparts
    //                                   option.
    //   </table>
    //
    //   When using the standard binary log file protocol ('file' in the
    //   <link TSmartInspect.Connections, connections string>), the default
    //   filename is set to 'log.sil'. When using text log files ('text'
    //   in the <link TSmartInspect.Connections, connections string>), the
    //   default filename is 'log.txt'.
    //
    //   The append option specifies if new packets should be appended to
    //   the destination file instead of overwriting the file. The default
    //   value of this option is 'false'.
    //
    //   The rotate option specifies the date log rotate mode for this
    //   file protocol. When this option is used, the filename of the
    //   resulting log consists of the value of the filename option and
    //   an appended time stamp (the used time stamp format thereby is
    //   'yyyy-MM-dd-HH-mm-ss'). To avoid problems with daylight saving
    //   time or time zone changes, the time stamp is always in UTC
    //   (Coordinated Universal Time). The following table lists the
    //   available rotate modes together with a short description.
    //
    //   <table>
    //   Rotate Mode       Description
    //   -                 -
    //   None              Rotating is disabled
    //   Hourly            Rotate hourly
    //   Daily             Rotate daily
    //   Weekly            Rotate weekly
    //   Monthly           Rotate monthly
    //   </table>
    //
    //   As example, if you specify 'log.sil' as value for the filename
    //   option and use the Daily rotate mode, the log file is rotated
    //   daily and always has a name of log-yyyy-MM-dd-HH-mm-ss.sil. In
    //   addition to, or instead of, rotating log files by date, you
    //   can also let the file protocol rotate log files by size. To
    //   enable this feature, set the maxsize option to the desired
    //   maximum size. Similar to rotating by date, the resulting log
    //   files include a time stamp. Note that starting with
    //   SmartInspect 3.0, it is supported to combine the maxsize and
    //   rotate options (i.e. use both options at the same time).
    //
    //   To control the maximum amount of created log files for the
    //   rotate and/or maxsize options, you can use the maxparts option.
    //   The default value for maxparts is 2 when used with the maxsize
    //   option, 0 when used with rotate and 0 when both options,
    //   maxsize and rotate, are used.
    //
    //   SmartInspect log files can be automatically encrypted by
    //   enabling the 'encrypt' option. The used cipher is Rijndael
    //   (AES) with a key size of 128 bit. The secret encryption key
    //   can be specified with the 'key' option. The specified
    //   key is automatically shortened or padded (with zeros) to a
    //   key size of 128 bit. Note that the 'append' option cannot be
    //   used in combination with encryption enabled. If encryption
    //   is enabled the 'append' option has no effect.
    //
    //   For further options which affect the behavior of this protocol,
    //   please have a look at the documentation of the
    //   <link TSiProtocol.IsValidOption, IsValidOption> method of the
    //   parent class.
    // </remarks>
    // <example>
    // <code>
    // Si.Connections := 'file()';
    // Si.Connections := 'file(filename="log.sil", append=true)';
    // Si.Connections := 'file(filename="log.sil")';
    // Si.Connections := 'file(maxsize="16MB", maxparts=5)';
    // Si.Connections := 'file(rotate=weekly)';
    // Si.Connections := 'file(encrypt=true, key="secret")';
    // </code>
    // </example>

    function IsValidOption(const AOption: UnicodeString): Boolean; override;

    // <summary>
    //   Overridden. Fills a TSiConnectionsBuilder instance with the
    //   options currently used by this file protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); override;

    // <summary>
    //   Overridden. Loads and inspects file specific options.
    // </summary>
    // <remarks>
    //   This method loads all relevant options and ensures their
    //   correctness. See IsValidOption for a list of options which
    //   are recognized by the file protocol.
    // </remarks>

    procedure LoadOptions; override;

    // <summary>
    //   Overridden. Opens the destination file.
    // </summary>
    // <remarks>
    //   This method tries to open the destination file, which can be
    //   specified by passing the "filename" option to the Initialize method.
    //   For other valid options which might affect the behavior of this
    //   method, please see the IsValidOption method.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   Exception              Opening the destination file failed.
    // </table>
    // </exception>

    procedure InternalConnect; override;

    // <summary>
    //   Overridden. Closes the destination file.
    // </summary>
    // <exception>
    // <remarks>
    //   This method closes the underlying file handle if previously
    //   created and frees any supplemental objects.
    // </remarks>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   Exception              Closing the destination file failed.
    // </table>
    // </exception>

    procedure InternalDisconnect; override;

    // <summary>
    //   Overridden. Writes a packet to the destination file.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   If the "maxsize" option is set and the supplied packet would
    //   exceed the maximum size of the destination file, then the
    //   current log file is closed and a new log file is opened.
    //   Additionally, if the "rotate" option is active, the log file
    //   is rotated if necessary. Please see the documentation of the
    //   IsValidOption method for more information.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   Exception              Writing the packet to the destination file
    //                            failed.
    // </table>
    // </exception>

    procedure InternalWritePacket(const APacket: TSiPacket); override;

    // <summary>
    //   Intended to write the header of a log file.
    // </summary>
    // <param name="AStream">
    //   The stream to which the header should be written to.
    // </param>
    // <param name="ASize">
    //   Specifies the current size of the supplied stream.
    // </param>
    // <returns>
    //   The new size of the stream after writing the header. If no
    //   header is written, the supplied ASize argument is returned.
    // </returns>
    // <remarks>
    //   This default implementation of this method writes the standard
    //   binary protocol header to the supplied TStream instance.
    //   Derived classes may change this behavior by overriding this
    //   method.
    // </remarks>

    function WriteHeader(const AStream: TStream;
      const ASize: Int64): Int64; virtual;

    // <summary>
    //   Intended to write the footer of a log file.
    // </summary>
    // <param name="AStream">
    //   The stream to which the footer should be written to.
    // </param>
    // <remarks>
    //   This default implementation of this method does nothing.
    //   Derived classes may change this behavior by overriding this
    //   method.
    // </remarks>

    procedure WriteFooter(const AStream: TStream); virtual;
  public

    // <summary>
    //   Overridden. Creates and initializes a TSiFileProtocol instance.
    //   For a list of available File protocol options, please refer to
    //   the IsValidOption method.
    // </summary>

    constructor Create; override;

    // <summary>
    //   Overridden. Releases any resources used by this TSiFileProtocol
    //   instance.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   Used for writing customizable plain text log files.
  // </summary>
  // <remarks>
  //   TSiTextProtocol is used for writing plain text log files. This
  //   class is used when the 'text' protocol is specified in the
  //   <link TSmartInspect.Connections, connections string>. See the
  //   IsValidOption method for a list of available protocol options.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiTextProtocol = class(TSiFileProtocol)
  private
    FIndent: Boolean;
    FPattern: UnicodeString;
    FFormatter: TSiTextFormatter;
  protected

    // <summary>
    //   Overridden. Returns 'text'.
    // </summary>
    // <returns>Just 'text'.</returns>

    function GetName: UnicodeString; override;

    // <summary>
    //   Overridden. Returns the formatter for this log file protocol.
    // </summary>
    // <returns>
    //   The formatter for this log file protocol.
    // </returns>
    // <remarks>
    //   The standard implementation of this method returns an instance
    //   of the TSiTextFormatter class. Derived classes can change this
    //   behavior by overriding this method.
    // </remarks>

    function GetFormatter: TSiFormatter; override;

    // <summary>
    //   Overridden. Returns the default filename for this log file
    //   protocol.
    // </summary>
    // <returns>
    //   The default filename for this log file protocol.
    // </returns>
    // <remarks>
    //   The standard implementation of this method returns the string
    //   'log.txt' here. Derived classes can change this behavior by
    //   overriding this method.
    // </remarks>

    function GetDefaultFileName: UnicodeString; override;

    // <summary>
    //   Overridden. Loads and inspects file specific options.
    // </summary>
    // <remarks>
    //   This method loads all relevant options and ensures their
    //   correctness. See IsValidOption for a list of options which
    //   are recognized by the file protocol.
    // </remarks>

    procedure LoadOptions; override;

    // <summary>
    //   Overridden. Fills a TSiConnectionsBuilder instance with the
    //   options currently used by this text protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); override;

    // <summary>
    //   Overridden. Validates if a protocol option is supported.
    // </summary>
    // <param name="AOption">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default values
    //   and descriptions for this text file protocol. For a list of
    //   options common to all file protocols, please have a look at the
    //   <link TSiFileProtocol.IsValidOption, IsValidOption> method of
    //   the parent class. Please note that this text protocol <b>does
    //   not support log file encryption</b>.
    //
    //   <table>
    //   Valid Options  Default Value                     Description
    //   -              -                                 -
    //   indent         false                             Indicates if the
    //                                                      logging output
    //                                                      should
    //                                                      automatically be
    //                                                      indented like in
    //                                                      the Console.
    //
    //   pattern        '[%timestamp%] %level%: %title%'  Specifies the
    //                                                      pattern used to
    //                                                      create a text
    //                                                      representation
    //                                                      of a packet.
    //   </table>
    //
    //   For detailed information of how a pattern string can look like,
    //   please have a look at the documentation of the TSiPatternParser
    //   class, especially the TSiPatternParser.Pattern property.
    // </remarks>
    // <example>
    // <code>
    // Si.Connections := 'text()';
    // Si.Connections := 'text(filename="log.txt", append=true)';
    // Si.Connections := 'text(filename="log.txt")';
    // Si.Connections := 'text(maxsize="16MB")';
    // </code>
    // </example>

    function IsValidOption(const AOption: UnicodeString): Boolean; override;

    // <summary>
    //   Overridden. Intended to write the header of a log file.
    // </summary>
    // <param name="AStream">
    //   The stream to which the header should be written to.
    // </param>
    // <param name="ASize">
    //   Specifies the current size of the supplied stream.
    // </param>
    // <returns>
    //   The new size of the stream after writing the header. If no
    //   header is written, the supplied ASize argument is returned.
    // </returns>
    // <remarks>
    //   The implementation of this method writes the standard UTF8
    //   BOM (byte order mark) to the supplied stream in order to
    //   identify the log file as text file in UTF8 encoding. Derived
    //   classes may change this behavior by overriding this method.
    // </remarks>

    function WriteHeader(const AStream: TStream;
      const ASize: Int64): Int64; override;

    // <summary>
    //   Overridden. Intended to write the footer of a log file.
    // </summary>
    // <param name="AStream">
    //   The stream to which the footer should be written to.
    // </param>
    // <remarks>
    //   The implementation of this method does nothing. Derived
    //   class may change this behavior by overriding this method.
    // </remarks>

    procedure WriteFooter(const AStream: TStream); override;
  public

    // <summary>
    //   Overridden. Creates and initializes a TSiTextProtocol instance.
    //   For a list of available text protocol options, please refer to
    //   the IsValidOption method.
    // </summary>

    constructor Create; override;

    // <summary>
    //   Overridden. Releases all resources used by this TSiTextProtocol
    //   instance.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   Used for sending packets to the SmartInspect Console over a TCP
  //   socket connection.
  // </summary>
  // <remarks>
  //   This class is used for sending packets over a TCP connection to
  //   the Console. It is used when the 'tcp' protocol is specified in
  //   the <link TSmartInspect.Connections, connections string>. Please
  //   see the IsValidOption method for a list of available protocol
  //   options.
  // </remarks>
  // <threadsafety>
  //   The public members of this class are threadsafe.
  // </threadsafety>

  TSiTcpProtocol = class(TSiProtocol)
  private
    FFormatter: TSiFormatter;
    FHost: AnsiString; // sic!
    FPort: Word;
    FTimeout: Integer;
    FStream: TSiTcpClientStream;
    FBuffer: TSiBufferedStream;
    FTcpClient: TSiTcpClient;
    procedure InitializeTcpClient;
  protected

    // <summary>
    //   Overridden. Returns 'tcp'.
    // </summary>
    // <returns>Just 'tcp'.</returns>

    function GetName: UnicodeString; override;

    // <summary>
    //   Overridden. Validates if a protocol option is supported.
    // </summary>
    // <param name="AOption">The option name to validate.</param>
    // <returns>
    //   True if the option is supported and false otherwise.
    // </returns>
    // <remarks>
    //   The following table lists all valid options, their default values
    //   and descriptions for the TCP protocol.
    //
    //   <table>
    //   Valid Options  Default Value  Description
    //   -              -              -
    //   host           '127.0.0.1'    Specifies the hostname where the
    //                                   Console is running.
    //
    //   port           4228           Specifies the Console port.
    //
    //   timeout        30000          Specifies the connect, receive and
    //                                   send timeout in milliseconds.
    //   </table>
    //
    //   For further options which affect the behavior of this protocol,
    //   please have a look at the documentation of the
    //   <link TSiProtocol.IsValidOption, IsValidOption> method of the
    //   parent class.
    // </remarks>
    // <example>
    // <code>
    // Si.Connections := 'tcp()';
    // Si.Connections := 'tcp(host="localhost", port=4229)';
    // Si.Connections := 'tcp(timeout=2500)';
    // </code>
    // </example>

    function IsValidOption(const AOption: UnicodeString): Boolean; override;

    // <summary>
    //   Overridden. Fills a TSiConnectionsBuilder instance with the
    //   options currently used by this TCP protocol.
    // </summary>
    // <param name="ABuilder">
    //   The TSiConnectionsBuilder object to fill with the current options
    //   of this protocol.
    // </param>

    procedure BuildOptions(const ABuilder: TSiConnectionsBuilder); override;

    // <summary>
    //   Overridden. Loads and inspects TCP specific options.
    // </summary>
    // <remarks>
    //   This method loads all relevant options and ensures their
    //   correctness. See IsValidOption for a list of options which
    //   are recognized by the TCP protocol.
    // </remarks>

    procedure LoadOptions; override;

    // <summary>
    //   Overridden. Creates and connects a TCP socket.
    // </summary>
    // <remarks>
    //   This method tries to connect a TCP socket to a SmartInspect
    //   Console. The hostname and port can be specified by passing
    //   the "hostname" and "port" option to the Initialize method.
    //   Furthermore, it is possible to specify the connect timeout by
    //   using the "timeout" option.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Creating or connecting the socket failed.
    // </table>
    // </exception>

    procedure InternalConnect; override;

    // <summary>
    //   Overridden. Closes the TCP socket connection.
    // </summary>
    // <remarks>
    //   This method closes the underlying socket handle if previously
    //   created and frees any supplemental objects.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Closing the TCP socket failed.
    // </table>
    // </exception>

    procedure InternalDisconnect; override;

    // <summary>
    //   Overridden. Sends a packet to the Console.
    // </summary>
    // <param name="APacket">The packet to write.</param>
    // <remarks>
    //   This method sends the supplied packet to the SmartInspect
    //   Console and waits for a valid response.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type      Condition
    //   -                   -
    //   Exception           Sending the packet to the Console failed.
    // </table>
    // </exception>

    procedure InternalWritePacket(const APacket: TSiPacket); override;
  public

    // <summary>
    //   Overridden. Creates and initializes a TSiTcpProtocol instance.
    //   For a list of available TCP protocol options, please refer to
    //   the IsValidOption method.
    // </summary>

    constructor Create; override;

    // <summary>
    //   Overridden. Releases any resources used by this TSiTcpProtocol
    //   instance.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   Represents the class-reference type of a protocol.
  // </summary>
  // <remarks>
  //   This type is used in the protocol registration process of the
  //   TSiProtocolFactory class. For information on how to use this type,
  //   please refer to the TSiProtocolFactory.RegisterProtocol method
  //   which can be used to add custom protocols.
  // </remarks>

  TSiProtocolClass = class of TSiProtocol;

  // <summary>
  //   Creates TSiProtocol instances and registers custom protocols.
  // </summary>
  // <remarks>
  //   This class is responsible for creating instances of TSiProtocol
  //   subclasses and registering custom protocol implementations. To
  //   add a custom protocol, please have a look at the documentation
  //   and example of the RegisterProtocol method.
  // </remarks>
  // <threadsafety>
  //   The class is fully threadsafe.
  // </threadsafety>

  TSiProtocolFactory = class
  public

    // <summary>
    //   Creates an instance of a TSiProtocol subclass.
    // </summary>
    // <param name="AName">The protocol name to search for.</param>
    // <param name="AOptions">
    //   The options to apply to the new TSiProtocol instance.
    // </param>
    // <returns>A new instance of a TSiProtocol subclass.</returns>
    // <remarks>
    //   This method tries to create an instance of a TSiProtocol subclass
    //   using the AName parameter. If you, for example, specify 'file'
    //   as AName parameter, this method returns an instance of the
    //   TSiFileProtocol class. If the creation of such an instance has
    //   been successful, then the supplied options will be applied to
    //   the protocol.
    //
    //   For a list of available protocols, please refer to the TSiProtocol
    //   class. Additionally, to add your own custom protocol implementation,
    //   please have a look at the RegisterProtocol method.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type         Condition
    //   -                      -
    //   ESmartInspectError     Unknown protocol or invalid options syntax.
    // </table>
    // </exception>

    class function GetProtocol(const AName, AOptions: UnicodeString):
      TSiProtocol;

    // <summary>
    //   Registers a custom protocol implementation to the SmartInspect
    //   Delphi library.
    // </summary>
    // <param name="AName">
    //   The name of the custom protocol to register.
    // </param>
    // <param name="AImpl">
    //   The class-reference of the custom protocol implementation.
    // </param>
    // <remarks>
    //   This method enables you to register your own custom protocols.
    //   This can be used to extend the built-in capabilities of the
    //   SmartInspect Delphi library. To add your own protocol, derive
    //   your custom protocol class from TSiProtocol, choose a name and
    //   pass this name and a reference to your class to this method.
    //   After registering your protocol, you are able to use it in the
    //   <link TSmartInspect.Connections, connections string> just like
    //   any other (standard) protocol.
    //
    //   If the AName parameter is an empty string or the AImpl argument
    //   is nil, then no custom protocol is added.
    // </remarks>
    // <example>
    // <code>
    //  uses
    //    SiAuto, SmartInspect;
    //
    //  type
    //    TStdoutProtocol = class(TSiProtocol)
    //    protected
    //      // Implement the abstract methods and handle your protocol
    //      // specific options ..
    //    end;
    //
    //    ...
    //
    //  procedure TForm1.Button1Click(Sender: TObject);
    //  begin
    //    TSiProtocolFactory.RegisterProtocol('stdout', TStdoutProtocol);
    //    Si.Connections := 'stdout()';
    //    Si.Enabled := True;
    //  end;
    // </code>
    // </example>

    class procedure RegisterProtocol(const AName: UnicodeString;
      const AImpl: TSiProtocolClass);
  end;

  // <summary>
  //   Is the abstract base class for a viewer context.
  // </summary>
  // <remarks>
  //   A viewer context consists of a <link TSiViewerId, viewer ID> and data
  //   which can be displayed in a viewer in the Console. Every viewer in
  //   the Console has a corresponding viewer context class in this library.
  //   A viewer context is capable of processing data and to format them
  //   in a way so that the corresponding viewer in the Console can display
  //   it.
  //
  //   Viewer contexts provide a simple way to extend the functionality of
  //   the SmartInspect Delphi library. See the TSiSession.LogCustomContext
  //   method for a detailed example.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiViewerContext = class(TObject)
  private
    FViewerId: TSiViewerId;
  protected

    // <summary>
    //   Intended to return the actual data which will be displayed in
    //   the viewer specified by the ViewerId property.
    // </summary>
    // <returns>
    //   The actual data which will be displayed.
    // </returns>

    function GetViewerData: TStream; virtual; abstract;
  public

    // <summary>
    //   Creates and initializes a TSiViewerContext instance.
    // </summary>
    // <param name="AViewerId">The viewer ID to use.</param>

    constructor Create(const AViewerId: TSiViewerId); virtual;

    // <summary>
    //   Returns the viewer ID which specifies the viewer to use in
    //   the Console.
    // </summary>

    property ViewerId: TSiViewerId read FViewerId;

    // <summary>
    //   Returns the actual data which will be displayed in the viewer
    //   specified by the ViewerId property.
    // </summary>

    property ViewerData: TStream read GetViewerData;
  end;

  // <summary>
  //   Is the base class for all viewer contexts, which deal with
  //   binary data. A viewer context is the library-side representation
  //   of a viewer in the Console.
  // </summary>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiBinaryContext = class(TSiViewerContext)
  private
    FData: TMemoryStream;
  protected

    // <summary>
    //   Returns the actual binary data which will be displayed in the
    //   viewer specified by the ViewerId property.
    // </summary>
    // <returns>
    //   The actual binary data which will be displayed.
    // </returns>

    function GetViewerData: TStream; override;

    // <summary>Resets the internal data stream.</summary>
    // <remarks>
    //   This method is intended to reset the internal data stream if
    //   custom handling of data is needed by derived classes.
    // </remarks>

    procedure ResetData;
  public

    // <summary>
    //   Creates and initializes a TSiBinaryContext instance.
    // </summary>
    // <param name="AViewerId">The viewer ID to use.</param>

    constructor Create(const AViewerId: TSiViewerId); override;

    // <summary>
    //   Overridden. Releases all resources used by this TSiBinaryContext
    //   instance.
    // </summary>

    destructor Destroy; override;

    // <summary>Loads the binary data from a file.</summary>
    // <param name="AFileName">
    //   The name of the file to load the binary data from.
    // </param>
    // <remarks>
    //   Please note that this method can raise an exception if the
    //   desired file can not be opened or an error occurs while reading
    //   the data from it.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        An I/O error occurred while trying
    //                               to open the requested file.
    //
    //   EStreamError              An I/O error occurred while trying
    //                               to load the binary data from the
    //                               specified file.
    // </table>
    // </exception>

    procedure LoadFromFile(const AFileName: UnicodeString);

    // <summary>Loads the binary data from a stream.</summary>
    // <param name="AStream">
    //   The stream to load the binary data from.
    // </param>
    // <remarks>
    //   Please note that this method can raise an exception if an error
    //   occurs while reading the data from the stream.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        The AStream argument is not assigned.
    //
    //   EStreamError              An I/O error occurred while trying
    //                               to load the binary data from the
    //                               specified stream.
    // </table>
    // </exception>

    procedure LoadFromStream(const AStream: TStream);

    // <summary>Appends a memory area.</summary>
    // <param name="AAddress">The address of the buffer.</param>
    // <param name="ASize">The amount of bytes to append.</param>
    // <exception>
    // <table>
    //   Exception Type           Condition
    //   -                        -
    //   ESmartInspectError       The AAddress argument is not assigned
    //                              or the ASize parameter is negative.
    // </table>
    // </exception>

    procedure AppendBytes(const AAddress: Pointer; const ASize: Integer);
  end;

  // <summary>
  //   Is the base class for all viewer contexts, which deal with
  //   text data. A viewer context is the library-side representation
  //   of a viewer in the Console.
  // </summary>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiTextContext = class(TSiViewerContext)
  private
    FBuilder: TSiStringBuilder;
    FData: TStream;
    procedure InternalLoadFromStream(const AStream: TStream);
  protected

    // <summary>
    //   Returns the actual text data which will be displayed in the
    //   viewer specified by the ViewerId property.
    // </summary>
    // <returns>
    //   The actual text data which will be displayed.
    // </returns>

    function GetViewerData: TStream; override;

    // <summary>Resets the internal text data.</summary>
    // <remarks>
    //   This method is intended to reset the internal text data if
    //   custom handling of data is needed by derived classes.
    // </remarks>

    procedure ResetData;

    // <summary>Escapes a line.</summary>
    // <param name="ALine">The line to escape.</param>
    // <returns>The escaped line.</returns>
    // <remarks>
    //   If overridden in derived classes, this method escapes a line
    //   depending on the used viewer format. The default implementation
    //   does no escaping.
    // </remarks>

    function EscapeLine(const ALine: UnicodeString): UnicodeString; virtual;
  public

    // <summary>
    //   Creates and initializes a TSiTextContext instance.
    // </summary>
    // <param name="AViewerId">The viewer ID to use.</param>

    constructor Create(const AViewerId: TSiViewerId); override;

    // <summary>
    //   Overridden. Releases all resources used by this TSiTextContext
    //   instance.
    // </summary>

    destructor Destroy; override;

    // <summary>Loads the text from a file.</summary>
    // <param name="AFileName">
    //   The name of the file to load the text data from.
    // </param>
    // <remarks>
    //   Please note that this method can raise an exception if the
    //   desired file can not be opened or an error occurs while reading
    //   the data from it.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        The desired file could not be opened.
    //
    //   EStreamError              An I/O error occurred while trying
    //                               to load the text data from the
    //                               specified file.
    // </table>
    // </exception>

    procedure LoadFromFile(const AFileName: UnicodeString);

    // <summary>Loads the text from a stream.</summary>
    // <param name="AStream">
    //   The stream to load the text data from.
    // </param>
    // <remarks>
    //   Please note that this method can raise an exception if an
    //   error occurs while reading the data from the stream.
    // </remarks>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        The AStream argument is not assigned.
    //
    //   EStreamError              An I/O error occurred while trying
    //                               to load the text data from the
    //                               specified stream.
    // </table>
    // </exception>

    procedure LoadFromStream(const AStream: TStream);

    // <summary>Loads the text.</summary>
    // <param name="AText">The text to load.</param>

    procedure LoadFromText(const AText: UnicodeString);

    // <summary>Appends text.</summary>
    // <param name="AText">The text to append.</param>

    procedure AppendText(const AText: UnicodeString);

    // <summary>Appends a line to the text data.</summary>
    // <param name="ALine">The line to append.</param>
    // <remarks>
    //   This method appends the supplied line and a carriage return +
    //   linefeed character to the internal text data after it has been
    //   escaped by the EscapeLine method.
    // </remarks>

    procedure AppendLine(const ALine: UnicodeString);
  end;

  // <summary>
  //   Represents the data viewer in the Console which can display simple
  //   and unformatted text.
  // </summary>
  // <remarks>
  //   The data viewer in the Console interprets the <link TSiLogEntry.Data,
  //   data of a Log Entry> as text and displays it in a read-only text
  //   field.
  //
  //   You can use the TSiDataViewerContext class for creating custom log
  //   methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom text data.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiDataViewerContext = class(TSiTextContext)
  public

    // <summary>
    //   Creates and initializes a TSiDataViewerContext instance.
    // </summary>

    constructor Create; reintroduce;
  end;

  // <summary>
  //   Represents the list viewer in the Console which can display simple
  //   lists of text data.
  // </summary>
  // <remarks>
  //   The list viewer in the Console displays the <link TSiLogEntry.Data,
  //   data of a Log Entry> as list. Every line in the text data is
  //   interpreted as one item of the list. This class takes care of the
  //   necessary formatting and escaping required by the corresponding list
  //   viewer in the Console.
  //
  //   You can use the TSiListViewerContext class for creating custom
  //   log methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom data organized as simple lists.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiListViewerContext = class(TSiTextContext)
  protected

    // <summary>Overridden. Escapes a line.</summary>
    // <param name="ALine">The line to escape.</param>
    // <returns>The escaped line.</returns>
    // <remarks>
    //   This method ensures that the escaped line does not contain any
    //   newline characters, such as the carriage return or linefeed
    //   characters.
    // </remarks>

    function EscapeLine(const ALine: UnicodeString): UnicodeString; overload;
      override;

    // <summary>Escapes a line.</summary>
    // <param name="ALine">The line to escape.</param>
    // <param name="AToEscape">
    //   A set of characters which should be escaped in addition
    //   to the newline characters. Can be empty.
    // </param>
    // <returns>The escaped line.</returns>
    // <remarks>
    //   This method ensures that the escaped line does not contain
    //   characters listed in the AToEscape parameter plus any newline
    //   characters, such as the carriage return or linefeed characters.
    // </remarks>

    class function EscapeLine(const ALine, AToEscape: UnicodeString):
      UnicodeString; reintroduce; overload;
  public

    // <summary>
    //   Overloaded. Creates and initializes a TSiListViewerContext
    //   instance.
    // </summary>

    constructor Create; reintroduce; overload;
  end;

  // <summary>
  //   Represents the value list viewer in the Console which can display
  //   data as a key/value list.
  // </summary>
  // <remarks>
  //   The value list viewer in the Console interprets the <link
  //   TSiLogEntry.Data, data of a Log Entry> as a simple key/value list.
  //   Every line in the text data is interpreted as one key/value item of
  //   the list. This class takes care of the necessary formatting and
  //   escaping required by the corresponding value list viewer of the
  //   Console.
  //
  //   You can use the TSiValueListViewerContext class for creating custom
  //   log methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom data organized as key/value lists.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiValueListViewerContext = class(TSiListViewerContext)
  public

    // <summary>
    //   Overloaded. Creates and initializes a TSiValueListViewerContext
    //   instance.
    // </summary>

    constructor Create; reintroduce; overload;

    // <summary>
    //   Overloaded. Appends a string value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The string value to use.</param>

    procedure AppendKeyValue(const AKey, AValue: UnicodeString); overload;

    // <summary>
    //   Overloaded. Appends a boolean value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The boolean value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString; const AValue: Boolean);
      overload;

    // <summary>
    //   Overloaded. Appends an integer value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The integer value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString; const AValue: Integer);
      overload;

    // <summary>
    //   Overloaded. Appends an int64 value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The int64 value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString; const AValue: Int64);
      overload;

    // <summary>
    //   Overloaded. Appends a cardinal value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The cardinal value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString; const AValue: Cardinal);
      overload;

    // <summary>
    //   Overloaded. Appends a TDateTime value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The TDateTime value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString;
      const AValue: TDateTime); overload;

    // <summary>
    //   Overloaded. Appends an extended value and its key.
    // </summary>
    // <param name="AKey">The key to use.</param>
    // <param name="AValue">The extended value to use.</param>

    procedure AppendKeyValue(const AKey: UnicodeString; const AValue: Extended);
      overload;

    // <summary>Escapes a key or a value.</summary>
    // <param name="AItem">The key or value to escape.</param>
    // <returns>The escaped key or value.</returns>
    // <remarks>
    //   This method ensures that the escaped key or value does not contain
    //   any newline characters, such as the carriage return or linefeed
    //   characters. Furthermore, it escapes the '\' and '=' characters.
    // </remarks>

    function EscapeItem(const AItem: UnicodeString): UnicodeString; virtual;
  end;

  // <summary>
  //   Represents the inspector viewer in the Console which displays
  //   key/value pairs in an object inspector control.
  // </summary>
  // <remarks>
  //   The inspector viewer in the Console displays the <link
  //   TSiLogEntry.Data, data of a Log Entry> as a key/value list with
  //   group support like object inspectors from popular IDEs. This
  //   class takes care of the necessary formatting and escaping required
  //   by the corresponding inspector viewer in the Console.
  //
  //   You can use the TSiInspectorViewerContext class for creating custom
  //   log methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom data organized as grouped key/value pairs.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiInspectorViewerContext = class(TSiValueListViewerContext)
  public

    // <summary>
    //   Creates and initializes a TSiInspectorViewerContext instance.
    // </summary>

    constructor Create;

    // <summary>Starts a new group.</summary>
    // <param name="AGroup">The name of the group to use.</param>

    procedure StartGroup(const AGroup: UnicodeString);

    // <summary>Overridden. Escapes a key or a value.</summary>
    // <param name="AItem">The key or value to escape.</param>
    // <returns>The escaped key or value.</returns>
    // <remarks>
    //   This method ensures that the escaped key or value does not
    //   contain any newline characters, such as the carriage return or
    //   linefeed characters. Furthermore, it escapes the '\',  '=', '['
    //   and ']' characters.
    // </remarks>

    function EscapeItem(const AItem: UnicodeString): UnicodeString; override;
  end;

  // <summary>
  //   Represents the table viewer in the Console which can display text
  //   data as a table.
  // </summary>
  // <remarks>
  //   The table viewer in the Console displays the <link
  //   TSiLogEntry.Data, data of a Log Entry> as a table. This class
  //   takes care of the necessary formatting and escaping required by
  //   the corresponding table viewer in the Console.
  //
  //   You can use the TSiTableViewerContext class for creating custom
  //   log methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom data organized as tables.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiTableViewerContext = class(TSiListViewerContext)
  private
    FLineStart: Boolean;
    class function EscapeCSVEntry(const AEntry: UnicodeString): UnicodeString;
  public

    // <summary>
    //   Creates and initializes a TSiTableViewerContext instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Appends a header to the text data.
    // </summary>
    // <param name="AHeader">The header to append.</param>

    procedure AppendHeader(const AHeader: UnicodeString);

    // <summary>
    //   Overloaded. Adds a string entry to the current row.
    // </summary>
    // <param name="AEntry">The string entry to add.</param>

    procedure AddRowEntry(const AEntry: UnicodeString); overload;

    // <summary>
    //   Overloaded. Adds a boolean entry to the current row.
    // </summary>
    // <param name="AEntry">The boolean entry to add.</param>

    procedure AddRowEntry(const AEntry: Boolean); overload;

    // <summary>
    //   Overloaded. Adds an integer entry to the current row.
    // </summary>
    // <param name="AEntry">The integer entry to add.</param>

    procedure AddRowEntry(const AEntry: Integer); overload;

    // <summary>
    //   Overloaded. Adds an int64 entry to the current row.
    // </summary>
    // <param name="AEntry">The int64 entry to add.</param>

    procedure AddRowEntry(const AEntry: Int64); overload;

    // <summary>
    //   Overloaded. Adds a cardinal entry to the current row.
    // </summary>
    // <param name="AEntry">The cardinal entry to add.</param>

    procedure AddRowEntry(const AEntry: Cardinal); overload;

    // <summary>
    //   Overloaded. Adds a TDateTime entry to the current row.
    // </summary>
    // <param name="AEntry">The TDateTime entry to add.</param>

    procedure AddRowEntry(const AEntry: TDateTime); overload;

    // <summary>
    //   Overloaded. Adds an extended entry to the current row.
    // </summary>
    // <param name="AEntry">The extended entry to add.</param>

    procedure AddRowEntry(const AEntry: Extended); overload;

    // <summary>
    //   Begins a new row in the table.
    // </summary>

    procedure BeginRow;

    // <summary>
    //   Ends the current row of the table.
    // </summary>

    procedure EndRow;
  end;

  // <summary>
  //   Represents the web viewer in the Console which can display HTML
  //   text content as web pages.
  // </summary>
  // <remarks>
  //   The web viewer in the Console interprets the <link TSiLogEntry.Data,
  //   data of a Log Entry> as an HTML website.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiWebViewerContext = class(TSiTextContext)
  public

    // <summary>
    //   Creates and initializes a TSiWebViewerContext instance.
    // </summary>

    constructor Create; reintroduce;
  end;

  // <summary>
  //   Represents the source viewer in the Console which can display text
  //   data as source code with syntax highlighting.
  // </summary>
  // <remarks>
  //   The source viewer in the Console interprets the <link TSiLogEntry.Data,
  //   data of a Log Entry> as source code and displays it in a read-only
  //   text editor with syntax highlighting.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiSourceViewerContext = class(TSiTextContext)
  public

    // <summary>
    //   Creates and initializes a TSiSourceViewerContext instance.
    // </summary>
    // <param name="ASourceId">The source ID to use.</param>

    constructor Create(const ASourceId: TSiSourceId); reintroduce;
  end;

  // <summary>
  //   Represents the graphic viewer in the Console which can display
  //   images.
  // </summary>
  // <remarks>
  //   The graphic viewer in the Console interprets the
  //   <link TSiLogEntry.Data, data of a Log Entry> as picture.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiGraphicViewerContext = class(TSiBinaryContext)
  public

    // <summary>
    //   Creates and initializes a TSiGraphicViewerContext instance.
    // </summary>
    // <param name="AGraphicId">The graphic ID to use.</param>

    constructor Create(const AGraphicId: TSiGraphicId); reintroduce;
  end;

  // <summary>
  //   Represents the binary viewer in the Console which can display binary
  //   data in a read-only hex editor.
  // </summary>
  // <remarks>
  //   The binary viewer in the Console interprets the <link TSiLogEntry.Data,
  //   data of a Log Entry> as binary data and displays it in a read-only
  //   hex editor.
  //
  //   You can use the TSiBinaryViewerContext class for creating custom log
  //   methods around <link TSiSession.LogCustomContext, LogCustomContext>
  //   for sending custom binary data.
  // </remarks>
  // <threadsafety>
  //   This class is not guaranteed to be threadsafe.
  // </threadsafety>

  TSiBinaryViewerContext = class(TSiBinaryContext)
  public

    // <summary>
    //   Creates and initializes a TSiBinaryViewerContext instance.
    // </summary>

    constructor Create; reintroduce;
  end;

  TSiSession = class;
  TSiSessionManager = class;
  TSmartInspect = class;

  // <summary>
  //   Used in the TrackMethod methods of the TSiSession class.
  // </summary>
  // <remarks>
  //   For an explanation of this interface please refer to the
  //   documentation of the TSiSession.TrackMethod method.
  // </remarks>

  ISiMethodTracker = interface
  end;

  // <summary>
  //   Used in the TrackMethod methods of the TSiSession class.
  // </summary>
  // <remarks>
  //   This class does nothing more than to call the
  //   TSiSession.EnterMethod method in the constructor Create and
  //   the TSiSession.LeaveMethod method in the destructor Destroy.
  //
  //   For more information about this class please refer to the
  //   documentation of the TSiSession.TrackMethod method.
  // </remarks>

  TSiMethodTracker = class(TInterfacedObject, ISiMethodTracker)
  private
    FLevel: TSiLevel;
    FMethodName: UnicodeString;
    FSession: TSiSession;
  public

    // <summary>
    //   Initializes a new instance of the TSiMethodTracker class and
    //   calls the EnterMethod method of the supplied session.
    // </summary>
    // <param name="ALevel">
    //   The log level for the EnterMethod and LeaveMethod calls.
    // <param name="ASession">
    //   The session to use to track the method.
    // </param>
    // <param name="AMethodName">The name of the method to track.</param>

    constructor Create(const ALevel: TSiLevel; const ASession: TSiSession;
      const AMethodName: UnicodeString);

    // <summary>
    //   Overridden. Releases all resources and calls the LeaveMethod
    //   method of the underlying session.
    // </summary>

    destructor Destroy; override;
  end;

  // <summary>
  //   This is the event handler type for the TSmartInspect.OnFilter
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="APacket">
  //   The packet which caused the event and is about to be processed.
  // </param>
  // <param name="ACancel">
  //   Specifies if the sending of the supplied packet should be
  //   canceled or not.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, the APacket and ACancel
  //   arguments will be passed to the event handlers which offer the
  //   possibility of retrieving information about the packet and
  //   canceling its processing.
  // </remarks>

  TSiFilterEvent = procedure (ASender: TSmartInspect;
    APacket: TSiPacket; var ACancel: Boolean) of object;

  // <summary>
  //   This is the event handler type for the TSmartInspect.OnProcessFlow
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AProcessFlow">
  //   The packet which caused the event and is about to be sent.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, a TSiProcessFlow argument
  //   will be passed to the event handlers which offers the possibility
  //   of retrieving information about the sent packet.
  // </remarks>

  TSiProcessFlowEvent = procedure (ASender: TSmartInspect;
    AProcessFlow: TSiProcessFlow) of object;

  // <summary>
  //   This is the event handler type for the TSmartInspect.OnError
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AException">The occurred exception.</param>
  // <remarks>
  //   In addition to the ASender parameter, an Exception
  //   argument will be passed to the event handlers which offers the
  //   possibility of retrieving information about the occurred error.
  // </remarks>

  TSiErrorEvent = procedure (ASender: TSmartInspect;
    AException: Exception) of object;

  // <summary>
  //   This is the event handler type for the TSmartInspect.OnWatch
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AWatch">
  //   The packet which caused the event and is about to be sent.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, a TSiWatch argument
  //   will be passed to the event handlers which offers the possibility
  //   of retrieving information about the sent packet.
  // </remarks>

  TSiWatchEvent = procedure (ASender: TSmartInspect;
    AWatch: TSiWatch) of object;

  // <summary>
  //   This is the event handler type for the TSmartInspect.OnLogEntry
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="ALogEntry">
  //   The packet which caused the event and is about to be sent.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, a TSiLogEntry argument
  //   will be passed to the event handlers which offers the possibility
  //   of retrieving information about the sent packet.
  // </remarks>

  TSiLogEntryEvent = procedure (ASender: TSmartInspect;
    ALogEntry: TSiLogEntry) of object;

  // <summary>
  //   This is the event type for the TSmartInspect.OnControlCommand
  //   event.
  // </summary>
  // <param name="ASender">The object which fired the event.</param>
  // <param name="AControlCommand">
  //   The packet which caused the event and is about to be sent.
  // </param>
  // <remarks>
  //   In addition to the ASender parameter, a TSiControlCommand argument
  //   will be passed to the event handlers which offers the possibility
  //   of retrieving information about the sent packet.
  // </remarks>

  TSiControlCommandEvent = procedure (ASender: TSmartInspect;
    AControlCommand: TSiControlCommand) of object;

  // <summary>
  //   Specifies the default property values for newly created sessions.
  // </summary>
  // <remarks>
  //   This class is used by the TSmartInspect class to customize the
  //   default property values for newly created sessions. Sessions
  //   that will be created by or passed to the AddSession method of
  //   the TSmartInspect class will be automatically configured with
  //   the values of the TSmartInspect.SessionDefaults property.
  // </remarks>
  // <threadsafety>
  //   This class is guaranteed to be threadsafe.
  // </threadsafety>

  TSiSessionDefaults = class(TObject)
  private
    FLevel: TSiLevel;
    FActive: Boolean;
    FColor: TColor;
  protected
    procedure Assign(const ASession: TSiSession);
  public

    // <summary>
    //   Creates and initializes a new TSiSessionDefaults instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Specifies the default Active property for newly created
    //   sessions.
    // </summary>
    // <remarks>
    //   Please see TSiSession.Active for general information about the
    //   active status of sessions.
    // </remarks>

    property Active: Boolean read FActive write FActive;

    // <summary>
    //   Specifies the default Color property for newly created
    //   sessions.
    // </summary>
    // <remarks>
    //   Please see TSiSession.Color for general information about the
    //   background color of sessions.
    // </remarks>

    property Level: TSiLevel read FLevel write FLevel;

    // <summary>
    //   Specifies the default Level property for newly created
    //   sessions.
    // </summary>
    // <remarks>
    //   Please see TSiSession.Level for general information about the
    //   log level of sessions.
    // </remarks>

    property Color: TColor read FColor write FColor;
  end;

  // <summary>
  //   TSmartInspect is the most important class in the SmartInspect
  //   Delphi library. It is an interface for the protocols, packets
  //   and sessions and is responsible for the error handling.
  // </summary>
  // <remarks>
  //   The TSmartInspect class is the most important class in the
  //   SmartInspect Delphi library. An instance of this class is able
  //   to write log messages to a file or to send them directly
  //   to the SmartInspect Console using TCP. You can control these
  //   connections by setting the Connections property.
  //
  //   The TSmartInspect class offers several properties for controlling
  //   the logging behavior. Besides the Connections property there
  //   is the Enabled property which controls if log messages should be
  //   sent or not. Furthermore, the AppName property specifies the
  //   application name displayed in the SmartInspect Console. And last
  //   but not least, we have the Level and DefaultLevel properties
  //   which specify the log level of an TSmartInspect object and its
  //   related sessions.
  //
  //   Additionally, the TSmartInspect class acts as parent for
  //   sessions, which contain the actual logging methods, like, for
  //   example, TSiSession.LogMessage or TSiSession.LogObject. It is
  //   possible and common that several different sessions have the same
  //   parent and thus share the same connections. The TSiSession class
  //   contains dozens of useful methods for logging any kind of data.
  //   Sessions can even log variable watches, generate illustrated process
  //   and thread information or control the behavior of the SmartInspect
  //   Console. It is possible, for example, to clear the entire log in
  //   the Console by calling the TSiSession.ClearLog method.
  //
  //   To accomplish these different tasks the SmartInspect concept uses
  //   several different packets. The TSmartInspect class manages these
  //   packets and logs them to its connections. It is possibility to
  //   register event handlers for every packet type which are called
  //   after a corresponding packet has been sent. Additionally, you can
  //   filter out packets when using the OnFilter event.
  //
  //   The error handling in the SmartInspect Delphi library is a
  //   bit different than in other libraries. This library uses an event,
  //   the OnError event, for reporting errors. This way was chosen because
  //   a logging framework should not alter the exception behavior of an
  //   application. The only exceptions you need to handle are thrown by
  //   the Connections property if the <link TSmartInspect.Connections,
  //   connections string> contains errors.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSmartInspect = class(TObject)
  private
    FIsMultiThreaded: Boolean;
    FProtocols: TObjectList;
    FSessions: TSiSessionManager;
    FCriticalSection: TCriticalSection;
    FEventLock: TCriticalSection;
    FConnections: UnicodeString;
    FEnabled: Boolean;
    FAppName: UnicodeString;
    FHostname: UnicodeString;
    FOnControlCommand: TSiControlCommandEvent;
    FOnLogEntry: TSiLogEntryEvent;
    FOnWatch: TSiWatchEvent;
    FOnError: TSiErrorEvent;
    FOnProcessFlow: TSiProcessFlowEvent;
    FOnFilter: TSiFilterEvent;
    FDefaultLevel: TSiLevel;
    FLevel: TSiLevel;
    FResolution: TSiClockResolution;
    FVariables: TSiProtocolVariables;
    procedure ProtocolError(ASender: TSiProtocol; AException: Exception);
    procedure ApplyConfiguration(const AConfig: TSiConfiguration);
    procedure Enable;
    procedure Disable;
    procedure AddConnection(ASender: TSiConnectionsParser;
      AProtocol, AOptions: UnicodeString);
    function TryConnections(const AConnections: UnicodeString): Boolean;
    procedure ApplyConnections(const AConnections: UnicodeString);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetConnections(const AValue: UnicodeString);
    function GetOnLogEntry: TSiLogEntryEvent;
    procedure SetOnLogEntry(const AValue: TSiLogEntryEvent);
    function GetOnWatch: TSiWatchEvent;
    procedure SetOnWatch(const AValue: TSiWatchEvent);
    function GetOnControlCommand: TSiControlCommandEvent;
    procedure SetOnControlCommand(const AValue: TSiControlCommandEvent);
    function GetOnError: TSiErrorEvent;
    procedure SetOnError(const AValue: TSiErrorEvent);
    function GetOnProcessFlow: TSiProcessFlowEvent;
    procedure SetOnProcessFlow(const AValue: TSiProcessFlowEvent);
    procedure Connect;
    procedure Disconnect;
    procedure CreateConnections(const AConnections: UnicodeString);
    procedure RemoveConnections;
    class function ReadConnections(const AFileName: UnicodeString;
      var AConnections: UnicodeString): Boolean;
    procedure ProcessPacket(const APacket: TSiPacket);
    function GetOnFilter: TSiFilterEvent;
    procedure SetOnFilter(const AValue: TSiFilterEvent);
    function FindProtocol(const ACaption: UnicodeString): TSiProtocol;
    function GetSessionDefaults: TSiSessionDefaults;
    function GetAppName: UnicodeString;
    procedure SetAppName(const AValue: UnicodeString);
    procedure UpdateProtocols;
  protected

    // <summary>
    //   Invokes the TSmartInspect.OnFilter event handler and determines
    //   if the supplied packet should be sent or not.
    // </summary>
    // <param name="APacket">
    //   The packet which is about to be processed.
    // </param>
    // <returns>
    //   True if the supplied packet shall be filtered and thus not be
    //   sent and False otherwise.
    // </returns>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnFilter event. This method is always called in
    //   an exception-safe context.
    // </remarks>

    function DoFilter(const APacket: TSiPacket): Boolean; virtual;

    // <summary>
    //   Invokes the TSmartInspect.OnError event handlers.
    // </summary>
    // <param name="AException">The occurred exception.</param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnError event.
    // </remarks>

    procedure DoError(const AException: Exception); virtual;

    // <summary>
    //   Invokes the TSmartInspect.OnProcessFlow event handlers.
    // </summary>
    // <param name="AProcessFlow">
    //   The Process Flow entry which has just been processed.
    // </param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnProcessFlow event. This event is always called
    //   in an exception-safe context.
    // </remarks>

    procedure DoProcessFlow(const AProcessFlow: TSiProcessFlow); virtual;

    // <summary>
    //   Invokes the TSmartInspect.OnWatch event handlers.
    // </summary>
    // <param name="AWatch">
    //   The Watch which has just been processed.
    // </param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnWatch event. This event is always called in an
    //   exception-safe context.
    // </remarks>

    procedure DoWatch(const AWatch: TSiWatch); virtual;

    // <summary>
    //   Invokes the TSmartInspect.OnLogEntry event handlers.
    // </summary>
    // <param name="ALogEntry">
    //   The Log Entry which has just been processed.
    // </param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnLogEntry event. This event is always called
    //   in an exception-safe context.
    // </remarks>

    procedure DoLogEntry(const ALogEntry: TSiLogEntry); virtual;

    // <summary>
    //   Invokes the TSmartInspect.OnControlCommand event handlers.
    // </summary>
    // <param name="AControlCommand">
    //   The Control Command which has just been processed.
    // </param>
    // <remarks>
    //   Derived classes can override this method to intercept the
    //   TSmartInspect.OnControlCommand event. This method is always
    //   called in an exception-safe context.
    // </remarks>

    procedure DoControlCommand(const AControlCommand: TSiControlCommand);
      virtual;

    // <summary>
    //   Updates an entry in the internal lookup table of sessions.
    // </summary>
    // <param name="ASession">
    //   The session whose name has changed and whose entry should be
    //   updated.
    // </param>
    // <param name="ATo">The new name of the session.</param>
    // <param name="AFrom">The old name of the session.</param>
    // <remarks>
    //   Once the name of a session has changed, this method is called
    //   to update the internal session lookup table. The 'ATo' argument
    //   specifies the new name and 'AFrom' the old name of the session.
    //   After this method returns, the new name can be passed to the
    //   GetSession method to lookup the supplied session.
    // </remarks>

    procedure UpdateSession(const ASession: TSiSession;
      const ATo, AFrom: UnicodeString);
  public

    // <summary>
    //   Creates and initializes a new instance of the TSmartInspect class.
    // </summary>
    // <param name="AAppName">
    //   The application name used for Log Entries. It is usually
    //   set to the name of the application which creates this object.
    // </param>

    constructor Create(const AAppName: UnicodeString);

    // <summary>
    //   Overridden. Releases all resources.
    // </summary>
    // <remarks>
    //   This destructor disconnects all connections, frees the internal
    //   lists and all synchronizers. In particular, all <link TSiSession,
    //   sessions> created with the AddSession method will also be freed.
    // </remarks>

    destructor Destroy; override;

    // <summary>
    //   Returns the version number of the SmartInspect Delphi library.
    // </summary>
    // <returns>The current version number.</returns>
    // <remarks>
    //   This class function returns the current version number of the
    //   SmartInspect Delphi library. The returned string always has the
    //   form "MAJOR.MINOR.RELEASE.BUILD".
    // </remarks>

    class function Version: UnicodeString;

    // <summary>
    //   Executes a custom protocol action of a connection.
    // </summary>
    // <param name="ACaption">
    //   The identifier of the connection.
    // </param>
    // <param name="AAction">
    //   The action to execute by the requested connection.
    // </param>
    // <param name="AState">
    //   An optional object which encapsulates additional protocol
    //   specific information about the custom action. Can be nil.
    // </param>
    // <seealso cref="TSiProtocol.Dispatch"/>
    // <seealso cref="TSiProtocol.IsValidOption"/>
    // <remarks>
    //   This method dispatches the AAction and AState parameters to the
    //   connection identified by the ACaption argument. If no suitable
    //   connection can be found, the OnError event is used. The OnError
    //   event is also used if an exception is thrown in the custom
    //   protocol action.
    //
    //   The SmartInspect Delphi library currently implements one custom
    //   protocol action in TSiMemoryProtocol. The TSiMemoryProtocol
    //   class is used for writing log packets to memory. On request, it
    //   can write its internal queue of packets to a user-supplied stream
    //   or TSiProtocol object with a custom protocol action.
    //
    //   The request for executing the custom action and writing the queue
    //   can be initiated with this Dispatch method. Please see the example
    //   section below for details.
    //
    //   For more information about custom protocol actions, please refer
    //   to the TSiProtocol.Dispatch method. Also have a look at the
    //   TSiProtocol.IsValidOption method which explains how to set the
    //   caption of a connection.
    //
    //   Please note that the custom protocol action is executed
    //   asynchronously if the requested connection operates in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>.
    // </remarks>
    // <example>
    // <code>
    // { Set the connections string and enable logging. We do not
    //   specify a caption for the memory connection and stick with
    //   the default. By default, the caption of a connection is set
    //   to the name of the protocol, in our case 'mem'. }
    // Si.Connections := 'mem()';
    // Si.Enabled := True;
    //
    // ...
    //
    // { Instrument your application with log statements as usual. }
    // SiMain.LogMessage('This is a message');
    // SiMain.LogMessage('This is a message');
    //
    // ...
    //
    // { Then, in case of an unexpected event, for example, in a
    //   global exception handler, you can write the entire queue
    //   of packets of your memory protocol connection to a file
    //   by using the Dispatch method. }
    // LStream := TFileStream.Create('log.sil', fmCreate);
    // try
    //   Si.Dispatch('mem', 0, LStream);
    // finally
    //   LStream.Free;
    // end;
    // </code>
    //
    // <code>
    // ...
    //
    // { Alternative Dispatch call with a Protocol object which sends
    //   the queue content to a local Console via a named pipe. }
    // LProtocol := TSiPipeProtocol.Create;
    // try
    //   { Optionally set some protocol options }
    //   // LProtocol.Initialize(..)
    //   LProtocol.Connect;
    //   try
    //     Si.Dispatch('mem', 0, LProtocol);
    //   finally
    //     LProtocol.Disconnect;
    //   end;
    // finally
    //   LProtocol.Free;
    // end;
    // </code>
    // </example>

    procedure Dispatch(const ACaption: UnicodeString; const AAction: Integer;
      const AState: TObject); reintroduce;

    // <summary>
    //   Adds a new or updates an existing connection variable.
    // </summary>
    // <param name="AKey">
    //   The key of the connection variable.
    // </param>
    // <param name="AValue">
    //   The value of the connection variable.
    // </param>
    // <remarks>
    //   This method sets the value of a given connection variable.
    //   A connection variable is a placeholder for strings in the
    //   <link TSmartInspect.Connections, connections string>. When
    //   setting a connections string (or loading it from a file
    //   with LoadConfiguration), any variables which have previously
    //   been defined with SetVariable are automatically replaced
    //   with their respective values.
    //
    //   The variables in the connections string are expected to
    //   have the following form: $variable$.
    //
    //   If a connection variable with the given key already exists,
    //   its value is overridden. To delete a connection variable,
    //   use UnsetVariable.
    //
    //   Connection variables are especially useful if you load a
    //   connections string from a file and would like to handle
    //   some protocol options in your application instead of the
    //   configuration file.
    //
    //   For example, if you encrypt log files, you probably do not
    //   want to specify the encryption key directly in your
    //   configuration file for security reasons. With connection
    //   variables, you can define a variable for the encryption
    //   key with SetVariable and then reference this variable in
    //   your configuration file. The variable is then automatically
    //   replaced with the defined value when loading the
    //   configuration file.
    //
    //   Another example deals with the directory or path of a log
    //   file. If you include a variable in the path of your log
    //   file, you can later replace it in your application with
    //   the real value. This might come in handy if you want to
    //   write a log file to an environment specific value, such
    //   as an application data directory, for example.
    // </remarks>
    // <example>
    // <code>
    // // Define the variable 'key' with the value 'secret'
    // Si.SetVariable('key', 'secret');
    //
    // ...
    //
    // // And include the variable $key$ in the related connections
    // // string (the connections string can either be set directly
    // // or loaded from a file).
    // file(encrypt="true", key="$key$")
    // </code>
    // </example>

    procedure SetVariable(const AKey, AValue: UnicodeString);

    // <summary>
    //   Returns the value of a connection variable.
    // </summary>
    // <param name="AKey">
    //   The key of the connection variable.
    // </param>
    // <returns>
    //   The value for the given connection variable or an empty
    //   string if the connection variable is unknown.
    // </returns>
    // <remarks>
    //   Please see the SetVariable method for more information
    //   about connection variables.
    // </remarks>

    function GetVariable(const AKey: UnicodeString): UnicodeString;

    // <summary>
    //   Unsets an existing connection variable.
    // </summary>
    // <param name="AKey">
    //   The key of the connection variable to delete.
    // </param>
    // <remarks>
    //   This method deletes the connection variable specified by the
    //   given key. Nothing happens if the connection variable doesn't
    //   exist.
    // </remarks>

    procedure UnsetVariable(const AKey: UnicodeString);

    // <summary>
    //   Overloaded. Adds and returns a new TSiSession instance with this
    //   TSmartInspect object set as parent and optionally saves it for
    //   later access.
    // </summary>
    // <param name="ASessionName">The name for the new session.</param>
    // <param name="AStore">
    //   Indicates if the newly created session should be stored for
    //   later access. This parameter is optional and defaults to False.
    // </param>
    // <returns>The new TSiSession instance.</returns>
    // <remarks>
    //   This method allocates a new session with this TSmartInspect
    //   instance set as parent and the supplied ASessionName parameter
    //   set as session name. The returned session will be configured
    //   with the default session properties as specified by the
    //   SessionDefaults property. This default configuration can be
    //   overridden on a per-session basis by loading the session
    //   configuration with the LoadConfiguration method. Please see
    //   the LoadConfiguration documentation for details.
    //
    //   If the 'AStore' parameter is true, the created and returned
    //   session is stored for later access and can be retrieved with the
    //   GetSession method. To remove a created session from the internal
    //   list, call the DeleteSession method. The AStore parameter is
    //   optional and defaults to False.
    //
    //   Regardless of the AStore parameter, there's no need to free the
    //   returned session by yourself. Sessions added with the AddSession
    //   method will be freed automatically when the TSmartInspect object
    //   is destroyed. To free a session manually, you can use the
    //   DeleteSession method, which frees the instance and removes it
    //   from the session list. You should never free such an instance
    //   by yourself.
    //
    //   If this method is called multiple times with the same session
    //   name, then the GetSession method operates on the session which
    //   got added last.
    // </remarks>

    function AddSession(const ASessionName: UnicodeString;
      const AStore: Boolean = False): TSiSession; overload;

    // <summary>
    //   Overloaded. Adds an existing TSiSession instance to the internal
    //   list of sessions and saves it for later access.
    // </summary>
    // <param name="ASession">The session to store.</param>
    // <remarks>
    //   This method adds the passed session to the internal list of
    //   sessions and saves it for later access. The passed session
    //   will be configured with the default session properties as
    //   specified by the SessionDefaults property. This default
    //   configuration can be overridden on a per-session basis by
    //   loading the session configuration with the LoadConfiguration
    //   method. Please see the LoadConfiguration documentation for
    //   details.
    //
    //   The passed session can later be retrieved with the GetSession
    //   method. To remove an added session from the internal list, call
    //   the DeleteSession method.
    //
    //   Sessions added with the AddSession method will be freed
    //   automatically when the TSmartInspect object is destroyed. To
    //   free a session manually, you can use the DeleteSession method,
    //   which frees the instance and removes it from the session list.
    //   You should never free an added session by yourself.
    // </remarks>

    procedure AddSession(const ASession: TSiSession); overload;

    // <summary>
    //   Frees a session and removes it from the internal session list.
    // </summary>
    // <param name="ASession">
    //   The session to free and remove from the internal list of sessions.
    // </param>
    // <remarks>
    //   TSiSession instances, created with the AddSession method, should
    //   only be manually freed using this DeleteSession method. It frees
    //   the TSiSession instance correctly and removes it from the internal
    //   session list.
    //
    //   After this method returns, the GetSession method returns nil when
    //   called with the same session name unless a different session with
    //   the same name has been added.
    // </remarks>

    procedure DeleteSession(const ASession: TSiSession);

    // <summary>
    //   Returns a previously added session.
    // </summary>
    // <param name="ASessionName">
    //   The name of the session to lookup and return.
    // </param>
    // <returns>
    //   The requested session or nil if the session is unknown.
    // </returns>
    // <remarks>
    //   This method returns a session which has previously been added
    //   with the AddSession method and can be identified by the supplied
    //   ASessionName argument. If the requested session is unknown, this
    //   method returns nil.
    //
    //   Note that the behavior of this method can be unexpected in terms
    //   of the result value if multiple sessions with the same name have
    //   been added. In this case, this method returns the session which
    //   got added last and not necessarily the session which you expect.
    //
    //   Adding multiple sessions with the same name should therefore be
    //   avoided.
    // </remarks>

    function GetSession(const ASessionName: UnicodeString): TSiSession;

    // <summary>
    //   Gets the session associated with the specified session name.
    // </summary>
    // <param name="ASessionName">
    //   The name of the session to lookup and return.
    // </param>
    // <returns>
    //   The requested session or nil if the session is unknown.
    // </returns>
    // <remarks>
    //   This property returns the session which has previously been
    //   added with the AddSession method and can be identified by the
    //   specified session name. If the specified session is unknown
    //   nil is returned. See the GetSession method for more information.
    // </remarks>

    property Sessions[const ASessionName: UnicodeString]: TSiSession read
      GetSession; default;

    // <summary>
    //   Loads the connections string of this object from a file.
    // </summary>
    // <param name="AFileName">
    //   The name of the file to load the connections string from.
    // </param>
    // <param name="ADoNotEnable">
    //   Specifies if this instance shouldn't be enabled automatically.
    //   The default value is False.
    // </param>
    // <seealso cref="ESiLoadConnectionsError"/>
    // <seealso cref="ESiInvalidConnectionsError"/>
    // <remarks>
    //   This method loads the <link TSmartInspect.Connections,
    //   connections string> from a file. This file should be a plain
    //   text file containing a line like in the following example:
    //
    //   <code>connections=file(filename=c:\\log.sil)</code>
    //
    //   Empty or unrecognized lines are ignored. This method enables
    //   logging automatically unless the ADoNotEnable parameter is
    //   true. Please note that the ADoNotEnable parameter has no effect
    //   if this TSmartInspect instance is already enabled.
    //
    //   The OnError event is used to notify the parent application if
    //   the specified file cannot be opened or does not contain a
    //   <link TSmartInspect.Connections, connections string>. The
    //   <link TSmartInspect.Connections, connections string> and the
    //   <link TSmartInspect.Enabled, enabled status> of this instance
    //   are not changed if such an error occurs.
    //
    //   The OnError event is also used if a connections string could be
    //   read but is found to be invalid. In this case, an instance of
    //   the ESiInvalidConnectionsError exception type is passed to
    //   the OnError event.
    //
    //   This method is useful for customizing the connections string
    //   after the deployment of an application. A typical use case for
    //   this method is the following scenario: imagine a customer who
    //   needs to send a log file to customer service to analyse a software
    //   problem. If the software in question uses this LoadConnections
    //   method, the customer service just needs to send a prepared
    //   connections file to the customer. To enable the logging, the
    //   customer now just needs to drop this file to the application's
    //   installation directory or any other predefined location.
    //
    //   See LoadConfiguration for a method which is not limited to
    //   loading the connections string, but is also capable of loading
    //   any other property of this object from a file.
    //
    //   The LoadConnections and LoadConfiguration methods are both
    //   capable of detecting the string encoding of the connections and
    //   configuration files. Please see the LoadConfiguration method for
    //   details.
    //
    //   To automatically replace placeholders in a loaded connections
    //   string, you can use so called connection variables. Please
    //   have a look at the SetVariable method for more information.
    // </remarks>

    procedure LoadConnections(const AFileName: UnicodeString;
      const ADoNotEnable: Boolean = False);

    // <summary>
    //   Loads the properties and sessions of this TSmartInspect instance
    //   from a file.
    // </summary>
    // <param name="AFileName">
    //   The name of the file to load the configuration from.
    // </param>
    // <seealso cref="TSiConfigurationTimer"/>
    // <seealso cref="ESiLoadConfigurationError"/>
    // <seealso cref="ESiInvalidConnectionsError"/>
    // <remarks>
    //   This method loads the properties and sessions of this
    //   TSmartInspect object from a file. This file should be a plain
    //   text file containing key/value pairs. Each key/value pair is
    //   expected to be on its own line. Empty, unrecognized lines and
    //   lines beginning with a ';' character are ignored.
    //
    //   The OnError event is used to notify the application if an error
    //   occurs while trying to load the configuration from the specified
    //   file. Such errors include I/O errors like trying to open a file
    //   which does not exist, for example.
    //
    //   The OnError event is also used if the specified configuraton
    //   file contains an invalid connections string. In this case, an
    //   instance of the ESiInvalidConnectionsError exception type is
    //   passed to the OnError event.
    //
    //   This method is useful for loading the properties of this
    //   TSmartInspect instance after the deployment of an application.
    //   A typical use case for this method is the following scenario:
    //   imagine a customer who needs to send a log file to customer
    //   service to analyse a software problem. If the software in
    //   question uses this LoadConfiguration method, the customer
    //   service just needs to send a prepared configuration file to
    //   the customer. Now, to load the TSmartInspect properties from
    //   a file, the customer now just needs to drop this file to the
    //   application's installation directory or any other predefined
    //   location.
    //
    //   To monitor a SmartInspect configuration file for changes,
    //   please have a look at the TSiConfigurationTimer class.
    //
    //   To automatically replace placeholders in a loaded connections
    //   string, you can use so called connection variables. Please
    //   have a look at the SetVariable method for more information.
    //
    //   The following table lists the recognized configuration values,
    //   the corresponding TSmartInspect properties and their types:
    //
    //   <table>
    //   Value          Property       Type
    //   -              -              -
    //   appname        AppName        String
    //   connections    Connections    String
    //   defaultlevel   DefaultLevel   TSiLevel
    //   enabled        Enabled        Boolean
    //   level          Level          TSiLevel
    //   </table>
    //
    //   In addition to these properties, this method also configures
    //   any stored sessions of this TSmartInspect object. Sessions that
    //   have been stored or will be added with the AddSession method
    //   will be configured with the properties of the related session
    //   entry of the passed configuration file. Please see the
    //   example section for details on how sessions entries look
    //   like.
    //
    //   If no entries can be found in the configuration file for a
    //   newly added session, this session will use the default session
    //   properties. The default session properties can also be
    //   specified in the configuration file. Please note that the
    //   session defaults do not apply to the main session SiMain
    //   since this session has already been added before a
    //   configuration file can be loaded. The session defaults only
    //   apply to newly added sessions and do not affect existing
    //   sessions.
    //
    //   The case of the configuration properties doesn't matter. This
    //   means, it makes no difference if you specify 'defaultlevel' or
    //   'DefaultLevel' as key, for example.
    //
    //   For a typical configuration file, please see the example below.
    //
    //   To support Unicode strings, both the LoadConnections and
    //   LoadConfiguration methods are capable of auto-detecting the
    //   string encoding when a BOM (Byte Order Mark) is given at the
    //   start of the file. The following table lists the supported
    //   encodings and the corresponding BOM identifiers.
    //
    //   <table>
    //   Encoding                BOM identifier
    //   -                       -
    //   UTF8                    0xEF, 0xBB, 0xBF
    //   Unicode                 0xFF, 0xFE
    //   Unicode big-endian      0xFE, 0xFF
    //   </table>
    //
    //   If no BOM is given, the text is assumed to be in the ANSI
    //   format. If the configuration file has been created or edited
    //   with the SmartInspect Configuration Builder, the file always
    //   has a UTF8 Byte Order Mark and Unicode strings are therefore
    //   handled automatically.
    // </remarks>
    // <example>
    // <code>
    // ; Specify the SmartInspect properties
    // connections = file(filename=c:\\log.sil)
    // enabled = true
    // level = verbose
    // defaultlevel = message
    // appname = client
    //
    // ; Then set the defaults for new sessions
    // sessiondefaults.active = false
    // sessiondefaults.level = message
    // sessiondefaults.color = 0xffff7f
    //
    // ; And finally configure some individual sessions
    //  session.main.level = verbose
    // session.client.active = true
    // session.client.color = 0x7fffff
    // </code>
    // </example>

    procedure LoadConfiguration(const AFileName: UnicodeString);

    // <summary>Logs a Log Entry packet.</summary>
    // <param name="ALogEntry">The Log Entry to log.</param>
    // <remarks>
    //   After setting the application name and hostname of the
    //   supplied Log Entry, this method determines if the Log Entry
    //   should really be sent by invoking the DoFilter method. If
    //   the Log Entry passes the filter test, it will be logged and
    //   the TSmartInspect.OnLogEntry event is fired.
    // </remarks>

    procedure SendLogEntry(const ALogEntry: TSiLogEntry);

    // <summary>Logs a Control Command packet.</summary>
    // <param name="AControlCommand">The Control Command to log.</param>
    // <remarks>
    //   At first, this method determines if the Control Command should
    //   really be sent by invoking the DoFilter method. If the Control
    //   Command passes the filter test, it will be logged and the
    //   TSmartInspect.OnControlCommand event is fired.
    // </remarks>

    procedure SendControlCommand(const AControlCommand: TSiControlCommand);

    // <summary>Logs a Process Flow entry packet.</summary>
    // <param name="AProcessFlow">The Process Flow entry to log.</param>
    // <remarks>
    //   After setting the hostname of the supplied Process Flow entry,
    //   this method determines if the Process Flow entry should really
    //   be sent by invoking the DoFilter method. If the Process Flow
    //   entry passes the filter test, it will be logged and the
    //   TSmartInspect.OnProcessFlow event is fired.
    // </remarks>

    procedure SendProcessFlow(const AProcessFlow: TSiProcessFlow);

    // <summary>Logs a Watch packet.</summary>
    // <param name="AWatch">The Watch to log.</param>
    // <remarks>
    //   At first, this method determines if the Watch should really be
    //   sent by invoking the DoFilter method. If the Watch passes the
    //  filter test, it will be logged and the TSmartInspect.OnWatch
    //   event is fired.
    // </remarks>

    procedure SendWatch(const AWatch: TSiWatch);

    // <summary>
    //   Returns the current date and time, optionally with a high
    //   resolution.
    // </summary>
    // <returns>The current date and time as TDateTime value.</returns>
    // <remarks>
    //   If the Resolution property specifies using a high resolution
    //   for timestamps, this method tries to return a timestamp with a
    //   microsecond resolution.
    //
    //   High-resolution timestamps are only available if the
    //   QueryPerformanceCounter and QueryPerformanceFrequency functions
    //   indicate a successfully working high-resolution performance
    //   counter. Please see the Windows Platform SDK documentation for
    //   details.
    //
    //   If high-resolution support is not available, this method simply
    //   returns SysUtils.Now.
    // </remarks>

    function Now: TDateTime; virtual;

    // <summary>
    //   Represents the hostname of the sending machine.
    // </summary>
    // <seealso cref="TSiLogEntry"/>
    // <remarks>
    //   This read-only property returns the hostname of the current
    //   machine. The hostname helps you to identify Log Entries from
    //   different machines in the SmartInspect Console.
    // </remarks>

    property HostName: UnicodeString read FHostName;

    // <summary>
    //   Represents the application name used for the Log Entries.
    // </summary>
    // <seealso cref="TSiLogEntry"/>
    // <remarks>
    //   The application name helps you to identify Log Entries from
    //   different applications in the SmartInspect Console.
    // </remarks>

    property AppName: UnicodeString read GetAppName write SetAppName;

    // <summary>
    //   This property allows you to control if anything should be
    //   logged at all.
    // </summary>
    // <remarks>
    //   If you set this property to true, all connections will try to
    //   connect to their destinations. For example, if the Connections
    //   property is set to "file(filename=c:\\log.sil)", the file
    //   "c:\\log.sil" will be opened to write all following packets to
    //   it. By setting this property to false, all connections will
    //   disconnect.
    //
    //   Additionally, every TSiSession method evaluates if its parent
    //   is enabled and returns immediately if this is not the case.
    //   This guarantees that the performance loss is minimal when
    //   logging is disabled. The default value of this property is
    //   false. You need to set this property to true before you can
    //   use the TSmartInspect instance and its related sessions.
    //
    //   <b>Please note:</b> If one or more connections of this
    //   TSmartInspect object operate in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>,
    //   disabling this instance may block until the related protocol
    //   threads are finished.
    // </remarks>

    property Enabled: Boolean read FEnabled write SetEnabled;

    // <summary>
    //   Specifies all connections used by a TSmartInspect instance.
    // </summary>
    // <remarks>
    //   You can set multiple connections by separating the connections
    //   with commas. A connection consists of a protocol identifier
    //   like 'file' plus optional protocol parameters in parentheses.
    //   If you, for example, want to log to a file, the Connections
    //   property must be set to 'file()'. You can specify the filename
    //   in the parentheses after the protocol identifier like this:
    //   'file(filename="c:\\mylogfile.sil")'. Please note that if the
    //   Enabled property is set to true, the connections try to connect
    //   to their destinations immediately. By default, no connections
    //   are used.
    //
    //   See the TSiProtocol class for a list of available protocols and
    //   TSiProtocolFactory for a way to add your own custom protocols.
    //   Also have a look at the LoadConnections and LoadConfiguration
    //   methods, which can load a connections string from a file.
    //   Furthermore, for a class which assists in building connections
    //   strings, please refer to the TSiConnectionsBuilder class.
    //
    //   To automatically replace placeholders in the given connections
    //   string, you can use so called connection variables. Please
    //   have a look at the SetVariable method for more information.
    //
    //   Please note that an ESiInvalidConnectionsError exception is
    //   thrown if an invalid connections string is supplied.
    // </remarks>
    // <exception>
    // <table>
    //   Exception Type                 Condition
    //   -                              -
    //   ESiInvalidConnectionsError     Invalid syntax, unknown protocols
    //                                    or inexistent options.
    // </table>
    // </exception>
    // <example>
    // <code>
    // Si.Connections := '';
    // Si.Connections := 'file()';
    // Si.Connections := 'file(filename="log.sil", append=true)';
    // Si.Connections := 'file(append=true), tcp(host="localhost")';
    // Si.Connections := 'file(), file(filename="anotherlog.sil")';
    // </code>
    // </example>

    property Connections: UnicodeString read FConnections write SetConnections;

    // <summary>
    //   Represents the default log level of this TSmartInspect
    //   instance and its related sessions.
    // </summary>
    // <seealso cref="TSiLevel"/>
    // <seealso cref="TSmartInspect.Level"/>
    // <remarks>
    //   The DefaultLevel property of this TSmartInspect instance
    //   represents the default log level used by its corresponding
    //   sessions. The default value of this property is lvMessage.
    //
    //   Every method in the TSiSession class which makes use of the
    //   parent's <link Level, log level> and does not take a Level
    //   argument, uses the default level of its parent as log level.
    //
    //   For more information on how to use this property, please have
    //   a look at the following example.
    // </remarks>
    // <example>
    // <code>
    // program DefaultLevelExample;
    //
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SiAuto,
    //   SmartInspect;
    //
    // procedure Method;
    // begin
    //   SiMain.EnterMethod('Method');
    //   try
    //     // ..
    //   finally
    //     SiMain.LeaveMethod('Method');
    //   end;
    // end;
    //
    // begin
    //   Si.Enabled := True;
    //   Si.Level := lvDebug;
    //   Si.DefaultLevel := lvVerbose;
    //
    //   // Since the EnterMethod and LeaveMethod calls do not
    //   // specify their log level explicitly (by passing a TSiLevel
    //   // argument), they use the default log level which has just
    //   // been set to Level.Verbose (see above). And since the log
    //   // level of the Si object is set to lvDebug, the EnterMethod
    //   // and LeaveMethod calls will be logged.
    //   Method;
    //
    //   Si.Level := lvMessage; // Switch to lvMessage
    //
    //   // Since EnterMethod and LeaveMethod still use lvVerbose
    //   // as their log level and the log level of the Si object is
    //   // now set to lvMessage, the EnterMethod and LeaveMethod
    //   // calls will be ignored and not be logged.
    //   Method;
    // end.
    // </code>
    // </example>

    property DefaultLevel: TSiLevel read FDefaultLevel write FDefaultLevel;

    // <summary>
    //   Represents the log level of this TSmartInspect instance and its
    //   related sessions.
    // </summary>
    // <seealso cref="TSiLevel"/>
    // <seealso cref="TSmartInspect.DefaultLevel"/>
    // <remarks>
    //   The Level property of this TSmartInspect instance represents
    //   the log level used by its corresponding sessions to determine
    //   if information should be logged or not. The default value of
    //   this property is lvDebug.
    //
    //   Every method (except the Clear method family) in the TSiSession
    //   class tests if its log level equals or is greater than the
    //   log level of its parent. If this is not the case, the methods
    //   return immediately and won't log anything.
    //
    //   The log level for a method in the TSiSession class can either be
    //   specified explicitly by passing a Level argument or implicitly
    //   by using the <link DefaultLevel, default level>. Every method
    //   in the TSiSession class which makes use of the parent's log level
    //   and does not take a Level argument, uses the <link DefaultLevel,
    //   default level> of its parent as log level.
    //
    //   For more information about the default level, please refer to
    //   the documentation of the DefaultLevel property.
    // </remarks>
    // <example>
    // <code>
    // program LevelExample;
    //
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SiAuto,
    //   SmartInspect;
    //
    // procedure Method;
    // begin
    //   SiMain.EnterMethod(lvDebug, 'Method');
    //   try
    //     // ..
    //   finally
    //     SiMain.LeaveMethod(lvDebug, 'Method');
    //   end;
    // end;
    //
    // begin
    //   Si.Enabled := True;
    //
    //   Si.Level := lvDebug;
    //   Method; // Logs EnterMethod and LeaveMethod calls
    //
    //   Si.Level := lvMessage;
    //   Method; // Ignores EnterMethod and LeaveMethod calls
    // end.
    // </code>
    // </example>

    property Level: TSiLevel read FLevel write FLevel;

    // <summary>
    //   Specifies the timestamp resolution mode for this TSmartInspect
    //   object.
    // </summary>
    // <seealso cref="TSiClockResolution"/>
    // <remarks>
    //   By changing this property, you can specify if this object
    //   should try to use high-resolution timestamps for TSiLogEntry,
    //   TSiWatch and TSiProcessFlow packets. High-resolution timestamps
    //   provide a microsecond resolution. Conversely, standard
    //   timestamps have a maximum resolution of 10-55 milliseconds.
    //
    //   High-resolution timestamps are only available if the
    //   QueryPerformanceCounter and QueryPerformanceFrequency functions
    //   indicate a successfully working high-resolution performance
    //   counter. Please see the Windows Platform SDK documentation for
    //   details.
    //
    //   Additionally, <b>high-resolution timestamps are not intended to
    //   be used on production systems</b>. It is recommended to use them
    //   only during development and debugging. High-resolution timestamps
    //   can introduce several problems that are acceptable on development
    //   machines but normally not tolerable on production systems:
    //
    //   <table>
    //   Problem      Description
    //   -            -
    //   Performance  High-resolution timestamps can be a lot slower than
    //                 standard timestamps. This actually depends on the
    //                 concrete implementation of QueryPerformanceCounter
    //                 (i.e. which timer is used for the high-resolution
    //                 performance counter [PIT, PMT, TSC, HPET]), but in
    //                 general one can say that standard timestamps are a
    //                 lot faster to read.
    //
    //   Accuracy     High-resolution timestamps tend to deviate from the
    //                 system timer when seen over a longer period of time.
    //                 Depending on the particular QueryPerformanceCounter
    //                 implementation, it can happen that high-resolution
    //                 timestamps induce an error of milliseconds within a
    //                 few minutes only.
    //
    //   Reliability  Depending on the used timer, QueryPerformanceCounter
    //                 provides unreliable results under certain, not so
    //                 uncommon, circumstances. When the TSC timer is used,
    //                 multi-processor/multi-core systems or processors
    //                 with varying frequencies (like found in most modern
    //                 notebooks or desktop machines) are known to cause
    //                 several problems which make high-resolution
    //                 timestamps unsuitable for production usage.
    //   </table>
    //
    //   Due to the mentioned problems, this property defaults to using
    //   the standard timestamp resolution.
    // </remarks>

    property Resolution: TSiClockResolution read FResolution write
      FResolution;

    // <summary>
    //   Specifies the default property values for new sessions.
    // </summary>
    // <remarks>
    //   This property lets you specify the default property values
    //   for new sessions which will be created by or passed to the
    //   AddSession method. Please see the AddSession method for more
    //   information. For information about the available session
    //   properties, please refer to the documentation of the TSiSession
    //   class.
    // </remarks>

    property SessionDefaults: TSiSessionDefaults read GetSessionDefaults;

    // <summary>
    //   Occurs before a packet is processed. Offers the opportunity to
    //   filter out packets.
    // </summary>
    // <seealso cref="TSiPacket"/>
    // <seealso cref="TSiFilterEvent"/>
    // <remarks>
    //   This event can be used if filtering of certain packets is
    //   needed. The event handlers are always called in the context of
    //   the thread which causes the event.
    //
    //   Please see the example for more information on how to use this
    //   event.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements to the event handlers can cause a presumably undesired
    //   recursive behavior.
    // </remarks>
    // <example>
    // <code>
    // program OnFilter;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnFilter(ASender: TSmartInspect; APacket: TSiPacket;
    //       var ACancel: Boolean);
    //   end;
    //
    // procedure TEventTest.OnFilter(ASender: TSmartInspect;
    //   APacket: TSiPacket; var ACancel: Boolean);
    // begin
    //   if APacket is TSiLogEntry then
    //   begin
    //     ACancel := TSiLogEntry(APacket).Title = 'Cancel Me';
    //   end;
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     Si.Enabled := True;
    //
    //     // Register an event handler for the filter event
    //     Si.OnFilter := EventTest.OnFilter;
    //
    //     // The second message will be ignored and not be logged
    //     SiMain.LogMessage('Message');
    //     SiMain.LogMessage('Cancel Me');
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnFilter: TSiFilterEvent read GetOnFilter write SetOnFilter;

    // <summary>
    //   Occurs when a Log Entry is processed.
    // </summary>
    // <seealso cref="TSiLogEntry"/>
    // <seealso cref="TSiLogEntryEvent"/>
    // <remarks>
    //   You can use this event if custom processing of TSiLogEntry
    //   packets is needed. The event handlers are always called in the
    //   context of the thread which causes the event.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements to the event handlers can cause a presumably undesired
    //   recursive behavior. Also, if you specified that one or more
    //   connections of this TSmartInspect object should operate in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>,
    //   you need to protect the passed TSiLogEntry packet and its data
    //   by calling its <link TSiPacket.Lock, Lock> and
    //   <link TSiPacket.Unlock, Unlock> methods before and after
    //   processing.
    // </remarks>
    // <example>
    // <code>
    // program OnLogEntry;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnLogEntry(ASender: TSmartInspect;
    //       ALogEntry: TSiLogEntry);
    //   end;
    //
    // procedure TEventTest.OnLogEntry(ASender: TSmartInspect;
    //   ALogEntry: TSiLogEntry);
    // begin
    //   WriteLn(Format('%s: %s', [ALogEntry.SessionName, ALogEntry.Title]));
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     Si.OnLogEntry := EventTest.OnLogEntry;
    //     Si.Enabled := True;
    //     SiMain.LogMessage('This is an event test!');
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnLogEntry: TSiLogEntryEvent read GetOnLogEntry write SetOnLogEntry;

    // <summary>
    //   Occurs when a Watch is processed.
    // </summary>
    // <seealso cref="TSiWatch"/>
    // <seealso cref="TSiWatchEvent"/>
    // <remarks>
    //   You can use this event if custom processing of TSiWatch packets
    //   is needed. The event handlers are always called in the context of
    //   the thread which causes the event.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements to the event handlers can cause a presumably undesired
    //   recursive behavior. Also, if you specified that one or more
    //   connections of this TSmartInspect object should operate in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>,
    //   you need to protect the passed TSiWatch packet and its data
    //   by calling its <link TSiPacket.Lock, Lock> and
    //   <link TSiPacket.Unlock, Unlock> methods before and after
    //   processing.
    // </remarks>
    // <example>
    // <code>
    // program OnWatch;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnWatch(ASender: TSmartInspect; AWatch: TSiWatch);
    //   end;
    //
    // procedure TEventTest.OnWatch(ASender: TSmartInspect; AWatch: TSiWatch);
    // begin
    //   WriteLn(Format('%s = %s', [AWatch.Name, AWatch.Value]));
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     Si.OnWatch := EventTest.OnWatch;
    //     Si.Enabled := True;
    //     SiMain.WatchInteger('Integer', 23);
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnWatch: TSiWatchEvent read GetOnWatch write SetOnWatch;

    // <summary>
    //   This event is fired after an error occurred.
    // </summary>
    // <seealso cref="TSiErrorEvent"/>
    // <remarks>
    //   This event is fired when an error occurs. An error could be a
    //   connection problem or wrong permissions when writing log files,
    //   for example. Instead of throwing exceptions, this event is used
    //   for error reporting in the SmartInspect Delphi library. The
    //   event handlers are always called in the context of the thread
    //   which causes the event. In <link TSiProtocol.IsValidOption,
    //   asynchronous protocol mode>, this is not necessarily the thread
    //   that initiated the related log call.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements or other code to the event handlers which can lead
    //   to the error event can cause a presumably undesired recursive
    //   behavior.
    // </remarks>
    // <example>
    // <code>
    // program OnError;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnError(ASender: TSmartInspect; AException: Exception);
    //   end;
    //
    // procedure TEventTest.OnError(ASender: TSmartInspect;
    //   AException: Exception);
    // begin
    //   WriteLn(AException.Message);
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     // Register our event handler for the error event.
    //     Si.OnError := EventTest.OnError;
    //
    //     // And force a connection error.
    //     Si.Connections := 'file(filename=c:\\)';
    //     Si.Enabled := True;
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnError: TSiErrorEvent read GetOnError write SetOnError;

    // <summary>
    //   Occurs when a Process Flow entry is processed.
    // </summary>
    // <seealso cref="TSiProcessFlow"/>
    // <seealso cref="TSiProcessFlowEvent"/>
    // <remarks>
    //   You can use this event if custom processing of TSiProcessFlow
    //   packets is needed. The event handlers are always called in the
    //   context of the thread which causes the event.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements to the event handlers can cause a presumably undesired
    //   recursive behavior. Also, if you specified that one or more
    //   connections of this TSmartInspect object should operate in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>,
    //   you need to protect the passed TSiProcessFlow packet and its
    //   data by calling its <link TSiPacket.Lock, Lock> and
    //   <link TSiPacket.Unlock, Unlock> methods before and after
    //   processing.
    // </remarks>
    // <example>
    // <code>
    // program OnProcessFlow;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnProcessFlow(ASender: TSmartInspect;
    //       AProcessFlow: TSiProcessFlow);
    //   end;
    //
    // procedure TEventTest.OnProcessFlow(ASender: TSmartInspect;
    //   AProcessFlow: TSiProcessFlow);
    // begin
    //   WriteLn(AProcessFlow.Title);
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     Si.OnProcessFlow := EventTest.OnProcessFlow;
    //     Si.Enabled := True;
    //     SiMain.EnterProcess(ExtractFileName(ParamStr(0)));
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnProcessFlow: TSiProcessFlowEvent read GetOnProcessFlow
      write SetOnProcessFlow;

    // <summary>
    //   Occurs when a Control Command is processed.
    // </summary>
    // <seealso cref="TSiControlCommand"/>
    // <seealso cref="TSiControlCommandEvent"/>
    // <remarks>
    //   You can use this event if custom processing of TSiControlCommand
    //   packets is needed. The event handlers are always called in the
    //   context of the thread which causes the event.
    //
    //   <b>Please note</b>: Keep in mind that adding SmartInspect log
    //   statements to the event handlers can cause a presumably undesired
    //   recursive behavior. Also, if you specified that one or more
    //   connections of this TSmartInspect object should operate in
    //   <link TSiProtocol.IsValidOption, asynchronous protocol mode>,
    //   you need to protect the passed TSiControlCommand packet and
    //   its data by calling its <link TSiPacket.Lock, Lock> and
    //   <link TSiPacket.Unlock, Unlock> methods before and after
    //   processing.
    // </remarks>
    // <example>
    // <code>
    // program OnControlCommand;
    // {$APPTYPE CONSOLE}
    //
    // uses
    //   SysUtils, SiAuto, SmartInspect;
    //
    // type
    //   TEventTest = class(TObject)
    //   public
    //     procedure OnControlCommand(ASender: TSmartInspect;
    //       AControlCommand: TSiControlCommand);
    //   end;
    //
    // procedure TEventTest.OnControlCommand(ASender: TSmartInspect;
    //   AControlCommand: TSiControlCommand);
    // begin
    //   WriteLn(Ord(AControlCommand.ControlCommandType));
    // end;
    //
    // var
    //   EventTest: TEventTest;
    //
    // begin
    //   EventTest := TEventTest.Create;
    //   try
    //     Si.OnControlCommand := EventTest.OnControlCommand;
    //     Si.Enabled := True;
    //     SiMain.ClearAll;
    //   finally
    //     EventTest.Free;
    //   end;
    // end.
    // </code>
    // </example>

    property OnControlCommand: TSiControlCommandEvent read GetOnControlCommand
      write SetOnControlCommand;
  end;

  // <summary>
  //   A configurable timer for monitoring and reloading SmartInspect
  //   configuration files on changes.
  // </summary>
  // <remarks>
  //   Use this class to monitor and automatically reload SmartInspect
  //   configuration files. This timer periodically checks if the
  //   related configuration file has changed (by comparing the last
  //   write time) and automatically tries to reload the configuration
  //   properties. You can pass the TSmartInspect object to configure,
  //   the name of the configuration file to monitor and the interval
  //   in which this timer should check for changes.
  //
  //   For information about SmartInspect configuration files, please
  //   refer to the documentation of the TSmartInspect.LoadConfiguration
  //   method.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiConfigurationTimer = class(TSiTimer)
  private
    FSmartInspect: TSmartInspect;
    FLastUpdate: TDateTime;
    FFileName: UnicodeString;
  private
    procedure Run(AState: TObject);
  public

    // <summary>
    //   Creates and initializes a new TSiConfigurationTimer object.
    // </summary>
    // <param name="ASmartInspect">
    //   The TSmartInspect object to configure.
    // </param>
    // <param name="AFileName">
    //   The name of the configuration file to monitor.
    // </param>
    // <param name="APeriod">
    //   The milliseconds interval in which this timer should check for
    //   changes.
    // </param>
    // <exception>
    // <table>
    //   Exception Type            Condition
    //   -                         -
    //   ESmartInspectError        The ASmartInspect parameter is nil
    //                               or APeriod is negative.
    // </table>
    // </exception>

    constructor Create(const ASmartInspect: TSmartInspect;
      const AFileName: UnicodeString; const APeriod: Integer);
  end;

  // <summary>
  //   Logs all kind of data and variables to the SmartInspect Console
  //   or to a log file.
  // </summary>
  // <remarks>
  //   The TSiSession class offers dozens of useful methods for sending
  //   any kind of data with the assistance of its <link TSiSession.Parent,
  //   parent>. Sessions can send simple messages, warnings, errors and
  //   more complex things like pictures, objects, exceptions, system
  //   information and much more. They are even able to send variable
  //   watches, generate illustrated process and thread information or
  //   control the behavior of the SmartInspect Console. It is possible,
  //   for example, to clear the entire log in the Console by calling the
  //   ClearLog method.
  //
  //   Please note that log methods of this class do nothing and return
  //   immediately if the session is currently <link Active, inactive>,
  //   its <link TSiSession.Parent, parent> is <link TSmartInspect.Enabled,
  //   disabled> or the <link TSmartInspect.Level, log level> is not
  //   sufficient.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiSession = class(TObject)
  private
    FCounter: TSiIntegerHash;
    FCheckpoints: TSiIntegerHash;
    FCriticalSection: TCriticalSection;
    FName: UnicodeString;
    FColor: TColor;
    FCheckpointCounter: Integer;
    FParent: TSmartInspect;
    FActive: Boolean;
    FIsStored: Boolean;
    FLevel: TSiLevel;
    procedure SetName(const AValue: UnicodeString);
    procedure SendContext(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AContext: TSiViewerContext);
    procedure SendLogEntry(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;
    procedure SendLogEntry(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId;
      const AColor: TColor; const AData: TStream); overload;
    procedure SendControlCommand(
      const AControlCommandType: TSiControlCommandType;
      const AData: TStream = nil);
    procedure SendWatch(const ALevel: TSiLevel; const AName, AValue: UnicodeString;
      const AWatchType: TSiWatchType);
    procedure SendProcessFlow(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AProcessFlowType: TSiProcessFlowType);
    function UpdateCounter(const AName: UnicodeString;
      const AIncrement: Boolean): Integer;
    function GetName: UnicodeString;
  protected

    // <summary>
    //   Overloaded. Logs an internal error message with a log level
    //   of lvError.
    // </summary>
    // <param name="ATitle">
    //   A string which describes the internal error.
    // </param>
    // <remarks>
    //   This method logs an internal error. Such errors can occur
    //   if session methods are invoked with invalid arguments. For
    //   example, if you pass an invalid format string to LogMessage,
    //   the exception will be caught and an internal error with the
    //   exception message will be sent.
    //
    //   This method is also intended to be used in derived classes to
    //   report any errors in your own methods.
    // </remarks>

    procedure LogInternalError(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an internal error message with a log level
    //   of lvError. The error message is created with a format string
    //   and a related array of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create a description of the internal error.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This method logs an internal error. Such errors can occur
    //   if session methods are invoked with invalid arguments. For
    //   example, if you pass an invalid format string to LogMessage,
    //   the exception will be caught and an internal error with the
    //   exception message will be sent.
    //
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the error message.
    //
    //   This method is also intended to be used in derived classes
    //   to report any errors in your own methods.
    // </remarks>

    procedure LogInternalError(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Indicates if this session is stored in the session tracking list
    //   of its Parent.
    // </summary>
    // <returns>
    //   True if this session is stored in the session tracking list and
    //   False otherwise.
    // </returns>
    // <remarks>
    //   See the TSmartInspect.GetSession and TSmartInspect.AddSession
    //   methods for more information about session tracking.
    // </remarks>

    property IsStored: Boolean read FIsStored write FIsStored;
  public

    // <summary>
    //   Creates and initializes a TSiSession instance.
    // </summary>
    // <param name="AParent">
    //   The parent of this session. It is responsible for sending
    //   the packets to the Console or to write them to a file.
    // </param>
    // <param name="ASessionName">
    //   The name of this session. This name will be displayed in
    //   the Console to differentiate between this and other sessions.
    // </param>
    // <exception>
    // <table>
    //   Exception Type                 Condition
    //   -                              -
    //   ESmartInspectError             The AParent argument is not assigned.
    // </table>
    // </exception>

    constructor Create(const AParent: TSmartInspect;
      const ASessionName: UnicodeString);

    // <summary>
    //   Overridden. Releases all resources.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Overloaded. Indicates if information would be logged for a
    //   certain log level or not.
    // </summary>
    // <param name="ALevel">The log level to check for.</param>
    // <returns>
    //   True if information can be logged and false otherwise.
    // </returns>
    // <remarks>
    //   This method is used by the logging methods in this class
    //   to determine if information should be logged or not. When
    //   extending the Session class by adding new log methods to a
    //   derived class it is recommended to call this method first.
    // </remarks>

    function IsOn(const ALevel: TSiLevel): Boolean; overload;
{$IFDEF DELPHI2005_OR_HIGHER}
      inline;
{$ENDIF}

    // <summary>
    //   Overloaded. Indicates if information can be logged or not.
    // </summary>
    // <returns>
    //   True if information can be logged and false otherwise.
    // </returns>
    // <remarks>
    //   This function is used by the logging methods in this class
    //   to determine if information should be logged or not. When
    //   extending the TSiSession class by adding new log methods to a
    //   derived class it is recommended to call this function first.
    // </remarks>

    function IsOn: Boolean; overload;
{$IFDEF DELPHI2005_OR_HIGHER}
      inline;
{$ENDIF}

    // <summary>
    //   Overloaded. Tracks a method call by using the default log
    //   level.
    // </summary>
    // <param name="AMethodName">The name of the method.</param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    function TrackMethod(const AMethodName: UnicodeString): ISiMethodTracker;
      overload;

    // <summary>
    //   Overloaded. Tracks a method call by using a custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodName">The name of the method.</param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    // </remarks>

    function TrackMethod(const ALevel: TSiLevel;
      const AMethodName: UnicodeString): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using the default log
    //   level. The method name consists of a format string and the
    //   related array of arguments.
    // </summary>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    function TrackMethod(const AMethodNameFmt: UnicodeString;
      const AArgs: array of const): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using a custom log
    //   level. The method name consists of a format string and the
    //   related array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    // </remarks>

    function TrackMethod(const ALevel: TSiLevel;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using the default log
    //   level. The resulting method name consists of the ClassName
    //   of the supplied AInstance parameter, followed by a dot and
    //   the supplied AMethodName argument.
    // </summary>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodName">The name of the method.</param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    function TrackMethod(const AInstance: TObject;
      const AMethodName: UnicodeString): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using a custom log
    //   level. The resulting method name consists of the ClassName
    //   of the supplied AInstance parameter, followed by a dot and
    //   the supplied AMethodName argument.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodName">The name of the method.</param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    // </remarks>

    function TrackMethod(const ALevel: TSiLevel; const AInstance: TObject;
      const AMethodName: UnicodeString): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using the default log level.
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   format string and its related array of arguments.
    // </summary>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   format string and its related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    function TrackMethod(const AInstance: TObject;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Tracks a method call by using a custom log level.
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   format string and its related array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <returns>
    //   An instance of a class which implements the ISiMethodTracker
    //   interface or nil if the parent of this session is not enabled.
    //   The result of this method can be silently ignored.
    // </returns>
    // <remarks>
    //   The TrackMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. Furthermore, the Console will be notified when the
    //   method will be left. If the TrackMethod method is used consequently,
    //   a full call stack is visible in the Console which helps locating
    //   bugs in the source code.
    //
    //   This method makes use of Delphi's reference counting feature.
    //   If the parent of this session is enabled, then this TrackMethod
    //   method will return an instance of TSiMethodTracker which
    //   implements the ISiMethodTracker interface. Delphi takes care
    //   of the memory management for this particular instance and will
    //   destroy it at the end of the method. So, there's no need to
    //   save the result of this method as you can see in the example.
    //
    //   The resulting method name consists of the ClassName of the
    //   supplied AInstance parameter, followed by a dot and the supplied
    //   format string and its related array of arguments.
    // </remarks>

    function TrackMethod(const ALevel: TSiLevel; const AInstance: TObject;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const): ISiMethodTracker; overload;

    // <summary>
    //   Overloaded. Logs a simple separator with the default log level.
    // </summary>
    // <seealso cref="TSiSession.AddCheckpoint"/>
    // <remarks>
    //   This method instructs the Console to draw a separator. This
    //   separator is intended to group related <link TSiLogEntry,
    //   Log Entries> and to separate them visually from others. So,
    //   this method helps you to organize Log Entries in the Console.
    //   See AddCheckpoint for a method with a similar intention.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSeparator; overload;

    // <summary>
    //   Overloaded. Logs a simple separator with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <seealso cref="TSiSession.AddCheckpoint"/>
    // <remarks>
    //   This method instructs the Console to draw a separator. This
    //   separator is intended to group related <link TSiLogEntry,
    //   Log Entries> and to separate them visually from others. So,
    //   this method helps you to organize Log Entries in the Console.
    //   See AddCheckpoint for a method with a similar intention.
    // </remarks>

    procedure LogSeparator(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Resets the call stack by using the default log
    //   level.
    // </summary>
    // <remarks>
    //   This method instructs the Console to reset the call stack
    //   generated by the EnterMethod and LeaveMethod methods. It
    //   is especially useful if you want to reset the indentation
    //   in the method hierarchy without clearing all
    //   <link TSiLogEntry, Log Entries>.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure ResetCallstack; overload;

    // <summary>
    //   Overloaded. Resets the call stack by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <remarks>
    //   This method instructs the Console to reset the call stack
    //   generated by the EnterMethod and LeaveMethod methods. It
    //   is especially useful if you want to reset the indentation
    //   in the method hierarchy without clearing all
    //   <link TSiLogEntry, Log Entries>.
    // </remarks>

    procedure ResetCallstack(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Enters a method by using the default log level.
    // </summary>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterMethod(const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a method by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure EnterMethod(const ALevel: TSiLevel;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a method by using the default log level.
    //   The method name consists of a format string and the related
    //   array of arguments.
    // </summary>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterMethod(const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a method by using a custom log level.
    //   The method name consists of a format string and the related
    //   array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure EnterMethod(const ALevel: TSiLevel;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a method by using the default log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    // </summary>
    // <param name="AMethodName">The name of the method.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied AMethodName
    //   argument.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterMethod(const AInstance: TObject;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a method by using a custom log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodName">The name of the method.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied AMethodName
    //   argument.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure EnterMethod(const ALevel: TSiLevel;
      const AInstance: TObject;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a method by using the default log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format
    //   string and its related array of arguments.
    // </summary>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format string
    //   and its related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterMethod(const AInstance: TObject;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a method by using a custom log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format
    //   string and its related array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterMethod method notifies the Console that a new method
    //   has been entered. The Console includes the method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the LeaveMethod method as the counter piece to
    //   EnterMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format string
    //   and its related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the LeaveMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure EnterMethod(const ALevel: TSiLevel;
      const AInstance: TObject; const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a method by using the default log level.
    // </summary>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveMethod(const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a method by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure LeaveMethod(const ALevel: TSiLevel;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a method by using the default log level. The
    //   method name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="AMethodNameFmt">
    //   The format string to create the method name to display in the
    //   Console.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveMethod(const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a method by using a custom log level. The
    //   method name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AMethodNameFmt">
    //   The format string to create the method name to display in the
    //   Console.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of a format string and the
    //   related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure LeaveMethod(const ALevel: TSiLevel;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a method by using the default log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    // </summary>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied AMethodName
    //   argument.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveMethod(const AInstance: TObject;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a method by using a custom log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied
    //   AMethodName argument.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodName">The name of the method.</param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied AMethodName
    //   argument.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure LeaveMethod(const ALevel: TSiLevel;
      const AInstance: TObject;
      const AMethodName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a method by using the default log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format
    //   string and its related array of arguments.
    // </summary>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format string
    //   and its related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveMethod(const AInstance: TObject;
      const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a method by using a custom log level. The
    //   resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format
    //   string and its related array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AInstance">
    //   The class name of this instance and a dot will be prepended
    //   to the method name.
    // </param>
    // <param name="AMethodNameFmt">
    //   The format string to create the name of the method.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveMethod method notifies the Console that a method has
    //   been left. The Console closes the current method in the method
    //   hierarchy. If this method is used consequently, a full call stack
    //   is visible in the Console which helps locating bugs in the source
    //   code. Please see the EnterMethod method as the counter piece to
    //   LeaveMethod.
    //
    //   The resulting method name consists of the ClassName of the supplied
    //   AInstance parameter, followed by a dot and the supplied format string
    //   and its related array of arguments.
    //
    //   For a method which combines the functionality of this and
    //   the EnterMethod method, please have a look at the TrackMethod
    //   documentation.
    // </remarks>

    procedure LeaveMethod(const ALevel: TSiLevel;
      const AInstance: TObject; const AMethodNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a new thread by using the default log level.
    // </summary>
    // <param name="AThreadName">The name of the thread.</param>
    // <remarks>
    //   The EnterThread method notifies the Console that a new thread has
    //   been entered. The Console displays this thread in the Process Flow
    //   toolbox. If this method is used consequently, all threads of a
    //   process are displayed. Please see the LeaveThread method as the
    //   counter piece to EnterThread.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterThread(const AThreadName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a new thread by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AThreadName">The name of the thread.</param>
    // <remarks>
    //   The EnterThread method notifies the Console that a new thread has
    //   been entered. The Console displays this thread in the Process Flow
    //   toolbox. If this method is used consequently, all threads of a
    //   process are displayed. Please see the LeaveThread method as the
    //   counter piece to EnterThread.
    // </remarks>

    procedure EnterThread(const ALevel: TSiLevel;
      const AThreadName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a new thread by using the default log level.
    //   The thread name consists of a format string and the related array
    //   of arguments.
    // </summary>
    // <param name="ThreadNameFmt">
    //   The format string to create the name of the thread.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterThread method notifies the Console that a new thread has
    //   been entered. The Console displays this thread in the Process Flow
    //   toolbox. If this method is used consequently, all threads of a
    //   process are displayed. Please see the LeaveThread method as the
    //   counter piece to EnterThread.
    //
    //   The resulting thread name consists of a format string and the
    //   related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterThread(const AThreadNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a new thread by using a custom log level.
    //   The thread name consists of a format string and the related
    //   array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ThreadNameFmt">
    //   The format string to create the name of the thread.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterThread method notifies the Console that a new thread has
    //   been entered. The Console displays this thread in the Process Flow
    //   toolbox. If this method is used consequently, all threads of a
    //   process are displayed. Please see the LeaveThread method as the
    //   counter piece to EnterThread.
    //
    //   The resulting thread name consists of a format string and the
    //   related array of arguments.
    // </remarks>

    procedure EnterThread(const ALevel: TSiLevel;
      const AThreadNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a thread by using the default log level.
    // </summary>
    // <param name="AThreadName">The name of the thread.</param>
    // <remarks>
    //   The LeaveThread method notifies the Console that a thread has
    //   been finished. The Console displays this change in the Process
    //   Flow toolbox. Please see the EnterThread method as the counter
    //   piece to LeaveThread.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveThread(const AThreadName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a thread by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AThreadName">The name of the thread.</param>
    // <remarks>
    //   The LeaveThread method notifies the Console that a thread has
    //   been finished. The Console displays this change in the Process
    //   Flow toolbox. Please see the EnterThread method as the counter
    //   piece to LeaveThread.
    // </remarks>

    procedure LeaveThread(const ALevel: TSiLevel;
      const AThreadName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a thread by using the default log level. The
    //   thread name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="AThreadNameFmt">
    //   The format string to create the name of the thread.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveThread method notifies the Console that a thread has
    //   been finished. The Console displays this change in the Process
    //   Flow toolbox. Please see the EnterThread method as the counter
    //   piece to LeaveThread.
    //
    //   The resulting thread name consists of a format string and the
    //   related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveThread(const AThreadNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a thread by using a custom log level. The
    //   thread name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AThreadNameFmt">
    //   The format string to create the name of the thread.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveThread method notifies the Console that a thread has
    //   been finished. The Console displays this change in the Process
    //   Flow toolbox. Please see the EnterThread method as the counter
    //   piece to LeaveThread.
    //
    //   The resulting thread name consists of a format string and the
    //   related array of arguments.
    // </remarks>

    procedure LeaveThread(const ALevel: TSiLevel;
      const AThreadNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a new process by using the default log level
    //   and the parent's application name as process name.
    // </summary>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterProcess; overload;

    // <summary>
    //   Overloaded. Enters a new process by using a custom level
    //   and the parent's application name as process name.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    // </remarks>

    procedure EnterProcess(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Enters a new process by using the default log level.
    // </summary>
    // <param name="AProcessName">The name of the process.</param>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterProcess(const AProcessName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a new process by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AProcessName">The name of the process.</param>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    // </remarks>

    procedure EnterProcess(const ALevel: TSiLevel;
      const AProcessName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Enters a process by using the default log level. The
    //   process name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="AProcessNameFmt">
    //   The format string to create the name to the process.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    //
    //   The resulting process name consists of a format string and the
    //   related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure EnterProcess(const AProcessNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Enters a process by using a custom log level. The
    //   process name consists of a format string and the related array
    //   of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AProcessNameFmt">
    //   The format string to create the name to the process.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The EnterProcess method notifies the Console that a new process
    //   has been entered. The Console displays this process in the Process
    //   Flow toolbox. Please see the LeaveProcess method as the counter
    //   piece to EnterProcess.
    //
    //   The resulting process name consists of a format string and the
    //   related array of arguments.
    // </remarks>

    procedure EnterProcess(const ALevel: TSiLevel;
      const AProcessNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a process by using the default log level and
    //   the parent's application name as process name.
    // </summary>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveProcess; overload;

    // <summary>
    //   Overloaded. Leaves a process by using a custom log level and
    //   the parent's application name as process name.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    // </remarks>

    procedure LeaveProcess(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Leaves a process by using the default log level.
    // </summary>
    // <param name="AProcessName">The name of the process.</param>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveProcess(const AProcessName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a process by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AProcessName">The name of the process.</param>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    // </remarks>

    procedure LeaveProcess(const ALevel: TSiLevel;
      const AProcessName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Leaves a process by using the default log level. The
    //   process name consists of a format string and the related array of
    //   arguments.
    // </summary>
    // <param name="AProcessNameFmt">
    //   The format string to create the name of the process.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    //
    //   The resulting process name consists of a format string and the
    //   related array of arguments.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LeaveProcess(const AProcessNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Leaves a process by using a custom log level. The
    //   process name consists of a format string and the related array
    //   of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AProcessNameFmt">
    //   The format string to create the name of the process.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   The LeaveProcess method notifies the Console that a process has
    //   finished. The Console displays this change in the Process Flow
    //   toolbox. Please see the EnterProcess method as the counter piece
    //   to LeaveProcess.
    //
    //   The resulting process name consists of a format string and the
    //   related array of arguments.
    // </remarks>

    procedure LeaveProcess(const ALevel: TSiLevel;
      const AProcessNameFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a colored message by using the default log level.
    // </summary>
    // <param name="AColor">The background color in the Console.</param>
    // <param name="ATitle">The message to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogColored(const AColor: TColor;
      const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a colored message by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AColor">The background color in the Console.</param>
    // <param name="ATitle">The message to log.</param>

    procedure LogColored(const ALevel: TSiLevel;
      const AColor: TColor; const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a colored message by using the default log level.
    //   The message is created with a format string and a related array of
    //   arguments.
    // </summary>
    // <param name="AColor">The background color in the Console.</param>
    // <param name="ATitleFmt">
    //   A format string to create the message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the message.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogColored(const AColor: TColor; const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a colored message by using a custom log level.
    //   The message is created with a format string and a related array of
    //   arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AColor">The background color in the Console.</param>
    // <param name="ATitleFmt">
    //   A format string to create the message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the message.
    // </remarks>

    procedure LogColored(const ALevel: TSiLevel;
      const AColor: TColor; const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a debug message with a log level of lvDebug.
    // </summary>
    // <param name="ATitle">The message to log.</param>

    procedure LogDebug(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a debug message with a log level of lvDebug.
    //   The message is created with a format string and a related array
    //   of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create the debug message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the message.
    // </remarks>

    procedure LogDebug(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a verbose message with a log level of lvVerbose.
    // </summary>
    // <param name="ATitle">The message to log.</param>

    procedure LogVerbose(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a verbose message with a log level of lvVerbose.
    //   The message is created with a format string and a related array
    //   of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create the verbose message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the message.
    // </remarks>

    procedure LogVerbose(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a simple message with a log level of lvMessage.
    // </summary>
    // <param name="ATitle">The message to log.</param>

    procedure LogMessage(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a simple message with a log level of lvMessage.
    //   The message is created with a format string and a related array
    //   of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create the message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the message.
    // </remarks>

    procedure LogMessage(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a warning message with a log level of lvWarning.
    // </summary>
    // <param name="ATitle">The warning to log.</param>

    procedure LogWarning(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a warning message with a log level of lvWarning.
    //   The warning message is created with a format string and a related
    //   array of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create the warning message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the warning message.
    // </remarks>

    procedure LogWarning(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs an error message with a log level of lvError.
    // </summary>
    // <param name="ATitle">The error to log.</param>
    // <remarks>
    //   This method is ideally used in error handling code such as
    //   exception handlers. If this method is used consequently, it
    //   is easy to troubleshoot and solve bugs in applications or
    //   configurations. See LogException for a similar method.
    // </remarks>

    procedure LogError(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an error message with a log level of lvError.
    //   The error message is created with a format string and a related
    //   array of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create a description of the error.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the error message.
    //
    //   This method is ideally used in error handling code such as
    //   exception handlers. If this method is used consequently, it
    //   is easy to troubleshoot and solve bugs in applications or
    //   configurations. See LogException for a similar method.
    // </remarks>

    procedure LogError(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a fatal error message with a log level of
    //   lvFatal.
    // </summary>
    // <param name="ATitle">The fatal error to log.</param>
    // <remarks>
    //   This method is ideally used in error handling code such as
    //   exception handlers. If this method is used consequently, it
    //   is easy to troubleshoot and solve bugs in applications or
    //   configurations. See LogError for a method which does not
    //   describe fatal but recoverable errors.
    // </remarks>

    procedure LogFatal(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a fatal error message with a log level of
    //   lvFatal. The error message is created with a format string and
    //   a related array of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   A format string to create a description of the fatal error.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed
    //   to the Format method and the resulting string will be
    //   the fatal error message.
    //
    //   This method is ideally used in error handling code such as
    //   exception handlers. If this method is used consequently, it
    //   is easy to troubleshoot and solve bugs in applications or
    //   configurations. See LogError for a method which does not
    //   describe fatal but recoverable errors.
    // </remarks>

    procedure LogFatal(const ATitleFmt: UnicodeString;
      const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs an assert message with a log level of lvError
    //   if a condition is false.
    // </summary>
    // <param name="ACondition">The condition to check.</param>
    // <param name="ATitle">The title of the Log Entry.</param>
    // <remarks>
    //   An assert message is logged if this method is called with a
    //   ACondition parameter of the value false. No <link TSiLogEntry,
    //   Log Entry> is generated if this method is called with a
    //   ACondition parameter of the value true.
    //
    //   A typical usage of this method would be to test if a variable
    //   is not set to nil before you use it. To do this, you just need
    //   to insert a LogAssert call to the code section in question with
    //   "AInstance <> nil" as first parameter. If the reference is nil
    //   and thus the expression evaluates to false, a message is logged.
    // </remarks>

    procedure LogAssert(const ACondition: Boolean;
      const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an assert message with a log level of lvError
    //   if a condition is false. The assert message is created with a
    //   format string and a related array of arguments.
    // </summary>
    // <param name="ATitleFmt">
    //   The format string to create the title of the Log Entry.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   An assert message is logged if this method is called with a
    //   ACondition parameter of the value false. No <link TSiLogEntry,
    //   Log Entry> is generated if this method is called with a
    //   ACondition parameter of the value true.
    //
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed to
    //   the Format method and the resulting string will be the assert
    //   message.
    //
    //   A typical usage of this method would be to test if a variable
    //   is not set to nil before you use it. To do this, you just need
    //   to insert a LogAssert call to the code section in question with
    //   "AInstance <> nil" as first parameter. If the reference is nil
    //   and thus the expression evaluates to false, a message is logged.
    // </remarks>

    procedure LogAssert(const ACondition: Boolean;
      const ATitleFmt: UnicodeString; const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs whether a variable is assigned or not with the
    //   default log level.
    // </summary>
    // <param name="ATitle">The title of the variable.</param>
    // <param name="ARef">
    //   The variable which should be checked for nil.
    // </param>
    // <remarks>
    //   If the ARef argument is nil, then ": Not assigned", otherwise
    //   ": Assigned" will be appended to the title before the <link
    //   TSiLogEntry, Log Entry> is sent.
    //
    //   This method is useful to check source code for nil references in
    //   places where you experienced or expect problems and want to log
    //   possible nil references.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogAssigned(const ATitle: UnicodeString;
      const ARef: Pointer); overload;

    // <summary>
    //   Overloaded. Logs whether a variable is assigned or not with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title of the variable.</param>
    // <param name="ARef">
    //   The variable which should be checked for nil.
    // </param>
    // <remarks>
    //   If the ARef argument is nil, then ": Not assigned", otherwise
    //   ": Assigned" will be appended to the title before the <link
    //   TSiLogEntry, Log Entry> is sent.
    //
    //   This method is useful to check source code for nil references in
    //   places where you experienced or expect problems and want to log
    //   possible nil references.
    // </remarks>

    procedure LogAssigned(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ARef: Pointer); overload;

    // <summary>
    //   Overloaded. Logs a conditional message with the default log
    //   level.
    // </summary>
    // <param name="ACondition">The condition to evaluate.</param>
    // <param name="ATitle">The title of the conditional message.</param>
    // <remarks>
    //   This method only sends a message if the passed 'ACondition'
    //   argument evaluates to true. If 'ACondition' is false, this
    //   method has no effect and nothing is logged. This method is
    //   thus the counter piece to LogAssert.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogConditional(const ACondition: Boolean;
      const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a conditional message with the default log
    //   level. The message is created with a format string and a
    //   related array of arguments.
    // </summary>
    // <param name="ACondition">The condition to evaluate.</param>
    // <param name="ATitleFmt">
    //   The format string to create the conditional message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This method only sends a message if the passed 'ACondition'
    //   argument evaluates to true. If 'ACondition' is false, this
    //   method has no effect and nothing is logged. This method is
    //   thus the counter piece to LogAssert.
    //
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed to
    //   the Format method and the resulting string will be the
    //   conditional message.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogConditional(const ACondition: Boolean;
      const ATitleFmt: UnicodeString; const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Logs a conditional message with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ACondition">The condition to evaluate.</param>
    // <param name="ATitle">The title of the conditional message.</param>
    // <remarks>
    //   This method only sends a message if the passed 'ACondition'
    //   argument evaluates to true. If 'ACondition' is false, this
    //   method has no effect and nothing is logged. This method is
    //   thus the counter piece to LogAssert.
    // </remarks>

    procedure LogConditional(const ALevel: TSiLevel; const ACondition: Boolean;
      const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a conditional message with a custom log
    //   level. The message is created with a format string and a
    //   related array of arguments.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ACondition">The condition to evaluate.</param>
    // <param name="ATitleFmt">
    //   The format string to create the conditional message.
    // </param>
    // <param name="AArgs">
    //   The array of arguments for the format string.
    // </param>
    // <remarks>
    //   This method only sends a message if the passed 'ACondition'
    //   argument evaluates to true. If 'ACondition' is false, this
    //   method has no effect and nothing is logged. This method is
    //   thus the counter piece to LogAssert.
    //
    //   This version of the method accepts a format string and a
    //   related array of arguments. These parameters will be passed to
    //   the Format method and the resulting string will be the
    //   conditional message.
    // </remarks>

    procedure LogConditional(const ALevel: TSiLevel; const ACondition: Boolean;
      const ATitleFmt: UnicodeString; const AArgs: array of const); overload;

    // <summary>
    //   Overloaded. Increments the default checkpoint counter and logs a
    //   message with the default log level.
    // </summary>
    // <seealso cref="TSiSession.LogSeparator"/>
    // <seealso cref="TSiSession.ResetCheckpoint"/>
    // <remarks>
    //   This method increments a checkpoint counter and then logs a
    //   message using "Checkpoint #M" as title. The initial value of
    //   the checkpoint counter is 0. You can use the ResetCheckpoint
    //   method to reset the checkpoint counter to 0 again.
    //
    //   This method is useful, for example, for tracking loops. If
    //   AddCheckpoint is called for each iteration of a loop, it is easy
    //   to follow the execution of the loop in question. This method can
    //   also be used in recursive methods to understand the execution flow.
    //   Furthermore you can use it to highlight important parts of your
    //   code. See LogSeparator for a method with a similar intention.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure AddCheckpoint; overload;

    // <summary>
    //   Overloaded. Increments the default checkpoint counter and logs a
    //   message with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <seealso cref="TSiSession.LogSeparator"/>
    // <seealso cref="TSiSession.ResetCheckpoint"/>
    // <remarks>
    //   This method increments a checkpoint counter and then logs a
    //   message using "Checkpoint #M" as title. The initial value of
    //   the checkpoint counter is 0. You can use the ResetCheckpoint
    //   method to reset the checkpoint counter to 0 again.
    //
    //   This method is useful, for example, for tracking loops. If
    //   AddCheckpoint is called for each iteration of a loop, it is easy
    //   to follow the execution of the loop in question. This method can
    //   also be used in recursive methods to understand the execution flow.
    //   Furthermore you can use it to highlight important parts of your
    //   code. See LogSeparator for a method with a similar intention.
    // </remarks>

    procedure AddCheckpoint(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Increments the counter of a named checkpoint and
    //   logs a message with the default log level and an optional
    //   message.
    // </summary>
    // <param name="AName">
    //   The name of the checkpoint to increment.
    // </param>
    // <param name="ADetails">
    //   An optional message to include in the resulting log entry.
    //   Can be empty.
    // </param>
    // <remarks>
    //   This method increments the counter for the given checkpoint
    //   and then logs a message using "%checkpoint% #N" as title where
    //   %checkpoint% stands for the name of the checkpoint and N for
    //   the incremented counter value. The initial value of the counter
    //   for a given checkpoint is 0. Specify the details parameter to
    //   include an optional message in the resulting log entry. You
    //   can use the ResetCheckpoint method to reset the counter to 0
    //   again.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure AddCheckpoint(const AName: UnicodeString;
      const ADetails: UnicodeString = ''); overload;

    // <summary>
    //   Overloaded. Increments the counter of a named checkpoint and
    //   logs a message with a custom log level and an optional
    //   message.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">
    //   The name of the checkpoint to increment.
    // </param>
    // <param name="ADetails">
    //   An optional message to include in the resulting log entry.
    //   Can be empty.
    // </param>
    // <remarks>
    //   This method increments the counter for the given checkpoint
    //   and then logs a message using "%checkpoint% #N" as title where
    //   %checkpoint% stands for the name of the checkpoint and N for
    //   the incremented counter value. The initial value of the counter
    //   for a given checkpoint is 0. Specify the details parameter to
    //   include an optional message in the resulting log entry. You
    //   can use the ResetCheckpoint method to reset the counter to 0
    //   again.
    // </remarks>

    procedure AddCheckpoint(const ALevel: TSiLevel;
      const AName: UnicodeString; const ADetails: UnicodeString = ''); overload;

    // <summary>
    //   Overloaded. Resets the default checkpoint counter.
    // </summary>
    // <seealso cref="TSiSession.AddCheckpoint"/>
    // <remarks>
    //   This method resets the default checkpoint counter to 0. The
    //   checkpoint counter is used by the AddCheckpoint method.
    // </remarks>

    procedure ResetCheckpoint; overload;

    // <summary>
    //   Overloaded. Resets the counter of a named checkpoint.
    // </summary>
    // <param name="AName">The name of the checkpoint to reset.</param>
    // <remarks>
    //   This method resets the counter of the given named checkpoint.
    //   Named checkpoints can be incremented and logged with the
    //   AddCheckpoint method.
    // </remarks>

    procedure ResetCheckpoint(const AName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Char value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Char variable.
    //   A title like "name = 'C'" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogChar(const AName: UnicodeString; const AValue: Char); overload;

    // <summary>
    //   Overloaded. Logs a Char value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Char variable.
    //   A title like "name = 'C'" will be displayed in the Console.
    // </remarks>

    procedure LogChar(const ALevel: TSiLevel;
      const AName: UnicodeString; const AValue: Char); overload;

    // <summary>
    //   Overloaded. Logs a String value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a String variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogString(const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a String value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a String variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    // </remarks>

    procedure LogString(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a WideString value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a WideString variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogWideString(const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs a WideString value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a WideString variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    // </remarks>

    procedure LogWideString(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs a PChar value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a PChar variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogPChar(const AName: UnicodeString; const AValue: PChar);
      overload;

    // <summary>
    //   Overloaded. Logs a PChar value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a PChar variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    // </remarks>

    procedure LogPChar(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs a PWideChar value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a PWideChar variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogPWideChar(const AName: UnicodeString;
      const AValue: PWideChar); overload;

    // <summary>
    //   Overloaded. Logs a PWideChar value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a PWideChar variable.
    //   A title like "name = 'String'" will be displayed in the Console.
    // </remarks>

    procedure LogPWideChar(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PWideChar); overload;

    // <summary>
    //   Overloaded. Logs an Integer value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Integer variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogInteger(const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs an Integer value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Integer variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogInteger(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs an Integer value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of an Integer variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogInteger(const AName: UnicodeString; const AValue: Integer;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Integer value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of an Integer variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogInteger(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Cardinal variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCardinal(const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Cardinal variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogCardinal(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Cardinal variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCardinal(const AName: UnicodeString; const AValue: Cardinal;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Cardinal variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogCardinal(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Shortint value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Shortint variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogShortint(const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs a Shortint value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Shortint variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogShortint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs a Shortint value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Shortint variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogShortint(const AName: UnicodeString; const AValue: Shortint;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Shortint value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Shortint variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogShortint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Smallint value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Smallint variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSmallint(const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs a Smallint value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Smallint variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogSmallint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs a Smallint value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Smallint variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSmallint(const AName: UnicodeString; const AValue: Smallint;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Smallint value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Smallint variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogSmallint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Int64 value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Int64 variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogInt64(const AName: UnicodeString; const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Int64 value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Int64 variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogInt64(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Int64 value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of an Int64 variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogInt64(const AName: UnicodeString; const AValue: Int64;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Int64 value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of an Int64 variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogInt64(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Byte value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Byte variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogByte(const AName: UnicodeString; const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Byte value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Byte variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogByte(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Byte value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Byte variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogByte(const AName: UnicodeString; const AValue: Byte;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Byte value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Byte variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogByte(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Word value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Word variable.
    //   A title like "name = 23" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogWord(const AName: UnicodeString; const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs a Word value with a custom level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Word variable.
    //   A title like "name = 23" will be displayed in the Console.
    // </remarks>

    procedure LogWord(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs a Word value with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Word variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogWord(const AName: UnicodeString; const AValue: Word;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Word value with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <remarks>
    //   This method logs the name and value of a Word variable.
    //   If you set the AIncludeHex argument to true then the
    //   hexadecimal representation of the supplied variable value
    //   is included as well.
    // </remarks>

    procedure LogWord(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Single value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Single variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSingle(const AName: UnicodeString;
      const AValue: Single); overload;

    // <summary>
    //   Overloaded. Logs a Single value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Single variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    // </remarks>

    procedure LogSingle(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Single); overload;

    // <summary>
    //   Overloaded. Logs a Double value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Double variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogDouble(const AName: UnicodeString;
      const AValue: Double); overload;

    // <summary>
    //   Overloaded. Logs a Double value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Double variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    // </remarks>

    procedure LogDouble(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Double); overload;

    // <summary>
    //   Overloaded. Logs an Extended value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Extended variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogExtended(const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs an Extended value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of an Extended variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    // </remarks>

    procedure LogExtended(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs a Currency value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Currency variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCurrency(const AName: UnicodeString;
      const AValue: Currency); overload;

    // <summary>
    //   Overloaded. Logs a Currency value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Currency variable.
    //   A title like "name = 3.1415" will be displayed in the Console.
    // </remarks>

    procedure LogCurrency(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Currency); overload;

    // <summary>
    //   Overloaded. Logs a Boolean value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Boolean variable.
    //   A title like "name = True" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBoolean(const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Boolean value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Boolean variable.
    //   A title like "name = True" will be displayed in the Console.
    // </remarks>

    procedure LogBoolean(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Pointer value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Pointer variable.
    //   A title like "name = $00000000" will be displayed in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogPointer(const AName: UnicodeString;
      const AValue: Pointer); overload;

    // <summary>
    //   Overloaded. Logs a Pointer value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a Pointer variable.
    //   A title like "name = $00000000" will be displayed in the Console.
    // </remarks>

    procedure LogPointer(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Pointer); overload;

    // <summary>
    //   Overloaded. Logs a TDateTime value with the default log level.
    // </summary>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a TDateTime variable.
    //   A title like "name = 16.12.2004 16:56:38" will be displayed
    //   in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogDateTime(const AName: UnicodeString;
      const AValue: TDateTime); overload;

    // <summary>
    //   Overloaded. Logs a TDateTime value with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The variable name.</param>
    // <param name="AValue">The variable value.</param>
    // <remarks>
    //   This method logs the name and value of a TDateTime variable.
    //   A title like "name = 16.12.2004 16:56:38" will be displayed
    //   in the Console.
    // </remarks>

    procedure LogDateTime(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: TDateTime); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a String variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The String value of the variable.</param>
    // <remarks>
    //   This method just calls the LogString method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString; const AValue: String);
      overload;

    // <summary>
    //   Overloaded. Logs the name and value of a String variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The String value of the variable.</param>
    // <remarks>
    //   This method just calls the LogString method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a WideString variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The WideString value of the variable.</param>
    // <remarks>
    //   This method just calls the LogWideString method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a WideString variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The WideString value of the variable.</param>
    // <remarks>
    //   This method just calls the LogWideString method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a PChar variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The PChar value of the variable.</param>
    // <remarks>
    //   This method just calls the LogPChar method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString; const AValue: PChar);
      overload;

    // <summary>
    //   Overloaded. Logs the name and value of a PChar variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The PChar value of the variable.</param>
    // <remarks>
    //   This method just calls the LogPChar method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a PWideChar variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The PWideChar value of the variable.</param>
    // <remarks>
    //   This method just calls the LogPWideChar method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

{$IFNDEF DELPHI2009_OR_HIGHER}
    procedure LogValue(const AName: UnicodeString;
      const AValue: PWideChar); overload;
{$ENDIF}

    // <summary>
    //   Overloaded. Logs the name and value of a PWideChar variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The PWideChar value of the variable.</param>
    // <remarks>
    //   This method just calls the LogPWideChar method.
    // </remarks>

{$IFNDEF DELPHI2009_OR_HIGHER}
    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PWideChar); overload;
{$ENDIF}

    // <summary>
    //   Overloaded. Logs the name and value of an Integer variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Integer value of the variable.</param>
    // <remarks>
    //   This method just calls the LogInteger method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs the name and value of an Integer variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Integer value of the variable.</param>
    // <remarks>
    //   This method just calls the LogInteger method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Shortint variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Shortint value of the variable.</param>
    // <remarks>
    //   This method just calls the LogShortint method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Shortint variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Shortint value of the variable.</param>
    // <remarks>
    //   This method just calls the LogShortint method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Smallint variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Smallint value of the variable.</param>
    // <remarks>
    //   This method just calls the LogSmallint method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Smallint variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Smallint value of the variable.</param>
    // <remarks>
    //   This method just calls the LogSmallint method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs the name and value of an Int64 variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Int64 value of the variable.</param>
    // <remarks>
    //   This method just calls the LogInt64 method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString; const AValue: Int64);
      overload;

    // <summary>
    //   Overloaded. Logs the name and value of an Int64 variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Int64 value of the variable.</param>
    // <remarks>
    //   This method just calls the LogInt64 method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Byte variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Byte value of the variable.</param>
    // <remarks>
    //   This method just calls the LogByte method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString; const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Byte variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Byte value of the variable.</param>
    // <remarks>
    //   This method just calls the LogByte method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Word variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Word value of the variable.</param>
    // <remarks>
    //   This method just calls the LogWord method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString; const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Word variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Word value of the variable.</param>
    // <remarks>
    //   This method just calls the LogWord method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Cardinal variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Cardinal value of the variable.</param>
    // <remarks>
    //   This method just calls the LogCardinal method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Cardinal variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Cardinal value of the variable.</param>
    // <remarks>
    //   This method just calls the LogCardinal method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs the name and value of an Extended variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Extended value of the variable.</param>
    // <remarks>
    //   This method just calls the LogExtended method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs the name and value of an Extended variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Extended value of the variable.</param>
    // <remarks>
    //   This method just calls the LogExtended method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Boolean variable
    //   with the default log level.
    // </summary>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Boolean value of the variable.</param>
    // <remarks>
    //   This method just calls the LogBoolean method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogValue(const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs the name and value of a Boolean variable
    //   with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the variable.</param>
    // <param name="AValue">The Boolean value of the variable.</param>
    // <remarks>
    //   This method just calls the LogBoolean method.
    // </remarks>

    procedure LogValue(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a string with the default log level and displays
    //   it in a read-only text field.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AText">The text to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogText(const ATitle, AText: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a string with a custom log level and displays
    //   it in a read-only text field.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AText">The text to log.</param>

    procedure LogText(const ALevel: TSiLevel;
      const ATitle, AText: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a text file with the default log level and
    //   displays the content in a read-only text field.
    // </summary>
    // <param name="AFileName">The file to log.</param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogTextFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a text file with a custom log level and
    //   displays the content in a read-only text field.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">The file to log.</param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogTextFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a text file and displays the content in a
    //   read-only text field using a custom title and default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The file to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogTextFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a text file and displays the content in a
    //   read-only text field using a custom title and custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The file to log.</param>

    procedure LogTextFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and displays
    //   the content in a read-only text field.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogTextStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and displays
    //   the content in a read-only text field.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to log.</param>

    procedure LogTextStream(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs HTML code with the default log level and
    //   displays it in a web browser.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AHtml">The HTML source code to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogHtml(const ATitle, AHtml: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs HTML code with a custom log level and
    //   displays it in a web browser.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AHtml">The HTML source code to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    // </remarks>

    procedure LogHtml(const ALevel: TSiLevel;
      const ATitle, AHtml: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an HTML file with the default log level and
    //   displays the content in a web browser.
    // </summary>
    // <param name="AFileName">The HTML file to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    //
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogHtmlFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an HTML file with a custom log level and
    //   displays the content in a web browser.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">The HTML file to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    //
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogHtmlFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an HTML file and displays the content in a web
    //   browser using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The HTML file to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogHtmlFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an HTML file and displays the content in a web
    //   browser using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The HTML file to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    // </remarks>

    procedure LogHtmlFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and displays
    //   the content in a web browser.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogHtmlStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and displays
    //   the content in a web browser.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display.</param>
    // <remarks>
    //   This method logs the supplied HTML source code. The source
    //   code is displayed as a website in the web viewer of the Console.
    // </remarks>

    procedure LogHtmlStream(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a binary stream with the default log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The binary stream to display in a hex viewer.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBinaryStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a binary stream with a custom log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The binary stream to display in a hex viewer.
    // </param>

    procedure LogBinaryStream(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a binary file with the default log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="AFileName">
    //   The binary file to display in a hex viewer.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBinaryFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a binary file with a custom log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The binary file to display in a hex viewer.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogBinaryFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a binary file and displays its content in a
    //   hex viewer using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The binary file to display in a hex viewer.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBinaryFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a binary file and displays its content in a
    //   hex viewer using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The binary file to display in a hex viewer.
    // </param>

    procedure LogBinaryFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a bitmap with the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ABitmap">The bitmap to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBitmap(const ATitle: UnicodeString;
      const ABitmap: TBitmap); overload;

    // <summary>
    //   Overloaded. Logs a bitmap with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ABitmap">The bitmap to log.</param>

    procedure LogBitmap(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const ABitmap: TBitmap); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs a bitmap file with the default log level and
    //   displays it in the Console.
    // </summary>
    // <param name="AFileName">
    //   The bitmap file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBitmapFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a bitmap file with a custom log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The bitmap file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogBitmapFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a bitmap file and displays it in the Console
    //   using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The bitmap file to display in the Console.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBitmapFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a bitmap file and displays it in the Console
    //   using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The bitmap file to display in the Console.
    // </param>

    procedure LogBitmapFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and
    //   interprets its content as a bitmap.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as bitmap.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogBitmapStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and
    //   interprets its content as a bitmap.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as bitmap.</param>

    procedure LogBitmapStream(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const AStream: TStream); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a JPEG image with the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AJpeg">The JPEG image to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogJpeg(const ATitle: UnicodeString;
      const AJpeg: TJpegImage); overload;

    // <summary>
    //   Overloaded. Logs a JPEG image with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AJpeg">The JPEG image to log.</param>

    procedure LogJpeg(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AJpeg: TJpegImage); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs a JPEG file with the default log level and
    //   displays it in the Console.
    // </summary>
    // <param name="AFileName">
    //   The JPEG file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogJpegFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a JPEG file with a custom log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The JPEG file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogJpegFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a JPEG file and displays it in the Console
    //   using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The JPEG file to display in the Console.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogJpegFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a JPEG file and displays it in the Console
    //   using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The JPEG file to display in the Console.
    // </param>

    procedure LogJpegFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and
    //   interprets its content as JPEG image.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as JPEG image.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogJpegStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and
    //   interprets its content as JPEG image.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as JPEG image.</param>

    procedure LogJpegStream(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStream: TStream); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a Windows icon with the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AIcon">The Windows icon to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogIcon(const ATitle: UnicodeString; const AIcon: TIcon); overload;

    // <summary>
    //   Overloaded. Logs a Windows icon with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AIcon">The Windows icon to log.</param>

    procedure LogIcon(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AIcon: TIcon); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs a Window icon file with the default log level
    //   and displays it in the Console.
    // </summary>
    // <param name="AFileName">
    //   The Windows icon file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogIconFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Window icon file with a custom log level
    //   and displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The Windows icon file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogIconFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Window icon file and displays it in the
    //   Console using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The Windows icon file to display in the Console.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogIconFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Window icon file and displays it in the
    //   Console using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The Windows icon file to display in the Console.
    // </param>

    procedure LogIconFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and
    //   interprets its content as Windows icon.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as Windows icon.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogIconStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and
    //   interprets its content as Windows icon.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to display as Windows icon.</param>

    procedure LogIconStream(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStream: TStream); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a Windows Metafile image with the default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AMetafile">The Windows Metafile image to send.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMetafile(const ATitle: UnicodeString;
      const AMetafile: TMetaFile); overload;

    // <summary>
    //   Overloaded. Logs a Windows Metafile image a custom default log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AMetafile">The Windows Metafile image to send.</param>

    procedure LogMetafile(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AMetafile: TMetaFile); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs a Windows Metafile file with the default log
    //   level and displays it in the Console.
    // </summary>
    // <param name="AFileName">
    //   The Windows Metafile file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMetafileFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Windows Metafile file with a custom log
    //   level and displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The Windows Metafile file to display in the Console.
    // </param>
    // <remarks>
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogMetafileFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Windows Metafile file and displays it in the
    //   Console using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The Windows Metafile file to display in the Console.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMetafileFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a Windows Metafile file and displays it in the
    //   Console using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The Windows Metafile file to display in the Console.
    // </param>

    procedure LogMetafileFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a stream with the default log level and
    //   interprets its content as Windows Metafile image.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The stream to display as Windows Metafile image.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMetafileStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a stream with a custom log level and
    //   interprets its content as Windows Metafile image.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The stream to display as Windows Metafile image.
    // </param>

    procedure LogMetafileStream(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a custom viewer context with the default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AContext">
    //   The viewer context which holds the actual data and the
    //   appropriate viewer ID.
    // </param>
    // <seealso cref="TSiViewerContext"/>
    // <remarks>
    //   This method can be used to extend the capabilities of the
    //   SmartInspect Delphi library. You can assemble a so called viewer
    //   context and thus can send custom data to the SmartInspect Console.
    //   Furthermore, you can choose the viewer in which your data should
    //   be displayed. Every viewer in the Console has a corresponding
    //   viewer context class in this library.
    //
    //   Have a look at the TSiViewerContext class and its derived classes
    //   to see a list of available viewer context classes.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCustomContext(const ATitle: UnicodeString;
      const ALogEntryType: TSiLogEntryType;
      const AContext: TSiViewerContext); overload;

    // <summary>
    //   Overloaded. Logs a custom viewer context with a custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AContext">
    //   The viewer context which holds the actual data and the
    //   appropriate viewer ID.
    // </param>
    // <seealso cref="TSiViewerContext"/>
    // <remarks>
    //   This method can be used to extend the capabilities of the
    //   SmartInspect Delphi library. You can assemble a so called viewer
    //   context and thus can send custom data to the SmartInspect Console.
    //   Furthermore, you can choose the viewer in which your data should
    //   be displayed. Every viewer in the Console has a corresponding
    //   viewer context class in this library.
    //
    //   Have a look at the TSiViewerContext class and its derived classes
    //   to see a list of available viewer context classes.
    // </remarks>

    procedure LogCustomContext(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AContext: TSiViewerContext); overload;

    // <summary>
    //   Overloaded. Logs a text using a custom Log Entry type, viewer ID
    //   and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AText">The text to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the text content.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCustomText(const ATitle, AText: UnicodeString;
      const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs a text using a custom Log Entry type, viewer ID
    //   and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AText">The text to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the text content.
    // </param>

    procedure LogCustomText(const ALevel: TSiLevel;
      const ATitle, AText: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file using a custom Log Entry
    //   type, viewer ID and default log level.
    // </summary>
    // <param name="AFileName">The file to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied file using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    //
    //   This version of the method uses the supplied AFileName argument as
    //   title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCustomFile(const AFileName: UnicodeString;
      const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file using a custom Log Entry
    //   type, viewer ID and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">The file to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied file using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    //
    //   This version of the method uses the supplied AFileName argument as
    //   title to display in the Console.
    // </remarks>

    procedure LogCustomFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file using a custom Log Entry
    //   type, viewer ID, title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The file to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied file using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCustomFile(const ATitle, AFileName: UnicodeString;
      const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file using a custom Log Entry
    //   type, viewer ID and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">The file to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied file using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    //
    //   This version of the method uses the supplied AFileName argument as
    //   title to display in the Console.
    // </remarks>

    procedure LogCustomFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString;
      const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a stream using a custom Log Entry
    //   type, viewer ID and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied stream using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCustomStream(const ATitle: UnicodeString;
      const AStream: TStream; const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs the content of a stream using a custom Log Entry
    //   type, viewer ID and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">The stream to log.</param>
    // <param name="ALogEntryType">The custom Log Entry type.</param>
    // <param name="AViewerId">
    //   The custom viewer ID which specifies the way the Console
    //   handles the file content.
    // </param>
    // <remarks>
    //   This method sends the content of the supplied stream using a custom
    //   Log Entry type and viewer ID, so that you can control the way how
    //   the content of the file will be displayed in the Console. Thus you
    //   can extend the functionality of the SmartInspect Delphi library with
    //   this method.
    // </remarks>

    procedure LogCustomStream(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const AStream: TStream;
      const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId); overload;

    // <summary>
    //   Overloaded. Logs a string containing SQL source code with the
    //   default log level. The SQL source code is displayed with syntax
    //   highlighting in the Console.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ASource">The SQL source code to log.</param>
    // <remarks>
    //   This method displays the supplied SQL source code with syntax
    //   highlighting in the Console.
    //
    //   It is especially useful to debug or track dynamically generated
    //   SQL source code.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSql(const ATitle, ASource: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a string containing SQL source code with a
    //   custom log level. The SQL source code is displayed with syntax
    //   highlighting in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ASource">The SQL source code to log.</param>
    // <remarks>
    //   This method displays the supplied SQL source code with syntax
    //   highlighting in the Console.
    //
    //   It is especially useful to debug or track dynamically generated
    //   SQL source code.
    // </remarks>

    procedure LogSql(const ALevel: TSiLevel;
      const ATitle, ASource: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs source code which will be displayed with syntax
    //   highlighting by using the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ASource">The source code to log.</param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the supplied source code with syntax
    //   highlighting in the Console. The type of the source code can be
    //   specified by the AId argument. Please see the TSiSourceId enum
    //   for information on the supported source code types.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSource(const ATitle, ASource: UnicodeString;
      const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs source code which will be displayed with syntax
    //   highlighting by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ASource">The source code to log.</param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the supplied source code with syntax
    //   highlighting in the Console. The type of the source code can be
    //   specified by the AId argument. Please see the TSiSourceId enum
    //   for information on the supported source code types.
    // </remarks>

    procedure LogSource(const ALevel: TSiLevel;
      const ATitle, ASource: UnicodeString; const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file as source code with
    //   syntax highlighting by using the default log level.
    // </summary>
    // <param name="AFileName">
    //   The file which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the source file with syntax highlighting in
    //   the Console. The type of the source code can be specified by the
    //   AId argument. Please see the TSiSourceId enum for information on
    //   the supported source code types.
    //
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSourceFile(const AFileName: UnicodeString;
      const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file as source code with
    //   syntax highlighting by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">
    //   The file which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the source file with syntax highlighting in
    //   the Console. The type of the source code can be specified by the
    //   AId argument. Please see the TSiSourceId enum for information on
    //   the supported source code types.
    //
    //   This version of the method uses the supplied AFileName argument
    //   as title to display in the Console.
    // </remarks>

    procedure LogSourceFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString; const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file as source code with
    //   syntax highlighting using a custom title and default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The file which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the source file with syntax highlighting in
    //   the Console. The type of the source code can be specified by the
    //   AId argument. Please see the TSiSourceId enum for information on
    //   the supported source code types.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSourceFile(const ATitle, AFileName: UnicodeString;
      const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a file as source code with
    //   syntax highlighting using a custom title and custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">
    //   The file which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the source file with syntax highlighting in
    //   the Console. The type of the source code can be specified by the
    //   AId argument. Please see the TSiSourceId enum for information on
    //   the supported source code types.
    // </remarks>

    procedure LogSourceFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString; const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a stream as source code with
    //   syntax highlighting by using the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The stream which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the content of a stream with syntax
    //   highlighting in the Console. The type of the source code can be
    //   specified by the AId argument. Please see the TSiSourceId enum
    //   for information on the supported source code types.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSourceStream(const ATitle: UnicodeString;
      const AStream: TStream; const AId: TSiSourceId); overload;

    // <summary>
    //   Overloaded. Logs the content of a stream as source code with
    //   syntax highlighting by using a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The stream which contains the source code.
    // </param>
    // <param name="AId">Specifies the type of source code.</param>
    // <remarks>
    //   This method displays the content of a stream with syntax
    //   highlighting in the Console. The type of the source code can be
    //   specified by the AId argument. Please see the TSiSourceId enum
    //   for information on the supported source code types.
    // </remarks>

    procedure LogSourceStream(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const AStream: TStream;
      const AId: TSiSourceId); overload;

    {$IFNDEF SI_DISABLE_RTTI}

    // <summary>
    //   Overloaded. Logs properties and events of an object with the
    //   default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AInstance">
    //   The object whose fields and events should be logged.
    // </param>
    // <remarks>
    //   This method sends all field and events names and their
    //   current values of an object. These key/value pairs will be
    //   displayed in the Console in an object inspector like viewer.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogObject(const ATitle: UnicodeString;
      const AInstance: TObject); overload;

    // <summary>
    //   Overloaded. Logs properties and events of an object with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AInstance">
    //   The object whose fields and events should be logged.
    // </param>
    // <remarks>
    //   This method sends all field and events names and their
    //   current values of an object. These key/value pairs will be
    //   displayed in the Console in an object inspector like viewer.
    // </remarks>

    procedure LogObject(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AInstance: TObject); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs the content of a TStrings instance with the
    //   default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStrings">The string list to log.</param>
    // <remarks>
    //   This method logs the content of supplied string list. Its
    //   elements will be displayed in a list view.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogStringList(const ATitle: UnicodeString;
      const AStrings: TStrings); overload;

    // <summary>
    //   Overloaded. Logs the content of a TStrings instance with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStrings">The string list to log.</param>
    // <remarks>
    //   This method logs the content of supplied string list. Its
    //   elements will be displayed in a list view.
    // </remarks>

    procedure LogStringList(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStrings: TStrings); overload;

    // <summary>
    //   Overloaded. Logs a binary stream with the default log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The binary stream to display in a hex viewer.
    // </param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogStream(const ATitle: UnicodeString;
      const AStream: TStream); overload;

    // <summary>
    //   Overloaded. Logs a binary stream with a custom log level and
    //   displays its content in a hex viewer.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AStream">
    //   The binary stream to display in a hex viewer.
    // </param>

    procedure LogStream(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AStream: TStream); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a rectangle with the default log level.
    // </summary>
    // <param name="ATitle">
    //   The first part of the title to display in the Console.
    // </param>
    // <param name="ARect">The rectangle to log.</param>
    // <remarks>
    //   The final title to display in the Console consists of the ATitle
    //   argument and the properties of the supplied ARectangle parameter like
    //   in this form: "rectangle = (Left: 0; Top: 0; Right: 0; Bottom: 0)".
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogRect(const ATitle: UnicodeString; const ARect: TRect); overload;

    // <summary>
    //   Overloaded. Logs a rectangle with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">
    //   The first part of the title to display in the Console.
    // </param>
    // <param name="ARect">The rectangle to log.</param>
    // <remarks>
    //   The final title to display in the Console consists of the ATitle
    //   argument and the properties of the supplied ARectangle parameter like
    //   in this form: "rectangle = (Left: 0; Top: 0; Right: 0; Bottom: 0)".
    // </remarks>

    procedure LogRect(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const ARect: TRect); overload;

    // <summary>
    //   Overloaded. Logs a point with the default log level.
    // </summary>
    // <param name="ATitle">
    //   The first part of the title to display in the Console.
    // </param>
    // <param name="APoint">The point to log.</param>
    // <remarks>
    //   The final title to display in the Console consists of
    //   the ATitle argument and the properties of the supplied APoint
    //   parameter like in this form: "point = (X: 0; Y: 0)".
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogPoint(const ATitle: UnicodeString;
      const APoint: TPoint); overload;

    // <summary>
    //   Overloaded. Logs a point with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">
    //   The first part of the title to display in the Console.
    // </param>
    // <param name="APoint">The point to log.</param>
    // <remarks>
    //   The final title to display in the Console consists of
    //   the ATitle argument and the properties of the supplied APoint
    //   parameter like in this form: "point = (X: 0; Y: 0)".
    // </remarks>

    procedure LogPoint(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const APoint: TPoint); overload;

    // <summary>
    //   Overloaded. Logs a graphic with the default log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AGraphic">The graphic to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogGraphic(const ATitle: UnicodeString;
      const AGraphic: TGraphic); overload;

    // <summary>
    //   Overloaded. Logs a graphic with a custom log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AGraphic">The graphic to log.</param>

    procedure LogGraphic(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AGraphic: TGraphic); overload;

    // <summary>
    //   Overloaded. Logs a picture with the default log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="APicture">The picture to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogPicture(const ATitle: UnicodeString;
      const APicture: TPicture); overload;

    // <summary>
    //   Overloaded. Logs a picture with a custom log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="APicture">The picture to log.</param>

    procedure LogPicture(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const APicture: TPicture); overload;

    // <summary>
    //   Overloaded. Logs a picture file with the default log level and
    //   displays it in the Console.
    // </summary>
    // <param name="AFileName">The picture file to log.</param>
    // <remarks>
    //   This version of the method uses the supplied AFileName
    //   argument as title to display in the Console.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    //
    //   <b>Please note:</b> Although this method takes a UnicodeString
    //   file name argument, this Unicode string is converted to a normal
    //   ANSI string in Delphi versions prior to Delphi 2009 due to
    //   shortcomings of the Delphi VCL.
    // </remarks>

    procedure LogPictureFile(const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a picture file with a custom log level and
    //   displays it in the Console.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AFileName">The picture file to log.</param>
    // <remarks>
    //   This version of the method uses the supplied AFileName
    //   argument as title to display in the Console.
    //
    //   <b>Please note:</b> Although this method takes a UnicodeString
    //   file name argument, this Unicode string is converted to a normal
    //   ANSI string in Delphi versions prior to Delphi 2009 due to
    //   shortcomings of the Delphi VCL.
    // </remarks>

    procedure LogPictureFile(const ALevel: TSiLevel;
      const AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a picture file and displays it in the Console
    //   using a custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The picture file to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    //
    //   <b>Please note:</b> Although this method takes a UnicodeString
    //   file name argument, this Unicode string is converted to a normal
    //   ANSI string in Delphi versions prior to Delphi 2009 due to
    //   due to shortcomings of the Delphi VCL.
    // </remarks>

    procedure LogPictureFile(const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs a picture file and displays it in the Console
    //   using a custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AFileName">The picture file to log.</param>
    // <remarks>
    //   <b>Please note:</b> Although this method takes a UnicodeString
    //   file name argument, this Unicode string is converted to a normal
    //   ANSI string due to shortcomings of the Delphi VCL.
    // </remarks>

    procedure LogPictureFile(const ALevel: TSiLevel;
      const ATitle, AFileName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs an area of a canvas with the default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ACanvas">
    //   An area of this canvas will be displayed in the Console.
    // </param>
    // <param name="ATitle">Specifies the area of the canvas to log.</param>
    // <remarks>
    //   This method sends the content of the supplied ACanvas argument as
    //   picture. You can specify the area to log with the ARect parameter.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogCanvas(const ATitle: UnicodeString; const ACanvas: TCanvas;
      const ARect: TRect); overload;

    // <summary>
    //   Overloaded. Logs an area of a canvas with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ACanvas">
    //   An area of this canvas will be displayed in the Console.
    // </param>
    // <param name="ATitle">Specifies the area of the canvas to log.</param>
    // <remarks>
    //   This method sends the content of the supplied ACanvas argument as
    //   picture. You can specify the area to log with the ARect parameter.
    // </remarks>

    procedure LogCanvas(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const ACanvas: TCanvas; const ARect: TRect); overload;

    {$ENDIF}

    // <summary>
    //   Overloaded. Logs a memory area with the default log level and
    //   displays the content in a hex viewer.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AAddress">The address of the memory area to log.</param>
    // <param name="ASize">The amount of bytes to log.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMemory(const ATitle: UnicodeString; const AAddress: Pointer;
      const ASize: Integer); overload;

    // <summary>
    //   Overloaded. Logs a memory area with a custom log level and
    //   displays the content in a hex viewer.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AAddress">The address of the memory area to log.</param>
    // <param name="ASize">The amount of bytes to log.</param>

    procedure LogMemory(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AAddress: Pointer; const ASize: Integer); overload;

    // <summary>
    //   Overloaded. Logs memory statistics about the system with the
    //   default log level.
    // </summary>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMemoryStatistic; overload;

    // <summary>
    //   Overloaded. Logs memory statistics about the system with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>

    procedure LogMemoryStatistic(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Logs memory statistics about the system using a
    //   custom title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogMemoryStatistic(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs memory statistics about the system using a
    //   custom title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>

    procedure LogMemoryStatistic(const ALevel: TSiLevel;
      const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs information about the system with the
    //   default log level.
    // </summary>
    // <remarks>
    //   The logged information include the version of the operating
    //   system, the Delphi version and more. This method is useful
    //   for logging general information at the program startup.
    //   This guarantees that the support staff or developers have
    //   general information about the execution environment.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSystem; overload;

    // <summary>
    //   Overloaded. Logs information about the system with a custom
    //   log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <remarks>
    //   The logged information include the version of the operating
    //   system, the Delphi version and more. This method is useful
    //   for logging general information at the program startup.
    //   This guarantees that the support staff or developers have
    //   general information about the execution environment.
    // </remarks>

    procedure LogSystem(const ALevel: TSiLevel); overload;

    // <summary>
    //   Overloaded. Logs information about the system using a custom
    //   title and default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <remarks>
    //   The logged information include the version of the operating
    //   system, the Delphi version and more. This method is useful
    //   for logging general information at the program startup.
    //   This guarantees that the support staff or developers have
    //   general information about the execution environment.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogSystem(const ATitle: UnicodeString); overload;

    // <summary>
    //   Overloaded. Logs information about the system using a custom
    //   title and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <remarks>
    //   The logged information include the version of the operating
    //   system, the Delphi version and more. This method is useful
    //   for logging general information at the program startup.
    //   This guarantees that the support staff or developers have
    //   general information about the execution environment.
    // </remarks>

    procedure LogSystem(const ALevel: TSiLevel;
      const ATitle: UnicodeString); overload;

    {$IFNDEF SI_DISABLE_GRAPHIC}

    // <summary>
    //   Overloaded. Logs a desktop screenshot with the default log
    //   level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AAsJpeg">
    //   Specifies if the screenshot should be sent as JPEG image
    //   instead of a Bitmap.
    // </param>
    // <remarks>
    //   This version of the method logs a screenshot of the desktop.
    //
    //   The AAsJpeg argument specifies if the screenshot should be
    //   sent as JPEG instead of a Bitmap. This results in a smaller
    //   <link TSiLogEntry, Log Entry> but is more time-consuming.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogScreenshot(const ATitle: UnicodeString;
      const AAsJpeg: Boolean = True); overload;

    // <summary>
    //   Overloaded. Logs a desktop screenshot with a custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AAsJpeg">
    //   Specifies if the screenshot should be sent as JPEG image
    //   instead of a Bitmap.
    // </param>
    // <remarks>
    //   This version of the method logs a screenshot of the desktop.
    //
    //   The AAsJpeg argument specifies if the screenshot should be
    //   sent as JPEG instead of a Bitmap. This results in a smaller
    //   <link TSiLogEntry, Log Entry> but is more time-consuming.
    // </remarks>

    procedure LogScreenshot(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AAsJpeg: Boolean = True); overload;

    // <summary>
    //   Overloaded. Logs a screenshot with the default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AWindow">
    //   Specifies the handle of the window which should be used for
    //   the screenshot.
    // </param>
    // <param name="AAsJpeg">
    //   Specifies if the screenshot should be sent as JPEG image
    //   instead of a Bitmap.
    // </param>
    // <remarks>
    //   This version of the method logs a screenshot of the supplied
    //   window handle.
    //
    //   The AAsJpeg argument specifies if the screenshot should be
    //   sent as JPEG instead of a Bitmap. This results in a smaller
    //   <link TSiLogEntry, Log Entry> but is more time-consuming.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogScreenshot(const ATitle: UnicodeString; const AWindow: HWND;
      const AAsJpeg: Boolean = True); overload;

    // <summary>
    //   Overloaded. Logs a screenshot with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="AWindow">
    //   Specifies the handle of the window which should be used for
    //   the screenshot.
    // </param>
    // <param name="AAsJpeg">
    //   Specifies if the screenshot should be sent as JPEG image
    //   instead of a Bitmap.
    // </param>
    // <remarks>
    //   This version of the method logs a screenshot of the supplied
    //   window handle.
    //
    //   The AAsJpeg argument specifies if the screenshot should be
    //   sent as JPEG instead of a Bitmap. This results in a smaller
    //   <link TSiLogEntry, Log Entry> but is more time-consuming.
    // </remarks>

    procedure LogScreenshot(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const AWindow: HWND; const AAsJpeg: Boolean = True); overload;

    {$ENDIF}

    {$IFNDEF SI_DISABLE_DB}

    // <summary>
    //   Overloaded. Logs the content of a TDataSet instance with the
    //   default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ADataSet">
    //   The TDataSet instance whose content should be logged.
    // </param>
    // <param name="ARestoreRecordPosition">
    //   Specifies if the original record position is restored.
    // </param>
    // <remarks>
    //   This method logs the content of the supplied data set.
    //
    //   LogDataSet is especially useful in database applications
    //   with lots of queries. It gives you the possibility to see the
    //   raw query results.
    //
    //   The ARestoreRecordPosition parameter specifies if the original
    //   record position is restored after sending the TDataSet content.
    //   This is only possible if the TDataSet descendant supports the
    //   RecNo property (like the ADO classes do, for example).
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogDataSet(const ATitle: UnicodeString;
      const ADataSet: TDataSet;
      const ARestoreRecordPosition: Boolean = True); overload;

    // <summary>
    //   Overloaded. Logs the content of a TDataSet instance with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ADataSet">
    //   The TDataSet instance whose content should be logged.
    // </param>
    // <param name="ARestoreRecordPosition">
    //   Specifies if the original record position is restored.
    // </param>
    // <remarks>
    //   This method logs the content of the supplied data set.
    //
    //   LogDataSet is especially useful in database applications
    //   with lots of queries. It gives you the possibility to see the
    //   raw query results.
    //
    //   The ARestoreRecordPosition parameter specifies if the original
    //   record position is restored after sending the TDataSet content.
    //   This is only possible if the TDataSet descendant supports the
    //   RecNo property (like the ADO classes do, for example).
    // </remarks>

    procedure LogDataSet(const ALevel: TSiLevel; const ATitle: UnicodeString;
      const ADataSet: TDataSet;
      const ARestoreRecordPosition: Boolean = True); overload;

    // <summary>
    //   Overloaded. Logs the schema of a TDataSet instance with the
    //   default log level.
    // </summary>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ADataSet">
    //   The TDataSet instance whose schema should be logged.
    // </param>
    // <remarks>
    //   This method logs the schema of the supplied data set.
    //
    //   LogDataSetSchema is especially useful in database applications
    //   with lots of queries. It gives you the possibility to see the
    //   raw schema of query results.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure LogDataSetSchema(const ATitle: UnicodeString;
      const ADataSet: TDataSet); overload;

    // <summary>
    //   Overloaded. Logs the schema of a TDataSet instance with a
    //   custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title to display in the Console.</param>
    // <param name="ADataSet">
    //   The TDataSet instance whose schema should be logged.
    // </param>
    // <remarks>
    //   This method logs the schema of the supplied data set.
    //
    //   LogDataSetSchema is especially useful in database applications
    //   with lots of queries. It gives you the possibility to see the
    //   raw schema of query results.
    // </remarks>

    procedure LogDataSetSchema(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ADataSet: TDataSet); overload;

    {$ENDIF}

    // <summary>
    //   Logs the last Win32 error with a log level of lvError.
    // </summary>
    // <param name="ATitle">
    //   The title prefix to display in the Console.
    // </param>
    // <param name="AIgnoreSuccess">
    //   Optional parameter which specifies if a successful operation
    //   should be ignored. The default value is False.
    // </param>
    // <remarks>
    //   This method sends the last Win32 error. The ATitle parameter and
    //   a colon will be prepended to the error message. Furthermore the
    //   error code is included. Please note that a last error code of 0
    //   (Success) will only be logged if the AIgnoreSuccess parameter is
    //   set to False.
    // </remarks>

    procedure LogLastError(const ATitle: UnicodeString;
      const AIgnoreSuccess: Boolean = False);

    // <summary>Logs a Win32 error with a log level of lvError.</summary>
    // <param name="ATitle">
    //   The title prefix to display in the Console.
    // </param>
    // <param name="AErrorCode">The value which represents the error.</param>
    // <param name="AIgnoreSuccess">
    //   Optional parameter which specifies if a successful operation
    //   should be ignored. The default value is False.
    // </param>
    // <remarks>
    //   This method sends the supplied Win32 error. The ATitle parameter
    //   and a colon will be prepended to the error message. Furthermore
    //   the error code is included. Please note that an error code of 0
    //   (Success) will only be logged if the AIgnoreSuccess parameter is
    //   set to False.
    // </remarks>

    procedure LogWin32Error(const ATitle: UnicodeString;
      const AErrorCode: DWord; const AIgnoreSuccess: Boolean = False);

    // <summary>
    //   Overloaded. Logs the current exception with a log level of
    //   lvError.
    // </summary>
    // <param name="ATitle">Optional title prefix.</param>
    // <remarks>
    //   This version of the method logs the current exception. If the
    //   ATitle argument is not an empty string it will be prepended to
    //   the title to display in the Console.
    //
    //   This method is especially useful in exception handlers like
    //   in the example.
    // </remarks>

    procedure LogException(const ATitle: UnicodeString = ''); overload;

    // <summary>
    //   Overloaded. Logs an exception with a log level of lvError.
    // </summary>
    // <param name="AException">The exception to log.</param>
    // <param name="ATitle">Optional title prefix.</param>
    // <remarks>
    //   This version of the method logs the supplied exception. If the
    //   ATitle argument is not an empty string it will be prepended to
    //   the title to display in the Console.
    //
    //   This method is especially useful in exception handlers like
    //   in the example, of course.
    // </remarks>

    procedure LogException(const AException: Exception;
      const ATitle: UnicodeString = ''); overload;

    // <summary>
    //   Acts as an exception handler for the TApplication.OnException
    //   event.
    // </summary>
    // <param name="ASender">The sender object of the event.</param>
    // <param name="AException">The occurred exception.</param>
    // <remarks>
    //   This method is intended to be used as global exception handler
    //   in Delphi GUI applications. It can be assigned to the
    //   TApplication.OnException event. If an unhandled exception occurs,
    //   this event handler logs this exception with the LogException
    //   method. Furthermore this exception <link TSiLogEntry, Log Entry>
    //   will be surrounded by calls to the EnterMethod and LeaveMethod
    //   methods for a better identification in the Console.
    // </remarks>
    // <example>
    // <code>
    //   Application.OnException := SiMain.ExceptionHandler;
    // </code>
    // </example>

    procedure ExceptionHandler(ASender: TObject; AException: Exception);

    // <summary>
    //   Overloaded. Increments a named counter by one and automatically
    //   sends its name and value as integer watch with the default log
    //   level.
    // </summary>
    // <param name="AName">The name of the counter to log.</param>
    // <remarks>
    //   The TSiSession class tracks a list of so called named counters.
    //   A counter has a name and a value of type Integer. This method
    //   increments the value for the specified counter by one and then
    //   sends a normal integer watch with the name and value of the
    //   counter. The initial value of a counter is 0. To reset the
    //   value of a counter to 0 again, you can call ResetCounter.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    //
    //   See DecCounter for a method which decrements the value of a
    //   named counter instead of incrementing it.
    // </remarks>

    procedure IncCounter(const AName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Increments a named counter by one and automatically
    //   sends its name and value as integer watch with a custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the counter to log.</param>
    // <remarks>
    //   The TSiSession class tracks a list of so called named counters.
    //   A counter has a name and a value of type Integer. This method
    //   increments the value for the specified counter by one and then
    //   sends a normal integer watch with the name and value of the
    //   counter. The initial value of a counter is 0. To reset the
    //   value of a counter to 0 again, you can call ResetCounter.
    //
    //   See DecCounter for a method which decrements the value of a
    //   named counter instead of incrementing it.
    // </remarks>

    procedure IncCounter(const ALevel: TSiLevel; const AName: UnicodeString);
      overload;

    // <summary>
    //   Overloaded. Decrements a named counter by one and automatically
    //   sends its name and value as integer watch with the default log
    //   level.
    // </summary>
    // <param name="AName">The name of the counter to log.</param>
    // <remarks>
    //   The TSiSession class tracks a list of so called named counters.
    //   A counter has a name and a value of type Integer. This method
    //   decrements the value for the specified counter by one and then
    //   sends a normal integer watch with the name and value of the
    //   counter. The initial value of a counter is 0. To reset the
    //   value of a counter to 0 again, you can call ResetCounter.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    //
    //   See IncCounter for a method which increments the value of a
    //   named counter instead of decrementing it.
    // </remarks>

    procedure DecCounter(const AName: UnicodeString); overload;

    // <summary>
    //   Overloaded. Decrements a named counter by one and automatically
    //   sends its name and value as integer watch with a custom log
    //   level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the counter to log.</param>
    // <remarks>
    //   The TSiSession class tracks a list of so called named counters.
    //   A counter has a name and a value of type Integer. This method
    //   decrements the value for the specified counter by one and then
    //   sends a normal integer watch with the name and value of the
    //   counter. The initial value of a counter is 0. To reset the
    //   value of a counter to 0 again, you can call ResetCounter.
    //
    //   See IncCounter for a method which increments the value of a
    //   named counter instead of decrementing it.
    // </remarks>

    procedure DecCounter(const ALevel: TSiLevel;
      const AName: UnicodeString); overload;

    // <summary>
    //   Resets a named counter to its initial value of 0.
    // </summary>
    // <param name="AName">The name of the counter to reset.</param>
    // <remarks>
		//   This method resets the integer value of a named counter to 0
		//   again. If the supplied counter is unknown, this method has no
		//   effect. Please refer to the IncCounter and DecCounter methods
		//   for more information about named counters.
    // </remarks>

    procedure ResetCounter(const AName: UnicodeString);

    // <summary>
    //   Overloaded. Logs a Char Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchChar(const AName: UnicodeString; const AValue: Char);
      overload;

    // <summary>
    //   Overloaded. Logs a Char Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchChar(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Char); overload;

    // <summary>
    //   Overloaded. Logs a String Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchString(const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a String Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchString(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a WideString Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchWideString(const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs a WideString Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchWideString(const ALevel: TSiLevel;
      const AName: UnicodeString; const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs a PChar Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchPChar(const AName: UnicodeString;
      const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs a PChar Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchPChar(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs a PWideChar Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchPWideChar(const AName: UnicodeString;
      const AValue: PWideChar); overload;

    // <summary>
    //   Overloaded. Logs a PWideChar Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchPWideChar(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PWideChar); overload;

    // <summary>
    //   Overloaded. Logs an Integer Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchInteger(const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs an Integer Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchInteger(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs an Integer Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs an Integer Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchInteger(const AName: UnicodeString; const AValue: Integer;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Integer Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs an Integer Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchInteger(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchCardinal(const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchCardinal(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Cardinal Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchCardinal(const AName: UnicodeString; const AValue: Cardinal;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Cardinal Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchCardinal(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchShortint(const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchShortint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Shortint Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchShortint(const AName: UnicodeString; const AValue: Shortint;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Shortint Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchShortint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchSmallint(const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchSmallint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Smallint Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchSmallint(const AName: UnicodeString; const AValue: Smallint;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Smallint Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchSmallint(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchInt64(const AName: UnicodeString;
      const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchInt64(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs an Int64 Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchInt64(const AName: UnicodeString; const AValue: Int64;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs an Int64 Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchInt64(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchByte(const AName: UnicodeString; const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchByte(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Byte Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchByte(const AName: UnicodeString; const AValue: Byte;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Byte Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchByte(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchWord(const AName: UnicodeString; const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchWord(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with an optional hexadecimal
    //   representation and default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Word Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchWord(const AName: UnicodeString; const AValue: Word;
      const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with an optional hexadecimal
    //   representation and custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <param name="AIncludeHex">
    //   Indicates if a hexadecimal representation should be included.
    // </param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method logs a Word Watch. You can specify if a
    //   hexadecimal representation should be included as well
    //   by setting the AIncludeHex parameter to true.
    // </remarks>

    procedure WatchWord(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word; const AIncludeHex: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Single Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchSingle(const AName: UnicodeString;
      const AValue: Single); overload;

    // <summary>
    //   Overloaded. Logs a Single Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchSingle(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Single); overload;

    // <summary>
    //   Overloaded. Logs a Double Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchDouble(const AName: UnicodeString;
      const AValue: Double); overload;

    // <summary>
    //   Overloaded. Logs a Double Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchDouble(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Double); overload;

    // <summary>
    //   Overloaded. Logs an Extended Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchExtended(const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs an Extended Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchExtended(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs a Currency Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchCurrency(const AName: UnicodeString;
      const AValue: Currency); overload;

    // <summary>
    //   Overloaded. Logs a Currency Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchCurrency(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Currency); overload;

    // <summary>
    //   Overloaded. Logs a Boolean Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchBoolean(const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a Boolean Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchBoolean(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Overloaded. Logs a TDateTime Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchDateTime(const AName: UnicodeString;
      const AValue: TDateTime); overload;

    // <summary>
    //   Overloaded. Logs a TDateTime Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchDateTime(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: TDateTime); overload;

    // <summary>
    //   Overloaded. Logs a Pointer Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure WatchPointer(const AName: UnicodeString;
      const AValue: Pointer); overload;

    // <summary>
    //   Overloaded. Logs a Pointer Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">The value to display as Watch value.</param>
    // <seealso cref="TSiWatch"/>

    procedure WatchPointer(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Pointer); overload;

    // <summary>
    //   Overloaded. Logs a String Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The String value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchString method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a String Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The String value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchString method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: String); overload;

    // <summary>
    //   Overloaded. Logs a WideString Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The WideString value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchWideString method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: WideString);
      overload;

    // <summary>
    //   Overloaded. Logs a WideString Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The WideString value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchWideString method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: WideString); overload;

    // <summary>
    //   Overloaded. Logs a PChar Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The PChar value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchPChar method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs a PChar Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The PChar value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchPChar method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PChar); overload;

    // <summary>
    //   Overloaded. Logs a PWideChar Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The PWideChar value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchPWideChar method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

{$IFNDEF DELPHI2009_OR_HIGHER}
    procedure Watch(const AName: UnicodeString;
      const AValue: PWideChar); overload;
{$ENDIF}

    // <summary>
    //   Overloaded. Logs a PWideChar Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The PWideChar value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchPWideChar method.</remarks>

{$IFNDEF DELPHI2009_OR_HIGHER}
    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: PWideChar); overload;
{$ENDIF}

    // <summary>
    //   Overloaded. Logs an Integer Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Integer value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchInteger method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Integer);
      overload;

    // <summary>
    //   Overloaded. Logs an Integer Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Integer value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchInteger method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Integer); overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Cardinal value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchCardinal method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Cardinal);
      overload;

    // <summary>
    //   Overloaded. Logs a Cardinal Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Cardinal value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchCardinal method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Cardinal); overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Shortint value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchShortint method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Shortint);
      overload;

    // <summary>
    //   Overloaded. Logs a Shortint Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Shortint value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchShortint method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Shortint); overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Smallint value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchSmallint method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Smallint);
      overload;

    // <summary>
    //   Overloaded. Logs a Smallint Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Smallint value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchSmallint method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Smallint); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Word value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchByte method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Byte Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Word value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchByte method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Byte); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Word value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchWord method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs a Word Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Word value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchWord method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Word); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Int64 value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchInt64 method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Int64 Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Int64 value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchInt64 method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Int64); overload;

    // <summary>
    //   Overloaded. Logs an Extended Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Extended value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchExtended method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Extended);
      overload;

    // <summary>
    //   Overloaded. Logs an Extended Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Extended value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchExtended method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Extended); overload;

    // <summary>
    //   Overloaded. Logs a Boolean Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Boolean value to display as Watch value.
    // </param>
    // <remarks>
    //   This method just calls the WatchBoolean method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure Watch(const AName: UnicodeString; const AValue: Boolean);
      overload;

    // <summary>
    //   Overloaded. Logs a Boolean Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the Watch.</param>
    // <param name="AValue">
    //   The Boolean value to display as Watch value.
    // </param>
    // <remarks>This method just calls the WatchBoolean method.</remarks>

    procedure Watch(const ALevel: TSiLevel; const AName: UnicodeString;
      const AValue: Boolean); overload;

    // <summary>
    //   Clears all Log Entries in the Console.
    // </summary>

    procedure ClearLog;

    // <summary>
    //   Clears all Process Flow entries in the Console.
    // </summary>

    procedure ClearProcessFlow;

    // <summary>
    //   Clears all Watches in the Console.
    // </summary>

    procedure ClearWatches;

    // <summary>
    //   Clears all AutoViews in the Console.
    // </summary>

    procedure ClearAutoViews;

    // <summary>
    //   Resets the whole Console.
    // </summary>
    // <remarks>
    //   This method resets the whole Console, that means all Watches,
    //   Log Entries, Process Flow entries and AutoViews will be deleted.
    // </remarks>

    procedure ClearAll;

    // <summary>
    //   Overloaded. Logs a custom Log Entry with the default log level.
    // </summary>
    // <param name="ATitle">The title of the new Log Entry.</param>
    // <param name="ALogEntryType">The Log Entry type to use.</param>
    // <param name="AViewerId">The Viewer Id to use.</param>
    // <param name="AData">Optional data block which can be nil.</param>
    // <seealso cref="TSiLogEntry"/>
    // <remarks>
    //   This method is useful for implementing custom Log Entry
    //   methods. For example, if you want to display some information
    //   in a particular way in the Console, you can just create a
    //   simple method which formats the data in question correctly
    //   and logs them using this SendCustomLogEntry method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure SendCustomLogEntry(const ATitle: UnicodeString;
      const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId;
      const AData: TStream = nil); overload;

    // <summary>
    //   Overloaded. Logs a custom Log Entry with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title of the new Log Entry.</param>
    // <param name="ALogEntryType">The Log Entry type to use.</param>
    // <param name="AViewerId">The Viewer Id to use.</param>
    // <param name="AData">Optional data block which can be nil.</param>
    // <seealso cref="TSiLogEntry"/>
    // <remarks>
    //   This method is useful for implementing custom Log Entry
    //   methods. For example, if you want to display some information
    //   in a particular way in the Console, you can just create a
    //   simple method which formats the data in question correctly
    //   and logs them using this SendCustomLogEntry method.
    // </remarks>

    procedure SendCustomLogEntry(const ALevel: TSiLevel;
      const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
      const AViewerId: TSiViewerId; const AData: TStream = nil); overload;

    // <summary>
    //   Overloaded. Logs a custom Watch with the default log level.
    // </summary>
    // <param name="AName">The name of the new Watch.</param>
    // <param name="AValue">The value of the new Watch.</param>
    // <param name="AWatchType">The Watch type to use.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method is useful for implementing custom Watch methods.
    //   For example, if you want to track the status of an instance of
    //   a specific class, you can just create a simple method which
    //   extracts all necessary information about this instance and logs
    //   them using this SendCustomWatch method.
    //
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure SendCustomWatch(const AName, AValue: UnicodeString;
      const AWatchType: TSiWatchType); overload;

    // <summary>
    //   Overloaded. Logs a custom Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the new Watch.</param>
    // <param name="AValue">The value of the new Watch.</param>
    // <param name="AWatchType">The Watch type to use.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method is useful for implementing custom Watch methods.
    //   For example, if you want to track the status of an instance of
    //   a specific class, you can just create a simple method which
    //   extracts all necessary information about this instance and logs
    //   them using this SendCustomWatch method.
    // </remarks>

    procedure SendCustomWatch(const ALevel: TSiLevel;
      const AName, AValue: UnicodeString;
      const AWatchType: TSiWatchType); overload;

    // <summary>
    //   Overloaded. Logs a custom Control Command with the default
    //   log level.
    // </summary>
    // <param name="AControlCommandType">
    //   The Control Command type to use.
    // </param>
    // <param name="AData">Optional data block which can be nil.</param>
    // <seealso cref="TSiControlCommand"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure SendCustomControlCommand(
      const AControlCommandType: TSiControlCommandType;
      const AData: TStream = nil); overload;

    // <summary>
    //   Overloaded. Logs a custom Watch with a custom log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="AName">The name of the new Watch.</param>
    // <param name="AValue">The value of the new Watch.</param>
    // <param name="AWatchType">The Watch type to use.</param>
    // <seealso cref="TSiWatch"/>
    // <remarks>
    //   This method is useful for implementing custom Watch methods.
    //   For example, if you want to track the status of an instance of
    //   a specific class, you can just create a simple method which
    //   extracts all necessary information about this instance and logs
    //   them using this SendCustomWatch method.
    // </remarks>

    procedure SendCustomControlCommand(const ALevel: TSiLevel;
      const AControlCommandType: TSiControlCommandType;
      const AData: TStream = nil); overload;

    // <summary>
    //   Overloaded. Logs a custom Process Flow entry with the default
    //   log level.
    // </summary>
    // <param name="ATitle">The title of the new Process Flow entry.</param>
    // <param name="AProcessFlowType">The Process Flow type to use.</param>
    // <seealso cref="TSiProcessFlow"/>
    // <remarks>
    //   This method uses the <link TSmartInspect.DefaultLevel,
    //   default level> of the session's <link Parent, parent> as log
    //   level. For more information, please refer to the documentation
    //   of the <link TSmartInspect.DefaultLevel, DefaultLevel> property
    //   of the TSmartInspect class.
    // </remarks>

    procedure SendCustomProcessFlow(const ATitle: UnicodeString;
      const AProcessFlowType: TSiProcessFlowType); overload;

    // <summary>
    //   Overloaded. Logs a custom Process Flow entry with a custom
    //   log level.
    // </summary>
    // <param name="ALevel">The log level of this method call.</param>
    // <param name="ATitle">The title of the new Process Flow entry.</param>
    // <param name="AProcessFlowType">The Process Flow type to use.</param>
    // <seealso cref="TSiProcessFlow"/>

    procedure SendCustomProcessFlow(const ALevel: TSiLevel;
      const ATitle: UnicodeString;
      const AProcessFlowType: TSiProcessFlowType); overload;

    // <summary>
    //   Resets the session color to its default value.
    // </summary>
    // <remarks>
    //   The default color of a session is transparent.
    // </remarks>

    procedure ResetColor;

    // <summary>
    //   Represents the background color in the SmartInspect Console
    //   of this session.
    // </summary>
    // <remarks>
    //   The session color helps you to identify Log Entries from
    //   different sessions in the SmartInspect Console by changing the
    //   background color.
    // </remarks>

    property Color: TColor read FColor write FColor;

    // <summary>
    //   Represents the session name used for Log Entries.
    // </summary>
    // <remarks>
    //   The session name helps you to identify Log Entries from different
    //   sessions in the SmartInspect Console.
    // </remarks>

    property Name: UnicodeString read GetName write SetName;

    // <summary>
    //   Represents the parent of the session.
    // </summary>
    // <remarks>
    //   The parent of a session is a TSmartInspect instance. It is
    //   responsible for sending the packets to the Console or to write
    //   them to a file. If the <link TSmartInspect.Enabled, Enabled>
    //   property of the parent is false, then all logging methods of
    //   this class will return immediately and do nothing.
    // </remarks>

    property Parent: TSmartInspect read FParent;

    // <summary>
    //   Specifies if the session is currently active.
    // </summary>
    // <remarks>
    //   If this property is set to false, all logging methods of this
    //   class will return immediately and do nothing. Please note that
    //   the <link Parent, parent> of this session also needs to be
    //   <link TSmartInspect.Enabled, enabled> in order to log information.
    //
    //   This property is especially useful if you are using multiple
    //   sessions at once and want to deactivate a subset of these
    //   sessions. To deactivate all your sessions, you can use the
    //   <link TSmartInspect.Enabled, Enabled> property of the <link
    //   Parent, parent>.
    // </remarks>

    property Active: Boolean read FActive write FActive;

    // <summary>
    //   Represents the log level of this TSiSession object.
    // </summary>
    // <remarks>
    //   Each TSiSession object can have its own log level. A log
    //   message is only logged if its log level is greater than or equal
    //   to the log level of a session and the session Parent. Log levels
    //   can thus be used to limit the logging output to important
    //   messages only.
    // </remarks>

    property Level: TSiLevel read FLevel write FLevel;
  end;

  TSiSessionInfo = class(TObject)
  private
    FLevel: TSiLevel;
    FColor: TColor;
    FActive: Boolean;
    FName: UnicodeString;
    FHasLevel: Boolean;
    FHasColor: Boolean;
    FHasActive: Boolean;
  public
    property Color: TColor read FColor write FColor;
    property HasColor: Boolean read FHasColor write FHasColor;
    property Name: UnicodeString read FName write FName;
    property Active: Boolean read FActive write FActive;
    property HasActive: Boolean read FHasActive write FHasActive;
    property Level: TSiLevel read FLevel write FLevel;
    property HasLevel: Boolean read FHasLevel write FHasLevel;
  end;

  // <summary>
  //   Manages and configures TSiSession instances.
  // </summary>
  // <remarks>
  //   This class manages and configures a list of sessions. Sessions
  //   can be configured and added to the list with the Add method. To
  //   lookup a stored session, you can use Get. To remove an existing
  //   session from the list, call Delete.
  //
  //   Stored sessions will be reconfigured if LoadConfiguration has
  //   been called and contains corresponding session entries.
  // </remarks>
  // <threadsafety>
  //   This class is fully threadsafe.
  // </threadsafety>

  TSiSessionManager = class(TObject)
  private
    FCriticalSection: TCriticalSection;
    FSessionList: TObjectList;
    FSessionTable: TSiObjectHash;
    FSessionInfos: TSiObjectHash;
    FDefaults: TSiSessionDefaults;
    procedure LoadDefaults(const AConfig: TSiConfiguration);
    procedure LoadInfos(const AConfig: TSiConfiguration);
    function LoadInfo(const AName: UnicodeString;
      const AConfig: TSiConfiguration): TSiSessionInfo;
    procedure Configure(const ASession: TSiSession; const AName: UnicodeString);
    procedure Assign(const ASession: TSiSession; const AInfo: TSiSessionInfo);
  public

    // <summary>
    //   Creates and initializes a new TSiSessionManager instance.
    // </summary>

    constructor Create;

    // <summary>
    //   Overridden. Releases all resources and frees this object.
    // </summary>

    destructor Destroy; override;

    // <summary>
    //   Loads the configuration properties of this session manager.
    // </summary>
    // <param name="AConfig">
    //   The TSiConfiguration object to load the configuration from.
    // </param>
    // <remarks>
    //   This method loads the configuration of this session manager
    //   from the passed TSiConfiguration object. Sessions which have
    //   already been stored or will be added with Add will
    //   automatically configured with the new properties if the
    //   passed TSiConfiguration object contains corresponding session
    //   entries. Moreover, this method also loads the default session
    //   properties which will be applied to all sessions which are
    //   passed to Add.
    //
    //   Please see the TSmartInspect.LoadConfiguration method for
    //   details on how session entries and session defaults look
    //   like.
    // </remarks>

    procedure LoadConfiguration(const AConfig: TSiConfiguration);

    // <summary>
    //   Configures a passed TSiSession instance and optionally saves
    //   it for later access.
    // </summary>
    // <param name="ASession">
    //   The session to configure and to save for later access, if
    //   desired.
    // </param>
    // <param name="AStore">
    //   Indicates if the passed session should be stored for later
    //   access.
    // </param>
    // <remarks>
    //   This method configures the passed session with the default
    //   session properties as specified by the Defaults property.
    //   This default configuration can be overridden on a per-session
    //   basis by loading the session configuration with the
    //   LoadConfiguration method.
    //
    //   If the 'AStore' parameter is true, the passed session is stored
    //   for later access and can be retrieved with the Get method. To
    //   remove a stored session from the internal list, call Delete.
    //
    //   If this method is called multiple times with the same session
    //   name, then the Get method operates on the session which got
    //   added last. If the ASession parameter is nil, this method does
    //   nothing.
    // </remarks>

    procedure Add(const ASession: TSiSession; const AStore: Boolean);

    // <summary>
    //   Removes a session from the internal list of sessions.
    // </summary>
    // <param name="ASession">
    //   The session to remove from the lookup table of sessions. Not
    //   allowed to be nil.
    // </param>
    // <remarks>
    //   This method removes a session which has previously been added
    //   with the Add method. After this method returns, the Get method
    //   returns nil when called with the same session name unless a
    //   different session with the same name has been added.
    //
    //   This method does nothing if the supplied ASession argument is
    //   nil.
    // </remarks>

    procedure Delete(const ASession: TSiSession);

    // <summary>
    //   Updates an entry in the internal lookup table of sessions.
    // </summary>
    // <param name="ASession">
    //   The session whose name has changed and whose entry should
    //   be updated.
    // </param>
    // <param name="ATo">The new name of the session.</param>
    // <param name="AFrom">The old name of the session.</param>
    // <remarks>
    //   Once the name of a session has changed, this method is called
    //   to update the internal session lookup table. The 'ATo' argument
    //   specifies the new name and 'AFrom' the old name of the session.
    //   After this method returns, the new name can be passed to the
    //   Get method to lookup the supplied session.
    // </remarks>

    procedure Update(const ASession: TSiSession; const ATo, AFrom: UnicodeString);

    // <summary>
    //   Returns a previously added session.
    // </summary>
    // <param name="AName">
    //   The name of the session to lookup and return.
    // </param>
    // <returns>
    //   The requested session or nil if the session is unknown.
    // </returns>
    // <remarks>
    //   This method returns a session which has previously been
    //   added with the Add method and can be identified by the
    //   supplied name parameter. If the requested session is unknown,
    //   this method returns nil.
    //
    //   Note that the behavior of this method can be unexpected in
    //   terms of the result value if multiple sessions with the same
    //   name have been added. In this case, this method returns the
    //   session which got added last and not necessarily the session
    //   which you expect.
    //
    //   Adding multiple sessions with the same name should therefore
    //   be avoided.
    // </remarks>

    function Get(const AName: UnicodeString): TSiSession;

    // <summary>
    //   Gets the session associated with the specified session name.
    // </summary>
    // <param name="AName">
    //   The name of the session to lookup and return.
    // </param>
    // <returns>
    //   The requested session or nil if the session is unknown.
    // </returns>
    // <remarks>
    //   This property returns the session which has previously been
    //   added with the Add method and can be identified by the
    //   specified session name. If the specified session is unknown
    //   nil is returned. See Get for more information.
    // </remarks>

    property Items[const AName: UnicodeString]: TSiSession read Get; default;

    // <summary>
    //   Specifies the default property values for new sessions.
    // </summary>
    // <remarks>
    //   This property lets you specify the default property values
    //   for new sessions which will be passed to the Add method.
    //   Please see the Add method for details. For information about
    //   the available session properties, please refer to the
    //   documentation of the TSiSession class.
    // </remarks>

    property Defaults: TSiSessionDefaults read FDefaults;
  end;

const

  { TSiPacketType lookup table }
  CSiPacketTypeLookup: array[TSiPacketType] of Word =
  (
    4,    { ptLogEntry }
    1,    { ptControlCommand }
    5,    { ptWatch }
    6,    { ptProcessFlow }
    7     { ptLogHeader }
  );

  { TSiLogEntryType lookup tables }

  CSiLogEntryTypeLookup: array[TSiLogEntryType] of Integer =
  (
    0,    { ltSeparator }
    1,    { ltEnterMethod }
    2,    { ltLeaveMethod }
    3,    { ltResetCallstack }
    100,  { ltMessage }
    101,  { ltWarning }
    102,  { ltError }
    103,  { ltInternalError }
    104,  { ltComment }
    105,  { ltVariableValue }
    106,  { ltCheckpoint }
    107,  { ltDebug }
    108,  { ltVerbose }
    109,  { ltFatal }
    110,  { ltConditional }
    111,  { ltAssert }
    200,  { ltText }
    201,  { ltBinary }
    202,  { ltGraphic }
    203,  { ltSource }
    204,  { ltObject }
    205,  { ltWebContent }
    206,  { ltSystem }
    207,  { ltMemoryStatistic }
    208,  { ltDatabaseResult }
    209   { ltDatabaseStructure }
  );

  CSiProcessFlowTypeString: array[TSiProcessFlowType] of UnicodeString =
  (
    'EnterMethod',  { pfEnterMethod }
    'LeaveMethod',  { pfLeaveMethod }
    'EnterThread',  { pfEnterThread }
    'LeaveThread',  { pfLeaveThread }
    'EnterProcess', { pfEnterProcess }
    'LeaveProcess'  { pfLeaveProcess }
  );

  CSiWatchTypeString: array[TSiWatchType] of UnicodeString =
  (
    'Char',       { wtChar }
    'String',     { wtString }
    'Integer',    { wtInteger }
    'Float',      { wtFloat }
    'Boolean',    { wtBoolean }
    'Address',    { wtAddress }
    'Timestamp',  { wtTimestamp }
    'Object'      { wtObject }
  );

  CSiControlCommandTypeString: array[TSiControlCommandType] of UnicodeString =
  (
    'ClearLog',         { ccClearLog }
    'ClearWatches',     { ccClearWatches }
    'ClearAutoViews',   { ccClearAutoViews }
    'ClearAll',         { ccClearAll }
    'ClearProcessFlow'  { ccClearProcessFlow }
  );

  CSiLogEntryTypeString: array[TSiLogEntryType] of UnicodeString =
  (
    'Separator',        { ltSeparator }
    'EnterMethod',      { ltEnterMethod }
    'LeaveMethod',      { ltLeaveMethod }
    'ResetCallstack',   { ltResetCallstack }
    'Message',          { ltMessage }
    'Warning',          { ltWarning }
    'Error',            { ltError }
    'InternalError',    { ltInternalError }
    'Comment',          { ltComment }
    'VariableValue',    { ltVariableValue }
    'Checkpoint',       { ltCheckpoint }
    'Debug',            { ltDebug }
    'Verbose',          { ltVerbose }
    'Fatal',            { ltFatal }
    'Conditional',      { ltConditional }
    'Assert',           { ltAssert } 
    'Text',             { ltText }
    'Binary',           { ltBinary }
    'Graphic',          { ltGraphic }
    'Source',           { ltSource }
    'Object',           { ltObject }
    'WebContent',       { ltWebContent }
    'System',           { ltSystem }
    'MemoryStatistic',  { ltMemoryStatistic }
    'DatabaseResult',   { ltDatabaseResult }
    'DatabaseStructure' { ltDatabaseStructure }
  );

  { TSiViewerId lookup tables }

  CSiViewerIdLookup: array[TSiViewerId] of Integer =
  (
    -1,   { viNone }
    0,    { viTitle }
    1,    { viData }
    2,    { viList }
    3,    { viValueList }
    4,    { viInspector }
    5,    { viTable }
    100,  { viWeb }
    200,  { viBinary }
    300,  { viHtmlSource }
    301,  { viJavaScriptSource }
    302,  { viVbScriptSource }
    303,  { viPerlSource }
    304,  { viSqlSource }
    305,  { viIniSource }
    306,  { viPythonSource }
    307,  { viXmlSource }
    400,  { viBitmap }
    401,  { viJpeg }
    402,  { viIcon }
    403   { viMetafile }
  );

  CSiViewerIdString: array[TSiViewerId] of UnicodeString =
  (
    'None',              { viNone }
    'Title',             { viTitle }
    'Data',              { viData }
    'List',              { viList }
    'ValueList',         { viValueList }
    'Inspector',         { viInspector }
    'Table',             { viTable }
    'Web',               { viWeb }
    'Binary',            { viBinary }
    'HtmlSource',        { viHtmlSource }
    'JavaScriptSource',  { viJavaScriptSource }
    'VbScriptSource',    { viVbScriptSource }
    'PerlSource',        { viPerlSource }
    'SqlSource',         { viSqlSource }
    'IniSource',         { viIniSource }
    'PythonSource',      { viPythonSource }
    'XmlSource',         { viXmlSource }
    'Bitmap',            { viBitmap }
    'Jpeg',              { viJpeg }
    'Icon',              { viIcon }
    'Metafile'           { viMetafile }
  );

  { TSiLevel lookup table }
  CSiLevelString: array[TSiLevel] of UnicodeString =
  (
    'Debug',     { lvDebug }
    'Verbose',   { lvVerbose }
    'Message',   { lvMessage }
    'Warning',   { lvWarning }
    'Error',     { lvError }
    'Fatal',     { lvFatal }
    'Unknown'    { lvControl, sic! }
  );

  { TSiFileRotate lookup table}
  CSiFileRotateString: array[TSiFileRotate] of UnicodeString =
  (
    'None',      { frNone }
    'Hourly',    { frHourly }
    'Daily',     { frDaily }
    'Weekly',    { frWeekly }
    'Monthly'    { frMonthly }
  );

  { TSiSourceId lookup table }
  CSiSourceIdLookup: array[TSiSourceId] of TSiViewerId =
  (
    viHtmlSource,
    viJavaScriptSource,
    viVbScriptSource,
    viPerlSource,
    viSqlSource,
    viIniSource,
    viPythonSource,
    viXmlSource
  );

  { TSiGraphicId lookup table }
  CSiGraphicLookup: array[TSiGraphicId] of TSiViewerId =
  (
    viBitmap,
    viJpeg,
    viIcon,
    viMetafile
  );

  { Console marker color constants }
  mcTransparent: TColor = clWindow;
  mcColor1: TColor = $9AFFFF;
  mcColor2: TColor = $9AFFEB;
  mcColor3: TColor = $9AFFB2;
  mcColor4: TColor = $FFE49A;
  mcColor5: TColor = $FFC49A;
  mcColor6: TColor = $FF9AA2;
  mcColor7: TColor = $FF9AD5;
  mcColor8: TColor = $D99AFF;
  mcColor9: TColor = $9E9AFF;
  mcColor10: TColor = $9AC0FF;
  mcColor11: TColor = $9AD7FF;
  mcColor12: TColor = $9AEBFF;

implementation

var
  GSiProtocols: array of record
    Name: UnicodeString;
    Impl: TSiProtocolClass;
  end;

  GSiProtocolsSync: TCriticalSection;

type

  { Private type declarations. }

  TSiPacketHeader = packed record
    PacketType: Word;
    PacketSize: Integer;
  end;

  TSiLogEntryHeader = packed record
    LogEntryType: Integer;
    ViewerId: Integer;
    AppNameLength: Integer;
    SessionNameLength: Integer;
    TitleLength: Integer;
    HostNameLength: Integer;
    DataLength: Integer;
    ProcessId: Cardinal;
    ThreadId: Cardinal;
    Timestamp: TDateTime;
    Color: TColor;
  end;

  TSiControlCommandHeader = packed record
    ControlCommandType: Integer;
    DataLength: Integer;
  end;

  TSiWatchHeader = packed record
    NameLength: Integer;
    ValueLength: Integer;
    WatchType: Integer;
    Timestamp: TDateTime;
  end;

  TSiProcessFlowHeader = packed record
    ProcessFlowType: Integer;
    TitleLength: Integer;
    HostNameLength: Integer;
    ProcessId: Cardinal;
    ThreadId: Cardinal;
    Timestamp: TDateTime;
  end;

  TSiAppNameToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiSessionToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiTitleToken = class(TSiToken)
  public
    constructor Create; override;
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiHostNameToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiTimestampToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiLiteralToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiLogEntryTypeToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiViewerIdToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiThreadIdToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiProcessIdToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiLevelToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

  TSiColorToken = class(TSiToken)
  public
    function Expand(const ALogEntry: TSiLogEntry): UnicodeString; override;
  end;

var
  GIsUnicodePlatform: Boolean;

{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF DELPHI7_OR_HIGHER}
function SiGetLocaleFormatSettings: TFormatSettings;
begin
{$IFDEF DELPHI2011_OR_HIGHER}
  Result := TFormatSettings.Create(GetThreadLocale);
{$ELSE}
  GetLocaleFormatSettings(GetThreadLocale, Result);
{$ENDIF}
end;
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}

function SiFormat(const AFmt: UnicodeString;
  const AArgs: array of const): UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
var
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF DELPHI7_OR_HIGHER}
  { Threadsafe way to call format. }
  LFormatSettings := SiGetLocaleFormatSettings();
  {$IFDEF DELPHI2009_OR_HIGHER}
  Result := Format(AFmt, AArgs, LFormatSettings);
  {$ELSE}
  Result := WideFormat(AFmt, AArgs, LFormatSettings);
  {$ENDIF}
{$ELSE}
  Result := WideFormat(AFmt, AArgs);
{$ENDIF}
end;

function SiCreateFile(lpFileName: UnicodeString;
  dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle; var AErrorCode: Cardinal): THandle;
var
  LFileName: AnsiString;
begin
  if not GIsUnicodePlatform then
  begin
    LFileName := AnsiString(lpFileName);
    Result := CreateFileA(
      PAnsiChar(LFileName),
      dwDesiredAccess,
      dwShareMode,
      lpSecurityAttributes,
      dwCreationDisposition,
      dwFlagsAndAttributes,
      hTemplateFile
    );
  end else
  begin
    Result := CreateFileW(
      PWideChar(lpFileName),
      dwDesiredAccess,
      dwShareMode,
      lpSecurityAttributes,
      dwCreationDisposition,
      dwFlagsAndAttributes,
      hTemplateFile
    );
  end;
  AErrorCode := GetLastError;
end;

function SiGetUserName(lpBuffer: PWideChar; var nSize: DWORD): BOOL;
var
  LBuffer: AnsiString;
begin
  if not GIsUnicodePlatform then
  begin
    SetLength(LBuffer, nSize);
    Result := GetUserNameA(PAnsiChar(LBuffer), nSize);
    if Result then
    begin
      CopyMemory(lpBuffer, PWideChar(UnicodeString(LBuffer)),
        nSize * SizeOf(WideChar));
    end;
  end else
    Result := GetUserNameW(lpBuffer, nSize);
end;

function SiGetCurrentDirectory(nBufferLength: DWORD;
  lpBuffer: PWideChar): DWORD;
var
  LBuffer: AnsiString;
begin
  if not GIsUnicodePlatform then
  begin
    SetLength(LBuffer, nBufferLength);
    Result := GetCurrentDirectoryA(nBufferLength, PAnsiChar(LBuffer));
    if Result <> 0 then
    begin
      CopyMemory(lpBuffer, PWideChar(UnicodeString(LBuffer)),
        nBufferLength * SizeOf(WideChar));
    end;
  end else
    Result := GetCurrentDirectoryW(nBufferLength, lpBuffer);
end;

function SiStrScan(const AStr: PWideChar; const AChr: WideChar): PWideChar;
begin
  Result := AStr;
  while Result^ <> AChr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function SiLastDelimiter(const ADelimiters, AStr: UnicodeString): Integer;
var
  P: PWideChar;
begin
  Result := Length(AStr);
  P := PWideChar(ADelimiters);
  while Result > 0 do
  begin
    if AStr[Result] <> #0 then
    begin
      if Assigned(SiStrScan(P, AStr[Result])) then
      begin
        Exit;
      end;
    end;
    Dec(Result);
  end;
end;

function SiExtractFileExt(const AFileName: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := SiLastDelimiter('.\:', AFileName);
  if (I > 0) and (AFileName[I] = '.') then
    Result := Copy(AFileName, I, MaxInt)
  else
    Result := '';
end;

function SiChangeFileExt(const AFileName, AExtension: UnicodeString):
  UnicodeString;
var
  I: Integer;
begin
  I := SiLastDelimiter('.\:', AFilename);
  if (I = 0) or (AFileName[I] <> '.') then
    I := MaxInt;
  Result := Copy(AFileName, 1, I - 1) + AExtension;
end;

function SiExtractFileName(const AFileName: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := SiLastDelimiter('\:', AFileName);
  Result := Copy(AFileName, I + 1, MaxInt);
end;

function SiExtractFileDir(const AFileName: UnicodeString): UnicodeString;
var
  I: Integer;
begin
  I := SiLastDelimiter('\:', AFileName);
  if (I > 1) and (AFileName[I] = '\') and
    not (AFileName[I] = '\') and not (AFileName[I] = ':') then
  begin
    Dec(I);
  end;
  Result := Copy(AFileName, 1, I);
end;

function SiLowerCase(const AString: UnicodeString): UnicodeString;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
  if GIsUnicodePlatform then
  begin
{$IFDEF DELPHI2009_OR_HIGHER}
    Result := LowerCase(AString)
{$ELSE}
    Result := WideLowerCase(AString)
{$ENDIF}
  end else
    Result := AnsiLowerCase(AString);
end;

function SiUpperCase(const AString: UnicodeString): UnicodeString;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
  if GIsUnicodePlatform then
  begin
{$IFDEF DELPHI2009_OR_HIGHER}
    Result := UpperCase(AString)
{$ELSE}
    Result := WideUpperCase(AString)
{$ENDIF}
  end else
    Result := AnsiUpperCase(AString);
end;

function SiTrim(const AString: UnicodeString): UnicodeString;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
  Result := Trim(AString);
end;

function SiUTF8Decode(const AString: TSiUTF8String): UnicodeString;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
{$IFDEF DELPHI2009_OR_HIGHER}
  Result := UTF8ToUnicodeString(AString);
{$ELSE}
  Result := UTF8Decode(AString);
{$ENDIF}
end;

function SiUTF8Encode(const AString: UnicodeString): TSiUTF8String;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
  Result := UTF8Encode(AString);
end;

function SiSameText(const S, T: UnicodeString): Boolean;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
{$IFDEF DELPHI2009_OR_HIGHER}
  Result := SameText(S, T);
{$ELSE}
  Result := WideSameText(S, T);
{$ENDIF}
end;

function SiSameStr(const S, T: UnicodeString): Boolean;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
{$IFDEF DELPHI2009_OR_HIGHER}
  Result := SameStr(S, T);
{$ELSE}
  Result := WideSameStr(S, T);
{$ENDIF}
end;

function SiCompareStr(const S, T: UnicodeString): Integer;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
{$IFDEF DELPHI2009_OR_HIGHER}
  Result := CompareStr(S, T);
{$ELSE}
  Result := WideCompareStr(S, T);
{$ENDIF}
end;

function SiCompareText(const S, T: UnicodeString): Integer;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
{$IFDEF DELPHI2009_OR_HIGHER}
  Result := CompareText(S, T);
{$ELSE}
  Result := WideCompareText(S, T);
{$ENDIF}
end;

function SiPos(const ASubString, AString: UnicodeString): Integer;
{$IFDEF DELPHI2005_OR_HIGHER}
inline;
{$ENDIF}
begin
  Result := Pos(ASubString, AString);
end;

function SiLastPos(const ASubString, AString: UnicodeString): Integer;
var
  I: Integer;
  J: Integer;
begin
  Result := 0;
  I := Length(AString);
  J := Length(ASubString);

  { No match if the substring is longer }
  if J > I then
  begin
    Exit;
  end;

  { If both strings have the same length, compare then directly }
  if J = I then
  begin
    if SiCompareStr(AString, ASubString) = 0 then
    begin
      Result := 1;
    end;
    Exit;
  end;

  { Otherwise, loop backwards and try to find the substring }
  while I >= 1 do
  begin
    if AString[I] = ASubString[J] then
    begin
      Dec(J);
    end else
    begin
      J := Length(ASubString); { Reset }
    end;

    if J = 0 then
    begin
      Result := I;  { Found substring }
      Break;
    end;

    Dec(I); { "Next" char }
  end;
end;

function SiStringReplace(const AString: UnicodeString;
  const AOldPattern, ANewPattern: UnicodeString): UnicodeString;
var
  LSearch: UnicodeString;
  LOffset: Integer;
begin
  LSearch := AString;
  SetLength(Result, 0);
  while LSearch <> '' do
  begin
    LOffset := SiPos(AOldPattern, LSearch);
    if LOffset = 0 then
    begin
      Result := Result + LSearch;
      Break;
    end;
    Result := Result + Copy(LSearch, 1, LOffset - 1) + ANewPattern;
    LSearch := Copy(LSearch, LOffset + Length(AOldPattern), MaxInt);
  end;
end;

function SiFileAge(const AFileName: UnicodeString;
  var AAge: TDateTime): Boolean;
var
  LHandle: THandle;
  LFileName: AnsiString;
  LDataA: TWin32FindDataA;
  LDataW: TWin32FindDataW;
  LFileTime: TFileTime;
  LSystemTime: TSystemTime;
  LAttributes: DWORD;
begin
  Result := False;

  if not GIsUnicodePlatform then
  begin
    LFileName := AnsiString(AFileName);
    LHandle := FindFirstFileA(PAnsiChar(LFileName), LDataA);

    if LHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;

    LFileTime := LDataA.ftLastWriteTime;
    LAttributes := LDataA.dwFileAttributes;
  end else
  begin
    LHandle := FindFirstFileW(PWideChar(AFileName), LDataW);

    if LHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;

    LFileTime := LDataW.ftLastWriteTime;
    LAttributes := LDataW.dwFileAttributes;
  end;

  Windows.FindClose(LHandle);

  if (LAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
  begin
    Result := True;
    FileTimeToSystemTime(LFileTime, LSystemTime);
    with LSystemTime do
    begin
      AAge := EncodeDate(wYear, wMonth, wDay) +
        EncodeTime(wHour, wMinute, wSecond, wMilliSeconds);
    end;
  end;
end;

function SiUtcNow: TDateTime;
var
  LSystemTime: TSystemTime;
begin
  GetSystemTime(LSystemTime);
  with LSystemTime do
  begin
    Result := EncodeDate(wYear, wMonth, wDay) +
      EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
  end;
end;

function SiDeleteFile(const AFileName: UnicodeString): Boolean;
var
  LFileName: AnsiString;
begin
  if not GIsUnicodePlatform then
  begin
    LFileName := AnsiString(AFileName);
    Result := DeleteFileA(PAnsiChar(LFileName));
  end else
    Result := DeleteFileW(PWideChar(AFileName));
end;

function SiFileOrDirExists(const AFileName: UnicodeString): Boolean;
var
  LFileName: AnsiString;
  LResultCode: Integer;
begin
  if not GIsUnicodePlatform then
  begin
    LFileName := AnsiString(AFileName);
    LResultCode := GetFileAttributesA(PAnsiChar(LFileName));
  end else
    LResultCode := GetFileAttributesW(PWideChar(AFileName));
  Result := LResultCode <> -1;
end;

procedure SiGetFiles(const AFileName: UnicodeString;
  const AFiles: TSiStringList);
var
  LFileName: AnsiString;
  LHandle: THandle;
  LDataA: TWin32FindDataA;
  LDataW: TWin32FindDataW;
  LEntry: UnicodeString;
  LDirName: UnicodeString;
begin
  LDirName := SiExtractFileDir(AFileName);

  if LDirName <> '' then
  begin
    if LDirName[Length(LDirName)] <> '\' then
    begin
      LDirName := LDirName + '\';
    end;
  end;

  if not GIsUnicodePlatform then
  begin
    LFileName := AnsiString(AFileName);
    LHandle := FindFirstFileA(PAnsiChar(LFileName), LDataA);

    if LHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;

    repeat
      LEntry := LDirName + UnicodeString(LDataA.cFileName);
      AFiles.Add(LEntry);
    until not FindNextFileA(LHandle, LDataA);
  end else
  begin
    LHandle := FindFirstFileW(PWideChar(AFileName), LDataW);

    if LHandle = INVALID_HANDLE_VALUE then
    begin
      Exit;
    end;

    repeat
      LEntry := LDirName + LDataW.cFileName;
      AFiles.Add(LEntry);
    until not FindNextFileW(LHandle, LDataW);
  end;

  Windows.FindClose(LHandle);
end;

procedure SiSplit(const AString: UnicodeString; const ASeparator: WideChar;
  const AResult: TSiStringList);
var
  LIndex: Integer;
  LString: UnicodeString;
  LValue: UnicodeString;
begin
  if AString <> '' then
  begin
    LString := AString;
    repeat
      LIndex := SiPos(ASeparator, LString);
      if LIndex > 0 then
      begin
        LValue := Copy(LString, 1, LIndex - 1);
        LString := Copy(LString, LIndex + 1, MaxInt);
        AResult.Add(LValue);
      end;
    until LIndex <= 0;
    AResult.Add(LString); { Final }
  end;
end;

const
  SiInvalidFileNameError = 'Invalid filename';
  SiKeyNotGivenError = 'No encryption key';
  SiKeyNotValidError = 'Invalid encryption key size';
  SiCaptionNotFoundError = 'No protocol found with the specified caption';
  SiIndexOutOfBoundsError = 'Index out of bounds';
  SiSocketClosedError = 'Socket connection has been unexpectedly closed';
  SiPipeClosedError = 'Pipe connection has been unexpectedly closed';
  SiInvalidSocketError = 'Invalid socket handle';
  SiConnectionEstablishedError = 'Connection already established';
  SiTimeoutError = 'Timed out while trying to connect';
  SiNoConnectionError = 'No socket connection established';

const
  SiVersion = '3.3.2.49';
  SiMaxBackupFileStorage = 5;
  SiBackupFileInstrumentation = '-backup';
  SiDefaultColor = clWindow;
  SiSessionPrefix : UnicodeString = 'session.';
  SiSessionDefaultActive = True;
  SiSessionDefaultLevel = lvDebug;
  SiSessionDefaultColor = SiDefaultColor;
  SiSchedulerBufferSize = $10;
  SiFileKeySize = 16;
  SiFileRotate = frNone;
  SiFileAppend = False;
  SiFileEncrypt = False;
  SiFileMaxSize = 0;
  SiFileBuffer = 0;
  SiFileFileName = 'log.sil';
  SiFileExtension = '.sil';
  SiTextFileName = 'log.txt';
  SiTextExtension = '.txt';
  SiTextPattern = '[%timestamp%] %level%: %title%';
  SiTextIndent = False;
  SiMemoryAsText = False;
  SiMemoryMaxSize = 2048;
  SiMemoryPattern = SiTextPattern;
  SiMemoryIndent = SiTextIndent;
  SiTcpHost = '127.0.0.1';
  SiTcpPort = 4228;
  SiTcpTimeout = 30000;
  SiClientBanner : AnsiString = 'SmartInspect Delphi Library v' + SIVERSION;
  SiPipeName = 'smartinspect';
  SiBufferSize = $2000;
  SiRotateDateTimeFormat = 'yyyy-mm-dd-hh-nn-ss';  { Do not change }
  SiRotateDateTimeSeparator = '-';  { Do not change }
  SiRotateDateTimeTokens = 6;  { Do not change }
  SiRotateAlreadyExistsSuffix = 'a';

var
  SiSILF: array[0..3] of Byte = ($53, $49, $4c, $46);
  SiSILE: array[0..3] of Byte = ($53, $49, $4c, $45);

{ TSiPacket }

procedure TSiPacket.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

constructor TSiPacket.Create;
begin
  FRefCount := 1;
  FLevel := lvMessage;
end;

destructor TSiPacket.Destroy;
begin
  if FThreadSafe then
  begin
    DeleteCriticalSection(FCriticalSection);
  end;
  inherited;
end;

procedure TSiPacket.Lock;
begin
  if FThreadSafe then
  begin
    EnterCriticalSection(FCriticalSection);
  end;
end;

procedure TSiPacket.Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
  begin
    Free;
  end;
end;

procedure TSiPacket.SetThreadSafe(const AValue: Boolean);
begin
  if AValue <> FThreadSafe then
  begin
    FThreadSafe := AValue;
    if AValue then
      InitializeCriticalSection(FCriticalSection)
    else
      DeleteCriticalSection(FCriticalSection);
  end;
end;

procedure TSiPacket.Unlock;
begin
  if FThreadSafe then
  begin
    LeaveCriticalSection(FCriticalSection);
  end;
end;

{ TSiLogEntry }

constructor TSiLogEntry.Create;
begin
  inherited Create;
end;

constructor TSiLogEntry.Create(const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
begin
  inherited Create;
  FLogEntryType := ALogEntryType;
  FViewerId := AViewerId;
  FColor := SiDefaultColor;
  FThreadId := GetCurrentThreadId;
  FProcessId := GetCurrentProcessId;
end;

function TSiLogEntry.GetSize: Integer;
begin
  Result :=
    SizeOf(TSiLogEntryHeader) +
    Length(FSessionName) +
    Length(FTitle) +
    Length(FAppName) +
    Length(FHostName);

  if Assigned(FData) then
  begin
    Inc(Result, FData.Size);
  end;
end;

destructor TSiLogEntry.Destroy;
begin
  if Assigned(FData) then
  begin
    FreeAndNil(FData);
  end;
  inherited;
end;

function TSiLogEntry.GetHasData: Boolean;
begin
  Result := Assigned(FData) and (FData.Size > 0);
end;

function TSiLogEntry.GetPacketType: TSiPacketType;
begin
  Result := ptLogEntry;
end;

procedure TSiLogEntry.SetData(const AValue: TStream);
begin
  if Assigned(AValue) then
  begin
    if not Assigned(FData) then
    begin
      FData := TMemoryStream.Create;
    end;
    FData.CopyFrom(AValue, 0);
  end else
  begin
    if Assigned(FData) then
    begin
      FData.Clear;
    end;
  end;
end;

function TSiLogEntry.GetData: TStream;
begin
  Result := FData;
end;

{ TSiControlCommand }

constructor TSiControlCommand.Create;
begin
  inherited Create;
end;

constructor TSiControlCommand.Create(
  const AControlCommandType: TSiControlCommandType);
begin
  inherited Create;
  FControlCommandType := AControlCommandType;
  Level := lvControl;
end;

function TSiControlCommand.GetSize: Integer;
begin
  Result := SizeOf(TSiControlCommandHeader);

  if Assigned(FData) then
  begin
    Inc(Result, FData.Size);
  end;
end;

function TSiControlCommand.GetData: TStream;
begin
  Result := FData;
end;

destructor TSiControlCommand.Destroy;
begin
  if Assigned(FData) then
  begin
    FreeAndNil(FData);
  end;
  inherited;
end;

function TSiControlCommand.GetHasData: Boolean;
begin
  Result := Assigned(FData) and (FData.Size > 0);
end;

function TSiControlCommand.GetPacketType: TSiPacketType;
begin
  Result := ptControlCommand;
end;

procedure TSiControlCommand.SetData(const AValue: TStream);
begin
  if Assigned(AValue) then
  begin
    if not Assigned(FData) then
    begin
      FData := TMemoryStream.Create;
    end;
    FData.CopyFrom(AValue, 0);
  end else
  begin
    if Assigned(FData) then
    begin
      FData.Clear;
    end;
  end;
end;

{ TSiWatch }

constructor TSiWatch.Create;
begin
  inherited Create;
end;

constructor TSiWatch.Create(const AWatchType: TSiWatchType);
begin
  inherited Create;
  FWatchType := AWatchType;
end;

function TSiWatch.GetSize: Integer;
begin
  Result :=
    SizeOf(TSiWatchHeader) +
    Length(FName) +
    Length(FValue);
end;

function TSiWatch.GetPacketType: TSiPacketType;
begin
  Result := ptWatch;
end;

{ TSiProcessFlow }

constructor TSiProcessFlow.Create;
begin
  inherited Create;
end;

constructor TSiProcessFlow.Create(const AProcessFlowType: TSiProcessFlowType);
begin
  inherited Create;
  FProcessFlowType := AProcessFlowType;
  FThreadId := GetCurrentThreadId;
  FProcessId := GetCurrentProcessId;
end;

function TSiProcessFlow.GetSize: Integer;
begin
  Result :=
    SizeOf(TSiProcessFlowHeader) +
    Length(FTitle) +
    Length(FHostName);
end;

function TSiProcessFlow.GetPacketType: TSiPacketType;
begin
  Result := ptProcessFlow;
end;

{ TSiViewerContext }

constructor TSiViewerContext.Create(const AViewerId: TSiViewerId);
begin
  FViewerId := AViewerId;
end;

{ TSiBinaryContext }

procedure TSiBinaryContext.AppendBytes(const AAddress: Pointer;
  const ASize: Integer);
begin
  if not Assigned(AAddress) then
  begin
    raise ESmartInspectError.Create('AAddress argument is not assigned');
  end else if ASize < 0 then
  begin
    raise ESmartInspectError.Create('ASize argument is negative');
  end;
  FData.Write(AAddress^, ASize);
end;

constructor TSiBinaryContext.Create(const AViewerId: TSiViewerId);
begin
  inherited Create(AViewerId);
  FData := TMemoryStream.Create;
end;

destructor TSiBinaryContext.Destroy;
begin
  FData.Free;
  inherited;
end;

function TSiBinaryContext.GetViewerData: TStream;
begin
  Result := FData;
end;

procedure TSiBinaryContext.LoadFromFile(const AFileName: UnicodeString);
var
  LStream: TStream;
begin
  LStream := TSiFileStream.Create(AFileName, GENERIC_READ,
    OPEN_EXISTING);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TSiBinaryContext.LoadFromStream(const AStream: TStream);
var
  LOldPosition: Int64;
begin
  if not Assigned(AStream) then
  begin
    raise ESmartInspectError.Create('AStream argument is not assigned');
  end;

  LOldPosition := AStream.Position;
  try
    ResetData;
    FData.LoadFromStream(AStream);
    FData.Position := FData.Size;
  finally
    AStream.Position := LOldPosition;
  end;
end;

procedure TSiBinaryContext.ResetData;
begin
  FData.Clear;
end;

{ TSiTextContext }

procedure TSiTextContext.AppendLine(const ALine: UnicodeString);
begin
  AppendText(EscapeLine(ALine));
  AppendText(#13#10);
end;

procedure TSiTextContext.AppendText(const AText: UnicodeString);
begin
  FBuilder.Append(AText);
end;

constructor TSiTextContext.Create(const AViewerId: TSiViewerId);
begin
  inherited;
  FBuilder := TSiStringBuilder.Create;
end;

destructor TSiTextContext.Destroy;
begin
  FreeAndNil(FBuilder);
  if Assigned(FData) then
  begin
    FreeAndNil(FData);
  end;
  inherited;
end;

function TSiTextContext.EscapeLine(const ALine: UnicodeString): UnicodeString;
begin
  // The default implementation does no escaping.
  Result := ALine;
end;

function TSiTextContext.GetViewerData: TStream;
const
  CSiBom: AnsiString = #$EF#$BB#$BF;
var
  LText: TSiUTF8String;
begin
  if not Assigned(FData) then
    FData := TMemoryStream.Create
  else
    FData.Size := 0; { Reset only }

  { Write BOM and Utf8 encoded text string}
  FData.Write(Pointer(CSiBom)^, Length(CSiBom));
  LText := SiUTF8Encode(FBuilder.Text);
  if LText <> '' then
  begin
    FData.Write(Pointer(LText)^, Length(LText));
  end;

  Result := FData;
end;

procedure TSiTextContext.InternalLoadFromStream(const AStream: TStream);
var
  LList: TSiStringList;
begin
  LList := TSiStringList.Create;
  try
    LList.LoadFromStream(AStream);
    LoadFromText(LList.Text);
  finally
    LList.Free;
  end;
end;

procedure TSiTextContext.LoadFromFile(const AFileName: UnicodeString);
var
  LStream: TStream;
begin
  LStream := TSiFileStream.Create(AFileName, GENERIC_READ,
    OPEN_EXISTING);
  try
    InternalLoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TSiTextContext.LoadFromStream(const AStream: TStream);
var
  LOldPosition: Int64;
begin
  if not Assigned(AStream) then
  begin
    raise ESmartInspectError.Create('AStream argument is not assigned');
  end;

  LOldPosition := AStream.Position;
  try
    AStream.Position := 0;
    InternalLoadFromStream(AStream);
  finally
    // Restore the original position.
    AStream.Position := LOldPosition;
  end;
end;

procedure TSiTextContext.LoadFromText(const AText: UnicodeString);
begin
  ResetData;
  AppendText(AText);
end;

procedure TSiTextContext.ResetData;
begin
  FBuilder.Clear;
end;

{ TSiDataViewerContext }

constructor TSiDataViewerContext.Create;
begin
  inherited Create(viData);
end;

{ TSiListViewerContext }

constructor TSiListViewerContext.Create;
begin
  inherited Create(viList);
end;

function TSiListViewerContext.EscapeLine(const ALine: UnicodeString): UnicodeString;
begin
  Result := EscapeLine(ALine, '');
end;

class function TSiListViewerContext.EscapeLine(const ALine,
  AToEscape: UnicodeString): UnicodeString;
var
  I: Integer;
  B, C: WideChar;
begin
  // TODO: Optimize me
  B := #0;
  SetLength(Result, 0);

  for I := 1 to Length(ALine) do
  begin
    C := ALine[I];
    if (C = #13) or (C = #10) then
    begin
      if (B <> #13) and (B <> #10) then
      begin
        // Newline characters need to be removed,
        // they would break the list format.
        Result := Result + ' ';
      end;
    end else if SiPos(C, AToEscape) > 0 then
    begin
      // The current character needs to be escaped as
      // well (with the \ character).
      Result := Result + '\' + C;
    end else
    begin
      // This character is valid, so just append it.
      Result := Result + C;
    end;
    B := C;
  end;
end;

{ TSiValueListViewerContext }

procedure TSiValueListViewerContext.AppendKeyValue(const AKey,
  AValue: UnicodeString);
begin
  AppendText(EscapeItem(AKey));
  AppendText('=');
  AppendText(EscapeItem(AValue));
  AppendText(#13#10);
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: Integer);
begin
  AppendKeyValue(AKey, IntToStr(AValue));
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: Cardinal);
begin
  AppendKeyValue(AKey, IntToStr(AValue));
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: Int64);
begin
  AppendKeyValue(AKey, IntToStr(AValue));
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: TDateTime);
var
  LValue: UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings := SiGetLocaleFormatSettings();
  LValue := DateTimeToStr(AValue, LFormatSettings);
{$ELSE}
  LValue := DateTimeToStr(AValue);
{$ENDIF}
  AppendKeyValue(AKey, LValue);
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: Extended);
var
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
  LValue: UnicodeString;
begin
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings := SiGetLocaleFormatSettings();
  LValue := FloatToStr(AValue, LFormatSettings);
{$ELSE}
  LValue := FloatToStr(AValue);
{$ENDIF}
  AppendKeyValue(AKey, LValue);
end;

procedure TSiValueListViewerContext.AppendKeyValue(const AKey: UnicodeString;
  const AValue: Boolean);
begin
  if AValue then
    AppendKeyValue(AKey, 'True')
  else
    AppendKeyValue(AKey, 'False');
end;

constructor TSiValueListViewerContext.Create;
begin
  inherited Create(viValueList);
end;

function TSiValueListViewerContext.EscapeItem(const AItem: UnicodeString):
  UnicodeString;
begin
  Result := EscapeLine(AItem, '\=');
end;

{ TSiInspectorViewerContext }

constructor TSiInspectorViewerContext.Create;
begin
  inherited Create(viInspector);
end;

function TSiInspectorViewerContext.EscapeItem(const AItem: UnicodeString):
  UnicodeString;
begin
  Result := EscapeLine(AItem, '\=[]');
end;

procedure TSiInspectorViewerContext.StartGroup(const AGroup: UnicodeString);
begin
  AppendText('[');
  AppendText(EscapeItem(AGroup));
  AppendText(']'#13#10);
end;

{ TSiTableViewerContext }

procedure TSiTableViewerContext.AddRowEntry(const AEntry: UnicodeString);
begin
  if FLineStart then
  begin
    FLineStart := False;
    AppendText(EscapeCSVEntry(AEntry));
  end else
  begin
    AppendText(', ');
    AppendText(EscapeCSVEntry(AEntry));
  end;
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: Int64);
begin
  AddRowEntry(IntToStr(AEntry));
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: Integer);
begin
  AddRowEntry(IntToStr(AEntry));
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: Cardinal);
begin
  AddRowEntry(IntToStr(AEntry));
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: Extended);
var
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
  LEntry: UnicodeString;
begin
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings := SiGetLocaleFormatSettings();
  LEntry := FloatToStr(AEntry, LFormatSettings);
{$ELSE}
  LEntry := FloatToStr(AEntry);
{$ENDIF}
  AddRowEntry(LEntry);
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: TDateTime);
var
  LEntry: UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings := SiGetLocaleFormatSettings();
  LEntry := DateTimeToStr(AEntry, LFormatSettings);
{$ELSE}
  LEntry := DateTimeToStr(AEntry);
{$ENDIF}
  AddRowEntry(LEntry);
end;

procedure TSiTableViewerContext.AddRowEntry(const AEntry: Boolean);
begin
  if AEntry then
    AddRowEntry('True')
  else
    AddRowEntry('False');
end;

procedure TSiTableViewerContext.AppendHeader(const AHeader: UnicodeString);
begin
  AppendLine(AHeader);
  AppendLine('');
end;

procedure TSiTableViewerContext.BeginRow;
begin
  FLineStart := True;
end;

constructor TSiTableViewerContext.Create;
begin
  inherited Create(viTable);
  FLineStart := True;
end;

procedure TSiTableViewerContext.EndRow;
begin
  AppendLine('');
end;

class function TSiTableViewerContext.EscapeCSVEntry(const AEntry: UnicodeString):
  UnicodeString;
var
  I: Integer;
  C: WideChar;
begin
  // TODO: Optimize me
  SetLength(Result, 0);
  for I := 1 to Length(AEntry) do
  begin
    C := AEntry[I];
    if (C = #13) or (C = #10) then
    begin
      // Newline characters need to be escaped,
      // they would break the csv format.
      Result := Result + ' ';
    end else if C = '"' then
    begin
      // '"' characters are used to surround entries
      // in the csv format, so they need to be escaped.
      Result := Result + '""';
    end else
    begin
      // This character is valid, so just append it.
      Result := Result + C;
    end;
  end;
  Result := '"' + Result + '"';
end;

{ TSiWebViewerContext }

constructor TSiWebViewerContext.Create;
begin
  inherited Create(viWeb);
end;

{ TSiSourceViewerContext }

constructor TSiSourceViewerContext.Create(const ASourceId: TSiSourceId);
begin
  inherited Create(CSiSourceIdLookup[ASourceId]);
end;

{ TSiBinaryViewerContext }

constructor TSiBinaryViewerContext.Create;
begin
  inherited Create(viBinary);
end;

{ TSiGraphicViewerContext }

constructor TSiGraphicViewerContext.Create(const AGraphicId: TSiGraphicId);
begin
  inherited Create(CSiGraphicLookup[AGraphicId]);
end;

{ TSiFileProtocol }

procedure TSiFileProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  inherited;
  ABuilder.AddOption('append', FAppend);
  ABuilder.AddOption('buffer', FIOBuffer div 1024);
  ABuilder.AddOption('filename', FFileName);
  ABuilder.AddOption('maxsize', FMaxSize div 1024);
  ABuilder.AddOption('maxparts', FMaxParts);
  ABuilder.AddOption('rotate', FRotate);

  { Do not add encrypt options for security }
end;

procedure TSiFileProtocol.InternalAfterConnect(
  const AFileName: UnicodeString);
var
  LFileDate: TDateTime;
begin
  if not IsRotating then
  begin
    Exit; { Nothing to do }
  end;

  if FRotate <> frNone then
  begin
    { We need to initialize the TSiFileRotater object with
      the creation time of the opened log file in order to
      be able to correctly rotate the log by date in
      InternalWritePacket. }
    LFileDate := TSiFileHelper.GetFileDate(FFileName,
      AFileName);
    FRotater.Initialize(LFileDate);
  end;

  if FMaxParts = 0 then { Unlimited log files }
  begin
    Exit;
  end;

  { Ensure that we have at most 'maxParts' files }
  TSiFileHelper.DeleteFiles(FFileName, FMaxParts);
end;

procedure TSiFileProtocol.InternalBeforeConnect;
begin
{$IFNDEF SI_DISABLE_ENCRYPT}
  { Validate encryption key first. We do this before opening
    the file since opening the file is unnecessary if we are
    in encryption mode but do not have a valid secret key. }
  if FEncrypt then
  begin
    if not Assigned(FKey) then
    begin
      RaiseException(SiKeyNotGivenError);
    end else
    begin
      if Length(FKey) <> SiFileKeySize then
      begin
        RaiseException(SiKeyNotValidError);
      end;
    end;
  end;
{$ENDIF}
end;

procedure TSiFileProtocol.InternalConnect;
begin
  InternalDoConnect(FAppend);
end;

constructor TSiFileProtocol.Create;
begin
  inherited;
  FRotater := TSiFileRotater.Create;
  LoadOptions; { Set default options }
end;

destructor TSiFileProtocol.Destroy;
begin
  FreeAndNil(FRotater);
  FreeAndNil(FFormatter);
  inherited;
end;

procedure TSiFileProtocol.InternalDoConnect(const AAppend: Boolean);
var
  LFileName: UnicodeString;
  LCreation: DWord;
{$IFNDEF SI_DISABLE_ENCRYPT}
  LIVector: OctetString;
{$ENDIF}
  LStream: TStream;
begin
  InternalBeforeConnect;

  { Expand the filename (include a timestamp) if we are in
    rotate mode. }
  if IsRotating then
    LFileName := TSiFileHelper.GetFileName(FFileName, AAppend)
  else
    LFileName := FFileName;

  if AAppend then
    LCreation := OPEN_ALWAYS
  else
    LCreation := CREATE_ALWAYS;

  { Open the destination file. We need to manually advance the
    file pointer to the end of the stream in case we open the
    file in 'append' mode. }
  FFileStream := TSiFileStream.Create(LFileName, GENERIC_WRITE,
    LCreation);
  FFileSize := FFileStream.Size;
  FFileStream.Position := FFileSize;

{$IFNDEF SI_DISABLE_ENCRYPT}
  if FEncrypt then
  begin
    { Prepend the encryption header. This header consists of
      the 'SILE' string to identify a log file as encrypted
      log file and the initialization vector with a length of
      16 bytes. }
    LIVector := GetIVector;
    FFileStream.WriteBuffer(SiSILE, Length(SiSILE));
    FFileStream.WriteBuffer(Pointer(LIVector)^, Length(LIVector));

    FCipher := TRijndael.Create(Pointer(FKey)^, Length(FKey));
    FCipher.Mode := cmCBC;
    FCipher.IVector := LIVector;

    { And then wrap the file stream. We need to store the
      cipher stream here in order to be able to free it later
      in the InternalDisconnect method. }
    FCipherStream := TEncryptStream.Create(FFileStream, FCipher);
    LStream := FCipherStream;
  end else
{$ENDIF}
    LStream := FFileStream;

  FStream := GetStream(LStream); { Possible custom stream }
  FFileSize := WriteHeader(FStream, FFileSize);

  if FIOBuffer > 0 then
  begin
    FBuffer := TSiBufferedStream.Create(FStream, FIOBuffer);
    FIOBufferCounter := 0; { Reset buffer counter }
  end else
    FBuffer := TSiBufferedStream.Create(FStream, SiBufferSize);

  InternalAfterConnect(LFileName);
end;

procedure TSiFileProtocol.InternalDisconnect;
begin
  if Assigned(FBuffer) then
  begin
    WriteFooter(FBuffer);
    FreeAndNil(FBuffer);
  end;

  if (FStream <> FFileStream)
{$IFNDEF SI_DISABLE_ENCRYPT}
    and (FStream <> FCipherStream)
{$ENDIF}
    then
    FreeAndNil(FStream)
  else
    FStream := nil; { Do not free }

{$IFNDEF SI_DISABLE_ENCRYPT}
  if Assigned(FCipherStream) then
  begin
    FCipherStream.Done;
    FreeAndNil(FCipherStream);
  end;

  FreeAndNil(FCipher);
{$ENDIF}

  FreeAndNil(FFileStream);
end;

function TSiFileProtocol.IsRotating: Boolean;
begin
  Result := (FRotate <> frNone) or (FMaxSize > 0); 
end;

function TSiFileProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  Result := (AOption = 'filename') or
            (AOption = 'buffer') or
            (AOption = 'append') or
{$IFNDEF SI_DISABLE_ENCRYPT}
            (AOption = 'encrypt') or
            (AOption = 'key') or
{$ENDIF}
            (AOption = 'maxsize') or
            (AOption = 'maxparts') or
            (AOption = 'rotate') or
            inherited IsValidOption(AOption);
end;

procedure TSiFileProtocol.LoadOptions;
begin
  inherited;

  FFileName := GetStringOption('filename', GetDefaultFileName);
  FAppend := GetBooleanOption('append', SiFileAppend);

  FIOBuffer := GetSizeOption('buffer', SiFileBuffer);
  FMaxSize := GetSizeOption('maxsize', SiFileMaxSize);
  FRotate := GetRotateOption('rotate', SiFileRotate);

  if (FMaxSize > 0) and (FRotate = frNone) then
    FMaxParts := GetIntegerOption('maxparts', 2)
  else
    FMaxParts := GetIntegerOption('maxparts', 0);

{$IFNDEF SI_DISABLE_ENCRYPT}
  FKey := GetBytesOption('key', SiFileKeySize, nil);
  FEncrypt := GetBooleanOption('encrypt', SiFileEncrypt);

  if FEncrypt then
  begin
    FAppend := False; { Not applicable }
  end;
{$ENDIF}

  FRotater.Mode := FRotate;
end;

procedure TSiFileProtocol.InternalWritePacket(const APacket: TSiPacket);
var
  LFormatter: TSiFormatter;
  LPacketSize: Integer;
begin
  LFormatter := GetFormatter;
  LPacketSize := LFormatter.Compile(APacket);

  if FRotate <> frNone then
  begin
    if FRotater.Update(SiUtcNow) then
    begin
      Rotate;
    end;
  end;

  if FMaxSize > 0 then
  begin
    Inc(FFileSize, LPacketSize);
    if FFileSize > FMaxSize then
    begin
      Rotate;
      if LPacketSize > FMaxSize then
      begin
        Exit;
      end;
      Inc(FFileSize, LPacketSize);
    end;
  end;

  LFormatter.Write(FBuffer);

  if FIOBuffer > 0 then
  begin
    Inc(FIOBufferCounter, LPacketSize);
    if FIOBufferCounter > FIOBuffer then
    begin
      FIOBufferCounter := 0;
      FBuffer.Flush;
    end;
  end else
    FBuffer.Flush;
end;

function TSiFileProtocol.GetDefaultFileName: UnicodeString;
begin
  Result := SiFileFileName;
end;

function TSiFileProtocol.GetFormatter: TSiFormatter;
begin
  if not Assigned(FFormatter) then
  begin
    FFormatter := TSiBinaryFormatter.Create;
  end;
  Result := FFormatter;
end;

{$IFNDEF SI_DISABLE_ENCRYPT}
function TSiFileProtocol.GetIVector: OctetString;
var
  LMD5: THash;
  LNow: TDateTime;
begin
  LNow := Now;
  LMD5 := TMD5.Create(LNow, SizeOf(LNow));
  try
    LMD5.Done(nil);
    Result := LMD5.Digest;
  finally
    LMD5.Free;
  end;
end;
{$ENDIF}

function TSiFileProtocol.GetName: UnicodeString;
begin
  Result := 'file';
end;

function TSiFileProtocol.GetStream(const AStream: TStream): TStream;
begin
  Result := AStream;
end;

function TSiFileProtocol.WriteHeader(const AStream: TStream;
  const ASize: Int64): Int64;
begin
  if ASize = 0 then
  begin
    AStream.WriteBuffer(SiSILF, Length(SiSILF));
    Result := Length(SiSILF);
  end else
    Result := ASize;
end;

procedure TSiFileProtocol.WriteFooter(const AStream: TStream);
begin

end;

{$IFNDEF SI_DISABLE_ENCRYPT}
procedure TSiFileProtocol.RaiseException(const AMessage: UnicodeString);
begin
  raise ESiProtocolError.Create(AMessage);
end;
{$ENDIF}

procedure TSiFileProtocol.Rotate;
begin
  InternalDisconnect;
  InternalDoConnect(False);
end;

{ TSiTcpProtocol }

procedure TSiTcpProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  inherited;
  ABuilder.AddOption('host', UnicodeString(FHost));
  ABuilder.AddOption('port', FPort);
  ABuilder.AddOption('timeout', FTimeout);
end;

procedure TSiTcpProtocol.InitializeTcpClient;
begin
  FreeAndNil(FTcpClient);
  FTcpClient := TSiTcpClient.Create(FHost, FPort);
end;

procedure TSiTcpProtocol.InternalConnect;
begin
  FTcpClient.Connect(FTimeout);
  FTcpClient.Timeout := FTimeout;
  FStream := TSiTcpClientStream.Create(FTcpClient);
  FStream.ReadLn;
  FStream.WriteLn(SiClientBanner);
  FBuffer := TSiBufferedStream.Create(FStream, SiBufferSize);
  InternalWriteLogHeader; { Write a log header }
end;

constructor TSiTcpProtocol.Create;
begin
  inherited;
  FFormatter := TSiBinaryFormatter.Create;
  LoadOptions; { Set default options }
end;

procedure TSiTcpProtocol.InternalDisconnect;
begin
  FTcpClient.Disconnect;
  FreeAndNil(FBuffer);
  FreeAndNil(FStream);
end;

function TSiTcpProtocol.GetName: UnicodeString;
begin
  Result := 'tcp';
end;

function TSiTcpProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  Result := (AOption = 'host') or
            (AOption = 'port') or
            (AOption = 'timeout') or
            inherited IsValidOption(AOption);
end;

procedure TSiTcpProtocol.LoadOptions;
begin
  inherited;
  FHost := AnsiString(GetStringOption('host', SiTcpHost));
  FPort := GetIntegerOption('port', SiTcpPort);
  FTimeout := GetIntegerOption('timeout', SiTcpTimeout);
  InitializeTcpClient;
end;

procedure TSiTcpProtocol.InternalWritePacket(const APacket: TSiPacket);
var
  LAnswer: array[1..2] of AnsiChar;
begin
  FFormatter.Format(APacket, FBuffer);
  FBuffer.Flush;
  if FBuffer.Read(LAnswer, SizeOf(LAnswer)) <> SizeOf(LAnswer) then
  begin
    raise ESmartInspectError.Create(SiSocketClosedError);
  end;
end;

destructor TSiTcpProtocol.Destroy;
begin
  FreeAndNil(FTcpClient);
  FreeAndNil(FFormatter);
  inherited;
end;

{ TSiProtocolFactory }

class function TSiProtocolFactory.GetProtocol(const AName,
  AOptions: UnicodeString): TSiProtocol;
var
  I: Integer;
  LName: UnicodeString;
begin
  Result := nil;
  LName := SiTrim(SiLowerCase(AName));

  GSiProtocolsSync.Enter;
  try
    for I := Low(GSiProtocols) to High(GSiProtocols) do
    begin
      if GSiProtocols[I].Name = LName then
      begin
        Result := GSiProtocols[I].Impl.Create;
        Break;
      end;
    end;
  finally
    GSiProtocolsSync.Leave;
  end;

  if not Assigned(Result) then
  begin
    raise ESmartInspectError.Create('Protocol "' + AName + '" not found');
  end;

  try
    Result.Initialize(AOptions);
  except
    on E: Exception do
    begin
      FreeAndNil(Result);
      raise;
    end;
  end;
end;

class procedure TSiProtocolFactory.RegisterProtocol(const AName: UnicodeString;
  const AImpl: TSiProtocolClass);
var
  I: Integer;
  LName: UnicodeString;
begin
  LName := SiTrim(SiLowerCase(AName));
  if (LName <> '') and Assigned(AImpl) then
  begin
    GSiProtocolsSync.Enter;
    try
      for I := Low(GSiProtocols) to High(GSiProtocols) do
      begin
        if GSiProtocols[I].Name = LName then
        begin
          GSiProtocols[I].Impl := AImpl;
          Exit;
        end;
      end;

      SetLength(GSiProtocols, Length(GSiProtocols) + 1);
      GSiProtocols[High(GSiProtocols)].Name := LName;
      GSiProtocols[High(GSiProtocols)].Impl := AImpl;
    finally
      GSiProtocolsSync.Leave;
    end;
  end;
end;

{ TSiProtocol }

procedure TSiProtocol.AddOption(ASender: TSiOptionsParser;
  AProtocol, AKey, AValue: UnicodeString);
begin
  if MapOption(AKey, AValue) then
  begin
    Exit; { See MapOption }
  end;

  if not IsValidOption(AKey) then
  begin
    { The option isn't supported by the protocol. }
    raise ESmartInspectError.CreateFmt('Option "%s" not available ' +
      'for protocol "%s"', [AKey, SiLowerCase(AProtocol)]);
  end;

  FOptions.Put(AKey, AValue);
end;

procedure TSiProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  { Asynchronous options }
  ABuilder.AddOption('async.enabled', FAsyncEnabled);
  ABuilder.AddOption('async.clearondisconnect', FAsyncClearOnDisconnect);
  ABuilder.AddOption('async.queue', FAsyncQueue div 1024);
  ABuilder.AddOption('async.throttle', FAsyncThrottle);

  { Backlog options }
  ABuilder.AddOption('backlog.enabled', FBacklogEnabled);
  ABuilder.AddOption('backlog.flushon', FBacklogFlushOn);
  ABuilder.AddOption('backlog.keepopen', FBacklogKeepOpen);
  ABuilder.AddOption('backlog.queue', FBacklogQueue div 1024);

  { General options }
  ABuilder.AddOption('level', FLevel);
  ABuilder.AddOption('reconnect', FReconnect);
  ABuilder.AddOption('reconnect.interval', FReconnectInterval);
  ABuilder.AddOption('caption', FCaption);
end;

procedure TSiProtocol.Connect;
begin
  FCriticalSection.Enter;
  try
    if FAsyncEnabled then
    begin
      if Assigned(FScheduler) then
      begin
        Exit; { Already running }
      end;

      StartScheduler;
      ScheduleConnect;
    end else
      ImplConnect;
  finally
    FCriticalSection.Leave;
  end;
end;

constructor TSiProtocol.Create;
begin
  FQueue := TSiPacketQueue.Create;
  FQueue.OnDelete := DeletePacket;
  FCriticalSection := TCriticalSection.Create;
  FEventLock := TCriticalSection.Create;
  FOptions := TSiLookupTable.Create;
end;

procedure TSiProtocol.CreateOptions(const AOptions: UnicodeString);
var
  LParser: TSiOptionsParser;
begin
  try
    LParser := TSiOptionsParser.Create;
    try
      LParser.Parse(GetName, AOptions, AddOption);
    finally
      LParser.Free;
    end;
  except
    RemoveOptions;
    raise;
  end;
end;

procedure TSiProtocol.DeletePacket(ASender: TSiPacketQueue;
  APacket: TSiPacket);
begin
  APacket.Release; { Decrement reference counter }
end;

destructor TSiProtocol.Destroy;
begin
  RemoveOptions;
  try
    Disconnect;
  except
    { Any possible exceptions which occur during the Disconnect
      operation are silently discarded. }
  end;
  FreeAndNil(FOptions);
  FreeAndNil(FEventLock);
  FreeAndNil(FCriticalSection);
  FreeAndNil(FQueue);
  inherited;
end;

procedure TSiProtocol.Disconnect;
begin
  FCriticalSection.Enter;
  try
    if FAsyncEnabled then
    begin
      if not Assigned(FScheduler) then
      begin
        Exit; { Not running }
      end;

      if FAsyncClearOnDisconnect then
      begin
        FScheduler.Clear;
      end;

      ScheduleDisconnect;
      StopScheduler;
    end else
      ImplDisconnect;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiProtocol.Dispatch(const ACommand: TSiProtocolCommand);
begin
  FCriticalSection.Enter;
  try
    if FAsyncEnabled then
    begin
      if not Assigned(FScheduler) then
      begin
        Exit; { Not running }
      end;

      ScheduleDispatch(ACommand);
    end else
      ImplDispatch(ACommand);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiProtocol.DoError(const AException: Exception);
var
  LOnError: TSiProtocolErrorEvent;
begin
  LOnError := OnError;
  if Assigned(LOnError) then
  begin
    LOnError(Self, AException);
  end;
end;

procedure TSiProtocol.ForwardPacket(const APacket: TSiPacket;
  const ADisconnect: Boolean);
begin
  if not FConnected then
  begin
    if not FKeepOpen then
    begin
      InternalConnect;
      FConnected := True;
      FFailed := False; { Success }
    end else
      Reconnect;
  end;

  if FConnected then
  begin
    APacket.Lock;
    try
      InternalWritePacket(APacket);
    finally
      APacket.Unlock;
    end;

    if ADisconnect then
    begin
      FConnected := False;
      InternalDisconnect;
    end;
  end;
end;

function TSiProtocol.GetBooleanOption(const AName: UnicodeString;
  const ADefaultValue: Boolean): Boolean;
begin
  Result := FOptions.GetBooleanValue(AName, ADefaultValue);
end;

function TSiProtocol.GetBytesOption(const AName: UnicodeString;
  const ASize: Integer; const ADefaultValue: TSiBytes): TSiBytes;
begin
  Result := FOptions.GetBytesValue(AName, ASize, ADefaultValue);
end;

function TSiProtocol.GetIntegerOption(const AName: UnicodeString;
  const ADefaultValue: Integer): Integer;
begin
  Result := FOptions.GetIntegerValue(AName, ADefaultValue);
end;

function TSiProtocol.GetLevelOption(const AName: UnicodeString;
  const ADefaultValue: TSiLevel): TSiLevel;
begin
  Result := FOptions.GetLevelValue(AName, ADefaultValue);
end;

function TSiProtocol.GetOnError: TSiProtocolErrorEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnError;
  finally
    FEventLock.Leave;
  end;
end;

function TSiProtocol.GetOptions: UnicodeString;
var
  LBuilder: TSiConnectionsBuilder;
begin
  LBuilder := TSiConnectionsBuilder.Create;
  try
    BuildOptions(LBuilder);
    Result := LBuilder.Connections;
  finally
    LBuilder.Free;
  end;
end;

function TSiProtocol.GetRotateOption(const AName: UnicodeString;
  const ADefaultValue: TSiFileRotate): TSiFileRotate;
begin
  Result := FOptions.GetRotateValue(AName, ADefaultValue);
end;

function TSiProtocol.GetSizeOption(const AName: UnicodeString;
  const ADefaultValue: Int64): Int64;
begin
  Result := FOptions.GetSizeValue(AName, ADefaultValue);
end;

function TSiProtocol.GetStringOption(const AName,
  ADefaultValue: UnicodeString): UnicodeString;
begin
  Result := FOptions.GetStringValue(AName, ADefaultValue);
end;

function TSiProtocol.GetTimespanOption(const AName: UnicodeString;
  const ADefaultValue: Cardinal): Cardinal;
begin
  Result := FOptions.GetTimespanValue(AName, ADefaultValue);
end;

procedure TSiProtocol.ImplConnect;
begin
  if not FConnected and FKeepOpen then
  begin
    try
      try
        InternalConnect;
        FConnected := True;
        FFailed := False; { Success }
      except
        Reset;
        raise;
      end;
    except
      on E: Exception do
      begin
        HandleException(E.Message);
      end;
    end;
  end;
end;

procedure TSiProtocol.ImplDisconnect;
begin
  if FConnected then
  begin
    try
      Reset;
    except
      on E: Exception do
      begin
        HandleException(E.Message);
      end;
    end;
  end else
    FQueue.Clear;
end;

procedure TSiProtocol.ImplDispatch(const ACommand: TSiProtocolCommand);
begin
  if FConnected then
  begin
    try
      InternalDispatch(ACommand);
    except
      on E: Exception do
      begin
        HandleException(E.Message);
      end;
    end;
  end;
end;

procedure TSiProtocol.ImplWritePacket(const APacket: TSiPacket);
var
  LSkip: Boolean;
  LPacket: TSiPacket;
begin
  if FConnected or FReconnect or not FKeepOpen then
  begin
    try
      try
        LSkip := False;

        if FBacklogEnabled then
        begin
          if (APacket.Level >= FBacklogFlushOn) and
           (APacket.Level <> lvControl) then
          begin
            LPacket := FQueue.Pop;
            while Assigned(LPacket) do
            begin
              try
                ForwardPacket(LPacket, False);
              finally
                LPacket.Release; { Decrement reference counter }
              end;
              LPacket := FQueue.Pop;
            end;
          end else
          begin
            APacket.AddRef; { Increment reference counter }
            FQueue.Push(APacket);
            LSkip := True;
          end;
        end;

        if not LSkip then
        begin
          ForwardPacket(APacket, not FKeepOpen);
        end;
      except
        Reset;
        raise;
      end;
    except
      on E: Exception do
      begin
        HandleException(E.Message);
      end;
    end;
  end;
end;

procedure TSiProtocol.Initialize(const AOptions: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    if not FInitialized then
    begin
      CreateOptions(AOptions);
      LoadOptions;
      FInitialized := True;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiProtocol.InternalDispatch(const ACommand: TSiProtocolCommand);
begin
  // Empty by default
end;

function TSiProtocol.InternalReconnect: Boolean;
begin
  InternalConnect;
  Result := True;
end;

function TSiProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  Result := (AOption = 'caption') or
            (AOption = 'level') or
            (AOption = 'reconnect') or
            (AOption = 'reconnect.interval') or
            (AOption = 'backlog.enabled') or
            (AOption = 'backlog.flushon') or
            (AOption = 'backlog.keepopen') or
            (AOption = 'backlog.queue') or
            (AOption = 'async.enabled') or
            (AOption = 'async.queue') or
            (AOption = 'async.throttle') or
            (AOption = 'async.clearondisconnect');
end;

procedure TSiProtocol.LoadOptions;
begin
  { General protocol options }
  FLevel := GetLevelOption('level', lvDebug);
  FCaption := GetStringOption('caption', GetName);
  FReconnect := GetBooleanOption('reconnect', False);
  FReconnectInterval := GetTimespanOption('reconnect.interval', 0);

  { Backlog protocol options }
  FBacklogEnabled := GetBooleanOption('backlog.enabled', False);
  FBacklogQueue := GetSizeOption('backlog.queue', 2048);
  FBacklogFlushOn := GetLevelOption('backlog.flushon', lvError);
  FBacklogKeepOpen := GetBooleanOption('backlog.keepopen', False);

  FQueue.Backlog := FBacklogQueue;
  FKeepOpen := not FBacklogEnabled or FBacklogKeepOpen;

  { Asynchronous protocol options }
  FAsyncEnabled := GetBooleanOption('async.enabled', False);
  FAsyncThrottle := GetBooleanOption('async.throttle', True);
  FAsyncQueue := GetSizeOption('async.queue', 2048);
  FAsyncClearOnDisconnect := GetBooleanOption('async.clearondisconnect',
    False);
end;

function TSiProtocol.MapOption(const AKey, AValue: UnicodeString): Boolean;
var
  LBacklog: Int64;
begin
  { This method is for backwards compatibility. In older versions
    the backlog options didn't have 'backlog.' prefix. This has been
    changed in version 3.0. This method does the mapping between the
    old and the new backlog options. }

  if AKey = 'backlog' then
  begin
    FOptions.Put(AKey, AValue);
    LBacklog := FOptions.GetSizeValue('backlog', 0);

    if LBacklog > 0 then
    begin
      FOptions.Add('backlog.enabled', 'true');
      FOptions.Add('backlog.queue', AValue);
    end else
    begin
      FOptions.Add('backlog.enabled', 'false');
      FOptions.Add('backlog.queue', '0');
    end;
    Result := True;
    Exit;
  end;

  if AKey = 'flushon' then
  begin
    FOptions.Put(AKey, AValue);
    FOptions.Add('backlog.flushon', AValue);
    Result := True;
    Exit;
  end;

  if AKey = 'keepopen' then
  begin
    FOptions.Put(AKey, AValue);
    FOptions.Add('backlog.keepopen', AValue);
    Result := True;
    Exit;
  end;

  Result := False;
end;

procedure TSiProtocol.HandleException(const AMessage: String);
var
  E: ESiProtocolError;
begin
  { Indicate that the last operation has failed. }
  FFailed := True;

  E := ESiProtocolError.Create(AMessage);
  E.ProtocolName := GetName;
  E.ProtocolOptions := GetOptions;

  if FAsyncEnabled then
  begin
    try
      DoError(E); { Notify event handlers }
    finally
      E.Free;
    end;
  end else
    raise E;
end;

procedure TSiProtocol.Reconnect;
begin
  if FReconnectInterval > 0 then
  begin
    if GetTickCount - FReconnectTickCount < FReconnectInterval then
    begin
      Exit; { The reconnect interval has not been reached }
    end;
  end;

  try
    if InternalReconnect then
    begin
      FConnected := True;
    end;
  except
    { Reconnect exceptions are not reported, but we
      need to record that the last connection attempt
      has failed (see below). }
  end;

  FFailed := not FConnected;

  if FFailed then
  begin
    try
      Reset;
    except
      { Ignored }
    end;
  end;
end;

procedure TSiProtocol.RemoveOptions;
begin
  FOptions.Clear;
end;

procedure TSiProtocol.Reset;
begin
  FQueue.Clear;
  FConnected := False;
  try
    InternalDisconnect;
  finally
    FReconnectTickCount := GetTickCount;
  end;
end;

procedure TSiProtocol.ScheduleConnect;
var
  LCommand: TSiSchedulerCommand;
begin
  LCommand := TSiSchedulerCommand.Create;
  try
    LCommand.Action := saConnect;
    FScheduler.Schedule(LCommand);
  finally
    LCommand.Release;
  end;
end;

procedure TSiProtocol.ScheduleDisconnect;
var
  LCommand: TSiSchedulerCommand;
begin
  LCommand := TSiSchedulerCommand.Create;
  try
    LCommand.Action := saDisconnect;
    FScheduler.Schedule(LCommand);
  finally
    LCommand.Release;
  end;
end;

procedure TSiProtocol.ScheduleDispatch(const ACommand: TSiProtocolCommand);
var
  LCommand: TSiSchedulerCommand;
begin
  LCommand := TSiSchedulerCommand.Create;
  try
    LCommand.Action := saDispatch;
    LCommand.State := ACommand;
    ACommand.AddRef; { Increment reference counter }

    if not FScheduler.Schedule(LCommand) then
    begin
      { Scheduling the protocol command failed, so decrement
        the reference counter accordingly. }
      ACommand.Release;
    end;
  finally
    LCommand.Release;
  end;
end;

procedure TSiProtocol.ScheduleWritePacket(const APacket: TSiPacket);
var
  LCommand: TSiSchedulerCommand;
begin
  LCommand := TSiSchedulerCommand.Create;
  try
    LCommand.Action := saWritePacket;
    LCommand.State := APacket;
    APacket.AddRef; { Increment reference counter }

    if not FScheduler.Schedule(LCommand) then
    begin
      { Scheduling the packet failed, so decrement the
        reference counter accordingly. }
      APacket.Release;
    end;
  finally
    LCommand.Release;
  end;
end;

procedure TSiProtocol.SetOnError(const AValue: TSiProtocolErrorEvent);
begin
  FEventLock.Enter;
  try
    FOnError := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSiProtocol.StartScheduler;
begin
  FScheduler := TSiScheduler.Create(Self);
  FScheduler.Threshold := FAsyncQueue;
  FScheduler.Throttle := FAsyncThrottle;
  FScheduler.Start;
end;

procedure TSiProtocol.StopScheduler;
begin
  FreeAndNil(FScheduler);
end;

procedure TSiProtocol.InternalWriteLogHeader;
var
  LLogHeader: TSiLogHeader;
begin
  LLogHeader := TSiLogHeader.Create;
  try
    LLogHeader.AppName := FAppName;
    LLogHeader.HostName := FHostName;
    InternalWritePacket(LLogHeader);
  finally
    LLogHeader.Release;
  end;
end;

procedure TSiProtocol.WritePacket(const APacket: TSiPacket);
begin
  if APacket.Level < FLevel then
  begin
    Exit;
  end;

  FCriticalSection.Enter;
  try
    if FAsyncEnabled then
    begin
      if not Assigned(FScheduler) then
      begin
        Exit; { Not running }
      end;

      ScheduleWritePacket(APacket);
    end else
      ImplWritePacket(APacket);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TSiFileStream }

constructor TSiFileStream.Create(const AFileName: UnicodeString;
  const AAccess: DWord; const ACreation: DWord);
var
  LHandle: Cardinal;
  LErrorCode: Cardinal;
begin
  LHandle := SiCreateFile(
    AFileName,                 // The name of the file
    AAccess,                   // Open for reading and/or writing
    FILE_SHARE_READ,           // Allow the Console to open the log
    nil,                       // Specify no security attributes
    ACreation,                 // Either append to, create or open
    FILE_ATTRIBUTE_NORMAL,     // No special file attributes required
    0,                         // No template file given
    LErrorCode);

  if LHandle = INVALID_HANDLE_VALUE then
  begin
    raise ESmartInspectError.Create(SysErrorMessage(LErrorCode));
  end;

  inherited Create(Integer(LHandle));
end;

destructor TSiFileStream.Destroy;
begin
  FileClose(FHandle);
  inherited;
end;

{ TSiBufferedStream }

constructor TSiBufferedStream.Create(const AStream: TStream;
  const ACapacity: Integer);
begin
  FStream := AStream;
  FCapacity := ACapacity;
  GetMem(FBuffer, FCapacity);
end;

destructor TSiBufferedStream.Destroy;
begin
  if Assigned(FBuffer) then
  begin
    Flush;
    FreeMem(FBuffer);
  end;
  inherited;
end;

procedure TSiBufferedStream.Flush;
begin
  if FCount > 0 then
  begin
    FStream.WriteBuffer(FBuffer^, FCount);
    FCount := 0;
  end;
end;

function TSiBufferedStream.Read(var ABuffer; ACount: Integer): Longint;
begin
  // We do not buffer Read calls.
  Result := FStream.Read(ABuffer, ACount);
end;

function TSiBufferedStream.Seek(AOffset: Integer; AOrigin: Word): Longint;
begin
  Flush;
  Result := FStream.Seek(AOffset, AOrigin);
end;

function TSiBufferedStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  if ACount >= FCapacity then
  begin
    Flush;
    // Write the supplied buffer directly to
    // the underlying stream, because the amount of
    // bytes doesn't fit in our internal buffer.
    Result := FStream.Write(ABuffer, ACount);
  end else
  begin
    if ACount > FCapacity - FCount then
    begin
      // The amount of bytes to write doesn't
      // fit in our internal buffer, so flush it
      // before we can copy some bytes to it.
      Flush;
    end;

    // Copy the desired amount of bytes to our internal buffer.
    CopyMemory(Pointer(Integer(FBuffer) + FCount), Pointer(@ABuffer), ACount);
    Inc(FCount, ACount);
    Result := ACount;
  end;
end;

{ TSiTcpClient }

function TSiTcpClient.BuildSocket: TSocket;
var
  LYes: Integer;
begin
  // Create the tcp socket and check for any errors.
  Result := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

  if Result = INVALID_SOCKET then
  begin
    // The socket couldn't be created properly,
    // so raise the last socket error.
    RaiseLastSocketError;
  end;

  LYes := 1;
  setsockopt(Result, SOL_SOCKET, TCP_NODELAY, PAnsiChar(@LYes),
    SizeOf(LYes));
end;

function TSiTcpClient.BuildSocketAddress(const AHostName: AnsiString;
  const APort: Word): TSockAddrIn;
var
  LAddress: Cardinal;
  LHostEnt: PHostEnt;
begin
  // Fill the common server details.
  ZeroMemory(@Result, SizeOf(Result));
  Result.sin_family := AF_INET; // IPv4
  Result.sin_port := htons(APort);

  LAddress := inet_addr(PAnsiChar(AHostName));
  if LAddress <> INADDR_NONE then
  begin
    // We've got an IP address, so there's no need
    // to do a DNS lookup.
    Result.sin_addr.S_addr := LAddress; // Fill address directly.
  end else
  begin
    // Do a DNS lookup.
    LHostEnt := gethostbyname(PAnsiChar(AHostName));

    if not Assigned(LHostEnt) then
    begin
      // The hostname couldn't be resolved.
      RaiseLastSocketError;
    end;

    Result.sin_addr.S_un_b.s_b1 := Ord(LHostEnt^.h_addr_list^[0]);
    Result.sin_addr.S_un_b.s_b2 := Ord(LHostEnt^.h_addr_list^[1]);
    Result.sin_addr.S_un_b.s_b3 := Ord(LHostEnt^.h_addr_list^[2]);
    Result.sin_addr.S_un_b.s_b4 := Ord(LHostEnt^.h_addr_list^[3]);
  end;
end;

procedure TSiTcpClient.ChangeBlockingMode(const ASocket: TSocket;
  const ABlocking: Boolean);
var
  LNonBlocking: DWord;
begin
  if ABlocking then
  begin
    // Socket should be set to normal blocking mode.
    LNonBlocking := 0;
  end else
  begin
    // Socket should be set to non blocking mode.
    LNonBlocking := 1;
  end;

  // Change blocking mode of the supplied socket.
  if ioctlsocket(ASocket, FIONBIO, LNonBlocking) = SOCKET_ERROR then
  begin
    // We couldn't change the blocking mode.
    RaiseLastSocketError;
  end;
end;

procedure TSiTcpClient.Connect(const ATimeout: Integer);
var
  LDestination: TSockAddrIn;
begin
  if FConnected then
  begin
    // The connection is already established. The
    // user needs to close the socket properly before
    // connecting to another destination.
    raise ESmartInspectError.Create(SiConnectionEstablishedError);
  end;
  LDestination := BuildSocketAddress(FHost, FPort);  
  try
    FSocket := BuildSocket;
    Timeout := SiTcpTimeout;

    // After creating the tcp socket, we can
    // try to connect now in a non-blocking way.
    InternalConnect(FSocket, @LDestination, ATimeout);
  except
    Reset;
    raise;
  end;
  FConnected := True;
end;

constructor TSiTcpClient.Create(const AHost: AnsiString; const APort: Word);
var
  LStatus: Integer;
  LWsaData: TWSAData;
begin
  // Initialize the WinSock library.
  LStatus := WSAStartup(MakeWord(2, 2), LWsaData);

  if LStatus <> 0 then
  begin
    // Initializing the WinSock library failed.
    raise ESmartInspectError.Create(SysErrorMessage(LStatus));
  end;

  FPort := APort;
  FHost := AHost;
  FSocket := INVALID_SOCKET;
  FTimeout := 0;
end;

destructor TSiTcpClient.Destroy;
begin
  if FConnected then
  begin
    // We are still connected, so disconnect
    // properly before this object will be freed.
    Disconnect;
  end;
  WSACleanup;
  inherited;
end;

procedure TSiTcpClient.Disconnect;
begin
  Reset;
end;

procedure TSiTcpClient.InternalConnect(const ASocket: TSocket;
  const ATo: PSockAddrIn; const ATimeout: Integer);
var
  LConnectResult: Integer;
begin
  ChangeBlockingMode(ASocket, False);
  try
    // Try a non-blocking connect to the destination.
    LConnectResult :=
      SiWinSock2.connect(ASocket, PSockAddr(ATo), SizeOf(ATo^));

    if LConnectResult = SOCKET_ERROR then
    begin
      // It is normal that the winsock connect function returns
      // SOCKET_ERROR if a socket is in non-blocking mode. This occurs
      // if the socket could not be connected immediately. But if the
      // last error isn't WSAEWOULDBLOCK we've got a real error.
      if WSAGetLastError <> WSAEWOULDBLOCK then
      begin
        // And indeed, we've got a real error, so
        // raise the last socket error accordingly.
        RaiseLastSocketError;
      end;
      // We are not connected yet, so we need to wait
      // until the socket connection has been established
      // or the timeout has been reached.
      WaitForConnect(ASocket, ATimeout);
    end;
  finally
    // Restore the normal blocking mode.
    ChangeBlockingMode(ASocket, True);
  end;
end;

procedure TSiTcpClient.RaiseLastSocketError;
begin
  // Raise the last socket error.
  raise ESmartInspectError.Create(SysErrorMessage(WSAGetLastError));
end;

function TSiTcpClient.Receive(var ABuffer; ALength: Integer): Integer;
var
  LRead: Integer;
  LCount: Integer;
  LLeft: Integer;
  LBuffer: Pointer;
begin
  if not FConnected then
  begin
    // The socket isn't connected.
    raise ESmartInspectError.Create(SiNoConnectionError);
  end;

  LRead := 0;
  LLeft := ALength;
  try
    // Try to receive the desired amount of bytes. We use
    // a loop here, because the recv method might fill less
    // bytes than expected/desired when executed only once.
    while LRead < ALength do
    begin
      // Receive as many bytes as we can.
      LBuffer := Pointer(Integer(Pointer(@ABuffer)) + LRead);
      LCount := SiWinSock2.recv(FSocket, LBuffer^, LLeft, 0);

      if LCount = SOCKET_ERROR then
      begin
        // The recv function returned an error.
        RaiseLastSocketError;
      end;

      if LCount = 0 then
      begin
        // A return value of 0 means that the socket connection
        // has been closed by the server. So leave this method.
        // The correct return value will be set below.
        Break;
      end;

      // Calculate new values.
      LRead := LRead + LCount;
      LLeft := ALength - LRead;
    end;
  except
    Reset;
    raise;
  end;
  Result := LRead;
end;

function TSiTcpClient.ReceiveLn: AnsiString;
var
  C: AnsiChar;
begin
  SetLength(Result, 0);
  repeat
    if Receive(C, 1) <> 1 then
    begin
      // The socket connection has been closed.
      raise ESmartInspectError.Create(SiSocketClosedError);
    end else
    begin
      if not (C in [#10, #13]) then
      begin
        // Append the new character, unless it
        // represents a newline (char #10 or #13).
        Result := Result + C;
      end;
    end;
  until C = #10;
end;

procedure TSiTcpClient.Reset;
begin
  FConnected := False;
  if FSocket <> INVALID_SOCKET then
  begin
    Shutdown(FSocket, SD_SEND);
    CloseSocket(FSocket);
    FSocket := INVALID_SOCKET;
  end;
end;

function TSiTcpClient.Send(const ABuffer; ALength: Integer): Integer;
var
  LSent: Integer;
  LCount: Integer;
  LLeft: Integer;
  LBuffer: Pointer;
begin
  if not FConnected then
  begin
    // The socket isn't connected.
    raise ESmartInspectError.Create(SiNoConnectionError);
  end;

  LSent := 0;
  LLeft := ALength;
  try
    // Try to send the desired amount of bytes. We use a
    // loop here, because the send function might send less
    // bytes than expected/desired when executed only once.
    while LSent < ALength do
    begin
      // Send as many bytes as we can.
      LBuffer := Pointer(Integer(Pointer(@ABuffer)) + LSent);
      LCount := SiWinSock2.Send(FSocket, LBuffer^, LLeft, 0);

      if LCount = SOCKET_ERROR then
      begin
        // The send function returned an error.
        RaiseLastSocketError;
      end;

      // Calculate new values.
      LSent := LSent + LCount;
      LLeft := ALength - LSent;
    end;
  except
    Reset;
    raise;
  end;
  Result := LSent;
end;

function TSiTcpClient.SendLn(const ALine: AnsiString): Integer;
var
  LLine: AnsiString;
begin
  LLine := ALine + #10;
  Result := Send(Pointer(LLine)^, Length(LLine));
end;

procedure TSiTcpClient.SetTimeout(const AValue: Integer);
begin
  if FSocket = INVALID_SOCKET then
  begin
    // We don't have a valid socket handle and
    // so we can't change the recv/send timeout.
    raise ESmartInspectError.Create(SiInvalidSocketError);
  end;

  if FTimeout <> AValue then
  begin
    FTimeout := AValue;
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, PAnsiChar(@AValue),
      SizeOf(AValue));
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, PAnsiChar(@AValue),
      SizeOf(AValue));
  end;
end;

procedure TSiTcpClient.WaitForConnect(const ASocket: TSocket;
  const ATimeout: Integer);
var
  LTimeout: Integer;
  LSelectResult: Integer;
  LReadSet, LWriteSet, LErrorSet: TFDSet;
  LError, LErrorLength: Integer;
  LTime: TTimeVal;
  PTime: PTimeVal;
begin
  FD_ZERO(LReadSet);
  FD_SET(ASocket, LReadSet);
  LWriteSet := LReadSet;
  LErrorSet := LReadSet;

  if ATimeout > 0 then
  begin
    if ATimeout >= 1000 then
    begin
      // Split ATimeout to seconds and microseconds correctly.
      LTimeout := ATimeout mod 1000;
      LTime.tv_sec := (ATimeout - LTimeout) div 1000;
      LTime.tv_usec := LTimeout * 1000;
    end else
    begin
      LTime.tv_sec := 0;
      LTime.tv_usec := ATimeout * 1000;
    end;
    PTime := @LTime;
  end else
  begin
    // A timeout <= 0 means, that we shouldn't use
    // a timeout at all.
    PTime := nil;
  end;

  // Now wait for a socket event using the supplied
  // timeout or forever if the ATimeout argument is <= 0.
  LSelectResult := select(ASocket + 1, @LReadSet, @LWriteSet,
    @LErrorSet, PTime);

  if LSelectResult = 0 then
  begin
    // The specified timeout has been reached before any
    // socket event occurred, so raise an exception here.
    raise ESmartInspectError.Create(SiTimeoutError);
  end;

  if LSelectResult = SOCKET_ERROR then
  begin
    // An error occurred while executing the select method.
    RaiseLastSocketError;
  end;

  // After the select function finished successfully, we need to
  // check which events and maybe errors occurred in the meantime.
  if FD_ISSET(ASocket, LReadSet) or FD_ISSET(ASocket, LWriteSet) or
    FD_ISSET(ASocket, LErrorSet) then
  begin
    LError := 0;
    LErrorLength := SizeOf(LError);

    // Now we need to check for any errors.
    if getsockopt(ASocket, SOL_SOCKET, SO_ERROR, PAnsiChar(@LError),
      LErrorLength) = SOCKET_ERROR then
    begin
      // The getsocktopt method failed to retrieve
      // the error status of the supplied socket.
      RaiseLastSocketError;
    end;

    if LError <> 0 then
    begin
      // An error occurred in the meantime.
      raise ESmartInspectError.Create(SysErrorMessage(LError));
    end;
  end else
  begin
    // This case shouldn't happen, because if no events have
    // occurred, then the select function should have returned 0.
    // But to be sure, lets raise an exception here.
    RaiseLastSocketError;
  end;
end;

{ TSiTcpClientStream }

constructor TSiTcpClientStream.Create(const ATcpClient: TSiTcpClient);
begin
  FTcpClient := ATcpClient;
end;

function TSiTcpClientStream.Read(var ABuffer; ACount: Integer): Longint;
begin
  Result := FTcpClient.Receive(ABuffer, ACount);
end;

function TSiTcpClientStream.ReadLn: AnsiString;
begin
  Result := FTcpClient.ReceiveLn;
end;

function TSiTcpClientStream.Seek(AOffset: Integer; AOrigin: Word): Longint;
begin
  Result := -1;
end;

function TSiTcpClientStream.Write(const ABuffer; ACount: Integer): Longint;
begin
  Result := FTcpClient.Send(ABuffer, ACount);
end;

function TSiTcpClientStream.WriteLn(const ALine: AnsiString): Integer;
begin
  Result := FTcpClient.SendLn(ALine);
end;

{ ESiLoadConnectionsError }

constructor ESiLoadConnectionsError.Create(const AFileName: UnicodeString;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FFileName := AFileName;
end;

{ ESiLoadConfigurationError }

constructor ESiLoadConfigurationError.Create(const AFileName: UnicodeString;
  const AMessage: String);
begin
  inherited Create(AMessage);
  FFileName := AFileName;
end;

{ TSmartInspect }

function TSmartInspect.AddSession(const ASessionName: UnicodeString;
  const AStore: Boolean): TSiSession;
begin
  Result := TSiSession.Create(Self, ASessionName);
  FSessions.Add(Result, AStore);
end;

procedure TSmartInspect.AddSession(const ASession: TSiSession);
begin
  FSessions.Add(ASession, True);
end;

procedure TSmartInspect.Connect;
var
  I: Integer;
begin
  I := 0;
  while I < FProtocols.Count do
  begin
    try
      TSiProtocol(FProtocols[I]).Connect;
    except
      on E: Exception do
      begin
        DoError(E);
      end;
    end;
    Inc(I);
  end;
end;

constructor TSmartInspect.Create(const AAppName: UnicodeString);
var
  LHostName: String;
  LHostNameLength: DWord;
begin
  FLevel := lvDebug;
  FDefaultLevel := lvMessage;

  FEventLock := TCriticalSection.Create;
  FCriticalSection := TCriticalSection.Create;

  FProtocols := TObjectList.Create;
  FAppName := AAppName;
  FEnabled := False;

  LHostnameLength := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(LHostname, LHostNameLength);

  if GetComputerName(PChar(LHostName), LHostNameLength) then
  begin
    SetLength(LHostName, LHostNameLength);
    FHostName := LHostName; { Store as Unicode string (implicit
      conversion from Ansi in Delphi versions before 2009) }
  end else
  begin
    { The GetComputerName function failed, so use an empty hostname. }
    SetLength(FHostName, 0);
  end;

  FSessions := TSiSessionManager.Create;
  FResolution := crStandard;
  FVariables := TSiProtocolVariables.Create;
end;

procedure TSmartInspect.CreateConnections(const AConnections: UnicodeString);
var
  LParser: TSiConnectionsParser;
begin
  FIsMultiThreaded := False; { See below }  
  try
    LParser := TSiConnectionsParser.Create;
    try
      LParser.Parse(FVariables.Expand(AConnections), AddConnection);
    finally
      LParser.Free;
    end;
  except
    on E: Exception do
    begin
      RemoveConnections;
      raise ESiInvalidConnectionsError.Create(E.Message);
    end;
  end;
end;

procedure TSmartInspect.AddConnection(ASender: TSiConnectionsParser;
  AProtocol, AOptions: UnicodeString);
var
  LProtocol: TSiProtocol;
begin
  LProtocol := TSiProtocolFactory.GetProtocol(AProtocol, AOptions);
  LProtocol.OnError := ProtocolError;
  FProtocols.Add(LProtocol);

  if LProtocol.Asynchronous then
  begin
    FIsMultiThreaded := True;
  end;

  LProtocol.AppName := FAppName;
  LProtocol.HostName := FHostName;
end;

procedure TSmartInspect.DeleteSession(const ASession: TSiSession);
begin
  FSessions.Delete(ASession);
end;

procedure TSmartInspect.UnsetVariable(const AKey: UnicodeString);
begin
  FVariables.Remove(AKey);
end;

destructor TSmartInspect.Destroy;
begin
  RemoveConnections;
  FreeAndNil(FVariables);
  FreeAndNil(FProtocols);
  FreeAndNil(FSessions);
  FreeAndNil(FEventLock);
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TSmartInspect.Disconnect;
var
  I: Integer;
begin
  I := 0;
  while I < FProtocols.Count do
  begin
    try
      TSiProtocol(FProtocols[I]).Disconnect;
    except
      on E: Exception do
      begin
        DoError(E);
      end;
    end;
    Inc(I);
  end;
end;

procedure TSmartInspect.Dispatch(const ACaption: UnicodeString;
  const AAction: Integer; const AState: TObject);
var
  LProtocol: TSiProtocol;
  LCommand: TSiProtocolCommand;
begin
  FCriticalSection.Enter;
  try
    try
      LProtocol := FindProtocol(ACaption);

      if not Assigned(LProtocol) then
      begin
        raise ESmartInspectError.Create(SiCaptionNotFoundError);
      end;

      LCommand := TSiProtocolCommand.Create(AAction, AState);
      try
        LProtocol.Dispatch(LCommand);
      finally
        LCommand.Release;
      end;
    except
      on E: Exception do
      begin
        DoError(E);
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSmartInspect.DoControlCommand(
  const AControlCommand: TSiControlCommand);
var
  LOnControlCommand: TSiControlCommandEvent;
begin
  LOnControlCommand := OnControlCommand;
  if Assigned(LOnControlCommand) then
  begin
    LOnControlCommand(Self, AControlCommand);
  end;
end;

procedure TSmartInspect.DoError(const AException: Exception);
var
  LOnError: TSiErrorEvent;
begin
  LOnError := OnError;
  if Assigned(LOnError) then
  begin
    LOnError(Self, AException);
  end;
end;

function TSmartInspect.DoFilter(const APacket: TSiPacket): Boolean;
var
  LOnFilter: TSiFilterEvent;
begin
  Result := False;
  LOnFilter := OnFilter;
  if Assigned(LOnFilter) then
  begin
    LOnFilter(Self, APacket, Result);
  end;
end;

procedure TSmartInspect.DoLogEntry(const ALogEntry: TSiLogEntry);
var
  LOnLogEntry: TSiLogEntryEvent;
begin
  LOnLogEntry := OnLogEntry;
  if Assigned(LOnLogEntry) then
  begin
    LOnLogEntry(Self, ALogEntry);
  end;
end;

procedure TSmartInspect.DoProcessFlow(const AProcessFlow: TSiProcessFlow);
var
  LOnProcessFlow: TSiProcessFlowEvent;
begin
  LOnProcessFlow := OnProcessFlow;
  if Assigned(LOnProcessFlow) then
  begin
    LOnProcessFlow(Self, AProcessFlow);
  end;
end;

procedure TSmartInspect.DoWatch(const AWatch: TSiWatch);
var
  LOnWatch: TSiWatchEvent;
begin
  LOnWatch := OnWatch;
  if Assigned(LOnWatch) then
  begin
    LOnWatch(Self, AWatch);
  end;
end;

function TSmartInspect.GetAppName: UnicodeString;
begin
  FCriticalSection.Enter;
  try
    Result := FAppName;
  finally
    FCriticalSection.Leave;
  end;
end;

function TSmartInspect.GetOnControlCommand: TSiControlCommandEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnControlCommand;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetOnError: TSiErrorEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnError;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetOnFilter: TSiFilterEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnFilter;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetOnLogEntry: TSiLogEntryEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnLogEntry;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetOnProcessFlow: TSiProcessFlowEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnProcessFlow;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetOnWatch: TSiWatchEvent;
begin
  FEventLock.Enter;
  try
    Result := FOnWatch;
  finally
    FEventLock.Leave;
  end;
end;

function TSmartInspect.GetSession(const ASessionName: UnicodeString): TSiSession;
begin
  Result := FSessions[ASessionName];
end;

function TSmartInspect.GetSessionDefaults: TSiSessionDefaults;
begin
  Result := FSessions.Defaults;
end;

function TSmartInspect.GetVariable(const AKey: UnicodeString): UnicodeString;
begin
  Result := FVariables.Get(AKey);
end;

procedure TSmartInspect.LoadConnections(const AFileName: UnicodeString;
  const ADoNotEnable: Boolean);
var
  LFound: Boolean;
  LConnections: UnicodeString;
begin
  LFound := False;
  try
    // Try to read the connections string.
    LFound := ReadConnections(AFileName, LConnections);
  except
    on E: Exception do
    begin
      // Catch exceptions while trying to read the
      // connections string and fire the error event.
      DoError(E);
    end;
  end;

  if LFound then
  begin
    FCriticalSection.Enter;
    try
      if TryConnections(LConnections) then
      begin
        if not ADoNotEnable then Enable;
      end;
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

function TSmartInspect.Now: TDateTime;
begin
  Result := TSiClock.Now(FResolution);
end;

class function TSmartInspect.ReadConnections(const AFileName: UnicodeString;
  var AConnections: UnicodeString): Boolean;
const
  CNotFound = 'A connections string doesn''t exist in the supplied file';
var
  LConfiguration: TSiConfiguration;
begin
  Result := False;
  try
    LConfiguration := TSiConfiguration.Create;
    try
      LConfiguration.LoadFromFile(AFileName);
      if LConfiguration.Contains('connections') then
      begin
        AConnections := LConfiguration.ReadString('connections', '');
        Result := True;
      end else
        raise ESmartInspectError.Create(CNotFound);
    finally
      LConfiguration.Free;
    end;
  except
    on E: Exception do
    begin
      raise ESiLoadConnectionsError.Create(AFileName, E.Message);
    end;
  end;
end;

procedure TSmartInspect.RemoveConnections;
begin
  Disconnect;
  FIsMultiThreaded := False; { See CreateConnections }
  FProtocols.Clear;
  SetLength(FConnections, 0);
end;

procedure TSmartInspect.ProcessPacket(const APacket: TSiPacket);
var
  I: Integer;
begin
  FCriticalSection.Enter;
  try
    I := 0;
    while I < FProtocols.Count do
    begin
      try
        TSiProtocol(FProtocols[I]).WritePacket(APacket);
      except
        on E: Exception do
        begin
          DoError(E);
        end;
      end;
      Inc(I);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSmartInspect.ProtocolError(ASender: TSiProtocol;
  AException: Exception);
begin
  DoError(AException);
end;

procedure TSmartInspect.SendControlCommand(
  const AControlCommand: TSiControlCommand);
begin
  { Initialize the control command for safe multi-threaded
    access only if this SmartInspect object has one or more
    connections which operate in asynchronous protocol mode.
    Also see CreateConnections. }

  if FIsMultiThreaded then
  begin
    AControlCommand.ThreadSafe := True;
  end;

  try
    if not DoFilter(AControlCommand) then
    begin
      ProcessPacket(AControlCommand);
      DoControlCommand(AControlCommand);
    end;
  except
    on E: Exception do
    begin
      DoError(E);
    end;
  end;
end;

procedure TSmartInspect.SendLogEntry(const ALogEntry: TSiLogEntry);
begin
  { Initialize the log entry packet for safe multi-threaded
    access only if this SmartInspect object has one or more
    connections which operate in asynchronous protocol mode.
    Also see CreateConnections. }

  if FIsMultiThreaded then
  begin
    ALogEntry.ThreadSafe := True;
  end;

  { Then fill the properties we are responsible for. }
  ALogEntry.AppName := AppName;
  ALogEntry.HostName := HostName;
  try
    if not DoFilter(ALogEntry) then
    begin
      ProcessPacket(ALogEntry);
      DoLogEntry(ALogEntry);
    end;
  except
    on E: Exception do
    begin
      DoError(E);
    end;
  end;
end;

procedure TSmartInspect.SendProcessFlow(const AProcessFlow: TSiProcessFlow);
begin
  { Initialize the process flow for safe multi-threaded
    access only if this SmartInspect object has one or more
    connections which operate in asynchronous protocol mode.
    Also see CreateConnections. }

  if FIsMultiThreaded then
  begin
    AProcessFlow.ThreadSafe := True;
  end;

  { Then fill the properties we are responsible for. }
  AProcessFlow.HostName := HostName;
  try
    if not DoFilter(AProcessFlow) then
    begin
      ProcessPacket(AProcessFlow);
      DoProcessFlow(AProcessFlow);
    end;
  except
    on E: Exception do
    begin
      DoError(E);
    end;
  end;
end;

procedure TSmartInspect.SendWatch(const AWatch: TSiWatch);
begin
  { Initialize the watch packet for safe multi-threaded
    access only if this SmartInspect object has one or more
    connections which operate in asynchronous protocol mode.
    Also see CreateConnections. }

  if FIsMultiThreaded then
  begin
    AWatch.ThreadSafe := True;
  end;

  try
    if not DoFilter(AWatch) then
    begin
      ProcessPacket(AWatch);
      DoWatch(AWatch);
    end;
  except
    on E: Exception do
    begin
      DoError(E);
    end;
  end;
end;

procedure TSmartInspect.SetAppName(const AValue: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    FAppName := AValue;
    UpdateProtocols;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSmartInspect.SetConnections(const AValue: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    ApplyConnections(AValue);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSmartInspect.SetEnabled(const AValue: Boolean);
begin
  FCriticalSection.Enter;
  try
    if AValue then
      Enable
    else
      Disable;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSmartInspect.SetOnControlCommand(
  const AValue: TSiControlCommandEvent);
begin
  FEventLock.Enter;
  try
    FOnControlCommand := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetOnError(const AValue: TSiErrorEvent);
begin
  FEventLock.Enter;
  try
    FOnError := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetOnFilter(const AValue: TSiFilterEvent);
begin
  FEventLock.Enter;
  try
    FOnFilter := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetOnLogEntry(const AValue: TSiLogEntryEvent);
begin
  FEventLock.Enter;
  try
    FOnLogEntry := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetOnProcessFlow(const AValue: TSiProcessFlowEvent);
begin
  FEventLock.Enter;
  try
    FOnProcessFlow := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetOnWatch(const AValue: TSiWatchEvent);
begin
  FEventLock.Enter;
  try
    FOnWatch := AValue;
  finally
    FEventLock.Leave;
  end;
end;

procedure TSmartInspect.SetVariable(const AKey, AValue: UnicodeString);
begin
  FVariables.Put(AKey, AValue)
end;

procedure TSmartInspect.ApplyConnections(const AConnections: UnicodeString);
begin
  RemoveConnections;
  CreateConnections(AConnections);
  FConnections := AConnections;

  if FEnabled then
  begin
    // This instance is currently enabled,
    // so we can try to connect now.
    Connect;
  end;
end;

function TSmartInspect.TryConnections(const AConnections: UnicodeString): Boolean;
begin
  Result := False;
  if AConnections <> '' then
  begin
    try
      ApplyConnections(AConnections);
      Result := True;
    except
      on E: Exception do
      begin
        DoError(E);
      end;
    end;
  end;
end;

procedure TSmartInspect.UpdateProtocols;
var
  I: Integer;
  LProtocol: TSiProtocol;
begin
  for I := 0 to FProtocols.Count - 1 do
  begin
    LProtocol := TSiProtocol(FProtocols[I]);
    LProtocol.AppName := FAppName;
    LProtocol.HostName := FHostName;
  end;
end;

procedure TSmartInspect.UpdateSession(const ASession: TSiSession;
  const ATo, AFrom: UnicodeString);
begin
  FSessions.Update(ASession, ATo, AFrom);
end;

class function TSmartInspect.Version: UnicodeString;
begin
  Result := SiVersion;
end;

procedure TSmartInspect.Disable;
begin
  if FEnabled then
  begin
    FEnabled := False;
    Disconnect;
  end;
end;

procedure TSmartInspect.Enable;
begin
  if not FEnabled then
  begin
    FEnabled := True;
    Connect;
  end;
end;

function TSmartInspect.FindProtocol(const ACaption: UnicodeString): TSiProtocol;
var
  I: Integer;
  LProtocol: TSiProtocol;
begin
  Result := nil;
  for I := 0 to FProtocols.Count - 1 do
  begin
    LProtocol := TSiProtocol(FProtocols[I]);
    if SiSameText(LProtocol.Caption, ACaption) then
    begin
      Result := LProtocol;
      Break;
    end;
  end;
end;

procedure TSmartInspect.LoadConfiguration(const AFileName: UnicodeString);
var
  LConfig: TSiConfiguration;
begin
  LConfig := TSiConfiguration.Create;
  try
    try
      try
        LConfig.LoadFromFile(AFileName);
      except
        on E: Exception do
        begin
          raise ESiLoadConfigurationError.Create(AFileName, E.Message);
        end;
      end;
    except
      on E: Exception do
      begin
        DoError(E);
        Exit;
      end;
    end;

    FCriticalSection.Enter;
    try
      ApplyConfiguration(LConfig);
    finally
      FCriticalSection.Leave;
    end;

    FSessions.LoadConfiguration(LConfig);
  finally
    LConfig.Free;
  end;
end;

procedure TSmartInspect.ApplyConfiguration(const AConfig: TSiConfiguration);
var
  LEnabled: Boolean;
  LConnections: UnicodeString;
begin
  if AConfig.Contains('appname') then
  begin
    FAppName := AConfig.ReadString('appname', FAppName);
  end;

  // The `enabled' configuration value needs to be handled special,
  // because its appearance and value have a direct impact on how
  // to treat the `connections' value and the order in which to
  // apply the values:
  //
  // If the `enabled' value is found, it is very important to
  // differentiate between the values true and false. If the
  // `enabled' value is false, the user obviously either wants
  // to disable this object or keep it disabled. To correctly
  // disable this SmartInspect instance, we need to do that before
  // the connections string is changed. Otherwise it can happen
  // that this SmartInspect instance temporarily uses the new
  // connections string (exactly in the case when it is already
  // enabled).
  //
  // Handling an `enabled' value of true is the other way round.
  // We cannot enable this SmartInspect instance before setting
  // the `connections' value, because this would cause this
  // SmartInspect instance to temporarily use its old connections
  // string.

  LConnections := AConfig.ReadString('connections', '');

  if AConfig.Contains('enabled') then
  begin
    LEnabled := AConfig.ReadBoolean('enabled', False);

    if LEnabled then
    begin
      TryConnections(LConnections);
      Enable;
    end else
    begin
      Disable;
      TryConnections(LConnections);
    end;
  end else
    TryConnections(LConnections);

  if AConfig.Contains('level') then
  begin
    FLevel := AConfig.ReadLevel('level', FLevel);
  end;

  if AConfig.Contains('defaultlevel') then
  begin
    FDefaultLevel := AConfig.ReadLevel('defaultlevel',
      FDefaultLevel);
  end;
end;

{ TSiSession }

constructor TSiSession.Create(const AParent: TSmartInspect;
  const ASessionName: UnicodeString);
begin
  if not Assigned(AParent) then
  begin
    raise ESmartInspectError.Create('AParent argument is not assigned');
  end;

  FParent := AParent;
  FName := ASessionName;
  FCheckpointCounter := 0;
  FColor := SiDefaultColor;
  FActive := True; // Active by default.

  FCriticalSection := TCriticalSection.Create;
  FCounter := TSiIntegerHash.Create;
  FCheckpoints := TSiIntegerHash.Create;
end;

procedure TSiSession.DecCounter(const AName: UnicodeString);
begin
  DecCounter(FParent.DefaultLevel, AName);
end;

procedure TSiSession.DecCounter(const ALevel: TSiLevel;
  const AName: UnicodeString);
var
  LValue: Integer;
begin
  if IsOn(ALevel) then
  begin
    LValue := UpdateCounter(AName, False);
    SendWatch(ALevel, AName, IntToStr(LValue), wtInteger);
  end;
end;

destructor TSiSession.Destroy;
begin
  FreeAndNil(FCheckpoints);
  FreeAndNil(FCounter);
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TSiSession.ResetColor;
begin
  Color := SiDefaultColor;
end;

procedure TSiSession.ResetCounter(const AName: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    FCounter.Remove(AName);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSession.SetName(const AValue: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    if FIsStored then
    begin
      FParent.UpdateSession(Self, AValue, FName);
    end;
    FName := AValue;
  finally
    FCriticalSection.Leave;
  end;
end;

function TSiSession.IsOn(const ALevel: TSiLevel): Boolean;
begin
  Result := FParent.Enabled and FActive and (ALevel >= FLevel) and
    (ALevel >= FParent.Level);
end;

function TSiSession.IsOn: Boolean;
begin
  Result := FParent.Enabled and FActive;
end;

procedure TSiSession.ExceptionHandler(ASender: TObject; AException: Exception);
begin
  if IsOn(lvError) then
  begin
    EnterMethod(lvError, 'ExceptionHandler');
    try
      LogException(AException);
    finally
      LeaveMethod(lvError, 'ExceptionHandler');
    end;
  end;
end;

function TSiSession.GetName: UnicodeString;
begin
  FCriticalSection.Enter;
  try
    Result := FName;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSession.LogSeparator;
begin
  LogSeparator(FParent.DefaultLevel);
end;

procedure TSiSession.LogSeparator(const ALevel: TSiLevel);
begin
  if IsOn(ALevel) then
  begin
    SendLogEntry(ALevel, '', ltSeparator, viNone);
  end;
end;

procedure TSiSession.ResetCallstack;
begin
  ResetCallstack(FParent.DefaultLevel);
end;

procedure TSiSession.ResetCallstack(const ALevel: TSiLevel);
begin
  if IsOn(ALevel) then
  begin
    SendLogEntry(ALevel, '', ltResetCallstack, viNone);
  end;
end;

procedure TSiSession.EnterMethod(const AMethodName: UnicodeString);
begin
  EnterMethod(FParent.DefaultLevel, AMethodName);
end;

procedure TSiSession.EnterMethod(const ALevel: TSiLevel;
  const AMethodName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    // Send two packets, one LogEntry and one ProcessFlow.
    SendLogEntry(ALevel, AMethodName, ltEnterMethod, viTitle);
    SendProcessFlow(ALevel, AMethodName, pfEnterMethod);
  end;
end;

procedure TSiSession.EnterMethod(const AMethodNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  EnterMethod(FParent.DefaultLevel, AMethodNameFmt, AArgs);
end;

procedure TSiSession.EnterMethod(const ALevel: TSiLevel;
  const AMethodNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      EnterMethod(ALevel, SiFormat(AMethodNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('EnterMethod: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.EnterMethod(const AInstance: TObject;
  const AMethodName: UnicodeString);
begin
  EnterMethod(FParent.DefaultLevel, AInstance, AMethodName);
end;

procedure TSiSession.EnterMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('EnterMethod: AInstance argument is not assigned');
    end else
    begin
      EnterMethod(ALevel, UnicodeString(AInstance.ClassName) + '.' +
        AMethodName);
    end;
  end;
end;

procedure TSiSession.EnterMethod(const AInstance: TObject;
  const AMethodNameFmt: UnicodeString; const AArgs: array of const);
begin
  EnterMethod(FParent.DefaultLevel, AInstance, AMethodNameFmt, AArgs);
end;

procedure TSiSession.EnterMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodNameFmt: UnicodeString;
  const AArgs: array of const);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('EnterMethod: AInstance argument is not assigned');
      Exit;
    end;

    try
      LTitle := UnicodeString(AInstance.ClassName) + '.' +
        SiFormat(AMethodNameFmt, AArgs);
      EnterMethod(ALevel, LTitle);
    except
      on E: Exception do
      begin
        LogInternalError('EnterMethod: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LeaveMethod(const AMethodName: UnicodeString);
begin
  LeaveMethod(FParent.DefaultLevel, AMethodName);
end;

procedure TSiSession.LeaveMethod(const ALevel: TSiLevel;
  const AMethodName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    // Send two packets, one Log Entry and one Process Flow entry.
    SendLogEntry(ALevel, AMethodName, ltLeaveMethod, viTitle);
    SendProcessFlow(ALevel, AMethodName, pfLeaveMethod);
  end;
end;

procedure TSiSession.LeaveMethod(const AMethodNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  LeaveMethod(FParent.DefaultLevel, AMethodNameFmt, AArgs);
end;

procedure TSiSession.LeaveMethod(const ALevel: TSiLevel;
  const AMethodNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      LeaveMethod(ALevel, SiFormat(AMethodNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LeaveMethod: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LeaveMethod(const AInstance: TObject;
  const AMethodName: UnicodeString);
begin
  LeaveMethod(FParent.DefaultLevel, AInstance, AMethodName);
end;

procedure TSiSession.LeaveMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('LeaveMethod: AInstance argument is not assigned');
    end else
    begin
      LeaveMethod(ALevel, UnicodeString(AInstance.ClassName) + '.' +
        AMethodName);
    end;
  end;
end;

procedure TSiSession.LeaveMethod(const AInstance: TObject;
  const AMethodNameFmt: UnicodeString; const AArgs: array of const);
begin
  LeaveMethod(FParent.DefaultLevel, AInstance, AMethodNameFmt, AArgs);
end;

procedure TSiSession.LeaveMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodNameFmt: UnicodeString;
  const AArgs: array of const);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('LeaveMethod: AInstance argument is not assigned');
      Exit;
    end;

    try
      LTitle := UnicodeString(AInstance.ClassName) + '.' +
        SiFormat(AMethodNameFmt, AArgs);
      LeaveMethod(ALevel, LTitle);
    except
      on E: Exception do
      begin
        LogInternalError('LeaveMethod: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.EnterThread(const AThreadName: UnicodeString);
begin
  EnterThread(FParent.DefaultLevel, AThreadName);
end;

procedure TSiSession.EnterThread(const ALevel: TSiLevel;
  const AThreadName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, AThreadName, pfEnterThread);
  end;
end;

procedure TSiSession.EnterThread(const AThreadNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  EnterThread(FParent.DefaultLevel, AThreadNameFmt, AArgs);
end;

procedure TSiSession.EnterThread(const ALevel: TSiLevel;
  const AThreadNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      EnterThread(ALevel, SiFormat(AThreadNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('EnterThread: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LeaveThread(const AThreadName: UnicodeString);
begin
  LeaveThread(FParent.DefaultLevel, AThreadName);
end;

procedure TSiSession.LeaveThread(const ALevel: TSiLevel;
  const AThreadName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, AThreadName, pfLeaveThread);
  end;
end;

procedure TSiSession.LeaveThread(const AThreadNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  LeaveThread(FParent.DefaultLevel, AThreadNameFmt, AArgs);
end;

procedure TSiSession.LeaveThread(const ALevel: TSiLevel;
  const AThreadNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      LeaveThread(ALevel, SiFormat(AThreadNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LeaveThread: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.EnterProcess;
begin
  EnterProcess(FParent.DefaultLevel);
end;

procedure TSiSession.EnterProcess(const ALevel: TSiLevel);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, FParent.AppName, pfEnterProcess);
    SendProcessFlow(ALevel, 'Main Thread', pfEnterThread);
  end;
end;

procedure TSiSession.EnterProcess(const AProcessName: UnicodeString);
begin
  EnterProcess(FParent.DefaultLevel, AProcessName);
end;

procedure TSiSession.EnterProcess(const ALevel: TSiLevel;
  const AProcessName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, AProcessName, pfEnterProcess);
    SendProcessFlow(ALevel, 'Main Thread', pfEnterThread);
  end;
end;

procedure TSiSession.EnterProcess(const AProcessNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  EnterProcess(FParent.DefaultLevel, AProcessNameFmt, AArgs);
end;

procedure TSiSession.EnterProcess(const ALevel: TSiLevel;
  const AProcessNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      EnterProcess(ALevel, SiFormat(AProcessNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('EnterProcess: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LeaveProcess;
begin
  LeaveProcess(FParent.DefaultLevel);
end;

procedure TSiSession.LeaveProcess(const ALevel: TSiLevel);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, 'Main Thread', pfLeaveThread);
    SendProcessFlow(ALevel, FParent.AppName, pfLeaveProcess);
  end;
end;

procedure TSiSession.LeaveProcess(const AProcessName: UnicodeString);
begin
  LeaveProcess(FParent.DefaultLevel, AProcessName);
end;

procedure TSiSession.LeaveProcess(const ALevel: TSiLevel;
  const AProcessName: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, 'Main Thread', pfLeaveThread);
    SendProcessFlow(ALevel, AProcessName, pfLeaveProcess);
  end;
end;

procedure TSiSession.LeaveProcess(const AProcessNameFmt: UnicodeString;
  const AArgs: array of const);
begin
  LeaveProcess(FParent.DefaultLevel, AProcessNameFmt, AArgs);
end;

procedure TSiSession.LeaveProcess(const ALevel: TSiLevel;
  const AProcessNameFmt: UnicodeString; const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      LeaveProcess(ALevel, SiFormat(AProcessNameFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LeaveProcess: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogColored(const AColor: TColor;
  const ATitle: UnicodeString);
begin
  LogColored(FParent.DefaultLevel, AColor, ATitle);
end;

procedure TSiSession.LogColored(const ALevel: TSiLevel;
  const AColor: TColor; const ATitle: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    SendLogEntry(ALevel, ATitle, ltMessage, viTitle, AColor, nil);
  end;
end;

procedure TSiSession.LogColored(const AColor: TColor;
  const ATitleFmt: UnicodeString; const AArgs: array of const);
begin
  LogColored(FParent.DefaultLevel, AColor, ATitleFmt, AArgs);
end;

procedure TSiSession.LogColored(const ALevel: TSiLevel;
  const AColor: TColor; const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(ALevel) then
  begin
    try
      LogColored(ALevel, AColor, SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogColored: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogConditional(const ACondition: Boolean;
  const ATitleFmt: UnicodeString; const AArgs: array of const);
begin
  LogConditional(FParent.DefaultLevel, ACondition, ATitleFmt, AArgs);
end;

procedure TSiSession.LogConditional(const ACondition: Boolean;
  const ATitle: UnicodeString);
begin
  LogConditional(FParent.DefaultLevel, ACondition, ATitle);
end;

procedure TSiSession.LogConditional(const ALevel: TSiLevel;
  const ACondition: Boolean; const ATitleFmt: UnicodeString;
  const AArgs: array of const);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if ACondition then
    begin
      try
        LTitle := SiFormat(ATitleFmt, AArgs);
        SendLogEntry(ALevel, LTitle, ltConditional, viTitle);
      except
        on E: Exception do
        begin
          LogInternalError('LogConditional: ' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TSiSession.LogConditional(const ALevel: TSiLevel;
  const ACondition: Boolean; const ATitle: UnicodeString);
begin
  if IsOn(ALevel) then
  begin
    if ACondition then
    begin
      try
        SendLogEntry(ALevel, ATitle, ltConditional, viTitle);
      except
        on E: Exception do
        begin
          LogInternalError('LogConditional: ' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TSiSession.LogDebug(const ATitle: UnicodeString);
begin
  if IsOn(lvDebug) then
  begin
    SendLogEntry(lvDebug, ATitle, ltDebug, viTitle);
  end;
end;

procedure TSiSession.LogDebug(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvDebug) then
  begin
    try
      LogDebug(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogDebug: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogVerbose(const ATitle: UnicodeString);
begin
  if IsOn(lvVerbose) then
  begin
    SendLogEntry(lvVerbose, ATitle, ltVerbose, viTitle);
  end;
end;

procedure TSiSession.LogVerbose(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvVerbose) then
  begin
    try
      LogVerbose(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogVerbose: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogMessage(const ATitle: UnicodeString);
begin
  if IsOn(lvMessage) then
  begin
    SendLogEntry(lvMessage, ATitle, ltMessage, viTitle);
  end;
end;

procedure TSiSession.LogMessage(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvMessage) then
  begin
    try
      LogMessage(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogMessage: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogWarning(const ATitle: UnicodeString);
begin
  if IsOn(lvWarning) then
  begin
    SendLogEntry(lvWarning, ATitle, ltWarning, viTitle);
  end;
end;

procedure TSiSession.LogWarning(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvWarning) then
  begin
    try
      LogWarning(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogWarning: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogError(const ATitle: UnicodeString);
begin
  if IsOn(lvError) then
  begin
    SendLogEntry(lvError, ATitle, ltError, viTitle);
  end;
end;

procedure TSiSession.LogError(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvError) then
  begin
    try
      LogError(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogError: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogFatal(const ATitle: UnicodeString);
begin
  if IsOn(lvFatal) then
  begin
    SendLogEntry(lvFatal, ATitle, ltFatal, viTitle);
  end;
end;

procedure TSiSession.LogFatal(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvFatal) then
  begin
    try
      LogFatal(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogFatal: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogInternalError(const ATitle: UnicodeString);
begin
  if IsOn(lvError) then
  begin
    SendLogEntry(lvError, ATitle, ltInternalError, viTitle);
  end;
end;

procedure TSiSession.LogInternalError(const ATitleFmt: UnicodeString;
  const AArgs: array of const);
begin
  if IsOn(lvError) then
  begin
    try
      LogInternalError(SiFormat(ATitleFmt, AArgs));
    except
      on E: Exception do
      begin
        LogInternalError('LogInternalError: ' + E.Message);
      end;
    end;
  end;
end;

procedure TSiSession.LogAssert(const ACondition: Boolean;
  const ATitle: UnicodeString);
begin
  if IsOn(lvError) then
  begin
    if not ACondition then
    begin
      SendLogEntry(lvError, ATitle, ltAssert, viTitle);
    end;
  end;
end;

procedure TSiSession.LogAssert(const ACondition: Boolean;
  const ATitleFmt: UnicodeString; const AArgs: array of const);
var
  LTitle: UnicodeString;
begin
  if IsOn(lvError) then
  begin
    if not ACondition then
    begin
      try
        LTitle := SiFormat(ATitleFmt, AArgs);
        SendLogEntry(lvError, LTitle, ltAssert, viTitle);
      except
        on E: Exception do
        begin
          LogInternalError('LogAssert: ' + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TSiSession.LogAssigned(const ATitle: UnicodeString;
  const ARef: Pointer);
begin
  LogAssigned(FParent.DefaultLevel, ATitle, ARef);
end;

procedure TSiSession.LogAssigned(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ARef: Pointer);
begin
  if IsOn(ALevel) then
  begin
    if Assigned(ARef) then
    begin
      LogMessage(ATitle + ': Assigned')
    end else
    begin
      LogMessage(ATitle + ': Not assigned')
    end;
  end;
end;

procedure TSiSession.AddCheckpoint;
begin
  AddCheckpoint(FParent.DefaultLevel);
end;

procedure TSiSession.AddCheckpoint(const ALevel: TSiLevel);
var
  LTitle: UnicodeString;
  LCounter: Integer;
begin
  if IsOn(ALevel) then
  begin
    LCounter := InterlockedIncrement(FCheckpointCounter);
    LTitle := SiFormat('Checkpoint #%d', [LCounter]);
    SendLogEntry(ALevel, LTitle, ltCheckpoint, viTitle);
  end;
end;

procedure TSiSession.AddCheckpoint(const AName, ADetails: UnicodeString);
begin
  AddCheckpoint(FParent.DefaultLevel, AName, ADetails);
end;

procedure TSiSession.AddCheckpoint(const ALevel: TSiLevel; const AName,
  ADetails: UnicodeString);
var
  LTitle: UnicodeString;
  LCounter: Integer;
begin
  if IsOn(ALevel) then
  begin
    FCriticalSection.Enter;
    try
      LCounter := FCheckpoints.Inc(AName);
    finally
      FCriticalSection.Leave;
    end;

    if ADetails <> '' then
      LTitle := SiFormat('%s #%d (%s)', [AName, LCounter, ADetails])
    else
      LTitle := SiFormat('%s #%d', [AName, LCounter]);

    SendLogEntry(ALevel, LTitle, ltCheckpoint, viTitle);
  end;
end;

procedure TSiSession.ResetCheckpoint;
begin
  FCheckpointCounter := 0;
end;

procedure TSiSession.ResetCheckpoint(const AName: UnicodeString);
begin
  FCriticalSection.Enter;
  try
    FCheckpoints.Remove(AName);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSession.LogBoolean(const AName: UnicodeString;
  const AValue: Boolean);
begin
  LogBoolean(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogBoolean(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AValue then
    begin
      LTitle := AName + ' = True';
    end else
    begin
      LTitle := AName + ' = False';
    end;
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogByte(const AName: UnicodeString; const AValue: Byte);
begin
  LogByte(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogByte(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Byte);
begin
  LogByte(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogChar(const AName: UnicodeString; const AValue: Char);
begin
  LogChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogChar(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Char);
begin
  LogString(ALevel, AName, AValue);
end;

procedure TSiSession.LogCurrency(const AName: UnicodeString;
  const AValue: Currency);
begin
  LogCurrency(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogCurrency(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Currency);
begin
  LogExtended(ALevel, AName, AValue);
end;

procedure TSiSession.LogDateTime(const AName: UnicodeString;
  const AValue: TDateTime);
begin
  LogDateTime(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogDateTime(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: TDateTime);
var
  LTitle: UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
  if IsOn(ALevel) then
  begin
{$IFDEF DELPHI7_OR_HIGHER}
    LFormatSettings := SiGetLocaleFormatSettings();
    LTitle := AName + ' = ' + DateTimeToStr(AValue, LFormatSettings);
{$ELSE}
    LTitle := AName + ' = ' + DateTimeToStr(AValue);
{$ENDIF}
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogDouble(const AName: UnicodeString; const AValue: Double);
begin
  LogDouble(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogDouble(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Double);
begin
  LogExtended(ALevel, AName, AValue);
end;

procedure TSiSession.LogExtended(const AName: UnicodeString;
  const AValue: Extended);
begin
  LogExtended(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogExtended(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Extended);
var
  LTitle: UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
  if IsOn(ALevel) then
  begin
{$IFDEF DELPHI7_OR_HIGHER}
    LFormatSettings := SiGetLocaleFormatSettings();
    LTitle := AName + ' = ' + FloatToStr(AValue, LFormatSettings);
{$ELSE}
    LTitle := AName + ' = ' + FloatToStr(AValue);
{$ENDIF}
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogInt64(const AName: UnicodeString; const AValue: Int64);
begin
  LogInt64(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogInt64(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Int64);
begin
  LogInt64(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogInteger(const AName: UnicodeString; const AValue: Integer);
begin
  LogInteger(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogInteger(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Integer);
begin
  LogInteger(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogCardinal(const AName: UnicodeString;
  const AValue: Cardinal);
begin
  LogCardinal(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogCardinal(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Cardinal);
begin
  LogCardinal(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogPChar(const AName: UnicodeString; const AValue: PChar);
begin
  LogPChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogPChar(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PChar);
begin
  LogString(ALevel, AName, AValue);
end;

procedure TSiSession.LogPointer(const AName: UnicodeString;
  const AValue: Pointer);
begin
  LogPointer(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogPointer(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Pointer);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    // %p cannot be used due to a bug in Delphi's Format
    LTitle := SiFormat('%s = $%.8x', [AName, Cardinal(AValue)]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogByte(const AName: UnicodeString; const AValue: Byte;
  const AIncludeHex: Boolean);
begin
  LogByte(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogByte(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Byte;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %u ($%.2x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %u', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogCardinal(const AName: UnicodeString;
  const AValue: Cardinal; const AIncludeHex: Boolean);
begin
  LogCardinal(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogCardinal(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Cardinal;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %u ($%.8x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %u', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogInt64(const AName: UnicodeString;
  const AValue: Int64; const AIncludeHex: Boolean);
begin
  LogInt64(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogInt64(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Int64;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %d ($%.16x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %d', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogInteger(const AName: UnicodeString;
  const AValue: Integer; const AIncludeHex: Boolean);
begin
  LogInteger(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogInteger(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Integer;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %d ($%.8x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %d', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogShortint(const AName: UnicodeString;
  const AValue: Shortint; const AIncludeHex: Boolean);
begin
  LogShortint(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogShortint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Shortint;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %d ($%.2x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %d', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogSmallint(const AName: UnicodeString;
  const AValue: Smallint; const AIncludeHex: Boolean);
begin
  LogSmallint(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogSmallint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Smallint;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %d ($%.4x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %d', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogWord(const AName: UnicodeString;
  const AValue: Word; const AIncludeHex: Boolean);
begin
  LogWord(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.LogWord(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Word;
  const AIncludeHex: Boolean);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LTitle := SiFormat('%s = %u ($%.4x)', [AName, AValue, AValue])
    else
      LTitle := SiFormat('%s = %u', [AName, AValue]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogPWideChar(const AName: UnicodeString;
  const AValue: PWideChar);
begin
  LogPWideChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogPWideChar(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PWideChar);
begin
  LogWideString(ALevel, AName, AValue);
end;

procedure TSiSession.LogShortint(const AName: UnicodeString;
  const AValue: Shortint);
begin
  LogShortint(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogShortint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Shortint);
begin
  LogShortint(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogSingle(const AName: UnicodeString; const AValue: Single);
begin
  LogSingle(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogSingle(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Single);
begin
  LogExtended(ALevel, AName, AValue);
end;

procedure TSiSession.LogSmallint(const AName: UnicodeString;
  const AValue: Smallint);
begin
  LogSmallint(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogSmallint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Smallint);
begin
  LogSmallint(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogString(const AName: UnicodeString; const AValue: String);
begin
  LogString(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogString(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: String);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    LTitle := AName + ' = ''' + AValue + '''';
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogWideString(const AName: UnicodeString;
  const AValue: WideString);
begin
  LogWideString(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogWideString(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: WideString);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    LTitle := AName + ' = ''' + AValue + '''';
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogWord(const AName: UnicodeString; const AValue: Word);
begin
  LogWord(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogWord(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Word);
begin
  LogWord(ALevel, AName, AValue, False);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Smallint);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Smallint);
begin
  LogSmallint(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Shortint);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Shortint);
begin
  LogShortint(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Int64);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Int64);
begin
  LogInt64(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Byte);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Byte);
begin
  LogByte(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Word);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Word);
begin
  LogWord(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Integer);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Integer);
begin
  LogInteger(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Cardinal);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Cardinal);
begin
  LogCardinal(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: String);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: String);
begin
  LogString(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString;
  const AValue: WideString);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: WideString);
begin
  LogWideString(ALevel, AName, AValue);
end;

{$IFNDEF DELPHI2009_OR_HIGHER}
procedure TSiSession.LogValue(const AName: UnicodeString;
  const AValue: PWideChar);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PWideChar);
begin
  LogPWideChar(ALevel, AName, AValue);
end;
{$ENDIF}

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: PChar);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PChar);
begin
  LogPChar(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Boolean);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Boolean);
begin
  LogBoolean(ALevel, AName, AValue);
end;

procedure TSiSession.LogValue(const AName: UnicodeString; const AValue: Extended);
begin
  LogValue(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.LogValue(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Extended);
begin
  LogExtended(ALevel, AName, AValue);
end;

procedure TSiSession.LogText(const ATitle, AText: UnicodeString);
begin
  LogText(FParent.DefaultLevel, ATitle, AText);
end;

procedure TSiSession.LogText(const ALevel: TSiLevel;
  const ATitle, AText: UnicodeString);
begin
  LogCustomText(ALevel, ATitle, AText, ltText, viData);
end;

procedure TSiSession.LogTextFile(const ATitle, AFileName: UnicodeString);
begin
  LogTextFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogTextFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltText, viData);
end;

procedure TSiSession.LogTextFile(const AFileName: UnicodeString);
begin
  LogTextFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogTextFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltText, viData);
end;

procedure TSiSession.LogTextStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogTextStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogTextStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltText, viData);
end;

procedure TSiSession.LogHtml(const ATitle, AHtml: UnicodeString);
begin
  LogHtml(FParent.DefaultLevel, ATitle, AHtml);
end;

procedure TSiSession.LogHtml(const ALevel: TSiLevel;
  const ATitle, AHtml: UnicodeString);
begin
  LogCustomText(ALevel, ATitle, AHtml, ltWebContent, viWeb);
end;

procedure TSiSession.LogHtmlFile(const AFileName: UnicodeString);
begin
  LogHtmlFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogHtmlFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltWebContent, viWeb);
end;

procedure TSiSession.LogHtmlFile(const ATitle, AFileName: UnicodeString);
begin
  LogHtmlFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogHtmlFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltWebContent, viWeb);
end;

procedure TSiSession.LogHtmlStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogHtmlStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogHtmlStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltWebContent, viWeb);
end;

procedure TSiSession.LogBinaryFile(const AFileName: UnicodeString);
begin
  LogBinaryFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogBinaryFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltBinary, viBinary);
end;

procedure TSiSession.LogBinaryFile(const ATitle, AFileName: UnicodeString);
begin
  LogBinaryFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogBinaryFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltBinary, viBinary);
end;

procedure TSiSession.LogBinaryStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogBinaryStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogBinaryStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltBinary, viBinary);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogBitmap(const ATitle: UnicodeString;
  const ABitmap: TBitmap);
begin
  LogBitmap(FParent.DefaultLevel, ATitle, ABitmap);
end;

procedure TSiSession.LogBitmap(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ABitmap: TBitmap);
var
  LStream: TMemoryStream;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(ABitmap) then
    begin
      LogInternalError('LogBitmap: ABitmap argument is not assigned');
    end else
    begin
      try
        LStream := TMemoryStream.Create;
        try
          ABitmap.SaveToStream(LStream);
          SendLogEntry(ALevel, ATitle, ltGraphic, viBitmap, Color,
            LStream);
        finally
          LStream.Free;
        end;
      except
        on E: Exception do
        begin
          LogInternalError('LogBitmap: ' + E.Message);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogBitmapFile(const AFileName: UnicodeString);
begin
  LogBitmapFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogBitmapFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltGraphic, viBitmap);
end;

procedure TSiSession.LogBitmapFile(const ATitle, AFileName: UnicodeString);
begin
  LogBitmapFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogBitmapFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltGraphic, viBitmap);
end;

procedure TSiSession.LogBitmapStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogBitmapStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogBitmapStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltGraphic, viBitmap);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogJpeg(const ATitle: UnicodeString;
  const AJpeg: TJpegImage);
begin
  LogJpeg(FParent.DefaultLevel, ATitle, AJpeg);
end;

procedure TSiSession.LogJpeg(const ALevel: TSiLevel; const ATitle: UnicodeString;
  const AJpeg: TJpegImage);
var
  LStream: TMemoryStream;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AJpeg) then
    begin
      LogInternalError('LogJpeg: AJpeg argument is not assigned');
    end else
    begin
      try
        LStream := TMemoryStream.Create;
        try
          AJpeg.SaveToStream(LStream);
          SendLogEntry(ALevel, ATitle, ltGraphic, viJpeg, Color, LStream);
        finally
          LStream.Free;
        end;
      except
        on E: Exception do
        begin
          LogInternalError('LogJpeg: ' + E.Message);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogJpegFile(const ATitle, AFileName: UnicodeString);
begin
  LogJpegFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogJpegFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltGraphic, viJpeg);
end;

procedure TSiSession.LogJpegFile(const AFileName: UnicodeString);
begin
  LogJpegFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogJpegFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltGraphic, viJpeg);
end;

procedure TSiSession.LogJpegStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogJpegStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogJpegStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltGraphic, viJpeg);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogIcon(const ATitle: UnicodeString;
  const AIcon: TIcon);
begin
  LogIcon(FParent.DefaultLevel, ATitle, AIcon);
end;

procedure TSiSession.LogIcon(const ALevel: TSiLevel; const ATitle: UnicodeString;
  const AIcon: TIcon);
var
  LStream: TMemoryStream;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AIcon) then
    begin
      LogInternalError('LogIcon: AIcon argument is not assigned');
    end else
    begin
      try
        LStream := TMemoryStream.Create;
        try
          AIcon.SaveToStream(LStream);
          SendLogEntry(ALevel, ATitle, ltGraphic, viIcon, Color, LStream);
        finally
          LStream.Free;
        end;
      except
        on E: Exception do
        begin
          LogInternalError('LogIcon: ' + E.Message);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogIconFile(const AFileName: UnicodeString);
begin
  LogIconFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogIconFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltGraphic, viIcon);
end;

procedure TSiSession.LogIconFile(const ATitle, AFileName: UnicodeString);
begin
  LogIconFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogIconFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltGraphic, viIcon);
end;

procedure TSiSession.LogIconStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogIconStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogIconStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltGraphic, viIcon);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogMetafile(const ATitle: UnicodeString;
  const AMetafile: TMetaFile);
begin
  LogMetafile(FParent.DefaultLevel, ATitle, AMetafile);
end;

procedure TSiSession.LogMetafile(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AMetafile: TMetaFile);
var
  LStream: TMemoryStream;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AMetafile) then
    begin
      LogInternalError('LogMetafile: AMetafile argument is not assigned');
    end else
    begin
      try
        LStream := TMemoryStream.Create;
        try
          AMetaFile.SaveToStream(LStream);
          SendLogEntry(ALevel, ATitle, ltGraphic, viMetafile, Color, LStream);
        finally
          LStream.Free;
        end;
      except
        on E: Exception do
        begin
          LogInternalError('LogMetafile: ' + E.Message);
        end;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogMetafileFile(const AFileName: UnicodeString);
begin
  LogMetafileFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogMetafileFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, AFileName, ltGraphic, viMetafile);
end;

procedure TSiSession.LogMetafileFile(const ATitle, AFileName: UnicodeString);
begin
  LogMetafileFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogMetafileFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltGraphic, viMetafile);
end;

procedure TSiSession.LogMetafileStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogMetafileStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogMetafileStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltGraphic, viMetafile);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogPicture(const ATitle: UnicodeString;
  const APicture: TPicture);
begin
  LogPicture(FParent.DefaultLevel, ATitle, APicture);
end;

procedure TSiSession.LogPicture(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const APicture: TPicture);
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(APicture) then
    begin
      LogInternalError('LogPicture: APicture argument is not assigned');
    end else
    begin
      LogGraphic(ALevel, ATitle, APicture.Graphic);
    end;
  end;
end;

procedure TSiSession.LogPictureFile(const AFileName: UnicodeString);
begin
  LogPictureFile(FParent.DefaultLevel, AFileName);
end;

procedure TSiSession.LogPictureFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString);
begin
  LogPictureFile(ALevel, AFileName, AFileName);
end;

procedure TSiSession.LogPictureFile(const ATitle, AFileName: UnicodeString);
begin
  LogPictureFile(FParent.DefaultLevel, ATitle, AFileName);
end;

procedure TSiSession.LogPictureFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString);
var
  LPicture: TPicture;
begin
  if IsOn(ALevel) then
  begin
    LPicture := TPicture.Create;
    try
      try
        LPicture.LoadFromFile(AFileName);
        LogGraphic(ALevel, ATitle, LPicture.Graphic);
      except
        on E: Exception do
        begin
          LogInternalError('LogPictureFile: ' + E.Message);
        end;
      end;
    finally
      LPicture.Free;
    end;
  end;
end;

procedure TSiSession.LogGraphic(const ATitle: UnicodeString;
  const AGraphic: TGraphic);
begin
  LogGraphic(FParent.DefaultLevel, ATitle, AGraphic);
end;

procedure TSiSession.LogGraphic(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AGraphic: TGraphic);
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AGraphic) then
    begin
      LogInternalError('LogGraphic: AGraphic argument is not assigned');
    end else
    begin
      if AGraphic is TBitmap then
      begin
        LogBitmap(ALevel, ATitle, TBitmap(AGraphic))
      end else if AGraphic is TJpegImage then
      begin
        LogJpeg(ALevel, ATitle, TJpegImage(AGraphic))
      end else if AGraphic is TIcon then
      begin
        LogIcon(ALevel, ATitle, TIcon(AGraphic))
      end else if AGraphic is TMetafile then
      begin
        LogMetafile(ALevel, ATitle, TMetaFile(AGraphic))
      end else
      begin
        // The supplied graphic is not supported.
        LogInternalError('LogGraphic: Class "%s" is not supported',
          [AGraphic.ClassName]);
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogCustomText(const ATitle, AText: UnicodeString;
  const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId);
begin
  LogCustomText(FParent.DefaultLevel, ATitle, AText, ALogEntryType, AViewerId);
end;

procedure TSiSession.LogCustomText(const ALevel: TSiLevel;
  const ATitle, AText: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
var
  LContext: TSiTextContext;
begin
  if IsOn(ALevel) then
  begin
    LContext := TSiTextContext.Create(AViewerId);
    try
      try
        LContext.LoadFromText(AText);
        SendContext(ALevel, ATitle, ALogEntryType, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogCustomText: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TSiSession.LogCustomContext(const ATitle: UnicodeString;
  const ALogEntryType: TSiLogEntryType; const AContext: TSiViewerContext);
begin
  LogCustomContext(FParent.DefaultLevel, ATitle, ALogEntryType, AContext);
end;

procedure TSiSession.LogCustomContext(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AContext: TSiViewerContext);
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AContext) then
    begin
      LogInternalError('LogCustomContext: AContext argument is not assigned');
    end else
    begin
      SendContext(ALevel, ATitle, ALogEntryType, AContext);
    end;
  end;
end;

procedure TSiSession.SendContext(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AContext: TSiViewerContext);
begin
  SendLogEntry(ALevel, ATitle, ALogEntryType, AContext.ViewerId,
    Color, AContext.ViewerData);
end;

procedure TSiSession.SendControlCommand(
  const AControlCommandType: TSiControlCommandType; const AData: TStream);
var
  LControlCommand: TSiControlCommand;
begin
  LControlCommand := TSiControlCommand.Create(AControlCommandType);
  try
    LControlCommand.Level := lvControl; // Always set to lvControl
    LControlCommand.Data := AData;
    FParent.SendControlCommand(LControlCommand);
  finally
    LControlCommand.Release;
  end;
end;

procedure TSiSession.SendLogEntry(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
begin
  SendLogEntry(ALevel, ATitle, ALogEntryType, AViewerId, Color, nil);
end;

procedure TSiSession.SendLogEntry(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId; const AColor: TColor;
  const AData: TStream);
var
  LLogEntry: TSiLogEntry;
begin
  LLogEntry := TSiLogEntry.Create(ALogEntryType, AViewerId);
  try
    LLogEntry.Timestamp := FParent.Now;
    LLogEntry.Level := ALevel;
    LLogEntry.Title := ATitle;
    if AColor = SiDefaultColor then
      LLogEntry.Color := AColor { Transparent }
    else
      LLogEntry.Color := ColorToRGB(AColor) and $FFFFFF;
    LLogEntry.SessionName := Name; // Our session name.
    LLogEntry.Data := AData;
    FParent.SendLogEntry(LLogEntry);
  finally
    LLogEntry.Release;
  end;
end;

procedure TSiSession.SendProcessFlow(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AProcessFlowType: TSiProcessFlowType);
var
  LProcessFlow: TSiProcessFlow;
begin
  LProcessFlow := TSiProcessFlow.Create(AProcessFlowType);
  try
    LProcessFlow.Timestamp := FParent.Now;
    LProcessFlow.Level := ALevel;
    LProcessFlow.Title := ATitle;
    FParent.SendProcessFlow(LProcessFlow);
  finally
    LProcessFlow.Release;
  end;
end;

procedure TSiSession.SendWatch(const ALevel: TSiLevel;
  const AName, AValue: UnicodeString; const AWatchType: TSiWatchType);
var
  LWatch: TSiWatch;
begin
  LWatch := TSiWatch.Create(AWatchType);
  try
    LWatch.Timestamp := FParent.Now;
    LWatch.Level := ALevel;
    LWatch.Name := AName;
    LWatch.Value := AValue;
    FParent.SendWatch(LWatch);
  finally
    LWatch.Release;
  end;
end;

procedure TSiSession.LogCustomFile(const AFileName: UnicodeString;
  const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId);
begin
  LogCustomFile(FParent.DefaultLevel, AFileName, ALogEntryType, AViewerId);
end;

procedure TSiSession.LogCustomFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
begin
  LogCustomFile(ALevel, AFileName, AFileName, ALogEntryType, AViewerId);
end;

procedure TSiSession.LogCustomFile(const ATitle, AFileName: UnicodeString;
  const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId);
begin
  LogCustomFile(FParent.DefaultLevel, ATitle, AFileName, ALogEntryType,
    AViewerId);
end;

procedure TSiSession.LogCustomFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
var
  LContext: TSiBinaryContext;
begin
  if IsOn(ALevel) then
  begin
    LContext := TSiBinaryContext.Create(AViewerId);
    try
      try
        LContext.LoadFromFile(AFileName);
        SendContext(ALevel, ATitle, ALogEntryType, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogCustomFile: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TSiSession.LogCustomStream(const ATitle: UnicodeString;
  const AStream: TStream; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId);
begin
  LogCustomStream(FParent.DefaultLevel, ATitle, AStream, ALogEntryType,
    AViewerId);
end;

procedure TSiSession.LogCustomStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream;
  const ALogEntryType: TSiLogEntryType; const AViewerId: TSiViewerId);
var
  LContext: TSiBinaryContext;
begin
  if IsOn(ALevel) then
  begin
    LContext := TSiBinaryContext.Create(AViewerId);
    try
      try
        LContext.LoadFromStream(AStream);
        SendContext(ALevel, ATitle, ALogEntryType, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogCustomStream: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TSiSession.LogSql(const ATitle, ASource: UnicodeString);
begin
  LogSql(FParent.DefaultLevel, ATitle, ASource);
end;

procedure TSiSession.LogSql(const ALevel: TSiLevel;
  const ATitle, ASource: UnicodeString);
begin
  LogSource(ALevel, ATitle, ASource, siSql);
end;

procedure TSiSession.LogSource(const ATitle, ASource: UnicodeString;
  const AId: TSiSourceId);
begin
  LogSource(FParent.DefaultLevel, ATitle, ASource, AId);
end;

procedure TSiSession.LogSource(const ALevel: TSiLevel;
  const ATitle, ASource: UnicodeString; const AId: TSiSourceId);
begin
  LogCustomText(ALevel, ATitle, ASource, ltSource, CSiSourceIdLookup[AId]);
end;

procedure TSiSession.LogSourceFile(const AFileName: UnicodeString;
  const AId: TSiSourceId);
begin
  LogSourceFile(FParent.DefaultLevel, AFileName, AId);
end;

procedure TSiSession.LogSourceFile(const ALevel: TSiLevel;
  const AFileName: UnicodeString; const AId: TSiSourceId);
begin
  LogCustomFile(ALevel, AFileName, ltSource, CSiSourceIdLookup[AId]);
end;

procedure TSiSession.LogSourceFile(const ATitle, AFileName: UnicodeString;
  const AId: TSiSourceId);
begin
  LogSourceFile(FParent.DefaultLevel, ATitle, AFileName, AId);
end;

procedure TSiSession.LogSourceFile(const ALevel: TSiLevel;
  const ATitle, AFileName: UnicodeString; const AId: TSiSourceId);
begin
  LogCustomFile(ALevel, ATitle, AFileName, ltSource, CSiSourceIdLookup[AId]);
end;

procedure TSiSession.LogSourceStream(const ATitle: UnicodeString;
  const AStream: TStream; const AId: TSiSourceId);
begin
  LogSourceStream(FParent.DefaultLevel, ATitle, AStream, AId);
end;

procedure TSiSession.LogSourceStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream;
  const AId: TSiSourceId);
begin
  LogCustomStream(ALevel, ATitle, AStream, ltSource, CSiSourceIdLookup[AId]);
end;

procedure TSiSession.LogMemory(const ATitle: UnicodeString;
  const AAddress: Pointer; const ASize: Integer);
begin
  LogMemory(FParent.DefaultLevel, ATitle, AAddress, ASize);
end;

procedure TSiSession.LogMemory(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AAddress: Pointer; const ASize: Integer);
var
  LContext: TSiBinaryViewerContext;
begin
  if IsOn(ALevel) then
  begin
    LContext := TSiBinaryViewerContext.Create;
    try
      try
        LContext.AppendBytes(AAddress, ASize);
        SendContext(ALevel, ATitle, ltBinary, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogMemory: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

{$IFNDEF SI_DISABLE_RTTI}

procedure TSiSession.LogObject(const ATitle: UnicodeString;
  const AInstance: TObject);
begin
  LogObject(FParent.DefaultLevel, ATitle, AInstance);
end;

procedure TSiSession.LogObject(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AInstance: TObject);

  procedure FillObjectData(const AInstance: TObject;
    const AContext: TSiInspectorViewerContext);
  var
    LPropList: PPropList;
    LPropInfo: PPropInfo;
    {$IFDEF DELPHI2009_OR_HIGHER}
    LPropName: UnicodeString;
    {$ELSE}
    LPropName: ShortString;
    {$ENDIF}
    LCount, I: Integer;
    LProperties: TSiStringList;
    LEvents: TSiStringList;
    LMethod: TMethod;
  begin
    LEvents := TSiStringList.Create;
    LProperties := TSiStringList.Create;
    try
      LCount := GetPropList(AInstance, LPropList);
      try
        for I := 0 to LCount - 1 do
        begin
          {$IFDEF DELPHI2009_OR_HIGHER}
          LPropName := GetPropName(LPropList[I]);
          {$ELSE}
          LPropName := LPropList[I]^.Name;
          {$ENDIF}

          LPropInfo := GetPropInfo(AInstance, LPropName);

          case LPropInfo^.PropType^.Kind of
            tkString,
            tkLString: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(GetStrProp(AInstance, LPropInfo))
              );
            end;

            tkWString: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(GetWideStrProp(AInstance, LPropInfo))
              );
            end;

            {$IFDEF DELPHI2009_OR_HIGHER}
            tkUString: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(GetUnicodeStrProp(AInstance, LPropInfo))
              );
            end;
            {$ENDIF}

            tkInteger: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(IntToStr(GetOrdProp(AInstance, LPropInfo)))
              );
            end;

            {$IFDEF DELPHI2009_OR_HIGHER}
            tkWChar,
            {$ENDIF}
            tkChar: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(Chr(GetOrdProp(AInstance, LPropInfo)))
              );
            end;

            tkFloat: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(FloatToStr(GetFloatProp(AInstance, LPropInfo)))
              );
            end;

            tkInt64: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(IntToStr(GetInt64Prop(AInstance, LPropInfo)))
              );
            end;

            tkEnumeration: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(GetEnumName(LPropInfo^.PropType^,
                  GetOrdProp(AInstance, LPropInfo)))
              );
            end;

            tkSet: begin
              LProperties.Add(
                AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                AContext.EscapeItem(GetSetProp(AInstance, LPropInfo, True))
              );
            end;

            tkMethod: begin
              LMethod := GetMethodProp(AInstance, LPropInfo);
              if Assigned(LMethod.Code) then
              begin
                LEvents.Add(
                  AContext.EscapeItem(UnicodeString(LPropName)) + '=' +
                  AContext.EscapeItem(SiFormat(
                    '(Object $%.8x of class %s, Method at address $%.8x)',
                    [Cardinal(LMethod.Data), TObject(LMethod.Data).ClassName,
                    Cardinal(LMethod.Code)])));
              end else
              begin
                LEvents.Add(AContext.EscapeItem(UnicodeString(LPropName)) + '=nil');
              end;
            end;
          end;
        end;
      finally
        if LCount > 0 then
        begin
          FreeMem(LPropList);
        end;
      end;

      // Add the properties to the context.
      LProperties.Sort;
      AContext.StartGroup('Properties');
      for I := 0 to LProperties.Count - 1 do
      begin
        AContext.AppendLine(LProperties[I]);
      end;

      // Add the events to the context.
      LEvents.Sort;
      AContext.StartGroup('Events');
      for I := 0 to LEvents.Count - 1 do
      begin
        AContext.AppendLine(LEvents[I]);
      end;
    finally
      LProperties.Free;
      LEvents.Free;
    end;
  end;

var
  LContext: TSiInspectorViewerContext;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('LogObject: AInstance argument is not assigned');
      Exit;
    end;

    if AInstance.ClassInfo = nil then
    begin
      LogInternalError('LogObject: AInstance argument has no RTTI');
      Exit;
    end;

    LContext := TSiInspectorViewerContext.Create;
    try
      try
        FillObjectData(AInstance, LContext);
        SendContext(ALevel, ATitle, ltObject, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogObject: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogStream(const ATitle: UnicodeString;
  const AStream: TStream);
begin
  LogStream(FParent.DefaultLevel, ATitle, AStream);
end;

procedure TSiSession.LogStream(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStream: TStream);
begin
  LogBinaryStream(ALevel, ATitle, AStream);
end;

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogPoint(const ATitle: UnicodeString; const APoint: TPoint);
begin
  LogPoint(FParent.DefaultLevel, ATitle, APoint);
end;

procedure TSiSession.LogPoint(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const APoint: TPoint);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    LTitle := SiFormat('%s = (X: %d; Y: %d)', [ATitle, APoint.X, APoint.Y]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogRect(const ATitle: UnicodeString; const ARect: TRect);
begin
  LogRect(FParent.DefaultLevel, ATitle, ARect);
end;

procedure TSiSession.LogRect(const ALevel: TSiLevel; const ATitle: UnicodeString;
  const ARect: TRect);
var
  LTitle: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    LTitle := SiFormat('%s = (Left: %d; Top: %d; Right: %d; Bottom: %d)',
      [ATitle, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom]);
    SendLogEntry(ALevel, LTitle, ltVariableValue, viTitle);
  end;
end;

procedure TSiSession.LogCanvas(const ATitle: UnicodeString;
  const ACanvas: TCanvas; const ARect: TRect);
begin
  LogCanvas(FParent.DefaultLevel, ATitle, ACanvas, ARect);
end;

procedure TSiSession.LogCanvas(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ACanvas: TCanvas; const ARect: TRect);
var
  LBitmap: TBitmap;
  LDestRect: TRect;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(ACanvas) then
    begin
      LogInternalError('LogCanvas: ACanvas argument is not assigned');
    end else
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.Width := Abs(ARect.Left - ARect.Right);
        LBitmap.Height := Abs(ARect.Bottom - ARect.Top);
        LDestRect.Left := 0;
        LDestRect.Top := 0;
        LDestRect.Right := LBitmap.Width;
        LDestRect.Bottom := LBitmap.Height;
        LBitmap.Canvas.CopyRect(LDestRect, ACanvas, ARect);
        LogBitmap(ALevel, ATitle, LBitmap);
      finally
        LBitmap.Free;
      end;
    end;
  end;
end;

{$ENDIF}

procedure TSiSession.LogException(const ATitle: UnicodeString = '');
var
  E: TObject;
  LBuffer: array[0..2048] of Char;
  LErrorMessage: String;
  LTitle: UnicodeString;
{$IFDEF DELPHI2009_OR_HIGHER}
  LException: Exception;
  LStackTrace: UnicodeString;
  LText: UnicodeString;
{$ENDIF}
begin
  if IsOn(lvError) then
  begin
    E := ExceptObject;

    if not Assigned(E) then
    begin
      LogInternalError('LogException: There is no exception handled currently');
      Exit;
    end;

    ExceptionErrorMessage(E, ExceptAddr, LBuffer, SizeOf(LBuffer));
    LErrorMessage := SiTrim(LBuffer);
    LErrorMessage := SiStringReplace(LErrorMessage, #13#10, ' ');

    if SiTrim(ATitle) <> '' then
      { Prepend the ATitle argument. }
      LTitle := ATitle + ': ' + LErrorMessage
    else
      LTitle := LErrorMessage; { Implicit conversion }

{$IFDEF DELPHI2009_OR_HIGHER}
    if E is Exception then
    begin
      LException := Exception(E);
      LStackTrace := LException.StackTrace;
      if LStackTrace <> '' then
      begin
        LText := LTitle + #13#10 + #13#10 + LStackTrace;
        LogCustomText(LTitle, LText, ltError, viData);
        Exit;
      end;
    end;
{$ENDIF}

    { Fallback for Delphi versions older than 2009 or if the
      StackTrace property is empty/could not be read. }
    LogError(LTitle);
  end;
end;

procedure TSiSession.LogException(const AException: Exception;
  const ATitle: UnicodeString = '');
var
  LTitle: UnicodeString;
  LErrorMessage: String;
{$IFDEF DELPHI2009_OR_HIGHER}
  LStackTrace: UnicodeString;
  LText: UnicodeString;
{$ENDIF}
begin
  if IsOn(lvError) then
  begin
    if not Assigned(AException) then
    begin
      LogInternalError('LogException: AException argument is not assigned');
      Exit;
    end;

    LErrorMessage := SiStringReplace(AException.Message, #13#10, ' ');
    LTitle := SiFormat('Exception %s: %s', [AException.ClassName,
      LErrorMessage]);

    if SiTrim(ATitle) <> '' then
    begin
      { Prepend the ATitle argument. }
      LTitle := ATitle + ': ' + LTitle;
    end;

{$IFDEF DELPHI2009_OR_HIGHER}
    LStackTrace := AException.StackTrace;
    if LStackTrace <> '' then
    begin
      LText := LTitle + #13#10 + #13#10 + LStackTrace;
      LogCustomText(LTitle, LText, ltError, viData);
      Exit;
    end;
{$ENDIF}

    { Fallback for Delphi versions older than 2009 or if the
      StackTrace property is empty/could not be read. }
    LogError(LTitle);
  end;
end;

procedure TSiSession.LogStringList(const ATitle: UnicodeString;
  const AStrings: TStrings);
begin
  LogStringList(FParent.DefaultLevel, ATitle, AStrings);
end;

procedure TSiSession.LogStringList(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AStrings: TStrings);
var
  LContext: TSiListViewerContext;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(AStrings) then
    begin
      LogInternalError('LogStringList: AStrings argument is not assigned');
      Exit;
    end;

    LContext := TSiListViewerContext.Create;
    try
      try
        LContext.LoadFromText(AStrings.Text);
        SendContext(ALevel, ATitle, ltText, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogStringList: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TSiSession.LogLastError(const ATitle: UnicodeString;
  const AIgnoreSuccess: Boolean = False);
begin
  if IsOn(lvError) then
  begin
    // Just call LogWin32Error with the last error.
    LogWin32Error(ATitle, GetLastError, AIgnoreSuccess);
  end;
end;

procedure TSiSession.LogWin32Error(const ATitle: UnicodeString;
  const AErrorCode: DWord; const AIgnoreSuccess: Boolean = False);
var
  LTitle: UnicodeString;
begin
  if IsOn(lvError) then
  begin
    if (AErrorCode <> 0) or not AIgnoreSuccess then
    begin
      // Turn the win32 error into an useful error message.
      LTitle := SiFormat('%s: %s (Error Code: %d, $%.08x)',
        [ATitle, SysErrorMessage(AErrorCode), AErrorCode, AErrorCode]);
      LogError(LTitle);
    end;
  end;
end;

{$IFNDEF SI_DISABLE_DB}

procedure TSiSession.LogDataSet(const ATitle: UnicodeString;
  const ADataSet: TDataSet; const ARestoreRecordPosition: Boolean);
begin
  LogDataSet(FParent.DefaultLevel, ATitle, ADataSet, ARestoreRecordPosition);
end;

procedure TSiSession.LogDataSet(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ADataSet: TDataSet;
  const ARestoreRecordPosition: Boolean);
var
  I: Integer;
  LContext: TSiTableViewerContext;
  LColumns: TFieldDefList;
  LField: TField;
  LOriginalRecordPosition: Integer;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(ADataSet) then
    begin
      LogInternalError('LogDataSet: ADataSet argument is not assigned');
      Exit;
    end;

    LContext := TSiTableViewerContext.Create;
    try
      try
        if ADataSet.FieldDefList.Count = 0 then
        begin
          LogInternalError('LogDataSet: data set is empty');
          Exit;
        end;

        LColumns := ADataSet.FieldDefList;

        // Add the header.
        LContext.BeginRow;
        try
          for I := 0 to LColumns.Count - 1 do
          begin
            LContext.AddRowEntry(LColumns[I].Name);
          end;
        finally
          LContext.EndRow;
        end;

        if not ADataSet.IsEmpty then
        begin
          LOriginalRecordPosition := -1;
          if ARestoreRecordPosition then
          begin
            LOriginalRecordPosition := ADataSet.RecNo;
          end;

          ADataSet.First;

          // And then the actual data.
          while not ADataSet.Eof do
          begin
            LContext.BeginRow;
            try
              for I := 0 to LColumns.Count - 1 do
              begin
                LField := ADataSet.FieldByName(LColumns[I].Name);
                if LField.IsNull then
                  LContext.AddRowEntry('<null>')
                else
                begin
                  if LField.IsBlob then
                    LContext.AddRowEntry('<binary>')
                  else
{$IFDEF DELPHI2006_OR_HIGHER}
                    LContext.AddRowEntry(LField.AsWideString);
{$ELSE}
                    LContext.AddRowEntry(LField.AsString);
{$ENDIF}
                end;
              end;
            finally
              LContext.EndRow;
            end;
            ADataSet.Next;
          end;

          // Restore original record position, if needed
          if ARestoreRecordPosition then
          begin
            // Check if the dataset supports RecNo
            if LOriginalRecordPosition <> -1 then
            begin
              ADataSet.RecNo := LOriginalRecordPosition;
            end;
          end;
        end;

        SendContext(ALevel, ATitle, ltDatabaseResult, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogDataSet: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

procedure TSiSession.LogDataSetSchema(const ATitle: UnicodeString;
  const ADataSet: TDataSet);
begin
  LogDataSetSchema(FParent.DefaultLevel, ATitle, ADataSet);
end;

procedure TSiSession.LogDataSetSchema(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ADataSet: TDataSet);
var
  I: Integer;
  LColumn: TFieldDef;
  LColumns: TFieldDefList;
  LType: TFieldType;
  LContext: TSiTableViewerContext;
begin
  if IsOn(ALevel) then
  begin
    if not Assigned(ADataSet) then
    begin
      LogInternalError('LogDataSetSchema: ADataSet argument is not assigned');
      Exit;
    end;

    LContext := TSiTableViewerContext.Create;
    try
      try
        if ADataSet.FieldDefList.Count = 0 then
        begin
          LogInternalError('LogDataSetSchema: data set is empty');
          Exit;
        end;

        LColumns := ADataSet.FieldDefList;
        LContext.AppendHeader('Name, Type, Size, "Read Only"');

        for I := 0 to LColumns.Count - 1 do
        begin
          LContext.BeginRow;
          try
            LColumn := LColumns[I];
            LContext.AddRowEntry(LColumn.Name);

            LType := LColumn.DataType;
            if (LType > High(DB.FieldTypeNames)) or
               (LType < Low(DB.FieldTypeNames)) then
            begin
              // The type of the column doesn't exist
              // in our names array, so just use 'Unknown'.
              LContext.AddRowEntry('Unknown');
            end else
            begin
              // Lookup the type of the column in our array.
              LContext.AddRowEntry(DB.FieldTypeNames[LType]);
            end;

            LContext.AddRowEntry(LColumn.Size);
            if faReadOnly in LColumn.Attributes then
            begin
              // The ReadOnly attribute is set for this column.
              LContext.AddRowEntry(True);
            end else
            begin
              // The ReadOnly attribute isn't set for this column.
              LContext.AddRowEntry(False);
            end;
          finally
            LContext.EndRow;
          end;
        end;

        SendContext(ALevel, ATitle, ltDatabaseStructure, LContext);
      except
        on E: Exception do
        begin
          LogInternalError('LogDataSetSchema: ' + E.Message);
        end;
      end;
    finally
      LContext.Free;
    end;
  end;
end;

{$ENDIF}

{$IFNDEF SI_DISABLE_GRAPHIC}

procedure TSiSession.LogScreenshot(const ATitle: UnicodeString;
  const AAsJpeg: Boolean);
begin
  LogScreenshot(FParent.DefaultLevel, ATitle, AAsJpeg);
end;

procedure TSiSession.LogScreenshot(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AAsJpeg: Boolean);
begin
  if IsOn(ALevel) then
  begin
    // Call LogScreenshot with the desktop window.
    LogScreenshot(ALevel, ATitle, GetDesktopWindow, AAsJpeg);
  end;
end;

procedure TSiSession.LogScreenshot(const ATitle: UnicodeString;
  const AWindow: HWND; const AAsJpeg: Boolean);
begin
  LogScreenshot(FParent.DefaultLevel, ATitle, AWindow, AAsJpeg);
end;

procedure TSiSession.LogScreenshot(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AWindow: HWND; const AAsJpeg: Boolean);
var
  LDeviceContext: HDC;
  LPalette : PLogPalette;
  LBitmap: TBitmap;
  LRect: TRect;
  LJpeg: TJpegImage;
begin
  if not IsOn(ALevel) then
    Exit;

  if not IsWindow(AWindow) then
  begin
    LogInternalError('LogScreenshot: Window handle is not a window');
    Exit;
  end;

  LBitmap := TBitmap.Create;
  try
    LDeviceContext := GetDc(0);
    if LDeviceContext = 0 then
    begin
      LogInternalError('LogScreenshot: Could not get device context of window');
      Exit;
    end;

    try
      GetWindowRect(AWindow, LRect);
      LBitmap.Width := LRect.Right - LRect.Left;
      LBitmap.Height := LRect.Bottom - LRect.Top;

      if GetDeviceCaps(LDeviceContext, RASTERCAPS) and RC_PALETTE = RC_PALETTE then
      begin
        GetMem(LPalette, SizeOf(TLogPalette) + (255 * SizeOf(TPaletteEntry)));
        try
          FillChar(LPalette^, SizeOf(TLogPalette) + (255 * SizeOf(TPaletteEntry)), #0);
          LPalette^.palVersion := $300;
          LPalette^.palNumEntries := GetSystemPaletteEntries(LDeviceContext,
            0, 256, LPalette^.palPalEntry);

          if LPalette^.PalNumEntries <> 0 then
          begin
            LBitmap.Palette := CreatePalette(LPalette^);
          end;
        finally
          FreeMem(LPalette, SizeOf(TLogPalette) + (255 * SizeOf(TPaletteEntry)));
        end;
      end;

      BitBlt(LBitmap.Canvas.Handle, 0, 0, LRect.Right - LRect.Left,
        LRect.Bottom - LRect.Top, LDeviceContext, LRect.Left, LRect.Top, SRCCOPY);
    finally
      ReleaseDc(0, LDeviceContext);
    end;

    if AAsJpeg then
    begin
      LJpeg := TJpegImage.Create;
      try
        LJpeg.CompressionQuality := 100;
        LJpeg.Assign(LBitmap);
        LogJpeg(ALevel, ATitle, LJpeg);
      finally
        LJpeg.Free;
      end;
    end else
    begin
      LogBitmap(ALevel, ATitle, LBitmap);
    end;
  finally
    LBitmap.Free;
  end;
end;

{$ENDIF}

procedure TSiSession.LogMemoryStatistic;
begin
  LogMemoryStatistic(FParent.DefaultLevel);
end;

procedure TSiSession.LogMemoryStatistic(const ALevel: TSiLevel);
begin
  LogMemoryStatistic(ALevel, 'Memory statistic');
end;

procedure TSiSession.LogMemoryStatistic(const ATitle: UnicodeString);
begin
  LogMemoryStatistic(FParent.DefaultLevel, ATitle);
end;

procedure TSiSession.LogMemoryStatistic(const ALevel: TSiLevel;
  const ATitle: UnicodeString);
var
  LStatus: TMemoryStatus;
  LContext: TSiValueListViewerContext;

  function BytesAsString(const ABytes: Cardinal): UnicodeString;
  var
    I: Integer;
    LFraction: Double;
  begin
    if ABytes = Cardinal(-1) then
    begin
      // To quote MSDN: "On computers with more than 4 GB of memory, the
      // GlobalMemoryStatus function can return incorrect information.
      // Windows 2000 and later report a value of -1 to indicate an overflow."
      Result := 'more than 4 GB';
    end else
    begin
      I := 0;
      LFraction := ABytes;
      while (LFraction >= 1024) and (I <= 3) do
      begin
        LFraction := LFraction / 1024;
        Inc(I);
      end;

      Result := SiFormat('%.2f', [LFraction]);
      case I of
        0: Result := Result + ' Bytes';
        1: Result := Result + ' KB';
        2: Result := Result + ' MB';
        3: Result := Result + ' GB';
      end;
    end;
  end;

begin
  if not IsOn(ALevel) then
    Exit;

  ZeroMemory(Pointer(@LStatus), SizeOf(LStatus));
  LStatus.dwLength := SizeOf(LStatus);
  GlobalMemoryStatus(LStatus);

  LContext := TSiValueListViewerContext.Create;
  try
    LContext.AppendKeyValue('Usage', LStatus.dwMemoryLoad);
    LContext.AppendKeyValue('Total physical', BytesAsString(LStatus.dwTotalPhys));
    LContext.AppendKeyValue('Free physical', BytesAsString(LStatus.dwAvailPhys));
    LContext.AppendKeyValue('Total pagefile', BytesAsString(LStatus.dwTotalPageFile));
    LContext.AppendKeyValue('Free pagefile', BytesAsString(LStatus.dwAvailPageFile));
    LContext.AppendKeyValue('Total virtual', BytesAsString(LStatus.dwTotalVirtual));
    LContext.AppendKeyValue('Free virtual', BytesAsString(LStatus.dwAvailVirtual));
    SendContext(ALevel, ATitle, ltMemoryStatistic, LContext);
  finally
    LContext.Free;
  end;
end;

procedure TSiSession.LogSystem;
begin
  LogSystem(FParent.DefaultLevel);
end;

procedure TSiSession.LogSystem(const ALevel: TSiLevel);
begin
  LogSystem(ALevel, 'System information');
end;

procedure TSiSession.LogSystem(const ATitle: UnicodeString);
begin
  LogSystem(FParent.DefaultLevel, ATitle);
end;

procedure TSiSession.LogSystem(const ALevel: TSiLevel;
  const ATitle: UnicodeString);
var
  LContext: TSiInspectorViewerContext;
  LUserName: UnicodeString;
  LUserNameLength: Cardinal;
  LOSVersion: TOSVersionInfo;
  LCurrentDir: UnicodeString;
  LCurrentDirLength: Cardinal;
  LOSName: UnicodeString;
begin
  if not IsOn(ALevel) then
    Exit;

  LContext := TSiInspectorViewerContext.Create;
  try
    ZeroMemory(Pointer(@LOSVersion), SizeOf(LOSVersion));
    LOSVersion.dwOSVersionInfoSize := SizeOf(LOSVersion);
    if GetVersionEx(LOSVersion) then
    begin
      case LOSVersion.dwPlatformId of
        VER_PLATFORM_WIN32_WINDOWS: LOSName := 'Windows 95';
        VER_PLATFORM_WIN32_NT: LOSName := 'Windows NT';
      end;

      LContext.StartGroup('Operating System');
      LContext.AppendKeyValue('Name', LOSName);
      LContext.AppendKeyValue('Version', SiFormat('%u.%u.%u %s',
        [LOSVersion.dwMajorVersion, LOSVersion.dwMinorVersion,
        LOSVersion.dwBuildNumber, LOSVersion.szCSDVersion]));
    end;

    LUserNameLength := $FF;
    SetLength(LUserName, LUserNameLength);
    if SiGetUserName(PWideChar(LUserName), LUserNameLength) then
    begin
      SetLength(LUserName, LUserNameLength - 1);

      LContext.StartGroup('User');
      LContext.AppendKeyValue('Name', LUserName);

      LCurrentDirLength := $FF;
      SetLength(LCurrentDir, LCurrentDirLength);
      LCurrentDirLength := SiGetCurrentDirectory(LCurrentDirLength,
        PWideChar(LCurrentDir));

      if (LCurrentDirLength <> 0) and (LCurrentDirLength < $FF) then
      begin
        SetLength(LCurrentDir, LCurrentDirLength);
        LContext.AppendKeyValue('Current directory', LCurrentDir);
      end;
    end;

    LContext.StartGroup('Delphi');
    LContext.AppendKeyValue('RTL version', RTLVersion);
    LContext.AppendKeyValue('Compiler version', CompilerVersion);

    SendContext(ALevel, ATitle, ltSystem, LContext);
  finally
    LContext.Free;
  end;
end;

procedure TSiSession.ClearAll;
begin
  if IsOn then
  begin
    SendControlCommand(ccClearAll);
  end;
end;

procedure TSiSession.ClearAutoViews;
begin
  if IsOn then
  begin
    SendControlCommand(ccClearAutoViews);
  end;
end;

procedure TSiSession.ClearWatches;
begin
  if IsOn then
  begin
    SendControlCommand(ccClearWatches);
  end;
end;

procedure TSiSession.ClearLog;
begin
  if IsOn then
  begin
    SendControlCommand(ccClearLog);
  end;
end;

procedure TSiSession.ClearProcessFlow;
begin
  if IsOn then
  begin
    SendControlCommand(ccClearProcessFlow);
  end;
end;

procedure TSiSession.WatchBoolean(const AName: UnicodeString;
  const AValue: Boolean);
begin
  WatchBoolean(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchBoolean(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Boolean);
begin
  if IsOn(ALevel) then
  begin
    if AValue then
      SendWatch(ALevel, AName, 'True', wtBoolean)
    else
      SendWatch(ALevel, AName, 'False', wtBoolean);
  end;
end;

procedure TSiSession.WatchChar(const AName: UnicodeString; const AValue: Char);
begin
  WatchChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchChar(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Char);
begin
  if IsOn(ALevel) then
  begin
    SendWatch(ALevel, AName, AValue, wtChar)
  end;
end;

procedure TSiSession.IncCounter(const AName: UnicodeString);
begin
  IncCounter(FParent.DefaultLevel, AName);
end;

procedure TSiSession.IncCounter(const ALevel: TSiLevel;
  const AName: UnicodeString);
var
  LValue: Integer;
begin
  if IsOn(ALevel) then
  begin
    LValue := UpdateCounter(AName, True);
    SendWatch(ALevel, AName, IntToStr(LValue), wtInteger);
  end;
end;

procedure TSiSession.WatchDateTime(const AName: UnicodeString;
  const AValue: TDateTime);
begin
  WatchDateTime(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchDateTime(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: TDateTime);
var
  LValue: UnicodeString;
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
begin
  if IsOn(ALevel) then
  begin
{$IFDEF DELPHI7_OR_HIGHER}
    LFormatSettings := SiGetLocaleFormatSettings();
    LValue := DateTimeToStr(AValue, LFormatSettings);
{$ELSE}
    LValue := DateTimeToStr(AValue);
{$ENDIF}
    SendWatch(ALevel, AName, LValue, wtTimestamp);
  end;
end;

procedure TSiSession.WatchPointer(const AName: UnicodeString;
  const AValue: Pointer);
begin
  WatchPointer(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchPointer(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Pointer);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    // %p cannot be used due to a bug in Delphi's Format
    LValue := SiFormat('$%.8x', [Cardinal(AValue)]);
    SendWatch(ALevel, AName, LValue, wtAddress);
  end;
end;

procedure TSiSession.WatchInteger(const AName: UnicodeString;
  const AValue: Integer);
begin
  WatchInteger(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchInteger(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Integer);
begin
  WatchInteger(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchCardinal(const AName: UnicodeString;
  const AValue: Cardinal);
begin
  WatchCardinal(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchCardinal(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Cardinal);
begin
  WatchCardinal(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchString(const AName: UnicodeString;
  const AValue: String);
begin
  WatchString(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchString(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: String);
begin
  if IsOn(ALevel) then
  begin
    SendWatch(ALevel, AName, AValue, wtString);
  end;
end;

procedure TSiSession.WatchByte(const AName: UnicodeString; const AValue: Byte;
  const AIncludeHex: Boolean);
begin
  WatchByte(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchByte(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Byte; const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%u ($%.2x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchCardinal(const AName: UnicodeString;
  const AValue: Cardinal; const AIncludeHex: Boolean);
begin
  WatchCardinal(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchCardinal(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Cardinal; const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%u ($%.8x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchInt64(const AName: UnicodeString; const AValue: Int64;
  const AIncludeHex: Boolean);
begin
  WatchInt64(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchInt64(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Int64;
  const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%d ($%.16x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchInteger(const AName: UnicodeString;
  const AValue: Integer; const AIncludeHex: Boolean);
begin
  WatchInteger(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchInteger(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Integer; const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%d ($%.8x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchShortint(const AName: UnicodeString;
  const AValue: Shortint; const AIncludeHex: Boolean);
begin
  WatchShortint(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchShortint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Shortint;
  const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%d ($%.2x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchSmallint(const AName: UnicodeString;
  const AValue: Smallint; const AIncludeHex: Boolean);
begin
  WatchSmallint(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchSmallint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Smallint;
  const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%d ($%.4x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchWord(const AName: UnicodeString; const AValue: Word;
  const AIncludeHex: Boolean);
begin
  WatchWord(FParent.DefaultLevel, AName, AValue, AIncludeHex);
end;

procedure TSiSession.WatchWord(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Word; const AIncludeHex: Boolean);
var
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
    if AIncludeHex then
      LValue := SiFormat('%u ($%.4x)', [AValue, AValue])
    else
      LValue := IntToStr(AValue);
    SendWatch(ALevel, AName, LValue, wtInteger);
  end;
end;

procedure TSiSession.WatchByte(const AName: UnicodeString; const AValue: Byte);
begin
  WatchByte(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchByte(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Byte);
begin
  WatchByte(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchInt64(const AName: UnicodeString; const AValue: Int64);
begin
  WatchInt64(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchInt64(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Int64);
begin
  WatchInt64(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchShortint(const AName: UnicodeString;
  const AValue: Shortint);
begin
  WatchShortint(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchShortint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Shortint);
begin
  WatchShortint(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchSmallint(const AName: UnicodeString;
  const AValue: Smallint);
begin
  WatchSmallint(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchSmallint(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Smallint);
begin
  WatchSmallint(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchWord(const AName: UnicodeString; const AValue: Word);
begin
  WatchWord(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchWord(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Word);
begin
  WatchWord(ALevel, AName, AValue, False);
end;

procedure TSiSession.WatchCurrency(const AName: UnicodeString;
  const AValue: Currency);
begin
  WatchCurrency(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchCurrency(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Currency);
begin
  WatchExtended(ALevel, AName, AValue);
end;

procedure TSiSession.WatchDouble(const AName: UnicodeString;
  const AValue: Double);
begin
  WatchDouble(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchDouble(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Double);
begin
  WatchExtended(ALevel, AName, AValue);
end;

procedure TSiSession.WatchExtended(const AName: UnicodeString;
  const AValue: Extended);
begin
  WatchExtended(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchExtended(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Extended);
var
{$IFDEF DELPHI7_OR_HIGHER}
  LFormatSettings: TFormatSettings;
{$ENDIF}
  LValue: UnicodeString;
begin
  if IsOn(ALevel) then
  begin
{$IFDEF DELPHI7_OR_HIGHER}
    LFormatSettings := SiGetLocaleFormatSettings();
    LValue := FloatToStr(AValue, LFormatSettings);
{$ELSE}
    LValue := FloatToStr(AValue);
{$ENDIF}
    SendWatch(ALevel, AName, LValue, wtFloat);
  end;
end;

procedure TSiSession.WatchSingle(const AName: UnicodeString;
  const AValue: Single);
begin
  WatchSingle(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchSingle(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: Single);
begin
  WatchExtended(ALevel, AName, AValue);
end;

procedure TSiSession.WatchPChar(const AName: UnicodeString; const AValue: PChar);
begin
  WatchPChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchPChar(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PChar);
begin
  WatchString(ALevel, AName, AValue);
end;

procedure TSiSession.WatchWideString(const AName: UnicodeString;
  const AValue: WideString);
begin
  WatchWideString(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchWideString(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: WideString);
begin
  if IsOn(ALevel) then
  begin
    SendWatch(ALevel, AName, AValue, wtString);
  end;
end;

procedure TSiSession.WatchPWideChar(const AName: UnicodeString;
  const AValue: PWideChar);
begin
  WatchPWideChar(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.WatchPWideChar(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: PWideChar);
begin
  WatchWideString(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Smallint);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Smallint);
begin
  WatchSmallint(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Shortint);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Shortint);
begin
  WatchShortint(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Int64);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Int64);
begin
  WatchInt64(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Byte);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Byte);
begin
  WatchByte(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Word);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Word);
begin
  WatchWord(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Integer);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Integer);
begin
  WatchInteger(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: String);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: String);
begin
  WatchString(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: WideString);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel;
  const AName: UnicodeString; const AValue: WideString);
begin
  WatchWideString(ALevel, AName, AValue);
end;

{$IFNDEF DELPHI2009_OR_HIGHER}
procedure TSiSession.Watch(const AName: UnicodeString; const AValue: PWideChar);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: PWideChar);
begin
  WatchPWideChar(ALevel, AName, AValue);
end;
{$ENDIF}

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: PChar);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: PChar);
begin
  WatchPChar(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Cardinal);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Cardinal);
begin
  WatchCardinal(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Boolean);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Boolean);
begin
  WatchBoolean(ALevel, AName, AValue);
end;

procedure TSiSession.Watch(const AName: UnicodeString; const AValue: Extended);
begin
  Watch(FParent.DefaultLevel, AName, AValue);
end;

procedure TSiSession.Watch(const ALevel: TSiLevel; const AName: UnicodeString;
  const AValue: Extended);
begin
  WatchExtended(ALevel, AName, AValue);
end;

procedure TSiSession.SendCustomControlCommand(
  const AControlCommandType: TSiControlCommandType; const AData: TStream);
begin
  SendCustomControlCommand(FParent.DefaultLevel, AControlCommandType, AData);
end;

procedure TSiSession.SendCustomControlCommand(const ALevel: TSiLevel;
  const AControlCommandType: TSiControlCommandType; const AData: TStream);
var
  LOldPosition: Int64;
begin
  if IsOn(ALevel) then
  begin
    if Assigned(AData) then
    begin
      try
        // Handle the position of the stream correctly.
        LOldPosition := AData.Position;
        try
          SendControlCommand(AControlCommandType, AData);
        finally
          AData.Position := LOldPosition;
        end;
      except
        on E: Exception do
        begin
          LogInternalError('SendCustomControlCommand: ' + E.Message);
        end;
      end;
    end else
    begin
      SendControlCommand(AControlCommandType, nil);
    end;
  end;
end;

procedure TSiSession.SendCustomLogEntry(const ATitle: UnicodeString;
  const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId; const AData: TStream);
begin
  SendCustomLogEntry(FParent.DefaultLevel, ATitle, ALogEntryType,
    AViewerId, AData);
end;

procedure TSiSession.SendCustomLogEntry(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const ALogEntryType: TSiLogEntryType;
  const AViewerId: TSiViewerId; const AData: TStream);
begin
  if IsOn(ALevel) then
  begin
    if Assigned(AData) then
    begin
      // Use the LogCustomStream method, because the
      // supplied stream needs to be processed correctly.
      LogCustomStream(ALevel, ATitle, AData, ALogEntryType, AViewerId);
    end else
    begin
      SendLogEntry(ALevel, ATitle, ALogEntryType, AViewerId);
    end;
  end;
end;

procedure TSiSession.SendCustomProcessFlow(const ATitle: UnicodeString;
  const AProcessFlowType: TSiProcessFlowType);
begin
  SendCustomProcessFlow(FParent.DefaultLevel, ATitle, AProcessFlowType);
end;

procedure TSiSession.SendCustomProcessFlow(const ALevel: TSiLevel;
  const ATitle: UnicodeString; const AProcessFlowType: TSiProcessFlowType);
begin
  if IsOn(ALevel) then
  begin
    SendProcessFlow(ALevel, ATitle, AProcessFlowType);
  end;
end;

procedure TSiSession.SendCustomWatch(const AName, AValue: UnicodeString;
  const AWatchType: TSiWatchType);
begin
  SendCustomWatch(FParent.DefaultLevel, AName, AValue, AWatchType);
end;

procedure TSiSession.SendCustomWatch(const ALevel: TSiLevel;
  const AName, AValue: UnicodeString; const AWatchType: TSiWatchType);
begin
  if IsOn(ALevel) then
  begin
    SendWatch(ALevel, AName, AValue, AWatchType);
  end;
end;

function TSiSession.TrackMethod(
  const AMethodName: UnicodeString): ISiMethodTracker;
begin
  Result := TrackMethod(FParent.DefaultLevel, AMethodName);
end;

function TSiSession.TrackMethod(const ALevel: TSiLevel;
  const AMethodName: UnicodeString): ISiMethodTracker;
begin
  if IsOn(ALevel) then
  begin
    Result := TSiMethodTracker.Create(ALevel, Self, AMethodName);
  end else
  begin
    Result := nil;
  end;
end;

function TSiSession.TrackMethod(const AMethodNameFmt: UnicodeString;
  const AArgs: array of const): ISiMethodTracker;
begin
  Result := TrackMethod(FParent.DefaultLevel, AMethodNameFmt, AArgs);
end;

function TSiSession.TrackMethod(const ALevel: TSiLevel;
  const AMethodNameFmt: UnicodeString;
  const AArgs: array of const): ISiMethodTracker;
var
  LMethodName: UnicodeString;
begin
  Result := nil;
  if IsOn(ALevel) then
  begin
    try
      LMethodName := SiFormat(AMethodNameFmt, AArgs);
      Result := TSiMethodTracker.Create(ALevel, Self, LMethodName);
    except
      on E: Exception do
      begin
        LogInternalError('TrackMethod: ' + E.Message);
      end;
    end;
  end;
end;

function TSiSession.TrackMethod(const AInstance: TObject;
  const AMethodName: UnicodeString): ISiMethodTracker;
begin
  Result := TrackMethod(FParent.DefaultLevel, AInstance, AMethodName);
end;

function TSiSession.TrackMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodName: UnicodeString): ISiMethodTracker;
var
  LMethodName: UnicodeString;
begin
  Result := nil;
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('TrackMethod: AInstance argument is not assigned');
    end else
    begin
      LMethodName := UnicodeString(AInstance.ClassName) + '.' + AMethodName;
      Result := TSiMethodTracker.Create(ALevel, Self, LMethodName);
    end;
  end;
end;

function TSiSession.TrackMethod(const AInstance: TObject;
  const AMethodNameFmt: UnicodeString;
  const AArgs: array of const): ISiMethodTracker;
begin
  Result := TrackMethod(FParent.DefaultLevel, AInstance, AMethodNameFmt,
    AArgs);
end;

function TSiSession.TrackMethod(const ALevel: TSiLevel;
  const AInstance: TObject; const AMethodNameFmt: UnicodeString;
  const AArgs: array of const): ISiMethodTracker;
var
  LMethodName: UnicodeString;
begin
  Result := nil;
  if IsOn(ALevel) then
  begin
    if not Assigned(AInstance) then
    begin
      LogInternalError('TrackMethod: AInstance argument is not assigned');
    end else
    begin
      try
        LMethodName := UnicodeString(AInstance.ClassName) + '.' +
          SiFormat(AMethodNameFmt, AArgs);
        Result := TSiMethodTracker.Create(ALevel, Self, LMethodName);
      except
        on E: Exception do
        begin
          LogInternalError('TrackMethod: ' + E.Message);
        end;
      end;
    end;
  end;
end;

function TSiSession.UpdateCounter(const AName: UnicodeString;
  const AIncrement: Boolean): Integer;
begin
  FCriticalSection.Enter;
  try
    if AIncrement then
      Result := FCounter.Inc(AName)
    else
      Result := FCounter.Dec(AName);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TSiMethodTracker }

constructor TSiMethodTracker.Create(const ALevel: TSiLevel;
  const ASession: TSiSession; const AMethodName: UnicodeString);
begin
  FSession := ASession;
  FMethodName := AMethodName;
  FLevel := ALevel;
  FSession.EnterMethod(FLevel, FMethodName);
end;

destructor TSiMethodTracker.Destroy;
begin
  FSession.LeaveMethod(FLevel, FMethodName);
  inherited;
end;

{ TSiLookupTable }

const
  SiHexTable: array[0..71] of Byte =
  (
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff,
     $00, $01, $02, $03, $04, $05, $06, $07,
     $08, $09, $ff, $ff, $ff, $ff, $ff, $ff,
     $ff, $0a, $0b, $0c, $0d, $0e, $0f, $ff
  );
  SiHexIdentifier: array[0..2] of UnicodeString = ('0x', '&H', '$');

procedure TSiLookupTable.Add(const AKey, AValue: UnicodeString);
begin
  if not Contains(AKey) then
  begin
    Put(AKey, AValue);
  end;
end;

procedure TSiLookupTable.Clear;
begin
  FItems.Clear;
end;

function TSiLookupTable.Contains(const AKey: UnicodeString): Boolean;
begin
  Result := FItems.Contains(AKey);
end;

class function TSiLookupTable.ConvertHexString(const AValue: UnicodeString):
  TSiBytes;
var
  LValue: UnicodeString;
  I, LIndex: Integer;
  LHigh, LLow: Byte;
begin
  LValue := SiUpperCase(AValue);
  if Odd(Length(LValue)) then
  begin
    LValue := LValue + '0';
  end;

  if IsValidHex(LValue) then
  begin
    SetLength(Result, Length(LValue) div 2);
    for I := 0 to Pred(Length(Result)) do
    begin
      LIndex := Succ(I shl 1);
      LHigh := SiHexTable[Ord(LValue[LIndex])];
      LLow := SiHexTable[Ord(LValue[LIndex + 1])];
      Result[I] := (LHigh shl 4) or LLow;
    end;
  end else
    Result := nil;
end;

class function TSiLookupTable.ConvertUnicodeValue(
  const AValue: UnicodeString): TSiBytes;
var
  LUTF8: TSiUTF8String;
begin
  LUTF8 := SiUTF8Encode(AValue);
  SetLength(Result, Length(LUTF8));
  CopyMemory(Result, Pointer(LUTF8), Length(LUTF8));
end;

class function TSiLookupTable.ConvertHexValue(const AValue: UnicodeString):
  TSiBytes;
var
  I: Integer;
  LIdentifier: UnicodeString;
  LPrefix: UnicodeString;
  LValue: UnicodeString;
begin
  Result := nil;
  for I := Low(SiHexIdentifier) to High(SiHexIdentifier) do
  begin
    LIdentifier := SiHexIdentifier[I];
    if Length(AValue) >= Length(LIdentifier) then
    begin
      LPrefix := Copy(AValue, 1, Length(LIdentifier));
      if SiSameStr(LPrefix, LIdentifier) then
      begin
        LValue := Copy(AValue, Length(LIdentifier) + 1, MaxInt);
        Result := ConvertHexString(LValue);
        Break;
      end;
    end;
  end;
end;

constructor TSiLookupTable.Create;
begin
  FItems := TSiStringHash.Create;
end;

destructor TSiLookupTable.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TSiLookupTable.GetBooleanValue(const AKey: UnicodeString;
  const ADefaultValue: Boolean): Boolean;
var
  LValue: UnicodeString;
begin
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LValue := SiTrim(SiLowerCase(LValue));
    Result := (LValue = 'true') or (LValue = '1') or (LValue = 'yes');
  end else
    Result := ADefaultValue;
end;

function TSiLookupTable.GetBytesValue(const AKey: UnicodeString;
  const ASize: Integer; const ADefaultValue: TSiBytes): TSiBytes;
var
  LValue: UnicodeString;
  LBuffer: TSiBytes;
begin
  LBuffer := nil; { For delphi 6 }
  Result := ADefaultValue;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LBuffer := ConvertUnicodeValue(SiTrim(LValue));
    if not Assigned(LBuffer) then
    begin
      Exit;
    end;

    if Length(LBuffer) <> ASize then
    begin
      if Length(LBuffer) > ASize then
      begin
        Result := Copy(LBuffer, 0, ASize);
      end else
      begin
        SetLength(Result, ASize);
        ZeroMemory(Result, ASize);
        CopyMemory(Result, LBuffer, Length(LBuffer));
      end;

      LBuffer := nil;
    end else
      Result := LBuffer;
  end;
end;

function TSiLookupTable.GetColorValue(const AKey: UnicodeString;
  const ADefaultValue: TColor): TColor;
var
  LValue: UnicodeString;
  LBuffer: TSiBytes;
begin
  LBuffer := nil; { For delphi 6 }
  Result := ADefaultValue;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LBuffer := ConvertHexValue(SiTrim(LValue));
    
    if not Assigned(LBuffer) then
    begin
      Exit;
    end;

    case Length(LBuffer) of
      3: begin
        Result :=
          (LBuffer[2] shl 16) or
          (LBuffer[1] shl 8) or
          LBuffer[0];
      end;
      4: begin
        Result :=
          (LBuffer[3] shl 16) or
          (LBuffer[2] shl 8) or
          LBuffer[1]; { Ignore alpha }
      end;
    end;
  end;
end;

function TSiLookupTable.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TSiLookupTable.GetIntegerValue(const AKey: UnicodeString;
  const ADefaultValue: Integer): Integer;
var
  LValue: UnicodeString;
begin
  Result := ADefaultValue;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LValue := SiTrim(LValue);
    if IsValidInteger(LValue) then
    begin
      try
        Result := StrToInt(LValue);
      except
        { Return default }
      end;
    end;
  end;
end;

function TSiLookupTable.GetLevelValue(const AKey: UnicodeString;
  const ADefaultValue: TSiLevel): TSiLevel;
var
  LValue: UnicodeString;
begin
  Result := ADefaultValue;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LValue := SiTrim(SiLowerCase(LValue));
    if LValue = 'debug' then
      Result := lvDebug
    else if LValue = 'verbose' then
      Result := lvVerbose
    else if LValue = 'message' then
      Result := lvMessage
    else if LValue = 'warning' then
      Result := lvWarning
    else if LValue = 'error' then
      Result := lvError
    else if LValue = 'fatal' then
      Result := lvFatal;
  end;
end;

function TSiLookupTable.GetSizeValue(const AKey: UnicodeString;
  const ADefaultValue: Int64): Int64;
const
  CKBFactor = 1024;
  CMBFactor = CKBFactor * 1024;
  CGBFactor = CMBFactor * 1024;
var
  LValue: UnicodeString;
  LUnit: UnicodeString;
  LFactor: Int64;
begin
  Result := ADefaultValue * CKBFactor;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LFactor := CKBFactor; // Backwards compabitility
    LValue := SiTrim(LValue);

    if Length(LValue) >= 2 then
    begin
      LUnit := SiLowerCase(Copy(LValue, Length(LValue) - 1, 2));      
      if IsValidSizeUnit(LUnit) then
      begin
        Delete(LValue, Length(LValue) - 1, 2);
        LValue := SiTrim(LValue);

        if LUnit = 'kb' then
          LFactor := CKBFactor
        else if LUnit = 'mb' then
          LFactor := CMBFactor
        else if LUnit = 'gb' then
          LFactor := CGBFactor
      end;
    end;

    if IsValidInteger(LValue) then
    begin
      try
        Result := LFactor * StrToInt(LValue);
      except
        { Return default }
      end;
    end;
  end;
end;

function TSiLookupTable.GetStringValue(const AKey,
  ADefaultValue: UnicodeString): UnicodeString;
begin
  if FItems.Contains(AKey) then
    Result := FItems[AKey]
  else
    Result := ADefaultValue;
end;

function TSiLookupTable.GetTimespanValue(const AKey: UnicodeString;
  const ADefaultValue: Cardinal): Cardinal;
const
  CSecondFactor = 1000;
  CMinuteFactor = CSecondFactor * 60;
  CHourFactor = CMinuteFactor * 60;
  CDayFactor = CHourFactor * 24;
var
  LValue: UnicodeString;
  LFactor: Cardinal;
  LUnit: UnicodeString;
begin
  Result := ADefaultValue * CSecondFactor;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LFactor := CSecondFactor; { Default are seconds }
    LValue := SiTrim(LValue);

    if Length(LValue) >= 1 then
    begin
      LUnit := SiLowerCase(Copy(LValue, Length(LValue), 1));
      if IsValidTimespanUnit(LUnit) then
      begin
        Delete(LValue, Length(LValue), 1);
        LValue := SiTrim(LValue);

        if LUnit = 's' then
          LFactor := CSecondFactor
        else if LUnit = 'm' then
          LFactor := CMinuteFactor
        else if LUnit = 'h' then
          LFactor := CHourFactor
        else if LUnit = 'd' then
          LFactor := CDayFactor
      end;
    end;

    if IsValidInteger(LValue) then
    begin
      try
        Result := LFactor * Cardinal(StrToInt(LValue));
      except
        { Return default }
      end;
    end;
  end;
end;

class function TSiLookupTable.IsValidHex(const AValue: UnicodeString): Boolean;
var
  LChar: Integer;
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(AValue) do
  begin
    LChar := Ord(AValue[I]);
    if (LChar > High(SiHexTable)) or (SiHexTable[LChar] > $f) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

class function TSiLookupTable.IsValidInteger(const AValue: UnicodeString): Boolean;
var
  I: Integer;
begin
  if AValue <> '' then
  begin
    Result := True;
    for I := 1 to Length(AValue) do
    begin
      if (AValue[I] < '0') or (AValue[I] > '9') then
      begin
        Result := False;
        Break;
      end;
    end;
  end else
    Result := False;
end;

class function TSiLookupTable.IsValidSizeUnit(
  const AUnit: UnicodeString): Boolean;
begin
  Result := (AUnit = 'kb') or (AUnit = 'mb') or (AUnit = 'gb');
end;

class function TSiLookupTable.IsValidTimespanUnit(
  const AUnit: UnicodeString): Boolean;
begin
  Result := (AUnit = 's') or (AUnit = 'm') or (AUnit = 'h') or (AUnit = 'd');
end;

procedure TSiLookupTable.Put(const AKey, AValue: UnicodeString);
begin
  FItems.Put(AKey, AValue);
end;

procedure TSiLookupTable.Remove(const AKey: UnicodeString);
begin
  FItems.Remove(AKey);
end;

function TSiLookupTable.GetRotateValue(const AKey: UnicodeString;
  const ADefaultValue: TSiFileRotate): TSiFileRotate;
var
  LValue: UnicodeString;
begin
  Result := ADefaultValue;
  LValue := GetStringValue(AKey, '');

  if Length(LValue) <> 0 then
  begin
    LValue := SiTrim(SiLowerCase(LValue));
    if LValue = 'none' then
      Result := frNone
    else if LValue = 'hourly' then
      Result := frHourly
    else if LValue = 'daily' then
      Result := frDaily
    else if LValue = 'weekly' then
      Result := frWeekly
    else if LValue = 'monthly' then
      Result := frMonthly;
  end;
end;

{ TSiConfiguration }

procedure TSiConfiguration.Clear;
begin
  FKeys.Clear;
  FItems.Clear;
end;

function TSiConfiguration.Contains(const AKey: UnicodeString): Boolean;
begin
  Result := FItems.Contains(AKey)
end;

constructor TSiConfiguration.Create;
begin
  FItems := TSiLookupTable.Create;
  FKeys := TSiStringList.Create;
end;

destructor TSiConfiguration.Destroy;
begin
  FreeAndNil(FKeys);
  FreeAndNil(FItems);
  inherited;
end;

function TSiConfiguration.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TSiConfiguration.LoadFromFile(const AFileName: UnicodeString);
var
  I: Integer;
  LLine: UnicodeString;
  LList: TSiStringList;
begin
  LList := TSiStringList.Create;
  try
    LList.LoadFromFile(AFileName);
    Clear;
    for I := 0 to Pred(LList.Count) do
    begin
      LLine := SiTrim(LList[I]);
      if (Length(LLine) > 0) and (LLine[1] <> ';') then
      begin
        Parse(LLine);
      end;
    end;
  finally
    LList.Free;
  end;
end;

procedure TSiConfiguration.Parse(const APair: UnicodeString);
var
  LIndex: Integer;
  LKey, LValue: UnicodeString;
begin
  LIndex := SiPos('=', APair);
  if LIndex > 0 then
  begin
    LKey := SiTrim(Copy(APair, 1, LIndex - 1));
    LValue := SiTrim(Copy(APair, LIndex + 1, Length(APair) - LIndex));

    if not FItems.Contains(LKey) then
    begin
      FKeys.Add(LKey);
    end;

    FItems.Put(LKey, LValue);
  end;
end;

function TSiConfiguration.ReadBoolean(const AKey: UnicodeString;
  const ADefaultValue: Boolean): Boolean;
begin
  Result := FItems.GetBooleanValue(AKey, ADefaultValue);
end;

function TSiConfiguration.ReadColor(const AKey: UnicodeString;
  const ADefaultValue: TColor): TColor;
begin
  Result := FItems.GetColorValue(AKey, ADefaultValue);
end;

function TSiConfiguration.ReadInteger(const AKey: UnicodeString;
  const ADefaultValue: Integer): Integer;
begin
  Result := FItems.GetIntegerValue(AKey, ADefaultValue);
end;

function TSiConfiguration.ReadKey(const AIndex: Integer): UnicodeString;
begin
  Result := FKeys[AIndex];  
end;

function TSiConfiguration.ReadLevel(const AKey: UnicodeString;
  const ADefaultValue: TSiLevel): TSiLevel;
begin
  Result := FItems.GetLevelValue(AKey, ADefaultValue);
end;

function TSiConfiguration.ReadString(const AKey,
  ADefaultValue: UnicodeString): UnicodeString;
begin
  Result := FItems.GetStringValue(AKey, ADefaultValue);
end;

{ TSiPacketQueue }

procedure TSiPacketQueue.Clear;
var
  LPacket: TSiPacket;
begin
  LPacket := Pop;
  while Assigned(LPacket) do
  begin
    DoDelete(LPacket);
    LPacket := Pop;
  end;
end;

procedure TSiPacketQueue.DoDelete(const APacket: TSiPacket);
begin
  if Assigned(FOnDelete) then
  begin
    FOnDelete(Self, APacket);
  end;
end;

function TSiPacketQueue.Pop: TSiPacket;
var
  LItem: PSiPacketQueueItem;
begin
  LItem := FHead;
  if Assigned(LItem) then
  begin
    Result := LItem.Packet;
    FHead := LItem.Next;

    if Assigned(FHead) then
      FHead.Previous := nil
    else
      FTail := nil;

    Dec(FCount);
    Dec(FSize, SizeOf(LItem^) + Result.Size);
    Dispose(LItem);
  end else
    Result := nil;
end;

procedure TSiPacketQueue.Push(const APacket: TSiPacket);
var
  LItem: PSiPacketQueueItem;
begin
  New(LItem);
  ZeroMemory(LItem, SizeOf(LItem^));
  LItem.Packet := APacket;
  if not Assigned(FTail) then
  begin
    FTail := LItem;
    FHead := LItem;
  end else
  begin
    FTail.Next := LItem;
    LItem.Previous := FTail;
    FTail := LItem;
  end;
  Inc(FCount);
  Inc(FSize, SizeOf(LItem^) + APacket.Size);
  Resize;
end;

procedure TSiPacketQueue.Resize;
var
  LPacket: TSiPacket;
begin
  while FBacklog < FSize do
  begin
    LPacket := Pop;
    if not Assigned(LPacket) then
    begin
      FSize := 0;
      Break;
    end;
    DoDelete(LPacket);
  end;
end;

procedure TSiPacketQueue.SetBacklog(const AValue: Int64);
begin
  FBacklog := AValue;
  Resize;
end;

{ TSiBinaryFormatter }

function TSiBinaryFormatter.Compile(const APacket: TSiPacket): Integer;
begin
  ResetStream;
  FPacket := APacket;

  case FPacket.PacketType of
    ptLogEntry: CompileLogEntry;
    ptLogHeader: CompileLogHeader;
    ptWatch: CompileWatch;
    ptControlCommand: CompileControlCommand;
    ptProcessFlow: CompileProcessFlow;
  end;

  FSize := FStream.Position;
  Result := FSize + SizeOf(TSiPacketHeader);
end;

procedure TSiBinaryFormatter.CompileControlCommand;
var
  LHeader: TSiControlCommandHeader;
  LControlCommand: TSiControlCommand;
begin
  LControlCommand := TSiControlCommand(FPacket);
  LHeader.ControlCommandType := Ord(LControlCommand.ControlCommandType);

  if LControlCommand.HasData then
    LHeader.DataLength := LControlCommand.Data.Size
  else
    LHeader.DataLength := 0;

  FStream.Write(LHeader, SizeOf(LHeader));

  if LControlCommand.HasData then
  begin
    FStream.CopyFrom(LControlCommand.Data, 0);
    LControlCommand.Data.Position := 0;
  end;
end;

procedure TSiBinaryFormatter.CompileLogEntry;
var
  LLogEntry: TSiLogEntry;
  LHeader: TSiLogEntryHeader;
  LTitle: TSiUTF8String;
  LHostName: TSiUTF8String;
  LSessionName: TSiUTF8String;
  LAppName: TSiUTF8String;
begin
  LLogEntry := TSiLogEntry(FPacket);
  LTitle := SiUTF8Encode(LLogEntry.Title);
  LHostName := SiUTF8Encode(LLogEntry.HostName);
  LSessionName := SiUTF8Encode(LLogEntry.SessionName);
  LAppName := SiUTF8Encode(LLogEntry.AppName);

  LHeader.LogEntryType := CSiLogEntryTypeLookup[LLogEntry.LogEntryType];
  LHeader.ViewerId := CSiViewerIdLookup[LLogEntry.ViewerId];
  LHeader.AppNameLength := Length(LAppName);
  LHeader.SessionNameLength := Length(LSessionName);
  LHeader.TitleLength := Length(LTitle);
  LHeader.HostNameLength := Length(LHostName);

  if LLogEntry.HasData then
    LHeader.DataLength := LLogEntry.Data.Size
  else
    LHeader.DataLength := 0;

  LHeader.ThreadId := LLogEntry.ThreadId;
  LHeader.ProcessId := LLogEntry.ProcessId;
  LHeader.TimeStamp := LLogEntry.Timestamp;
  LHeader.Color := LLogEntry.Color;

  FStream.Write(LHeader, SizeOf(LHeader));
  WriteString(LAppName, FStream);
  WriteString(LSessionName, FStream);
  WriteString(LTitle, FStream);
  WriteString(LHostName, FStream);

  if LLogEntry.HasData then
  begin
    FStream.CopyFrom(LLogEntry.Data, 0);
    LLogEntry.Data.Position := 0;
  end;
end;

procedure TSiBinaryFormatter.CompileLogHeader;
var
  LLogHeader: TSiLogHeader;
  LContent: TSiUTF8String;
  LContentLength: Integer;
begin
  LLogHeader := TSiLogHeader(FPacket);
  LContent := SiUTF8Encode(LLogHeader.Content);
  LContentLength := Length(LContent);
  FStream.Write(LContentLength, SizeOf(Integer));
  WriteString(LContent, FStream);
end;

procedure TSiBinaryFormatter.CompileProcessFlow;
var
  LProcessFlow: TSiProcessFlow;
  LHeader: TSiProcessFlowHeader;
  LTitle: TSiUTF8String;
  LHostName: TSiUTF8String;
begin
  LProcessFlow := TSiProcessFlow(FPacket);
  LTitle := SiUTF8Encode(LProcessFlow.Title);
  LHostName := SiUTF8Encode(LProcessFlow.HostName);

  LHeader.ProcessFlowType := Ord(LProcessFlow.ProcessFlowType);
  LHeader.TitleLength := Length(LTitle);
  LHeader.HostNameLength := Length(LHostName);
  LHeader.ThreadId := LProcessFlow.ThreadId;
  LHeader.ProcessId := LProcessFlow.ProcessId;
  LHeader.Timestamp := LProcessFlow.Timestamp;

  FStream.Write(LHeader, SizeOf(LHeader));
  WriteString(LTitle, FStream);
  WriteString(LHostName, FStream);
end;

procedure TSiBinaryFormatter.CompileWatch;
var
  LWatch: TSiWatch;
  LHeader: TSiWatchHeader;
  LName: TSiUTF8String;
  LValue: TSiUTF8String;
begin
  LWatch := TSiWatch(FPacket);
  LName := SiUTF8Encode(LWatch.Name);
  LValue := SiUTF8Encode(LWatch.Value);

  LHeader.NameLength := Length(LName);
  LHeader.ValueLength := Length(LValue);
  LHeader.WatchType := Ord(LWatch.WatchType);
  LHeader.Timestamp := LWatch.Timestamp;

  FStream.Write(LHeader, SizeOf(LHeader));
  WriteString(LName, FStream);
  WriteString(LValue, FStream);
end;

procedure TSiBinaryFormatter.Write(const AStream: TStream);
var
  LHeader: TSiPacketHeader;
begin
  if FSize > 0 then
  begin
    LHeader.PacketSize := FSize;
    LHeader.PacketType := CSiPacketTypeLookup[FPacket.PacketType];
    AStream.WriteBuffer(LHeader, SizeOf(LHeader));
    FStream.Position := 0;
    AStream.CopyFrom(FStream, FSize);
  end;
end;

procedure TSiBinaryFormatter.WriteString(const AString: TSiUTF8String;
  const AStream: TStream);
begin
  if AString <> '' then
  begin
    AStream.Write(Pointer(AString)^, Length(AString));
  end;
end;

constructor TSiBinaryFormatter.Create;
begin
  FStream := TMemoryStream.Create;
end;

destructor TSiBinaryFormatter.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TSiBinaryFormatter.ResetStream;
const
  CSiMaxCapacity = 1 * 1024 * 1024;
begin
  if FSize > CSiMaxCapacity then
  begin
    // Reset the stream capacity if the previous packet
    // was very big. This ensures that the amount of memory
    // can shrink again after a big packet has been sent.
    FStream.Clear;
  end else
  begin
    // Only reset the position. This ensures a very good
    // performance since no reallocations are necessary.
    FStream.Position := 0;
  end;
end;

{ TSiTextFormatter }

function TSiTextFormatter.Compile(const APacket: TSiPacket): Integer;
begin
  if APacket.PacketType = ptLogEntry then
  begin
    FLine := SiUTF8Encode(FParser.Expand(TSiLogEntry(APacket)) + #13#10);
    Result := Length(FLine);
  end else
  begin
    Result := 0;
    FLine := '';
  end;
end;

constructor TSiTextFormatter.Create;
begin
  FParser := TSiPatternParser.Create;
end;

destructor TSiTextFormatter.Destroy;
begin
  FreeAndNil(FParser);
  inherited;
end;

function TSiTextFormatter.GetIndent: Boolean;
begin
  Result := FParser.Indent;
end;

function TSiTextFormatter.GetPattern: UnicodeString;
begin
  Result := FParser.Pattern;
end;

procedure TSiTextFormatter.SetIndent(const AValue: Boolean);
begin
  FParser.Indent := AValue;
end;

procedure TSiTextFormatter.SetPattern(const AValue: UnicodeString);
begin
  FParser.Pattern := AValue;
end;

procedure TSiTextFormatter.Write(const AStream: TStream);
begin
  if FLine <> '' then
  begin
    AStream.WriteBuffer(Pointer(FLine)^, Length(FLine));
  end;
end;

{ TSiTextProtocol }

procedure TSiTextProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  inherited;
  ABuilder.AddOption('indent', FIndent);
  ABuilder.AddOption('pattern', FPattern);
end;

constructor TSiTextProtocol.Create;
begin
  inherited;
  FPattern := SiTextPattern;
end;

destructor TSiTextProtocol.Destroy;
begin
  FreeAndNil(FFormatter);
  inherited;
end;

procedure TSiTextProtocol.WriteFooter(const AStream: TStream);
begin

end;

function TSiTextProtocol.WriteHeader(const AStream: TStream;
  const ASize: Int64): Int64;
const
  CSiBom: array[0..2] of Byte = ($EF, $BB, $BF);
begin
  if ASize = 0 then
  begin
    AStream.WriteBuffer(CSiBom, Length(CSiBom));
    Result := Length(CSiBom);
  end else
    Result := ASize;
end;

function TSiTextProtocol.GetDefaultFileName: UnicodeString;
begin
  Result := SiTextFileName;
end;

function TSiTextProtocol.GetFormatter: TSiFormatter;
begin
  if not Assigned(FFormatter) then
  begin
    FFormatter := TSiTextFormatter.Create;
  end;
  Result := FFormatter;
end;

function TSiTextProtocol.GetName: UnicodeString;
begin
  Result := 'text';
end;

function TSiTextProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  if (AOption <> 'encrypt') and (AOption <> 'key') then
  begin
    Result := (AOption = 'pattern') or
              (AOption = 'indent') or
              inherited IsValidOption(AOption);
  end else
    Result := False;
end;

procedure TSiTextProtocol.LoadOptions;
begin
  inherited;
  FPattern := GetStringOption('pattern', SiTextPattern);
  FIndent := GetBooleanOption('indent', SiTextIndent);
  TSiTextFormatter(GetFormatter).Pattern := FPattern;
  TSiTextFormatter(GetFormatter).Indent := FIndent;
end;

{ TSiFormatter }

procedure TSiFormatter.Format(const APacket: TSiPacket;
  const AStream: TStream);
begin
  Compile(APacket);
  Write(AStream);
end;

{ TSiPatternParser }

constructor TSiPatternParser.Create;
begin
  FTokens := TObjectList.Create;
  FBuilder := TSiStringBuilder.Create;
end;

destructor TSiPatternParser.Destroy;
begin
  FreeAndNil(FBuilder);
  FreeAndNil(FTokens);
  inherited;
end;

function TSiPatternParser.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
const
  CSpaces: UnicodeString = '   ';
var
  I, J: Integer;
  LToken: TSiToken;
  LPad: Integer;
  LExpanded: UnicodeString;
begin
  if FTokens.Count > 0 then
  begin
    FBuilder.Clear;

    { Adjust indent level }
    if ALogEntry.LogEntryType = ltLeaveMethod then
    begin
      if FIndentLevel > 0 then
      begin
        Dec(FIndentLevel);
      end;
    end;

    { Then append the tokens }
    for I := 0 to Pred(FTokens.Count) do
    begin
      LToken := TSiToken(FTokens[I]);
      if FIndent and LToken.Indent then
      begin
        { Prepend indentation }
        for J := 1 to FIndentLevel do
        begin
          FBuilder.Append(CSpaces);
        end;
      end;

      LExpanded := LToken.Expand(ALogEntry);
      if LToken.Width < 0 then
      begin
        // Left-aligned
        FBuilder.Append(LExpanded);

        LPad := -LToken.Width - Length(LExpanded);
        for J := 1 to LPad do
        begin
          FBuilder.Append(' ');
        end;
      end else if LToken.Width > 0 then
      begin
        LPad := LToken.Width - Length(LExpanded);
        for J := 1 to LPad do
        begin
          FBuilder.Append(' ');
        end;

        // Right-aligned
        FBuilder.Append(LExpanded);
      end else
        FBuilder.Append(LExpanded);
    end;

    { Adjust indent level }
    if ALogEntry.LogEntryType = ltEnterMethod then
    begin
      Inc(FIndentLevel);
    end;
    
    Result := FBuilder.Text;
  end else
    Result := '';
end;

function TSiPatternParser.Next: TSiToken;
var
  LIsVariable: Boolean;
  LLength: Integer;
  LPosition: Integer;
  LValue: UnicodeString;
begin
  LLength := Length(FPattern);
  if FPosition <= LLength then
  begin
    LPosition := FPosition;

    if FPattern[LPosition] = '%' then
    begin
      LIsVariable := True;
      Inc(LPosition);
    end else
      LIsVariable := False;

    while LPosition <= LLength do
    begin
      if FPattern[LPosition] = '%' then
      begin
        if LIsVariable then Inc(LPosition);
        Break;
      end;
      Inc(LPosition);
    end;

    LValue := Copy(FPattern, FPosition, LPosition - FPosition);
    Result := TSiTokenFactory.GetToken(LValue);
    FPosition := LPosition;
  end else
    Result := nil;
end;

procedure TSiPatternParser.Parse;
var
  LToken: TSiToken;
begin
  FTokens.Clear;
  LToken := Next;
  while Assigned(LToken) do
  begin
    FTokens.Add(LToken);
    LToken := Next;
  end;
end;

procedure TSiPatternParser.SetPattern(const AValue: UnicodeString);
begin
  FPosition := 1;
  FPattern := SiTrim(AValue);
  FIndentLevel := 0;
  Parse;
end;

{ TSiAppNameToken }

function TSiAppNameToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := ALogEntry.AppName;
end;

{ TSiTimestampToken }

function TSiTimestampToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
const
  CSiDateTimeFormat = 'yyyy-mm-dd hh:nn:ss.zzz';
begin
  if Options <> '' then
  begin
    try
      Result := FormatDateTime(Options, ALogEntry.Timestamp);
      Exit;
    except
      // Ignore possible formatting exceptions and use the default
      // format string below.
    end;
  end;
  Result := FormatDateTime(CSiDateTimeFormat, ALogEntry.Timestamp);
end;

{ TSiSessionToken }

function TSiSessionToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := ALogEntry.SessionName;
end;

constructor TSiTitleToken.Create;
begin
  FIndent := True;
end;

function TSiTitleToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := ALogEntry.Title;
end;

{ TSiHostNameToken }

function TSiHostNameToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := ALogEntry.HostName;
end;

{ TSiLiteralToken }

function TSiLiteralToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := Value;
end;

{ TSiColorToken }

function TSiColorToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
var
  LRed, LGreen, LBlue: Byte;
begin
  if ALogEntry.Color <> SiDefaultColor then
  begin
    LRed := ALogEntry.Color and $ff;
    LGreen := (ALogEntry.Color shr 8) and $ff;
    LBlue := (ALogEntry.Color shr 16) and $ff;
    Result := SiFormat('$%.2x%.2x%.2x', [LRed, LGreen, LBlue]);
  end else
    Result := '<default>';
end;

{ TSiLogEntryTypeToken }

function TSiLogEntryTypeToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := CSiLogEntryTypeString[ALogEntry.LogEntryType];
end;

{ TSiViewerIdToken }

function TSiViewerIdToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := CSiViewerIdString[ALogEntry.ViewerId];
end;

{ TSiThreadIdToken }

function TSiThreadIdToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := IntToStr(ALogEntry.ThreadId);
end;

{ TSiProcessIdToken }

function TSiProcessIdToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := IntToStr(ALogEntry.ProcessId);
end;

{ TSiLevelToken }

function TSiLevelToken.Expand(const ALogEntry: TSiLogEntry): UnicodeString;
begin
  Result := CSiLevelString[ALogEntry.Level]
end;

{ TSiTokenFactory }

class function TSiTokenFactory.CreateLiteral(const AValue: UnicodeString):
  TSiToken;
begin
  Result := TSiLiteralToken.Create;
  Result.Options := '';
  Result.Value := AValue;
end;

class function TSiTokenFactory.GetToken(const AValue: UnicodeString): TSiToken;
type
  TSiTokenClass = class of TSiToken;

const
  CTokenLookup: array[0..10] of record
    Name: UnicodeString;
    Impl: TSiTokenClass
  end =
  (
    (Name: '%appname%'; Impl: TSiAppNameToken),
    (Name: '%session%'; Impl: TSiSessionToken),
    (Name: '%hostname%'; Impl: TSiHostNameToken),
    (Name: '%title%'; Impl: TSiTitleToken),
    (Name: '%timestamp%'; Impl: TSiTimestampToken),
    (Name: '%level%'; Impl: TSiLevelToken),
    (Name: '%color%'; Impl: TSiColorToken),
    (Name: '%logentrytype%'; Impl: TSiLogEntryTypeToken),
    (Name: '%viewerid%'; Impl: TSiViewerIdToken),
    (Name: '%thread%'; Impl: TSiThreadIdToken),
    (Name: '%process%'; Impl: TSiProcessIdToken)
  );

var
  I: Integer;
  LValue: UnicodeString;
  LOptions: UnicodeString;
  LIndex: Integer;
  LLength: Integer;
  LWidth: UnicodeString;
begin
  LLength := Length(AValue);

  if LLength <= 2 then
  begin
    Result := CreateLiteral(AValue);
    Exit;
  end;

  if (AValue[1] <> '%') or (AValue[LLength] <> '%') then
  begin
    Result := CreateLiteral(AValue);
    Exit;
  end;

  LOptions := '';
  LValue := AValue;

  // Extract the token options: %token{options}%
  if LValue[LLength - 1] = '}' then
  begin
    LIndex := SiPos('{', LValue);
    if LIndex > 0 then
    begin
      Inc(LIndex);
      LOptions := Copy(LValue, LIndex, LLength - LIndex - 1);
      Delete(LValue, LIndex - 1, LLength - LIndex + 1);
      LLength := Length(LValue);
    end;
  end;

  // Extract the token width: %token,width%
  LWidth := '';
  LIndex := SiPos(',', LValue);
  if LIndex > 0 then
  begin
    Inc(LIndex);
    LWidth := Copy(LValue, LIndex, LLength - LIndex);
    Delete(LValue, LIndex - 1, LLength - LIndex + 1);
  end;

  // Find the token class
  for I := Low(CTokenLookup) to High(CTokenLookup) do
  begin
    if SiSameText(CTokenLookup[I].Name, LValue) then
    begin
      Result := CTokenLookup[I].Impl.Create;
      Result.Options := LOptions;
      Result.Value := AValue;
      Result.Width := ParseWidth(LWidth);
      Exit;
    end;
  end;

  // Default handling
  Result := CreateLiteral(AValue);
end;

class function TSiTokenFactory.ParseWidth(const AValue: UnicodeString):
  Integer;
var
  LValue: UnicodeString;
begin
  LValue := SiTrim(AValue);
  if LValue = '' then
  begin
    Result := 0;
    Exit;
  end;
  try
    Result := StrToInt(LValue);
  except
    Result := 0;
  end;
end;

{ TSiConnectionsBuilder }

procedure TSiConnectionsBuilder.AddOption(const AKey: UnicodeString;
  const AValue: Integer);
begin
  AddOption(AKey, IntToStr(AValue));
end;

procedure TSiConnectionsBuilder.AddOption(const AKey: UnicodeString;
  const AValue: Boolean);
begin
  if AValue then
    AddOption(AKey, 'true')
  else
    AddOption(AKey, 'false');
end;

procedure TSiConnectionsBuilder.AddOption(const AKey, AValue: UnicodeString);
begin
  if FHasOptions then
  begin
    Append(', ');
  end;
  Append(AKey);
  Append('="');
  Append(Escape(AValue));
  Append('"');
  FHasOptions := True;
end;

procedure TSiConnectionsBuilder.AddOption(const AKey: UnicodeString;
  const AValue: TSiLevel);
begin
  AddOption(AKey, SiLowerCase(CSiLevelString[AValue]));
end;

procedure TSiConnectionsBuilder.AddOption(const AKey: UnicodeString;
  const AValue: TSiFileRotate);
begin
  AddOption(AKey, SiLowerCase(CSiFileRotateString[AValue]));
end;

procedure TSiConnectionsBuilder.Append(const AValue: UnicodeString);
begin
  FBuilder.Append(AValue);
end;

procedure TSiConnectionsBuilder.BeginProtocol(const AProtocol: UnicodeString);
begin
  if FBuilder.Count > 0 then
  begin
    Append(', ');
  end;
  Append(AProtocol);
  Append('(');
  FHasOptions := False;
end;

procedure TSiConnectionsBuilder.Clear;
begin
  FBuilder.Clear;
end;

constructor TSiConnectionsBuilder.Create;
begin
  FBuilder := TSiStringBuilder.Create;
end;

destructor TSiConnectionsBuilder.Destroy;
begin
  FreeAndNil(FBuilder);
  inherited;
end;

procedure TSiConnectionsBuilder.EndProtocol;
begin
  Append(')');
end;

function TSiConnectionsBuilder.Escape(const AValue: UnicodeString):
  UnicodeString;
var
  I, J: Integer;
begin
  if SiPos('"', AValue) > 0 then
  begin
    J := 1;
    SetLength(Result, 2 * Length(AValue));

    for I := 1 to Length(AValue) do
    begin
      Result[J] := AValue[I];
      if AValue[I] = '"' then
      begin
        Inc(J);
        Result[J] := '"';
      end;
      Inc(J);
    end;

    SetLength(Result, J - 1);
  end else
    Result := AValue;
end;

function TSiConnectionsBuilder.GetConnections: UnicodeString;
begin
  Result := FBuilder.Text;
end;

{ TSiConnectionsParser }

procedure TSiConnectionsParser.DoProtocol(
  const ACallback: TSiConnectionsParserEvent;
  const AProtocol, AOptions: UnicodeString);
var
  LProtocol: UnicodeString;
  LOptions: UnicodeString;
begin
  LOptions := SiTrim(AOptions);
  LProtocol := SiTrim(SiLowerCase(AProtocol));
  ACallback(Self, LProtocol, LOptions);
end;

procedure TSiConnectionsParser.InternalParse(const AConnections: UnicodeString;
  const ACallback: TSiConnectionsParserEvent);
var
  I: Integer;
  LProtocol: UnicodeString;
  LOptions: UnicodeString;
  LConnections: UnicodeString;
  LQuoted: Boolean;
begin
  I := 1;
  LConnections := AConnections + #0;

  // Parse the connections string
  while LConnections[I] <> #0 do
  begin
    // Get the protocol name
    while (LConnections[I] <> '(') and (LConnections[I] <> #0) do
    begin
      LProtocol := LProtocol + LConnections[I];
      Inc(I);
    end;

    if LConnections[I] <> '(' then
    begin
      raise ESmartInspectError.Create('Missing "(" at position ' +
        IntToStr(I));
    end;

    Inc(I);
    LProtocol := SiTrim(SiLowerCase(LProtocol));

    // Get all options
    LQuoted := False;
    while LConnections[I] <> #0 do
    begin
      if LConnections[I] = '"' then
      begin
        if LConnections[I+1] = '"' then
        begin
          Inc(I);
          LOptions := LOptions + '"';
        end else
          LQuoted := not LQuoted;
      end else if (LConnections[I] = ')') then
      begin
        if not LQuoted then
        begin
          Break;
        end;
      end;
      LOptions := LOptions + LConnections[I];
      Inc(I);
    end;

    if LQuoted then
    begin
      raise ESmartInspectError.CreateFmt(
        'Quoted values not closed at protocol "%s"', [LProtocol]);
    end;

    if LConnections[I] <> ')' then
    begin
      raise ESmartInspectError.Create('Missing ")" at position ' +
        IntToStr(I));
    end;

    Inc(I);

    if LConnections[I] = ',' then
    begin
      Inc(I);
    end;

    DoProtocol(ACallback, LProtocol, LOptions);
    SetLength(LProtocol, 0);
    SetLength(LOptions, 0);
  end;
end;

procedure TSiConnectionsParser.Parse(const AConnections: UnicodeString;
  const ACallback: TSiConnectionsParserEvent);
var
  LConnections: UnicodeString;
begin
  LConnections := SiTrim(AConnections);
  if Length(LConnections) > 0 then
  begin
    InternalParse(LConnections, ACallback);
  end;
end;

{ TSiOptionsParser }

procedure TSiOptionsParser.DoOption(const ACallback: TSiOptionsParserEvent;
  const AProtocol, AKey, AValue: UnicodeString);
var
  LKey: UnicodeString;
  LValue: UnicodeString;
begin
  LValue := SiTrim(AValue);
  LKey := SiTrim(SiLowerCase(AKey));
  ACallback(Self, AProtocol, LKey, LValue);
end;

procedure TSiOptionsParser.InternalParse(const AProtocol,
  AOptions: UnicodeString; const ACallback: TSiOptionsParserEvent);
var
  I: Integer;
  LKey, LValue: UnicodeString;
  LOptions: UnicodeString;
  LQuoted: Boolean;
begin
  I := 1;
  LOptions := AOptions + #0;
  while LOptions[I] <> #0 do
  begin
    // Get key
    while (LOptions[I] <> '=') and (LOptions[I] <> #0) do
    begin
      LKey := LKey + LOptions[I];
      Inc(I);
    end;

    if LOptions[I] = '=' then
    begin
      Inc(I);
    end else
    begin
      // The '=' character is missing.
      raise ESmartInspectError.CreateFmt('Missing "=" at "%s" protocol',
        [SiLowerCase(AProtocol)]);
    end;

    // Get value
    LQuoted := False;
    while LOptions[I] <> #0 do
    begin
      if LOptions[I] = '"' then
      begin
        Inc(I);
        if LOptions[I] <> '"' then
        begin
          LQuoted := not LQuoted;
          Continue;
        end;
      end else if (LOptions[I] = ',') then
      begin
        if not LQuoted then
        begin
          Break;
        end;
      end;
      LValue := LValue + LOptions[I];
      Inc(I);
    end;

    if LQuoted then
    begin
      // The ending '"' character is missing
      raise ESmartInspectError.CreateFmt(
        'Quoted value not closed at protocol "%s"',
        [SiLowerCase(AProtocol)]);
    end;

    if LOptions[I] = ',' then
    begin
      Inc(I);
    end;

    DoOption(ACallback, AProtocol, LKey, LValue);
    SetLength(LKey, 0);
    SetLength(LValue, 0);
  end;
end;

procedure TSiOptionsParser.Parse(const AProtocol, AOptions: UnicodeString;
  const ACallback: TSiOptionsParserEvent);
var
  LOptions: UnicodeString;
begin
  LOptions := SiTrim(AOptions);
  if Length(LOptions) > 0 then
  begin
    InternalParse(AProtocol, LOptions, ACallback);
  end;
end;

{ TSiFileRotater }

constructor TSiFileRotater.Create;
begin
  FMode := frNone;
end;

function TSiFileRotater.GetTimeValue(const ANow: TDateTime): Integer;
var
  LDate: Cardinal;
begin
  LDate := Trunc(DateOf(ANow));

  case FMode of
    frHourly: Result := LDate * 24 + HourOf(ANow);
    frDaily: Result := LDate;
    frWeekly: Result := Trunc(IncDay(LDate, -(DayOfTheWeek(LDate) - 1)));
    frMonthly: Result := YearOf(LDate) * 12 + MonthOf(LDate);
  else
    Result := 0;
  end;
end;

procedure TSiFileRotater.Initialize(const ANow: TDateTime);
begin
  FTimeValue := GetTimeValue(ANow);
end;

function TSiFileRotater.Update(const ANow: TDateTime): Boolean;
var
  LTimeValue: Integer;
begin
  LTimeValue := GetTimeValue(ANow);
  if LTimeValue <> FTimeValue then
  begin
    FTimeValue := LTimeValue;
    Result := True;
  end else
    Result := False;
end;

{ TSiProtocolCommand }

procedure TSiProtocolCommand.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

constructor TSiProtocolCommand.Create(const AAction: Integer;
  const AState: TObject);
begin
  FRefCount := 1;
  FAction := AAction;
  FState := AState;
end;

procedure TSiProtocolCommand.Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
  begin
    Free;
  end;
end;

{ TSiMemoryProtocol }

procedure TSiMemoryProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  inherited;
  ABuilder.AddOption('maxsize', FMaxSize div 1024);
  ABuilder.AddOption('astext', FAsText);
  ABuilder.AddOption('indent', FIndent);
  ABuilder.AddOption('pattern', FPattern);
end;

constructor TSiMemoryProtocol.Create;
begin
  inherited;
  LoadOptions; { Set default options }
end;

procedure TSiMemoryProtocol.DeletePacket(ASender: TSiPacketQueue;
  APacket: TSiPacket);
begin
  APacket.Release; { Decrement reference counter }
end;

destructor TSiMemoryProtocol.Destroy;
begin
  FreeAndNil(FFormatter);
  inherited;
end;

procedure TSiMemoryProtocol.FlushToProtocol(const AProtocol: TSiProtocol);
var
  LPacket: TSiPacket;
begin
  LPacket := FQueue.Pop;
  while Assigned(LPacket) do
  begin
    AProtocol.WritePacket(LPacket);
    LPacket.Release; { Decrement reference counter }
    LPacket := FQueue.Pop;
  end;
end;

procedure TSiMemoryProtocol.FlushToStream(const AStream: TStream);
const
  CSiBom: array[0..2] of Byte = ($EF, $BB, $BF);
var
  LPacket: TSiPacket;
begin
  if FAsText then
    AStream.Write(CSiBom, Length(CSiBom))
  else
    AStream.Write(SiSILF, Length(SiSILF));

  LPacket := FQueue.Pop;
  while Assigned(LPacket) do
  begin
    FFormatter.Format(LPacket, AStream);
    LPacket.Release; { Decrement reference counter }
    LPacket := FQueue.Pop;
  end;
end;

function TSiMemoryProtocol.GetName: UnicodeString;
begin
  Result := 'mem';
end;

procedure TSiMemoryProtocol.InitializeFormatter;
begin
  FreeAndNil(FFormatter); { Free a previous instance }

  if FAsText then
  begin
    FFormatter := TSiTextFormatter.Create;
    TSiTextFormatter(FFormatter).Pattern := FPattern;
    TSiTextFormatter(FFormatter).Indent := FIndent;
  end else
    FFormatter := TSiBinaryFormatter.Create;
end;

procedure TSiMemoryProtocol.InternalConnect;
begin
  FQueue := TSiPacketQueue.Create;
  FQueue.Backlog := FMaxSize;
  FQueue.OnDelete := DeletePacket;
end;

procedure TSiMemoryProtocol.InternalDisconnect;
begin
  if Assigned(FQueue) then
  begin
    FQueue.Clear;
    FreeAndNil(FQueue);
  end;
end;

procedure TSiMemoryProtocol.InternalDispatch(
  const ACommand: TSiProtocolCommand);
begin
  if Assigned(ACommand) then
  begin
    if ACommand.State is TStream then
      FlushToStream(TStream(ACommand.State))
    else if ACommand.State is TSiProtocol then
      FlushToProtocol(TSiProtocol(ACommand.State));
  end;
end;

procedure TSiMemoryProtocol.InternalWritePacket(const APacket: TSiPacket);
begin
  APacket.AddRef; { Increment reference counter }
  FQueue.Push(APacket);
end;

function TSiMemoryProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  Result := (AOption = 'astext') or
            (AOption = 'maxsize') or
            (AOption = 'pattern') or
            (AOption = 'indent') or
            inherited IsValidOption(AOption);
end;

procedure TSiMemoryProtocol.LoadOptions;
begin
  inherited;
  FAsText := GetBooleanOption('astext', SiMemoryAsText);
  FMaxSize := GetSizeOption('maxsize', SiMemoryMaxSize);
  FPattern := GetStringOption('pattern', SiMemoryPattern);
  FIndent := GetBooleanOption('indent', SiMemoryIndent);
  InitializeFormatter;
end;

{ TSiHash }

const
  SiLookupTablePrimes: array[0..20] of Integer =
  (
    7, 17, 37, 79, 163, 331, 673, 1361, 2729, 5471, 10949, 21911,
    43853, 87719, 175447, 350899, 701819, 1403641, 2807303, 5614657,
    11229331
  );
  SiLookupTablePrimeIndex = 0;
  SiLookupTableLoadFactor = 0.75;

procedure TSiHash.Clear;
var
  I: Integer;
  LItem, LNext: PSiHashItem;
begin
  FLoad := 0;
  if FCount > 0 then
  begin
    for I := 0 to Pred(Length(FItems)) do
    begin
      LItem := FItems[I];
      while Assigned(LItem) do
      begin
        LNext := LItem.Next;
        DoDispose(LItem.Value);
        Dispose(LItem);
        LItem := LNext;
      end;
      FItems[I] := nil;
    end;
    FCount := 0;
  end;
  Restart; { Reset iterator }
end;

function TSiHash.Contains(const AKey: UnicodeString): Boolean;
begin
  Result := Assigned(Find(AKey)^);
end;

constructor TSiHash.Create;
begin
  FLoadFactor := SiLookupTableLoadFactor;
  FPrimeIndex := SiLookupTablePrimeIndex;
  SetLength(FItems, SiLookupTablePrimes[FPrimeIndex]);
end;

destructor TSiHash.Destroy;
begin
  Clear;
  inherited;
end;

function TSiHash.Find(const AKey: UnicodeString): PPSiHashItem;
var
  LIndex: Cardinal;
begin
  Result := FindEx(AKey, LIndex);
end;

function TSiHash.FindEx(const AKey: UnicodeString;
  var AIndex: Cardinal): PPSiHashItem;
begin
  AIndex := HashOf(AKey) mod Cardinal(Length(FItems));
  Result := @FItems[AIndex];
  while Assigned(Result^) do
  begin
    if SiSameText(Result^.Key, AKey) then
    begin
      Break;
    end;
    Result := @Result^.Next;
  end;
end;

function TSiHash.GetCurrent: Pointer;
begin
  if Assigned(FCurrent) then
    Result := FCurrent.Value
  else
    Result := nil;
end;

function TSiHash.GetCurrentKey: UnicodeString;
begin
  if Assigned(FCurrent) then
    Result := FCurrent.Key
  else
    Result := '';
end;

procedure TSiHash.Grow(const ACapacity: Cardinal);
var
  LIndex: Cardinal;
  LItem, LNext: PSiHashItem;
  LItems: PSiHashItems;
  I: Integer;
begin
  FLoad := 0;
  SetLength(LItems, ACapacity); // Increase size

  for I := 0 to Pred(Length(FItems)) do
  begin
    LItem := FItems[I];
    while Assigned(LItem) do
    begin
      LIndex := LItem.Hash mod ACapacity;

      LNext := LItem.Next; // Save next
      LItem.Next := LItems[LIndex];
      LItems[LIndex] := LItem;

      if LItem.Next = nil then
      begin
        Inc(FLoad);
      end;

      LItem := LNext;
    end;
  end;

  FItems := LItems; // Switch item table
end;

function TSiHash.HashOf(const AKey: UnicodeString): Cardinal;
var
  I: Integer;
  LKey: UnicodeString;
begin
  { djb2 (k=33) hash algorithm }
  LKey := SiLowerCase(AKey);
  Result := 5381;
  for I := 1 to Length(LKey) do
  begin
    Result := ((Result shl 5) + Result) + Ord(LKey[I]);
  end;
end;

function TSiHash.InternalGet(const AKey: UnicodeString): Pointer;
var
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if Assigned(LItem) then
    Result := LItem.Value
  else
    Result := nil;
end;

procedure TSiHash.InternalPut(const AKey: UnicodeString; const AValue: Pointer);
var
  LHash, LIndex: Cardinal;
  LItem: PSiHashItem;
begin
  LHash := HashOf(AKey);
  LIndex := LHash mod Cardinal(Length(FItems));

  New(LItem);
  LItem.Key := AKey;
  LItem.Value := AValue;
  LItem.Next := FItems[LIndex];
  LItem.Hash := LHash;
  FItems[LIndex] := LItem;

  if LItem.Next = nil then
  begin
    Inc(FLoad);
    Resize;
  end;

  Inc(FCount);
  Restart; { Reset iterator }
end;

function TSiHash.Next: Boolean;
var
  LLength: Integer;
begin
  Result := FCurrentIndex < Length(FItems);
  if Result then
  begin
    if Assigned(FCurrent) then
    begin
      if not Assigned(FCurrent.Next) then
      begin
        { Start with the next index on a successive call }
        Inc(FCurrentIndex);
        FCurrent := nil;
      end;
    end;

    if Assigned(FCurrent) and Assigned(FCurrent.Next) then
    begin
      { Simply return the next item in the linked list }
      FCurrent := FCurrent.Next;
    end else
    begin
      LLength := Length(FItems);
      while FCurrentIndex < LLength do
      begin
        if Assigned(FItems[FCurrentIndex]) then
        begin
          FCurrent := FItems[FCurrentIndex];
          Break;
        end;
        Inc(FCurrentIndex);
      end;
    end;

    Result := Assigned(FCurrent);
  end;
end;

procedure TSiHash.Remove(const AKey: UnicodeString);
var
  LHelper: PPSiHashItem;
  LItem: PSiHashItem;
  LIndex: Cardinal;
begin
  LHelper := FindEx(AKey, LIndex);
  LItem := LHelper^;
  if Assigned(LItem) then
  begin
    Dec(FCount);
    LHelper^ := LItem.Next;
    if FItems[LIndex] = nil then
    begin
      Dec(FLoad);
    end;
    DoDispose(LItem.Value);
    Dispose(LItem);
  end;
  Restart; { Reset iterator }
end;

procedure TSiHash.Resize;
begin
  if (Length(FItems) * FLoadFactor <= FLoad) and
     (FPrimeIndex < Length(SiLookupTablePrimes) - 1) then
  begin
    Inc(FPrimeIndex);
    Grow(SiLookupTablePrimes[FPrimeIndex]);
  end;
end;

procedure TSiHash.Restart;
begin
  FCurrentIndex := 0;
  FCurrent := nil;
end;

{ TSiStringHash }

procedure TSiStringHash.Add(const AKey, AValue: UnicodeString);
begin
  if not Contains(AKey) then
  begin
    Put(AKey, AValue);
  end;
end;

procedure TSiStringHash.DoDispose(const AValue: Pointer);
begin
  Dispose(PUnicodeString(AValue));
end;

function TSiStringHash.Get(const AKey: UnicodeString): UnicodeString;
var
  P: PUnicodeString;
begin
  P := InternalGet(AKey);
  if Assigned(P) then
    Result := P^
  else
    Result := '';
end;

function TSiStringHash.GetCurrentValue: UnicodeString;
var
  LItem: PUnicodeString;
begin
  LItem := PUnicodeString(Current);
  if Assigned(LItem) then
    Result := LItem^
  else
    Result := '';
end;

procedure TSiStringHash.Put(const AKey, AValue: UnicodeString);
var
  P: PUnicodeString;
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if not Assigned(LItem) then
  begin
    New(P);
    P^ := AValue;
    InternalPut(AKey, P);
  end else
    PUnicodeString(LItem.Value)^ := AValue;
end;

{ TSiObjectHash }

procedure TSiObjectHash.Add(const AKey: UnicodeString; const AValue: TObject);
begin
  if not Contains(AKey) then
  begin
    Put(AKey, AValue);
  end;
end;

constructor TSiObjectHash.Create(const AOwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := AOwnsObjects;
end;

procedure TSiObjectHash.DoDispose(const AValue: Pointer);
begin
  if FOwnsObjects then
  begin
    TObject(AValue).Free;
  end;
end;

function TSiObjectHash.Get(const AKey: UnicodeString): TObject;
begin
  Result := TObject(InternalGet(AKey));
end;

function TSiObjectHash.GetCurrentValue: TObject;
begin
  Result := TObject(Current);
end;

procedure TSiObjectHash.Put(const AKey: UnicodeString; const AValue: TObject);
var
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if Assigned(LItem) then
  begin
    if FOwnsObjects then
    begin
      TObject(LItem.Value).Free;
    end;
    LItem.Value := AValue;
  end else
    InternalPut(AKey, AValue);
end;

{ TSiStringList }

procedure TSiStringList.Add(const AString: UnicodeString);
begin
  if Succ(FCount) > FLength then
  begin
    Grow;
  end;
  FItems[FCount] := AString;
  Inc(FCount);
end;

procedure TSiStringList.Clear;
begin
  FCount := 0;
  SetLength(FItems, 0);
  FLength := 0;
end;

procedure TSiStringList.Delete(const AIndex: Integer);
begin
  if (AIndex >= FCount) or (AIndex < 0) then
  begin
    raise ESmartInspectError.Create(SiIndexOutOfBoundsError);
  end;
  SetLength(FItems[AIndex], 0);
  Dec(FCount);
  if AIndex < FCount then
  begin
    Move(FItems[AIndex + 1], FItems[AIndex], (FCount - AIndex) *
      SizeOf(UnicodeString));
    Pointer(FItems[FCount]) := nil; { Prevents double free }
  end;
end;

destructor TSiStringList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSiStringList.Exchange(const I, J: Integer);
var
  S: UnicodeString;
begin
  // TODO: Optimize me
  S := FItems[I];
  FItems[I] := FItems[J];
  FItems[J] := S;
end;

function TSiStringList.Get(AIndex: Integer): UnicodeString;
begin
  if (AIndex >= FCount) or (AIndex < 0) then
  begin
    raise ESmartInspectError.Create(SiIndexOutOfBoundsError);
  end;
  Result := FItems[AIndex];
end;

function TSiStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TSiStringList.GetText: UnicodeString;
const
  CLineBreak: UnicodeString = #13#10;
var
  LSize, I: Integer;
  LBuilder: TSiStringBuilder;
begin
  if FCount <> 0 then
  begin
    LSize := 0;
    for I := 0 to Pred(FCount) do
    begin
      Inc(LSize, Length(FItems[I]));
      Inc(LSize, Length(CLineBreak));
    end;

    LBuilder := TSiStringBuilder.Create(LSize);
    try
      for I := 0 to Pred(FCount) do
      begin
        LBuilder.Append(FItems[I]);
        LBuilder.Append(CLineBreak);
      end;
      Result := LBuilder.Text;
    finally
      LBuilder.Free;
    end;
  end else
    Result := '';
end;

procedure TSiStringList.Grow;
begin
  if FLength = 0 then
    FLength := $04
  else
    FLength := FLength shl 1;
  SetLength(FItems, FLength);
end;

function TSiStringList.IndexOf(const AString: UnicodeString): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Pred(FCount) do
  begin
    if SiSameStr(FItems[I], AString) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

procedure TSiStringList.LoadFromFile(const AFileName: UnicodeString);
var
  LStream: TStream;
begin
  LStream := TSiFileStream.Create(AFileName, GENERIC_READ,
    OPEN_EXISTING);
  try
    LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TSiStringList.LoadFromStream(const AStream: TStream);
type
  TSiCharacterSet = (csUcs2Le, csUcs2Be, csUtf8, csAnsi);
var
  LUtf8: array[0..2] of Byte;
  LUnicode: array[0..1] of Byte;
  LCharSet: TSiCharacterSet;
  LLeft: Integer;
begin
  LLeft := AStream.Size - AStream.Position;
  LCharSet := csAnsi;

  { Is the file Utf8 encoded? }
  if LLeft >= Length(LUtf8) then
  begin
    AStream.ReadBuffer(LUtf8, Length(LUtf8));
    if (LUtf8[0] = $EF) and (LUtf8[1] = $BB) and (LUtf8[2] = $BF) then
    begin
      LCharSet := csUtf8;
      Dec(LLeft, Length(LUtf8));
    end else
      AStream.Seek(-Length(LUtf8), soCurrent);
  end;

  { Or in UCS2 little or big endian? }
  if LCharSet <> csUtf8 then
  begin
    if LLeft > Length(LUnicode) then
    begin
      AStream.ReadBuffer(LUnicode, Length(LUnicode));
      if (LUnicode[0] = $FF) and (LUnicode[1] = $FE) then
      begin
        LCharSet := csUcs2Le;
        Dec(LLeft, Length(LUnicode));
      end else if (LUnicode[0] = $FE) and (LUnicode[1] = $FF) then
      begin
        LCharSet := csUcs2Be;
        Dec(LLeft, Length(LUnicode));
      end else
        AStream.Seek(-Length(LUnicode), soCurrent);
    end;
  end;

  if LLeft > 0 then
  begin
    { Decode file and set list content }
    case LCharSet of
      csUcs2Be: Text := ReadUcs2Be(AStream);
      csUcs2Le: Text := ReadUcs2Le(AStream);
      csUtf8: Text := ReadUtf8(AStream);
      csAnsi: Text := ReadAnsi(AStream);
    end;
  end else
    Clear; { Text file is empty }
end;

function TSiStringList.Partition(const P, R: Integer): Integer;
var
  X: UnicodeString;
  J, I: Integer;
begin
  X := FItems[R];
  I := Pred(P);

  for J := P to Pred(R) do
  begin
    if SiCompareStr(FItems[J], X) < 0 then
    begin
      Inc(I);
      Exchange(I, J);
    end;
  end;

  Exchange(Succ(I), R);
  Result := Succ(I);
end;

procedure TSiStringList.Put(AIndex: Integer; const AValue: UnicodeString);
begin
  if (AIndex >= FCount) or (AIndex < 0) then
  begin
    raise ESmartInspectError.Create(SiIndexOutOfBoundsError);
  end;
  FItems[AIndex] := AValue;
end;

procedure TSiStringList.QuickSort(const P, R: Integer);
var
  Q: Integer;
begin
  if P < R then
  begin
    Q := Partition(P, R);
    QuickSort(P, Pred(Q));
    QuickSort(Succ(Q), R);
  end;
end;

function TSiStringList.ReadAnsi(const AStream: TStream): UnicodeString;
var
  LTemp: AnsiString;
  LLeft: Int64;
begin
  LLeft := AStream.Size - AStream.Position;
  SetLength(LTemp, LLeft);
  AStream.ReadBuffer(Pointer(LTemp)^, Length(LTemp));
  Result := UnicodeString(LTemp);
end;

function TSiStringList.ReadUcs2Le(const AStream: TStream): UnicodeString;
var
  LLeft: Int64;
begin
  LLeft := AStream.Size - AStream.Position;
  SetLength(Result, (LLeft div SizeOf(WideChar)));
  AStream.ReadBuffer(Pointer(Result)^, Length(Result) * SizeOf(WideChar));
end;

function TSiStringList.ReadUcs2Be(const AStream: TStream): UnicodeString;
var
  P: PWord;
begin
  Result := ReadUcs2Le(AStream);
  P := Pointer(Result);
  while P^ <> 0 do
  begin
    P^ := MakeWord(HiByte(P^), LoByte(P^));
    Inc(P);
  end;
end;

function TSiStringList.ReadUtf8(const AStream: TStream): UnicodeString;
var
  LTemp: TSiUTF8String;
  LLeft: Int64;
begin
  LLeft := AStream.Size - AStream.Position;
  SetLength(LTemp, LLeft);
  AStream.ReadBuffer(Pointer(LTemp)^, Length(LTemp));
  Result := SiUTF8Decode(LTemp);
end;

procedure TSiStringList.Remove(const AString: UnicodeString);
var
  LIndex: Integer;
begin
  LIndex := IndexOf(AString);
  if LIndex <> -1 then
  begin
    Delete(LIndex);
  end;
end;

procedure TSiStringList.SetText(const AValue: UnicodeString);
var
  S: UnicodeString;
  P, H: PWideChar;
begin
  Clear;
  if AValue <> '' then
  begin
    P := PWideChar(AValue);
    while P^ <> #0 do
    begin
      H := P;
      while (P^ <> #0) and (P^ <> #10) and (P^ <> #13) do
      begin
        Inc(P);
      end;

      if P - H > 0 then
        SetString(S, H, P - H)
      else
        S := '';

      Add(S);
      if P^ = #13 then Inc(P);
      if P^ = #10 then Inc(P);
    end;
  end;
end;

procedure TSiStringList.Sort;
begin
  QuickSort(0, Pred(FCount));
end;

{ TSiStringBuilder }

procedure TSiStringBuilder.Append(const AString: UnicodeString);
var
  LLength: Integer;
  P: PWideChar;
begin
  if AString <> '' then
  begin
    LLength := Length(AString);
    while LLength + FCount > FCapacity do
    begin
      Grow;
    end;
    P := FBuffer + FCount;
    CopyMemory(P, Pointer(AString), LLength * SizeOf(WideChar));
    Inc(FCount, LLength);
  end;
end;

procedure TSiStringBuilder.Clear;
const
  CMaxSize = 1 * 1024 * 1024; { 1MB }
begin
  FCount := 0;
  if FCapacity >= CMaxSize then
  begin
    // Reset only if the buffer was very large. This prevents
    // that memory is never released after building a big string
    // but also guarantees a good performance if an object is
    // used more than once.
    Reset($100);
  end;
end;

constructor TSiStringBuilder.Create(const ACapacity: Integer);
begin
  Reset(ACapacity);
end;

destructor TSiStringBuilder.Destroy;
begin
  Reset(0);
  inherited;
end;

function TSiStringBuilder.GetText: UnicodeString;
begin
  if FCount > 0 then
    SetString(Result, FBuffer, FCount)
  else
    Result := '';
end;

procedure TSiStringBuilder.Grow;
begin
  FCapacity := FCapacity shl 1;
  ReallocMem(FBuffer, FCapacity * SizeOf(WideChar));
end;

procedure TSiStringBuilder.Reset(const ACapacity: Integer);
begin
  FCount := 0;
  ReallocMem(FBuffer, ACapacity * SizeOf(WideChar));
  FCapacity := ACapacity;
end;

{ TSiIntegerHash }

procedure TSiIntegerHash.Add(const AKey: UnicodeString; const AValue: Integer);
begin
  if not Contains(AKey) then
  begin
    Put(AKey, AValue);
  end;
end;

function TSiIntegerHash.Dec(const AKey: UnicodeString): Integer;
var
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if Assigned(LItem) then
  begin
    Result := Pred(PInteger(LItem.Value)^);
    PInteger(LItem.Value)^ := Result;
  end else
  begin
    Put(AKey, -1);
    Result := -1;
  end;
end;

procedure TSiIntegerHash.DoDispose(const APointer: Pointer);
begin
  Dispose(PInteger(APointer));
end;

function TSiIntegerHash.Get(const AKey: UnicodeString): Integer;
var
  P: PInteger;
begin
  P := InternalGet(AKey);
  if Assigned(P) then
    Result := P^
  else
    Result := -1;
end;

function TSiIntegerHash.GetCurrentValue: Integer;
var
  LItem: PInteger;
begin
  LItem := PInteger(Current);
  if Assigned(LItem) then
    Result := LItem^
  else
    Result := -1;
end;

function TSiIntegerHash.Inc(const AKey: UnicodeString): Integer;
var
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if Assigned(LItem) then
  begin
    Result := Succ(PInteger(LItem.Value)^);
    PInteger(LItem.Value)^ := Result;
  end else
  begin
    Put(AKey, 1);
    Result := 1;
  end;
end;

procedure TSiIntegerHash.Put(const AKey: UnicodeString; const AValue: Integer);
var
  P: PInteger;
  LItem: PSiHashItem;
begin
  LItem := Find(AKey)^;
  if not Assigned(LItem) then
  begin
    New(P);
    P^ := AValue;
    InternalPut(AKey, P);
  end else
    PInteger(LItem.Value)^ := AValue;
end;

{ TSiToken }

constructor TSiToken.Create;
begin
  { Intentionally left empty }
end;

{ TSiTimer }

constructor TSiTimer.Create(const ACallback: TSiTimerCallback;
  const AState: TObject; const APeriod: Integer);
begin
  FThread := TSiTimerThread.Create(ACallback, AState, APeriod);
{$IFDEF DELPHI2010_OR_HIGHER}
  FThread.Start;
{$ELSE}
  FThread.Resume;
{$ENDIF}
end;

destructor TSiTimer.Destroy;
begin
  FThread.Cancel;
  FThread.WaitFor;
  FreeAndNil(FThread);
  inherited;
end;

{ TSiTimerThread }

procedure TSiTimerThread.Cancel;
begin
  Terminate;
  FEvent.SetEvent;
end;

constructor TSiTimerThread.Create(const ACallback: TSiTimerCallback;
  const AState: TObject; const APeriod: Integer);
begin
  inherited Create(True);
  FCallback := ACallback;
  FState := AState;
  FPeriod := APeriod;
  FEvent := TEvent.Create(nil, False, False, '');
end;

destructor TSiTimerThread.Destroy;
begin
  FreeAndNil(FEvent);
  inherited;
end;

procedure TSiTimerThread.Execute;
begin
  while not Terminated do
  begin
    if FEvent.WaitFor(FPeriod) = wrTimeout then
    begin
      FCallback(FState);
    end;
  end;
end;

{ TSiConfigurationTimer }

constructor TSiConfigurationTimer.Create(
  const ASmartInspect: TSmartInspect; const AFileName: UnicodeString;
  const APeriod: Integer);
begin
  if not Assigned(ASmartInspect) then
  begin
    raise ESmartInspectError.Create('ASmartInspect is not assigned');
  end;

  if APeriod < 0 then
  begin
    raise ESmartInspectError.Create('APeriod is negative');
  end;

  FSmartInspect := ASmartInspect;
  FFileName := AFileName;

  if SiFileAge(FFileName, FLastUpdate) then
    FSmartInspect.LoadConfiguration(FFileName)
  else
    FLastUpdate := 0;

  inherited Create(Run, nil, APeriod); { Start timer }
end;

procedure TSiConfigurationTimer.Run(AState: TObject);
var
  LLastUpdate: TDateTime;
begin
  if SiFileAge(FFileName, LLastUpdate) then
  begin
    if LLastUpdate > FLastUpdate then
    begin
      FLastUpdate := LLastUpdate;
      FSmartInspect.LoadConfiguration(FFileName);
    end;
  end;
end;

{ TSiClock }

var
  GSiClockOffset: TDateTime = 0;
  GSiClockSupported: Boolean = False;
  GSiClockFrequency: Double = 0;

class function TSiClock.Now(const AResolution: TSiClockResolution): TDateTime;
begin
  if (AResolution = crHigh) and GSiClockSupported then
    Result := GetTicks + GSiClockOffset
  else
    Result := SysUtils.Now; { Fallback }
end;

class procedure TSiClock.SortArray(var AArray: array of TDateTime);
var
  I, J: Integer;
  X: TDateTime;
begin
  for I := 1 to Length(AArray) - 1 do
  begin
    X := AArray[I];
    J := I - 1;
    while (J >= 0) and (AArray[J] > X) do
    begin
      AArray[J + 1] := AArray[J];
      Dec(J);
    end;
    AArray[J + 1] := X;
  end;
end;

class procedure TSiClock.Calibrate;
const
  CRounds = 5;
var
  I: Integer;
  LRounds: array[0..CRounds-1] of TDateTime;
begin
  if GSiClockSupported then
  begin
    for I := 0 to Length(LRounds) - 1 do
    begin
      LRounds[I] := DoCalibrate;
    end;
    GSiClockOffset := GetMedian(LRounds);
  end;
end;

class function TSiClock.DoCalibrate: TDateTime;
var
  LNow: TDateTime;
begin
  LNow := SysUtils.Now;
  while LNow = SysUtils.Now do ;
  Result := GetOffset;
end;

class function TSiClock.GetMedian(var AArray: array of TDateTime): TDateTime;
var
  N, M: TDateTime;
begin
  SortArray(AArray);
  if (Length(AArray) and 1) = 1 then
  begin
    Result := AArray[(Length(AArray) - 1) div 2];
  end else
  begin
    N := AArray[(Length(AArray) div 2) - 1];
    M := AArray[Length(AArray) div 2];
    Result := (N + M) / 2;
  end;
end;

class function TSiClock.GetOffset: TDateTime;
begin
  Result := SysUtils.Now - TSiClock.GetTicks;
end;

class function TSiClock.GetTicks: TDateTime;
const
  CUSecsPerDay: Int64 = 86400000000;
var
  LCounter: Int64;
begin
  if QueryPerformanceCounter(LCounter) then
    Result := (LCounter / GSiClockFrequency) / CUSecsPerDay
  else
    Result := SysUtils.Now - GSiClockOffset; { Fallback }
end;

{ TSiSessionManager }

procedure TSiSessionManager.Add(const ASession: TSiSession;
  const AStore: Boolean);
begin
  if not Assigned(ASession) then
  begin
    Exit;
  end;

  FCriticalSection.Enter;
  try
    FDefaults.Assign(ASession);
    FSessionList.Add(ASession);

    if AStore then
    begin
      FSessionTable[ASession.Name] := ASession;
      ASession.IsStored := True;
    end;

    Configure(ASession, ASession.Name);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSessionManager.Assign(const ASession: TSiSession;
  const AInfo: TSiSessionInfo);
begin
  if AInfo.Active then
  begin
    if AInfo.HasColor then
      ASession.Color := AInfo.Color;
    if AInfo.HasLevel then
      ASession.Level := AInfo.Level;
    if AInfo.HasActive then
      ASession.Active := AInfo.Active;
  end else
  begin
    if AInfo.HasActive then
      ASession.Active := AInfo.Active;
    if AInfo.HasLevel then
      ASession.Level := AInfo.Level;
    if AInfo.HasColor then
      ASession.Color := AInfo.Color;
  end;
end;

procedure TSiSessionManager.Configure(const ASession: TSiSession;
  const AName: UnicodeString);
var
  LInfo: TSiSessionInfo;
begin
  LInfo := TSiSessionInfo(FSessionInfos[AName]);

  if Assigned(LInfo) then
  begin
    Assign(ASession, LInfo);
  end;
end;

constructor TSiSessionManager.Create;
begin
  FCriticalSection := TCriticalSection.Create;
  FSessionTable := TSiObjectHash.Create(False);
  FSessionInfos := TSiObjectHash.Create(True);
  FSessionList := TObjectList.Create(True);
  FDefaults := TSiSessionDefaults.Create;
end;

procedure TSiSessionManager.Delete(const ASession: TSiSession);
begin
  if not Assigned(ASession) then
  begin
    Exit;
  end;

  FCriticalSection.Enter;
  try
    if FSessionTable[ASession.Name] = ASession then
    begin
      FSessionTable.Remove(ASession.Name);
    end;

    FSessionList.Remove(ASession); { Free }
  finally
    FCriticalSection.Leave;
  end;
end;

destructor TSiSessionManager.Destroy;
begin
  FreeAndNil(FDefaults);
  FreeAndNil(FSessionList);
  FreeAndNil(FSessionInfos);
  FreeAndNil(FSessionTable);
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TSiSessionManager.Get(const AName: UnicodeString): TSiSession;
begin
  FCriticalSection.Enter;
  try
    Result := TSiSession(FSessionTable[AName]);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSessionManager.LoadConfiguration(
  const AConfig: TSiConfiguration);
begin
  FCriticalSection.Enter;
  try
    FSessionInfos.Clear;
    LoadInfos(AConfig);
    LoadDefaults(AConfig);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TSiSessionManager.LoadDefaults(const AConfig: TSiConfiguration);
begin
  FDefaults.Active := AConfig.ReadBoolean('sessiondefaults.active',
    FDefaults.Active);
  FDefaults.Level := AConfig.ReadLevel('sessiondefaults.level',
    FDefaults.Level);
  FDefaults.Color := AConfig.ReadColor('sessiondefaults.color',
    FDefaults.Color);
end;

function TSiSessionManager.LoadInfo(const AName: UnicodeString;
  const AConfig: TSiConfiguration): TSiSessionInfo;
begin
  Result := TSiSessionInfo.Create;
  Result.Name := AName;
  Result.HasActive := AConfig.Contains(SiSessionPrefix + AName + '.active');

  if Result.HasActive then
  begin
    Result.Active := AConfig.ReadBoolean(SiSessionPrefix + AName + '.active',
      SiSessionDefaultActive);
  end;

  Result.HasLevel := AConfig.Contains(SiSessionPrefix + AName + '.level');

  if Result.HasLevel then
  begin
    Result.Level := AConfig.ReadLevel(SiSessionPrefix + AName + '.level',
      SiSessionDefaultLevel);
  end;

  Result.HasColor := AConfig.Contains(SiSessionPrefix + AName + '.color');

  if Result.HasColor then
  begin
    Result.Color := AConfig.ReadColor(SiSessionPrefix + AName + '.color',
      SiSessionDefaultColor);
  end;
end;

procedure TSiSessionManager.LoadInfos(const AConfig: TSiConfiguration);
var
  LIndex: Integer;
  LSuffix: UnicodeString;
  LPrefix: UnicodeString;
  LName: UnicodeString;
  LInfo: TSiSessionInfo;
  LSession: TSiSession;
  LKey: UnicodeString;
  I: Integer;
begin
  for I := 0 to Pred(AConfig.Count) do
  begin
    LKey := AConfig.ReadKey(I);

    { Do we have a session here? }
    
    if Length(LKey) < Length(SiSessionPrefix) then
    begin
      Continue; { No, too short }
    end;
    LPrefix := Copy(LKey, 1, Length(SiSessionPrefix));
    if not SiSameText(LPrefix, SiSessionPrefix) then
    begin
      Continue; { No prefix match }
    end;

    LSuffix := Copy(LKey, Length(SiSessionPrefix) + 1, MaxInt);
    LIndex := SiLastPos('.', LSuffix);

    if LIndex = 0 then
    begin
      Continue;
    end;

    LName := Copy(LSuffix, 1, LIndex - 1);

    { Duplicate session entry? }

    if FSessionInfos.Contains(LName) then
    begin
      Continue;
    end;

    LInfo := LoadInfo(LName, AConfig);
    FSessionInfos[LName] := LInfo;

    { Do we need to update a related session? }

    LSession := TSiSession(FSessionTable[LName]);
    if Assigned(LSession) then
    begin
      Assign(LSession, LInfo);
    end;
  end;
end;

procedure TSiSessionManager.Update(const ASession: TSiSession;
  const ATo, AFrom: UnicodeString);
begin
  if not Assigned(ASession) then
  begin
    Exit;
  end;

  FCriticalSection.Enter;
  try
    if FSessionTable[AFrom] = ASession then
    begin
      FSessionTable.Remove(AFrom);
    end;

    Configure(ASession, ATo);
    FSessionTable[ATo] := ASession;
  finally
    FCriticalSection.Leave;
  end;
end;

{ TSiSessionDefaults }

procedure TSiSessionDefaults.Assign(const ASession: TSiSession);
begin
  ASession.Active := FActive;
  ASession.Color := FColor;
  ASession.Level := FLevel;
end;

constructor TSiSessionDefaults.Create;
begin
  FActive := SiSessionDefaultActive;
  FLevel := SiSessionDefaultLevel;
  FColor := SiSessionDefaultColor;
end;

{ TSiSchedulerCommand }

procedure TSiSchedulerCommand.AddRef;
begin
  InterlockedIncrement(FRefCount);
end;

constructor TSiSchedulerCommand.Create;
begin
  FRefCount := 1;
end;

function TSiSchedulerCommand.GetSize: Integer;
begin
  Result := 0;
  if FAction = saWritePacket then
  begin
    if Assigned(FState) then
    begin
      Result := TSiPacket(FState).Size
    end;
  end;
end;

procedure TSiSchedulerCommand.Release;
begin
  if InterlockedDecrement(FRefCount) = 0 then
  begin
    Free;
  end;
end;

{ TSiScheduler }

procedure TSiScheduler.Clear;
begin
  FLock.Enter;
  try
    FQueue.Clear;
    SetEvent(FNotFull);
  finally
    FLock.Leave;
  end;
end;

constructor TSiScheduler.Create(const AProtocol: TSiProtocol);
begin
  inherited Create(True);
  FLock := TCriticalSection.Create;
  FNotFull := CreateEvent(nil, False, True, nil);
  FNotEmpty := CreateEvent(nil, False, False, nil);
  FProtocol := AProtocol;
  FQueue := TSiSchedulerQueue.Create;
  FQueue.OnDelete := DeleteCommand;
  SetLength(FBuffer, SiSchedulerBufferSize);
end;

procedure TSiScheduler.DeleteCommand(ASender: TSiSchedulerQueue;
  ACommand: TSiSchedulerCommand);
begin
  ReleaseCommand(ACommand);
end;

function TSiScheduler.Dequeue: Integer;
var
  LCount: Integer;
  LLength: Integer;
begin
  LCount := 0;
  LLength := Length(FBuffer);

  FLock.Enter;
  try
    while FQueue.Count = 0 do
    begin
      if FStopped then
      begin
        Break;
      end;
      FLock.Leave;
      try
        WaitForSingleObject(FNotEmpty, INFINITE);
      finally
        FLock.Enter;
      end;
    end;

    while FQueue.Count > 0 do
    begin
      FBuffer[LCount] := FQueue.Dequeue;

      Inc(LCount);
      if LCount >= LLength then
      begin
        Break;
      end;
    end;

    SetEvent(FNotFull);
  finally
    FLock.Leave;
  end;

  Result := LCount;
end;

destructor TSiScheduler.Destroy;
begin
  Stop;
  FreeAndNil(FLock);
  CloseHandle(FNotFull);
  CloseHandle(FNotEmpty);
  FreeAndNil(FQueue);
  inherited;
end;

function TSiScheduler.Enqueue(const ACommand: TSiSchedulerCommand): Boolean;
var
  LCommandSize: Integer;
begin
  Result := False;

  if not FStarted then
  begin
    Exit; { Not yet started }
  end;

  if FStopped then
  begin
    Exit; { No new commands anymore }
  end;

  LCommandSize := ACommand.Size;

  if LCommandSize > FThreshold then
  begin
    Exit;
  end;

  ACommand.AddRef;

  FLock.Enter;
  try
    if not FThrottle or FProtocol.Failed then
    begin
      if FQueue.Size + LCommandSize > FThreshold then
      begin
        FQueue.Trim(LCommandSize);
      end;
    end else
    begin
      while FQueue.Size + LCommandSize > FThreshold do
      begin
        FLock.Leave;
        try
          WaitForSingleObject(FNotFull, INFINITE);
        finally
          FLock.Enter;
        end;
      end;
    end;
    FQueue.Enqueue(ACommand);
    SetEvent(FNotEmpty);
  finally
    FLock.Leave;
  end;

  Result := True;
end;

procedure TSiScheduler.Execute;
begin
  Run;
end;

procedure TSiScheduler.ReleaseCommand(const ACommand: TSiSchedulerCommand);
begin
  { This method is called whenever a scheduler command is no
    longer needed by the scheduler. This can happen automatically
    when the DeletePacket event handler is fired or manually
    after processing commands in the RunCommands method. In
    addition to decrement the reference counter of the command,
    we also need to release the state objects, if available. }

  if Assigned(ACommand.State) then
  begin
    case ACommand.Action of
      saWritePacket: TSiPacket(ACommand.State).Release;
      saDispatch: TSiProtocolCommand(ACommand.State).Release;
    end;
  end;

  ACommand.Release; { Decrement reference counter of command }
end;

procedure TSiScheduler.Run;
var
  LCount: Integer;
begin
  while True do
  begin
    LCount := Dequeue;

    if LCount = 0 then
    begin
      Break; { Stopped and no more commands }
    end;

    if not RunCommands(LCount) then
    begin
      Break; { Stopped }
    end;
  end;
end;

procedure TSiScheduler.RunCommand(const ACommand: TSiSchedulerCommand);
var
  LPacket: TSiPacket;
  LCommand: TSiProtocolCommand;
begin
  { Process the dequeued command. The Impl methods cannot
    throw an exception. Exceptions are reported with the
    error event of the protocol in asynchronous mode. }

  case ACommand.Action of
    saConnect: FProtocol.ImplConnect;

    saWritePacket: begin
      LPacket := TSiPacket(ACommand.State);
      FProtocol.ImplWritePacket(LPacket);
    end;

    saDisconnect: FProtocol.ImplDisconnect;

    saDispatch: begin
      LCommand := TSiProtocolCommand(ACommand.State);
      FProtocol.ImplDispatch(LCommand);
    end;
  end;
end;

function TSiScheduler.RunCommands(const ACount: Integer): Boolean;
var
  I, J: Integer;
  LStopped: Boolean;
begin
  Result := True;

  for I := 0 to Pred(ACount) do
  begin
    LStopped := FStopped; { See below }

    RunCommand(FBuffer[I]);
    ReleaseCommand(FBuffer[I]); { Decrement reference counters }
    FBuffer[I] := nil;

    if LStopped then
    begin
      { The scheduler has been stopped before the last
        command has been processed. To shutdown this
        thread as fast as possible we check if the last
        command of the protocol has failed (or if the
        last command has failed to change the previous
        failure status, respectively). If this is the
        case, we clear the queue and exit this thread
        immediately. }
      if FProtocol.Failed then
      begin
        Clear;
        Result := False;
        Break;
      end;
    end;
  end;

  if not Result then
  begin
    { Cleanup the remaining commands which have not been
      processed because this thread is exiting early. }
    for J := Succ(I) to Pred(ACount) do
    begin
      ReleaseCommand(FBuffer[J]);
      FBuffer[J] := nil;
    end;
  end;
end;

function TSiScheduler.Schedule(const ACommand: TSiSchedulerCommand): Boolean;
begin
  Result := Enqueue(ACommand);
end;

procedure TSiScheduler.Start;
begin
  FLock.Enter;
  try
    if FStarted then
    begin
      Exit; { Start only once }
    end;
{$IFDEF DELPHI2010_OR_HIGHER}
    inherited Start;
{$ELSE}
    Resume;
{$ENDIF}
    FStarted := True;
  finally
    FLock.Leave;
  end;
end;

procedure TSiScheduler.Stop;
begin
  FLock.Enter;
  try
    if not FStarted then
    begin
      Exit;
    end;
    FStopped := True;
    SetEvent(FNotEmpty);
  finally
    FLock.Leave;
  end;
  WaitFor;
end;

{ TSiSchedulerQueue }

procedure TSiSchedulerQueue.Add(const AItem: PSiSchedulerQueueItem);
begin
  if not Assigned(FTail) then
  begin
    FTail := AItem;
    FHead := AItem;
  end else
  begin
    FTail.Next := AItem;
    AItem.Previous := FTail;
    FTail := AItem;
  end;
  Inc(FCount);
  Inc(FSize, AItem.Command.Size + SizeOf(AItem^));
end;

procedure TSiSchedulerQueue.Clear;
var
  LCommand: TSiSchedulerCommand;
begin
  LCommand := Dequeue;
  while Assigned(LCommand) do
  begin
    DoDelete(LCommand);
    LCommand := Dequeue;
  end;
end;

function TSiSchedulerQueue.Dequeue: TSiSchedulerCommand;
var
  LItem: PSiSchedulerQueueItem;
begin
  LItem := FHead;
  if Assigned(LItem) then
  begin
    Remove(LItem);
    Result := LItem.Command;
    Dispose(LItem);
  end else
    Result := nil;
end;

procedure TSiSchedulerQueue.DoDelete(const ACommand: TSiSchedulerCommand);
begin
  if Assigned(FOnDelete) then
  begin
    FOnDelete(Self, ACommand);
  end;
end;

procedure TSiSchedulerQueue.Enqueue(const ACommand: TSiSchedulerCommand);
var
  LItem: PSiSchedulerQueueItem;
begin
  New(LItem);
  ZeroMemory(LItem, SizeOf(LItem^));
  LItem.Command := ACommand;
  Add(LItem);
end;

procedure TSiSchedulerQueue.Remove(const AItem: PSiSchedulerQueueItem);
begin
  if AItem = FHead then { Head }
  begin
    FHead := AItem.Next;
    if Assigned(FHead) then
      FHead.Previous := nil
    else { Was also tail }
      FTail := nil;
  end else
  begin
    AItem.Previous.Next := AItem.Next;
    if not Assigned(AItem.Next) then { Tail }
      FTail := AItem.Previous
    else
      AItem.Next.Previous := AItem.Previous;
  end;
  Dec(FCount);
  Dec(FSize, AItem.Command.Size + SizeOf(AItem^));
end;

function TSiSchedulerQueue.Trim(const ASize: Integer): Boolean;
var
  LRemovedBytes: Integer;
  LItem: PSiSchedulerQueueItem;
begin
  if ASize <= 0 then
  begin
    Result := True;
    Exit;
  end;

  LRemovedBytes := 0;
  LItem := FHead;

  while Assigned(LItem) do
  begin
    if LItem.Command.Action = saWritePacket then
    begin
      Inc(LRemovedBytes, LItem.Command.Size + SizeOf(LItem^));

      Remove(LItem);
      DoDelete(LItem.Command);
      Dispose(LItem);

      if LRemovedBytes >= ASize then
      begin
        Result := True;
        Exit;
      end;
    end;
    LItem := LItem.Next;
  end;

  Result := False;
end;

var
  LVersionInfo: TOSVersionInfo;
  LFrequency: Int64;

{ TSiPipeProtocol }

procedure TSiPipeProtocol.BuildOptions(const ABuilder: TSiConnectionsBuilder);
begin
  inherited;
  ABuilder.AddOption('pipename', FPipeName);
end;

constructor TSiPipeProtocol.Create;
begin
  inherited;
  FFormatter := TSiBinaryFormatter.Create;
  LoadOptions; { Set default options }
end;

function TSiPipeProtocol.CreateHandle(const APipeName: UnicodeString;
  var AErrorCode: Cardinal): Boolean;
var
  LFileName: UnicodeString;
begin
  LFileName := '\\.\pipe\' + APipeName;

  FHandle := SiCreateFile(
    LFileName,
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0,
    AErrorCode);

  Result := FHandle <> INVALID_HANDLE_VALUE;
end;

destructor TSiPipeProtocol.Destroy;
begin
  FreeAndNil(FFormatter);
  inherited;
end;

class procedure TSiPipeProtocol.DoHandShake(const AStream: TStream);
const
  SiLineBreak = #10;
var
  C: Char;
  LBanner: AnsiString;
begin
  C := #0;
  while C <> #10 do
  begin
    if AStream.Read(C, 1) <> 1 then
    begin
      raise ESmartInspectError.Create(SiPipeClosedError);
    end;
  end;
  LBanner := SiClientBanner + #10;
  AStream.Write(Pointer(LBanner)^, Length(LBanner));
end;

function TSiPipeProtocol.GetName: UnicodeString;
begin
  Result := 'pipe';
end;

procedure TSiPipeProtocol.InternalConnect;
var
  LErrorCode: Cardinal;
begin
  if CreateHandle(FPipeName, LErrorCode) then
  begin
    FStream := THandleStream.Create(Integer(FHandle));
    DoHandShake(FStream);
    FBuffer := TSiBufferedStream.Create(FStream, SiBufferSize);
    InternalWriteLogHeader; { Write a log header }
  end else
    raise ESmartInspectError.Create(SysErrorMessage(LErrorCode));
end;

procedure TSiPipeProtocol.InternalDisconnect;
begin
  FreeAndNil(FBuffer);
  FreeAndNil(FStream);

  if FHandle <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(FHandle);
    FHandle := INVALID_HANDLE_VALUE;
  end;
end;

function TSiPipeProtocol.InternalReconnect: Boolean;
var
  LErrorCode: Cardinal;
begin
  Result := CreateHandle(FPipeName, LErrorCode);

  if Result then
  begin
    FStream := THandleStream.Create(Integer(FHandle));
    DoHandShake(FStream);
    FBuffer := TSiBufferedStream.Create(FStream, SiBufferSize);
    InternalWriteLogHeader; { Write a log header }
  end;
end;

procedure TSiPipeProtocol.InternalWritePacket(const APacket: TSiPacket);
begin
  FFormatter.Format(APacket, FBuffer);
  FBuffer.Flush;
end;

function TSiPipeProtocol.IsValidOption(const AOption: UnicodeString): Boolean;
begin
  Result := (AOption = 'pipename') or
            inherited IsValidOption(AOption);
end;

procedure TSiPipeProtocol.LoadOptions;
begin
  inherited;
  FPipeName := GetStringOption('pipename', SiPipeName);
end;

{ TSiLogHeader }

function TSiLogHeader.GetContent: UnicodeString;
begin
  Result := 'hostname=' +
    FHostName +
    #13#10 +
    'appname=' +
    FAppName +
    #13#10;
end;

function TSiLogHeader.GetPacketType: TSiPacketType;
begin
  Result := ptLogHeader;
end;

function TSiLogHeader.GetSize: Integer;
begin
  Result := SizeOf(Integer) + Length(Content);
end;

{ TSiFileHelper }

class procedure TSiFileHelper.DeleteFiles(const ABaseName: UnicodeString;
  const AMaxParts: Integer);
var
  LFiles: TSiStringList;
  I: Integer;
begin
  LFiles := TSiStringList.Create;
  try
    GetFiles(ABaseName, LFiles);

    for I := 0 to LFiles.Count - 1 do
    begin
      if I + AMaxParts >= LFiles.Count then
      begin
        Break;
      end;
      SiDeleteFile(LFiles[I]);
    end;
  finally
    LFiles.Free;
  end;
end;

class function TSiFileHelper.ExpandFileName(
  const ABaseName: UnicodeString): UnicodeString;
var
  LDateTime: UnicodeString;
begin
  LDateTime := FormatDateTime(SiRotateDateTimeFormat, SiUtcNow);
  Result := SiChangeFileExt(ABaseName, '') + '-' + LDateTime +
      SiExtractFileExt(ABaseName);

  { Append a special character/suffix to the expanded file
    name if the file already exists in order to not override
    an existing log file. }

  while SiFileOrDirExists(Result) do
  begin
    Result := SiChangeFileExt(Result, '') + SiRotateAlreadyExistsSuffix +
      SiExtractFileExt(Result);
  end;
end;

class function TSiFileHelper.FindFileName(
  const ABaseName: UnicodeString): UnicodeString;
var
  LFiles: TSiStringList;
begin
  LFiles := TSiStringList.Create;
  try
    GetFiles(ABaseName, LFiles);
    if LFiles.Count > 0 then
      Result := LFiles[LFiles.Count - 1]
    else
      Result := '';
  finally
    LFiles.Free;
  end;
end;

class function TSiFileHelper.GetFileDate(const ABaseName,
  APath: UnicodeString): TDateTime;
begin
  if not TryGetFileDate(ABaseName, APath, Result) then
  begin
    raise ESiProtocolError.Create(SiInvalidFileNameError);
  end;
end;

class function TSiFileHelper.GetFileName(const ABaseName: UnicodeString;
  const AAppend: Boolean): UnicodeString;
begin
  if AAppend then
  begin
    Result := FindFileName(ABaseName);
    if Result <> '' then
    begin
      Exit;
    end;
  end;
  Result := ExpandFileName(ABaseName); { Fallback }
end;

class procedure TSiFileHelper.GetFiles(const ABaseName: UnicodeString;
  const AFiles: TSiStringList);
var
  LPath: UnicodeString;
  I: Integer;
begin
  LPath := SiChangeFileExt(ABaseName, '') + '-*' +
    SiExtractFileExt(ABaseName);
    
  SiGetFiles(LPath, AFiles);
  AFiles.Sort;

  { Only return files with a valid file name (see
    IsValidFile) and ignore all others. SiGetFiles
    all files which match the given path, so it might
    return files which are not really related to our
    current log file. }

  for I := AFiles.Count - 1 downto 0 do
  begin
    if not IsValidFile(ABaseName, AFiles[I]) then
    begin
      AFiles.Delete(I);
    end;
  end;
end;

class function TSiFileHelper.IsValidFile(const ABaseName,
  APath: UnicodeString): Boolean;
var
  LFileDate: TDateTime;
begin
  Result := TryGetFileDate(ABaseName, APath, LFileDate);
end;

class function TSiFileHelper.TryGetFileDate(const ABaseName,
  APath: UnicodeString; var AFileDate: TDateTime): Boolean;
var
  LFileName: UnicodeString;
  LBaseName: UnicodeString;
  LValue: UnicodeString;
  LIndex: Integer;
begin
  LFileName := SiExtractFileName(APath);
  LBaseName := SiChangeFileExt(SiExtractFileName(ABaseName), '');

  { In order to avoid possible bugs with the creation
   time of file names (log files on Windows or Samba
   shares, for instance), we parse the name of the log
   file and do not use its creation time. }

  LIndex := SiPos(LBaseName, LFileName);

  if LIndex <> 1 then
  begin
    Result := False;
    Exit;
  end;

  LValue := Copy(LFileName, Succ(Length(LBaseName)) + 1, MaxInt);
  LValue := SiChangeFileExt(LValue, '');

  { Strip any added SiRotateAlreadyExistsSuffix characters.
    This can happen if we are non-append mode and need to
    add this special character/suffix in order to not
    override an existing file. }

  if Length(LValue) > Length(SiRotateDateTimeFormat) then
  begin
    LValue := Copy(LValue, 1, Length(SiRotateDateTimeFormat));
  end;

  Result := TryParseFileDate(LValue, AFileDate);
end;

class function TSiFileHelper.TryParseFileDate(const AFileDate: UnicodeString;
  var ADateTime: TDateTime): Boolean;
var
  I: Integer;
  LValues: TSiStringList;
begin
  Result := False;

  if Length(AFileDate) <> Length(SiRotateDateTimeFormat) then
  begin
    Exit;
  end;

  for I := 1 to Length(AFileDate) do
  begin
    case AFileDate[I] of
      '0'..'9', SiRotateDateTimeSeparator: { Valid} ;
    else
      Exit;
    end;
  end;

  LValues := TSiStringList.Create;
  try
    SiSplit(AFileDate, SiRotateDateTimeSeparator, LValues);

    if LValues.Count <> SiRotateDateTimeTokens then
    begin
      Exit;
    end;

    ADateTime := EncodeDateTime(
      StrToInt(LValues[0]), { Year }
      StrToInt(LValues[1]), { Month }
      StrToInt(LValues[2]), { Day }
      StrToInt(LValues[3]), { Hour }
      StrToInt(LValues[4]), { Minute }
      StrToInt(LValues[5]), { Second }
      0);
  finally
    LValues.Free;
  end;

  Result := True;
end;

{ TSiProtocolVariables }

procedure TSiProtocolVariables.Add(const AKey, AValue: UnicodeString);
begin
  FLock.Enter;
  try
    FItems.Add(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TSiProtocolVariables.Clear;
begin
  FLock.Enter;
  try
    FItems.Clear;
  finally
    FLock.Leave;
  end;
end;

function TSiProtocolVariables.Contains(const AKey: UnicodeString): Boolean;
begin
  FLock.Enter;
  try
    Result := FItems.Contains(AKey);
  finally
    FLock.Leave;
  end;
end;

constructor TSiProtocolVariables.Create;
begin
  FItems := TSiStringHash.Create;
  FLock := TCriticalSection.Create;
end;

destructor TSiProtocolVariables.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FItems);
  inherited;
end;

function TSiProtocolVariables.Expand(
  const AConnections: UnicodeString): UnicodeString;
var
  LKey: UnicodeString;
begin
  FLock.Enter;
  try
    Result := AConnections;
    if FItems.Count = 0 then
    begin
      Exit;
    end;
    FItems.Restart;
    while FItems.Next do
    begin
      LKey := '$' + FItems.CurrentKey + '$';
      Result := SiStringReplace(Result, LKey, FItems.CurrentValue);
    end;
  finally
    FLock.Leave;
  end;
end;

function TSiProtocolVariables.Get(const AKey: UnicodeString): UnicodeString;
begin
  FLock.Enter;
  try
    Result := FItems.Get(AKey);
  finally
    FLock.Leave;
  end;
end;

function TSiProtocolVariables.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TSiProtocolVariables.Put(const AKey, AValue: UnicodeString);
begin
  FLock.Enter;
  try
    FItems.Put(AKey, AValue);
  finally
    FLock.Leave;
  end;
end;

procedure TSiProtocolVariables.Remove(const AKey: UnicodeString);
begin
  FLock.Enter;
  try
    FItems.Remove(AKey);
  finally
    FLock.Leave;
  end;
end;

initialization
  { Protocol initialization }
  GSiProtocolsSync := TCriticalSection.Create;
  TSiProtocolFactory.RegisterProtocol('file', TSiFileProtocol);
  TSiProtocolFactory.RegisterProtocol('mem', TSiMemoryProtocol);
  TSiProtocolFactory.RegisterProtocol('pipe', TSiPipeProtocol);
  TSiProtocolFactory.RegisterProtocol('tcp', TSiTcpProtocol);
  TSiProtocolFactory.RegisterProtocol('text', TSiTextProtocol);

  { Is Unicode fully supported? }
  LVersionInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  if GetVersionEx(LVersionInfo) then
  begin
    { 2K and up }
    GIsUnicodePlatform := (LVersionInfo.dwMajorVersion >= 5)
      and (LVersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT);
  end;

  GSiClockSupported := QueryPerformanceFrequency(LFrequency);

  { Is a high-resolution performance counter available? }
  if GSiClockSupported then
  begin
    { Initialize and synchronize the performance counter with the
      system time. The following code is not very accurate (it
      can be off to the system clock by a few milliseconds) but
      since high-resolution timestamps are a developer-only feature
      and not intended for production usage, this code is sufficient.
      To improve the synchronization of the counter with the system
      clock, call TSiClock.Calibrate. }
    GSiClockFrequency := LFrequency / 1000000.0;
    GSiClockOffset := TSiClock.GetOffset;
  end;
finalization
  FreeAndNil(GSiProtocolsSync);
end.

