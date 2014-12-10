//
// <!-- Copyright (C) 2003-2011 Gurock Software GmbH. All rights reserved. -->
//
// <summary>
//   Contains the Si and SiMain variables.
// </summary>
// <remarks>
//   This unit provides a variable called Si of type TSmartInspect.
//   Furthermore a TSiSession instance named SiMain with Si as parent is
//   ready to use. The Si and SiMain variables are especially useful if
//   you do not want to create TSmartInspect and TSiSession instances by
//   yourself.
//
//   The <link TSmartInspect.Connections, connections string> of Si is
//   set to 'pipe(reconnect=true, reconnect.interval=1s)', the
//   <link TSmartInspect.AppName, application name> to the filename of
//   the current executable and the <link TSiSession.Name, session name>
//   of SiMain to 'Main'.
//
//   <b>Please note that the default connections string has been
//   changed in SmartInspect 3.0</b>. In previous versions, the default
//   connections string was set to 'tcp()'.
// </remarks>
// <example>
// <code>
// program SiAutoExample;
// {$APPTYPE CONSOLE}
//
// uses
//  SysUtils, SiAuto;
//
// begin
//   Si.Enabled := True;
//   SiMain.EnterProcess('SiAutoExample');
//   try
//     .
//     .
//     .
//   finally
//     SiMain.LeaveProcess('SiAutoExample');
//   end;
// end.
// </code>
// </example>

unit SiAuto;

interface

uses
  SysUtils,
  SmartInspect;

var

  // <summary>
  //   Automatically created TSmartInspect instance.
  // </summary>
  // <remarks>
  //   The <link TSmartInspect.Connections, connections string> is set
  //   to 'pipe(reconnect=true, reconnect.interval=1s)'. Please see
  //   TSiProtocol.IsValidOption for information on the used options. The
  //   <link TSmartInspect.AppName, application name> is set to 'Auto'.
  //
  //   <b>Please note that the default connections string has been
  //   changed in SmartInspect 3.0</b>. In previous versions, the default
  //   connections string was set to 'tcp()'.
  // </remarks>

  Si: TSmartInspect;

  // <summary>
  //   Automatically created TSiSession instance.
  // </summary>
  // <remarks>
  //   The <link TSiSession.Name, session name> is set to 'Main' and the
  //   <link TSiSession.Parent, parent> to Si.
  // </remarks>

  SiMain: TSiSession;

implementation

const
  SiConnections = 'pipe(reconnect=true, reconnect.interval=1s)';
  SiSession = 'Main';

initialization
  Si := TSmartInspect.Create(ExtractFileName(ParamStr(0)));
  Si.Connections := SiConnections;
  SiMain := Si.AddSession(SiSession, True);
finalization
  SiMain := nil;
  FreeAndNil(Si);
end.

