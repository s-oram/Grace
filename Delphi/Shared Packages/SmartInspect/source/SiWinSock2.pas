//-------------------------------------------------------------
//
//       Borland Delphi Runtime Library
//       <API> interface unit
//
// Portions created by Microsoft are
// Copyright (C) 1995-1999 Microsoft Corporation.
// All Rights Reserved.
//
// The original file is: Winsock2.h from CBuilder5 distribution.
// The original Pascal code is: winsock2.pas, released 04 Mar 2000.
// The initial developer of the Pascal code is Alex Konshin
// (alexk@mtgroup.ru).
//
// Portions created by Alex Konshin are
// Copyright (C) 1998-2000 Alex Konshin
//
// Contributor(s): Alex Konshin
//
//       Obtained through:
//
//       Joint Endeavour of Delphi Innovators (Project JEDI)
//
// You may retrieve the latest version of this file at the Project
// JEDI home page, located at http://delphi-jedi.org
//
// The contents of this file are used with permission, subject to
// the Mozilla Public License Version 1.1 (the "License"); you may
// not use this file except in compliance with the License. You may
// obtain a copy of the License at
// http://www.mozilla.org/MPL/MPL-1.1.html
//
// Software distributed under the License is distributed on an
// "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
// implied. See the License for the specific language governing
// rights and limitations under the License.
//
//-------------------------------------------------------------


{ Winsock2.h -- definitions to be used with the WinSock 2 DLL and
  WinSock 2 applications. This header file corresponds to version 2.2.x
  of the WinSock API specification. This file includes parts which are
  Copyright (c) 1982-1986 Regents of the University of California. All
  rights reserved. The Berkeley Software License Agreement specifies the
  terms and conditions for redistribution. }

// converted by Alex Konshin, mailto:alexk@mtgroup.ru
// modified March,4 2000

unit SiWinSock2;

interface

uses SysUtils, Windows;

{$IFDEF WIN32}
{$ALIGN OFF}
{$ELSE}
{$ALIGN ON}
{$ENDIF}

{$RANGECHECKS OFF}
{$WRITEABLECONST OFF}

(*$HPPEMIT '#include <Winsock2.h>'*)
(*$HPPEMIT 'namespace Siwinsock2 {'*)
(*$HPPEMIT 'typedef sockaddr_in TSockAddrIn;'*)
(*$HPPEMIT 'typedef sockaddr_in* PSockAddrIn;'*)
(*$HPPEMIT '};'*)

//  Define the current Winsock version. To build an earlier Winsock version
//  application redefine this value prior to including Winsock2.h
const
  {$EXTERNALSYM WINSOCK_VERSION}
  WINSOCK_VERSION = $0202;
  {$EXTERNALSYM WINSOCK2_DLL}
  WINSOCK2_DLL = 'ws2_32.dll';

type
  {$EXTERNALSYM u_char}
  u_char  = Byte;
  {$EXTERNALSYM u_short}
  u_short = Word;
  {$EXTERNALSYM u_int}
  u_int   = DWORD;
  {$EXTERNALSYM u_long}
  u_long  = DWORD;

// The new type to be used in all instances which refer to sockets.
{$IFDEF WIN32}
  {$EXTERNALSYM TSocket}
  TSocket = u_int;
{$ELSE}
  {$EXTERNALSYM TSocket}
  TSocket = UINT_PTR;
{$ENDIF}

  {$EXTERNALSYM WSAEVENT}
  WSAEVENT = THandle;
  {$EXTERNALSYM PWSAEVENT}
  PWSAEVENT = ^WSAEVENT;
  {$EXTERNALSYM LPWSAEVENT}
  LPWSAEVENT = PWSAEVENT;
{$IFDEF UNICODE}
  {$EXTERNALSYM PMBChar}
  PMBChar = PWideChar;
{$ELSE}
  {$EXTERNALSYM PMBChar}
  PMBChar = PAnsiChar;
{$ENDIF}

const
  {$EXTERNALSYM FD_SETSIZE}
  FD_SETSIZE     =   64;

type
  {$EXTERNALSYM PFDSet}
  PFDSet = ^TFDSet;
  {$EXTERNALSYM TFDSet}
  TFDSet = packed record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

  {$EXTERNALSYM PTimeVal}
  PTimeVal = ^TTimeVal;
  {$EXTERNALSYM TTimeVal}
  TTimeVal = packed record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  {$EXTERNALSYM IOCPARM_MASK}
  IOCPARM_MASK = $7f;
  {$EXTERNALSYM IOC_VOID}
  IOC_VOID     = $20000000;
  {$EXTERNALSYM IOC_OUT}
  IOC_OUT      = $40000000;
  {$EXTERNALSYM IOC_IN}
  IOC_IN       = $80000000;
  {$EXTERNALSYM IOC_INOUT}
  IOC_INOUT    = (IOC_IN or IOC_OUT);

// get # bytes to read
  {$EXTERNALSYM FIONREAD}
  FIONREAD     = IOC_OUT or (SizeOf(Longint) shl 16) or (Ord('f') shl 8) or 127;
// set/clear non-blocking i/o
  {$EXTERNALSYM FIONBIO}
  FIONBIO      = IOC_IN  or (SizeOf(Longint) shl 16) or (Ord('f') shl 8) or 126;
// set/clear async i/o
  {$EXTERNALSYM FIOASYNC}
  FIOASYNC     = IOC_IN  or (SizeOf(Longint) shl 16) or (Ord('f') shl 8) or 125;

//  Socket I/O Controls

// set high watermark
  {$EXTERNALSYM SIOCSHIWAT}
  SIOCSHIWAT   = IOC_IN  or (SizeOf(Longint) shl 16) or (Ord('s') shl 8);
// get high watermark
  {$EXTERNALSYM SIOCGHIWAT}
  SIOCGHIWAT   = IOC_OUT or (SizeOf(Longint) shl 16) or (Ord('s') shl 8) or 1;
// set low watermark
  {$EXTERNALSYM SIOCSLOWAT}
  SIOCSLOWAT   = IOC_IN  or (SizeOf(Longint) shl 16) or (Ord('s') shl 8) or 2;
// get low watermark
  {$EXTERNALSYM SIOCGLOWAT}
  SIOCGLOWAT   = IOC_OUT or (SizeOf(Longint) shl 16) or (Ord('s') shl 8) or 3;
// at oob mark?
  {$EXTERNALSYM SIOCATMARK}
  SIOCATMARK   = IOC_OUT or (SizeOf(Longint) shl 16) or (Ord('s') shl 8) or 7;


//  Structures returned by network data base library, taken from the
//  BSD file netdb.h.  All addresses are supplied in host order, and
//  returned in network order (suitable for use in system calls).
type
  {$EXTERNALSYM PHostEnt}
  PHostEnt = ^THostEnt;
  {$EXTERNALSYM THostEnt}
  THostEnt = packed record
    h_name: PAnsiChar;                 // official name of host
    h_aliases: ^PAnsiChar;             // alias list
    h_addrtype: Smallint;          // host address type
    h_length: Smallint;            // length of address
    case Byte of
      0: (h_addr_list: ^PAnsiChar);    // list of addresses
      1: (h_addr: ^PAnsiChar);         // address, for backward compat
  end;

//  It is assumed here that a network number
//  fits in 32 bits.
  {$EXTERNALSYM PNetEnt}
  PNetEnt = ^TNetEnt;
  {$EXTERNALSYM TNetEnt}
  TNetEnt = packed record
    n_name: PAnsiChar;                 // official name of net
    n_aliases: ^PAnsiChar;             // alias list
    n_addrtype: Smallint;          // net address type
    n_net: u_long;                 // network #
  end;

  {$EXTERNALSYM PServEnt}
  PServEnt = ^TServEnt;
  {$EXTERNALSYM TServEnt}
  TServEnt = packed record
    s_name: PAnsiChar;                 // official service name
    s_aliases: ^PAnsiChar;             // alias list
    s_port: Smallint;              // protocol to use
    s_proto: PAnsiChar;                // port #
  end;

  {$EXTERNALSYM PProtoEnt}
  PProtoEnt = ^TProtoEnt;
  {$EXTERNALSYM TProtoEnt}
  TProtoEnt = packed record
    p_name: PAnsiChar;                 // official protocol name
    p_aliases: ^PAnsiChar;             // alias list
    p_proto: Smallint;             // protocol #
  end;

// Constants and structures defined by the internet system,
// Per RFC 790, September 1981, taken from the BSD file netinet/in.h.
const

// Protocols
  {$EXTERNALSYM IPPROTO_IP}
  IPPROTO_IP     =   0;             // dummy for IP
  {$EXTERNALSYM IPPROTO_ICMP}
  IPPROTO_ICMP   =   1;             // control message protocol
  {$EXTERNALSYM IPPROTO_IGMP}
  IPPROTO_IGMP   =   2;             // group management protocol
  {$EXTERNALSYM IPPROTO_GGP}
  IPPROTO_GGP    =   3;             // gateway^2 (deprecated)
  {$EXTERNALSYM IPPROTO_TCP}
  IPPROTO_TCP    =   6;             // TCP
  {$EXTERNALSYM IPPROTO_PUP}
  IPPROTO_PUP    =  12;             // pup
  {$EXTERNALSYM IPPROTO_UDP}
  IPPROTO_UDP    =  17;             // UDP - user datagram protocol
  {$EXTERNALSYM IPPROTO_IDP}
  IPPROTO_IDP    =  22;             // xns idp
  {$EXTERNALSYM IPPROTO_ND}
  IPPROTO_ND     =  77;             // UNOFFICIAL net disk proto

  {$EXTERNALSYM IPPROTO_RAW}
  IPPROTO_RAW    = 255;             // raw IP packet
  {$EXTERNALSYM IPPROTO_MAX}
  IPPROTO_MAX    = 256;

// Port/socket numbers: network standard functions
  {$EXTERNALSYM IPPORT_ECHO}
  IPPORT_ECHO        =   7;
  {$EXTERNALSYM IPPORT_DISCARD}
  IPPORT_DISCARD     =   9;
  {$EXTERNALSYM IPPORT_SYSTAT}
  IPPORT_SYSTAT      =  11;
  {$EXTERNALSYM IPPORT_DAYTIME}
  IPPORT_DAYTIME     =  13;
  {$EXTERNALSYM IPPORT_NETSTAT}
  IPPORT_NETSTAT     =  15;
  {$EXTERNALSYM IPPORT_FTP}
  IPPORT_FTP         =  21;
  {$EXTERNALSYM IPPORT_TELNET}
  IPPORT_TELNET      =  23;
  {$EXTERNALSYM IPPORT_SMTP}
  IPPORT_SMTP        =  25;
  {$EXTERNALSYM IPPORT_TIMESERVER}
  IPPORT_TIMESERVER  =  37;
  {$EXTERNALSYM IPPORT_NAMESERVER}
  IPPORT_NAMESERVER  =  42;
  {$EXTERNALSYM IPPORT_WHOIS}
  IPPORT_WHOIS       =  43;
  {$EXTERNALSYM IPPORT_MTP}
  IPPORT_MTP         =  57;

// Port/socket numbers: host specific functions
  {$EXTERNALSYM IPPORT_TFTP}
  IPPORT_TFTP        =  69;
  {$EXTERNALSYM IPPORT_RJE}
  IPPORT_RJE         =  77;
  {$EXTERNALSYM IPPORT_FINGER}
  IPPORT_FINGER      =  79;
  {$EXTERNALSYM IPPORT_TTYLINK}
  IPPORT_TTYLINK     =  87;
  {$EXTERNALSYM IPPORT_SUPDUP}
  IPPORT_SUPDUP      =  95;

// UNIX TCP sockets
  {$EXTERNALSYM IPPORT_EXECSERVER}
  IPPORT_EXECSERVER  = 512;
  {$EXTERNALSYM IPPORT_LOGINSERVER}
  IPPORT_LOGINSERVER = 513;
  {$EXTERNALSYM IPPORT_CMDSERVER}
  IPPORT_CMDSERVER   = 514;
  {$EXTERNALSYM IPPORT_EFSSERVER}
  IPPORT_EFSSERVER   = 520;

// UNIX UDP sockets
  {$EXTERNALSYM IPPORT_BIFFUDP}
  IPPORT_BIFFUDP     = 512;
  {$EXTERNALSYM IPPORT_WHOSERVER}
  IPPORT_WHOSERVER   = 513;
  {$EXTERNALSYM IPPORT_ROUTESERVER}
  IPPORT_ROUTESERVER = 520;

// Ports < IPPORT_RESERVED are reserved for  privileged processes (e.g. root).
  {$EXTERNALSYM IPPORT_RESERVED}
  IPPORT_RESERVED    =1024;

// Link numbers
  {$EXTERNALSYM IMPLINK_IP}
  IMPLINK_IP         = 155;
  {$EXTERNALSYM IMPLINK_LOWEXPER}
  IMPLINK_LOWEXPER   = 156;
  {$EXTERNALSYM IMPLINK_HIGHEXPER}
  IMPLINK_HIGHEXPER  = 158;

  {$EXTERNALSYM TF_DISCONNECT}
  TF_DISCONNECT      = $01;
  {$EXTERNALSYM TF_REUSE_SOCKET}
  TF_REUSE_SOCKET    = $02;
  {$EXTERNALSYM TF_WRITE_BEHIND}
  TF_WRITE_BEHIND    = $04;

// This is used instead of -1, since the TSocket type is unsigned.
  {$EXTERNALSYM INVALID_SOCKET}
  INVALID_SOCKET     = TSocket(not(0));
  {$EXTERNALSYM SOCKET_ERROR}
  SOCKET_ERROR       = -1;

//  The  following  may  be used in place of the address family, socket type, or
//  protocol  in  a  call  to WSASocket to indicate that the corresponding value
//  should  be taken from the supplied WSAPROTOCOL_INFO structure instead of the
//  parameter itself.
  {$EXTERNALSYM FROM_PROTOCOL_INFO}
  FROM_PROTOCOL_INFO = -1;


// Types
  {$EXTERNALSYM SOCK_STREAM}
  SOCK_STREAM     = 1;               { stream socket }
  {$EXTERNALSYM SOCK_DGRAM}
  SOCK_DGRAM      = 2;               { datagram socket }
  {$EXTERNALSYM SOCK_RAW}
  SOCK_RAW        = 3;               { raw-protocol interface }
  {$EXTERNALSYM SOCK_RDM}
  SOCK_RDM        = 4;               { reliably-delivered message }
  {$EXTERNALSYM SOCK_SEQPACKET}
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

// Option flags per-socket.
  {$EXTERNALSYM SO_DEBUG}
  SO_DEBUG            = $0001;            // turn on debugging info recording
  {$EXTERNALSYM SO_ACCEPTCONN}
  SO_ACCEPTCONN       = $0002;            // socket has had listen()
  {$EXTERNALSYM SO_REUSEADDR}
  SO_REUSEADDR        = $0004;            // allow local address reuse
  {$EXTERNALSYM SO_KEEPALIVE}
  SO_KEEPALIVE        = $0008;            // keep connections alive
  {$EXTERNALSYM SO_DONTROUTE}
  SO_DONTROUTE        = $0010;            // just use interface addresses
  {$EXTERNALSYM SO_BROADCAST}
  SO_BROADCAST        = $0020;            // permit sending of broadcast msgs
  {$EXTERNALSYM SO_USELOOPBACK}
  SO_USELOOPBACK      = $0040;            // bypass hardware when possible
  {$EXTERNALSYM SO_LINGER}
  SO_LINGER           = $0080;            // linger on close if data present
  {$EXTERNALSYM SO_OOBINLINE}
  SO_OOBINLINE        = $0100;            // leave received OOB data in line

  {$EXTERNALSYM SO_DONTLINGER}
  SO_DONTLINGER       = not SO_LINGER;
  {$EXTERNALSYM SO_EXCLUSIVEADDRUSE}
  SO_EXCLUSIVEADDRUSE = not SO_REUSEADDR; // disallow local address reuse

// Additional options.

  {$EXTERNALSYM SO_SNDBUF}
  SO_SNDBUF           = $1001;      // send buffer size
  {$EXTERNALSYM SO_RCVBUF}
  SO_RCVBUF           = $1002;      // receive buffer size
  {$EXTERNALSYM SO_SNDLOWAT}
  SO_SNDLOWAT         = $1003;      // send low-water mark
  {$EXTERNALSYM SO_RCVLOWAT}
  SO_RCVLOWAT         = $1004;      // receive low-water mark
  {$EXTERNALSYM SO_SNDTIMEO}
  SO_SNDTIMEO         = $1005;      // send timeout
  {$EXTERNALSYM SO_RCVTIMEO}
  SO_RCVTIMEO         = $1006;      // receive timeout
  {$EXTERNALSYM SO_ERROR}
  SO_ERROR            = $1007;      // get error status and clear
  {$EXTERNALSYM SO_TYPE}
  SO_TYPE             = $1008;      // get socket type

// Options for connect and disconnect data and options.
// Used only by non-TCP/IP transports such as DECNet, OSI TP4, etc.
  {$EXTERNALSYM SO_CONNDATA}
  SO_CONNDATA         = $7000;
  {$EXTERNALSYM SO_CONNOPT}
  SO_CONNOPT          = $7001;
  {$EXTERNALSYM SO_DISCDATA}
  SO_DISCDATA         = $7002;
  {$EXTERNALSYM SO_DISCOPT}
  SO_DISCOPT          = $7003;
  {$EXTERNALSYM SO_CONNDATALEN}
  SO_CONNDATALEN      = $7004;
  {$EXTERNALSYM SO_CONNOPTLEN}
  SO_CONNOPTLEN       = $7005;
  {$EXTERNALSYM SO_DISCDATALEN}
  SO_DISCDATALEN      = $7006;
  {$EXTERNALSYM SO_DISCOPTLEN}
  SO_DISCOPTLEN       = $7007;

// Option for opening sockets for synchronous access.
  {$EXTERNALSYM SO_OPENTYPE}
  SO_OPENTYPE         = $7008;
  {$EXTERNALSYM SO_SYNCHRONOUS_ALERT}
  SO_SYNCHRONOUS_ALERT    = $10;
  {$EXTERNALSYM SO_SYNCHRONOUS_NONALERT}
  SO_SYNCHRONOUS_NONALERT = $20;

// Other NT-specific options.
  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG                 = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG             = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME          = $700C;

// TCP options.
  {$EXTERNALSYM TCP_NODELAY}
  TCP_NODELAY              = $0001;
  {$EXTERNALSYM TCP_BSDURGENT}
  TCP_BSDURGENT            = $7000;

// Address families.
  {$EXTERNALSYM AF_UNSPEC}
  AF_UNSPEC       = 0;               // unspecified
  {$EXTERNALSYM AF_UNIX}
  AF_UNIX         = 1;               // local to host (pipes, portals)
  {$EXTERNALSYM AF_INET}
  AF_INET         = 2;               // internetwork: UDP, TCP, etc.
  {$EXTERNALSYM AF_IMPLINK}
  AF_IMPLINK      = 3;               // arpanet imp addresses
  {$EXTERNALSYM AF_PUP}
  AF_PUP          = 4;               // pup protocols: e.g. BSP
  {$EXTERNALSYM AF_CHAOS}
  AF_CHAOS        = 5;               // mit CHAOS protocols
  {$EXTERNALSYM AF_IPX}
  AF_IPX          = 6;               // IPX and SPX
  {$EXTERNALSYM AF_NS}
  AF_NS           = AF_IPX;          // XEROX NS protocols
  {$EXTERNALSYM AF_ISO}
  AF_ISO          = 7;               // ISO protocols
  {$EXTERNALSYM AF_OSI}
  AF_OSI          = AF_ISO;          // OSI is ISO
  {$EXTERNALSYM AF_ECMA}
  AF_ECMA         = 8;               // european computer manufacturers
  {$EXTERNALSYM AF_DATAKIT}
  AF_DATAKIT      = 9;               // datakit protocols
  {$EXTERNALSYM AF_CCITT}
  AF_CCITT        = 10;              // CCITT protocols, X.25 etc
  {$EXTERNALSYM AF_SNA}
  AF_SNA          = 11;              // IBM SNA
  {$EXTERNALSYM AF_DECnet}
  AF_DECnet       = 12;              // DECnet
  {$EXTERNALSYM AF_DLI}
  AF_DLI          = 13;              // Direct data link interface
  {$EXTERNALSYM AF_LAT}
  AF_LAT          = 14;              // LAT
  {$EXTERNALSYM AF_HYLINK}
  AF_HYLINK       = 15;              // NSC Hyperchannel
  {$EXTERNALSYM AF_APPLETALK}
  AF_APPLETALK    = 16;              // AppleTalk
  {$EXTERNALSYM AF_NETBIOS}
  AF_NETBIOS      = 17;              // NetBios-style addresses
  {$EXTERNALSYM AF_VOICEVIEW}
  AF_VOICEVIEW    = 18;              // VoiceView
  {$EXTERNALSYM AF_FIREFOX}
  AF_FIREFOX      = 19;              // FireFox
  {$EXTERNALSYM AF_UNKNOWN1}
  AF_UNKNOWN1     = 20;              // Somebody is using this!
  {$EXTERNALSYM AF_BAN}
  AF_BAN          = 21;              // Banyan
  {$EXTERNALSYM AF_ATM}
  AF_ATM          = 22;              // Native ATM Services
  {$EXTERNALSYM AF_INET6}
  AF_INET6        = 23;              // Internetwork Version 6
  {$EXTERNALSYM AF_CLUSTER}
  AF_CLUSTER      = 24;              // Microsoft Wolfpack
  {$EXTERNALSYM AF_12844}
  AF_12844        = 25;              // IEEE 1284.4 WG AF
  {$EXTERNALSYM AF_IRDA}
  AF_IRDA         = 26;              // IrDA
  {$EXTERNALSYM AF_NETDES}
  AF_NETDES       = 28;              // Network Designers OSI & gateway enabled protocols

  {$EXTERNALSYM AF_MAX}
  AF_MAX          = 29;


// Protocol families, same as address families for now.

  {$EXTERNALSYM PF_UNSPEC}
  PF_UNSPEC       = AF_UNSPEC;
  {$EXTERNALSYM PF_UNIX}
  PF_UNIX         = AF_UNIX;
  {$EXTERNALSYM PF_INET}
  PF_INET         = AF_INET;
  {$EXTERNALSYM PF_IMPLINK}
  PF_IMPLINK      = AF_IMPLINK;
  {$EXTERNALSYM PF_PUP}
  PF_PUP          = AF_PUP;
  {$EXTERNALSYM PF_CHAOS}
  PF_CHAOS        = AF_CHAOS;
  {$EXTERNALSYM PF_NS}
  PF_NS           = AF_NS;
  {$EXTERNALSYM PF_IPX}
  PF_IPX          = AF_IPX;
  {$EXTERNALSYM PF_ISO}
  PF_ISO          = AF_ISO;
  {$EXTERNALSYM PF_OSI}
  PF_OSI          = AF_OSI;
  {$EXTERNALSYM PF_ECMA}
  PF_ECMA         = AF_ECMA;
  {$EXTERNALSYM PF_DATAKIT}
  PF_DATAKIT      = AF_DATAKIT;
  {$EXTERNALSYM PF_CCITT}
  PF_CCITT        = AF_CCITT;
  {$EXTERNALSYM PF_SNA}
  PF_SNA          = AF_SNA;
  {$EXTERNALSYM PF_DECnet}
  PF_DECnet       = AF_DECnet;
  {$EXTERNALSYM PF_DLI}
  PF_DLI          = AF_DLI;
  {$EXTERNALSYM PF_LAT}
  PF_LAT          = AF_LAT;
  {$EXTERNALSYM PF_HYLINK}
  PF_HYLINK       = AF_HYLINK;
  {$EXTERNALSYM PF_APPLETALK}
  PF_APPLETALK    = AF_APPLETALK;
  {$EXTERNALSYM PF_VOICEVIEW}
  PF_VOICEVIEW    = AF_VOICEVIEW;
  {$EXTERNALSYM PF_FIREFOX}
  PF_FIREFOX      = AF_FIREFOX;
  {$EXTERNALSYM PF_UNKNOWN1}
  PF_UNKNOWN1     = AF_UNKNOWN1;
  {$EXTERNALSYM PF_BAN}
  PF_BAN          = AF_BAN;
  {$EXTERNALSYM PF_ATM}
  PF_ATM          = AF_ATM;
  {$EXTERNALSYM PF_INET6}
  PF_INET6        = AF_INET6;

  {$EXTERNALSYM PF_MAX}
  PF_MAX          = AF_MAX;

type

  {$EXTERNALSYM SunB}
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;

  {$EXTERNALSYM SunW}
  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  {$EXTERNALSYM TInAddr}
  TInAddr = packed record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  {$EXTERNALSYM PInAddr}
  PInAddr = ^TInAddr;

  // Structure used by kernel to store most addresses.

  {$EXTERNALSYM TSockAddrIn}
  TSockAddrIn = packed record
    case Integer of
      0: (sin_family : u_short;
          sin_port   : u_short;
          sin_addr   : TInAddr;
          sin_zero   : array[0..7] of AnsiChar);
      1: (sa_family  : u_short;
          sa_data    : array[0..13] of AnsiChar)
  end;
  {$EXTERNALSYM PSockAddrIn}
  PSockAddrIn = ^TSockAddrIn;
  {$EXTERNALSYM TSockAddr}
  TSockAddr   = TSockAddrIn;
  {$EXTERNALSYM PSockAddr}
  PSockAddr   = ^TSockAddr;
  {$EXTERNALSYM SOCKADDR}
  SOCKADDR    = TSockAddr;
  {$EXTERNALSYM SOCKADDR_IN}
  SOCKADDR_IN = TSockAddrIn;

  // Structure used by kernel to pass protocol information in raw sockets.
  {$EXTERNALSYM PSockProto}
  PSockProto = ^TSockProto;
  {$EXTERNALSYM TSockProto}
  TSockProto = packed record
    sp_family   : u_short;
    sp_protocol : u_short;
  end;

// Structure used for manipulating linger option.
  {$EXTERNALSYM PLinger}
  PLinger = ^TLinger;
  {$EXTERNALSYM TLinger}
  TLinger = packed record
    l_onoff: u_short;
    l_linger: u_short;
  end;

const
  {$EXTERNALSYM INADDR_ANY}
  INADDR_ANY       = $00000000;
  {$EXTERNALSYM INADDR_LOOPBACK}
  INADDR_LOOPBACK  = $7F000001;
  {$EXTERNALSYM INADDR_BROADCAST}
  INADDR_BROADCAST = $FFFFFFFF;
  {$EXTERNALSYM INADDR_NONE}
  INADDR_NONE      = $FFFFFFFF;

  {$EXTERNALSYM ADDR_ANY}
  ADDR_ANY         = INADDR_ANY;

  {$EXTERNALSYM SOL_SOCKET}
  SOL_SOCKET       = $ffff;          // options for socket level

  {$EXTERNALSYM MSG_OOB}
  MSG_OOB          = $1;             // process out-of-band data
  {$EXTERNALSYM MSG_PEEK}
  MSG_PEEK         = $2;             // peek at incoming message
  {$EXTERNALSYM MSG_DONTROUTE}
  MSG_DONTROUTE    = $4;             // send without using routing tables

  {$EXTERNALSYM MSG_PARTIAL}
  MSG_PARTIAL      = $8000;          // partial send or recv for message xport

// Define constant based on rfc883, used by gethostbyxxxx() calls.

  {$EXTERNALSYM MAXGETHOSTSTRUCT}
  MAXGETHOSTSTRUCT = 1024;

// Maximum queue length specifiable by listen.
  {$EXTERNALSYM SOMAXCONN}
  SOMAXCONN        = $7fffffff;

// WinSock 2 extension -- bit values and indices for FD_XXX network events
  {$EXTERNALSYM FD_READ_BIT}
  FD_READ_BIT      = 0;
  {$EXTERNALSYM FD_WRITE_BIT}
  FD_WRITE_BIT     = 1;
  {$EXTERNALSYM FD_OOB_BIT}
  FD_OOB_BIT       = 2;
  {$EXTERNALSYM FD_ACCEPT_BIT}
  FD_ACCEPT_BIT    = 3;
  {$EXTERNALSYM FD_CONNECT_BIT}
  FD_CONNECT_BIT   = 4;
  {$EXTERNALSYM FD_CLOSE_BIT}
  FD_CLOSE_BIT     = 5;
  {$EXTERNALSYM FD_QOS_BIT}
  FD_QOS_BIT       = 6;
  {$EXTERNALSYM FD_GROUP_QOS_BIT}
  FD_GROUP_QOS_BIT = 7;

  {$EXTERNALSYM FD_MAX_EVENTS}
  FD_MAX_EVENTS    = 8;

  {$EXTERNALSYM FD_READ}
  FD_READ       = (1 shl FD_READ_BIT);
  {$EXTERNALSYM FD_WRITE}
  FD_WRITE      = (1 shl FD_WRITE_BIT);
  {$EXTERNALSYM FD_OOB}
  FD_OOB        = (1 shl FD_OOB_BIT);
  {$EXTERNALSYM FD_ACCEPT}
  FD_ACCEPT     = (1 shl FD_ACCEPT_BIT);
  {$EXTERNALSYM FD_CONNECT}
  FD_CONNECT    = (1 shl FD_CONNECT_BIT);
  {$EXTERNALSYM FD_CLOSE}
  FD_CLOSE      = (1 shl FD_CLOSE_BIT);
  {$EXTERNALSYM FD_QOS}
  FD_QOS        = (1 shl FD_QOS_BIT);
  {$EXTERNALSYM FD_GROUP_QOS}
  FD_GROUP_QOS  = (1 shl FD_GROUP_QOS_BIT);

  {$EXTERNALSYM FD_ALL_EVENTS}
  FD_ALL_EVENTS = (1 shl FD_MAX_EVENTS) - 1;

// All Windows Sockets error constants are biased by WSABASEERR from the "normal"

  {$EXTERNALSYM WSABASEERR}
  WSABASEERR              = 10000;

// Windows Sockets definitions of regular Microsoft C error constants

  {$EXTERNALSYM WSAEINTR}
  WSAEINTR                = WSABASEERR+  4;
  {$EXTERNALSYM WSAEBADF}
  WSAEBADF                = WSABASEERR+  9;
  {$EXTERNALSYM WSAEACCES}
  WSAEACCES               = WSABASEERR+ 13;
  {$EXTERNALSYM WSAEFAULT}
  WSAEFAULT               = WSABASEERR+ 14;
  {$EXTERNALSYM WSAEINVAL}
  WSAEINVAL               = WSABASEERR+ 22;
  {$EXTERNALSYM WSAEMFILE}
  WSAEMFILE               = WSABASEERR+ 24;

// Windows Sockets definitions of regular Berkeley error constants

  {$EXTERNALSYM WSAEWOULDBLOCK}
  WSAEWOULDBLOCK          = WSABASEERR+ 35;
  {$EXTERNALSYM WSAEINPROGRESS}
  WSAEINPROGRESS          = WSABASEERR+ 36;
  {$EXTERNALSYM WSAEALREADY}
  WSAEALREADY             = WSABASEERR+ 37;
  {$EXTERNALSYM WSAENOTSOCK}
  WSAENOTSOCK             = WSABASEERR+ 38;
  {$EXTERNALSYM WSAEDESTADDRREQ}
  WSAEDESTADDRREQ         = WSABASEERR+ 39;
  {$EXTERNALSYM WSAEMSGSIZE}
  WSAEMSGSIZE             = WSABASEERR+ 40;
  {$EXTERNALSYM WSAEPROTOTYPE}
  WSAEPROTOTYPE           = WSABASEERR+ 41;
  {$EXTERNALSYM WSAENOPROTOOPT}
  WSAENOPROTOOPT          = WSABASEERR+ 42;
  {$EXTERNALSYM WSAEPROTONOSUPPORT}
  WSAEPROTONOSUPPORT      = WSABASEERR+ 43;
  {$EXTERNALSYM WSAESOCKTNOSUPPORT}
  WSAESOCKTNOSUPPORT      = WSABASEERR+ 44;
  {$EXTERNALSYM WSAEOPNOTSUPP}
  WSAEOPNOTSUPP           = WSABASEERR+ 45;
  {$EXTERNALSYM WSAEPFNOSUPPORT}
  WSAEPFNOSUPPORT         = WSABASEERR+ 46;
  {$EXTERNALSYM WSAEAFNOSUPPORT}
  WSAEAFNOSUPPORT         = WSABASEERR+ 47;
  {$EXTERNALSYM WSAEADDRINUSE}
  WSAEADDRINUSE           = WSABASEERR+ 48;
  {$EXTERNALSYM WSAEADDRNOTAVAIL}
  WSAEADDRNOTAVAIL        = WSABASEERR+ 49;
  {$EXTERNALSYM WSAENETDOWN}
  WSAENETDOWN             = WSABASEERR+ 50;
  {$EXTERNALSYM WSAENETUNREACH}
  WSAENETUNREACH          = WSABASEERR+ 51;
  {$EXTERNALSYM WSAENETRESET}
  WSAENETRESET            = WSABASEERR+ 52;
  {$EXTERNALSYM WSAECONNABORTED}
  WSAECONNABORTED         = WSABASEERR+ 53;
  {$EXTERNALSYM WSAECONNRESET}
  WSAECONNRESET           = WSABASEERR+ 54;
  {$EXTERNALSYM WSAENOBUFS}
  WSAENOBUFS              = WSABASEERR+ 55;
  {$EXTERNALSYM WSAEISCONN}
  WSAEISCONN              = WSABASEERR+ 56;
  {$EXTERNALSYM WSAENOTCONN}
  WSAENOTCONN             = WSABASEERR+ 57;
  {$EXTERNALSYM WSAESHUTDOWN}
  WSAESHUTDOWN            = WSABASEERR+ 58;
  {$EXTERNALSYM WSAETOOMANYREFS}
  WSAETOOMANYREFS         = WSABASEERR+ 59;
  {$EXTERNALSYM WSAETIMEDOUT}
  WSAETIMEDOUT            = WSABASEERR+ 60;
  {$EXTERNALSYM WSAECONNREFUSED}
  WSAECONNREFUSED         = WSABASEERR+ 61;
  {$EXTERNALSYM WSAELOOP}
  WSAELOOP                = WSABASEERR+ 62;
  {$EXTERNALSYM WSAENAMETOOLONG}
  WSAENAMETOOLONG         = WSABASEERR+ 63;
  {$EXTERNALSYM WSAEHOSTDOWN}
  WSAEHOSTDOWN            = WSABASEERR+ 64;
  {$EXTERNALSYM WSAEHOSTUNREACH}
  WSAEHOSTUNREACH         = WSABASEERR+ 65;
  {$EXTERNALSYM WSAENOTEMPTY}
  WSAENOTEMPTY            = WSABASEERR+ 66;
  {$EXTERNALSYM WSAEPROCLIM}
  WSAEPROCLIM             = WSABASEERR+ 67;
  {$EXTERNALSYM WSAEUSERS}
  WSAEUSERS               = WSABASEERR+ 68;
  {$EXTERNALSYM WSAEDQUOT}
  WSAEDQUOT               = WSABASEERR+ 69;
  {$EXTERNALSYM WSAESTALE}
  WSAESTALE               = WSABASEERR+ 70;
  {$EXTERNALSYM WSAEREMOTE}
  WSAEREMOTE              = WSABASEERR+ 71;

// Extended Windows Sockets error constant definitions

  {$EXTERNALSYM WSASYSNOTREADY}
  WSASYSNOTREADY          = WSABASEERR+ 91;
  {$EXTERNALSYM WSAVERNOTSUPPORTED}
  WSAVERNOTSUPPORTED      = WSABASEERR+ 92;
  {$EXTERNALSYM WSANOTINITIALISED}
  WSANOTINITIALISED       = WSABASEERR+ 93;
  {$EXTERNALSYM WSAEDISCON}
  WSAEDISCON              = WSABASEERR+101;
  {$EXTERNALSYM WSAENOMORE}
  WSAENOMORE              = WSABASEERR+102;
  {$EXTERNALSYM WSAECANCELLED}
  WSAECANCELLED           = WSABASEERR+103;
  {$EXTERNALSYM WSAEINVALIDPROCTABLE}
  WSAEINVALIDPROCTABLE    = WSABASEERR+104;
  {$EXTERNALSYM WSAEINVALIDPROVIDER}
  WSAEINVALIDPROVIDER     = WSABASEERR+105;
  {$EXTERNALSYM WSAEPROVIDERFAILEDINIT}
  WSAEPROVIDERFAILEDINIT  = WSABASEERR+106;
  {$EXTERNALSYM WSASYSCALLFAILURE}
  WSASYSCALLFAILURE       = WSABASEERR+107;
  {$EXTERNALSYM WSASERVICE_NOT_FOUND}
  WSASERVICE_NOT_FOUND    = WSABASEERR+108;
  {$EXTERNALSYM WSATYPE_NOT_FOUND}
  WSATYPE_NOT_FOUND       = WSABASEERR+109;
  {$EXTERNALSYM WSA_E_NO_MORE}
  WSA_E_NO_MORE           = WSABASEERR+110;
  {$EXTERNALSYM WSA_E_CANCELLED}
  WSA_E_CANCELLED         = WSABASEERR+111;
  {$EXTERNALSYM WSAEREFUSED}
  WSAEREFUSED             = WSABASEERR+112;


{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

// Authoritative Answer: Host not found
  {$EXTERNALSYM WSAHOST_NOT_FOUND}
  WSAHOST_NOT_FOUND        = WSABASEERR+1001;
  {$EXTERNALSYM HOST_NOT_FOUND}
  HOST_NOT_FOUND           = WSAHOST_NOT_FOUND;

// Non-Authoritative: Host not found, or SERVERFAIL
  {$EXTERNALSYM WSATRY_AGAIN}
  WSATRY_AGAIN             = WSABASEERR+1002;
  {$EXTERNALSYM TRY_AGAIN}
  TRY_AGAIN                = WSATRY_AGAIN;

// Non recoverable errors, FORMERR, REFUSED, NOTIMP
  {$EXTERNALSYM WSANO_RECOVERY}
  WSANO_RECOVERY           = WSABASEERR+1003;
  {$EXTERNALSYM NO_RECOVERY}
  NO_RECOVERY              = WSANO_RECOVERY;

// Valid name, no data record of requested type
  {$EXTERNALSYM WSANO_DATA}
  WSANO_DATA               = WSABASEERR+1004;
  {$EXTERNALSYM NO_DATA}
  NO_DATA                  = WSANO_DATA;

// no address, look for MX record
  {$EXTERNALSYM WSANO_ADDRESS}
  WSANO_ADDRESS            = WSANO_DATA;
  {$EXTERNALSYM NO_ADDRESS}
  NO_ADDRESS               = WSANO_ADDRESS;

// Define QOS related error return codes

  {$EXTERNALSYM WSA_QOS_RECEIVERS}
  WSA_QOS_RECEIVERS          = WSABASEERR+1005; // at least one Reserve has arrived
  {$EXTERNALSYM WSA_QOS_SENDERS}
  WSA_QOS_SENDERS            = WSABASEERR+1006; // at least one Path has arrived
  {$EXTERNALSYM WSA_QOS_NO_SENDERS}
  WSA_QOS_NO_SENDERS         = WSABASEERR+1007; // there are no senders
  {$EXTERNALSYM WSA_QOS_NO_RECEIVERS}
  WSA_QOS_NO_RECEIVERS       = WSABASEERR+1008; // there are no receivers
  {$EXTERNALSYM WSA_QOS_REQUEST_CONFIRMED}
  WSA_QOS_REQUEST_CONFIRMED  = WSABASEERR+1009; // Reserve has been confirmed
  {$EXTERNALSYM WSA_QOS_ADMISSION_FAILURE}
  WSA_QOS_ADMISSION_FAILURE  = WSABASEERR+1010; // error due to lack of resources
  {$EXTERNALSYM WSA_QOS_POLICY_FAILURE}
  WSA_QOS_POLICY_FAILURE     = WSABASEERR+1011; // rejected for administrative reasons - bad credentials
  {$EXTERNALSYM WSA_QOS_BAD_STYLE}
  WSA_QOS_BAD_STYLE          = WSABASEERR+1012; // unknown or conflicting style
  {$EXTERNALSYM WSA_QOS_BAD_OBJECT}
  WSA_QOS_BAD_OBJECT         = WSABASEERR+1013; // problem with some part of the filterspec or providerspecific buffer in general
  {$EXTERNALSYM WSA_QOS_TRAFFIC_CTRL_ERROR}
  WSA_QOS_TRAFFIC_CTRL_ERROR = WSABASEERR+1014; // problem with some part of the flowspec
  {$EXTERNALSYM WSA_QOS_GENERIC_ERROR}
  WSA_QOS_GENERIC_ERROR      = WSABASEERR+1015; // general error
  {$EXTERNALSYM WSA_QOS_ESERVICETYPE}
  WSA_QOS_ESERVICETYPE       = WSABASEERR+1016; // invalid service type in flowspec
  {$EXTERNALSYM WSA_QOS_EFLOWSPEC}
  WSA_QOS_EFLOWSPEC          = WSABASEERR+1017; // invalid flowspec
  {$EXTERNALSYM WSA_QOS_EPROVSPECBUF}
  WSA_QOS_EPROVSPECBUF       = WSABASEERR+1018; // invalid provider specific buffer
  {$EXTERNALSYM WSA_QOS_EFILTERSTYLE}
  WSA_QOS_EFILTERSTYLE       = WSABASEERR+1019; // invalid filter style
  {$EXTERNALSYM WSA_QOS_EFILTERTYPE}
  WSA_QOS_EFILTERTYPE        = WSABASEERR+1020; // invalid filter type
  {$EXTERNALSYM WSA_QOS_EFILTERCOUNT}
  WSA_QOS_EFILTERCOUNT       = WSABASEERR+1021; // incorrect number of filters
  {$EXTERNALSYM WSA_QOS_EOBJLENGTH}
  WSA_QOS_EOBJLENGTH         = WSABASEERR+1022; // invalid object length
  {$EXTERNALSYM WSA_QOS_EFLOWCOUNT}
  WSA_QOS_EFLOWCOUNT         = WSABASEERR+1023; // incorrect number of flows
  {$EXTERNALSYM WSA_QOS_EUNKOWNPSOBJ}
  WSA_QOS_EUNKOWNPSOBJ       = WSABASEERR+1024; // unknown object in provider specific buffer
  {$EXTERNALSYM WSA_QOS_EPOLICYOBJ}
  WSA_QOS_EPOLICYOBJ         = WSABASEERR+1025; // invalid policy object in provider specific buffer
  {$EXTERNALSYM WSA_QOS_EFLOWDESC}
  WSA_QOS_EFLOWDESC          = WSABASEERR+1026; // invalid flow descriptor in the list
  {$EXTERNALSYM WSA_QOS_EPSFLOWSPEC}
  WSA_QOS_EPSFLOWSPEC        = WSABASEERR+1027; // inconsistent flow spec in provider specific buffer
  {$EXTERNALSYM WSA_QOS_EPSFILTERSPEC}
  WSA_QOS_EPSFILTERSPEC      = WSABASEERR+1028; // invalid filter spec in provider specific buffer
  {$EXTERNALSYM WSA_QOS_ESDMODEOBJ}
  WSA_QOS_ESDMODEOBJ         = WSABASEERR+1029; // invalid shape discard mode object in provider specific buffer
  {$EXTERNALSYM WSA_QOS_ESHAPERATEOBJ}
  WSA_QOS_ESHAPERATEOBJ      = WSABASEERR+1030; // invalid shaping rate object in provider specific buffer
  {$EXTERNALSYM WSA_QOS_RESERVED_PETYPE}
  WSA_QOS_RESERVED_PETYPE    = WSABASEERR+1031; // reserved policy element in provider specific buffer


{ WinSock 2 extension -- new error codes and type definition }
  {$EXTERNALSYM WSA_IO_PENDING}
  WSA_IO_PENDING          = ERROR_IO_PENDING;
  {$EXTERNALSYM WSA_IO_INCOMPLETE}
  WSA_IO_INCOMPLETE       = ERROR_IO_INCOMPLETE;
  {$EXTERNALSYM WSA_INVALID_HANDLE}
  WSA_INVALID_HANDLE      = ERROR_INVALID_HANDLE;
  {$EXTERNALSYM WSA_INVALID_PARAMETER}
  WSA_INVALID_PARAMETER   = ERROR_INVALID_PARAMETER;
  {$EXTERNALSYM WSA_NOT_ENOUGH_MEMORY}
  WSA_NOT_ENOUGH_MEMORY   = ERROR_NOT_ENOUGH_MEMORY;
  {$EXTERNALSYM WSA_OPERATION_ABORTED}
  WSA_OPERATION_ABORTED   = ERROR_OPERATION_ABORTED;
  {$EXTERNALSYM WSA_INVALID_EVENT}
  WSA_INVALID_EVENT       = WSAEVENT(nil);
  {$EXTERNALSYM WSA_MAXIMUM_WAIT_EVENTS}
  WSA_MAXIMUM_WAIT_EVENTS = MAXIMUM_WAIT_OBJECTS;
  {$EXTERNALSYM WSA_WAIT_FAILED}
  WSA_WAIT_FAILED         = $ffffffff;
  {$EXTERNALSYM WSA_WAIT_EVENT_0}
  WSA_WAIT_EVENT_0        = WAIT_OBJECT_0;
  {$EXTERNALSYM WSA_WAIT_IO_COMPLETION}
  WSA_WAIT_IO_COMPLETION  = WAIT_IO_COMPLETION;
  {$EXTERNALSYM WSA_WAIT_TIMEOUT}
  WSA_WAIT_TIMEOUT        = WAIT_TIMEOUT;
  {$EXTERNALSYM WSA_INFINITE}
  WSA_INFINITE            = INFINITE;

{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

  {$EXTERNALSYM EWOULDBLOCK}
  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  {$EXTERNALSYM EINPROGRESS}
  EINPROGRESS        =  WSAEINPROGRESS;
  {$EXTERNALSYM EALREADY}
  EALREADY           =  WSAEALREADY;
  {$EXTERNALSYM ENOTSOCK}
  ENOTSOCK           =  WSAENOTSOCK;
  {$EXTERNALSYM EDESTADDRREQ}
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  {$EXTERNALSYM EMSGSIZE}
  EMSGSIZE           =  WSAEMSGSIZE;
  {$EXTERNALSYM EPROTOTYPE}
  EPROTOTYPE         =  WSAEPROTOTYPE;
  {$EXTERNALSYM ENOPROTOOPT}
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  {$EXTERNALSYM EPROTONOSUPPORT}
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  {$EXTERNALSYM ESOCKTNOSUPPORT}
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  {$EXTERNALSYM EOPNOTSUPP}
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  {$EXTERNALSYM EPFNOSUPPORT}
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  {$EXTERNALSYM EAFNOSUPPORT}
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  {$EXTERNALSYM EADDRINUSE}
  EADDRINUSE         =  WSAEADDRINUSE;
  {$EXTERNALSYM EADDRNOTAVAIL}
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  {$EXTERNALSYM ENETDOWN}
  ENETDOWN           =  WSAENETDOWN;
  {$EXTERNALSYM ENETUNREACH}
  ENETUNREACH        =  WSAENETUNREACH;
  {$EXTERNALSYM ENETRESET}
  ENETRESET          =  WSAENETRESET;
  {$EXTERNALSYM ECONNABORTED}
  ECONNABORTED       =  WSAECONNABORTED;
  {$EXTERNALSYM ECONNRESET}
  ECONNRESET         =  WSAECONNRESET;
  {$EXTERNALSYM ENOBUFS}
  ENOBUFS            =  WSAENOBUFS;
  {$EXTERNALSYM EISCONN}
  EISCONN            =  WSAEISCONN;
  {$EXTERNALSYM ENOTCONN}
  ENOTCONN           =  WSAENOTCONN;
  {$EXTERNALSYM ESHUTDOWN}
  ESHUTDOWN          =  WSAESHUTDOWN;
  {$EXTERNALSYM ETOOMANYREFS}
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  {$EXTERNALSYM ETIMEDOUT}
  ETIMEDOUT          =  WSAETIMEDOUT;
  {$EXTERNALSYM ECONNREFUSED}
  ECONNREFUSED       =  WSAECONNREFUSED;
  {$EXTERNALSYM ELOOP}
  ELOOP              =  WSAELOOP;
  {$EXTERNALSYM ENAMETOOLONG}
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  {$EXTERNALSYM EHOSTDOWN}
  EHOSTDOWN          =  WSAEHOSTDOWN;
  {$EXTERNALSYM EHOSTUNREACH}
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
  {$EXTERNALSYM ENOTEMPTY}
  ENOTEMPTY          =  WSAENOTEMPTY;
  {$EXTERNALSYM EPROCLIM}
  EPROCLIM           =  WSAEPROCLIM;
  {$EXTERNALSYM EUSERS}
  EUSERS             =  WSAEUSERS;
  {$EXTERNALSYM EDQUOT}
  EDQUOT             =  WSAEDQUOT;
  {$EXTERNALSYM ESTALE}
  ESTALE             =  WSAESTALE;
  {$EXTERNALSYM EREMOTE}
  EREMOTE            =  WSAEREMOTE;


  {$EXTERNALSYM WSADESCRIPTION_LEN}
  WSADESCRIPTION_LEN     =   256;
  {$EXTERNALSYM WSASYS_STATUS_LEN}
  WSASYS_STATUS_LEN      =   128;

type
  {$EXTERNALSYM PWSAData}
  PWSAData = ^TWSAData;
  {$EXTERNALSYM TWSAData}
  TWSAData = packed record
    wVersion       : Word;
    wHighVersion   : Word;
    szDescription  : Array[0..WSADESCRIPTION_LEN] of AnsiChar;
    szSystemStatus : Array[0..WSASYS_STATUS_LEN] of AnsiChar;
    iMaxSockets    : Word;
    iMaxUdpDg      : Word;
    lpVendorInfo   : PAnsiChar;
  end;

const
//  WinSock 2 extension -- manifest constants for shutdown()
  {$EXTERNALSYM SD_RECEIVE}
  SD_RECEIVE = $00;
  {$EXTERNALSYM SD_SEND}
  SD_SEND    = $01;
  {$EXTERNALSYM SD_BOTH}
  SD_BOTH    = $02;

// SockAddr Information
type
  {$EXTERNALSYM SOCKET_ADDRESS}
  SOCKET_ADDRESS = packed record
    lpSockaddr : PSockAddr;
    iSockaddrLength : Integer;
  end;
  {$EXTERNALSYM PSOCKET_ADDRESS}
  PSOCKET_ADDRESS = ^SOCKET_ADDRESS;

// CSAddr Information
  {$EXTERNALSYM CSADDR_INFO}
  CSADDR_INFO = packed record
    LocalAddr, RemoteAddr  : SOCKET_ADDRESS;
    iSocketType, iProtocol : LongInt;
  end;
  {$EXTERNALSYM PCSADDR_INFO}
  PCSADDR_INFO = ^CSADDR_INFO;
  {$EXTERNALSYM LPCSADDR_INFO}
  LPCSADDR_INFO = ^CSADDR_INFO;

// Address list returned via WSAIoctl( SIO_ADDRESS_LIST_QUERY )
  {$EXTERNALSYM SOCKET_ADDRESS_LIST}
  SOCKET_ADDRESS_LIST = packed record
    iAddressCount : Integer;
    Address       : Array [0..0] of SOCKET_ADDRESS;
  end;
  {$EXTERNALSYM LPSOCKET_ADDRESS_LIST}
  LPSOCKET_ADDRESS_LIST = ^SOCKET_ADDRESS_LIST;

// Address Family/Protocol Tuples
  {$EXTERNALSYM AFProtocols}
  AFProtocols = record
    iAddressFamily : Integer;
    iProtocol      : Integer;
  end;
  {$EXTERNALSYM TAFProtocols}
  TAFProtocols = AFProtocols;
  {$EXTERNALSYM PAFProtocols}
  PAFProtocols = ^TAFProtocols;

  {$EXTERNALSYM accept}
function accept( const s: TSocket; var addr: TSockAddr; var addrlen: Integer ): TSocket; stdcall;
  {$EXTERNALSYM bind}
function bind( const s: TSocket; const addr: PSockAddr; const namelen: Integer ): Integer; stdcall;
  {$EXTERNALSYM closesocket}
function closesocket( const s: TSocket ): Integer; stdcall;
  {$EXTERNALSYM connect}
function connect( const s: TSocket; const name: PSockAddr; namelen: Integer): Integer; stdcall;
  {$EXTERNALSYM ioctlsocket}
function ioctlsocket( const s: TSocket; const cmd: DWORD; var arg: u_long ): Integer; stdcall;
  {$EXTERNALSYM getpeername}
function getpeername( const s: TSocket; var name: TSockAddr; var namelen: Integer ): Integer; stdcall;
  {$EXTERNALSYM getsockname}
function getsockname( const s: TSocket; var name: TSockAddr; var namelen: Integer ): Integer; stdcall;
  {$EXTERNALSYM getsockopt}
function getsockopt( const s: TSocket; const level, optname: Integer; optval: PAnsiChar; var optlen: Integer ): Integer; stdcall;
  {$EXTERNALSYM htonl}
function htonl(hostlong: u_long): u_long; stdcall;
  {$EXTERNALSYM htons}
function htons(hostshort: u_short): u_short; stdcall;
  {$EXTERNALSYM inet_addr}
function inet_addr(cp: PAnsiChar): u_long; stdcall;
  {$EXTERNALSYM inet_ntoa}
function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall;
  {$EXTERNALSYM listen}
function listen(s: TSocket; backlog: Integer): Integer; stdcall;
  {$EXTERNALSYM ntohl}
function ntohl(netlong: u_long): u_long; stdcall;
  {$EXTERNALSYM ntohs}
function ntohs(netshort: u_short): u_short; stdcall;
  {$EXTERNALSYM recv}
function recv(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
  {$EXTERNALSYM recvfrom}
function recvfrom(s: TSocket; var Buf; len, flags: Integer; var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
  {$EXTERNALSYM select}
function select(nfds: Integer; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Integer; stdcall;
  {$EXTERNALSYM send}
function send(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
  {$EXTERNALSYM sendto}
function sendto(s: TSocket; var Buf; len, flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
  {$EXTERNALSYM setsockopt}
function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  {$EXTERNALSYM shutdown}
function shutdown(s: TSocket; how: Integer): Integer; stdcall;
  {$EXTERNALSYM socket}
function socket( const af, _type, protocol: Integer ): TSocket; stdcall;
  {$EXTERNALSYM gethostbyaddr}
function gethostbyaddr(addr: Pointer; len, _type: Integer): PHostEnt; stdcall;
  {$EXTERNALSYM gethostbyname}
function gethostbyname(name: PAnsiChar): PHostEnt; stdcall;
  {$EXTERNALSYM gethostname}
function gethostname(name: PAnsiChar; len: Integer): Integer; stdcall;
  {$EXTERNALSYM getservbyport}
function getservbyport(port: Integer; proto: PAnsiChar): PServEnt; stdcall;
  {$EXTERNALSYM getservbyname}
function getservbyname(const name, proto: PAnsiChar): PServEnt; stdcall;
  {$EXTERNALSYM getprotobynumber}
function getprotobynumber(const proto: Integer): PProtoEnt; stdcall;
  {$EXTERNALSYM getprotobyname}
function getprotobyname(const name: PAnsiChar): PProtoEnt; stdcall;
  {$EXTERNALSYM WSAStartup}
function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Integer; stdcall;
  {$EXTERNALSYM WSACleanup}
function WSACleanup: Integer; stdcall;
  {$EXTERNALSYM WSASetLastError}
procedure WSASetLastError(iError: Integer); stdcall;
  {$EXTERNALSYM WSAGetLastError}
function WSAGetLastError: Integer; stdcall;
  {$EXTERNALSYM __WSAFDIsSet}
function __WSAFDIsSet(s: TSOcket; var FDSet: TFDSet): Bool; stdcall;


  {$EXTERNALSYM FD_CLR}
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
  {$EXTERNALSYM FD_ISSET}
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
  {$EXTERNALSYM FD_SET}
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
  {$EXTERNALSYM FD_ZERO}
procedure FD_ZERO(var FDSet: TFDSet);



// IPv6 definitions
type
  {$EXTERNALSYM IN_ADDR6}
	IN_ADDR6 = packed record
		s6_addr : array[0..15] of u_char; // IPv6 address
	end;
  {$EXTERNALSYM TIn6Addr}
  TIn6Addr   = IN_ADDR6;
  {$EXTERNALSYM PIn6Addr}
  PIn6Addr   = ^IN_ADDR6;
  {$EXTERNALSYM IN6_ADDR}
  IN6_ADDR   = IN_ADDR6;
  {$EXTERNALSYM PIN6_ADDR}
  PIN6_ADDR  = ^IN_ADDR6;
  {$EXTERNALSYM LPIN6_ADDR}
  LPIN6_ADDR = ^IN_ADDR6;

// Old IPv6 socket address structure (retained for sockaddr_gen definition below)
  {$EXTERNALSYM SOCKADDR_IN6_OLD}
	SOCKADDR_IN6_OLD = packed record
		sin6_family   : Smallint;         // AF_INET6
		sin6_port     : u_short;          // Transport level port number
		sin6_flowinfo : u_long;           // IPv6 flow information
		sin6_addr     : IN_ADDR6;         // IPv6 address
	end;

// IPv6 socket address structure, RFC 2553
  {$EXTERNALSYM SOCKADDR_IN6}
	SOCKADDR_IN6 = packed record
		sin6_family   : Smallint;         // AF_INET6
		sin6_port     : u_short;          // Transport level port number
		sin6_flowinfo : u_long;           // IPv6 flow information
		sin6_addr     : IN_ADDR6;         // IPv6 address
		sin6_scope_id : u_long;           // set of interfaces for a scope
	end;
  {$EXTERNALSYM TSockAddrIn6}
  TSockAddrIn6   = SOCKADDR_IN6;
  {$EXTERNALSYM PSockAddrIn6}
  PSockAddrIn6   = ^SOCKADDR_IN6;
  {$EXTERNALSYM PSOCKADDR_IN6}
  PSOCKADDR_IN6  = ^SOCKADDR_IN6;
  {$EXTERNALSYM LPSOCKADDR_IN6}
  LPSOCKADDR_IN6 = ^SOCKADDR_IN6;

  {$EXTERNALSYM sockaddr_gen}
	sockaddr_gen = packed record
		case Integer of
		1 : ( Address : SOCKADDR; );
		2 : ( AddressIn : SOCKADDR_IN; );
		3 : ( AddressIn6 : SOCKADDR_IN6_OLD; );
	end;

// Structure to keep interface specific information
  {$EXTERNALSYM INTERFACE_INFO}
	INTERFACE_INFO = packed record
		iiFlags            : u_long;       // Interface flags
		iiAddress          : sockaddr_gen; // Interface address
		iiBroadcastAddress : sockaddr_gen; // Broadcast address
		iiNetmask          : sockaddr_gen; // Network mask
	end;
  {$EXTERNALSYM TINTERFACE_INFO}
	TINTERFACE_INFO  = INTERFACE_INFO;
  {$EXTERNALSYM LPINTERFACE_INFO}
	LPINTERFACE_INFO = ^INTERFACE_INFO;

// New structure that does not have dependency on the address size
  {$EXTERNALSYM INTERFACE_INFO_EX}
	INTERFACE_INFO_EX = packed record
		iiFlags            : u_long;         // Interface flags
		iiAddress          : SOCKET_ADDRESS; // Interface address
		iiBroadcastAddress : SOCKET_ADDRESS; // Broadcast address
		iiNetmask : SOCKET_ADDRESS;          // Network mask
	end;
  {$EXTERNALSYM TINTERFACE_INFO_EX}
	TINTERFACE_INFO_EX  = INTERFACE_INFO_EX;
  {$EXTERNALSYM LPINTERFACE_INFO_EX}
	LPINTERFACE_INFO_EX = ^INTERFACE_INFO_EX;

implementation

function accept;  external WINSOCK2_DLL name 'accept';
function bind;  external WINSOCK2_DLL name 'bind';
function closesocket; external WINSOCK2_DLL name 'closesocket';
function connect; external WINSOCK2_DLL name 'connect';
function ioctlsocket; external WINSOCK2_DLL name 'ioctlsocket';
function getpeername; external WINSOCK2_DLL name 'getpeername';
function getsockname; external WINSOCK2_DLL name 'getsockname';
function getsockopt;  external WINSOCK2_DLL name 'getsockopt';
function htonl; external WINSOCK2_DLL name 'htonl';
function htons; external WINSOCK2_DLL name 'htons';
function inet_addr; external WINSOCK2_DLL name 'inet_addr';
function inet_ntoa; external WINSOCK2_DLL name 'inet_ntoa';
function listen;  external WINSOCK2_DLL name 'listen';
function ntohl; external WINSOCK2_DLL name 'ntohl';
function ntohs; external WINSOCK2_DLL name 'ntohs';
function recv;  external WINSOCK2_DLL name 'recv';
function recvfrom;  external WINSOCK2_DLL name 'recvfrom';
function select;  external WINSOCK2_DLL name 'select';
function send;  external WINSOCK2_DLL name 'send';
function sendto;  external WINSOCK2_DLL name 'sendto';
function setsockopt;  external WINSOCK2_DLL name 'setsockopt';
function shutdown;  external WINSOCK2_DLL name 'shutdown';
function socket;  external WINSOCK2_DLL name 'socket';
function gethostbyaddr; external WINSOCK2_DLL name 'gethostbyaddr';
function gethostbyname; external WINSOCK2_DLL name 'gethostbyname';
function gethostname; external WINSOCK2_DLL name 'gethostname';
function getservbyport; external WINSOCK2_DLL name 'getservbyport';
function getservbyname; external WINSOCK2_DLL name 'getservbyname';
function getprotobynumber;  external WINSOCK2_DLL name 'getprotobynumber';
function getprotobyname;  external WINSOCK2_DLL name 'getprotobyname';
function WSAStartup;  external WINSOCK2_DLL name 'WSAStartup';
function WSACleanup;  external WINSOCK2_DLL name 'WSACleanup';
procedure WSASetLastError;  external WINSOCK2_DLL name 'WSASetLastError';
function WSAGetLastError; external WINSOCK2_DLL name 'WSAGetLastError';
function __WSAFDIsSet;  external WINSOCK2_DLL name '__WSAFDIsSet';

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var i: DWORD;
begin
  i := 0;
  while i < FDSet.fd_count do
  begin
    if FDSet.fd_array[i] = Socket then
    begin
      while i < FDSet.fd_count - 1 do
      begin
        FDSet.fd_array[i] := FDSet.fd_array[i+1];
        Inc(i);
      end;
      Dec(FDSet.fd_count);
      Break;
    end;
    Inc(i);
  end;
end;

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
begin
  Result := __WSAFDIsSet(Socket, FDSet);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
  if FDSet.fd_count < FD_SETSIZE then
  begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;

end.
