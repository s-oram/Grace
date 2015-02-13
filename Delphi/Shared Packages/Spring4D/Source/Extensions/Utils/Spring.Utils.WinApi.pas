{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

///	<summary>
///	  Declares some Windows API for the framework.
///	</summary>
unit Spring.Utils.WinAPI; // platform;

interface

{$IFDEF MSWINDOWS}
uses
  Windows, WinSvc;

const
  VER_NT_WORKSTATION       = $0000001;
  {$EXTERNALSYM VER_NT_WORKSTATION}
  VER_NT_DOMAIN_CONTROLLER = $0000002;
  {$EXTERNALSYM VER_NT_DOMAIN_CONTROLLER}
  VER_NT_SERVER            = $0000003;
  {$EXTERNALSYM VER_NT_SERVER}

const
  VER_SERVER_NT                      = DWORD($80000000);
  {$EXTERNALSYM VER_SERVER_NT}
  VER_WORKSTATION_NT                 = $40000000;
  {$EXTERNALSYM VER_WORKSTATION_NT}
  VER_SUITE_SMALLBUSINESS            = $00000001;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS}
  VER_SUITE_ENTERPRISE               = $00000002;
  {$EXTERNALSYM VER_SUITE_ENTERPRISE}
  VER_SUITE_BACKOFFICE               = $00000004;
  {$EXTERNALSYM VER_SUITE_BACKOFFICE}
  VER_SUITE_COMMUNICATIONS           = $00000008;
  {$EXTERNALSYM VER_SUITE_COMMUNICATIONS}
  VER_SUITE_TERMINAL                 = $00000010;
  {$EXTERNALSYM VER_SUITE_TERMINAL}
  VER_SUITE_SMALLBUSINESS_RESTRICTED = $00000020;
  {$EXTERNALSYM VER_SUITE_SMALLBUSINESS_RESTRICTED}
  VER_SUITE_EMBEDDEDNT               = $00000040;
  {$EXTERNALSYM VER_SUITE_EMBEDDEDNT}
  VER_SUITE_DATACENTER               = $00000080;
  {$EXTERNALSYM VER_SUITE_DATACENTER}
  VER_SUITE_SINGLEUSERTS             = $00000100;
  {$EXTERNALSYM VER_SUITE_SINGLEUSERTS}
  VER_SUITE_PERSONAL                 = $00000200;
  {$EXTERNALSYM VER_SUITE_PERSONAL}
  VER_SUITE_BLADE                    = $00000400;
  {$EXTERNALSYM VER_SUITE_BLADE}
  VER_SUITE_EMBEDDED_RESTRICTED      = $00000800;
  {$EXTERNALSYM VER_SUITE_EMBEDDED_RESTRICTED}
  VER_SUITE_SECURITY_APPLIANCE       = $00001000;
  {$EXTERNALSYM VER_SUITE_SECURITY_APPLIANCE}
  VER_SUITE_STORAGE_SERVER           = $00002000;
  {$EXTERNALSYM VER_SUITE_STORAGE_SERVER}
  VER_SUITE_COMPUTE_SERVER           = $00004000;
  {$EXTERNALSYM VER_SUITE_COMPUTE_SERVER}


{$IFDEF SUPPORTS_REGION}{$REGION 'IP Types'}{$ENDIF}

const
  iphlpapilib = 'iphlpapi.dll';

const
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128; // arb.
  {$EXTERNALSYM MAX_ADAPTER_DESCRIPTION_LENGTH}
  MAX_ADAPTER_NAME_LENGTH        = 256; // arb.
  {$EXTERNALSYM MAX_ADAPTER_NAME_LENGTH}
  MAX_ADAPTER_ADDRESS_LENGTH     = 8; // arb.
  {$EXTERNALSYM MAX_ADAPTER_ADDRESS_LENGTH}
  DEFAULT_MINIMUM_ENTITIES       = 32; // arb.
  {$EXTERNALSYM DEFAULT_MINIMUM_ENTITIES}
  MAX_HOSTNAME_LEN               = 128; // arb.
  {$EXTERNALSYM MAX_HOSTNAME_LEN}
  MAX_DOMAIN_NAME_LEN            = 128; // arb.
  {$EXTERNALSYM MAX_DOMAIN_NAME_LEN}
  MAX_SCOPE_ID_LEN               = 256; // arb.
  {$EXTERNALSYM MAX_SCOPE_ID_LEN}

type
  time_t = Longint;
  {$EXTERNALSYM time_t}

  PIP_MASK_STRING = ^IP_MASK_STRING;
  {$EXTERNALSYM PIP_MASK_STRING}
  IP_ADDRESS_STRING = record
    S: array [0..15] of AnsiChar;
  end;
  {$EXTERNALSYM IP_ADDRESS_STRING}
  PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
  {$EXTERNALSYM PIP_ADDRESS_STRING}
  IP_MASK_STRING = IP_ADDRESS_STRING;
  {$EXTERNALSYM IP_MASK_STRING}
  TIpAddressString = IP_ADDRESS_STRING;
  PIpAddressString = PIP_MASK_STRING;

  PIP_ADDR_STRING = ^IP_ADDR_STRING;
  {$EXTERNALSYM PIP_ADDR_STRING}
  _IP_ADDR_STRING = record
    Next: PIP_ADDR_STRING;
    IpAddress: IP_ADDRESS_STRING;
    IpMask: IP_MASK_STRING;
    Context: DWORD;
  end;
  {$EXTERNALSYM _IP_ADDR_STRING}
  IP_ADDR_STRING = _IP_ADDR_STRING;
  {$EXTERNALSYM IP_ADDR_STRING}
  TIpAddrString = IP_ADDR_STRING;
  PIpAddrString = PIP_ADDR_STRING;

  PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
  {$EXTERNALSYM PIP_ADAPTER_INFO}
  _IP_ADAPTER_INFO = record
    Next: PIP_ADAPTER_INFO;
    ComboIndex: DWORD;
    AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of AnsiChar;
    Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of AnsiChar;
    AddressLength: UINT;
    Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
    Index: DWORD;
    Type_: UINT;
    DhcpEnabled: UINT;
    CurrentIpAddress: PIP_ADDR_STRING;
    IpAddressList: IP_ADDR_STRING;
    GatewayList: IP_ADDR_STRING;
    DhcpServer: IP_ADDR_STRING;
    HaveWins: BOOL;
    PrimaryWinsServer: IP_ADDR_STRING;
    SecondaryWinsServer: IP_ADDR_STRING;
    LeaseObtained: time_t;
    LeaseExpires: time_t;
  end;
  {$EXTERNALSYM _IP_ADAPTER_INFO}
  IP_ADAPTER_INFO = _IP_ADAPTER_INFO;
  {$EXTERNALSYM IP_ADAPTER_INFO}
  TIpAdapterInfo = IP_ADAPTER_INFO;
  PIpAdapterInfo = PIP_ADAPTER_INFO;

function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: ULONG): DWORD; stdcall;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'Service Control'}{$ENDIF}

const

//
// Service flags for QueryServiceStatusEx
//

  SERVICE_RUNS_IN_SYSTEM_PROCESS = $00000001;
  {$EXTERNALSYM SERVICE_RUNS_IN_SYSTEM_PROCESS}

//
// Info levels for ChangeServiceConfig2 and QueryServiceConfig2
//

  SERVICE_CONFIG_DESCRIPTION     = 1;
  {$EXTERNALSYM SERVICE_CONFIG_DESCRIPTION}
  SERVICE_CONFIG_FAILURE_ACTIONS = 2;
  {$EXTERNALSYM SERVICE_CONFIG_FAILURE_ACTIONS}

//
// Service description string
//

type
  LPSERVICE_DESCRIPTIONA = ^SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM LPSERVICE_DESCRIPTIONA}
  _SERVICE_DESCRIPTIONA = record
    lpDescription: LPSTR;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONA}
  SERVICE_DESCRIPTIONA = _SERVICE_DESCRIPTIONA;
  {$EXTERNALSYM SERVICE_DESCRIPTIONA}
  TServiceDescriptionA = SERVICE_DESCRIPTIONA;
  PServiceDescriptionA = LPSERVICE_DESCRIPTIONA;

//
// Service description string
//

  LPSERVICE_DESCRIPTIONW = ^SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM LPSERVICE_DESCRIPTIONW}
  _SERVICE_DESCRIPTIONW = record
    lpDescription: LPWSTR;
  end;
  {$EXTERNALSYM _SERVICE_DESCRIPTIONW}
  SERVICE_DESCRIPTIONW = _SERVICE_DESCRIPTIONW;
  {$EXTERNALSYM SERVICE_DESCRIPTIONW}
  TServiceDescriptionW = SERVICE_DESCRIPTIONW;
  PServiceDescriptionW = LPSERVICE_DESCRIPTIONW;

  {$IFDEF UNICODE}
    SERVICE_DESCRIPTION = SERVICE_DESCRIPTIONW;
    {$EXTERNALSYM SERVICE_DESCRIPTION}
    LPSERVICE_DESCRIPTION = LPSERVICE_DESCRIPTIONW;
    {$EXTERNALSYM LPSERVICE_DESCRIPTION}
    TServiceDescription = TServiceDescriptionW;
    PServiceDescription = PServiceDescriptionW;
  {$ELSE}
    SERVICE_DESCRIPTION = SERVICE_DESCRIPTIONA;
    {$EXTERNALSYM SERVICE_DESCRIPTION}
    LPSERVICE_DESCRIPTION = LPSERVICE_DESCRIPTIONA;
    {$EXTERNALSYM LPSERVICE_DESCRIPTION}
    TServiceDescription = TServiceDescriptionA;
    PServiceDescription = PServiceDescriptionA;
  {$ENDIF UNICODE}

//
// Service Status Structures
//

  LPSERVICE_STATUS_PROCESS = ^SERVICE_STATUS_PROCESS;
  {$EXTERNALSYM LPSERVICE_STATUS_PROCESS}
  _SERVICE_STATUS_PROCESS = record
    dwServiceType: DWORD;
    dwCurrentState: DWORD;
    dwControlsAccepted: DWORD;
    dwWin32ExitCode: DWORD;
    dwServiceSpecificExitCode: DWORD;
    dwCheckPoint: DWORD;
    dwWaitHint: DWORD;
    dwProcessId: DWORD;
    dwServiceFlags: DWORD;
  end;
  {$EXTERNALSYM _SERVICE_STATUS_PROCESS}
  SERVICE_STATUS_PROCESS = _SERVICE_STATUS_PROCESS;
  {$EXTERNALSYM SERVICE_STATUS_PROCESS}
  TServiceStatusProcess = SERVICE_STATUS_PROCESS;
  PServiceStatusProcess = LPSERVICE_STATUS_PROCESS;

//
// Info levels for EnumServicesStatusEx
//

  _SC_ENUM_TYPE = (SC_ENUM_PROCESS_INFO);
  {$EXTERNALSYM _SC_ENUM_TYPE}
  SC_ENUM_TYPE = _SC_ENUM_TYPE;
  {$EXTERNALSYM SC_ENUM_TYPE}
  LPENUM_SERVICE_STATUS_PROCESSA = ^ENUM_SERVICE_STATUS_PROCESSA;
  {$EXTERNALSYM LPENUM_SERVICE_STATUS_PROCESSA}
  _ENUM_SERVICE_STATUS_PROCESSA = record
    lpServiceName: LPSTR;
    lpDisplayName: LPSTR;
    ServiceStatusProcess: SERVICE_STATUS_PROCESS;
  end;
  {$EXTERNALSYM _ENUM_SERVICE_STATUS_PROCESSA}
  ENUM_SERVICE_STATUS_PROCESSA = _ENUM_SERVICE_STATUS_PROCESSA;
  {$EXTERNALSYM ENUM_SERVICE_STATUS_PROCESSA}
  TEnumServiceStatusProcessA = ENUM_SERVICE_STATUS_PROCESSA;
  PEnumServiceStatusProcessA = LPENUM_SERVICE_STATUS_PROCESSA;

  LPENUM_SERVICE_STATUS_PROCESSW = ^ENUM_SERVICE_STATUS_PROCESSW;
  {$EXTERNALSYM LPENUM_SERVICE_STATUS_PROCESSW}
  _ENUM_SERVICE_STATUS_PROCESSW = record
    lpServiceName: LPWSTR;
    lpDisplayName: LPWSTR;
    ServiceStatusProcess: SERVICE_STATUS_PROCESS;
  end;
  {$EXTERNALSYM _ENUM_SERVICE_STATUS_PROCESSW}
  ENUM_SERVICE_STATUS_PROCESSW = _ENUM_SERVICE_STATUS_PROCESSW;
  {$EXTERNALSYM ENUM_SERVICE_STATUS_PROCESSW}
  TEnumServiceStatusProcessW = ENUM_SERVICE_STATUS_PROCESSW;
  PEnumServiceStatusProcessW = LPENUM_SERVICE_STATUS_PROCESSW;

  {$IFDEF UNICODE}
    ENUM_SERVICE_STATUS_PROCESS = ENUM_SERVICE_STATUS_PROCESSW;
    {$EXTERNALSYM ENUM_SERVICE_STATUS_PROCESS}
    LPENUM_SERVICE_STATUS_PROCESS = LPENUM_SERVICE_STATUS_PROCESSW;
    {$EXTERNALSYM LPENUM_SERVICE_STATUS_PROCESS}
    TEnumServiceStatusProcess = TEnumServiceStatusProcessW;
    PEnumServiceStatusProcess = PEnumServiceStatusProcessW;
  {$ELSE}
    ENUM_SERVICE_STATUS_PROCESS = ENUM_SERVICE_STATUS_PROCESSA;
    {$EXTERNALSYM ENUM_SERVICE_STATUS_PROCESS}
    LPENUM_SERVICE_STATUS_PROCESS = LPENUM_SERVICE_STATUS_PROCESSA;
    {$EXTERNALSYM LPENUM_SERVICE_STATUS_PROCESS}
    TEnumServiceStatusProcess = TEnumServiceStatusProcessA;
    PEnumServiceStatusProcess = PEnumServiceStatusProcessA;
  {$ENDIF UNICODE}

function QueryServiceConfig2A(hService: SC_HANDLE; dwInfoLevel: DWORD;
  lpBuffer: LPBYTE; cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryServiceConfig2A}
function QueryServiceConfig2W(hService: SC_HANDLE; dwInfoLevel: DWORD;
  lpBuffer: LPBYTE; cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryServiceConfig2W}
function QueryServiceConfig2(hService: SC_HANDLE; dwInfoLevel: DWORD;
  lpBuffer: LPBYTE; cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;
{$EXTERNALSYM QueryServiceConfig2}

function EnumServicesStatusExA(hSCManager: SC_HANDLE; InfoLevel: SC_ENUM_TYPE;
  dwServiceType: DWORD; dwServiceState: DWORD; lpServices: LPBYTE;
  cbBufSize: DWORD; var pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  pszGroupName: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM EnumServicesStatusExA}
function EnumServicesStatusExW(hSCManager: SC_HANDLE; InfoLevel: SC_ENUM_TYPE;
  dwServiceType: DWORD; dwServiceState: DWORD; lpServices: LPBYTE;
  cbBufSize: DWORD; var pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  pszGroupName: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM EnumServicesStatusExW}
function EnumServicesStatusEx(hSCManager: SC_HANDLE; InfoLevel: SC_ENUM_TYPE;
  dwServiceType: DWORD; dwServiceState: DWORD; lpServices: LPBYTE;
  cbBufSize: DWORD; var pcbBytesNeeded, lpServicesReturned, lpResumeHandle: DWORD;
  pszGroupName: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM EnumServicesStatusEx}

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

function ConvertSidToStringSid(sid: PSID; var stringSid: LPWSTR): BOOL; stdcall;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}
const
  AWSuffix = {$IFDEF UNICODE}'W'{$ELSE}'A'{$ENDIF};

function GetAdaptersInfo; external iphlpapilib name 'GetAdaptersInfo';

function QueryServiceConfig2A; external advapi32 name 'QueryServiceConfig2A';
function QueryServiceConfig2W; external advapi32 name 'QueryServiceConfig2W';
function QueryServiceConfig2; external advapi32 name 'QueryServiceConfig2' + AWSuffix;

function EnumServicesStatusExA; external advapi32 name 'EnumServicesStatusExA';
function EnumServicesStatusExW; external advapi32 name 'EnumServicesStatusExW';
function EnumServicesStatusEx; external advapi32 name 'EnumServicesStatusEx' + AWSuffix;

function ConvertSidToStringSid; external advapi32 name 'ConvertSidToStringSidW';
{$ENDIF}

end.
