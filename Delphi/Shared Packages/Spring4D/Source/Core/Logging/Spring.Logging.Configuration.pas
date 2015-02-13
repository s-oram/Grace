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

unit Spring.Logging.Configuration;

interface

uses
  SysUtils,
  TypInfo,
  Classes,
  Rtti,
  Spring,
  Spring.Collections,
  Spring.Container;

type
  {$REGION 'TLoggingConfiguration'}
  TLoggingConfiguration = class
  private
    fTypes: IDictionary<PTypeInfo, string>;
  public
    constructor Create;

    procedure RegisterLogger(typeInfo: PTypeInfo; const name: string); overload;
    procedure RegisterLogger<T>(const name: string); overload; inline;

    function HasLogger(typeInfo: PTypeInfo): Boolean;
    function GetLogger(typeInfo: PTypeInfo): string;

    class procedure LoadFromStrings(const container: TContainer;
      const strings: TStrings);
    class procedure LoadFromString(const container: TContainer;
      const str: string);
    class procedure LoadFromStream(const container: TContainer;
      const stream: TStream; const encoding: TEncoding = nil);
    class procedure LoadFromFile(const container: TContainer;
      const fileName: string; const encoding: TEncoding = nil);
  end;
  {$ENDREGION}


implementation

uses
  IniFiles,
  RTLConsts,
  Spring.Reflection,
  Spring.Container.Common,
  Spring.Container.Registration,
  Spring.Logging,
  Spring.Logging.Extensions,
  Spring.Logging.Controller,
  Spring.Logging.Appenders,
  Spring.Logging.Loggers,
  Spring.Logging.ResourceStrings,
  Spring.Logging.Container;

type
  TRegisterProc = reference to procedure (const reg: IRegistration;
    const name: string);
  TPropertyFunc = reference to function (const reg: IRegistration;
    const serviceName, name, value: string; var ctx: Boolean): Boolean;
  TPropertiesReadProc = reference to procedure (const reg: IRegistration;
    const ctx: Boolean);


{$REGION 'TConfigurationReader'}
  TConfigurationReader = class
  private const //Do not localize
    SAppenders = 'appenders';
    SControllers = 'controllers';
    SAppender = 'appender';
    SSerializer = 'serializer';
    SController = 'controller';
    SAssign = 'assign';
    SLoggers = 'loggers';
    SDefault = 'default';
    SClass = 'class';
    /// <summary>
    ///   Prefix for all names so we don't minimize posibility of collision
    ///   with any user defined services.
    /// </summary>
    SPrefix = 'logging.';
    /// <summary>
    ///   Since controllers may act as appenders we have to separate the two
    ///   by appending some text to its name.
    /// </summary>
    SControllerSuffix = '.controller';
    /// <summary>
    ///   Since controllers may act as appenders we have to separate the two
    ///   by appending some text to its name.
    /// </summary>
    SAppenderSuffix = '.appender';
    SAddAppenderProc = 'AddAppender';
    SAddSerializerProc = 'AddSerializer';
    SControllerField = 'fController';
  private
    fContainer: TContainer;
    fIni: TCustomIniFile;
    fAppenders: IList<string>;

    function GetType(const className: string; needInstance: Boolean): TRttiType;
    function AsValue(const valueType: TRttiType; const value: string): TValue;
    /// <summary>
    ///   Registers new serializer and returns its service name or just return
    ///   the name if this serializer was already registered before.
    /// </summary>
    function RegisterSerializer(const typeName: string): string;

    /// <summary>
    ///   Reads all subsections of sectionName and calls registerProc on each
    ///   of the subsections, then it reads all the properties and calls
    ///   propertyFunc on each of them, it may do it own processing and return
    ///   true, if false is returned property is read instead. After all
    ///   properties are read propertiesRead function is called and may be used
    ///   to finish the registration based on context. The method returns
    ///   true if any subsections are read or false otherwise.
    /// </summary>
    function ReadSection(const sectionName: string;
      const defaultType: TRttiType; const registerProc: TRegisterProc;
      const propertyFunc: TPropertyFunc = nil;
      const propertiesRead: TPropertiesReadProc = nil): Boolean;

    procedure ReadAppenders;
    procedure ReadControllers;
    procedure ReadLoggers(const configuration: TLoggingConfiguration);
  public
    constructor Create(const container: TContainer; const ini: TCustomIniFile);

    procedure Read(const configuration: TLoggingConfiguration);
  end;
{$ENDREGION}


{$REGION 'TLoggingConfiguration'}

constructor TLoggingConfiguration.Create;
begin
  inherited;
  fTypes := TCollections.CreateDictionary<PTypeInfo, string>;
end;

function TLoggingConfiguration.GetLogger(typeInfo: PTypeInfo): string;
begin
  Result := fTypes[typeInfo];
end;

function TLoggingConfiguration.HasLogger(typeInfo: PTypeInfo): Boolean;
begin
  Result := fTypes.ContainsKey(typeInfo);
end;

class procedure TLoggingConfiguration.LoadFromFile(const container: TContainer;
  const fileName: string; const encoding: TEncoding = nil);
var
  stream: TStream;
begin
  Guard.CheckNotNull(container, 'container');

  stream := TFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(container, stream, encoding);
  finally
    stream.Free;
  end;
end;

class procedure TLoggingConfiguration.LoadFromStream(const container: TContainer;
  const stream: TStream; const encoding: TEncoding = nil);
var
  strings: TStrings;
begin
  Guard.CheckNotNull(container, 'container');
  Guard.CheckNotNull(stream, 'stream');

  strings := TStringList.Create;
  try
    strings.LoadFromStream(stream, encoding);
    LoadFromStrings(container, strings);
  finally
    strings.Free;
  end;
end;

class procedure TLoggingConfiguration.LoadFromString(
  const container: TContainer; const str: string);
var
  strings: TStrings;
begin
  Guard.CheckNotNull(container, 'container');

  strings := TStringList.Create;
  try
    strings.Text := str;
    LoadFromStrings(container, strings);
  finally
    strings.Free;
  end;
end;

class procedure TLoggingConfiguration.LoadFromStrings(const container: TContainer;
  const strings: TStrings);
var
  ini: TMemIniFile;
  configuration: TLoggingConfiguration;
  reader: TConfigurationReader;
begin
  Guard.CheckNotNull(container, 'container');
  Guard.CheckNotNull(strings, 'strings');
  if container.Kernel.Registry.HasService(TypeInfo(TLoggingConfiguration)) then
    raise ERegistrationException.CreateRes(@SLogConfigurationAlreadyRegistered);
  container.Kernel.Resolver.AddSubResolver(
    TLoggerResolver.Create(container.Kernel));

  ini := TMemIniFile.Create('');
  try
    ini.SetStrings(strings);

    //Register and build the configuration so we can be sure container will
    //properly release it
    container.RegisterType<TLoggingConfiguration>.AsSingleton.AsDefault;
    container.Kernel.Builder.Build(container.Kernel.Registry.FindOne(
      TypeInfo(TLoggingConfiguration)));
    configuration := container.Resolve<TLoggingConfiguration>;

    reader := TConfigurationReader.Create(container, ini);
    try
      reader.Read(configuration);
    finally
      reader.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TLoggingConfiguration.RegisterLogger(typeInfo: PTypeInfo;
  const name: string);
begin
  Guard.CheckNotNull(name <> '', 'name');
  Guard.CheckTypeKind(typeInfo, [tkClass, tkRecord], 'T');
  fTypes.Add(typeInfo, name);
end;

procedure TLoggingConfiguration.RegisterLogger<T>(const name: string);
begin
  RegisterLogger(TypeInfo(T), name);
end;
{$ENDREGION}


{$REGION 'TConfigurationReader'}

constructor TConfigurationReader.Create(const container: TContainer;
  const ini: TCustomIniFile);
begin
  fContainer := container;
  fIni := ini;
end;

function TConfigurationReader.AsValue(const valueType: TRttiType;
  const value: string): TValue;
begin
  case valueType.TypeKind of
    tkUString:
      Result := value;
    tkInteger:
      Result := StrToInt(value);
    tkEnumeration:
      if valueType.Handle = TypeInfo(Boolean) then
        Result := StrToBool(value)
      else TValue.Make(GetEnumValue(valueType.Handle, value), valueType.Handle,
        Result);
    tkSet:
      TValue.Make(StringToSet(valueType.Handle, value), valueType.Handle, Result);

    else raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType,
      [valueType.Name]);
  end;
end;

function TConfigurationReader.GetType(const className: string;
  needInstance: Boolean): TRttiType;
begin
  Guard.CheckNotNull(className <> '', SClass);
  //Both fully qualified and just class name supported
  TRttiType(Result) := TType.FindType(className);
  if (Result = nil) or (needInstance and (not Result.IsInstance)) then
    raise EClassNotFound.CreateResFmt(@SClassNotFound, [className]);
end;

procedure TConfigurationReader.Read(const configuration: TLoggingConfiguration);
begin
  ReadAppenders;
  ReadControllers;
  ReadLoggers(configuration);
end;

procedure TConfigurationReader.ReadAppenders;
begin
  ReadSection(SAppenders, nil,
    procedure (const reg: IRegistration; const name: string)
    begin
      if fAppenders = nil then
        fAppenders := TCollections.CreateList<string>;

      reg.Implements(TypeInfo(ILogAppender), SPrefix + name + SAppenderSuffix);
      fAppenders.Add(name);
    end);
end;

procedure TConfigurationReader.ReadControllers;
var
  result: Boolean;
  reg: TRegistration<TLoggerController>;
  s: string;
begin
  result := ReadSection(SControllers, TType.GetType(TLoggerController),
    procedure (const reg: IRegistration; const name: string)
    begin
      reg.InjectConstructor;
      reg.Implements(TypeInfo(ILogAppender), SPrefix + name + SAppenderSuffix);
      reg.Implements(TypeInfo(ILoggerController), SPrefix + name + SControllerSuffix);
    end,

    function (const reg: IRegistration; const serviceName, name, value: string;
      var ctx: Boolean): Boolean
    begin
      if SameText(SAppender, name) then
      begin
        Result := True;
        reg.InjectMethod(SAddAppenderProc, [SPrefix + value + SAppenderSuffix]);
      end else
      if SameText(SSerializer, name) then
      begin
        Result := True;
        reg.InjectMethod(SAddSerializerProc, [RegisterSerializer(value)]);
      end
      else
        Result := False;
    end);

  if not result then
  begin
    reg := fContainer.RegisterType<TLoggerController>;
    reg.InjectConstructor;
    reg.Implements<ILoggerController>.AsDefault;
    if Assigned(fAppenders) then
      for s in fAppenders do
        reg.InjectMethod(SAddAppenderProc, [SPrefix + s + SAppenderSuffix])
  end;
end;

procedure TConfigurationReader.ReadLoggers(
  const configuration: TLoggingConfiguration);
var
  result: Boolean;
begin
  result := ReadSection(SLoggers, TType.GetType(TLogger),
    procedure (const reg: IRegistration; const name: string)
    begin
      reg.Implements(TypeInfo(ILogger), SPrefix + name);
    end,

    function (const reg: IRegistration; const serviceName, name, value: string;
      var ctx: Boolean): Boolean
    begin
      if SameText(SController, name) then
      begin
        Result := True;
        ctx := True;
        reg.InjectField(SControllerField, SPrefix + value + SControllerSuffix);
      end else
      if SameText(SAssign, name) then
      begin
        Result := True;
        configuration.RegisterLogger(GetType(value, False).Handle,
          SPrefix + serviceName);
      end
      else
        Result := False;
    end,

    procedure (const reg: IRegistration; const ctx: Boolean)
    begin
      //If controller was not seen inject the default one
      if not ctx then
        reg.InjectField(SControllerField);
    end);

  if not result then
  begin
    fContainer.RegisterType<TLogger>.Implements<ILogger>.AsDefault
      .InjectField(SControllerField);
  end;
end;

function TConfigurationReader.ReadSection(const sectionName: string;
  const defaultType: TRttiType; const registerProc: TRegisterProc;
  const propertyFunc: TPropertyFunc = nil;
  const propertiesRead: TPropertiesReadProc = nil): Boolean;
var
  str:  TStrings;
  values: TStrings;
  section: string;
  name: string;
  s: string;
  value: string;
  classType: TRttiType;
  prop: TRttiProperty;
  reg: IRegistration;
  i: Integer;
  ctx: Boolean;
begin
  Assert(Assigned(registerProc));
  str := TStringList.Create;
  try
    fIni.ReadSubSections(sectionName, str, False);
    Result := str.Count > 0;
    for name in str do
    begin
      section := sectionName + '\' + name;
      s := fIni.ReadString(section, SClass, '');
      if (s = '') and (defaultType <> nil) then
        classType := defaultType
      else classType := GetType(s, True); //Will raise an exception for empty string

      reg := fContainer.RegisterType(classType.Handle);
      registerProc(reg, name);
      reg.AsSingleton;
      if SameText(SDefault, name) then
        reg.AsDefault;

      values := TStringList.Create;
      try
        ctx := False;
        fIni.ReadSectionValues(section, values);
        for i := 0 to values.Count - 1 do
        begin
          s := values.Names[i];
          if SameText(SClass, s) then
            Continue;

          value := values.ValueFromIndex[i];
          if Assigned(propertyFunc) and propertyFunc(reg, name, s, value, ctx) then
            Continue;

          prop := classType.GetProperty(s);
          if prop = nil then
            raise EPropertyError.CreateResFmt(@SUnknownProperty,
              [classType.Name + '.' + s]);

          reg.InjectProperty(s, AsValue(prop.PropertyType, value));
        end;

        if Assigned(propertiesRead) then
          propertiesRead(reg, ctx);

      finally
        values.Free;
      end;
    end;
  finally
    str.Free;
  end;
end;

function TConfigurationReader.RegisterSerializer(
  const typeName: string): string;
var
  classType: TRttiType;
begin
  classType := GetType(typeName, True);
  Result := classType.QualifiedName;

  if fContainer.Kernel.Registry.FindOne(classType.Handle) = nil then
    fContainer.RegisterType(classType.Handle).AsSingleton
      .Implements(TypeInfo(ITypeSerializer), Result);
end;
{$ENDREGION}


procedure DummyRegister(const classes: array of TClass);
begin
  //Does nothing but the classes are already referenced which forces RTTI
  //generation (linker doesn't strip them)
end;

initialization
  DummyRegister([TTextLogAppender]);

end.
