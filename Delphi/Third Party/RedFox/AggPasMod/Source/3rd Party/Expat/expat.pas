unit Expat;

// ----------------------------------------------------------------------------
// Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
// milan@marusinec.sk
// http://www.pasports.org/pasexpat
// Copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// [Pascal Port History] -----------------------------------------------------
//
// 01.05.2006-Milano: Unit port establishment
// 07.06.2006-Milano: porting
//

interface

uses
  ExpatBasics,
  ExpatExternal,
  Xmltok,
  Xmlrole;

{$I ExpatMode.inc}

type
  TXmlParser = ^TXmlParserStruct;

  TXmlBool = Int8u;

  { The TXmlStatus enum gives the possible return values for several API functions. }
  TXmlStatus = (xsError, xsOK, xsSuspended);

  TXmlError = (xeNone, xeNoMemory, xeSyntax, xeNoElements, xeInvalidToken,
    xeUnclosedToken, xePartialChar, xeTagMismatch, xeDuplicateAttribute,
    xeJunkAfterDocElement, xeParamEntityRef, xeUndefinedEntity,
    xeRecursiveEntityRef, xeAsyncEntity, xeBadCharRef, xeBinaryEntityRef,
    xeAttributeExternalEntityRef, xeMisplacedXmlProcessingInstruction,
    xeUnknownEncoding, xeIncorrectEncoding, xeUnclosedCDataSection,
    xeExternalEntityHandling, xeNotStandalone, xeUnexpectedState,
    xeEntityDeclaredInPe, xeFeatureRequiresXmlDtd,
    xeCantChangeFeatureOnceParsing, xeUnboundPrefix,
    { Added in 1.95.8. }
    xeUndeclaringPrefix, xeIncompletePe, xeXmlDecl, xeTextDecl, xePublicID,
    xeSuspended, xeNotSuspended, xeAborted, xeFinished, xeSuspendPe,
    { Added in 2.0. }
    xeReservedPrefixXml, xeReservedPrefixXmlNS,
    xeReservedNamespaceURI);

  TXmlContentType = (___SKIP_ZERO____, ctEmpty, ctAny, ctMixed, ctName,
    ctChoice, ctSEQ);

  TXmlContentQuant = (cqNone, cqOpt, cqRep, cqPlus);

  TXmlParamEntityParsing = (pepNever, pepUnlessStandalone, pepAlways);

  { If type = ctEmpty or ctAny, then quant will be
    cqNone, and the other fields will be zero or NULL.
    If type = ctMixed, then quant will be none or REP and
    numchildren will contain number of elements that may be mixed in
    and children point to an array of TXmlContent cells that will be
    all of ctName type with no quantification.

    If type = ctName, then the name points to the name, and
    the numchildren field will be zero and children will be NULL. The
    quant fields indicates any quantifiers placed on the name.

    CHOICE and SEQ will have name NULL, the number of children in
    numchildren and children will point, recursively, to an array
    of TXmlContent cells.

    The EMPTY, ANY, and MIXED types will only occur at top level. }
  PXmlContent = ^TXmlContent;
  TXmlContent = record
    ContentType: TXmlContentType;
    Quant: TXmlContentQuant;
    Name: PXmlChar;

    Numchildren: Cardinal;
    Children: PXmlContent;
  end;

  { This is called for an element declaration. See above for
    description of the model argument. It's the caller's responsibility
    to free model when finished with it. }
  TXmlElementDeclHandler = procedure(UserData: Pointer; Name: PXmlChar;
    Model: PXmlContent);

  { The Attlist declaration handler is called for *each* TAttribute. So
    a single Attlist declaration with multiple attributes declared will
    generate multiple calls to this handler. The "default" parameter
    may be NULL in the case of the "#IMPLIED" or "#REQUIRED"
    keyword. The "isrequired" parameter will be true and the default
    value will be NULL in the case of "#REQUIRED". If "isrequired" is
    true and default is non-NULL, then this is a "#FIXED" default. }
  TXmlAttlistDeclHandler = procedure(UserData: Pointer;
    ElName, AttributeName, AttributeType, Dflt: PXmlChar; IsRequired: Integer);

  { The XML declaration handler is called for *both* XML declarations
    and text declarations. The way to distinguish is that the version
    parameter will be NULL for text declarations. The encoding
    parameter may be NULL for XML declarations. The standalone
    parameter will be -1, 0, or 1 indicating respectively that there
    was no standalone parameter in the declaration, that it was given
    as no, or that it was given as yes. }
  TXmlXmlDeclHandler = procedure(UserData: Pointer;
    Version, Encoding: PXmlChar; Standalone: Integer);

  { This is called for TEntity declarations. The IsParameterEntity
    argument will be non-zero if the TEntity is a parameter TEntity, zero
    otherwise.

    For internal entities (<!TEntity foo "bar">), value will
    be non-NULL and systemId, publicID, and notationName will be NULL.
    The value string is NOT nul-terminated; the length is provided in
    the ValueLength argument. Since it is legal to have zero-length
    values, do not use this argument to test for internal entities.

    For external entities, value will be NULL and systemId will be
    non-NULL. The publicId argument will be NULL unless a public
    identifier was provided. The notationName argument will have a
    non-NULL value only for unparsed TEntity declarations.

    Note that IsParameterEntity can't be changed to XML_Bool, since
    that would break binary compatibility. }
  TXmlEntityDeclHandler = procedure(UserData: Pointer; EntityName: PXmlChar;
    IsParameterEntity: Integer; Value: PXmlChar; ValueLength: Integer;
    Base, SystemId, PublicId, NotationName: PXmlChar);

  { atts is array of name/value pairs, terminated by 0;
    names and values are 0 terminated. }
  TXmlStartElementHandler = procedure(UserData: Pointer; Name: PXmlChar;
    Atts: PPXmlChar);
  TXmlEndElementHandler = procedure(UserData: Pointer; Name: PXmlChar);

  { s is not 0 terminated. }
  TXmlCharacterDataHandler = procedure(UserData: Pointer; S: PXmlChar;
    Len: Integer);

  { target and data are 0 terminated }
  TXmlProcessingInstructionHandler = procedure(UserData: Pointer;
    Target, Data: PXmlChar);

  { data is 0 terminated }
  TXmlCommentHandler = procedure(UserData: Pointer; Data: PXmlChar);

  TXmlStartCdataSectionHandler = procedure(UserData: Pointer);
  TXmlEndCdataSectionHandler = procedure(UserData: Pointer);

  { This is called for any characters in the XML document for which
    there is no applicable handler.  This includes both characters that
    are part of markup which is of a kind that is not reported
    (comments, markup declarations), or characters that are part of a
    construct which could be reported but for which no handler has been
    supplied. The characters are passed exactly as they were in the XML
    document except that they will be encoded in UTF-8 or UTF-16.
    Line boundaries are not normalized. Note that a byte order mark
    character is not passed to the default handler. There are no
    guarantees about how characters are divided between calls to the
    default handler: for example, a comment might be split between
    multiple calls. }
  TXmlDefaultHandler = procedure(UserData: Pointer; S: PXmlChar; Len: Integer);

  { This is called for the start of the DOCTYPE declaration, before
    any TDocTypeDeclaration or internal subset is parsed. }
  TXmlStartDoctypeDeclHandler = procedure(UserData: Pointer;
    DoctypeName, Sysid, Pubid: PXmlChar; HasInternalSubset: Integer);

  { This is called for the start of the DOCTYPE declaration when the
    closing > is encountered, but after processing any external
    subset. }
  TXmlEndDoctypeDeclHandler = procedure(UserData: Pointer);

  { OBSOLETE -- OBSOLETE -- OBSOLETE
    This handler has been superceded by the EntityDeclHandler above.
    It is provided here for backward compatibility.

    This is called for a declaration of an unparsed (NDATA) TEntity.
    The base argument is whatever was set by XML_SetBase. The
    entityName, systemId and notationName arguments will never be
    NULL. The other arguments may be. }
  TXmlUnparsedEntityDeclHandler = procedure(UserData: Pointer;
    EntityName, Base, SystemId, PublicId, NotationName: PXmlChar);

  { This is called for a declaration of notation.  The base argument is
    whatever was set by XML_SetBase. The notationName will never be
    NULL.  The other arguments can be. }
  TXmlNotationDeclHandler = procedure(UserData: Pointer;
    NotationName, Base, SystemId, PublicId: PXmlChar);

  { When namespace processing is enabled, these are called once for
    each namespace declaration. The call to the start and end element
    handlers occur between the calls to the start and end namespace
    declaration handlers. For an xmlns TAttribute, TPrefix will be
    NULL.  For an xmlns="" TAttribute, uri will be NULL. }
  TXmlStartNamespaceDeclHandler = procedure(UserData: Pointer;
    TPrefix, Uri: PXmlChar);
  TXmlEndNamespaceDeclHandler = procedure(UserData: Pointer;
    TPrefix: PXmlChar);

  { This is called if the document is not standalone, that is, it has an
    external subset or a reference to a parameter TEntity, but does not
    have standalone="yes". If this handler returns xsError,
    then processing will not continue, and the parser will return a
    xeNotStandalone error.
    If parameter TEntity parsing is enabled, then in addition to the
    conditions above this handler will only be called if the referenced
    TEntity was actually read. }
  TXmlNotStandaloneHandler = function(UserData: Pointer): Integer;

  { This is called for a reference to an external parsed general
    TEntity.  The referenced TEntity is not automatically parsed.  The
    application can parse it immediately or later using
    XML_ExternalEntityParserCreate.

    The parser argument is the parser parsing the TEntity containing the
    reference; it can be passed as the parser argument to
    XML_ExternalEntityParserCreate.  The systemId argument is the
    system identifier as specified in the TEntity declaration; it will
    not be NULL.

    The base argument is the system identifier that should be used as
    the base for resolving systemId if systemId was relative; this is
    set by XML_SetBase; it may be NULL.

    The publicId argument is the public identifier as specified in the
    TEntity declaration, or NULL if none was specified; the whitespace
    in the public identifier will have been normalized as required by
    the XML spec.

    The context argument specifies the parsing context in the format
    expected by the context argument to XML_ExternalEntityParserCreate;
    context is valid only until the handler returns, so if the
    referenced TEntity is to be parsed later, it must be copied.
    context is NULL only when the TEntity is a parameter TEntity.

    The handler should return xsError if processing should not
    continue because of a fatal error in the handling of the external
    TEntity.  In this case the calling parser will return an
    xeExternalEntityHandling error.

    Note that unlike other handlers the first argument is the parser,
    not userData. }
  TXmlExternalEntityRefHandler = function(Parser: TXmlParser;
    Context, Base, SystemId, PublicId: PXmlChar): Integer;

  { This is called in two situations:
    1) An TEntity reference is encountered for which no declaration
    has been read *and* this is not an error.
    2) An internal TEntity reference is read, but not expanded, because
    XML_SetDefaultHandler has been called.
    Note: skipped parameter entities in declarations and skipped general
    entities in attribute values cannot be reported, because
    the event would be out of sync with the reporting of the
    declarations or attribute values }
  TXmlSkippedEntityHandler = procedure(UserData: Pointer;
    EntityName: PXmlChar; IsParameterEntity: Integer);

  (* This structure is filled in by the TXmlUnknownEncodingHandler to
    provide information to the parser about encodings that are unknown
    to the parser.

    The map[b] member gives information about byte sequences whose
    first byte is b.

    If map[b] is c where c is >= 0, then b by itself encodes the
    Unicode scalar value c.

    If map[b] is -1, then the byte sequence is malformed.

    If map[b] is -n, where n >= 2, then b is the first byte of an
    n-byte sequence that encodes a single Unicode scalar value.

    The data member will be passed as the first argument to the convert
    function.

    The convert function is used to convert multibyte sequences; s will
    point to a n-byte sequence where map[(Cardinal char)*s] == -n.  The
    convert function must return the Unicode scalar value represented
    by this byte sequence or -1 if the byte sequence is malformed.

    The convert function may be NULL if the encoding is a single-byte
    encoding, that is if map[b] >= -1 for all bytes b.

    When the parser is finished with the encoding, then if release is
    not NULL, it will call release passing it the data member; once
    release has been called, the convert function will not be called
    again.

    Expat places certain restrictions on the encodings that are supported
    using this mechanism.

    1. Every ASCII character that can appear in a well-formed XML document,
    other than the characters

    $@\^`{}~

    must be represented by a single byte, and that byte must be the
    same byte that represents that character in ASCII.

    2. No character may require more than 4 bytes to encode.

    3. All characters encoded must have Unicode scalar values <=
    0xFFFF, (i.e., characters that would be encoded by surrogates in
    UTF-16 are  not alLowed).  Note that this restriction doesn't
    apply to the built-in support for UTF-8 and UTF-16.

    4. No Unicode character may be encoded by more than one distinct
    sequence of bytes. *)
  PXmlEncoding = ^TXmlEncoding;
  TXmlEncoding = record
    Map: array [0..255] of Integer;
    Data: Pointer;

    Convert: function(Data: Pointer; S: PAnsiChar): Integer;
    Release: procedure(Data: Pointer);
  end;

  { This is called for an encoding that is unknown to the parser.

    The encodingHandlerData argument is that which was passed as the
    second argument to XML_SetUnknownEncodingHandler.

    The name argument gives the name of the encoding as specified in
    the encoding declaration.

    If the callback can provide information about the encoding, it must
    fill in the TXmlEncoding structure, and return xsOK.
    Otherwise it must return xsError.

    If info does not describe a suitable encoding, then the parser will
    return an XML_UNKNOWN_ENCODING error. }
  TXmlUnknownEncodingHandler = function(EncodingHandlerData: Pointer;
    Name: PXmlChar; Info: PXmlEncoding): Integer;

  PXmlMemoryHandlingSuite = ^TXmlMemoryHandlingSuite;
  TXmlMemoryHandlingSuite = record
    MallocFunction: function(var Ptr: Pointer; Size: Integer): Boolean;
    ReallocFunction: function(var Ptr: Pointer; Old, Size: Integer): Boolean;
    FreeFunction: function(var Ptr: Pointer; Size: Integer): Boolean;
  end;

  KEY = PXmlChar;

  PPNamed = ^PNamed;
  PNamed = ^TNamed;
  TNamed = record
    Name: KEY;
    Alloc: Integer;
  end;

  PHashTable = ^THashTable;
  THashTable = record
    V: PPNamed;
    A: Integer;

    Power: Int8u;
    Size, Used: TSize;
    Mem: PXmlMemoryHandlingSuite;
  end;

  PEntity = ^TEntity;
  TEntity = record
    Name: PXmlChar;
    Alloc: Integer;

    TextPtr: PXmlChar;
    TextLen, { length in TXmlChars }
    Processed: Integer; { # of processed bytes - when suspended }
    SystemId, Base, PublicId, Notation: PXmlChar;

    Open, IsParam, IsInternal: TXmlBool;
    { true if declared in internal subset outside PE }
  end;

  POpenInternalEntity = ^TOpenInternalEntity;
  TOpenInternalEntity = record
    InternalEventPtr, InternalEventEndPtr: PAnsiChar;

    Next: POpenInternalEntity;
    TEntity: PEntity;

    StartTagLevel: Integer;
    BetweenDecl: TXmlBool; { WFC: PE Between Declarations }
  end;

  PContentScaffold = ^TContentScaffold;
  TContentScaffold = record
    ContentType: TXmlContentType;
    Quant: TXmlContentQuant;
    Name: PXmlChar;

    Firstchild, Lastchild, Childcnt, Nextsib: Integer;
  end;

  PPrefix = ^TPrefix;

  PAttributeID = ^TAttributeID;
  TAttributeID = record
    Name: PXmlChar;
    Alloc: Integer;
    TPrefix: PPrefix;

    MaybeTokenized, Xmlns: TXmlBool;
  end;

  PDefaultAttribute = ^TDefaultAttribute;
  TDefaultAttribute = record
    Id: PAttributeID;

    IsCdata: TXmlBool;
    Value: PXmlChar;
  end;

  PElementType = ^TElementType;
  TElementType = record
    Name: PXmlChar;
    Alloc: Integer;
    TPrefix: PPrefix;
    IdAtt: PAttributeID;

    NDefaultAtts, AllocDefaultAtts, DefaultAttsAlloc: Integer;

    DefaultAtts: PDefaultAttribute;
  end;

  PTagName = ^TTagName;
  TTagName = record
    Str, LocalPart, TPrefix: PXmlChar;
    StrLen, UriLen, PrefixLen: Integer;
  end;

  { TTag represents an open element.
    The name of the element is stored in both the document and API
    encodings.  The memory buffer 'buf' is a separately-allocated
    memory area which stores the name.  During the XmlParse()/
    XMLParseBuffer() when the element is open, the memory for the 'raw'
    version of the name (in the document encoding) is shared with the
    document buffer.  If the element is open across calls to
    XmlParse()/XML_ParseBuffer(), the buffer is re-allocated to
    contain the 'raw' name as well.

    A parser re-uses these structures, maintaining a list of allocated
    TTag objects in a free list. }
  PPBinding = ^PBinding;
  PBinding = ^TBinding;

  PTag = ^TTag;
  TTag = record
    Parent: PTag; { parent of this element }
    RawName: PAnsiChar; { tagName in the original encoding }

    RawNameLength: Integer;

    Name: TTagName; { tagName in the API encoding }

    Buf, { buffer for name components }
    BufEnd: PAnsiChar; { end of the buffer }
    Alloc: Integer;

    Bindings: PBinding;
  end;

  TBinding = record
    TPrefix: PPrefix;

    NextTagBinding, PrevPrefixBinding: PBinding;

    AttId: PAttributeID;
    Uri: PXmlChar;

    UriLen, UriAlloc: Integer;
  end;

  TPrefix = record
    Name: PXmlChar;
    Alloc: Integer;
    TBinding: PBinding;
  end;

  PNameSpaceAtt = ^TNameSpaceAtt;

  TNameSpaceAtt = record
    Version, Hash: Int32u;
    UriName: PXmlChar;
  end;

  PBlock = ^TBlock;
  TBlock = record
    Next: PBlock;
    Size, Alloc: Integer;

    S: array [0..0] of TXmlChar;
  end;

  PStringPool = ^TStringPool;
  TStringPool = record
    Blocks, FreeBlocks: PBlock;

    Stop, Ptr, Start: PXmlChar;

    Mem: PXmlMemoryHandlingSuite;
  end;

  PDocTypeDeclaration = ^TDocTypeDeclaration;
  TDocTypeDeclaration = record
    GeneralEntities, ElementTypes, AttributeIds, Prefixes: THashTable;

    Pool, EntityValuePool: TStringPool;

    { false once a parameter TEntity reference has been skipped }
    KeepProcessing: TXmlBool;

    { true once an internal or external PE reference has been encountered;
      this includes the reference to an external subset }
    HasParamEntityRefs, Standalone: TXmlBool;

{$IFDEF XML_DTD}
    { indicates if external PE has been read }
    ParamEntityRead: TXmlBool;
    ParamEntities: THashTable;
{$ENDIF}

    DefaultPrefix: TPrefix;

    { === scaffolding for building content model === }
    In_eldecl: TXmlBool;
    Scaffold: PContentScaffold;

    ContentStringLen, ScaffSize, ScaffCount: Cardinal;

    ScaffLevel: Integer;
    ScaffIndex: PInteger;
    ScaffAlloc: Integer;
  end;

  TXmlParsing = (xpInitialized, xpParsing, xpFinished, xpSuspended);

  TXmlParsingStatus = record
    Parsing: TXmlParsing;
    FinalBuffer: TXmlBool;
  end;

  TProcessor = function(Parser: TXmlParser; Start, Stop: PAnsiChar;
    EndPtr: PPAnsiChar): TXmlError;

  TXmlParserStruct = record
    UserData, HandlerArg: Pointer;

    Buffer: PAnsiChar;
    Mem: TXmlMemoryHandlingSuite;

    { first character to be parsed }
    BufferPtr: PAnsiChar;

    { past last character to be parsed }
    BufferEnd: PAnsiChar;

    { allocated end of buffer }
    BufferLim: PAnsiChar;

    { the size of the allocated buffer }
    BufferAloc: Integer;

    ParseEndByteIndex: TXmlIndex;

    ParseEndPtr: PAnsiChar;
    DataBuf, DataBufEnd: PXmlChar;

    { XML Handlers }
    StartElementHandler: TXmlStartElementHandler;
    EndElementHandler: TXmlEndElementHandler;
    CharacterDataHandler: TXmlCharacterDataHandler;
    ProcessingInstructionHandler: TXmlProcessingInstructionHandler;
    CommentHandler: TXmlCommentHandler;
    StartCdataSectionHandler: TXmlStartCdataSectionHandler;
    EndCdataSectionHandler: TXmlEndCdataSectionHandler;
    DefaultHandler: TXmlDefaultHandler;
    StartDoctypeDeclHandler: TXmlStartDoctypeDeclHandler;
    EndDoctypeDeclHandler: TXmlEndDoctypeDeclHandler;
    UnparsedEntityDeclHandler: TXmlUnparsedEntityDeclHandler;
    NotationDeclHandler: TXmlNotationDeclHandler;
    StartNamespaceDeclHandler: TXmlStartNamespaceDeclHandler;
    EndNamespaceDeclHandler: TXmlEndNamespaceDeclHandler;
    NotStandaloneHandler: TXmlNotStandaloneHandler;
    ExternalEntityRefHandler: TXmlExternalEntityRefHandler;
    ExternalEntityRefHandlerArg: TXmlParser;
    SkippedEntityHandler: TXmlSkippedEntityHandler;
    UnknownEncodingHandler: TXmlUnknownEncodingHandler;
    ElementDeclHandler: TXmlElementDeclHandler;
    AttlistDeclHandler: TXmlAttlistDeclHandler;
    EntityDeclHandler: TXmlEntityDeclHandler;
    XmlDeclHandler: TXmlXmlDeclHandler;

    Encoding: PEncoding;
    InitEncoding: TInitEncoding;
    InternalEncoding: PEncoding;
    ProtocolEncodingName: PXmlChar;

    NameSpace, NameSpaceTriplets: TXmlBool;

    UnknownEncodingMem, UnknownEncodingData,
      UnknownEncodingHandlerData: Pointer;
    UnknownEncodingAlloc: Integer;

    UnknownEncodingRelease: procedure(Void: Pointer);

    PrologState: TPrologState;
    TProcessor: TProcessor;
    ErrorCode: TXmlError;
    EventPtr, EventEndPtr, PositionPtr: PAnsiChar;

    OpenInternalEntities, FreeInternalEntities: POpenInternalEntity;

    DefaultExpandInternalEntities: TXmlBool;

    TagLevel: Integer;
    DeclEntity: PEntity;

    DoctypeName, DoctypeSysid, DoctypePubid, DeclAttributeType,
      DeclNotationName, DeclNotationPublicId: PXmlChar;

    DeclElementType: PElementType;
    DeclAttributeId: PAttributeID;

    DeclAttributeIsCdata, DeclAttributeIsId: TXmlBool;

    DocTypeDeclaration: PDocTypeDeclaration;

    CurBase: PXmlChar;

    TagStack, FreeTagList: PTag;

    InheritedBindings, FreeBindingList: PBinding;

    AttsSize, AttsAlloc, NameSpaceAttsAlloc, NumSpecifiedAtts, IdAttIndex: Integer;

    Atts: PAttribute;
    NameSpaceAtts: PNameSpaceAtt;

    NameSpaceAttsVersion: Cardinal;
    NameSpaceAttsPower: Int8u;

    Position: TPosition;
    TempPool, Temp2Pool: TStringPool;

    GroupConnector: PAnsiChar;
    GroupSize, GroupAlloc: Cardinal;

    NamespaceSeparator: TXmlChar;

    ParentParser: TXmlParser;
    ParsingStatus: TXmlParsingStatus;

{$IFDEF XML_DTD}
    IsParamEntity, UseForeignDTD: TXmlBool;

    ParamEntityParsing: TXmlParamEntityParsing;
{$ENDIF}
  end;

const
  CXmlTrue = 1;
  CXmlFalse = 0;

{ Constructs a new parser; encoding is the encoding specified by the
  external protocol or NIL if there is none specified. }
function XmlParserCreate(const Encoding: PXmlChar): TXmlParser;

{ Constructs a new parser using the memory management suite referred to
  by memsuite. If memsuite is NULL, then use the standard library memory
  suite. If namespaceSeparator is non-NULL it creates a parser with
  namespace processing as described above. The character pointed at
  will serve as the namespace separator.

  All further memory operations used for the created parser will come from
  the given suite. }
function XmlParserCreate_MM(Encoding: PXmlChar;
  Memsuite: PXmlMemoryHandlingSuite; NamespaceSeparator: PXmlChar)
  : TXmlParser;

{ This value is passed as the userData argument to callbacks. }
procedure XmlSetUserData(Parser: TXmlParser; UserData: Pointer);

procedure XmlSetElementHandler(Parser: TXmlParser;
  Start: TXmlStartElementHandler; Stop: TXmlEndElementHandler);

procedure XmlSetCharacterDataHandler(Parser: TXmlParser;
  Handler: TXmlCharacterDataHandler);

{ Parses some input. Returns xsError if a fatal error is
  detected.  The last call to XmlParse must have isFinal true; len
  may be zero for this call (or any other).

  Though the return values for these functions has always been
  described as a Boolean value, the implementation, at least for the
  1.95.x series, has always returned exactly one of the TXmlStatus
  values. }
function XmlParse(Parser: TXmlParser; const S: PAnsiChar; Len, IsFinal: Integer)
  : TXmlStatus;

{ If XmlParse or XML_ParseBuffer have returned xsError, then
  XmlGetErrorCode returns information about the error. }
function XmlGetErrorCode(Parser: TXmlParser): TXmlError;

{ Returns a string describing the error. }
function XmlErrorString(Code: TXmlError): PXmlLChar;

{ These functions return information about the current parse
  location.  They may be called from any callback called to report
  some parse event; in this case the location is the location of the
  first of the sequence of characters that generated the event.  When
  called from callbacks generated by declarations in the document
  prologue, the location identified isn't as neatly defined, but will
  be within the relevant markup.  When called outside of the callback
  functions, the position indicated will be just past the last parse
  event (regardless of whether there was an associated callback).

  They may also be called after returning from a call to XmlParse
  or XML_ParseBuffer.  If the return value is xsError then
  the location is the location of the character at which the error
  was detected; otherwise the location is the location of the last
  parse event, as described above. }
function XmlGetCurrentLineNumber(Parser: TXmlParser): TXmlSize;

{ Frees memory used by the parser. }
procedure XmlParserFree(Parser: TXmlParser);

implementation

{$Q-}
{$R-}

function PoolStoreString(Pool: PStringPool; Enc: PEncoding;
  Ptr, Stop: PAnsiChar): PXmlChar; forward;
procedure PoolFinish(Pool: PStringPool); forward;
procedure PoolClear(Pool: PStringPool); forward;
procedure PoolDestroy(Pool: PStringPool); forward;
function PoolAppendChar(Pool: PStringPool; C: AnsiChar): Integer; forward;

function ReportProcessingInstruction(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): Integer; forward;
function ReportComment(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): Integer; forward;

function GetAttributeId(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): PAttributeID; forward;

function StoreAttributeValue(Parser: TXmlParser; Enc: PEncoding;
  IsCdata: TXmlBool; Ptr, Stop: PAnsiChar; Pool: PStringPool)
  : TXmlError; forward;

const
  CImplicitContext: array [0 .. 40] of TXmlChar = ('x', 'm', 'l', '=', 'h', 't',
    't', 'p', ':', '/', '/', 'w', 'w', 'w', '.', 'w', '3', '.', 'o', 'r', 'g',
    '/', 'X', 'M', 'L', '/', '1', '9', '9', '8', '/', 'n', 'a', 'm', 'e', 's',
    'p', 'a', 'c', 'e', #0);

  CInitTagBufferSize = 32; { must be a multiple of SizeOf(TXmlChar) }
  CInitDataBufferSize = 1024;
  CInitAttsSize = 16;
  CInitAttsVersion = $FFFFFFFF;
  CInitBlockSize = 1024;
  CInitBufferSize = 1024;

  CExpandSpare = 24;

  CInitScaffoldElements = 32;

  CInitPower = 6;

type
  PPIntChar = ^PIntChar;
  PIntChar = ^IntChar;

{$IFDEF XML_UNICODE}
  IntChar = Int16u;
{$ELSE }
  IntChar = AnsiChar;
{$ENDIF}

  PHashTableIter = ^THashTableIter;
  THashTableIter = record
    P, Stop: PPNamed;
  end;

const
{$IFDEF XML_UNICODE}
  CXmlEncodeMax = CXmlUTF16EncodeMax;
{$ELSE }
  CXmlEncodeMax = CXmlUTF8EncodeMax;
{$ENDIF}

function MemCmp(P1, P2: PInt8u; L: Integer): Integer;
begin
  while L > 0 do
  begin
    if P1^ <> P2^ then
    begin
      Result := P1^ - P2^;

      Exit;
    end;

    Dec(L);
    Inc(PtrComp(P1));
    Inc(PtrComp(P2));
  end;

  Result := 0;
end;

{ Basic character hash algorithm, taken from Python's string hash:
  h = h * 1000003 ^ character, the constant being a prime number. }
function CharHash(H: Int32u; C: TXmlChar): Int32u;  {$IFDEF SUPPORTS_INLINE}
  inline; {$ENDIF}
begin
{$IFDEF XML_UNICODE}
  Result := (H * $F4243) xor Int16u(C);
{$ELSE }
  Result := (H * $F4243) xor Int8u(C);
{$ENDIF}
end;

function MUSRasterizerConverterERT(Enc: PEncoding; S: PAnsiChar): Integer;
begin
{$IFDEF XML_UNICODE}
  Result := Integer(not Boolean(Enc.IsUtf16) or Boolean(Int32u(S) and 1));
{$ELSE }
  Result := Integer(not Boolean(Enc.IsUtf8));
{$ENDIF}
end;

{ For probing (after a collision) we need a step size relative prime
  to the hash table size, which is a power of 2. We use double-hashing,
  since we can calculate a second hash value cheaply by taking those bits
  of the first hash value that were discarded (masked out) when the table
  index was calculated: index:=hash and mask, where mask:=table.size - 1.
  We limit the maximum step size to table.size div 4 (mask shr 2 ) and make
  it odd, since odd numbers are always relative prime to a power of 2. }
function SecontHash(Hash, Mask: Int32u; Power: Int8u): Int8u;
 {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := ((Hash and not Mask) shr (Power - 1)) and (Mask shr 2);
end;

function ProbeStep(Hash, Mask: Int32u; Power: Int8u): Int8u;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := SecontHash(Hash, Mask, Power) or 1;
end;

function XML_T(X: AnsiChar): TXmlChar;
begin
  Result := TXmlChar(X);
end;

function XML_L(X: AnsiChar): TXmlChar;
begin
  Result := TXmlChar(X);
end;

{ Round up n to be a multiple of Size, where Size is a power of 2. }
function RoundUp(N, Size: Integer): Integer;
begin
  Result := (N + (Size - 1)) and not (Size - 1);
end;

procedure XmlConvert(Enc: PEncoding; FromP, FromLim, ToP, ToLim: Pointer);
begin
{$IFDEF XML_UNICODE}
  XmlUtf16Convert(Enc, FromP, FromLim, ToP, ToLim);
{$ELSE }
  XmlUtf8Convert(Enc, FromP, FromLim, ToP, ToLim);
{$ENDIF}
end;

function XmlEncode(CharNumber: Integer; Buf: Pointer): Integer;
begin
{$IFDEF XML_UNICODE}
  Result := XmlUtf16Encode(CharNumber, Buf);
{$ELSE }
  Result := XmlUtf8Encode(CharNumber, Buf);
{$ENDIF}
end;

procedure PoolInit(Pool: PStringPool; Ms: PXmlMemoryHandlingSuite);
begin
  Pool.Blocks := nil;
  Pool.FreeBlocks := nil;
  Pool.Start := nil;
  Pool.Ptr := nil;
  Pool.Stop := nil;
  Pool.Mem := Ms;
end;

procedure HashTableDestroy(Table: PHashTable);
var
  I: TSize;
begin
  I := 0;

  while I < Table.Size do
  begin
    if PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^ <> nil then
      Table.Mem.FreeFunction(Pointer(PPNamed(PtrComp(Table.V) + I *
        SizeOf(PNamed))^), PPNamed(PtrComp(Table.V) + I *
        SizeOf(PNamed))^^.Alloc);

    Inc(I);
  end;

  Table.Mem.FreeFunction(Pointer(Table.V), Table.A);
end;

procedure HashTableInit(P: PHashTable; Ms: PXmlMemoryHandlingSuite);
begin
  P.Power := 0;
  P.Size := 0;
  P.Used := 0;
  P.V := nil;
  P.Mem := Ms;
end;

procedure HashTableIterInit(Iter: PHashTableIter; Table: PHashTable);
begin
  Iter.P := Table.V;
  Iter.Stop := PPNamed(PtrComp(Iter.P) + Table.Size * SizeOf(PNamed));
end;

function HashTableIterNext(Iter: PHashTableIter): PNamed;
var
  Tem: PNamed;
begin
  while Iter.P <> Iter.Stop do
  begin
    Tem := Iter.P^;

    Inc(PtrComp(Iter.P), SizeOf(PNamed));

    if Tem <> nil then
    begin
      Result := Tem;
      Exit;
    end;
  end;
  Result := nil;
end;

function DtdCreate(Ms: PXmlMemoryHandlingSuite): PDocTypeDeclaration;
begin
  Ms.MallocFunction(Pointer(Result), SizeOf(TDocTypeDeclaration));

  if Result = nil then
    Exit;

  PoolInit(@Result.Pool, Ms);
  PoolInit(@Result.EntityValuePool, Ms);

  HashTableInit(@Result.GeneralEntities, Ms);
  HashTableInit(@Result.ElementTypes, Ms);
  HashTableInit(@Result.AttributeIds, Ms);
  HashTableInit(@Result.Prefixes, Ms);

{$IFDEF XML_DTD}
  Result.ParamEntityRead := CXmlFalse;
  HashTableInit(@Result.ParamEntities, Ms);
{$ENDIF}

  Result.DefaultPrefix.Name := nil;
  Result.DefaultPrefix.TBinding := nil;

  Result.In_eldecl := CXmlFalse;
  Result.ScaffIndex := nil;
  Result.ScaffAlloc := 0;
  Result.Scaffold := nil;
  Result.ScaffLevel := 0;
  Result.ScaffSize := 0;
  Result.ScaffCount := 0;
  Result.ContentStringLen := 0;

  Result.KeepProcessing := CXmlTrue;
  Result.HasParamEntityRefs := CXmlFalse;
  Result.Standalone := CXmlFalse;
end;

procedure DtdDestroy(P: PDocTypeDeclaration; IsDocEntity: TXmlBool;
  Ms: PXmlMemoryHandlingSuite);
var
  Iter: THashTableIter;
  E: PElementType;
begin
  HashTableIterInit(@Iter, @P.ElementTypes);

  repeat
    E := PElementType(HashTableIterNext(@Iter));

    if E = nil then
      Break;

    if E.AllocDefaultAtts <> 0 then
      Ms.FreeFunction(Pointer(E.DefaultAtts), E.DefaultAttsAlloc);
  until False;

  HashTableDestroy(@P.GeneralEntities);

{$IFDEF XML_DTD}
  HashTableDestroy(@P.ParamEntities);
{$ENDIF}

  HashTableDestroy(@P.ElementTypes);
  HashTableDestroy(@P.AttributeIds);
  HashTableDestroy(@P.Prefixes);

  PoolDestroy(@P.Pool);
  PoolDestroy(@P.EntityValuePool);

  if IsDocEntity <> 0 then
  begin
    Ms.FreeFunction(Pointer(P.ScaffIndex), P.ScaffAlloc);
    Ms.FreeFunction(Pointer(P.Scaffold), SizeOf(TContentScaffold));
  end;

  Ms.FreeFunction(Pointer(P), SizeOf(TDocTypeDeclaration));
end;

function HandleUnknownEncoding(Parser: TXmlParser; EncodingName: PXmlChar)
  : TXmlError;
begin
end;

function InitializeEncoding(Parser: TXmlParser): TXmlError;
var
  S : PAnsiChar;
  Ok: Integer;
begin
{$IFDEF XML_UNICODE {..}
{$ELSE }
  S := Pointer(Parser.ProtocolEncodingName);
{$ENDIF}

  if Parser.NameSpace <> 0 then
    Ok := XmlInitEncodingNS(@Parser.InitEncoding, @Parser.Encoding,
      Pointer(S))
  else
    Ok := XmlInitEncoding(@Parser.InitEncoding, @Parser.Encoding,
      Pointer(S));

  if Ok <> 0 then
    Result := xeNone
  else
    Result := HandleUnknownEncoding(Parser, Parser.ProtocolEncodingName);
end;

procedure ReportDefault(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar);
begin
end;

function GetContext(Parser: TXmlParser): PXmlChar;
begin
end;

function ProcessXmlDecl(Parser: TXmlParser; IsGeneralTextEntity: Integer;
  S, Next: PAnsiChar): TXmlError;
var
  EncodingName, Version, Versionend: PAnsiChar;
  StoredEncName, Storedversion: PXmlChar;
  NewEncoding: PEncoding;
  Standalone, Ok: Integer;
begin
  EncodingName := nil;
  StoredEncName := nil;
  NewEncoding := nil;
  Version := nil;
  Storedversion := nil;
  Standalone := -1;

  if Parser.NameSpace <> 0 then
    Ok := XmlParseXmlDeclNS(IsGeneralTextEntity, Parser.Encoding, Pointer(S),
      Pointer(Next), @Parser.EventPtr, @Version, @Versionend, @EncodingName,
      @NewEncoding, @Standalone)
  else
    Ok := XmlParseXmlDecl(IsGeneralTextEntity, Parser.Encoding, Pointer(S),
      Pointer(Next), @Parser.EventPtr, @Version, @Versionend, @EncodingName,
      @NewEncoding, @Standalone);

  if Ok = 0 then
    if IsGeneralTextEntity <> 0 then
    begin
      Result := xeTextDecl;

      Exit;
    end
    else
    begin
      Result := xeXmlDecl;

      Exit;
    end;

  if (IsGeneralTextEntity = 0) and (Standalone = 1) then
  begin
    Parser.DocTypeDeclaration.Standalone := CXmlTrue;

{$IFDEF XML_DTD}
    if Parser.ParamEntityParsing = pepUnlessStandalone
    then
      Parser.ParamEntityParsing := pepNever;
{$ENDIF}
  end;

  if @Parser.XmlDeclHandler <> nil then
  begin
    if EncodingName <> nil then
    begin
      StoredEncName := PoolStoreString(@Parser.Temp2Pool, Parser.Encoding,
        EncodingName, PAnsiChar(PtrComp(EncodingName) +
        XmlNameLength(Parser.Encoding, Pointer(EncodingName))));

      if StoredEncName = nil then
      begin
        Result := xeNoMemory;

        Exit;
      end;

      PoolFinish(@Parser.Temp2Pool);
    end;

    if Version <> nil then
    begin
      Storedversion := PoolStoreString(@Parser.Temp2Pool, Parser.Encoding,
        Version, PAnsiChar(PtrComp(Versionend) -
        Parser.Encoding.MinBytesPerChar));

      if Storedversion = nil then
      begin
        Result := xeNoMemory;

        Exit;
      end;
    end;

    Parser.XmlDeclHandler(Parser.HandlerArg, Storedversion, StoredEncName,
      Standalone);
  end
  else if @Parser.DefaultHandler <> nil then
    ReportDefault(Parser, Parser.Encoding, S, Next);

  if Parser.ProtocolEncodingName = nil then
  begin
    if NewEncoding <> nil then
    begin
      if NewEncoding.MinBytesPerChar <> Parser.Encoding.MinBytesPerChar then
      begin
        Parser.EventPtr := EncodingName;

        Result := xeIncorrectEncoding;

        Exit;
      end;

      Parser.Encoding := NewEncoding;
    end
    else if EncodingName <> nil then
    begin
      if StoredEncName = nil then
      begin
        StoredEncName := PoolStoreString(@Parser.Temp2Pool, Parser.Encoding,
          EncodingName, PAnsiChar(PtrComp(EncodingName) +
          XmlNameLength(Parser.Encoding, Pointer(EncodingName))));

        if StoredEncName = nil then
        begin
          Result := xeNoMemory;

          Exit;
        end;
      end;

      Result := HandleUnknownEncoding(Parser, StoredEncName);
      PoolClear(@Parser.Temp2Pool);

      if Result = xeUnknownEncoding then
        Parser.EventPtr := EncodingName;

      Exit;
    end;
  end;

  if (StoredEncName <> nil) or (Storedversion <> nil) then
    PoolClear(@Parser.Temp2Pool);

  Result := xeNone;
end;

procedure PoolClear(Pool: PStringPool);
var
  P, Tem: PBlock;
begin
  if Pool.FreeBlocks = nil then
    Pool.FreeBlocks := Pool.Blocks

  else
  begin
    P := Pool.Blocks;

    while P <> nil do
    begin
      Tem := P.Next;
      P.Next := Pool.FreeBlocks;
      Pool.FreeBlocks := P;
      P := Tem;
    end;
  end;

  Pool.Blocks := nil;
  Pool.Start := nil;
  Pool.Ptr := nil;
  Pool.Stop := nil;
end;

procedure PoolDestroy(Pool: PStringPool);
var
  P, Tem: PBlock;
begin
  P := Pool.Blocks;

  while P <> nil do
  begin
    Tem := P.Next;

    Pool.Mem.FreeFunction(Pointer(P), P.Alloc);

    P := Tem;
  end;

  P := Pool.FreeBlocks;

  while P <> nil do
  begin
    Tem := P.Next;

    Pool.Mem.FreeFunction(Pointer(P), P.Alloc);

    P := Tem;
  end;
end;

function PoolGrow(Pool: PStringPool): TXmlBool;
var
  Tem: PBlock;
  BlockSize: Integer;
begin
  if Pool.FreeBlocks <> nil then
  begin
    if Pool.Start = nil then
    begin
      Pool.Blocks := Pool.FreeBlocks;
      Pool.FreeBlocks := Pool.FreeBlocks.Next;
      Pool.Blocks.Next := nil;

      Pool.Start := @Pool.Blocks.S;
      Pool.Stop := PXmlChar(PtrComp(Pool.Start) + Pool.Blocks.Size *
        SizeOf(TXmlChar));
      Pool.Ptr := Pool.Start;

      Result := CXmlTrue;

      Exit;
    end;

    if PtrComp(Pool.Stop) - PtrComp(Pool.Start) < Pool.FreeBlocks.Size then
    begin
      Tem := Pool.FreeBlocks.Next;

      Pool.FreeBlocks.Next := Pool.Blocks;
      Pool.Blocks := Pool.FreeBlocks;
      Pool.FreeBlocks := Tem;

      Move(Pool.Start^, Pointer(@Pool.Blocks.S)^, PtrComp(Pool.Stop) -
        PtrComp(Pool.Start));

      Pool.Ptr := PXmlChar(PtrComp(@Pool.Blocks.S) + PtrComp(Pool.Ptr) -
        PtrComp(Pool.Start));
      Pool.Start := @Pool.Blocks.S;
      Pool.Stop := PXmlChar(PtrComp(Pool.Start) + Pool.Blocks.Size *
        SizeOf(TXmlChar));

      Result := CXmlTrue;

      Exit;
    end;
  end;

  if (Pool.Blocks <> nil) and (Pool.Start = @Pool.Blocks.S) then
  begin
    BlockSize := (PtrComp(Pool.Stop) - PtrComp(Pool.Start)) *
      2 div SizeOf(TXmlChar);

    Pool.Mem.ReallocFunction(Pointer(Pool.Blocks), Pool.Blocks.Alloc,
      (SizeOf(PBlock) + SizeOf(Integer) * 2) + BlockSize * SizeOf(TXmlChar));

    if Pool.Blocks = nil then
    begin
      Result := CXmlFalse;

      Exit;
    end
    else
      Pool.Blocks.Alloc := (SizeOf(PBlock) + SizeOf(Integer) * 2) + BlockSize *
        SizeOf(TXmlChar);

    Pool.Blocks.Size := BlockSize;

    Pool.Ptr := PXmlChar(PtrComp(@Pool.Blocks.S) +
      (PtrComp(Pool.Ptr) - PtrComp(Pool.Start)));
    Pool.Start := @Pool.Blocks.S;
    Pool.Stop := PXmlChar(PtrComp(Pool.Start) + BlockSize *
      SizeOf(TXmlChar));
  end
  else
  begin
    BlockSize := (PtrComp(Pool.Stop) - PtrComp(Pool.Start))
      div SizeOf(TXmlChar);

    if BlockSize < CInitBlockSize then
      BlockSize := CInitBlockSize
    else
      BlockSize := BlockSize * 2;

    Pool.Mem.MallocFunction(Pointer(Tem), (SizeOf(PBlock) + SizeOf(Integer) * 2) +
      BlockSize * SizeOf(TXmlChar));

    if Tem = nil then
    begin
      Result := CXmlFalse;

      Exit;
    end;

    Tem.Size := BlockSize;
    Tem.Alloc := (SizeOf(PBlock) + SizeOf(Integer) * 2) + BlockSize *
      SizeOf(TXmlChar);
    Tem.Next := Pool.Blocks;

    Pool.Blocks := Tem;

    if Pool.Ptr <> Pool.Start then
      Move(Pool.Start^, Pointer(@Tem.S)^, PtrComp(Pool.Ptr) -
        PtrComp(Pool.Start));

    Pool.Ptr := PXmlChar(PtrComp(@Tem.S) +
      (PtrComp(Pool.Ptr) - PtrComp(Pool.Start)) * SizeOf(TXmlChar));
    Pool.Start := @Tem.S;
    Pool.Stop := PXmlChar(PtrComp(@Tem.S) + BlockSize * SizeOf(TXmlChar));
  end;

  Result := CXmlTrue;
end;

function PoolAppend(Pool: PStringPool; Enc: PEncoding;
  Ptr, Stop: PAnsiChar): PXmlChar;
begin
  Result := nil;
  if (Pool.Ptr = nil) and (PoolGrow(Pool) = 0) then
    Exit;

  repeat
    XmlConvert(Enc, @Ptr, Stop, PPIntChar(@Pool.Ptr), PIntChar(Pool.Stop));

    if Ptr = Stop then
      Break;

    if PoolGrow(Pool) = 0 then
      Exit;
  until False;

  Result := Pool.Start;
end;

function PoolStoreString(Pool: PStringPool; Enc: PEncoding;
  Ptr, Stop: PAnsiChar): PXmlChar;
begin
  Result := nil;

  if PoolAppend(Pool, Enc, Ptr, Stop) = nil then
    Exit;

  if (Pool.Ptr = Pool.Stop) and (PoolGrow(Pool) = 0) then
    Exit;

  Pool.Ptr^ := TXmlChar(0);
  Inc(PtrComp(Pool.Ptr));
  Result := Pool.Start;
end;

function PoolCopyString(Pool: PStringPool; S: PXmlChar): PXmlChar;
begin
  Result := nil;

  repeat
    if PoolAppendChar(Pool, S^) = 0 then
      Exit;

    Inc(PtrComp(S), SizeOf(TXmlChar));
  until S^ = TXmlChar(0);

  S := Pool.Start;
  PoolFinish(Pool);
  Result := S;
end;

function PoolAppendString(Pool: PStringPool; S: PXmlChar): PXmlChar;
begin
end;

function PoolStart(Pool: PStringPool): PXmlChar;
begin
  Result := Pool.Start;
end;

function PoolLength(Pool: PStringPool): Integer;
begin
  Result := PtrComp(Pool.Ptr) - PtrComp(Pool.Start);
end;

procedure PoolChop(Pool: PStringPool);
begin
  Dec(PtrComp(Pool.Ptr), SizeOf(TXmlChar));
end;

function PoolLastChar(Pool: PStringPool): TXmlChar;
begin
  Result := PXmlChar(PtrComp(Pool.Ptr) - 1 * SizeOf(TXmlChar))^;
end;

procedure PoolDiscard(Pool: PStringPool);
begin
  Pool.Ptr := Pool.Start;
end;

procedure PoolFinish(Pool: PStringPool);
begin
  Pool.Start := Pool.Ptr;
end;

function PoolAppendChar(Pool: PStringPool; C: AnsiChar): Integer;
begin
  if (Pool.Ptr = Pool.Stop) and (PoolGrow(Pool) = 0) then
    Result := 0
  else
  begin
    Pool.Ptr^ := C;

    Inc(PtrComp(Pool.Ptr));

    Result := 1;
  end;
end;

function Keyeq(S1, S2: KEY): TXmlBool;
begin
  while S1^ = S2^ do
  begin
    if S1^ = #0 then
    begin
      Result := CXmlTrue;

      Exit;
    end;

    Inc(PtrComp(S1), SizeOf(TXmlChar));
    Inc(PtrComp(S2), SizeOf(TXmlChar));
  end;

  Result := CXmlFalse;
end;

function Hash(S: KEY): Int32u;
var
  H: Int32u;
begin
  H := 0;

  while S^ <> TXmlChar(0) do
  begin
    H := CharHash(H, S^);

    Inc(PtrComp(S), SizeOf(TXmlChar));
  end;

  Result := H;
end;

function Lookup(Table: PHashTable; Name: KEY; CreateSize: TSize): PNamed;
var
  I, TableSize, NewSize, J: TSize;
  H, Mask, NewMask, NewHash: Int32u;
  Step, NewPower: Int8u;
  NewV: PPNamed;
begin
  Result := nil;
  if Table.Size = 0 then
  begin
    if CreateSize = 0 then
      Exit;

    Table.Power := CInitPower;

    { table->size is a power of 2 }
    Table.Size := TSize(1 shl CInitPower);

    TableSize := Table.Size * SizeOf(PNamed);

    Table.Mem.MallocFunction(Pointer(Table.V), TableSize);

    if Table.V = nil then
    begin
      Table.Size := 0;

      Exit;
    end
    else
      Table.A := TableSize;

    FillChar(Table.V^, TableSize, 0);

    I := Hash(name) and (Table.Size - 1);
  end
  else
  begin
    H := Hash(name);
    Mask := Table.Size - 1;
    Step := 0;
    I := H and Mask;

    while PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^ <> nil do
    begin
      if Keyeq(name, PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))
        ^^.Name) <> 0 then
      begin
        Result := PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^;
        Exit;
      end;

      if Step = 0 then
        Step := ProbeStep(H, Mask, Table.Power);

      if I < Step then
        Inc(I, Table.Size - Step)
      else
        Dec(I, Step);
    end;

    if CreateSize = 0 then
      Exit;

    { check for overfLow (table is half full) }
    if Table.Used shr (Table.Power - 1) <> 0 then
    begin
      NewPower := Table.Power + 1;
      NewSize := TSize(1 shl NewPower);
      NewMask := NewSize - 1;
      TableSize := NewSize * SizeOf(PNamed);

      Table.Mem.MallocFunction(Pointer(NewV), TableSize);

      if NewV = nil then
        Exit;

      FillChar(NewV^, TableSize, 0);

      I := 0;

      while I < Table.Size do
      begin
        if PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^ <> nil then
        begin
          NewHash := Hash(PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed)
            )^^.Name);
          J := NewHash and NewMask;
          Step := 0;

          while PPNamed(PtrComp(NewV) + J * SizeOf(PNamed))^ <> nil do
          begin
            if Step = 0 then
              Step := ProbeStep(NewHash, NewMask, NewPower);

            if J < Step then
              Inc(J, NewSize - Step)
            else
              Dec(J, Step);
          end;

          PPNamed(PtrComp(NewV) + J * SizeOf(PNamed))^ :=
            PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^;
        end;

        Inc(I);
      end;

      Table.Mem.FreeFunction(Pointer(Table.V), Table.A);

      Table.V := NewV;
      Table.A := TableSize;
      Table.Power := NewPower;
      Table.Size := NewSize;

      I := H and NewMask;
      Step := 0;

      while PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^ <> nil do
      begin
        if Step = 0 then
          Step := ProbeStep(H, NewMask, NewPower);

        if I < Step then
          Inc(I, NewSize - Step)
        else
          Dec(I, Step);
      end;
    end;
  end;

  Table.Mem.MallocFunction(Pointer(PPNamed(PtrComp(Table.V) + I *
    SizeOf(PNamed))^), CreateSize);

  if PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^ = nil then
    Exit;

  FillChar(PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^^,
    CreateSize, 0);

  PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^^.Name := name;
  PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^^.Alloc := CreateSize;

  Inc(Table.Used);

  Result := PPNamed(PtrComp(Table.V) + I * SizeOf(PNamed))^;
end;

procedure NormalizePublicId(PublicId: PXmlChar);
var
  P, S: PXmlChar;
begin
  P := PublicId;
  S := PublicId;

  while S^ <> TXmlChar(0) do
  begin
    case S^ of
      TXmlChar($20), TXmlChar($D), TXmlChar($A):
        if (P <> PublicId) and (PXmlChar(PtrComp(P) - 1 * SizeOf(TXmlChar))
          ^ <> TXmlChar($20)) then
        begin
          P^ := TXmlChar($20);

          Inc(PtrComp(P), SizeOf(TXmlChar));
        end;

    else
      begin
        P^ := S^;

        Inc(PtrComp(P), SizeOf(TXmlChar));
      end;
    end;

    Inc(PtrComp(S), SizeOf(TXmlChar));
  end;

  if (P <> PublicId) and (PXmlChar(PtrComp(P) - 1 * SizeOf(TXmlChar))
    ^ = TXmlChar($20)) then
    Dec(PtrComp(P), SizeOf(TXmlChar));

  P^ := XML_T(#0);
end;

function SetElementTypePrefix(Parser: TXmlParser;
  ElementType: PElementType): Integer;
begin
end;

{ addBinding overwrites the value of TPrefix.TBinding without checking.
  Therefore one must keep track of the old value outside of addBinding. }
function AddBinding(Parser: TXmlParser; TPrefix: PPrefix;
  AttId: PAttributeID; Uri: PXmlChar; BindingsPtr: PPBinding)
  : TXmlError;
begin
end;

{ Initially TTag.rawName always points into the parse buffer;
  for those TTag instances opened while the current parse buffer was
  processed, and not yet closed, we need to store TTag.rawName in a more
  permanent location, since the parse buffer is about to be discarded. }
function StoreRawNames(Parser: TXmlParser): TXmlBool;
var
  TTag: PTag;
  BufSize, NameLen: Integer;
  RawNameBuf, Temp: PAnsiChar;
begin
  TTag := Parser.TagStack;

  while TTag <> nil do
  begin
    NameLen := SizeOf(TXmlChar) * (TTag.Name.StrLen + 1);
    RawNameBuf := PAnsiChar(PtrComp(TTag.Buf) + NameLen);

    { Stop if already stored. Since tagStack is a stack, we can stop
      at the first entry that has already been copied; everything
      below it in the stack is already been accounted for in a
      previous call to this function. }
    if TTag.RawName = RawNameBuf then
      Break;

    { For re-use purposes we need to ensure that the
      size of TTag.buf is a multiple of SizeOf(TXmlChar ). }
    BufSize := NameLen + RoundUp(TTag.RawNameLength, SizeOf(TXmlChar));

    if BufSize > PtrComp(TTag.BufEnd) - PtrComp(TTag.Buf) then
    begin
      if Parser.Mem.ReallocFunction(Pointer(TTag.Buf), TTag.Alloc, BufSize) then
        Temp := TTag.Buf
      else
        Temp := nil;

      if Temp = nil then
      begin
        Result := CXmlFalse;

        Exit;
      end;

      TTag.Alloc := BufSize;

      { if TTag.name.str points to TTag.buf (only when namespace
        processing is off) then we have to update it }
      if TTag.Name.Str = PXmlChar(TTag.Buf) then
        TTag.Name.Str := PXmlChar(Temp);

      { if TTag->name.localPart is set (when namespace processing is on)
        then update it as well, since it will always point into TTag->buf }
      if TTag.Name.LocalPart <> nil then
        TTag.Name.LocalPart :=
          PXmlChar(PtrComp(Temp) + (PtrComp(TTag.Name.LocalPart) -
          PtrComp(TTag.Buf)));

      TTag.Buf := Temp;
      TTag.BufEnd := PAnsiChar(PtrComp(Temp) + BufSize);
      RawNameBuf := PAnsiChar(PtrComp(Temp) + NameLen);
    end;

    Move(TTag.RawName^, RawNameBuf^, TTag.RawNameLength);

    TTag.RawName := RawNameBuf;
    TTag := TTag.Parent;
  end;

  Result := CXmlTrue;
end;

{ Precondition: all arguments must be non-NULL;
  Purpose:
  - normalize attributes
  - check attributes for well-formedness
  - generate namespace aware attribute names (URI, TPrefix)
  - build list of attributes for startElementHandler
  - default attributes
  - process namespace declarations (check and report them)
  - generate namespace aware element name (URI, TPrefix) }
function StoreAtts(Parser: TXmlParser; Enc: PEncoding; AttStr: PAnsiChar;
  TagNamePtr: PTagName; BindingsPtr: PPBinding): TXmlError;
var
  DocTypeDeclaration: PDocTypeDeclaration;
  ElementType: PElementType;
  NDefaultAtts, AttIndex, PrefixLen, I, N, NPrefixes, OldAttsSize, J,
    NsAttsSize: Integer;
  Version, UriHash, Mask: Int32u;
  Step: Int8u;
  AppAtts: PPXmlChar; { the attribute list for the application }
  Uri, LocalPart, Name, S, S1, S2: PXmlChar;
  C: TXmlChar;
  TBinding, B: PBinding;
  AttId, Id: PAttributeID;
  Result_: TXmlError;
  IsCdata: TXmlBool;
  Da: PDefaultAttribute;
  P: PTag;

label
  _w0, _w1;

begin
  DocTypeDeclaration := Parser.DocTypeDeclaration; { save one level of indirection }

  AttIndex := 0;
  NPrefixes := 0;

  { lookup the element type name }
  ElementType := PElementType(Lookup(@DocTypeDeclaration.ElementTypes, TagNamePtr.Str, 0));

  if ElementType = nil then
  begin
    name := PoolCopyString(@DocTypeDeclaration.Pool, TagNamePtr.Str);

    if name = nil then
    begin
      Result := xeNoMemory;

      Exit;
    end;

    ElementType := PElementType(Lookup(@DocTypeDeclaration.ElementTypes, name,
      SizeOf(TElementType)));

    if ElementType = nil then
    begin
      Result := xeNoMemory;

      Exit;
    end;

    if (Parser.NameSpace <> 0) and (SetElementTypePrefix(Parser, ElementType) = 0)
    then
    begin
      Result := xeNoMemory;

      Exit;
    end;
  end;

  NDefaultAtts := ElementType.NDefaultAtts;

  { get the attributes from the tokenizer }
  N := XmlGetAttributes(Enc, Pointer(AttStr), Parser.AttsSize, Parser.Atts);

  if N + NDefaultAtts > Parser.AttsSize then
  begin
    OldAttsSize := Parser.AttsSize;
    Parser.AttsSize := N + NDefaultAtts + CInitAttsSize;

    if not Parser.Mem.ReallocFunction(Pointer(Parser.Atts), Parser.AttsAlloc,
      Parser.AttsSize * SizeOf(TAttribute)) then
    begin
      Result := xeNoMemory;

      Exit;
    end
    else
      Parser.AttsAlloc := Parser.AttsSize * SizeOf(TAttribute);

    if N > OldAttsSize then
      XmlGetAttributes(Enc, Pointer(AttStr), N, Parser.Atts);
  end;

  AppAtts := PPXmlChar(Parser.Atts);

  I := 0;

  while I < N do
  begin
    { add the name and value to the attribute list }
    AttId := GetAttributeId(Parser, Enc,
      Pointer(PAttribute(PtrComp(Parser.Atts) + I * SizeOf(TAttribute))
      ^.Name), Pointer(PtrComp(PAttribute(PtrComp(Parser.Atts) + I *
      SizeOf(TAttribute))^.Name) + XmlNameLength(Enc,
      PAttribute(PtrComp(Parser.Atts) + I * SizeOf(TAttribute))^.Name)));

    if AttId = nil then
    begin
      Result := xeNoMemory;

      Exit;
    end;

    { Detect duplicate attributes by their QNames. This does not work when
      namespace processing is turned on and different prefixes for the same
      namespace are used. For this case we have a check further down. }
    if PXmlChar(PtrComp(AttId.Name) - 1 * SizeOf(TXmlChar))^ <> TXmlChar(0)
    then
    begin
      if Enc = Parser.Encoding then
        Parser.EventPtr :=
          Pointer(PAttribute(PtrComp(Parser.Atts) + I *
          SizeOf(TAttribute))^.Name);

      Result := xeDuplicateAttribute;

      Exit;
    end;

    PXmlChar(PtrComp(AttId.Name) - 1 * SizeOf(TXmlChar))^ := TXmlChar(1);

    PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ :=
      AttId.Name;

    Inc(AttIndex);

    if PAttribute(PtrComp(Parser.Atts) + I * SizeOf(TAttribute))
      ^.Normalized = #0 then
    begin
      IsCdata := CXmlTrue;

      { figure out whether declared as other than CDATA }
      if AttId.MaybeTokenized <> 0 then
      begin
        J := 0;

        while J < NDefaultAtts do
        begin
          if AttId = PDefaultAttribute(PtrComp(ElementType.DefaultAtts) + J
            * SizeOf(TDefaultAttribute))^.Id then
          begin
            IsCdata := PDefaultAttribute(PtrComp(ElementType.DefaultAtts) +
              J * SizeOf(TDefaultAttribute))^.IsCdata;

            Break;
          end;

          Inc(J);
        end;
      end;

      { normalize the attribute value }
      Result_ := StoreAttributeValue(Parser, Enc, IsCdata,
        Pointer(PAttribute(PtrComp(Parser.Atts) + I * SizeOf(TAttribute))
        ^.ValuePtr), Pointer(PAttribute(PtrComp(Parser.Atts) + I *
        SizeOf(TAttribute))^.ValueEnd), @Parser.TempPool);

      if Result_ <> TXmlError(0) then
      begin
        Result := Result_;

        Exit;
      end;

      PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ :=
        PoolStart(@Parser.TempPool);

      PoolFinish(@Parser.TempPool);
    end
    else
    begin
      { the value did not need normalizing }
      PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ :=
        PoolStoreString(@Parser.TempPool, Enc,
        Pointer(PAttribute(PtrComp(Parser.Atts) + I * SizeOf(TAttribute))
        ^.ValuePtr), Pointer(PAttribute(PtrComp(Parser.Atts) + I *
        SizeOf(TAttribute))^.ValueEnd));

      if PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ = nil
      then
      begin
        Result := xeNoMemory;

        Exit;
      end;

      PoolFinish(@Parser.TempPool);
    end;

    { handle prefixed attribute names }
    if AttId.TPrefix <> nil then
      if AttId.Xmlns <> 0 then
      begin
        { deal with namespace declarations here }
        Result_ := AddBinding(Parser, AttId.TPrefix, AttId,
          PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^,
          BindingsPtr);

        if Result_ <> TXmlError(0) then
        begin
          Result := Result_;

          Exit;
        end;

        Dec(AttIndex);
      end
      else
      begin
        { deal with other prefixed names later }
        Inc(AttIndex);
        Inc(NPrefixes);

        PXmlChar(PtrComp(AttId.Name) - 1 * SizeOf(TXmlChar))^ :=
          TXmlChar(2);
      end
    else
      Inc(AttIndex);

    Inc(I);
  end;

  { set-up for XML_GetSpecifiedAttributeCount and XML_GetIdAttributeIndex }
  Parser.NumSpecifiedAtts := AttIndex;

  if (ElementType.IdAtt <> nil) and
    (PXmlChar(PtrComp(ElementType.IdAtt.Name) - 1 * SizeOf(TXmlChar))^ <>
    TXmlChar(0)) then
  begin
    I := 0;

    while I < AttIndex do
    begin
      if PPXmlChar(PtrComp(AppAtts) + I * SizeOf(PXmlChar))
        ^ = ElementType.IdAtt.Name then
      begin
        Parser.IdAttIndex := I;

        Break;
      end;

      Inc(I, 2);
    end;
  end
  else
    Parser.IdAttIndex := -1;

  { do attribute defaulting }
  I := 0;

  while I < NDefaultAtts do
  begin
    Da := PDefaultAttribute(PtrComp(ElementType.DefaultAtts) + I *
      SizeOf(TDefaultAttribute));

    if (PXmlChar(PtrComp(Da.Id.Name) - 1 * SizeOf(TXmlChar))^ = TXmlChar(0)
      ) and (Da.Value <> nil) then
      if Da.Id.TPrefix <> nil then
        if Da.Id.Xmlns <> 0 then
        begin
          Result_ := AddBinding(Parser, Da.Id.TPrefix, Da.Id, Da.Value,
            BindingsPtr);

          if Result_ <> TXmlError(0) then
          begin
            Result := Result_;

            Exit;
          end;
        end
        else
        begin
          PXmlChar(PtrComp(Da.Id.Name) - 1 * SizeOf(TXmlChar))^ :=
            TXmlChar(2);

          Inc(NPrefixes);

          PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^
            := Da.Id.Name;

          Inc(AttIndex);

          PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^
            := Da.Value;

          Inc(AttIndex);
        end
      else
      begin
        PXmlChar(PtrComp(Da.Id.Name) - 1 * SizeOf(TXmlChar))^ :=
          TXmlChar(1);

        PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ :=
          Da.Id.Name;

        Inc(AttIndex);

        PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^
          := Da.Value;

        Inc(AttIndex);
      end;

    Inc(I);
  end;

  PPXmlChar(PtrComp(AppAtts) + AttIndex * SizeOf(PXmlChar))^ := nil;

  { expand prefixed attribute names, check for duplicates,
    and clear flags that say whether attributes were specified }
  I := 0;

  if NPrefixes <> 0 then
  begin
    { j = hash table index }
    Version := Parser.NameSpaceAttsVersion;
    NsAttsSize := 1 shl Parser.NameSpaceAttsPower;

    { size of hash table must be at least 2 * (# of prefixed attributes) }
    if ShrInt32(NPrefixes shl 1, Parser.NameSpaceAttsPower) <> 0
    then { true for nsAttsPower = 0 }
    begin
      { hash table size must also be a power of 2 and >= 8 }
      while ShrInt32(NPrefixes, Parser.NameSpaceAttsPower) <> 0 do
        Inc(Parser.NameSpaceAttsPower);

      if Parser.NameSpaceAttsPower < 3 then
        Parser.NameSpaceAttsPower := 3;

      NsAttsSize := 1 shl Parser.NameSpaceAttsPower;

      if not Parser.Mem.ReallocFunction(Pointer(Parser.NameSpaceAtts),
        Parser.NameSpaceAttsAlloc, NsAttsSize * SizeOf(TNameSpaceAtt)) then
      begin
        Result := xeNoMemory;

        Exit;
      end
      else
        Parser.NameSpaceAttsAlloc := NsAttsSize * SizeOf(TNameSpaceAtt);

      Version := 0; { force re-initialization of nsAtts hash table }
    end;

    { using a version flag saves us from initializing nsAtts every time }
    if Version = 0 then { initialize version flags when version wraps around }
    begin
      Version := CInitAttsVersion;

      J := NsAttsSize;

      while J <> 0 do
      begin
        Dec(J);

        PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))^.Version
          := Version;
      end;
    end;

    Dec(Version);

    Parser.NameSpaceAttsVersion := Version;

    { expand prefixed names and check for duplicates }
    while I < AttIndex do
    begin
      S := PPXmlChar(PtrComp(AppAtts) + I * SizeOf(PXmlChar))^;

      if PXmlChar(PtrComp(S) - 1 * SizeOf(TXmlChar))^ = TXmlChar(2)
      then { prefixed }
      begin
        UriHash := 0;

        PXmlChar(PtrComp(S) - 1 * SizeOf(TXmlChar))^ := TXmlChar(0);
        { clear flag }

        Id := PAttributeID(Lookup(@DocTypeDeclaration.AttributeIds, S, 0));
        B := Id.TPrefix.TBinding;

        if B = nil then
        begin
          Result := xeUnboundPrefix;

          Exit;
        end;

        { as we expand the name we also calculate its hash value }
        J := 0;

        while J < B.UriLen do
        begin
          C := PXmlChar(PtrComp(B.Uri) + J * SizeOf(TXmlChar))^;

          if PoolAppendChar(@Parser.TempPool, C) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          UriHash := CharHash(UriHash, C);

          Inc(J);
        end;

        while S^ <> XML_T(':') do
          Inc(PtrComp(S), SizeOf(TXmlChar));

        goto _w0;

        while S^ <> TXmlChar(0) do { copies null terminator }
        begin
        _w0:
          C := S^;

          if PoolAppendChar(@Parser.TempPool, S^) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          UriHash := CharHash(UriHash, C);

          Inc(PtrComp(S), SizeOf(TXmlChar));
        end;

        { Check hash table for duplicate of expanded name (uriName).
          Derived from code in lookup(THashTable *table, ...). }
        Step := 0;
        Mask := NsAttsSize - 1;
        J := UriHash and Mask; { index into hash table }

        while PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))
          ^.Version = Version do
        begin
          { for speed we compare stored hash values first }
          if UriHash = PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))
            ^.Hash then
          begin
            S1 := PoolStart(@Parser.TempPool);
            S2 := PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt)
              )^.UriName;

            { s1 is null terminated, but not s2 }
            while (S1^ = S2^) and (S1^ <> TXmlChar(0)) do
            begin
              Inc(PtrComp(S1), SizeOf(TXmlChar));
              Inc(PtrComp(S2), SizeOf(TXmlChar));
            end;

            if S1^ = TXmlChar(0) then
            begin
              Result := xeDuplicateAttribute;

              Exit;
            end;
          end;

          if Step = 0 then
            Step := ProbeStep(UriHash, Mask, Parser.NameSpaceAttsPower);

          if J < Step then
            Inc(J, NsAttsSize - Step)
          else
            Dec(J, Step);
        end;

        if Parser.NameSpaceTriplets <> 0
        then { append namespace separator and TPrefix }
        begin
          PXmlChar(PtrComp(Parser.TempPool.Ptr) - 1 * SizeOf(TXmlChar))^
            := Parser.NamespaceSeparator;

          S := B.TPrefix.Name;

          goto _w1;

          while S^ <> TXmlChar(0) do
          begin
          _w1:
            if PoolAppendChar(@Parser.TempPool, S^) = 0 then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Inc(PtrComp(S), SizeOf(TXmlChar));
          end;
        end;

        { store expanded name in attribute list }
        S := PoolStart(@Parser.TempPool);

        PoolFinish(@Parser.TempPool);

        PPXmlChar(PtrComp(AppAtts) + I * SizeOf(PXmlChar))^ := S;

        { fill empty slot with new version, uriName and hash value }
        PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))^.Version
          := Version;
        PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))^.Hash
          := UriHash;
        PNameSpaceAtt(PtrComp(Parser.NameSpaceAtts) + J * SizeOf(TNameSpaceAtt))^.UriName := S;

        Dec(NPrefixes);

        if NPrefixes = 0 then
        begin
          Inc(I, 2);

          Break;
        end;
      end
      else { not prefixed }
        PXmlChar(PtrComp(S) - 1 * SizeOf(TXmlChar))^ := TXmlChar(0);
      { clear flag }

      Inc(I, 2);
    end;
  end;

  { clear flags for the remaining attributes }
  while I < AttIndex do
  begin
    PXmlChar(PtrComp(PPXmlChar(PtrComp(AppAtts) + I *
      SizeOf(PXmlChar))^) - 1 * SizeOf(TXmlChar))^ := TXmlChar(0);

    Inc(I, 2);
  end;

  TBinding := BindingsPtr^;

  while TBinding <> nil do
  begin
    PXmlChar(PtrComp(TBinding.AttId.Name) - 1 * SizeOf(TXmlChar))^ :=
      TXmlChar(0);

    TBinding := TBinding.NextTagBinding;
  end;

  if Parser.NameSpace = 0 then
  begin
    Result := xeNone;

    Exit;
  end;

  { expand the element type name }
  if ElementType.TPrefix <> nil then
  begin
    TBinding := ElementType.TPrefix.TBinding;

    if TBinding = nil then
    begin
      Result := xeUnboundPrefix;

      Exit;
    end;

    LocalPart := TagNamePtr.Str;

    while LocalPart^ <> XML_T(':') do
      Inc(PtrComp(LocalPart), SizeOf(TXmlChar));

  end
  else if DocTypeDeclaration.DefaultPrefix.TBinding <> nil then
  begin
    TBinding := DocTypeDeclaration.DefaultPrefix.TBinding;
    LocalPart := TagNamePtr.Str;
  end
  else
  begin
    Result := xeNone;

    Exit;
  end;

  PrefixLen := 0;

  if (Parser.NameSpaceTriplets <> 0) and (TBinding.TPrefix.Name <> nil) then
  begin
    while PXmlChar(PtrComp(TBinding.TPrefix.Name) + PrefixLen *
      SizeOf(TXmlChar))^ <> TXmlChar(0) do
      Inc(PrefixLen);

    Inc(PrefixLen); { prefixLen includes null terminator }
  end;

  TagNamePtr.LocalPart := LocalPart;
  TagNamePtr.UriLen := TBinding.UriLen;
  TagNamePtr.TPrefix := TBinding.TPrefix.Name;
  TagNamePtr.PrefixLen := PrefixLen;

  I := 0;

  while PXmlChar(PtrComp(LocalPart) + I * SizeOf(TXmlChar))^ <>
    TXmlChar(0) do
    Inc(I);

  Inc(I); { i includes null terminator }

  N := I + TBinding.UriLen + PrefixLen;

  if N > TBinding.UriAlloc then
  begin
    Parser.Mem.MallocFunction(Pointer(Uri), (N + CExpandSpare) *
      SizeOf(TXmlChar));

    if Uri = nil then
    begin
      Result := xeNoMemory;

      Exit;
    end;

    J := TBinding.UriAlloc;

    TBinding.UriAlloc := N + CExpandSpare;

    Move(TBinding.Uri^, Uri^, TBinding.UriLen * SizeOf(TXmlChar));

    P := Parser.TagStack;

    while P <> nil do
    begin
      if P.Name.Str = TBinding.Uri then
        P.Name.Str := Uri;

      P := P.Parent;
    end;

    Parser.Mem.FreeFunction(Pointer(TBinding.Uri), J * SizeOf(TXmlChar));

    TBinding.Uri := Uri;
  end;

  { if namespaceSeparator != '\0' then uri includes it already }
  Uri := PXmlChar(PtrComp(TBinding.Uri) + TBinding.UriLen *
    SizeOf(TXmlChar));

  Move(LocalPart^, Uri^, I * SizeOf(TXmlChar));

  { we always have a namespace separator between localPart and TPrefix }
  if PrefixLen <> 0 then
  begin
    Inc(PtrComp(Uri), (I - 1) * SizeOf(TXmlChar));

    Uri^ := Parser.NamespaceSeparator; { replace null terminator }

    Move(TBinding.TPrefix.Name^, PXmlChar(PtrComp(Uri) + 1 * SizeOf(TXmlChar)
      )^, PrefixLen * SizeOf(TXmlChar));
  end;

  TagNamePtr.Str := TBinding.Uri;

  Result := xeNone;
end;

function ProcessInternalEntity(Parser: TXmlParser; TEntity: PEntity;
  BetweenDecl: TXmlBool): TXmlError;
begin
end;

function EpilogProcessor(Parser: TXmlParser; S, Stop: PAnsiChar;
  NextPtr: PPAnsiChar): TXmlError;
var
  Next: PAnsiChar;
  Tok: TXmlTok;
begin
  Parser.TProcessor := @EpilogProcessor;
  Parser.EventPtr := S;

  repeat
    Next := nil;
    Tok := XmlPrologTok(Parser.Encoding, Pointer(S), Pointer(Stop), @Next);

    Parser.EventEndPtr := Next;

    case Tok of
      TXmlTok(-Integer(xtProlog_S)):
        begin
          if @Parser.DefaultHandler <> nil then
          begin
            ReportDefault(Parser, Parser.Encoding, S, Next);

            if Parser.ParsingStatus.Parsing = xpFinished then
            begin
              Result := xeAborted;

              Exit;
            end;
          end;

          NextPtr^ := Next;
          Result := xeNone;

          Exit;
        end;

      xtNone:
        begin
          NextPtr^ := S;
          Result := xeNone;

          Exit;
        end;

      xtProlog_S:
        if @Parser.DefaultHandler <> nil then
          ReportDefault(Parser, Parser.Encoding, S, Next);

      xtProcessingInstruction:
        if ReportProcessingInstruction(Parser, Parser.Encoding, S, Next) = 0
        then
        begin
          Result := xeNoMemory;

          Exit;
        end;

      xtComment:
        if ReportComment(Parser, Parser.Encoding, S, Next) = 0 then
        begin
          Result := xeNoMemory;

          Exit;
        end;

      xtInvalid:
        begin
          Parser.EventPtr := Next;

          Result := xeInvalidToken;

          Exit;
        end;

      xtPartial:
        begin
          if Parser.ParsingStatus.FinalBuffer = 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          Result := xeUnclosedToken;

          Exit;
        end;

      xtPartialChar:
        begin
          if Parser.ParsingStatus.FinalBuffer = 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          Result := xePartialChar;
          Exit;
        end;
    else
      begin
        Result := xeJunkAfterDocElement;
        Exit;
      end;
    end;

    Parser.EventPtr := Next;

    S := Next;

    case Parser.ParsingStatus.Parsing of
      xpSuspended:
        begin
          NextPtr^ := Next;
          Result := xeNone;

          Exit;
        end;

      xpFinished:
        begin
          Result := xeAborted;
          Exit;
        end;
    end;
  until False;
end;

{ startPtr gets set to non-null if the section is closed, and to null if
  the section is not yet closed. }
function DoCdataSection(Parser: TXmlParser; Enc: PEncoding;
  StartPtr: PPAnsiChar; Stop: PAnsiChar; NextPtr: PPAnsiChar;
  HaveMore: TXmlBool): TXmlError;
begin
end;

{ The idea here is to avoid using stack for each CDATA section when
  the whole file is parsed with one call. }
function CdataSectionProcessor(Parser: TXmlParser; Start, Stop: PAnsiChar;
  EndPtr: PPAnsiChar): TXmlError;
begin
end;

function DoContent(Parser: TXmlParser; StartTagLevel: Integer; Enc: PEncoding;
  S, Stop: PAnsiChar; NextPtr: PPAnsiChar; HaveMore: TXmlBool): TXmlError;
var
  TDocTypeDeclaration: PDocTypeDeclaration;
  EventPP, EventEndPP: PPAnsiChar;
  Next, RawNameEnd, FromPtr, Temp, RawName: PAnsiChar;
  Tok: TXmlTok;
  BufSize, ConvLen, Len, N: Integer;
  C, Ch: TXmlChar;
  Name, Context, ToPtr, LocalPart, TPrefix, Uri: PXmlChar;
  TEntity: PEntity;
  Result_: TXmlError;
  TTag: PTag;
  Bindings, B: PBinding;
  NoElmHandlers: TXmlBool;
  Name_: TTagName;
  Buf: array [0 .. CXmlEncodeMax - 1] of TXmlChar;
  DataPtr: PIntChar;

label
  _break;

begin
  { save one level of indirection }
  TDocTypeDeclaration := Parser.DocTypeDeclaration;

  if Enc = Parser.Encoding then
  begin
    EventPP := @Parser.EventPtr;
    EventEndPP := @Parser.EventEndPtr;
  end
  else
  begin
    EventPP := @Parser.OpenInternalEntities.InternalEventPtr;
    EventEndPP := @Parser.OpenInternalEntities.InternalEventEndPtr;
  end;

  EventPP^ := S;

  repeat
    Next := S; { XmlContentTok doesn't always set the last arg }

    Tok := XmlContentTok(Enc, Pointer(S), Pointer(Stop), @Next);

    EventEndPP^ := Next;

    case Tok of
      xtTrailingCR:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          EventEndPP^ := Stop;

          if @Parser.CharacterDataHandler <> nil then
          begin
            C := TXmlChar($A);

            Parser.CharacterDataHandler(Parser.HandlerArg, @C, 1);

          end
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Stop);

          { We are at the end of the final buffer, should we check for
            xpSuspended, xpFinished? }
          if StartTagLevel = 0 then
          begin
            Result := xeNoElements;

            Exit;
          end;

          if Parser.TagLevel <> StartTagLevel then
          begin
            Result := xeAsyncEntity;

            Exit;
          end;

          NextPtr^ := Stop;
          Result := xeNone;

          Exit;
        end;

      xtNone:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          if StartTagLevel > 0 then
          begin
            if Parser.TagLevel <> StartTagLevel then
            begin
              Result := xeAsyncEntity;

              Exit;
            end;

            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          Result := xeNoElements;

          Exit;
        end;

      xtInvalid:
        begin
          EventPP^ := Next;
          Result := xeInvalidToken;

          Exit;
        end;

      xtPartial:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          Result := xeUnclosedToken;

          Exit;
        end;

      xtPartialChar:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          Result := xePartialChar;

          Exit;
        end;

      xtEntityRef:
        begin
          Ch := TXmlChar(XmlPredefinedEntityName(Enc,
            Pointer(PtrComp(S) + Enc.MinBytesPerChar),
            Pointer(PtrComp(Next) - Enc.MinBytesPerChar)));

          if Ch <> TXmlChar(0) then
          begin
            if @Parser.CharacterDataHandler <> nil then
              Parser.CharacterDataHandler(Parser.HandlerArg, @Ch, 1)
            else if @Parser.DefaultHandler <> nil then
              ReportDefault(Parser, Enc, S, Next);

            goto _break;
          end;

          name := PoolStoreString(@TDocTypeDeclaration.Pool, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if name = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          TEntity := PEntity(Lookup(@TDocTypeDeclaration.GeneralEntities, name, 0));

          PoolDiscard(@TDocTypeDeclaration.Pool);

          { First, determine if a check for an existing declaration is needed;
            if yes, check that the TEntity exists, and that it is internal,
            otherwise call the skipped TEntity or default handler. }
          if (TDocTypeDeclaration.HasParamEntityRefs = 0) or (TDocTypeDeclaration.Standalone <> 0) then
            if TEntity = nil then
            begin
              Result := xeUndefinedEntity;

              Exit;
            end
            else if TEntity.IsInternal = 0 then
            begin
              Result := xeEntityDeclaredInPe;

              Exit;
            end
            else
          else if TEntity = nil then
          begin
            if @Parser.SkippedEntityHandler <> nil then
              Parser.SkippedEntityHandler(Parser.HandlerArg, name, 0)
            else if @Parser.DefaultHandler <> nil then
              ReportDefault(Parser, Enc, S, Next);

            goto _break;
          end;

          if TEntity.Open <> 0 then
          begin
            Result := xeRecursiveEntityRef;

            Exit;
          end;

          if TEntity.Notation <> nil then
          begin
            Result := xeBinaryEntityRef;

            Exit;
          end;

          if TEntity.TextPtr <> nil then
          begin
            if Parser.DefaultExpandInternalEntities <> 0 then
            begin
              if @Parser.SkippedEntityHandler <> nil then
                Parser.SkippedEntityHandler(Parser.HandlerArg,
                  TEntity.Name, 0)
              else if @Parser.DefaultHandler <> nil then
                ReportDefault(Parser, Enc, S, Next);

              goto _break;
            end;

            Result_ := ProcessInternalEntity(Parser, TEntity, CXmlFalse);

            if Result_ <> xeNone then
            begin
              Result := Result_;

              Exit;
            end;

          end
          else if @Parser.ExternalEntityRefHandler <> nil then
          begin
            TEntity.Open := CXmlTrue;
            Context := GetContext(Parser);
            TEntity.Open := CXmlFalse;

            if Context = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            if Parser.ExternalEntityRefHandler
              (Parser.ExternalEntityRefHandlerArg, Context, TEntity.Base,
              TEntity.SystemId, TEntity.PublicId) = 0 then
            begin
              Result := xeExternalEntityHandling;

              Exit;
            end;

            PoolDiscard(@Parser.TempPool);
          end
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

        end;

      xtStartTagNoAtts, xtStartTagWithAtts:
        begin
          if Parser.FreeTagList <> nil then
          begin
            TTag := Parser.FreeTagList;

            Parser.FreeTagList := Parser.FreeTagList.Parent;
          end
          else
          begin
            Parser.Mem.MallocFunction(Pointer(TTag), SizeOf(Expat.TTag));

            if TTag = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Parser.Mem.MallocFunction(Pointer(TTag.Buf), CInitTagBufferSize);

            if TTag.Buf = nil then
            begin
              Parser.Mem.FreeFunction(Pointer(TTag), SizeOf(Expat.TTag));

              Result := xeNoMemory;

              Exit;
            end
            else
              TTag.Alloc := CInitTagBufferSize;

            TTag.BufEnd := PAnsiChar(PtrComp(TTag.Buf) + CInitTagBufferSize);
          end;

          TTag.Bindings := nil;
          TTag.Parent := Parser.TagStack;
          Parser.TagStack := TTag;
          TTag.Name.LocalPart := nil;
          TTag.Name.TPrefix := nil;
          TTag.RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar);
          TTag.RawNameLength := XmlNameLength(Enc, Pointer(TTag.RawName));

          Inc(Parser.TagLevel);

          RawNameEnd := PAnsiChar(PtrComp(TTag.RawName) + TTag.RawNameLength);
          FromPtr := TTag.RawName;
          ToPtr := PXmlChar(TTag.Buf);

          repeat
            XmlConvert(Enc, @FromPtr, RawNameEnd, PPIntChar(@ToPtr),
              PIntChar(PtrComp(TTag.BufEnd) - 1));

            ConvLen := (PtrComp(ToPtr) - PtrComp(TTag.Buf)) div SizeOf(TXmlChar);

            if FromPtr = RawNameEnd then
            begin
              TTag.Name.StrLen := ConvLen;

              Break;
            end;

            BufSize := (PtrComp(TTag.BufEnd) - PtrComp(TTag.Buf)) shl 1;

            Parser.Mem.ReallocFunction(Pointer(TTag.Buf), TTag.Alloc, BufSize);

            if Temp = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end
            else
              TTag.Alloc := BufSize;

            TTag.Buf := Temp;
            TTag.BufEnd := PAnsiChar(PtrComp(Temp) + BufSize);

            ToPtr := PXmlChar(PtrComp(Temp) + ConvLen);
          until False;

          TTag.Name.Str := PXmlChar(TTag.Buf);

          ToPtr^ := XML_T(#0);
          Result_ := StoreAtts(Parser, Enc, S, @TTag.Name, @TTag.Bindings);

          if Result_ <> TXmlError(0) then
          begin
            Result := Result_;

            Exit;
          end;

          if @Parser.StartElementHandler <> nil then
            Parser.StartElementHandler(Parser.HandlerArg, TTag.Name.Str,
              PPXmlChar(Parser.Atts))
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          PoolClear(@Parser.TempPool);
        end;

      xtEmptyElementNoAtts, xtEmptyElementWithAtts:
        begin
          RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar);
          Bindings := nil;
          NoElmHandlers := CXmlTrue;

          Name_.Str := PoolStoreString(@Parser.TempPool, Enc, RawName,
            PAnsiChar(PtrComp(RawName) + XmlNameLength(Enc, Pointer(RawName))));

          if Name_.Str = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          PoolFinish(@Parser.TempPool);

          Result_ := StoreAtts(Parser, Enc, S, @Name_, @Bindings);

          if Result_ <> TXmlError(0) then
          begin
            Result := Result_;

            Exit;
          end;

          PoolFinish(@Parser.TempPool);

          if @Parser.StartElementHandler <> nil then
          begin
            Parser.StartElementHandler(Parser.HandlerArg, Name_.Str,
              PPXmlChar(Parser.Atts));

            NoElmHandlers := CXmlFalse;
          end;

          if @Parser.EndElementHandler <> nil then
          begin
            if @Parser.StartElementHandler <> nil then
              EventPP^ := EventEndPP^;

            Parser.EndElementHandler(Parser.HandlerArg, Name_.Str);

            NoElmHandlers := CXmlFalse;
          end;

          if (NoElmHandlers <> 0) and (@Parser.DefaultHandler <> nil) then
            ReportDefault(Parser, Enc, S, Next);

          PoolClear(@Parser.TempPool);

          while Bindings <> nil do
          begin
            B := Bindings;

            if @Parser.EndNamespaceDeclHandler <> nil then
              Parser.EndNamespaceDeclHandler(Parser.HandlerArg,
                B.TPrefix.Name);

            Bindings := Bindings.NextTagBinding;
            B.NextTagBinding := Parser.FreeBindingList;

            Parser.FreeBindingList := B;
            B.TPrefix.TBinding := B.PrevPrefixBinding;
          end;

          if Parser.TagLevel = 0 then
          begin
            Result := EpilogProcessor(Parser, Next, Stop, NextPtr);

            Exit;
          end;
        end;

      xtEndTag:
        if Parser.TagLevel = StartTagLevel then
        begin
          Result := xeAsyncEntity;

          Exit;
        end
        else
        begin
          TTag := Parser.TagStack;
          Parser.TagStack := TTag.Parent;
          TTag.Parent := Parser.FreeTagList;
          Parser.FreeTagList := TTag;

          RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar * 2);
          Len := XmlNameLength(Enc, Pointer(RawName));

          if (Len <> TTag.RawNameLength) or
            (MemCmp(Pointer(TTag.RawName), Pointer(RawName), Len) <> 0) then
          begin
            EventPP^ := RawName;
            Result := xeTagMismatch;

            Exit;
          end;

          Dec(Parser.TagLevel);

          if @Parser.EndElementHandler <> nil then
          begin
            LocalPart := TTag.Name.LocalPart;

            if (Parser.NameSpace <> 0) and (LocalPart <> nil) then
            begin
              { localPart and TPrefix may have been overwritten in
                TTag->name.str, since this points to the TBinding->uri
                buffer which gets re-used; so we have to add them again }
              Uri := PXmlChar(PtrComp(TTag.Name.Str) + TTag.Name.UriLen);

              { don't need to check for space - already done in storeAtts() }
              while LocalPart^ <> TXmlChar(0) do
              begin
                Uri^ := LocalPart^;

                Inc(PtrComp(Uri), SizeOf(TXmlChar));
                Inc(PtrComp(LocalPart), SizeOf(TXmlChar));
              end;

              TPrefix := PXmlChar(TTag.Name.TPrefix);

              if (Parser.NameSpaceTriplets <> 0) and (TPrefix <> nil) then
              begin
                Uri^ := Parser.NamespaceSeparator;

                Inc(PtrComp(Uri), SizeOf(TXmlChar));

                while TPrefix^ <> TXmlChar(0) do
                begin
                  Uri^ := TPrefix^;

                  Inc(PtrComp(Uri), SizeOf(TXmlChar));
                  Inc(PtrComp(TPrefix), SizeOf(TXmlChar));
                end;
              end;

              Uri^ := XML_T(#0);
            end;

            Parser.EndElementHandler(Parser.HandlerArg, TTag.Name.Str);
          end
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          while TTag.Bindings <> nil do
          begin
            B := TTag.Bindings;

            if @Parser.EndNamespaceDeclHandler <> nil then
              Parser.EndNamespaceDeclHandler(Parser.HandlerArg,
                B.TPrefix.Name);

            TTag.Bindings := TTag.Bindings.NextTagBinding;
            B.NextTagBinding := Parser.FreeBindingList;
            Parser.FreeBindingList := B;
            B.TPrefix.TBinding := B.PrevPrefixBinding;
          end;

          if Parser.TagLevel = 0 then
          begin
            Result := EpilogProcessor(Parser, Next, Stop, NextPtr);

            Exit;
          end;
        end;

      xtCharRef:
        begin
          N := XmlCharRefNumber(Enc, Pointer(S));

          if Integer(N) < 0 then
          begin
            Result := xeBadCharRef;

            Exit;
          end;

          if @Parser.CharacterDataHandler <> nil then
            Parser.CharacterDataHandler(Parser.HandlerArg, @Buf[0],
              XmlEncode(Integer(N), PIntChar(@Buf)))
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);
        end;

      xtXmlDecl:
        begin
          Result := xeMisplacedXmlProcessingInstruction;

          Exit;
        end;

      xtDataNewLine:
        if @Parser.CharacterDataHandler <> nil then
        begin
          C := TXmlChar($A);

          Parser.CharacterDataHandler(Parser.HandlerArg, @C, 1);
        end
        else if @Parser.DefaultHandler <> nil then
          ReportDefault(Parser, Enc, S, Next);

      xtCDataSectOpen:
        begin
          if @Parser.StartCdataSectionHandler <> nil then
            Parser.StartCdataSectionHandler(Parser.HandlerArg)
{$IFDEF 0}
            { Suppose you doing a transformation on a document that involves
              changing only the AnsiCharacter data.  You set up a defaultHandler
              and a AnsiCharacterDataHandler.  The defaultHandler simply copies
              AnsiCharacters through.  The AnsiCharacterDataHandler does the
              transformation and writes the AnsiCharacters out escaping them as
              necessary.  This case will fail to work if we leave out the
              following two lines (because & and < inside CDATA sections will
              be incorrectly escaped).

              However, now we have a start/endCdataSectionHandler, so it seems
              easier to let the user deal with this. }
          else if @Parser.CharacterDataHandler <> nil then
            Parser.CharacterDataHandler(Parser.HandlerArg,
              Parser.DataBuf, 0)
{$ENDIF}
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          Result_ := DoCdataSection(Parser, Enc, @Next, Stop, NextPtr,
            HaveMore);

          if Result_ <> xeNone then
          begin
            Result := Result_;

            Exit;
          end
          else if Next = nil then
          begin
            Parser.TProcessor := @CdataSectionProcessor;

            Result := Result_;

            Exit;
          end;

        end;

      xtTrailingRSQB:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := xeNone;

            Exit;
          end;

          if @Parser.CharacterDataHandler <> nil then
            if MUSRasterizerConverterERT(Enc, S) <> 0 then
            begin
              DataPtr := PIntChar(Parser.DataBuf);

              XmlConvert(Enc, @S, Stop, @DataPtr,
                PIntChar(Parser.DataBufEnd));

              Parser.CharacterDataHandler(Parser.HandlerArg,
                Parser.DataBuf, (PtrComp(DataPtr) - PtrComp(Parser.DataBuf))
                div SizeOf(IntChar));
            end
            else
              Parser.CharacterDataHandler(Parser.HandlerArg,
                PXmlChar(S), (PtrComp(Stop) - PtrComp(S))
                div SizeOf(TXmlChar))
          else if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Stop);

          { We are at the end of the final buffer, should we check for
            xpSuspended, xpFinished? }
          if StartTagLevel = 0 then
          begin
            EventPP^ := Stop;
            Result := xeNoElements;

            Exit;
          end;

          if Parser.TagLevel <> StartTagLevel then
          begin
            EventPP^ := Stop;
            Result := xeAsyncEntity;

            Exit;
          end;

          NextPtr^ := Stop;
          Result := xeNone;

          Exit;
        end;

      xtDataChars:
        if @Parser.CharacterDataHandler <> nil then
          if MUSRasterizerConverterERT(Enc, S) <> 0 then
            repeat
              DataPtr := PIntChar(Parser.DataBuf);

              XmlConvert(Enc, @S, Next, @DataPtr,
                PIntChar(Parser.DataBufEnd));

              EventEndPP^ := S;

              Parser.CharacterDataHandler(Parser.HandlerArg,
                Parser.DataBuf, (PtrComp(DataPtr) - PtrComp(Parser.DataBuf))
                div SizeOf(IntChar));

              if S = Next then
                Break;

              EventPP^ := S;
            until False
          else
            Parser.CharacterDataHandler(Parser.HandlerArg, PXmlChar(S),
              (PtrComp(Next) - PtrComp(S)) div SizeOf(TXmlChar))

        else if @Parser.DefaultHandler <> nil then
          ReportDefault(Parser, Enc, S, Next);

      xtProcessingInstruction:
        if ReportProcessingInstruction(Parser, Enc, S, Next) = 0 then
        begin
          Result := xeNoMemory;

          Exit;
        end;

      xtComment:
        if ReportComment(Parser, Enc, S, Next) = 0 then
        begin
          Result := xeNoMemory;

          Exit;
        end;

    else
      if @Parser.DefaultHandler <> nil then
        ReportDefault(Parser, Enc, S, Next);
    end;

  _break:
    EventPP^ := Next;
    S := Next;

    case Parser.ParsingStatus.Parsing of
      xpSuspended:
        begin
          NextPtr^ := Next;
          Result := xeNone;

          Exit;
        end;

      xpFinished:
        begin
          Result := xeAborted;

          Exit;
        end;
    end;
  until False;

  { not reached }
end;

function ContentProcessor(Parser: TXmlParser; Start, Stop: PAnsiChar;
  EndPtr: PPAnsiChar): TXmlError;
begin
  Result := DoContent(Parser, 0, Parser.Encoding, Start, Stop, EndPtr,
    TXmlBool(not Parser.ParsingStatus.FinalBuffer));

  if Result = xeNone then
    if StoreRawNames(Parser) = 0 then
      Result := xeNoMemory;
end;

function GetElementType(Parser: TXmlParser; Enc: PEncoding;
  Ptr, Stop: PAnsiChar): PElementType;
begin
end;

function GetAttributeId(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): PAttributeID;
var
  DocTypeDeclaration: PDocTypeDeclaration;
  Id: PAttributeID;
  Name: PXmlChar;
  I, J: Integer;
begin
  Result := nil;

  { save one level of indirection }
  DocTypeDeclaration := Parser.DocTypeDeclaration;

  if PoolAppendChar(@DocTypeDeclaration.Pool, XML_T(#0)) = 0 then
    Exit;

  Name := PoolStoreString(@DocTypeDeclaration.Pool, Enc, Start, Stop);

  if Name = nil then
    Exit;

  { skip quotation mark - its storage will be re-used (like in name[-1]) }
  Inc(PtrComp(name), SizeOf(TXmlChar));

  Id := PAttributeID(Lookup(@DocTypeDeclaration.AttributeIds, name, SizeOf(TAttributeID)));
  if Id = nil then
    Exit;

  if Id.Name <> name then
    PoolDiscard(@DocTypeDeclaration.Pool)
  else
  begin
    PoolFinish(@DocTypeDeclaration.Pool);

    if Parser.NameSpace = 0 then
    else if (PXmlChar(PtrComp(name) + 0 * SizeOf(TXmlChar))^ = XML_T('x'))
      and (PXmlChar(PtrComp(name) + 1 * SizeOf(TXmlChar))^ = XML_T('m'))
      and (PXmlChar(PtrComp(name) + 2 * SizeOf(TXmlChar))^ = XML_T('l'))
      and (PXmlChar(PtrComp(name) + 3 * SizeOf(TXmlChar))^ = XML_T('n'))
      and (PXmlChar(PtrComp(name) + 4 * SizeOf(TXmlChar))^ = XML_T('s'))
      and ((PXmlChar(PtrComp(name) + 5 * SizeOf(TXmlChar))^ = XML_T(#0)) or
      (PXmlChar(PtrComp(name) + 5 * SizeOf(TXmlChar))^ = XML_T(':'))) then
    begin
      if PXmlChar(PtrComp(name) + 5 * SizeOf(TXmlChar))^ = XML_T(#0) then
        Id.TPrefix := @DocTypeDeclaration.DefaultPrefix
      else
        Id.TPrefix := PPrefix(Lookup(@DocTypeDeclaration.Prefixes,
          PXmlChar(PtrComp(name) + 6 * SizeOf(TXmlChar)), SizeOf(TPrefix)));

      Id.Xmlns := CXmlTrue;
    end
    else
    begin
      I := 0;

      while PXmlChar(PtrComp(name) + I * SizeOf(TXmlChar))^ <> TXmlChar(0) do
      begin
        { attributes without TPrefix are *not* in the default namespace }
        if PXmlChar(PtrComp(name) + I * SizeOf(TXmlChar))^ = XML_T(':') then
        begin
          J := 0;

          while J < I do
          begin
            if PoolAppendChar(@DocTypeDeclaration.Pool,
              PXmlChar(PtrComp(name) + J * SizeOf(TXmlChar))^) = 0 then
              Exit;

            Inc(J);
          end;

          if PoolAppendChar(@DocTypeDeclaration.Pool, XML_T(#0)) = 0 then
            Exit;

          Id.TPrefix := PPrefix(Lookup(@DocTypeDeclaration.Prefixes, PoolStart(@DocTypeDeclaration.Pool),
            SizeOf(TPrefix)));

          if Id.TPrefix.Name = PoolStart(@DocTypeDeclaration.Pool) then
            PoolFinish(@DocTypeDeclaration.Pool)
          else
            PoolDiscard(@DocTypeDeclaration.Pool);

          Break;
        end;

        Inc(I);
      end;
    end;
  end;

  Result := Id;
end;

function DefineAttribute(ContentType: PElementType; AttId: PAttributeID;
  IsCdata, IsId: TXmlBool; Value: PXmlChar; Parser: TXmlParser): Integer;
begin
  Result := 0;
end;

function AppendAttributeValue(Parser: TXmlParser; Enc: PEncoding;
  IsCdata: TXmlBool; Ptr, Stop: PAnsiChar; Pool: PStringPool): TXmlError;
var
  TDocTypeDeclaration: PDocTypeDeclaration;
  Next: PAnsiChar;
  Tok: TXmlTok;
  I, N: Integer;
  Buf: array [0 .. CXmlEncodeMax - 1] of TXmlChar;
  Name, TextEnd: PXmlChar;
  TEntity: PEntity;
  CheckEntityDecl: AnsiChar;
  Ch: TXmlChar;
  Result_: TXmlError;

label
  _break, _go0;

begin
  { save one level of indirection }
  TDocTypeDeclaration := Parser.DocTypeDeclaration;

  repeat
    Tok := XmlAttributeValueTok(Enc, Pointer(Ptr), Pointer(Stop), @Next);

    case Tok of
      xtNone:
        begin
          Result := xeNone;

          Exit;
        end;

      xtInvalid:
        begin
          if Enc = Parser.Encoding then
            Parser.EventPtr := Next;

          Result := xeInvalidToken;
        end;

      xtPartial:
        begin
          if Enc = Parser.Encoding then
            Parser.EventPtr := Ptr;

          Result := xeInvalidToken;
        end;

      xtCharRef:
        begin
          N := XmlCharRefNumber(Enc, Pointer(Ptr));

          if N < 0 then
          begin
            if Enc = Parser.Encoding then
              Parser.EventPtr := Ptr;

            Result := xeBadCharRef;
          end;

          if (IsCdata = 0) and (N = $20) and { space }
            ((PoolLength(Pool) = 0) or (PoolLastChar(Pool) = TXmlChar($20)))
          then
            goto _break;

          N := XmlEncode(N, PIntChar(@Buf[0]));

          if N = 0 then
          begin
            if Enc = Parser.Encoding then
              Parser.EventPtr := Ptr;

            Result := xeBadCharRef;

            Exit;
          end;

          I := 0;

          while I < N do
          begin
            if PoolAppendChar(Pool, Buf[I]) = 0 then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Inc(I);
          end;
        end;

      xtDataChars:
        if PoolAppend(Pool, Enc, Ptr, Next) = nil then
        begin
          Result := xeNoMemory;

          Exit;
        end;

      xtTrailingCR:
        begin
          Next := PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar);

          goto _go0;
        end;

      xtAttributeValue_S, xtDataNewLine:
      _go0:
        begin
          if (IsCdata = 0) and
            ((PoolLength(Pool) = 0) or (PoolLastChar(Pool) = TXmlChar($20)))
          then
            goto _break;

          if PoolAppendChar(Pool, AnsiChar($20)) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;
        end;

      xtEntityRef:
        begin
          Ch := TXmlChar(XmlPredefinedEntityName(Enc,
            Pointer(PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar)),
            Pointer(PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar))));

          if Ch <> TXmlChar(0) then
          begin
            if PoolAppendChar(Pool, Ch) = 0 then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            goto _break;
          end;

          name := PoolStoreString(@Parser.Temp2Pool, Enc,
            PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if name = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          TEntity := PEntity(Lookup(@Parser.DocTypeDeclaration.GeneralEntities, name, 0));

          PoolDiscard(@Parser.Temp2Pool);

          { First, determine if a check for an existing declaration is needed;
            if yes, check that the TEntity exists, and that it is internal. }
          if Pool = @Parser.DocTypeDeclaration.Pool then { are we called from prolog? }
          begin
            if TDocTypeDeclaration.Standalone <> 0 then
              CheckEntityDecl := AnsiChar(Parser.OpenInternalEntities = nil)
            else
              CheckEntityDecl := AnsiChar(TDocTypeDeclaration.HasParamEntityRefs = 0);

{$IFDEF XML_DTD}
            CheckEntityDecl := AnsiChar((CheckEntityDecl <> #0) and
              (Parser.PrologState.DocumentEntity <> 0))
{$ENDIF}
          end
          else { if pool = @tempPool: we are called from content }
            CheckEntityDecl := AnsiChar((TDocTypeDeclaration.HasParamEntityRefs = 0) or
              (TDocTypeDeclaration.Standalone <> 0));

          if CheckEntityDecl <> #0 then
            if TEntity = nil then
            begin
              Result := xeUndefinedEntity;

              Exit;
            end
            else if TEntity.IsInternal = 0 then
            begin
              Result := xeEntityDeclaredInPe;

              Exit;
            end
            else
          else if TEntity = nil then
            { Cannot report skipped TEntity here - see comments on
              skippedEntityHandler.
              if @parser.SkippedEntityHandler <> NIL then
              parser.SkippedEntityHandler(parser.HandlerArg ,name ,0 ); }
            { Cannot call the default handler because this would be
              out of sync with the call to the startElementHandler.
              if (pool = @parser.TempPool ) and
              (@parser.DefaultHandler <> NIL ) then
              reportDefault(parser ,enc ,ptr ,next ); }
            goto _break;

          if TEntity.Open <> 0 then
          begin
            if Enc = Parser.Encoding then
              Parser.EventPtr := Ptr;

            Result := xeRecursiveEntityRef;

            Exit;
          end;

          if TEntity.Notation <> nil then
          begin
            if Enc = Parser.Encoding then
              Parser.EventPtr := Ptr;

            Result := xeBinaryEntityRef;

            Exit;
          end;

          if TEntity.TextPtr = nil then
          begin
            if Enc = Parser.Encoding then
              Parser.EventPtr := Ptr;

            Result := xeAttributeExternalEntityRef;

            Exit;
          end
          else
          begin
            TextEnd := PXmlChar(PtrComp(TEntity.TextPtr) + TEntity.TextLen *
              SizeOf(TXmlChar));

            TEntity.Open := CXmlTrue;

            Result_ := AppendAttributeValue(Parser, Parser.InternalEncoding,
              IsCdata, PAnsiChar(TEntity.TextPtr), PAnsiChar(TextEnd), Pool);

            TEntity.Open := CXmlFalse;

            if Result_ <> TXmlError(0) then
            begin
              Result := Result_;

              Exit;
            end;
          end;
        end;
    else
      begin
        if Enc = Parser.Encoding then
          Parser.EventPtr := Ptr;

        Result := xeUnexpectedState;

        Exit;
      end;
    end;

  _break:
    Ptr := Next;

  until False;

  { not reached }
end;

function StoreAttributeValue(Parser: TXmlParser; Enc: PEncoding;
  IsCdata: TXmlBool; Ptr, Stop: PAnsiChar; Pool: PStringPool): TXmlError;
begin
  Result := AppendAttributeValue(Parser, Enc, IsCdata, Ptr, Stop, Pool);

  if Result <> TXmlError(0) then
    Exit;

  if (IsCdata = 0) and (PoolLength(Pool) <> 0) and
    (PoolLastChar(Pool) = TXmlChar($20)) then
    PoolChop(Pool);

  if PoolAppendChar(Pool, XML_T(#0)) = 0 then
  begin
    Result := xeNoMemory;

    Exit;
  end;

  Result := xeNone;
end;

function StoreEntityValue(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): TXmlError;
begin
end;

{ startPtr gets set to non-null is the section is closed, and to null
  if the section is not yet closed. }
function DoIgnoreSection(Parser: TXmlParser; Enc: PEncoding;
  StartPtr: PPAnsiChar; Stop: PAnsiChar; NextPtr: PPAnsiChar;
  HaveMore: TXmlBool): TXmlError;
begin
end;

{ The idea here is to avoid using stack for each IGNORE section when
  the whole file is parsed with one call. }
function IgnoreSectionProcessor(Parser: TXmlParser; Start, Stop: PAnsiChar;
  EndPtr: PPAnsiChar): TXmlError;
begin
end;

function NextScaffoldPart(Parser: TXmlParser): Integer;
begin
end;

function BuildModel(Parser: TXmlParser): PXmlContent;
begin
end;

function ReportProcessingInstruction(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): Integer;
begin
end;

procedure NormalizeLines(S: PXmlChar);
begin
end;

function ReportComment(Parser: TXmlParser; Enc: PEncoding;
  Start, Stop: PAnsiChar): Integer;
var
  Data: PXmlChar;
begin
  Result := 1;
  if @Parser.CommentHandler = nil then
  begin
    if @Parser.DefaultHandler <> nil then
      ReportDefault(Parser, Enc, Start, Stop);

    Exit;
  end;

  Data := PoolStoreString(@Parser.TempPool, Enc,
    PAnsiChar(PtrComp(Start) + Enc.MinBytesPerChar * 4),
    PAnsiChar(PtrComp(Stop) - Enc.MinBytesPerChar * 3));

  if Data = nil then
  begin
    Result := 0;
    Exit;
  end;

  NormalizeLines(Data);

  Parser.CommentHandler(Parser.HandlerArg, Data);

  PoolClear(@Parser.TempPool);
end;

function DoProlog(Parser: TXmlParser; Enc: PEncoding; S, Stop: PAnsiChar;
  Tok: TXmlTok; Next: PAnsiChar; NextPtr: PPAnsiChar; HaveMore: TXmlBool)
  : TXmlError;
const
{$IFDEF XML_DTD}
  ExternalSubsetName: array [0 .. 1] of TXmlChar = ('#', #0);
{$ENDIF}
  AtypeCDATA: array [0 .. 5] of TXmlChar = ('C', 'D', 'A', 'T', 'A', #0);
  AtypeID: array [0 .. 2] of TXmlChar = ('I', 'D', #0);
  AtypeIDREF: array [0 .. 5] of TXmlChar = ('I', 'D', 'R', 'E', 'F', #0);
  AtypeIDREFS: array [0 .. 6] of TXmlChar = ('I', 'D', 'R', 'E', 'F', 'S', #0);
  AtypeENTITY: array [0 .. 6] of TXmlChar = ('E', 'N', 'T', 'I', 'T', 'Y', #0);
  AtypeENTITIES: array [0 .. 8] of TXmlChar = ('E', 'N', 'T', 'I', 'T', 'I',
    'E', 'S', #0);
  AtypeNMTOKEN: array [0 .. 7] of TXmlChar = ('N', 'M', 'T', 'O', 'K',
    'E', 'N', #0);
  AtypeNMTOKENS: array [0 .. 8] of TXmlChar = ('N', 'M', 'T', 'O', 'K', 'E',
    'N', 'S', #0);
  NotationPrefix: array [0 .. 8] of TXmlChar = ('N', 'O', 'T', 'A', 'T', 'I',
    'O', 'N', #0);
  EnumValueSep: array [0 .. 1] of TXmlChar = ('|', #0);
  EnumValueStart: array [0 .. 1] of TXmlChar = ('(', #0);

var
  TDocTypeDeclaration: PDocTypeDeclaration;
  EventPP, EventEndPP: PPAnsiChar;
  Quant: TXmlContentQuant;
  Role: TXmlRole;
  Myindex, NameLen: Integer;
  HandleDefault, HadParamEntityRefs, Ok, BetweenDecl: TXmlBool;
  Result_: TXmlError;
  Tem, TPrefix, AttVal, Name, SystemId: PXmlChar;
  TEntity: PEntity;
  Nxt: PAnsiChar;
  Content, Model: PXmlContent;
  El: PElementType;

label
  _break, _go0, _go1,
  AlreadyChecked, CheckAttListDeclHandler, ElementContent, CloseGroup;

begin
  { save one level of indirection }
  TDocTypeDeclaration := Parser.DocTypeDeclaration;

  if Enc = Parser.Encoding then
  begin
    EventPP := @Parser.EventPtr;
    EventEndPP := @Parser.EventEndPtr;
  end
  else
  begin
    EventPP := @Parser.OpenInternalEntities.InternalEventPtr;
    EventEndPP := @Parser.OpenInternalEntities.InternalEventEndPtr;
  end;

  repeat
    HandleDefault := CXmlTrue;
    EventPP^ := S;
    EventEndPP^ := Next;

    if Integer(Tok) <= 0 then
    begin
      if (HaveMore <> 0) and (Tok <> xtInvalid) then
      begin
        NextPtr^ := S;
        Result := xeNone;

        Exit;
      end;

      case Tok of
        xtInvalid:
          begin
            EventPP^ := Next;
            Result := xeInvalidToken;

            Exit;
          end;

        xtPartial:
          begin
            Result := xeUnclosedToken;

            Exit;
          end;

        xtPartialChar:
          begin
            Result := xePartialChar;

            Exit;
          end;

        xtNone:
          begin
{$IFDEF XML_DTD}
            { for internal PE NOT referenced between declarations }
            if (Enc <> Parser.Encoding) and
              (Parser.OpenInternalEntities.BetweenDecl = 0) then
            begin
              NextPtr^ := S;
              Result := xeNone;

              Exit;
            end;

            { WFC: PE Between Declarations - must check that PE contains
              complete markup, not only for external PEs, but also for
              internal PEs if the reference occurs between declarations. }
            if (Parser.IsParamEntity <> 0) or (Enc <> Parser.Encoding) then
            begin
              if XmlTokenRole(@Parser.PrologState, xtNone,
                Pointer(Stop), Pointer(Stop), Enc) = xrError then
              begin
                Result := xeIncompletePe;

                Exit;
              end;

              NextPtr^ := S;
              Result := xeNone;

              Exit;
            end;
{$ENDIF}

            Result := xeNoElements;

            Exit;
          end;

      else
        begin
          Tok := TXmlTok(-Integer(Tok));
          Next := Stop;
        end;
      end;
    end;

    Role := XmlTokenRole(@Parser.PrologState, Tok, Pointer(S),
      Pointer(Next), Enc);

    case Role of
      xrXmlDecl:
        begin
          Result_ := ProcessXmlDecl(Parser, 0, S, Next);

          if Result_ <> xeNone then
          begin
            Result := Result_;

            Exit;
          end;

          Enc := Parser.Encoding;
          HandleDefault := CXmlFalse;
        end;

      xrDocTypeName:
        begin
          if @Parser.StartDoctypeDeclHandler <> nil then
          begin
            Parser.DoctypeName := PoolStoreString(@Parser.TempPool,
              Enc, S, Next);

            if Parser.DoctypeName = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            PoolFinish(@Parser.TempPool);

            Parser.DoctypePubid := nil;
            HandleDefault := CXmlFalse;
          end;

          Parser.DoctypeSysid := nil; { always initialize to NULL }
        end;

      xrDocTypeInternalSubset:
        if @Parser.StartDoctypeDeclHandler <> nil then
        begin
          Parser.StartDoctypeDeclHandler(Parser.HandlerArg,
            Parser.DoctypeName, Parser.DoctypeSysid,
            Parser.DoctypePubid, 1);

          Parser.DoctypeName := nil;
          PoolClear(@Parser.TempPool);
          HandleDefault := CXmlFalse;
        end;

{$IFDEF XML_DTD}
      xrTextDecl:
        begin
          Result_ := ProcessXmlDecl(Parser, 1, S, Next);

          if Result_ <> xeNone then
          begin
            Result := Result_;

            Exit;
          end;

          Enc := Parser.Encoding;
          HandleDefault := CXmlFalse;
        end;
{$ENDIF}

      xrDocTypePublicID:
        begin
{$IFDEF XML_DTD}
          Parser.UseForeignDTD := CXmlFalse;
          Parser.DeclEntity :=
            PEntity(Lookup(@TDocTypeDeclaration.ParamEntities, @ExternalSubsetName[0],
            SizeOf(Expat.TEntity)));

          if Parser.DeclEntity = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;
{$ENDIF}

          TDocTypeDeclaration.HasParamEntityRefs := CXmlTrue;

          if @Parser.StartDoctypeDeclHandler <> nil then
          begin
            if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
            then
            begin
              Result := xePublicID;

              Exit;
            end;

            Parser.DoctypePubid := PoolStoreString(@Parser.TempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Parser.DoctypePubid = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            NormalizePublicId(PXmlChar(Parser.DoctypePubid));
            PoolFinish(@Parser.TempPool);

            HandleDefault := CXmlFalse;

            goto AlreadyChecked;
          end;

          { fall through }
          goto _go0;
        end;

      xrEntityPublicID:
      _go0:
        begin
          if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
          then
          begin
            Result := xePublicID;

            Exit;
          end;

        AlreadyChecked:
          if (TDocTypeDeclaration.KeepProcessing <> 0) and (Parser.DeclEntity <> nil) then
          begin
            Tem := PoolStoreString(@TDocTypeDeclaration.Pool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Tem = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            NormalizePublicId(Tem);
            Parser.DeclEntity.PublicId := Tem;
            PoolFinish(@TDocTypeDeclaration.Pool);

            if @Parser.EntityDeclHandler <> nil then
              HandleDefault := CXmlFalse;
          end;
        end;

      xrDocTypeClose:
        begin
          if Parser.DoctypeName <> nil then
          begin
            Parser.StartDoctypeDeclHandler(Parser.HandlerArg,
              Parser.DoctypeName, Parser.DoctypeSysid,
              Parser.DoctypePubid, 0);

            PoolClear(@Parser.TempPool);
            HandleDefault := CXmlFalse;
          end;

          { doctypeSysid will be non-NULL in the case of a previous
            xrDocTypeSYSTEM_ID, even if startDoctypeDeclHandler
            was not set, indicating an external subset }
{$IFDEF XML_DTD }
          if (Parser.DoctypeSysid <> nil) or (Parser.UseForeignDTD <> 0)
          then
          begin
            HadParamEntityRefs := TDocTypeDeclaration.HasParamEntityRefs;
            TDocTypeDeclaration.HasParamEntityRefs := CXmlTrue;

            if (Parser.ParamEntityParsing <> TXmlParamEntityParsing(0)) and
              (@Parser.ExternalEntityRefHandler <> nil) then
            begin
              TEntity := PEntity(Lookup(@TDocTypeDeclaration.ParamEntities,
                @ExternalSubsetName[0], SizeOf(Expat.TEntity)));

              if TEntity = nil then
              begin
                Result := xeNoMemory;

                Exit;
              end;

              if Parser.UseForeignDTD <> 0 then
                TEntity.Base := Parser.CurBase;

              TDocTypeDeclaration.ParamEntityRead := CXmlFalse;

              if Parser.ExternalEntityRefHandler
                (Parser.ExternalEntityRefHandlerArg, nil, TEntity.Base,
                TEntity.SystemId, TEntity.PublicId) = 0 then
              begin
                Result := xeExternalEntityHandling;

                Exit;
              end;

              if TDocTypeDeclaration.ParamEntityRead <> 0 then
                if (TDocTypeDeclaration.Standalone = 0) and
                  (@Parser.NotStandaloneHandler <> nil) and
                  (Parser.NotStandaloneHandler(Parser.HandlerArg) = 0) then
                begin
                  Result := xeNotStandalone;

                  Exit;
                end
                else
              else
                { if we didn't read the foreign DTD then this means that there
                  is no external subset and we must reset TDocTypeDeclaration.hasParamEntityRefs }
                if Parser.DoctypeSysid = nil then
                  TDocTypeDeclaration.HasParamEntityRefs := HadParamEntityRefs;

              { end of TDocTypeDeclaration - no need to update TDocTypeDeclaration.keepProcessing }
            end;

            Parser.UseForeignDTD := CXmlFalse;
          end;
{$ENDIF}

          if @Parser.EndDoctypeDeclHandler <> nil then
          begin
            Parser.EndDoctypeDeclHandler(Parser.HandlerArg);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrInstanceStart:
        begin
{$IFDEF XML_DTD}
          { if there is no DOCTYPE declaration then now is the
            last chance to read the foreign TDocTypeDeclaration }
          if Parser.UseForeignDTD <> 0 then
          begin
            HadParamEntityRefs := TDocTypeDeclaration.HasParamEntityRefs;
            TDocTypeDeclaration.HasParamEntityRefs := CXmlTrue;

            if (Parser.ParamEntityParsing <> TXmlParamEntityParsing(0)) and
              (@Parser.ExternalEntityRefHandler <> nil) then
            begin
              TEntity := PEntity(Lookup(@TDocTypeDeclaration.ParamEntities,
                @ExternalSubsetName[0], SizeOf(Expat.TEntity)));

              if TEntity = nil then
              begin
                Result := xeNoMemory;

                Exit;
              end;

              TEntity.Base := Parser.CurBase;
              TDocTypeDeclaration.ParamEntityRead := CXmlFalse;

              if Parser.ExternalEntityRefHandler
                (Parser.ExternalEntityRefHandlerArg, nil, TEntity.Base,
                TEntity.SystemId, TEntity.PublicId) = 0 then
              begin
                Result := xeExternalEntityHandling;

                Exit;
              end;

              if TDocTypeDeclaration.ParamEntityRead <> 0 then
                if (TDocTypeDeclaration.Standalone = 0) and
                  (@Parser.NotStandaloneHandler <> nil) and
                  (Parser.NotStandaloneHandler(Parser.HandlerArg) = 0) then
                begin
                  Result := xeNotStandalone;

                  Exit;
                end
                else
              else
                { if we didn't read the foreign DTD then this means that there
                  is no external subset and we must reset TDocTypeDeclaration.hasParamEntityRefs }
                TDocTypeDeclaration.HasParamEntityRefs := HadParamEntityRefs;

              { end of TDocTypeDeclaration - no need to update TDocTypeDeclaration.keepProcessing }
            end;
          end;

{$ENDIF}
          Parser.TProcessor := @ContentProcessor;
          Result := ContentProcessor(Parser, S, Stop, NextPtr);

          Exit;
        end;

      xrAttributeListElementName:
        begin
          Parser.DeclElementType := GetElementType(Parser, Enc, S, Next);

          if Parser.DeclElementType = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          goto CheckAttListDeclHandler;
        end;

      xrAttributeName:
        begin
          Parser.DeclAttributeId := GetAttributeId(Parser, Enc, S, Next);

          if Parser.DeclAttributeId = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          Parser.DeclAttributeIsCdata := CXmlFalse;
          Parser.DeclAttributeType := nil;
          Parser.DeclAttributeIsId := CXmlFalse;

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_CDATA:
        begin
          Parser.DeclAttributeIsCdata := CXmlTrue;
          Parser.DeclAttributeType := @AtypeCDATA[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_ID:
        begin
          Parser.DeclAttributeIsId := CXmlTrue;
          Parser.DeclAttributeType := @AtypeID[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_IDREF:
        begin
          Parser.DeclAttributeType := @AtypeIDREF[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_IDREFS:
        begin
          Parser.DeclAttributeType := @AtypeIDREFS[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_ENTITY:
        begin
          Parser.DeclAttributeType := @AtypeENTITY[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_ENTITIES:
        begin
          Parser.DeclAttributeType := @AtypeENTITIES[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_NMTOKEN:
        begin
          Parser.DeclAttributeType := @AtypeNMTOKEN[0];

          goto CheckAttListDeclHandler;
        end;

      xrAttributeType_NMTOKENS:
        begin
          Parser.DeclAttributeType := @AtypeNMTOKENS[0];

        CheckAttListDeclHandler:
          if (TDocTypeDeclaration.KeepProcessing <> 0) and (@Parser.AttlistDeclHandler <> nil)
          then
            HandleDefault := CXmlFalse;
        end;

      xrAttributeEnumValue, xrAttributeNotationValue:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (@Parser.AttlistDeclHandler <> nil)
        then
        begin
          if Parser.DeclAttributeType <> nil then
            TPrefix := @EnumValueSep[0]

          else if Role = xrAttributeNotationValue then
            TPrefix := @NotationPrefix[0]
          else
            TPrefix := @EnumValueStart[0];

          if PoolAppendString(@Parser.TempPool, TPrefix) = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          if PoolAppend(@Parser.TempPool, Enc, S, Next) = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          Parser.DeclAttributeType := Parser.TempPool.Start;

          HandleDefault := CXmlFalse;
        end;

      xrImpliedAttributeValue, xrRequiredAttributeValue:
        if TDocTypeDeclaration.KeepProcessing <> 0 then
        begin
          if DefineAttribute(Parser.DeclElementType, Parser.DeclAttributeId,
            Parser.DeclAttributeIsCdata, Parser.DeclAttributeIsId, nil,
            Parser) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          if (@Parser.AttlistDeclHandler <> nil) and
            (Parser.DeclAttributeType <> nil) then
          begin
            if (Parser.DeclAttributeType^ = XML_T('(')) or
              ((Parser.DeclAttributeType^ = XML_T('N')) and
              (PXmlChar(PtrComp(Parser.DeclAttributeType) + 1)
              ^ = XML_T('O'))) then
            begin
              { Enumerated or Notation type }
              if (PoolAppendChar(@Parser.TempPool, XML_T(')')) = 0) or
                (PoolAppendChar(@Parser.TempPool, XML_T(#0)) = 0) then
              begin
                Result := xeNoMemory;

                Exit;
              end;

              Parser.DeclAttributeType := Parser.TempPool.Start;

              PoolFinish(@Parser.TempPool);
            end;

            EventEndPP^ := S;

            Parser.AttlistDeclHandler(Parser.HandlerArg,
              Parser.DeclElementType.Name, Parser.DeclAttributeId.Name,
              Parser.DeclAttributeType, nil,
              Integer(Role = xrRequiredAttributeValue));

            PoolClear(@Parser.TempPool);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrDefaultAttributeValue, xrFixedAttributeValue:
        if TDocTypeDeclaration.KeepProcessing <> 0 then
        begin
          Result_ := StoreAttributeValue(Parser, Enc,
            Parser.DeclAttributeIsCdata,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar), @TDocTypeDeclaration.Pool);

          if Result_ <> TXmlError(0) then
          begin
            Result := Result_;

            Exit;
          end;

          AttVal := PoolStart(@TDocTypeDeclaration.Pool);

          PoolFinish(@TDocTypeDeclaration.Pool);

          { ID attributes aren't alLowed to have a default }
          if DefineAttribute(Parser.DeclElementType, Parser.DeclAttributeId,
            Parser.DeclAttributeIsCdata, CXmlFalse, AttVal, Parser) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          if (@Parser.AttlistDeclHandler <> nil) and
            (Parser.DeclAttributeType <> nil) then
          begin
            if (Parser.DeclAttributeType^ = XML_T('(')) or
              ((Parser.DeclAttributeType^ = XML_T('N')) and
              (PXmlChar(PtrComp(Parser.DeclAttributeType) + 1)
              ^ = XML_T('O'))) then
            begin
              { Enumerated or Notation type }
              if (PoolAppendChar(@Parser.TempPool, XML_T(')')) = 0) or
                (PoolAppendChar(@Parser.TempPool, XML_T(#0)) = 0) then
              begin
                Result := xeNoMemory;

                Exit;
              end;

              Parser.DeclAttributeType := Parser.TempPool.Start;

              PoolFinish(@Parser.TempPool);

            end;

            EventEndPP^ := S;

            Parser.AttlistDeclHandler(Parser.HandlerArg,
              Parser.DeclElementType.Name, Parser.DeclAttributeId.Name,
              Parser.DeclAttributeType, AttVal,
              Integer(Role = xrFixedAttributeValue));

            PoolClear(@Parser.TempPool);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrEntityValue:
        if TDocTypeDeclaration.KeepProcessing <> 0 then
        begin
          Result_ := StoreEntityValue(Parser, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if Parser.DeclEntity <> nil then
          begin
            Parser.DeclEntity.TextPtr := PoolStart(@TDocTypeDeclaration.EntityValuePool);
            Parser.DeclEntity.TextLen := PoolLength(@TDocTypeDeclaration.EntityValuePool);

            PoolFinish(@TDocTypeDeclaration.EntityValuePool);

            if @Parser.EntityDeclHandler <> nil then
            begin
              EventEndPP^ := S;

              Parser.EntityDeclHandler(Parser.HandlerArg,
                Parser.DeclEntity.Name, Parser.DeclEntity.IsParam,
                Parser.DeclEntity.TextPtr, Parser.DeclEntity.TextLen,
                Parser.CurBase, nil, nil, nil);

              HandleDefault := CXmlFalse;
            end;
          end
          else
            PoolDiscard(@TDocTypeDeclaration.EntityValuePool);

          if Result_ <> xeNone then
          begin
            Result := Result_;

            Exit;
          end;
        end;

      xrDocTypeSystemID:
        begin
{$IFDEF XML_DTD}
          Parser.UseForeignDTD := CXmlFalse;
{$ENDIF}

          TDocTypeDeclaration.HasParamEntityRefs := CXmlTrue;

          if @Parser.StartDoctypeDeclHandler <> nil then
          begin
            Parser.DoctypeSysid := PoolStoreString(@Parser.TempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Parser.DoctypeSysid = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            PoolFinish(@Parser.TempPool);

            HandleDefault := CXmlFalse;
          end
{$IFDEF XML_DTD}
          else
            { use externalSubsetName to make doctypeSysid non-NULL
              for the case where no startDoctypeDeclHandler is set }
            Parser.DoctypeSysid := @ExternalSubsetName[0];
{$ELSE}; {$ENDIF}

          if (TDocTypeDeclaration.Standalone = 0) and
{$IFDEF XML_DTD}
            (Parser.ParamEntityParsing = TXmlParamEntityParsing(0)) and
{$ENDIF}
            (@Parser.NotStandaloneHandler <> nil) and
            (Parser.NotStandaloneHandler(Parser.HandlerArg) = 0) then
          begin
            Result := xeNotStandalone;

            Exit;
          end;

{$IFNDEF XML_DTD}
{$ELSE}
          if Parser.DeclEntity = nil then
          begin
            Parser.DeclEntity :=
              PEntity(Lookup(@TDocTypeDeclaration.ParamEntities, @ExternalSubsetName[0],
              SizeOf(Expat.TEntity)));

            if Parser.DeclEntity = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Parser.DeclEntity.PublicId := nil;
          end;
{$ENDIF}

          { fall through }
          goto _go1;
        end;

      xrEntitySystemID:
      _go1:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (Parser.DeclEntity <> nil) then
        begin
          Parser.DeclEntity.SystemId := PoolStoreString(@TDocTypeDeclaration.Pool, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if Parser.DeclEntity.SystemId = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          Parser.DeclEntity.Base := Parser.CurBase;

          PoolFinish(@TDocTypeDeclaration.Pool);

          if @Parser.EntityDeclHandler <> nil then
            HandleDefault := CXmlFalse;
        end;

      xrEntityComplete:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (Parser.DeclEntity <> nil) and
          (@Parser.EntityDeclHandler <> nil) then
        begin
          EventEndPP^ := S;

          Parser.EntityDeclHandler(Parser.HandlerArg,
            Parser.DeclEntity.Name, Parser.DeclEntity.IsParam, nil, 0,
            Parser.DeclEntity.Base, Parser.DeclEntity.SystemId,
            Parser.DeclEntity.PublicId, nil);

          HandleDefault := CXmlFalse;
        end;

      xrEntityNotation_Name:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (Parser.DeclEntity <> nil) then
        begin
          Parser.DeclEntity.Notation := PoolStoreString(@TDocTypeDeclaration.Pool,
            Enc, S, Next);

          if Parser.DeclEntity.Notation = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          PoolFinish(@TDocTypeDeclaration.Pool);

          if @Parser.UnparsedEntityDeclHandler <> nil then
          begin
            EventEndPP^ := S;

            Parser.UnparsedEntityDeclHandler(Parser.HandlerArg,
              Parser.DeclEntity.Name, Parser.DeclEntity.Base,
              Parser.DeclEntity.SystemId, Parser.DeclEntity.PublicId,
              Parser.DeclEntity.Notation);

            HandleDefault := CXmlFalse;
          end
          else if @Parser.EntityDeclHandler <> nil then
          begin
            EventEndPP^ := S;

            Parser.EntityDeclHandler(Parser.HandlerArg,
              Parser.DeclEntity.Name, 0, nil, 0, Parser.DeclEntity.Base,
              Parser.DeclEntity.SystemId, Parser.DeclEntity.PublicId,
              Parser.DeclEntity.Notation);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrGeneralEntityName:
        begin
          if XmlPredefinedEntityName(Enc, Pointer(S), Pointer(Next)) <> 0 then
          begin
            Parser.DeclEntity := nil;

            goto _break;
          end;

          if TDocTypeDeclaration.KeepProcessing <> 0 then
          begin
            name := PoolStoreString(@TDocTypeDeclaration.Pool, Enc, S, Next);

            if name = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Parser.DeclEntity :=
              PEntity(Lookup(@TDocTypeDeclaration.GeneralEntities, name,
              SizeOf(Expat.TEntity)));

            if Parser.DeclEntity = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            if Parser.DeclEntity.Name <> name then
            begin
              PoolDiscard(@TDocTypeDeclaration.Pool);

              Parser.DeclEntity := nil;
            end
            else
            begin
              PoolFinish(@TDocTypeDeclaration.Pool);

              Parser.DeclEntity.PublicId := nil;
              Parser.DeclEntity.IsParam := CXmlFalse;

              { if we have a parent parser or are reading an internal parameter
                TEntity, then the TEntity declaration is not considered "internal" }
              Parser.DeclEntity.IsInternal :=
                TXmlBool(not((Parser.ParentParser <> nil) or
                (Parser.OpenInternalEntities <> nil)));

              if @Parser.EntityDeclHandler <> nil then
                HandleDefault := CXmlFalse;
            end;
          end
          else
          begin
            PoolDiscard(@TDocTypeDeclaration.Pool);

            Parser.DeclEntity := nil;
          end;
        end;

      xrParamEntityName:
{$IFDEF XML_DTD}
        if TDocTypeDeclaration.KeepProcessing <> 0 then
        begin
          name := PoolStoreString(@TDocTypeDeclaration.Pool, Enc, S, Next);

          if name <> nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          Parser.DeclEntity :=
            PEntity(Lookup(@TDocTypeDeclaration.ParamEntities, name, SizeOf(Expat.TEntity)));

          if Parser.DeclEntity = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          if Parser.DeclEntity.Name <> name then
          begin
            PoolDiscard(@TDocTypeDeclaration.Pool);

            Parser.DeclEntity := nil;
          end
          else
          begin
            PoolFinish(@TDocTypeDeclaration.Pool);

            Parser.DeclEntity.PublicId := nil;
            Parser.DeclEntity.IsParam := CXmlTrue;

            { if we have a parent parser or are reading an internal parameter
              TEntity, then the TEntity declaration is not considered "internal" }
            Parser.DeclEntity.IsInternal :=
              TXmlBool(not((Parser.ParentParser <> nil) or
              (Parser.OpenInternalEntities <> nil)));

            if @Parser.EntityDeclHandler <> nil then
              HandleDefault := CXmlFalse;
          end;
        end
        else
        begin
          PoolDiscard(@TDocTypeDeclaration.Pool);

          Parser.DeclEntity := nil;
        end;

{$ELSE}
        Parser.DeclEntity := nil;

{$ENDIF}
      xrNotationName:
        begin
          Parser.DeclNotationPublicId := nil;
          Parser.DeclNotationName := nil;

          if @Parser.NotationDeclHandler <> nil then
          begin
            Parser.DeclNotationName := PoolStoreString(@Parser.TempPool,
              Enc, S, Next);

            if Parser.DeclNotationName = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            PoolFinish(@Parser.TempPool);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrNotationPublicID:
        begin
          if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
          then
          begin
            Result := xePublicID;

            Exit;
          end;

          if Parser.DeclNotationName <> nil
          then { means notationDeclHandler <> NIL }
          begin
            Tem := PoolStoreString(@Parser.TempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Tem = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            NormalizePublicId(Tem);

            Parser.DeclNotationPublicId := Tem;

            PoolFinish(@Parser.TempPool);

            HandleDefault := CXmlFalse;
          end;
        end;

      xrNotationSystemID:
        begin
          if (Parser.DeclNotationName <> nil) and
            (@Parser.NotationDeclHandler <> nil) then
          begin
            SystemId := PoolStoreString(@Parser.TempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if SystemId = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            EventEndPP^ := S;

            Parser.NotationDeclHandler(Parser.HandlerArg,
              Parser.DeclNotationName, Parser.CurBase, SystemId,
              Parser.DeclNotationPublicId);

            HandleDefault := CXmlFalse;
          end;

          PoolClear(@Parser.TempPool);
        end;

      xrNotationNoSystem_ID:
        begin
          if (Parser.DeclNotationPublicId <> nil) and
            (@Parser.NotationDeclHandler <> nil) then
          begin
            EventEndPP^ := S;

            Parser.NotationDeclHandler(Parser.HandlerArg,
              Parser.DeclNotationName, Parser.CurBase, nil,
              Parser.DeclNotationPublicId);

            HandleDefault := CXmlFalse;
          end;

          PoolClear(@Parser.TempPool);
        end;

      xrError:
        case Tok of
          xtParamEntityRef:
            { PE references in internal subset are
              not alLowed within declarations. }
            begin
              Result := xeParamEntityRef;

              Exit;
            end;

          xtXmlDecl:
            begin
              Result := xeMisplacedXmlProcessingInstruction;

              Exit;
            end;

        else
          begin
            Result := xeSyntax;

            Exit;
          end;
        end;

{$IFDEF XML_DTD}
      xrIgnoreSect:
        begin
          if @Parser.DefaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          HandleDefault := CXmlFalse;

          Result_ := DoIgnoreSection(Parser, Enc, @Next, Stop, NextPtr,
            HaveMore);

          if Result_ <> xeNone then
          begin
            Result := Result_;

            Exit;
          end
          else if Next = nil then
          begin
            Parser.TProcessor := @IgnoreSectionProcessor;

            Result := Result_;

            Exit;
          end;
        end;

{$ENDIF}
      xrGroupOpen:
        begin
          if Parser.PrologState.Level >= Parser.GroupSize then
            if Parser.GroupSize <> 0 then
            begin
              Parser.GroupSize := Parser.GroupSize * 2;

              if Parser.Mem.ReallocFunction(Pointer(Parser.GroupConnector),
                Parser.GroupAlloc, Parser.GroupSize) then
                Parser.GroupAlloc := Parser.GroupSize

              else
              begin
                Result := xeNoMemory;

                Exit;
              end;

              if TDocTypeDeclaration.ScaffIndex <> nil then
                if Parser.Mem.ReallocFunction(Pointer(TDocTypeDeclaration.ScaffIndex),
                  TDocTypeDeclaration.ScaffAlloc, Parser.GroupSize * SizeOf(Integer)) then
                  TDocTypeDeclaration.ScaffAlloc := Parser.GroupSize * SizeOf(Integer)

                else
                begin
                  Result := xeNoMemory;

                  Exit;
                end;
            end
            else
            begin
              Parser.GroupSize := 32;

              if Parser.Mem.MallocFunction(Pointer(Parser.GroupConnector),
                Parser.GroupSize) then
                Parser.GroupAlloc := Parser.GroupSize
              else
              begin
                Result := xeNoMemory;

                Exit;
              end;
            end;

          PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ := #0;

          if TDocTypeDeclaration.In_eldecl <> 0 then
          begin
            Myindex := NextScaffoldPart(Parser);

            if Myindex < 0 then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            PInteger(PtrComp(TDocTypeDeclaration.ScaffIndex) + TDocTypeDeclaration.ScaffLevel * SizeOf(Integer))^
              := Myindex;

            Inc(TDocTypeDeclaration.ScaffLevel);

            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) + Myindex *
              SizeOf(TContentScaffold))^.ContentType := ctSEQ;

            if @Parser.ElementDeclHandler <> nil then
              HandleDefault := CXmlFalse;
          end;
        end;

      xrGroupSequence:
        begin
          if PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ = '|' then
          begin
            Result := xeSyntax;

            Exit;
          end;

          PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ := ',';

          if (TDocTypeDeclaration.In_eldecl <> 0) and (@Parser.ElementDeclHandler <> nil) then
            HandleDefault := CXmlFalse;
        end;

      xrGroupChoice:
        begin
          if PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ = ',' then
          begin
            Result := xeSyntax;

            Exit;
          end;

          if (TDocTypeDeclaration.In_eldecl <> 0) and
            (PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ <> #0) and
            (PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) +
            PInteger(PtrComp(TDocTypeDeclaration.ScaffIndex) + (TDocTypeDeclaration.ScaffLevel - 1) * SizeOf(Integer))^
            * SizeOf(TContentScaffold))^.ContentType <> ctMixed) then
          begin
            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) +
              PInteger(PtrComp(TDocTypeDeclaration.ScaffIndex) + (TDocTypeDeclaration.ScaffLevel - 1) * SizeOf(Integer))
              ^ * SizeOf(TContentScaffold))^.ContentType := ctChoice;

            if @Parser.ElementDeclHandler <> nil then
              HandleDefault := CXmlFalse;
          end;

          PAnsiChar(PtrComp(Parser.GroupConnector) +
            Parser.PrologState.Level)^ := '|';
        end;

      xrParamEntityRef
      {$IFDEF XML_DTD} , xrInnerParamEntityRef: {$ELSE}: {$ENDIF}
        begin
{$IFDEF XML_DTD}
          TDocTypeDeclaration.HasParamEntityRefs := CXmlTrue;

          if Parser.ParamEntityParsing = TXmlParamEntityParsing(0) then
            TDocTypeDeclaration.KeepProcessing := TDocTypeDeclaration.Standalone
          else
          begin
            name := PoolStoreString(@TDocTypeDeclaration.Pool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if name = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            TEntity := PEntity(Lookup(@TDocTypeDeclaration.ParamEntities, name, 0));

            PoolDiscard(@TDocTypeDeclaration.Pool);

            { first, determine if a check for an existing declaration is needed;
              if yes, check that the TEntity exists, and that it is internal,
              otherwise call the skipped TEntity handler }
            if TDocTypeDeclaration.Standalone <> 0 then
              Ok := TXmlBool(Parser.OpenInternalEntities = nil)
            else
              Ok := TXmlBool(TDocTypeDeclaration.HasParamEntityRefs = 0);

            if (Parser.PrologState.DocumentEntity <> 0) and (Ok <> 0) then
              if TEntity = nil then
              begin
                Result := xeUndefinedEntity;

                Exit;
              end
              else if TEntity.IsInternal = 0 then
              begin
                Result := xeEntityDeclaredInPe;

                Exit;
              end
              else
            else if TEntity = nil then
            begin
              TDocTypeDeclaration.KeepProcessing := TDocTypeDeclaration.Standalone;

              { cannot report skipped entities in declarations }
              if (Role = xrParamEntityRef) and
                (@Parser.SkippedEntityHandler <> nil) then
              begin
                Parser.SkippedEntityHandler(Parser.HandlerArg, name, 1);

                HandleDefault := CXmlFalse;
              end;

              goto _break;
            end;

            if TEntity.Open <> 0 then
            begin
              Result := xeRecursiveEntityRef;

              Exit;
            end;

            if TEntity.TextPtr <> nil then
            begin
              if Role = xrParamEntityRef then
                BetweenDecl := CXmlTrue
              else
                BetweenDecl := CXmlFalse;

              Result_ := ProcessInternalEntity(Parser, TEntity, BetweenDecl);

              if Result_ <> xeNone then
              begin
                Result := Result_;

                Exit;
              end;

              HandleDefault := CXmlFalse;

              goto _break;
            end;

            if @Parser.ExternalEntityRefHandler <> nil then
            begin
              TDocTypeDeclaration.ParamEntityRead := CXmlFalse;
              TEntity.Open := CXmlTrue;

              if Parser.ExternalEntityRefHandler
                (Parser.ExternalEntityRefHandlerArg, nil, TEntity.Base,
                TEntity.SystemId, TEntity.PublicId) = 0 then
              begin
                TEntity.Open := CXmlFalse;

                Result := xeExternalEntityHandling;

                Exit;
              end;

              TEntity.Open := CXmlFalse;
              HandleDefault := CXmlFalse;

              if TDocTypeDeclaration.ParamEntityRead = 0 then
              begin
                TDocTypeDeclaration.KeepProcessing := TDocTypeDeclaration.Standalone;

                goto _break;
              end;
            end
            else
            begin
              TDocTypeDeclaration.KeepProcessing := TDocTypeDeclaration.Standalone;

              goto _break;
            end;
          end;

{$ENDIF}
          if (TDocTypeDeclaration.Standalone = 0) and (@Parser.NotStandaloneHandler <> nil)
            and (Parser.NotStandaloneHandler(Parser.HandlerArg) = 0) then
          begin
            Result := xeNotStandalone;

            Exit;
          end;
        end;

      { Element declaration stuff }
      xrElementName:
        if @Parser.ElementDeclHandler <> nil then
        begin
          Parser.DeclElementType := GetElementType(Parser, Enc, S, Next);

          if Parser.DeclElementType = nil then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          TDocTypeDeclaration.ScaffLevel := 0;
          TDocTypeDeclaration.ScaffCount := 0;
          TDocTypeDeclaration.In_eldecl := CXmlTrue;
          HandleDefault := CXmlFalse;
        end;

      xrContentAny, xrContentEmpty:
        if TDocTypeDeclaration.In_eldecl <> 0 then
        begin
          if @Parser.ElementDeclHandler <> nil then
          begin
            Parser.Mem.MallocFunction(Pointer(Content), SizeOf(TXmlContent));

            if Content = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            Content.Quant := cqNone;
            Content.Name := nil;
            Content.Numchildren := 0;
            Content.Children := nil;

            if Role = xrContentAny then
              Content.ContentType := ctAny
            else
              Content.ContentType := ctEmpty;

            EventEndPP^ := S;

            Parser.ElementDeclHandler(Parser.HandlerArg,
              Parser.DeclElementType.Name, Content);

            HandleDefault := CXmlFalse;
          end;

          TDocTypeDeclaration.In_eldecl := CXmlFalse;
        end;

      xrContentPCData:
        if TDocTypeDeclaration.In_eldecl <> 0 then
        begin
          PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) +
            PInteger(PtrComp(TDocTypeDeclaration.ScaffIndex) + (TDocTypeDeclaration.ScaffLevel - 1) * SizeOf(Integer))^
            * SizeOf(TContentScaffold))^.ContentType := ctMixed;

          if @Parser.ElementDeclHandler <> nil then
            HandleDefault := CXmlFalse;
        end;

      xrContentElement:
        begin
          Quant := cqNone;

          goto ElementContent;
        end;

      xrContentElementOpt:
        begin
          Quant := cqOpt;

          goto ElementContent;
        end;

      xrContentElementRep:
        begin
          Quant := cqRep;

          goto ElementContent;
        end;

      xrContentElementPlus:
        begin
          Quant := cqPlus;

        ElementContent:
          if TDocTypeDeclaration.In_eldecl <> 0 then
          begin
            if Quant = cqNone then
              Nxt := Next
            else
              Nxt := PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar);

            Myindex := NextScaffoldPart(Parser);

            if Myindex < 0 then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) + Myindex *
              SizeOf(TContentScaffold))^.ContentType := ctName;

            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) + Myindex *
              SizeOf(TContentScaffold))^.Quant := Quant;

            El := GetElementType(Parser, Enc, S, Nxt);

            if El = nil then
            begin
              Result := xeNoMemory;

              Exit;
            end;

            name := El.Name;

            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) + Myindex *
              SizeOf(TContentScaffold))^.Name := name;

            NameLen := 0;

            while PXmlChar(PtrComp(name) + NameLen)^ <> TXmlChar(0) do
              Inc(NameLen);

            Inc(TDocTypeDeclaration.ContentStringLen, NameLen);

            if @Parser.ElementDeclHandler <> nil then
              HandleDefault := CXmlFalse;
          end;
        end;

      xrGroupClose:
        begin
          Quant := cqNone;

          goto CloseGroup;
        end;

      xrGroupCloseOpt:
        begin
          Quant := cqOpt;

          goto CloseGroup;
        end;

      xrGroupCloseRep:
        begin
          Quant := cqRep;

          goto CloseGroup;
        end;

      xrGroupClosePlus:
        begin
          Quant := cqPlus;

        CloseGroup:
          if TDocTypeDeclaration.In_eldecl <> 0 then
          begin
            if @Parser.ElementDeclHandler <> nil then
              HandleDefault := CXmlFalse;

            Dec(TDocTypeDeclaration.ScaffLevel);

            PContentScaffold(PtrComp(TDocTypeDeclaration.Scaffold) +
              PInteger(PtrComp(TDocTypeDeclaration.ScaffIndex) + TDocTypeDeclaration.ScaffLevel * SizeOf(Integer))^ *
              SizeOf(TContentScaffold))^.Quant := Quant;

            if TDocTypeDeclaration.ScaffLevel = 0 then
            begin
              if HandleDefault = 0 then
              begin
                Model := BuildModel(Parser);

                if Model = nil then
                begin
                  Result := xeNoMemory;

                  Exit;
                end;

                EventEndPP^ := S;

                Parser.ElementDeclHandler(Parser.HandlerArg,
                  Parser.DeclElementType.Name, Model);
              end;

              TDocTypeDeclaration.In_eldecl := CXmlFalse;
              TDocTypeDeclaration.ContentStringLen := 0;
            end;
          end;
        end; { End element declaration stuff }

      xrProcessingInstruction:
        begin
          if ReportProcessingInstruction(Parser, Enc, S, Next) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          HandleDefault := CXmlFalse;
        end;

      xrComment:
        begin
          if ReportComment(Parser, Enc, S, Next) = 0 then
          begin
            Result := xeNoMemory;

            Exit;
          end;

          HandleDefault := CXmlFalse;
        end;

      xrNone:
        case Tok of
          xtBOM:
            HandleDefault := CXmlFalse;
        end;

      xrDocTypeNone:
        if @Parser.StartDoctypeDeclHandler <> nil then
          HandleDefault := CXmlFalse;

      xrEntityNone:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (@Parser.EntityDeclHandler <> nil)
        then
          HandleDefault := CXmlFalse;

      xrNotationNone:
        if @Parser.NotationDeclHandler <> nil then
          HandleDefault := CXmlFalse;

      xrAttributeListNone:
        if (TDocTypeDeclaration.KeepProcessing <> 0) and (@Parser.AttlistDeclHandler <> nil)
        then
          HandleDefault := CXmlFalse;

      xrElementNone:
        if @Parser.ElementDeclHandler <> nil then
          HandleDefault := CXmlFalse;

    end; { end of big case }

  _break:
    if (HandleDefault = CXmlTrue) and (@Parser.DefaultHandler <> nil) then
      ReportDefault(Parser, Enc, S, Next);

    case Parser.ParsingStatus.Parsing of
      xpSuspended:
        begin
          NextPtr^ := Next;
          Result := xeNone;

          Exit;
        end;

      xpFinished:
        begin
          Result := xeAborted;

          Exit;
        end;

    else
      begin
        S := Next;
        Tok := XmlPrologTok(Enc, Pointer(S), Pointer(Stop), @Next);
      end;

    end;

  until False;

  { not reached }
end;

function PrologProcessor(Parser: TXmlParser; S, Stop: PAnsiChar;
  NextPtr: PPAnsiChar): TXmlError;
var
  Next: PAnsiChar;
  Tok : TXmlTok;
begin
  Next := S;
  Tok := XmlPrologTok(Parser.Encoding, Pointer(S), Pointer(Stop), @Next);

  Result := DoProlog(Parser, Parser.Encoding, S, Stop, Tok, Next, NextPtr,
    TXmlBool(not Parser.ParsingStatus.FinalBuffer));

end;

function PrologInitProcessor(Parser: TXmlParser; S, Stop: PAnsiChar;
  NextPtr: PPAnsiChar): TXmlError;
begin
  Result := InitializeEncoding(Parser);

  if Result <> xeNone then
    Exit;

  Parser.TProcessor := @PrologProcessor;

  Result := PrologProcessor(Parser, S, Stop, NextPtr);
end;

procedure ParserInit(Parser: TXmlParser; EncodingName: PXmlChar);
begin
  Parser.TProcessor := @PrologInitProcessor;

  XmlPrologStateInit(@Parser.PrologState);

  if EncodingName <> nil then
    Parser.ProtocolEncodingName := PoolCopyString(@Parser.TempPool,
      EncodingName)
  else
    Parser.ProtocolEncodingName := nil;

  Parser.CurBase := nil;

  XmlInitEncoding(@Parser.InitEncoding, @Parser.Encoding, nil);

  Parser.UserData := nil;
  Parser.HandlerArg := nil;

  Parser.StartElementHandler := nil;
  Parser.EndElementHandler := nil;
  Parser.CharacterDataHandler := nil;
  Parser.ProcessingInstructionHandler := nil;
  Parser.CommentHandler := nil;
  Parser.StartCdataSectionHandler := nil;
  Parser.EndCdataSectionHandler := nil;
  Parser.DefaultHandler := nil;
  Parser.StartDoctypeDeclHandler := nil;
  Parser.EndDoctypeDeclHandler := nil;
  Parser.UnparsedEntityDeclHandler := nil;
  Parser.NotationDeclHandler := nil;
  Parser.StartNamespaceDeclHandler := nil;
  Parser.EndNamespaceDeclHandler := nil;
  Parser.NotStandaloneHandler := nil;
  Parser.ExternalEntityRefHandler := nil;
  Parser.ExternalEntityRefHandlerArg := Parser;
  Parser.SkippedEntityHandler := nil;
  Parser.ElementDeclHandler := nil;
  Parser.AttlistDeclHandler := nil;
  Parser.EntityDeclHandler := nil;
  Parser.XmlDeclHandler := nil;

  Parser.BufferPtr := Parser.Buffer;
  Parser.BufferEnd := Parser.Buffer;

  Parser.ParseEndByteIndex := 0;
  Parser.ParseEndPtr := nil;

  Parser.DeclElementType := nil;
  Parser.DeclAttributeId := nil;
  Parser.DeclEntity := nil;

  Parser.DoctypeName := nil;
  Parser.DoctypeSysid := nil;
  Parser.DoctypePubid := nil;

  Parser.DeclAttributeType := nil;
  Parser.DeclNotationName := nil;
  Parser.DeclNotationPublicId := nil;
  Parser.DeclAttributeIsCdata := CXmlFalse;
  Parser.DeclAttributeIsId := CXmlFalse;

  FillChar(Parser.Position, SizeOf(TPosition), 0);

  Parser.ErrorCode := xeNone;

  Parser.EventPtr := nil;
  Parser.EventEndPtr := nil;
  Parser.PositionPtr := nil;

  Parser.OpenInternalEntities := nil;
  Parser.DefaultExpandInternalEntities := CXmlTrue;

  Parser.TagLevel := 0;
  Parser.TagStack := nil;
  Parser.InheritedBindings := nil;
  Parser.NumSpecifiedAtts := 0;

  Parser.UnknownEncodingMem := nil;
  Parser.UnknownEncodingRelease := nil;
  Parser.UnknownEncodingData := nil;
  Parser.UnknownEncodingAlloc := 0;

  Parser.ParentParser := nil;
  Parser.ParsingStatus.Parsing := xpInitialized;

{$IFDEF XML_DTD}
  Parser.IsParamEntity := CXmlFalse;
  Parser.UseForeignDTD := CXmlFalse;

  Parser.ParamEntityParsing := pepNever;
{$ENDIF}
end;

function ParserCreate(EncodingName: PXmlChar;
  Memsuite: PXmlMemoryHandlingSuite; NameSep: PXmlChar; TDocTypeDeclaration: PDocTypeDeclaration)
  : TXmlParser;
var
  Parser: TXmlParser;
  Mtemp : PXmlMemoryHandlingSuite;

begin
  Parser := nil;

  if Memsuite <> nil then
  begin
    Memsuite.MallocFunction(Pointer(Parser), SizeOf(TXmlParserStruct));

    if Parser <> nil then
    begin
      Mtemp := @Parser.Mem;

      Mtemp.MallocFunction := Memsuite.MallocFunction;
      Mtemp.ReallocFunction := Memsuite.ReallocFunction;
      Mtemp.FreeFunction := Memsuite.FreeFunction;
    end;
  end
  else
  begin
    ExpatGetMem(Pointer(Parser), SizeOf(TXmlParserStruct));

    if Parser <> nil then
    begin
      Mtemp := @Parser.Mem;

      Mtemp.MallocFunction := @ExpatGetMem;
      Mtemp.ReallocFunction := @ExpatRealloc;
      Mtemp.FreeFunction := @ExpatFreeMem;
    end;
  end;

  if Parser = nil then
  begin
    Result := nil;

    Exit;
  end;

  Parser.Buffer := nil;
  Parser.BufferLim := nil;
  Parser.AttsSize := CInitAttsSize;

  Parser.AttsAlloc := 0;
  Parser.NameSpaceAttsAlloc := 0;

  Parser.Mem.MallocFunction(Pointer(Parser.Atts),
    Parser.AttsSize * SizeOf(TAttribute));

  if Parser.Atts = nil then
  begin
    Parser.Mem.FreeFunction(Pointer(Parser), SizeOf(TXmlParserStruct));

    Result := nil;

    Exit;
  end
  else
    Parser.AttsAlloc := Parser.AttsSize * SizeOf(TAttribute);

  Parser.Mem.MallocFunction(Pointer(Parser.DataBuf),
    CInitDataBufferSize * SizeOf(TXmlChar));

  if Parser.DataBuf = nil then
  begin
    Parser.Mem.FreeFunction(Pointer(Parser.Atts), Parser.AttsAlloc);
    Parser.Mem.FreeFunction(Pointer(Parser), SizeOf(TXmlParserStruct));

    Result := nil;

    Exit;
  end;

  Parser.DataBufEnd := PXmlChar(PtrComp(Parser.DataBuf) +
    CInitDataBufferSize);

  if TDocTypeDeclaration <> nil then
    Parser.DocTypeDeclaration := TDocTypeDeclaration
  else
  begin
    Parser.DocTypeDeclaration := DtdCreate(@Parser.Mem);

    if Parser.DocTypeDeclaration = nil then
    begin
      Parser.Mem.FreeFunction(Pointer(Parser.DataBuf),
        CInitDataBufferSize * SizeOf(TXmlChar));
      Parser.Mem.FreeFunction(Pointer(Parser.Atts), Parser.AttsAlloc);
      Parser.Mem.FreeFunction(Pointer(Parser), SizeOf(TXmlParserStruct));

      Result := nil;

      Exit;
    end;
  end;

  Parser.FreeBindingList := nil;
  Parser.FreeTagList := nil;
  Parser.FreeInternalEntities := nil;

  Parser.GroupSize := 0;
  Parser.GroupAlloc := 0;
  Parser.GroupConnector := nil;

  Parser.UnknownEncodingHandler := nil;
  Parser.UnknownEncodingHandlerData := nil;

  Parser.NamespaceSeparator := '!';

  Parser.NameSpace := CXmlFalse;
  Parser.NameSpaceTriplets := CXmlFalse;

  Parser.NameSpaceAtts := nil;
  Parser.NameSpaceAttsVersion := 0;
  Parser.NameSpaceAttsPower := 0;

  PoolInit(@Parser.TempPool, @Parser.Mem);
  PoolInit(@Parser.Temp2Pool, @Parser.Mem);
  ParserInit(Parser, EncodingName);

  if (EncodingName <> nil) and (Parser.ProtocolEncodingName = nil) then
  begin
    XmlParserFree(Parser);

    Result := nil;

    Exit;
  end;

  if NameSep <> nil then
  begin
    Parser.NameSpace := CXmlTrue;

    Parser.InternalEncoding := XmlGetInternalEncodingNS;
    Parser.NamespaceSeparator := NameSep^;
  end
  else
    Parser.InternalEncoding := XmlGetInternalEncoding;

  Result := Parser;
end;

function SetContext(Parser: TXmlParser; Context: PXmlChar): TXmlBool;
begin
  Result := CXmlFalse;
end;

function XmlParserCreate;
begin
  Result := XmlParserCreate_MM(Encoding, nil, nil);
end;

function XmlParserCreate_MM;
var
  Parser: TXmlParser;
begin
  Parser := ParserCreate(Encoding, Memsuite, NamespaceSeparator, nil);

  if (Parser <> nil) and (Parser.NameSpace <> 0) then
    { implicit context only set for root parser, since child
      parsers (i.e. external TEntity parsers) will inherit it }
    if not SetContext(Parser, @CImplicitContext[0]) <> 0 then
    begin
      XmlParserFree(Parser);

      Result := nil;
      Exit;
    end;

  Result := Parser;
end;

procedure XmlSetUserData;
begin
  if Parser.HandlerArg = Parser.UserData then
  begin
    Parser.HandlerArg := UserData;
    Parser.UserData := UserData;
  end
  else
    Parser.UserData := UserData;
end;

procedure XmlSetElementHandler;
begin
  Parser.StartElementHandler := Start;
  Parser.EndElementHandler := Stop;
end;

procedure XmlSetCharacterDataHandler;
begin
  Parser.CharacterDataHandler := Handler;
end;

function XML_GetBuffer(Parser: TXmlParser; Len: Integer): Pointer;
var
  NeededSize, Keep, Offset, BufferSize: Integer;
  NewBuf: PAnsiChar;
begin
  case Parser.ParsingStatus.Parsing of
    xpSuspended:
      begin
        Parser.ErrorCode := xeSuspended;
        Result := nil;
        Exit;
      end;

    xpFinished:
      begin
        Parser.ErrorCode := xeFinished;
        Result := nil;
        Exit;
      end;
  end;

  if Len > PtrComp(Parser.BufferLim) - PtrComp(Parser.BufferEnd) then
  begin
    { FIXME avoid integer overflow }
    NeededSize := Len + (PtrComp(Parser.BufferEnd) -
      PtrComp(Parser.BufferPtr));

{$IFDEF XML_CONTEXT_BYTES}
    Keep := PtrComp(Parser.BufferPtr) - PtrComp(Parser.Buffer);

    if Keep > _XML_CONTEXT_BYTES then
      Keep := _XML_CONTEXT_BYTES;

    Inc(NeededSize, Keep);

{$ENDIF}
    if NeededSize <= PtrComp(Parser.BufferLim) - PtrComp(Parser.Buffer) then
    begin
{$IFDEF XML_CONTEXT_BYTES}
      if Keep < PtrComp(Parser.BufferPtr) - PtrComp(Parser.Buffer) then
      begin
        Offset := PtrComp(Parser.BufferPtr) - PtrComp(Parser.Buffer) - Keep;

        Move(PAnsiChar(PtrComp(Parser.Buffer) + Offset)^, Parser.Buffer^,
          PtrComp(Parser.BufferEnd) - PtrComp(Parser.BufferPtr) + Keep);

        Dec(PtrComp(Parser.BufferEnd), Offset);
        Dec(PtrComp(Parser.BufferPtr), Offset);

      end;

{$ELSE}
      Move(Parser.BufferPtr^, Parser.Buffer^, PtrComp(Parser.BufferEnd) -
        PtrComp(Parser.BufferPtr));

      Parser.BufferEnd := PAnsiChar(PtrComp(Parser.Buffer) +
        (PtrComp(Parser.BufferEnd) - PtrComp(Parser.BufferPtr)));
      Parser.BufferPtr := Parser.Buffer;
{$ENDIF}
    end
    else
    begin
      BufferSize := PtrComp(Parser.BufferLim) - PtrComp(Parser.BufferPtr);

      if BufferSize = 0 then
        BufferSize := CInitBufferSize;

      repeat
        BufferSize := BufferSize * 2;

      until BufferSize >= NeededSize;

      Parser.Mem.MallocFunction(Pointer(NewBuf), BufferSize);

      if NewBuf = nil then
      begin
        Parser.ErrorCode := xeNoMemory;

        Result := nil;

        Exit;
      end;

      Parser.BufferLim := PAnsiChar(PtrComp(NewBuf) + BufferSize);

{$IFDEF XML_CONTEXT_BYTES}
      if Parser.BufferPtr <> nil then
      begin
        Keep := PtrComp(Parser.BufferPtr) - PtrComp(Parser.Buffer);

        if Keep > _XML_CONTEXT_BYTES then
          Keep := _XML_CONTEXT_BYTES;

        Move(PAnsiChar(PtrComp(Parser.BufferPtr) - Keep)^, NewBuf^,
          PtrComp(Parser.BufferEnd) - PtrComp(Parser.BufferPtr) + Keep);

        ExpatFreeMem(Pointer(Parser.Buffer), Parser.BufferAloc);

        Parser.Buffer := NewBuf;
        Parser.BufferAloc := BufferSize;

        Parser.BufferEnd :=
          PAnsiChar(PtrComp(Parser.Buffer) + (PtrComp(Parser.BufferEnd) -
          PtrComp(Parser.BufferPtr)) + Keep);

        Parser.BufferPtr := PAnsiChar(PtrComp(Parser.Buffer) + Keep);

      end
      else
      begin
        Parser.BufferEnd :=
          PAnsiChar(PtrComp(NewBuf) + (PtrComp(Parser.BufferEnd) -
          PtrComp(Parser.BufferPtr)));
        Parser.Buffer := NewBuf;
        Parser.BufferPtr := NewBuf;
        Parser.BufferAloc := BufferSize;
      end;
{$ELSE}
      if Parser.BufferPtr <> nil then
      begin
        Move(Parser.BufferPtr^, NewBuf^, PtrComp(Parser.BufferEnd) -
          PtrComp(Parser.BufferPtr));

        ExpatFreeMem(Pointer(Parser.Buffer), Parser.BufferAloc);
      end;

      Parser.BufferEnd :=
        PAnsiChar(PtrComp(NewBuf) + (PtrComp(Parser.BufferEnd) -
        PtrComp(Parser.BufferPtr)));
      Parser.Buffer := NewBuf;
      Parser.BufferPtr := NewBuf;
      Parser.BufferAloc := BufferSize;
{$ENDIF}
    end;
  end;

  Result := Parser.BufferEnd;
end;

function ErrorProcessor(Parser: TXmlParser; S, Stop: PAnsiChar;
  NextPtr: PPAnsiChar): TXmlError;
begin
  Result := Parser.ErrorCode;
end;

function XML_ParseBuffer(Parser: TXmlParser; Len, IsFinal: Integer): TXmlStatus;
var
  Start  : PAnsiChar;
begin
  Result := xsOK;

  case Parser.ParsingStatus.Parsing of
    xpSuspended:
      begin
        Parser.ErrorCode := xeSuspended;
        Result := xsError;
        Exit;
      end;

    xpFinished:
      begin
        Parser.ErrorCode := xeFinished;
        Result := xsError;
        Exit;
      end;

  else
    Parser.ParsingStatus.Parsing := xpParsing;
  end;

  Start := Parser.BufferPtr;
  Parser.PositionPtr := Start;

  Inc(PtrComp(Parser.BufferEnd), Len);

  Parser.ParseEndPtr := Parser.BufferEnd;

  Inc(Parser.ParseEndByteIndex, Len);

  Parser.ParsingStatus.FinalBuffer := TXmlBool(IsFinal);

  Parser.ErrorCode := Parser.TProcessor(Parser, Start, Parser.ParseEndPtr,
    @Parser.BufferPtr);

  if Parser.ErrorCode <> xeNone then
  begin
    Parser.EventEndPtr := Parser.EventPtr;
    Parser.TProcessor := @ErrorProcessor;

    Result := xsError;
    Exit;
  end
  else
    case Parser.ParsingStatus.Parsing of
      xpSuspended:
        Result := xsSuspended;

      xpInitialized, xpParsing:
        if IsFinal <> 0 then
        begin
          Parser.ParsingStatus.Parsing := xpFinished;
          Exit;
        end;
    else
      { should not happen }
      NoP;
    end;

  Parser.Encoding.UpdatePosition(Parser.Encoding,
    Pointer(Parser.PositionPtr), Pointer(Parser.BufferPtr),
    @Parser.Position);

  Parser.PositionPtr := Parser.BufferPtr;
end;

function XmlParse(Parser: TXmlParser; const S: PAnsiChar; Len, IsFinal: Integer)
  : TXmlStatus;
var
  Buff: Pointer;
begin
  case Parser.ParsingStatus.Parsing of
    xpSuspended:
      begin
        Parser.ErrorCode := xeSuspended;
        Result := xsError;
        Exit;
      end;

    xpFinished:
      begin
        Parser.ErrorCode := xeFinished;
        Result := xsError;
        Exit;
      end;

  else
    Parser.ParsingStatus.Parsing := xpParsing;
  end;

  if Len = 0 then
    NoP
{$IFNDEF XML_CONTEXT_BYTES}
  else if Parser.BufferPtr = Parser.BufferEnd then
    NoP
{$ENDIF}
  else
  begin
    Buff := XML_GetBuffer(Parser, Len);

    if Buff = nil then
      Result := xsError

    else
    begin
      Move(S^, Buff^, Len);

      Result := XML_ParseBuffer(Parser, Len, IsFinal);
    end;
  end;
end;

function XmlGetErrorCode(Parser: TXmlParser): TXmlError;
begin
end;

function XmlErrorString(Code: TXmlError): PXmlLChar;
begin
end;

function XmlGetCurrentLineNumber(Parser: TXmlParser): TXmlSize;
begin
end;

procedure DestroyBindings(Bindings: PBinding; Parser: TXmlParser);
var
  B: PBinding;
begin
  repeat
    B := Bindings;

    if B = nil then
      Break;

    Bindings := B.NextTagBinding;

    Parser.Mem.FreeFunction(Pointer(B.Uri), B.UriAlloc);
    Parser.Mem.FreeFunction(Pointer(B), SizeOf(Expat.TBinding));
  until False;
end;

procedure XmlParserFree;
var
  TagList, P: PTag;
  EntityList, OpenEntity: POpenInternalEntity;
begin
  if Parser = nil then
    Exit;

  { free tagStack and freeTagList }
  TagList := Parser.TagStack;

  repeat
    if TagList = nil then
    begin
      if Parser.FreeTagList = nil then
        Break;

      TagList := Parser.FreeTagList;

      Parser.FreeTagList := nil;
    end;

    P := TagList;
    TagList := TagList.Parent;

    Parser.Mem.FreeFunction(Pointer(P.Buf), P.Alloc);
    DestroyBindings(P.Bindings, Parser);
    Parser.Mem.FreeFunction(Pointer(P), SizeOf(Expat.TTag));
  until False;

  { free openInternalEntities and freeInternalEntities }
  EntityList := Parser.OpenInternalEntities;

  repeat
    if EntityList = nil then
    begin
      if Parser.FreeInternalEntities = nil then
        Break;

      EntityList := Parser.FreeInternalEntities;

      Parser.FreeInternalEntities := nil;
    end;

    OpenEntity := EntityList;
    EntityList := EntityList.Next;

    Parser.Mem.FreeFunction(Pointer(OpenEntity), SizeOf(TOpenInternalEntity));
  until False;

  DestroyBindings(Parser.FreeBindingList, Parser);
  DestroyBindings(Parser.InheritedBindings, Parser);

  PoolDestroy(@Parser.TempPool);
  PoolDestroy(@Parser.Temp2Pool);

{$IFDEF XML_DTD}
  { external parameter TEntity parsers share the TDocTypeDeclaration structure
    parser->DocTypeDeclaration with the root parser, so we must not destroy it }
  if (Parser.IsParamEntity = 0) and (Parser.DocTypeDeclaration <> nil) then
{$ELSE}
  if Parser.DocTypeDeclaration <> nil then
{$ENDIF}
    DtdDestroy(Parser.DocTypeDeclaration, TXmlBool(Parser.ParentParser = nil),
      @Parser.Mem);

  Parser.Mem.FreeFunction(Pointer(Parser.Atts), Parser.AttsAlloc);
  Parser.Mem.FreeFunction(Pointer(Parser.GroupConnector), Parser.GroupAlloc);
  Parser.Mem.FreeFunction(Pointer(Parser.Buffer), Parser.BufferAloc);
  Parser.Mem.FreeFunction(Pointer(Parser.DataBuf),
    CInitDataBufferSize * SizeOf(TXmlChar));
  Parser.Mem.FreeFunction(Pointer(Parser.NameSpaceAtts), Parser.NameSpaceAttsAlloc);
  Parser.Mem.FreeFunction(Pointer(Parser.UnknownEncodingMem),
    Parser.UnknownEncodingAlloc);

  if @Parser.UnknownEncodingRelease <> nil then
    Parser.UnknownEncodingRelease(Parser.UnknownEncodingData);

  Parser.Mem.FreeFunction(Pointer(Parser), SizeOf(TXmlParserStruct));
end;

end.
