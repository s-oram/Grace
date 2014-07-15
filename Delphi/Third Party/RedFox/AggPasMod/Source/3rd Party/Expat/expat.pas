unit expat;

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
  Expat_basics,
  Xmltok,
  Xmlrole;

{$I expat_mode.inc }


type
{$I expat_external.inc }
  XML_Parser = ^XML_ParserStruct;

  XML_Bool = Int8u;

  { The XML_Status enum gives the possible return values for several API functions. }
  XML_Status = (XML_STATUS_ERROR, XML_STATUS_OK, XML_STATUS_SUSPENDED);

  XML_Error = (XML_ERROR_NONE, XML_ERROR_NO_MEMORY, XML_ERROR_SYNTAX,
    XML_ERROR_NO_ELEMENTS, XML_ERROR_INVALID_TOKEN, XML_ERROR_UNCLOSED_TOKEN,
    XML_ERROR_PARTIAL_CHAR, XML_ERROR_TAG_MISMATCH,
    XML_ERROR_DUPLICATE_ATTRIBUTE, XML_ERROR_JUNK_AFTER_DOC_ELEMENT,
    XML_ERROR_PARAM_ENTITY_REF, XML_ERROR_UNDEFINED_ENTITY,
    XML_ERROR_RECURSIVE_ENTITY_REF, XML_ERROR_ASYNC_ENTITY,
    XML_ERROR_BAD_CHAR_REF, XML_ERROR_BINARY_ENTITY_REF,
    XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF, XML_ERROR_MISPLACED_XML_PI,
    XML_ERROR_UNKNOWN_ENCODING, XML_ERROR_INCORRECT_ENCODING,
    XML_ERROR_UNCLOSED_CDATA_SECTION, XML_ERROR_EXTERNAL_ENTITY_HANDLING,
    XML_ERROR_NOT_STANDALONE, XML_ERROR_UNEXPECTED_STATE,
    XML_ERROR_ENTITY_DECLARED_IN_PE, XML_ERROR_FEATURE_REQUIRES_XML_DTD,
    XML_ERROR_CANT_CHANGE_FEATURE_ONCE_PARSING,
    { Added in 1.95.7. }
    XML_ERROR_UNBOUND_PREFIX,
    { Added in 1.95.8. }
    XML_ERROR_UNDECLARING_PREFIX, XML_ERROR_INCOMPLETE_PE, XML_ERROR_XML_DECL,
    XML_ERROR_TEXT_DECL, XML_ERROR_PUBLICID, XML_ERROR_SUSPENDED,
    XML_ERROR_NOT_SUSPENDED, XML_ERROR_ABORTED, XML_ERROR_FINISHED,
    XML_ERROR_SUSPEND_PE,
    { Added in 2.0. }
    XML_ERROR_RESERVED_PREFIX_XML, XML_ERROR_RESERVED_PREFIX_XMLNS,
    XML_ERROR_RESERVED_NAMESPACE_URI);

  XML_Content_Type = (___SKIP_ZERO____, XML_CTYPE_EMPTY, XML_CTYPE_ANY,
    XML_CTYPE_MIXED, XML_CTYPE_NAME, XML_CTYPE_CHOICE, XML_CTYPE_SEQ);

  XML_Content_Quant = (XML_CQUANT_NONE, XML_CQUANT_OPT, XML_CQUANT_REP,
    XML_CQUANT_PLUS);

  XML_ParamEntityParsing = (XML_PARAM_ENTITY_PARSING_NEVER,
    XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE,
    XML_PARAM_ENTITY_PARSING_ALWAYS);

  { If type == XML_CTYPE_EMPTY or XML_CTYPE_ANY, then quant will be
    XML_CQUANT_NONE, and the other fields will be zero or NULL.
    If type == XML_CTYPE_MIXED, then quant will be NONE or REP and
    numchildren will contain number of elements that may be mixed in
    and children point to an array of XML_Content cells that will be
    all of XML_CTYPE_NAME type with no quantification.

    If type == XML_CTYPE_NAME, then the name points to the name, and
    the numchildren field will be zero and children will be NULL. The
    quant fields indicates any quantifiers placed on the name.

    CHOICE and SEQ will have name NULL, the number of children in
    numchildren and children will point, recursively, to an array
    of XML_Content cells.

    The EMPTY, ANY, and MIXED types will only occur at top level. }
  XML_Content_ptr = ^XML_Content;

  XML_cp = record
    Type_: XML_Content_Type;
    Quant: XML_Content_Quant;
    Name: XML_PAnsiChar;

    Numchildren: Cardinal;
    Children: XML_Content_ptr;

  end;

  XML_Content = XML_cp;

  { This is called for an element declaration. See above for
    description of the model argument. It's the caller's responsibility
    to free model when finished with it. }
  XML_ElementDeclHandler = procedure(UserData: Pointer; Name: XML_PAnsiChar;
    Model: XML_Content_ptr);

  { The Attlist declaration handler is called for *each* attribute. So
    a single Attlist declaration with multiple attributes declared will
    generate multiple calls to this handler. The "default" parameter
    may be NULL in the case of the "#IMPLIED" or "#REQUIRED"
    keyword. The "isrequired" parameter will be true and the default
    value will be NULL in the case of "#REQUIRED". If "isrequired" is
    true and default is non-NULL, then this is a "#FIXED" default. }
  XML_AttlistDeclHandler = procedure(UserData: Pointer;
    Elname, Attname, Att_type, Dflt: XML_PAnsiChar; Isrequired: Integer);

  { The XML declaration handler is called for *both* XML declarations
    and text declarations. The way to distinguish is that the version
    parameter will be NULL for text declarations. The encoding
    parameter may be NULL for XML declarations. The standalone
    parameter will be -1, 0, or 1 indicating respectively that there
    was no standalone parameter in the declaration, that it was given
    as no, or that it was given as yes. }
  XML_XmlDeclHandler = procedure(UserData: Pointer;
    Version, Encoding: XML_PAnsiChar; Standalone: Integer);

  { This is called for entity declarations. The is_parameter_entity
    argument will be non-zero if the entity is a parameter entity, zero
    otherwise.

    For internal entities (<!ENTITY foo "bar">), value will
    be non-NULL and systemId, publicID, and notationName will be NULL.
    The value string is NOT nul-terminated; the length is provided in
    the value_length argument. Since it is legal to have zero-length
    values, do not use this argument to test for internal entities.

    For external entities, value will be NULL and systemId will be
    non-NULL. The publicId argument will be NULL unless a public
    identifier was provided. The notationName argument will have a
    non-NULL value only for unparsed entity declarations.

    Note that is_parameter_entity can't be changed to XML_Bool, since
    that would break binary compatibility. }
  XML_EntityDeclHandler = procedure(UserData: Pointer; EntityName: XML_PAnsiChar;
    Is_parameter_entity: Integer; Value: XML_PAnsiChar; Value_length: Integer;
    Base, SystemId, PublicId, NotationName: XML_PAnsiChar);

  { atts is array of name/value pairs, terminated by 0;
    names and values are 0 terminated. }
  XML_StartElementHandler = procedure(UserData: Pointer; Name: XML_PAnsiChar;
    Atts: XML_PPAnsiChar);
  XML_EndElementHandler = procedure(UserData: Pointer; Name: XML_PAnsiChar);

  { s is not 0 terminated. }
  XML_CharacterDataHandler = procedure(UserData: Pointer; S: XML_PAnsiChar;
    Len: Integer);

  { target and data are 0 terminated }
  XML_ProcessingInstructionHandler = procedure(UserData: Pointer;
    Target, Data: XML_PAnsiChar);

  { data is 0 terminated }
  XML_CommentHandler = procedure(UserData: Pointer; Data: XML_PAnsiChar);

  XML_StartCdataSectionHandler = procedure(UserData: Pointer);
  XML_EndCdataSectionHandler = procedure(UserData: Pointer);

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
  XML_DefaultHandler = procedure(UserData: Pointer; S: XML_PAnsiChar; Len: Integer);

  { This is called for the start of the DOCTYPE declaration, before
    any DTD or internal subset is parsed. }
  XML_StartDoctypeDeclHandler = procedure(UserData: Pointer;
    DoctypeName, Sysid, Pubid: XML_PAnsiChar; HasInternal_subset: Integer);

  { This is called for the start of the DOCTYPE declaration when the
    closing > is encountered, but after processing any external
    subset. }
  XML_EndDoctypeDeclHandler = procedure(UserData: Pointer);

  { OBSOLETE -- OBSOLETE -- OBSOLETE
    This handler has been superceded by the EntityDeclHandler above.
    It is provided here for backward compatibility.

    This is called for a declaration of an unparsed (NDATA) entity.
    The base argument is whatever was set by XML_SetBase. The
    entityName, systemId and notationName arguments will never be
    NULL. The other arguments may be. }
  XML_UnparsedEntityDeclHandler = procedure(UserData: Pointer;
    EntityName, Base, SystemId, PublicId, NotationName: XML_PAnsiChar);

  { This is called for a declaration of notation.  The base argument is
    whatever was set by XML_SetBase. The notationName will never be
    NULL.  The other arguments can be. }
  XML_NotationDeclHandler = procedure(UserData: Pointer;
    NotationName, Base, SystemId, PublicId: XML_PAnsiChar);

  { When namespace processing is enabled, these are called once for
    each namespace declaration. The call to the start and end element
    handlers occur between the calls to the start and end namespace
    declaration handlers. For an xmlns attribute, prefix will be
    NULL.  For an xmlns="" attribute, uri will be NULL. }
  XML_StartNamespaceDeclHandler = procedure(UserData: Pointer;
    Prefix, Uri: XML_PAnsiChar);
  XML_EndNamespaceDeclHandler = procedure(UserData: Pointer;
    Prefix: XML_PAnsiChar);

  { This is called if the document is not standalone, that is, it has an
    external subset or a reference to a parameter entity, but does not
    have standalone="yes". If this handler returns XML_STATUS_ERROR,
    then processing will not continue, and the parser will return a
    XML_ERROR_NOT_STANDALONE error.
    If parameter entity parsing is enabled, then in addition to the
    conditions above this handler will only be called if the referenced
    entity was actually read. }
  XML_NotStandaloneHandler = function(UserData: Pointer): Integer;

  { This is called for a reference to an external parsed general
    entity.  The referenced entity is not automatically parsed.  The
    application can parse it immediately or later using
    XML_ExternalEntityParserCreate.

    The parser argument is the parser parsing the entity containing the
    reference; it can be passed as the parser argument to
    XML_ExternalEntityParserCreate.  The systemId argument is the
    system identifier as specified in the entity declaration; it will
    not be NULL.

    The base argument is the system identifier that should be used as
    the base for resolving systemId if systemId was relative; this is
    set by XML_SetBase; it may be NULL.

    The publicId argument is the public identifier as specified in the
    entity declaration, or NULL if none was specified; the whitespace
    in the public identifier will have been normalized as required by
    the XML spec.

    The context argument specifies the parsing context in the format
    expected by the context argument to XML_ExternalEntityParserCreate;
    context is valid only until the handler returns, so if the
    referenced entity is to be parsed later, it must be copied.
    context is NULL only when the entity is a parameter entity.

    The handler should return XML_STATUS_ERROR if processing should not
    continue because of a fatal error in the handling of the external
    entity.  In this case the calling parser will return an
    XML_ERROR_EXTERNAL_ENTITY_HANDLING error.

    Note that unlike other handlers the first argument is the parser,
    not userData. }
  XML_ExternalEntityRefHandler = function(Parser: XML_Parser;
    Context, Base, SystemId, PublicId: XML_PAnsiChar): Integer;

  { This is called in two situations:
    1) An entity reference is encountered for which no declaration
    has been read *and* this is not an error.
    2) An internal entity reference is read, but not expanded, because
    XML_SetDefaultHandler has been called.
    Note: skipped parameter entities in declarations and skipped general
    entities in attribute values cannot be reported, because
    the event would be out of sync with the reporting of the
    declarations or attribute values }
  XML_SkippedEntityHandler = procedure(UserData: Pointer;
    EntityName: XML_PAnsiChar; Is_parameter_entity: Integer);

  (* This structure is filled in by the XML_UnknownEncodingHandler to
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
  XML_Encoding_ptr = ^XML_Encoding;

  XML_Encoding = record
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
    fill in the XML_Encoding structure, and return XML_STATUS_OK.
    Otherwise it must return XML_STATUS_ERROR.

    If info does not describe a suitable encoding, then the parser will
    return an XML_UNKNOWN_ENCODING error. }
  XML_UnknownEncodingHandler = function(EncodingHandlerData: Pointer;
    Name: XML_PAnsiChar; Info: XML_Encoding_ptr): Integer;

  XML_Memory_Handling_Suite_ptr = ^XML_Memory_Handling_Suite;

  XML_Memory_Handling_Suite = record
    Malloc_fcn: function(var Ptr: Pointer; Sz: Integer): Boolean;
    Realloc_fcn: function(var Ptr: Pointer; Old, Sz: Integer): Boolean;
    Free_fcn: function(var Ptr: Pointer; Sz: Integer): Boolean;
  end;

  KEY = XML_PAnsiChar;

  NAMED_ptr_ptr = ^NAMED_ptr;
  NAMED_ptr = ^NAMED;

  NAMED = record
    Name: KEY;
    Alloc: Integer;
  end;

  HASH_TABLE_ptr = ^HASH_TABLE;

  HASH_TABLE = record
    V: NAMED_ptr_ptr;
    A: Integer;

    Power: Int8u;
    Size, Used: Size_t;
    Mem: XML_Memory_Handling_Suite_ptr;
  end;

  ENTITY_ptr = ^ENTITY;

  ENTITY = record
    Name: XML_PAnsiChar;
    Alloc: Integer;

    TextPtr: XML_PAnsiChar;
    TextLen, { length in XML_Chars }
    Processed: Integer; { # of processed bytes - when suspended }
    SystemId, Base, PublicId, Notation: XML_PAnsiChar;

    Open, Is_param, IsInternal: XML_Bool;
    { true if declared in internal subset outside PE }
  end;

  OPEN_INTERNAL_ENTITY_ptr = ^OPEN_INTERNAL_ENTITY;

  OPEN_INTERNAL_ENTITY = record
    InternalEventPtr, InternalEventEndPtr: PAnsiChar;

    Next: OPEN_INTERNAL_ENTITY_ptr;
    Entity: ENTITY_ptr;

    StartTagLevel: Integer;
    BetweenDecl: XML_Bool; { WFC: PE Between Declarations }
  end;

  CONTENT_SCAFFOLD_ptr = ^CONTENT_SCAFFOLD;

  CONTENT_SCAFFOLD = record
    Type_: XML_Content_Type;
    Quant: XML_Content_Quant;
    Name: XML_PAnsiChar;

    Firstchild, Lastchild, Childcnt, Nextsib: Integer;
  end;

  PREFIX_ptr = ^PREFIX;

  ATTRIBUTE_ID_ptr = ^ATTRIBUTE_ID;

  ATTRIBUTE_ID = record
    Name: XML_PAnsiChar;
    Alloc: Integer;
    Prefix: PREFIX_ptr;

    MaybeTokenized, Xmlns: XML_Bool;
  end;

  DEFAULT_ATTRIBUTE_ptr = ^DEFAULT_ATTRIBUTE;

  DEFAULT_ATTRIBUTE = record
    Id: ATTRIBUTE_ID_ptr;

    IsCdata: XML_Bool;
    Value: XML_PAnsiChar;
  end;

  ELEMENT_TYPE_ptr = ^ELEMENT_TYPE;

  ELEMENT_TYPE = record
    Name: XML_PAnsiChar;
    Alloc: Integer;
    Prefix: PREFIX_ptr;
    IdAtt: ATTRIBUTE_ID_ptr;

    NDefaultAtts, AllocDefaultAtts, DefaultAttsAlloc: Integer;

    DefaultAtts: DEFAULT_ATTRIBUTE_ptr;
  end;

  TAG_NAME_ptr = ^TAG_NAME;

  TAG_NAME = record
    Str, LocalPart, Prefix: XML_PAnsiChar;
    StrLen, UriLen, PrefixLen: Integer;
  end;

  { TAG represents an open element.
    The name of the element is stored in both the document and API
    encodings.  The memory buffer 'buf' is a separately-allocated
    memory area which stores the name.  During the XML_Parse()/
    XMLParseBuffer() when the element is open, the memory for the 'raw'
    version of the name (in the document encoding) is shared with the
    document buffer.  If the element is open across calls to
    XML_Parse()/XML_ParseBuffer(), the buffer is re-allocated to
    contain the 'raw' name as well.

    A parser re-uses these structures, maintaining a list of allocated
    TAG objects in a free list. }
  BINDING_ptr_ptr = ^BINDING_ptr;
  BINDING_ptr = ^BINDING;

  TAG_ptr = ^TAG;

  TAG = record
    Parent: TAG_ptr; { parent of this element }
    RawName: PAnsiChar; { tagName in the original encoding }

    RawNameLength: Integer;

    Name: TAG_NAME; { tagName in the API encoding }

    Buf, { buffer for name components }
    BufEnd: PAnsiChar; { end of the buffer }
    Alloc: Integer;

    Bindings: BINDING_ptr;
  end;

  BINDING = record
    Prefix: PREFIX_ptr;

    NextTagBinding, PrevPrefixBinding: BINDING_ptr;

    AttId: ATTRIBUTE_ID_ptr;
    Uri: XML_PAnsiChar;

    UriLen, UriAlloc: Integer;
  end;

  PREFIX = record
    Name: XML_PAnsiChar;
    Alloc: Integer;
    Binding: BINDING_ptr;
  end;

  NS_ATT_ptr = ^NS_ATT;

  NS_ATT = record
    Version, Hash: Int32u;
    UriName: XML_PAnsiChar;
  end;

  BLOCK_ptr = ^BLOCK;

  BLOCK = record
    Next: BLOCK_ptr;
    Size, Alloc: Integer;

    S: array [0..0] of XML_Char;
  end;

  STRING_POOL_ptr = ^STRING_POOL;

  STRING_POOL = record
    Blocks, FreeBlocks: BLOCK_ptr;

    End_, Ptr, Start: XML_PAnsiChar;

    Mem: XML_Memory_Handling_Suite_ptr;
  end;

  DTD_ptr = ^DTD;

  DTD = record
    GeneralEntities, ElementTypes, AttributeIds, Prefixes: HASH_TABLE;

    Pool, EntityValuePool: STRING_POOL;

    { false once a parameter entity reference has been skipped }
    KeepProcessing: XML_Bool;

    { true once an internal or external PE reference has been encountered;
      this includes the reference to an external subset }
    HasParamEntityRefs, Standalone: XML_Bool;

{$IFDEF XML_DTD }
    { indicates if external PE has been read }
    ParamEntityRead: XML_Bool;
    ParamEntities: HASH_TABLE;

{$ENDIF }
    DefaultPrefix: PREFIX;

    { === scaffolding for building content model === }
    In_eldecl: XML_Bool;
    Scaffold: CONTENT_SCAFFOLD_ptr;

    ContentStringLen, ScaffSize, ScaffCount: Cardinal;

    ScaffLevel: Integer;
    ScaffIndex: PInteger;
    ScaffAlloc: Integer;
  end;

  XML_Parsing = (XML_INITIALIZED, XML_PARSING_, XML_FINISHED, XML_SUSPENDED);

  XML_ParsingStatus = record
    Parsing: XML_Parsing;
    FinalBuffer: XML_Bool;
  end;

  Processor = function(Parser: XML_Parser; Start, End_: PAnsiChar;
    EndPtr: PPAnsiChar): XML_Error;

  XML_ParserStruct = record
    M_userData, M_handlerArg: Pointer;

    M_buffer: PAnsiChar;
    M_mem: XML_Memory_Handling_Suite;

    { first character to be parsed }
    M_bufferPtr: PAnsiChar;

    { past last character to be parsed }
    M_bufferEnd: PAnsiChar;

    { allocated end of buffer }
    M_bufferLim: PAnsiChar;

    { the size of the allocated buffer }
    M_bufferAloc: Integer;

    M_parseEndByteIndex: XML_Index;

    M_parseEndPtr: PAnsiChar;
    M_dataBuf, M_dataBufEnd: XML_PAnsiChar;

    { XML Handlers }
    FStartElementHandler: XML_StartElementHandler;
    M_endElementHandler: XML_EndElementHandler;
    M_characterDataHandler: XML_CharacterDataHandler;
    M_processingInstructionHandler: XML_ProcessingInstructionHandler;
    M_commentHandler: XML_CommentHandler;
    FStartCdataSectionHandler: XML_StartCdataSectionHandler;
    M_endCdataSectionHandler: XML_EndCdataSectionHandler;
    M_defaultHandler: XML_DefaultHandler;
    FStartDoctypeDeclHandler: XML_StartDoctypeDeclHandler;
    M_endDoctypeDeclHandler: XML_EndDoctypeDeclHandler;
    M_unparsedEntityDeclHandler: XML_UnparsedEntityDeclHandler;
    M_notationDeclHandler: XML_NotationDeclHandler;
    FStartNamespaceDeclHandler: XML_StartNamespaceDeclHandler;
    M_endNamespaceDeclHandler: XML_EndNamespaceDeclHandler;
    M_notStandaloneHandler: XML_NotStandaloneHandler;
    M_externalEntityRefHandler: XML_ExternalEntityRefHandler;
    M_externalEntityRefHandlerArg: XML_Parser;
    M_skippedEntityHandler: XML_SkippedEntityHandler;
    M_unknownEncodingHandler: XML_UnknownEncodingHandler;
    M_elementDeclHandler: XML_ElementDeclHandler;
    M_attlistDeclHandler: XML_AttlistDeclHandler;
    M_entityDeclHandler: XML_EntityDeclHandler;
    M_xmlDeclHandler: XML_XmlDeclHandler;

    M_encoding: ENCODING_ptr;
    MInitEncoding: INIT_ENCODING;
    MInternalEncoding: ENCODING_ptr;
    M_protocolEncodingName: XML_PAnsiChar;

    M_ns, M_ns_triplets: XML_Bool;

    M_unknownEncodingMem, M_unknownEncodingData,
      M_unknownEncodingHandlerData: Pointer;
    M_unknownEncodingAlloc: Integer;

    M_unknownEncodingRelease: procedure(Void: Pointer);

    M_prologState: PROLOG_STATE;
    M_processor: Processor;
    M_errorCode: XML_Error;
    M_eventPtr, M_eventEndPtr, M_positionPtr: PAnsiChar;

    M_openInternalEntities, M_freeInternalEntities: OPEN_INTERNAL_ENTITY_ptr;

    M_defaultExpandInternalEntities: XML_Bool;

    M_tagLevel: Integer;
    M_declEntity: ENTITY_ptr;

    M_doctypeName, M_doctypeSysid, M_doctypePubid, M_declAttributeType,
      M_declNotationName, M_declNotationPublicId: XML_PAnsiChar;

    M_declElementType: ELEMENT_TYPE_ptr;
    M_declAttributeId: ATTRIBUTE_ID_ptr;

    M_declAttributeIsCdata, M_declAttributeIsId: XML_Bool;

    M_dtd: DTD_ptr;

    FCurBase: XML_PAnsiChar;

    M_tagStack, M_freeTagList: TAG_ptr;

    MInheritedBindings, M_freeBindingList: BINDING_ptr;

    M_attsSize, M_attsAlloc, M_nsAttsAlloc, M_nSpecifiedAtts, M_idAttIndex: Integer;

    M_atts: ATTRIBUTE_ptr;
    M_nsAtts: NS_ATT_ptr;

    M_nsAttsVersion: Cardinal;
    M_nsAttsPower: Int8u;

    M_position: POSITION;
    M_tempPool, M_temp2Pool: STRING_POOL;

    M_groupConnector: PAnsiChar;
    M_groupSize, M_groupAlloc: Cardinal;

    M_namespaceSeparator: XML_Char;

    M_parentParser: XML_Parser;
    M_parsingStatus: XML_ParsingStatus;

{$IFDEF XML_DTD }
    M_isParamEntity, M_useForeignDTD: XML_Bool;

    M_paramEntityParsing: XML_ParamEntityParsing;

{$ENDIF }
  end;

  { GLOBAL CONSTANTS }
const
  XML_TRUE = 1;
  XML_FALSE = 0;

  { GLOBAL PROCEDURES }
  { Constructs a new parser; encoding is the encoding specified by the
    external protocol or NIL if there is none specified. }
function XML_ParserCreate(const Encoding: XML_PAnsiChar): XML_Parser;

{ Constructs a new parser using the memory management suite referred to
  by memsuite. If memsuite is NULL, then use the standard library memory
  suite. If namespaceSeparator is non-NULL it creates a parser with
  namespace processing as described above. The character pointed at
  will serve as the namespace separator.

  All further memory operations used for the created parser will come from
  the given suite. }
function XML_ParserCreate_MM(Encoding: XML_PAnsiChar;
  Memsuite: XML_Memory_Handling_Suite_ptr; NamespaceSeparator: XML_PAnsiChar)
  : XML_Parser;

{ This value is passed as the userData argument to callbacks. }
procedure XML_SetUserData(Parser: XML_Parser; UserData: Pointer);

procedure XML_SetElementHandler(Parser: XML_Parser;
  Start: XML_StartElementHandler; End_: XML_EndElementHandler);

procedure XML_SetCharacterDataHandler(Parser: XML_Parser;
  Handler: XML_CharacterDataHandler);

{ Parses some input. Returns XML_STATUS_ERROR if a fatal error is
  detected.  The last call to XML_Parse must have isFinal true; len
  may be zero for this call (or any other).

  Though the return values for these functions has always been
  described as a Boolean value, the implementation, at least for the
  1.95.x series, has always returned exactly one of the XML_Status
  values. }
function XML_Parse(Parser: XML_Parser; const S: PAnsiChar; Len, IsFinal: Integer)
  : XML_Status;

{ If XML_Parse or XML_ParseBuffer have returned XML_STATUS_ERROR, then
  XML_GetErrorCode returns information about the error. }
function XML_GetErrorCode(Parser: XML_Parser): XML_Error;

{ Returns a string describing the error. }
function XML_ErrorString(Code: XML_Error): XML_LPAnsiChar;

{ These functions return information about the current parse
  location.  They may be called from any callback called to report
  some parse event; in this case the location is the location of the
  first of the sequence of characters that generated the event.  When
  called from callbacks generated by declarations in the document
  prologue, the location identified isn't as neatly defined, but will
  be within the relevant markup.  When called outside of the callback
  functions, the position indicated will be just past the last parse
  event (regardless of whether there was an associated callback).

  They may also be called after returning from a call to XML_Parse
  or XML_ParseBuffer.  If the return value is XML_STATUS_ERROR then
  the location is the location of the character at which the error
  was detected; otherwise the location is the location of the last
  parse event, as described above. }
function XML_GetCurrentLineNumber(Parser: XML_Parser): XML_Size;

{ Frees memory used by the parser. }
procedure XML_ParserFree(Parser: XML_Parser);

implementation



{$I xmlparse.inc }

end.
