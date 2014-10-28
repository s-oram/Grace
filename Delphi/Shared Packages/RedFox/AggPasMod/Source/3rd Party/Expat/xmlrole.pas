unit xmlrole;

// ----------------------------------------------------------------------------
// Copyright (C) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
// milan@marusinec.sk
// http://www.pasports.org/pasexpat
// Copyright (C) 2006
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
// 10.05.2006-Milano: Unit port establishment
// 08.06.2006-Milano: porting
//

interface

uses
  ExpatBasics,
  ExpatExternal,
  Xmltok;

{$I ExpatMode.inc}

{$WARNINGS OFF}
{$HINTS OFF}

type
  TXmlRole = (
    xrError = -1,
    xrNone = 0,
    xrXmlDecl = xrNone + 1,
    xrInstanceStart = xrXmlDecl + 1,
    xrDocTypeNone = xrInstanceStart + 1,
    xrDocTypeName = xrDocTypeNone + 1,
    xrDocTypeSystemID = xrDocTypeName + 1,
    xrDocTypePublicID = xrDocTypeSystemID + 1,
    xrDocTypeInternalSubset = xrDocTypePublicID + 1,
    xrDocTypeClose = xrDocTypeInternalSubset + 1,
    xrGeneralEntityName = xrDocTypeClose + 1,
    xrParamEntityName = xrGeneralEntityName + 1,
    xrEntityNone = xrParamEntityName + 1,
    xrEntityValue = xrEntityNone + 1,
    xrEntitySystemID = xrEntityValue + 1,
    xrEntityPublicID = xrEntitySystemID + 1,
    xrEntityComplete = xrEntityPublicID + 1,
    xrEntityNotation_Name = xrEntityComplete + 1,
    xrNotationNone = xrEntityNotation_Name + 1,
    xrNotationName = xrNotationNone + 1,
    xrNotationSystemID = xrNotationName + 1,
    xrNotationNoSystem_ID = xrNotationSystemID + 1,
    xrNotationPublicID = xrNotationNoSystem_ID + 1,
    xrAttributeName = xrNotationPublicID + 1,
    xrAttributeType_CDATA = xrAttributeName + 1,
    xrAttributeType_ID = xrAttributeType_CDATA + 1,
    xrAttributeType_IDREF = xrAttributeType_ID + 1,
    xrAttributeType_IDREFS = xrAttributeType_IDREF + 1,
    xrAttributeType_ENTITY = xrAttributeType_IDREFS + 1,
    xrAttributeType_ENTITIES = xrAttributeType_ENTITY + 1,
    xrAttributeType_NMTOKEN = xrAttributeType_ENTITIES + 1,
    xrAttributeType_NMTOKENS = xrAttributeType_NMTOKEN + 1,
    xrAttributeEnumValue = xrAttributeType_NMTOKENS + 1,
    xrAttributeNotationValue = xrAttributeEnumValue + 1,
    xrAttributeListNone = xrAttributeNotationValue + 1,
    xrAttributeListElementName = xrAttributeListNone + 1,
    xrImpliedAttributeValue = xrAttributeListElementName + 1,
    xrRequiredAttributeValue = xrImpliedAttributeValue + 1,
    xrDefaultAttributeValue = xrRequiredAttributeValue + 1,
    xrFixedAttributeValue = xrDefaultAttributeValue + 1,
    xrElementNone = xrFixedAttributeValue + 1,
    xrElementName = xrElementNone + 1,
    xrContentAny = xrElementName + 1,
    xrContentEmpty = xrContentAny + 1,
    xrContentPCData = xrContentEmpty + 1,
    xrGroupOpen = xrContentPCData + 1,
    xrGroupClose = xrGroupOpen + 1,
    xrGroupCloseRep = xrGroupClose + 1,
    xrGroupCloseOpt = xrGroupCloseRep + 1,
    xrGroupClosePlus = xrGroupCloseOpt + 1,
    xrGroupChoice = xrGroupClosePlus + 1,
    xrGroupSequence = xrGroupChoice + 1,
    xrContentElement = xrGroupSequence + 1,
    xrContentElementRep = xrContentElement + 1,
    xrContentElementOpt = xrContentElementRep + 1,
    xrContentElementPlus = xrContentElementOpt + 1,
    xrProcessingInstruction = xrContentElementPlus + 1,
    xrComment = xrProcessingInstruction + 1,

{$IFDEF XML_DTD}
    xrTextDecl = xrComment + 1,
    xrIgnoreSect = xrTextDecl + 1,
    xrInnerParamEntityRef = xrIgnoreSect + 1,
    xrParamEntityRef = xrInnerParamEntityRef + 1
{$ELSE }
    xrParamEntityRef = xrComment + 1
{$ENDIF}
  );

type
  PPrologState = ^TPrologState;

  TPrologState = record
    Handler: function(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
      Enc: PEncoding): TXmlRole;

    Level: Cardinal;
    RoleNone: Integer;

{$IFDEF XML_DTD}
    IncludeLevel: Cardinal;
    DocumentEntity, InEntityValue: Integer;
{$ENDIF}
  end;

procedure XmlPrologStateInit(State: PPrologState);
function XmlTokenRole(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;

implementation

const

{$I ascii.inc }

  { Doesn't check:

    that ,| are not mixed in a model group
    content of literals }

  CKeywordANY: array [0..3] of Char = (ASCII_A, ASCII_N, ASCII_Y, #0);

  CKeywordATTLIST: array [0..7] of Char = (ASCII_A, ASCII_T, ASCII_T, ASCII_L,
    ASCII_I, ASCII_S, ASCII_T, #0);

  CKeywordCDATA: array [0..5] of Char = (ASCII_C, ASCII_D, ASCII_A, ASCII_T,
    ASCII_A, #0);

  CKeywordDOCTYPE: array [0..7] of Char = (ASCII_D, ASCII_O, ASCII_C, ASCII_T,
    ASCII_Y, ASCII_P, ASCII_E, #0);

  CKeywordELEMENT: array [0..7] of Char = (ASCII_E, ASCII_L, ASCII_E, ASCII_M,
    ASCII_E, ASCII_N, ASCII_T, #0);

  CKeywordEMPTY: array [0..5] of Char = (ASCII_E, ASCII_M, ASCII_P, ASCII_T,
    ASCII_Y, #0);

  CKeywordENTITIES: array [0..8] of Char = (ASCII_E, ASCII_N, ASCII_T, ASCII_I,
    ASCII_T, ASCII_I, ASCII_E, ASCII_S, #0);

  CKeywordENTITY: array [0..6] of Char = (ASCII_E, ASCII_N, ASCII_T, ASCII_I,
    ASCII_T, ASCII_Y, #0);

  CKeywordFIXED: array [0..5] of Char = (ASCII_F, ASCII_I, ASCII_X, ASCII_E,
    ASCII_D, #0);

  CKeywordID: array [0..2] of Char = (ASCII_I, ASCII_D, #0);

  CKeywordIDREF: array [0..5] of Char = (ASCII_I, ASCII_D, ASCII_R, ASCII_E,
    ASCII_F, #0);

  CKeywordIDREFS: array [0..6] of Char = (ASCII_I, ASCII_D, ASCII_R, ASCII_E,
    ASCII_F, ASCII_S, #0);

  CKeywordIGNORE: array [0..6] of Char = (ASCII_I, ASCII_G, ASCII_N, ASCII_O,
    ASCII_R, ASCII_E, #0);

  CKeywordIMPLIED: array [0..7] of Char = (ASCII_I, ASCII_M, ASCII_P, ASCII_L,
    ASCII_I, ASCII_E, ASCII_D, #0);

  CKeywordINCLUDE: array [0..7] of Char = (ASCII_I, ASCII_N, ASCII_C, ASCII_L,
    ASCII_U, ASCII_D, ASCII_E, #0);

  CKeywordNDATA: array [0..5] of Char = (ASCII_N, ASCII_D, ASCII_A, ASCII_T,
    ASCII_A, #0);

  CKeywordNMTOKEN: array [0..7] of Char = (ASCII_N, ASCII_M, ASCII_T, ASCII_O,
    ASCII_K, ASCII_E, ASCII_N, #0);

  CKeywordNMTOKENS: array [0..8] of Char = (ASCII_N, ASCII_M, ASCII_T, ASCII_O,
    ASCII_K, ASCII_E, ASCII_N, ASCII_S, #0);

  CKeywordNOTATION: array [0..8] of Char = (ASCII_N, ASCII_O, ASCII_T, ASCII_A,
    ASCII_T, ASCII_I, ASCII_O, ASCII_N, #0);

  CKeywordPCDATA: array [0..6] of Char = (ASCII_P, ASCII_C, ASCII_D, ASCII_A,
    ASCII_T, ASCII_A, #0);

  CKeywordPUBLIC: array [0..6] of Char = (ASCII_P, ASCII_U, ASCII_B, ASCII_L,
    ASCII_I, ASCII_C, #0);

  CKeywordREQUIRED: array [0..8] of Char = (ASCII_R, ASCII_E, ASCII_Q, ASCII_U,
    ASCII_I, ASCII_R, ASCII_E, ASCII_D, #0);

  CKeywordSYSTEM: array [0..6] of Char = (ASCII_S, ASCII_Y, ASCII_S, ASCII_T,
    ASCII_E, ASCII_M, #0);

  
function MinBytesPerChar(Enc: PEncoding): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := Enc.MinBytesPerChar;
end;

function Error(State: PPrologState; Tok: Integer; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  Result := xrNone;
end;

function Common(State: PPrologState; Tok: TXmlTok): TXmlRole;
begin
end;

function InternalSubset(State: PPrologState; Tok: Integer; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): Integer;
begin
end;

function Prolog2(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrNone;

        Exit;
      end;

    xtProcessingInstruction:
      begin
        Result := xrProcessingInstruction;

        Exit;
      end;

    xtComment:
      begin
        Result := xrComment;

        Exit;
      end;

    xtInstanceStart:
      begin
        State.Handler := @Error;

        Result := xrInstanceStart;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Doctype4(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrDocTypeNone;

        Exit;
      end;

    xtOpenBracket:
      begin
        State.Handler := @InternalSubset;

        Result := xrDocTypeInternalSubset;

        Exit;
      end;

    xtDeclClose:
      begin
        State.Handler := @Prolog2;

        Result := xrDocTypeClose;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Doctype3(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrDocTypeNone;

        Exit;
      end;

    xtLiteral:
      begin
        State.Handler := @Doctype4;

        Result := xrDocTypeSystemID;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Doctype2(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrDocTypeNone;

        Exit;
      end;

    xtLiteral:
      begin
        State.Handler := @Doctype3;

        Result := xrDocTypePublicID;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Doctype1(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrDocTypeNone;

        Exit;
      end;

    xtOpenBracket:
      begin
        State.Handler := @InternalSubset;

        Result := xrDocTypeInternalSubset;

        Exit;
      end;

    xtDeclClose:
      begin
        State.Handler := @Prolog2;

        Result := xrDocTypeClose;

        Exit;
      end;

    xtName:
      begin
        if XmlNameMatchesAscii(Enc, Ptr, Stop, @CKeywordSYSTEM[0]) <> 0 then
        begin
          State.Handler := @Doctype3;

          Result := xrDocTypeNone;

          Exit;
        end;

        if XmlNameMatchesAscii(Enc, Ptr, Stop, @CKeywordPUBLIC[0]) <> 0 then
        begin
          State.Handler := @Doctype2;

          Result := xrDocTypeNone;

          Exit;
        end;
      end;
  end;

  Result := Common(State, Tok);
end;

function Doctype0(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrDocTypeNone;

        Exit;
      end;

    xtName, xtPrefixedName:
      begin
        State.Handler := @Doctype1;

        Result := xrDocTypeName;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Prolog1(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        Result := xrNone;

        Exit;
      end;

    xtProcessingInstruction:
      begin
        Result := xrProcessingInstruction;

        Exit;
      end;

    xtComment:
      begin
        Result := xrComment;

        Exit;
      end;

    xtBOM:
      begin
        Result := xrNone;

        Exit;
      end;

    xtDeclOpen:
      if XmlNameMatchesAscii(Enc,
        PAnsiChar(PtrComp(Ptr) + 2 * MinBytesPerChar(Enc)), Stop,
        @CKeywordDOCTYPE[0]) = 0 then
      else
      begin
        State.Handler := @Doctype0;

        Result := xrDocTypeNone;

        Exit;
      end;

    xtInstanceStart:
      begin
        State.Handler := @Error;

        Result := xrInstanceStart;

        Exit;
      end;
  end;

  Result := Common(State, Tok);
end;

function Prolog0(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  case Tok of
    xtProlog_S:
      begin
        State.Handler := @Prolog1;
        Result := xrNone;
      end;

    xtXmlDecl:
      begin
        State.Handler := @Prolog1;
        Result := xrXmlDecl;
      end;

    xtProcessingInstruction:
      begin
        State.Handler := @Prolog1;
        Result := xrProcessingInstruction;
      end;

    xtComment:
      begin
        State.Handler := @Prolog1;
        Result := xrComment;
      end;

    xtBOM:
      Result := xrNone;

    xtDeclOpen:
      begin
        if XmlNameMatchesAscii(Enc,
          PAnsiChar(PtrComp(Ptr) + 2 * MinBytesPerChar(Enc)), Stop,
          @CKeywordDOCTYPE[0]) = 0 then
        begin
          Result := Common(State, Tok);
          Exit;
        end;

        State.Handler := @Doctype0;

        Result := xrDocTypeNone;
      end;

    xtInstanceStart:
      begin
        State.Handler := @Error;

        Result := xrInstanceStart;
      end;
  end;
end;

procedure XmlPrologStateInit(State: PPrologState);
begin
  State.Handler := @Prolog0;

{$IFDEF XML_DTD}
  State.DocumentEntity := 1;
  State.IncludeLevel := 0;
  State.InEntityValue := 0;
{$ENDIF}
end;

function XmlTokenRole(State: PPrologState; Tok: TXmlTok; Ptr, Stop: PAnsiChar;
  Enc: PEncoding): TXmlRole;
begin
  Result := State.Handler(State, Tok, Ptr, Stop, Enc);
end;

end.
