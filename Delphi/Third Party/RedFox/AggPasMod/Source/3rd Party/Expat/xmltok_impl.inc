// ----------------------------------------------------------------------------
// copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
// milan@marusinec.sk
// http://www.pasports.org/pasexpat
// copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a Copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, Copy, modify, merge, publish,
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
// IN NO EVENT SHALL THE AUTHORS OR copyright HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// [Pascal Port History] -----------------------------------------------------
//
// 17.05.2006-Milano: Unit port establishment
// 02.06.2006-Milano: porting
// 05.06.2006-Milano: -"-
// 06.06.2006-Milano: -"-
// 12.06.2006-Milano: -"-
// 15.06.2006-Milano: normal_contentTok
// 16.06.2006-Milano: -"-, porting
// 17.06.2006-Milano: -"-
// 21.06.2006-Milano: -"-
// 22.06.2006-Milano: -"-
//
{ xmltok_impl.inc }
{$Q-}
{$R-}

{ normal_scanRef {.. }
function Normal_scanRef(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_scanAtts }
function Normal_scanAtts(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
{$IFDEF XML_NS }
  HadColon: Integer;

{$ENDIF }
  T, Open, Tok: Integer;

label
  _bt0, _bt1, _bte, Sol, Gt, _bt2;

begin
{$IFDEF XML_NS }
  HadColon := 0;

{$ENDIF }
  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end
        else
          goto _bt0;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt0:
        Inc(PtrComp(Ptr), MINBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      { CHECK_NAME_CASES #define }

{$IFDEF XML_NS }
      BT_COLON:
        begin
          if HadColon <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          HadColon := 1;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr <> End_ then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          case BYTE_TYPE(Enc, Ptr) of
            { #define CHECK_NMSTRT_CASES }
            BT_NONASCII:
              if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end
              else
                goto _bt1;

            BT_NMSTRT, BT_HEX:
            _bt1:
              Inc(PtrComp(Ptr), MINBPC(Enc));

            BT_LEAD2:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 2 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 2);

              end;

            BT_LEAD3:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 3 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 3);

              end;

            BT_LEAD4:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 4 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 4);

              end;

            { CHECK_NMSTRT_CASES #define }

          else
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

          end;

        end;

{$ENDIF }
      BT_S, BT_CR, BT_LF:
        begin
          repeat
            Inc(PtrComp(Ptr), MINBPC(Enc));

            if Ptr <> End_ then
              Result := XML_TOK_PARTIAL;

            T := BYTE_TYPE(Enc, Ptr);

            if T = BT_EQUALS then
              Break;

            case T of
              BT_S, BT_LF, BT_CR:
                Break;

            else
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

            end;

          until False;

          { fall through }
          goto _bte;

        end;

      BT_EQUALS:
      _bte:
        begin
{$IFDEF XML_NS }
          HadColon := 0;

{$ENDIF }
          repeat
            Inc(PtrComp(Ptr), MINBPC(Enc));

            if Ptr = End_ then
            begin
              Result := XML_TOK_PARTIAL;

              Exit;

            end;

            Open := BYTE_TYPE(Enc, Ptr);

            if (Open = BT_QUOT) or (Open = BT_APOS) then
              Break;

            case Open of
              BT_S, BT_LF, BT_CR:
              else
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

            end;

          until False;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          { in attribute value }
          repeat
            if Ptr = End_ then
            begin
              Result := XML_TOK_PARTIAL;

              Exit;

            end;

            T := BYTE_TYPE(Enc, Ptr);

            if T = Open then
              Break;

            case T of
              { #define INVALID_CASES }
              BT_LEAD2:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 2 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 2) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 2);

                end;

              BT_LEAD3:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 3 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 3) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 3);

                end;

              BT_LEAD4:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 4 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 4) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 4);

                end;

              BT_NONXML, BT_MALFORM, BT_TRAIL:
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                  Exit;

                end;

              { INVALID_CASES #define }

              BT_AMP:
                begin
                  Tok := Normal_scanRef(Enc,
                    PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, @Ptr);

                  if Tok <= 0 then
                  begin
                    if Tok = XML_TOK_INVALID then
                      NextTokPtr^ := Ptr;

                    Result := Tok;

                    Exit;

                  end;

                end;

              BT_LT:
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                  Exit;

                end;

            else
              Inc(PtrComp(Ptr), MINBPC(Enc));

            end;

          until False;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr = End_ then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          case BYTE_TYPE(Enc, Ptr) of
            BT_SOL:
              goto Sol;

            BT_GT:
              goto Gt;

            BT_S, BT_CR, BT_LF:
            else
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

          end;

          { ptr points to closing quote }
          repeat
            Inc(PtrComp(Ptr), MINBPC(Enc));

            if Ptr = End_ then
            begin
              Result := XML_TOK_PARTIAL;

              Exit;

            end;

            case BYTE_TYPE(Enc, Ptr) of
              { #define CHECK_NMSTRT_CASES }
              BT_NONASCII:
                if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                  Exit;

                end
                else
                  goto _bt2;

              BT_NMSTRT, BT_HEX:
              _bt2:
                Inc(PtrComp(Ptr), MINBPC(Enc));

              BT_LEAD2:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 2 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 2);

                end;

              BT_LEAD3:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 3 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 3);

                end;

              BT_LEAD4:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 4 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 4);

                end;

              { CHECK_NMSTRT_CASES #define }

              BT_S, BT_CR, BT_LF:
                Continue;

              BT_GT:
              Gt:
                begin
                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

                  Result := XML_TOK_START_TAG_WITH_ATTS;

                  Exit;

                end;

              BT_SOL:
              Sol:
                begin
                  Inc(PtrComp(Ptr), MINBPC(Enc));

                  if Ptr = End_ then
                  begin
                    Result := XML_TOK_PARTIAL;

                    Exit;
                  end;

                  if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;
                  end;

                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

                  Result := XML_TOK_EMPTY_ELEMENT_WITH_ATTS;

                  Exit;
                end;

            else
              begin
                NextTokPtr^ := Ptr;
                Result := XML_TOK_INVALID;
                Exit;
              end;

            end;

            Break;
          until False;
        end;
    else
      begin
        NextTokPtr^ := Ptr;
        Result := XML_TOK_INVALID;
        Exit;
      end;
    end;
  Result := XML_TOK_PARTIAL;
end;

{ normal_scanEndTag }
{ ptr points to character following "</" }
function Normal_scanEndTag(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
label
  _bt0, _bt1;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_PARTIAL;

    Exit;
  end;

  case BYTE_TYPE(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;
      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MINBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 2 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;
        end;

        Inc(PtrComp(Ptr), 2);
      end;

    BT_LEAD3:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 3 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;
        end;

        Inc(PtrComp(Ptr), 3);
      end;

    BT_LEAD4:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 4 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;
        end;

        Inc(PtrComp(Ptr), 4);
      end;

    { CHECK_NMSTRT_CASES #define }

  else
    begin
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

  end;

  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MINBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      { CHECK_NAME_CASES #define }

      BT_S, BT_CR, BT_LF:
        begin
          Inc(PtrComp(Ptr), MINBPC(Enc));

          while Ptr <> End_ do
          begin
            case BYTE_TYPE(Enc, Ptr) of
              BT_GT:
                begin
                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

                  Result := XML_TOK_END_TAG;

                  Exit;

                end;

              BT_S, BT_CR, BT_LF:
              else
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

            end;

            Inc(PtrComp(Ptr), MINBPC(Enc));

          end;

          Result := XML_TOK_PARTIAL;

          Exit;

        end;

{$IFDEF XML_NS }
      BT_COLON:
        { no need to check qname syntax here,
          since end-tag must match exactly }
        Inc(PtrComp(Ptr), MINBPC(Enc));

{$ENDIF }
      BT_GT:
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_END_TAG;

          Exit;

        end;

    else
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    end;

  Result := XML_TOK_PARTIAL;

end;

{ normal_scanComment }
{ ptr points to character following "<!-" }
function Normal_scanComment(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
  if Ptr <> End_ then
  begin
    if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_MINUS)) = 0 then
    begin
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

    Inc(PtrComp(Ptr), MINBPC(Enc));

    while Ptr <> End_ do
      case BYTE_TYPE(Enc, Ptr) of
        { #define INVALID_CASES }
        BT_LEAD2:
          begin
            if PtrComp(End_) - PtrComp(Ptr) < 2 then
            begin
              Result := XML_TOK_PARTIAL_CHAR;

              Exit;

            end;

            if IS_INVALID_CHAR(Enc, Ptr, 2) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

            Inc(PtrComp(Ptr), 2);

          end;

        BT_LEAD3:
          begin
            if PtrComp(End_) - PtrComp(Ptr) < 3 then
            begin
              Result := XML_TOK_PARTIAL_CHAR;

              Exit;

            end;

            if IS_INVALID_CHAR(Enc, Ptr, 3) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

            Inc(PtrComp(Ptr), 3);

          end;

        BT_LEAD4:
          begin
            if PtrComp(End_) - PtrComp(Ptr) < 4 then
            begin
              Result := XML_TOK_PARTIAL_CHAR;

              Exit;

            end;

            if IS_INVALID_CHAR(Enc, Ptr, 4) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

            Inc(PtrComp(Ptr), 4);

          end;

        BT_NONXML, BT_MALFORM, BT_TRAIL:
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

        { INVALID_CASES #define }

        BT_MINUS:
          begin
            Inc(PtrComp(Ptr), MINBPC(Enc));

            if Ptr = End_ then
            begin
              Result := XML_TOK_PARTIAL;

              Exit;

            end;

            if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_MINUS)) <> 0 then
            begin
              Inc(PtrComp(Ptr), MINBPC(Enc));

              if Ptr = End_ then
              begin
                Result := XML_TOK_PARTIAL;

                Exit;

              end;

              if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

              Result := XML_TOK_COMMENT;

              Exit;

            end;

          end;

      else
        Inc(PtrComp(Ptr), MINBPC(Enc));

      end;

  end;

  Result := XML_TOK_PARTIAL;

end;

{ normal_scanCdataSection {.. }
function Normal_scanCdataSection(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_checkPiTarget }
function Normal_checkPiTarget(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  TokPtr: PInteger): Integer;
var
  Upper: Integer;

begin
  Upper := 0;
  TokPtr^ := XML_TOK_PI;

  if PtrComp(End_) - PtrComp(Ptr) <> MINBPC(Enc) * 3 then
  begin
    Result := 1;

    Exit;

  end;

  case BYTETo_ASCII(Enc, Ptr) of
    Integer(ASCII_X):
      Upper := 1;

    Integer(ASCII_xl):
    else
    begin
      Result := 1;

      Exit;

    end;

  end;

  Inc(PtrComp(Ptr), MINBPC(Enc));

  case BYTETo_ASCII(Enc, Ptr) of
    Integer(ASCII_M):
      Upper := 1;

    Integer(ASCII_ml):
    else
    begin
      Result := 1;

      Exit;

    end;

  end;

  Inc(PtrComp(Ptr), MINBPC(Enc));

  case BYTETo_ASCII(Enc, Ptr) of
    Integer(ASCII_L):
      Upper := 1;

    Integer(ASCII_ll):
    else
    begin
      Result := 1;

      Exit;

    end;

  end;

  if Upper <> 0 then
  begin
    Result := 0;

    Exit;

  end;

  TokPtr^ := XML_TOK_XML_DECL;
  Result := 1;

end;

{ normal_scanPi }
{ ptr points to character following "<?" }
function Normal_scanPi(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
  Tok: Integer;

  Target: PAnsiChar;

label
  _bt0, _bt1, _else;

begin
  Target := Ptr;

  if Ptr = End_ then
  begin
    Result := XML_TOK_PARTIAL;

    Exit;

  end;

  case BYTE_TYPE(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MINBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 2 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 2);

      end;

    BT_LEAD3:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 3 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 3);

      end;

    BT_LEAD4:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 4 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 4);

      end;

    { CHECK_NMSTRT_CASES #define }

  else
    begin
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

  end;

  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MINBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      { CHECK_NAME_CASES #define }

      BT_S, BT_CR, BT_LF:
        begin
          if Normal_checkPiTarget(Enc, Target, Ptr, @Tok) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          while Ptr <> End_ do
            case BYTE_TYPE(Enc, Ptr) of
              { #define INVALID_CASES }
              BT_LEAD2:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 2 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 2) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 2);

                end;

              BT_LEAD3:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 3 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 3) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 3);

                end;

              BT_LEAD4:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 4 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if IS_INVALID_CHAR(Enc, Ptr, 4) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                    Exit;

                  end;

                  Inc(PtrComp(Ptr), 4);

                end;

              BT_NONXML, BT_MALFORM, BT_TRAIL:
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                  Exit;

                end;

              { INVALID_CASES #define }

              BT_QUEST:
                begin
                  Inc(PtrComp(Ptr), MINBPC(Enc));

                  if Ptr = End_ then
                  begin
                    Result := XML_TOK_PARTIAL;

                    Exit;

                  end;

                  if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) <> 0 then
                  begin
                    NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

                    Result := Tok;

                    Exit;

                  end;

                end;

            else
              Inc(PtrComp(Ptr), MINBPC(Enc));

            end;

          Result := XML_TOK_PARTIAL;

          Exit;

        end;

      BT_QUEST:
        begin
          if Normal_checkPiTarget(Enc, Target, Ptr, @Tok) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr = End_ then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) <> 0 then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

            Result := Tok;

            Exit;

          end;

          { fall through }
          goto _else;

        end;

    else
      begin
      _else:
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    end;

  Result := XML_TOK_PARTIAL;

end;

{ normal_scanLt }
{ ptr points to character following "<" }
function Normal_scanLt(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
{$IFDEF XML_NS }
var
  HadColon: Integer;

{$ENDIF }
label
  _bt0, _bt1, _bt2, _bt3, Gt, Sol;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_PARTIAL;

    Exit;

  end;

  case BYTE_TYPE(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MINBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 2 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 2);

      end;

    BT_LEAD3:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 3 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 3);

      end;

    BT_LEAD4:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 4 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

        end;

        Inc(PtrComp(Ptr), 4);

      end;

    { CHECK_NMSTRT_CASES #define }

    BT_EXCL:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := XML_TOK_PARTIAL;

          Exit;

        end;

        case BYTE_TYPE(Enc, Ptr) of
          BT_MINUS:
            begin
              Result := Normal_scanComment(Enc,
                PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, NextTokPtr);

              Exit;

            end;

          BT_LSQB:
            begin
              Result := Normal_scanCdataSection(Enc,
                PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, NextTokPtr);

              Exit;

            end;

        end;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    BT_QUEST:
      begin
        Result := Normal_scanPi(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

    BT_SOL:
      begin
        Result := Normal_scanEndTag(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

  else
    begin
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

  end;

{$IFDEF XML_NS }
  HadColon := 0;

{$ENDIF }
  { we have a start-tag }
  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MINBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      { CHECK_NAME_CASES #define }

{$IFDEF XML_NS }
      BT_COLON:
        begin
          if HadColon <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          HadColon := 1;

          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr = End_ then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          case BYTE_TYPE(Enc, Ptr) of
            { #define CHECK_NMSTRT_CASES }
            BT_NONASCII:
              if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end
              else
                goto _bt2;

            BT_NMSTRT, BT_HEX:
            _bt2:
              Inc(PtrComp(Ptr), MINBPC(Enc));

            BT_LEAD2:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 2 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 2);

              end;

            BT_LEAD3:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 3 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 3);

              end;

            BT_LEAD4:
              begin
                if PtrComp(End_) - PtrComp(Ptr) < 4 then
                begin
                  Result := XML_TOK_PARTIAL_CHAR;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                end;

                Inc(PtrComp(Ptr), 4);

              end;

            { CHECK_NMSTRT_CASES #define }

          else
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_INVALID;

              Exit;

            end;

          end;

        end;

{$ENDIF }
      BT_S, BT_CR, BT_LF:
        begin
          Inc(PtrComp(Ptr), MINBPC(Enc));

          while Ptr <> End_ do
          begin
            case BYTE_TYPE(Enc, Ptr) of
              { #define CHECK_NMSTRT_CASES }
              BT_NONASCII:
                if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := XML_TOK_INVALID;

                  Exit;

                end
                else
                  goto _bt3;

              BT_NMSTRT, BT_HEX:
              _bt3:
                Inc(PtrComp(Ptr), MINBPC(Enc));

              BT_LEAD2:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 2 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 2);

                end;

              BT_LEAD3:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 3 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 3);

                end;

              BT_LEAD4:
                begin
                  if PtrComp(End_) - PtrComp(Ptr) < 4 then
                  begin
                    Result := XML_TOK_PARTIAL_CHAR;

                    Exit;

                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := XML_TOK_INVALID;

                  end;

                  Inc(PtrComp(Ptr), 4);

                end;

              { CHECK_NMSTRT_CASES #define }

              BT_GT:
                goto Gt;

              BT_SOL:
                goto Sol;

              BT_S, BT_CR, BT_LF:
                begin
                  Inc(PtrComp(Ptr), MINBPC(Enc));

                  Continue;

                end;

            else
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

            end;

            Result := Normal_scanAtts(Enc, Ptr, End_, NextTokPtr);

            Exit;

          end;

          Result := XML_TOK_PARTIAL;

          Exit;

        end;

      BT_GT:
      Gt:
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_START_TAG_NO_ATTS;

          Exit;

        end;

      BT_SOL:
      Sol:
        begin
          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr <> End_ then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_EMPTY_ELEMENT_NO_ATTS;

          Exit;

        end;

    else
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    end;

  Result := XML_TOK_PARTIAL;

end;

{ normal_scanDecl }
{ ptr points to character following "<!" }
function Normal_scanDecl(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
label
  _fall0;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_PARTIAL;

    Exit;

  end;

  case BYTE_TYPE(Enc, Ptr) of
    BT_MINUS:
      begin
        Result := Normal_scanComment(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

    BT_LSQB:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_COND_SECT_OPEN;

        Exit;

      end;

    BT_NMSTRT, BT_HEX:
      Inc(PtrComp(Ptr), MINBPC(Enc));

  else
    begin
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

  end;

  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      BT_PERCNT:
        begin
          if PtrComp(Ptr) + MINBPC(Enc) = PtrComp(End_) then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          { don't alLow <!ENTITY% foo "whatever"> }
          case BYTE_TYPE(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc))) of
            BT_S, BT_CR, BT_LF, BT_PERCNT:
              begin
                NextTokPtr^ := Ptr;

                Result := XML_TOK_INVALID;

                Exit;

              end;

          end;

          { fall through }
          goto _fall0;

        end;

      BT_S, BT_CR, BT_LF:
      _fall0:
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_DECL_OPEN;

          Exit;

        end;

      BT_NMSTRT, BT_HEX:
        Inc(PtrComp(Ptr), MINBPC(Enc));

    else
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    end;

  Result := XML_TOK_PARTIAL;

end;

{ scanPercent {.. }
{ ptr points to character following "%" }
function ScanPercent(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ scanPoundName {.. }
function ScanPoundName(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_scanLit }
function Normal_scanLit(Open: Integer; Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
  T: Integer;

label
  _break;

begin
  while Ptr <> End_ do
  begin
    T := BYTE_TYPE(Enc, Ptr);

    case T of
      { #define INVALID_CASES }
      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_INVALID_CHAR(Enc, Ptr, 2) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_INVALID_CHAR(Enc, Ptr, 3) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_INVALID_CHAR(Enc, Ptr, 4) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      BT_NONXML, BT_MALFORM, BT_TRAIL:
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

      { INVALID_CASES #define }

      BT_QUOT, BT_APOS:
        begin
          Inc(PtrComp(Ptr), MINBPC(Enc));

          if T <> Open then
            goto _break;

          if Ptr = End_ then
          begin
            Result := -XML_TOK_LITERAL;

            Exit;

          end;

          NextTokPtr^ := Ptr;

          case BYTE_TYPE(Enc, Ptr) of
            BT_S, BT_CR, BT_LF, BT_GT, BT_PERCNT, BT_LSQB:
              begin
                Result := XML_TOK_LITERAL;

                Exit;

              end;

          else
            begin
              Result := XML_TOK_INVALID;

              Exit;

            end;

          end;

        end;

    else
      Inc(PtrComp(Ptr), MINBPC(Enc));

    end;

  _break:
  end;

  Result := XML_TOK_PARTIAL;

end;

{ normal_prologTok }
function Normal_prologTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
  Tok: Integer;

  N: Size_t;

label
  _bt_s, _else, _else2, _bt0, _bt1;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_NONE;

    Exit;

  end;

  if MINBPC(Enc) > 1 then
  begin
    N := PtrComp(End_) - PtrComp(Ptr);

    if N and (MINBPC(Enc) - 1) <> 0 then
    begin
      N := N and not(MINBPC(Enc) - 1);

      if N = 0 then
      begin
        Result := XML_TOK_PARTIAL;

        Exit;

      end;

      End_ := PAnsiChar(PtrComp(Ptr) + N);

    end;

  end;

  case BYTE_TYPE(Enc, Ptr) of
    BT_QUOT:
      begin
        Result := Normal_scanLit(BT_QUOT, Enc,
          PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, NextTokPtr);

        Exit;

      end;

    BT_APOS:
      begin
        Result := Normal_scanLit(BT_APOS, Enc,
          PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, NextTokPtr);

        Exit;

      end;

    BT_LT:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := XML_TOK_PARTIAL;

          Exit;

        end;

        case BYTE_TYPE(Enc, Ptr) of
          BT_EXCL:
            begin
              Result := Normal_scanDecl(Enc,
                PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_, NextTokPtr);

              Exit;

            end;

          BT_QUEST:
            begin
              Result := Normal_scanPi(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)
                ), End_, NextTokPtr);

              Exit;

            end;

          BT_NMSTRT, BT_HEX, BT_NONASCII, BT_LEAD2, BT_LEAD3, BT_LEAD4:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) - MINBPC(Enc));

              Result := XML_TOK_INSTANCE_START;

              Exit;

            end;

        end;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    BT_CR:
      if PtrComp(Ptr) + MINBPC(Enc) = PtrComp(End_) then
      begin
        NextTokPtr^ := End_;

        { indicate that this might be part of a CR/LF pair }
        Result := -XML_TOK_PROLOG_S;

        Exit;

      end
      else
        { fall through }
        goto _bt_s;

    BT_S, BT_LF:
    _bt_s:
      begin
        repeat
          Inc(PtrComp(Ptr), MINBPC(Enc));

          if Ptr = End_ then
            Break;

          case BYTE_TYPE(Enc, Ptr) of
            BT_CR:
              { don't split CR/LF pair }
              if PtrComp(Ptr) + MINBPC(Enc) <> PtrComp(End_) then
              else
                { fall through }
                goto _else;

            BT_S, BT_LF:
            else
            begin
            _else:
              NextTokPtr^ := Ptr;

              Result := XML_TOK_PROLOG_S;

              Exit;

            end;

          end;

        until False;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_PROLOG_S;

        Exit;

      end;

    BT_PERCNT:
      begin
        Result := ScanPercent(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)), End_,
          NextTokPtr);

        Exit;

      end;

    BT_COMMA:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_COMMA;

        Exit;

      end;

    BT_LSQB:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_OPEN_BRACKET;

        Exit;

      end;

    BT_RSQB:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := -XML_TOK_CLOSE_BRACKET;

          Exit;

        end;

        if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_RSQB)) <> 0 then
        begin
          if PtrComp(Ptr) + MINBPC(Enc) = PtrComp(End_) then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;

          end;

          if CHAR_MATCHES(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
            Integer(ASCII_GT)) <> 0 then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2 * MINBPC(Enc));

            Result := XML_TOK_COND_SECT_CLOSE;

            Exit;

          end;

        end;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_CLOSE_BRACKET;

        Exit;

      end;

    BT_LPAR:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_OPEN_PAREN;

        Exit;

      end;

    BT_RPAR:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := -XML_TOK_CLOSE_PAREN;

          Exit;

        end;

        case BYTE_TYPE(Enc, Ptr) of
          BT_AST:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

              Result := XML_TOK_CLOSE_PAREN_ASTERISK;

              Exit;

            end;

          BT_QUEST:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

              Result := XML_TOK_CLOSE_PAREN_QUESTION;

              Exit;

            end;

          BT_PLUS:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

              Result := XML_TOK_CLOSE_PAREN_PLUS;

              Exit;

            end;

          BT_CR, BT_LF, BT_S, BT_GT, BT_COMMA, BT_VERBAR, BT_RPAR:
            begin
              NextTokPtr^ := Ptr;

              Result := XML_TOK_CLOSE_PAREN;

              Exit;

            end;

        end;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    BT_VERBAR:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_OR;

        Exit;

      end;

    BT_GT:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_DECL_CLOSE;

        Exit;

      end;

    BT_NUM:
      begin
        Result := ScanPoundName(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

    BT_LEAD2:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 2 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 2) <> 0 then
        begin
          Inc(PtrComp(Ptr), 2);

          Tok := XML_TOK_NAME;

        end
        else if IS_NAME_CHAR(Enc, Ptr, 2) <> 0 then
        begin
          Inc(PtrComp(Ptr), 2);

          Tok := XML_TOK_NMTOKEN;

        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

      end;

    BT_LEAD3:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 3 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 3) <> 0 then
        begin
          Inc(PtrComp(Ptr), 3);

          Tok := XML_TOK_NAME;

        end
        else if IS_NAME_CHAR(Enc, Ptr, 3) <> 0 then
        begin
          Inc(PtrComp(Ptr), 3);

          Tok := XML_TOK_NMTOKEN;

        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

      end;

    BT_LEAD4:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 4 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 4) <> 0 then
        begin
          Inc(PtrComp(Ptr), 4);

          Tok := XML_TOK_NAME;

        end
        else if IS_NAME_CHAR(Enc, Ptr, 4) <> 0 then
        begin
          Inc(PtrComp(Ptr), 4);

          Tok := XML_TOK_NMTOKEN;

        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

      end;

    BT_NMSTRT, BT_HEX:
      begin
        Tok := XML_TOK_NAME;

        Inc(PtrComp(Ptr), MINBPC(Enc));

      end;

    BT_DIGIT, BT_NAME, BT_MINUS {$IFDEF XML_NS } , BT_COLON: {$ELSE }: {$ENDIF }
      begin
        Tok := XML_TOK_NMTOKEN;

        Inc(PtrComp(Ptr), MINBPC(Enc));

      end;

    BT_NONASCII:
      if IS_NMSTRT_CHAR_MINBPC(Enc, Ptr) <> 0 then
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        Tok := XML_TOK_NAME;

      end
      else if IS_NAME_CHAR_MINBPC(Enc, Ptr) <> 0 then
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        Tok := XML_TOK_NMTOKEN;

      end
      else
        { fall through }
        goto _else2;

  else
    begin
    _else2:
      NextTokPtr^ := Ptr;

      Result := XML_TOK_INVALID;

      Exit;

    end;

  end;

  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end
        else
          goto _bt0;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt0:
        Inc(PtrComp(Ptr), MINBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 2 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 3 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(End_) - PtrComp(Ptr) < 4 then
          begin
            Result := XML_TOK_PARTIAL_CHAR;

            Exit;

          end;

          if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      { CHECK_NAME_CASES #define }

      BT_GT, BT_RPAR, BT_COMMA, BT_VERBAR, BT_LSQB, BT_PERCNT, BT_S,
        BT_CR, BT_LF:
        begin
          NextTokPtr^ := Ptr;

          Result := Tok;

          Exit;

        end;

{$IFDEF XML_NS }
      BT_COLON:
        begin
          Inc(PtrComp(Ptr), MINBPC(Enc));

          case Tok of
            XML_TOK_NAME:
              begin
                if Ptr = End_ then
                begin
                  Result := XML_TOK_PARTIAL;

                  Exit;

                end;

                Tok := XML_TOK_PREFIXED_NAME;

                case BYTE_TYPE(Enc, Ptr) of
                  { #define CHECK_NAME_CASES }
                  BT_NONASCII:
                    if IS_NAME_CHAR_MINBPC(Enc, Ptr) = 0 then
                    begin
                      NextTokPtr^ := Ptr;

                      Result := XML_TOK_INVALID;

                      Exit;

                    end
                    else
                      goto _bt1;

                  BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
                  _bt1:
                    Inc(PtrComp(Ptr), MINBPC(Enc));

                  BT_LEAD2:
                    begin
                      if PtrComp(End_) - PtrComp(Ptr) < 2 then
                      begin
                        Result := XML_TOK_PARTIAL_CHAR;

                        Exit;

                      end;

                      if IS_NAME_CHAR(Enc, Ptr, 2) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := XML_TOK_INVALID;

                        Exit;

                      end;

                      Inc(PtrComp(Ptr), 2);

                    end;

                  BT_LEAD3:
                    begin
                      if PtrComp(End_) - PtrComp(Ptr) < 3 then
                      begin
                        Result := XML_TOK_PARTIAL_CHAR;

                        Exit;

                      end;

                      if IS_NAME_CHAR(Enc, Ptr, 3) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := XML_TOK_INVALID;

                        Exit;

                      end;

                      Inc(PtrComp(Ptr), 3);

                    end;

                  BT_LEAD4:
                    begin
                      if PtrComp(End_) - PtrComp(Ptr) < 4 then
                      begin
                        Result := XML_TOK_PARTIAL_CHAR;

                        Exit;

                      end;

                      if IS_NAME_CHAR(Enc, Ptr, 4) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := XML_TOK_INVALID;

                        Exit;

                      end;

                      Inc(PtrComp(Ptr), 4);

                    end;

                  { CHECK_NAME_CASES #define }

                else
                  Tok := XML_TOK_NMTOKEN;

                end;

              end;

            XML_TOK_PREFIXED_NAME:
              Tok := XML_TOK_NMTOKEN;

          end;

        end;

{$ENDIF }
      BT_PLUS:
        begin
          if Tok = XML_TOK_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_NAME_PLUS;

          Exit;

        end;

      BT_AST:
        begin
          if Tok = XML_TOK_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_NAME_ASTERISK;

          Exit;

        end;

      BT_QUEST:
        begin
          if Tok = XML_TOK_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_INVALID;

            Exit;

          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          Result := XML_TOK_NAME_QUESTION;

          Exit;

        end;

    else
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    end;

  Result := -Tok;

end;

{ normal_contentTok }
function Normal_contentTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
  N: Size_t;

label
  _break, _go0, _break2;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_NONE;

    Exit;

  end;

  if MINBPC(Enc) > 1 then
  begin
    N := PtrComp(End_) - PtrComp(Ptr);

    if N and (MINBPC(Enc) - 1) <> 0 then
    begin
      N := N and not(MINBPC(Enc) - 1);

      if N = 0 then
      begin
        Result := XML_TOK_PARTIAL;

        Exit;

      end;

      End_ := PAnsiChar(PtrComp(Ptr) + N);

    end;

  end;

  case BYTE_TYPE(Enc, Ptr) of
    BT_LT:
      begin
        Result := Normal_scanLt(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

    BT_AMP:
      begin
        Result := Normal_scanRef(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
          End_, NextTokPtr);

        Exit;

      end;

    BT_CR:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := XML_TOK_TRAILING_CR;

          Exit;

        end;

        if BYTE_TYPE(Enc, Ptr) = BT_LF then
          Inc(PtrComp(Ptr), MINBPC(Enc));

        NextTokPtr^ := Ptr;

        Result := XML_TOK_DATA_NEWLINE;

        Exit;

      end;

    BT_LF:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

        Result := XML_TOK_DATA_NEWLINE;

        Exit;

      end;

    BT_RSQB:
      begin
        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := XML_TOK_TRAILING_RSQB;

          Exit;

        end;

        if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_RSQB)) = 0 then
          goto _break;

        Inc(PtrComp(Ptr), MINBPC(Enc));

        if Ptr = End_ then
        begin
          Result := XML_TOK_TRAILING_RSQB;

          Exit;

        end;

        if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_GT)) = 0 then
        begin
          Dec(PtrComp(Ptr), MINBPC(Enc));

          goto _break;

        end;

        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    { #define INVALID_CASES }
    BT_LEAD2:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 2 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_INVALID_CHAR(Enc, Ptr, 2) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

        Inc(PtrComp(Ptr), 2);

      end;

    BT_LEAD3:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 3 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_INVALID_CHAR(Enc, Ptr, 3) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

        Inc(PtrComp(Ptr), 3);

      end;

    BT_LEAD4:
      begin
        if PtrComp(End_) - PtrComp(Ptr) < 4 then
        begin
          Result := XML_TOK_PARTIAL_CHAR;

          Exit;

        end;

        if IS_INVALID_CHAR(Enc, Ptr, 4) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

        Inc(PtrComp(Ptr), 4);

      end;

    BT_NONXML, BT_MALFORM, BT_TRAIL:
      begin
        NextTokPtr^ := Ptr;

        Result := XML_TOK_INVALID;

        Exit;

      end;

    { INVALID_CASES #define }

  else
    Inc(PtrComp(Ptr), MINBPC(Enc));

  end;

_break:
  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      BT_LEAD2:
        begin
          if (PtrComp(End_) - PtrComp(Ptr) < 2) or
            (IS_INVALID_CHAR(Enc, Ptr, 2) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_DATA_CHARS;

            Exit;

          end;

          Inc(PtrComp(Ptr), 2);

        end;

      BT_LEAD3:
        begin
          if (PtrComp(End_) - PtrComp(Ptr) < 3) or
            (IS_INVALID_CHAR(Enc, Ptr, 3) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_DATA_CHARS;

            Exit;

          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if (PtrComp(End_) - PtrComp(Ptr) < 4) or
            (IS_INVALID_CHAR(Enc, Ptr, 4) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := XML_TOK_DATA_CHARS;

            Exit;

          end;

          Inc(PtrComp(Ptr), 4);

        end;

      BT_RSQB:
        begin
          if PtrComp(Ptr) + MINBPC(Enc) <> PtrComp(End_) then
          begin
            if CHAR_MATCHES(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
              Integer(ASCII_RSQB)) = 0 then
            begin
              Inc(PtrComp(Ptr), MINBPC(Enc));

              goto _break2;

            end;

            if PtrComp(Ptr) + 2 * MINBPC(Enc) <> PtrComp(End_) then
            begin
              if CHAR_MATCHES(Enc, PAnsiChar(PtrComp(Ptr) + 2 * MINBPC(Enc)),
                Integer(ASCII_GT)) = 0 then
              begin
                Inc(PtrComp(Ptr), MINBPC(Enc));

                goto _break2;

              end;

              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2 * MINBPC(Enc));

              Result := XML_TOK_INVALID;

              Exit;

            end;

          end;

          { fall through }
          goto _go0;

        end;

      BT_AMP, BT_LT, BT_NONXML, BT_MALFORM, BT_TRAIL, BT_CR, BT_LF:
      _go0:
        begin
          NextTokPtr^ := Ptr;

          Result := XML_TOK_DATA_CHARS;

          Exit;

        end;

    else
      Inc(PtrComp(Ptr), MINBPC(Enc));

    end;

_break2:
  NextTokPtr^ := Ptr;

  Result := XML_TOK_DATA_CHARS;

end;

{ normal_cdataSectionTok {.. }
function Normal_cdataSectionTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_ignoreSectionTok {.. }
function Normal_ignoreSectionTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_attributeValueTok }
function Normal_attributeValueTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
var
  Start: PAnsiChar;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_NONE;

    Exit;

  end;

  Start := Ptr;

  while Ptr <> End_ do
    case BYTE_TYPE(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_AMP:
        begin
          if Ptr = Start then
          begin
            Result := Normal_scanRef(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)),
              End_, NextTokPtr);

            Exit;

          end;

          NextTokPtr^ := Ptr;

          Result := XML_TOK_DATA_CHARS;

          Exit;

        end;

      BT_LT:
        begin
          { this is for inside entity references }
          NextTokPtr^ := Ptr;

          Result := XML_TOK_INVALID;

          Exit;

        end;

      BT_LF:
        begin
          if Ptr = Start then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

            Result := XML_TOK_DATA_NEWLINE;

            Exit;

          end;

          NextTokPtr^ := Ptr;

          Result := XML_TOK_DATA_CHARS;

          Exit;

        end;

      BT_CR:
        begin
          if Ptr = Start then
          begin
            Inc(PtrComp(Ptr), MINBPC(Enc));

            if Ptr = End_ then
            begin
              Result := XML_TOK_TRAILING_CR;

              Exit;

            end;

            if BYTE_TYPE(Enc, Ptr) = BT_LF then
              Inc(PtrComp(Ptr), MINBPC(Enc));

            NextTokPtr^ := Ptr;

            Result := XML_TOK_DATA_NEWLINE;

            Exit;

          end;

          NextTokPtr^ := Ptr;

          Result := XML_TOK_DATA_CHARS;

          Exit;

        end;

      BT_S:
        begin
          if Ptr = Start then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

            Result := XML_TOK_ATTRIBUTE_VALUE_S;

            Exit;

          end;

          NextTokPtr^ := Ptr;

          Result := XML_TOK_DATA_CHARS;

          Exit;

        end;

    else
      Inc(PtrComp(Ptr), MINBPC(Enc));

    end;

  NextTokPtr^ := Ptr;

  Result := XML_TOK_DATA_CHARS;

end;

{ normal_entityValueTok {.. }
function Normal_entityValueTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

{ normal_sameName {.. }
function Normal_sameName(Enc: ENCODING_ptr; Ptr1, Ptr2: PAnsiChar): Integer;
begin
end;

{ normal_nameMatchesAscii }
function Normal_nameMatchesAscii(Enc: ENCODING_ptr;
  Ptr1, End1, Ptr2: PAnsiChar): Integer;
begin
  while Ptr2^ <> #0 do
  begin
    if Ptr1 = End1 then
    begin
      Result := 0;

      Exit;

    end;

    if CHAR_MATCHES(Enc, Ptr1, Integer(Ptr2^)) = 0 then
    begin
      Result := 0;

      Exit;

    end;

    Inc(PtrComp(Ptr1), MINBPC(Enc));
    Inc(PtrComp(Ptr2));

  end;

  Result := Integer(Ptr1 = End1);

end;

{ normal_nameLength }
function Normal_nameLength(Enc: ENCODING_ptr; Ptr: PAnsiChar): Integer;
var
  Start: PAnsiChar;

begin
  Start := Ptr;

  repeat
    case BYTE_TYPE(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_NONASCII, BT_NMSTRT, {$IFDEF XML_NS }BT_COLON, {$ENDIF }
      BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
        Inc(PtrComp(Ptr), MINBPC(Enc));

    else
      begin
        Result := PtrComp(Ptr) - PtrComp(Start);

        Exit;

      end;

    end;

  until False;

end;

{ normal_skipS {.. }
function Normal_skipS(Enc: ENCODING_ptr; Ptr: PAnsiChar): PAnsiChar;
begin
end;

{ normal_getAtts }
{ This must only be called for a well-formed start-tag or empty
  element tag.  Returns the number of attributes.  Pointers to the
  first attsMax attributes are stored in atts. }
function Normal_getAtts(Enc: ENCODING_ptr; Ptr: PAnsiChar; AttsMax: Integer;
  Atts: ATTRIBUTE_ptr): Integer;
type
  State_enum = (Other, InName, InValue);

var
  State: State_enum;

  NAtts, Open: Integer;

begin
  State := InName;
  NAtts := 0;
  Open := 0; { defined when state = inValue;
    initialization just to shut up compilers }

  Inc(PtrComp(Ptr), MINBPC(Enc));

  repeat
    case BYTE_TYPE(Enc, Ptr) of
      BT_LEAD2:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Name := Ptr;
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Normalized := #1;

            end;

            State := InName;

          end;

          Inc(PtrComp(Ptr), 2 - MINBPC(Enc));

        end;

      BT_LEAD3:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Name := Ptr;
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Normalized := #1;

            end;

            State := InName;

          end;

          Inc(PtrComp(Ptr), 3 - MINBPC(Enc));

        end;

      BT_LEAD4:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Name := Ptr;
              ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
                ^.Normalized := #1;

            end;

            State := InName;

          end;

          Inc(PtrComp(Ptr), 4 - MINBPC(Enc));

        end;

      BT_NONASCII, BT_NMSTRT, BT_HEX:
        if State = Other then
        begin
          if NAtts < AttsMax then
          begin
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
              ^.Name := Ptr;
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
              ^.Normalized := #1;

          end;

          State := InName;

        end;

      BT_QUOT:
        if State <> InValue then
        begin
          if NAtts < AttsMax then
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))^.ValuePtr
              := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          State := InValue;
          Open := BT_QUOT;

        end
        else if Open = BT_QUOT then
        begin
          State := Other;

          if NAtts < AttsMax then
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
              ^.ValueEnd := Ptr;

          Inc(NAtts);

        end;

      BT_APOS:
        if State <> InValue then
        begin
          if NAtts < AttsMax then
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))^.ValuePtr
              := PAnsiChar(PtrComp(Ptr) + MINBPC(Enc));

          State := InValue;
          Open := BT_APOS;

        end
        else if Open = BT_APOS then
        begin
          State := Other;

          if NAtts < AttsMax then
            ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
              ^.ValueEnd := Ptr;

          Inc(NAtts);

        end;

      BT_AMP:
        if NAtts < AttsMax then
          ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
            ^.Normalized := #0;

      BT_S:
        if State = InName then
          State := Other
        else if (State = InValue) and (NAtts < AttsMax) and
          (ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))^.Normalized
          <> #0) and
          ((Ptr = ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
          ^.ValuePtr) or (BYTETo_ASCII(Enc, Ptr) <> Integer(ASCII_SPACE)) or
          (BYTETo_ASCII(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc)))
          = Integer(ASCII_SPACE)) or
          (BYTE_TYPE(Enc, PAnsiChar(PtrComp(Ptr) + MINBPC(Enc))) = Open)) then
          ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
            ^.Normalized := #0;

      BT_CR, BT_LF:
        { This case ensures that the first attribute name is counted
          Apart from that we could just change state on the quote. }
        if State = InName then
          State := Other
        else if (State = InValue) and (NAtts < AttsMax) then
          ATTRIBUTE_ptr(PtrComp(Atts) + NAtts * SizeOf(ATTRIBUTE))
            ^.Normalized := #0;

      BT_GT, BT_SOL:
        if State <> InValue then
        begin
          Result := NAtts;

          Exit;

        end;

    end;

    Inc(PtrComp(Ptr), MINBPC(Enc));

  until False;

  { not reached }

end;

{ normal_charRefNumber {.. }
function Normal_charRefNumber(Enc: ENCODING_ptr; Ptr: PAnsiChar): Integer;
begin
end;

{ normal_predefinedEntityName {.. }
function Normal_predefinedEntityName(Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar): Integer;
begin
end;

{ normal_updatePosition }
procedure Normal_updatePosition(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  Pos: POSITION_ptr);
begin
  while Ptr <> End_ do
  begin
    case BYTE_TYPE(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_LF:
        begin
          Pos.ColumnNumber := XML_Size(-1);

          Inc(Pos.LineNumber);
          Inc(PtrComp(Ptr), MINBPC(Enc));

        end;

      BT_CR:
        begin
          Inc(Pos.LineNumber);
          Inc(PtrComp(Ptr), MINBPC(Enc));

          if (Ptr <> End_) and (BYTE_TYPE(Enc, Ptr) = BT_LF) then
            Inc(PtrComp(Ptr), MINBPC(Enc));

          Pos.ColumnNumber := XML_Size(-1);

        end;

    else
      Inc(PtrComp(Ptr), MINBPC(Enc));

    end;

    Inc(Pos.ColumnNumber);

  end;

end;

{ normal_isPublicId }
function Normal_isPublicId(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  BadPtr: PPAnsiChar): Integer;
label
  _else;

begin
  Inc(PtrComp(Ptr), MINBPC(Enc));
  Dec(PtrComp(End_), MINBPC(Enc));

  while Ptr <> End_ do
  begin
    case BYTE_TYPE(Enc, Ptr) of
      BT_S:
        if CHAR_MATCHES(Enc, Ptr, Integer(ASCII_TAB)) <> 0 then
        begin
          BadPtr^ := Ptr;
          Result := 0;

          Exit;

        end;

      BT_NAME, BT_NMSTRT:
        if BYTETo_ASCII(Enc, Ptr) and not $7F = 0 then
        else
          goto _else;

      BT_DIGIT, BT_HEX, BT_MINUS, BT_APOS, BT_LPAR, BT_RPAR, BT_PLUS, BT_COMMA,
        BT_SOL, BT_EQUALS, BT_QUEST, BT_CR, BT_LF, BT_SEMI, BT_EXCL, BT_AST,
        BT_PERCNT, BT_NUM {$IFDEF XML_NS } , BT_COLON: {$ELSE } : {$ENDIF }
      else
      _else:
        case BYTETo_ASCII(Enc, Ptr) of
          $24, { $ }
          $40: { @ }
          else
          begin
            BadPtr^ := Ptr;
            Result := 0;

            Exit;

          end;

        end;

    end;

    Inc(PtrComp(Ptr), MINBPC(Enc));

  end;

  Result := 1;

end;
