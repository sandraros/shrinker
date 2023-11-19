*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_program_load IMPLEMENTATION.

  METHOD select_prog.

    DATA ls_load_status TYPE ty_s_load_status.
    DATA: BEGIN OF ls_trdir_partial,
            name TYPE trdir-name,
          END OF ls_trdir_partial.
    DATA l_utimstp TYPE tzntimestp.
    DATA l_stimstp TYPE tzntimestp.

    DATA(lt_rng_prog) = it_rng_prog.
    WHILE lt_rng_prog IS NOT INITIAL.
      DATA(lt_small_rng_prog) = VALUE ty_t_rng_prog( ( LINES OF lt_rng_prog FROM 1 TO 100 ) ).
      DELETE lt_rng_prog FROM 1 TO 100.
      SELECT trdir~name, repoload~sdat, repoload~stime, repoload~udat, repoload~utime
        FROM trdir
          LEFT OUTER JOIN repoload
            ON trdir~name = repoload~progname
        WHERE trdir~name IN @lt_small_rng_prog
          AND trdir~subc NE 'I'
          AND ( repoload~udat IS NULL OR repoload~udat IN @it_rng_udat )
          AND ( repoload~sdat IS NULL OR repoload~sdat IN @it_rng_sdat )
        APPENDING TABLE @kit_load_status.
    ENDWHILE.

    IF i_nevergen = abap_false.
      DELETE kit_load_status WHERE sdat IS INITIAL.
    ENDIF.

    LOOP AT kit_load_status INTO ls_load_status.
      IF ls_load_status-sdat IS INITIAL.
        ls_load_status-stat = 'NO_LOAD'.
      ELSE.
        CONCATENATE ls_load_status-udat ls_load_status-utime INTO l_utimstp.
        CONCATENATE ls_load_status-sdat ls_load_status-stime INTO l_stimstp.
        IF l_utimstp LT l_stimstp.
          ls_load_status-stat = 'INVALID_LOAD'.
        ELSE.
          ls_load_status-stat = 'VALID_LOAD'.
        ENDIF.
      ENDIF.
      MODIFY kit_load_status FROM ls_load_status TRANSPORTING stat.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_progs.
    et_load_status = kit_load_status.
  ENDMETHOD.


  METHOD get_number_of_progs.
    DESCRIBE TABLE kit_load_status LINES e_number.
  ENDMETHOD.


  METHOD filter_prog.

    DATA ls_load_status TYPE ty_s_load_status.
    DATA g_do_generate TYPE flag.

    LOOP AT kit_load_status INTO ls_load_status.

      CLEAR g_do_generate.
      IF ls_load_status-stat = 'NO_LOAD' AND nevergen = 'X'.
        g_do_generate = 'X'.
      ELSEIF ls_load_status-stat = 'INVALID_LOAD' AND toberege = 'X'.
        g_do_generate = 'X'.
      ELSEIF ls_load_status-stat = 'VALID_LOAD' AND generatd = 'X'.
        g_do_generate = 'X'.
      ENDIF.

      IF g_do_generate IS INITIAL.
        DELETE kit_load_status.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD gen_progs.
    DATA ls_load_status TYPE ty_s_load_status.
    DATA l_commit_count TYPE i.

    LOOP AT kit_load_status INTO ls_load_status.
      GENERATE REPORT ls_load_status-progname MESSAGE ls_load_status-gen_message.
      ls_load_status-gen_result = sy-subrc.
      MODIFY kit_load_status FROM ls_load_status.
      RAISE EVENT program_generated EXPORTING progname = ls_load_status-progname
                                              subrc    = sy-subrc
                                              counter  = sy-tabix
                                              total    = lines( kit_load_status ).
      " the following is needed to avoid PXA_NO_FREE_SPACE dump
      " (see note 302500 Transaction SGEN terminates with PXA_NO_FREE_SPACE)
      " when there are many GENERATE REPORT executed
      ADD 1 TO l_commit_count.
      IF i_commit_frequency > 0 AND l_commit_count = i_commit_frequency.
        COMMIT WORK.
        l_commit_count = 0.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD invalidate.
    DATA ls_load_status TYPE ty_s_load_status.

    ls_load_status-sdat = sy-datum.
    ls_load_status-stime = sy-uzeit.
    ls_load_status-stat = 'INVALID_LOAD'.
    MODIFY kit_load_status FROM ls_load_status TRANSPORTING sdat stime stat
          WHERE stat <> ls_load_status-stat.
    ku_date = ls_load_status-sdat.
    ku_time = ls_load_status-stime.

    IF i_test IS INITIAL.
      LOOP AT kit_load_status INTO ls_load_status.
        TRY.
            ku_program = ls_load_status-progname.
            EXEC SQL.
              UPDATE repoload
                    SET sdat = :ku_date
                        stime = :ku_time
                    WHERE prog = :ku_program
            ENDEXEC.
            DATA exc_ref TYPE REF TO cx_sy_native_sql_error.
          CATCH cx_sy_native_sql_error INTO exc_ref.
            DATA error_text TYPE string.
            error_text = exc_ref->get_text( ).
        ENDTRY.
      ENDLOOP.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_abap_statement_at_cursor IMPLEMENTATION.

  METHOD get.

    raw_scan_lines_around( EXPORTING it_source = it_source
                                     i_linenr  = i_linenr
                                     i_offset  = i_offset
                           IMPORTING result    = DATA(raw_scan_lines_around) ).

    result = rework_raw_scan_lines(
                raw_scan_lines_around = raw_scan_lines_around
                i_linenr              = i_linenr
                i_offset              = i_offset ).

  ENDMETHOD.


  METHOD parse_line.
    TYPES ty_char_1 TYPE c LENGTH 1.
    DATA: l_empty_chars    TYPE string,
          l_offset         TYPE i,
          l_offset2        TYPE i,
          l_length         TYPE i,
          l_length2        TYPE i,
          l_state          TYPE i,
          l_delimiter      TYPE c LENGTH 1,
          ls_current_token TYPE zif_shrinker_abap_scan=>ty_stokes,
          l_is_in_token    TYPE abap_bool,
          l_end_of_line    TYPE abap_bool,
          l_end_of_token   TYPE abap_bool,
          l_concat         TYPE abap_bool,
          l_character_2    TYPE ty_char_1.

    result-linenr = i_linenr.

    l_concat = abap_false.

    CONCATENATE ` ` cl_abap_char_utilities=>horizontal_tab INTO l_empty_chars.

    IF i_line IS INITIAL OR i_line CO l_empty_chars.
      " line is empty
      RETURN.
    ENDIF.

    IF i_line(1) = '*'.
      " line is a comment
      result = VALUE #(
            pseudo_tokens = VALUE #(
                ( str  = i_line
                  row  = i_linenr
                  col  = 0
                  type = scan_token_type-comment
                  ) ) ).
      RETURN.
    ENDIF.

    l_offset = 0.
    l_state = 0.
    l_end_of_line = abap_false.
    l_end_of_token = abap_false.

    DO.

      DATA(l_character) = VALUE ty_char_1( ).
      IF l_offset < strlen( i_line ).
        l_character = i_line+l_offset(1).
        l_length = 1.
      ELSE.
        l_character = space.
        l_end_of_token = abap_true.
        l_end_of_line = abap_true.
        l_length = 0.
      ENDIF.

      l_is_in_token = abap_false.
      DATA(l_colon_detected) = abap_false.

      CASE l_state.
        WHEN 0.
          CASE l_character.
            WHEN ':'.
              l_colon_detected = abap_true.
              l_end_of_token = abap_true.
            WHEN '.'.
              l_end_of_token = abap_true.
            WHEN ','.
              l_end_of_token = abap_true.
            WHEN '"'.
              " remaining characters of the line are a comment
              IF ls_current_token IS NOT INITIAL.
                l_length = 0.
                l_end_of_token = abap_true.
              ELSE.
                l_end_of_token = abap_true.
                l_end_of_line = abap_true.
                " the comment is stored without the leading double quote (what if it's a double quote without comment behind?)
                DATA(end_comment) = substring( val = i_line off = l_offset + 1 ).
                " NB: completely ignore the double quote without a comment
                IF end_comment IS NOT INITIAL.
                  ls_current_token-str = substring( val = i_line off = l_offset + 1 ).
                  ls_current_token-type = type-token-comment.
                ENDIF.
              ENDIF.
            WHEN '#'.
              ls_current_token-type = type-token-pragma.
            WHEN '''' OR '`'.
              " start of text or string literal
              IF ls_current_token IS NOT INITIAL.
                " special case no space before quote (var ='').
                " ('DYNAMIC') -> continue current token
                IF '(' = substring( val = ls_current_token-str off = strlen( ls_current_token-str ) - 1 ).
                  l_is_in_token = abap_true.
                ELSE.
                  l_length = 0.
                  l_end_of_token = abap_true.
                ENDIF.
              ELSE.
                l_state = 1.
                l_is_in_token = abap_true.
                l_delimiter = l_character.
                ls_current_token-col = l_offset.
                ls_current_token-type = type-token-literal. "type-token-literal_with_ampersand ?
              ENDIF.
            WHEN '|'.
              " start of string template
              l_state = 2.
              l_end_of_token = abap_true.
            WHEN '}'.
              IF ls_current_token IS INITIAL.
                l_state = 2.
                l_end_of_token = abap_true.
              ELSE.
                " TODO error
              ENDIF.
            WHEN space OR cl_abap_char_utilities=>horizontal_tab.
              " token separator
              l_end_of_token = abap_true.
            WHEN '&'.
              " could be & or && or &[1-9] in macros
              l_offset2 = l_offset + 1.
              l_character_2 = COND #( WHEN l_offset2 < strlen( i_line ) THEN
                                        i_line+l_offset2(1)
                                      ELSE
                                        space ).
              IF l_character_2 = '&'.
                " &&
                l_length = 2.
                l_is_in_token = abap_true.
                l_end_of_token = abap_true.
              ELSEIF l_character_2 CO '123456789'.
                " &[1-9] in macros
                l_is_in_token = abap_true.
              ELSE.
                " &
                " previous token must be continued
                l_concat = abap_true.
                l_state = 6.
                l_end_of_token = abap_true.
              ENDIF.
            WHEN '('.
              " 'Textsymbol'(001)
              " CALL METHOD (DYNAMIC)
              " SELECT ... WHERE column IN ('A',1)
              " method( )
              " variable(length)
              " A = ( 1 + 2 ).
              " ASSIGN (ZIF=>C)=>(MEMBER)
              " ASSIGN (ZIF=>C)=>('MEMBER')
              l_offset2 = l_offset + 1.
              l_character_2 = COND #( WHEN l_offset2 < strlen( i_line ) THEN
                                        i_line+l_offset2(1)
                                      ELSE
                                        space ).
              IF ls_current_token IS INITIAL.
                IF l_character_2 = space.
                  l_is_in_token = abap_true.
                  l_end_of_token = abap_true.
                ELSE.
                  " start of list
                  l_state = 7.
                  l_is_in_token = abap_true.
                  ls_current_token-col = l_offset.
                  ls_current_token-type = type-pseudo_token-std-list.
                ENDIF.
              ELSE.
                IF l_character_2 = space.
                  " method( )
                  l_is_in_token = abap_true.
                  l_end_of_token = abap_true.
                ELSE.
                  " variable(length)
                  " ASSIGN (ZIF=>C)=>(MEMBER)
                  " ASSIGN (ZIF=>C)=>('MEMBER')
                  l_state = 7.
                  l_is_in_token = abap_true.
                ENDIF.
              ENDIF.
            WHEN OTHERS.
              " "payload" character
              l_is_in_token = abap_true.
              IF ls_current_token IS INITIAL.
                ls_current_token-type = type-token-identifier.
                ls_current_token-col = l_offset.
              ENDIF.
          ENDCASE.

        WHEN 1.
          "=============================================
          " we are inside a text or string literal
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF l_offset2 < strlen( i_line )
                AND i_line+l_offset2(1) = l_delimiter.
              " doubled delimiter, so process the 2 subsequent delimiters as one normal
              " character of value "delimiter" (either ' or `)
              l_length = 2.
            ELSE.
              " end of text or string literal
              l_offset2 = l_offset + 1.
              IF l_offset2 < strlen( i_line )
                    AND i_line+l_offset2(1) = '('.
                " aa = 'text symbol'(001).
                l_end_of_token = abap_false.
              ELSE.
                l_end_of_token = abap_true.
              ENDIF.
              l_state = 0.
              " note: important to keep L_DELIMITER value in case followed with literal operator (&)
            ENDIF.
          ENDIF.

        WHEN 2.
          "=============================================
          " we are inside a string template |...|
          " (or after } which means the next possible character is same after the opening pipe)
          "=============================================
          CASE l_character.
            WHEN '|'.
              " end of string template
              l_state = 0.
              l_end_of_token = abap_true.
            WHEN '{'.
              " start of a string template expression
              l_state = 3.
              l_end_of_token = abap_true.
            WHEN '\'.
              " it's an escape character; ignore both this one and the next one
              l_length = 2.
          ENDCASE.

        WHEN 3.
          "=============================================
          " we are inside a string template expression |...{ ... }...|
          "=============================================
          CASE l_character.
            WHEN ''''
                  OR '`'.
              l_state = 4.
              l_delimiter = l_character.
              l_is_in_token = abap_true.
            WHEN '}'.
              " end of string template expression
              l_state = 2.
              l_end_of_token = abap_true.
            WHEN space OR cl_abap_char_utilities=>horizontal_tab.
              " token separator
              l_end_of_token = abap_true.
            WHEN OTHERS.
              l_is_in_token = abap_true.
              IF ls_current_token IS INITIAL.
                ls_current_token-type = type-token-identifier.
                ls_current_token-col = l_offset.
              ENDIF.
          ENDCASE.

        WHEN 4.
          "=============================================
          " we are inside a text or string literal within a string expression expression |...{ ...'...'... }...|
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF l_character_2 = l_delimiter.
              " doubled delimiter = consider a single occurrence as being part of the literal
              l_length = 2.
            ELSE.
              " end of literal
              l_state = 3.
            ENDIF.
          ENDIF.

        WHEN 6.
          "=============================================
          " after literal operator (&), we are expecting the same separator
          " (either '...' & '...' or `...` & `...`)
          "=============================================
          IF l_character CO l_empty_chars.
            " do nothing, process next character
          ELSEIF l_character = l_delimiter.
            " continue literal
            l_state = 1.
          ELSE.
            " error ( this is an invalid syntax: 'literal' &[\s]*[^'] )
            " TODO
          ENDIF.

        WHEN 7.
          "=============================================
          " start of list (1,2,3) or dynamic name
          "=============================================
          " (AB)
          " ('AB')
          " (AB)=>(CD)
          " (AB)=>('CD')
          " AB=>('CD')
          l_is_in_token = abap_true.
          IF ls_current_token IS INITIAL.
            ls_current_token-type = type-token-list.
          ENDIF.
          CASE l_character.
            WHEN '''' OR '`'.
              l_state = 8.
              l_delimiter = l_character.
            WHEN ')'.
              " TODO ERROR
            WHEN ' '.
              " TODO ERROR
            WHEN OTHERS.
              l_state = 9.
          ENDCASE.

        WHEN 8.
          "=============================================
          " we are inside a text or string literal within a list ('A','B')
          "=============================================
          l_is_in_token = abap_true.
          IF l_character = l_delimiter.
            l_offset2 = l_offset + 1.
            IF l_offset2 < strlen( i_line ) AND i_line+l_offset2(1) = l_delimiter.
              " doubled delimiter = consider a single occurrence as being part of the literal
              l_length = 2.
            ELSE.
              " end of literal
              l_state = 10.
            ENDIF.
          ENDIF.

        WHEN 9.
          "=============================================
          " we are inside a non-text/string literal within a list (123,constant,variable)
          "=============================================
          CASE l_character.
            WHEN ' ' OR ','.
              l_is_in_token = abap_true.
            WHEN ')'.
              l_is_in_token = abap_true.
              l_state = 12.
            WHEN OTHERS.
              l_is_in_token = abap_true.
          ENDCASE.

        WHEN 10.
          "=============================================
          " After a literal within a list (1,'A'...
          "=============================================
          CASE l_character.
            WHEN ' '.
            WHEN ')'.
              l_is_in_token = abap_true.
              l_state = 12.
            WHEN ','.
              l_is_in_token = abap_true.
              l_state = 11.
            WHEN OTHERS.
              " TODO ERROR
          ENDCASE.

        WHEN 11.
          "=============================================
          " After comma within a list ('A','B')
          "=============================================
          l_is_in_token = abap_true.
          ls_current_token-type = type-token-list.
          CASE l_character.
            WHEN '''' OR '`'.
              l_state = 8.
              l_delimiter = l_character.
            WHEN OTHERS.
              l_state = 9.
          ENDCASE.

        WHEN 12.
          "=============================================
          " Right after RIGHT PARENTHESIS within a list ('A','B')
          "=============================================
          " call method (xxxxx)=>xxxxxxxx : 3 tokens, (..)=>.. is just one token of type List.
          IF ls_current_token-str CA ','.
            l_end_of_token = abap_true.
            l_state = 0.
          ELSEIF ls_current_token-str NA space
                AND l_character CA '-='.
            l_offset2 = l_offset + 1.
            IF l_offset2 < strlen( i_line )
                AND i_line+l_offset2(1) = '>'.
              l_is_in_token = abap_true.
              l_state = 0.
            ELSE.
              l_end_of_token = abap_true.
              l_state = 0.
            ENDIF.
          ELSE.
            l_end_of_token = abap_true.
            l_state = 0.
          ENDIF.

      ENDCASE.

      IF l_is_in_token = abap_true.
        IF ls_current_token IS INITIAL.
          " start of token
          ls_current_token-col = l_offset.
        ENDIF.
        IF l_offset + l_length > strlen( i_line ).
          " i_line+l_offset(l_length) would trigger CX_SY_RANGE_OUT_OF_BOUNDS
          ASSERT 1 = 1. " debug helper
        ENDIF.
        ls_current_token-str = ls_current_token-str && i_line+l_offset(l_length).
      ENDIF.

      IF l_end_of_token = abap_true.
        IF ls_current_token IS NOT INITIAL.
          IF l_concat = abap_true.
            " 'hello' & ' world' must be interpreted like 'hello world'
            DESCRIBE TABLE result-pseudo_tokens.
            READ TABLE result-pseudo_tokens INDEX sy-tfill INTO ls_current_token.
            DELETE result-pseudo_tokens INDEX sy-tfill.
            l_length2 = strlen( ls_current_token-str ) - 1.
            ls_current_token-str = ls_current_token-str(l_length2).
          ENDIF.
          IF ls_current_token-type = type-pseudo_token-std-list.
            " SELECT ... WHERE col IN (AB,1,'A',`B`,'Paul''s')
            FIND ALL OCCURRENCES OF
                REGEX `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.` ##regex_posix
                IN ls_current_token-str
                RESULTS DATA(matches).
            LOOP AT matches REFERENCE INTO DATA(match).
              IF ls_current_token-str+match->offset(1) NA |(),'`|.
                REPLACE SECTION OFFSET match->offset LENGTH match->length OF ls_current_token-str
                    WITH to_upper( ls_current_token-str+match->offset(match->length) ).
              ENDIF.
            ENDLOOP.
          ENDIF.
          IF ls_current_token-type = type-pseudo_token-std-list
                AND ls_current_token-str CS ','.
            " SELECT ... WHERE col IN (AB,1,'A',`B`,'Paul''s')
*            FIND ALL OCCURRENCES OF
*                REGEX `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.` ##regex_posix
*                IN ls_current_token-str
*                RESULTS matches.
            result-pseudo_tokens = VALUE #(
                FOR <match> IN matches
                ( str  = ls_current_token-str+<match>-offset(<match>-length)
                  col  = ls_current_token-col + <match>-offset
                  type = type-pseudo_token-std-identifier ) ).
          ELSE.
            IF ls_current_token-str IS INITIAL.
              ASSERT 1 = 1. " debug helper
            ENDIF.
            IF ls_current_token-type = type-pseudo_token-std-identifier
                AND ls_current_token-str(1) NA '''`'.
              TRANSLATE ls_current_token-str TO UPPER CASE.
            ENDIF.
            ls_current_token-row = i_linenr.
            APPEND ls_current_token TO result-pseudo_tokens.
          ENDIF.
        ENDIF.

        DATA(pseudo_token_type) = SWITCH token_type( l_character
                WHEN '.' THEN type-pseudo_token-dot
                WHEN ',' THEN type-pseudo_token-comma
                WHEN ':' THEN type-pseudo_token-colon
                WHEN '}' THEN type-pseudo_token-std-identifier
                WHEN '{' THEN type-pseudo_token-std-identifier
                WHEN '|' THEN type-pseudo_token-std-identifier ).
        IF pseudo_token_type IS NOT INITIAL.
          APPEND VALUE #( str  = l_character
                          row  = i_linenr
                          col  = l_offset
                          type = pseudo_token_type
              ) TO result-pseudo_tokens.
        ENDIF.

        CLEAR ls_current_token.
        l_end_of_token = abap_false.
        l_concat = abap_false.
      ENDIF.

*      IF l_end_of_statement = abap_true.
*        l_end_of_statement = abap_false.
*      ENDIF.

      IF l_end_of_line = abap_true.
        EXIT.
      ENDIF.

      ADD l_length TO l_offset.
    ENDDO.

  ENDMETHOD.


  METHOD raw_scan_lines_around.
    DATA: pseudo_token TYPE REF TO lcl_abap_statement_at_cursor=>ty_pseudo_token.

    result = VALUE #( ).

    DATA(parsed_line) = VALUE ty_line_scan( ).

    "=======================================================
    " Go backwards from cursor till finding the previous statement (one ending with period).
    "=======================================================
    DATA(l_linenr) = EXACT i( i_linenr ) + 1.
    WHILE l_linenr > 1.
      l_linenr = l_linenr - 1.

      parsed_line = parse_line( i_line   = it_source[ l_linenr ]
                                i_linenr = l_linenr ).

      " Backup parsing of i_linenr to avoid parsing again during forward parsing.
      IF l_linenr = i_linenr.
        DATA(parsed_line_of_requested_line) = parsed_line.
      ELSE.
        INSERT LINES OF parsed_line-pseudo_tokens INTO result-pseudo_tokens INDEX 1.
      ENDIF.

      LOOP AT parsed_line-pseudo_tokens TRANSPORTING NO FIELDS
          WHERE str = '.'
              AND ( row < i_linenr
                 OR ( row = i_linenr
                  AND col < i_offset ) ).
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        EXIT.
      ENDIF.

    ENDWHILE.

    DATA(tabix_dot) = line_index( result-pseudo_tokens[ str = '.' ] )."REFERENCE INTO pseudo_token
    IF tabix_dot >= 1.
      DELETE result-pseudo_tokens TO tabix_dot.
      tabix_dot = 0.
    ENDIF.
    IF result-pseudo_tokens IS NOT INITIAL.
      LOOP AT result-pseudo_tokens
            TRANSPORTING NO FIELDS
            FROM tabix_dot + 1
            WHERE type <> type-pseudo_token-std-comment.
        EXIT.
      ENDLOOP.
      IF sy-subrc <> 0.
        " Only comments after the dot, delete them.
        DELETE result-pseudo_tokens FROM tabix_dot + 1.
      ENDIF.
    ENDIF.

    "=======================================================
    " Go forward from the cursor till finding end of statement.
    "=======================================================
    DATA(terminators) = `.,`.
    DATA(terminators_determined) = abap_false.

    DATA(terminator_found) = abap_false.
    l_linenr = i_linenr - 1.

    WHILE terminator_found = abap_false
        AND l_linenr < lines( it_source ).
      l_linenr = l_linenr + 1.

      IF l_linenr = i_linenr.
        parsed_line = parsed_line_of_requested_line.
      ELSE.
        parsed_line = parse_line( i_line   = it_source[ l_linenr ]
                                  i_linenr = l_linenr ).
      ENDIF.

      LOOP AT parsed_line-pseudo_tokens REFERENCE INTO pseudo_token.

        APPEND pseudo_token->* TO result-pseudo_tokens.

        IF terminators_determined = abap_false.
          IF strlen( pseudo_token->str ) = 1
            AND pseudo_token->str CA ',.:'
            AND ( pseudo_token->row > i_linenr
                 OR ( pseudo_token->row = i_linenr
                  AND pseudo_token->col >= i_offset ) ).
            IF pseudo_token->str = ':'.
              terminators = `.`.
            ELSE.
              terminators = '.,'.
            ENDIF.
            terminators_determined = abap_true.
          ENDIF.
        ENDIF.

        IF terminators_determined = abap_true
            AND strlen( pseudo_token->str ) = 1
            AND pseudo_token->str CA terminators.
          terminator_found = abap_true.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF terminator_found = abap_true.
        EXIT.
      ENDIF.

    ENDWHILE.

  ENDMETHOD.


  METHOD rework_raw_scan_lines.
    DATA pseudo_token TYPE REF TO ty_pseudo_token.

    DATA(pseudo_tokens) = raw_scan_lines_around-pseudo_tokens.

    IF remove_comments = abap_true.
      DELETE pseudo_tokens WHERE type = type-pseudo_token-std-comment.
    ENDIF.

    READ TABLE pseudo_tokens REFERENCE INTO pseudo_token
        WITH KEY row = i_linenr
                 col = i_offset
        BINARY SEARCH.
    DATA(tabix_word_at_cursor) = sy-tabix.

    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
        FROM tabix_word_at_cursor
        WHERE str = '.'.
      DELETE pseudo_tokens FROM sy-tabix + 1.
      EXIT.
    ENDLOOP.

    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
        FROM 1
        TO tabix_word_at_cursor - 1
        WHERE str = '.'.
      tabix_word_at_cursor = tabix_word_at_cursor - sy-tabix + 1.
      DELETE pseudo_tokens FROM 1 TO sy-tabix.
      EXIT.
    ENDLOOP.

    DATA(line_sstmnt) = VALUE zif_shrinker_abap_scan=>ty_sstmnt( ).
    DATA(sstmnt) = VALUE zif_shrinker_abap_scan=>ty_ut_sstmnt( ).
    DATA(line_stokes) = VALUE zif_shrinker_abap_scan=>ty_stokes( ).
    DATA(stokes) = VALUE zif_shrinker_abap_scan=>ty_ut_stokes( ).

    DATA(tabix_colon) = line_index( pseudo_tokens[ str = ':' ] ).

    IF tabix_colon > 0 AND tabix_word_at_cursor > tabix_colon.

      DATA(tabix_comma) = 0.
      LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
            FROM tabix_colon + 1
            TO tabix_word_at_cursor
            WHERE str = ','.
        tabix_comma = sy-tabix.
      ENDLOOP.
      IF tabix_comma > 0.
        tabix_word_at_cursor = tabix_word_at_cursor - tabix_comma + tabix_colon + 1.
        DELETE pseudo_tokens FROM tabix_colon + 1 TO tabix_comma.
      ENDIF.
      LOOP AT pseudo_tokens REFERENCE INTO pseudo_token
            FROM tabix_word_at_cursor
            WHERE str = ','.
        " Delete all tokens from this comma to the last token except the last one (dot)
        DELETE pseudo_tokens FROM sy-tabix TO lines( pseudo_tokens ) - 1.
        EXIT.
      ENDLOOP.

    ENDIF.

    DATA(colonrow) = 0.
    DATA(coloncol) = 0.
    DATA(from_token) = 1.
    DATA(prefixlen) = 0.
    LOOP AT pseudo_tokens REFERENCE INTO pseudo_token.
      CASE pseudo_token->str.
        WHEN ':'.
          colonrow = pseudo_token->row.
          coloncol = pseudo_token->col.
          prefixlen = lines( stokes ).
          DATA(stokes_before_colon) = stokes.
        WHEN ',' OR '.'.
          line_sstmnt = VALUE zif_shrinker_abap_scan=>ty_sstmnt(
              level      = 0
              struc      = 0
              from       = from_token
              to         = lines( stokes )
              number     = lines( sstmnt ) + 1
              colonrow   = colonrow
              trow       = pseudo_token->row
              coloncol   = coloncol
              tcol       = pseudo_token->col
              prefixlen  = prefixlen
              type       = COND #(
                                   WHEN 1 = 2 THEN
                                      type-stmnt-abap_doc
*                                   WHEN result-stokes[ ls_sstmnt-from ]-type = type-token-comment THEN
*                                      type-stmnt-comment
                                   WHEN 1 = 2 THEN
                                      type-stmnt-comment_in_stmnt
                                   WHEN 1 = 2 THEN
                                      type-stmnt-compute_direct
                                   WHEN 1 = 2 THEN
                                      type-stmnt-empty
                                   WHEN 1 = 2 THEN
                                      type-stmnt-include
                                   WHEN 1 = 2 THEN
                                      type-stmnt-include_miss
                                   WHEN 1 = 2 THEN
                                      type-stmnt-macro_call
                                   WHEN 1 = 2 THEN
                                      type-stmnt-macro_definition
                                   WHEN 1 = 2 THEN
                                      type-stmnt-method_direct
                                   WHEN 1 = 2 THEN
                                      type-stmnt-native_sql
                                   WHEN 1 = 2 THEN
                                      type-stmnt-opaque_body
                                   WHEN 1 = 2 THEN
                                      type-stmnt-pragma
                                   WHEN 1 = 2 THEN
                                      type-stmnt-trmac_call
                                   WHEN 1 = 2 THEN
                                      type-stmnt-type_pools
                                   WHEN 1 = 2 THEN
                                      type-stmnt-type_pools_miss
                                   WHEN 1 = 2 THEN
                                      type-stmnt-unknown
                                   ELSE
                                      type-stmnt-standard )
              terminator = pseudo_token->str
              enhmt      = 0 ).
          APPEND line_sstmnt TO sstmnt.
          from_token = lines( stokes ) + 1.
          DATA(stokes_buffer) = stokes_before_colon.
        WHEN OTHERS.
          line_stokes = VALUE zif_shrinker_abap_scan=>ty_stokes(
              str  = pseudo_token->str
              row  = pseudo_token->row
              col  = pseudo_token->col
              type = pseudo_token->type ).
          APPEND LINES OF stokes_buffer TO stokes.
          stokes_buffer = VALUE #( ).
          APPEND line_stokes TO stokes.
      ENDCASE.
    ENDLOOP.

    result-sstmnt = sstmnt.
    result-stokes = stokes.

  ENDMETHOD.

ENDCLASS.
