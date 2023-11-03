********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_LICENSE
*
********************************************************************************
*>>>>>>> ZCL_SHRINKER <<<<<<<*

*"* macro definitions
*include zcl_shrinker==================ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_shrinker==================ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2WBRY IMPLEMENTATION.

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


CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY IMPLEMENTATION.

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
          ls_current_token TYPE ty_stokes,
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
    DATA: pseudo_token TYPE REF TO SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_pseudo_token.

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

    DATA(line_sstmnt) = VALUE ty_sstmnt( ).
    DATA(sstmnt) = VALUE ty_ut_sstmnt( ).
    DATA(line_stokes) = VALUE ty_stokes( ).
    DATA(stokes) = VALUE ty_ut_stokes( ).

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
          line_sstmnt = VALUE ty_sstmnt(
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
          line_stokes = VALUE ty_stokes(
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

*"* test class
*include zcl_shrinker==================ccau.
*"* use this source file for your ABAP unit test classes

*CLASS SHRIS5ZPAUXVKEPN5HWER7BEX2YBRY DEFINITION DEFERRED.

*CLASS zcl_shrinker DEFINITION LOCAL FRIENDS
*    SHRIS5ZPAUXVKEPN5HWER7BEX2YBRY.

* c : cursor
* cbo : curly bracket opening (string template)
* cbc : curly bracket closing (string template)
* dot : dot/period
* ecmt : end comment (")
* lcmt : line comment (*)
* lf : line feed
* lon : colon
* ma : comma
* pip : pipe (string template opening/closing operator)
* sp : space
* sq : single quote
* w : word




























class LCL_SHRINKER implementation.
*"* method's implementations
*include methods.
  METHOD convert_methodindx_to_extensio.
    CONSTANTS base TYPE c LENGTH 36 VALUE '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

    result = ''.
    DATA(methodindx_2) = methodindx.
    DO 3 TIMES.
      DATA(indx)  = methodindx_2 MOD 36.
      result = base+indx(1) && result.
      methodindx_2 = methodindx_2 DIV 36.
    ENDDO.
    result = 'CM' && result.
  ENDMETHOD.
  METHOD get_abap_for_data_element.

    result = get_abap_for_ddic_elem_info( type_name = data_element-rollname
                                          ref_to    = xsdbool( data_element-reftype IS NOT INITIAL )
                                          elem_info = data_element-elem_info ).

  ENDMETHOD.
  METHOD get_abap_for_data_elements.

    result = VALUE #(
        FOR <data_element> IN data_elements
        ( LINES OF get_abap_for_data_element( <data_element> ) ) ).

  ENDMETHOD.
  METHOD get_abap_for_ddic_elem_info.

    TYPES: BEGIN OF ty_type_equivalence_abap_ddic,
             ddic TYPE datatype_d,
             abap TYPE c LENGTH 10,
             " abap_internal type abap_typekind,
           END OF ty_type_equivalence_abap_ddic.
    TYPES tt_type_equivalence_abap_ddic TYPE STANDARD TABLE OF ty_type_equivalence_abap_ddic WITH EMPTY KEY.

    DATA(type_equivalence_abap_ddic) = VALUE tt_type_equivalence_abap_ddic(
        ( ddic = 'ACCP' abap = 'N'          )
        ( ddic = 'CHAR' abap = 'C'          )
        ( ddic = 'CLNT' abap = 'C'          )
        ( ddic = 'CUKY' abap = 'C'          )
        ( ddic = 'CURR' abap = 'P'          )
        ( ddic = 'DATS' abap = 'D'          )
        ( ddic = 'DEC ' abap = 'P'          )
        ( ddic = 'D16R' abap = 'decfloat16' )
        ( ddic = 'D16S' abap = 'decfloat16' )
        ( ddic = 'D34R' abap = 'decfloat34' )
        ( ddic = 'D34S' abap = 'decfloat34' )
        ( ddic = 'FLTP' abap = 'F'          )
        ( ddic = 'INT1' abap = 'INT1'       )
        ( ddic = 'INT2' abap = 'INT2'       )
        ( ddic = 'INT4' abap = 'I'          )
        ( ddic = 'LANG' abap = 'C'          )
        ( ddic = 'LCHR' abap = 'C'          )
        ( ddic = 'LRAW' abap = 'X'          )
        ( ddic = 'NUMC' abap = 'N'          )
        ( ddic = 'PREC' abap = 'INT2'       )
        ( ddic = 'QUAN' abap = 'P'          )
        ( ddic = 'RAW ' abap = 'X'          )
        ( ddic = 'RSTR' abap = 'xstring'    )
        ( ddic = 'SSTR' abap = 'string'     )
        ( ddic = 'STRG' abap = 'string'     )
        ( ddic = 'TIMS' abap = 'T'          )
        ( ddic = 'UNIT' abap = 'C'          ) ).

    DATA(abap_type) = VALUE #( type_equivalence_abap_ddic[ ddic = elem_info-datatype ]-abap OPTIONAL ).
    IF abap_type IS INITIAL.
      ASSERT 1 = 1. " debug helper
    ENDIF.

    DATA(length) = SWITCH string( abap_type
            WHEN 'C' OR 'N' OR 'X' THEN |{ CONV i( elem_info-leng ) }| " LENG is type N -> remove leading zeroes and no trailing spaces
            WHEN 'P' THEN |{ 1 + round( val = ( elem_info-leng - 1 ) / 2 dec = 0 ) }|
            ELSE '' ).

    DATA(decimals) = COND string( WHEN abap_type = 'P' THEN |{ CONV i( elem_info-decimals ) }| ). " DECIMALS is type N -> remove leading zeroes and no trailing spaces

    result = VALUE #(
            ( |TYPES { type_name } TYPE { COND #( WHEN ref_to = abap_true THEN |REF TO | )
                                        && abap_type
                                        && COND #( WHEN length IS NOT INITIAL THEN | LENGTH { length }| )
                                        && COND #( WHEN decimals IS NOT INITIAL THEN | DECIMALS { decimals }| ) }.| ) ).

  ENDMETHOD.
  METHOD get_abap_for_structure.

    result = VALUE #(
        ( |TYPES BEGIN OF { structure-tabname }.| )
        ( LINES OF VALUE #(
            FOR <component> IN structure_components
            WHERE ( tabname = structure-tabname )
            ( LINES OF COND #( WHEN <component>-fieldname(1) = '.' THEN
                                    VALUE #( ( |INCLUDE TYPE { <component>-precfield }.| ) )
                                WHEN <component>-rollname IS NOT INITIAL THEN
                                    VALUE #( ( |TYPES { <component>-fieldname } TYPE { COND #( WHEN <component>-reftype IS NOT INITIAL THEN |REF TO | )
                                                                                        && <component>-rollname }.| ) )
                                ELSE
                                    get_abap_for_ddic_elem_info( type_name = <component>-fieldname
                                                                 ref_to    = xsdbool( <component>-reftype IS NOT INITIAL )
                                                                 elem_info = <component>-elem_info ) ) ) ) )
        ( |TYPES END OF { structure-tabname }.| ) ).

  ENDMETHOD.
  METHOD get_abap_for_structures.

    result = VALUE #(
        FOR <structure> IN structures
        ( LINES OF get_abap_for_structure( structure            = <structure>
                                           structure_components = structure_components ) ) ).

  ENDMETHOD.
  METHOD get_abap_for_table_type.

    result = VALUE #(
        LET auxiliary_type_name = COND #( WHEN table_type-rowkind IS INITIAL THEN | OF | )
            auxiliary_type_abap_line = COND #( WHEN auxiliary_type_name IS NOT INITIAL THEN
                                            get_abap_for_ddic_elem_info( type_name = auxiliary_type_name
                                                                         ref_to    = xsdbool( table_type-reftype IS NOT INITIAL )
                                                                         elem_info = table_type-elem_info ) )
            last_secondary_key_name = REDUCE #( INIT t = ``
                                                 FOR <sec_key> IN sec_keys
                                                 WHERE ( typename = table_type-typename )
                                                 NEXT t = <sec_key>-seckeyname )
        IN
        ( LINES OF COND #( WHEN auxiliary_type_abap_line IS NOT INITIAL THEN auxiliary_type_abap_line ) )
        ( |TYPES { table_type-typename
            } TYPE { SWITCH #( table_type-accessmode
                        WHEN 'T' THEN `STANDARD`
                        WHEN 'S' THEN `SORTED`
                        WHEN 'H' THEN `HASHED`
                        WHEN 'A' THEN `ANY`
                        WHEN 'I' THEN `INDEX` )
            } TABLE OF { COND #( WHEN auxiliary_type_name IS NOT INITIAL THEN
                            auxiliary_type_name
                        ELSE
                            table_type-rowtype )
            }{ COND #( WHEN table_type-keykind <> 'G' THEN | WITH { SWITCH #( table_type-keykind WHEN 'U' THEN `UNIQUE` ELSE `NON-UNIQUE` )
                } { SWITCH #( table_type-keydef WHEN 'D' THEN |DEFAULT KEY|
                                                WHEN 'T' THEN |KEY table_line|
                                                WHEN 'K' THEN |KEY { get_table_type_key_components( typename       = table_type-typename
                                                                                                    keyname        = ''
                                                                                                    key_components = key_components ) }|
                           )
                }| " end of WITH
                )
            }{ COND #( WHEN last_secondary_key_name IS INITIAL THEN '.' ) }| )
        ( LINES OF COND #( WHEN last_secondary_key_name IS NOT INITIAL THEN VALUE #(
            FOR <sec_key> IN sec_keys
            WHERE ( typename = table_type-typename )
            ( | WITH { SWITCH #( <sec_key>-kind WHEN 'U' THEN `UNIQUE` ELSE `NON-UNIQUE` )
                    }{ SWITCH #( <sec_key>-accessmode
                        WHEN 'T' THEN `STANDARD`
                        WHEN 'S' THEN `SORTED`
                        WHEN 'H' THEN `HASHED`
                        WHEN 'A' THEN `ANY`
                        WHEN 'I' THEN `INDEX` )
                    } KEY { <sec_key>-seckeyname
                    } COMPONENTS { get_table_type_key_components( typename       = table_type-typename
                                                                  keyname        = <sec_key>-seckeyname
                                                                  key_components = key_components )
                    }{ COND #( WHEN <sec_key>-seckeyname = last_secondary_key_name THEN '.' ) }| ) ) ) ) ).

  ENDMETHOD.
  METHOD get_abap_for_table_types.

    result = VALUE #(
        FOR <table_type> IN table_types
        ( LINES OF get_abap_for_table_type(
                     table_type     = <table_type>
                     key_components = key_components
                     sec_keys       = sec_keys ) ) ).

  ENDMETHOD.
  METHOD get_class_include_name.

    TYPES: BEGIN OF ty_class_and_extension,
             class_name TYPE c LENGTH 30,
             extension  TYPE ty_extension_code,
           END OF ty_class_and_extension.

    result = translate( val  = CONV string( VALUE ty_class_and_extension(
                                 class_name = class_name
                                 extension  = extension ) )
                        from = ` `
                        to   = '=' ).

  ENDMETHOD.
  METHOD get_table_type_key_components.

    result = concat_lines_of( sep   = ` `
                              table = REDUCE #( INIT a = VALUE string_table( )
                                                FOR <key_component> IN key_components
                                                WHERE ( typename   = typename
                                                    AND seckeyname = keyname )
                                                NEXT a = VALUE #( BASE a ( |{ <key_component>-keyfield }| ) ) ) ).

  ENDMETHOD.
  METHOD read_report.

    READ REPORT include_name INTO result.

  ENDMETHOD.
  METHOD read_report_class_include.

    result = VALUE #(
        LET include_name = get_class_include_name( class_name = class_name
                                                   extension  = extension )
        IN
        include_name     = include_name
        extension_code   = extension
        method_name      = method_name
        abap_source_code = read_report( include_name ) ).

    LOOP AT cc_renamings REFERENCE INTO DATA(renaming).
      REPLACE ALL OCCURRENCES OF
            REGEX |\\<{ renaming->posix_regex }\\>| ##REGEX_POSIX
            IN TABLE result-abap_source_code
            WITH renaming->with
            IGNORING CASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD select_ddic_objects.

*    SELECT FROM dd01l
*            INNER JOIN tadir
*              ON tadir~obj_name = dd01l~domname
*            FIELDS dd01l~domname, dd01l~datatype, dd01l~leng, dd01l~decimals
*            WHERE tadir~object = 'DOMA'
*              AND tadir~devclass IN @package_range
*            INTO TABLE @result-domains.

    SELECT FROM dd04l
            INNER JOIN tadir
              ON tadir~obj_name = dd04l~rollname
            FIELDS dd04l~rollname, dd04l~domname, dd04l~reftype, dd04l~datatype, dd04l~leng, dd04l~decimals
            WHERE tadir~object = 'DTEL'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd04l~as4local = 'A'
            INTO TABLE @result-data_elements.

    SELECT FROM dd02l
            INNER JOIN tadir
              ON tadir~obj_name = dd02l~tabname
            FIELDS dd02l~tabname, dd02l~tabclass
            WHERE tadir~object = 'TABL'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd02l~as4local = 'A'
            INTO TABLE @result-structures.

    SELECT FROM dd03l
            INNER JOIN tadir
              ON tadir~obj_name = dd03l~tabname
            FIELDS dd03l~tabname, dd03l~fieldname, dd03l~rollname, dd03l~precfield, dd03l~comptype, dd03l~reftype, dd03l~datatype, dd03l~leng, dd03l~decimals
            WHERE tadir~object = 'TABL'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd03l~as4local = 'A'
              AND dd03l~adminfield = '0' " skip components added via ".INCLUDE structure"
              AND dd03l~depth = 0        " skip components added via "component TYPE structure"
            INTO TABLE @result-structure_components.

    SELECT FROM dd40l
            INNER JOIN tadir
              ON tadir~obj_name = dd40l~typename
            FIELDS dd40l~typename, dd40l~rowkind, dd40l~reftype, dd40l~rowtype, dd40l~keydef, dd40l~keykind, dd40l~ttypkind, dd40l~accessmode, dd40l~datatype, dd40l~leng, dd40l~decimals
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd40l~as4local = 'A'
            INTO TABLE @result-table_types.

    SELECT FROM dd42s
            INNER JOIN tadir
              ON tadir~obj_name = dd42s~typename
            FIELDS dd42s~typename, dd42s~seckeyname, dd42s~keyfdpos, dd42s~keyfield
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd42s~as4local = 'A'
            INTO TABLE @result-table_type_key_components.

    SELECT FROM dd43l
            INNER JOIN tadir
              ON tadir~obj_name = dd43l~typename
            FIELDS dd43l~typename, dd43l~seckeyname, dd43l~seckeyunique, dd43l~accessmode, dd43l~kind
            WHERE tadir~object = 'TTYP'
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
              AND dd43l~as4local = 'A'
            INTO TABLE @result-table_type_sec_keys.

  ENDMETHOD.
  METHOD select_miscellaneous.

    IF objects IS INITIAL.
      SELECT pgmid, object, obj_name
          FROM tadir
          WHERE devclass IN @package_range
            AND object NOT IN ('DOMA')
            AND object IN @range_of_obj_type
            AND obj_name IN @range_of_obj_name
          INTO TABLE @result-all_objects.
    ELSE.
      SELECT pgmid, object, obj_name
          FROM tadir
          FOR ALL ENTRIES IN @objects
          WHERE pgmid = @objects-pgmid
            AND object = @objects-object
            AND obj_name = @objects-obj_name
            AND devclass IN @package_range
            AND object NOT IN ('DOMA')
            AND object IN @range_of_obj_type
            AND obj_name IN @range_of_obj_name
          INTO TABLE @result-all_objects.
    ENDIF.

    "=================================================
    " Make sure D010INC and WBCROSSGT are up-to-date
    "=================================================
    SELECT FROM tadir
        FIELDS tadir~pgmid,
               tadir~object,
               tadir~obj_name,
               tadir~devclass,
               CAST( CASE
               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
                    AND left( tadir~obj_name, 1 ) = '/'
                    THEN concat( '/', replace( substring( tadir~obj_name, 2, 39 ), '/', '/SAPL' ) )
               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
                    AND left( tadir~obj_name, 1 ) <> '/'
                    THEN concat( 'SAPL', tadir~obj_name )
               WHEN tadir~object = 'CLAS'
                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'CP' )
               WHEN tadir~object = 'INTF'
                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'IP' )
               WHEN tadir~object = 'CNTX'
                    THEN concat( 'CONTEXT_X_', tadir~obj_name )
               ELSE " PROG
                    tadir~obj_name
               END AS CHAR( 40 ) ) AS master
        WHERE tadir~devclass IN @package_range
          AND tadir~object   IN ('CLAS','CNTX','FUGR','FUGS','INTF','PROG')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~obj_name IN @range_of_obj_name
    INTO TABLE @DATA(main_programs).

    DATA(lo_prog) = NEW SHRIS5ZPAUXVKEPN5HWER7BEX2WBRY( ).

    lo_prog->select_prog( it_rng_prog = VALUE #( FOR <main_program> IN main_programs
                                                 ( sign   = 'I'
                                                   option = 'EQ'
                                                   low    = <main_program>-master ) ) ).

    lo_prog->filter_prog( generatd = abap_false ).

    " Used to regenerate WBCROSSGT
    SET HANDLER on_program_generated FOR lo_prog.

    lo_prog->gen_progs( i_commit_frequency = 10 ).


*    "=================================================
*    " Recalculate WBCROSSGT of all programs
*    "=================================================
*
*    LOOP AT main_programs REFERENCE INTO DATA(main_program).
*      DATA(wb_crossreference) = NEW cl_wb_crossreference( p_name    = main_program->master
*                                                          p_include = '' ).
*      wb_crossreference->index_actualize( IMPORTING p_error = DATA(error) ).
*    ENDLOOP.
*    COMMIT WORK.


    " CLSNAME               RELTYPE  REFCLSNAME
    " ====================  =======  ====================
    " ZCL_EXCEL             1        ZIF_EXCEL_BOOK_PROPERTIES
    " ZCL_EXCEL_GRAPH_BARS  2        ZCL_EXCEL_GRAPH
    " ZCX_EXCEL             2        CX_STATIC_CHECK


    " At statement "CLASS zcl_excel_graph_bars DEFINITION INHERITING FROM zcl_excel_graph", this syntax error MESSAGEG)F:
*    " "Components of classes declared using "CLASS ZCL_EXCEL_GRAPH DEFINITION DEFERRED" can only be accessed after the class is defined (CLASS ZCL_EXCEL_GRAPH DEFINITION)."

    SELECT FROM seometarel
            INNER JOIN tadir
              ON tadir~obj_name = seometarel~clsname
            FIELDS seometarel~clsname, seometarel~reltype, seometarel~refclsname
            WHERE tadir~object   IN ('CLAS','INTF')
              AND tadir~object   IN @range_of_obj_type
              AND tadir~devclass IN @package_range
              AND tadir~obj_name IN @range_of_obj_name
            INTO TABLE @result-oo_relationships.





*    WITH
*    +exception_class AS (
*    SELECT FROM seoclassdf
*        INNER JOIN tadir
*            ON tadir~pgmid  = 'R3TR'
*           AND tadir~object = 'CLAS'
*           AND tadir~obj_name = seoclassdf~clsname
*        FIELDS seoclassdf~clsname AS name
*        WHERE seoclassdf~category = '40'
*          AND tadir~devclass IN @package_range
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~obj_name IN @range_of_obj_name
*    ),
*    +tadir_master AS (
*    SELECT FROM tadir
*        FIELDS tadir~pgmid,
*               tadir~object,
*               tadir~obj_name,
*               tadir~devclass,
*               CASE
*               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                    AND left( tadir~obj_name, 1 ) = '/'
*                    THEN concat( '/', replace( substring( tadir~obj_name, 2, 39 ), '/', '/SAPL' ) )
*               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
*                    AND left( tadir~obj_name, 1 ) <> '/'
*                    THEN concat( 'SAPL', tadir~obj_name )
*               WHEN tadir~object = 'CLAS'
*                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'CP' )
*               WHEN tadir~object = 'INTF'
*                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'IP' )
*               WHEN tadir~object = 'CNTX'
*                    THEN concat( 'CONTEXT_X_', tadir~obj_name )
*               ELSE " PROG
*                    tadir~obj_name
*               END AS master
*        WHERE tadir~devclass IN @package_range
*          AND tadir~object   IN ('CLAS','CNTX','FUGR','FUGS','INTF','PROG')
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~obj_name IN @range_of_obj_name
*    )
*    SELECT
*        FROM wbcrossgt
*        INNER JOIN +exception_class
*            ON +exception_class~name = wbcrossgt~name
*        INNER JOIN d010inc
*            ON d010inc~include = wbcrossgt~include
*        INNER JOIN +tadir_master
*            ON +tadir_master~master = d010inc~master
*        FIELDS DISTINCT
*            +tadir_master~object   AS using_object_type,
*            +tadir_master~obj_name AS using_object_name,
*            'CLAS'                 AS used_object_type,
*            +exception_class~name  AS used_object_name
*        WHERE wbcrossgt~otype        =  'TY'
*          AND +tadir_master~devclass IN @package_range
*          AND +tadir_master~object   IN @range_of_obj_type
*          AND +tadir_master~obj_name IN @range_of_obj_name
*          AND +tadir_master~obj_name <> +exception_class~name " remove meaningless link CLAS ZCX_EXCEL ZCX_EXCEL
*INTO TABLE @DATA(xx).

    " The definition of exception classes must be positioned before any class definition containing RAISING of this exception class,
    " because the compiler checks that the mentioned class is an exception class, otherwise syntax error:
    " The class "&1" was not derived from either "CX_STATIC_CHECK" or "CX_DYNAMIC_CHECK".
    "
    " TADIR
    " OBJECT  OBJ_NAME
    " ======  ====================
    " CLAS    ZCL_EXCEL
    " CLAS    ZCX_EXCEL
    " INTF    ZIF_EXCEL_WRITER



    " WBCROSSGT contains a few groups of relationships:
    "
    " 1) The ones due to "INTERFACES ...", here "INTERFACES zif_excel_book_properties" in ZCL_EXCEL:
    "    OTYPE  NAME                       INCLUDE
    "    =====  =========================  ==================================
    "    TY     ZIF_EXCEL_BOOK_PROPERTIES  ZCL_EXCEL=====================CU
    "
    " 1) The ones due to "METHODS ... IMPORTING ... TYPE ... DEFAULT otherclassinterface=>attribute",
    "    here "METHODS add_new_drawing IMPORTING ... DEFAULT zcl_excel_drawing=>type_image" for ZCL_EXCEL:
    "    OTYPE  NAME               INCLUDE
    "    =====  =================  ==================================
    "    TY     ZCL_EXCEL_DRAWING  ZCL_EXCEL=====================CU
    "
    " ERROR "Direct access to components of the global class ZCL_EXCEL_DRAWING is not possible. The statement CLASS ZCL_EXCEL_DRAWING DEFINITION LOAD is missing."
    " In ZCL_EXCEL:
    "   METHODS add_new_drawing
    "     IMPORTING
    "       !ip_type TYPE zexcel_drawing_type DEFAULT zcl_excel_drawing=>type_image " <=== "CLASS zcl_excel_drawing DEFINITION"... should be located above
    "    OTYPE  NAME                                        INCLUDE
    "    =====  =========================================   ==================================
    "    DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE             ZCL_EXCEL=====================CU

    "   METHODS xx
    "     IMPORTING
    "       xx TYPE ZIF_ABAPGIT_AJSON_FILTER=>TY_FILTER_TAB
    "    OTYPE  NAME                                        INCLUDE
    "    =====  =========================================   ==================================
    "    TY     ZIF_ABAPGIT_AJSON_FILTER\TY:TY_FILTER_TAB   ZCL_ABAPGIT_AJSON_FILTER_LIB==CCAU

*    SELECT FROM wbcrossgt
*            CROSS JOIN tadir
*        FIELDS DISTINCT
*            tadir~object   AS using_object_type,
*            tadir~obj_name AS using_object_name,
*            'CLIF'         AS used_object_type, " CLAS or INTF ?
*            wbcrossgt~name AS used_object_name  " DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE   ZCL_EXCEL=====================CU
*        WHERE ( concat( rpad( tadir~obj_name, 30, '=' ), 'CU' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CI' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CO' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCIMP' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCDEF' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCAU' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCMAC' ) = wbcrossgt~include
*             OR concat( rpad( tadir~obj_name, 30, '=' ), 'IU' ) = wbcrossgt~include )
*          AND tadir~object   IN ('CLAS','INTF')
*          AND tadir~object   IN @range_of_obj_type
*          AND tadir~devclass IN @package_range
*          AND tadir~obj_name IN @range_of_obj_name
*          AND wbcrossgt~name LIKE '%\%'
*into table @data(itab).
*ASSERT 1 = 1. " debug helper

    " The following SELECT will result in, which must be interpreted as declare first ZCX_EXCEL,
    " then ZCL_EXCEL and ZIF_EXCEL_CONVERTER in any order:
    "    USING_OBJECT_TYPE  USING_OBJECT_NAME    USED_OBJECT_TYPE  USED_OBJECT_NAME
    "    ===========================================================================
    "    CLAS               ZCL_EXCEL            CLAS              ZCX_EXCEL
    "    CLAS               ZCX_EXCEL            CLAS              CX_STATIC_CHECK
    "    CLAS               ZCX_EXCEL            CLAS              ZCX_EXCEL             "<==== meaningless link to be removed
    "    INTF               ZIF_EXCEL_CONVERTER  CLAS              ZCX_EXCEL
    "
    " 2) The ones due to "INHERITING FROM cx_static_check" (or any other exception class), here for ZCX_EXCEL:
    "    (wbcrossgt_cx below)
    "    OTYPE  NAME       INCLUDE
    "    =====  =========  ==================================
    "    TY     CX_ROOT    ZCX_EXCEL=====================CU
    WITH
    +exception_class AS (
    SELECT FROM seoclassdf
        INNER JOIN tadir
            ON tadir~pgmid  = 'R3TR'
           AND tadir~object = 'CLAS'
           AND tadir~obj_name = seoclassdf~clsname
        FIELDS seoclassdf~clsname AS name
        WHERE seoclassdf~category = '40'
          AND tadir~devclass IN @package_range
          AND tadir~object   IN @range_of_obj_type
          AND tadir~obj_name IN @range_of_obj_name
*    +class_include_cu AS (
*    SELECT FROM tadir
*        FIELDS tadir~obj_name AS class_name,
*               concat( rpad( tadir~obj_name, 30, '=' ), 'CU' ) AS cu_include_name
*        WHERE tadir~devclass IN @package_range
*          AND tadir~object = 'CLAS'
*          AND TADIR~object   IN @range_of_obj_type
*          AND tadir~obj_name IN @range_of_obj_name
*    ),
*    +exception_class AS (
*    SELECT FROM wbcrossgt
*        INNER JOIN +class_include_cu
*            ON +class_include_cu~cu_include_name = wbcrossgt~include
*        FIELDS +class_include_cu~cu_include_name AS name
*        WHERE wbcrossgt~otype = 'TY'
*          AND wbcrossgt~name  = 'CX_ROOT'
**        INNER JOIN tadir
**            ON rpad( tadir~obj_name, 30, '=' ) = wbcrossgt~include
**           AND SUBSTRING( tadir~obj_name, 31, 2 ) IN ('CU','CO','CI')
**        FIELDS tadir~obj_name AS name
**        WHERE wbcrossgt~otype = 'TY'
**          AND wbcrossgt~name  = 'CX_ROOT'
**          and tadir~devclass IN @package_range
**          AND tadir~object = 'CLAS'
**          AND TADIR~object   IN @range_of_obj_type
**          AND tadir~obj_name IN @range_of_obj_name
    ),
    " TADIR              D010INC-MASTER
    "==================  ==================================
    " R3TR PROG XXXXX    XXXXX
    " R3TR FUGR XXXXX    SAPLXXXXX
    " R3TR FUGR /XX/XXX  /XX/SAPLXXX
    " R3TR CLAS XXXXX    XXXXX=========================CP
    +tadir_master AS (
    SELECT FROM tadir
        FIELDS tadir~pgmid,
               tadir~object,
               tadir~obj_name,
               tadir~devclass,
               CASE
               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
                    AND left( tadir~obj_name, 1 ) = '/'
                    THEN concat( '/', replace( substring( tadir~obj_name, 2, 39 ), '/', '/SAPL' ) )
               WHEN ( tadir~object = 'FUGR' OR tadir~object = 'FUGS' )
                    AND left( tadir~obj_name, 1 ) <> '/'
                    THEN concat( 'SAPL', tadir~obj_name )
               WHEN tadir~object = 'CLAS'
                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'CP' )
               WHEN tadir~object = 'INTF'
                    THEN concat( rpad( tadir~obj_name, 30, '=' ), 'IP' )
               WHEN tadir~object = 'CNTX'
                    THEN concat( 'CONTEXT_X_', tadir~obj_name )
               ELSE " PROG
                    tadir~obj_name
               END AS master
        WHERE tadir~devclass IN @package_range
          AND tadir~object   IN ('CLAS','CNTX','FUGR','FUGS','INTF','PROG')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~obj_name IN @range_of_obj_name
    )

    " 1) The ones due to "METHODS ... RAISING ...", here for ZCL_EXCEL and ZIF_EXCEL_WRITER which are using ZCX_EXCEL:
    "    (wbcrossgt below)
    "    OTYPE  NAME       INCLUDE
    "    =====  =========  ==================================
    "    TY     ZCX_EXCEL  ZCL_EXCEL=====================CU
    "    TY     ZCX_EXCEL  ZIF_EXCEL_WRITER==============IU
    "    TY     ZCX_EXCEL  ZCL_EXCEL_WRITER_CSV==========CI
    SELECT
        FROM wbcrossgt
        INNER JOIN +exception_class
            ON +exception_class~name = wbcrossgt~name
        INNER JOIN d010inc
            ON d010inc~include = wbcrossgt~include
        INNER JOIN +tadir_master
            ON +tadir_master~master = d010inc~master
        FIELDS DISTINCT
            +tadir_master~object   AS using_object_type,
            +tadir_master~obj_name AS using_object_name,
            'CLAS'                 AS used_object_type,
            +exception_class~name  AS used_object_name
        WHERE wbcrossgt~otype        =  'TY'
          AND +tadir_master~devclass IN @package_range
          AND +tadir_master~object   IN @range_of_obj_type
          AND +tadir_master~obj_name IN @range_of_obj_name
          AND +tadir_master~obj_name <> +exception_class~name " remove meaningless link CLAS ZCX_EXCEL ZCX_EXCEL
    UNION
    SELECT from wbcrossgt
            cross join tadir
        fields distinct
            tadir~object   AS using_object_type,
            tadir~obj_name AS using_object_name,
            'CLIF'         AS used_object_type, " CLAS or INTF ?
            wbcrossgt~name AS used_object_name  " DA     ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE   ZCL_EXCEL=====================CU
        WHERE ( concat( rpad( tadir~obj_name, 30, '=' ), 'CU'    ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CI'    ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CO'    ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCIMP' ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCDEF' ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCAU'  ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'CCMAC' ) = wbcrossgt~include
             OR concat( rpad( tadir~obj_name, 30, '=' ), 'IU'    ) = wbcrossgt~include )
          AND tadir~object   =  'CLAS'
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
          AND wbcrossgt~name LIKE '%\%'
    UNION
    SELECT from wbcrossgt
            cross join tadir
        fields distinct
            tadir~object   AS using_object_type,
            tadir~obj_name AS using_object_name,
            'CLIF'         AS used_object_type, " CLAS or INTF ?
            wbcrossgt~name AS used_object_name  " DA     ZIF_WWWWWW\DA:TYPE_IMAGE   ZIF_XXXXX=====================IU
        WHERE concat( rpad( tadir~obj_name, 30, '=' ), 'IU' ) = wbcrossgt~include
          AND tadir~object   =  'INTF'
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
          AND wbcrossgt~name LIKE '%\%'
    INTO TABLE @result-used_exception_classes.

    " At this point, USED_EXCEPTION_CLASSES contains:
    "
    " USING_OBJECT_TYPE  USING_OBJECT_NAME     USED_OBJECT_TYPE  USED_OBJECT_NAME
    " =================  ====================  ================  ===============================
    " CLAS               ZCL_EXCEL             CLAS              ZCX_EXCEL
    " INTF               ZIF_EXCEL_CONVERTER   CLAS              ZCX_EXCEL
    " CLAS               ZCL_EXCEL             CLIF              ZCL_EXCEL_DRAWING\DA:TYPE_IMAGE
    " CLAS               ZCL_EXCEL_AUTOFILTER  CLIF              ZCL_EXCEL_AUTOFILTER\TY:TT_FILTERS
    " CLAS               ZCL_EXCEL_AUTOFILTER  CLIF              ZCL_EXCEL_AUTOFILTER\TY:TS_FILTER
    " INTF               ZIF_EXCEL_READER      CLIF              SY\DA:BATCH
    " INTF               ZIF_EXCEL_READER      CLIF              SYST\TY:BATCH

    SORT result-used_exception_classes BY table_line.
    DELETE ADJACENT DUPLICATES FROM result-used_exception_classes COMPARING table_line.

    " Replace CLIF with either CLAS or INTF.
    LOOP AT result-used_exception_classes REFERENCE INTO DATA(using_used_object_type)
        WHERE used_object_type = 'CLIF'.
      using_used_object_type->used_object_name = substring_before( val = using_used_object_type->used_object_name
                                                                   sub = '\' ).
      IF using_used_object_type->using_object_name = using_used_object_type->used_object_name.
        DELETE result-used_exception_classes USING KEY loop_key.
      ELSEIF line_exists( result-all_objects[ pgmid    = 'R3TR'
                                              object   = 'CLAS'
                                              obj_name = using_used_object_type->used_object_name ] ).
        using_used_object_type->used_object_type = 'CLAS'.
      ELSEIF line_exists( result-all_objects[ pgmid    = 'R3TR'
                                              object   = 'INTF'
                                              obj_name = using_used_object_type->used_object_name ] ).
        using_used_object_type->used_object_type = 'INTF'.
      ELSE.
        DELETE result-used_exception_classes USING KEY loop_key.
      ENDIF.
    ENDLOOP.

    SORT result-used_exception_classes BY table_line.
    DELETE ADJACENT DUPLICATES FROM result-used_exception_classes COMPARING table_line.


    " NAME                  CLSCCINCL
    " ====================  =========
    " ZCL_EXCEL             X
    " ZCL_EXCEL_AUTOFILTER  X
    SELECT
        FROM seoclassdf
            INNER JOIN seoclass
              ON seoclass~clsname = seoclassdf~clsname
            INNER JOIN tadir
              ON tadir~obj_name = seoclassdf~clsname
        FIELDS seoclassdf~clsname, seoclass~clstype, seoclassdf~clsccincl, seoclassdf~category
        WHERE tadir~object   IN ('CLAS','INTF')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @result-class_descriptions.


    " CLASSNAME  METHODINDX  METHODNAME
    " =========  ==========  =======================
    " ZCL_EXCEL  00000
    " ZCL_EXCEL  00001       ADD_NEW_AUTOFILTER
    " ZCL_EXCEL  00002       ADD_NEW_COMMENT
    SELECT
        FROM tmdir
            INNER JOIN tadir
              ON tadir~obj_name = tmdir~classname
        FIELDS tmdir~classname, tmdir~methodindx, tmdir~methodname
        WHERE tadir~object   IN ('CLAS','INTF')
          AND tadir~object   IN @range_of_obj_type
          AND tadir~devclass IN @package_range
          AND tadir~obj_name IN @range_of_obj_name
        INTO TABLE @result-class_methods.

  ENDMETHOD.
  METHOD create.

    result = NEW Lcl_shrinker( ).
    result->customizer = customizer.

  ENDMETHOD.
  METHOD read_class_interface_includes.

    result-classes = VALUE #(
        FOR <class> IN classes
        WHERE ( clstype = '0' ) " class pools (not interface pools)
        LET aux_renamings = VALUE ty_cc_renamings(
            FOR <renaming> IN renamings
            WHERE ( class_name = <class>-name )
            ( <renaming> ) )
        IN
        ( name      = <class>-name
          clsccincl = <class>-clsccincl
          includes  = VALUE #(
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCAU'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCDEF'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCIMP'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CCMAC'
                                             cc_renamings  = aux_renamings ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CI' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CL' ) )
                " Methods = CM### extensions
                ( LINES OF VALUE #(
                    FOR <method> IN class_methods
                    WHERE ( classname = <class>-name )
                    ( read_report_class_include( class_name  = <class>-name
                                                 extension   = convert_methodindx_to_extensio( <method>-methodindx )
                                                 method_name = <method>-methodname ) ) ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CO' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CP' ) )
                ( read_report_class_include( class_name = <class>-name
                                             extension  = 'CU' ) ) ) ) ).

    result-interfaces = VALUE #(
        FOR <object> IN interfaces
        LET intf_name = CONV seoclsname( <object>-obj_name )
        IN
        ( name     = <object>-obj_name
          includes = VALUE #(
                ( read_report_class_include( class_name = intf_name
                                             extension  = 'IP' ) )
                ( read_report_class_include( class_name = intf_name
                                             extension  = 'IU' ) ) ) ) ).

  ENDMETHOD.
  METHOD replace_texts.

    LOOP AT replacements REFERENCE INTO DATA(local_obj_renaming).

      FIND ALL OCCURRENCES OF
            REGEX local_obj_renaming->posix_regex ##REGEX_POSIX
            IN TABLE abap_source_code
            IGNORING CASE
            RESULTS DATA(matches).

      SORT matches BY line ASCENDING offset DESCENDING.

      LOOP AT matches REFERENCE INTO DATA(match).
        DATA(line) = REF #( abap_source_code[ match->line ] ).
        DATA(old_name) = substring( val = line->* off = match->offset len = match->length ).

        IF old_name CP 'zcl_abapgit_data_injector'.
          ASSERT 1 = 1. " debug helper
        ENDIF.

        IF line->* CS '>' AND line->* CS '~'.
          ASSERT 1 = 1. " debug helper
        ENDIF.

        DATA(do_replace) = abap_false.
        IF local_obj_renaming->start_of_abap_word = abap_false
            AND local_obj_renaming->member_interface_prefix = abap_false.
          do_replace = abap_true.
        ELSE.
          DATA(abap_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = abap_source_code
                                                                    i_linenr  = match->line ).
          IF local_obj_renaming->start_of_abap_word = abap_true
                AND line_exists( abap_statement-stokes[ row = match->line
                                                        col = match->offset ] ).
            do_replace = abap_true.
          ENDIF.
          IF do_replace = abap_false
                AND local_obj_renaming->member_interface_prefix = abap_true.
            DATA(tabix) = 0.
            LOOP AT abap_statement-stokes REFERENCE INTO DATA(token)
                WHERE row = match->line.
              IF token->col > match->offset.
                EXIT.
              ENDIF.
              tabix = sy-tabix.
            ENDLOOP.
            IF tabix > 0.
              token = REF #( abap_statement-stokes[ tabix ] ).
              DATA(token_offset) = match->offset - token->col - 2.
              IF token_offset >= 0
                    AND strlen( token->str ) >= token_offset + 2.
                CASE token->str+token_offset(2).
                  WHEN '->' OR '=>'.
                    do_replace = abap_true.
                ENDCASE.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF do_replace = abap_true.
          DATA(new_name) = replace( val   = old_name
                                    regex = local_obj_renaming->posix_regex ##REGEX_POSIX
                                    with  = local_obj_renaming->with
                                    case  = abap_false ).
          REPLACE SECTION OFFSET match->offset LENGTH match->length OF line->* WITH new_name.
        ENDIF.
*        ENDIF.

        ASSERT 1 = 1. " debug helper
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_one_abap_code.

    TRY.

        me->package_range     = package_range.
        me->objects           = objects.
        me->range_of_obj_name = range_of_obj_name.
        me->range_of_obj_type = range_of_obj_type.
        me->cc_renamings      = class_replacements.
        me->replacements      = global_replacements.

        DATA(miscellaneous) = select_miscellaneous( package_range     = package_range
                                                    objects           = objects
                                                    range_of_obj_type = range_of_obj_type
                                                    range_of_obj_name = range_of_obj_name ).

        range_all_objects = VALUE ty_range_of_obj_name(
                FOR <object> IN miscellaneous-all_objects
                WHERE ( obj_name IN range_of_obj_name )
                ( sign   = 'I'
                  option = 'EQ'
                  low    = <object>-obj_name ) ).

        DATA(classes_interfaces) = read_class_interface_includes(
                    classes       = miscellaneous-class_descriptions
                    class_methods = miscellaneous-class_methods
                    interfaces    = VALUE #(
                                    FOR <object> IN miscellaneous-all_objects
                                    WHERE ( object = 'INTF' )
                                    ( <object> ) )
                    renamings     = cc_renamings ).
        LOOP AT classes_interfaces-classes REFERENCE INTO DATA(class).
          DATA(class_cu_include) = REF #( class->includes[ extension_code = 'CU' ] ).
          FIND ALL OCCURRENCES OF 'FRIENDS' IN TABLE class_cu_include->abap_source_code RESULTS DATA(matches).
          LOOP AT matches REFERENCE INTO DATA(match).
            DATA(abap_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = class_cu_include->abap_source_code
                                                                      i_linenr  = match->line
                                                                      i_offset  = match->offset ).
            IF abap_statement-stokes[ 1 ]-str = 'CLASS'.
              DATA(tabix_friends) = line_index( abap_statement-stokes[ str = 'FRIENDS' ] ).
              IF tabix_friends >= 2
                    AND abap_statement-stokes[ tabix_friends - 1 ]-str = 'GLOBAL'.
                DATA(at_least_one_friend_remains) = abap_false.
                DATA(tokens_to_remove) = VALUE SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_ut_stokes( ).
                LOOP AT abap_statement-stokes REFERENCE INTO DATA(token)
                    FROM tabix_friends + 1.
                  IF line_exists( miscellaneous-class_descriptions[ name     = token->str
                                                                    category = '05' ] ).
                    " Remove the friend if it's a class pool FOR TESTING (05)
                    APPEND token->* TO tokens_to_remove.
                  ELSE.
                    at_least_one_friend_remains = abap_true.
                  ENDIF.
                ENDLOOP.
                IF at_least_one_friend_remains = abap_false.
                  " Remove GLOBAL
                  token = REF #( abap_statement-stokes[ tabix_friends - 1 ] ).
                  APPEND token->* TO tokens_to_remove.
                  " Remove FRIENDS
                  token = REF #( abap_statement-stokes[ tabix_friends ] ).
                  APPEND token->* TO tokens_to_remove.
                ENDIF.
                SORT tokens_to_remove BY row ASCENDING col DESCENDING.
                LOOP AT tokens_to_remove REFERENCE INTO token.
                  DATA(abap_line) = REF #( class_cu_include->abap_source_code[ token->row ] OPTIONAL ).
                  IF abap_line IS BOUND.
                    REPLACE SECTION
                          OFFSET token->col
                          LENGTH strlen( token->str )
                          OF abap_line->*
                          WITH space.
                  ENDIF.
                ENDLOOP.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        IF customizer IS BOUND.
          customizer->adapt_source_code( CHANGING classes_interfaces = classes_interfaces ).
        ENDIF.

        DATA(deferred) = VALUE ty_abap_source_code(
            FOR <clas_intf> IN miscellaneous-class_descriptions
            WHERE ( category <> '05' ) " exclude class pools FOR TESTING
            ( SWITCH #( <clas_intf>-clstype WHEN '0' THEN |CLASS { <clas_intf>-name } DEFINITION DEFERRED.|
                                            WHEN '1' THEN |INTERFACE { <clas_intf>-name } DEFERRED.| ) ) ).

        DATA(ddic) = select_ddic_objects( package_range     = package_range
                                          range_of_obj_name = range_of_obj_name ).

        TYPES: BEGIN OF ty_dependency,
                 object     TYPE ty_object,
                 "! Object which is needed by OBJECT i.e. REF_OBJECT must be defined before OBJECT.
                 ref_object TYPE ty_object,
               END OF ty_dependency.
        TYPES ty_dependencies TYPE STANDARD TABLE OF ty_dependency WITH EMPTY KEY.

        DATA(dependencies) = VALUE ty_dependencies( ).
        LOOP AT ddic-data_elements REFERENCE INTO DATA(data_element).
          IF data_element->domname IS NOT INITIAL
              AND data_element->domname <> data_element->rollname
              AND data_element->domname <> 'OBJECT'
              AND data_element->domname <> 'DATA'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'DTEL'
                                                  obj_name = data_element->rollname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( data_element->reftype
                                                            WHEN 'C' THEN 'CLAS'
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'I' THEN 'INTF'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'S' THEN 'TABL'
                                                            ELSE          'DOMA' )
                                                  obj_name = data_element->domname )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT ddic-structure_components REFERENCE INTO DATA(structure_component).
          IF structure_component->fieldname(1) = '.'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->tabname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->precfield )
                          ) INTO TABLE dependencies.
          ELSEIF structure_component->rollname IS NOT INITIAL
                AND structure_component->reftype <> 'B'
                AND structure_component->reftype <> 'D'
                AND structure_component->rollname <> 'OBJECT'.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TABL'
                                                  obj_name = structure_component->tabname )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( structure_component->comptype
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'R' THEN SWITCH #( structure_component->reftype
                                                                    WHEN 'C' THEN 'CLAS'
                                                                    WHEN 'E' THEN 'DTEL'
                                                                    WHEN 'I' THEN 'INTF'
                                                                    WHEN 'L' THEN 'TTYP'
                                                                    WHEN 'S' THEN 'TABL'
                                                                    ELSE          '?' )
                                                            WHEN 'S' THEN 'TABL'
                                                            ELSE          '?' )
                                                  obj_name = structure_component->rollname )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT ddic-table_types REFERENCE INTO DATA(table_type).
          IF table_type->rowtype IS NOT INITIAL.
            INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                  object   = 'TTYP'
                                                  obj_name = table_type->typename )
                            ref_object = VALUE #( pgmid    = 'R3TR'
                                                  object   = SWITCH #( table_type->rowkind
                                                            WHEN 'E' THEN 'DTEL'
                                                            WHEN 'L' THEN 'TTYP'
                                                            WHEN 'R' THEN SWITCH #( table_type->reftype
                                                                    WHEN 'C' THEN 'CLAS'
                                                                    WHEN 'E' THEN 'DTEL'
                                                                    WHEN 'I' THEN 'INTF'
                                                                    WHEN 'L' THEN 'TTYP'
                                                                    ELSE          '?' )
                                                            WHEN 'S' THEN COND #( WHEN table_type->ttypkind IS INITIAL
                                                                                THEN 'TABL'   " pas un range
                                                                                ELSE 'DTEL' ) " range
                                                            ELSE          '?' )
                                                  obj_name = table_type->rowtype )
                          ) INTO TABLE dependencies.
          ENDIF.
        ENDLOOP.

        LOOP AT miscellaneous-oo_relationships REFERENCE INTO DATA(oo_relationship).
          INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                object   = SWITCH #( oo_relationship->reltype
                                                              WHEN '0' THEN 'INTF'
                                                              ELSE          'CLAS' )
                                                obj_name = oo_relationship->clsname )
                          ref_object = VALUE #( pgmid    = 'R3TR'
                                                object   = SWITCH #( oo_relationship->reltype
                                                              WHEN '2' THEN 'CLAS'
                                                              ELSE          'INTF' )
                                                obj_name = oo_relationship->refclsname )
                        ) INTO TABLE dependencies.
        ENDLOOP.

        LOOP AT miscellaneous-used_exception_classes REFERENCE INTO DATA(used_exception_class).
          INSERT VALUE #( object     = VALUE #( pgmid    = 'R3TR'
                                                object   = used_exception_class->using_object_type
                                                obj_name = used_exception_class->using_object_name )
                          ref_object = VALUE #( pgmid    = 'R3TR'
                                                object   = used_exception_class->used_object_type
                                                obj_name = used_exception_class->used_object_name )
                        ) INTO TABLE dependencies.
        ENDLOOP.

        SORT dependencies BY table_line.
        DELETE ADJACENT DUPLICATES FROM dependencies COMPARING table_line.

        TYPES ty_ref_objects TYPE STANDARD TABLE OF ty_object WITH EMPTY KEY.
        TYPES: BEGIN OF ty_dependency_2,
                 object      TYPE ty_object,
                 "! Object which is needed by OBJECT i.e. REF_OBJECT must be defined before OBJECT.
                 ref_objects TYPE ty_objects,
               END OF ty_dependency_2.
        TYPES ty_dependencies_2 TYPE STANDARD TABLE OF ty_dependency_2 WITH EMPTY KEY.

        DATA(dependencies_2) = VALUE ty_dependencies_2(
                ( LINES OF VALUE #(
                    FOR GROUPS <group_object> OF <dependency> IN dependencies
                    GROUP BY <dependency>-object
                    ( object      = <group_object>
                      ref_objects = VALUE #(
                                    FOR <dependency_bis> IN GROUP <group_object>
                                    ( LINES OF COND #( WHEN line_exists( miscellaneous-all_objects[ table_line = <dependency_bis>-ref_object ] )
                                                       THEN VALUE #( ( <dependency_bis>-ref_object ) ) ) ) ) ) ) )
                ( LINES OF VALUE #(
                    FOR <object> IN miscellaneous-all_objects
                    ( LINES OF COND #( WHEN NOT line_exists( dependencies[ object = <object> ] ) THEN VALUE #( ( object = <object> ) ) ) ) ) ) ).

        SORT dependencies_2 BY table_line.

        " All dictionary objects will be positioned before classes and interfaces because
        " the object types will be defined as DEFERRED before the TYPES equivalences of dictionary objects.

        TYPES ty_range_objects TYPE RANGE OF trobjtype.

        DATA(objects_in_order) = VALUE ty_objects( ).
        DATA(range_ddic_objects) = VALUE ty_range_objects(
                sign   = 'I'
                option = 'EQ'
                ( low = 'DOMA' )
                ( low = 'DTEL' )
                ( low = 'TABL' )
                ( low = 'TTYP' ) ).

        LOOP AT dependencies_2 REFERENCE INTO DATA(dependency_2)
              WHERE object-object IN range_ddic_objects.
          DELETE dependency_2->ref_objects
              WHERE object = 'CLAS'
                 OR object = 'INTF'.
        ENDLOOP.

        DATA(range_objects) = range_ddic_objects.

        DO.
          DATA(dependencies_implying_a_move) = VALUE ty_dependencies( ).
          LOOP AT dependencies_2 REFERENCE INTO dependency_2
                WHERE object-object IN range_objects
                  AND ref_objects IS INITIAL.
            INSERT dependency_2->object INTO TABLE objects_in_order.
            LOOP AT dependencies_2 REFERENCE INTO DATA(dependency_2_bis).
              DELETE dependency_2_bis->ref_objects WHERE table_line = dependency_2->object.
            ENDLOOP.
            DELETE dependencies_2 USING KEY loop_key.
          ENDLOOP.
          IF sy-subrc <> 0 OR dependencies_2 IS INITIAL.
            IF range_objects IS NOT INITIAL.
              range_objects = VALUE #( ).
            ELSE.
              EXIT.
            ENDIF.
          ENDIF.
        ENDDO.

        IF dependencies_2 IS NOT INITIAL.
          " circular reference error (e.g. object A requires object B which requires A)
          RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING text = 'Circular reference while resolving order in DDIC objects'(001).
        ENDIF.


        DATA(abap_source_code_4_classes) = VALUE ty_abap_source_code_4_classes(
                    FOR <class> IN classes_interfaces-classes
                    ( get_abap_for_class_pool( <class> ) ) ).

        IF omit_test_classes = abap_true.
          remove_test_classes( CHANGING abap_source_code = abap_source_code_4_classes ).
        ENDIF.


        result-def_abap_source_code = VALUE ty_abap_source_code(
            ( LINES OF deferred )
            ( LINES OF VALUE #(
                FOR <object> IN objects_in_order
                ( LINES OF SWITCH #( <object>-object
                    WHEN 'DTEL' THEN get_abap_for_data_element( ddic-data_elements[ rollname = <object>-obj_name ] )
                    WHEN 'CLAS' THEN abap_source_code_4_classes[ name = <object>-obj_name ]-definition
                    WHEN 'INTF' THEN get_abap_for_interface_pool( classes_interfaces-interfaces[ name = <object>-obj_name ] )
                    WHEN 'TABL' THEN get_abap_for_structure( structure            = ddic-structures[ tabname = <object>-obj_name ]
                                                             structure_components = ddic-structure_components )
                    WHEN 'TTYP' THEN get_abap_for_table_type( table_type     = ddic-table_types[ typename = <object>-obj_name ]
                                                              key_components = ddic-table_type_key_components
                                                              sec_keys       = ddic-table_type_sec_keys ) ) ) ) ) ).


        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result-def_abap_source_code ).

        result-imp_abap_source_code = VALUE ty_abap_source_code(
                FOR <object> IN miscellaneous-all_objects
                WHERE ( object = 'CLAS' )
                ( LINES OF abap_source_code_4_classes[ name = <object>-obj_name ]-implementation ) ).

        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result-imp_abap_source_code ).

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
  METHOD syntax_check.

    DATA(synt) = VALUE ty_syntax_check( dir = VALUE #( name = '$$DUMMY' subc = '1' fixpt = 'X' uccheck = 'X' ) ).

    SYNTAX-CHECK FOR abap_source_code MESSAGE synt-mess LINE synt-lin WORD synt-wrd DIRECTORY ENTRY synt-dir INCLUDE synt-incl OFFSET synt-off MESSAGE-ID synt-mid.
    synt-subrc = sy-subrc.
    " SYNTAX-CHECK FOR itab MESSAGE mess LINE lin WORD wrd
    "                  [PROGRAM prog] [DIRECTORY ENTRY dir]
    "                  [WITH CURRENT SWITCHSTATES]
    " ... [INCLUDE incl]
    "     [OFFSET off]
    "     [MESSAGE-ID mid] ...

    result = synt.

  ENDMETHOD.
  METHOD split_at_regex.

    FIND ALL OCCURRENCES OF REGEX regex IN val RESULTS DATA(matches).

    " 0 match means 1 segment (split 'ab' at ':' -> 0 match and result is 'ab')
    IF matches IS INITIAL.
      result = VALUE #( ( CONV #( val ) ) ).
    ELSE.
      " 1 match means 2 segments (split 'a:b' at ':' -> 1 match and result is 'a' and 'b'),
      " 2 matches means 3 segments,
      " etc.
      IF max_splits >= 1 AND lines( matches ) >= max_splits.
        DELETE matches FROM max_splits.
      ENDIF.
      DATA(offset) = 0.
      LOOP AT matches ASSIGNING FIELD-SYMBOL(<match>).
        DATA(length) = <match>-offset - offset.
        APPEND substring( val = val off = offset len = length ) TO result.
        offset = <match>-offset + <match>-length.
      ENDLOOP.
      APPEND substring( val = val off = offset len = strlen( val ) - offset ) TO result.
    ENDIF.

  ENDMETHOD.
  METHOD get_abap_for_class_pool.
    TYPES: BEGIN OF ty_find_replace_item,
             from TYPE string,
             to   TYPE string_table,
           END OF ty_find_replace_item.
    TYPES ty_find_replace_items TYPE STANDARD TABLE OF ty_find_replace_item WITH EMPTY KEY.
    TYPES ty_extension_codes TYPE STANDARD TABLE OF ty_extension_code WITH EMPTY KEY.
    TYPES ty_ref_includes TYPE STANDARD TABLE OF REF TO ty_oo_include WITH EMPTY KEY.

    DATA(class) = class2.

    IF class-name = 'ZCL_EXCEL_READER_2007'.
      ASSERT 1 = 1. " debug helper
    ENDIF.

    "=====================================
    " Convert to lower case
    "=====================================
    DATA(class_pool_abap_source_code) = REF #( class-includes[ extension_code = 'CP' ]-abap_source_code ).
    FIND ALL OCCURRENCES OF REGEX '^ *(?:class-pool|include)[^\.]*\. *$'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS DATA(matches) ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO DATA(match).
      DATA(line) = REF #( class_pool_abap_source_code->*[ match->line ] ).
      TRANSLATE line->* TO LOWER CASE.
      CONDENSE line->*.
    ENDLOOP.

    class_pool_abap_source_code = REF #( class-includes[ extension_code = 'CU' ]-abap_source_code ).
    FIND ALL OCCURRENCES OF REGEX '^ *public *$'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS matches ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO match.
      line = REF #( class_pool_abap_source_code->*[ match->line ] ).
      TRANSLATE line->* TO LOWER CASE.
      CONDENSE line->*.
    ENDLOOP.


    "=================================================
    " Rename all symbols declared in CCDEF, CCMAC, CCIMP and CCAU includes,
    " because the same names may be used for other local code in other class pools,
    " which would lead to syntax errors because one same name is used in the same program
    " to describe different objects.
    "   - TYPES XXX (except the ones between TYPES BEGIN OF and END OF)
    "   - TYPES BEGIN OF XXX
    "   - CONSTANTS XXX (except the ones between CONSTANTS BEGIN OF and END OF)
    "   - CONSTANTS BEGIN OF XXX
    "   - CLASS XXX DEFINITION
    "   - INTERFACE XXX
    " XXX may be used in CI and CM### includes, so XXX must be renamed there also.
    " SOLUTION: assign a new unique name which will be a 25-characters base 36 GUID preceded with "SHRIN"
    "           e.g. SHRIN46Y0UXXVUGVEYH0K3GH55TMOB
    "=================================================

    " Row  CLASS_NAME            ABAP_KEYWORD  SYMBOL_NAME
    " ======================================================================
    " 1    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_ABAPGIT_TADIR_MOCK
    " 2    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_NO_DEPENDENCY_INJECTION
    " 3    ZCL_ABAPGIT_INJECTOR  CLASS         LTCL_SIMPLE_DEPENDENCY_INJECT
    " 4    ZCL_ABAPGIT_SETTINGS  CLASS         LTCL_SETTINGS

    DATA(local_class_pool_local_symbols) = VALUE ty_class_pool_local_symbols( ).
    LOOP AT VALUE ty_extension_codes(
                ( 'CCDEF' )
                ( 'CCMAC' )
                ( 'CCIMP' )
                ( 'CCAU' ) )
        REFERENCE INTO DATA(extension_code).
      DATA(cc_include_abap_source_code) = REF #( class-includes[ extension_code = extension_code->* ]-abap_source_code ).
      FIND ALL OCCURRENCES OF REGEX '\<TYPES\>|\<CONSTANTS\>|\<ENDCLASS\>|\<ENDINTERFACE\>|\<CLASS\>|\<INTERFACE\>'
                              IN TABLE cc_include_abap_source_code->*
                              IGNORING CASE
                              RESULTS matches ##REGEX_POSIX.


      DATA(inside_class) = abap_false.

      LOOP AT matches REFERENCE INTO match.

        DATA(abap_line) = REF #( cc_include_abap_source_code->*[ match->line ] ).
        DATA(abap_keyword) = to_upper( abap_line->*+match->offset(match->length) ).
        DATA(abap_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = cc_include_abap_source_code->*
                                                                  i_linenr  = match->line ).

        IF abap_statement-stokes IS NOT INITIAL.

          IF inside_class = abap_false.

            DATA(symbol_name) = to_upper( SWITCH string( abap_statement-stokes[ 1 ]-str
              WHEN 'TYPES' OR 'CONSTANTS' THEN COND #( WHEN abap_statement-stokes[ 2 ]-str = 'BEGIN' AND abap_statement-stokes[ 3 ]-str = 'OF' THEN
                                                          abap_statement-stokes[ 4 ]-str
                                                       WHEN abap_statement-stokes[ 2 ]-str = 'END' AND abap_statement-stokes[ 3 ]-str = 'OF' THEN
                                                          ''
                                                       ELSE
                                                          abap_statement-stokes[ 2 ]-str )
              WHEN 'CLASS' OR 'INTERFACE' THEN COND #( WHEN abap_statement-stokes[ 2 ]-str <> class-name THEN abap_statement-stokes[ 2 ]-str ) ) ).

            IF symbol_name IS NOT INITIAL.
              INSERT VALUE #(
                      class_name   = class-name
                      abap_keyword = abap_keyword
                      symbol_name  = symbol_name
                      replacement  = get_random_replacement_name( )
                  ) INTO TABLE local_class_pool_local_symbols.
            ENDIF.

          ENDIF.

          CASE abap_statement-stokes[ 1 ]-str.
            WHEN 'CLASS' OR 'INTERFACE'.
              inside_class = abap_true.
            WHEN 'ENDCLASS' OR 'ENDINTERFACE'.
              inside_class = abap_false.
          ENDCASE.

        ENDIF.
      ENDLOOP.
    ENDLOOP.

    INSERT LINES OF local_class_pool_local_symbols INTO TABLE class_pool_local_symbols.


    "=====================================
    " GLOBAL FRIENDS -> FRIENDS
    "=====================================
    FIND ALL OCCURRENCES OF REGEX 'global +friends'
                            IN TABLE class_pool_abap_source_code->*
                            IGNORING CASE
                            RESULTS matches ##REGEX_POSIX.
    LOOP AT matches REFERENCE INTO match.
      line = REF #( class_pool_abap_source_code->*[ match->line ] ).
      REPLACE SECTION OFFSET match->offset LENGTH match->length OF line->* WITH 'friends'.
    ENDLOOP.


    "=====================================
    " ???
    "=====================================
    DATA(class_abap_source_code) = class-includes[ extension_code = 'CP' ]-abap_source_code.

    DATA(ref_includes) = VALUE ty_ref_includes(
        FOR <extension_code> IN VALUE ty_extension_codes(
                ( 'CCDEF' )
                ( 'CU' )
                ( 'CO' )
                ( 'CI' )
                ( 'CCMAC' )
                ( 'CCIMP' )
                ( 'CCAU' ) )
        LET ref_include = REF #( class-includes[ extension_code = <extension_code> ] OPTIONAL )
        IN
        ( LINES OF COND #( WHEN ref_include IS BOUND THEN VALUE #( ( ref_include ) ) ) ) ).

    DATA(find_replace_items) = VALUE ty_find_replace_items(
            ( LINES OF VALUE #(
              FOR <ref_include> IN ref_includes
              ( from = |INCLUDE { <ref_include>->include_name }.|
                to   = <ref_include>->abap_source_code ) ) )
            ( from = |class-pool .|
              to   = VALUE #( ) )
            ( from = |PUBLIC| " word part of CLASS DEFINITION to declare a global class (part of a class pool).
              to   = VALUE #( ) )
            ( from = |INCLUDE METHODS.|
              to   = VALUE #( FOR <class_pool_abap_include> IN class-includes
                              WHERE ( extension_code CP 'CM*' )
                              ( LINES OF <class_pool_abap_include>-abap_source_code ) ) ) ).


    LOOP AT find_replace_items REFERENCE INTO DATA(find_replace_item).
      DATA(line_index) = line_index( class_abap_source_code[ table_line = to_lower( find_replace_item->from ) ] ).
      IF line_index <> 0.
        DATA(replaced_abap_line) = class_abap_source_code[ line_index ].
        DELETE class_abap_source_code INDEX line_index.
        INSERT LINES OF find_replace_item->to INTO class_abap_source_code INDEX line_index.
        INSERT |*{ replaced_abap_line }| INTO class_abap_source_code INDEX line_index.
      ELSE.
        log = VALUE #( BASE log ( |Line not found in class { class-name }. Line: { find_replace_item->from }| ) ).
      ENDIF.
    ENDLOOP.


    "=======================================================================
    "=======================================================================

    LOOP AT local_class_pool_local_symbols REFERENCE INTO DATA(local_class_pool_local_symbol).
      REPLACE ALL OCCURRENCES OF REGEX |\\<{ local_class_pool_local_symbol->symbol_name }\\>|
              IN TABLE class_abap_source_code
              WITH local_class_pool_local_symbol->replacement
              IGNORING CASE.
    ENDLOOP.


    "=======================================================================
    " Remove PUBLIC from the global class definition (CLASS z... DEFINITION ... PUBLIC ...)
    " but not the one of CREATE PUBLIC
    "=======================================================================
    DATA(regex) = |^\\s*class\\s+{ class-name }\\s+definition|.
    FIND ALL OCCURRENCES OF
            REGEX regex
            IN TABLE class_abap_source_code
            IGNORING CASE
            RESULTS DATA(class_definition_matches) ##REGEX_POSIX.

    IF sy-subrc = 0.
      DATA(class_definition_statement) = get_whole_abap_statement(
                              line_index       = class_definition_matches[ 1 ]-line
                              abap_source_code = class_abap_source_code ).

      REPLACE REGEX '(?!create)(\<\w+\>\s+)public'
            IN SECTION OFFSET class_definition_statement-offset
            LENGTH class_definition_statement-length
            OF class_definition_statement-whole_text
            WITH '$1'
            IGNORING CASE.
      IF sy-subrc = 0.

        SPLIT class_definition_statement-whole_text AT |\r\n| INTO TABLE DATA(class_definition_source_code).

        DELETE class_abap_source_code
                FROM class_definition_statement-first_line_index
                TO   class_definition_statement-last_line_index.

        INSERT LINES OF class_definition_source_code
                INTO class_abap_source_code
                INDEX class_definition_statement-first_line_index.

      ENDIF.
    ENDIF.

    " Special case for local test classes of class pools:
    "========================================
    "║ CLASS zcl... DEFINITION ... PUBLIC ...
    "║ ...
*    "║ CLASS ltc_1 DEFINITION DEFERRED.
*    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS zcl... DEFINITION LOCAL FRIENDS ltc_1 ltc_2 ...
    "║ ...
    "========================================
    " should be turned into local classes with only one CLASS ... DEFINITION:
    "========================================
    "║ PROGRAM.
*    "║ CLASS ltc_1 DEFINITION DEFERRED.
*    "║ CLASS ltc_2 DEFINITION DEFERRED.
    "║ CLASS zcl... DEFINITION ... FRIENDS ltc_1 ltc_2 ...
    "║ ...
    "========================================
    " To simplify:
*    "   1. all lines "CLASS ... DEFINITION DEFERRED" will be moved at the very top, above
    "      CLASS zcl... DEFINITION ... PUBLIC ...
    "      (note that "INTERFACE ... DEFERRED" is also possible).
    "   2. all classes/interfaces after "local friends" will be moved after "friends" of the
    "      first "CLASS zcl... DEFINITION ...", if "friends" is not present, it will be added before the terminal dot.
    "   3. the second CLASS zcl... DEFINITION LOCAL FRIENDS ... is removed

    IF lines( class_definition_matches ) >= 2.

      IF lines( class_definition_matches ) = 3.
        ASSERT 1 = 1. " debug helper
      ENDIF.

      FIND ALL OCCURRENCES OF
              REGEX 'class\s+(\S+)\s+definition\s+deferred'
              IN TABLE class_abap_source_code
              IGNORING CASE
              RESULTS matches ##REGEX_POSIX.

      DATA(deferred_lines) = VALUE string_table( ).
      DATA(deferred_classes) = VALUE string_table( ).
      SORT matches BY line DESCENDING.
      LOOP AT matches REFERENCE INTO match.
        line = REF #( class_abap_source_code[ match->line ] ).
        INSERT line->* INTO TABLE deferred_lines.
        DATA(submatch) = REF #( match->submatches[ 1 ] ).
        INSERT to_upper( line->*+submatch->offset(submatch->length) ) INTO TABLE deferred_classes.
        line->* = '*' && line->*.
      ENDLOOP.

      "=====================================
      " extract the list of LOCAL FRIENDS
      "=====================================
      DATA(local_friends_including_dot) = ``.
      LOOP AT class_definition_matches
            FROM 2
            REFERENCE INTO DATA(class_definition_match).

        DATA(class_local_friends_statement) = get_whole_abap_statement(
                                line_index       = class_definition_match->line
                                abap_source_code = class_abap_source_code ).

        CONSTANTS: BEGIN OF case,
                     insensitive TYPE abap_bool VALUE abap_false,
                     sensitive   TYPE abap_bool VALUE abap_true,
                   END OF case.

        REPLACE ALL OCCURRENCES OF |\r\n| IN class_local_friends_statement-whole_text WITH ` `.
        CONDENSE class_local_friends_statement-whole_text.

        " Get all the local friends, including the trailing dot.
        local_friends_including_dot = local_friends_including_dot
                           && substring_after( val  = class_local_friends_statement-whole_text
                                               sub  = `local friends `
                                               case = case-insensitive ).

        LOOP AT class_abap_source_code REFERENCE INTO line
            FROM class_local_friends_statement-first_line_index
            TO   class_local_friends_statement-last_line_index.
          line->* = '*' && line->*.
        ENDLOOP.

        DATA(local_friends) = split_at_regex( val   = to_upper( replace( val = local_friends_including_dot sub = '.' with = ` ` occ = 0 ) )
                                              regex = ` +` ).
        DELETE local_friends WHERE table_line = ``.
        LOOP AT local_friends REFERENCE INTO DATA(local_friend).
          IF NOT line_exists( deferred_classes[ table_line = local_friend->* ] ).
            INSERT local_friend->* INTO TABLE deferred_classes.
            INSERT |CLASS { local_friend->* } DEFINITION DEFERRED.| INTO TABLE deferred_lines.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      DATA(local_friends_excluding_dots) = replace( val  = local_friends_including_dot
                                                    sub  = `.`
                                                    with = ` `
                                                    occ  = 0 ).

      DATA(friends) = substring_after( val  = class_definition_statement-whole_text
                                       sub  = ` friends `
                                       case = case-insensitive ).

      line = REF #( class_abap_source_code[ class_definition_statement-last_line_index ] ).
      REPLACE '.' IN line->* WITH ''.

      DATA(abap_local_friends) = VALUE string_table(
            ( |{ COND #( WHEN NOT contains( val = class_definition_statement-whole_text sub = ` friends ` ) THEN `FRIENDS ` ) }| )
            ( LINES OF split_at_regex( val   = condense( local_friends_excluding_dots )
                                       regex = ` ` ) )
            ( `.` ) ).

      INSERT LINES OF abap_local_friends
            INTO class_abap_source_code
            INDEX class_definition_statement-last_line_index + 1.

      INSERT LINES OF deferred_lines INTO class_abap_source_code INDEX 1.

    ENDIF.

    "=====================================
    " Split the lines into DEFINITION and IMPLEMENTATION parts.
    "=====================================
    FIND FIRST OCCURRENCE OF
            REGEX '^endclass\..+'
            IN TABLE class_abap_source_code
            IGNORING CASE
            MATCH LINE DATA(endclass_line) ##REGEX_POSIX.
    IF sy-subrc <> 0.
      log = VALUE #( BASE log ( |ENDCLASS not found in class { class-name }.| ) ).
      RETURN.
    ENDIF.

    result = VALUE #(
            name           = class-name
            definition     = VALUE ty_abap_source_code(
                            FOR <abap_line> IN class_abap_source_code
                            FROM 1
                            TO endclass_line
                            ( <abap_line> ) )
            implementation = VALUE ty_abap_source_code(
                            ( |*>>>>>>> { class-name } <<<<<<<*| )
                            ( LINES OF VALUE #(
                            FOR <abap_line> IN class_abap_source_code
                            FROM endclass_line + 1
                            ( <abap_line> ) ) ) ) ).

  ENDMETHOD.
  METHOD get_abap_for_interface_pool.
    TYPES: BEGIN OF ty_find_replace_item,
             from TYPE string,
             to   TYPE string_table,
           END OF ty_find_replace_item.
    TYPES ty_find_replace_items TYPE STANDARD TABLE OF ty_find_replace_item WITH EMPTY KEY.
    TYPES ty_extension_codes TYPE STANDARD TABLE OF ty_extension_code WITH EMPTY KEY.
    TYPES ty_ref_includes TYPE STANDARD TABLE OF REF TO ty_oo_include WITH EMPTY KEY.

    DATA(interface_abap_source_code) = interface-includes[ extension_code = 'IP' ]-abap_source_code.

    DATA(ref_includes) = VALUE ty_ref_includes(
        FOR <extension_code> IN VALUE ty_extension_codes(
                ( 'IU' ) )
        LET ref_include = REF #( interface-includes[ extension_code = <extension_code> ] OPTIONAL )
        IN
        ( LINES OF COND #( WHEN ref_include IS BOUND THEN VALUE #( ( ref_include ) ) ) ) ).

    DATA(find_replace_items) = VALUE ty_find_replace_items(
            ( from = |interface-pool.|
              to   = VALUE #( ) )
            ( LINES OF VALUE #(
              FOR <ref_include> IN ref_includes
              ( from = |  INCLUDE { <ref_include>->include_name }.|
                to   = <ref_include>->abap_source_code ) ) ) ).

    LOOP AT find_replace_items REFERENCE INTO DATA(find_replace_item).
      FIND REGEX find_replace_item->from
            IN TABLE interface_abap_source_code
            IGNORING CASE
            MATCH LINE DATA(line_index).
      IF sy-subrc = 0.
        DELETE interface_abap_source_code INDEX line_index.
        INSERT LINES OF find_replace_item->to INTO interface_abap_source_code INDEX line_index.
      ELSE.
        log = VALUE #( BASE log ( |Line not found in class { interface-name }. Line: { find_replace_item->from }| ) ).
      ENDIF.
    ENDLOOP.

    DATA(regex) = `^\s*interface\s+`.
    FIND ALL OCCURRENCES OF
            REGEX regex
            IN TABLE interface_abap_source_code
            IGNORING CASE
            RESULTS DATA(matches) ##REGEX_POSIX.

    LOOP AT matches REFERENCE INTO DATA(match).

      DATA(interface_statement) = get_whole_abap_statement(
                              line_index       = match->line
                              abap_source_code = interface_abap_source_code ).
      LOOP AT interface_abap_source_code
          REFERENCE INTO DATA(abap_line)
          FROM interface_statement-first_line_index
          TO interface_statement-last_line_index.
        REPLACE REGEX `\<public\>` IN abap_line->* WITH `` IGNORING CASE.
      ENDLOOP.

    ENDLOOP..

    result = interface_abap_source_code.

  ENDMETHOD.
  METHOD get_whole_abap_statement.

    DATA(abap_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = abap_source_code
                                                              i_linenr  = line_index ).
    IF abap_statement-stokes IS INITIAL.
      result = VALUE #(
          first_line_index = line_index
          last_line_index  = line_index
          whole_text       = '' ).
    ELSE.
      result = VALUE #(
          LET tabix_last_stokes = lines( abap_statement-stokes )
              whole_text = concat_lines_of(
                            sep   = |\r\n|
                            table = VALUE string_table(
                                ( LINES OF abap_source_code
                                  FROM abap_statement-stokes[ 1 ]-row
                                  TO abap_statement-stokes[ tabix_last_stokes ]-row ) ) )
          IN
          first_line_index = abap_statement-stokes[ 1 ]-row
          last_line_index  = abap_statement-stokes[ tabix_last_stokes ]-row
          whole_text       = whole_text
*          concat_lines_of( sep = |\r\n| table = VALUE string_table(
*                              ( LINES OF abap_source_code FROM abap_statement-stokes[ 1 ]-row TO abap_statement-stokes[ lines( abap_statement-stokes ) ]-row ) ) )
*                              FOR <token> IN abap_statement-stokes
*                              ( <token>-str ) ) )
          offset           = abap_statement-stokes[ 1 ]-col
          length           = strlen( whole_text ) - abap_statement-stokes[ 1 ]-col ).
    ENDIF.

*    result-first_line_index = line_index.
*    result-last_line_index = line_index.
*    result-whole_text  = ``.
*    WHILE result-last_line_index <= lines( abap_source_code ).
*      result-whole_text = result-whole_text
*                        && abap_source_code[ result-last_line_index ]
*                        && ` `.
*      IF abap_source_code[ result-last_line_index ] CA '.'.
*        EXIT.
*      ENDIF.
*      result-last_line_index = result-last_line_index + 1.
*    ENDWHILE.

  ENDMETHOD.
  METHOD get_random_replacement_name.

    STATICS uuid_generator TYPE REF TO if_system_uuid.

    IF uuid_generator IS NOT BOUND.
      uuid_generator = cl_uuid_factory=>create_system_uuid( ).
    ENDIF.

    result = 'SHRI' && uuid_generator->create_uuid_c26( ).

  ENDMETHOD.
  METHOD on_program_generated.

    "=================================================
    " Recalculate WBCROSSGT
    "=================================================

*    LOOP AT main_programs REFERENCE INTO DATA(main_program).
    DATA(wb_crossreference) = NEW cl_wb_crossreference( p_name    = progname
                                                        p_include = '' ).
    wb_crossreference->index_actualize( IMPORTING p_error = DATA(error) ).
*    COMMIT WORK.

  ENDMETHOD.
  METHOD remove_test_classes.
    DATA class_2 TYPE REF TO ty_scanned_class.
    DATA: abap_line TYPE REF TO string,
          friend    TYPE REF TO SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_stokes.

    LOOP AT abap_source_code REFERENCE INTO DATA(class).

      DATA(classes) = VALUE ty_scanned_classes( ).

      get_class_endclass_positions( EXPORTING itab = class->definition
                                              cu_co_ci = abap_true
                                    CHANGING  classes = classes ).

      get_class_endclass_positions( EXPORTING itab = class->implementation
                                              cu_co_ci = abap_false
                                    CHANGING  classes = classes ).

      "====================================
      " get the names of test classes
      "====================================
      TYPES ty_class_names TYPE HASHED TABLE OF seoclsname WITH UNIQUE KEY table_line.
      DATA(test_class_names) = VALUE ty_class_names( ).

*      " Don't remove now the lines CLASS ... DEFINITION DEFERRED because they may
      " refer to test classes although there is no FOR TESTING. If it's a test
      " class, the DEFERRED line will have to be removed too.
      " Same remark for LOCAL FRIENDS.
      LOOP AT classes REFERENCE INTO class_2
            WHERE is_test_class = abap_true.
        INSERT class_2->name INTO TABLE test_class_names.
      ENDLOOP.

      "====================================
      " keep only classes to delete (not test classes)
      "====================================
      DATA(test_classes) = classes.
      LOOP AT test_classes REFERENCE INTO DATA(test_class).
        IF NOT line_exists( test_class_names[ table_line = test_class->name ] ).
          DELETE test_classes
                WHERE name          = test_class->name
                  AND is_definition = test_class->is_definition
                  AND is_deferred   = test_class->is_deferred
                  AND local_friends = test_class->local_friends.
        ENDIF.
      ENDLOOP.

*      " Remove implementation lines without definition (deleted above)
*      LOOP AT test_classes REFERENCE INTO class_2
*          WHERE is_definition = abap_false.
*        IF NOT line_exists( test_classes[ name          = class_2->name
*                                          is_definition = abap_true ] ).
*          DELETE test_classes
*                WHERE name          = class_2->name
*                  AND is_definition = class_2->is_definition.
*        ENDIF.
*      ENDLOOP.

      "====================================
      " Replace test classes from local friends (CLASS ... DEFINITION ... LOCAL FRIENDS lcl_1 ... ltc_1 ltc_2 ...)
      "   or friends (CLASS ... DEFINITION ... FRIENDS lcl_1 ... ltc_1 ltc_2 ...)
      "   with spaces.
      " Remove whole class definition if all friends after "LOCAL FRIENDS" have been removed (case it concerns class pool, the main class pool definition remains intact).
      " Remove only "FRIENDS" if all friends have been removed (case class is local class).
      "====================================
      LOOP AT classes REFERENCE INTO class_2
          WHERE any_friends = abap_true.

        TYPES ty_ref_abap_source_code TYPE REF TO ty_abap_source_code.
        DATA(ref_abap_source_code) = COND ty_ref_abap_source_code(
                                        WHEN class_2->cu_co_ci = abap_true THEN
                                            REF #( class->definition )
                                        ELSE
                                            REF #( class->implementation ) ).

        DATA(friends_to_remove) = VALUE SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_ut_stokes( ).
        LOOP AT class_2->statement_tokens REFERENCE INTO friend
            FROM class_2->tabix_first_friend.
          IF line_exists( test_classes[ name = friend->str ] ).
            INSERT friend->* INTO TABLE friends_to_remove.
          ENDIF.
        ENDLOOP.

        IF friends_to_remove IS NOT INITIAL.
          DATA(total_number_friends) = lines( class_2->statement_tokens ) - class_2->tabix_first_friend + 1.
          IF lines( friends_to_remove ) = total_number_friends.
            "===================
            " Remove all friend test classes
            "===================
            IF class_2->statement_tokens[ class_2->tabix_local_or_friends ]-str = 'LOCAL'.
              " 1) If statement is CLASS ... LOCAL FRIENDS then comment out whole CLASS ... DEFINITION LOCAL FRIENDS...
              LOOP AT ref_abap_source_code->* REFERENCE INTO abap_line
                  FROM class_2->statement_tokens[ class_2->tabix_first_friend ]-row
                  TO class_2->statement_tokens[ lines( class_2->statement_tokens ) ]-row.
                abap_line->* = '*' && abap_line->*.
              ENDLOOP.
            ELSE.
              " 2) Else it's CLASS ... FRIENDS ... just remove FRIENDS and all classes after FRIENDS.

              " Remove "FRIENDS"
              INSERT class_2->statement_tokens[ class_2->tabix_local_or_friends ]
                    INTO TABLE friends_to_remove.

              " What is import is COL DESCENDING in case two tokens are on same line, first remove the rightmost word
              " (ROW could have been DESCENDING instead of ASCENDING)
              SORT friends_to_remove BY row ASCENDING col DESCENDING.

              " Remove test classes
              LOOP AT friends_to_remove REFERENCE INTO friend.
                abap_line = REF #( ref_abap_source_code->*[ friend->row ] OPTIONAL ).
                CHECK abap_line IS BOUND.
                REPLACE SECTION
                        OFFSET friend->col
                        LENGTH strlen( friend->str )
                        OF abap_line->*
                        WITH space.
              ENDLOOP.
            ENDIF.

          ELSE.
            "===================
            " Remove not all friends, remove only test classes
            "===================

            " What is import is COL DESCENDING in case two tokens are on same line, first remove the rightmost word
            " (ROW could have been DESCENDING instead of ASCENDING)
            SORT friends_to_remove BY row ASCENDING col DESCENDING.

            LOOP AT friends_to_remove REFERENCE INTO friend.
              abap_line = REF #( ref_abap_source_code->*[ friend->row ] OPTIONAL ).
              CHECK abap_line IS BOUND.
              REPLACE SECTION
                      OFFSET friend->col
                      LENGTH strlen( friend->str )
                      OF abap_line->*
                      WITH space.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "====================================
      " delete ABAP lines corresponding to test classes
      " (definition deferred, definition, implementation)
      "====================================

      SORT test_classes BY cu_co_ci DESCENDING start_line DESCENDING.
      LOOP AT test_classes REFERENCE INTO class_2.

        IF class_2->cu_co_ci = abap_true.
          DELETE class->definition
                FROM class_2->start_line
                TO   class_2->end_line.
        ELSE.
          DELETE class->implementation
                FROM class_2->start_line
                TO   class_2->end_line.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
  METHOD get_class_endclass_positions.
    DATA current_class TYPE ty_scanned_class.

    FIND ALL OCCURRENCES OF
        REGEX 'definition|implementation|endclass'
        IN TABLE itab
        IGNORING CASE
        RESULTS DATA(matches).

    LOOP AT matches REFERENCE INTO DATA(match).
      " skip lines corresponding to comments (line which starts with an asterisk)
      CHECK itab[ match->line ] NP '#**'.

      DATA(abap_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = itab
                                                                i_linenr  = match->line ).
      CHECK lines( abap_statement-stokes ) > 0.

      CASE abap_statement-stokes[ 1 ]-str.
        WHEN 'CLASS'.
          current_class = VALUE #( LET aux_tabix_friends          = line_index( abap_statement-stokes[ str = 'FRIENDS' ] )
                                       aux_local_global_friends   = COND #( LET word_before_friends = COND #(
                                                                                    WHEN aux_tabix_friends > 1
                                                                                    THEN abap_statement-stokes[ aux_tabix_friends - 1 ]-str )
                                                                            IN
                                                                            WHEN SWITCH #( word_before_friends WHEN 'LOCAL' OR 'GLOBAL' THEN abap_true ) = abap_true
                                                                            THEN word_before_friends )
                                       aux_tabix_local_or_friends = COND i( WHEN aux_tabix_friends > 0
                                                                            AND abap_statement-stokes[ aux_tabix_friends - 1 ]-str = 'LOCAL'
                                                                            THEN
                                                                            aux_tabix_friends - 1
                                                                            ELSE
                                                                            aux_tabix_friends )
                                   IN
                                   name                   = abap_statement-stokes[ 2 ]-str
                                   cu_co_ci               = cu_co_ci
                                   is_definition          = xsdbool( abap_statement-stokes[ 3 ]-str = 'DEFINITION' )
                                   is_test_class          = xsdbool( line_index( abap_statement-stokes[ str = 'FOR' ] )
                                                                    = line_index( abap_statement-stokes[ str = 'TESTING' ] ) - 1 )
                                   is_deferred            = xsdbool( line_exists( abap_statement-stokes[ str = 'DEFERRED' ] ) )
                                   statement_tokens       = abap_statement-stokes
                                   any_friends            = xsdbool( aux_tabix_friends > 0 )
                                   local_global_friends   = aux_local_global_friends
                                   local_friends          = xsdbool( aux_local_global_friends = 'LOCAL' )
                                   tabix_local_or_friends = aux_tabix_local_or_friends
                                   tabix_first_friend     = COND #( WHEN aux_tabix_friends > 0 THEN aux_tabix_friends + 1 )
                                   start_line             = abap_statement-stokes[ 1 ]-row ).
          IF current_class-is_deferred = abap_true
                OR current_class-local_friends = abap_true.
            current_class-end_line = abap_statement-stokes[ lines( abap_statement-stokes ) ]-row.
            INSERT current_class INTO TABLE classes.
            current_class = VALUE #( ).
          ENDIF.
        WHEN 'ENDCLASS'.
          IF current_class IS NOT INITIAL.
            current_class-end_line = abap_statement-stokes[ 1 ]-row.
            INSERT current_class INTO TABLE classes.
            current_class = VALUE #( ).
          ENDIF.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_abap_for_program.

    TRY.

        me->program_name = program_name.
        me->replacements = global_replacements.

        DATA(miscellaneous) = VALUE ty_miscellaneous( ).

        SELECT pgmid, object, obj_name
            FROM tadir
            WHERE pgmid    = 'R3TR'
              AND object   = 'PROG'
              AND obj_name = @program_name
            INTO TABLE @miscellaneous-all_objects.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = program_name.
        ENDIF.

        DATA(source_units) = VALUE Lif_shrinker_abap_code_adapter=>ty_source_units( ).

        INSERT VALUE #(
                name = program_name )
            INTO TABLE source_units
            REFERENCE INTO DATA(source_unit).

        READ REPORT program_name INTO source_unit->abap_source_code.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = program_name.
        ENDIF.

        FIND ALL OCCURRENCES
            OF 'INCLUDE'
            IN TABLE result
            RESULTS DATA(matches).

        TYPES:
          BEGIN OF ty_include_statement,
            row    TYPE i,
            stokes TYPE SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>ty_ut_stokes,
          END OF ty_include_statement.
        TYPES ty_include_statements TYPE STANDARD TABLE OF ty_include_statement WITH EMPTY KEY.
        TYPES:
          BEGIN OF ty_include,
            name     TYPE syrepid,
            if_found TYPE abap_bool,
*            abap_source_code TYPE ty_abap_source_code,
          END OF ty_include.
        TYPES ty_includes TYPE STANDARD TABLE OF ty_include WITH EMPTY KEY.
        DATA(include_statements) = VALUE ty_include_statements( ).
        DATA(includes) = VALUE ty_includes( ).
        LOOP AT matches REFERENCE INTO DATA(match).
          DATA(include_statement) = SHRIS5ZPAUXVKEPN5HWER7BEX2VBRY=>get( it_source = result
                                                                       i_linenr  = match->line
                                                                       i_offset  = match->offset ).
          IF include_statement-stokes IS NOT INITIAL
                AND include_statement-stokes[ 1 ]-str = 'INCLUDE'.
            INSERT VALUE ty_include(
                    name     = include_statement-stokes[ 2 ]-str
                    if_found = xsdbool( lines( include_statement-stokes ) >= 4
                                    AND include_statement-stokes[ 3 ]-str = 'IF'
                                    AND include_statement-stokes[ 4 ]-str = 'FOUND' )
                ) INTO TABLE includes.
            INSERT VALUE #(
                    row    = include_statement-stokes[ 1 ]-row
                    stokes = include_statement-stokes
                ) INTO TABLE include_statements.
          ENDIF.
        ENDLOOP.

        LOOP AT includes REFERENCE INTO DATA(include).

          INSERT VALUE #(
                  name = include->name )
              INTO TABLE source_units
              REFERENCE INTO source_unit.

          READ REPORT include->name INTO source_unit->abap_source_code.
          IF sy-subrc <> 0
              AND include->if_found = abap_false.
            RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING text = 'Program &1 does not exist'(002) msgv1 = program_name.
          ENDIF.

        ENDLOOP.

        IF customizer IS BOUND.
          customizer->adapt_source_code( CHANGING other_source_units = source_units ).
        ENDIF.

        result = source_units[ name = program_name ]-abap_source_code.

        SORT include_statements BY row DESCENDING.
        LOOP AT include_statements REFERENCE INTO DATA(include_statement_2).
          DATA(line_index) = include_statement_2->stokes[ 1 ]-row.
          source_unit = REF #( source_units[ name = include_statement_2->stokes[ 2 ]-str ] OPTIONAL ).
          IF include IS NOT BOUND.
            result[ line_index ] = '*' && result[ line_index ].
          ELSE.
            DATA(replaced_abap_line) = result[ line_index ].
            DELETE result INDEX line_index.
            INSERT LINES OF source_unit->abap_source_code INTO result INDEX line_index.
            INSERT |*{ replaced_abap_line }| INTO result INDEX line_index.
          ENDIF.
        ENDLOOP.

        replace_texts( EXPORTING replacements     = replacements
                       CHANGING  abap_source_code = result ).

      CATCH Lcx_shrinker INTO DATA(error_2).
        RAISE EXCEPTION error_2.
      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE Lcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_SHRINKER implementation

*>>>>>>> ZCX_SHRINKER <<<<<<<*

*"* macro definitions
*include zcx_shrinker==================ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_shrinker==================ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcx_shrinker==================ccau.
*"* use this source file for your ABAP unit test classes


class LCX_SHRINKER implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->text = text.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    IF me->text IS NOT INITIAL.
      IF substitute_placeholders = abap_true.
        me->text = replace( val = me->text sub = '&1' with = msgv1 ).
        me->text = replace( val = me->text sub = '&2' with = msgv2 ).
        me->text = replace( val = me->text sub = '&3' with = msgv3 ).
        me->text = replace( val = me->text sub = '&4' with = msgv4 ).
      ENDIF.
    ELSE.
      me->text = 'General error, please contact the support.'(001).
    ENDIF.
  ENDMETHOD.
  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.
  METHOD get_text.
    result = text.
  ENDMETHOD.
endclass. "ZCX_SHRINKER implementation

