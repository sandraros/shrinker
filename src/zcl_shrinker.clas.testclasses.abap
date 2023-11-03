*"* use this source file for your ABAP unit test classes

CLASS ltc_replace_texts DEFINITION DEFERRED.

CLASS zcl_shrinker DEFINITION LOCAL FRIENDS
    ltc_replace_texts.

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

CLASS ltc_get_whole_abap_statement DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS several_lines FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_raw_scan_lines_around DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "! If the cursor is before the colon, all the lines of the chained statement should be returned.
    METHODS c_w_lon_lf_w_ma_lf_w_dot FOR TESTING RAISING cx_static_check.
    METHODS lcmt_lf_w_dot FOR TESTING RAISING cx_static_check.
    "! A = |{ 1 }{ 1 }| on 3 lines, with }{ as the middle line.
    METHODS w_eq_pip_cbo_w_lf_cbc_cbo_lf FOR TESTING RAISING cx_static_check.
    "! If the cursor is after the colon, only the lines till the next comma or dot after the cursor should be returned.
    METHODS w_lon_lf_c_w_ma_lf_w_dot FOR TESTING RAISING cx_static_check.


    TYPES ty_raw_scan_lines_around  TYPE lcl_abap_statement_at_cursor=>ty_raw_scan_lines_around.
    CONSTANTS type LIKE lcl_abap_statement_at_cursor=>type VALUE lcl_abap_statement_at_cursor=>type.
    DATA scan_result TYPE ty_raw_scan_lines_around .

ENDCLASS.


CLASS ltc_regex DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    DATA: regex TYPE string.
    METHODS test FOR TESTING RAISING cx_static_check.
*    class-methods class_setup.
*    class-methods class_teardown.
    METHODS setup.
*    methods teardown.
ENDCLASS.


CLASS ltc_replace_texts DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS member_interface_prefix FOR TESTING RAISING cx_static_check.
    METHODS start_of_abap_word FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_scan_abap_parse_line DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS cbc_cbo FOR TESTING RAISING cx_static_check.
    METHODS dynamic_method_call FOR TESTING RAISING cx_static_check.
    METHODS ecmt FOR TESTING RAISING cx_static_check.
    METHODS ecmt_w FOR TESTING RAISING cx_static_check.
    METHODS interface_prefixed_member_name FOR TESTING RAISING cx_static_check.
    "! Check col is not zero
    METHODS sp_sq_hello_sq FOR TESTING RAISING cx_static_check.
    METHODS w_cbc_pip_dot FOR TESTING RAISING cx_static_check.
    METHODS w_dot FOR TESTING RAISING cx_static_check.
    METHODS w_dot_w FOR TESTING RAISING cx_static_check.
    METHODS w_dot_w_dot FOR TESTING RAISING cx_static_check.
    METHODS w_lon FOR TESTING RAISING cx_static_check.
    METHODS w_lon_w_ma_w_dot FOR TESTING RAISING cx_static_check.
    METHODS w_ma_lf_w FOR TESTING RAISING cx_static_check.
    METHODS a FOR TESTING RAISING cx_static_check.

    TYPES ty_line_scan TYPE lcl_abap_statement_at_cursor=>ty_line_scan.
    CONSTANTS type LIKE lcl_abap_statement_at_cursor=>type VALUE lcl_abap_statement_at_cursor=>type.
    DATA scan_result TYPE ty_line_scan.

ENDCLASS.


CLASS ltc_scan_abap_statement DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    "! If the cursor is before the colon, all the lines of the chained statement should be returned.
    METHODS c_w_lon_lf_w_ma_lf_w_dot FOR TESTING RAISING cx_static_check.
    "! Make sure a previous statement is completely ignored
    METHODS dot_lf_c_w FOR TESTING RAISING cx_static_check.
*    METHODS ecmt_lf_w_dot FOR TESTING RAISING cx_static_check.
    METHODS w_dot_c_w_dot_w_dot FOR TESTING RAISING cx_static_check.
    METHODS w_dot_w_lf_w_lf_w_dot_w_dot FOR TESTING RAISING cx_static_check.
    "! If the cursor is after the colon, only the lines till the next comma or dot after the cursor should be returned.
    METHODS w_lon_lf_w_ma_lf_c_w_dot FOR TESTING RAISING cx_static_check.

    TYPES ty_scan_result TYPE lcl_abap_statement_at_cursor=>ty_scan_result.
    CONSTANTS type LIKE lcl_abap_statement_at_cursor=>type VALUE lcl_abap_statement_at_cursor=>type.
    DATA scan_result TYPE ty_scan_result.

ENDCLASS.


CLASS ltc_standard_scan_abap_source DEFINITION
    FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS dynamic_method_call FOR TESTING RAISING cx_static_check.
    METHODS interface_prefixed_member_name FOR TESTING RAISING cx_static_check.
    METHODS select_where_in_list FOR TESTING RAISING cx_static_check.
    METHODS single_end_comment FOR TESTING RAISING cx_static_check.
    METHODS string_template FOR TESTING RAISING cx_static_check.
    METHODS a FOR TESTING RAISING cx_static_check.

    TYPES ty_statements TYPE STANDARD TABLE OF sstmnt WITH EMPTY KEY.
    TYPES ty_tokens TYPE STANDARD TABLE OF stokes WITH EMPTY KEY.

    DATA abap TYPE string_table.
    DATA statements TYPE ty_statements.
    DATA tokens TYPE ty_tokens.

ENDCLASS.


CLASS ltc_get_whole_abap_statement IMPLEMENTATION.

  METHOD several_lines.

    DATA(aaa) = zcl_shrinker=>get_whole_abap_statement(
                line_index       = 1
                abap_source_code = VALUE #(
                            ( `CLASS zcl DEFINITION` )
                            ( `PUBLIC` )
                            ( `CREATE PUBLIC.` ) ) ).
    cl_abap_unit_assert=>assert_equals(
        act = aaa
        exp = VALUE zcl_shrinker=>ty_get_next_lines_of_statement(
            first_line_index = 1
            last_line_index  = 3
            offset           = 0
            length           = 44
            whole_text       = |CLASS zcl DEFINITION\r\nPUBLIC\r\nCREATE PUBLIC.| ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_raw_scan_lines_around IMPLEMENTATION.

  METHOD c_w_lon_lf_w_ma_lf_w_dot.

    lcl_abap_statement_at_cursor=>raw_scan_lines_around(
        EXPORTING
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A:` )
                        ( `B,` )
                        ( `C.` ) )
            i_linenr  = 1
        IMPORTING
            result    = scan_result ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = CORRESPONDING ty_raw_scan_lines_around( VALUE ty_raw_scan_lines_around(
            pseudo_tokens = VALUE #(
                ( str = 'A' row = 1 col = 0 type = type-pseudo_token-std-identifier )
                ( str = ':' row = 1 col = 1 type = type-pseudo_token-colon )
                ( str = 'B' row = 2 col = 0 type = type-pseudo_token-std-identifier )
                ( str = ',' row = 2 col = 1 type = type-pseudo_token-comma )
                ( str = 'C' row = 3 col = 0 type = type-pseudo_token-std-identifier )
                ( str = '.' row = 3 col = 1 type = type-pseudo_token-dot ) ) ) ) ).

  ENDMETHOD.


  METHOD lcmt_lf_w_dot.

    lcl_abap_statement_at_cursor=>raw_scan_lines_around(
        EXPORTING
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `*` )
                        ( `A.` ) )
            i_linenr  = 2
        IMPORTING
            result    = scan_result ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = CORRESPONDING ty_raw_scan_lines_around( VALUE ty_raw_scan_lines_around(
            pseudo_tokens = VALUE #(
                ( str = 'A' row = 2 col = 0 type = type-pseudo_token-std-identifier )
                ( str = '.' row = 2 col = 1 type = type-pseudo_token-dot ) ) ) ) ).

  ENDMETHOD.


  METHOD w_eq_pip_cbo_w_lf_cbc_cbo_lf.

    DATA(a) = |{ 1
               }{      " <==== this is the parsed line
               1 }|.

    lcl_abap_statement_at_cursor=>raw_scan_lines_around(
        EXPORTING
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A = |{ 1` )
                        ( `}{` )
                        ( `1 }|.` ) )
            i_linenr  = 2
        IMPORTING
            result    = scan_result ).

    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_raw_scan_lines_around(
            pseudo_tokens = VALUE #(
                ( str = 'A' row = 1 col = 0 type = type-pseudo_token-std-identifier )
                ( str = '=' row = 1 col = 2 type = type-pseudo_token-std-identifier )
                ( str = '|' row = 1 col = 4 type = type-pseudo_token-std-identifier )
                ( str = '{' row = 1 col = 5 type = type-pseudo_token-std-identifier )
                ( str = '1' row = 1 col = 7 type = type-pseudo_token-std-identifier )
                ( str = '}' row = 2 col = 0 type = type-pseudo_token-std-identifier )
                ( str = '{' row = 2 col = 1 type = type-pseudo_token-std-identifier )
                ( str = '1' row = 3 col = 0 type = type-pseudo_token-std-identifier )
                ( str = '}' row = 3 col = 2 type = type-pseudo_token-std-identifier )
                ( str = '|' row = 3 col = 3 type = type-pseudo_token-std-identifier )
                ( str = '.' row = 3 col = 4 type = type-pseudo_token-dot ) ) ) ).

  ENDMETHOD.


  METHOD w_lon_lf_c_w_ma_lf_w_dot.

    lcl_abap_statement_at_cursor=>raw_scan_lines_around(
        EXPORTING
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A:` )
                        ( `B,` )
                        ( `C.` ) )
            i_linenr  = 2
        IMPORTING
            result    = scan_result ).

    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_raw_scan_lines_around(
            pseudo_tokens = VALUE #(
                ( str = 'A' row = 1 col = 0 type = type-pseudo_token-std-identifier )
                ( str = ':' row = 1 col = 1 type = type-pseudo_token-colon )
                ( str = 'B' row = 2 col = 0 type = type-pseudo_token-std-identifier )
                ( str = ',' row = 2 col = 1 type = type-pseudo_token-comma ) ) ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_regex IMPLEMENTATION.

  METHOD setup.
    regex = `(['``])((?=[^\\1]|\1\1).*)\1|[^\(,\)'``]+|\(|,|\)|.`.
  ENDMETHOD.


  METHOD test.
    FIND ALL OCCURRENCES OF
        REGEX regex ##regex_posix
        IN `(zif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION')`
*                IN `(A)=>('B')`
        RESULTS DATA(matches).
    " cl_abap_unit_assert=>assert_equals( ACT = ? EXP = ? MSG = ? ).
  ENDMETHOD.
ENDCLASS.


CLASS ltc_replace_texts IMPLEMENTATION.

  METHOD member_interface_prefix.
    DATA(abap_source_code) = VALUE zcl_shrinker=>ty_abap_source_code(
            ( ` lo_cut ?= lo_cut->zif_abapgit_ajson~slice( '/issues' ).` ) ).
    DATA(cut) = zcl_shrinker=>create( ).
    cut->replace_texts(
        EXPORTING
            replacements     = VALUE #( ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' member_interface_prefix = abap_true ) )
        CHANGING
            abap_source_code = abap_source_code ).
    cl_abap_unit_assert=>assert_equals(
        act = abap_source_code
        exp = VALUE zcl_shrinker=>ty_abap_source_code(
            ( ` lo_cut ?= lo_cut->Lif_abapgit_ajson~slice( '/issues' ).` ) ) ).
  ENDMETHOD.


  METHOD start_of_abap_word.
    DATA(abap_source_code) = VALUE zcl_shrinker=>ty_abap_source_code(
            ( `zif_abapgit_ajson=>member.` ) ).
    DATA(cut) = zcl_shrinker=>create( ).
    cut->replace_texts(
        EXPORTING
            replacements     = VALUE #( ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' start_of_abap_word = abap_true ) )
        CHANGING
            abap_source_code = abap_source_code ).
    cl_abap_unit_assert=>assert_equals(
        act = abap_source_code
        exp = VALUE zcl_shrinker=>ty_abap_source_code(
            ( `Lif_abapgit_ajson=>member.` ) ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_scan_abap_parse_line IMPLEMENTATION.

  METHOD a.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `ASSIGN (zif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION') TO <lv_interface_vers>.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
            ( str = `ASSIGN`                                                                            col = 0   type = type-pseudo_token-std-identifier )
            ( str = `(ZIF_ABAPGIT_APACK_DEFINITIONS=>C_APACK_INTERFACE_CUST)=>('CO_INTERFACE_VERSION')` col = 7   type = type-pseudo_token-std-list )
            ( str = `TO`                                                                                col = 89  type = type-pseudo_token-std-identifier )
            ( str = `<LV_INTERFACE_VERS>`                                                               col = 92  type = type-pseudo_token-std-identifier )
            ( str = `.`                                                                                 col = 111 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD cbc_cbo.
    DATA(a) = |{ 1
               }{      " <==== this is the parsed line
               1 }|.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `}{` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = '}' col = 0 type = type-pseudo_token-std-identifier )
                ( str = '{' col = 1 type = type-pseudo_token-std-identifier ) ) ) ).
  ENDMETHOD.


  METHOD dynamic_method_call.
    IF 0 = 1.
      CALL METHOD ('anyclass')=>anymethod.
    ENDIF.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `Call Method ('anyclass')=>anymethod.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'CALL'                    col =  0 type = type-pseudo_token-std-identifier )
                ( str = 'METHOD'                  col =  5 type = type-pseudo_token-std-identifier )
                ( str = `('anyclass')=>ANYMETHOD` col = 12 type = type-pseudo_token-std-list )
                ( str = '.'                       col = 35 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD ecmt.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `"` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan( ) ).
  ENDMETHOD.


  METHOD ecmt_w.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `"X` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'X' col = 0 type = type-pseudo_token-std-comment ) ) ) ).
  ENDMETHOD.


  METHOD interface_prefixed_member_name.

    scan_result = lcl_abap_statement_at_cursor=>parse_line( `lo_ajson->zif_abapgit_ajson~to_abap( ).` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = `LO_AJSON->ZIF_ABAPGIT_AJSON~TO_ABAP(` col =  0 type = type-pseudo_token-std-identifier )
                ( str = `)`                                    col = 37 type = type-pseudo_token-std-identifier )
                ( str = `.`                                    col = 38 type = type-pseudo_token-dot ) ) ) ).

  ENDMETHOD.


  METHOD sp_sq_hello_sq.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( ` 'hello'` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = `'hello'` col = 1 type = type-pseudo_token-std-literal ) ) ) ).
  ENDMETHOD.


  METHOD w_cbc_pip_dot.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `1 }|.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = '1' col = 0 type = type-pseudo_token-std-identifier )
                ( str = '}' col = 2 type = type-pseudo_token-std-identifier )
                ( str = '|' col = 3 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 4 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD w_dot.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 1 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD w_dot_w.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A.B` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 1 type = type-pseudo_token-dot )
                ( str = 'B' col = 2 type = type-pseudo_token-std-identifier ) ) ) ).
  ENDMETHOD.


  METHOD w_dot_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A.B.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 1 type = type-pseudo_token-dot )
                ( str = 'B' col = 2 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 3 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD w_lon.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A:` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = ':' col = 1 type = type-pseudo_token-colon ) ) ) ).
  ENDMETHOD.


  METHOD w_lon_w_ma_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A:B,C.` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = ':' col = 1 type = type-pseudo_token-colon )
                ( str = 'B' col = 2 type = type-pseudo_token-std-identifier )
                ( str = ',' col = 3 type = type-pseudo_token-comma )
                ( str = 'C' col = 4 type = type-pseudo_token-std-identifier )
                ( str = '.' col = 5 type = type-pseudo_token-dot ) ) ) ).
  ENDMETHOD.


  METHOD w_ma_lf_w.
    scan_result = lcl_abap_statement_at_cursor=>parse_line( `A,B` ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_line_scan(
            pseudo_tokens = VALUE #(
                ( str = 'A' col = 0 type = type-pseudo_token-std-identifier )
                ( str = ',' col = 1 type = type-pseudo_token-comma )
                ( str = 'B' col = 2 type = type-pseudo_token-std-identifier ) ) ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_scan_abap_statement IMPLEMENTATION.

  METHOD c_w_lon_lf_w_ma_lf_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>get(
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A:` )
                        ( `B,` )
                        ( `C.` ) )
            i_linenr  = 1 ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_scan_result(
            sstmnt = VALUE #(
                ( number = 1 type = type-stmnt-standard from = 1 to = 2 colonrow = 1 trow = 2 coloncol = 1 tcol = 1 prefixlen = 1 terminator = ',' )
                ( number = 2 type = type-stmnt-standard from = 3 to = 4 colonrow = 1 trow = 3 coloncol = 1 tcol = 1 prefixlen = 1 terminator = '.' ) )
            stokes = VALUE #(
                ( str = 'A' row = 1 col = 0 type = type-token-identifier )
                ( str = 'B' row = 2 col = 0 type = type-token-identifier )
                ( str = 'A' row = 1 col = 0 type = type-token-identifier )
                ( str = 'C' row = 3 col = 0 type = type-token-identifier ) ) ) ).
  ENDMETHOD.


  METHOD dot_lf_c_w.
    scan_result = lcl_abap_statement_at_cursor=>get(
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `.` )
                        ( `A` ) )
            i_linenr  = 2 ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_scan_result(
            sstmnt = VALUE #( )
            stokes = VALUE #(
                ( str = 'A' row = 2 col = 0 type = type-token-identifier ) ) ) ).
  ENDMETHOD.


  METHOD w_dot_c_w_dot_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>get(
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A.B.C.` ) )
            i_linenr  = 1
            i_offset  = 2 ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_scan_result(
            sstmnt = VALUE #(
                ( number = 1 type = type-stmnt-standard from = 1 to = 1 trow = 1 tcol = 3 terminator = '.' ) )
            stokes = VALUE #(
                ( str = 'B' row = 1 col = 2 type = type-token-identifier ) ) ) ).
  ENDMETHOD.


  METHOD w_dot_w_lf_w_lf_w_dot_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>get(
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A.B` )
                        ( `C` )
                        ( `D.E.` ) )
            i_linenr  = 2 ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_scan_result(
            sstmnt = VALUE #(
                ( number = 1 type = type-stmnt-standard from = 1 to = 3 trow = 3 tcol = 1 terminator = '.' ) )
            stokes = VALUE #(
                ( str = 'B' row = 1 col = 2 type = type-token-identifier )
                ( str = 'C' row = 2 col = 0 type = type-token-identifier )
                ( str = 'D' row = 3 col = 0 type = type-token-identifier ) ) ) ).
  ENDMETHOD.


  METHOD w_lon_lf_w_ma_lf_c_w_dot.
    scan_result = lcl_abap_statement_at_cursor=>get(
            it_source = VALUE zcl_shrinker=>ty_abap_source_code(
                        ( `A:` )
                        ( `B,` )
                        ( `C.` ) )
            i_linenr  = 3 ).
    cl_abap_unit_assert=>assert_equals(
        act = scan_result
        exp = VALUE ty_scan_result(
            sstmnt = VALUE #(
                ( number = 1 type = type-stmnt-standard from = 1 to = 2 colonrow = 1 trow = 3 coloncol = 1 tcol = 1 prefixlen = 1 terminator = '.' ) )
            stokes = VALUE #(
                ( str = 'A' row = 1 col = 0 type = type-token-identifier )
                ( str = 'C' row = 3 col = 0 type = type-token-identifier ) ) ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_standard_scan_abap_source IMPLEMENTATION.

  METHOD a.

    abap = VALUE #( ( `ASSIGN (zif_abapgit_apack_definitions=>c_apack_interface_cust)=>('CO_INTERFACE_VERSION') TO <lv_interface_vers>.` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens(
            ( str = `ASSIGN`                                                                            row = 1 col = 0  type = 'I' )
            ( str = `(ZIF_ABAPGIT_APACK_DEFINITIONS=>C_APACK_INTERFACE_CUST)=>('CO_INTERFACE_VERSION')` row = 1 col = 7  type = 'L' )
            ( str = `TO`                                                                                row = 1 col = 89 type = 'I' )
            ( str = `<LV_INTERFACE_VERS>`                                                               row = 1 col = 92 type = 'I' ) ) ).

  ENDMETHOD.


  METHOD dynamic_method_call.

    abap = VALUE #( ( `        CALL METHOD (ls_per-method)=>zif_abapgit_background~get_settings` )
                    ( `          CHANGING                                                      ` )
                    ( `            ct_settings = lt_settings.                                  ` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens(
            ( str = 'CALL'                                                 row = 1 col = 8  type = 'I' )
            ( str = 'METHOD'                                               row = 1 col = 13 type = 'I' )
            ( str = `(LS_PER-METHOD)=>ZIF_ABAPGIT_BACKGROUND~GET_SETTINGS` row = 1 col = 20 type = 'L' )
            ( str = 'CHANGING'                                             row = 2 col = 10 type = 'I' )
            ( str = 'CT_SETTINGS'                                          row = 3 col = 12 type = 'I' )
            ( str = '='                                                    row = 3 col = 24 type = 'I' )
            ( str = 'LT_SETTINGS'                                          row = 3 col = 26 type = 'I' ) ) ).

  ENDMETHOD.


  METHOD interface_prefixed_member_name.

    abap = VALUE #( ( `lo_ajson->zif_abapgit_ajson~to_abap( ).` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens(
            ( str = `LO_AJSON->ZIF_ABAPGIT_AJSON~TO_ABAP(` row = 1 col =  0 type = 'I' )
            ( str = `)`                                    row = 1 col = 37 type = 'I' ) ) ).

  ENDMETHOD.


  METHOD select_where_in_list.

    abap = VALUE #( ( `SELECT *` )
                    ( `FROM TABLE` )
                    ( `WHERE col IN (A,'1').` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens(
            ( str = `SELECT` row = 1 col =  0 type = 'I' )
            ( str = `*`      row = 1 col =  7 type = 'I' )
            ( str = `FROM`   row = 2 col =  0 type = 'I' )
            ( str = `TABLE`  row = 2 col =  5 type = 'I' )
            ( str = `WHERE`  row = 3 col =  0 type = 'I' )
            ( str = `COL`    row = 3 col =  6 type = 'I' )
            ( str = `IN`     row = 3 col = 10 type = 'I' )
            ( str = `(`      row = 3 col = 13 type = 'I' )
            ( str = `A`      row = 3 col = 14 type = 'I' )
            ( str = `,`      row = 3 col = 15 type = 'I' )
            ( str = `'1'`    row = 3 col = 16 type = 'S' )
            ( str = `)`      row = 3 col = 19 type = 'I' ) ) ).

  ENDMETHOD.


  METHOD single_end_comment.

    abap = VALUE #( ( `"` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens( ) ).

  ENDMETHOD.


  METHOD string_template.

    abap = VALUE #( ( `a = |{ 1 }{ 1 }|.` ) ).
    SCAN ABAP-SOURCE abap STATEMENTS INTO statements TOKENS INTO tokens.
    cl_abap_unit_assert=>assert_equals(
        act = tokens
        exp = VALUE ty_tokens(
            ( str = 'A' row = 1 col = 0  type = 'I' )
            ( str = '=' row = 1 col = 2  type = 'I' )
            ( str = '|' row = 1 col = 4  type = 'I' )
            ( str = '{' row = 1 col = 5  type = 'I' )
            ( str = '1' row = 1 col = 7  type = 'I' )
            ( str = '}' row = 1 col = 9  type = 'I' )
            ( str = '{' row = 1 col = 10 type = 'I' )
            ( str = '1' row = 1 col = 12 type = 'I' )
            ( str = '}' row = 1 col = 14 type = 'I' )
            ( str = '|' row = 1 col = 15 type = 'I' ) ) ).

  ENDMETHOD.

ENDCLASS.
