*&---------------------------------------------------------------------*
*& Report zshrinker_demo_shrinker
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_demo_shrinker.

PARAMETERS p_devc TYPE devclass DEFAULT '$SHRINKER'.
PARAMETERS p_def TYPE devclass DEFAULT 'ZSHRINKER_DEMO_SHRINKER_DEF'.
PARAMETERS p_imp TYPE devclass DEFAULT 'ZSHRINKER_DEMO_SHRINKER_IMP'.


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>main.
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcl_app DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main
      RAISING
        zcx_shrinker.
ENDCLASS.


CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA(include_does_not_exist) = CONV string( 'Include &1 does not exist. Create it manually as empty.'(001) ).

    SELECT SINGLE * FROM trdir WHERE name = @p_def INTO @DATA(trdir_def).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
    ENDIF.

    SELECT SINGLE * FROM trdir WHERE name = @p_imp INTO @DATA(trdir_imp).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
    ENDIF.

    DATA(shrinker) = zcl_shrinker=>create( ).
    DATA(abap_code) = shrinker->get_one_abap_code(
                package_range       = VALUE #( ( sign = 'I' option = 'EQ' low = p_devc ) )
                global_replacements = VALUE #( ( posix_regex = '\<Z(.._SHRINKER\w*)' with = 'L$1' start_of_abap_word = abap_true member_interface_prefix = abap_true ) ) ).

    DATA(header_lines) = VALUE string_table(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( `* See include program ZSHRINKER_LICENSE                                         ` )
    ( `*                                                                               ` )
    ( `********************************************************************************` ) ).

    INSERT LINES OF header_lines
        INTO abap_code-def_abap_source_code
        INDEX 1.

    INSERT LINES OF header_lines
        INTO abap_code-imp_abap_source_code
        INDEX 1.

    DATA(syntax_check) = shrinker->syntax_check( VALUE #(
                        ( `REPORT.` )
                        ( LINES OF abap_code-def_abap_source_code )
                        ( LINES OF abap_code-imp_abap_source_code ) ) ).

    IF syntax_check-mess IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Syntax error &1'(001) msgv1 = syntax_check-mess.
    ENDIF.

    INSERT REPORT p_def FROM abap_code-def_abap_source_code DIRECTORY ENTRY trdir_def.
    DELETE REPORT p_def STATE 'I'.

    INSERT REPORT p_imp FROM abap_code-imp_abap_source_code DIRECTORY ENTRY trdir_imp.
    DELETE REPORT p_imp STATE 'I'.

    COMMIT WORK.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_main IMPLEMENTATION.

  METHOD test.

    p_devc = '$SHRINKER'.
    p_def = 'ZSHRINKER_DEMO_SHRINKER_DEF'.
    p_imp = 'ZSHRINKER_DEMO_SHRINKER_IMP'.
    lcl_app=>main( ).

  ENDMETHOD.

ENDCLASS.
