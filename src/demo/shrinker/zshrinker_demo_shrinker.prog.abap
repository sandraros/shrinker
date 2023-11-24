*&---------------------------------------------------------------------*
*& Report zshrinker_demo_shrinker
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_demo_shrinker.


TYPES ty_include_program_name TYPE c LENGTH 30.


SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_devc VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_devc TYPE devclass DEFAULT '$SHRINKER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_def VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_def TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_imp TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_IMP'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_devc = 'Package containing Shrinker, from which class/interface pools are read'(t03).
  t_def  = 'Include to update with all Shrinker class/interface pool definitions, made local'(t01).
  t_imp  = 'Include to update with all Shrinker class pool implementations, made local'(t02).


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

    INSERT REPORT p_imp FROM abap_code-imp_abap_source_code DIRECTORY ENTRY trdir_imp.

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
