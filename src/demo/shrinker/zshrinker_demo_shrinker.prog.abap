*&---------------------------------------------------------------------*
*& Report zshrinker_demo_shrinker
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_demo_shrinker.


TYPES ty_include_program_name TYPE c LENGTH 30.
TYPES ty_database_table_name TYPE c LENGTH 16.


SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_frdevc VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_frdevc TYPE devclass DEFAULT '$SHRINKER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_todevc VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_todevc TYPE devclass DEFAULT '$SHRINKER_DEMO_SHRINKER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_tab_ec VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_tab_ec TYPE ty_database_table_name DEFAULT 'ZSHRIDEMO_GTT_EC'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_tab_in VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_tab_in TYPE ty_database_table_name DEFAULT 'ZSHRIDEMO_GTT_IN'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_tab_mp VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_tab_mp TYPE ty_database_table_name DEFAULT 'ZSHRIDEMO_GTT_MP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_def VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_def TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_imp VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_imp TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_IMP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (83) t_licens VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_licens TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_LICENS'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_frdevc = 'Package containing Shrinker, from which class/interface pools are read'(t03).
  t_todevc = 'Package to contain the created objects'(t05).
  t_tab_ec = 'Create table as copy of ZSHRINKER_GTT_EC'(t06).
  t_tab_in = 'Create table as copy of ZSHRINKER_GTT_IN'(t07).
  t_tab_mp = 'Create table as copy of ZSHRINKER_GTT_MP'(t08).
  t_def    = 'Create include containing all Shrinker class/interface pool definitions, made local'(t01).
  t_imp    = 'Include to update with all Shrinker class pool implementations, made local'(t02).
  t_licens = 'Include containing the information about the license and notice'(t04).


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>main.
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcl_app DEFINITION.

  PUBLIC SECTION.

    INTERFACES zif_shrinker_user_exit_abapgit.

    CLASS-METHODS main
      RAISING
        zcx_shrinker.

  PRIVATE SECTION.

    METHODS main_2
      RAISING
        zcx_shrinker.

    DATA objects_to_copy TYPE zcl_shrinker=>ty_object_copies.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA(app) = NEW lcl_app( ).

    app->main_2( ).

  ENDMETHOD.


  METHOD main_2.

    DATA shrinker TYPE REF TO zcl_shrinker.


    DATA(include_does_not_exist) = CONV string( 'Include &1 does not exist. Create it manually as empty.'(001) ).

    SELECT SINGLE * FROM trdir WHERE name = @p_def INTO @DATA(trdir_def).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
    ENDIF.

    SELECT SINGLE * FROM trdir WHERE name = @p_imp INTO @DATA(trdir_imp).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
    ENDIF.


    "**********************************************************************
    "* Copy database tables
    "**********************************************************************

    objects_to_copy = VALUE #(
      ( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_EC' target_obj_name = p_tab_ec )
      ( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_IN' target_obj_name = p_tab_in )
      ( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_MP' target_obj_name = p_tab_mp ) ).

    shrinker = zcl_shrinker=>create( ).

    shrinker->copy_objects(
      EXPORTING
        source_package = p_frdevc
        target_package = p_todevc
        user_exit      = me
        objects        = objects_to_copy ).


    "**********************************************************************
    "* Classes and interfaces all together in DEF and IMP includes
    "**********************************************************************

    shrinker = zcl_shrinker=>create( ).
    DATA(abap_code) = shrinker->get_one_abap_code(
                package_range       = VALUE #( ( sign = 'I' option = 'EQ' low = p_frdevc ) )
                range_of_obj_type   = VALUE #( sign = 'I' option = 'EQ' ( low = 'CLAS' ) ( low = 'INTF' ) ( low = 'PROG' ) )
                global_replacements = VALUE #( ( posix_regex = '\<Z(.._SHRINKER\w*)' with = 'L$1' start_of_abap_word = abap_true member_interface_prefix = abap_true )
                                               ( LINES OF VALUE #( FOR <object_to_copy> IN objects_to_copy
                                                                   ( posix_regex = <object_to_copy>-source_obj_name with = <object_to_copy>-target_obj_name ) ) ) ) ).

    DATA(header_lines) = VALUE string_table(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( |* See include program { p_licens WIDTH = 30        }                            | )
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


  METHOD zif_shrinker_user_exit_abapgit~is_to_be_deserialized.

    IF line_exists( objects_to_copy[ object          = object
                                     target_obj_name = obj_name ] ).
      result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
