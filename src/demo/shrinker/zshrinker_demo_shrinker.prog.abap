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


*SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE t_b01.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN COMMENT (79) t_dci VISIBLE LENGTH 63.
*    SELECTION-SCREEN POSITION 65.
*    PARAMETERS p_dci AS CHECKBOX DEFAULT abap_true.
*  SELECTION-SCREEN END OF LINE.

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

*SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_cl_dci VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_cl_dci TYPE seoclsname DEFAULT 'ZCL_SHRIDEMO_DDIC_CLASS_INTERF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_cl_ap VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_cl_ap TYPE seoclsname DEFAULT 'ZCL_SHRIDEMO_ANY_PROGRAM'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_cl_as VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_cl_as TYPE seoclsname DEFAULT 'ZCL_SHRIDEMO_ABAP_SCAN'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_cl_uti VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_cl_uti TYPE seoclsname DEFAULT 'ZCL_SHRIDEMO_UTILS'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_cx VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_cx TYPE seoclsname DEFAULT 'ZCX_SHRIDEMO'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_if_aca VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_if_aca TYPE seoclsname DEFAULT 'ZIF_SHRIDEMO_ABAP_CODE_ADAPTER'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_if_uea VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_if_uea TYPE seoclsname DEFAULT 'ZIF_SHRIDEMO_USER_EXIT_ABAPGIT'.
SELECTION-SCREEN END OF LINE.

*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT (83) t_def VISIBLE LENGTH 63.
*  SELECTION-SCREEN POSITION 65.
*  PARAMETERS p_def TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_DEF'.
*SELECTION-SCREEN END OF LINE.
*
*SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT (83) t_imp VISIBLE LENGTH 63.
*  SELECTION-SCREEN POSITION 65.
*  PARAMETERS p_imp TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_IMP'.
*SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (83) t_licens VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_licens TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_SHRINKER_LICENS'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_frdevc = 'Package containing Shrinker, from which class/interface pools are read'(t03).
  t_todevc = 'Package to contain the created objects'(t05).
*  t_dci    = 'Objects needed only by ZCL_SHRINKER_DDIC_CLASS_INTERF'(t09).
  t_tab_ec = 'Create table as copy of ZSHRINKER_GTT_EC'(t06).
  t_tab_in = 'Create table as copy of ZSHRINKER_GTT_IN'(t07).
  t_tab_mp = 'Create table as copy of ZSHRINKER_GTT_MP'(t08).
  t_cl_dci = 'Create class as copy of ZCL_SHRINKER_DDIC_CLASS_INTERF'(t09).
  t_cl_ap  = 'Create class as copy of ZCL_SHRINKER_ANY_PROGRAM'(t10).
  t_cl_as  = 'Create class as copy of ZCL_SHRINKER_ABAP_SCAN'(t11).
  t_cl_uti = 'Create class as copy of ZCL_SHRINKER_UTILS'(t12).
  t_cx     = 'Create class as copy of ZCX_SHRINKER'(t13).
  t_if_aca = 'Create interface as copy of ZIF_SHRINKER_ABAP_CODE_ADAPTER'(t14).
  t_if_uea = 'Create interface as copy of ZIF_SHRINKER_USER_EXIT_ABAPGIT'(t15).
*  t_def    = 'Create include containing all Shrinker class/interface pool definitions, made local'(t01).
*  t_imp    = 'Include to update with all Shrinker class pool implementations, made local'(t02).
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

    INTERFACES zif_shrinker_abap_code_adapter.
    INTERFACES zif_shrinker_user_exit_abapgit.


    CLASS-METHODS main
      RAISING
        zcx_shrinker.


  PRIVATE SECTION.

    DATA objects_to_copy TYPE zcl_shrinker_copy_objects=>ty_object_copies.
    DATA header_lines TYPE string_table.


    METHODS main_2
      RAISING
        zcx_shrinker.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA(app) = NEW lcl_app( ).

    app->main_2( ).

  ENDMETHOD.


  METHOD main_2.

    header_lines = VALUE #(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( |* See include program { p_licens WIDTH = 30        }                            | )
    ( `*                                                                               ` )
    ( `********************************************************************************` ) ).

*    DATA(include_does_not_exist) = CONV string( 'Include &1 does not exist. Create it manually as empty.'(001) ).
*
*    SELECT SINGLE * FROM trdir WHERE name = @p_def INTO @DATA(trdir_def).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
*    ENDIF.
*
*    SELECT SINGLE * FROM trdir WHERE name = @p_imp INTO @DATA(trdir_imp).
*    IF sy-subrc <> 0.
*      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = p_def.
*    ENDIF.


    "**********************************************************************
    "* Build the list of objects to copy
    "**********************************************************************

    IF p_tab_ec IS NOT INITIAL.
      INSERT VALUE #( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_EC' target_obj_name = p_tab_ec )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_tab_in IS NOT INITIAL.
      INSERT VALUE #( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_IN' target_obj_name = p_tab_in )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_tab_mp IS NOT INITIAL.
      INSERT VALUE #( object = 'TABL' source_obj_name = 'ZSHRINKER_GTT_MP' target_obj_name = p_tab_mp )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_cl_dci IS NOT INITIAL.
      INSERT VALUE #( object = 'CLAS' source_obj_name = 'ZCL_SHRINKER_DDIC_CLASS_INTERF' target_obj_name = p_cl_dci )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_cl_ap IS NOT INITIAL.
      INSERT VALUE #( object = 'CLAS' source_obj_name = 'ZCL_SHRINKER_ANY_PROGRAM' target_obj_name = p_cl_ap )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_cl_as IS NOT INITIAL.
      INSERT VALUE #( object = 'CLAS' source_obj_name = 'ZCL_SHRINKER_ABAP_SCAN' target_obj_name = p_cl_as )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_cl_uti IS NOT INITIAL.
      INSERT VALUE #( object = 'CLAS' source_obj_name = 'ZCL_SHRINKER_UTILS' target_obj_name = p_cl_uti )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_cx IS NOT INITIAL.
      INSERT VALUE #( object = 'CLAS' source_obj_name = 'ZCX_SHRINKER' target_obj_name = p_cx )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_if_aca IS NOT INITIAL.
      INSERT VALUE #( object = 'INTF' source_obj_name = 'ZIF_SHRINKER_ABAP_CODE_ADAPTER' target_obj_name = p_if_aca )
            INTO TABLE objects_to_copy.
    ENDIF.

    IF p_if_uea IS NOT INITIAL.
      INSERT VALUE #( object = 'INTF' source_obj_name = 'ZIF_SHRINKER_USER_EXIT_ABAPGIT' target_obj_name = p_if_uea )
            INTO TABLE objects_to_copy.
    ENDIF.

    "**********************************************************************
    "* Copy the objects
    "**********************************************************************

    DATA(shrinker_copy_objects) = zcl_shrinker_copy_objects=>create( me ).

    shrinker_copy_objects->run(
        source_package = p_frdevc
        target_package = p_todevc
        user_exit      = me
        objects        = objects_to_copy ).

*    "**********************************************************************
*    "* Classes and interfaces all together in DEF and IMP includes
*    "**********************************************************************
*
*    DATA(shrinker_ddic_class_interf) = zcl_shrinker_ddic_class_interf=>create( ).
*    DATA(abap_code) = shrinker_ddic_class_interf->get_one_abap_code(
*                package_range       = VALUE #( ( sign = 'I' option = 'EQ' low = p_frdevc ) )
*                range_of_obj_type   = VALUE #( sign = 'I' option = 'EQ' ( low = 'CLAS' ) ( low = 'INTF' ) ( low = 'PROG' ) )
*                global_replacements = VALUE #( ( posix_regex = '\<Z(.._SHRINKER\w*)' with = 'L$1' start_of_abap_word = abap_true member_interface_prefix = abap_true )
*                                               ( LINES OF VALUE #( FOR <object_to_copy> IN objects_to_copy
*                                                                   ( posix_regex = <object_to_copy>-source_obj_name with = <object_to_copy>-target_obj_name ) ) ) ) ).
*
*    INSERT LINES OF header_lines
*        INTO abap_code-def_abap_source_code
*        INDEX 1.
*
*    INSERT LINES OF header_lines
*        INTO abap_code-imp_abap_source_code
*        INDEX 1.
*
*    DATA(syntax_check) = zcl_shrinker_abap_scan=>syntax_check( VALUE #(
*                        ( `REPORT.` )
*                        ( LINES OF abap_code-def_abap_source_code )
*                        ( LINES OF abap_code-imp_abap_source_code ) ) ).
*
*    IF syntax_check-mess IS NOT INITIAL.
*      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Syntax error &1'(001) msgv1 = syntax_check-mess.
*    ENDIF.
*
*    INSERT REPORT p_def FROM abap_code-def_abap_source_code DIRECTORY ENTRY trdir_def.
*
*    INSERT REPORT p_imp FROM abap_code-imp_abap_source_code DIRECTORY ENTRY trdir_imp.
*
*    COMMIT WORK.

  ENDMETHOD.


  METHOD zif_shrinker_abap_code_adapter~adapt_source_code.

    LOOP AT other_source_units REFERENCE INTO DATA(source_unit).
      INSERT LINES OF header_lines INTO source_unit->abap_source_code INDEX 1.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_shrinker_abap_code_adapter~adapt_source_code_before_rep_i.

  ENDMETHOD.


  METHOD zif_shrinker_user_exit_abapgit~is_to_be_deserialized.

    IF line_exists( objects_to_copy[ object          = object
                                     target_obj_name = obj_name ] ).
      result = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
