*&---------------------------------------------------------------------*
*& Report zshrinker_demo_abap2xlsx
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_demo_abap2xlsx.

INCLUDE zshrinker_demo_abapgit_def.
INCLUDE zshrinker_demo_shrinker_def.

INCLUDE zshrinker_demo_abapgit_imp1.
INCLUDE zshrinker_demo_abapgit_imp2.
INCLUDE zshrinker_demo_abapgit_imp3.
INCLUDE zshrinker_demo_abapgit_imp4.
INCLUDE zshrinker_demo_abapgit_imp5.
INCLUDE zshrinker_demo_shrinker_imp.

DATA: BEGIN OF dummy_select_options,
        devclass TYPE devclass,
      END OF dummy_select_options.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_zip VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_zip TYPE string LOWER CASE DEFAULT `C:\temp\$SHRINKER.zip`.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_devc_i VISIBLE LENGTH 60.
SELECTION-SCREEN POSITION 62.
SELECT-OPTIONS s_devc_i FOR dummy_select_options-devclass DEFAULT '$ABAP2XLSX*' SIGN I OPTION CP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_devc_e VISIBLE LENGTH 60.
SELECTION-SCREEN POSITION 62.
SELECT-OPTIONS s_devc_e FOR dummy_select_options-devclass DEFAULT '$ABAP2XLSXDEMOS*' SIGN I OPTION CP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_oo_def VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_to_def TYPE syrepid DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_oo_imp VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_to_imp TYPE syrepid DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_IMP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_tr_s1 VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_tr_s1 TYPE syrepid DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S1'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_tr_s2 VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_tr_s2 TYPE syrepid DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S2'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (80) t_intf VISIBLE LENGTH 63.
SELECTION-SCREEN POSITION 65.
PARAMETERS p_intf TYPE seoclsname DEFAULT 'ZIF_SHRINKER_DEMO_ABAP2XLSX_WH'.
SELECTION-SCREEN END OF LINE.



INITIALIZATION.
  t_zip    = 'Temporary ZIP file to permit object recreation via abapGit'(t09).
  t_devc_i = 'abap2xlsx packages of objects to include'(t06).
  t_devc_e = 'abap2xlsx packages of objects to exclude'(t07).
  t_oo_def = 'Include = definitions of all abap2xlsx class and interface pools, made local'(t01).
  t_oo_imp = 'Include = class implementations of all abap2xlsx class pools, made local'(t02).
  t_tr_s1  = 'Transformation = copy of ZEXCEL_TR_SHARED_STRINGS'(t03).
  t_tr_s2  = 'Transformation = copy of ZEXCEL_TR_SHEET + rename ZCL_EXCEL_WRITER_HUGE_FILE'(t04).
  t_intf   = 'Interface pool to contain public section of ZCL_EXCEL_WRITER_HUGE_FILE'(t05).


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>('CREATE_MAIN').
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcl_app DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES lif_shrinker_abap_code_adapter.

    CLASS-METHODS create_main
      RAISING
        lcx_shrinker.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lcl_app.

    METHODS main
      RAISING
        lcx_shrinker.

  PRIVATE SECTION.
    DATA shrinker_objects_to_recreate TYPE lif_abapgit_definitions=>ty_tadir_tt.
    DATA tabix_public_section TYPE i.
    DATA zip TYPE REF TO cl_abap_zip.
    DATA li_fe_serv TYPE REF TO lif_abapgit_frontend_services.
    DATA repo_shrinker TYPE REF TO lif_abapgit_repo.

    METHODS create_zip_of_shrinker_repo
      RAISING
        lcx_shrinker.

    METHODS recreate_abap_objects_from_zip
      RAISING
        lcx_shrinker.

    METHODS zip_replace
      IMPORTING
        file_path TYPE string
        content   TYPE xstring.

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD create.
    result = NEW lcl_app( ).
  ENDMETHOD.


  METHOD create_main.
    DATA(app) = NEW lcl_app( ).
    app->main( ).
  ENDMETHOD.


  METHOD main.
    DATA xstring TYPE xstring.

    "==============================================================================
    "
    "==============================================================================

    create_zip_of_shrinker_repo( ).


    "==============================================================================
    "   - Create ZIF_SHRINKER_DEMO_ABAP2XLSX_WH = "INTERFACE zif_shrinker_demo_abap2xlsx_wh PUBLIC."
    "                                           + copy of the public section of class ZCL_EXCEL_WRITER_HUGE_FILE
    "                                           + "ENDINTERFACE."
    "==============================================================================

    DATA(table_abap_code_clas) = VALUE string_table( ).
    READ REPORT 'ZCL_EXCEL_WRITER_HUGE_FILE====CU' INTO table_abap_code_clas.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_shrinker.
    ENDIF.

    FIND '  PUBLIC SECTION.' IN TABLE table_abap_code_clas IGNORING CASE MATCH LINE tabix_public_section.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_shrinker.
    ENDIF.

    DATA(header_lines) = VALUE string_table(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( `* See include program ZSHRINKER_DEMO_ABAP2XLSX_LICEN                            ` )
    ( `*                                                                               ` )
    ( `********************************************************************************` ) ).

    DATA(table_abap_code_intf_new) = VALUE string_table(
            ( LINES OF header_lines )
            ( `INTERFACE ZIF_SHRINKER_DEMO_ABAP2XLSX_WH PUBLIC.` )
            ( LINES OF table_abap_code_clas FROM tabix_public_section + 1 )
            ( `ENDINTERFACE.` ) ).

    xstring = cl_abap_codepage=>convert_to( concat_lines_of( table = table_abap_code_intf_new sep = |\n| ) && |\n| ).

    zip_replace( file_path = `src/demo/abap2xlsx/zif_shrinker_demo_abap2xlsx_wh.intf.abap`
                 content   = xstring ).


    "==============================================================================
    "   - ZSHRINKER_DEMO_ABAP2XLSX_DEF = code generated by Shrinker as shown below
    "   - ZSHRINKER_DEMO_ABAP2XLSX_IMP = code generated by Shrinker as shown below
    "==============================================================================

    DATA(package_range) = VALUE lcl_shrinker=>ty_package_range( ).
    SELECT 'I'      AS sign,
           'EQ'     AS option,
           devclass AS low
    FROM tdevc
    WHERE devclass IN @s_devc_i
      AND devclass NOT IN @s_devc_e
    INTO TABLE @package_range.

    DATA(shrinker) = lcl_shrinker=>create( customizer = me ).
    DATA(abap_code) = shrinker->get_one_abap_code(
                    package_range        = package_range
                    global_replacements  = VALUE #( ( posix_regex = 'ZEXCEL_TR_SHARED_STRINGS' with = 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S1' )
                                                    ( posix_regex = 'ZEXCEL_TR_SHEET'          with = 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S2' )
                                                    ( posix_regex = '\<Z(.._EXCEL\w*)'         with = 'L$1' ) ) ).

    DATA(syntax_check) = shrinker->syntax_check( VALUE #(
                        ( `REPORT.` )
                        ( LINES OF abap_code-def_abap_source_code )
                        ( LINES OF abap_code-imp_abap_source_code ) ) ).

    IF syntax_check-mess IS NOT INITIAL.
      RAISE EXCEPTION TYPE lcx_shrinker EXPORTING text = 'Syntax error &1'(001) msgv1 = syntax_check-mess.
    ENDIF.

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = value string_table(
                                ( LINES OF header_lines )
                                ( lines of abap_code-def_abap_source_code ) )
                        sep   = |\n| ) && |\n| ).

    zip_replace( file_path = |src/demo/abap2xlsx/{ to_lower( p_to_def ) }.prog.abap|
                 content   = xstring ).

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = value string_table(
                                ( LINES OF header_lines )
                                ( lines of abap_code-imp_abap_source_code ) )
                        sep   = |\n| ) && |\n| ).

    zip_replace( file_path = |src/demo/abap2xlsx/{ to_lower( p_to_imp ) }.prog.abap|
                 content   = xstring ).


    "==============================================================================
    " ZSHRINKER_DEMO_ABAP2XLSX_TR_S1 = exact copy of ZEXCEL_TR_SHARED_STRINGS
    "==============================================================================

    DATA(table_abap_code_xslt) = VALUE string_table( ).
    READ REPORT 'ZEXCEL_TR_SHARED_STRINGS======XT' INTO table_abap_code_xslt.
    IF sy-subrc <> 0.
      " TODO ERROR DOES NOT EXIST
      RAISE EXCEPTION TYPE lcx_shrinker.
    ENDIF.

    DATA(xml_header_lines) = VALUE string_table(
    ( `<!--********************************************************************************-->` )
    ( `<!--*                                                                               -->` )
    ( `<!--* LICENSE and NOTICE                                                            -->` )
    ( `<!--*                                                                               -->` )
    ( `<!--* See include program ZSHRINKER_DEMO_ABAP2XLSX_LICEN                            -->` )
    ( `<!--*                                                                               -->` )
    ( `<!--********************************************************************************-->` ) ).

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = value string_table(
                                ( LINES OF xml_header_lines )
                                ( lines of table_abap_code_xslt ) )
                        sep   = |\r\n| ) && |\r\n| ).

    zip_replace( file_path = `src/demo/abap2xlsx/zshrinker_demo_abap2xlsx_tr_s1.xslt.source.xml`
                 content   = xstring ).


    "==============================================================================
    " ZSHRINKER_DEMO_ABAP2XLSX_TR_S2 = copy of ZEXCEL_TR_SHEET with text "ZCL_EXCEL_WRITER_HUGE_FILE"
    "     replaced with "ZIF_SHRINKER_DEMO_ABAP2XLSX_WH".
    "==============================================================================

    READ REPORT 'ZEXCEL_TR_SHEET===============XT' INTO table_abap_code_xslt.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_shrinker.
    ENDIF.

    REPLACE ALL OCCURRENCES
            OF 'ZCL_EXCEL_WRITER_HUGE_FILE'
            IN TABLE table_abap_code_xslt
            WITH 'ZIF_SHRINKER_DEMO_ABAP2XLSX_WH'
            IGNORING CASE.

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = value string_table(
                                ( LINES OF xml_header_lines )
                                ( lines of table_abap_code_xslt ) )
                        sep   = |\r\n| ) && |\r\n| ).

    zip_replace( file_path = `src/demo/abap2xlsx/zshrinker_demo_abap2xlsx_tr_s2.xslt.source.xml`
                 content   = xstring ).


    "==============================================================================
    " Recreate ABAP objects from the ZIP file = Deserialize objects via abapGit
    "==============================================================================
    recreate_abap_objects_from_zip( ).


  ENDMETHOD.


  METHOD create_zip_of_shrinker_repo.

    TRY.

        shrinker_objects_to_recreate = VALUE lif_abapgit_definitions=>ty_tadir_tt(
                        ( object = 'PROG' obj_name = 'ZSHRINKER_DEMO_ABAP2XLSX_DEF' )      " definitions of all abap2xlsx class and interface pools, made local
                        ( object = 'PROG' obj_name = 'ZSHRINKER_DEMO_ABAP2XLSX_IMP' )      " class implementations of all abap2xlsx class pools, made local
                        ( object = 'INTF' obj_name = 'ZIF_SHRINKER_DEMO_ABAP2XLSX_WH' )    " Interface pool to contain public section of ZCL_EXCEL_WRITER_HUGE_FILE
                        ( object = 'XSLT' obj_name = 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S1' )    " copy of ZEXCEL_TR_SHARED_STRINGS
                        ( object = 'XSLT' obj_name = 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S2' ) ). " copy of ZEXCEL_TR_SHEET + rename ZCL_EXCEL_WRITER_HUGE_FILE

        SELECT object, obj_name, devclass
            FROM tadir
            FOR ALL ENTRIES IN @shrinker_objects_to_recreate
            WHERE object   = @shrinker_objects_to_recreate-object
              AND obj_name = @shrinker_objects_to_recreate-obj_name
            INTO TABLE @DATA(table_tadir).

        DATA(shrinker_package) = VALUE devclass( ).

        IF table_tadir IS INITIAL.
          " TODO ERROR PLEASE CREATE AT LEAST ONE OBJECT (TO DETERMINE SHRINKER PACKAGE)
          RAISE EXCEPTION TYPE lcx_shrinker.
        ENDIF.

        LOOP AT table_tadir REFERENCE INTO DATA(tadir).
          IF shrinker_package IS INITIAL.
            shrinker_package = tadir->devclass.
          ELSEIF tadir->devclass <> shrinker_package.
            " TODO ERROR ONE OBJECT IS NOT PART OF SHRINKER PACKAGE
            RAISE EXCEPTION TYPE lcx_shrinker.
          ENDIF.
        ENDLOOP.

        "==========================================
        " Serialize abap2xlsx files
        "==========================================
        lcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package(
          EXPORTING
            iv_package    = shrinker_package
          IMPORTING
            ei_repo       = repo_shrinker ).

        repo_shrinker->refresh( ).
        DATA(files_shrinker) = repo_shrinker->get_files_local( ).

        "==========================================
        " Create a ZIP file of the repository
        " Credit: method ZIP_SERVICES of lcl_abapgit_GUI_ROUTER.
        "==========================================
        DATA(xstring) = lcl_abapgit_zip=>encode_files( files_shrinker ).

        li_fe_serv = lcl_abapgit_ui_factory=>get_frontend_services( ).

        li_fe_serv->file_download(
          iv_path = p_zip
          iv_xstr = xstring ).

        "==========================================
        " Instantiate a ZIP instance, whose content will be modified
        "==========================================
        zip = NEW cl_abap_zip( ).
        zip->load(
          EXPORTING
            zip             = xstring
          EXCEPTIONS
            zip_parse_error = 1
            OTHERS          = 2 ).
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE lcx_shrinker.
        ENDIF.

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE lcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD recreate_abap_objects_from_zip.

    TRY.

        DATA(xstring) = zip->save( ).
        DATA(files_shrinker_bis) = lcl_abapgit_zip=>load( xstring ).

        CAST lcl_abapgit_repo( repo_shrinker )->set_files_remote( files_shrinker_bis ).

        " Credit: method GUI_DESERIALIZE of zcl_abapgit_SERVICES_REPO
        " Note that the source code units are compared in the method CALCULATE of ZCL_ABAPGIT_REPO_STATUS.
        DATA(ls_checks) = repo_shrinker->deserialize_checks( ).
        LOOP AT ls_checks-overwrite REFERENCE INTO DATA(check_overwrite).
          CASE check_overwrite->action.
            WHEN lif_abapgit_objects=>c_deserialize_action-add
               OR lif_abapgit_objects=>c_deserialize_action-update
               OR lif_abapgit_objects=>c_deserialize_action-overwrite.
              " ACTION (credit: structured constant ZIF_ABAPGIT_OBJECTS=>C_DESERIALIZE_ACTION and
              " method WARNING_OVERWRITE_FIND of ZCL_ABAPGIT_OBJECTS_CHECK):
              "   1:
              "   2: update local object
              "   3: overwrite local object
              "   4: delete local object
              IF line_exists( shrinker_objects_to_recreate[ object   = check_overwrite->obj_type
                                                            obj_name = check_overwrite->obj_name ] ).
                check_overwrite->decision = lif_abapgit_definitions=>c_yes.
              ELSE.
                check_overwrite->decision = lif_abapgit_definitions=>c_no.
              ENDIF.
            WHEN OTHERS.
              check_overwrite->decision = lif_abapgit_definitions=>c_no.
          ENDCASE.
        ENDLOOP.

        DATA(li_log) = NEW lcl_abapgit_log( ).
        " NB: REPO->DESERIALIZE does a COMMIT WORK.
        repo_shrinker->deserialize(
          is_checks = ls_checks
          ii_log    = li_log ).

      CATCH cx_root INTO DATA(error).
        RAISE EXCEPTION TYPE lcx_shrinker EXPORTING previous = error.
    ENDTRY.

  ENDMETHOD.


  METHOD zip_replace.

    zip->delete(
      EXPORTING
        name            = file_path
      EXCEPTIONS
        zip_index_error = 1
        OTHERS          = 2 ).

    IF sy-subrc <> 0.
      " TODO ERROR
    ENDIF.

    zip->add( name    = file_path
              content = content ).

  ENDMETHOD.


  METHOD lif_shrinker_abap_code_adapter~adapt_source_code.

    DATA(reader_2007_ccimp) = REF #( classes_interfaces-classes[ name = 'ZCL_EXCEL_READER_2007' ]-includes[ extension_code = 'CCIMP' ] OPTIONAL ).
    IF reader_2007_ccimp IS BOUND.
      " There is a T_RELATIONSHIP type in the Local Types of ZCL_EXCEL_READER_2007, which is not used,
      " and whose name is already used as a Class Type in the protected section. Shrinker isn't able to distinguish
      " which occurrences of T_RELATIONSHIP refer to the Local Type which ones refer to the Class Type, hence
      " renaming all which later lead to syntax errors in other objects referring to the Class Type T_RELATIONSHIP
      " which doesn't exist because it has been renamed.
      " Solution: Local Type not used so just remove it.
      FIND REGEX '\<T_RELATIONSHIP\>'
          IN TABLE reader_2007_ccimp->abap_source_code
          MATCH LINE DATA(line_index)
          IGNORING CASE.
      IF sy-subrc = 0.
        DATA(types_t_relationship) = lcl_shrinker=>get_whole_abap_statement(
            line_index       = line_index
            abap_source_code = reader_2007_ccimp->abap_source_code ).
        DELETE reader_2007_ccimp->abap_source_code
            FROM types_t_relationship-first_line_index
            TO types_t_relationship-last_line_index.
      ENDIF.
    ENDIF.

    DATA(writer_huge_file_cu) = REF #( classes_interfaces-classes[ name = 'ZCL_EXCEL_WRITER_HUGE_FILE' ]-includes[ extension_code = 'CU' ] OPTIONAL ).
    IF writer_huge_file_cu IS BOUND.
      DATA(table_abap_code_clas_new) = VALUE string_table(
              ( LINES OF writer_huge_file_cu->abap_source_code FROM 1 TO tabix_public_section )
              ( `  INTERFACES ZIF_SHRINKER_DEMO_ABAP2XLSX_WH.` ) ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
