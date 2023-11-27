*&---------------------------------------------------------------------*
*& Report zshrinker_demo_abap2xlsx
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_demo_abap2xlsx.


TYPES ty_include_program_name TYPE c LENGTH 30.
TYPES ty_transformation_name TYPE c LENGTH 30.


DATA: BEGIN OF dummy_select_options,
        devclass TYPE devclass,
      END OF dummy_select_options.


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
  SELECTION-SCREEN COMMENT (80) t_devc VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_devc TYPE devclass DEFAULT '$SHRINKER_DEMO_ABAP2XLSX'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_oo_def VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_to_def TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_oo_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_to_imp TYPE ty_include_program_name DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_IMP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_tr_s1 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_tr_s1 TYPE ty_transformation_name DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S1'. " ZEXCEL_TR_SHARED_STRINGS
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_tr_s2 VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_tr_s2 TYPE ty_transformation_name DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_TR_S2'. " ZEXCEL_TR_SHEET
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_intf VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_intf TYPE seoclsname DEFAULT 'ZIF_SHRINKER_DEMO_ABAP2XLSX_WH'. " ZCL_EXCEL_WRITER_HUGE_FILE
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_msag VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_msag TYPE t100a-arbgb DEFAULT 'ZSHRINKER_DEMO_ABAP2'. " ZABAP2XLSX
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_licens VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_licens TYPE seoclsname DEFAULT 'ZSHRINKER_DEMO_ABAP2XLSX_LICEN'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_devc_i = 'abap2xlsx packages of objects to include'(t06).
  t_devc_e = 'abap2xlsx packages of objects to exclude'(t07).
  t_devc   = 'Package of objects to be (re)generated'(t08).
  t_oo_def = 'Include = definitions of all abap2xlsx class and interface pools, made local'(t01).
  t_oo_imp = 'Include = class implementations of all abap2xlsx class pools, made local'(t02).
  t_tr_s1  = 'Transformation = copy of ZEXCEL_TR_SHARED_STRINGS'(t03).
  t_tr_s2  = 'Transformation = copy of ZEXCEL_TR_SHEET + rename ZCL_EXCEL_WRITER_HUGE_FILE'(t04).
  t_intf   = 'Interface pool to contain public section of ZCL_EXCEL_WRITER_HUGE_FILE'(t05).
  t_msag   = 'Message class = copy of ZABAP2XLSX'(t11).
  t_licens = 'Include program containing the license and notice'(t10).


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>('CREATE_MAIN').
    CATCH cx_root INTO DATA(error).
      zcl_shrinker_utils=>abap_message_i_for_exception( error ).
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcl_app DEFINITION
    CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_shrinker_abap_code_adapter.
    INTERFACES zif_shrinker_user_exit_abapgit.

    CLASS-METHODS create_main
      RAISING
        zcx_shrinker.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO lcl_app.

    METHODS main
      RAISING
        zcx_shrinker.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_tadir,
        object   TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
      END OF ty_tadir .
    TYPES:
      ty_tadir_tt TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY .

    DATA shrinker_objects_to_recreate TYPE ty_tadir_tt.
    DATA tabix_public_section TYPE i.
    DATA zip TYPE REF TO cl_abap_zip.

    METHODS get_include_program_xml
      IMPORTING
        include_program_name TYPE ty_include_program_name
      RETURNING
        VALUE(result)        TYPE xstring.

    METHODS get_interface_xml
      IMPORTING
        interface_name TYPE seoclsname
      RETURNING
        VALUE(result)  TYPE xstring.

    METHODS get_transformation_xml
      IMPORTING
        transformation_name TYPE ty_transformation_name
      RETURNING
        VALUE(result)       TYPE xstring.

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

    shrinker_objects_to_recreate = VALUE #(
                    ( object = 'PROG' obj_name = p_to_def )   " definitions of all abap2xlsx class and interface pools, made local
                    ( object = 'PROG' obj_name = p_to_imp )   " class implementations of all abap2xlsx class pools, made local
                    ( object = 'INTF' obj_name = p_intf )     " Interface pool to contain public section of ZCL_EXCEL_WRITER_HUGE_FILE
                    ( object = 'XSLT' obj_name = p_tr_s1 )    " copy of transformation ZEXCEL_TR_SHARED_STRINGS
                    ( object = 'XSLT' obj_name = p_tr_s2 )    " copy of transformation ZEXCEL_TR_SHEET + rename ZCL_EXCEL_WRITER_HUGE_FILE
                    ( object = 'MSAG' obj_name = p_msag ) ).  " copy of message class ZABAP2XLSX

    DATA(package_range) = VALUE zcl_shrinker_ddic_class_interf=>ty_package_range( ).
    SELECT 'I'      AS sign,
           'EQ'     AS option,
           devclass AS low
        FROM tdevc
        WHERE devclass IN @s_devc_i
          AND devclass NOT IN @s_devc_e
        INTO TABLE @package_range.


    "==============================================================================
    " Serialize the source Git repository
    "==============================================================================

    " Taking the first one arbitrarily, because using any package is sufficient to locate the Git repository.
    DATA(git_abap2xlsx) = zcl_shrinker_connect_abapgit=>create( package = package_range[ 1 ]-low ).
    git_abap2xlsx->serialize( ).
    DATA(zip_git_abap2xlsx) = git_abap2xlsx->get_zip( ).


    "==============================================================================
    " Serialize the target package
    "==============================================================================

    DATA(abapgit) = zcl_shrinker_connect_abapgit=>create( package   = p_devc
                                                          user_exit = me ).

    " P_DEVC may not be the root package of the Git repository, so getting
    " here the right path.
    " e.g. package "$SHRINKER_DEMO_ABAP2XLX", root package being "$SHRINKER",
    " the corresponding path is "src/demo/abap2xlsx/".
    DATA(target_package_path) = abapgit->get_package_path( package = p_devc ).
    SHIFT target_package_path LEFT DELETING LEADING '/'.


    abapgit->serialize( ).


    "==============================================================================
    " Create ZIF_SHRINKER_DEMO_ABAP2XLSX_WH = "INTERFACE zif_shrinker_demo_abap2xlsx_wh PUBLIC."
    "                                       + copy of the public section of class ZCL_EXCEL_WRITER_HUGE_FILE
    "                                       + "ENDINTERFACE."
    "==============================================================================

    DATA(table_abap_code_clas) = VALUE string_table( ).
    READ REPORT 'ZCL_EXCEL_WRITER_HUGE_FILE====CU' INTO table_abap_code_clas.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker.
    ENDIF.

    FIND '  PUBLIC SECTION.' IN TABLE table_abap_code_clas IGNORING CASE MATCH LINE tabix_public_section.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker.
    ENDIF.

    DATA(header_lines) = VALUE string_table(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( |* See include program { p_licens WIDTH = 30        }                            | )
    ( `*                                                                               ` )
    ( `********************************************************************************` ) ).

    DATA(table_abap_code_intf_new) = VALUE string_table(
            ( LINES OF header_lines )
            ( |INTERFACE { p_intf } PUBLIC.| )
            ( LINES OF table_abap_code_clas FROM tabix_public_section + 1 )
            ( `ENDINTERFACE.` ) ).

    xstring = cl_abap_codepage=>convert_to( concat_lines_of( table = table_abap_code_intf_new sep = |\n| ) && |\n| ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_intf ) }.intf.abap|
                          content   = xstring ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_intf ) }.intf.xml|
                          content   = get_interface_xml( p_intf ) ).


    "==============================================================================
    " Interface and Class definitions, and class implementations
    "==============================================================================

    DATA(shrinker) = zcl_shrinker_ddic_class_interf=>create( customizer = me ).
    DATA(abap_code) = shrinker->get_one_abap_code(
                    package_range        = package_range
                    global_replacements  = VALUE #( ( posix_regex = 'ZEXCEL_TR_SHARED_STRINGS' with = p_tr_s1 )
                                                    ( posix_regex = 'ZEXCEL_TR_SHEET'          with = p_tr_s2 )
                                                    ( posix_regex = 'ZABAP2XLSX'               with = p_msag )
                                                    ( posix_regex = '\<Z(.._EXCEL\w*)'         with = 'L$1' ) ) ).

    DATA(syntax_check) = zcl_shrinker_abap_scan=>syntax_check( VALUE #(
                        ( `REPORT.` )
                        ( LINES OF abap_code-def_abap_source_code )
                        ( LINES OF abap_code-imp_abap_source_code ) ) ).

    " Fail for any error except CALL608 (Transformation "&1" does not have an active version or contains syntax errors)
    " concerning the copy of ZEXCEL_TR_SHARED_STRINGS, which happens if it's the first time because it's not created yet.
    IF syntax_check-mess IS NOT INITIAL
        AND NOT ( syntax_check-wrd = p_tr_s1
              AND syntax_check-mid-keyword = 'CALL'
              AND syntax_check-mid-msgnumber = '608' ).
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Syntax error &1'(001) msgv1 = syntax_check-mess.
    ENDIF.

    "-------------------
    " Class/Interface definitions
    "-------------------

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = VALUE string_table(
                                ( LINES OF header_lines )
                                ( LINES OF abap_code-def_abap_source_code ) )
                        sep   = |\n| ) && |\n| ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_to_def ) }.prog.abap|
                          content   = xstring ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_to_def ) }.prog.xml|
                          content   = get_include_program_xml( p_to_def ) ).

    "-------------------
    " Class implementations
    "-------------------

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = VALUE string_table(
                                ( LINES OF header_lines )
                                ( LINES OF abap_code-imp_abap_source_code ) )
                        sep   = |\n| ) && |\n| ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_to_imp ) }.prog.abap|
                          content   = xstring ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_to_imp ) }.prog.xml|
                          content   = get_include_program_xml( p_to_imp ) ).


    "==============================================================================
    " ZSHRINKER_DEMO_ABAP2XLSX_TR_S1 = exact copy of ZEXCEL_TR_SHARED_STRINGS
    "==============================================================================

    DATA(table_abap_code_xslt) = VALUE string_table( ).
    READ REPORT 'ZEXCEL_TR_SHARED_STRINGS======XT' INTO table_abap_code_xslt.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker.
    ENDIF.

    DATA(xml_header_lines) = VALUE string_table(
    ( `<!--********************************************************************************-->` )
    ( `<!--*                                                                               -->` )
    ( `<!--* LICENSE and NOTICE                                                            -->` )
    ( `<!--*                                                                               -->` )
    ( |<!--* See include program { p_licens WIDTH = 30 }                            -->| )
    ( `<!--*                                                                               -->` )
    ( `<!--********************************************************************************-->` ) ).

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = VALUE string_table(
                                ( LINES OF xml_header_lines )
                                ( LINES OF table_abap_code_xslt ) )
                        sep   = |\r\n| ) && |\r\n| ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_tr_s1 ) }.xslt.source.xml|
                          content   = xstring ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_tr_s1 ) }.xslt.xml|
                          content   = get_transformation_xml( p_tr_s1 ) ).


    "==============================================================================
    " ZSHRINKER_DEMO_ABAP2XLSX_TR_S2 = copy of ZEXCEL_TR_SHEET with text "ZCL_EXCEL_WRITER_HUGE_FILE"
    "     replaced with "ZIF_SHRINKER_DEMO_ABAP2XLSX_WH".
    "==============================================================================

    READ REPORT 'ZEXCEL_TR_SHEET===============XT' INTO table_abap_code_xslt.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker.
    ENDIF.

    REPLACE ALL OCCURRENCES
            OF 'ZCL_EXCEL_WRITER_HUGE_FILE'
            IN TABLE table_abap_code_xslt
            WITH p_intf
            IGNORING CASE.

    xstring = cl_abap_codepage=>convert_to( concat_lines_of(
                        table = VALUE string_table(
                                ( LINES OF xml_header_lines )
                                ( LINES OF table_abap_code_xslt ) )
                        sep   = |\r\n| ) && |\r\n| ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_tr_s2 ) }.xslt.source.xml|
                          content   = xstring ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_tr_s2 ) }.xslt.xml|
                          content   = get_transformation_xml( p_tr_s2 ) ).


    "==============================================================================
    " Message class ZABAP2XLSX
    "==============================================================================

    zip_git_abap2xlsx->get( EXPORTING name    = 'src/zabap2xlsx.msag.xml'
                            IMPORTING content = DATA(msag_xml_xstring) ).

    data(msag_xml_string) = cl_abap_codepage=>convert_from( msag_xml_xstring ).
    REPLACE ALL OCCURRENCES OF 'ZABAP2XLSX' IN msag_xml_string WITH p_msag.
    msag_xml_xstring = cl_abap_codepage=>convert_to( msag_xml_string ).

    abapgit->zip_replace( file_path = |{ target_package_path }{ to_lower( p_msag ) }.msag.xml|
                          content   = msag_xml_xstring ).

    "==============================================================================
    " Recreate ABAP objects from the ZIP file = Deserialize objects via abapGit
    "==============================================================================

    abapgit->deserialize( ).


  ENDMETHOD.


  METHOD get_include_program_xml.

    result = cl_abap_codepage=>convert_to( concat_lines_of( sep = |\n| table = VALUE string_table(
            ( `﻿<?xml version="1.0" encoding="utf-8"?>                                             ` )
            ( `<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">` )
            ( ` <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">                   ` )
            ( `  <asx:values>                                                                     ` )
            ( `   <PROGDIR>                                                                       ` )
            ( |    <NAME>{ include_program_name }</NAME>                                          | )
            ( `    <DBAPL>S</DBAPL>                                                               ` )
            ( `    <DBNA>D$</DBNA>                                                                ` )
            ( `    <SUBC>I</SUBC>                                                                 ` )
            ( `    <FIXPT>X</FIXPT>                                                               ` )
            ( `    <LDBNAME>D$S</LDBNAME>                                                         ` )
            ( `    <UCCHECK>X</UCCHECK>                                                           ` )
            ( `   </PROGDIR>                                                                      ` )
            ( `   <TPOOL>                                                                         ` )
            ( `    <item>                                                                         ` )
            ( `     <ID>R</ID>                                                                    ` )
            ( `     <ENTRY>Demo - Result of shrinking abap2xlsx</ENTRY>                           ` )
            ( `     <LENGTH>36</LENGTH>                                                           ` )
            ( `    </item>                                                                        ` )
            ( `   </TPOOL>                                                                        ` )
            ( `  </asx:values>                                                                    ` )
            ( ` </asx:abap>                                                                       ` )
            ( `</abapGit>                                                                         ` ) ) ) ).

  ENDMETHOD.


  METHOD get_interface_xml.

    result = cl_abap_codepage=>convert_to( concat_lines_of( sep = |\n| table = VALUE string_table(
            ( `﻿<?xml version="1.0" encoding="utf-8"?>                                             ` )
            ( `<abapGit version="v1.0.0" serializer="LCL_OBJECT_INTF" serializer_version="v1.0.0">` )
            ( ` <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">                   ` )
            ( `  <asx:values>                                                                     ` )
            ( `   <VSEOINTERF>                                                                    ` )
            ( |    <CLSNAME>{ interface_name }</CLSNAME>                                          | )
            ( `    <LANGU>E</LANGU>                                                               ` )
            ( `    <DESCRIPT>Demo - Result of shrinking abap2xlsx</DESCRIPT>                      ` )
            ( `    <EXPOSURE>2</EXPOSURE>                                                         ` )
            ( `    <STATE>1</STATE>                                                               ` )
            ( `    <UNICODE>X</UNICODE>                                                           ` )
            ( `   </VSEOINTERF>                                                                   ` )
            ( `  </asx:values>                                                                    ` )
            ( ` </asx:abap>                                                                       ` )
            ( `</abapGit>                                                                         ` ) ) ) ).

  ENDMETHOD.


  METHOD get_transformation_xml.

    result = cl_abap_codepage=>convert_to( concat_lines_of( sep = |\n| table = VALUE string_table(
            ( `<?xml version="1.0" encoding="utf-8"?>                                             ` )
            ( `<abapGit version="v1.0.0" serializer="LCL_OBJECT_XSLT" serializer_version="v1.0.0">` )
            ( ` <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">                   ` )
            ( `  <asx:values>                                                                     ` )
            ( `   <ATTRIBUTES>                                                                    ` )
            ( |    <XSLTDESC>{ transformation_name }</XSLTDESC>                                   | )
            ( `    <LANGU>E</LANGU>                                                               ` )
            ( `    <DESCRIPT>Demo - Result of shrinking abap2xlsx</DESCRIPT>                      ` )
            ( `   </ATTRIBUTES>                                                                   ` )
            ( `  </asx:values>                                                                    ` )
            ( ` </asx:abap>                                                                       ` )
            ( `</abapGit>                                                                         ` ) ) ) ).

  ENDMETHOD.


  METHOD zif_shrinker_abap_code_adapter~adapt_source_code.

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
          IGNORING CASE ##REGEX_POSIX.
      IF sy-subrc = 0.
        DATA(types_t_relationship) = zcl_shrinker_abap_scan=>get_whole_abap_statement(
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
              ( |  INTERFACES { p_intf }.| ) ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_shrinker_abap_code_adapter~adapt_source_code_before_rep_i.

  ENDMETHOD.


  METHOD zif_shrinker_user_exit_abapgit~is_to_be_deserialized.

    result = xsdbool( line_exists( shrinker_objects_to_recreate[ object   = object
                                                                 obj_name = obj_name ] ) ).

  ENDMETHOD.

ENDCLASS.
