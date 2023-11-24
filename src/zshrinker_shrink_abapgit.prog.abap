*&---------------------------------------------------------------------*
*& Report zshrinker_shrink_abapgit
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zshrinker_shrink_abapgit.


TYPES ty_include_program_name TYPE c LENGTH 30.


DATA: BEGIN OF dummy_select_options,
        devclass TYPE devclass,
      END OF dummy_select_options.


SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_devc VISIBLE LENGTH 60.
  SELECTION-SCREEN POSITION 62.
  SELECT-OPTIONS s_devc FOR dummy_select_options-devclass DEFAULT '$ABAPGIT*' SIGN I OPTION CP.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_def VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS incl_def TYPE ty_include_program_name DEFAULT 'ZSHRINKER_ABAPGIT_DEF'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_nb_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_nb_imp TYPE i DEFAULT 5.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_imp VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS incl_imp TYPE ty_include_program_name DEFAULT 'ZSHRINKER_ABAPGIT_IMP'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_standa VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  " IMPORTANT : do not create the standalone program because of two reasons:
  "   1) It's not needed by Shrinker. If you need it, install the official ZABAPGIT_STANDALONE program.
  "   2) It makes the ZSHRINKER_ABAPGIT* include programs have double usage, first one by this one,
  "      second by ZCL_SHRINKER_CONNECT_ABAPGIT, which interrupts many times the installation of Shrinker
  "      by abapGit with popup asking about selecting the main program for each include when activating them.
  PARAMETERS standalo TYPE syrepid DEFAULT ''. " ZSHRINKER_ABAPGIT_STANDALONE
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
  SELECTION-SCREEN COMMENT (80) t_licens VISIBLE LENGTH 63.
  SELECTION-SCREEN POSITION 65.
  PARAMETERS p_licens TYPE ty_include_program_name DEFAULT 'ZSHRINKER_ABAPGIT_LICENSE'.
SELECTION-SCREEN END OF LINE.


INITIALIZATION.
  t_devc = 'abapGit installation packages in your system'(t01).
  t_def = 'Include for class and interface definitions '(t02).
  t_nb_imp = 'Number of includes for class implementations'(t06).
  t_imp = 'Prefix of includes for class implementations'(t03).
  t_standa = 'Main abapGit executable program (~ZABAPGIT_STANDALONE)'(t04).
  t_licens = 'Include program containing the license and notice'(t05).


START-OF-SELECTION.
  TRY.
      CALL METHOD ('LCL_APP')=>('MAIN').
    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
  ASSERT 1 = 1.


CLASS lcl_app DEFINITION.
  PUBLIC SECTION.

    INTERFACES zif_shrinker_abap_code_adapter.

    CLASS-METHODS main
      RAISING
        zcx_shrinker.

  PRIVATE SECTION.
    CLASS-DATA shrinker_classes_interfaces TYPE REF TO zcl_shrinker.

    CLASS-METHODS get_mime_object
      IMPORTING
        objid         TYPE w3objid
      RETURNING
        VALUE(result) TYPE string_table.

    METHODS main_2
      RAISING
        zcx_shrinker.

    CLASS-METHODS replace_abapmerge
      CHANGING
        abap_source_code TYPE zcl_shrinker=>ty_abap_source_code.

    CLASS-METHODS split_at_regex
      IMPORTING
        val           TYPE csequence
        regex         TYPE csequence
        max_splits    TYPE i DEFAULT 0
      RETURNING
        VALUE(result) TYPE string_table .

ENDCLASS.


CLASS lcl_app IMPLEMENTATION.

  METHOD main.

    DATA(app) = NEW lcl_app( ).
    app->main_2( ).

  ENDMETHOD.


  METHOD main_2.

    " LOGIC:
    "   1. Convert all classes and interfaces into many lines.
    "      Append this interface (with anything you like instead of abapmerge 0.15.0...:
    "      ****************************************************
    "      INTERFACE lif_abapmerge_marker.
    "      * abapmerge 0.15.0 - 2023-03-19T11:05:04.771Z
    "      ENDINTERFACE.
    "      ****************************************************
    "   2. Replace all "@@abapmerge include <MIME-object-name>.w3mi.data.<MIME-file-type> > <abapstatement$$>" and its next ABAP statement
    "      with each line of <filename> replaced with <abapstatement$$> where '$$' contains the <filename> line.
    "   3. Replace all "@@abapmerge include-cua <filename> <variablename>"
    "      with the details of the each line of <filename> replaced with <abapstatement$$> where '$$' contains the <filename> line.
    "   4. Read ZABAPGIT, replace the INCLUDE lines with the code of the corresponding source units,
    "      insert all previous lines from the line 2.


    DATA(include_does_not_exist) = CONV string( 'Include/program &1 does not exist. Create it manually as empty.'(001) ).

    IF standalo IS NOT INITIAL.
      SELECT SINGLE * FROM trdir WHERE name = @standalo INTO @DATA(trdir_standalo).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = standalo.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM trdir WHERE name = @incl_def INTO @DATA(trdir_def).
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = incl_def.
    ENDIF.

    DO p_nb_imp TIMES.
      DATA(incl_impx) = EXACT progname( |{ incl_imp }{ sy-index }| ).
      SELECT SINGLE * FROM trdir WHERE name = @incl_impx INTO @DATA(trdir_imp).
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = include_does_not_exist msgv1 = incl_impx.
      ENDIF.
    ENDDO.

    "=================================
    " All classes and interfaces
    "=================================
    shrinker_classes_interfaces = zcl_shrinker=>create( me ).

    DATA(abap_of_zabapgit_oo) = shrinker_classes_interfaces->get_one_abap_code(
                                package_range        = s_devc[]
                                range_of_obj_type    = VALUE #( sign = 'I' option = 'EQ' ( low = 'CLAS' ) ( low = 'INTF' ) )
                                global_replacements  = VALUE #(
                                                        ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' start_of_abap_word = abap_true member_interface_prefix = abap_true )
                                                        ( posix_regex = `'Z(.._ABAPGIT)`     with = `'L$1` )
                                                        ( posix_regex = `\(Z(.._ABAPGIT)`    with = `(L$1` ) " SELECT * FROM (zcl_abapgit_persistence_db=>c_tabname)
                                                        ( posix_regex = `'Z(IF_APACK)`       with = `'L$1` ) " CONSTANTS c_apack_interface_cust TYPE seoclsname VALUE 'ZIF_APACK_MANIFEST' ##NO_TEXT.
                                                        ) ).

    replace_abapmerge( CHANGING abap_source_code = abap_of_zabapgit_oo-imp_abap_source_code ).

    DATA(header_lines) = VALUE string_table(
    ( `********************************************************************************` )
    ( `*                                                                               ` )
    ( `* LICENSE and NOTICE                                                            ` )
    ( `*                                                                               ` )
    ( |* See include program { p_licens WIDTH = 30 }                            | )
    ( `*                                                                               ` )
    ( `********************************************************************************` ) ).

    INSERT LINES OF header_lines
        INTO abap_of_zabapgit_oo-def_abap_source_code
        INDEX 1.

    INSERT LINES OF VALUE string_table(
        ( `` )
        ( `****************************************************` )
        ( `INTERFACE lif_abapmerge_marker.                     ` )
        ( |* ZSHRINKEDABAPGIT_RUN_SHRINKER { sy-datum(4) }     | )
        ( `ENDINTERFACE.                                       ` )
        ( `****************************************************` )
        ) INTO TABLE abap_of_zabapgit_oo-def_abap_source_code.

    DATA(syntax_check_oo) = shrinker_classes_interfaces->syntax_check( VALUE #(
                        ( `REPORT.` )
                        ( LINES OF abap_of_zabapgit_oo-def_abap_source_code )
                        ( LINES OF abap_of_zabapgit_oo-imp_abap_source_code ) ) ).

    IF syntax_check_oo-mess IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Syntax error &1'(002) msgv1 = syntax_check_oo-mess.
    ENDIF.

    "=================================
    " ZABAPGIT
    "=================================

    DATA(shrinker_zabapgit_standalone) = zcl_shrinker=>create( me ).

    DATA(abap_of_zabapgit_standalone) = shrinker_zabapgit_standalone->get_abap_for_program(
            program_name        = 'ZABAPGIT'
            global_replacements = VALUE #( ( posix_regex = '\<Z(.._ABAPGIT\w*)' with = 'L$1' start_of_abap_word = abap_true member_interface_prefix = abap_true ) ) ).

    INSERT LINES OF header_lines
        INTO abap_of_zabapgit_standalone
        INDEX 1.

    DATA(syntax_check) = shrinker_zabapgit_standalone->syntax_check( abap_of_zabapgit_standalone ).

    IF syntax_check-mess IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_shrinker EXPORTING text = 'Syntax error &1'(002) msgv1 = syntax_check-mess.
    ENDIF.

    "=================================
    " Replace code of includes and program
    "=================================
    INSERT REPORT incl_def FROM abap_of_zabapgit_oo-def_abap_source_code DIRECTORY ENTRY trdir_def.

    " To write 5 includes
    DATA(total_include_lines) = lines( abap_of_zabapgit_oo-imp_abap_source_code ) DIV p_nb_imp.
    DATA(first_line) = 1.
    DO p_nb_imp TIMES.
      IF sy-index < p_nb_imp.
        DATA(last_line) = first_line + total_include_lines - 1.
      ELSE.
        last_line = lines( abap_of_zabapgit_oo-imp_abap_source_code ).
      ENDIF.

      " make sure to not split an ABAP statement in the middle.
      WHILE last_line < lines( abap_of_zabapgit_oo-imp_abap_source_code )
          AND abap_of_zabapgit_oo-imp_abap_source_code[ last_line ] NP '*.'.
        ADD 1 TO last_line.
      ENDWHILE.

      DATA(abap_source_code) = VALUE zcl_shrinker=>ty_abap_source_code(
                    ( LINES OF header_lines )
                    ( LINES OF abap_of_zabapgit_oo-imp_abap_source_code FROM first_line TO last_line ) ).

      incl_impx = EXACT #( |{ incl_imp }{ sy-index }| ).
      trdir_imp-name = incl_impx.
      INSERT REPORT incl_impx FROM abap_source_code DIRECTORY ENTRY trdir_imp.

      first_line = last_line + 1.
    ENDDO.

    IF standalo IS NOT INITIAL.
      INSERT REPORT standalo FROM abap_of_zabapgit_standalone DIRECTORY ENTRY trdir_standalo.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


  METHOD replace_abapmerge.

*CATCH zcx_shrinker. " General exception
    " Replace all R3TR W3MI objects
    " e.g. " @@abapmerge include zabapgit_css_page_db.w3mi.data.css > lo_buf->add( '$$' ).

    FIND ALL OCCURRENCES OF
        REGEX '@@abapmerge include (\S+)\.w3mi\.data\.\S+ > (.*)$'
        IN TABLE abap_source_code
        IGNORING CASE
        RESULTS DATA(matches) ##REGEX_POSIX.

    SORT matches BY line DESCENDING.

    LOOP AT matches REFERENCE INTO DATA(match).
      ASSIGN match->submatches[ 1 ] TO FIELD-SYMBOL(<mime_object_name>).
      ASSIGN match->submatches[ 2 ] TO FIELD-SYMBOL(<abap_replacement>).
      ASSIGN abap_source_code[ match->line ] TO FIELD-SYMBOL(<abap_line>).
      DATA(mime_object_name) = CONV w3objid( to_upper( <abap_line>+<mime_object_name>-offset(<mime_object_name>-length) ) ).
      DATA(abap_replacement_template) = <abap_line>+<abap_replacement>-offset(<abap_replacement>-length).

      DATA(mime_object_content) = get_mime_object( mime_object_name ).
      DATA(abap_replacement) = VALUE string_table(
                            FOR <mime_object_line> IN mime_object_content
                            ( replace( val  = abap_replacement_template
                                       sub  = `'$$'`
                                       with = `'`
                                            && replace( val = <mime_object_line> sub = `'` with = `''` occ = 0 )
                                            && `'` ) ) ).

      DELETE abap_source_code INDEX match->line.
      INSERT LINES OF abap_replacement INTO abap_source_code INDEX match->line.
    ENDLOOP.

    " Replace CUA information (GUI status, etc.)
    " e.g. " @@abapmerge include-cua zabapgit.prog.xml > rs_cua
    FIND ALL OCCURRENCES OF
        REGEX '@@abapmerge include-cua (\S+)\.prog.xml > (.*)$'
        IN TABLE abap_source_code
        IGNORING CASE
        RESULTS matches ##REGEX_POSIX.

    SORT matches BY line DESCENDING.

    LOOP AT matches REFERENCE INTO match.

      abap_replacement = VALUE #( ).

      ASSIGN match->submatches[ 1 ] TO FIELD-SYMBOL(<program_name>).
      ASSIGN match->submatches[ 2 ] TO FIELD-SYMBOL(<variable_name>).
      ASSIGN abap_source_code[ match->line ] TO <abap_line>.

      DATA(program_name) = CONV progname( to_upper( <abap_line>+<program_name>-offset(<program_name>-length) ) ).
      DATA(variable_name) = <abap_line>+<variable_name>-offset(<variable_name>-length).

      DATA adm TYPE rsmpe_adm.
      DATA sta TYPE TABLE OF rsmpe_stat.
      DATA fun TYPE TABLE OF rsmpe_funt.
      DATA men TYPE TABLE OF rsmpe_men.
      DATA mtx TYPE TABLE OF rsmpe_mnlt.
      DATA act TYPE TABLE OF rsmpe_act.
      DATA but TYPE TABLE OF rsmpe_but.
      DATA pfk TYPE TABLE OF rsmpe_pfk.
      DATA set TYPE TABLE OF rsmpe_staf.
      DATA doc TYPE TABLE OF rsmpe_atrt.
      DATA tit TYPE TABLE OF rsmpe_titt.
      DATA biv TYPE TABLE OF rsmpe_buts.

      CALL FUNCTION 'RS_CUA_INTERNAL_FETCH'
        EXPORTING
          program         = program_name
        IMPORTING
          adm             = adm
        TABLES
          sta             = sta "SET PF-STATUS
          fun             = fun
          men             = men
          mtx             = mtx
          act             = act
          but             = but
          pfk             = pfk
          set             = set
          doc             = doc
          tit             = tit "SET TITLEBAR
          biv             = biv
        EXCEPTIONS
          not_found       = 1
          unknown_version = 2
          OTHERS          = 3.

      IF sy-subrc <> 0.
        " TODO
      ENDIF.

****************************************************
* abapmerge Pragma [include-cua] - ZABAPGIT.PROG.XML
****************************************************
*    CLEAR ls_sta.
*    ls_sta-code = 'DECIDE_DIALOG'.
*    ls_sta-modal = 'P'.
*    ls_sta-pfkcode = '000001'.
*    ls_sta-butcode = '0001'.
*    ls_sta-int_note = 'Object list decide dialog toolbar'.
*    APPEND ls_sta TO rs_cua-sta.
*    CLEAR ls_fun.
*    ls_fun-code = '&ILD'.
*    ls_fun-textno = '001'.
*    ls_fun-text_type = 'S'.
*    ls_fun-text_name = 'ICON_FILTER_UNDO'.
*    ls_fun-icon_id = '@GD@'.
*    ls_fun-fun_text = 'Reset filter'.
*    APPEND ls_fun TO rs_cua-fun.
*    CLEAR ls_but.
*    ls_but-pfk_code = '000001'.
*    ls_but-code = '0001'.
*    ls_but-no = '01'.
*    ls_but-pfno = '00'.
*    APPEND ls_but TO rs_cua-but.
*    CLEAR ls_pfk.
*    ls_pfk-code = '000001'.
*    ls_pfk-pfno = '00'.
*    ls_pfk-funcode = 'OK'.
*    ls_pfk-funno = '001'.
*    APPEND ls_pfk TO rs_cua-pfk.
*    CLEAR ls_set.
*    ls_set-status = 'DECIDE_DIALOG'.
*    ls_set-function = '&ILD'.
*    APPEND ls_set TO rs_cua-set.
*    CLEAR ls_doc.
*    ls_doc-obj_type = 'P'.
*    ls_doc-obj_code = '000001'.
*    ls_doc-modal = 'P'.
*    ls_doc-int_note = 'Object list decide dialog FK settings'.
*    APPEND ls_doc TO rs_cua-doc.
*      types tt_ref type STANDARD TABLE OF ref to data with EMPTY KEY.
      DATA(cua_table_names) = VALUE string_table( ( `sta` )
                                                  ( `fun` )
                                                  ( `but` )
                                                  ( `pfk` )
                                                  ( `set` )
                                                  ( `doc` ) ).
      DATA(cua_table_name) = VALUE string( ).
      FIELD-SYMBOLS <table> TYPE STANDARD TABLE.
      LOOP AT cua_table_names INTO cua_table_name.
        ASSIGN (cua_table_name) TO <table>.
        IF lines( <table> ) > 0.
          abap_replacement = VALUE #(
                              BASE abap_replacement
                              ( |    DATA ls_{ cua_table_name } LIKE LINE OF { variable_name }-{ cua_table_name }.| ) ).
        ENDIF.
      ENDLOOP.
      abap_replacement = VALUE #(
                        BASE abap_replacement
                        ( |    { variable_name }-adm-pfkcode = '{ adm-pfkcode }'.| ) ).
      LOOP AT cua_table_names INTO cua_table_name.
        ASSIGN (cua_table_name) TO <table>.
        IF lines( <table> ) > 0.
          DATA(rtti_struct) = CAST cl_abap_structdescr( CAST cl_abap_tabledescr( cl_abap_typedescr=>describe_by_data( <table> ) )->get_table_line_type( ) ).
          abap_replacement = VALUE #(
                            BASE abap_replacement
                            ( |    CLEAR ls_{ cua_table_name }.| ) ).
          LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
            LOOP AT rtti_struct->components REFERENCE INTO DATA(rtti_component).
              ASSIGN COMPONENT rtti_component->name OF STRUCTURE <line> TO FIELD-SYMBOL(<component>).
              IF sy-subrc = 0 AND <component> IS NOT INITIAL.
                abap_replacement = VALUE #(
                                    BASE abap_replacement
                                    ( |    ls_{ cua_table_name }-{ to_lower( rtti_component->name ) } = '{ replace( val  = |{ <component> }|
                                                                                                                    sub  = `'`
                                                                                                                    with = `''`
                                                                                                                    occ  = 0 )
                                                                                                         }'.| ) ).
              ENDIF.
            ENDLOOP.
          ENDLOOP.
          abap_replacement = VALUE #(
                            BASE abap_replacement
                            ( |    APPEND ls_{ cua_table_name } TO { variable_name }-{ cua_table_name }.| ) ).
        ENDIF.
      ENDLOOP.

      DELETE abap_source_code INDEX match->line.
      INSERT LINES OF abap_replacement INTO abap_source_code INDEX match->line.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_mime_object.

    DATA query_table TYPE TABLE OF w3query.
    DATA html_data TYPE TABLE OF w3html.
    DATA mime_data TYPE TABLE OF w3mime.
    DATA return_code TYPE w3param-ret_code.
    DATA content_type TYPE w3param-cont_type.
    DATA content_length TYPE w3param-cont_len.

    query_table = VALUE #( ( name = '_OBJECT_ID' value = objid ) ).

    CALL FUNCTION 'WWW_GET_MIME_OBJECT'
      TABLES
        query_string        = query_table
        html                = html_data
        mime                = mime_data
      CHANGING
        return_code         = return_code
        content_type        = content_type
        content_length      = content_length
      EXCEPTIONS
        object_not_found    = 1
        parameter_not_found = 2
        OTHERS              = 3.

    IF sy-subrc = 0.

      DATA(hex_string) = REDUCE string( INIT t = ``
                                        FOR <mime_line> IN mime_data
                                        NEXT t = t && <mime_line>-line ).
      DATA(xstring) = CONV xstring( hex_string ).
      xstring = xstring(content_length).
      DATA(string) = cl_abap_codepage=>convert_from( xstring ).
      result = split_at_regex( val   = string
                               regex = `\r\n|\n` ).

    ENDIF.

  ENDMETHOD.


  METHOD split_at_regex.

    FIND ALL OCCURRENCES OF REGEX regex IN val RESULTS DATA(matches) ##REGEX_POSIX.

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


  METHOD zif_shrinker_abap_code_adapter~adapt_source_code.

    DATA(zabapgit) = REF #( other_source_units[ name = 'ZABAPGIT' ] OPTIONAL ).
    IF zabapgit IS BOUND.
      INSERT LINES OF VALUE string_table(
                ( |INCLUDE { incl_def }.| )
                ( |INCLUDE { incl_imp }1.| )
                ( |INCLUDE { incl_imp }2.| )
                ( |INCLUDE { incl_imp }3.| )
                ( |INCLUDE { incl_imp }4.| )
                ( |INCLUDE { incl_imp }5.| )
                ( `` ) )
            INTO zabapgit->abap_source_code
            INDEX 3.
    ENDIF.

    DATA(zcl_abapgit_objects) = REF #( classes_interfaces-classes[ name = 'ZCL_ABAPGIT_OBJECTS' ]-includes[ method_name = 'CREATE_OBJECT' ] OPTIONAL ).
    IF zcl_abapgit_objects IS BOUND.
      " The method CREATE_OBJECT of class ZCL_ABAPGIT_OBJECTS contains these lines:
      "
      "    REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.
      "
      "    IF zcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      "      " Prevent accidental usage of object handlers in the developer version
      "      lv_class_name = |\\PROGRAM={ sy-repid }\\CLASS={ lv_class_name }|.
      "    ENDIF.
      "
      " where lv_class_name can be either the "serializer" from the serialized XML files, e.g. possibly "LCL_OBJECT_CLAS", "LCL_OBJECT_XSLT", etc.
      "    e.g. <abapGit version="v1.0.0" serializer="LCL_OBJECT_XSLT" serializer_version="v1.0.0">
      "
      " or "ZCL_ABAPGIT_OBJECT_CLAS" when calculated internally by the method CLASS_NAME of ZCL_ABAPGIT_OBJECTS.
      "
      " in the case of Shrinker, the localized class names are prefixed "LCL_ABAPGIT_OBJECT_CLAS", so replacing "LCL" with "LCL_ABAPGIT"
      " would give "LCL_ABAPGIT_ABAPGIT_OBJECT_CLAS". So using "LCL_O" to avoid this situation, and to still be valid for serializer names.

      REPLACE `REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.`
          IN TABLE zcl_abapgit_objects->abap_source_code
          WITH `REPLACE FIRST OCCURRENCE OF 'LCL_O' IN lv_class_name WITH 'LCL_ABAPGIT_O'.`
          IGNORING CASE.
*      data(find) = `REPLACE FIRST OCCURRENCE OF 'LCL' IN lv_class_name WITH 'ZCL_ABAPGIT'.`.
*      REPLACE find
*          IN TABLE zcl_abapgit_objects->abap_source_code
*          WITH |"{ find }|
*          IGNORING CASE.
    ENDIF.

  ENDMETHOD.

ENDCLASS.


CLASS ltc_main DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_main IMPLEMENTATION.
  METHOD test.

    s_devc[] = VALUE #( ( sign = 'I' option = 'CP' low = '$ABAPGIT*' ) ).
    incl_def = 'ZSHRINKER_DEMO_ABAPGIT_DEF'.
    incl_imp = 'ZSHRINKER_DEMO_ABAPGIT_IMP'.
    standalo = 'ZSHRINKER_DEMO_ABAPGIT_STANDAL'.

    lcl_app=>main( ).

  ENDMETHOD.
ENDCLASS.
