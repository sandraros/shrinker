********************************************************************************
*
* LICENSE and NOTICE
*
* See include program ZSHRINKER_ABAPGIT_LICENSE
*
********************************************************************************
      lv_found   = abap_true.
      EXIT.
    ENDLOOP.

    " Check if changed_by for function module was requested
    lt_functions = functions( ).

    LOOP AT lt_functions ASSIGNING <ls_function> WHERE funcname = to_upper( iv_extra ).
      lv_program = <ls_function>-include.
      lv_found   = abap_true.
      EXIT.
    ENDLOOP.

    SELECT unam AS user udat AS date utime AS time FROM reposrc
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE progname = lv_program
      AND r3state = 'A'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    IF mt_includes_all IS NOT INITIAL AND lv_found = abap_false.
      SELECT unam AS user udat AS date utime AS time FROM reposrc
        APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
        FOR ALL ENTRIES IN mt_includes_all
        WHERE progname = mt_includes_all-table_line
        AND r3state = 'A'.                                "#EC CI_SUBRC
    ENDIF.

    SELECT unam AS user udat AS date utime AS time FROM repotext " Program text pool
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE progname = lv_program
      AND r3state = 'A'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    SELECT vautor AS user vdatum AS date vzeit AS time FROM eudb         " GUI
      APPENDING CORRESPONDING FIELDS OF TABLE lt_stamps
      WHERE relid = 'CU'
      AND name = lv_program
      AND srtf2 = 0
      ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS.

* Screens: username not stored in D020S database table

    SORT lt_stamps BY date DESCENDING time DESCENDING.

    READ TABLE lt_stamps INDEX 1 ASSIGNING <ls_stamp>.
    IF sy-subrc = 0.
      rv_user = <ls_stamp>-user.
    ELSE.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_area     TYPE rs38l-area,
          lt_includes TYPE ty_sobj_name_tt.

    " FUGR related to change documents will be deleted by CHDO
    SELECT SINGLE fgrp FROM tcdrps INTO lv_area WHERE fgrp = ms_item-obj_name.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    lt_includes = includes( ).

    lv_area = ms_item-obj_name.

    CALL FUNCTION 'RS_FUNCTION_POOL_DELETE'
      EXPORTING
        area                   = lv_area
        suppress_popups        = abap_true
        skip_progress_ind      = abap_true
        corrnum                = iv_transport
      EXCEPTIONS
        canceled_in_corr       = 1
        enqueue_system_failure = 2
        function_exist         = 3
        not_executed           = 4
        no_modify_permission   = 5
        no_show_permission     = 6
        permission_failure     = 7
        pool_not_exist         = 8
        cancelled              = 9
        OTHERS                 = 10.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    update_where_used( lt_includes ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_program_name TYPE syrepid,
          lv_abap_version TYPE trdir-uccheck,
          lt_functions    TYPE ty_function_tt,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    lv_abap_version = get_abap_version( io_xml ).

    deserialize_xml(
      ii_xml       = io_xml
      iv_version   = lv_abap_version
      iv_package   = iv_package
      iv_transport = iv_transport ).

    io_xml->read( EXPORTING iv_name = 'FUNCTIONS'
                  CHANGING cg_data = lt_functions ).

    deserialize_functions(
      it_functions = lt_functions
      ii_log       = ii_log
      iv_version   = lv_abap_version
      iv_package   = iv_package
      iv_transport = iv_transport ).

    deserialize_includes(
      ii_xml     = io_xml
      iv_package = iv_package
      ii_log     = ii_log ).

    lv_program_name = main_name( ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      deserialize_texts( iv_prog_name = lv_program_name
                         ii_xml       = io_xml ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DYNPROS'
                  CHANGING cg_data = lt_dynpros ).

    deserialize_dynpros( lt_dynpros ).

    io_xml->read( EXPORTING iv_name = 'CUA'
                  CHANGING cg_data = ls_cua ).

    deserialize_cua( iv_program_name = lv_program_name
                     is_cua = ls_cua ).

    deserialize_function_docs(
      iv_prog_name = lv_program_name
      it_functions = lt_functions
      ii_xml       = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_pool  TYPE tlibg-area.


    lv_pool = ms_item-obj_name.
    CALL FUNCTION 'RS_FUNCTION_POOL_EXISTS'
      EXPORTING
        function_pool   = lv_pool
      EXCEPTIONS
        pool_not_exists = 1.
    rv_bool = boolc( sy-subrc <> 1 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_program TYPE program.

    lv_program = main_name( ).

    IF is_function_group_locked( )        = abap_true
    OR is_any_include_locked( )           = abap_true
    OR is_any_function_module_locked( )   = abap_true
    OR is_any_dynpro_locked( lv_program ) = abap_true
    OR is_cua_locked( lv_program )        = abap_true
    OR is_text_locked( lv_program )       = abap_true.

      rv_is_locked = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA:
      ls_item      TYPE Lif_abapgit_definitions=>ty_item,
      lt_functions TYPE ty_rs38l_incl_tt,
      lt_includes  TYPE ty_sobj_name_tt.

    FIELD-SYMBOLS:
      <ls_function> LIKE LINE OF lt_functions,
      <lv_include>  LIKE LINE OF lt_includes.

    ls_item-obj_type = 'PROG'.
    ls_item-obj_name = to_upper( iv_extra ).

    lt_functions = functions( ).

    LOOP AT lt_functions ASSIGNING <ls_function> WHERE funcname = ls_item-obj_name.
      ls_item-obj_name = <ls_function>-include.
      rv_exit = Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump( ls_item ).
      IF rv_exit = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    lt_includes = includes( ).

    LOOP AT lt_includes ASSIGNING <lv_include> WHERE table_line = ls_item-obj_name.
      rv_exit = Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump( ls_item ).
      IF rv_exit = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Otherwise covered by ZCL_ABAPGIT_OBJECTS=>JUMP

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

* function group SEUF
* function group SIFP
* function group SUNI

    DATA: lt_functions    TYPE ty_function_tt,
          ls_progdir      TYPE Lif_abapgit_sap_report=>ty_progdir,
          lv_program_name TYPE syrepid,
          lt_dynpros      TYPE ty_dynpro_tt,
          ls_cua          TYPE ty_cua.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_xml( io_xml ).

    lt_functions = serialize_functions( ).

    io_xml->add( iv_name = 'FUNCTIONS'
                 ig_data = lt_functions ).

    serialize_includes( ).

    lv_program_name = main_name( ).

    ls_progdir = Lcl_abapgit_factory=>get_sap_report( )->read_progdir( lv_program_name ).

    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts(
        iv_prog_name = lv_program_name
        ii_xml       = io_xml ).
    ENDIF.

    IF ls_progdir-subc = 'F'.
      lt_dynpros = serialize_dynpros( lv_program_name ).
      io_xml->add( iv_name = 'DYNPROS'
                   ig_data = lt_dynpros ).

      ls_cua = serialize_cua( lv_program_name ).
      io_xml->add( iv_name = 'CUA'
                   ig_data = ls_cua ).
    ENDIF.

    serialize_function_docs( iv_prog_name = lv_program_name
                             it_functions = lt_functions
                             ii_xml       = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_FUGR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IAMU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iamu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iamu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IAMU implementation.
*"* method's implementations
*include methods.
  METHOD load_mime_api.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    cl_w3_api_mime=>if_w3_api_mime~load(
      EXPORTING
        p_mime_name         = ls_mime_name
      IMPORTING
        p_mime              = mi_mime_api
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 6 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from if_w3_api_mime~load' ).
    ENDIF.

  ENDMETHOD.
  METHOD read.

    load_mime_api( ).

    mi_mime_api->get_attributes(
      IMPORTING
        p_attributes   = rs_internet_appl_comp_binary-attributes
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_attributes| ).
    ENDIF.

    CLEAR: rs_internet_appl_comp_binary-attributes-chname,
           rs_internet_appl_comp_binary-attributes-tdate,
           rs_internet_appl_comp_binary-attributes-ttime,
           rs_internet_appl_comp_binary-attributes-devclass.

    mi_mime_api->get_source(
      IMPORTING
        p_source       = rs_internet_appl_comp_binary-source
        p_datalength   = rs_internet_appl_comp_binary-length
      EXCEPTIONS
        object_invalid = 1
        mime_deleted   = 2
        error_occured  = 3
        OTHERS         = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~get_source| ).
    ENDIF.

  ENDMETHOD.
  METHOD save.

    IF Lif_abapgit_object~exists( ) = abap_true.
      load_mime_api( ).
      lock( abap_true ).

      mi_mime_api->set_source(
        EXPORTING
          p_source     = is_internet_appl_comp_binary-source
          p_datalength = is_internet_appl_comp_binary-length
        EXCEPTIONS
          object_not_changeable = 1
          object_deleted        = 2
          object_invalid        = 3
          authorize_failure     = 4
          invalid_content       = 5
          error_occured         = 6
          OTHERS                = 7 ).

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error { sy-subrc } from set_source| ).
      ENDIF.
    ELSE.
      cl_w3_api_mime=>if_w3_api_mime~create_new(
        EXPORTING
          p_mime_data             = is_internet_appl_comp_binary-attributes
          p_mime_content          = is_internet_appl_comp_binary-source
          p_datalength            = is_internet_appl_comp_binary-length
        IMPORTING
          p_mime                  = mi_mime_api
        EXCEPTIONS
          object_already_existing = 1
          object_just_created     = 2
          not_authorized          = 3
          undefined_name          = 4
          author_not_existing     = 5
          action_cancelled        = 6
          error_occured           = 7
          OTHERS                  = 8 ).

      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error { sy-subrc } from create_new| ).
      ENDIF.
    ENDIF.

    " Create_new does not update text, so set attributes explicitly
    mi_mime_api->set_attributes(
      EXPORTING
        p_attributes          = is_internet_appl_comp_binary-attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error { sy-subrc } from set_attributes| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error { sy-subrc } from save| ).
    ENDIF.

    lock( abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = read( )-attributes-chname.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    load_mime_api( ).

    mi_mime_api->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = abap_true
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~delete| ).
    ENDIF.

    mi_mime_api->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~save| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_internet_appl_comp_binary TYPE ty_internet_appl_comp_binary.
    DATA lv_xstring TYPE xstring.

    io_xml->read(
      EXPORTING
        iv_name = 'IAMU'
      CHANGING
        cg_data = ls_internet_appl_comp_binary ).

    ls_internet_appl_comp_binary-attributes-devclass = iv_package.

    IF io_xml->get_metadata( )-version = 'v2.0.0'.
      lv_xstring = Lif_abapgit_object~mo_files->read_raw( ls_internet_appl_comp_binary-extension ).

      Lcl_abapgit_convert=>xstring_to_bintab(
        EXPORTING
          iv_xstr   = lv_xstring
        IMPORTING
          et_bintab = ls_internet_appl_comp_binary-source
          ev_size   = ls_internet_appl_comp_binary-length ).
    ENDIF.

    save( ls_internet_appl_comp_binary ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_mime_name TYPE iacikeym.

    ls_mime_name = ms_item-obj_name.

    cl_w3_api_mime=>s_check_exist(
      EXPORTING
        p_mime_name = ls_mime_name
      IMPORTING
        p_exists    = rv_bool ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata         = get_metadata( ).
    rs_metadata-version = 'v2.0.0'. " Serialization v2, separate data file
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_internet_appl_comp_binary TYPE ty_internet_appl_comp_binary.
    DATA lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <lv_data> LIKE LINE OF ls_internet_appl_comp_binary-source.

    ls_internet_appl_comp_binary = read( ).

    " Seriazation v2, separate data file
    LOOP AT ls_internet_appl_comp_binary-source ASSIGNING <lv_data>.
      lv_xstring = lv_xstring && <lv_data>-line.
    ENDLOOP.
    lv_xstring = lv_xstring(ls_internet_appl_comp_binary-length).

    CLEAR: ls_internet_appl_comp_binary-source, ls_internet_appl_comp_binary-length.

    ls_internet_appl_comp_binary-extension = get_extension(
      iv_name = ls_internet_appl_comp_binary-attributes-longname
      iv_data = lv_xstring ).

    Lif_abapgit_object~mo_files->add_raw(
      iv_data = lv_xstring
      iv_ext  = ls_internet_appl_comp_binary-extension ).

    io_xml->add( iv_name = 'IAMU'
                 ig_data = ls_internet_appl_comp_binary ).

  ENDMETHOD.
  METHOD get_extension.

    CONSTANTS:
      lc_jpg TYPE xstring VALUE 'FFD8FF',
      lc_png TYPE xstring VALUE '89504E470D0A1A0A',
      lc_gif TYPE xstring VALUE '47494638',
      lc_bmp TYPE xstring VALUE '424D'.

    DATA lv_len TYPE i.

    " Try to derive type of MIME object from the long name
    FIND REGEX '\.(\w)$' IN iv_name SUBMATCHES rv_extension.
    IF sy-subrc = 0.
      rv_extension = to_lower( rv_extension ).
    ELSEIF Lcl_abapgit_utils=>is_binary( iv_data ) = abap_true.
      " Use magic numbers to detect common file types
      lv_len = xstrlen( iv_data ).
      IF lv_len > 3 AND iv_data(3) = lc_jpg.
        rv_extension = 'jpg'.
      ELSEIF lv_len > 8 AND iv_data(8) = lc_png.
        rv_extension = 'png'.
      ELSEIF lv_len > 4 AND iv_data(4) = lc_gif.
        rv_extension = 'git'.
      ELSEIF lv_len > 2 AND iv_data(2) = lc_bmp.
        rv_extension = 'bmp'.
      ELSE.
        rv_extension = 'bin'.
      ENDIF.
    ELSE.
      rv_extension = 'txt'.
    ENDIF.

  ENDMETHOD.
  METHOD lock.

    " As a side effect this method removes also existing locks
    mi_mime_api->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changable
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_mime~set_changeable| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IAMU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IARP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iarp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iarp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IARP implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    ms_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD read.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_load( ).
    es_attributes = w3_api_get_attributes( li_resource ).

    CLEAR: es_attributes-chname,
           es_attributes-tdate,
           es_attributes-ttime,
           es_attributes-devclass.

    et_parameters = w3_api_get_parameters( li_resource ).

  ENDMETHOD.
  METHOD save.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_create_new( is_attributes ).

    w3_api_set_attributes(
        ii_resource   = li_resource
        is_attributes = is_attributes ).

    w3_api_set_parameters(
        ii_resource   = li_resource
        it_parameters = it_parameters ).

    w3_api_save( li_resource ).

    " Release locks
    w3_api_set_changeable(
      ii_resource   = li_resource
      iv_changeable = abap_false ).

  ENDMETHOD.
  METHOD w3_api_create_new.

    cl_w3_api_resource=>if_w3_api_resource~create_new(
      EXPORTING
        p_resource_data         = is_attributes
      IMPORTING
        p_resource              = ri_resource
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        OTHERS                  = 8 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_resource~create_new. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_delete.

    ii_resource->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~delete. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_get_attributes.

    ii_resource->get_attributes(
      IMPORTING
        p_attributes     = rs_attributes
      EXCEPTIONS
        object_invalid   = 1
        resource_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_resource~get_attributes. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_get_parameters.

    ii_resource->get_parameters(
      IMPORTING
        p_parameters     = rt_parameters
      EXCEPTIONS
        object_invalid   = 1
        resource_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_resource~get_parameters. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_load.

    cl_w3_api_resource=>if_w3_api_resource~load(
      EXPORTING
        p_resource_name     = ms_name
      IMPORTING
        p_resource          = ri_resource
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from w3api_resource~load. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_save.

    ii_resource->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~save. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_attributes.

    ii_resource->set_attributes(
      EXPORTING
        p_attributes          = is_attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_resource~set_attributes. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_changeable.

    ii_resource->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changeable
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~set_changeable. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_parameters.

    ii_resource->set_parameters(
      EXPORTING
        p_parameters          = it_parameters
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_parameter     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_resource~set_parameters. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_attributes TYPE w3resoattr.

    read( IMPORTING es_attributes = ls_attributes ).

    rv_user = ls_attributes-chname.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_resource TYPE REF TO if_w3_api_resource.

    li_resource = w3_api_load( ).
    w3_api_set_changeable( li_resource ).
    w3_api_delete( li_resource ).
    w3_api_save( li_resource ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attributes       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.

    TRY.
        w3_api_load( ).
        rv_bool = abap_true.

      CATCH Lcx_abapgit_exception INTO lx_error.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_attr       TYPE w3resoattr,
          lt_parameters TYPE w3resopara_tabletype.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attributes       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IARP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IASP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iasp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iasp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IASP implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_name = ms_item-obj_name.

  ENDMETHOD.
  METHOD read.

    DATA: li_service TYPE REF TO if_w3_api_service.

    li_service = w3_api_load( ).
    es_attr = w3_api_get_attributes( li_service ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    et_parameters = w3_api_get_parameters( li_service ).

  ENDMETHOD.
  METHOD save.

    DATA: li_service TYPE REF TO if_w3_api_service.

    li_service = w3_api_create_new( is_attr ).

    w3_api_set_attributes(
        ii_service    = li_service
        is_attributes = is_attr ).

    w3_api_set_parameters(
        ii_service    = li_service
        it_parameters = it_parameters ).

    w3_api_save( li_service ).

    " Release locks
    w3_api_set_changeable(
      ii_service    = li_service
      iv_changeable = abap_false ).

  ENDMETHOD.
  METHOD w3_api_create_new.

    cl_w3_api_service=>if_w3_api_service~create_new(
      EXPORTING
        p_service_data = is_attributes
      IMPORTING
        p_service      = ri_service
      EXCEPTIONS
        object_already_existing = 1
        object_just_created     = 2
        not_authorized          = 3
        undefined_name          = 4
        author_not_existing     = 5
        action_cancelled        = 6
        error_occured           = 7
        invalid_parameter       = 8
        OTHERS                  = 9 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_service~create_new. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_delete.

    ii_service->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~delete. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_get_attributes.

    ii_service->get_attributes( IMPORTING p_attributes = rs_attributes ).

  ENDMETHOD.
  METHOD w3_api_get_parameters.

    ii_service->get_parameters( IMPORTING p_parameters = rt_parameters ).

  ENDMETHOD.
  METHOD w3_api_load.

    cl_w3_api_service=>if_w3_api_service~load(
      EXPORTING
        p_service_name     = mv_name
      IMPORTING
        p_service          = ri_service
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from w3api_service~load' ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_save.

    ii_service->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~save. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_attributes.

    ii_service->set_attributes(
      EXPORTING
        p_attributes          = is_attributes
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_service~set_attributes. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_changeable.

    ii_service->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changeable
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_object~set_changeable. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_parameters.

    ii_service->set_parameters(
      EXPORTING
        p_parameters          = it_parameters
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_parameter     = 5
        error_occured         = 6
        OTHERS                = 7 ).
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |error from if_w3_api_service~set_parameters. Subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " todo
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_service TYPE REF TO if_w3_api_service.

    li_service = w3_api_load( ).

    w3_api_set_changeable( li_service ).
    w3_api_delete( li_service ).
    w3_api_save( li_service ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).
    io_xml->read( EXPORTING iv_name = 'PARAMETERS'
                  CHANGING cg_data = lt_parameters ).

    ls_attr-devclass = iv_package.
    save( is_attr       = ls_attr
          it_parameters = lt_parameters ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lx_error TYPE REF TO Lcx_abapgit_exception.

    TRY.
        w3_api_load( ).
        rv_bool = abap_true.

      CATCH Lcx_abapgit_exception INTO lx_error.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_attr       TYPE w3servattr,
          lt_parameters TYPE w3servpara_tabletype.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr       = ls_attr
                    et_parameters = lt_parameters ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).
    io_xml->add( iv_name = 'PARAMETERS'
                 ig_data = lt_parameters ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IASP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IATU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iatu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iatu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IATU implementation.
*"* method's implementations
*include methods.
  METHOD read.

    DATA: li_template TYPE REF TO if_w3_api_template,
          lt_source   TYPE w3htmltabtype,
          ls_name     TYPE iacikeyt.

    ls_name = ms_item-obj_name.

    li_template = w3_api_load( ls_name ).

    es_attr = w3_api_get_attributes( li_template ).

    CLEAR: es_attr-chname,
           es_attr-tdate,
           es_attr-ttime,
           es_attr-devclass.

    lt_source = w3_api_get_source( li_template ).

    CONCATENATE LINES OF lt_source INTO ev_source RESPECTING BLANKS.

  ENDMETHOD.
  METHOD save.

    DATA: lt_source   TYPE w3htmltabtype,
          lv_source   TYPE string,
          li_template TYPE REF TO if_w3_api_template.


    li_template = w3_api_create_new( is_attr ).

    w3_api_set_attributes( ii_template = li_template
                           is_attr     = is_attr ).

    lv_source = iv_source.
    WHILE strlen( lv_source ) >= 255.
      APPEND lv_source(255) TO lt_source.
      lv_source = lv_source+255.
    ENDWHILE.
    IF NOT lv_source IS INITIAL.
      APPEND lv_source TO lt_source.
    ENDIF.

    w3_api_set_source( ii_template = li_template
                       it_source   = lt_source ).

    w3_api_save( li_template ).

    " Release locks
    w3_api_set_changeable(
      ii_template   = li_template
      iv_changeable = abap_false ).

  ENDMETHOD.
  METHOD w3_api_create_new.

    cl_w3_api_template=>if_w3_api_template~create_new(
      EXPORTING
        p_template_data          = is_template_data
        p_program_name           = is_template_data-programm
      IMPORTING
        p_template               = ri_template
      EXCEPTIONS
        object_already_existing  = 1
        object_just_created      = 2
        not_authorized           = 3
        undefined_name           = 4
        author_not_existing      = 5
        action_cancelled         = 6
        error_occured            = 7
        user_error               = 8
        OTHERS                   = 9 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~create_new subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_delete.

    ii_template->if_w3_api_object~delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~delete subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_get_attributes.

    ii_template->get_attributes(
      IMPORTING
        p_attributes     = rs_attributes
      EXCEPTIONS
        object_invalid   = 1
        template_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~get_attributes subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_get_source.

    ii_template->get_source(
      IMPORTING
        p_source         = rt_source
      EXCEPTIONS
        object_invalid   = 1
        template_deleted = 2
        error_occured    = 3
        OTHERS           = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~get_source subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_load.

    cl_w3_api_template=>if_w3_api_template~load(
      EXPORTING
        p_template_name     = is_name
      IMPORTING
        p_template          = ri_template
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        error_occured       = 3
        OTHERS              = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from if_w3_api_template~load subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_save.

    ii_template->if_w3_api_object~save(
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~save subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_attributes.

    ii_template->set_attributes(
      EXPORTING
        p_attributes          = is_attr
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        author_not_existing   = 4
        authorize_failure     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~set_attributes subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_changeable.

    ii_template->if_w3_api_object~set_changeable(
      EXPORTING
        p_changeable                 = iv_changeable
      EXCEPTIONS
        action_cancelled             = 1
        object_locked_by_other_user  = 2
        permission_failure           = 3
        object_already_changeable    = 4
        object_already_unlocked      = 5
        object_just_created          = 6
        object_deleted               = 7
        object_modified              = 8
        object_not_existing          = 9
        object_invalid               = 10
        error_occured                = 11
        OTHERS                       = 12 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~set_changeable subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_source.

    ii_template->set_source(
      EXPORTING
        p_source              = it_source
      EXCEPTIONS
        object_not_changeable = 1
        object_deleted        = 2
        object_invalid        = 3
        authorize_failure     = 4
        invalid_parameter     = 5
        error_occured         = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_template~set_source subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_attributes TYPE w3tempattr.

    read( IMPORTING es_attr = ls_attributes ).

    rv_user = ls_attributes-chname.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_template TYPE REF TO if_w3_api_template,
          ls_name     TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    li_template = w3_api_load( ls_name ).

    w3_api_set_changeable( ii_template   = li_template
                           iv_changeable = abap_true ).

    w3_api_delete( li_template ).

    w3_api_save( li_template ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING cg_data = ls_attr ).

    lv_source = Lif_abapgit_object~mo_files->read_string( 'html' ).

    ls_attr-devclass = iv_package.
    save( is_attr   = ls_attr
          iv_source = lv_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_name TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    cl_w3_api_template=>s_check_exist( EXPORTING p_template_name = ls_name
                                       IMPORTING p_exists        = rv_bool ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_attr   TYPE w3tempattr,
          lv_source TYPE string.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    read( IMPORTING es_attr   = ls_attr
                    ev_source = lv_source ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

    Lif_abapgit_object~mo_files->add_string(
      iv_ext    = 'html'
      iv_string = lv_source ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IATU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IAXU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iaxu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iaxu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IAXU implementation.
*"* method's implementations
*include methods.
  METHOD read.

    DATA: ls_name TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name = ls_name
                 IMPORTING es_attr = rs_attr ).

    CLEAR: rs_attr-chname,
           rs_attr-tdate,
           rs_attr-ttime,
           rs_attr-devclass.

  ENDMETHOD.
  METHOD save.

    DATA: lo_xml_api TYPE REF TO object.

    lo_xml_api = w3_api_create_new( is_attr ).

    w3_api_save( lo_xml_api ).

    w3_api_set_changeable( io_xml_api    = lo_xml_api
                           iv_changeable = abap_false ).

  ENDMETHOD.
  METHOD w3_api_create_new.

    DATA: lr_xml_api TYPE REF TO data.

    FIELD-SYMBOLS: <lg_xml_api> TYPE any.

    CREATE DATA lr_xml_api TYPE REF TO ('CL_W3_API_XML3').
    ASSIGN lr_xml_api->* TO <lg_xml_api>.
    ASSERT sy-subrc = 0.

    CALL METHOD ('CL_W3_API_XML3')=>create_new
      EXPORTING
        p_source_style_2006     = mv_source_style_2006
        p_xml_data              = is_attr
        p_generator_class       = mv_generator_class
        p_program_name          = is_attr-programm
      IMPORTING
        p_xml                   = <lg_xml_api>
      EXCEPTIONS
        undefined_name          = 1
        error_occured           = 2
        object_already_existing = 3
        not_authorized          = 4
        action_cancelled        = 5
        OTHERS                  = 6.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_xml3~create_new subrc={ sy-subrc }| ).
    ENDIF.

    ro_xml_api ?= <lg_xml_api>.

  ENDMETHOD.
  METHOD w3_api_delete.

    CALL METHOD io_xml_api->('IF_W3_API_OBJECT~DELETE')
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        error_occured         = 4
        OTHERS                = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_xml3~delete subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_load.

    DATA: lr_xml_api TYPE REF TO data.

    FIELD-SYMBOLS: <lg_xml_api> TYPE any.

    CREATE DATA lr_xml_api TYPE REF TO ('CL_W3_API_XML3').
    ASSIGN lr_xml_api->* TO <lg_xml_api>.
    ASSERT sy-subrc = 0.

    CALL METHOD ('CL_W3_API_XML3')=>load
      EXPORTING
        p_xml_name          = is_name
      IMPORTING
        p_attributes        = es_attr
        p_xml               = <lg_xml_api>
      EXCEPTIONS
        object_not_existing = 1
        permission_failure  = 2
        data_corrupt        = 3
        error_occured       = 4
        OTHERS              = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_xml3~load subrc={ sy-subrc }| ).
    ENDIF.

    eo_xml_api ?= <lg_xml_api>.

  ENDMETHOD.
  METHOD w3_api_save.

    CALL METHOD io_xml_api->('IF_W3_API_OBJECT~SAVE')
      EXCEPTIONS
        object_invalid        = 1
        object_not_changeable = 2
        action_cancelled      = 3
        permission_failure    = 4
        not_changed           = 5
        data_invalid          = 6
        error_occured         = 7
        OTHERS                = 8.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_xml3~save subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD w3_api_set_changeable.

    CALL METHOD io_xml_api->('IF_W3_API_OBJECT~SET_CHANGEABLE')
      EXPORTING
        p_changeable                = iv_changeable
      EXCEPTIONS
        action_cancelled            = 1
        object_locked_by_other_user = 2
        permission_failure          = 3
        object_already_changeable   = 4
        object_already_unlocked     = 5
        object_just_created         = 6
        object_deleted              = 7
        object_modified             = 8
        object_not_existing         = 9
        object_invalid              = 10
        error_occured               = 11
        OTHERS                      = 12.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error from w3_api_xml3~set_changeable subrc={ sy-subrc }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = read( )-chname.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_xml_api TYPE REF TO object,
          ls_name    TYPE iacikeyt.

    ls_name = ms_item-obj_name.

    w3_api_load( EXPORTING is_name    = ls_name
                 IMPORTING eo_xml_api = lo_xml_api ).

    w3_api_set_changeable( io_xml_api    = lo_xml_api
                           iv_changeable = abap_true ).

    w3_api_delete( lo_xml_api ).

    w3_api_save( lo_xml_api ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_attr TYPE w3tempattr.


    io_xml->read( EXPORTING iv_name = 'ATTR'
                  CHANGING  cg_data = ls_attr ).

    ls_attr-devclass = iv_package.

    IF Lif_abapgit_object~exists( ) = abap_true.
      Lif_abapgit_object~delete( iv_package   = iv_package
                                 iv_transport = iv_transport ).
    ENDIF.

    save( ls_attr ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_name  TYPE iacikeyt.


    ls_name = ms_item-obj_name.

    CALL METHOD ('CL_W3_API_XML3')=>s_check_exist
      EXPORTING
        p_xml_name = ls_name
      IMPORTING
        p_exists   = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_attr TYPE w3tempattr.


    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    ls_attr = read( ).

    io_xml->add( iv_name = 'ATTR'
                 ig_data = ls_attr ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IAXU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_IOBJ <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_iobj=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_iobj=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_IOBJ implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_objna TYPE c LENGTH 30,
          lr_viobj TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_tstpnm> TYPE any,
      <lg_viobj>  TYPE any.

    lv_objna = ms_item-obj_name.

    TRY.
        CREATE DATA lr_viobj TYPE ('RSD_S_VIOBJ').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_viobj->* TO <lg_viobj>.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm         = lv_objna
        i_objvers        = 'A'
      IMPORTING
        e_s_viobj        = <lg_viobj>
      EXCEPTIONS
        iobj_not_found   = 1
        illegal_input    = 2
        bct_comp_invalid = 3
        not_authorized   = 4
        OTHERS           = 5.
    IF sy-subrc = 0.
      ASSIGN COMPONENT 'TSTPNM' OF STRUCTURE <lg_viobj> TO <lg_tstpnm>.
      rv_user = <lg_tstpnm>.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    TYPES: BEGIN OF ty_iobj,
             objnm TYPE c LENGTH 30.
    TYPES END OF ty_iobj.

    DATA: lt_iobjname TYPE STANDARD TABLE OF ty_iobj,
          lv_subrc    TYPE sy-subrc.

    APPEND ms_item-obj_name TO lt_iobjname.

    CALL FUNCTION 'RSDG_IOBJ_MULTI_DELETE'
      EXPORTING
        i_t_iobjnm = lt_iobjname
      IMPORTING
        e_subrc    = lv_subrc.

    IF lv_subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error when deleting InfoObject { ms_item-obj_name }| ).
    ENDIF.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lr_details                  TYPE REF TO data,
      lr_infoobj                  TYPE REF TO data,
      ls_return                   TYPE bapiret2,
      lt_return                   TYPE STANDARD TABLE OF bapiret2,
      lr_compounds                TYPE REF TO data,
      lr_attributes               TYPE REF TO data,
      lr_navigationattributes     TYPE REF TO data,
      lr_atrnavinfoprovider       TYPE REF TO data,
      lr_hierarchycharacteristics TYPE REF TO data,
      lr_elimination              TYPE REF TO data,
      lr_hanafieldsmapping        TYPE REF TO data,
      lr_xxlattributes            TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_details>                  TYPE any,
      <lt_compounds>                TYPE STANDARD TABLE,
      <lt_attributes>               TYPE STANDARD TABLE,
      <lt_navigationattributes>     TYPE STANDARD TABLE,
      <lt_atrnavinfoprovider>       TYPE STANDARD TABLE,
      <lt_hierarchycharacteristics> TYPE STANDARD TABLE,
      <lt_elimination>              TYPE STANDARD TABLE,
      <lt_hanafieldsmapping>        TYPE STANDARD TABLE,
      <lt_xxlattributes>            TYPE STANDARD TABLE,
      <lg_infoobject>               TYPE data,
      <lt_infoobjects>              TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details TYPE ('BAPI6108').
        CREATE DATA lr_compounds TYPE STANDARD TABLE OF ('BAPI6108CM').
        CREATE DATA lr_attributes TYPE STANDARD TABLE OF ('BAPI6108AT').
        CREATE DATA lr_navigationattributes TYPE STANDARD TABLE OF ('BAPI6108AN').
        CREATE DATA lr_atrnavinfoprovider TYPE STANDARD TABLE OF ('BAPI6108NP').
        CREATE DATA lr_hierarchycharacteristics TYPE STANDARD TABLE OF ('BAPI6108HC').
        CREATE DATA lr_elimination TYPE STANDARD TABLE OF ('BAPI6108IE').
        CREATE DATA lr_hanafieldsmapping TYPE STANDARD TABLE OF ('BAPI6108HANA_MAP').
        CREATE DATA lr_xxlattributes TYPE STANDARD TABLE OF ('BAPI6108ATXXL').
        CREATE DATA lr_infoobj TYPE STANDARD TABLE OF ('BAPI6108').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.
    ASSIGN lr_compounds->* TO <lt_compounds>.
    ASSIGN lr_attributes->* TO <lt_attributes>.
    ASSIGN lr_navigationattributes->* TO <lt_navigationattributes>.
    ASSIGN lr_atrnavinfoprovider->* TO <lt_atrnavinfoprovider>.
    ASSIGN lr_hierarchycharacteristics->* TO <lt_hierarchycharacteristics>.
    ASSIGN lr_elimination->* TO <lt_elimination>.
    ASSIGN lr_hanafieldsmapping->* TO <lt_hanafieldsmapping>.
    ASSIGN lr_xxlattributes->* TO <lt_xxlattributes>.
    ASSIGN lr_infoobj->* TO <lt_infoobjects>.

    io_xml->read( EXPORTING iv_name = 'IOBJ'
                  CHANGING cg_data = <lg_details> ).

    io_xml->read( EXPORTING iv_name = 'COMPOUNDS'
                  CHANGING  cg_data = <lt_compounds> ).

    io_xml->read( EXPORTING iv_name = 'ATTRIBUTES'
                  CHANGING  cg_data = <lt_attributes> ).

    io_xml->read( EXPORTING iv_name = 'NAVIGATION_ATTRIBUTES'
                  CHANGING  cg_data = <lt_navigationattributes> ).

    io_xml->read( EXPORTING iv_name = 'ATTR_NAVIGATION'
                  CHANGING  cg_data = <lt_atrnavinfoprovider> ).

    io_xml->read( EXPORTING iv_name = 'HIERARCHY'
                  CHANGING  cg_data = <lt_hierarchycharacteristics> ).

    io_xml->read( EXPORTING iv_name = 'ELIMINATION'
                  CHANGING  cg_data = <lt_elimination> ).

    io_xml->read( EXPORTING iv_name = 'HANA_FIELDS_MAPPING'
                  CHANGING  cg_data = <lt_hanafieldsmapping> ).

    io_xml->read( EXPORTING iv_name = 'XXL_ATTRIBUTES'
                  CHANGING  cg_data = <lt_xxlattributes> ).

    " Number ranges are local (should not have been serialized)
    clear_field( EXPORTING iv_fieldname = 'NUMBRANR'
                 CHANGING  cg_metadata  = <lg_details> ).

    TRY.

        ASSIGN
          COMPONENT 'INFOOBJECT'
          OF STRUCTURE <lg_details>
          TO <lg_infoobject>.
        ASSERT sy-subrc = 0.

        IF Lif_abapgit_object~exists( ) = abap_false.
          CALL FUNCTION 'BAPI_IOBJ_CREATE'
            EXPORTING
              details                  = <lg_details>
            IMPORTING
              return                   = ls_return
            TABLES
              compounds                = <lt_compounds>
              attributes               = <lt_attributes>
              navigationattributes     = <lt_navigationattributes>
              atrnavinfoprovider       = <lt_atrnavinfoprovider>
              hierarchycharacteristics = <lt_hierarchycharacteristics>
              elimination              = <lt_elimination>
              hanafieldsmapping        = <lt_hanafieldsmapping>
              xxlattributes            = <lt_xxlattributes>.
        ELSE.
          CALL FUNCTION 'BAPI_IOBJ_CHANGE'
            EXPORTING
              infoobject               = <lg_infoobject>
              details                  = <lg_details>
            IMPORTING
              return                   = ls_return
            TABLES
              compounds                = <lt_compounds>
              attributes               = <lt_attributes>
              navigationattributes     = <lt_navigationattributes>
              atrnavinfoprovider       = <lt_atrnavinfoprovider>
              hierarchycharacteristics = <lt_hierarchycharacteristics>
              elimination              = <lt_elimination>
              hanafieldsmapping        = <lt_hanafieldsmapping>
              xxlattributes            = <lt_xxlattributes>.
        ENDIF.

        IF ls_return-type = 'E'.
          Lcx_abapgit_exception=>raise( |Error when creating iobj: { ls_return-message }| ).
        ENDIF.

        APPEND <lg_infoobject> TO <lt_infoobjects>.

        CALL FUNCTION 'BAPI_IOBJ_ACTIVATE_MULTIPLE'
          TABLES
            infoobjects = <lt_infoobjects>
            return      = lt_return.

        READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
        IF sy-subrc = 0.
          Lcx_abapgit_exception=>raise( |Error when activating iobj: { ls_return-message }| ).
        ENDIF.

      CATCH cx_sy_dyn_call_illegal_func.
        Lcx_abapgit_exception=>raise( |Necessary BW function modules not found| ).
    ENDTRY.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE c LENGTH 30.

    SELECT SINGLE iobjnm
      FROM ('RSDIOBJ')
      INTO lv_iobjnm
      WHERE iobjnm = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    DATA: lv_objna TYPE c LENGTH 30,
          lr_viobj TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_objstat> TYPE any,
      <lg_viobj>   TYPE any.

    lv_objna = ms_item-obj_name.

    TRY.
        CREATE DATA lr_viobj TYPE ('RSD_S_VIOBJ').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_viobj->* TO <lg_viobj>.

    CALL FUNCTION 'RSD_IOBJ_GET'
      EXPORTING
        i_iobjnm  = lv_objna
        i_objvers = 'A'
      IMPORTING
        e_s_viobj = <lg_viobj>.

    ASSIGN COMPONENT 'OBJSTAT' OF STRUCTURE <lg_viobj> TO <lg_objstat>.

    IF <lg_objstat> = 'ACT' AND sy-subrc = 0.
      rv_active = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = ms_item-obj_name.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_BIW_PROV'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lv_iobjnam                  TYPE rsiobjnm,
      ls_return                   TYPE bapiret2,
      lr_details                  TYPE REF TO data,
      lr_compounds                TYPE REF TO data,
      lr_attributes               TYPE REF TO data,
      lr_navigationattributes     TYPE REF TO data,
      lr_atrnavinfoprovider       TYPE REF TO data,
      lr_hierarchycharacteristics TYPE REF TO data,
      lr_elimination              TYPE REF TO data,
      lr_hanafieldsmapping        TYPE REF TO data,
      lr_xxlattributes            TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_details>                  TYPE any,
      <lt_compounds>                TYPE STANDARD TABLE,
      <lt_attributes>               TYPE STANDARD TABLE,
      <lt_navigationattributes>     TYPE STANDARD TABLE,
      <lt_atrnavinfoprovider>       TYPE STANDARD TABLE,
      <lt_hierarchycharacteristics> TYPE STANDARD TABLE,
      <lt_elimination>              TYPE STANDARD TABLE,
      <lt_hanafieldsmapping>        TYPE STANDARD TABLE,
      <lt_xxlattributes>            TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details TYPE ('BAPI6108').
        CREATE DATA lr_compounds TYPE STANDARD TABLE OF ('BAPI6108CM').
        CREATE DATA lr_attributes TYPE STANDARD TABLE OF ('BAPI6108AT').
        CREATE DATA lr_navigationattributes TYPE STANDARD TABLE OF ('BAPI6108AN').
        CREATE DATA lr_atrnavinfoprovider TYPE STANDARD TABLE OF ('BAPI6108NP').
        CREATE DATA lr_hierarchycharacteristics TYPE STANDARD TABLE OF ('BAPI6108HC').
        CREATE DATA lr_elimination TYPE STANDARD TABLE OF ('BAPI6108IE').
        CREATE DATA lr_hanafieldsmapping TYPE STANDARD TABLE OF ('BAPI6108HANA_MAP').
        CREATE DATA lr_xxlattributes TYPE STANDARD TABLE OF ('BAPI6108ATXXL').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |IOBJ is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.
    ASSIGN lr_compounds->* TO <lt_compounds>.
    ASSIGN lr_attributes->* TO <lt_attributes>.
    ASSIGN lr_navigationattributes->* TO <lt_navigationattributes>.
    ASSIGN lr_atrnavinfoprovider->* TO <lt_atrnavinfoprovider>.
    ASSIGN lr_hierarchycharacteristics->* TO <lt_hierarchycharacteristics>.
    ASSIGN lr_elimination->* TO <lt_elimination>.
    ASSIGN lr_hanafieldsmapping->* TO <lt_hanafieldsmapping>.
    ASSIGN lr_xxlattributes->* TO <lt_xxlattributes>.

    lv_iobjnam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_IOBJ_GETDETAIL'
      EXPORTING
        infoobject               = lv_iobjnam
      IMPORTING
        details                  = <lg_details>
        return                   = ls_return
      TABLES
        compounds                = <lt_compounds>
        attributes               = <lt_attributes>
        navigationattributes     = <lt_navigationattributes>
        atrnavinfoprovider       = <lt_atrnavinfoprovider>
        hierarchycharacteristics = <lt_hierarchycharacteristics>
        elimination              = <lt_elimination>
        hanafieldsmapping        = <lt_hanafieldsmapping>
        xxlattributes            = <lt_xxlattributes>.

    IF ls_return-type = 'E'.
      Lcx_abapgit_exception=>raise( |Error getting details of InfoObject: { ls_return-message }| ).
    ENDIF.

    clear_field( EXPORTING iv_fieldname = 'TSTPNM'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'TIMESTMP'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'DBROUTID'
                 CHANGING  cg_metadata  = <lg_details> ).

    " Number ranges are local
    clear_field( EXPORTING iv_fieldname = 'NUMBRANR'
                 CHANGING  cg_metadata  = <lg_details> ).

    io_xml->add( iv_name = 'IOBJ'
                 ig_data = <lg_details> ).

    io_xml->add( iv_name = 'COMPOUNDS'
                 ig_data = <lt_compounds> ).

    io_xml->add( iv_name = 'ATTRIBUTES'
                 ig_data = <lt_attributes> ).

    io_xml->add( iv_name = 'NAVIGATION_ATTRIBUTES'
                 ig_data = <lt_navigationattributes> ).

    io_xml->add( iv_name = 'ATTR_NAVIGATION'
                  ig_data = <lt_atrnavinfoprovider> ).

    io_xml->add( iv_name = 'HIERARCHY'
                 ig_data = <lt_hierarchycharacteristics> ).

    io_xml->add( iv_name = 'ELIMINATION'
                 ig_data = <lt_elimination> ).

    io_xml->add( iv_name = 'HANA_FIELDS_MAPPING'
                 ig_data = <lt_hanafieldsmapping> ).

    io_xml->add( iv_name = 'XXL_ATTRIBUTES'
                 ig_data = <lt_xxlattributes> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_IOBJ implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_JOBD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_jobd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_jobd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_JOBD implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lr_job_definition TYPE REF TO data,
          lo_job_definition TYPE REF TO object,
          lv_name           TYPE ty_jd_name.

    FIELD-SYMBOLS: <lg_job_definition> TYPE any,
                   <lg_field>          TYPE any.

    lv_name = ms_item-obj_name.

    TRY.
        CREATE DATA lr_job_definition TYPE ('CL_JR_JOB_DEFINITION=>TY_JOB_DEFINITION').
        ASSIGN lr_job_definition->* TO <lg_job_definition>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT lo_job_definition TYPE ('CL_JR_JOB_DEFINITION')
          EXPORTING
            im_jd_name = lv_name.

        CALL METHOD lo_job_definition->('GET_JD_ATTRIBUTES')
          IMPORTING
            ex_jd_attributes = <lg_job_definition>.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        IF sy-subrc = 0.
          rv_user = <lg_field>.
        ENDIF.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_job_definition TYPE REF TO object,
          lv_name           TYPE c LENGTH 32.

    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_job_definition TYPE ('CL_JR_JOB_DEFINITION')
          EXPORTING
            im_jd_name = lv_name.

        CALL METHOD lo_job_definition->('DELETE_JD').

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error deleting JOBD| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lr_job_definition TYPE REF TO data,
          lo_job_definition TYPE REF TO object,
          lv_name           TYPE ty_jd_name.

    FIELD-SYMBOLS: <lg_job_definition> TYPE any,
                   <lg_field>          TYPE any.


    lv_name = ms_item-obj_name.

    TRY.
        CREATE DATA lr_job_definition TYPE ('CL_JR_JOB_DEFINITION=>TY_JOB_DEFINITION').
        ASSIGN lr_job_definition->* TO <lg_job_definition>.
        ASSERT sy-subrc = 0.

        io_xml->read(
          EXPORTING
            iv_name = 'JOBD'
          CHANGING
            cg_data = <lg_job_definition> ).

        CREATE OBJECT lo_job_definition TYPE ('CL_JR_JOB_DEFINITION')
          EXPORTING
            im_jd_name = lv_name.

        ASSIGN COMPONENT 'JDPACKAGE' OF STRUCTURE <lg_job_definition> TO <lg_field>.

        <lg_field> = iv_package.

        CALL METHOD lo_job_definition->('CREATE_JD')
          EXPORTING
            im_jd_attributes = <lg_job_definition>.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error deserializing JOBD| ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_name TYPE ty_jd_name.

    lv_name = ms_item-obj_name.

    TRY.
        CALL METHOD ('CL_JR_JD_MANAGER')=>('CHECK_JD_EXISTENCE')
          EXPORTING
            im_jd_name     = lv_name
          IMPORTING
            ex_is_existing = rv_bool.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |JOBD not supported| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_obj_name TYPE e071-obj_name.


    lv_obj_name = ms_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = ms_item-obj_type
        iv_obj_name       = lv_obj_name
        iv_action         = 'SHOW'
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lr_job_definition TYPE REF TO data,
          lo_job_definition TYPE REF TO object,
          lv_name           TYPE ty_jd_name.

    FIELD-SYMBOLS: <lg_job_definition> TYPE any,
                   <lg_field>          TYPE any.


    lv_name = ms_item-obj_name.

    TRY.
        CREATE DATA lr_job_definition TYPE ('CL_JR_JOB_DEFINITION=>TY_JOB_DEFINITION').
        ASSIGN lr_job_definition->* TO <lg_job_definition>.
        ASSERT sy-subrc = 0.

        CREATE OBJECT lo_job_definition TYPE ('CL_JR_JOB_DEFINITION')
          EXPORTING
            im_jd_name = lv_name.

        CALL METHOD lo_job_definition->('GET_JD_ATTRIBUTES')
          IMPORTING
            ex_jd_attributes = <lg_job_definition>.

        ASSIGN COMPONENT 'JDPACKAGE' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'BTCJOB_USER' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'OWNER' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_DATE' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CREATED_TIME' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CHANGED_BY' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CHANGED_DATE' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        ASSIGN COMPONENT 'CHANGED_TIME' OF STRUCTURE <lg_job_definition> TO <lg_field>.
        CLEAR <lg_field>.

        io_xml->add( iv_name = 'JOBD'
                     ig_data = <lg_job_definition> ).

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Error serializing JOBD| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_JOBD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_ODSO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_odso=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_odso=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_ODSO implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN COMPONENT iv_fieldname
           OF STRUCTURE cg_metadata
           TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_dsonam  TYPE c LENGTH 30,
          ls_return  TYPE bapiret2,
          lr_details TYPE REF TO data.

    FIELD-SYMBOLS: <lg_details> TYPE any,
                   <lg_tstpnm>  TYPE any.

    TRY.
        CREATE DATA lr_details TYPE ('BAPI6116').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject = lv_dsonam
      IMPORTING
        details   = <lg_details>
        return    = ls_return.

    IF ls_return-type = 'E'.
      Lcx_abapgit_exception=>raise( |Error when geting changed by of ODSO: { ls_return-message }| ).
    ENDIF.

    ASSIGN COMPONENT 'TSTPNM' OF STRUCTURE <lg_details> TO <lg_tstpnm>.

    rv_user = <lg_tstpnm>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_odsonam    TYPE c LENGTH 30,
          lv_objname    TYPE sobj_name,
          lo_collection TYPE REF TO object,
          lt_msg        TYPE STANDARD TABLE OF bal_s_msg,
          ls_msg        TYPE bal_s_msg.

    TRY.
        CREATE OBJECT lo_collection TYPE ('CL_RSD_ODSO_COLLECTION').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    lv_odsonam = ms_item-obj_name.
    lv_objname = ms_item-obj_name.

    TRY.
        CALL METHOD lo_collection->('ADD_TLOGO')
          EXPORTING
            i_objnm  = lv_objname
            i_modify = abap_true
            i_delete = abap_true.

        CALL METHOD lo_collection->('DELETE').

        CALL METHOD ('CL_RSO_APPLICATION_LOG')=>('APPL_LOG_MSG_READ')
          IMPORTING
            e_t_msg = lt_msg.

        READ TABLE lt_msg WITH KEY msgty = 'E' INTO ls_msg.
        IF sy-subrc = 0.
          Lcx_abapgit_exception=>raise(
          |Error when deleting ODSO: { ms_item-obj_name } { ls_msg-msgv1 } { ls_msg-msgv2 }| ).
        ENDIF.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Canceled deletion of ODSO: { ms_item-obj_name }| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_dsonam      TYPE c LENGTH 30,
          lr_details     TYPE REF TO data,
          lr_infoobjects TYPE REF TO data,
          lr_navigation  TYPE REF TO data,
          lr_indexes     TYPE REF TO data,
          lr_index_iobj  TYPE REF TO data,
          lt_return      TYPE STANDARD TABLE OF bapiret2,
          ls_return      TYPE bapiret2.

    FIELD-SYMBOLS:
      <lg_details>     TYPE any,
      <lg_odsobject>   TYPE any,
      <lt_infoobjects> TYPE STANDARD TABLE,
      <lt_navigation>  TYPE STANDARD TABLE,
      <lt_indexes>     TYPE STANDARD TABLE,
      <lt_index_iobj>  TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details     TYPE ('BAPI6116').
        CREATE DATA lr_infoobjects TYPE STANDARD TABLE OF ('BAPI6116IO').
        CREATE DATA lr_navigation  TYPE STANDARD TABLE OF ('BAPI6116NA').
        CREATE DATA lr_indexes     TYPE STANDARD TABLE OF ('BAPI6116IN').
        CREATE DATA lr_index_iobj  TYPE STANDARD TABLE OF ('BAPI6116II').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.
    ASSIGN lr_infoobjects->* TO <lt_infoobjects>.
    ASSIGN lr_navigation->* TO <lt_navigation>.
    ASSIGN lr_indexes->* TO <lt_indexes>.
    ASSIGN lr_index_iobj->* TO <lt_index_iobj>.

    io_xml->read( EXPORTING iv_name = 'ODSO'
                  CHANGING  cg_data = <lg_details> ).

    io_xml->read( EXPORTING iv_name = 'INFOOBJECTS'
                  CHANGING  cg_data = <lt_infoobjects> ).

    io_xml->read( EXPORTING iv_name = 'NAVIGATION'
                  CHANGING  cg_data = <lt_navigation> ).

    io_xml->read( EXPORTING iv_name = 'INDEXES'
                  CHANGING  cg_data = <lt_indexes> ).

    io_xml->read( EXPORTING iv_name = 'INDEX_IOBJ'
                  CHANGING  cg_data = <lt_index_iobj> ).
    TRY.

        ASSIGN COMPONENT 'ODSOBJECT' OF STRUCTURE <lg_details> TO <lg_odsobject>.
        ASSERT sy-subrc = 0.

        IF Lif_abapgit_object~exists( ) = abap_false.
          CALL FUNCTION 'BAPI_ODSO_CREATE'
            EXPORTING
              details              = <lg_details>
            IMPORTING
              odsobject            = lv_dsonam
            TABLES
              infoobjects          = <lt_infoobjects>
              navigationattributes = <lt_navigation>
              indexes              = <lt_indexes>
              indexesinfoobjects   = <lt_index_iobj>
              return               = lt_return.
        ELSE.
          CALL FUNCTION 'BAPI_ODSO_CHANGE'
            EXPORTING
              odsobject            = <lg_odsobject>
              details              = <lg_details>
            TABLES
              infoobjects          = <lt_infoobjects>
              navigationattributes = <lt_navigation>
              indexes              = <lt_indexes>
              indexesinfoobjects   = <lt_index_iobj>
              return               = lt_return.
        ENDIF.

      CATCH cx_sy_dyn_call_illegal_func.
        Lcx_abapgit_exception=>raise( |Necessary BW function modules not found or object not supported| ).
    ENDTRY.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( |Error when creating ODSO: { ls_return-message }| ).
    ENDIF.

    CALL FUNCTION 'BAPI_ODSO_ACTIVATE'
      EXPORTING
        odsobject = <lg_odsobject>
      TABLES
        return    = lt_return.

    READ TABLE lt_return WITH KEY type = 'E' INTO ls_return.
    IF sy-subrc = 0.
      Lcx_abapgit_exception=>raise( |Error when activating ODSO: { ls_return-message }| ).
    ENDIF.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_iobjnm TYPE c LENGTH 30.

    SELECT SINGLE odsobject
    FROM ('RSDODSO')
    INTO lv_iobjnm
    WHERE odsobject = ms_item-obj_name.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    DATA: lv_dsona TYPE c LENGTH 30,
          lo_odso  TYPE REF TO object,
          lv_isact TYPE abap_bool.

    lv_dsona = ms_item-obj_name.

    CALL METHOD ('CL_RSD_ODSO')=>('FACTORY')
      EXPORTING
        i_odsobject = lv_dsona
      RECEIVING
        r_r_odso    = lo_odso.

    CALL METHOD lo_odso->('IS_ACTIVE')
      RECEIVING
        r_is_active = lv_isact.

    rv_active = lv_isact.

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lv_object TYPE eqegraarg.

    lv_object = ms_item-obj_name.
    OVERLAY lv_object WITH '                                          '.
    lv_object = lv_object && '*'.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'RSD_S_PROV'
                                            iv_argument    = lv_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_dsonam      TYPE c LENGTH 30,
          lr_details     TYPE REF TO data,
          lr_infoobjects TYPE REF TO data,
          lr_navigation  TYPE REF TO data,
          lr_indexes     TYPE REF TO data,
          lr_index_iobj  TYPE REF TO data,
          ls_return      TYPE bapiret2.

    FIELD-SYMBOLS:
      <lg_details>     TYPE any,
      <lt_infoobjects> TYPE STANDARD TABLE,
      <lt_navigation>  TYPE STANDARD TABLE,
      <lt_indexes>     TYPE STANDARD TABLE,
      <lt_index_iobj>  TYPE STANDARD TABLE.

    TRY.
        CREATE DATA lr_details     TYPE ('BAPI6116').
        CREATE DATA lr_infoobjects TYPE STANDARD TABLE OF ('BAPI6116IO').
        CREATE DATA lr_navigation  TYPE STANDARD TABLE OF ('BAPI6116NA').
        CREATE DATA lr_indexes     TYPE STANDARD TABLE OF ('BAPI6116IN').
        CREATE DATA lr_index_iobj  TYPE STANDARD TABLE OF ('BAPI6116II').
      CATCH cx_sy_create_data_error.
        Lcx_abapgit_exception=>raise( |ODSO is not supported on this system| ).
    ENDTRY.

    ASSIGN lr_details->* TO <lg_details>.
    ASSIGN lr_infoobjects->* TO <lt_infoobjects>.
    ASSIGN lr_navigation->* TO <lt_navigation>.
    ASSIGN lr_indexes->* TO <lt_indexes>.
    ASSIGN lr_index_iobj->* TO <lt_index_iobj>.

    lv_dsonam = ms_item-obj_name.

    CALL FUNCTION 'BAPI_ODSO_GETDETAIL'
      EXPORTING
        odsobject            = lv_dsonam
      IMPORTING
        details              = <lg_details>
        return               = ls_return
      TABLES
        infoobjects          = <lt_infoobjects>
        navigationattributes = <lt_navigation>
        indexes              = <lt_indexes>
        indexesinfoobjects   = <lt_index_iobj>.

    IF ls_return-type = 'E'.
      Lcx_abapgit_exception=>raise( |Error when geting details of ODSO: { ls_return-message }| ).
    ENDIF.

    clear_field( EXPORTING iv_fieldname = 'TSTPNM'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'TIMESTMP'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'CONTTIMESTMP'
                 CHANGING  cg_metadata  = <lg_details> ).

    clear_field( EXPORTING iv_fieldname = 'OWNER'
                 CHANGING  cg_metadata  = <lg_details> ).

    io_xml->add( iv_name = 'ODSO'
                 ig_data = <lg_details> ).

    io_xml->add( iv_name = 'INFOOBJECTS'
                 ig_data = <lt_infoobjects> ).

    io_xml->add( iv_name = 'NAVIGATION'
                 ig_data = <lt_navigation> ).

    io_xml->add( iv_name = 'INDEXES'
                 ig_data = <lt_indexes> ).

    io_xml->add( iv_name = 'INDEX_IOBJ'
                 ig_data = <lt_index_iobj> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_ODSO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_OTGR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_otgr=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_otgr=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_OTGR implementation.
*"* method's implementations
*include methods.
  METHOD instantiate_and_lock_otgr.
    DATA:
      lv_new   TYPE abap_bool,
      lv_name  TYPE cls_attribute_name,
      lv_state TYPE cls_type_group-activation_state.

    SELECT SINGLE name FROM cls_type_group INTO lv_name WHERE name = ms_item-obj_name.
    IF sy-subrc = 0.
      lv_new   = abap_false.
      lv_state = cl_pak_wb_domains=>co_activation_state-invalid.
    ELSE.
      lv_new   = abap_true.
      lv_state = cl_pak_wb_domains=>co_activation_state-active.
    ENDIF.
    lv_name = ms_item-obj_name.

    TRY.
        CREATE OBJECT ro_otgr
          EXPORTING
            im_name             = lv_name
            im_new              = lv_new
            im_activation_state = lv_state.
      CATCH cx_pak_invalid_data
          cx_pak_not_authorized
          cx_pak_invalid_state
          cx_pak_wb_object_locked.
        Lcx_abapgit_exception=>raise( |OTGR { lv_name }: error while instantiating CL_CLS_OBJECT_TYPE_GROUP| ).
    ENDTRY.

    IF lv_new = abap_false.
      TRY.
          ro_otgr->if_pak_wb_object~lock_and_refresh( ).
        CATCH cx_pak_invalid_data
            cx_pak_not_authorized
            cx_pak_invalid_state
            cx_pak_wb_object_locked.
          Lcx_abapgit_exception=>raise( |OTGR { lv_name }: could not aquire lock| ).
      ENDTRY.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE changed_by FROM cls_type_group INTO rv_user
      WHERE name = ms_item-obj_name
      AND activation_state = cl_pak_wb_domains=>co_activation_state-active.

    IF rv_user IS INITIAL.
      SELECT SINGLE created_by FROM cls_type_group INTO rv_user
        WHERE name = ms_item-obj_name
        AND activation_state = cl_pak_wb_domains=>co_activation_state-active.
    ENDIF.

    IF rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: lo_otgr      TYPE REF TO cl_cls_object_type_group,
          lx_pak_error TYPE REF TO cx_root,
          lv_text      TYPE string.

    lo_otgr = instantiate_and_lock_otgr( ).

    TRY.
        lo_otgr->if_pak_wb_object~delete( ).
        lo_otgr->if_pak_wb_object~save( ).
        lo_otgr->unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_otgr->unlock( ).

        lv_text = lx_pak_error->get_text( ).
        Lcx_abapgit_exception=>raise( |OTGR { ms_item-obj_name }: delete: { lv_text }| ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    DATA: ls_otgr      TYPE ty_otgr,
          lo_otgr      TYPE REF TO cl_cls_object_type_group,
          lx_pak_error TYPE REF TO cx_root,
          lv_text      TYPE string,
          lv_main_lang TYPE sy-langu,
          lo_parents   TYPE REF TO data.

    FIELD-SYMBOLS: <ls_groupt>  LIKE LINE OF ls_otgr-texts,
                   <ls_element> LIKE LINE OF ls_otgr-elements,
                   <lv_field>   TYPE any,
                   <ls_parent>  TYPE any,
                   <lt_parents> TYPE ANY TABLE.

    io_xml->read( EXPORTING iv_name = 'OTGR'
                  CHANGING  cg_data = ls_otgr ).

    LOOP AT ls_otgr-texts ASSIGNING <ls_groupt>.
      <ls_groupt>-activation_state = cl_pak_wb_domains=>co_activation_state-inactive.
      " Removed in the method serialize.
      <ls_groupt>-name = ms_item-obj_name.
    ENDLOOP.

    " Parents (cls_tygr_parent) does not exist in lower releases
    TRY.
        CREATE DATA lo_parents TYPE TABLE OF ('CLS_TYGR_PARENT').
        ASSIGN lo_parents->* TO <lt_parents>.
      CATCH cx_sy_create_data_error.
    ENDTRY.

    IF <lt_parents> IS ASSIGNED.
      io_xml->read( EXPORTING iv_name = 'PARENTS'
                    CHANGING  cg_data = <lt_parents> ).

      LOOP AT <lt_parents> ASSIGNING <ls_parent>.
        ASSIGN COMPONENT 'ACTIVATION_STATE' OF STRUCTURE <ls_parent> TO <lv_field>.
        IF sy-subrc = 0.
          <lv_field> = cl_pak_wb_domains=>co_activation_state-inactive.
        ENDIF.
        ASSIGN COMPONENT 'OBJ_TYPE_GROUP' OF STRUCTURE <ls_parent> TO <lv_field>.
        IF sy-subrc = 0.
          " Removed in the method serialize.
          <lv_field> = ms_item-obj_name.
        ENDIF.
      ENDLOOP.
    ENDIF.

    LOOP AT ls_otgr-elements ASSIGNING <ls_element>.
      <ls_element>-activation_state = cl_pak_wb_domains=>co_activation_state-inactive.
      " Removed in the method serialize.
      <ls_element>-obj_type_group = ms_item-obj_name.
    ENDLOOP.

    tadir_insert( iv_package ).

    lo_otgr = instantiate_and_lock_otgr( ).

    TRY.
        lo_otgr->if_cls_object_type_group~set_proxy_filter( ls_otgr-cls_type_group-proxy_flag ).
        lo_otgr->if_cls_object_type_group~set_elements( ls_otgr-elements ).

        IF <lt_parents> IS ASSIGNED.
          CALL METHOD lo_otgr->('IF_CLS_OBJECT_TYPE_GROUP~SET_PARENT_GROUPS')
            EXPORTING
              im_parent_groups = <lt_parents>.
        ENDIF.

        lv_main_lang = lo_otgr->if_pak_wb_object~get_master_language( ).
        READ TABLE ls_otgr-texts WITH KEY langu = lv_main_lang ASSIGNING <ls_groupt>.
        IF sy-subrc = 0.
          lo_otgr->set_description( <ls_groupt>-text ).
          " ELSE.
          "   Do we want to clear the main language description if not present in the XML conent?
          "   Main language is non-deterministic - it depends on sy-langu, so rather don't touch
          "   description if the main language is not present
          "   Perhaps, we can display some sort of a message but how?
        ENDIF.

        set_default_package( iv_package ).

        lo_otgr->if_pak_wb_object~save( ).

        lo_otgr->if_pak_wb_object~activate( ).
        lo_otgr->unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_otgr->unlock( ).

        lv_text = lx_pak_error->get_text( ).
        Lcx_abapgit_exception=>raise( |OTGR { ms_item-obj_name }: deserialize: { lv_text }| ).
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    rv_bool = cl_cls_object_type_group=>exists_object_type_group( ms_item-obj_name ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ECLS_ATTRIBUTE'
                                            iv_argument    = |{ ms_item-obj_name }*| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    DATA: lv_text      TYPE string,
          lv_name      TYPE ty_otgr-cls_type_group,
          ls_otgr      TYPE ty_otgr,
          lo_otgr      TYPE REF TO cl_cls_object_type_group,
          lx_pak_error TYPE REF TO cx_root,
          lo_parents   TYPE REF TO data.

    FIELD-SYMBOLS: <ls_groupt>  LIKE LINE OF ls_otgr-texts,
                   <ls_element> LIKE LINE OF ls_otgr-elements,
                   <lv_field>   TYPE any,
                   <ls_parent>  TYPE any,
                   <lt_parents> TYPE ANY TABLE.

    lo_otgr = instantiate_and_lock_otgr( ).

*   Description part 1:
*   Dealing with Description of OTGR objects is problematic.
*   The API supports setting of main language only and
*   if we want to save also translations we would have to implement
*   our own logic for merging and activation. To keep it simple stupid
*   the current version focuses on the main language only.
*   If anybody ever runs into the need to version also translation,
*   ask the maintainers of CL_CLS_OBJECT_TYPE_GROUP to add a method for it.
*
*   However, the XML content will pretend we support also translations,
*   so if someone adds support for them in future, there will be no format change.
    APPEND INITIAL LINE TO ls_otgr-texts ASSIGNING <ls_groupt>.

    " Parents (cls_tygr_parent) does not exist in lower releases
    TRY.
        CREATE DATA lo_parents TYPE TABLE OF ('CLS_TYGR_PARENT').
        ASSIGN lo_parents->* TO <lt_parents>.
      CATCH cx_sy_create_data_error.
    ENDTRY.

    TRY.
        ls_otgr-cls_type_group-name = lo_otgr->if_cls_object_type_group~get_name( ).
        ls_otgr-cls_type_group-proxy_flag = lo_otgr->if_cls_object_type_group~get_proxy_filter( ).

        TRY.
            CALL METHOD lo_otgr->('GET_ELEMENTS')
              EXPORTING
                im_explicit_elements_only = abap_true " doesn't exist on lower releases. Eg. 752 SP04
              IMPORTING
                ex_elements               = ls_otgr-elements.

          CATCH cx_sy_dyn_call_param_not_found.

            lo_otgr->get_elements( IMPORTING ex_elements = ls_otgr-elements ).

        ENDTRY.

        " Remove children since they are created automatically (by the child group)
        LOOP AT ls_otgr-elements ASSIGNING <ls_element>.
          SELECT SINGLE name FROM cls_type_group INTO lv_name WHERE name = <ls_element>-type.
          IF sy-subrc = 0.
            DELETE ls_otgr-elements.
          ENDIF.
        ENDLOOP.

        IF <lt_parents> IS ASSIGNED.
          CALL METHOD lo_otgr->('IF_CLS_OBJECT_TYPE_GROUP~GET_PARENT_GROUPS')
            EXPORTING
              im_explicit_parents_only = abap_true
            IMPORTING
              ex_parent_groups         = <lt_parents>.
        ENDIF.

        " Beware: the following method returns the main language description only if the object is locked!
        <ls_groupt>-text = lo_otgr->if_cls_object_type_group~get_description( ).
        <ls_groupt>-langu = lo_otgr->if_pak_wb_object~get_master_language( ).

        lo_otgr->unlock( ).

      CATCH cx_pak_invalid_state cx_pak_invalid_data cx_pak_not_authorized INTO lx_pak_error.
        lo_otgr->unlock( ).

        lv_text = lx_pak_error->get_text( ).
        Lcx_abapgit_exception=>raise( |OTGR { ms_item-obj_name }: serialize: { lv_text }| ).
    ENDTRY.

    CLEAR: ls_otgr-cls_type_group-created_by,
           ls_otgr-cls_type_group-created_on,
           ls_otgr-cls_type_group-changed_by,
           ls_otgr-cls_type_group-changed_on.

*    Description part 2:
*
* lt_lang_sel  TYPE RANGE OF langu,
* ls_lang_sel  LIKE LINE OF lt_lang_sel,
*
*    IF io_xml->i18n_params( )-main_language_only = abap_true.
*      ls_lang_sel-low = mv_language.
*      ls_lang_sel-sign = 'I'.
*      ls_lang_sel-option = 'EQ'.
*    ENDIF.
*
*    SELECT * FROM cls_type_groupt INTO TABLE ls_otgr-texts
*      WHERE name = ms_item-obj_name
*        AND activation_state = 'A'
*        AND langu in lt_lang_sel.
*
*   Description ideas end

    LOOP AT ls_otgr-texts ASSIGNING <ls_groupt>.
      " Not necessary as we serialize only Active
      CLEAR <ls_groupt>-activation_state.
      " Not necessary as we have it in the root XML node
      CLEAR <ls_groupt>-name.
    ENDLOOP.

    LOOP AT ls_otgr-elements ASSIGNING <ls_element>.
      " Not necessary as we serialize only Active
      CLEAR <ls_element>-activation_state.
      " Not necessary as we have it in the root XML node
      CLEAR <ls_element>-obj_type_group.
    ENDLOOP.

    io_xml->add( iv_name = 'OTGR'
                 ig_data = ls_otgr ).

    IF <lt_parents> IS ASSIGNED.
      LOOP AT <lt_parents> ASSIGNING <ls_parent>.
        ASSIGN COMPONENT 'ACTIVATION_STATE' OF STRUCTURE <ls_parent> TO <lv_field>.
        IF sy-subrc = 0.
          " Not necessary as we serialize only Active
          CLEAR <lv_field>.
        ENDIF.
        ASSIGN COMPONENT 'OBJ_TYPE_GROUP' OF STRUCTURE <ls_parent> TO <lv_field>.
        IF sy-subrc = 0.
          " Not necessary as we have it in the root XML node
          CLEAR <lv_field>.
        ENDIF.
      ENDLOOP.

      io_xml->add( iv_name = 'PARENTS'
                   ig_data = <lt_parents> ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_OTGR implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PARA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_para=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_para=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_PARA implementation.
*"* method's implementations
*include methods.
  METHOD unlock.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode         = 'FREE'
        object       = iv_paramid
        object_class = 'PARA'.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
* looks like "changed by user" is not stored in the database
    rv_user = c_user_unknown.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    " We can't use FM RS_PARAMETER_DELETE because of the popup to confirm
    "Therefore we have to reimplement most of the FMs logic

    DATA lv_paramid TYPE tpara-paramid.

    lv_paramid = ms_item-obj_name.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        global_lock              = abap_true
        language_upd_exit        = 'RS_PARAMETER_LANGUAGE_EXIT'    " Name FuBa for maintenance language change
        object                   = lv_paramid
        object_class             = ms_item-obj_type
        suppress_language_check  = space
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    SELECT COUNT(*) FROM cross
      WHERE ( type = 'P' OR type = 'Q' ) AND name = lv_paramid.
    IF sy-subrc = 0.
      unlock( lv_paramid ).
      Lcx_abapgit_exception=>raise( 'PARA: Parameter is still used' ).
    ELSE.
      SELECT COUNT(*) FROM dd04l BYPASSING BUFFER
        WHERE memoryid = lv_paramid
        AND as4local = 'A'.
      IF sy-subrc = 0.
        unlock( lv_paramid ).
        Lcx_abapgit_exception=>raise( 'PARA: Parameter is still used' ).
      ENDIF.
    ENDIF.

    unlock( lv_paramid ).

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = 'PARA'
      iv_obj_name = lv_paramid
      iv_package  = iv_package
      iv_language = mv_language
      iv_mode     = Lif_abapgit_cts_api=>c_transport_mode-delete ).

    DELETE FROM tpara WHERE paramid = lv_paramid.
    DELETE FROM tparat WHERE paramid = lv_paramid.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = lv_paramid
        operation = 'DELETE'
        type      = 'CR'.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
* see fm RS_PARAMETER_ADD and RS_PARAMETER_EDIT

    DATA: lv_mode   TYPE c LENGTH 1,
          ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.

    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc = 0.
      lv_mode = 'M'.
    ELSE.
      lv_mode = 'I'.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'TPARA'
                  CHANGING cg_data = ls_tpara ).

    CALL FUNCTION 'RS_CORR_INSERT'
      EXPORTING
        object              = ms_item-obj_name
        object_class        = 'PARA'
        mode                = lv_mode
        global_lock         = abap_true
        devclass            = iv_package
        master_language     = mv_language
        suppress_dialog     = abap_true
      EXCEPTIONS
        cancelled           = 1
        permission_failure  = 2
        unknown_objectclass = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    MODIFY tpara FROM ls_tpara.                           "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING iv_name = 'TPARAT'
      CHANGING  cg_data = ls_tparat ).

    MODIFY tparat FROM ls_tparat.                         "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_paramid TYPE tpara-paramid.


    SELECT SINGLE paramid FROM tpara INTO lv_paramid
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'PA' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_tpara  TYPE tpara,
          ls_tparat TYPE tparat.

    SELECT SINGLE * FROM tpara INTO ls_tpara
      WHERE paramid = ms_item-obj_name.                 "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM tparat INTO ls_tparat
      WHERE paramid = ms_item-obj_name
      AND sprache = mv_language.          "#EC CI_GENBUFF "#EC CI_SUBRC

    io_xml->add( iv_name = 'TPARA'
                 ig_data = ls_tpara ).

    io_xml->add(
      iv_name = 'TPARAT'
      ig_data = ls_tparat ).
    " Here only the original language is serialized,
    " so it should be present for the moment. LXEs are just translations

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PARA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PDXX_SUPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pdxx_super=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pdxx_super=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_object_pdxx_super=ccau.








class LCL_ABAPGIT_OBJECT_PDXX_SUPER implementation.
*"* method's implementations
*include methods.
  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    ms_objkey-otype = is_item-obj_type+2(2).
    ms_objkey-objid = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE uname
      INTO rv_user
      FROM hrs1201
      WHERE otype = ms_item-obj_type AND
            objid = ms_item-obj_name.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    CALL FUNCTION 'RH_HRSOBJECT_DELETE'
      EXPORTING
        act_otype           = ms_objkey-otype
        act_objid           = ms_objkey-objid
        no_confirmation_msg = abap_true
      EXCEPTIONS
        enqueue_failed      = 1
        object_not_deleted  = 2
        object_not_found    = 3
        OTHERS              = 4.       "#EC SUBRC_OK

    check_subrc_for( `RH_HRSOBJECT_DELETE` ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.
    ASSERT 1 = 2. "Must be redefined
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'RH_READ_OBJECT'
      EXPORTING
        plvar     = '01'
        otype     = ms_objkey-otype
        objid     = ms_objkey-objid
        istat     = '1'
        begda     = sy-datum
        endda     = '99991231'
        ointerval = 'X'
        read_db   = 'X'
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'HRSOBJECT'
                                            iv_argument    = ms_objkey-otype && ms_objkey-objid ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.
    ASSERT 1 = 2. "Must be redefined
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PDXX_SUPER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PERS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pers=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pers=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_PERS implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).


    mv_pers_key = ms_item-obj_name.

  ENDMETHOD.
  METHOD get_personalization_object.

    CREATE OBJECT ro_personalization_object
      EXPORTING
        p_create                = iv_create
        p_pers_key              = mv_pers_key
        p_view_only             = iv_view_only
      EXCEPTIONS
        pers_key_already_exists = 1
        pers_key_does_not_exist = 2
        transport_view_only     = 3
        transport_canceled      = 4
        OTHERS                  = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE author FROM spers_reg INTO rv_user
      WHERE pers_key = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_personalization_object TYPE REF TO cl_pers_reg.

    lo_personalization_object = get_personalization_object( ).

    lo_personalization_object->delete(
      EXPORTING
        p_no_confirm       = abap_true
      EXCEPTIONS
        deletion_canceled  = 1
        deletion_failed    = 2
        transport_canceled = 3
        OTHERS             = 4 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      ls_personalization_object TYPE ty_personalization_object,
      lo_personalization_object TYPE REF TO cl_pers_reg.

    io_xml->read(
      EXPORTING
        iv_name = 'PERS'
      CHANGING
        cg_data = ls_personalization_object ).

    tadir_insert( iv_package ).

    lo_personalization_object = get_personalization_object( iv_create = abap_true ).

    lo_personalization_object->set_reg_data(
        p_pers_reg      = ls_personalization_object-pers_reg
        p_pers_reg_text = ls_personalization_object-pers_reg_text ).

    lo_personalization_object->save(
      EXPORTING
        no_check           = abap_true
      EXCEPTIONS
        data_not_saved     = 1
        transport_canceled = 2
        OTHERS             = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    cl_pers_reg=>exists(
      EXPORTING
        p_pers_key              = mv_pers_key
      EXCEPTIONS
        pers_key_does_not_exist = 1
        OTHERS                  = 2 ).

    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    " There's no object specific locking. Just a global one.
    rv_is_locked = exists_a_lock_entry_for( 'E_SPERSREG' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: ls_bcdata TYPE bdcdata,
          lt_bcdata TYPE STANDARD TABLE OF bdcdata.

    ls_bcdata-program  = 'SAPLSPERS_REG_DIALOG'.
    ls_bcdata-dynpro   = '0100'.
    ls_bcdata-dynbegin = 'X'.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam     = 'SPERS_REG-PERS_KEY'.
    ls_bcdata-fval     = ms_item-obj_name.
    APPEND ls_bcdata TO lt_bcdata.

    CLEAR ls_bcdata.
    ls_bcdata-fnam = 'BDC_OKCODE'.
    ls_bcdata-fval = '=PERSDISPLAY'.
    APPEND ls_bcdata TO lt_bcdata.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'PERSREG'
      it_bdcdata = lt_bcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lo_personalization_object TYPE REF TO cl_pers_reg,
      ls_personalization_object TYPE ty_personalization_object.

    lo_personalization_object = get_personalization_object( iv_view_only = abap_true ).

    lo_personalization_object->get_reg_data(
      IMPORTING
        p_pers_reg      = ls_personalization_object-pers_reg
        p_pers_reg_text = ls_personalization_object-pers_reg_text ).

    CLEAR:
      ls_personalization_object-pers_reg-author,
      ls_personalization_object-pers_reg-fdate,
      ls_personalization_object-pers_reg-ftime.

    io_xml->add( iv_name = 'PERS'
                 ig_data = ls_personalization_object ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PERS implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PINF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_pinf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_pinf=======ccimp.
CLASS SHRITEFUH64VYIPO5IWUYB3KW5GSFY IMPLEMENTATION.

  METHOD constructor.

    mi_interface = ii_interface.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~get_elements.

    mi_interface->get_elements(
      IMPORTING
        e_elements     = rt_elements
      EXCEPTIONS
        object_invalid = 1
        intern_err     = 2
        OTHERS         = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~set_elements_changeable.

    mi_interface->set_elements_changeable(
      EXPORTING
        i_changeable                = iv_changeable
      EXCEPTIONS
        object_already_changeable   = 1
        object_already_unlocked     = 2
        object_locked_by_other_user = 3
        object_modified             = 4
        object_just_created         = 5
        object_deleted              = 6
        permission_failure          = 7
        object_invalid              = 8
        unexpected_error            = 9
        OTHERS                      = 10 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~save_elements.

    mi_interface->save_elements(
      EXCEPTIONS
        object_not_changeable = 1
        object_invalid        = 2
        cancelled_in_corr     = 3
        permission_failure    = 4
        unexpected_error      = 5
        intern_err            = 6
        OTHERS                = 7 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~get_all_attributes.

    mi_interface->get_all_attributes(
      IMPORTING
        e_package_interface_data = rs_package_interface_data
      EXCEPTIONS
        object_invalid           = 1
        OTHERS                   = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~set_changeable.

    mi_interface->set_changeable(
      EXPORTING
        i_changeable                 = iv_changeable
      EXCEPTIONS
        object_locked_by_other_user  = 1
        permission_failure           = 2
        object_already_changeable    = 3
        object_already_unlocked      = 4
        object_just_created          = 5
        object_deleted               = 6
        object_modified              = 7
        object_not_existing          = 8
        object_invalid               = 9
        unexpected_error             = 10
        OTHERS                       = 11 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~delete.

    mi_interface->delete(
      EXCEPTIONS
        object_not_empty      = 1
        object_not_changeable = 2
        object_invalid        = 3
        intern_err            = 4
        OTHERS                = 5 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~save.

    mi_interface->save(
      EXCEPTIONS
        short_text_missing    = 1
        object_not_changeable = 2
        object_invalid        = 3
        cancelled_in_corr     = 4
        permission_failure    = 5
        unexpected_error      = 6
        intern_err            = 7
        OTHERS                = 8 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~remove_elements.

    mi_interface->remove_elements(
      EXPORTING
        i_elements            = it_elements
      EXCEPTIONS
        object_deleted        = 1
        object_invalid        = 2
        object_not_changeable = 3
        element_not_contained = 4
        intern_err            = 5
        OTHERS                = 6 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~add_elements.

    DATA:
      lt_mismatched TYPE scomeldata,
      ls_mismatched LIKE LINE OF lt_mismatched.

    mi_interface->add_elements(
      EXPORTING
        i_elements_data        = it_elements_data
      IMPORTING
        e_mismatched_elem_data = lt_mismatched
      EXCEPTIONS
        object_invalid         = 1
        intern_err             = 2
        OTHERS                 = 3 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    LOOP AT lt_mismatched INTO ls_mismatched.
      Lcx_abapgit_exception=>raise( |Object { ls_mismatched-elem_type } { ls_mismatched-elem_key } | &&
                                    |from different package { ls_mismatched-elem_pack }| ).
    ENDLOOP.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~set_all_attributes.

    mi_interface->set_all_attributes(
      EXPORTING
        i_package_interface_data     = is_package_interface_data
        i_data_sign                  = is_data_sign
      EXCEPTIONS
        object_deleted               = 1
        object_not_changeable        = 2
        interface_not_empty          = 3
        acl_not_empty                = 4
        author_not_existing          = 5
        object_type_mismatch         = 6
        object_invalid               = 7
        OTHERS                       = 8 ).
* Downport: exception "logical_package_types_differ"
* does not exist in lower versions

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

  METHOD SHRITEFUH64VYIPO5IWUYB3KW5FSFY~get_changeable.

    mi_interface->get_changeable(
      IMPORTING
        e_changeable   = rv_changeable
      EXCEPTIONS
        object_invalid = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_OBJECT_PINF implementation.
*"* method's implementations
*include methods.
  METHOD create_facade.

    CREATE OBJECT ri_facade TYPE SHRITEFUH64VYIPO5IWUYB3KW5GSFY
      EXPORTING
        ii_interface = ii_interface.

  ENDMETHOD.
  METHOD create_or_load.

    DATA: li_interface          TYPE REF TO if_package_interface,
          lv_pkg_interface_data TYPE scompidtln.

    lv_pkg_interface_data-default_if = is_pinf-attributes-default_if.
    lv_pkg_interface_data-tadir_devc = iv_package.

    "Important if the package name comes from another package
    IF is_pinf-attributes-pack_name IS INITIAL.
      lv_pkg_interface_data-pack_name = iv_package.
    ELSE.
      lv_pkg_interface_data-pack_name = is_pinf-attributes-pack_name.
    ENDIF.

    IF Lif_abapgit_object~exists( ) = abap_false.
      cl_package_interface=>create_new_package_interface(
        EXPORTING
          i_pkg_interface_name    = is_pinf-attributes-intf_name
          i_publisher_pkg_name    = lv_pkg_interface_data-pack_name
          i_pkg_interface_data    = lv_pkg_interface_data
        IMPORTING
          e_package_interface     = li_interface
        EXCEPTIONS
          object_already_existing = 1
          object_just_created     = 2
          interface_name_invalid  = 3
          unexpected_error        = 4
          OTHERS                  = 7 ).
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'error creating new package interface' ).
      ENDIF.

      ri_interface = create_facade( li_interface ).

    ELSE.

      ri_interface = load( is_pinf-attributes-intf_name ).

    ENDIF.

  ENDMETHOD.
  METHOD delete_elements.

    DATA: lt_elements TYPE ty_elements.

    FIELD-SYMBOLS: <li_element> LIKE LINE OF lt_elements.


    ii_interface->set_elements_changeable( abap_true ).

    lt_elements = ii_interface->get_elements( ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      <li_element>->delete( ).
    ENDLOOP.

    ii_interface->save_elements( ).

  ENDMETHOD.
  METHOD load.

    DATA: li_interface TYPE REF TO  if_package_interface.

    cl_package_interface=>load_package_interface(
      EXPORTING
        i_package_interface_name = iv_name
        i_force_reload           = abap_true
      IMPORTING
        e_package_interface      = li_interface ).

    ri_interface = create_facade( li_interface ).

  ENDMETHOD.
  METHOD update_attributes.

    DATA: ls_sign       TYPE scompisign,
          lv_changeable TYPE abap_bool.


    lv_changeable = ii_interface->get_changeable( ).
    IF lv_changeable = abap_false.
* at creation the object is already in change mode
      ii_interface->set_changeable( abap_true ).
    ENDIF.

    ls_sign-descript       = abap_true.
    ls_sign-pinftype       = abap_true.
    ls_sign-restricted     = abap_true.
    ls_sign-default_if     = abap_true.
    ls_sign-def_sever      = abap_true.
    ls_sign-acl_flag       = abap_true.
    ls_sign-pifstablty     = abap_true.
    ls_sign-release_status = abap_true.

    ii_interface->set_all_attributes(
      is_package_interface_data = is_pinf-attributes
      is_data_sign              = ls_sign ).

    set_default_package( iv_package ).
* looks like setting "i_suppress_dialog = abap_true" will make
* it fail for local($) packages
    ii_interface->save( ).

    ii_interface->set_changeable( abap_false ).

  ENDMETHOD.
  METHOD update_elements.

    DATA: lt_existing TYPE ty_elements,
          ls_element  LIKE LINE OF is_pinf-elements,
          lt_add      TYPE scomeldata,
          lv_index    TYPE i,
          lv_found    TYPE abap_bool,
          ls_attr     TYPE scomeldtln.

    FIELD-SYMBOLS <li_element> LIKE LINE OF lt_existing.

    ii_interface->set_elements_changeable( abap_true ).

    lt_existing = ii_interface->get_elements( ).

    LOOP AT is_pinf-elements INTO ls_element.

      lv_found = abap_false.
      LOOP AT lt_existing ASSIGNING <li_element>.
        lv_index = sy-tabix.
        <li_element>->get_all_attributes( IMPORTING e_element_data = ls_attr ).
        IF ls_element-elem_type = ls_attr-elem_type
            AND ls_element-elem_key = ls_attr-elem_key.
          DELETE lt_existing INDEX lv_index.
          CONTINUE. " current loop
        ENDIF.
      ENDLOOP.

      IF lv_found = abap_false.
        ls_element-elem_pack = iv_package.
        APPEND ls_element TO lt_add.
      ENDIF.
    ENDLOOP.

    ii_interface->remove_elements( lt_existing ).

    ii_interface->add_elements( lt_add ).

    ii_interface->save_elements( ).

    ii_interface->set_elements_changeable( abap_false ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE changed_by FROM intf INTO rv_user
      WHERE intf_name = ms_item-obj_name.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: li_interface TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW5FSFY.

    corr_insert( iv_package ).

    li_interface = load( |{ ms_item-obj_name }| ).

* elements must be deleted before the package interface
* can be deleted
    delete_elements( li_interface ).

    li_interface->set_changeable( abap_true ).

    li_interface->delete( ).

    li_interface->save( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: li_interface TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW5FSFY,
          ls_pinf      TYPE ty_pinf.


    io_xml->read( EXPORTING iv_name = 'PINF'
                  CHANGING cg_data = ls_pinf ).

    "needed for update_attributes
    ls_pinf-attributes-tadir_devc = iv_package.

    li_interface = create_or_load(
      is_pinf    = ls_pinf
      iv_package = iv_package ).

    update_attributes(
      iv_package   = iv_package
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

    update_elements(
      iv_package   = iv_package
      is_pinf      = ls_pinf
      ii_interface = li_interface ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_pack_name TYPE intf-pack_name,
          lv_main_pack TYPE tdevc-mainpack.


    SELECT SINGLE pack_name FROM intf INTO lv_pack_name
      WHERE intf_name = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

    IF rv_bool = abap_true.
      SELECT SINGLE mainpack FROM tdevc INTO lv_main_pack
        WHERE devclass = lv_pack_name.                  "#EC CI_GENBUFF
      rv_bool = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'EEUDB'
                                            iv_argument    = ms_item-obj_name
                                            iv_prefix      = 'PF' ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_pinf      TYPE ty_pinf,
          lt_elements  TYPE ty_elements,
          li_interface TYPE REF TO SHRITEFUH64VYIPO5IWUYB3KW5FSFY.

    FIELD-SYMBOLS: <lg_any>     TYPE any,
                   <li_element> LIKE LINE OF lt_elements,
                   <ls_element> LIKE LINE OF ls_pinf-elements.

    li_interface = load( |{ ms_item-obj_name }| ).

    ls_pinf-attributes = li_interface->get_all_attributes( ).

    "Delete the package name if it comes from the same package
    IF ls_pinf-attributes-tadir_devc = ls_pinf-attributes-pack_name OR
      ms_item-devclass = ls_pinf-attributes-pack_name.
      CLEAR ls_pinf-attributes-pack_name.
    ENDIF.

    CLEAR: ls_pinf-attributes-author,
           ls_pinf-attributes-created_by,
           ls_pinf-attributes-created_on,
           ls_pinf-attributes-changed_by,
           ls_pinf-attributes-changed_on,
           ls_pinf-attributes-tadir_devc.

* fields does not exist in older SAP versions
    ASSIGN COMPONENT 'SW_COMP_LOGICAL_PACKAGE' OF STRUCTURE ls_pinf-attributes TO <lg_any>.
    IF sy-subrc = 0.
      CLEAR <lg_any>.
    ENDIF.
    ASSIGN COMPONENT 'SW_COMP_TADIR_PACKAGE' OF STRUCTURE ls_pinf-attributes TO <lg_any>.
    IF sy-subrc = 0.
      CLEAR <lg_any>.
    ENDIF.

    lt_elements = li_interface->get_elements( ).

    LOOP AT lt_elements ASSIGNING <li_element>.
      APPEND INITIAL LINE TO ls_pinf-elements ASSIGNING <ls_element>.
      <li_element>->get_all_attributes( IMPORTING e_element_data = <ls_element> ).
      CLEAR <ls_element>-elem_pack.
    ENDLOOP.

    io_xml->add( ig_data = ls_pinf
                 iv_name = 'PINF' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PINF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PRAG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_prag=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_prag=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_PRAG implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lo_pragma TYPE REF TO cl_abap_pragma,
          lx_error  TYPE REF TO cx_root.

    TRY.
        lo_pragma = cl_abap_pragma=>get_ref( ms_item-obj_name ).

        lo_pragma->delete( ).
        lo_pragma->leave_change( ). "unlock

      CATCH cx_root INTO lx_error.
        IF lo_pragma IS BOUND.
          lo_pragma->leave_change( ).
        ENDIF.
        Lcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_pragma TYPE ty_pragma,
          lo_pragma TYPE REF TO cl_abap_pragma,
          lx_error  TYPE REF TO cx_root.

    tadir_insert( iv_package ).

    TRY.
        io_xml->read(
          EXPORTING
            iv_name = 'PRAG'
          CHANGING
            cg_data = ls_pragma ).

        lo_pragma = cl_abap_pragma=>create( p_pragma  = ms_item-obj_name
                                            p_package = iv_package ).

        lo_pragma->set_info( p_description = ls_pragma-description
                             p_signature   = ls_pragma-signature
                             p_extension   = ls_pragma-extension ).

        lo_pragma->save( ).
        lo_pragma->leave_change( ). "unlock
      CATCH cx_root INTO lx_error.
        IF lo_pragma IS BOUND.
          lo_pragma->leave_change( ).
        ENDIF.
        Lcx_abapgit_exception=>raise( lx_error->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        cl_abap_pragma=>get_ref( ms_item-obj_name ).

      CATCH cx_abap_pragma_not_exists.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lo_pragma TYPE REF TO cl_abap_pragma,
          ls_pragma TYPE ty_pragma.

    TRY.
        lo_pragma = cl_abap_pragma=>get_ref( ms_item-obj_name ).

        ls_pragma-pragma      = lo_pragma->pragma.
        ls_pragma-extension   = lo_pragma->extension.
        ls_pragma-signature   = lo_pragma->signature.
        ls_pragma-description = lo_pragma->description.

        io_xml->add( iv_name = 'PRAG'
                     ig_data = ls_pragma ).

      CATCH cx_abap_pragma_not_exists.
        Lcx_abapgit_exception=>raise( |Pragma { ms_item-obj_name } doesn't exist| ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PRAG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_PROG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_prog=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_prog=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_PROG implementation.
*"* method's implementations
*include methods.
  METHOD deserialize_texts.

    DATA: lt_tpool_i18n TYPE ty_tpools_i18n,
          lt_tpool      TYPE textpool_table.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.


    ii_xml->read( EXPORTING iv_name = 'I18N_TPOOL'
                  CHANGING  cg_data = lt_tpool_i18n ).

    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      lt_tpool = read_tpool( <ls_tpool>-textpool ).
      deserialize_textpool( iv_program  = ms_item-obj_name
                            iv_language = <ls_tpool>-language
                            it_tpool    = lt_tpool ).
    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_with_ext.

    " Special treatment for extensions
    " If the program name exceeds 30 characters it is not a usual ABAP program but might be
    " some extension, which requires the internal addition EXTENSION TYPE
    " https://help.sap.com/doc/abapdocu_755_index_htm/7.55/en-US/index.htm?file=abapinsert_report_internal.htm
    " This e.g. occurs in case of transportable Code Inspector variants (ending with ===VC)

    Lcl_abapgit_factory=>get_sap_report( )->insert_report(
      iv_name           = is_progdir-name
      iv_package        = iv_package
      it_source         = it_source
      iv_state          = 'I'
      iv_version        = is_progdir-uccheck
      iv_program_type   = is_progdir-subc
      iv_extension_type = is_progdir-name+30 ).

    Lcl_abapgit_factory=>get_sap_report( )->update_progdir(
      is_progdir = is_progdir
      iv_state   = 'I'
      iv_package = iv_package ).

    Lcl_abapgit_objects_activation=>add(
      iv_type = 'REPS'
      iv_name = is_progdir-name ).

  ENDMETHOD.
  METHOD is_program_locked.

    rv_is_program_locked = exists_a_lock_entry_for( iv_lock_object = 'ESRDIRE'
                                                    iv_argument    = |{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD serialize_texts.

    DATA: lt_tpool_i18n      TYPE ty_tpools_i18n,
          lt_tpool           TYPE textpool_table,
          lt_language_filter TYPE Lif_abapgit_environment=>ty_system_language_filter.

    FIELD-SYMBOLS <ls_tpool> LIKE LINE OF lt_tpool_i18n.

    IF mo_i18n_params->ms_params-main_language_only = abap_true.
      RETURN.
    ENDIF.

    " Table d010tinf stores info. on languages in which program is maintained
    " Select all active translations of program texts
    " Skip main language - it was already serialized
    lt_language_filter = mo_i18n_params->build_language_filter( ).

    SELECT DISTINCT language
      INTO CORRESPONDING FIELDS OF TABLE lt_tpool_i18n
      FROM d010tinf
      WHERE r3state = 'A'
      AND prog = ms_item-obj_name
      AND language <> mv_language
      AND language IN lt_language_filter
      ORDER BY language ##TOO_MANY_ITAB_FIELDS.

    SORT lt_tpool_i18n BY language ASCENDING.
    LOOP AT lt_tpool_i18n ASSIGNING <ls_tpool>.
      READ TEXTPOOL ms_item-obj_name
        LANGUAGE <ls_tpool>-language
        INTO lt_tpool.
      <ls_tpool>-textpool = add_tpool( lt_tpool ).
    ENDLOOP.

    IF lines( lt_tpool_i18n ) > 0.
      ii_xml->add( iv_name = 'I18N_TPOOL'
                   ig_data = lt_tpool_i18n ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    SELECT SINGLE unam FROM reposrc INTO rv_user
      WHERE progname = ms_item-obj_name
      AND r3state = 'A'.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lv_program  LIKE sy-repid,
      lv_obj_name TYPE e071-obj_name.

    lv_program = ms_item-obj_name.

    CALL FUNCTION 'RS_DELETE_PROGRAM'
      EXPORTING
        corrnumber                 = iv_transport
        program                    = lv_program
        suppress_popup             = abap_true
        mass_delete_call           = abap_true
        tadir_devclass             = iv_package
        force_delete_used_includes = abap_true
      EXCEPTIONS
        enqueue_lock               = 1
        object_not_found           = 2
        permission_failure         = 3
        reject_deletion            = 4
        OTHERS                     = 5.
    IF sy-subrc = 2.
      " Drop also any inactive code that is left in REPOSRC
      Lcl_abapgit_factory=>get_sap_report( )->delete_report( lv_program ).

      " Remove inactive objects from work area
      lv_obj_name = lv_program.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPS'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.

      CALL FUNCTION 'RS_DELETE_FROM_WORKING_AREA'
        EXPORTING
          object                 = 'REPT'
          obj_name               = lv_obj_name
          immediate              = 'X'
          actualize_working_area = 'X'.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    delete_longtexts( c_longtext_id_prog ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_program_name TYPE syrepid,
          ls_progdir      TYPE Lif_abapgit_sap_report=>ty_progdir,
          lt_tpool        TYPE textpool_table,
          lt_dynpros      TYPE ty_dynpro_tt,
          lt_tpool_ext    TYPE Lif_abapgit_definitions=>ty_tpool_tt,
          ls_cua          TYPE ty_cua,
          lt_source       TYPE abaptxt255_tab.

    " Add R3TR PROG to transport first, otherwise we get several LIMUs
    corr_insert( iv_package ).

    lv_program_name = ms_item-obj_name.

    lt_source = Lif_abapgit_object~mo_files->read_abap( ).

    io_xml->read( EXPORTING iv_name = 'TPOOL'
                  CHANGING cg_data = lt_tpool_ext ).
    lt_tpool = read_tpool( lt_tpool_ext ).

    io_xml->read( EXPORTING iv_name = 'PROGDIR'
                  CHANGING cg_data  = ls_progdir ).

    set_abap_language_version( CHANGING cv_abap_language_version = ls_progdir-uccheck ).

    IF strlen( lv_program_name ) > 30.

      " Objects with extension for example transportable Code Inspector variants (ending with ===VC)
      deserialize_with_ext( is_progdir = ls_progdir
                            iv_package = iv_package
                            it_source  = lt_source ).

    ELSE.

      deserialize_program( is_progdir = ls_progdir
                           it_source  = lt_source
                           it_tpool   = lt_tpool
                           iv_package = iv_package ).

      io_xml->read( EXPORTING iv_name = 'DYNPROS'
                    CHANGING cg_data  = lt_dynpros ).
      deserialize_dynpros( lt_dynpros ).

      io_xml->read( EXPORTING iv_name = 'CUA'
                    CHANGING cg_data  = ls_cua ).
      deserialize_cua( iv_program_name = lv_program_name
                       is_cua = ls_cua ).

      " Texts deserializing (English)
      deserialize_textpool( iv_program = lv_program_name
                            it_tpool   = lt_tpool ).

      " Texts deserializing (translations)
      IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
        deserialize_texts( io_xml ).
      ENDIF.

      deserialize_longtexts( ii_xml         = io_xml
                             iv_longtext_id = c_longtext_id_prog ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_progname TYPE reposrc-progname.

    SELECT SINGLE progname FROM reposrc INTO lv_progname
      WHERE progname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    IF is_program_locked( ) = abap_true
        OR is_any_dynpro_locked( ms_item-obj_name ) = abap_true
        OR is_cua_locked( ms_item-obj_name ) = abap_true
        OR is_text_locked( ms_item-obj_name ) = abap_true.

      rv_is_locked = abap_true.

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

* see SAP note 1025291, run report DELETE_TADIR_FOR_EIMP_INCLUDE to clean bad TADIR entries
    ASSERT NOT ms_item-obj_name CP '*=E'.

    serialize_program( io_xml   = io_xml
                       is_item  = ms_item
                       io_files = Lif_abapgit_object~mo_files ).

    " Texts serializing (translations)
    IF mo_i18n_params->is_lxe_applicable( ) = abap_false.
      serialize_texts( io_xml ).
    ENDIF.

    serialize_longtexts( ii_xml         = io_xml
                         iv_longtext_id = c_longtext_id_prog ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_PROG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SCVI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_scvi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_scvi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SCVI implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    SELECT SINGLE chuser
    FROM shdsvci
    INTO rv_user
    WHERE scvariant = lv_screen_variant.
    IF sy-subrc <> 0
    OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_DELETE_SC_VARIANT'
      EXPORTING
        scvariant        = lv_screen_variant
      EXCEPTIONS
        variant_enqueued = 1
        no_correction    = 2
        scvariant_used   = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    io_xml->read(
      EXPORTING
        iv_name = 'SCVI'
      CHANGING
        cg_data = ls_screen_variant ).

    CALL FUNCTION 'ENQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-shdsvci-scvariant
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE e413(ms) WITH ls_screen_variant-shdsvci-scvariant INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

*   Populate user details
    ls_screen_variant-shdsvci-crdate = sy-datum.
    ls_screen_variant-shdsvci-cruser = sy-uname.
    ls_screen_variant-shdsvci-chdate = sy-datum.
    ls_screen_variant-shdsvci-chuser = sy-uname.

    MODIFY shdsvci    FROM ls_screen_variant-shdsvci.
    MODIFY shdsvtxci  FROM TABLE ls_screen_variant-shdsvtxci[].
    MODIFY shdsvfvci  FROM TABLE ls_screen_variant-shdsvfvci[].
    MODIFY shdguixt   FROM TABLE ls_screen_variant-shdguixt[].
    MODIFY shdgxtcode FROM TABLE ls_screen_variant-shdgxtcode[].

    CALL FUNCTION 'DEQUEUE_ESSCVARCIU'
      EXPORTING
        scvariant = ls_screen_variant-shdsvci-scvariant.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_screen_variant TYPE scvariant.

    lv_screen_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_SC_VARIANT_DB'
      EXPORTING
        scvariant  = lv_screen_variant
      EXCEPTIONS
        no_variant = 1
        OTHERS     = 2.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    rv_active = is_active( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_screen_variant TYPE ty_screen_variant.

    ls_screen_variant-shdsvci-scvariant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_SC_VARIANT_DB'
      EXPORTING
        scvariant        = ls_screen_variant-shdsvci-scvariant
      IMPORTING
        header_scvariant = ls_screen_variant-shdsvci
      TABLES
        values_scvariant = ls_screen_variant-shdsvfvci[]
        guixt_scripts    = ls_screen_variant-shdguixt[]
      EXCEPTIONS
        no_variant       = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

*   Clear all user details
    CLEAR: ls_screen_variant-shdsvci-crdate,
           ls_screen_variant-shdsvci-cruser,
           ls_screen_variant-shdsvci-chdate,
           ls_screen_variant-shdsvci-chuser.

    SELECT *
    FROM shdsvtxci
    INTO TABLE ls_screen_variant-shdsvtxci[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant
    ORDER BY PRIMARY KEY.

    SELECT *
    FROM shdgxtcode
    INTO TABLE ls_screen_variant-shdgxtcode[]
    WHERE scvariant = ls_screen_variant-shdsvci-scvariant
    ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'SCVI'
                 ig_data = ls_screen_variant ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SCVI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHI5 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shi5=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shi5=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHI5 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_extension = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      ls_msg              TYPE hier_mess,
      lv_found_users      TYPE hier_yesno,
      ls_check_extensions TYPE treenamesp,
      lt_check_extensions TYPE TABLE OF treenamesp,
      lv_obj_name         TYPE ko200-obj_name.

    " STREE_EXTENSION_DELETE shows a popup so do the same here

    ls_check_extensions-extension = mv_extension.
    INSERT ls_check_extensions INTO TABLE lt_check_extensions.

    CALL FUNCTION 'STREE_CHECK_EXTENSION'
      IMPORTING
        message         = ls_msg
      TABLES
        check_extension = lt_check_extensions.

    READ TABLE lt_check_extensions INTO ls_check_extensions INDEX 1.
    IF ls_check_extensions-original = abap_false.
      Lcx_abapgit_exception=>raise( 'Delete enhancement ID in your source system' ).
    ENDIF.

    lv_obj_name = mv_extension.

    CALL FUNCTION 'STREE_TRANSPORT_CHECK'
      EXPORTING
        object   = 'SHI5'
        obj_name = lv_obj_name
      IMPORTING
        message  = ls_msg.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'STREE_EXTENSION_USAGE'
      EXPORTING
        extension         = mv_extension
        no_display        = abap_true
      IMPORTING
        message           = ls_msg
        extension_is_used = lv_found_users.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF lv_found_users = abap_true.
      Lcx_abapgit_exception=>raise( 'Enhancement ID is still used' ).
    ENDIF.

    CALL FUNCTION 'STREE_TRANSPORT_INSERT'
      EXPORTING
        object   = 'SHI5'
        obj_name = lv_obj_name
      IMPORTING
        message  = ls_msg.

    IF ls_msg-msgty = 'E'.
      MESSAGE ID ls_msg-msgid TYPE ls_msg-msgty NUMBER ls_msg-msgno
        WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    DELETE FROM ttree_ext WHERE extension = mv_extension.
    DELETE FROM ttree_extt WHERE extension = mv_extension.

    IF ls_check_extensions-transport = abap_false.
      " no transportable Devclass -> delete TADIR
      tadir_delete( ).
    ENDIF.

    " reset some internal tables
    CALL FUNCTION 'STREE_RESET_FUGR_SHI5_TABLES'.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    " We cannot use STREE_EXTENSION_NAME_CREATE
    " the create logic is directly tied to the UI
    "
    " Do it like here LSHI20F01 -> SAVE_DATA

    DATA: ls_extension TYPE ty_extension.

    io_xml->read(
      EXPORTING
        iv_name = 'SHI5'
      CHANGING
        cg_data = ls_extension ).

    INSERT ttree_ext  FROM ls_extension-header.

    DELETE FROM ttrees WHERE extension = ls_extension-header-extension.
    MODIFY ttrees FROM TABLE ls_extension-sequences.

    DELETE FROM ttree_extt WHERE extension = ls_extension-header-extension.
    MODIFY ttree_extt FROM TABLE ls_extension-texts.

    corr_insert( iv_package ).

    tadir_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_extension_header TYPE ttree_ext.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension_header.

    rv_bool = boolc( ls_extension_header IS NOT INITIAL ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    DATA: lt_extension TYPE STANDARD TABLE OF ttree_ext.
    FIELD-SYMBOLS: <ls_extension> LIKE LINE OF lt_extension.

    INSERT INITIAL LINE INTO TABLE lt_extension ASSIGNING <ls_extension>.
    <ls_extension>-extension = mv_extension.

    CALL FUNCTION 'STREE_EXTENSION_NAME_F4'
      EXPORTING
        originals_only       = abap_true
      TABLES
        show_only_extensions = lt_extension.

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_extension TYPE ty_extension.

    CALL FUNCTION 'STREE_EXTENSION_EXISTS'
      EXPORTING
        extension        = mv_extension
      IMPORTING
        extension_header = ls_extension-header.

    SELECT * FROM ttree_extt
             INTO TABLE ls_extension-texts
             WHERE extension = mv_extension ORDER BY PRIMARY KEY.

    SELECT * FROM ttrees
            INTO TABLE ls_extension-sequences
            WHERE extension = mv_extension ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'SHI5'
                 ig_data = ls_extension ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHI5 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHI8 <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shi8=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shi8=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHI8 implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    mv_assignment_id = ms_item-obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_deleted TYPE abap_bool,
          ls_message TYPE hier_mess.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_DELETE'
      EXPORTING
        assignment_id = mv_assignment_id
      IMPORTING
        id_deleted    = lv_deleted
        message       = ls_message.

    IF lv_deleted = abap_false.
      Lcx_abapgit_exception=>raise( |{ ls_message-msgtxt }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_assignment_data TYPE ttree_sfw_nodes,
          ls_node_data       TYPE hier_iface,
          lv_saved           TYPE abap_bool,
          ls_message         TYPE hier_mess.

    io_xml->read(
      EXPORTING
        iv_name = 'SHI8'
      CHANGING
        cg_data = ls_assignment_data ).

    ls_node_data-tree_id = ls_assignment_data-tree_id.
    ls_node_data-node_id = ls_assignment_data-node_id.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_SAVE'
      EXPORTING
        assignment_id = ls_assignment_data-sfw_ass_id
        switch_id     = ls_assignment_data-switch_id
        reaction      = ls_assignment_data-reaction
        node_data     = ls_node_data
      IMPORTING
        data_saved    = lv_saved
        message       = ls_message.

    IF lv_saved = abap_false.
      Lcx_abapgit_exception=>raise( |{ ls_message-msgtxt }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_ID_EXISTS'
      EXPORTING
        assignment_id = mv_assignment_id
      IMPORTING
        exists        = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lt_assignments     TYPE STANDARD TABLE OF hier_sfw_assignment_id,
          ls_assignment      LIKE LINE OF lt_assignments,
          lt_assignment_data TYPE STANDARD TABLE OF ttree_sfw_nodes,
          ls_assignment_data LIKE LINE OF lt_assignment_data.

    ls_assignment-sfw_ass_id = mv_assignment_id.
    INSERT ls_assignment INTO TABLE lt_assignments.

    CALL FUNCTION 'STREE_SFW_ASSIGNMENT_READ'
      TABLES
        it_assignments     = lt_assignments
        et_assignment_data = lt_assignment_data.

    READ TABLE lt_assignment_data INTO ls_assignment_data
                                  INDEX 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error serializing { ms_item-obj_type } { ms_item-obj_name  }| ).
    ENDIF.

    io_xml->add( iv_name = 'SHI8'
                 ig_data = ls_assignment_data ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHI8 implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SHLP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_shlp=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_shlp=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SHLP implementation.
*"* method's implementations
*include methods.
  METHOD adjust_exit.

    CONSTANTS lc_standard_exit TYPE dd30v-selmexit VALUE 'RS_DD_SELMEXIT'.

    IF cv_exit IS NOT INITIAL
    AND Lcl_abapgit_factory=>get_function_module( )->function_exists( cv_exit ) = abap_false.
      " If exit function does not exist, replace it with standard SAP function
      " which exists in 7.02 and higher
      cv_exit = lc_standard_exit.
    ENDIF.

  ENDMETHOD.
  METHOD check_exit.

    DATA lv_exit TYPE dd30v-selmexit.

    rv_done = abap_true.

    IF iv_exit IS NOT INITIAL.
      " Check if exit function is set correctly
      SELECT SINGLE selmexit FROM dd30v INTO lv_exit WHERE shlpname = ms_item-obj_name.
      IF sy-subrc = 0 AND lv_exit <> iv_exit.
        rv_done = abap_false.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD handle_dependencies.

    " For search helps with dependency on exit function, we use two phases:
    " 1) DDIC phase:
    "    - If function does not exit, replace it with a standard SAP function
    " 2) LATE phase
    "    - If function was replaced, change it to the correct exit function
    CASE iv_step.
      WHEN Lif_abapgit_object=>gc_step_id-ddic.
        adjust_exit( CHANGING cv_exit = cv_exit ).

      WHEN Lif_abapgit_object=>gc_step_id-late.
        cv_done = check_exit( cv_exit ).

      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE as4user FROM dd30l INTO rv_user
      WHERE shlpname = ms_item-obj_name
      AND as4local = 'A'.                               "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    delete_ddic( 'H' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_name  TYPE ddobjname,
          lv_done  TYPE abap_bool,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.

    io_xml->read( EXPORTING iv_name = 'DD30V'
                  CHANGING cg_data = ls_dd30v ).

    handle_dependencies(
      EXPORTING
        iv_step = iv_step
      CHANGING
        cv_exit = ls_dd30v-selmexit
        cv_done = lv_done ).

    IF lv_done = abap_true.
      RETURN.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DD31V_TABLE'
                  CHANGING cg_data = lt_dd31v ).
    io_xml->read( EXPORTING iv_name = 'DD32P_TABLE'
                  CHANGING cg_data = lt_dd32p ).
    io_xml->read( EXPORTING iv_name = 'DD33V_TABLE'
                  CHANGING cg_data = lt_dd33v ).

    corr_insert( iv_package = iv_package
                 ig_object_class = 'DICT' ).

    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_PUT'
      EXPORTING
        name              = lv_name
        dd30v_wa          = ls_dd30v
      TABLES
        dd31v_tab         = lt_dd31v
        dd32p_tab         = lt_dd32p
        dd33v_tab         = lt_dd33v
      EXCEPTIONS
        shlp_not_found    = 1
        name_inconsistent = 2
        shlp_inconsistent = 3
        put_failure       = 4
        put_refused       = 5
        OTHERS            = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_shlpname TYPE dd30l-shlpname.

    SELECT SINGLE shlpname FROM dd30l INTO lv_shlpname
      WHERE shlpname = ms_item-obj_name.
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-ddic TO rt_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECT=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_name  TYPE ddobjname,
          lv_state TYPE ddgotstate,
          ls_dd30v TYPE dd30v,
          lt_dd31v TYPE TABLE OF dd31v,
          lt_dd32p TYPE TABLE OF dd32p,
          lt_dd33v TYPE TABLE OF dd33v.

    FIELD-SYMBOLS: <ls_dd32p> LIKE LINE OF lt_dd32p.


    lv_name = ms_item-obj_name.

    CALL FUNCTION 'DDIF_SHLP_GET'
      EXPORTING
        name          = lv_name
        state         = 'A'
        langu         = mv_language
      IMPORTING
        gotstate      = lv_state
        dd30v_wa      = ls_dd30v
      TABLES
        dd31v_tab     = lt_dd31v
        dd32p_tab     = lt_dd32p
        dd33v_tab     = lt_dd33v
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    IF ls_dd30v IS INITIAL OR lv_state <> 'A'.
      RETURN.
    ENDIF.

    CLEAR: ls_dd30v-as4user,
           ls_dd30v-as4date,
           ls_dd30v-as4time.

    LOOP AT lt_dd32p ASSIGNING <ls_dd32p>.
* clear information inherited from domain
      CLEAR: <ls_dd32p>-domname,
        <ls_dd32p>-headlen,
        <ls_dd32p>-scrlen1,
        <ls_dd32p>-scrlen2,
        <ls_dd32p>-datatype,
        <ls_dd32p>-leng,
        <ls_dd32p>-outputlen,
        <ls_dd32p>-decimals,
        <ls_dd32p>-lowercase,
        <ls_dd32p>-signflag,
        <ls_dd32p>-convexit.
    ENDLOOP.

    io_xml->add( iv_name = 'DD30V'
                 ig_data = ls_dd30v ).
    io_xml->add( ig_data = lt_dd31v
                 iv_name = 'DD31V_TABLE' ).
    io_xml->add( ig_data = lt_dd32p
                 iv_name = 'DD32P_TABLE' ).
    io_xml->add( ig_data = lt_dd33v
                 iv_name = 'DD33V_TABLE' ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SHLP implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SKTD <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sktd=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sktd=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SKTD implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_data TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-NAME'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-TYPE'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_SYSTEM'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-VERSION'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'REF_OBJECT-URI'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'REF_OBJECT-DESCRIPTION'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_AT'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CREATED_BY'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_AT'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-CHANGED_BY'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-MASTER_LANGUAGE'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-RESPONSIBLE'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_data = cs_data ).

    clear_field(
      EXPORTING
        iv_fieldname          = 'METADATA-LINKS'
      CHANGING
        cs_data = cs_data ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
        is_item     = is_item
        iv_language = iv_language ).

    mv_object_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_data TYPE ('CL_KTD_OBJECT_DATA=>TY_KTD_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_KTD_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |SKTD not supported by your NW release| ).
    ENDTRY.

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mi_wb_object_operator IS BOUND.
      ri_wb_object_operator = mi_wb_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'SKTD'.
    ls_object_type-subtype_wb = 'TYP'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_object_key
          RECEIVING
            result      = mi_wb_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ri_wb_object_operator = mi_wb_object_operator.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).

        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          IMPORTING
            eo_object_data = li_object_data_model.

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA:
      lx_error              TYPE REF TO cx_root,
      li_wb_object_operator TYPE REF TO object.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~DELETE')
          EXPORTING
            transport_request = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA li_wb_object_operator TYPE REF TO object.
    DATA li_object_data_model  TYPE REF TO if_wb_object_data_model.

    FIELD-SYMBOLS <ls_data> TYPE any.

    ASSIGN mr_data->* TO <ls_data>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'SKTD'
      CHANGING
        cg_data = <ls_data> ).

    li_wb_object_operator = get_wb_object_operator( ).

    CREATE OBJECT li_object_data_model TYPE ('CL_KTD_OBJECT_DATA').
    li_object_data_model->set_data( <ls_data> ).

    tadir_insert( iv_package ).

    IF Lif_abapgit_object~exists( ) = abap_true.

      CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
        EXPORTING
          io_object_data    = li_object_data_model
          transport_request = iv_transport.

    ELSE.

      CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
        EXPORTING
          io_object_data    = li_object_data_model
          data_selection    = 'P' " if_wb_object_data_selection_co=>c_properties
          package           = iv_package
          transport_request = iv_transport.

      CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
        EXPORTING
          io_object_data    = li_object_data_model
          data_selection    = 'D' " if_wb_object_data_selection_co=>c_data_content
          transport_request = iv_transport.
    ENDIF.

    CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~ACTIVATE').

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        mi_persistence->get(
            p_object_key           = mv_object_key
            p_version              = 'A'
            p_existence_check_only = abap_true ).
        rv_bool = abap_true.

      CATCH cx_swb_exception.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'WBS_ENQUEUE_STRU'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_wb_object_operator TYPE REF TO object,
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS <ls_data> TYPE any.

    ASSIGN mr_data->* TO <ls_data>.
    ASSERT sy-subrc = 0.

    li_wb_object_operator = get_wb_object_operator( ).

    TRY.
        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
          IMPORTING
            data           = <ls_data>
            eo_object_data = li_object_data_model.

        clear_fields( CHANGING cs_data = <ls_data> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
      iv_name = 'SKTD'
      ig_data = <ls_data> ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SKTD implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SMTG <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_smtg=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_smtg=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SMTG implementation.
*"* method's implementations
*include methods.
  METHOD add_component.

    DATA:
      ls_component LIKE LINE OF ct_components,
      lo_typedescr TYPE REF TO cl_abap_typedescr.

    cl_abap_structdescr=>describe_by_name(
      EXPORTING
        p_name         = iv_structure_name
      RECEIVING
        p_descr_ref    = lo_typedescr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |SMTG not supported| ).
    ENDIF.

    ls_component-name = iv_fielname.
    ls_component-type ?= lo_typedescr.
    INSERT ls_component INTO TABLE ct_components.

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE data.

    ASSIGN
      COMPONENT iv_fieldname
      OF STRUCTURE cg_header
      TO <lg_field>.
    ASSERT sy-subrc = 0.

    CLEAR: <lg_field>.

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_template_id = ms_item-obj_name.
    mo_structdescr = get_structure( ).

  ENDMETHOD.
  METHOD get_structure.

    DATA: lt_components TYPE abap_component_tab.

    add_component(
      EXPORTING
        iv_fielname       = `HEADER`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GS_TMPL_HDR`
      CHANGING
        ct_components     = lt_components ).

    add_component(
      EXPORTING
        iv_fielname       = `HEADER_T`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GT_TMPL_HDR_T`
      CHANGING
        ct_components     = lt_components ).

    add_component(
      EXPORTING
        iv_fielname       = `CONTENT`
        iv_structure_name = `IF_SMTG_EMAIL_TEMPLATE=>TY_GT_TMPL_CONT`
      CHANGING
        ct_components     = lt_components ).

    ro_structdescr = cl_abap_structdescr=>create( lt_components ).

  ENDMETHOD.
  METHOD get_template.

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root,
      lo_template TYPE REF TO object.

    FIELD-SYMBOLS:
      <lg_template> TYPE data,
      <lg_header>   TYPE data,
      <lt_header>   TYPE INDEX TABLE,
      <lt_content>  TYPE INDEX TABLE.


    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER_T'
      OF STRUCTURE <lg_template>
      TO <lt_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'CONTENT'
      OF STRUCTURE <lg_template>
      TO <lt_content>.
    ASSERT sy-subrc = 0.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
          EXPORTING
            iv_id       = mv_template_id
          RECEIVING
            ro_instance = lo_template.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_HDR')
          RECEIVING
            rs_tmpl_hdr = <lg_header>.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_HDR_T_ALL')
          RECEIVING
            rt_tmpl_hdr_t = <lt_header>.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~GET_TMPL_CONT_ALL')
          RECEIVING
            rt_tmpl_cont = <lt_content>.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    es_template = <lg_template>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      lr_template TYPE REF TO data.

    FIELD-SYMBOLS:
      <lg_template>          TYPE data,
      <lv_last_changed_user> TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    get_template( IMPORTING es_template = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER-LST_CH_USER_ACCT'
      OF STRUCTURE <lg_template>
      TO <lv_last_changed_user>.
    ASSERT sy-subrc = 0.

    rv_user = <lv_last_changed_user>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>delete
          EXPORTING
            iv_id = mv_template_id.
      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root,
      lo_template TYPE REF TO object.

    FIELD-SYMBOLS:
      <lg_template>    TYPE data,
      <lg_header>      TYPE data,
      <lt_header>      TYPE INDEX TABLE,
      <lt_content>     TYPE INDEX TABLE,
      <lg_name>        TYPE data,
      <lg_description> TYPE data,
      <lg_header_text> TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'SMTG'
      CHANGING
        cg_data = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'HEADER_T'
      OF STRUCTURE <lg_template>
      TO <lt_header>.
    ASSERT sy-subrc = 0.

    ASSIGN
      COMPONENT 'CONTENT'
      OF STRUCTURE <lg_template>
      TO <lt_content>.
    ASSERT sy-subrc = 0.

    TRY.
        IF Lif_abapgit_object~exists( ) = abap_true.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
            EXPORTING
              iv_id       = mv_template_id
            RECEIVING
              ro_instance = lo_template.
        ELSE.
          CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>create
            EXPORTING
              is_tmpl_hdr       = <lg_header>
            RECEIVING
              ro_email_template = lo_template.
        ENDIF.

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SET_TMPL_CONT_ALL')
          EXPORTING
            it_tmpl_cont = <lt_content>.

        READ TABLE <lt_header> ASSIGNING <lg_header_text>
                               INDEX 1.
        IF sy-subrc = 0.
          ASSIGN
            COMPONENT 'NAME'
            OF STRUCTURE <lg_header_text>
            TO <lg_name>.
          ASSERT sy-subrc = 0.

          ASSIGN
            COMPONENT 'DESCRIPTION'
            OF STRUCTURE <lg_header_text>
            TO <lg_description>.
          ASSERT sy-subrc = 0.

          CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SET_TEXT')
            EXPORTING
              iv_name        = <lg_name>
              iv_description = <lg_description>.
        ENDIF.

        tadir_insert( iv_package ).
        corr_insert( iv_package ).

        CALL METHOD lo_template->('IF_SMTG_EMAIL_TEMPLATE~SAVE')
          EXPORTING
            iv_lock   = abap_true
            iv_commit = abap_true
            iv_wait   = abap_true.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    TRY.
        CALL METHOD ('CL_SMTG_EMAIL_TEMPLATE')=>get
          EXPORTING
            iv_id = mv_template_id.

        rv_bool = abap_true.

      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'E_SMTG'
                                            iv_argument    = |{ mv_template_id }| ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      lr_template TYPE REF TO data,
      lx_error    TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <lg_template> TYPE data,
      <lg_header>   TYPE data.

    CREATE DATA lr_template TYPE HANDLE mo_structdescr.
    ASSIGN lr_template->* TO <lg_template>.
    ASSERT sy-subrc = 0.

    get_template( IMPORTING es_template = <lg_template> ).

    ASSIGN
      COMPONENT 'HEADER'
      OF STRUCTURE <lg_template>
      TO <lg_header>.
    ASSERT sy-subrc = 0.

    TRY.
        clear_field( EXPORTING iv_fieldname = 'CREA_DATE_TIME'   CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'CREA_USER_ACCT'   CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'LST_CH_DATE_TIME' CHANGING cg_header = <lg_header> ).
        clear_field( EXPORTING iv_fieldname = 'LST_CH_USER_ACCT' CHANGING cg_header = <lg_header> ).

        io_xml->add(
            iv_name = 'SMTG'
            ig_data = <lg_template> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SMTG implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SOBJ <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sobj=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sobj=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SOBJ implementation.
*"* method's implementations
*include methods.
  METHOD get_field_rules.

    ri_rules = Lcl_abapgit_field_rules=>create( ).
    ri_rules->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CREA_USER'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CREA_DATE'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CREA_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CHAN_USER'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CHAN_DATE'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'CHAN_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'ACTV_USER'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'ACTV_DATE'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'ACTV_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'REL_USER'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-user
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'REL_DATE'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-date
    )->add(
      iv_table     = 'TOJTB'
      iv_field     = 'REL_TIME'
      iv_fill_rule = Lif_abapgit_field_rules=>c_fill_rule-time ).

  ENDMETHOD.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        io_field_rules = get_field_rules( )
        is_item        = ms_item
        iv_language    = mv_language.

  ENDMETHOD.
  METHOD get_program.
    SELECT SINGLE progname INTO rv_program FROM tojtb WHERE name = ms_item-obj_name.
  ENDMETHOD.
  METHOD is_locked.
    rv_is_locked = boolc( is_objtype_locked( ) = abap_true OR is_program_locked( ) = abap_true ).
  ENDMETHOD.
  METHOD is_objtype_locked.
    CONSTANTS lc_tabname TYPE tabname VALUE 'SWOTBASDAT'.
    DATA lv_varkey TYPE vim_enqkey.

    rv_is_locked = abap_false.
    lv_varkey = ms_item-obj_name.

    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname      = lc_tabname
        varkey       = lv_varkey
      EXCEPTIONS
        foreign_lock = 1
        OTHERS       = 999.
    IF sy-subrc IS NOT INITIAL.
      rv_is_locked = abap_true.
    ELSE.
      CALL FUNCTION 'DEQUEUE_E_TABLE'
        EXPORTING
          tabname = lc_tabname
          varkey  = lv_varkey.
    ENDIF.
  ENDMETHOD.
  METHOD is_program_locked.
    CONSTANTS lc_enqueue_exclusive TYPE enqmode VALUE 'X'.
    DATA lv_progname TYPE progname.

    rv_is_locked = abap_false.
    lv_progname = get_program( ).

    IF lv_progname IS NOT INITIAL.
      CALL FUNCTION 'ENQUEUE_ESRDIRE'
        EXPORTING
          mode_trdir   = lc_enqueue_exclusive
          name         = lv_progname
        EXCEPTIONS
          foreign_lock = 1
          OTHERS       = 999.
      IF sy-subrc IS NOT INITIAL.
        rv_is_locked = abap_true.
      ELSE.
        CALL FUNCTION 'DEQUEUE_ESRDIRE'
          EXPORTING
            mode_trdir = lc_enqueue_exclusive
            name       = lv_progname.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    DATA: BEGIN OF ls_userinfo,
            crea_user TYPE tojtb-crea_user,
            chan_user TYPE tojtb-chan_user,
          END   OF ls_userinfo.

    SELECT SINGLE
        crea_user
        chan_user
    INTO (ls_userinfo-crea_user, ls_userinfo-chan_user)
    FROM tojtb WHERE name = ms_item-obj_name.

    IF ls_userinfo-chan_user IS INITIAL.
      ls_userinfo-chan_user = ls_userinfo-crea_user.
    ENDIF.
    rv_user = ls_userinfo-chan_user.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = is_locked( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    "No need as GENERIC class already handles it
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SOBJ implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SPPF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sppf=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sppf=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SPPF implementation.
*"* method's implementations
*include methods.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        is_item     = ms_item
        iv_language = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    set_default_transport( iv_transport ).

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    set_default_transport( iv_transport ).

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SPPF implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SQSC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sqsc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sqsc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SQSC implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    FIELD-SYMBOLS: <lv_dbproxyname> TYPE ty_abap_name.

    super->constructor( is_item     = is_item
                        iv_language = iv_language ).

    TRY.
        CREATE OBJECT mo_proxy
          TYPE ('CL_DDIC_WB_DBPROC_PROXY').

        ASSIGN ('MO_PROXY->IF_DDIC_WB_DBPROC_PROXY~DBPROXYNAME')
            TO <lv_dbproxyname>.
        ASSERT sy-subrc = 0.

      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |SQSC not supported| ).
    ENDTRY.

    <lv_dbproxyname> = ms_item-obj_name.

  ENDMETHOD.
  METHOD delete_interface_if_it_exists.

    DATA: ls_item      TYPE Lif_abapgit_definitions=>ty_item,
          lo_interface TYPE REF TO Lcl_abapgit_object_intf.

    " The interface is managed by the proxy. If abapGit
    " has created it before we have to delete it. Otherwise
    " if_dbproc_proxy_ui~create will throw errors.

    ls_item-obj_name = iv_interface.
    ls_item-obj_type = 'INTF'.

    IF Lcl_abapgit_objects=>exists( ls_item ) = abap_true.

      CREATE OBJECT lo_interface
        EXPORTING
          is_item     = ls_item
          iv_language = mv_language.

      lo_interface->Lif_abapgit_object~delete( iv_package   = iv_package
                                               iv_transport = iv_transport ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~READ_FROM_SOURCE')
          EXPORTING
            if_version     = 'A'
          IMPORTING
            ef_change_user = rv_user.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~DELETE')
          EXPORTING
            if_transport_req = iv_transport.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_proxy TYPE ty_proxy,
          lx_error TYPE REF TO cx_root.

    io_xml->read(
      EXPORTING
        iv_name = 'SQSC'
      CHANGING
        cg_data = ls_proxy ).

    IF Lif_abapgit_object~exists( ) = abap_false.

      delete_interface_if_it_exists(
          iv_package   = iv_package
          iv_transport = iv_transport
          iv_interface = ls_proxy-header-interface_pool ).

      CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~CREATE')
        EXPORTING
          if_interface_pool = ls_proxy-header-interface_pool
          if_transport_req  = iv_transport
          if_package        = iv_package
          if_langu          = mv_language.

    ENDIF.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~WRITE_TO_SOURCE')
          EXPORTING
            if_transport_req  = iv_transport
            is_header         = ls_proxy-header
            it_parameter      = ls_proxy-parameters
            it_parameter_type = ls_proxy-parameter_types.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~WRITE_DESCR')
          EXPORTING
            if_langu = mv_language
            if_descr = ls_proxy-description.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~ACTIVATE').

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~EXISTS')
      RECEIVING
        ef_exists = rv_bool.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_ADT_LINK=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_proxy TYPE ty_proxy,
          lx_error TYPE REF TO cx_root.

    TRY.
        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~READ_FROM_SOURCE')
          EXPORTING
            if_version        = 'A'
          IMPORTING
            es_header         = ls_proxy-header
            et_parameter      = ls_proxy-parameters
            et_parameter_type = ls_proxy-parameter_types.

        CALL METHOD mo_proxy->('IF_DBPROC_PROXY_UI~READ_DESCR')
          EXPORTING
            if_langu   = mv_language
            if_version = 'A'
          IMPORTING
            ef_descr   = ls_proxy-description.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add( iv_name = 'SQSC'
                 ig_data = ls_proxy ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SQSC implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SRVB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_srvb=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_srvb=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SRVB implementation.
*"* method's implementations
*include methods.
  METHOD clear_field.

    FIELD-SYMBOLS: <lv_value> TYPE data.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cs_service_binding
           TO <lv_value>.
    ASSERT sy-subrc = 0.

    CLEAR: <lv_value>.

  ENDMETHOD.
  METHOD clear_fields.

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-VERSION'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-CREATED_AT'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-CREATED_BY'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-CHANGED_AT'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-CHANGED_BY'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-LANGUAGE'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-PACKAGE_REF'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-MASTER_SYSTEM'
      CHANGING
        cs_service_binding = cs_service_binding ).

    clear_field(
      EXPORTING
        iv_fieldname       = 'METADATA-LINKS'
      CHANGING
        cs_service_binding = cs_service_binding ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      is_item     = is_item
      iv_language = iv_language ).

    mv_service_binding_key = ms_item-obj_name.

    TRY.
        CREATE DATA mr_service_binding TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
        CREATE OBJECT mi_persistence TYPE ('CL_SRVB_OBJECT_PERSIST').

      CATCH cx_sy_create_error.
        Lcx_abapgit_exception=>raise( |SRVB not supported by your NW release| ).
    ENDTRY.

    mv_is_inactive_supported = is_ai_supported( ).

  ENDMETHOD.
  METHOD get_object_data.

    FIELD-SYMBOLS:
      <ls_service_binding> TYPE any,
      <lv_language>        TYPE data.

    ASSIGN mr_service_binding->* TO <ls_service_binding>.
    ASSERT sy-subrc = 0.

    io_xml->read(
      EXPORTING
        iv_name = 'SRVB'
      CHANGING
        cg_data = <ls_service_binding> ).


    " We have to set the language explicitly,
    " because otherwise the description isn't stored
    ASSIGN COMPONENT 'METADATA-LANGUAGE' OF STRUCTURE <ls_service_binding>
           TO <lv_language>.
    ASSERT sy-subrc = 0.
    <lv_language> = mv_language.

    CREATE OBJECT ro_object_data TYPE ('CL_SRVB_OBJECT_DATA').
    ro_object_data->set_data( p_data = <ls_service_binding> ).

  ENDMETHOD.
  METHOD get_wb_object_operator.

    DATA:
      ls_object_type TYPE wbobjtype,
      lx_error       TYPE REF TO cx_root.

    IF mo_object_operator IS BOUND.
      ro_object_operator = mo_object_operator.
    ENDIF.

    ls_object_type-objtype_tr = 'SRVB'.
    ls_object_type-subtype_wb = 'SVB'.

    TRY.
        CALL METHOD ('CL_WB_OBJECT_OPERATOR')=>('CREATE_INSTANCE')
          EXPORTING
            object_type = ls_object_type
            object_key  = mv_service_binding_key
          RECEIVING
            result      = mo_object_operator.

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    ro_object_operator = mo_object_operator.

  ENDMETHOD.
  METHOD is_ai_supported.
    TRY.
        CREATE OBJECT mr_srvb_svrs_config TYPE ('CL_SRVB_SVRS_CONFIG')
          EXPORTING iv_objtype = 'SRVB'.
      CATCH cx_sy_create_error.
        rv_ai_supported = abap_false.
    ENDTRY.
    CALL METHOD mr_srvb_svrs_config->('HAS_INACTIVE_VERSION')
      RECEIVING
        rv_has_inactive = rv_ai_supported.

  ENDMETHOD.
  METHOD merge_object_data.

    DATA:
      lo_object_data        TYPE REF TO object,
      lo_object_data_old    TYPE REF TO if_wb_object_data_model,
      lr_new                TYPE REF TO data,
      lr_old                TYPE REF TO data,
      lo_wb_object_operator TYPE REF TO object.

    FIELD-SYMBOLS:
      <ls_new>       TYPE any,
      <ls_old>       TYPE any,
      <lv_field_old> TYPE any,
      <lv_field_new> TYPE any.

    CREATE OBJECT lo_object_data TYPE ('CL_SRVB_OBJECT_DATA').
    lo_object_data = io_object_data.

    CREATE DATA lr_new TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_new->* TO <ls_new>.
    ASSERT sy-subrc = 0.

    CREATE DATA lr_old TYPE ('CL_SRVB_OBJECT_DATA=>TY_OBJECT_DATA').
    ASSIGN lr_old->* TO <ls_old>.
    ASSERT sy-subrc = 0.

    CALL METHOD lo_object_data->('IF_WB_OBJECT_DATA_MODEL~GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL'
      IMPORTING
        p_data           = <ls_new>.

    lo_wb_object_operator = get_wb_object_operator( ).

    CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
      EXPORTING
        data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        eo_object_data = lo_object_data_old.

    CALL METHOD lo_object_data_old->('GET_DATA')
      EXPORTING
        p_metadata_only  = abap_false
        p_data_selection = 'AL' " if_wb_object_data_selection_co=>c_all_data
      IMPORTING
        p_data           = <ls_old>.

    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_old> TO <lv_field_old>.
    ASSIGN COMPONENT 'METADATA-DESCRIPTION' OF STRUCTURE <ls_new> TO <lv_field_new>.
    <lv_field_old> = <lv_field_new>.

    CREATE OBJECT ro_object_data_merged TYPE ('CL_SRVB_OBJECT_DATA').

    CALL METHOD ro_object_data_merged->('SET_DATA')
      EXPORTING
        p_data = <ls_old>.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA:
      li_object_data_model TYPE REF TO if_wb_object_data_model.

    TRY.
        mi_persistence->get(
          EXPORTING
            p_object_key  = mv_service_binding_key
            p_version     = 'A'
          CHANGING
            p_object_data = li_object_data_model ).

        rv_user = li_object_data_model->get_changed_by( ).

      CATCH cx_swb_exception.
        rv_user = c_user_unknown.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lx_error TYPE REF TO cx_swb_exception.

    TRY.
        mi_persistence->delete( mv_service_binding_key ).

      CATCH cx_swb_exception INTO lx_error.
        CALL FUNCTION 'DEQUEUE_ESWB_EO'
          EXPORTING
            objtype = ms_item-obj_type
            objname = ms_item-obj_name.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.


    DATA:
      lo_object_data        TYPE REF TO if_wb_object_data_model,
      lx_error              TYPE REF TO cx_root,
      lo_wb_object_operator TYPE REF TO object,
      lo_merged_data_all    TYPE REF TO if_wb_object_data_model,
      lv_version            TYPE r3state.

    TRY.
        lo_object_data = get_object_data( io_xml ).
        lo_wb_object_operator = get_wb_object_operator( ).

        IF mv_is_inactive_supported = abap_true.
          lv_version = 'I'.
        ELSE.
          lv_version = 'A'.
        ENDIF.

        tadir_insert( iv_package ).

        IF Lif_abapgit_object~exists( ) = abap_false.
          "if_wb_adt_plugin_resource_co=>co_sfs_res_category_atomic.
          CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~CREATE')
            EXPORTING
              io_object_data    = lo_object_data
              data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
              version           = lv_version
              package           = iv_package
              transport_request = iv_transport.

        ELSE.

          lo_merged_data_all = merge_object_data( lo_object_data ).
          CALL METHOD lo_wb_object_operator->('IF_WB_OBJECT_OPERATOR~UPDATE')
            EXPORTING
              io_object_data    = lo_merged_data_all
              data_selection    = 'AL' "if_wb_object_data_selection_co=>c_all_data
              version           = lv_version
              transport_request = iv_transport.

        ENDIF.

        corr_insert( iv_package ).

      CATCH cx_root INTO lx_error.
        CALL FUNCTION 'DEQUEUE_ESWB_EO'
          EXPORTING
            objtype = ms_item-obj_type
            objname = ms_item-obj_name.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    Lcl_abapgit_objects_activation=>add_item( ms_item ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA lo_object_data TYPE REF TO if_wb_object_data_model.

    TRY.
        IF mv_is_inactive_supported = abap_true.
          TRY.
              mi_persistence->get(
                EXPORTING
                  p_object_key     = mv_service_binding_key
                  p_version        = 'I'
                  p_data_selection = 'ST'
                CHANGING
                  p_object_data    = lo_object_data ).

            CATCH cx_root.
              mi_persistence->get(
                EXPORTING
                  p_object_key     = mv_service_binding_key
                  p_version        = 'A'
                  p_data_selection = 'ST'
                CHANGING
                  p_object_data    = lo_object_data ).

          ENDTRY.
        ELSE.

          mi_persistence->get(
            EXPORTING
              p_object_key     = mv_service_binding_key
              p_version        = 'A'
              p_data_selection = 'ST'
            CHANGING
              p_object_data    = lo_object_data ).

        ENDIF.
        rv_bool = boolc( lo_object_data IS NOT INITIAL AND lo_object_data->get_object_key( ) IS NOT INITIAL ).
      CATCH cx_root.
        rv_bool = abap_false.
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for( iv_lock_object = 'ESWB_EO'
                                            iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA:
      li_object_data_model  TYPE REF TO if_wb_object_data_model,
      li_wb_object_operator TYPE REF TO object,
      lx_error              TYPE REF TO cx_root.

    FIELD-SYMBOLS:
      <ls_service_binding> TYPE any.

    ASSIGN mr_service_binding->* TO <ls_service_binding>.
    ASSERT sy-subrc = 0.

    TRY.
        li_wb_object_operator = get_wb_object_operator( ).


        CALL METHOD li_wb_object_operator->('IF_WB_OBJECT_OPERATOR~READ')
          EXPORTING
            version        = 'A'
            data_selection = 'AL'
          IMPORTING
            eo_object_data = li_object_data_model.

        li_object_data_model->get_data( IMPORTING p_data = <ls_service_binding> ).

        clear_fields( CHANGING cs_service_binding = <ls_service_binding> ).

      CATCH cx_root INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    io_xml->add(
      iv_name = 'SRVB'
      ig_data = <ls_service_binding> ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SRVB implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_STVI <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_stvi=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_stvi=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_STVI implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_transaction_variant TYPE utcvariant.

    lv_transaction_variant = ms_item-obj_name.

    SELECT SINGLE chuser
    FROM shdtvciu
    INTO rv_user
    WHERE tcvariant = lv_transaction_variant.
    IF sy-subrc <> 0
    OR rv_user IS INITIAL.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_transaction_variant TYPE tcvariant.

    lv_transaction_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_DELETE_VARIANT'
      EXPORTING
        tcvariant                 = lv_transaction_variant
        i_flag_client_independent = abap_true
      EXCEPTIONS
        variant_enqueued          = 1
        no_correction             = 2
        OTHERS                    = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_transaction_variant TYPE ty_transaction_variant.

    io_xml->read(
      EXPORTING
        iv_name = 'STVI'
      CHANGING
        cg_data = ls_transaction_variant ).

    CALL FUNCTION 'ENQUEUE_ESTCVARCIU'
      EXPORTING
        tcvariant = ls_transaction_variant-shdtvciu-tcvariant
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      MESSAGE e413(ms) WITH ls_transaction_variant-shdtvciu-tcvariant INTO Lcx_abapgit_exception=>null.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    corr_insert( iv_package ).

*   Populate user details
    ls_transaction_variant-shdtvciu-crdate = sy-datum.
    ls_transaction_variant-shdtvciu-cruser = sy-uname.
    ls_transaction_variant-shdtvciu-chdate = sy-datum.
    ls_transaction_variant-shdtvciu-chuser = sy-uname.

    MODIFY shdtvciu   FROM ls_transaction_variant-shdtvciu.
    MODIFY shdttciu   FROM TABLE ls_transaction_variant-shdttciu[].
    INSERT shdfvguicu FROM TABLE ls_transaction_variant-shdfvguicu[] ACCEPTING DUPLICATE KEYS.
    INSERT shdtvsvciu FROM TABLE ls_transaction_variant-shdtvsvciu[] ACCEPTING DUPLICATE KEYS.

    CALL FUNCTION 'DEQUEUE_ESTCVARCIU'
      EXPORTING
        tcvariant = ls_transaction_variant-shdtvciu-tcvariant.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_transaction_variant TYPE tcvariant.

    lv_transaction_variant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_EXIST_CHECK_VARIANT'
      EXPORTING
        tcvariant                 = lv_transaction_variant
        i_flag_client_independent = abap_true
      EXCEPTIONS
        no_variant                = 1
        OTHERS                    = 2.
    IF sy-subrc = 0.
      rv_bool = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.

    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.

    rs_metadata = get_metadata( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.

    rv_active = is_active( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_transaction_variant TYPE ty_transaction_variant.

    ls_transaction_variant-shdtvciu-tcvariant = ms_item-obj_name.

    CALL FUNCTION 'RS_HDSYS_READ_TC_VARIANT_DB'
      EXPORTING
        tcvariant               = ls_transaction_variant-shdtvciu-tcvariant
        flag_client_independent = abap_true
      IMPORTING
        header_tcvariant        = ls_transaction_variant-shdtvciu
      TABLES
        screen_variants         = ls_transaction_variant-shdtvsvciu[]
        inactive_functions      = ls_transaction_variant-shdfvguicu[]
      EXCEPTIONS
        no_variant              = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

*   Clear all user details
    CLEAR: ls_transaction_variant-shdtvciu-crdate,
           ls_transaction_variant-shdtvciu-cruser,
           ls_transaction_variant-shdtvciu-chdate,
           ls_transaction_variant-shdtvciu-chuser.

    SELECT *
      FROM shdttciu
      INTO TABLE ls_transaction_variant-shdttciu[]
      WHERE tcvariant = ls_transaction_variant-shdtvciu-tcvariant
      ORDER BY PRIMARY KEY.

    io_xml->add( iv_name = 'STVI'
                 ig_data = ls_transaction_variant ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_STVI implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_SUCU <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_sucu=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_sucu=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_SUCU implementation.
*"* method's implementations
*include methods.
  METHOD get_generic.

    CREATE OBJECT ro_generic
      EXPORTING
        is_item     = ms_item
        iv_language = mv_language.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.
    rv_user = c_user_unknown. " not stored by SAP
  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    get_generic( )->delete( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    get_generic( )->deserialize(
      iv_package = iv_package
      io_xml     = io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    rv_bool = get_generic( )->exists( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = abap_false.

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    get_generic( )->serialize( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_SUCU implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_TOBJ <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_tobj=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_tobj=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_TOBJ implementation.
*"* method's implementations
*include methods.
  METHOD delete_extra.

    DELETE FROM tddat WHERE tabname = iv_tabname.
    DELETE FROM tvdir WHERE tabname = iv_tabname.
    DELETE FROM tvimf WHERE tabname = iv_tabname.

  ENDMETHOD.
  METHOD read_extra.

    SELECT SINGLE * FROM tddat INTO rs_tobj-tddat WHERE tabname = iv_tabname.

    SELECT SINGLE * FROM tvdir INTO rs_tobj-tvdir WHERE tabname = iv_tabname.
    CLEAR: rs_tobj-tvdir-gendate, rs_tobj-tvdir-gentime, rs_tobj-tvdir-devclass.

    SELECT * FROM tvimf INTO TABLE rs_tobj-tvimf WHERE tabname = iv_tabname
      ORDER BY PRIMARY KEY.

  ENDMETHOD.
  METHOD update_extra.
    DATA: lt_current_tvimf TYPE STANDARD TABLE OF tvimf.
    FIELD-SYMBOLS: <ls_tvimf> TYPE tvimf.

    MODIFY tddat FROM is_tobj-tddat.
    MODIFY tvdir FROM is_tobj-tvdir.

    SELECT * INTO TABLE lt_current_tvimf
      FROM tvimf
      WHERE tabname = is_tobj-tddat-tabname
      ORDER BY PRIMARY KEY.

    LOOP AT lt_current_tvimf ASSIGNING <ls_tvimf>.
      READ TABLE is_tobj-tvimf WITH KEY tabname = <ls_tvimf>-tabname
                                        event   = <ls_tvimf>-event
                               TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        DELETE FROM tvimf
          WHERE tabname = <ls_tvimf>-tabname
          AND event = <ls_tvimf>-event.
      ENDIF.
    ENDLOOP.

    MODIFY tvimf FROM TABLE is_tobj-tvimf.
  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    SELECT SINGLE luser FROM objh INTO rv_user
      WHERE objectname = ms_item-obj_name(lv_type_pos)
      AND objecttype = ms_item-obj_name+lv_type_pos.    "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: ls_objh     TYPE objh,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

    IF ls_objh-objecttype = 'L'.
      Lcx_abapgit_exception=>raise( |Use transaction SOBJ to delete transport objects { ls_objh-objectname }| ).
    ENDIF.

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_korrnum            = iv_transport
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'D'
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    delete_extra( ls_objh-objectname ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_objh  TYPE objh,
          ls_objt  TYPE objt,
          lt_objs  TYPE tt_objs,
          lt_objsl TYPE tt_objsl,
          lt_objm  TYPE tt_objm,
          ls_tobj  TYPE ty_tobj.


    io_xml->read( EXPORTING iv_name = 'OBJH'
                  CHANGING cg_data = ls_objh ).
    io_xml->read( EXPORTING iv_name = 'OBJT'
                  CHANGING cg_data = ls_objt ).
    io_xml->read( EXPORTING iv_name = 'OBJS'
                  CHANGING cg_data = lt_objs ).
    io_xml->read( EXPORTING iv_name = 'OBJSL'
                  CHANGING cg_data = lt_objsl ).
    io_xml->read( EXPORTING iv_name = 'OBJM'
                  CHANGING cg_data = lt_objm ).

    CALL FUNCTION 'OBJ_GENERATE'
      EXPORTING
        iv_korrnum            = iv_transport
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_maint_mode         = 'I'
        iv_objecttext         = ls_objt-ddtext
        iv_objcateg           = ls_objh-objcateg
        iv_objtransp          = ls_objh-objtransp
        iv_devclass           = iv_package
      TABLES
        tt_v_obj_s            = lt_objs
        tt_objm               = lt_objm
      EXCEPTIONS
        illegal_call          = 1
        object_not_found      = 2
        generate_error        = 3
        transport_error       = 4
        object_enqueue_failed = 5
        OTHERS                = 6.
    IF sy-subrc <> 0.
* TOBJ has to be saved/generated after the DDIC tables have been
* activated - fixed with late deserialization
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL FUNCTION 'OBJ_SET_IMPORTABLE'
      EXPORTING
        iv_objectname         = ls_objh-objectname
        iv_objecttype         = ls_objh-objecttype
        iv_importable         = ls_objh-importable
      EXCEPTIONS
        object_not_defined    = 1
        invalid               = 2
        transport_error       = 3
        object_enqueue_failed = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

* fm OBJ_GENERATE takes the defaults from the DDIC object
* set OBJTRANSP directly, should be okay looking at the code in OBJ_SET_IMPORTABLE
* locking has been done in OBJ_SET_IMPORTABLE plus recording of transport
    UPDATE objh SET objtransp = ls_objh-objtransp
      WHERE objectname = ls_objh-objectname
      AND objecttype = ls_objh-objecttype.

    io_xml->read( EXPORTING iv_name = 'TOBJ'
                  CHANGING cg_data = ls_tobj ).
    ls_tobj-tvdir-gendate = sy-datum.
    ls_tobj-tvdir-gentime = sy-uzeit.
    ls_tobj-tvdir-devclass = iv_package.

    update_extra( ls_tobj ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_objectname TYPE objh-objectname,
          lv_type_pos   TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    SELECT SINGLE objectname FROM objh INTO lv_objectname
      WHERE objectname = ms_item-obj_name(lv_type_pos)
      AND objecttype = ms_item-obj_name+lv_type_pos.    "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-late TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    DATA: lv_object_name TYPE e071-obj_name.

    lv_object_name = ms_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_pgmid          = 'R3TR'
        iv_object         = ms_item-obj_type
        iv_obj_name       = lv_object_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_objh     TYPE objh,
          ls_objt     TYPE objt,
          lt_objs     TYPE tt_objs,
          lt_objsl    TYPE tt_objsl,
          lt_objm     TYPE tt_objm,
          ls_tobj     TYPE ty_tobj,
          lv_type_pos TYPE i.

    lv_type_pos = strlen( ms_item-obj_name ) - 1.

    ls_objh-objectname = ms_item-obj_name(lv_type_pos).
    ls_objh-objecttype = ms_item-obj_name+lv_type_pos.

    CALL FUNCTION 'CTO_OBJECT_GET'
      EXPORTING
        iv_objectname      = ls_objh-objectname
        iv_objecttype      = ls_objh-objecttype
        iv_language        = mv_language
        iv_sel_objt        = abap_true
        iv_sel_objs        = abap_true
        iv_sel_objsl       = abap_true
        iv_sel_objm        = abap_true
      IMPORTING
        es_objh            = ls_objh
        es_objt            = ls_objt
      TABLES
        tt_objs            = lt_objs
        tt_objsl           = lt_objsl
        tt_objm            = lt_objm
      EXCEPTIONS
        object_not_defined = 1
        OTHERS             = 2.
    IF sy-subrc = 1.
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR: ls_objh-luser,
           ls_objh-ldate.

    SORT lt_objs BY objectname objecttype tabname.
    SORT lt_objsl BY objectname objecttype trwcount.
    SORT lt_objm BY objectname objecttype method.

    io_xml->add( iv_name = 'OBJH'
                 ig_data = ls_objh ).
    io_xml->add( iv_name = 'OBJT'
                 ig_data = ls_objt ).
    io_xml->add( iv_name = 'OBJS'
                 ig_data = lt_objs ).
    io_xml->add( iv_name = 'OBJSL'
                 ig_data = lt_objsl ).
    io_xml->add( iv_name = 'OBJM'
                 ig_data = lt_objm ).

    ls_tobj = read_extra( ls_objh-objectname ).

    IF ls_tobj-tvdir-detail = ``.
      " to prevent xslt serialization error,
      " force clear if numc field is empty
      CLEAR ls_tobj-tvdir-detail.
    ENDIF.

    io_xml->add( iv_name = 'TOBJ'
                 ig_data = ls_tobj ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_TOBJ implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_UCSA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_ucsa=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_ucsa=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_UCSA implementation.
*"* method's implementations
*include methods.
  METHOD clear_dynamic_fields.

    FIELD-SYMBOLS: <lg_header> TYPE any.


    ASSIGN COMPONENT 'HEADER' OF STRUCTURE cg_complete_comm_assembly
           TO <lg_header>.
    ASSERT sy-subrc = 0.

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDBY'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDON'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CREATEDAT'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDBY'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDON'
      CHANGING  cg_header    = <lg_header> ).

    clear_field(
      EXPORTING iv_fieldname = 'CHANGEDAT'
      CHANGING  cg_header    = <lg_header> ).

  ENDMETHOD.
  METHOD clear_field.

    FIELD-SYMBOLS: <lg_field> TYPE any.

    ASSIGN COMPONENT iv_fieldname OF STRUCTURE cg_header
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    CLEAR <lg_field>.

  ENDMETHOD.
  METHOD get_persistence.

    CALL METHOD ('CL_UCON_SA_DB_PERSIST')=>('IF_UCON_SA_PERSIST~GET_INSTANCE')
      EXPORTING
        id       = iv_id
      RECEIVING
        instance = ro_persistence.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA: lv_id                     TYPE ty_id,
          lx_root                   TYPE REF TO cx_root,
          lo_persistence            TYPE REF TO object,
          lr_complete_comm_assembly TYPE REF TO data.

    FIELD-SYMBOLS: <lg_complete_comm_assembly> TYPE any,
                   <lv_user>                   TYPE any.

    lv_id = ms_item-obj_name.

    TRY.
        CREATE DATA lr_complete_comm_assembly TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_complete_comm_assembly->* TO <lg_complete_comm_assembly>.
        ASSERT sy-subrc = 0.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = c_version-active
            language = mv_language
          IMPORTING
            sa       = <lg_complete_comm_assembly>.

        ASSIGN COMPONENT 'CHANGEDBY' OF STRUCTURE <lg_complete_comm_assembly> TO <lv_user>.
        IF sy-subrc = 0.
          rv_user = <lv_user>.
        ENDIF.

      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    DATA: lv_id          TYPE ty_id,
          lx_root        TYPE REF TO cx_root,
          lv_text        TYPE string,
          lo_persistence TYPE REF TO object.

    TRY.
        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~DELETE')
          EXPORTING
            version = c_version-active.

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

    tadir_delete( ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_id                     TYPE ty_id,
          lx_root                   TYPE REF TO cx_root,
          lv_text                   TYPE string,
          lo_persistence            TYPE REF TO object,
          lr_complete_comm_assembly TYPE REF TO data.

    FIELD-SYMBOLS: <lg_complete_comm_assembly> TYPE any.

    TRY.
        CREATE DATA lr_complete_comm_assembly TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_complete_comm_assembly->* TO <lg_complete_comm_assembly>.
        ASSERT sy-subrc = 0.

        io_xml->read(
          EXPORTING
            iv_name = 'UCSA'
          CHANGING
            cg_data = <lg_complete_comm_assembly> ).

        lv_id = ms_item-obj_name.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~CREATE').

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~SAVE')
          EXPORTING
            sa      = <lg_complete_comm_assembly>
            version = c_version-active.

        tadir_insert( iv_package ).

      CATCH cx_root INTO lx_root.
        lv_text = lx_root->get_text( ).
        Lcx_abapgit_exception=>raise( lv_text ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: lv_id          TYPE ty_id,
          lo_persistence TYPE REF TO object.

    lv_id = ms_item-obj_name.

    TRY.
        lo_persistence = get_persistence( lv_id ).

        " Interface IF_UCON_SA_PERSIST and other objects are not present
        " in lower Netweaver realeses. Therefore we have to call them
        " dynamically to be downward comapatible.

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = c_version-active
            language = mv_language.

      CATCH cx_root.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_id                     TYPE ty_id,
          lx_root                   TYPE REF TO cx_root,
          lo_persistence            TYPE REF TO object,
          lr_complete_comm_assembly TYPE REF TO data.

    FIELD-SYMBOLS: <lg_complete_comm_assembly> TYPE any.


    lv_id = ms_item-obj_name.

    TRY.
        CREATE DATA lr_complete_comm_assembly TYPE ('UCONSERVASCOMPLETE').
        ASSIGN lr_complete_comm_assembly->* TO <lg_complete_comm_assembly>.
        ASSERT sy-subrc = 0.

        lo_persistence = get_persistence( lv_id ).

        CALL METHOD lo_persistence->('IF_UCON_SA_PERSIST~LOAD')
          EXPORTING
            version  = c_version-active
            language = mv_language
          IMPORTING
            sa       = <lg_complete_comm_assembly>.

        clear_dynamic_fields( CHANGING cg_complete_comm_assembly = <lg_complete_comm_assembly> ).

        io_xml->add( iv_name = 'UCSA'
                     ig_data = <lg_complete_comm_assembly> ).

      CATCH cx_root INTO lx_root.
        Lcx_abapgit_exception=>raise_with_text( lx_root ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_UCSA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_UDMO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_udmo=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_udmo=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_UDMO implementation.
*"* method's implementations
*include methods.
  METHOD access_free.

    " Release the lock on the object.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        mode                     = 'FREE'
        object                   = ms_object_type
        object_class             = c_transport_object_class
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD access_modify.

* You are reminded that mode modify is the same as insert, with one important difference:

* Mode INSERT is intended for newly created objects, for which a TADIR entry does not yet
* exist. In that case, the system shows a pop-up for the entry of the package, which isn't
* desirable when the SAPGUI is not available.

* In the context of abapGit, the package is known.

    CALL FUNCTION 'RS_ACCESS_PERMISSION'
      EXPORTING
        authority_check          = abap_true
        global_lock              = abap_true
        mode                     = 'MODIFY'
        object                   = ms_object_type
        object_class             = c_transport_object_class
      EXCEPTIONS
        canceled_in_corr         = 1
        enqueued_by_user         = 2
        enqueue_system_failure   = 3
        illegal_parameter_values = 4
        locked_by_author         = 5
        no_modify_permission     = 6
        no_show_permission       = 7
        permission_failure       = 8
        request_language_denied  = 9
        OTHERS                   = 10.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ELSE.
      rv_result = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( is_item  = is_item
                        iv_language = iv_language ).


    " Conversion to Data model
    mv_data_model = is_item-obj_name.
    " Default activation state is active
    mv_activation_state = c_active_state.
    " Derive the data model's text object
    mv_text_object = 'UDMD' && is_item-obj_name.
    " And set the text object to active
    mv_text_object+30(1) = mv_activation_state.
    mv_lxe_text_name = mv_text_object.

    " Correction and Transport System object
    ms_object_type-objtype = c_correction_object_type.
    ms_object_type-objname = is_item-obj_name.


  ENDMETHOD.
  METHOD corr_insert.

    DATA lv_obj_name TYPE tadir-obj_name.

    " You are reminded that SUDM - Data Model has no part objects e.g. no LIMU
    " Therefore global lock is always appropriate

    " You are reminded that the main language (in TADIR) is taken from MV_LANGUAGE.
    lv_obj_name = ms_object_type.

    Lcl_abapgit_factory=>get_cts_api( )->insert_transport_object(
      iv_object   = c_transport_object_class
      iv_obj_name = lv_obj_name
      iv_package  = iv_package
      iv_language = mv_language ).

  ENDMETHOD.
  METHOD deserialize_entities.

    DATA lt_udmo_entities TYPE STANDARD TABLE OF dm41s WITH DEFAULT KEY.
    DATA ls_udmo_entity LIKE LINE OF lt_udmo_entities.


    io_xml->read( EXPORTING iv_name = 'UDMO_ENTITIES'
                  CHANGING  cg_data = lt_udmo_entities ).

    LOOP AT lt_udmo_entities INTO ls_udmo_entity.

      CALL FUNCTION 'SDU_DMO_ENT_PUT'
        EXPORTING
          object = ls_udmo_entity
        EXCEPTIONS
          OTHERS = 0.

    ENDLOOP.

  ENDMETHOD.
  METHOD deserialize_long_texts.

    DATA BEGIN OF ls_udmo_long_text.
    DATA language TYPE dm40t-sprache.
    DATA header   TYPE thead.
    DATA content TYPE xstring.
    DATA END OF ls_udmo_long_text.

    DATA lt_udmo_long_texts LIKE STANDARD TABLE OF ls_udmo_long_text.
    DATA ls_header TYPE thead.

    io_xml->read( EXPORTING iv_name = 'UDMO_LONG_TEXTS'
                  CHANGING  cg_data = lt_udmo_long_texts ).

    LOOP AT lt_udmo_long_texts INTO ls_udmo_long_text.

      ls_udmo_long_text-header-tdfuser = sy-uname.
      ls_udmo_long_text-header-tdfdate = sy-datum.
      ls_udmo_long_text-header-tdftime = sy-uzeit.

      " You are reminded that the target system may already have some texts in
      " existence. So we determine the highest existent version.

      CLEAR ls_header-tdversion.

      SELECT MAX( dokversion )
      INTO ls_header-tdversion
      FROM dokhl
      WHERE id = c_lxe_text_type
      AND object = mv_text_object
      AND langu  = ls_udmo_long_text-language.

      " Increment the version
      ls_header-tdversion = ls_header-tdversion + 1.
      ls_udmo_long_text-header-tdversion = ls_header-tdversion.

      " This function module takes care of the variation in text processing between various objects.
      CALL FUNCTION 'LXE_OBJ_DOKU_PUT_XSTRING'
        EXPORTING
          slang   = mv_language
          tlang   = ls_udmo_long_text-language
          objtype = c_lxe_text_type
          objname = mv_lxe_text_name
          header  = ls_udmo_long_text-header
          content = ls_udmo_long_text-content.


    ENDLOOP.


  ENDMETHOD.
  METHOD deserialize_model.

    DATA ls_dm40l TYPE dm40l.


    io_xml->read( EXPORTING iv_name = 'DM40L'
                  CHANGING cg_data = ls_dm40l ).


    " See SDU_MODEL_PUT
    GET TIME.

    ls_dm40l-flg_frame = abap_true.
    ls_dm40l-fstdate   = sy-datum.
    ls_dm40l-fsttime   = sy-uzeit.
    ls_dm40l-fstuser   = sy-uname.
    ls_dm40l-lstdate   = sy-datum.
    ls_dm40l-lsttime   = sy-uzeit.
    ls_dm40l-lstuser   = sy-uname.

    MODIFY dm40l FROM ls_dm40l.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from SDU_MODEL_PUT' ).
    ENDIF.



  ENDMETHOD.
  METHOD deserialize_short_texts.

    DATA lt_udmo_texts TYPE STANDARD TABLE OF ty_udmo_text_type WITH DEFAULT KEY.
    DATA ls_udmo_text  TYPE ty_udmo_text_type.
    DATA ls_dm40t TYPE dm40t.


    " Deserialize the XML
    io_xml->read( EXPORTING iv_name = 'UDMO_TEXTS'
                  CHANGING  cg_data = lt_udmo_texts ).

    " For every text provided
    LOOP AT lt_udmo_texts INTO ls_udmo_text.

      " Does the text already exist? This is the same logic as used
      " in the FM SDU_MODEL_PUT
      SELECT SINGLE *
        FROM dm40t
        INTO ls_dm40t
        WHERE sprache = ls_udmo_text-sprache
        AND dmoid     = ls_udmo_text-dmoid
        AND as4local  = mv_activation_state.

      IF sy-subrc = 0.
        " There is already an active description for this language
        " but the provided description differs
        IF ls_dm40t-langbez <> ls_udmo_text-langbez.

          ls_dm40t-langbez = ls_udmo_text-langbez.
          ls_dm40t-lstdate = sy-datum.
          ls_dm40t-lsttime = sy-uzeit.
          ls_dm40t-lstuser = sy-uname.

          MODIFY dm40t FROM ls_dm40t.

        ENDIF.
      ELSE.

        " There is no EXISTING active description in this language

        ls_dm40t-as4local = ls_udmo_text-as4local.
        ls_dm40t-dmoid    = ls_udmo_text-dmoid.
        ls_dm40t-langbez  = ls_udmo_text-langbez.
        ls_dm40t-lstdate  = sy-datum.
        ls_dm40t-lsttime  = sy-uzeit.
        ls_dm40t-lstuser  = sy-uname.
        ls_dm40t-sprache  = ls_udmo_text-sprache.


        INSERT dm40t FROM ls_dm40t.

      ENDIF.

    ENDLOOP.


  ENDMETHOD.
  METHOD is_name_permitted.

    " It is unlikely that a serialised data model will have a name that is not permitted. However
    " there may be reservations in TRESE which could prohibit the data model name.
    " So to be safe, we check. Tx SD11 does this check.


    CALL FUNCTION 'SDU_SAA_CHECK'
      EXPORTING
        obj_name   = ms_object_type-objname
        obj_type   = ms_object_type-objtype
      EXCEPTIONS
        wrong_type = 1.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_entities.

    DATA lt_udmo_entities TYPE STANDARD TABLE OF dm41s WITH DEFAULT KEY.
    FIELD-SYMBOLS <ls_udmo_entity> TYPE dm41s.

    SELECT * FROM dm41s
      INTO TABLE lt_udmo_entities
      WHERE dmoid = mv_data_model
      AND as4local = mv_activation_state
      ORDER BY PRIMARY KEY.

    LOOP AT lt_udmo_entities ASSIGNING <ls_udmo_entity>.
      " You are reminded that administrative information, such as last changed by user, date, time is not serialised.
      CLEAR <ls_udmo_entity>-lstuser.
      CLEAR <ls_udmo_entity>-lstdate.
      CLEAR <ls_udmo_entity>-lsttime.
      CLEAR <ls_udmo_entity>-fstuser.
      CLEAR <ls_udmo_entity>-fstdate.
      CLEAR <ls_udmo_entity>-fsttime.
    ENDLOOP.

    " You are reminded that descriptions in other languages do not have to be in existence, although they may.
    IF lines( lt_udmo_entities ) > 0.
      io_xml->add( iv_name = 'UDMO_ENTITIES'
                   ig_data = lt_udmo_entities ).
    ENDIF.

  ENDMETHOD.
  METHOD serialize_long_texts.

    " The model has short texts in multiple languages. These are held in DM40T.

    " The model has a long description also in a main language, with other long descriptions
    " maintained as translations using SE63 Translation Editor. All of these long texts are held in DOK*

    TYPES BEGIN OF ty_language_type.
    TYPES language TYPE dm40t-sprache.
    TYPES END OF ty_language_type.

    DATA BEGIN OF ls_udmo_long_text.
    DATA language TYPE dm40t-sprache.
    DATA header   TYPE thead.
    DATA content TYPE xstring.
    DATA END OF ls_udmo_long_text.

    DATA lt_udmo_long_texts LIKE STANDARD TABLE OF ls_udmo_long_text.
    DATA lt_udmo_languages TYPE STANDARD TABLE OF ty_language_type.
    DATA ls_udmo_language  LIKE LINE OF lt_udmo_languages.
    DATA: lv_error_status  TYPE lxestatprc.


    " In which languages are the short texts are maintained.
    SELECT sprache AS language
      FROM dm40t
      INTO TABLE lt_udmo_languages
      WHERE dmoid    = mv_data_model
      AND as4local = mv_activation_state
      ORDER BY sprache ASCENDING.                       "#EC CI_NOFIRST

    " For every language for which a short text is maintained,
    LOOP AT lt_udmo_languages INTO ls_udmo_language.

      CLEAR ls_udmo_long_text.
      CLEAR lv_error_status.

      ls_udmo_long_text-language = ls_udmo_language-language.

      " You are reminded that this function gets the most recent version of the texts.
      CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
        EXPORTING
          lang    = ls_udmo_language-language
          objtype = c_lxe_text_type
          objname = mv_lxe_text_name
        IMPORTING
          header  = ls_udmo_long_text-header
          content = ls_udmo_long_text-content
          pstatus = lv_error_status.

      CHECK lv_error_status = 'S'. "Success

      " Administrative information is not serialised
      CLEAR ls_udmo_long_text-header-tdfuser.
      CLEAR ls_udmo_long_text-header-tdfdate.
      CLEAR ls_udmo_long_text-header-tdftime.

      CLEAR ls_udmo_long_text-header-tdluser.
      CLEAR ls_udmo_long_text-header-tdldate.
      CLEAR ls_udmo_long_text-header-tdltime.

      APPEND ls_udmo_long_text TO lt_udmo_long_texts.

    ENDLOOP.

    " You are reminded that long texts do not have to be in existence
    IF lines( lt_udmo_long_texts ) > 0.
      io_xml->add( iv_name = 'UDMO_LONG_TEXTS'
                   ig_data = lt_udmo_long_texts ).
    ENDIF.


  ENDMETHOD.
  METHOD serialize_model.

    DATA ls_dm40l TYPE dm40l.

    " See SDU_MODEL_GET.
    SELECT SINGLE *
    FROM dm40l
    INTO ls_dm40l
    WHERE dmoid    = mv_data_model
    AND as4local = mv_activation_state.


    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from UDMO - model serialisation' ).
    ENDIF.

    " You are reminded that administrative data is not serialised.
    CLEAR ls_dm40l-lstdate.
    CLEAR ls_dm40l-lsttime.
    CLEAR ls_dm40l-lstuser.
    CLEAR ls_dm40l-fstdate.
    CLEAR ls_dm40l-fsttime.
    CLEAR ls_dm40l-fstuser.

    io_xml->add( iv_name = 'DM40L'
                 ig_data = ls_dm40l ).

  ENDMETHOD.
  METHOD serialize_short_texts.

    DATA lt_udmo_texts TYPE STANDARD TABLE OF ty_udmo_text_type WITH DEFAULT KEY.
    " You are reminded that administrative information, such as last changed by user, date, time is not serialised.

    " You are reminded that active short texts of all (existent) languages are serialised.

    SELECT sprache dmoid as4local langbez
      FROM dm40t
      INTO CORRESPONDING FIELDS OF TABLE lt_udmo_texts
      WHERE dmoid    = mv_data_model
      AND as4local = mv_activation_state
      ORDER BY sprache ASCENDING.                       "#EC CI_NOFIRST

    " You are reminded that descriptions in other languages do not have to be in existence.
    IF lines( lt_udmo_texts ) > 0.
      io_xml->add( iv_name = 'UDMO_TEXTS'
                   ig_data = lt_udmo_texts ).
    ENDIF.


  ENDMETHOD.
  METHOD update_tree.

    CALL FUNCTION 'RS_TREE_OBJECT_PLACEMENT'
      EXPORTING
        object    = mv_data_model
        operation = 'INSERT'
        type      = c_correction_object_type.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    SELECT SINGLE lstuser INTO rv_user
      FROM dm40l
      WHERE dmoid = mv_data_model
      AND as4local = mv_activation_state.

    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

* You are reminded that this function model checks for
*  - permissions
*  - locks
*  - connection to transport and correction system
*  - deletion of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock

    CALL FUNCTION 'RPY_DATAMODEL_DELETE'
      EXPORTING
        model_name       = mv_data_model
      EXCEPTIONS
        cancelled        = 1
        permission_error = 2
        not_found        = 3
        is_used          = 4
        OTHERS           = 5.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.


  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

* You are reminded that this method checks for
*  - validity of data model name with regard to naming conventions
*  - permissions and locks
*  - connection to transport and correction system
*  - insert of data model, model relations and all documentation
*  - update of object tree
*  - releasing of lock


* Is the data model name compliant with naming conventions?
    is_name_permitted( ).

* Access Permission granted?
    access_modify( ).

* Connection to transport and correction system
    corr_insert( iv_package ).

* Insert the data model, relations and documentation
    TRY.
        deserialize_model( io_xml ).
        deserialize_entities( io_xml ).
        deserialize_short_texts( io_xml ).
        deserialize_long_texts( io_xml ).
        update_tree( ).
        access_free( ).

      CATCH Lcx_abapgit_exception.

        access_free( ).

        Lcx_abapgit_exception=>raise( 'Error in deserialisation of UDMO' ).


    ENDTRY.

    " You are reminded that data models are not relevant for activation.


  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    "  See Function Module SDU_MODEL_EXISTS

    SELECT COUNT( * ) FROM dm40l
      WHERE dmoid = mv_data_model AND as4local = mv_activation_state.

    rv_bool = boolc( sy-subrc = 0 ).



  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    rv_is_locked = exists_a_lock_entry_for(
      iv_lock_object = 'ESDUM'
      iv_argument    = |{ ms_item-obj_type }{ ms_item-obj_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.

    " The function module listed below do not open a new window - so we revert to BDC.
    "    CALL FUNCTION 'SDU_MODEL_SHOW'
    "    CALL FUNCTION 'RS_TOOL_ACCESS'

    DATA lt_bdcdata TYPE TABLE OF bdcdata.

    FIELD-SYMBOLS: <ls_bdcdata> LIKE LINE OF lt_bdcdata.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-program  = 'SAPMUD00'.
    <ls_bdcdata>-dynpro   = '0100'.
    <ls_bdcdata>-dynbegin = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'BDC_OKCODE'.
    <ls_bdcdata>-fval = '=SHOW'.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-DATM'.
    <ls_bdcdata>-fval = abap_true.

    APPEND INITIAL LINE TO lt_bdcdata ASSIGNING <ls_bdcdata>.
    <ls_bdcdata>-fnam = 'RSUD3-OBJ_KEY'.
    <ls_bdcdata>-fval = ms_item-obj_name.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_batch_input(
      iv_tcode   = 'SD11'
      it_bdcdata = lt_bdcdata ).

    rv_exit = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    IF Lif_abapgit_object~exists( ) = abap_false.
      RETURN.
    ENDIF.

    serialize_model( io_xml ).
    serialize_entities( io_xml ).
    serialize_short_texts( io_xml ).
    serialize_long_texts( io_xml ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_UDMO implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDCA <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdca=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdca=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDCA implementation.
*"* method's implementations
*include methods.
  METHOD check.

    FIELD-SYMBOLS: <ls_message> TYPE LINE OF cts_messages.

    LOOP AT it_messages ASSIGNING <ls_message> WHERE severity = 'E'.
      Lcx_abapgit_exception=>raise( <ls_message>-text ).
    ENDLOOP.

  ENDMETHOD.
  METHOD delete.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_outline   TYPE wdy_cfg_outline_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO ls_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = ls_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        lo_cfg->set_transport( trkorr   = iv_transport
                               devclass = iv_package ).

        lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_delete.
        " First call, check, second call, delete
        DO 2 TIMES.
          lo_cfg->do_next_step(
            IMPORTING
              e_messages  = lt_messages
            CHANGING
              c_operation = lv_operation ).
          check( lt_messages ).
        ENDDO.

      CATCH cx_wd_configuration INTO lx_err.
        IF lx_err->textid = cx_wd_configuration=>conf_config_not_exist.
          RETURN.
        ELSE.
          Lcx_abapgit_exception=>raise( 'WDCA, delete error:' && lx_err->get_text( ) ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
  METHOD read.

    DATA:
      lo_cfg    TYPE REF TO cl_wdr_cfg_persistence_appl,
      ls_key    TYPE wdy_config_key,
      lv_exists TYPE abap_bool,
      lx_err    TYPE REF TO cx_wd_configuration,
      lv_name   TYPE wdy_md_object_name.

    FIELD-SYMBOLS:
      <ls_data>        LIKE LINE OF et_data,
      <ls_appl_params> LIKE LINE OF <ls_data>-appl_params.

    CLEAR: es_outline, et_data.

    ls_key = ms_item-obj_name.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        MOVE-CORRESPONDING ls_key TO es_outline.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = es_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

        IF lv_exists = abap_false.
          RETURN.
        ENDIF.

        es_outline = lo_cfg->read_outline_data( ).

        CLEAR: es_outline-devclass,
               es_outline-author,
               es_outline-createdon,
               es_outline-changedby,
               es_outline-changedon.

        et_data = lo_cfg->read_data( ).

        " Clear descriptions since they are release and language-specific
        LOOP AT et_data ASSIGNING <ls_data>.
          LOOP AT <ls_data>-appl_params ASSIGNING <ls_appl_params>.
            CLEAR <ls_appl_params>-description.
          ENDLOOP.
        ENDLOOP.

      CATCH cx_wd_configuration INTO lx_err.
        Lcx_abapgit_exception=>raise( 'WDCA, read error:' && lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD save.

    DATA:
      lo_cfg       TYPE REF TO cl_wdr_cfg_persistence_appl,
      lx_err       TYPE REF TO cx_wd_configuration,
      lt_messages  TYPE cts_messages,
      ls_key       TYPE wdy_config_key,
      ls_data      LIKE LINE OF it_data,
      lv_operation TYPE i,
      lv_name      TYPE wdy_md_object_name,
      lv_exists    TYPE wdy_boolean.

    MOVE-CORRESPONDING is_outline TO ls_key.

    TRY.
        CREATE OBJECT lo_cfg
          EXPORTING
            config_key  = ls_key
            object_name = lv_name.

        READ TABLE it_data INDEX 1 INTO ls_data.
        ASSERT sy-subrc = 0.

        lo_cfg->check_config_existent(
          EXPORTING
            i_outline_data       = is_outline
            i_only_current_layer = abap_false
            i_is_original        = abap_true
          IMPORTING
            e_is_existent        = lv_exists ).

      CATCH cx_wd_configuration ##NO_HANDLER.
        " Ignore
    ENDTRY.

    TRY.
        lo_cfg->set_transport( trkorr   = iv_transport
                               devclass = iv_package ).
        lo_cfg->set_save_data( ls_data ).
        lo_cfg->set_config_description( is_outline ).

        IF lv_exists = abap_false.
          lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_create.
        ELSE.
          lv_operation = if_wdr_cfg_constants=>c_cts_operation-e_save.
        ENDIF.

        " First call, check, second call, create/save
        DO 2 TIMES.
          lo_cfg->do_next_step(
            IMPORTING
              e_messages  = lt_messages
            CHANGING
              c_operation = lv_operation ).
          check( lt_messages ).
        ENDDO.

      CATCH cx_wd_configuration INTO lx_err.
        Lcx_abapgit_exception=>raise( 'WDCA, save error:' && lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_object~changed_by.

    DATA ls_key TYPE wdy_config_key.

    ls_key = ms_item-obj_name.

    SELECT SINGLE changedby FROM wdy_config_appl INTO rv_user
      WHERE config_id = ls_key-config_id AND config_type = ls_key-config_type AND config_var = ls_key-config_var.
    IF sy-subrc <> 0.
      rv_user = c_user_unknown.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.

    delete( iv_package   = iv_package
            iv_transport = iv_transport ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: ls_outline     TYPE wdy_cfg_outline_data,
          lt_data        TYPE wdy_cfg_persist_data_appl_tab,
          lt_config_appt TYPE TABLE OF wdy_config_appt,
          lv_xml_string  TYPE string,
          lv_xml_xstring TYPE xstring.

    io_xml->read( EXPORTING iv_name = 'OUTLINE'
                  CHANGING  cg_data = ls_outline ).
    io_xml->read( EXPORTING iv_name = 'DATA'
                  CHANGING  cg_data = lt_data ).

    save( is_outline   = ls_outline
          it_data      = lt_data
          iv_package   = iv_package
          iv_transport = iv_transport ).

    TRY.
        lv_xml_string = Lif_abapgit_object~mo_files->read_string(
          iv_extra = 'appl_config'
          iv_ext   = 'xml' ).

        TRY.
            lv_xml_string = Lcl_abapgit_xml_pretty=>print( iv_xml           = lv_xml_string
                                                           iv_ignore_errors = abap_false
                                                           iv_unpretty      = abap_true ).
          CATCH Lcx_abapgit_exception.
            Lcx_abapgit_exception=>raise( 'Error Un-Pretty Printing WDCA XML Content: ' && ms_item-obj_name ).
        ENDTRY.

        REPLACE FIRST OCCURRENCE
          OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
          IN lv_xml_string
          WITH '<?xml version="1.0"?>'.
        ASSERT sy-subrc = 0.

        lv_xml_xstring = Lcl_abapgit_convert=>string_to_xstring( lv_xml_string ).
        UPDATE wdy_config_appl
          SET xcontent = lv_xml_xstring
          WHERE config_id   = ls_outline-config_id
            AND config_type = ls_outline-config_type
            AND config_var  = ls_outline-config_var.
      CATCH Lcx_abapgit_exception.
        " File not found
    ENDTRY.


    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = lt_config_appt ).

    IF lt_config_appt IS NOT INITIAL.
      DELETE FROM wdy_config_appt
        WHERE config_id   = ls_outline-config_id
          AND config_type = ls_outline-config_type
          AND config_var  = ls_outline-config_var.
      MODIFY wdy_config_appt FROM TABLE lt_config_appt.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_APPT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.
    DATA: ls_wdy_config_appl TYPE wdy_config_appl.
    DATA: ls_wdy_config_key TYPE wdy_config_key.

    ls_wdy_config_key = ms_item-obj_name.
    SELECT SINGLE * FROM wdy_config_appl
      INTO ls_wdy_config_appl
      WHERE config_id = ls_wdy_config_key-config_id
        AND config_type = ls_wdy_config_key-config_type
        AND config_var = ls_wdy_config_key-config_var.  "#EC CI_GENBUFF
    rv_bool = boolc( sy-subrc = 0 ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = is_active( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.
    rv_is_locked = abap_false.
  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: ls_outline     TYPE wdy_cfg_outline_data,
          lt_data        TYPE wdy_cfg_persist_data_appl_tab,
          lt_cc_text     TYPE TABLE OF wdy_config_appt,
          lv_xml_xstring TYPE xstring,
          lv_xml_string  TYPE string.

    read( IMPORTING es_outline = ls_outline
                    et_data    = lt_data ).

    IF ls_outline IS INITIAL.
      RETURN.
    ENDIF.

    io_xml->add( iv_name = 'OUTLINE'
                 ig_data = ls_outline ).
    io_xml->add( iv_name = 'DATA'
                 ig_data = lt_data ).


    SELECT SINGLE xcontent
      INTO lv_xml_xstring
      FROM wdy_config_appl
      WHERE config_id = ls_outline-config_id
        AND config_type = ls_outline-config_type
        AND config_var = ls_outline-config_var.
    lv_xml_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xml_xstring ).
    IF lv_xml_string IS NOT INITIAL.
      TRY.
          lv_xml_string = Lcl_abapgit_xml_pretty=>print(
            iv_xml           = lv_xml_string
            iv_ignore_errors = abap_false ).
        CATCH Lcx_abapgit_exception.
          Lcx_abapgit_exception=>raise( 'Error Pretty Printing WDCA XML Content: ' && ms_item-obj_name ).
      ENDTRY.

      REPLACE FIRST OCCURRENCE
        OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
        IN lv_xml_string
        WITH '<?xml version="1.0" encoding="utf-8"?>'.
      ASSERT sy-subrc = 0.
    ENDIF.

    Lif_abapgit_object~mo_files->add_string(
      iv_extra  = 'appl_config'
      iv_ext    = 'xml'
      iv_string = lv_xml_string ).

    SELECT * FROM wdy_config_appt INTO TABLE lt_cc_text
      WHERE config_id   = ls_outline-config_id
      AND config_type = ls_outline-config_type
      AND config_var  = ls_outline-config_var
      ORDER BY PRIMARY KEY.
    IF lt_cc_text IS NOT INITIAL.
      io_xml->add( iv_name = 'DESCR_LANG'
                   ig_data = lt_cc_text ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WDCA implementation

*>>>>>>> ZCL_ABAPGIT_OBJECT_WDCC <<<<<<<*

*"* macro definitions
*include zcl_abapgit_object_wdcc=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_object_wdcc=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OBJECT_WDCC implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_object~changed_by.

    DATA: ls_outline    TYPE wdy_cfg_outline_data,
          ls_config_key TYPE wdy_config_key.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = ms_item-obj_name+32(2).
    ls_config_key-config_var = ms_item-obj_name+34(6).

    TRY.
        cl_wdr_cfg_persistence_utils=>read_comp_config_from_db(
          EXPORTING
            config_key   = ls_config_key
          IMPORTING
            outline_data = ls_outline ).
      CATCH cx_static_check.
        Lcx_abapgit_exception=>raise( 'Error Reading Component Config from DB: ' && ms_item-obj_name ).
    ENDTRY.

    rv_user = ls_outline-changedby.

  ENDMETHOD.
  METHOD Lif_abapgit_object~delete.
    DATA: ls_config_key TYPE wdy_config_key,
          lv_subrc      TYPE sysubrc.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = ms_item-obj_name+32(2).
    ls_config_key-config_var = ms_item-obj_name+34(6).

    TRY.
        " does not exist in 702
        CALL METHOD cl_wdr_cfg_persistence_utils=>('DELETE_CONFIGURATION')
          EXPORTING
            config_key = ls_config_key
          RECEIVING
            subrc      = lv_subrc.
        IF lv_subrc <> 0.
          Lcx_abapgit_exception=>raise( 'Error deleting WDCC: ' && ms_item-obj_name ).
        ENDIF.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type WDCC not supported for this release' ).
    ENDTRY.

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~deserialize.

    DATA: lv_config_id   TYPE c LENGTH 32,
          lv_config_type TYPE n LENGTH 2,
          lv_config_var  TYPE c LENGTH 6,
          lt_otr_texts   TYPE TABLE OF wdy_config_compt,
          ls_orig_config TYPE wdy_config_data,
          lt_config_datt TYPE TABLE OF wdy_config_datt,
          lv_xml_string  TYPE string,
          lv_xml_xstring TYPE xstring.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    io_xml->read( EXPORTING iv_name = 'CONFIG_ID'
                  CHANGING  cg_data = ls_orig_config-config_id ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_TYPE'
                  CHANGING  cg_data = ls_orig_config-config_type ).

    io_xml->read( EXPORTING iv_name = 'CONFIG_VAR'
                  CHANGING  cg_data = ls_orig_config-config_var ).

    lv_config_id = ls_orig_config-config_id.
    lv_config_type = ls_orig_config-config_type.
    lv_config_var = ls_orig_config-config_var.

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_IDPAR'
                     CHANGING cg_data = <lv_data> ).
    ELSE.
      ii_log->add_error( iv_msg  = |Object type WDCC not supported for this release|
                         is_item = ms_item ).
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_TYPEPAR'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'CONFIG_VARPAR'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'WDA_COMPONENT'
                  CHANGING  cg_data = ls_orig_config-component ).

    lv_xml_string = Lif_abapgit_object~mo_files->read_string(
      iv_extra = 'comp_config'
      iv_ext   = 'xml' ).

    TRY.
        lv_xml_string = Lcl_abapgit_xml_pretty=>print( iv_xml           = lv_xml_string
                                                       iv_ignore_errors = abap_false
                                                       iv_unpretty      = abap_true ).
      CATCH Lcx_abapgit_exception.
        Lcx_abapgit_exception=>raise( 'Error Un-Pretty Printing WDCC XML Content: ' && ms_item-obj_name ).
    ENDTRY.

    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml_string
      WITH '<?xml version="1.0"?>'.
    ASSERT sy-subrc = 0.

    lv_xml_xstring = Lcl_abapgit_convert=>string_to_xstring( lv_xml_string ).
    ls_orig_config-xcontent = lv_xml_xstring.

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->read( EXPORTING iv_name = 'PARENT'
                     CHANGING cg_data = <lv_data> ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'RELID'
                  CHANGING  cg_data = ls_orig_config-relid ).

    SELECT SINGLE author createdon FROM wdy_config_data INTO (ls_orig_config-author, ls_orig_config-createdon)
      WHERE config_id = lv_config_id AND
    config_type = lv_config_type AND
    config_var = lv_config_var.

    IF ls_orig_config-author IS INITIAL.
      ls_orig_config-author = sy-uname.
    ENDIF.
    ls_orig_config-changedby = sy-uname.
    ls_orig_config-changedon = sy-datum.

    IF ls_orig_config-createdon IS INITIAL.
      ls_orig_config-createdon = sy-datum.
    ENDIF.

    CALL FUNCTION 'ENQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'E' "if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'
      EXCEPTIONS
        foreign_lock         = 1
        system_failure       = 2
        OTHERS               = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error Enqueueing Component Config: ' && ms_item-obj_name ).
    ENDIF.

    " CL_WDR_CFG_PERSISTENCE_UTILS=>SAVE_COMP_CONFIG_TO_DB does not exist in 702 so we save directly to DB
    DELETE FROM wdy_config_data
      WHERE config_id   = ls_orig_config-config_id
        AND config_type = ls_orig_config-config_type
        AND config_var  = ls_orig_config-config_var.
    MODIFY wdy_config_data FROM ls_orig_config.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATA for Component Config ' && ms_item-obj_name ).
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'OTR_TEXT'
                  CHANGING  cg_data = lt_otr_texts ).

    IF lt_otr_texts IS NOT INITIAL.
      DELETE FROM wdy_config_compt
        WHERE config_id   = ls_orig_config-config_id
          AND config_type = ls_orig_config-config_type
          AND config_var  = ls_orig_config-config_var.
      MODIFY wdy_config_compt FROM TABLE lt_otr_texts.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_COMPT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    io_xml->read( EXPORTING iv_name = 'DESCR_LANG'
                  CHANGING  cg_data = lt_config_datt ).

    IF lt_config_datt IS NOT INITIAL.
      DELETE FROM wdy_config_datt
        WHERE config_id   = ls_orig_config-config_id
          AND config_type = ls_orig_config-config_type
          AND config_var  = ls_orig_config-config_var.
      MODIFY wdy_config_datt FROM TABLE lt_config_datt.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( 'Error Updating WDY_CONFIG_DATT for Component Config ' && ms_item-obj_name ).
      ENDIF.
    ENDIF.

    CALL FUNCTION 'DEQUEUE_E_WDY_CONFCOMP'
      EXPORTING
        mode_wdy_config_data = 'E' "if_wdr_cfg_constants=>c_lock_mode_exclusive
        config_id            = lv_config_id
        config_type          = lv_config_type
        config_var           = lv_config_var
        x_config_id          = 'X'
        x_config_type        = 'X'
        x_config_var         = 'X'.

    tadir_insert( iv_package ).

    corr_insert( iv_package ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~exists.

    DATA: ls_outline    TYPE wdy_cfg_outline_data,
          ls_config_key TYPE wdy_config_key.

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = ms_item-obj_name+32(2).
    ls_config_key-config_var = ms_item-obj_name+34(6).

    TRY.
        cl_wdr_cfg_persistence_utils=>read_comp_config_from_db(
          EXPORTING
            config_key   = ls_config_key
          IMPORTING
            outline_data = ls_outline ).
      CATCH cx_static_check.
        rv_bool = abap_false.
        RETURN.
    ENDTRY.

    rv_bool = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_comparator.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_steps.
    APPEND Lif_abapgit_object=>gc_step_id-abap TO rt_steps.
  ENDMETHOD.
  METHOD Lif_abapgit_object~get_metadata.
    rs_metadata = get_metadata( ).
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_active.
    rv_active = abap_true.
  ENDMETHOD.
  METHOD Lif_abapgit_object~is_locked.

    DATA: lt_enq   TYPE STANDARD TABLE OF seqg3,
          lv_subrc TYPE sysubrc,
          lv_garg  TYPE eqegraarg.

    lv_garg = ms_item-obj_name.

    CALL FUNCTION 'ENQUEUE_READ'
      EXPORTING
        gclient               = sy-mandt
        gname                 = 'WDY_CONFIG_DATA'
        garg                  = lv_garg
      IMPORTING
        subrc                 = lv_subrc
      TABLES
        enq                   = lt_enq
      EXCEPTIONS
        communication_failure = 2
        OTHERS                = 1.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error check object lock WDCC: ' && ms_item-obj_name ).
    ENDIF.

    rv_is_locked = boolc( lines( lt_enq ) > 0 ).

  ENDMETHOD.
  METHOD Lif_abapgit_object~jump.
    " Covered by ZCL_ABAPGIT_OBJECTS=>JUMP
  ENDMETHOD.
  METHOD Lif_abapgit_object~serialize.

    DATA: lv_xml_xstring TYPE xstring,
          lt_otr_texts   TYPE TABLE OF wdy_config_compt,
          lt_cc_text     TYPE TABLE OF wdy_config_datt,
          ls_orig_config TYPE wdy_config_data,
          ls_outline     TYPE wdy_cfg_outline_data,
          ls_config_key  TYPE wdy_config_key,
          lv_xml_string  TYPE string.

    FIELD-SYMBOLS: <lv_data> TYPE any.

    io_xml->add( iv_name = 'OBJECT_NAME'
                 ig_data = ms_item-obj_name ).

    ls_config_key-config_id = ms_item-obj_name+0(32).
    ls_config_key-config_type = ms_item-obj_name+32(2).
    ls_config_key-config_var = ms_item-obj_name+34(6).

    TRY.
        " original_config_data does not exist in 702
        CALL METHOD cl_wdr_cfg_persistence_utils=>('READ_COMP_CONFIG_FROM_DB')
          EXPORTING
            config_key           = ls_config_key
          IMPORTING
            xml_xcontent         = lv_xml_xstring
            original_config_data = ls_orig_config
            outline_data         = ls_outline.

      CATCH cx_static_check.
        Lcx_abapgit_exception=>raise( 'Error Reading Component Config from DB: ' && ms_item-obj_name ).
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( 'Object type WDCC not supported for this release' ).
    ENDTRY.

    io_xml->add( iv_name = 'CONFIG_ID'
                 ig_data = ls_orig_config-config_id ).

    io_xml->add( iv_name = 'CONFIG_TYPE'
                 ig_data = ls_orig_config-config_type ).

    io_xml->add( iv_name = 'CONFIG_VAR'
                 ig_data = ls_orig_config-config_var ).

    io_xml->add( iv_name = 'WDA_COMPONENT'
                 ig_data = ls_orig_config-component ).

    ASSIGN COMPONENT 'CONFIG_IDPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_IDPAR'
                   ig_data = <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_TYPEPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_TYPEPAR'
                   ig_data = <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'CONFIG_VARPAR' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'CONFIG_VARPAR'
                   ig_data = <lv_data> ).
    ENDIF.

    ASSIGN COMPONENT 'PARENT' OF STRUCTURE ls_orig_config TO <lv_data>.
    IF sy-subrc = 0.
      io_xml->add( iv_name = 'PARENT'
                   ig_data = <lv_data> ).
    ENDIF.

    io_xml->add( iv_name = 'RELID'
                 ig_data = ls_orig_config-relid ).

    lv_xml_string = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xml_xstring ).
    IF lv_xml_string IS NOT INITIAL.
      TRY.
          lv_xml_string = Lcl_abapgit_xml_pretty=>print(
            iv_xml           = lv_xml_string
            iv_ignore_errors = abap_false ).
        CATCH Lcx_abapgit_exception.
          Lcx_abapgit_exception=>raise( 'Error Pretty Printing WDCC XML Content: ' && ms_item-obj_name ).
      ENDTRY.

      REPLACE FIRST OCCURRENCE
        OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
        IN lv_xml_string
        WITH '<?xml version="1.0" encoding="utf-8"?>'.
      ASSERT sy-subrc = 0.
    ENDIF.

    Lif_abapgit_object~mo_files->add_string(
      iv_extra  = 'comp_config'
      iv_ext    = 'xml'
      iv_string = lv_xml_string ).

    SELECT * FROM wdy_config_compt INTO TABLE lt_otr_texts
      WHERE config_id   = ls_orig_config-config_id
      AND config_type = ls_orig_config-config_type
      AND config_var  = ls_orig_config-config_var
      ORDER BY PRIMARY KEY.
    IF lt_otr_texts IS NOT INITIAL.
      io_xml->add( iv_name = 'OTR_TEXT'
                   ig_data = lt_otr_texts ).
    ENDIF.

    SELECT * FROM wdy_config_datt INTO TABLE lt_cc_text
      WHERE config_id   = ls_orig_config-config_id
      AND config_type = ls_orig_config-config_type
      AND config_var  = ls_orig_config-config_var
      ORDER BY PRIMARY KEY.
    IF lt_cc_text IS NOT INITIAL.
      io_xml->add( iv_name = 'DESCR_LANG'
                   ig_data = lt_cc_text ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_object~get_deserialize_order.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_filename_to_object.
    RETURN.
  ENDMETHOD.
  METHOD Lif_abapgit_object~map_object_to_filename.
    RETURN.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OBJECT_WDCC implementation

*>>>>>>> ZCL_ABAPGIT_OO_BASE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_base===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_base===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OO_BASE implementation.
*"* method's implementations
*include methods.
  METHOD convert_attrib_to_vseoattrib.
    FIELD-SYMBOLS: <ls_attribute>  LIKE LINE OF it_attributes,
                   <ls_vseoattrib> LIKE LINE OF rt_vseoattrib.

    LOOP AT it_attributes ASSIGNING <ls_attribute>.
      INSERT INITIAL LINE INTO TABLE rt_vseoattrib ASSIGNING <ls_vseoattrib>.
      MOVE-CORRESPONDING <ls_attribute> TO <ls_vseoattrib>.
      <ls_vseoattrib>-clsname = iv_clsname.
      <ls_vseoattrib>-state = seoc_state_implemented.
      <ls_vseoattrib>-exposure = <ls_attribute>-exposure.
      UNASSIGN <ls_vseoattrib>.
    ENDLOOP.
    UNASSIGN <ls_attribute>.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~add_to_activation_list.
    Lcl_abapgit_objects_activation=>add_item( is_item ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create_documentation.
    CALL FUNCTION 'DOCU_UPD'
      EXPORTING
        id            = iv_id
        langu         = iv_language
        object        = iv_object_name
        no_masterlang = iv_no_masterlang
        state         = c_docu_state_active
      TABLES
        line          = it_lines
      EXCEPTIONS
        ret_code      = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~delete.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~delete_documentation.
    CALL FUNCTION 'DOCU_DEL'
      EXPORTING
        id       = iv_id
        langu    = iv_language
        object   = iv_object_name
        typ      = 'E'
      EXCEPTIONS
        ret_code = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error from DOCU_DEL' ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~deserialize_source.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~exists.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~generate_locals.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_class_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_includes.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_interface_properties.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_skip_test_classes.
    rv_skip = mv_skip_test_classes.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~insert_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_attributes.
    SELECT cmpname attbusobj attkeyfld exposure
      FROM seocompodf
      INTO CORRESPONDING FIELDS OF TABLE rt_attributes
      WHERE clsname = iv_object_name
        AND ( attbusobj <> space OR attkeyfld <> space )
        AND version = '1'
      ORDER BY PRIMARY KEY.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_descriptions.
    FIELD-SYMBOLS <ls_description> LIKE LINE OF rt_descriptions.

    IF iv_language IS INITIAL.
      " load all languages
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
             WHERE clsname   = iv_object_name
               AND descript <> ''
             ORDER BY PRIMARY KEY.                        "#EC CI_SUBRC
    ELSE.
      " load main language
      SELECT * FROM seocompotx INTO TABLE rt_descriptions
              WHERE clsname   = iv_object_name
                AND langu     = iv_language
                AND descript <> ''
              ORDER BY PRIMARY KEY.                       "#EC CI_SUBRC
    ENDIF.

    LOOP AT rt_descriptions ASSIGNING <ls_description>.
      CLEAR <ls_description>-clsname.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_descriptions_sub.
    FIELD-SYMBOLS <ls_description> LIKE LINE OF rt_descriptions.

    IF iv_language IS INITIAL.
      " load all languages
      SELECT * FROM seosubcotx INTO TABLE rt_descriptions
             WHERE clsname   = iv_object_name
               AND descript <> ''
             ORDER BY PRIMARY KEY.                        "#EC CI_SUBRC
    ELSE.
      " load main language
      SELECT * FROM seosubcotx INTO TABLE rt_descriptions
              WHERE clsname   = iv_object_name
                AND langu     = iv_language
                AND descript <> ''
              ORDER BY PRIMARY KEY.                       "#EC CI_SUBRC
    ENDIF.

    LOOP AT rt_descriptions ASSIGNING <ls_description>.
      CLEAR <ls_description>-clsname.
    ENDLOOP.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_documentation.
    DATA: lv_state  TYPE dokstate,
          lt_lines  TYPE tlinetab.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                     = iv_id
        langu                  = iv_language
        object                 = iv_object_name
        version_active_or_last = space " retrieve active version
      IMPORTING
        dokstate               = lv_state
      TABLES
        line                   = lt_lines
      EXCEPTIONS
        no_docu_on_screen      = 1
        no_docu_self_def       = 2
        no_docu_temp           = 3
        ret_code               = 4
        OTHERS                 = 5.
    IF sy-subrc = 0 AND lv_state = c_docu_state_active.
      rt_lines = lt_lines.
    ELSE.
      CLEAR rt_lines.
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_sotr.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_superclass.
    SELECT SINGLE refclsname FROM vseoextend INTO rv_superclass
      WHERE clsname = iv_classname.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_text_pool.
    ASSERT 0 = 1. "Subclass responsibility
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~serialize_abap.
    DATA lo_oo_serializer TYPE REF TO Lcl_abapgit_oo_serializer.
    CREATE OBJECT lo_oo_serializer.
    CASE iv_type.
      WHEN seop_ext_class_locals_def.
        rt_source = lo_oo_serializer->serialize_locals_def( is_class_key ).
      WHEN seop_ext_class_locals_imp.
        rt_source = lo_oo_serializer->serialize_locals_imp( is_class_key ).
      WHEN seop_ext_class_macros.
        rt_source = lo_oo_serializer->serialize_macros( is_class_key ).
      WHEN seop_ext_class_testclasses.
        rt_source = lo_oo_serializer->serialize_testclasses( is_class_key ).
        mv_skip_test_classes = lo_oo_serializer->are_test_classes_skipped( ).
      WHEN OTHERS.
        rt_source = lo_oo_serializer->serialize_abap_clif_source( is_class_key ).
    ENDCASE.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~update_descriptions.
    DATA lt_descriptions LIKE it_descriptions.
    DATA lt_components   TYPE seo_components.
    DATA ls_description  LIKE LINE OF it_descriptions.
    DATA lv_lang         TYPE tadir-masterlang.

    FIELD-SYMBOLS <ls_description> LIKE LINE OF it_descriptions.
    FIELD-SYMBOLS <ls_component> TYPE vseocompdf.

    lt_descriptions = it_descriptions.
    LOOP AT lt_descriptions ASSIGNING <ls_description>.
      <ls_description>-clsname = is_key-clsname.
    ENDLOOP.

    " make sure to not damage VSEO* views by deleting texts of all components - an empty text must be kept!!
    SELECT * FROM vseocompdf INTO TABLE lt_components
      WHERE clsname = is_key-clsname
        AND version <> seoc_version_deleted
        AND state = seoc_state_implemented
        AND alias = seox_false ORDER BY clsname cmpname version.

    IF lt_components IS NOT INITIAL.
      SELECT SINGLE masterlang FROM tadir INTO lv_lang
        WHERE pgmid = 'R3TR' AND ( object = 'CLAS' OR object = 'INTF' )
          AND obj_name = is_key-clsname.                  "#EC CI_GENBUFF
      IF sy-subrc <> 0.
        lv_lang = sy-langu.
      ENDIF.

      LOOP AT lt_components ASSIGNING <ls_component>.
        READ TABLE lt_descriptions TRANSPORTING NO FIELDS WITH KEY
          clsname = is_key-clsname
          cmpname = <ls_component>-cmpname.
        IF sy-subrc <> 0.
          ls_description-clsname = is_key-clsname.
          ls_description-cmpname = <ls_component>-cmpname.
          ls_description-langu  = lv_lang.
          ls_description-descript = space.
          APPEND ls_description TO lt_descriptions.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DELETE FROM seocompotx WHERE clsname = is_key-clsname."#EC CI_SUBRC
    INSERT seocompotx FROM TABLE lt_descriptions.         "#EC CI_SUBRC
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~update_descriptions_sub.
    DATA lt_descriptions  LIKE it_descriptions.
    DATA lt_subcomponents TYPE seo_subcomponents.
    DATA ls_description   LIKE LINE OF it_descriptions.
    DATA lv_lang          TYPE tadir-masterlang.

    FIELD-SYMBOLS <ls_description> LIKE LINE OF it_descriptions.
    FIELD-SYMBOLS <ls_subcomponent> TYPE vseosubcdf.

    lt_descriptions = it_descriptions.
    LOOP AT lt_descriptions ASSIGNING <ls_description>.
      <ls_description>-clsname = is_key-clsname.
    ENDLOOP.

    " make sure to not damage VSEO* views by deleting texts of all subcomponents - an empty text must be kept!!
    SELECT * FROM vseosubcdf INTO TABLE lt_subcomponents
      WHERE clsname = is_key-clsname
        AND version <> seoc_version_deleted ORDER BY clsname cmpname sconame version.

    IF lt_subcomponents IS NOT INITIAL.
      SELECT SINGLE masterlang FROM tadir INTO lv_lang
        WHERE pgmid = 'R3TR' AND ( object = 'CLAS' OR object = 'INTF' )
          AND obj_name = is_key-clsname.                   "#EC CI_GENBUFF
      IF sy-subrc <> 0.
        lv_lang = sy-langu.
      ENDIF.

      LOOP AT lt_subcomponents ASSIGNING <ls_subcomponent>.
        READ TABLE lt_descriptions TRANSPORTING NO FIELDS WITH KEY
          clsname = is_key-clsname
          cmpname = <ls_subcomponent>-cmpname
          sconame = <ls_subcomponent>-sconame.
        IF sy-subrc <> 0.
          ls_description-clsname = is_key-clsname.
          ls_description-cmpname = <ls_subcomponent>-cmpname.
          ls_description-sconame = <ls_subcomponent>-sconame.
          ls_description-langu  = lv_lang.
          ls_description-descript = space.
          APPEND ls_description TO lt_descriptions.
        ENDIF.
      ENDLOOP.
    ENDIF.

    DELETE FROM seosubcotx WHERE clsname = is_key-clsname."#EC CI_SUBRC
    INSERT seosubcotx FROM TABLE lt_descriptions.         "#EC CI_SUBRC
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_BASE implementation

*>>>>>>> ZCL_ABAPGIT_OO_CLASS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_class==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_class==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OO_CLASS implementation.
*"* method's implementations
*include methods.
  METHOD create_report.
    Lcl_abapgit_factory=>get_sap_report( )->insert_report(
      iv_name           = iv_program
      iv_package        = iv_package
      it_source         = it_source
      iv_state          = iv_state
      iv_version        = iv_version
      iv_program_type   = iv_program_type
      iv_extension_type = iv_extension ).
  ENDMETHOD.
  METHOD delete_report.
    Lcl_abapgit_factory=>get_sap_report( )->delete_report( iv_program ).
  ENDMETHOD.
  METHOD determine_method_include.

    DATA: ls_mtdkey TYPE seocpdkey.


    ls_mtdkey-clsname = iv_name.
    ls_mtdkey-cpdname = iv_method.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = rv_program
      EXCEPTIONS
        method_not_existing = 1 ).
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'SEO_METHOD_GENERATE_INCLUDE'
      EXPORTING
        suppress_mtdkey_check          = abap_true
        mtdkey                         = ls_mtdkey
      EXCEPTIONS
        not_existing                   = 1
        model_only                     = 2
        include_existing               = 3
        method_imp_not_generated       = 4
        method_imp_not_initialised     = 5
        _internal_class_not_existing   = 6
        _internal_method_overflow      = 7
        cancelled                      = 8
        method_is_abstract_implemented = 9
        method_is_final_implemented    = 10
        internal_error_insert_report   = 11
        OTHERS                         = 12.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    rv_program = cl_oo_classname_service=>get_method_include( ls_mtdkey ).

  ENDMETHOD.
  METHOD generate_classpool.

    DATA: ls_clskey TYPE seoclskey.

    ls_clskey-clsname = iv_name.

    CALL FUNCTION 'SEO_CLASS_GENERATE_CLASSPOOL'
      EXPORTING
        clskey                        = ls_clskey
        suppress_corr                 = abap_true
      EXCEPTIONS
        not_existing                  = 1
        model_only                    = 2
        class_pool_not_generated      = 3
        class_stment_not_generated    = 4
        locals_not_generated          = 5
        macros_not_generated          = 6
        public_sec_not_generated      = 7
        protected_sec_not_generated   = 8
        private_sec_not_generated     = 9
        typeref_not_generated         = 10
        class_pool_not_initialised    = 11
        class_stment_not_initialised  = 12
        locals_not_initialised        = 13
        macros_not_initialised        = 14
        public_sec_not_initialised    = 15
        protected_sec_not_initialised = 16
        private_sec_not_initialised   = 17
        typeref_not_initialised       = 18
        _internal_class_overflow      = 19
        OTHERS                        = 20.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD get_method_includes.
    " get method includes for implemented interfaces
    " this will contain also leftover includes for deleted interface methods
    rt_includes = cl_oo_classname_service=>get_all_method_includes( iv_classname ).
  ENDMETHOD.
  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.
    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        Lcx_abapgit_exception=>raise( 'error initializing CLAS scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD repair_classpool.

    CALL FUNCTION 'SEO_CLASS_REPAIR_CLASSPOOL'
      EXPORTING
        clskey       = is_key
      EXCEPTIONS
        not_existing = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error repairing class { is_key-clsname }| ).
    ENDIF.

  ENDMETHOD.
  METHOD repair_redefinitions.

    " Same logic as SE24 > Utilities > Clean-up > Redefinitions (LSEODCCO)

    DATA:
      lt_inheritance     TYPE vseoextend,
      lt_redefinitions   TYPE seor_redefinitions_r,
      ls_cpdkey          TYPE seocpdkey,
      lv_tabix           TYPE sy-tabix,
      lv_exposure        TYPE n LENGTH 1,
      lv_update          TYPE abap_bool,
      lv_local_component TYPE abap_bool.

    FIELD-SYMBOLS <ls_redef> TYPE seoredef.

    CALL FUNCTION 'SEO_CLASS_TYPEINFO_GET'
      EXPORTING
        clskey        = is_key
        version       = seoc_version_active
      IMPORTING
        inheritance   = lt_inheritance
        redefinitions = lt_redefinitions
      EXCEPTIONS
        not_existing  = 1
        is_interface  = 2
        model_only    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " check redefinitions validity
    LOOP AT lt_redefinitions ASSIGNING <ls_redef>.
      lv_tabix = sy-tabix.

      ls_cpdkey-clsname = is_key-clsname.
      ls_cpdkey-cpdname = <ls_redef>-mtdname.

      CALL FUNCTION 'SEO_COMPONENT_BY_INHERITANCE'
        EXPORTING
          cpdkey             = ls_cpdkey
          version            = seoc_version_active
        IMPORTING
          exposure           = lv_exposure
          is_local_component = lv_local_component
        EXCEPTIONS
          not_existing       = 1
          model_only         = 2
          OTHERS             = 3.
      IF sy-subrc <> 0.
        DELETE lt_redefinitions INDEX lv_tabix.
        lv_update = abap_true.
      ELSEIF <ls_redef>-exposure <> lv_exposure.
        <ls_redef>-exposure = lv_exposure.
        lv_update = abap_true.
      ELSEIF lv_local_component = abap_true AND <ls_redef>-attvalue IS INITIAL AND
             <ls_redef>-mtdabstrct IS INITIAL AND <ls_redef>-mtdfinal IS INITIAL.
        DELETE lt_redefinitions INDEX lv_tabix.
        lv_update = abap_true.
      ENDIF.
    ENDLOOP.

    IF lv_update = abap_true.
      CALL FUNCTION 'SEO_INHERITANC_CHANGE_F_DATA'
        EXPORTING
          save            = abap_false
        CHANGING
          inheritance     = lt_inheritance
          redefinitions   = lt_redefinitions
        EXCEPTIONS
          not_existing    = 1
          deleted         = 2
          is_comprising   = 3
          is_implementing = 4
          not_changed     = 5
          db_error        = 6
          OTHERS          = 7.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error repairing redefinitions for { is_key-clsname }| ).
      ENDIF.

      CALL FUNCTION 'SEO_CLIF_SAVE_ALL'
        EXPORTING
          cifkey                   = is_key
        EXCEPTIONS
          not_existing             = 1
          nothing_to_do            = 2
          access_error             = 3
          db_error                 = 4
          error_in_code_generation = 5
          OTHERS                   = 6.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |Error repairing redefinitions for { is_key-clsname }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD update_cs_number_of_methods.

    " Indirect access to keep downward compatibility
    DATA lr_cache_entry TYPE REF TO data.

    FIELD-SYMBOLS: <lg_cache_entry> TYPE any,
                   <lg_field>       TYPE any.


    TRY.
        CREATE DATA lr_cache_entry TYPE ('SEO_CS_CACHE').
      CATCH cx_sy_create_data_error.
* does not exist in some older systems
        RETURN.
    ENDTRY.

    ASSIGN lr_cache_entry->* TO <lg_cache_entry>.
    ASSERT sy-subrc = 0.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_classname.

    ASSIGN COMPONENT 'NO_OF_METHOD_IMPLS' OF STRUCTURE <lg_cache_entry>
           TO <lg_field>.
    ASSERT sy-subrc = 0.
    <lg_field> = iv_number_of_impl_methods.

    MODIFY ('SEO_CS_CACHE') FROM <lg_cache_entry>.

  ENDMETHOD.
  METHOD update_full_class_include.

    CONSTANTS: lc_class_source_extension TYPE c LENGTH 2 VALUE 'CS',
               lc_include_program_type   TYPE c LENGTH 1 VALUE 'I',
               lc_active_version         TYPE r3state VALUE 'A'.


    create_report( iv_program      = cl_oo_classname_service=>get_cs_name( iv_classname )
                   iv_package      = iv_package
                   it_source       = it_source
                   iv_extension    = lc_class_source_extension
                   iv_program_type = lc_include_program_type
                   iv_state        = lc_active_version
                   iv_version      = iv_version ).

    " Assuming that all methods that were scanned are implemented
    update_cs_number_of_methods( iv_classname              = iv_classname
                                 iv_number_of_impl_methods = lines( it_methods ) ).

  ENDMETHOD.
  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_class_section_source,
          lx_error      TYPE REF TO cx_oo_source_save_failure,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
        CREATE OBJECT lo_update TYPE ('CL_OO_CLASS_SECTION_SOURCE')
          EXPORTING
            clskey                        = ls_clskey
            exposure                      = iv_exposure
            state                         = 'A'
            source                        = it_source
            suppress_constrctr_generation = abap_true
          EXCEPTIONS
            class_not_existing            = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_class_section_source
          EXPORTING
            clskey             = ls_clskey
            exposure           = iv_exposure
            state              = 'A'
          EXCEPTIONS
            class_not_existing = 1
            read_source_error  = 2
            OTHERS             = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).
    TRY.
        CALL METHOD lo_update->('SET_AMDP_SUPPORT')
          EXPORTING
            enabled = abap_true.
      CATCH cx_sy_dyn_call_illegal_method ##NO_HANDLER.
* AMDP not supported in this system, ignore error
    ENDTRY.
    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      Lcx_abapgit_exception=>raise( |CLAS, error while scanning source. Subrc = { sy-subrc }| ).
    ENDIF.

* this will update the SEO* database tables
    TRY.
        lo_update->revert_scan_result( ).
      CATCH cx_oo_source_save_failure INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

    IF iv_exposure = seoc_exposure_public.
      generate_classpool( iv_name ).
    ENDIF.

  ENDMETHOD.
  METHOD update_report.
    DATA lv_type TYPE c LENGTH 1.

    lv_type = Lcl_abapgit_oo_base=>c_include_program_type.

    IF iv_program+30 = srext_ext_class_pool.
      lv_type = Lcl_abapgit_oo_base=>c_cp_program_type.
    ENDIF.

    rv_updated = Lcl_abapgit_factory=>get_sap_report( )->update_report(
      iv_name         = iv_program
      iv_package      = iv_package
      iv_version      = iv_version
      it_source       = it_source
      iv_program_type = lv_type ).
  ENDMETHOD.
  METHOD update_source_index.

    CONSTANTS:
      lc_version_active   TYPE r3state VALUE 'A',
      lc_version_inactive TYPE r3state VALUE 'I'.

    "    dynamic invocation, IF_OO_SOURCE_POS_INDEX_HELPER doesn't exist in 702.
    DATA lo_index_helper TYPE REF TO object.

    TRY.
        CREATE OBJECT lo_index_helper TYPE ('CL_OO_SOURCE_POS_INDEX_HELPER').

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~CREATE_INDEX_WITH_SCANNER')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_active
            scanner    = io_scanner.

        CALL METHOD lo_index_helper->('IF_OO_SOURCE_POS_INDEX_HELPER~DELETE_INDEX')
          EXPORTING
            class_name = iv_clsname
            version    = lc_version_inactive.

      CATCH cx_root.
        " it's probably okay to no update the index
        RETURN.
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create.

    DATA:
      lt_vseoattrib TYPE seoo_attributes_r,
      ls_class_key  TYPE seoclskey,
      ls_properties TYPE vseoclass,
      lt_attributes TYPE Lif_abapgit_definitions=>ty_obj_attribute_tt.

    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    " Get existing class properties and attributes and check if the class
    " needs to be created/updated (or is the same)
    IF iv_check = abap_true.
      ls_class_key-clsname = <lv_clsname>.
      ls_properties = Lif_abapgit_oo_object_fnc~get_class_properties( ls_class_key ).
      lt_attributes = Lif_abapgit_oo_object_fnc~read_attributes( <lv_clsname> ).

      IF ls_properties = cg_properties AND lt_attributes = it_attributes.
        RETURN.
      ENDIF.
    ENDIF.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    " Hardcode STATE (#2612)
    ls_properties = cg_properties.
    ls_properties-state = seoc_state_implemented.

    TRY.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            class           = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_CLASS_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
          CHANGING
            class           = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_interface    = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create_sotr.
    Lcl_abapgit_sotr_handler=>create_sotr(
      iv_package = iv_package
      io_xml     = ii_xml ).
    Lcl_abapgit_sots_handler=>create_sots(
      iv_package = iv_package
      io_xml     = ii_xml ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~delete.

    " SEO_CLASS_DELETE_COMPLETE deletes OTR usage, only
    " Use handler to also delete OTR header and texts
    Lcl_abapgit_sotr_handler=>delete_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = is_deletion_key-clsname ).
    Lcl_abapgit_sots_handler=>delete_sots(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = is_deletion_key-clsname ).

    CALL FUNCTION 'SEO_CLASS_DELETE_COMPLETE'
      EXPORTING
        clskey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_interface = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc = 1.
* ignore deletion of objects that does not exist
* this can happen when the SXCI object is deleted before the implementing CLAS
      RETURN.
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_class,
          lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
          lt_incls   TYPE seop_methods_w_include,
          lv_method  LIKE LINE OF lt_methods,
          lt_public  TYPE seop_source_string,
          lt_source  TYPE seop_source_string.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

* public
    lt_public = lo_scanner->get_public_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_pubsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  iv_package = iv_package
                                  iv_version = iv_version
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_public
                     it_source   = lt_public ).
      ENDIF.
    ENDIF.

* protected
    lt_source = lo_scanner->get_protected_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prosec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  iv_package = iv_package
                                  iv_version = iv_version
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_protected
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* private
    lt_source = lo_scanner->get_private_section_source( ).
    IF lt_source IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_prisec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  iv_package = iv_package
                                  iv_version = iv_version
                                  it_source  = lt_source ).
      IF lv_updated = abap_true.
        update_meta( iv_name     = is_key-clsname
                     iv_exposure = seoc_exposure_private
                     it_source   = lt_source ).
      ENDIF.
    ENDIF.

* methods
    lt_methods = lo_scanner->get_method_implementations( ).

    lt_incls = get_method_includes( is_key-clsname ).

    LOOP AT lt_methods INTO lv_method.
      TRY.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
        CATCH cx_oo_clif_component.
          Lcx_abapgit_exception=>raise( 'error from GET_METHOD_IMPL_SOURCE' ).
      ENDTRY.
      lv_program = determine_method_include(
        iv_name   = is_key-clsname
        iv_method = lv_method ).

      update_report(
        iv_program = lv_program
        iv_package = iv_package
        iv_version = iv_version
        it_source  = lt_source ).

      " If method was implemented before, remove from list
      DELETE lt_incls WHERE cpdkey-clsname = is_key-clsname AND cpdkey-cpdname = lv_method.
    ENDLOOP.

* full class include
    update_full_class_include( iv_classname = is_key-clsname
                               iv_package   = iv_package
                               iv_version   = iv_version
                               it_source    = it_source
                               it_methods   = lt_methods ).

    " If there are leftover method includes, then class needs to be repaired
    " which will delete the obsolete includes
    IF lt_incls IS NOT INITIAL.
      repair_classpool( is_key ).
      repair_redefinitions( is_key ).
    ENDIF.

    update_source_index(
      iv_clsname = is_key-clsname
      io_scanner = lo_scanner ).

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc = 0 OR sy-subrc = 4 ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~generate_locals.

    DATA: lv_program TYPE syrepid.

    IF lines( it_local_definitions ) > 0.
      lv_program = cl_oo_classname_service=>get_ccdef_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     iv_package = iv_package
                     iv_version = iv_version
                     it_source  = it_local_definitions ).
    ENDIF.

    IF lines( it_local_implementations ) > 0.
      lv_program = cl_oo_classname_service=>get_ccimp_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     iv_package = iv_package
                     iv_version = iv_version
                     it_source  = it_local_implementations ).
    ENDIF.

    IF lines( it_local_macros ) > 0.
      lv_program = cl_oo_classname_service=>get_ccmac_name( is_key-clsname ).
      update_report( iv_program = lv_program
                     iv_package = iv_package
                     iv_version = iv_version
                     it_source  = it_local_macros ).
    ENDIF.

    lv_program = cl_oo_classname_service=>get_ccau_name( is_key-clsname ).
    IF lines( it_local_test_classes ) > 0.
      update_report( iv_program = lv_program
                     iv_package = iv_package
                     iv_version = iv_version
                     it_source  = it_local_test_classes ).
    ELSE.
      " Drop the include to remove left-over test classes
      delete_report( lv_program ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_class_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_class_key
        version      = seoc_version_active
      IMPORTING
        class        = rs_class_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR:
      " TODO 2023-08-01: Clear rs_class_properties-state (#2612)
      rs_class_properties-uuid,
      rs_class_properties-author,
      rs_class_properties-createdon,
      rs_class_properties-changedby,
      rs_class_properties-changedon,
      rs_class_properties-r3release,
      rs_class_properties-chgdanyby,
      rs_class_properties-chgdanyon,
      rs_class_properties-clsfinal,
      rs_class_properties-clsabstrct,
      rs_class_properties-exposure,
      rs_class_properties-version.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_includes.
* note: includes returned might not exist
* method cl_oo_classname_service=>GET_ALL_CLASS_INCLUDES does not exist in 702

    DATA: lv_class_name TYPE seoclsname,
          lt_methods    TYPE seop_methods_w_include.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.

    lv_class_name = iv_object_name.

    APPEND cl_oo_classname_service=>get_ccdef_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccmac_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccimp_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_cl_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ccau_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_pubsec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prosec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_prisec_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_classpool_name( lv_class_name ) TO rt_includes.
    APPEND cl_oo_classname_service=>get_ct_name( lv_class_name ) TO rt_includes.

* skip the CS include, as it is sometimes generated on the fly instead of
* when the methods are changed

    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = lv_class_name
      RECEIVING
        result             = lt_methods
      EXCEPTIONS
        class_not_existing = 1 ).

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Class { lv_class_name } not existing| ).
    ENDIF.

    LOOP AT lt_methods ASSIGNING <ls_method>.
      APPEND <ls_method>-incname TO rt_includes.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~insert_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).

    INSERT TEXTPOOL lv_cp
      FROM it_text_pool
      LANGUAGE iv_language
      STATE iv_state.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'error from INSERT TEXTPOOL' ).
    ENDIF.

    Lcl_abapgit_objects_activation=>add( iv_type = 'REPT'
                                         iv_name = lv_cp ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_sotr.
    Lcl_abapgit_sotr_handler=>read_sotr(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = iv_object_name
      io_i18n_params = io_i18n_params
      io_xml      = ii_xml ).
    Lcl_abapgit_sots_handler=>read_sots(
      iv_pgmid    = 'LIMU'
      iv_object   = 'CPUB'
      iv_obj_name = iv_object_name
      io_i18n_params = io_i18n_params
      io_xml      = ii_xml ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~read_text_pool.
    DATA: lv_cp TYPE program.

    lv_cp = cl_oo_classname_service=>get_classpool_name( iv_class_name ).
    READ TEXTPOOL lv_cp INTO rt_text_pool LANGUAGE iv_language. "#EC CI_READ_REP
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_CLASS implementation

*>>>>>>> ZCL_ABAPGIT_OO_INTERFACE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_interface======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_interface======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_OO_INTERFACE implementation.
*"* method's implementations
*include methods.
  METHOD init_scanner.

    DATA: lx_exc       TYPE REF TO cx_root,
          lv_message   TYPE string,
          lv_classname TYPE abap_abstypename.

    FIELD-SYMBOLS: <lv_line> TYPE i.

    TRY.
        ro_scanner = cl_oo_source_scanner_interface=>create_interface_scanner(
          clif_name = iv_name
          source    = it_source ).
        ro_scanner->scan( ).
      CATCH cx_clif_scan_error.
        Lcx_abapgit_exception=>raise( 'error initializing INTF scanner' ).
      CATCH cx_root INTO lx_exc.
        lv_classname = cl_abap_classdescr=>get_class_name( lx_exc ).
        IF lv_classname = '\CLASS=CX_OO_CLIF_SCAN_ERROR_DETAIL'.
          ASSIGN lx_exc->('SOURCE_POSITION-LINE') TO <lv_line>.
          ASSERT sy-subrc = 0.
          lv_message = |{ lx_exc->get_text( ) }, line { <lv_line> }|.
        ELSE.
          lv_message = lx_exc->get_text( ).
        ENDIF.
        Lcx_abapgit_exception=>raise( lv_message ).
    ENDTRY.

  ENDMETHOD.
  METHOD update_meta.

    DATA: lo_update     TYPE REF TO cl_oo_interface_section_source,
          lx_error      TYPE REF TO cx_oo_source_save_failure,
          ls_clskey     TYPE seoclskey,
          lv_scan_error TYPE abap_bool.


    ls_clskey-clsname = iv_name.

    TRY.
        CALL FUNCTION 'SEO_BUFFER_REFRESH'
          EXPORTING
            cifkey  = ls_clskey
            version = seoc_version_active.
        CREATE OBJECT lo_update TYPE ('CL_OO_INTERFACE_SECTION_SOURCE')
          EXPORTING
            intkey                        = ls_clskey
            state                         = 'A'
            source                        = it_source
          EXCEPTIONS
            interface_not_existing        = 1
            read_source_error             = 2
            OTHERS                        = 3 ##SUBRC_OK.
      CATCH cx_sy_dyn_call_param_not_found.
* downport to 702, see https://github.com/abapGit/abapGit/issues/933
* this will READ REPORT instead of using it_source, which should be okay
        CREATE OBJECT lo_update TYPE cl_oo_interface_section_source
          EXPORTING
            intkey                 = ls_clskey
            state                  = 'A'
          EXCEPTIONS
            interface_not_existing = 1
            read_source_error      = 2
            OTHERS                 = 3.
    ENDTRY.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lo_update->set_dark_mode( abap_true ).

    lo_update->scan_section_source(
      RECEIVING
        scan_error             = lv_scan_error
      EXCEPTIONS
        scan_abap_source_error = 1
        OTHERS                 = 2 ).
    IF sy-subrc <> 0 OR lv_scan_error = abap_true.
      Lcx_abapgit_exception=>raise( |INTF, error while scanning source. Subrc = { sy-subrc }| ).
    ENDIF.

* this will update the SEO* database tables
    TRY.
        lo_update->revert_scan_result( ).
      CATCH cx_oo_source_save_failure INTO lx_error.
        Lcx_abapgit_exception=>raise_with_text( lx_error ).
    ENDTRY.

  ENDMETHOD.
  METHOD update_report.
    DATA lv_type TYPE c LENGTH 1.

    lv_type = Lcl_abapgit_oo_base=>c_include_program_type.

    IF iv_program+30 = srext_ext_interface_pool.
      lv_type = Lcl_abapgit_oo_base=>c_ip_program_type.
    ENDIF.

    rv_updated = Lcl_abapgit_factory=>get_sap_report( )->update_report(
      iv_name         = iv_program
      iv_package      = iv_package
      iv_version      = iv_version
      it_source       = it_source
      iv_program_type = lv_type ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~create.

    DATA:
      lt_vseoattrib    TYPE seoo_attributes_r,
      ls_interface_key TYPE seoclskey,
      ls_properties    TYPE vseointerf.

    FIELD-SYMBOLS: <lv_clsname> TYPE seoclsname.

    ASSIGN COMPONENT 'CLSNAME' OF STRUCTURE cg_properties TO <lv_clsname>.
    ASSERT sy-subrc = 0.

    " Get existing interface properties and check if the interface
    " needs to be created/updated (or is the same)
    IF iv_check = abap_true.
      ls_interface_key-clsname = <lv_clsname>.
      ls_properties = Lif_abapgit_oo_object_fnc~get_interface_properties( ls_interface_key ).

      IF ls_properties = cg_properties.
        RETURN.
      ENDIF.
    ENDIF.

    lt_vseoattrib = convert_attrib_to_vseoattrib(
                      iv_clsname    = <lv_clsname>
                      it_attributes = it_attributes ).

    " Hardcode STATE (#2612)
    ls_properties = cg_properties.
    ls_properties-state = seoc_state_implemented.

    TRY.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
            suppress_dialog = abap_true " Parameter missing in 702
          CHANGING
            interface       = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
      CATCH cx_sy_dyn_call_param_not_found.
        CALL FUNCTION 'SEO_INTERFACE_CREATE_COMPLETE'
          EXPORTING
            devclass        = iv_package
            overwrite       = abap_true
            version         = seoc_version_active
          CHANGING
            interface       = ls_properties
            attributes      = lt_vseoattrib
          EXCEPTIONS
            existing        = 1
            is_class        = 2
            db_error        = 3
            component_error = 4
            no_access       = 5
            other           = 6
            OTHERS          = 7.
    ENDTRY.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~delete.
    CALL FUNCTION 'SEO_INTERFACE_DELETE_COMPLETE'
      EXPORTING
        intkey       = is_deletion_key
      EXCEPTIONS
        not_existing = 1
        is_class     = 2
        db_error     = 3
        no_access    = 4
        other        = 5
        OTHERS       = 6.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~deserialize_source.

    DATA: lv_updated TYPE abap_bool,
          lv_program TYPE program,
          lo_scanner TYPE REF TO cl_oo_source_scanner_interface,
          lt_public  TYPE seop_source_string.

    "Buffer needs to be refreshed,
    "otherwise standard SAP CLIF_SOURCE reorder methods alphabetically
    CALL FUNCTION 'SEO_BUFFER_INIT'.
    CALL FUNCTION 'SEO_BUFFER_REFRESH'
      EXPORTING
        cifkey  = is_key
        version = seoc_version_inactive.

    lo_scanner = init_scanner(
      it_source = it_source
      iv_name   = is_key-clsname ).

    lt_public = lo_scanner->get_interface_section_source( ).
    IF lt_public IS NOT INITIAL.
      lv_program = cl_oo_classname_service=>get_intfsec_name( is_key-clsname ).
      lv_updated = update_report( iv_program = lv_program
                                  iv_package = iv_package
                                  iv_version = iv_version
                                  it_source  = lt_public ).
      IF lv_updated = abap_true.
        update_meta( iv_name   = is_key-clsname
                     it_source = lt_public ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~exists.
    CALL FUNCTION 'SEO_INTERFACE_EXISTENCE_CHECK'
      EXPORTING
        intkey        = is_object_name
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_class      = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    rv_exists = boolc( sy-subrc = 0 OR sy-subrc = 4 ).
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_includes.
    DATA lv_interface_name TYPE seoclsname.
    lv_interface_name = iv_object_name.
    APPEND cl_oo_classname_service=>get_interfacepool_name( lv_interface_name ) TO rt_includes.
  ENDMETHOD.
  METHOD Lif_abapgit_oo_object_fnc~get_interface_properties.
    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_interface_key
        version      = seoc_version_active
      IMPORTING
        interface    = rs_interface_properties
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc = 1.
      RETURN. " in case only inactive version exists
    ELSEIF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CLEAR:
      " TODO 2023-08-01: Clear rs_interface_properties-state (#2612)
      rs_interface_properties-uuid,
      rs_interface_properties-author,
      rs_interface_properties-createdon,
      rs_interface_properties-changedby,
      rs_interface_properties-changedon,
      rs_interface_properties-chgdanyby,
      rs_interface_properties-chgdanyon,
      rs_interface_properties-r3release,
      rs_interface_properties-version.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_INTERFACE implementation

*>>>>>>> ZCL_ABAPGIT_OO_SERIALIZER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_oo_serializer=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_oo_serializer=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_oo_serializer=====ccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KW5LSFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_oo_serializer DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW5LSFY.




class LCL_ABAPGIT_OO_SERIALIZER implementation.
*"* method's implementations
*include methods.
  METHOD are_test_classes_skipped.
    rv_return = mv_skip_testclass.
  ENDMETHOD.
  METHOD calculate_skip_testclass.

    DATA: lv_line1 LIKE LINE OF it_source,
          lv_line2 LIKE LINE OF it_source.

* when creating classes in Eclipse it automatically generates the
* testclass include, but it is not needed, so skip to avoid
* creating an extra file in the repository.
* Also remove it if the content is manually removed, but
* the class still thinks it contains tests

    rv_skip_testclass = abap_false.
    IF lines( it_source ) = 2.
      READ TABLE it_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      READ TABLE it_source INDEX 2 INTO lv_line2.
      ASSERT sy-subrc = 0.
      IF strlen( lv_line1 ) >= 3 AND lv_line1(3) = '*"*' AND lv_line2 IS INITIAL.
        rv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( it_source ) = 1.
      READ TABLE it_source INDEX 1 INTO lv_line1.
      ASSERT sy-subrc = 0.
      IF lv_line1 IS INITIAL
          OR ( strlen( lv_line1 ) >= 3 AND lv_line1(3) = '*"*' )
          OR ( strlen( lv_line1 ) = 1 AND lv_line1(1) = '*' ).
        rv_skip_testclass = abap_true.
      ENDIF.
    ELSEIF lines( it_source ) = 0.
      rv_skip_testclass = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD read_include.

    DATA ls_include TYPE progstruc.
    DATA lv_program TYPE syrepid.
    DATA lt_source  TYPE abaptxt255_tab.

    ASSERT iv_type = seop_ext_class_locals_def
      OR iv_type = seop_ext_class_locals_imp
      OR iv_type = seop_ext_class_macros
      OR iv_type = seop_ext_class_testclasses.

    ls_include-rootname = is_clskey-clsname.
    TRANSLATE ls_include-rootname USING ' ='.
    ls_include-categorya = iv_type(1).
    ls_include-codea = iv_type+1(4).

* it looks like there is an issue in function module SEO_CLASS_GET_INCLUDE_SOURCE
* on 750 kernels, where the READ REPORT without STATE addition does not
* return the active version, this method is a workaround for this issue
    lv_program = ls_include.
    TRY.
        lt_source = Lcl_abapgit_factory=>get_sap_report( )->read_report( lv_program ).
      CATCH Lcx_abapgit_exception.
* ignore if the report is not found, sometimes the CCDEF include does not exist
    ENDTRY.
    rt_source = lt_source.

  ENDMETHOD.
  METHOD reduce.

    DATA: lv_source LIKE LINE OF ct_source,
          lv_found  TYPE abap_bool.


* skip files that only contain the standard comments
    lv_found = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF strlen( lv_source ) >= 3 AND lv_source(3) <> '*"*'.
        lv_found = abap_true.
      ENDIF.
    ENDLOOP.
    IF lv_found = abap_false.
      CLEAR ct_source.
    ENDIF.

  ENDMETHOD.
  METHOD remove_signatures.

* signatures messes up in CL_OO_SOURCE when deserializing and serializing
* within same session

    DATA: lv_begin  TYPE string,
          lv_end    TYPE string,
          lv_remove TYPE abap_bool,
          lv_source LIKE LINE OF ct_source.

    "@TODO: Put under test
    CONCATENATE '* <SIGNATURE>------------------------------------'
      '---------------------------------------------------+'
      INTO lv_begin.

    CONCATENATE '* +------------------------------------------------'
      '--------------------------------------</SIGNATURE>'
      INTO lv_end.

    lv_remove = abap_false.
    LOOP AT ct_source INTO lv_source.
      IF lv_source = lv_begin.
        lv_remove = abap_true.
      ENDIF.
      IF lv_remove = abap_true.
        DELETE ct_source INDEX sy-tabix.
      ENDIF.
      IF lv_source = lv_end.
        lv_remove = abap_false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD serialize_abap_clif_source.
    rt_source = Lcl_abapgit_exit=>get_instance( )->custom_serialize_abap_clif( is_class_key ).
    IF rt_source IS NOT INITIAL.
      RETURN.
    ENDIF.

    TRY.
        rt_source = serialize_abap_new( is_class_key ).
      CATCH cx_sy_dyn_call_error.
        rt_source = serialize_abap_old( is_class_key ).
    ENDTRY.

    " Call exit again for optional post-processing
    rt_source = Lcl_abapgit_exit=>get_instance( )->custom_serialize_abap_clif(
      is_class_key = is_class_key
      it_source    = rt_source ).
  ENDMETHOD.
  METHOD serialize_abap_new.

    DATA: lo_source   TYPE REF TO object,
          lo_instance TYPE REF TO object.

* do not call the class/methods statically, as it will
* give syntax errors on old versions
    CALL METHOD ('CL_OO_FACTORY')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CALL METHOD lo_instance->('CREATE_CLIF_SOURCE')
      EXPORTING
        clif_name = is_clskey-clsname
        version   = 'A'
      RECEIVING
        result    = lo_source.

    CALL METHOD lo_source->('GET_SOURCE')
      IMPORTING
        source = rt_source.

  ENDMETHOD.
  METHOD serialize_abap_old.
* for old ABAP AS versions
    DATA: lo_source TYPE REF TO object.

    CREATE OBJECT lo_source TYPE ('CL_OO_SOURCE')
      EXPORTING
        clskey             = is_clskey
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    CALL METHOD lo_source->('READ')
      EXPORTING
        version = 'A'.
    CALL METHOD lo_source->('GET_OLD_SOURCE')
      RECEIVING
        old_source = rt_source.
    remove_signatures( CHANGING ct_source = rt_source ).

  ENDMETHOD.
  METHOD serialize_locals_def.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_def ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.
  METHOD serialize_locals_imp.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_locals_imp ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.
  METHOD serialize_macros.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_macros ).

    reduce( CHANGING ct_source = rt_source ).

  ENDMETHOD.
  METHOD serialize_testclasses.

    DATA ls_vseoclass TYPE vseoclass.

    CALL FUNCTION 'SEO_CLIF_GET'
      EXPORTING
        cifkey       = is_clskey
        version      = seoc_version_active
      IMPORTING
        class        = ls_vseoclass
      EXCEPTIONS
        not_existing = 1
        deleted      = 2
        model_only   = 3
        OTHERS       = 4.
    IF sy-subrc <> 0 OR ls_vseoclass-with_unit_tests = abap_false.
      mv_skip_testclass = abap_true.
      RETURN.
    ENDIF.

    rt_source = read_include( is_clskey = is_clskey
                              iv_type = seop_ext_class_testclasses ).

    mv_skip_testclass = calculate_skip_testclass( rt_source ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_OO_SERIALIZER implementation

*>>>>>>> ZCL_ABAPGIT_PERSISTENCE_DB <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persistence_db====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persistence_db====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSISTENCE_DB implementation.
*"* method's implementations
*include methods.
  METHOD add.

    DATA ls_table TYPE Lif_abapgit_persistence=>ty_content.

    validate_entry_type( iv_type ).
    ls_table-type  = iv_type.
    ls_table-value = iv_value.
    ls_table-data_str = iv_data.

    INSERT (c_tabname) FROM ls_table.                     "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

  ENDMETHOD.
  METHOD delete.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    " Ignore errors since record might not exist
    DELETE FROM (c_tabname)
      WHERE type = iv_type
      AND value = iv_value.

  ENDMETHOD.
  METHOD get_instance.

    IF go_db IS NOT BOUND.
      CREATE OBJECT go_db.
    ENDIF.
    ro_db = go_db.

  ENDMETHOD.
  METHOD get_update_function.
    IF mv_update_function IS INITIAL.
      mv_update_function = 'CALL_V1_PING'.
      IF Lcl_abapgit_factory=>get_function_module( )->function_exists( mv_update_function ) = abap_false.
        mv_update_function = 'BANK_OBJ_WORKL_RELEASE_LOCKS'.
      ENDIF.
    ENDIF.
    rv_funcname = mv_update_function.

  ENDMETHOD.
  METHOD list.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content.                              "#EC CI_SUBRC
  ENDMETHOD.
  METHOD list_by_keys.
    FIELD-SYMBOLS: <ls_key> LIKE LINE OF it_keys.
    LOOP AT it_keys ASSIGNING <ls_key>.
      SELECT * FROM (c_tabname)
      APPENDING TABLE rt_contents
      WHERE value = <ls_key> AND
            type  = iv_type.
    ENDLOOP.
  ENDMETHOD.
  METHOD list_by_type.
    SELECT * FROM (c_tabname)
      INTO TABLE rt_content
      WHERE type = iv_type
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
  ENDMETHOD.
  METHOD lock.
    DATA: lv_dummy_update_function TYPE funcname.

    CALL FUNCTION 'ENQUEUE_EZABAPGIT'
      EXPORTING
        mode_zabapgit  = iv_mode
        type           = iv_type
        value          = iv_value
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    lv_dummy_update_function = get_update_function( ).

* trigger dummy update task to automatically release locks at commit
    CALL FUNCTION lv_dummy_update_function
      IN UPDATE TASK.

  ENDMETHOD.
  METHOD modify.

    DATA: ls_content TYPE Lif_abapgit_persistence=>ty_content.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    ls_content-type  = iv_type.
    ls_content-value = iv_value.
    ls_content-data_str = iv_data.

    MODIFY (c_tabname) FROM ls_content.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'DB modify failed' ).
    ENDIF.

  ENDMETHOD.
  METHOD read.

    SELECT SINGLE data_str FROM (c_tabname) INTO rv_data
      WHERE type = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE Lcx_abapgit_not_found.
    ENDIF.

  ENDMETHOD.
  METHOD update.

    DATA lv_data LIKE iv_data.

    IF iv_data CS '<?xml'.
      lv_data = validate_and_unprettify_xml( iv_data ).
    ELSE.
      lv_data = iv_data.
    ENDIF.

    lock( iv_type  = iv_type
          iv_value = iv_value ).

    UPDATE (c_tabname) SET data_str = lv_data
      WHERE type  = iv_type
      AND value = iv_value.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'DB update failed' ).
    ENDIF.

  ENDMETHOD.
  METHOD validate_and_unprettify_xml.

    rv_xml = Lcl_abapgit_xml_pretty=>print(
      iv_xml           = iv_xml
      iv_unpretty      = abap_true
      iv_ignore_errors = abap_false ).

  ENDMETHOD.
  METHOD validate_entry_type.

    IF NOT (
      iv_type = c_type_repo OR
      iv_type = c_type_repo_csum OR
      iv_type = c_type_user OR
      iv_type = c_type_settings OR
      iv_type = c_type_background OR
      iv_type = c_type_packages ).
      Lcx_abapgit_exception=>raise( |Invalid DB entry type [{ iv_type }]| ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSISTENCE_DB implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_factory===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_factory===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_PERSIST_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_repo.

    IF gi_repo IS INITIAL.
      CREATE OBJECT gi_repo TYPE Lcl_abapgit_persistence_repo.
    ENDIF.

    ri_repo = gi_repo.

  ENDMETHOD.
  METHOD get_repo_cs.

    IF gi_repo_cs IS INITIAL.
      CREATE OBJECT gi_repo_cs TYPE Lcl_abapgit_persistence_repo.
    ENDIF.

    ri_repo_cs = gi_repo_cs.

  ENDMETHOD.
  METHOD get_settings.

    IF gi_settings IS INITIAL.
      CREATE OBJECT gi_settings TYPE Lcl_abapgit_persist_settings.
    ENDIF.

    ri_settings = gi_settings.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PERSIST_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_PERSIST_INJECTOR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_persist_injector==ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_persist_injector==ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations



*>>>>>>> ZCL_ABAPGIT_POPUP_BRANCH_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popup_branch_list=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popup_branch_list=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_POPUP_BRANCH_LIST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_repo_url        = iv_url.
    mv_default_branch  = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix && iv_default_branch.
    mv_show_new_option = iv_show_new_option.
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ri_popup TYPE Lcl_abapgit_popup_branch_list
      EXPORTING
        iv_url             = iv_url
        iv_default_branch  = iv_default_branch
        iv_show_new_option = iv_show_new_option.
  ENDMETHOD.
  METHOD fetch_branch_list.

    DATA lo_branches    TYPE REF TO Lcl_abapgit_git_branch_list.
    DATA lv_head_symref TYPE string.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF rt_branches.

    lo_branches    = Lcl_abapgit_git_transport=>branches( mv_repo_url ).
    rt_branches    = lo_branches->get_branches_only( ).
    lv_head_symref = lo_branches->get_head_symref( ).

    IF rt_branches IS INITIAL.
      Lcx_abapgit_exception=>raise( 'No branches are available to select' ).
    ENDIF.

    " Clean up branches: HEAD duplicates, empty names
    LOOP AT rt_branches ASSIGNING <ls_branch>.
      IF <ls_branch>-name IS INITIAL.
        DELETE rt_branches INDEX sy-tabix.
      ELSEIF <ls_branch>-is_head = abap_true AND lv_head_symref IS NOT INITIAL AND <ls_branch>-name <> lv_head_symref.
        DELETE rt_branches INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    SORT rt_branches BY is_head DESCENDING display_name ASCENDING.

    IF mv_show_new_option = abap_true.
      APPEND INITIAL LINE TO rt_branches ASSIGNING <ls_branch>.
      <ls_branch>-name = Lif_abapgit_popups=>c_new_branch_label.
      <ls_branch>-display_name = Lif_abapgit_popups=>c_new_branch_label.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_render_item~render.

    DATA lv_head_marker TYPE string.
    FIELD-SYMBOLS <ls_b> TYPE Lif_abapgit_git_definitions=>ty_git_branch.

    ASSIGN iv_item TO <ls_b>.
    ASSERT sy-subrc = 0.

    " TODO render mv_default_branch properly, needs respecting support from the picklist components

    IF <ls_b>-is_head = abap_true.
      lv_head_marker = | (<b>{ Lif_abapgit_git_definitions=>c_head_name }</b>)|.
    ENDIF.

    ri_html = Lcl_abapgit_html=>create( |{ <ls_b>-display_name }{ lv_head_marker }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Branch'
        it_list          = fetch_branch_list( )
        ii_item_renderer = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUP_BRANCH_LIST implementation

*>>>>>>> ZCL_ABAPGIT_POPUP_TAG_LIST <<<<<<<*

*"* macro definitions
*include zcl_abapgit_popup_tag_list====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_popup_tag_list====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_POPUP_TAG_LIST implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_repo_url = iv_url.
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ri_popup TYPE Lcl_abapgit_popup_tag_list
      EXPORTING
        iv_url = iv_url.
  ENDMETHOD.
  METHOD fetch_tag_list.

    DATA lo_branches  TYPE REF TO Lcl_abapgit_git_branch_list.

    lo_branches = Lcl_abapgit_git_transport=>branches( mv_repo_url ).
    rt_tags     = lo_branches->get_tags_only( ).

    DELETE rt_tags WHERE name CP '*' && Lif_abapgit_git_definitions=>c_git_branch-peel.

    IF lines( rt_tags ) = 0.
      Lcx_abapgit_exception=>raise( 'No tags are available to select' ).
    ENDIF.

    SORT rt_tags BY display_name ASCENDING.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_render_item~render.

    FIELD-SYMBOLS <ls_tag> TYPE Lif_abapgit_git_definitions=>ty_git_branch.

    ASSIGN iv_item TO <ls_tag>.
    ASSERT sy-subrc = 0.

    ri_html = Lcl_abapgit_html=>create( |{ <ls_tag>-display_name }| ).

  ENDMETHOD.
  METHOD Lif_abapgit_html_popup~create_picklist.

    CREATE OBJECT ro_picklist
      EXPORTING
        iv_title         = 'Choose Tag'
        it_list          = fetch_tag_list( )
        ii_item_renderer = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_POPUP_TAG_LIST implementation

*>>>>>>> ZCL_ABAPGIT_PO_FILE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_po_file===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_po_file===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_po_file===========ccau.

*CLASS zcl_abapgit_po_file DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW5ZSFY.


class LCL_ABAPGIT_PO_FILE implementation.
*"* method's implementations
*include methods.
  METHOD build_po_body.

    FIELD-SYMBOLS <ls_pair> LIKE LINE OF mt_pairs.
    FIELD-SYMBOLS <ls_comment> LIKE LINE OF <ls_pair>-comments.

    CREATE OBJECT ro_buf.

    LOOP AT mt_pairs ASSIGNING <ls_pair>.
      IF sy-tabix <> 1.
        ro_buf->add( '' ).
      ENDIF.

      " TODO integrate translator comments ?

      SORT <ls_pair>-comments BY kind.
      LOOP AT <ls_pair>-comments ASSIGNING <ls_comment>.
        ro_buf->add( |#{ get_comment_marker( <ls_comment>-kind ) } { <ls_comment>-text }| ).
      ENDLOOP.

      ro_buf->add( |msgid { quote( <ls_pair>-source ) }| ).
      ro_buf->add( |msgstr { quote( <ls_pair>-target ) }| ).
    ENDLOOP.

  ENDMETHOD.
  METHOD build_po_head.

    CREATE OBJECT ro_buf.

    " TODO, more headers ? sample: https://www.gnu.org/software/trans-coord/manual/gnun/html_node/PO-Header.html
    " TODO, does \n really necessary ? check editors support for non-\n
    " TODO, should be unfuzzy for final version, and potentially should have more fields

    ro_buf->add( '#, fuzzy' ).
    ro_buf->add( 'msgid ""' ).
    ro_buf->add( 'msgstr ""' ).
    ro_buf->add( '"MIME-Version: 1.0\n"' ).
    ro_buf->add( '"Content-Type: text/plain; charset=UTF-8\n"' ).
    ro_buf->add( '"Content-Transfer-Encoding: 8bit\n"' ).
    ro_buf->add( '' ).

  ENDMETHOD.
  METHOD constructor.
    mv_lang = to_lower( iv_lang ).
  ENDMETHOD.
  METHOD get_comment_marker.
    CASE iv_comment_kind.
      WHEN c_comment-translator.
        rv_marker = ''.
      WHEN c_comment-extracted.
        rv_marker = '.'.
      WHEN c_comment-reference.
        rv_marker = ':'.
      WHEN c_comment-flag.
        rv_marker = ','.
      WHEN c_comment-previous.
        rv_marker = '|'.
    ENDCASE.
  ENDMETHOD.
  METHOD parse.

    DATA lv_xdata TYPE xstring.
    DATA lv_data TYPE string.

    IF xstrlen( iv_xdata ) > 3 AND iv_xdata(3) = cl_abap_char_utilities=>byte_order_mark_utf8.
      lv_xdata = iv_xdata+3.
    ELSE.
      lv_xdata = iv_xdata.
    ENDIF.

    lv_data = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xdata ).

    parse_po( lv_data ).

  ENDMETHOD.
  METHOD parse_po.

    CONSTANTS:
      BEGIN OF c_state,
        wait_id  TYPE i VALUE 0,
        wait_str TYPE i VALUE 1,
        wait_eos TYPE i VALUE 2,
        " TODO msgctx
      END OF c_state.

    DATA lv_state TYPE i VALUE c_state-wait_id.
    DATA lt_lines TYPE string_table.
    DATA ls_pair LIKE LINE OF mt_pairs.
    DATA lv_whitespace TYPE c LENGTH 2.
    FIELD-SYMBOLS <lv_i> TYPE string.

    lv_whitespace = ` ` && cl_abap_char_utilities=>horizontal_tab.

    SPLIT iv_data AT cl_abap_char_utilities=>newline INTO TABLE lt_lines.
    APPEND '' TO lt_lines. " terminator

    LOOP AT lt_lines ASSIGNING <lv_i>.
      IF lv_state = c_state-wait_eos.
        IF strlen( <lv_i> ) >= 1 AND <lv_i>+0(1) = '"'.
          ls_pair-target = ls_pair-target && unquote( <lv_i> ).
          CONTINUE.
        ELSE.
          lv_state = c_state-wait_id.
          IF ls_pair-source IS NOT INITIAL. " skip header entry for now
            INSERT ls_pair INTO TABLE mt_pairs. " Sorted, duplicates will not be inserted
          ENDIF.
          CLEAR ls_pair.
        ENDIF.
      ENDIF.

      CASE lv_state.
        WHEN c_state-wait_id.
          IF <lv_i> IS INITIAL
            OR <lv_i>+0(1) = '#' " TODO, potentially parse comments in future, to re-integrate
            OR <lv_i> CO lv_whitespace.
            CONTINUE.
          ENDIF.
          IF strlen( <lv_i> ) >= 6 AND <lv_i>+0(6) = `msgid `. " w/trailing space
            ls_pair-source = unquote( substring(
              val = <lv_i>
              off = 6 ) ).
            lv_state = c_state-wait_str.
          ELSE.
            Lcx_abapgit_exception=>raise( 'PO file format error: expected msgid' ).
          ENDIF.

        WHEN c_state-wait_str.
          IF strlen( <lv_i> ) >= 7 AND <lv_i>+0(7) = `msgstr `. " w/trailing space
            ls_pair-target = unquote( substring(
              val = <lv_i>
              off = 7 ) ).
            lv_state = c_state-wait_eos.
          ELSE.
            Lcx_abapgit_exception=>raise( 'PO file format error: expected msgstr' ).
          ENDIF.

      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD push_text_pairs.

    DATA ls_out LIKE LINE OF mt_pairs.
    FIELD-SYMBOLS <ls_in> LIKE LINE OF it_text_pairs.
    FIELD-SYMBOLS <ls_out> LIKE LINE OF mt_pairs.
    DATA ls_comment LIKE LINE OF <ls_out>-comments.

    LOOP AT it_text_pairs ASSIGNING <ls_in>.
      CHECK <ls_in>-s_text IS NOT INITIAL.

      READ TABLE mt_pairs ASSIGNING <ls_out> WITH KEY source = <ls_in>-s_text.
      IF sy-subrc <> 0.
        ls_out-source = <ls_in>-s_text.
        INSERT ls_out INTO TABLE mt_pairs ASSIGNING <ls_out>.
        ASSERT sy-subrc = 0.
      ENDIF.

      IF <ls_out>-target IS INITIAL. " For a case of orig text duplication
        <ls_out>-target = <ls_in>-t_text.
      ENDIF.

      ls_comment-kind = c_comment-reference.
      ls_comment-text = condense( |{ iv_objtype }/{ iv_objname }/{ <ls_in>-textkey }| )
        && |, maxlen={ <ls_in>-unitmlt }|.
      APPEND ls_comment TO <ls_out>-comments.
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.
  METHOD quote.
    rv_text = '"' && replace(
      val  = iv_text
      sub  = '"'
      with = '\"'
      occ  = 0 ) && '"'.
  ENDMETHOD.
  METHOD unquote.

    DATA lv_len TYPE i.
    DATA lv_prev_char TYPE i.

    rv_text = iv_text.
    SHIFT rv_text RIGHT DELETING TRAILING space. " Measure perf ? Could be slowish, maybe use find
    SHIFT rv_text LEFT DELETING LEADING space.
    lv_len = strlen( rv_text ).

    IF lv_len < 2.
      Lcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    lv_prev_char = lv_len - 1.
    IF rv_text+0(1) <> '"' OR rv_text+lv_prev_char(1) <> '"'.
      Lcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    lv_prev_char = lv_prev_char - 1.
    IF lv_len >= 3 AND rv_text+lv_prev_char(1) = '\'. " escaped quote
      Lcx_abapgit_exception=>raise( 'PO file format error: bad quoting' ).
    ENDIF.

    rv_text = substring(
      val = rv_text
      off = 1
      len = lv_len - 2 ).

    rv_text = replace(
      val  = rv_text
      sub  = '\"'
      with = '"'
      occ  = 0 ).

    rv_text = replace(
      val  = rv_text
      sub  = '\n'
      with = cl_abap_char_utilities=>newline
      occ  = 0 ).

    " TODO: theoretically there can be unescaped " - is it a problem ? check standard

  ENDMETHOD.
  METHOD Lif_abapgit_i18n_file~ext.
    rv_ext = 'po'.
  ENDMETHOD.
  METHOD Lif_abapgit_i18n_file~lang.
    rv_lang = mv_lang.
  ENDMETHOD.
  METHOD Lif_abapgit_i18n_file~render.

    DATA lv_str TYPE string.

    lv_str = build_po_body( )->join_w_newline_and_flush( ).

    IF lv_str IS NOT INITIAL.
      lv_str = build_po_head( )->join_w_newline_and_flush( )
        && cl_abap_char_utilities=>newline
        && lv_str
        && cl_abap_char_utilities=>newline. " Trailing LF
      rv_data = Lcl_abapgit_convert=>string_to_xstring_utf8_bom( lv_str ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_i18n_file~translate.

    FIELD-SYMBOLS <ls_lxe> LIKE LINE OF ct_text_pairs.
    FIELD-SYMBOLS <ls_tr> LIKE LINE OF mt_pairs.
    DATA lv_idx TYPE i.

    LOOP AT ct_text_pairs ASSIGNING <ls_lxe>.
      CHECK <ls_lxe>-s_text IS NOT INITIAL.
      lv_idx = sy-tabix.

      READ TABLE mt_pairs ASSIGNING <ls_tr> WITH KEY source = <ls_lxe>-s_text.
      IF sy-subrc = 0 AND <ls_tr>-target IS NOT INITIAL.
        <ls_lxe>-t_text = <ls_tr>-target.
      ELSE.
        DELETE ct_text_pairs INDEX lv_idx. " Otherwise error in LXE FMs for empty translation
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_PO_FILE implementation

*>>>>>>> ZCL_ABAPGIT_SAP_NAMESPACE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sap_namespace=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sap_namespace=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_sap_namespace=====ccau.


class LCL_ABAPGIT_SAP_NAMESPACE implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_sap_namespace~exists.
    DATA lv_editflag TYPE trnspace-editflag.
    DATA lo_obj TYPE REF TO object.
    DATA lo_nsp TYPE REF TO object.
    FIELD-SYMBOLS <lg_obj> TYPE any.
    TRY.
        SELECT SINGLE editflag FROM ('TRNSPACE') INTO lv_editflag WHERE namespace = iv_namespace.
        rv_yes = boolc( sy-subrc = 0 ).
      CATCH cx_sy_dynamic_osql_error.
        ASSIGN ('XCO_CP_SYSTEM=>NAMESPACE') TO <lg_obj>.
        lo_obj = <lg_obj>.
        CALL METHOD lo_obj->('IF_XCO_CP_NAMESPACE_FACTORY~FOR')
          EXPORTING
            iv_value     = iv_namespace
          RECEIVING
            ro_namespace = lo_nsp.
        CALL METHOD lo_nsp->('IF_XCO_CP_NAMESPACE~EXISTS')
          RECEIVING
            rv_exists = rv_yes.
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_sap_namespace~is_editable.
    DATA lv_editflag TYPE trnspace-editflag.
    DATA lo_obj TYPE REF TO object.
    DATA lo_nsp TYPE REF TO object.
    FIELD-SYMBOLS <lg_obj> TYPE any.
    TRY.
        SELECT SINGLE editflag FROM ('TRNSPACE') INTO lv_editflag WHERE namespace = iv_namespace.
        rv_yes = boolc( sy-subrc = 0 AND lv_editflag = 'X' ).
      CATCH cx_sy_dynamic_osql_error.
        ASSIGN ('XCO_CP_SYSTEM=>NAMESPACE') TO <lg_obj>.
        lo_obj = <lg_obj>.
        CALL METHOD lo_obj->('IF_XCO_CP_NAMESPACE_FACTORY~FOR')
          EXPORTING
            iv_value     = iv_namespace
          RECEIVING
            ro_namespace = lo_nsp.
        CALL METHOD lo_nsp->('IF_XCO_CP_NAMESPACE~IS_CHANGEABLE')
          RECEIVING
            rv_exists = rv_yes.
    ENDTRY.
  ENDMETHOD.
  METHOD Lif_abapgit_sap_namespace~split_by_name.
* use this method instead of function module RS_NAME_SPLIT_NAMESPACE
    DATA lv_regex  TYPE string.
    DATA lv_length TYPE i.
    DATA lr_ex     TYPE REF TO cx_root.

    lv_regex = '^\/[^\/]{1,8}\/'.

    TRY.
        FIND REGEX lv_regex IN iv_obj_with_namespace MATCH LENGTH lv_length.
      CATCH cx_root INTO lr_ex.
        Lcx_abapgit_exception=>raise( lr_ex->get_text( ) ).
    ENDTRY.

    IF sy-subrc = 0 AND lv_length > 1.
      rs_obj_namespace-namespace = iv_obj_with_namespace(lv_length).
      rs_obj_namespace-obj_without_namespace = iv_obj_with_namespace+lv_length.
    ELSE.
      IF iv_obj_with_namespace(1) = '/'.
        Lcx_abapgit_exception=>raise( |The object { iv_obj_with_namespace } has an invalid namespace| ).
      ENDIF.
      rs_obj_namespace-obj_without_namespace = iv_obj_with_namespace.
    ENDIF.

    IF iv_allow_slash_in_name = abap_false AND rs_obj_namespace-obj_without_namespace CA '/'.
      Lcx_abapgit_exception=>raise(
       |Object without namespace { rs_obj_namespace-obj_without_namespace } contains a '/'| ).
    ENDIF.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SAP_NAMESPACE implementation

*>>>>>>> ZCL_ABAPGIT_SAP_REPORT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sap_report========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sap_report========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SAP_REPORT implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_sap_report~read_progdir.

    DATA ls_sapdir TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = iv_name
        i_state    = iv_state
      IMPORTING
        e_progdir  = ls_sapdir
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise_t100( ).
    ENDIF.

    MOVE-CORRESPONDING ls_sapdir TO rs_progdir.

    CLEAR: rs_progdir-edtx,
           rs_progdir-cnam,
           rs_progdir-cdat,
           rs_progdir-unam,
           rs_progdir-udat,
           rs_progdir-levl,
           rs_progdir-vern,
           rs_progdir-rmand,
           rs_progdir-sdate,
           rs_progdir-stime,
           rs_progdir-idate,
           rs_progdir-itime,
           rs_progdir-varcl,
           rs_progdir-state.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_report~update_progdir.

    DATA ls_progdir_new TYPE progdir.

    CALL FUNCTION 'READ_PROGDIR'
      EXPORTING
        i_progname = is_progdir-name
        i_state    = iv_state
      IMPORTING
        e_progdir  = ls_progdir_new
      EXCEPTIONS
        not_exists = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error reading program directory' ).
    ENDIF.

    ls_progdir_new-ldbname = is_progdir-ldbname.
    ls_progdir_new-dbna    = is_progdir-dbna.
    ls_progdir_new-dbapl   = is_progdir-dbapl.
    ls_progdir_new-rload   = is_progdir-rload.
    ls_progdir_new-fixpt   = is_progdir-fixpt.
    ls_progdir_new-appl    = is_progdir-appl.
    ls_progdir_new-rstat   = is_progdir-rstat.
    ls_progdir_new-uccheck = is_progdir-uccheck.
    ls_progdir_new-sqlx    = is_progdir-sqlx.
    ls_progdir_new-clas    = is_progdir-clas.
    ls_progdir_new-secu    = is_progdir-secu.

    CALL FUNCTION 'UPDATE_PROGDIR'
      EXPORTING
        i_progdir    = ls_progdir_new
        i_progname   = ls_progdir_new-name
        i_state      = ls_progdir_new-state
      EXCEPTIONS
        not_executed = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( 'Error updating program directory' ).
    ENDIF.

    " Function UPDATE_PROGDIR does not update VARCL, so we do it here
    SELECT SINGLE * FROM progdir INTO ls_progdir_new
      WHERE name  = ls_progdir_new-name
        AND state = ls_progdir_new-state.
    IF sy-subrc = 0 AND is_progdir-varcl <> ls_progdir_new-varcl.
      UPDATE progdir SET varcl = is_progdir-varcl
        WHERE name  = ls_progdir_new-name
          AND state = ls_progdir_new-state.               "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.
  METHOD authorization_check.

    IF is_item IS NOT INITIAL.
      TRY.
          CALL FUNCTION 'RS_ACCESS_PERMISSION'
            EXPORTING
              mode                           = iv_mode
              object                         = is_item-obj_name
              object_class                   = is_item-obj_type
              suppress_corr_check            = abap_true
              suppress_language_check        = abap_true
              suppress_extend_dialog         = abap_true
              abap_langu_version_upon_insert = is_item-abap_language_version " does not exist on lower releases
            EXCEPTIONS
              canceled_in_corr               = 1
              enqueued_by_user               = 2
              enqueue_system_failure         = 3
              illegal_parameter_values       = 4
              locked_by_author               = 5
              no_modify_permission           = 6
              no_show_permission             = 7
              permission_failure             = 8
              request_language_denied        = 9
              OTHERS                         = 10.
        CATCH cx_sy_dyn_call_param_not_found.
          CALL FUNCTION 'RS_ACCESS_PERMISSION'
            EXPORTING
              mode                     = iv_mode
              object                   = is_item-obj_name
              object_class             = is_item-obj_type
              suppress_corr_check      = abap_true
              suppress_language_check  = abap_true
              suppress_extend_dialog   = abap_true
            EXCEPTIONS
              canceled_in_corr         = 1
              enqueued_by_user         = 2
              enqueue_system_failure   = 3
              illegal_parameter_values = 4
              locked_by_author         = 5
              no_modify_permission     = 6
              no_show_permission       = 7
              permission_failure       = 8
              request_language_denied  = 9
              OTHERS                   = 10.
      ENDTRY.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_report~delete_report.

    authorization_check(
      iv_mode = 'DELETE'
      is_item = is_item ).

    DELETE REPORT iv_name.

    IF sy-subrc <> 0 AND iv_raise_error = abap_true.
      Lcx_abapgit_exception=>raise( |Error deleting report { iv_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_report~insert_report.

    ASSERT iv_state CA ' AI'.
    ASSERT iv_program_type CA ' 1FIJKMST'.

    authorization_check(
      iv_mode = 'MODIFY'
      is_item = is_item ).

    IF iv_state IS INITIAL.
      INSERT REPORT iv_name FROM it_source.
    ELSEIF iv_program_type IS INITIAL AND iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state.
    ELSEIF iv_extension_type IS INITIAL.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state
        PROGRAM TYPE iv_program_type.
    ELSE.
      INSERT REPORT iv_name FROM it_source
        STATE iv_state
        EXTENSION TYPE iv_extension_type
        PROGRAM TYPE iv_program_type.
    ENDIF.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error inserting report { iv_name }| ).
    ENDIF.

    " In lower releases, INSERT REPORT does not support setting ABAP Language version (VERSION)
    " Therefore, update the flag directly
    UPDATE progdir SET uccheck = iv_version WHERE name = iv_name AND state = iv_state.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_report~read_report.

    ASSERT iv_state CA ' AI'.

    authorization_check(
      iv_mode = 'SHOW'
      is_item = is_item ).

    IF iv_state IS INITIAL.
      READ REPORT iv_name INTO rt_source.
    ELSE.
      READ REPORT iv_name INTO rt_source STATE iv_state.
    ENDIF.

    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Error reading report { iv_name }| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_sap_report~update_report.

    DATA lt_new TYPE string_table.
    DATA lt_old TYPE string_table.

    lt_new = it_source.
    lt_old = Lif_abapgit_sap_report~read_report( iv_name ).

    IF lt_old <> lt_new.
      Lif_abapgit_sap_report~insert_report(
        iv_name           = iv_name
        it_source         = it_source
        iv_state          = iv_state
        iv_program_type   = iv_program_type
        iv_extension_type = iv_extension_type
        iv_package        = iv_package
        iv_version        = iv_version
        is_item           = is_item ).

      rv_updated = abap_true.
    ELSE.
      rv_updated = abap_false.
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SAP_REPORT implementation

*>>>>>>> ZCL_ABAPGIT_SERVICES_GIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_services_git======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_services_git======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SERVICES_GIT implementation.
*"* method's implementations
*include methods.
  METHOD commit.

    DATA: ls_comment TYPE Lif_abapgit_git_definitions=>ty_comment,
          li_user    TYPE REF TO Lif_abapgit_persist_user.

    li_user = Lcl_abapgit_persistence_user=>get_instance( ).
    li_user->set_repo_git_user_name( iv_url      = io_repo->get_url( )
                                     iv_username = is_commit-committer_name ).
    li_user->set_repo_git_user_email( iv_url   = io_repo->get_url( )
                                      iv_email = is_commit-committer_email ).

    IF is_commit-committer_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Commit: Committer name empty' ).
    ELSEIF is_commit-committer_email IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Commit: Committer email empty' ).
    ELSEIF is_commit-author_email IS NOT INITIAL AND is_commit-author_name IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Commit: Author name empty' ). " Opposite should be OK ?
    ELSEIF is_commit-comment IS INITIAL.
      Lcx_abapgit_exception=>raise( 'Commit: empty comment' ).
    ENDIF.

    ls_comment-committer-name  = is_commit-committer_name.
    ls_comment-committer-email = is_commit-committer_email.
    ls_comment-author-name     = is_commit-author_name.
    ls_comment-author-email    = is_commit-author_email.
    ls_comment-comment         = is_commit-comment.

    IF NOT is_commit-body IS INITIAL.
      CONCATENATE ls_comment-comment '' is_commit-body
        INTO ls_comment-comment SEPARATED BY cl_abap_char_utilities=>newline.
    ENDIF.

    Lcl_abapgit_exit=>get_instance( )->validate_before_push(
      is_comment = ls_comment
      io_stage   = io_stage
      io_repo    = io_repo ).

    io_repo->push( is_comment = ls_comment
                   io_stage   = io_stage ).

    COMMIT WORK.

  ENDMETHOD.
  METHOD create_branch.

    DATA: lv_name               TYPE string,
          lv_cancel             TYPE abap_bool,
          lo_repo               TYPE REF TO Lcl_abapgit_repo_online,
          lv_msg                TYPE string,
          li_popups             TYPE REF TO Lif_abapgit_popups,
          lv_source_branch_name TYPE string.


    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
    lv_source_branch_name = lo_repo->get_selected_branch( ).

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    li_popups->create_branch_popup(
      EXPORTING
        iv_source_branch_name = lv_source_branch_name
      IMPORTING
        ev_name               = lv_name
        ev_cancel             = lv_cancel ).

    IF lv_cancel = abap_true.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    lo_repo->create_branch( lv_name ).

    lv_msg = |Branch switched from { Lcl_abapgit_git_branch_list=>get_display_name( lv_source_branch_name )
      } to new branch { Lcl_abapgit_git_branch_list=>get_display_name( lv_name ) }|.
    MESSAGE lv_msg TYPE 'S'.

  ENDMETHOD.
  METHOD delete_branch.

    DATA: lo_repo   TYPE REF TO Lcl_abapgit_repo_online,
          ls_branch TYPE Lif_abapgit_git_definitions=>ty_git_branch,
          lv_msg    TYPE string,
          li_popups TYPE REF TO Lif_abapgit_popups.


    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).
    ls_branch = li_popups->branch_list_popup( iv_url         = lo_repo->get_url( )
                                              iv_hide_branch = lo_repo->get_selected_branch( )
                                              iv_hide_head   = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    Lcl_abapgit_git_porcelain=>delete_branch(
      iv_url    = lo_repo->get_url( )
      is_branch = ls_branch ).

    lv_msg = |Branch { ls_branch-display_name } deleted|.
    MESSAGE lv_msg TYPE 'S'.

  ENDMETHOD.
  METHOD delete_tag.

    DATA: lo_repo TYPE REF TO Lcl_abapgit_repo_online,
          ls_tag  TYPE Lif_abapgit_git_definitions=>ty_git_tag,
          lv_text TYPE string.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = Lcl_abapgit_ui_factory=>get_popups( )->tag_list_popup( lo_repo->get_url( ) ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    Lcl_abapgit_git_porcelain=>delete_tag(
      iv_url = lo_repo->get_url( )
      is_tag = ls_tag ).

    lv_text = |Tag { ls_tag-display_name } deleted|.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.
  METHOD pull.

    DATA: lo_repo TYPE REF TO Lcl_abapgit_repo.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    lo_repo->refresh( ).

    Lcl_abapgit_services_repo=>gui_deserialize( lo_repo ).

  ENDMETHOD.
  METHOD switch_branch.

    DATA: lo_repo   TYPE REF TO Lcl_abapgit_repo_online,
          ls_branch TYPE Lif_abapgit_git_definitions=>ty_git_branch.


    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_branch = Lcl_abapgit_ui_factory=>get_popups( )->branch_list_popup(
      iv_url             = lo_repo->get_url( )
      iv_default_branch  = lo_repo->get_selected_branch( )
      iv_show_new_option = abap_true ).
    IF ls_branch IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    IF ls_branch-name = Lif_abapgit_popups=>c_new_branch_label.
      create_branch( iv_key ).
      RETURN.
    ENDIF.

    IF lo_repo->get_selected_commit( ) IS NOT INITIAL.
      lo_repo->select_commit( space ).
    ENDIF.

    lo_repo->select_branch( ls_branch-name ).
    COMMIT WORK AND WAIT.

  ENDMETHOD.
  METHOD switch_tag.

    DATA: lo_repo TYPE REF TO Lcl_abapgit_repo_online,
          ls_tag  TYPE Lif_abapgit_git_definitions=>ty_git_tag,
          lv_text TYPE string.

    lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    ls_tag = Lcl_abapgit_ui_factory=>get_popups( )->tag_list_popup( lo_repo->get_url( ) ).
    IF ls_tag IS INITIAL.
      RAISE EXCEPTION TYPE Lcx_abapgit_cancel.
    ENDIF.

    lo_repo->select_branch( Lcl_abapgit_git_tag=>remove_peel( ls_tag-name ) ).

    COMMIT WORK AND WAIT.

    lv_text = |Tag switched to { ls_tag-display_name } |.

    MESSAGE lv_text TYPE 'S'.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SERVICES_GIT implementation

*>>>>>>> ZCL_ABAPGIT_SOTS_HANDLER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_sots_handler======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_sots_handler======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_SOTS_HANDLER implementation.
*"* method's implementations
*include methods.
  METHOD create_sots.

    DATA:
      lt_sots     TYPE ty_sots_tt,
      lt_sots_use TYPE ty_sots_use_tt.

    io_xml->read( EXPORTING iv_name = 'SOTS'
                  CHANGING  cg_data = lt_sots ).
    io_xml->read( EXPORTING iv_name = 'SOTS_USE'
                  CHANGING  cg_data = lt_sots_use ).

    create_sots_from_data(
      iv_package  = iv_package
      it_sots     = lt_sots
      it_sots_use = lt_sots_use ).

  ENDMETHOD.
  METHOD create_sots_from_data.

    DATA:
      lt_objects         TYPE sotr_objects,
      lv_object          LIKE LINE OF lt_objects,
      lv_subrc           TYPE sy-subrc,
      ls_header          TYPE btfr_head,
      lt_text_tab        TYPE sotr_text_tt,
      lt_string_tab      TYPE sotr_textl_tt,
      ls_entry           LIKE LINE OF lt_string_tab,
      lv_concept         TYPE sotr_conc,
      lv_concept_default TYPE sotr_conc.

    FIELD-SYMBOLS <ls_sots> LIKE LINE OF it_sots.

    LOOP AT it_sots ASSIGNING <ls_sots>.

      CALL FUNCTION 'SOTR_OBJECT_GET_OBJECTS'
        EXPORTING
          object_vector    = <ls_sots>-header-objid_vec
        IMPORTING
          objects          = lt_objects
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

      READ TABLE lt_objects INDEX 1 INTO lv_object.
      ASSERT sy-subrc = 0.

      " Reimplementation of SOTR_STRING_CREATE_CONCEPT because we can't supply
      " concept and it would then be generated.

      LOOP AT <ls_sots>-entries INTO ls_entry.
        ls_entry-langu   = <ls_sots>-header-crea_lan.
        ls_entry-concept = <ls_sots>-header-concept.
        INSERT ls_entry INTO TABLE lt_string_tab.
      ENDLOOP.

      MOVE-CORRESPONDING <ls_sots>-header TO ls_header.
      ls_header-paket = iv_package.

      lv_concept = <ls_sots>-header-concept.

      PERFORM btfr_create IN PROGRAM saplsotr_db_string
        USING lv_object
              sy-langu
              abap_false
              abap_true
        CHANGING lt_text_tab
                 lt_string_tab
                 ls_header
                 lv_concept
                 lv_concept_default
                 lv_subrc.

      CASE lv_subrc.
        WHEN 1.
          MESSAGE e100(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 2.
          MESSAGE e101(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 3.
          MESSAGE i305(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 4.
          " The concept will be created in the non-original system (not an error)
        WHEN 5.
          MESSAGE e504(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 6.
          MESSAGE e035(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 7.
          MESSAGE e170(sotr_mess) INTO Lcx_abapgit_exception=>null.
        WHEN 9.
          MESSAGE e102(sotr_mess) INTO Lcx_abapgit_exception=>null.
      ENDCASE.

      IF lv_subrc <> 0 AND lv_subrc <> 4.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'SOTR_USAGE_STRING_MODIFY'
      EXPORTING
        sotr_usage = it_sots_use.

  ENDMETHOD.
  METHOD delete_sots.

    DATA lt_sots_use TYPE ty_sots_use_tt.

    FIELD-SYMBOLS <ls_sots_use> LIKE LINE OF lt_sots_use.

    lt_sots_use = get_sots_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    " Remove any usage to ensure deletion, see function module BTFR_CHECK
    DELETE sotr_useu FROM TABLE lt_sots_use ##SUBRC_OK.

    LOOP AT lt_sots_use ASSIGNING <ls_sots_use> WHERE concept IS NOT INITIAL.

      CALL FUNCTION 'BTFR_DELETE_SINGLE_TEXT'
        EXPORTING
          concept             = <ls_sots_use>-concept
          flag_string         = abap_true
        EXCEPTIONS
          text_not_found      = 1 "ok
          invalid_package     = 3
          text_not_changeable = 4
          text_enqueued       = 5
          no_correction       = 6
          parameter_error     = 7
          OTHERS              = 8.
      IF sy-subrc > 2.
        Lcx_abapgit_exception=>raise_t100( ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD get_sots_4_concept.

    DATA: ls_header  TYPE ty_sots-header,
          lt_entries TYPE ty_sots-entries.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF lt_entries.

    CALL FUNCTION 'SOTR_STRING_GET_CONCEPT'
      EXPORTING
        concept        = iv_concept
      IMPORTING
        header         = ls_header
        entries        = lt_entries
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CLEAR: ls_header-paket,
           ls_header-crea_name,
           ls_header-crea_tstut,
           ls_header-chan_name,
           ls_header-chan_tstut,
           ls_header-system_id.

    LOOP AT lt_entries ASSIGNING <ls_entry>.
      CLEAR: <ls_entry>-version,
             <ls_entry>-crea_name,
             <ls_entry>-crea_tstut,
             <ls_entry>-chan_name,
             <ls_entry>-chan_tstut.
    ENDLOOP.

    rs_sots-header  = ls_header.
    rs_sots-entries = lt_entries.

  ENDMETHOD.
  METHOD get_sots_usage.

    DATA: lv_obj_name TYPE trobj_name.

    lv_obj_name = iv_obj_name.

    " Objects with multiple components
    IF iv_pgmid = 'LIMU' AND ( iv_object CP 'WDY*' OR iv_object = 'WAPP' ).
      lv_obj_name+30 = '%'.
    ENDIF.

    CALL FUNCTION 'SOTR_USAGE_STRING_READ'
      EXPORTING
        pgmid          = iv_pgmid
        object         = iv_object
        obj_name       = lv_obj_name
      IMPORTING
        sotr_usage     = rt_sots_use
      EXCEPTIONS
        no_entry_found = 1
        error_in_pgmid = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      SORT rt_sots_use.
    ENDIF.

  ENDMETHOD.
  METHOD read_sots.

    FIELD-SYMBOLS <ls_sots_use> LIKE LINE OF et_sots_use.

    DATA ls_sots TYPE ty_sots.

    " OTR long text (string) usage: see TABLE BTFR_OBJ_IDS
    " LIMU: CPUB, WAPP
    " R3TR: SICF, SMIF, XSLT

    et_sots_use = get_sots_usage( iv_pgmid    = iv_pgmid
                                  iv_object   = iv_object
                                  iv_obj_name = iv_obj_name ).

    LOOP AT et_sots_use ASSIGNING <ls_sots_use> WHERE concept IS NOT INITIAL.
      ls_sots = get_sots_4_concept( <ls_sots_use>-concept ).

      IF io_i18n_params->ms_params-main_language_only = abap_true.
        DELETE ls_sots-entries WHERE langu <> io_i18n_params->ms_params-main_language.
        CHECK ls_sots-entries IS NOT INITIAL.
      ENDIF.

      INSERT ls_sots INTO TABLE et_sots.
    ENDLOOP.

    IF io_xml IS BOUND.
      io_xml->add( iv_name = 'SOTS'
                   ig_data = et_sots ).
      io_xml->add( iv_name = 'SOTS_USE'
                   ig_data = et_sots_use ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_SOTS_HANDLER implementation

*>>>>>>> ZCL_ABAPGIT_STRING_BUFFER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_string_buffer=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_string_buffer=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_string_buffer=====ccau.


class LCL_ABAPGIT_STRING_BUFFER implementation.
*"* method's implementations
*include methods.
  METHOD add.
    APPEND iv_str TO mt_buffer.
    ro_me = me.
  ENDMETHOD.
  METHOD join_and_flush.
    rv_str = concat_lines_of( mt_buffer ).
    CLEAR mt_buffer.
  ENDMETHOD.
  METHOD join_w_newline_and_flush.
    rv_str = concat_lines_of(
      table = mt_buffer
      sep   = cl_abap_char_utilities=>newline ).
    CLEAR mt_buffer.
  ENDMETHOD.
  METHOD join_w_space_and_flush.
    rv_str = concat_lines_of(
      table = mt_buffer
      sep   = ` ` ).
    CLEAR mt_buffer.
  ENDMETHOD.
  METHOD new.
    CREATE OBJECT ro_me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_STRING_BUFFER implementation

*>>>>>>> ZCL_ABAPGIT_STRING_MAP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_string_map========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_string_map========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_string_map========ccau.


class LCL_ABAPGIT_STRING_MAP implementation.
*"* method's implementations
*include methods.
  METHOD clear.
    IF mv_read_only = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot clear. This string map is immutable' ).
    ENDIF.
    CLEAR mt_entries.
  ENDMETHOD.
  METHOD constructor.
    mv_is_strict = abap_true.
    mv_case_insensitive = iv_case_insensitive.
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_case_insensitive = iv_case_insensitive.
  ENDMETHOD.
  METHOD delete.

    IF mv_read_only = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot delete. This string map is immutable' ).
    ENDIF.

    DELETE mt_entries WHERE k = iv_key.

  ENDMETHOD.
  METHOD freeze.
    mv_read_only = abap_true.
  ENDMETHOD.
  METHOD get.

    DATA lv_key LIKE iv_key.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = lv_key.
    IF sy-subrc IS INITIAL.
      rv_val = <ls_entry>-v.
    ENDIF.

  ENDMETHOD.
  METHOD has.

    READ TABLE mt_entries TRANSPORTING NO FIELDS WITH KEY k = iv_key.
    rv_has = boolc( sy-subrc IS INITIAL ).

  ENDMETHOD.
  METHOD is_empty.
    rv_yes = boolc( lines( mt_entries ) = 0 ).
  ENDMETHOD.
  METHOD set.

    DATA lv_key LIKE iv_key.
    DATA ls_entry LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    IF mv_read_only = abap_true.
      Lcx_abapgit_exception=>raise( 'Cannot set. This string map is immutable' ).
    ENDIF.

    IF mv_case_insensitive = abap_true.
      lv_key = to_upper( iv_key ).
    ELSE.
      lv_key = iv_key.
    ENDIF.

    READ TABLE mt_entries ASSIGNING <ls_entry> WITH KEY k = lv_key.
    IF sy-subrc IS INITIAL.
      <ls_entry>-v = iv_val.
    ELSE.
      ls_entry-k = lv_key.
      ls_entry-v = iv_val.
      INSERT ls_entry INTO TABLE mt_entries.
    ENDIF.

    ro_map = me.

  ENDMETHOD.
  METHOD size.

    rv_size = lines( mt_entries ).

  ENDMETHOD.
  METHOD strict.
    mv_is_strict = iv_strict.
    ro_instance = me.
  ENDMETHOD.
  METHOD to_abap.

    DATA lo_type TYPE REF TO cl_abap_typedescr.
    DATA lv_field TYPE string.
    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.
    FIELD-SYMBOLS <lv_val> TYPE any.

    lo_type = cl_abap_typedescr=>describe_by_data( cs_container ).
    IF lo_type->type_kind <> cl_abap_typedescr=>typekind_struct1
      AND lo_type->type_kind <> cl_abap_typedescr=>typekind_struct2.
      Lcx_abapgit_exception=>raise( 'Only structures supported' ).
    ENDIF.

    LOOP AT mt_entries ASSIGNING <ls_entry>.
      lv_field = to_upper( <ls_entry>-k ).
      ASSIGN COMPONENT lv_field OF STRUCTURE cs_container TO <lv_val>.
      IF sy-subrc = 0.
        " TODO check target type ?
        <lv_val> = <ls_entry>-v.
      ELSEIF mv_is_strict = abap_false.
        CONTINUE.
      ELSE.
        Lcx_abapgit_exception=>raise( |Component { lv_field } not found in target| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD merge.

    FIELD-SYMBOLS <ls_entry> LIKE LINE OF mt_entries.

    LOOP AT io_string_map->mt_entries ASSIGNING <ls_entry>.
      set(
        iv_key = <ls_entry>-k
        iv_val = <ls_entry>-v ).
    ENDLOOP.

    ro_instance = me.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_STRING_MAP implementation

*>>>>>>> ZCL_ABAPGIT_TADIR <<<<<<<*

*"* macro definitions
*include zcl_abapgit_tadir=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_tadir=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_TADIR implementation.
*"* method's implementations
*include methods.
  METHOD add_local_packages.

    FIELD-SYMBOLS:
      <lv_package> LIKE LINE OF it_packages,
      <ls_tadir>   LIKE LINE OF ct_tadir.

    LOOP AT it_packages ASSIGNING <lv_package>.

      " Local packages are not in TADIR, only in TDEVC, act as if they were
      IF <lv_package> CP '$*'. " OR <package> CP 'T*' ).
        APPEND INITIAL LINE TO ct_tadir ASSIGNING <ls_tadir>.
        <ls_tadir>-pgmid      = 'R3TR'.
        <ls_tadir>-object     = 'DEVC'.
        <ls_tadir>-obj_name   = <lv_package>.
        <ls_tadir>-devclass   = <lv_package>.
        <ls_tadir>-srcsystem  = sy-sysid.
        <ls_tadir>-masterlang = sy-langu.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD add_namespaces.

    DATA lt_tadir_nspc TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    " Namespaces are not in TADIR, but are necessary for creating objects in transportable packages
    LOOP AT ct_tadir ASSIGNING <ls_tadir> WHERE obj_name(1) = '/'.
      add_namespace(
        EXPORTING
          iv_package    = iv_package
          iv_object     = <ls_tadir>-obj_name
        CHANGING
          ct_tadir      = ct_tadir
          ct_tadir_nspc = lt_tadir_nspc ).
    ENDLOOP.

    " Root package of repo might not exist yet but needs to be considered, too
    IF iv_package CP '/*'.
      add_namespace(
        EXPORTING
          iv_package    = iv_package
          iv_object     = iv_package
        CHANGING
          ct_tadir      = ct_tadir
          ct_tadir_nspc = lt_tadir_nspc ).
    ENDIF.

  ENDMETHOD.
  METHOD build.

    DATA lt_packages TYPE Lif_abapgit_sap_package=>ty_devclass_tt.

    select_objects(
      EXPORTING
        iv_package            = iv_package
        iv_ignore_subpackages = iv_ignore_subpackages
        iv_only_local_objects = iv_only_local_objects
      IMPORTING
        et_tadir              = rt_tadir
        et_packages           = lt_packages ).

    add_local_packages(
      EXPORTING
        it_packages = lt_packages
      CHANGING
        ct_tadir    = rt_tadir ).

    add_namespaces(
      EXPORTING
        iv_package = iv_package
      CHANGING
        ct_tadir   = rt_tadir ).

    determine_path(
      EXPORTING
        iv_package = iv_package
        io_dot     = io_dot
      CHANGING
        ct_tadir   = rt_tadir ).

  ENDMETHOD.
  METHOD check_exists.

    DATA: li_progress TYPE REF TO Lif_abapgit_progress,
          ls_item     TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF it_tadir.


    li_progress = Lcl_abapgit_progress=>get_instance( lines( it_tadir ) ).

* rows from database table TADIR are not removed for
* transportable objects until the transport is released
    LOOP AT it_tadir ASSIGNING <ls_tadir>.
      IF sy-tabix MOD 200 = 0.
        li_progress->show(
          iv_current = sy-tabix
          iv_text    = |Check object exists { <ls_tadir>-object } { <ls_tadir>-obj_name }| ).
      ENDIF.

      ls_item-obj_type = <ls_tadir>-object.
      ls_item-obj_name = <ls_tadir>-obj_name.
      ls_item-devclass = <ls_tadir>-devclass.

      IF Lcl_abapgit_objects=>exists( ls_item ) = abap_true.
        APPEND <ls_tadir> TO rt_tadir.
      ENDIF.
    ENDLOOP.

    li_progress->off( ).

  ENDMETHOD.
  METHOD determine_path.

    DATA:
      lv_path         TYPE string,
      lo_folder_logic TYPE REF TO Lcl_abapgit_folder_logic,
      lv_last_package TYPE devclass VALUE cl_abap_char_utilities=>horizontal_tab.

    FIELD-SYMBOLS <ls_tadir> LIKE LINE OF ct_tadir.

    lo_folder_logic = Lcl_abapgit_folder_logic=>get_instance( ).

    LOOP AT ct_tadir ASSIGNING <ls_tadir>.

      IF lv_last_package <> <ls_tadir>-devclass.
        "Change in Package
        lv_last_package = <ls_tadir>-devclass.

        IF NOT io_dot IS INITIAL.
          lv_path = lo_folder_logic->package_to_path(
            iv_top     = iv_package
            io_dot     = io_dot
            iv_package = <ls_tadir>-devclass ).
        ENDIF.
      ENDIF.

      <ls_tadir>-path = lv_path.
      <ls_tadir>-korrnum = ''.
    ENDLOOP.

  ENDMETHOD.
  METHOD select_objects.

    DATA:
      lt_excludes  TYPE RANGE OF trobjtype,
      ls_exclude   LIKE LINE OF lt_excludes,
      lt_srcsystem TYPE RANGE OF tadir-srcsystem,
      ls_srcsystem LIKE LINE OF lt_srcsystem.

    " Determine packages to read
    IF iv_ignore_subpackages = abap_false.
      et_packages = Lcl_abapgit_factory=>get_sap_package( iv_package )->list_subpackages( ).
    ENDIF.
    INSERT iv_package INTO et_packages INDEX 1.

    " Exclude object types with tadir entries that are included elsewhere
    ls_exclude-sign   = 'I'.
    ls_exclude-option = 'EQ'.
    ls_exclude-low    = 'SOTR'. " automatically created for SAP packages (DEVC)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB1'. " covered by business function sets (SFBS)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'SFB2'. " covered by business functions (SFBF)
    APPEND ls_exclude TO lt_excludes.
    ls_exclude-low    = 'STOB'. " auto generated by core data services (DDLS)
    APPEND ls_exclude TO lt_excludes.

    IF is_sots_excluded( et_packages ) = abap_true.
      ls_exclude-low = 'SOTS'.
      APPEND ls_exclude TO lt_excludes.
    ENDIF.

    " Limit to objects belonging to this system
    IF iv_only_local_objects = abap_true.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    IF et_packages IS NOT INITIAL.
      SELECT * FROM tadir INTO CORRESPONDING FIELDS OF TABLE et_tadir
        FOR ALL ENTRIES IN et_packages
        WHERE devclass = et_packages-table_line
        AND pgmid      = 'R3TR'
        AND object     NOT IN lt_excludes
        AND delflag    = abap_false
        AND srcsystem  IN lt_srcsystem
        ORDER BY PRIMARY KEY ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC
    ENDIF.

    SORT et_tadir BY devclass pgmid object obj_name.

  ENDMETHOD.
  METHOD Lif_abapgit_tadir~get_object_package.

    DATA: ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir,
          ls_item  TYPE Lif_abapgit_definitions=>ty_item.

    ls_tadir = Lif_abapgit_tadir~read_single(
      iv_pgmid    = iv_pgmid
      iv_object   = iv_object
      iv_obj_name = iv_obj_name ).

    IF ls_tadir-delflag = abap_true.
      RETURN. "Mark for deletion -> return nothing
    ENDIF.

    ls_item-obj_type = ls_tadir-object.
    ls_item-obj_name = ls_tadir-obj_name.
    ls_item-devclass = ls_tadir-devclass.

    IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
      RETURN.
    ENDIF.

    rv_devclass = ls_tadir-devclass.

  ENDMETHOD.
  METHOD Lif_abapgit_tadir~read.

    DATA: li_exit TYPE REF TO Lif_abapgit_exit.
    DATA: lr_tadir TYPE REF TO Lif_abapgit_definitions=>ty_tadir.
    DATA: lt_filter TYPE Lif_abapgit_definitions=>ty_tadir_tt.

    " Start recursion
    " hmm, some problems here, should TADIR also build path?
    rt_tadir = build(
      iv_package            = iv_package
      io_dot                = io_dot
      iv_ignore_subpackages = iv_ignore_subpackages
      iv_only_local_objects = iv_only_local_objects ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->change_tadir(
      EXPORTING
        iv_package = iv_package
        ii_log     = ii_log
      CHANGING
        ct_tadir   = rt_tadir ).

    IF it_filter IS NOT INITIAL.
      "Apply filter manually instead of calling zcl_abapgit_repo_filter->apply,
      "so that we can execute a unit test. The method applies addition filtering
      "and does therefore additional selects
      lt_filter = it_filter.
      SORT lt_filter BY object obj_name.
      LOOP AT rt_tadir REFERENCE INTO lr_tadir.
        READ TABLE lt_filter TRANSPORTING NO FIELDS
                 WITH KEY object = lr_tadir->object
                          obj_name = lr_tadir->obj_name
                          BINARY SEARCH.
        IF sy-subrc <> 0.
          DELETE rt_tadir.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF iv_check_exists = abap_true.
      rt_tadir = check_exists( rt_tadir ).
    ENDIF.
  ENDMETHOD.
  METHOD Lif_abapgit_tadir~read_single.

    SELECT SINGLE * FROM tadir INTO CORRESPONDING FIELDS OF rs_tadir
      WHERE pgmid = iv_pgmid
      AND object = iv_object
      AND obj_name = iv_obj_name.                         "#EC CI_SUBRC
    CLEAR rs_tadir-korrnum.

  ENDMETHOD.
  METHOD is_sots_excluded.

    " Todo: once all OTR longtexts are handled by object-specific class,
    " we can exclude SOTS completely (just like SOTR)
    " Until then, we need an object-type specific check here

    DATA:
      lt_concepts TYPE STANDARD TABLE OF sotr_headu-concept,
      lv_count    TYPE i.

    ASSERT it_packages IS NOT INITIAL.

    rv_exclude = abap_false.

    " Get all OTR longtexts
    SELECT concept FROM sotr_headu INTO TABLE lt_concepts
      FOR ALL ENTRIES IN it_packages WHERE paket = it_packages-table_line.
    IF lines( lt_concepts ) > 0.
      " Check if there are any texts related to objects that do not serialize these texts (yet)
      " If yes, we need to keep processing SOTS
      SELECT COUNT(*) FROM sotr_useu INTO lv_count
        FOR ALL ENTRIES IN lt_concepts WHERE concept = lt_concepts-table_line
        AND NOT ( pgmid = 'R3TR' AND object = 'SICF' )
        AND NOT ( pgmid = 'LIMU' AND object = 'CPUB' ).
      IF lv_count > 0.
        RETURN.
      ENDIF.
    ENDIF.

    " If no, SOTS can be excluded from the TADIR selection
    rv_exclude = abap_true.

  ENDMETHOD.
  METHOD add_namespace.

    DATA ls_tadir TYPE Lif_abapgit_definitions=>ty_tadir.
    DATA ls_obj_with_namespace TYPE Lif_abapgit_definitions=>ty_obj_namespace.

    TRY.
        ls_obj_with_namespace = Lcl_abapgit_factory=>get_sap_namespace( )->split_by_name( iv_object ).
      CATCH Lcx_abapgit_exception.
        "Ignore the exception like before the replacement of the FM RS_NAME_SPLIT_NAMESPACE
        RETURN.
    ENDTRY.

    IF ls_obj_with_namespace-namespace IS NOT INITIAL.

      READ TABLE ct_tadir_nspc TRANSPORTING NO FIELDS
        WITH KEY pgmid = 'R3TR' object = 'NSPC' obj_name = ls_obj_with_namespace-namespace.
      IF sy-subrc <> 0.
        ls_tadir-pgmid      = 'R3TR'.
        ls_tadir-object     = 'NSPC'.
        ls_tadir-obj_name   = ls_obj_with_namespace-namespace.
        ls_tadir-devclass   = iv_package.
        ls_tadir-srcsystem  = sy-sysid.
        ls_tadir-masterlang = sy-langu.
        INSERT ls_tadir INTO TABLE ct_tadir.
        INSERT ls_tadir INTO TABLE ct_tadir_nspc.
      ENDIF.

    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TADIR implementation

*>>>>>>> ZCL_ABAPGIT_TIMER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_timer=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_timer=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_timer=============ccau.


class LCL_ABAPGIT_TIMER implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mv_text  = iv_text.
    mv_count = iv_count.
  ENDMETHOD.
  METHOD create.
    CREATE OBJECT ro_timer
      EXPORTING
        iv_text  = iv_text
        iv_count = iv_count.
  ENDMETHOD.
  METHOD end.

    DATA:
      lv_timestamp TYPE timestampl,
      lv_runtime   TYPE timestampl,
      lv_sec       TYPE p LENGTH 11 DECIMALS 2.

    IF mv_timer IS INITIAL.
      rv_result = 'Runtime measurement has not been started'.
    ELSE.
      GET TIME STAMP FIELD lv_timestamp.

      TRY.
          lv_runtime = cl_abap_tstmp=>subtract(
            tstmp1 = lv_timestamp
            tstmp2 = mv_timer ).

          lv_sec = lv_runtime. " round to 2 decimal places

          IF mv_count = 1.
            rv_result = |1 object, |.
          ELSEIF mv_count > 1.
            rv_result = |{ mv_count } objects, |.
          ENDIF.

          rv_result = rv_result && |{ lv_sec } seconds|.

        CATCH cx_parameter_invalid.
          rv_result = 'Error getting runtime measurement'.
      ENDTRY.
    ENDIF.

    IF iv_output_as_status_message = abap_true.
      MESSAGE s000(oo) WITH mv_text rv_result.
    ENDIF.

    IF mv_text IS NOT INITIAL.
      rv_result = |{ mv_text } { rv_result }|.
    ENDIF.

  ENDMETHOD.
  METHOD start.
    GET TIME STAMP FIELD mv_timer.
    ro_timer = me.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TIMER implementation

*>>>>>>> ZCL_ABAPGIT_TRANSPORT_MASS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_transport_mass====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_transport_mass====ccimp.
CLASS SHRITEFUH64VYIPO5IWUYCIJCOESFY DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS f4_folder
      RETURNING
        VALUE(rv_folder) TYPE string
      RAISING
        Lcx_abapgit_exception.
    CLASS-METHODS open_folder_frontend
      IMPORTING
        iv_folder TYPE string
      RAISING
        Lcx_abapgit_exception.
    CLASS-METHODS select_tr_requests
      RETURNING
        VALUE(rt_trkorr) TYPE trwbo_request_headers.

  PRIVATE SECTION.
    CLASS-DATA gv_last_folder TYPE string.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYCIJCOESFY IMPLEMENTATION.

  METHOD f4_folder.

    DATA: lv_title   TYPE string,
          lo_fe_serv TYPE REF TO Lif_abapgit_frontend_services.

    lo_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).
    lv_title = 'Choose the destination folder for the ZIP files'.

    lo_fe_serv->directory_browse(
      EXPORTING
         iv_window_title   = lv_title
         iv_initial_folder = gv_last_folder
      CHANGING
        cv_selected_folder = rv_folder ).

    "Store the last directory for user friendly UI
    gv_last_folder = rv_folder.

  ENDMETHOD.

  METHOD open_folder_frontend.
    IF iv_folder IS INITIAL.
      RETURN.
    ENDIF.

    Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = iv_folder ).
  ENDMETHOD.

  METHOD select_tr_requests.

    DATA: ls_popup     TYPE strhi_popup,
          ls_selection TYPE trwbo_selection.

    ls_popup-start_column = 5.
    ls_popup-start_row    = 5.

    " Prepare the selection
    ls_selection-trkorrpattern = space.
    ls_selection-client        = space.
    ls_selection-stdrequest    = space.
    ls_selection-reqfunctions  = 'K'.
    ls_selection-reqstatus     = 'RNODL'.

    " Call transport selection popup
    CALL FUNCTION 'TRINT_SELECT_REQUESTS'
      EXPORTING
        iv_username_pattern    = '*'
        iv_via_selscreen       = 'X'
        is_selection           = ls_selection
        iv_complete_projects   = space
        iv_title               = 'ABAPGit Transport Mass Downloader'
        is_popup               = ls_popup
      IMPORTING
        et_requests            = rt_trkorr
      EXCEPTIONS
        action_aborted_by_user = 1
        OTHERS                 = 2.
    IF sy-subrc <> 0.
      CLEAR rt_trkorr.
    ELSE.
      SORT rt_trkorr BY trkorr.
      DELETE ADJACENT DUPLICATES FROM rt_trkorr COMPARING trkorr.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYCIJCOGSFY DEFINITION FINAL.
  PUBLIC SECTION.
    TYPES ty_folder TYPE string.
    TYPES ty_filename TYPE string.

    CONSTANTS c_zip_ext TYPE string VALUE '.zip'.

    METHODS constructor
      IMPORTING
        iv_folder TYPE ty_folder
      RAISING
        Lcx_abapgit_exception.

    METHODS generate_files
      IMPORTING
        it_trkorr TYPE trwbo_request_headers
        ig_logic  TYPE any
      RAISING
        Lcx_abapgit_exception.

    METHODS get_folder
      RETURNING
        VALUE(rv_full_folder) TYPE ty_folder.

    CLASS-METHODS does_folder_exist
      IMPORTING
        iv_folder              TYPE string
      RETURNING
        VALUE(rv_folder_exist) TYPE abap_bool
      RAISING
        Lcx_abapgit_exception.

  PRIVATE SECTION.
    DATA: mv_timestamp   TYPE string,
          mv_separator   TYPE c,
          mv_full_folder TYPE ty_folder.

    METHODS get_full_folder
      IMPORTING
        iv_folder             TYPE ty_folder
      RETURNING
        VALUE(rv_full_folder) TYPE ty_folder
      RAISING
        Lcx_abapgit_exception.

    METHODS get_filename
      IMPORTING
        is_trkorr          TYPE trwbo_request_header
      RETURNING
        VALUE(rv_filename) TYPE ty_filename.

ENDCLASS.

CLASS SHRITEFUH64VYIPO5IWUYCIJCOGSFY IMPLEMENTATION.

  METHOD constructor.
    DATA lo_fe_serv TYPE REF TO Lif_abapgit_frontend_services.

    lo_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    mv_timestamp = |{ sy-datlo }_{ sy-timlo }|.
    mv_full_folder = get_full_folder( iv_folder ).

    TRY.
        lo_fe_serv->get_file_separator( CHANGING cv_file_separator = mv_separator ).
      CATCH Lcx_abapgit_exception.
        "Default MS Windows separator
        mv_separator = '\'.
    ENDTRY.
  ENDMETHOD.

  METHOD get_folder.
    rv_full_folder = mv_full_folder.
  ENDMETHOD.

  METHOD does_folder_exist.
    rv_folder_exist = Lcl_abapgit_ui_factory=>get_frontend_services( )->directory_exist( iv_folder ).
  ENDMETHOD.

  METHOD get_full_folder.

    DATA: lv_sep     TYPE c,
          lv_rc      TYPE i,
          lo_fe_serv TYPE REF TO Lif_abapgit_frontend_services.

    lo_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lo_fe_serv->get_file_separator( CHANGING cv_file_separator = lv_sep ).
    rv_full_folder = |{ iv_folder }{ lv_sep }{ mv_timestamp }|.

    IF does_folder_exist( rv_full_folder ) = abap_false.
      lo_fe_serv->directory_create(
        EXPORTING
          iv_directory = rv_full_folder
        CHANGING
          cv_rc        = lv_rc ).
    ENDIF.
  ENDMETHOD.

  METHOD get_filename.

    " Generate filename
    rv_filename = |{ is_trkorr-trkorr }_{ is_trkorr-as4text }_{ mv_timestamp }{ c_zip_ext }|.

    " Remove reserved characters (for Windows based systems)
    TRANSLATE rv_filename USING '/ \ : " * > < ? | '.

    rv_filename = |{ mv_full_folder }{ mv_separator }{ rv_filename }|.

  ENDMETHOD.

  METHOD generate_files.

    DATA: ls_trkorr       LIKE LINE OF it_trkorr,
          lv_zipbinstring TYPE xstring.

    LOOP AT it_trkorr INTO ls_trkorr.

      lv_zipbinstring = Lcl_abapgit_transport_mass=>zip( is_trkorr         = ls_trkorr
                                                         iv_logic          = ig_logic
                                                         iv_show_log_popup = abap_false ).

      Lcl_abapgit_zip=>save_binstring_to_localfile( iv_binstring = lv_zipbinstring
                                                    iv_filename  = get_filename( ls_trkorr ) ).

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

class LCL_ABAPGIT_TRANSPORT_MASS implementation.
*"* method's implementations
*include methods.
  METHOD run.

    DATA:
      lt_trkorr           TYPE trwbo_request_headers,
      lo_transport_zipper TYPE REF TO SHRITEFUH64VYIPO5IWUYCIJCOGSFY,
      lx_except           TYPE REF TO cx_root,
      lv_folder           TYPE string,
      lv_text             TYPE string.

    TRY.

        lt_trkorr = SHRITEFUH64VYIPO5IWUYCIJCOESFY=>select_tr_requests( ).

        IF lt_trkorr[] IS NOT INITIAL.

          lv_folder = SHRITEFUH64VYIPO5IWUYCIJCOESFY=>f4_folder( ).

          IF lv_folder IS INITIAL.
* Empty folder
            Lcx_abapgit_exception=>raise( 'Empty destination folder' ).
          ENDIF.

* Instantiate transport zipper object that will also create the timestamped output folder
          CREATE OBJECT lo_transport_zipper TYPE SHRITEFUH64VYIPO5IWUYCIJCOGSFY
            EXPORTING
              iv_folder = lv_folder.

* Generate the local zip files from the given list of transport requests
          lo_transport_zipper->generate_files(
            it_trkorr = lt_trkorr
            ig_logic  = Lcl_abapgit_ui_factory=>get_popups( )->popup_folder_logic( ) ).

* Open output folder if user asked it
          SHRITEFUH64VYIPO5IWUYCIJCOESFY=>open_folder_frontend( lo_transport_zipper->get_folder( ) ).

        ELSE.
* No data found for the provided selection criterias
          Lcx_abapgit_exception=>raise( 'No transport requests selected' ).
        ENDIF.

      CATCH Lcx_abapgit_exception INTO lx_except.

        lv_text = lx_except->get_text( ).
        MESSAGE lv_text TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_TRANSPORT_MASS implementation

*>>>>>>> ZCL_ABAPGIT_UTILS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_utils=============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_utils=============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_utils=============ccau.



class LCL_ABAPGIT_UTILS implementation.
*"* method's implementations
*include methods.
  METHOD is_binary.

    " Previously we did a simple char range test described here
    " stackoverflow.com/questions/277521/how-to-identify-the-file-content-as-ascii-or-binary
    " but this is insufficient if the data contains german umlauts and other special characters.
    " Therefore we adopted another algorithm, which is similarily used by AL11
    " RSWATCH0 / GUESS_FILE_TYPE
    " We count non-printable characters if there are more than XX% it's binary.

    CONSTANTS:
      lc_binary_threshold TYPE i VALUE 10,
      lc_bytes_to_check   TYPE i VALUE 1000.

    DATA: lv_string_data           TYPE string,
          lv_printable_chars_count TYPE i,
          lv_percentage            TYPE i,
          lv_data                  TYPE xstring,
          lv_xlen                  TYPE i.

    lv_xlen = xstrlen( iv_data ).
    IF lv_xlen = 0.
      RETURN.
    ENDIF.

    lv_xlen = nmin(
                val1 = lv_xlen
                val2 = lc_bytes_to_check ).

    lv_data = iv_data(lv_xlen).

    TRY.
        lv_string_data = Lcl_abapgit_convert=>xstring_to_string_utf8( lv_data ).
      CATCH Lcx_abapgit_exception.
        " Contains data that does not convert to UTF-8 so consider it binary
        rv_is_binary = abap_true.
        RETURN.
    ENDTRY.

    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN lv_string_data WITH space.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_string_data WITH space.

    FIND ALL OCCURRENCES OF REGEX '[^[:print:]]' IN lv_string_data MATCH COUNT lv_printable_chars_count.
    lv_percentage = lv_printable_chars_count * 100 / strlen( lv_string_data ).
    rv_is_binary = boolc( lv_percentage > lc_binary_threshold ).

  ENDMETHOD.
  METHOD is_valid_email.

    " Email address validation (RFC 5322)
    " https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/ch04s01.html
    CONSTANTS lc_email_regex TYPE string VALUE
      '[\w!#$%&*+/=?`{|}~^-]+(?:\.[\w!#$%&*+/=?`{|}~^-]+)*@(?:[A-Za-z0-9-]+\.)+[A-Za-z]{2,6}'.

    IF iv_email IS INITIAL.
      rv_valid = abap_true.
    ELSE.
      FIND REGEX lc_email_regex IN iv_email.
      rv_valid = boolc( sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_UTILS implementation

*>>>>>>> ZCL_ABAPGIT_XML <<<<<<<*

*"* macro definitions
*include zcl_abapgit_xml===============ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_xml===============ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_xml===============ccau.






class LCL_ABAPGIT_XML implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    mi_ixml     = cl_ixml=>create( ).
    mi_xml_doc  = mi_ixml->create_document( ).
    mv_filename = iv_filename.
  ENDMETHOD.
  METHOD error.

    IF ii_parser->num_errors( ) <> 0.
      raise_exception_for( ii_parser->get_error( 0 ) ).
    ENDIF.

    IF mv_filename IS INITIAL.
      Lcx_abapgit_exception=>raise( |Error while parsing XML| ).
    ELSE.
      Lcx_abapgit_exception=>raise( |Error while parsing XML file { mv_filename }| ).
    ENDIF.

  ENDMETHOD.
  METHOD parse.

    DATA: li_stream_factory TYPE REF TO if_ixml_stream_factory,
          li_istream        TYPE REF TO if_ixml_istream,
          li_element        TYPE REF TO if_ixml_element,
          li_version        TYPE REF TO if_ixml_node,
          li_parser         TYPE REF TO if_ixml_parser.


    ASSERT NOT iv_xml IS INITIAL.

    li_stream_factory = mi_ixml->create_stream_factory( ).
    li_istream = li_stream_factory->create_istream_string( iv_xml ).
    li_parser = mi_ixml->create_parser( stream_factory = li_stream_factory
                                        istream        = li_istream
                                        document       = mi_xml_doc ).
    li_parser->add_strip_space_element( ).
    IF li_parser->parse( ) <> 0.
      error( li_parser ).
    ENDIF.

    li_istream->close( ).


    li_element = mi_xml_doc->find_from_name_ns( depth = 0
                                                name = c_abapgit_tag ).
    li_version = li_element->if_ixml_node~get_attributes(
      )->get_named_item_ns( c_attr_version ).
    IF li_version->get_value( ) <> Lif_abapgit_version=>c_xml_version.
      raise_version_mismatch( li_version->get_value( ) ).
    ENDIF.

* buffer serializer metadata. Git node will be removed lateron
    ms_metadata-class   = li_element->get_attribute_ns( c_attr_serializer ).
    ms_metadata-version = li_element->get_attribute_ns( c_attr_serializer_version ).

  ENDMETHOD.
  METHOD raise_exception_for.
    DATA lv_message TYPE string.

    lv_message = |XML parser error: { ii_error->get_reason( ) }, | &&
                 |Line { ii_error->get_line( ) } | &&
                 |Col. { ii_error->get_column( ) }|.

    IF mv_filename IS NOT INITIAL.
      lv_message = lv_message && | File { mv_filename }|.
    ENDIF.

    Lcx_abapgit_exception=>raise( lv_message ).

  ENDMETHOD.
  METHOD raise_version_mismatch.

    DATA lv_text TYPE string.

    lv_text = |The XML versions do not match, expected: { Lif_abapgit_version=>c_xml_version }, actual: { iv_vers }|.

    IF mv_filename IS NOT INITIAL.
      lv_text = lv_text && |, file: { mv_filename }|.
    ENDIF.

    lv_text = lv_text && | (see https://docs.abapgit.org/other-xml-mismatch.html)|.

    Lcx_abapgit_exception=>raise( lv_text ).

  ENDMETHOD.
  METHOD to_xml.
* will render to codepage UTF-16

    DATA: li_ostream       TYPE REF TO if_ixml_ostream,
          li_renderer      TYPE REF TO if_ixml_renderer,
          li_streamfactory TYPE REF TO if_ixml_stream_factory.

    li_streamfactory = mi_ixml->create_stream_factory( ).

    li_ostream = li_streamfactory->create_ostream_cstring( rv_xml ).

    li_renderer = mi_ixml->create_renderer( ostream  = li_ostream
                                            document = mi_xml_doc ).
    li_renderer->set_normalizing( iv_normalize ).

    li_renderer->render( ).

    " handling of BOM moved to zcl_abapgit_convert=>string_to_xstring_utf8_bom

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_XML implementation

*>>>>>>> ZCL_ABAPGIT_XML_INPUT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_xml_input=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_xml_input=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_xml_input=========ccau.



class LCL_ABAPGIT_XML_INPUT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( iv_filename ).
    parse( iv_xml ).
    fix_xml( ).

  ENDMETHOD.
  METHOD fix_xml.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_node.


    li_git ?= mi_xml_doc->find_from_name_ns( depth = 0
                                             name = c_abapgit_tag ).
    li_abap = li_git->get_first_child( ).

    mi_xml_doc->get_root( )->remove_child( li_git ).
    mi_xml_doc->get_root( )->append_child( li_abap ).

  ENDMETHOD.
  METHOD Lif_abapgit_xml_input~get_metadata.
    rs_metadata = ms_metadata.
  ENDMETHOD.
  METHOD Lif_abapgit_xml_input~get_raw.
    ri_raw = mi_xml_doc.
  ENDMETHOD.
  METHOD Lif_abapgit_xml_input~read.

    DATA: lx_error TYPE REF TO cx_transformation_error,
          lt_rtab  TYPE abap_trans_resbind_tab.

    FIELD-SYMBOLS: <ls_rtab> LIKE LINE OF lt_rtab.

    ASSERT NOT iv_name IS INITIAL.

    CLEAR cg_data. "Initialize result to avoid problems with empty values

    APPEND INITIAL LINE TO lt_rtab ASSIGNING <ls_rtab>.
    <ls_rtab>-name = iv_name.
    GET REFERENCE OF cg_data INTO <ls_rtab>-value.

    TRY.
        CALL TRANSFORMATION id
          OPTIONS value_handling = 'accept_data_loss'
          SOURCE XML mi_xml_doc
          RESULT (lt_rtab).
      CATCH cx_transformation_error INTO lx_error.
        IF mv_filename IS INITIAL.
          Lcx_abapgit_exception=>raise( lx_error->if_message~get_text( ) ).
        ELSE.
          Lcx_abapgit_exception=>raise( |File { mv_filename }: { lx_error->if_message~get_text( ) }| ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_XML_INPUT implementation

*>>>>>>> ZCL_ABAPGIT_XML_OUTPUT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_xml_output========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_xml_output========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_xml_output========ccau.
*CLASS SHRITEFUH64VYIPO5IWUYCIJCPASFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_xml_output DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCPASFY.


class LCL_ABAPGIT_XML_OUTPUT implementation.
*"* method's implementations
*include methods.
  METHOD build_asx_node.

    DATA: li_attr TYPE REF TO if_ixml_attribute.


    ri_element = mi_xml_doc->create_element_ns(
      name   = 'abap'
      prefix = 'asx' ).

    li_attr = mi_xml_doc->create_attribute_ns( 'version' ).
    li_attr->if_ixml_node~set_value( '1.0' ).
    ri_element->set_attribute_node_ns( li_attr ).

    li_attr = mi_xml_doc->create_attribute_ns(
      name   = 'asx'
      prefix = 'xmlns' ).
    li_attr->if_ixml_node~set_value( 'http://www.sap.com/abapxml' ).
    ri_element->set_attribute_node_ns( li_attr ).

  ENDMETHOD.
  METHOD Lif_abapgit_xml_output~add.

    DATA: li_node TYPE REF TO if_ixml_node,
          li_doc  TYPE REF TO if_ixml_document,
          lt_stab TYPE abap_trans_srcbind_tab.

    FIELD-SYMBOLS: <ls_stab> LIKE LINE OF lt_stab.


    ASSERT NOT iv_name IS INITIAL.

    IF ig_data IS INITIAL.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lt_stab ASSIGNING <ls_stab>.
    <ls_stab>-name = iv_name.
    GET REFERENCE OF ig_data INTO <ls_stab>-value.

    li_doc = cl_ixml=>create( )->create_document( ).

    CALL TRANSFORMATION id
      OPTIONS initial_components = 'suppress'
      SOURCE (lt_stab)
      RESULT XML li_doc.

    li_node = mi_xml_doc->get_root( )->get_first_child( ).
    IF li_node IS BOUND.
      mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child(
        li_doc->get_root( )->get_first_child( )->get_first_child( )->get_first_child( ) ).
    ELSE.
      mi_xml_doc->get_root( )->append_child( li_doc->get_root( )->get_first_child( ) ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_xml_output~add_xml.

    DATA: li_element TYPE REF TO if_ixml_element.

    li_element = mi_xml_doc->create_element( iv_name ).
    li_element->append_child( ii_xml ).

    mi_xml_doc->get_root( )->get_first_child( )->get_first_child( )->append_child( li_element ).

  ENDMETHOD.
  METHOD Lif_abapgit_xml_output~render.

    DATA: li_git  TYPE REF TO if_ixml_element,
          li_abap TYPE REF TO if_ixml_element.


    IF mi_raw IS INITIAL.
      li_abap ?= mi_xml_doc->get_root( )->get_first_child( ).
      mi_xml_doc->get_root( )->remove_child( li_abap ).
      IF li_abap IS INITIAL.
        li_abap = build_asx_node( ).
      ENDIF.
    ELSE.
      li_abap = mi_raw.
    ENDIF.

    li_git = mi_xml_doc->create_element( c_abapgit_tag ).
    li_git->set_attribute( name = c_attr_version
                           value = Lif_abapgit_version=>c_xml_version ).
    IF NOT is_metadata IS INITIAL.
      li_git->set_attribute( name  = c_attr_serializer
                             value = is_metadata-class ).
      li_git->set_attribute( name  = c_attr_serializer_version
                             value = is_metadata-version ).
    ENDIF.
    li_git->append_child( li_abap ).
    mi_xml_doc->get_root( )->append_child( li_git ).

    rv_xml = to_xml( iv_normalize ).

  ENDMETHOD.
  METHOD Lif_abapgit_xml_output~set_raw.
    mi_raw = ii_raw.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_XML_OUTPUT implementation

*>>>>>>> ZCX_ABAPGIT_EXCEPTION <<<<<<<*

*"* macro definitions
*include zcx_abapgit_exception=========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcx_abapgit_exception=========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcx_abapgit_exception=========ccau.
*CLASS SHRITEFUH64VYIPO5IWUYCIJCPOSFY DEFINITION DEFERRED.
*CLASS SHRITEFUH64VYIPO5IWUYCIJCPPSFY DEFINITION DEFERRED.
*CLASS zcx_abapgit_exception DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYCIJCPOSFY SHRITEFUH64VYIPO5IWUYCIJCPPSFY.













class LCX_ABAPGIT_EXCEPTION implementation.
*"* method's implementations
*include methods.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.

    super->constructor( previous = previous ).

    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    mi_log = log.
    mv_longtext = longtext.

    CLEAR me->textid.

    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    save_callstack( ).

  ENDMETHOD.
  METHOD get_source_position.

    FIELD-SYMBOLS: <ls_callstack> LIKE LINE OF mt_callstack.

    READ TABLE mt_callstack ASSIGNING <ls_callstack>
                            INDEX 1.
    IF sy-subrc = 0.
      program_name = <ls_callstack>-mainprogram.
      include_name = <ls_callstack>-include.
      source_line  = <ls_callstack>-line.
    ELSE.
      super->get_source_position(
        IMPORTING
          program_name = program_name
          include_name = include_name
          source_line  = source_line ).
    ENDIF.

  ENDMETHOD.
  METHOD get_t100_longtext_itf.

    DATA: lv_docu_key TYPE doku_obj.

    FIELD-SYMBOLS <lv_msgv> TYPE any.

    lv_docu_key = if_t100_message~t100key-msgid && if_t100_message~t100key-msgno.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id     = 'NA'
        langu  = sy-langu
        object = lv_docu_key
        typ    = 'E'
      TABLES
        line   = rt_itf
      EXCEPTIONS
        OTHERS = 1.

    IF sy-subrc = 0.
      ASSIGN me->(if_t100_message~t100key-attr1) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V1&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr2) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V2&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr3) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V3&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
      ASSIGN me->(if_t100_message~t100key-attr4) TO <lv_msgv>.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '&V4&' IN TABLE rt_itf WITH <lv_msgv>.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD if_message~get_longtext.
    DATA: lv_preserve_newlines_handled TYPE abap_bool VALUE abap_false.

    IF mv_longtext IS NOT INITIAL.
      result = mv_longtext.
    ELSEIF if_t100_message~t100key IS NOT INITIAL.
      result = itf_to_string( get_t100_longtext_itf( ) ).
    ELSE.
      result = super->get_longtext( preserve_newlines ).
      lv_preserve_newlines_handled = abap_true.
    ENDIF.

    IF lv_preserve_newlines_handled = abap_false AND preserve_newlines = abap_false.
      result = remove_newlines_from_string( result ).
    ENDIF.
  ENDMETHOD.
  METHOD itf_to_string.

    CONSTANTS: lc_format_section TYPE string VALUE 'U1'.

    DATA:
      lt_stream      TYPE TABLE OF tdline,
      lt_string      TYPE TABLE OF string,
      lv_string      LIKE LINE OF lt_string,
      lt_itf         TYPE tline_tab,
      lv_has_content TYPE abap_bool,
      lv_tabix_from  TYPE syst-tabix,
      lv_tabix_to    TYPE syst-tabix.

    FIELD-SYMBOLS: <ls_itf_section>      TYPE tline,
                   <ls_itf_section_item> TYPE tline.

    lt_itf = it_itf.

    " You should remember that we replace the U1 format because
    " that preserves the section header of longtexts.
    LOOP AT lt_itf ASSIGNING <ls_itf_section>
                   WHERE tdformat = lc_format_section.

      CLEAR:
        lv_has_content,
        lv_tabix_to.

      lv_tabix_from = sy-tabix.

      LOOP AT lt_itf ASSIGNING <ls_itf_section_item>
                     FROM sy-tabix + 1.

        IF <ls_itf_section_item>-tdformat = lc_format_section.
          lv_tabix_to = sy-tabix.
          EXIT.
        ELSEIF <ls_itf_section_item>-tdline IS NOT INITIAL.
          lv_has_content = abap_true.
        ENDIF.

      ENDLOOP.

      IF lv_has_content = abap_false.
        remove_empty_section(
          EXPORTING
            iv_tabix_from = lv_tabix_from
            iv_tabix_to   = lv_tabix_to
          CHANGING
            ct_itf        = lt_itf ).
        CONTINUE.
      ENDIF.

      replace_section_head_with_text( CHANGING cs_itf = <ls_itf_section> ).

    ENDLOOP.

    CALL FUNCTION 'CONVERT_ITF_TO_STREAM_TEXT'
      EXPORTING
        lf           = 'X'
      IMPORTING
        stream_lines = lt_string
      TABLES
        itf_text     = lt_itf
        text_stream  = lt_stream.

    LOOP AT lt_string INTO lv_string.
      IF sy-tabix = 1.
        rv_result = lv_string.
      ELSE.
        CONCATENATE rv_result lv_string
                    INTO rv_result
                    SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD raise.

    DATA lv_text TYPE string.

    IF iv_text IS INITIAL.
      lv_text = c_generic_error_msg.
    ELSE.
      lv_text = iv_text.
    ENDIF.

    split_text_to_symsg( lv_text ).

    raise_t100(
      ii_log      = ii_log
      ix_previous = ix_previous
      iv_longtext = iv_longtext ).

  ENDMETHOD.
  METHOD raise_t100.
    DATA: ls_t100_key TYPE scx_t100key.

    ls_t100_key-msgid = iv_msgid.
    ls_t100_key-msgno = iv_msgno.
    ls_t100_key-attr1 = 'MSGV1'.
    ls_t100_key-attr2 = 'MSGV2'.
    ls_t100_key-attr3 = 'MSGV3'.
    ls_t100_key-attr4 = 'MSGV4'.

    IF iv_msgid IS INITIAL.
      CLEAR ls_t100_key.
    ENDIF.

    RAISE EXCEPTION TYPE Lcx_abapgit_exception
      EXPORTING
        textid   = ls_t100_key
        log      = ii_log
        msgv1    = iv_msgv1
        msgv2    = iv_msgv2
        msgv3    = iv_msgv3
        msgv4    = iv_msgv4
        previous = ix_previous
        longtext = iv_longtext.
  ENDMETHOD.
  METHOD raise_with_text.
    raise(
      iv_text     = ix_previous->get_text( )
      ix_previous = ix_previous
      iv_longtext = iv_longtext ).
  ENDMETHOD.
  METHOD remove_empty_section.
    IF iv_tabix_to BETWEEN iv_tabix_from AND lines( ct_itf ).
      DELETE ct_itf FROM iv_tabix_from TO iv_tabix_to.
    ELSE.
      DELETE ct_itf FROM iv_tabix_from.
    ENDIF.
  ENDMETHOD.
  METHOD remove_newlines_from_string.
    rv_result = iv_string.

    REPLACE ALL OCCURRENCES OF ` ` && cl_abap_char_utilities=>cr_lf IN rv_result WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_result WITH ` `.
    REPLACE ALL OCCURRENCES OF ` ` && cl_abap_char_utilities=>newline IN rv_result WITH ` `.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_result WITH ` `.
  ENDMETHOD.
  METHOD replace_section_head_with_text.

    CASE cs_itf-tdline.
      WHEN c_section_token-cause.
        cs_itf-tdline = c_section_text-cause.
      WHEN c_section_token-system_response.
        cs_itf-tdline = c_section_text-system_response.
      WHEN c_section_token-what_to_do.
        cs_itf-tdline = c_section_text-what_to_do.
      WHEN c_section_token-sys_admin.
        cs_itf-tdline = c_section_text-sys_admin.
    ENDCASE.

  ENDMETHOD.
  METHOD save_callstack.

    FIELD-SYMBOLS: <ls_callstack> LIKE LINE OF mt_callstack.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      IMPORTING
        callstack = mt_callstack.

    " You should remember that the first lines are from zcx_abapgit_exception
    " and are removed so that highest level in the callstack is the position where
    " the exception is raised.
    "
    " For the merged report it's hard to do that, because zcx_abapgit_exception
    " isn't visible in the callstack. Therefore we have to check the Events.
    LOOP AT mt_callstack ASSIGNING <ls_callstack>.

      IF <ls_callstack>-mainprogram CP |ZCX_ABAPGIT_EXCEPTION*| " full
      OR <ls_callstack>-blockname = `SAVE_CALLSTACK` " merged
      OR <ls_callstack>-blockname = `CONSTRUCTOR` " merged
      OR <ls_callstack>-blockname CP `RAISE*`. "merged
        DELETE TABLE mt_callstack FROM <ls_callstack>.
      ELSE.
        EXIT.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD split_text_to_symsg.

    DATA ls_msg TYPE symsg.

    cl_message_helper=>set_msg_vars_for_clike( iv_text ).
    ls_msg-msgv1 = sy-msgv1.
    ls_msg-msgv2 = sy-msgv2.
    ls_msg-msgv3 = sy-msgv3.
    ls_msg-msgv4 = sy-msgv4.

    " Set syst using generic error message
    MESSAGE e001(00) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3 ls_msg-msgv4 INTO null.

    rs_msg = ls_msg.

  ENDMETHOD.
endclass. "ZCX_ABAPGIT_EXCEPTION implementation

*>>>>>>> ZCL_ABAPGIT_FEATURE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_feature===========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_feature===========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_feature===========ccau.


class LCL_ABAPGIT_FEATURE implementation.
*"* method's implementations
*include methods.
  METHOD is_enabled.

    DATA:
      lv_features TYPE string,
      lt_features TYPE string_table.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      RETURN.
    ENDIF.

    lv_features = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_experimental_features( ).
    CONDENSE lv_features NO-GAPS.

    rv_run = boolc( lv_features = abap_true ).

    IF iv_feature IS NOT INITIAL.
      SPLIT lv_features AT ',' INTO TABLE lt_features.
      READ TABLE lt_features TRANSPORTING NO FIELDS WITH TABLE KEY table_line = iv_feature.
      rv_run = boolc( rv_run = abap_true OR sy-subrc = 0 ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_FEATURE implementation

*>>>>>>> ZCL_ABAPGIT_GITV2_PORCELAIN <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gitv2_porcelain===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gitv2_porcelain===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GITV2_PORCELAIN implementation.
*"* method's implementations
*include methods.
  METHOD decode_pack.

    DATA lv_xstring TYPE xstring.
    DATA lv_contents  TYPE xstring.
    DATA lv_pack      TYPE xstring.
    DATA lv_pktlen    TYPE i.
    DATA lv_hex4      TYPE xstring.

    lv_xstring = iv_xstring.

* The data transfer of the packfile is always multiplexed, using the same semantics of the
* side-band-64k capability from protocol version 1
    WHILE xstrlen( lv_xstring ) > 0.
      lv_hex4 = lv_xstring(4).
      lv_pktlen = Lcl_abapgit_git_utils=>length_utf8_hex( lv_hex4 ).
      IF lv_pktlen = 0.
        EXIT.
      ELSEIF lv_pktlen = 1.
* its a delimiter package
        lv_xstring = lv_xstring+4.
        CONTINUE.
      ENDIF.
      lv_contents = lv_xstring(lv_pktlen).
      IF lv_contents+4(1) = '01'.
        CONCATENATE lv_pack lv_contents+5 INTO lv_pack IN BYTE MODE.
      ENDIF.
      lv_xstring = lv_xstring+lv_pktlen.
    ENDWHILE.

    rt_objects = Lcl_abapgit_git_pack=>decode( lv_pack ).

  ENDMETHOD.
  METHOD send_command.

    CONSTANTS lc_content_regex TYPE string VALUE '^[0-9a-f]{4}#'.

    DATA lo_client   TYPE REF TO Lcl_abapgit_http_client.
    DATA lv_cmd_pkt  TYPE string.
    DATA lt_headers  TYPE Lcl_abapgit_http=>ty_headers.
    DATA ls_header   LIKE LINE OF lt_headers.
    DATA lv_argument TYPE string.


    ls_header-key = 'Git-Protocol'.
    ls_header-value = 'version=2'.
    APPEND ls_header TO lt_headers.

    lo_client = Lcl_abapgit_http=>create_by_url(
      iv_url     = iv_url
      iv_service = c_service-upload
      it_headers = lt_headers ).

    lo_client->check_smart_response(
      iv_expected_content_type = |application/x-git-{ iv_service }-pack-advertisement|
      iv_content_regex         = lc_content_regex ).

    lv_cmd_pkt = Lcl_abapgit_git_utils=>pkt_string( |command={ iv_command }\n| )
      && Lcl_abapgit_git_utils=>pkt_string( |agent={ Lcl_abapgit_http=>get_agent( ) }\n| ).
    IF lines( it_arguments ) > 0.
      lv_cmd_pkt = lv_cmd_pkt && c_delim_pkt.
      LOOP AT it_arguments INTO lv_argument.
        lv_cmd_pkt = lv_cmd_pkt && Lcl_abapgit_git_utils=>pkt_string( lv_argument ).
      ENDLOOP.
    ENDIF.
    lv_cmd_pkt = lv_cmd_pkt && c_flush_pkt.

    lo_client->set_header(
      iv_key   = '~request_uri'
      iv_value = Lcl_abapgit_url=>path_name( iv_url ) && |/git-{ iv_service }-pack| ).

    lo_client->set_header(
      iv_key   = '~request_method'
      iv_value = 'POST' ).

    lo_client->set_header(
      iv_key   = 'Content-Type'
      iv_value = |application/x-git-{ iv_service }-pack-request| ).

    lo_client->set_header(
      iv_key   = 'Accept'
      iv_value = |application/x-git-{ iv_service }-pack-result| ).

    rv_response = lo_client->send_receive_close( Lcl_abapgit_convert=>string_to_xstring_utf8( lv_cmd_pkt ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_gitv2_porcelain~commits_last_year.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    lv_argument = |deepen-since { Lcl_abapgit_git_time=>get_one_year_ago( ) }|.
    APPEND lv_argument TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
* 'filter object:type=commit' doesnt work on github
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).
    DELETE rt_objects WHERE type <> Lif_abapgit_git_definitions=>c_type-commit.

  ENDMETHOD.
  METHOD Lif_abapgit_gitv2_porcelain~list_branches.
    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_data      TYPE string.

    IF iv_prefix IS NOT INITIAL.
      lv_argument = |ref-prefix { iv_prefix }|.
      APPEND lv_argument TO lt_arguments.
    ENDIF.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |ls-refs|
      it_arguments = lt_arguments ).

    " add dummy packet so the v1 branch parsing can be reused
    lv_data = |0004\n{ Lcl_abapgit_convert=>xstring_to_string_utf8( lv_xstring ) }|.

    CREATE OBJECT ro_list
      EXPORTING
        iv_data = lv_data.

  ENDMETHOD.
  METHOD Lif_abapgit_gitv2_porcelain~list_no_blobs.

    DATA lt_sha1    TYPE Lif_abapgit_git_definitions=>ty_sha1_tt.
    DATA lt_objects TYPE Lif_abapgit_definitions=>ty_objects_tt.

    ASSERT iv_sha1 IS NOT INITIAL.
    APPEND iv_sha1 TO lt_sha1.

    lt_objects = Lif_abapgit_gitv2_porcelain~list_no_blobs_multi(
      iv_url  = iv_url
      it_sha1 = lt_sha1 ).

    rt_expanded = Lcl_abapgit_git_porcelain=>full_tree(
      it_objects = lt_objects
      iv_parent  = iv_sha1 ).

  ENDMETHOD.
  METHOD Lif_abapgit_gitv2_porcelain~list_no_blobs_multi.

    DATA lv_xstring   TYPE xstring.
    DATA lt_arguments TYPE string_table.
    DATA lv_argument  TYPE string.
    DATA lv_sha1      LIKE LINE OF it_sha1.


    ASSERT lines( it_sha1 ) > 0.

    APPEND 'deepen 1' TO lt_arguments.
    LOOP AT it_sha1 INTO lv_sha1.
      lv_argument = |want { lv_sha1 }|.
      APPEND lv_argument TO lt_arguments.
    ENDLOOP.
    APPEND 'filter blob:none' TO lt_arguments.
    APPEND 'no-progress' TO lt_arguments.
    APPEND 'done' TO lt_arguments.

    lv_xstring = send_command(
      iv_url       = iv_url
      iv_service   = c_service-upload
      iv_command   = |fetch|
      it_arguments = lt_arguments ).

    rt_objects = decode_pack( lv_xstring ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GITV2_PORCELAIN implementation

*>>>>>>> ZCL_ABAPGIT_GIT_FACTORY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_factory=======ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_factory=======ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GIT_FACTORY implementation.
*"* method's implementations
*include methods.
  METHOD get_v2_porcelain.
    CREATE OBJECT ri_v2 TYPE Lcl_abapgit_gitv2_porcelain.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_FACTORY implementation

*>>>>>>> ZCL_ABAPGIT_GIT_TIME <<<<<<<*

*"* macro definitions
*include zcl_abapgit_git_time==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_git_time==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_git_time==========ccau.



class LCL_ABAPGIT_GIT_TIME implementation.
*"* method's implementations
*include methods.
  METHOD get_unix.
* returns seconds since unix epoch, including timezone indicator

    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_time TYPE timestamp.
    DATA lv_seconds TYPE i.

    GET TIME STAMP FIELD lv_time.

    lv_seconds = cl_abap_tstmp=>subtract(
      tstmp1 = lv_time
      tstmp2 = lc_epoch ).

    rv_time = lv_seconds.
    CONDENSE rv_time.
    rv_time+11 = '+000000'.

  ENDMETHOD.
  METHOD get_utc.

    CONSTANTS lc_epoch TYPE d VALUE '19700101'.

    DATA: lv_i       TYPE i,
          lv_utcdiff TYPE t,
          lv_utcsign TYPE c LENGTH 1.


    lv_i = iv_unix(10).
    lv_utcsign = iv_unix+11.
    lv_utcdiff = iv_unix+12.

    " GMT + time-zone
    CASE lv_utcsign.
      WHEN '+'.
        lv_i = lv_i + lv_utcdiff.
      WHEN '-'.
        lv_i = lv_i - lv_utcdiff.
    ENDCASE.

    ev_time = lv_i MOD 86400.
    lv_i = lv_i - ev_time.
    lv_i = lv_i / 86400.
    ev_date = lv_i + lc_epoch.

  ENDMETHOD.
  METHOD get_one_year_ago.
* https://www.epochconverter.com
    CONSTANTS lc_epoch TYPE timestamp VALUE '19700101000000'.
    DATA lv_time TYPE timestamp.

    GET TIME STAMP FIELD lv_time.

    rv_time = cl_abap_tstmp=>subtract(
      tstmp1 = lv_time
      tstmp2 = lc_epoch ).

    rv_time = rv_time - 31536000.
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GIT_TIME implementation

*>>>>>>> ZCL_ABAPGIT_GUI_JUMPER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_jumper========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_jumper========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_JUMPER implementation.
*"* method's implementations
*include methods.
  METHOD Lif_abapgit_gui_jumper~jump_abapgit.

    DATA lt_spagpa        TYPE STANDARD TABLE OF rfc_spagpa.
    DATA ls_spagpa        LIKE LINE OF lt_spagpa.
    DATA lv_save_sy_langu TYPE sy-langu.
    DATA lv_subrc         TYPE syst-subrc.
    DATA lv_tcode         TYPE tcode.
    DATA lv_langu_text    TYPE string.
    DATA lv_msg           TYPE c LENGTH 200.

    " https://blogs.sap.com/2017/01/13/logon-language-sy-langu-and-rfc/

    lv_tcode = Lcl_abapgit_services_abapgit=>get_abapgit_tcode( ).

    lv_save_sy_langu = sy-langu.
    SET LOCALE LANGUAGE iv_language.

    ls_spagpa-parid  = Lif_abapgit_definitions=>c_spagpa_param_repo_key.
    ls_spagpa-parval = iv_key.
    INSERT ls_spagpa INTO TABLE lt_spagpa.

    CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
      DESTINATION 'NONE'
      STARTING NEW TASK 'ABAPGIT'
      EXPORTING
        tcode                 = lv_tcode
      TABLES
        spagpa_tab            = lt_spagpa
      EXCEPTIONS
        communication_failure = 1 MESSAGE lv_msg
        system_failure        = 2 MESSAGE lv_msg
        OTHERS                = 3.

    lv_subrc = sy-subrc.

    SET LOCALE LANGUAGE lv_save_sy_langu.

    CASE lv_subrc.
      WHEN 1.
        lv_msg = |Communication error { lv_msg }|.
      WHEN 2.
        SELECT SINGLE sptxt FROM t002t INTO lv_langu_text WHERE spras = sy-langu AND sprsl = iv_language.
        IF sy-subrc <> 0.
          lv_langu_text = iv_language.
        ENDIF.
        lv_msg = |Language { lv_langu_text } ({ Lcl_abapgit_convert=>language_sap1_to_sap2( iv_language ) })|
              && | is not installed|.
      WHEN 3.
        lv_msg = |{ lv_subrc }|.
    ENDCASE.

    IF lv_msg IS INITIAL.
      MESSAGE 'Repository opened in a new window' TYPE 'S'.
    ELSE.
      Lcx_abapgit_exception=>raise( |Error starting transaction { lv_tcode }: { lv_msg }| ).
    ENDIF.

  ENDMETHOD.
  METHOD jump_bw.

    DATA:
      lv_exit  TYPE abap_bool,
      lv_tlogo TYPE c LENGTH 4, "rstlogo
      lv_objnm TYPE c LENGTH 40. "rsawbnobjnm

    lv_tlogo = is_item-obj_type.
    lv_objnm = is_item-obj_name.

    TRY.
        CALL METHOD ('CL_RSAWBN_AWB')=>('IS_SUPPORTED_NAVIGATION')
          EXPORTING
            i_tlogo               = lv_tlogo
            i_fcode               = 'DISPLAY'
          IMPORTING
            re_is_supported_fcode = lv_exit.

        IF lv_exit = abap_false.
          RETURN.
        ENDIF.
      CATCH cx_root.
        " Not a BW system
        RETURN.
    ENDTRY.

    TRY.
        CALL METHOD ('CL_RSAWBN_AWB')=>('NAVIGATE_FROM_APPLICATION')
          EXPORTING
            i_tlogo                = lv_tlogo
            i_objnm                = lv_objnm
            i_new_mode             = iv_new_window
          IMPORTING
            e_exit_own_application = lv_exit.

      CATCH cx_root.
        " Older release without i_new_mode
        CALL METHOD ('CL_RSAWBN_AWB')=>('NAVIGATE_FROM_APPLICATION')
          EXPORTING
            i_tlogo                = lv_tlogo
            i_objnm                = lv_objnm
          IMPORTING
            e_exit_own_application = lv_exit.
    ENDTRY.

    rv_exit = lv_exit.

  ENDMETHOD.
  METHOD jump_tr.

    DATA:
      lv_e071_object   TYPE e071-object,
      lv_e071_obj_name TYPE e071-obj_name.

    lv_e071_object   = is_item-obj_type.
    lv_e071_obj_name = is_item-obj_name.

    CALL FUNCTION 'TR_OBJECT_JUMP_TO_TOOL'
      EXPORTING
        iv_action         = 'SHOW'
        iv_pgmid          = 'R3TR'
        iv_object         = lv_e071_object
        iv_obj_name       = lv_e071_obj_name
      EXCEPTIONS
        jump_not_possible = 1
        OTHERS            = 2.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD jump_wb.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = is_item-obj_name
        object_type         = is_item-obj_type
        devclass            = is_item-devclass
        in_new_window       = iv_new_window
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.

    rv_exit = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD jump_wb_line.

    IF iv_line_number IS NOT INITIAL AND iv_sub_obj_type IS NOT INITIAL AND iv_sub_obj_name IS NOT INITIAL.

      " For the line navigation we have to supply the sub object type (iv_sub_obj_type).
      " If we use is_item-obj_type it navigates only to the object.
      CALL FUNCTION 'RS_TOOL_ACCESS'
        EXPORTING
          operation           = 'SHOW'
          object_name         = is_item-obj_name
          object_type         = iv_sub_obj_type
          devclass            = is_item-devclass
          include             = iv_sub_obj_name
          position            = iv_line_number
          in_new_window       = iv_new_window
        EXCEPTIONS
          not_executed        = 1
          invalid_object_type = 2
          OTHERS              = 3.

      rv_exit = boolc( sy-subrc = 0 ).

    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_jumper~jump.

    " WebGUI cannot open windows or ADT
    IF Lcl_abapgit_ui_factory=>get_frontend_services( )->is_webgui( ) = abap_true.
      Lcx_abapgit_exception=>raise( |Jump not possible in WebGUI| ).
    ENDIF.

    " Try all generic jump options

    " 1) ADT Jump
    rv_exit = Lif_abapgit_gui_jumper~jump_adt(
      is_item         = is_item
      iv_sub_obj_name = is_sub_item-obj_name
      iv_line_number  = iv_line_number ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 2) WB Jump with Line Number
    rv_exit = jump_wb_line(
      is_item         = is_item
      iv_sub_obj_name = is_sub_item-obj_name
      iv_sub_obj_type = is_sub_item-obj_type
      iv_line_number  = iv_line_number
      iv_new_window   = iv_new_window ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 3) WB Jump without Line Number
    rv_exit = jump_wb(
      is_item       = is_item
      iv_new_window = iv_new_window ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 4) Transport Tool Jump
    rv_exit = jump_tr( is_item ).

    IF rv_exit = abap_true.
      RETURN.
    ENDIF.

    " 5) BW Jump
    rv_exit = jump_bw(
      is_item       = is_item
      iv_new_window = iv_new_window ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_jumper~jump_adt.

    " Open object in ADT (if enabled)

    DATA lv_adt_jump_enabled TYPE abap_bool.

    lv_adt_jump_enabled = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).

    IF lv_adt_jump_enabled = abap_true.
      TRY.
          Lcl_abapgit_adt_link=>jump(
            iv_obj_name     = is_item-obj_name
            iv_obj_type     = is_item-obj_type
            iv_sub_obj_name = iv_sub_obj_name
            iv_line_number  = iv_line_number ).

          rv_exit = abap_true.
        CATCH Lcx_abapgit_exception ##NO_HANDLER.
          " Use fallback
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_jumper~jump_batch_input.

    DATA lv_msg TYPE c LENGTH 80.

    IF iv_new_window = abap_true.
      CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
        STARTING NEW TASK 'GIT'
        EXPORTING
          tcode                 = iv_tcode
          mode_val              = 'E'
        TABLES
          using_tab             = it_bdcdata
        EXCEPTIONS
          system_failure        = 1 MESSAGE lv_msg
          communication_failure = 2 MESSAGE lv_msg
          resource_failure      = 3
          OTHERS                = 4.
    ELSE.
      CALL FUNCTION 'ABAP4_CALL_TRANSACTION'
        EXPORTING
          tcode     = iv_tcode
          mode_val  = 'E'
        TABLES
          using_tab = it_bdcdata
        EXCEPTIONS
          OTHERS    = 4.
    ENDIF.

    CASE sy-subrc.
      WHEN 1 OR 2.
        Lcx_abapgit_exception=>raise( |Batch input error for transaction { iv_tcode }: { lv_msg }| ).
      WHEN 3 OR 4.
        Lcx_abapgit_exception=>raise( |Batch input error for transaction { iv_tcode }| ).
    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_JUMPER implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page==========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page==========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).
    ms_control-page_layout = c_page_layout-centered.

  ENDMETHOD.
  METHOD footer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="footer">' ).
    ri_html->add( '<table class="w100"><tr>' ).

    ri_html->add( '<td class="w40 sponsor">' ).
    ri_html->add_a( iv_act = Lif_abapgit_definitions=>c_action-sponsor
                    iv_txt = ri_html->icon( iv_name = 'heart-regular/pink'
                                            iv_hint = 'Sponsor us' ) ).
    ri_html->add_a( iv_act = Lif_abapgit_definitions=>c_action-sponsor
                    iv_txt = 'Sponsor us' ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="center">' ).
    ri_html->add( '<div class="logo">' ).
    ri_html->add_a( iv_act = Lif_abapgit_definitions=>c_action-homepage
                    iv_txt = ri_html->icon( 'git-alt' ) ).
    ri_html->add_a( iv_act = Lif_abapgit_definitions=>c_action-homepage
                    iv_txt = ri_html->icon( iv_name = 'abapgit'
                                            iv_hint = iv_time ) ).
    ri_html->add( '</div>' ).
    ri_html->add( |<div id="footer-version" class="version">{ get_version_details( ) }</div>| ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td id="debug-output" class="w40"></td>' ).

    ri_html->add( '</tr></table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD header_script_links.

    ii_html->add( '<script src="js/common.js"></script>' ).

    IF ms_control-extra_js_url IS NOT INITIAL.
      ii_html->add( |<script src="{ ms_control-extra_js_url }"></script>| ).
    ENDIF.

  ENDMETHOD.
  METHOD header_stylesheet_links.

    ii_html->add( '<link rel="stylesheet" type="text/css" href="css/common.css">' ).
    ii_html->add( '<link rel="stylesheet" type="text/css" href="css/ag-icons.css">' ).

    " Themes
    ii_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-default.css">' ). " Theme basis
    CASE mo_settings->get_ui_theme( ).
      WHEN Lcl_abapgit_settings=>c_ui_theme-dark.
        ii_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-dark.css">' ).
      WHEN Lcl_abapgit_settings=>c_ui_theme-belize.
        ii_html->add( '<link rel="stylesheet" type="text/css" href="css/theme-belize-blue.css">' ).
    ENDCASE.

    " Page stylesheets
    IF ms_control-extra_css_url IS NOT INITIAL.
      ii_html->add( |<link rel="stylesheet" type="text/css" href="{ ms_control-extra_css_url }">| ).
    ENDIF.

  ENDMETHOD.
  METHOD html_head.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<head>' ).

    ri_html->add( '<meta http-equiv="content-type" content="text/html; charset=utf-8">' ).
    ri_html->add( '<meta http-equiv="X-UA-Compatible" content="IE=11,10,9,8" />' ).

    ri_html->add( '<title>abapGit</title>' ).

    header_stylesheet_links( ri_html ).
    header_script_links( ri_html ).

    " Overwrite the automatic icon scaling done in zcl_abapgit_html=>icon
    CASE mo_settings->get_icon_scaling( ).
      WHEN mo_settings->c_icon_scaling-large.
        ri_html->add( '<style>.icon { font-size: 200% }</style>' ).
      WHEN mo_settings->c_icon_scaling-small.
        ri_html->add( '<style>.icon.large { font-size: inherit }</style>' ).
    ENDCASE.

    ri_html->add( '</head>' ).

  ENDMETHOD.
  METHOD render_command_palettes.

    ii_html->add( 'var gCommandPalette = new CommandPalette(enumerateUiActions, {' ).
    ii_html->add( '  toggleKey: "F1",' ).
    ii_html->add( '  hotkeyDescription: "Command ..."' ).
    ii_html->add( '});' ).

  ENDMETHOD.
  METHOD render_deferred_parts.

    DATA lt_parts TYPE Lif_abapgit_html=>ty_table_of.
    DATA li_part LIKE LINE OF lt_parts.

    lt_parts = gui_services( )->get_html_parts( )->get_parts( iv_part_category ).
    LOOP AT lt_parts INTO li_part.
      ii_html->add( li_part ).
    ENDLOOP.

  ENDMETHOD.
  METHOD render_error_message_box.

    " You should remember that the we have to instantiate ro_html even
    " it's overwritten further down. Because ADD checks whether it's
    " bound.
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    " You should remember that we render the message panel only
    " if we have an error.
    IF mx_error IS NOT BOUND.
      RETURN.
    ENDIF.

    ri_html = Lcl_abapgit_gui_chunk_lib=>render_error_message_box( mx_error ).

    " You should remember that the exception viewer dispatches the events of
    " error message panel
    CREATE OBJECT mo_exception_viewer
      EXPORTING
        ix_error = mx_error.

    " You should remember that we render the message panel just once
    " for each exception/error text.
    CLEAR:
      mx_error.

  ENDMETHOD.
  METHOD render_hotkey_overview.

    DATA lo_hotkeys_component TYPE REF TO Lif_abapgit_gui_renderable.

    lo_hotkeys_component ?= gui_services( )->get_hotkeys_ctl( ). " Mmmm ...
    ro_html = lo_hotkeys_component->render( ).

  ENDMETHOD.
  METHOD render_link_hints.

    DATA: lv_link_hint_key TYPE c LENGTH 1.

    lv_link_hint_key = mo_settings->get_link_hint_key( ).

    IF mo_settings->get_link_hints_enabled( ) = abap_true AND lv_link_hint_key IS NOT INITIAL.

      ii_html->add( |activateLinkHints("{ lv_link_hint_key }");| ).
      ii_html->add( |setInitialFocusWithQuerySelector('#header', false);| ).
      ii_html->add( |enableArrowListNavigation();| ).

    ENDIF.

  ENDMETHOD.
  METHOD scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    render_deferred_parts(
      ii_html          = ri_html
      iv_part_category = c_html_parts-scripts ).

    render_link_hints( ri_html ).
    render_command_palettes( ri_html ).
    ri_html->add( |toggleBrowserControlWarning();| ).
    ri_html->add( |displayBrowserControlFooter();| ).

  ENDMETHOD.
  METHOD title.

    DATA lo_page_menu LIKE ms_control-page_menu.
    DATA lv_page_title TYPE string.

    lo_page_menu = ms_control-page_menu.
    IF lo_page_menu IS NOT BOUND AND ms_control-page_menu_provider IS BOUND.
      lo_page_menu = ms_control-page_menu_provider->get_menu( ).
    ENDIF.

    lv_page_title = ms_control-page_title.
    IF ms_control-page_title_provider IS BOUND.
      lv_page_title = ms_control-page_title_provider->get_page_title( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="header">' ).

    ri_html->add( '<div class="logo">' ).
    ri_html->add_a(
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home
      iv_txt = ri_html->icon( 'git-alt' ) ).
    ri_html->add_a(
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home
      iv_txt = ri_html->icon( 'abapgit' ) ).
    ri_html->add( '</div>' ).

    ri_html->add( |<div class="page-title"><span class="spacer">&#x25BA;</span>{ lv_page_title }</div>| ).

    IF lo_page_menu IS BOUND.
      ri_html->add( '<div class="float-right">' ).
      ri_html->add( lo_page_menu->render( iv_right = abap_true ) ).
      ri_html->add( '</div>' ).
    ENDIF.

    IF is_edge_control_warning_needed( ) = abap_true.
      render_browser_control_warning( ri_html ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_error_handler~handle_error.

    mx_error = ix_error.
    rv_handled = abap_true.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-goto_source.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_source( ).
        ENDIF.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-show_callstack.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->show_callstack( ).
        ENDIF.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-goto_message.

        IF mo_exception_viewer IS BOUND.
          mo_exception_viewer->goto_message( ).
        ENDIF.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA:
      li_script TYPE REF TO Lif_abapgit_html,
      lo_timer  TYPE REF TO Lcl_abapgit_timer.

    register_handlers( ).

    lo_timer = Lcl_abapgit_timer=>create( )->start( ).

    " Real page
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<!DOCTYPE html>' ).
    ri_html->add( '<html lang="en">' ).
    ri_html->add( html_head( ) ).
    ri_html->add( |<body class="{ ms_control-page_layout }">| ).

    ri_html->add( title( ) ).

    ri_html->add( '<div class="not_sticky">' ).

    ri_html->add( render_content( ) ). " TODO -> render child

    ri_html->add( render_hotkey_overview( ) ).
    ri_html->add( render_error_message_box( ) ).

    render_deferred_parts(
      ii_html          = ri_html
      iv_part_category = c_html_parts-hidden_forms ).

    ri_html->add( footer( lo_timer->end( ) ) ).

    ri_html->add( '</div>' ).

    li_script = scripts( ).

    IF li_script IS BOUND AND li_script->is_empty( ) = abap_false.
      ri_html->add( '<script>' ).
      ri_html->add( li_script ).
      ri_html->add( 'confirmInitialized();' ).
      ri_html->add( '</script>' ).
    ENDIF.

    ri_html->add( '</body>' ).
    ri_html->add( '</html>' ).

  ENDMETHOD.
  METHOD is_edge_control_warning_needed.

    DATA:
      lv_gui_release       TYPE Lif_abapgit_frontend_services=>ty_gui_release,
      lv_gui_sp            TYPE Lif_abapgit_frontend_services=>ty_gui_sp,
      lv_gui_patch         TYPE Lif_abapgit_frontend_services=>ty_gui_patch,
      li_frontend_services TYPE REF TO Lif_abapgit_frontend_services.

    " With SAGUI 8.00 PL3 and 7.70 PL13 edge browser control is basically working.
    " For lower releases we render the browser control warning
    " an toggle it via JS function toggleBrowserControlWarning.

    rv_result = abap_true.

    TRY.
        li_frontend_services = Lcl_abapgit_ui_factory=>get_frontend_services( ).
        li_frontend_services->get_gui_version(
          IMPORTING
            ev_gui_release        = lv_gui_release
            ev_gui_sp             = lv_gui_sp
            ev_gui_patch          = lv_gui_patch ).

      CATCH Lcx_abapgit_exception.
        RETURN.
    ENDTRY.

    IF lv_gui_release >= '7700' AND lv_gui_sp >= '1' AND lv_gui_patch >= '13'
    OR lv_gui_release >= '8000' AND lv_gui_sp >= '1' AND lv_gui_patch >= '3'.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.
  METHOD get_version_details.

    DATA lo_frontend_serv TYPE REF TO Lif_abapgit_frontend_services.

    rv_version = Lif_abapgit_version=>c_abap_version.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      rv_version = rv_version && ` - Standalone Version`.
    ELSE.
      rv_version = rv_version && ` - Developer Version`.
    ENDIF.

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    CASE abap_true.
      WHEN lo_frontend_serv->is_webgui( ).
        rv_version = rv_version && ` - Web`.
      WHEN lo_frontend_serv->is_sapgui_for_windows( ).
        rv_version = rv_version && ` - Win`.
      WHEN lo_frontend_serv->is_sapgui_for_java( ).
        rv_version = rv_version && ` - Java`.
      WHEN OTHERS.
* eg. open-abap?
        rv_version = rv_version && ` - Unknown`.
    ENDCASE.

    " Will be filled by JS method displayBrowserControlFooter
    rv_version = rv_version && '<span id="browser-control-footer"></span>'.

  ENDMETHOD.
  METHOD render_browser_control_warning.

    DATA li_documentation_link TYPE REF TO Lif_abapgit_html.

    CREATE OBJECT li_documentation_link TYPE Lcl_abapgit_html.

    li_documentation_link->add_a(
        iv_txt = 'Documentation'
        iv_typ = Lif_abapgit_html=>c_action_type-url
        iv_act = 'https://docs.abapgit.org/guide-sapgui.html#sap-gui-for-windows' ).

    ii_html->add( '<div id="browser-control-warning" class="browser-control-warning">' ).
    ii_html->add( Lcl_abapgit_gui_chunk_lib=>render_warning_banner(
                    |Attention: You use Edge browser control. |
                 && |There are several known malfunctions. See |
                 && li_documentation_link->render( ) ) ).
    ii_html->add( '</div>' ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_modal~is_modal.
    rv_yes = boolc( ms_control-show_as_modal = abap_true ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_ADDONLINE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_addonlineccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_addonlineccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_ADDONLINE implementation.
*"* method's implementations
*include methods.
  METHOD choose_labels.

    DATA:
      lv_old_labels TYPE string,
      lv_new_labels TYPE string.

    lv_old_labels = mo_form_data->get( c_id-labels ).

    lv_new_labels = Lcl_abapgit_ui_factory=>get_popups( )->popup_to_select_labels( lv_old_labels ).

    mo_form_data->set(
      iv_key = c_id-labels
      iv_val = lv_new_labels ).

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_addonline.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'New Online Repository'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'add-repo-online-form'
                iv_help_page = 'https://docs.abapgit.org/guide-online-install.html' ).

    ro_form->text(
      iv_name        = c_id-url
      iv_required    = abap_true
      iv_condense    = abap_true
      iv_label       = 'Git Repository URL'
      iv_hint        = 'HTTPS address of the repository'
      iv_placeholder = 'https://github.com/...git'
    )->text(
      iv_name        = c_id-package
      iv_side_action = c_event-choose_package
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_label       = 'Package'
      iv_hint        = 'SAP package for repository (should be a dedicated one)'
      iv_placeholder = 'Z... / $...'
    )->text(
      iv_name        = c_id-branch_name
      iv_side_action = c_event-choose_branch
      iv_label       = 'Branch'
      iv_hint        = 'Switch to a specific branch (default: autodetect)'
      iv_placeholder = 'Autodetect default branch'
    )->radio(
      iv_name        = c_id-folder_logic
      iv_default_value = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_label       = 'Folder Logic'
      iv_hint        = 'Define how package folders are named in repository'
    )->option(
      iv_label       = 'Prefix'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
    )->option(
      iv_label       = 'Full'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-full
    )->option(
      iv_label       = 'Mixed'
      iv_value       = Lif_abapgit_dot_abapgit=>c_folder_logic-mixed
    )->text(
      iv_name        = c_id-display_name
      iv_label       = 'Display Name'
      iv_hint        = 'Name to show instead of original repository name (optional)'
    )->text(
      iv_name        = c_id-labels
      iv_side_action = c_event-choose_labels
      iv_label       = |Labels (comma-separated, allowed chars: "{ Lcl_abapgit_repo_labels=>c_allowed_chars }")|
      iv_hint        = 'Comma-separated labels for grouping and repo organization (optional)'
    )->checkbox(
      iv_name        = c_id-ignore_subpackages
      iv_label       = 'Ignore Subpackages'
      iv_hint        = 'Synchronize root package only'
    )->checkbox(
      iv_name        = c_id-main_lang_only
      iv_label       = 'Serialize Main Language Only'
      iv_hint        = 'Ignore translations, serialize just main language' ).

    IF Lcl_abapgit_feature=>is_enabled( Lcl_abapgit_abap_language_vers=>c_feature_flag ) = abap_true.
      ro_form->radio(
        iv_name        = c_id-abap_lang_vers
        iv_default_value = ''
        iv_label       = 'ABAP Language Version'
        iv_hint        = 'Define the ABAP language version for objects in the repository'
      )->option(
        iv_label       = 'Any'
        iv_value       = ''
      )->option(
        iv_label       = 'Standard'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-standard
      )->option(
        iv_label       = 'For Key Users'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-key_user
      )->option(
        iv_label       = 'For Cloud Development'
        iv_value       = Lif_abapgit_dot_abapgit=>c_abap_language_version-cloud_development ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Create Online Repo'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-add_online_repo
    )->command(
      iv_label       = 'Create Package'
      iv_action      = c_event-create_package
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD validate_form.

    DATA:
      lv_url TYPE string,
      lo_url TYPE REF TO Lcl_abapgit_git_url,
      lx_err TYPE REF TO Lcx_abapgit_exception.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    lv_url = io_form_data->get( c_id-url ).
    IF lv_url IS NOT INITIAL.
      TRY.
          Lcl_abapgit_repo_srv=>get_instance( )->validate_url( lv_url ).

          " Provider-specific URL check
          CREATE OBJECT lo_url.
          lo_url->validate_url( lv_url ).
        CATCH Lcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-url
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-package ) IS NOT INITIAL.
      TRY.
          Lcl_abapgit_repo_srv=>get_instance( )->validate_package(
            iv_package    = |{ io_form_data->get( c_id-package ) }|
            iv_ign_subpkg = |{ io_form_data->get( c_id-ignore_subpackages ) }| ).
        CATCH Lcx_abapgit_exception INTO lx_err.
          ro_validation_log->set(
            iv_key = c_id-package
            iv_val = lx_err->get_text( ) ).
      ENDTRY.
    ENDIF.

    IF io_form_data->get( c_id-folder_logic ) <> Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
        AND io_form_data->get( c_id-folder_logic ) <> Lif_abapgit_dot_abapgit=>c_folder_logic-full
        AND io_form_data->get( c_id-folder_logic ) <> Lif_abapgit_dot_abapgit=>c_folder_logic-mixed.
      ro_validation_log->set(
        iv_key = c_id-folder_logic
        iv_val = |Invalid folder logic { io_form_data->get( c_id-folder_logic ) }| ).
    ENDIF.

    TRY.
        Lcl_abapgit_repo_labels=>validate( io_form_data->get( c_id-labels ) ).
      CATCH Lcx_abapgit_exception INTO lx_err.
        ro_validation_log->set(
          iv_key = c_id-labels
          iv_val = lx_err->get_text( ) ).
    ENDTRY.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA: ls_repo_params     TYPE Lif_abapgit_services_repo=>ty_repo_params,
          lo_new_online_repo TYPE REF TO Lcl_abapgit_repo_online.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-create_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = Lcl_abapgit_services_repo=>create_package(
            iv_prefill_package = |{ mo_form_data->get( c_id-package ) }| ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = Lcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).
        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          mo_validation_log = validate_form( mo_form_data ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.

      WHEN c_event-choose_branch.

        mo_validation_log = validate_form( mo_form_data ).
        IF mo_validation_log->has( c_id-url ) = abap_true.
          mo_validation_log->set(
            iv_key = c_id-branch_name
            iv_val = 'Check URL issues' ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render. " Display errors
          RETURN.
        ENDIF.
        mo_form_data->set(
          iv_key = c_id-branch_name
          iv_val = Lcl_abapgit_ui_factory=>get_popups( )->branch_list_popup( mo_form_data->get( c_id-url ) )-name ).

        IF mo_form_data->get( c_id-branch_name ) IS INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-branch_name
            iv_val = replace( " strip technical
              val = mo_form_data->get( c_id-branch_name )
              sub = Lif_abapgit_git_definitions=>c_git_branch-heads_prefix
              with = '' ) ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-choose_labels.

        choose_labels( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_event-add_online_repo.

        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.
          mo_form_data->to_abap( CHANGING cs_container = ls_repo_params ).
          lo_new_online_repo = Lcl_abapgit_services_repo=>new_online( ls_repo_params ).
          rs_handled-page  = Lcl_abapgit_gui_page_repo_view=>create( lo_new_online_repo->get_key( ) ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render. " Display errors
        ENDIF.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_ADDONLINE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_CODE_INSP <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_code_inspccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_code_inspccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_CODE_INSP implementation.
*"* method's implementations
*include methods.
  METHOD ask_user_for_check_variant.

    rv_check_variant = Lcl_abapgit_ui_factory=>get_popups( )->choose_code_insp_check_variant( ).

    IF rv_check_variant IS INITIAL.
      Lcx_abapgit_exception=>raise( |Please select a check variant.| ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.
    super->constructor( ).
    mo_repo = io_repo.
    mo_stage = io_stage.
    mv_check_variant = iv_check_variant.
    determine_check_variant( ).
    run_code_inspector( ).

    IF mt_result IS INITIAL AND iv_raise_when_no_results = abap_true.
      Lcx_abapgit_exception=>raise( 'No results' ).
    ENDIF.
  ENDMETHOD.
  METHOD determine_check_variant.

    IF mv_check_variant IS NOT INITIAL.
      RETURN.
    ENDIF.

    mv_check_variant = mo_repo->get_local_settings( )-code_inspector_check_variant.

    IF mv_check_variant IS INITIAL.
      mv_check_variant = ask_user_for_check_variant( ).
    ENDIF.

  ENDMETHOD.
  METHOD has_inspection_errors.

    READ TABLE mt_result TRANSPORTING NO FIELDS
                         WITH KEY kind = 'E'.
    rv_has_inspection_errors = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD is_stage_allowed.

    rv_is_stage_allowed = boolc( NOT ( mo_repo->get_local_settings( )-block_commit = abap_true
                                           AND has_inspection_errors( ) = abap_true ) ).

  ENDMETHOD.
  METHOD run_code_inspector.

    DATA: li_code_inspector TYPE REF TO Lif_abapgit_code_inspector.

    li_code_inspector = Lcl_abapgit_factory=>get_code_inspector( mo_repo->get_package( ) ).

    mt_result = li_code_inspector->run(
      iv_variant = |{ mv_check_variant }|
      iv_save    = abap_true ).

    mv_summary = li_code_inspector->get_summary( ).

    DELETE mt_result WHERE kind = 'N'.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Code Inspector'.

    ls_hotkey_action-description = |Stage|.
    ls_hotkey_action-action = c_actions-stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Re-Run|.
    ls_hotkey_action-action = c_actions-rerun.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_code_insp.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo                  = io_repo
        io_stage                 = io_stage
        iv_check_variant         = iv_check_variant
        iv_raise_when_no_results = iv_raise_when_no_results.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Code Inspector'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA lo_repo_online TYPE REF TO Lcl_abapgit_repo_online.
    DATA lv_sci_result TYPE Lif_abapgit_definitions=>ty_sci_result.

    CASE ii_event->mv_action.
      WHEN c_actions-stage.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.
          " we need to refresh as the source might have changed
          lo_repo_online->refresh( ).

          READ TABLE mt_result TRANSPORTING NO FIELDS WITH KEY kind = 'E'.
          IF sy-subrc = 0.
            lv_sci_result = Lif_abapgit_definitions=>c_sci_result-failed.
          ELSE.
            READ TABLE mt_result TRANSPORTING NO FIELDS WITH KEY kind = 'W'.
            IF sy-subrc = 0.
              lv_sci_result = Lif_abapgit_definitions=>c_sci_result-warning.
            ELSE.
              lv_sci_result = Lif_abapgit_definitions=>c_sci_result-passed.
            ENDIF.
          ENDIF.

          rs_handled-page   = Lcl_abapgit_gui_page_stage=>create(
            io_repo       = lo_repo_online
            iv_sci_result = lv_sci_result ).

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-commit.

        lo_repo_online ?= mo_repo.

        IF is_stage_allowed( ) = abap_true.

          rs_handled-page = Lcl_abapgit_gui_page_commit=>create(
            io_repo  = lo_repo_online
            io_stage = mo_stage ).

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

        ELSE.

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

        ENDIF.

      WHEN c_actions-rerun.

        run_code_inspector( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        rs_handled = on_event( ii_event ).
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    DATA: lv_opt TYPE c LENGTH 1.

    ro_toolbar = build_base_menu( ).

    IF is_stage_allowed( ) = abap_false.
      lv_opt = Lif_abapgit_html=>c_html_opt-crossout.
    ENDIF.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    IF mo_stage IS BOUND.

      " Staging info already available, we can directly
      " offer to commit

      ro_toolbar->add( iv_txt = 'Commit'
                       iv_act = c_actions-commit
                       iv_opt = lv_opt ).

    ELSE.

      ro_toolbar->add( iv_txt = 'Stage'
                       iv_act = c_actions-stage
                       iv_opt = lv_opt ).

    ENDIF.

    ro_toolbar->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top( io_repo        = mo_repo
                                                              iv_show_commit = abap_false ) ).
    ri_html->add( `</div>` ).

    IF mv_check_variant IS INITIAL.
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_error( iv_error = 'No check variant supplied.' ) ).
      RETURN.
    ENDIF.

    ri_html->add( render_variant(
      iv_variant = mv_check_variant
      iv_summary = mv_summary ) ).

    IF lines( mt_result ) = 0.
      ri_html->add( '<div class="dummydiv success">' ).
      ri_html->add( ri_html->icon( 'check' ) ).
      ri_html->add( 'No code inspector findings' ).
      ri_html->add( '</div>' ).
    ELSE.
      render_result(
        ii_html   = ri_html
        it_result = mt_result ).
    ENDIF.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_CODE_INSP implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_CODI_BASE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_codi_baseccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_codi_baseccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_CODI_BASE implementation.
*"* method's implementations
*include methods.
  METHOD build_base_menu.

    DATA:
      lo_sort_menu TYPE REF TO Lcl_abapgit_html_toolbar.

    CREATE OBJECT lo_sort_menu.

    lo_sort_menu->add(
      iv_txt = 'By Object, Check, Sub-object'
      iv_act = c_actions-sort_1
    )->add(
      iv_txt = 'By Object, Sub-object, Line'
      iv_act = c_actions-sort_2
    )->add(
      iv_txt = 'By Check, Object, Sub-object'
      iv_act = c_actions-sort_3 ).

    CREATE OBJECT ro_menu.

    ro_menu->add( iv_txt = 'Sort'
                  io_sub = lo_sort_menu ).

    ro_menu->add( iv_txt = 'Re-Run'
                  iv_act = c_actions-rerun ).

  ENDMETHOD.
  METHOD build_nav_link.

    rv_link = |{ c_ci_sig }| &&
      |{ is_result-objtype }{ is_result-objname }| &&
      |{ c_object_separator }{ is_result-sobjtype }{ is_result-sobjname }| &&
      |{ c_object_separator }{ is_result-line }|.

  ENDMETHOD.
  METHOD jump.

    DATA: lo_test             TYPE REF TO cl_ci_test_root,
          ls_info             TYPE scir_rest,
          lo_result           TYPE REF TO cl_ci_result_root,
          lv_adt_jump_enabled TYPE abap_bool,
          lv_line_number      TYPE i,
          ls_item             TYPE Lif_abapgit_definitions=>ty_item,
          ls_sub_item         TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF mt_result.


    IF is_sub_item IS NOT INITIAL.
      READ TABLE mt_result WITH KEY objtype  = is_item-obj_type
                                    objname  = is_item-obj_name
                                    sobjtype = is_sub_item-obj_type
                                    sobjname = is_sub_item-obj_name
                                    line     = iv_line_number
                           ASSIGNING <ls_result>.
    ELSE.
      READ TABLE mt_result WITH KEY objtype = is_item-obj_type
                                    objname = is_item-obj_name
                                    line    = iv_line_number
                           ASSIGNING <ls_result>.
    ENDIF.
    ASSERT <ls_result> IS ASSIGNED.
    ls_item-obj_name = <ls_result>-objname.
    ls_item-obj_type = <ls_result>-objtype.

    ls_sub_item-obj_name = <ls_result>-sobjname.
    ls_sub_item-obj_type = <ls_result>-sobjtype.

    " see SCI_LCL_DYNP_530 / HANDLE_DOUBLE_CLICK

    lv_adt_jump_enabled = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).

    TRY.
        IF lv_adt_jump_enabled = abap_true.

          lv_line_number = <ls_result>-line.

          Lcl_abapgit_objects=>jump(
            is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_line_number = lv_line_number ).
          RETURN.

        ENDIF.
      CATCH Lcx_abapgit_exception.
    ENDTRY.

    TRY.
        CALL METHOD ('CL_CI_TESTS')=>('GET_TEST_REF')
          EXPORTING
            p_test   = <ls_result>-test
          RECEIVING
            p_result = lo_test.
      CATCH cx_root.
        Lcx_abapgit_exception=>raise( |Jump to object not supported in your NW release| ).
    ENDTRY.

    lo_result = lo_test->get_result_node( <ls_result>-kind ).

    MOVE-CORRESPONDING <ls_result> TO ls_info.

    lo_result->set_info( ls_info ).
    lo_result->if_ci_test~navigate( ).

  ENDMETHOD.
  METHOD render_result.

    CONSTANTS: lc_limit TYPE i VALUE 500.
    FIELD-SYMBOLS: <ls_result> LIKE LINE OF it_result.

    ii_html->add( '<div class="ci-result">' ).

    LOOP AT it_result ASSIGNING <ls_result> TO lc_limit.
      render_result_line(
        ii_html = ii_html
        is_result = <ls_result> ).
    ENDLOOP.

    ii_html->add( '</div>' ).

    IF lines( it_result ) > lc_limit.
      ii_html->add( '<div class="dummydiv warning">' ).
      ii_html->add( ii_html->icon( 'exclamation-triangle' ) ).
      ii_html->add( |Only first { lc_limit } findings shown in list!| ).
      ii_html->add( '</div>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_result_line.

    DATA: lv_class   TYPE string,
          lv_obj_txt TYPE string,
          lv_msg     TYPE string,
          lv_line    TYPE i,
          ls_mtdkey  TYPE seocpdkey.

    CASE is_result-kind.
      WHEN 'E'.
        lv_class = 'ci-error'.
      WHEN 'W'.
        lv_class = 'ci-warning'.
      WHEN OTHERS.
        lv_class = 'ci-info'.
    ENDCASE.

    lv_msg = escape( val = is_result-text
                     format = cl_abap_format=>e_html_attr ).

    IF is_result-sobjname IS INITIAL OR
       ( is_result-sobjname = is_result-objname AND
         is_result-sobjtype = is_result-objtype ).
      lv_obj_txt = |{ is_result-objtype } { is_result-objname }|.
    ELSEIF is_result-objtype = 'CLAS' OR
         ( is_result-objtype = 'PROG' AND NOT is_result-sobjname+30(*) IS INITIAL ).
      TRY.
          CASE is_result-sobjname+30(*).
            WHEN 'CCDEF'.
              lv_obj_txt = |CLAS { is_result-objname } : Local Definitions|.
            WHEN 'CCIMP'.
              lv_obj_txt = |CLAS { is_result-objname } : Local Implementations|.
            WHEN 'CCMAC'.
              lv_obj_txt = |CLAS { is_result-objname } : Macros|.
            WHEN 'CCAU'.
              lv_obj_txt = |CLAS { is_result-objname } : Test Classes|.
            WHEN 'CU'.
              lv_obj_txt = |CLAS { is_result-objname } : Public Section|.
            WHEN 'CO'.
              lv_obj_txt = |CLAS { is_result-objname } : Protected Section|.
            WHEN 'CI'.
              lv_obj_txt = |CLAS { is_result-objname } : Private Section|.
            WHEN OTHERS.
              cl_oo_classname_service=>get_method_by_include(
                EXPORTING
                  incname             = is_result-sobjname
                RECEIVING
                  mtdkey              = ls_mtdkey
                EXCEPTIONS
                  class_not_existing  = 1
                  method_not_existing = 2
                  OTHERS              = 3 ).
              IF sy-subrc = 0.
                lv_obj_txt = |CLAS { ls_mtdkey-clsname }->{ ls_mtdkey-cpdname }|.
              ELSE.
                lv_obj_txt = |{ is_result-objtype } { is_result-sobjname }|.
              ENDIF.

          ENDCASE.
        CATCH cx_root.
          lv_obj_txt = ''. "use default below
      ENDTRY.
    ENDIF.
    IF lv_obj_txt IS INITIAL.
      lv_obj_txt = |{ is_result-objtype } { is_result-objname } &gt; { is_result-sobjtype } { is_result-sobjname }|.
    ENDIF.
    lv_line = is_result-line. " convert from numc to integer
    lv_obj_txt = |{ lv_obj_txt } [ @{ lv_line } ]|.

    ii_html->add( |<li class="{ lv_class }">| ).
    ii_html->add_a(
      iv_txt = lv_obj_txt
      iv_act = build_nav_link( is_result )
      iv_typ = Lif_abapgit_html=>c_action_type-sapevent ).
    ii_html->add( |<span>{ lv_msg }</span>| ).
    ii_html->add( '</li>' ).

  ENDMETHOD.
  METHOD render_variant.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="ci-head">' ).
    ri_html->add( |Code inspector check variant <span class="ci-variant">{ iv_variant }</span>|
               && | completed ({ iv_summary })| ).
    ri_html->add( `</div>` ).

  ENDMETHOD.
  METHOD on_event.

    DATA: ls_item          TYPE Lif_abapgit_definitions=>ty_item,
          ls_sub_item      TYPE Lif_abapgit_definitions=>ty_item,
          lv_temp          TYPE string,
          lv_main_object   TYPE string,
          lv_sub_object    TYPE string,
          lv_line_number_s TYPE string,
          lv_line_number   TYPE i.

    lv_temp = replace( val   = ii_event->mv_action
                       regex = |^{ c_ci_sig }|
                       with  = `` ).

    IF lv_temp <> ii_event->mv_action. " CI navigation request detected

      SPLIT lv_temp AT c_object_separator INTO lv_main_object lv_sub_object lv_line_number_s.
      ls_item-obj_type = to_upper( lv_main_object(4) ).
      ls_item-obj_name = to_upper( lv_main_object+4(*) ).

      IF lv_sub_object IS NOT INITIAL.
        ls_sub_item-obj_type = to_upper( lv_sub_object(4) ).
        ls_sub_item-obj_name = to_upper( lv_sub_object+4(*) ).
      ENDIF.

      lv_line_number = lv_line_number_s.

      jump( is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_line_number = lv_line_number ).

      rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

    ENDIF.

    CASE ii_event->mv_action.

      WHEN c_actions-sort_1.
        SORT mt_result BY objtype objname test code sobjtype sobjname line col.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-sort_2.
        SORT mt_result BY objtype objname sobjtype sobjname line col test code.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_actions-sort_3.
        SORT mt_result BY test code objtype objname sobjtype sobjname line col.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_CODI_BASE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_COMMIT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_commit===ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_commit===ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_COMMIT implementation.
*"* method's implementations
*include methods.
  METHOD branch_name_to_internal.
    rv_new_branch_name = Lcl_abapgit_git_branch_list=>complete_heads_branch_name(
      Lcl_abapgit_git_branch_list=>normalize_branch_name( iv_branch_name ) ).
  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mo_repo       = io_repo.
    mo_stage      = io_stage.
    mt_stage      = mo_stage->get_all( ).
    mv_sci_result = iv_sci_result.

    " Get settings from DB
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_commit.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        io_stage      = io_stage
        iv_sci_result = iv_sci_result.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Commit'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_comment_default.

    rv_text = mo_settings->get_commitmsg_comment_default( ).

    IF rv_text IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE '$FILE'   IN rv_text WITH get_comment_file( mt_stage ).
    REPLACE '$OBJECT' IN rv_text WITH get_comment_object( mt_stage ).

  ENDMETHOD.
  METHOD get_comment_file.

    DATA lv_count TYPE i.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    lv_count = lines( it_stage ).

    IF lv_count = 1.
      " Just one file so we use the file name
      READ TABLE it_stage ASSIGNING <ls_stage> INDEX 1.
      ASSERT sy-subrc = 0.

      rv_text = <ls_stage>-file-filename.
    ELSE.
      " For multiple file we use the count instead
      rv_text = |{ lv_count } files|.
    ENDIF.

  ENDMETHOD.
  METHOD get_comment_object.

    DATA:
      lv_count TYPE i,
      ls_item  TYPE Lif_abapgit_definitions=>ty_item,
      lt_items TYPE Lif_abapgit_definitions=>ty_items_tt.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF it_stage.

    " Get objects
    LOOP AT it_stage ASSIGNING <ls_stage>.
      CLEAR ls_item.
      ls_item-obj_type = <ls_stage>-status-obj_type.
      ls_item-obj_name = <ls_stage>-status-obj_name.
      COLLECT ls_item INTO lt_items.
    ENDLOOP.

    lv_count = lines( lt_items ).

    IF lv_count = 1.
      " Just one object so we use the object name
      READ TABLE lt_items INTO ls_item INDEX 1.
      ASSERT sy-subrc = 0.

      CONCATENATE ls_item-obj_type ls_item-obj_name INTO rv_text SEPARATED BY space.
    ELSE.
      " For multiple objects we use the count instead
      rv_text = |{ lv_count } objects|.
    ENDIF.

  ENDMETHOD.
  METHOD get_committer_email.

    DATA li_user TYPE REF TO Lif_abapgit_persist_user.

    li_user = Lcl_abapgit_persistence_user=>get_instance( ).

    rv_email = li_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF rv_email IS INITIAL.
      rv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF rv_email IS INITIAL.
      " get default from user record
      rv_email = Lcl_abapgit_user_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

  ENDMETHOD.
  METHOD get_committer_name.

    DATA li_user TYPE REF TO Lif_abapgit_persist_user.

    li_user = Lcl_abapgit_persistence_user=>get_instance( ).

    rv_user  = li_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF rv_user IS INITIAL.
      rv_user  = li_user->get_default_git_user_name( ).
    ENDIF.
    IF rv_user IS INITIAL.
      " get default from user record
      rv_user = Lcl_abapgit_user_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD get_defaults.

    ms_commit-committer_name  = get_committer_name( ).
    ms_commit-committer_email = get_committer_email( ).
    ms_commit-comment         = get_comment_default( ).

    " Committer
    mo_form_data->set(
      iv_key = c_id-committer_name
      iv_val = ms_commit-committer_name ).
    mo_form_data->set(
      iv_key = c_id-committer_email
      iv_val = ms_commit-committer_email ).

    " Message
    mo_form_data->set(
      iv_key = c_id-comment
      iv_val = ms_commit-comment ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA: lv_commitmsg_comment_length TYPE i.

    ro_form = Lcl_abapgit_html_form=>create(
      iv_form_id   = 'commit-form'
      iv_help_page = 'https://docs.abapgit.org/guide-stage-commit.html' ).

    lv_commitmsg_comment_length = mo_settings->get_commitmsg_comment_length( ).

    ro_form->text(
      iv_name        = c_id-comment
      iv_label       = 'Comment'
      iv_required    = abap_true
      iv_max         = lv_commitmsg_comment_length
      iv_placeholder = |Add a mandatory comment with max { lv_commitmsg_comment_length } characters|
    )->textarea(
      iv_name        = c_id-body
      iv_label       = 'Body'
      iv_rows        = 6
      iv_cols        = mo_settings->get_commitmsg_body_size( )
      iv_placeholder = 'Add an optional description...'
    )->text(
      iv_name        = c_id-committer_name
      iv_label       = 'Committer Name'
      iv_required    = abap_true
    )->text(
      iv_name        = c_id-committer_email
      iv_label       = 'Committer Email'
      iv_required    = abap_true ).

    IF mo_settings->get_commitmsg_hide_author( ) IS INITIAL.
      ro_form->text(
        iv_name        = c_id-author_name
        iv_label       = 'Author Name'
        iv_placeholder = 'Optionally, specify an author (same as committer by default)'
      )->text(
        iv_name        = c_id-author_email
        iv_label       = 'Author Email' ).
    ENDIF.

    ro_form->text(
      iv_name        = c_id-new_branch_name
      iv_label       = 'New Branch Name'
      iv_placeholder = 'Optionally, enter a new branch name for this commit'
      iv_condense    = abap_true ).


    ro_form->command(
      iv_label       = 'Commit'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-commit
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD render_stage_details.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<table class="stage_tab">' ).
    ri_html->add( '<thead>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th colspan="3">Staged Files (See <a href="#top">Summary</a> Above)</th>' ).
    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

    ri_html->add( '<tbody>' ).
    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ri_html->add( '<tr>' ).
      ri_html->add( '<td>' ).
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state(
        iv_lstate = <ls_stage>-status-lstate
        iv_rstate = <ls_stage>-status-rstate ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td class="method">' ).
      ri_html->add( Lcl_abapgit_stage=>method_description( <ls_stage>-method ) ).
      ri_html->add( '</td>' ).
      ri_html->add( '<td>' ).
      ri_html->add( <ls_stage>-file-path && <ls_stage>-file-filename ).
      ri_html->add( '</td>' ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</tbody>' ).

    ri_html->add( '</table>' ).

  ENDMETHOD.
  METHOD render_stage_summary.

    DATA:
      BEGIN OF ls_sum,
        method TYPE string,
        count  TYPE i,
      END OF ls_sum,
      lt_sum LIKE STANDARD TABLE OF ls_sum WITH DEFAULT KEY.

    FIELD-SYMBOLS <ls_stage> LIKE LINE OF mt_stage.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    LOOP AT mt_stage ASSIGNING <ls_stage>.
      ls_sum-method = <ls_stage>-method.
      ls_sum-count  = 1.
      COLLECT ls_sum INTO lt_sum.
    ENDLOOP.

    ri_html->add( 'Stage Summary: ' ).

    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = Lif_abapgit_definitions=>c_method-add.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_ins" title="add">+ { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = Lif_abapgit_definitions=>c_method-rm.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_del" title="remove">- { ls_sum-count }</span>| ).
    ENDIF.
    READ TABLE lt_sum INTO ls_sum WITH TABLE KEY method = Lif_abapgit_definitions=>c_method-ignore.
    IF sy-subrc = 0.
      ri_html->add( |<span class="diff_banner diff_upd" title="ignore">~ { ls_sum-count }</span>| ).
    ENDIF.

    IF lines( mt_stage ) = 1.
      ri_html->add( 'file' ).
    ELSE.
      ri_html->add( 'files' ).
    ENDIF.

    ri_html->add( '(See <a href="#stage-details">Details</a> Below)' ).

  ENDMETHOD.
  METHOD validate_form.

    DATA: lt_branches        TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
          lv_new_branch_name TYPE string.

    ro_validation_log = mo_form_util->validate( io_form_data ).

    IF Lcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-committer_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-committer_email
        iv_val = |Invalid email address| ).
    ENDIF.

    IF Lcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-author_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-author_email
        iv_val = |Invalid email address| ).
    ENDIF.

    lv_new_branch_name = io_form_data->get( c_id-new_branch_name ).
    IF lv_new_branch_name IS NOT INITIAL.
      " check if branch already exists
      lt_branches = Lcl_abapgit_git_transport=>branches( mo_repo->get_url( ) )->get_branches_only( ).
      READ TABLE lt_branches TRANSPORTING NO FIELDS WITH TABLE KEY name_key
        COMPONENTS name = branch_name_to_internal( lv_new_branch_name ).
      IF sy-subrc = 0.
        ro_validation_log->set(
          iv_key = c_id-new_branch_name
          iv_val = |Branch already exists| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.
    DATA lv_new_branch_name   TYPE string.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-commit.
        " Validate form entries before committing
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          " new branch fields not needed in commit data
          mo_form_data->strict( abap_false ).

          mo_form_data->to_abap( CHANGING cs_container = ms_commit ).

          REPLACE ALL OCCURRENCES
            OF cl_abap_char_utilities=>cr_lf
            IN ms_commit-body
            WITH cl_abap_char_utilities=>newline.

          lv_new_branch_name = mo_form_data->get( c_id-new_branch_name ).
          " create new branch and commit to it if branch name is not empty
          IF lv_new_branch_name IS NOT INITIAL.
            lv_new_branch_name = branch_name_to_internal( lv_new_branch_name ).
            " creates a new branch and automatically switches to it
            mo_repo->create_branch( lv_new_branch_name ).
          ENDIF.

          Lcl_abapgit_services_git=>commit(
            is_commit = ms_commit
            io_repo   = mo_repo
            io_stage  = mo_stage ).


          MESSAGE 'Commit was successful' TYPE 'S'.

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back_to_bookmark.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.
      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    IF mo_form_util->is_empty( mo_form_data ) = abap_true.
      get_defaults( ).
    ENDIF.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( '<div id="top" class="paddings">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="stage-summary" class="dialog w800px paddings">' ).
    ri_html->add( render_stage_summary( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( mo_form->render(
      iv_form_class     = 'w800px'
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).

    ri_html->add( '<div id="stage-details" class="dialog w800px">' ).
    ri_html->add( render_stage_details( ) ).
    ri_html->add( '</div>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_COMMIT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DB_ENTRY <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_db_entry=ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_db_entry=ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_DB_ENTRY implementation.
*"* method's implementations
*include methods.
  METHOD build_toolbar.

    CREATE OBJECT ro_toolbar.

    IF mv_edit_mode = abap_true.
      ro_toolbar->add(
        iv_act = |submitFormById('{ c_edit_form_id }');|
        iv_txt = 'Save'
        iv_typ = Lif_abapgit_html=>c_action_type-onclick
        iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
    ELSE.
      ro_toolbar->add(
        iv_act = |{ c_action-switch_mode }|
        iv_txt = 'Edit' ).
    ENDIF.

    ro_toolbar->add(
      iv_act = |{ c_action-back }|
      iv_txt = 'Back' ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).
    register_stylesheet( ).
    mv_edit_mode = iv_edit_mode.
    ms_key       = is_key.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_db_entry.

    CREATE OBJECT lo_component
      EXPORTING
        iv_edit_mode = iv_edit_mode
        is_key       = is_key.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_extra_css_url       = c_css_url
      ii_page_title_provider = lo_component
      ii_child_component     = lo_component ).

  ENDMETHOD.
  METHOD dbcontent_decode.

    rs_content-type     = io_form_data->get( 'TYPE' ).
    rs_content-value    = io_form_data->get( 'VALUE' ).
    rs_content-data_str = io_form_data->get( 'XMLDATA' ).

    IF rs_content-data_str(1) <> '<' AND rs_content-data_str+1(1) = '<'. " Hmmm ???
      rs_content-data_str = rs_content-data_str+1.
    ENDIF.

  ENDMETHOD.
  METHOD do_update.

    ASSERT is_content-type IS NOT INITIAL.

    Lcl_abapgit_persistence_db=>get_instance( )->update(
      iv_type  = is_content-type
      iv_value = is_content-value
      iv_data  = is_content-data_str ).

    COMMIT WORK.

  ENDMETHOD.
  METHOD register_stylesheet.

    DATA lo_buf TYPE REF TO Lcl_abapgit_string_buffer.

    CREATE OBJECT lo_buf.

lo_buf->add( '/*' ).
lo_buf->add( ' * PAGE DB ENTRY CSS' ).
lo_buf->add( ' */' ).
lo_buf->add( '' ).
lo_buf->add( '/* LAYOUT */' ).
lo_buf->add( '' ).
lo_buf->add( '.db-entry {' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry pre {' ).
lo_buf->add( '  display: block;' ).
lo_buf->add( '  font-size: 10pt;' ).
lo_buf->add( '  overflow: hidden;' ).
lo_buf->add( '  word-wrap:break-word;' ).
lo_buf->add( '  white-space: pre-wrap;' ).
lo_buf->add( '  border: 1px  solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  padding: 0.5em;' ).
lo_buf->add( '  margin: 0.5em 0em;' ).
lo_buf->add( '  width: 98%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry textarea {' ).
lo_buf->add( '  margin: 0.5em 0em;' ).
lo_buf->add( '  width: 98%;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry .toolbar {' ).
lo_buf->add( '  padding-left: 0.5em;' ).
lo_buf->add( '  padding-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dl.entry-tag div {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  border: 1px solid;' ).
lo_buf->add( '  border-radius: 3px;' ).
lo_buf->add( '  margin-right: 0.5em;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dl.entry-tag div:last-child {' ).
lo_buf->add( '  margin-right: 0px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dt, .db-entry dd {' ).
lo_buf->add( '  display: inline-block;' ).
lo_buf->add( '  margin-left: 0px;' ).
lo_buf->add( '  padding: 2px 5px;' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dt::after {  content: ":" }' ).
lo_buf->add( '' ).
lo_buf->add( '/* COLORS */' ).
lo_buf->add( '' ).
lo_buf->add( '.db-entry {' ).
lo_buf->add( '  background-color: var(--theme-container-background-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry pre, .db-entry textarea {' ).
lo_buf->add( '  background-color: var(--theme-table-background-color);' ).
lo_buf->add( '  border-color: var(--theme-container-border-color);' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dl.entry-tag div {' ).
lo_buf->add( '  border-color: hsl(206, 20%, 75%);' ).
lo_buf->add( '  background-color: hsl(206, 20%, 90%);' ).
lo_buf->add( '}' ).
lo_buf->add( '.db-entry dt {' ).
lo_buf->add( '  background-color: hsl(206, 20%, 75%);' ).
lo_buf->add( '}' ).
lo_buf->add( '' ).
    gui_services( )->register_page_asset(
      iv_url       = c_css_url
      iv_type      = 'text/css'
      iv_mime_name = 'ZABAPGIT_CSS_PAGE_DB_ENTRY'
      iv_inline    = lo_buf->join_w_newline_and_flush( ) ).

  ENDMETHOD.
  METHOD render_edit.

    DATA lv_formatted TYPE string.

    lv_formatted = escape(
      val    = Lcl_abapgit_xml_pretty=>print( iv_raw_db_value )
      format = cl_abap_format=>e_html_attr ).

    " Form
    ii_html->add( |<form id="{ c_edit_form_id }" method="post" action="sapevent:{ c_action-update }">| ).
    ii_html->add( |<input type="hidden" name="type" value="{ ms_key-type }">| ).
    ii_html->add( |<input type="hidden" name="value" value="{ ms_key-value }">| ).
    ii_html->add( |<textarea rows="20" cols="100" name="xmldata">{ lv_formatted }</textarea>| ).
    ii_html->add( '</form>' ).

  ENDMETHOD.
  METHOD render_entry_tag.

    rv_html =
      |<dl class="entry-tag">| &&
      |<div><dt>Type</dt><dd>{ is_key-type }</dd></div>| &&
      |<div><dt>Key</dt><dd>{ is_key-value }</dd></div>| &&
      |</dl>|.

  ENDMETHOD.
  METHOD render_header.

    ii_html->add( '<div class="toolbar">' ).
    ii_html->add( io_toolbar->render( iv_right = abap_true ) ).
    ii_html->add( render_entry_tag( ms_key ) ).
    ii_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_view.

    DATA lo_highlighter TYPE REF TO Lcl_abapgit_syntax_highlighter.
    DATA lv_formatted   TYPE string.

    " Create syntax highlighter
    lo_highlighter = Lcl_abapgit_syntax_factory=>create( '*.xml' ).
    lv_formatted   = lo_highlighter->process_line( Lcl_abapgit_xml_pretty=>print( iv_raw_db_value ) ).

    ii_html->add( |<pre class="syntax-hl">{ lv_formatted }</pre>| ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_action-switch_mode.
        mv_edit_mode = boolc( mv_edit_mode = abap_false ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN c_action-update.
        do_update( dbcontent_decode( ii_event->form_data( ) ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_page_title~get_page_title.

    IF mv_edit_mode = abap_true.
      rv_title = 'Config Edit'.
    ELSE.
      rv_title = 'Config Display'.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA lv_raw_db_value TYPE Lif_abapgit_persistence=>ty_content-data_str.

    register_handlers( ).

    TRY.
        lv_raw_db_value = Lcl_abapgit_persistence_db=>get_instance( )->read(
          iv_type  = ms_key-type
          iv_value = ms_key-value ).
      CATCH Lcx_abapgit_not_found ##NO_HANDLER.
    ENDTRY.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="db-entry">' ).

    render_header(
      ii_html    = ri_html
      io_toolbar = build_toolbar( ) ).

    IF mv_edit_mode = abap_true.
      Lcl_abapgit_persistence_db=>get_instance( )->lock(
        iv_type  = ms_key-type
        iv_value = ms_key-value ).
      render_edit(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ELSE.
      render_view(
        iv_raw_db_value = lv_raw_db_value
        ii_html         = ri_html ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DB_ENTRY implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DEBUGINFO <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_debuginfoccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_debuginfoccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_DEBUGINFO implementation.
*"* method's implementations
*include methods.
  METHOD build_toolbar.

    CREATE OBJECT ro_menu EXPORTING iv_id = 'toolbar-debug'.

    ro_menu->add(
      iv_txt = 'Save'
      iv_act = c_action-save ).
    ro_menu->add(
      iv_txt = 'Back'
      iv_act = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_debuginfo.

    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Debug Info'
      io_page_menu       = build_toolbar( )
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_jump_object.

    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    CREATE OBJECT li_html TYPE Lcl_abapgit_html.

    lv_encode = Lcl_abapgit_html_action_utils=>jump_encode( iv_obj_type = |{ iv_obj_type }|
                                                            iv_obj_name = |{ iv_obj_name }| ).

    rv_html = li_html->a(
      iv_txt = |{ iv_obj_name }|
      iv_act = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_encode }| ).

  ENDMETHOD.
  METHOD render_debug_info.

    DATA: ls_release       TYPE Lif_abapgit_environment=>ty_release_sp,
          lv_gui_version   TYPE string,
          lv_devclass      TYPE devclass,
          lo_frontend_serv TYPE REF TO Lif_abapgit_frontend_services.

    lo_frontend_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).
    TRY.
        lo_frontend_serv->get_gui_version( IMPORTING ev_gui_version_string = lv_gui_version ).
      CATCH Lcx_abapgit_exception ##NO_HANDLER.
        " Continue rendering even if this fails
    ENDTRY.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      ri_html->add( '<h2>abapGit - Standalone Version</h2>' ).
      ri_html->add( '<div>To keep abapGit up-to-date (or also to contribute) you need to' ).
      ri_html->add( |install it as a repository ({ ri_html->a(
        iv_txt = 'Developer Version'
        iv_act = 'https://github.com/abapGit/abapGit'
        iv_typ = Lif_abapgit_html=>c_action_type-url ) }).</div>| ).
    ELSE.
      lv_devclass = Lcl_abapgit_services_abapgit=>is_installed( ).
      ri_html->add( '<h2>abapGit - Developer Version</h2>' ).
      ri_html->add( |<div>abapGit is installed in package { lv_devclass }</div>| ).
    ENDIF.

    ri_html->add( '<br><div>' ).
    ri_html->add_a(
      iv_txt = 'Contribution guidelines for abapGit'
      iv_act = 'https://github.com/abapGit/abapGit/blob/main/CONTRIBUTING.md'
      iv_typ = Lif_abapgit_html=>c_action_type-url ).
    ri_html->add( '</div>' ).

    ls_release = Lcl_abapgit_factory=>get_environment( )->get_basis_release( ).

    ri_html->add( '<h2>Environment</h2>' ).

    ri_html->add( |<table>| ).
    ri_html->add( |<tr><td>abapGit version:</td><td>{ Lif_abapgit_version=>c_abap_version }</td></tr>| ).
    ri_html->add( |<tr><td>XML version:    </td><td>{ Lif_abapgit_version=>c_xml_version }</td></tr>| ).
    ri_html->add( |<tr><td>GUI version:    </td><td>{ lv_gui_version }</td></tr>| ).
    ri_html->add( |<tr><td>APACK version:  </td><td>{
                  Lcl_abapgit_apack_migration=>c_apack_interface_version }</td></tr>| ).
    ri_html->add( |<tr><td>LCL_TIME:       </td><td>{ Lcl_abapgit_git_time=>get_unix( ) }</td></tr>| ).
    ri_html->add( |<tr><td>SY time:        </td><td>{ sy-datum } { sy-uzeit } { sy-tzone }</td></tr>| ).
    ri_html->add( |<tr><td>SY release:     </td><td>{ ls_release-release } SP { ls_release-sp }</td></tr>| ).
    ri_html->add( |</table>| ).
    ri_html->add( |<br>| ).

  ENDMETHOD.
  METHOD render_exit_info.

    DATA lt_source TYPE string_table.
    DATA ls_class_key TYPE seoclskey.
    DATA lo_oo_serializer TYPE REF TO Lcl_abapgit_oo_serializer.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<h2>User Exits</h2>' ).

    IF Lcl_abapgit_factory=>get_environment( )->is_merged( ) = abap_true.
      " Standalone version
      lt_source = Lcl_abapgit_factory=>get_sap_report( )->read_report( c_exit_standalone ).
      IF sy-subrc = 0.
        ri_html->add( |<div>User exits are active (include { get_jump_object(
          iv_obj_type = 'PROG'
          iv_obj_name = c_exit_standalone ) } found)</div><br>| ).
        ri_html->add( render_exit_info_methods( lt_source ) ).
      ELSE.
        ri_html->add( |<div>No user exits implemented (include { c_exit_standalone } not found)</div><br>| ).
      ENDIF.
    ELSE.
      " Developer version
      TRY.
          ls_class_key-clsname = c_exit_class.
          CREATE OBJECT lo_oo_serializer.
          lt_source = lo_oo_serializer->serialize_abap_clif_source( ls_class_key ).

          ri_html->add( |<div>User exits are active (class { get_jump_object( c_exit_class ) } found)</div><br>| ).
          ri_html->add( render_exit_info_methods( lt_source ) ).
        CATCH cx_root.
          ri_html->add( |<div>No user exits implemented (class { c_exit_class } not found)</div><br>| ).
      ENDTRY.
    ENDIF.

  ENDMETHOD.
  METHOD render_exit_info_methods.

    DATA:
      lo_scanner TYPE REF TO cl_oo_source_scanner_class,
      lx_exc     TYPE REF TO cx_root,
      lt_methods TYPE cl_oo_source_scanner_class=>type_method_implementations,
      lv_method  LIKE LINE OF lt_methods,
      lt_source  TYPE seop_source_string,
      lv_source  TYPE string,
      lv_rest    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<table border="1px"><thead><tr>' ).
    ri_html->add( '<td>Exit</td><td class="center">Implemented?</td>' ).
    ri_html->add( '</tr></thead><tbody>' ).

    TRY.
        lo_scanner = cl_oo_source_scanner_class=>create_class_scanner(
          clif_name = c_exit_class
          source    = it_source ).
        lo_scanner->scan( ).

        lt_methods = lo_scanner->get_method_implementations( ).

        LOOP AT lt_methods INTO lv_method WHERE table_line CS c_exit_interface.
          lt_source = lo_scanner->get_method_impl_source( lv_method ).
          DELETE lt_source INDEX 1.
          DELETE lt_source INDEX lines( lt_source ).
          CONCATENATE LINES OF lt_source INTO lv_source.
          lv_source = to_upper( condense(
            val = lv_source
            del = ` ` ) ).
          SPLIT lv_method AT '~' INTO lv_rest lv_method.
          ri_html->add( |<tr><td>{ lv_method }</td><td class="center">| ).
          IF lv_source IS INITIAL OR lv_source = 'RETURN.' OR lv_source = 'EXIT.'.
            ri_html->add( 'No' ).
          ELSE.
            ri_html->add( '<strong>Yes</strong>' ).
          ENDIF.
          ri_html->add( |</td></tr>| ).
        ENDLOOP.

      CATCH cx_root INTO lx_exc.
        ri_html->add( |<tr><td colspan="2">{ lx_exc->get_text( ) }</td></tr>| ).
    ENDTRY.

    ri_html->add( '</tbody></table>' ).

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'debugOutput("<table><tr><td>Browser:</td><td>" + navigator.userAgent + ' &&
      '"</td></tr><tr><td>Frontend time:</td><td>" + new Date() + "</td></tr></table>", "debug_info");' ).

  ENDMETHOD.
  METHOD render_supported_object_types.

    DATA: lv_list     TYPE string,
          li_html     TYPE REF TO Lif_abapgit_html,
          lt_types    TYPE Lcl_abapgit_objects=>ty_types_tt,
          lv_type     LIKE LINE OF lt_types,
          lt_obj      TYPE STANDARD TABLE OF ko100 WITH DEFAULT KEY,
          lv_class    TYPE seoclsname,
          li_object   TYPE REF TO Lif_abapgit_object,
          ls_item     TYPE Lif_abapgit_definitions=>ty_item,
          ls_metadata TYPE Lif_abapgit_definitions=>ty_metadata,
          lv_step     TYPE Lif_abapgit_definitions=>ty_deserialization_step,
          lt_steps    TYPE Lif_abapgit_definitions=>ty_deserialization_step_tt.

    FIELD-SYMBOLS: <ls_obj> TYPE ko100.

    CALL FUNCTION 'TR_OBJECT_TABLE'
      TABLES
        wt_object_text = lt_obj.

    lt_types = Lcl_abapgit_objects=>supported_list( ).

    CREATE OBJECT li_html TYPE Lcl_abapgit_html.

    rv_html = '<h2>Object Types</h2>'.

    rv_html = rv_html && li_html->a(
      iv_txt = 'Complete list of object types supported by abapGit'
      iv_act = 'https://docs.abapgit.org/ref-supported.html'
      iv_typ = Lif_abapgit_html=>c_action_type-url ).

    rv_html = rv_html && |<br><br>Supported object types in <strong>this</strong> system:<br><br>|.

    rv_html = rv_html && |<table border="1px"><thead><tr>|.
    rv_html = rv_html && |<td>Object</td><td>Description</td><td>Class</td><td>Version</td>|.
    rv_html = rv_html && |<td>Steps</td>|.
    rv_html = rv_html && |</tr></thead><tbody>|.

    LOOP AT lt_types INTO lv_type.
      lv_class = 'LCL_ABAPGIT_OBJECT_' && lv_type.

      rv_html = rv_html && |<tr>|.

      rv_html = rv_html && |<td>{ lv_type }</td>|.

      READ TABLE lt_obj ASSIGNING <ls_obj> WITH KEY pgmid = 'R3TR' object = lv_type.
      IF sy-subrc = 0.
        rv_html = rv_html && |<td>{ <ls_obj>-text }</td>|.
      ELSE.
        rv_html = rv_html && |<td class="warning">No description</td>|.
      ENDIF.


      TRY.
          ls_item-obj_type = lv_type.
          ls_item-obj_name = 'TEST'.

          CREATE OBJECT li_object TYPE (lv_class)
            EXPORTING
              is_item     = ls_item
              iv_language = sy-langu.

          rv_html = rv_html && |<td>{ get_jump_object( lv_class ) }</td>|.

        CATCH cx_sy_create_object_error.
          TRY. " 2nd step, try looking for plugins
              CREATE OBJECT li_object TYPE Lcl_abapgit_objects_bridge
                EXPORTING
                  is_item = ls_item.
            CATCH cx_sy_create_object_error.
              rv_html = rv_html && |<td class="error" colspan="5">{ lv_class } - error instantiating class</td>|.
              CONTINUE.
          ENDTRY.

          rv_html = rv_html && |<td>{ get_jump_object( lv_class ) } (Plug-in)</td>|.
      ENDTRY.

      ls_metadata = li_object->get_metadata( ).

      rv_html = rv_html && |<td>{ ls_metadata-version }</td>|.

      lt_steps = li_object->get_deserialize_steps( ).

      CLEAR lv_list.
      LOOP AT lt_steps INTO lv_step.
        CASE lv_step.
          WHEN Lif_abapgit_object=>gc_step_id-early.
            lv_step = |<i>{ lv_step } (1)</i>|.
          WHEN Lif_abapgit_object=>gc_step_id-ddic.
            lv_step = |<strong>{ lv_step } (2)</strong>|.
          WHEN Lif_abapgit_object=>gc_step_id-abap.
            lv_step = |{ lv_step } (3)|.
          WHEN Lif_abapgit_object=>gc_step_id-late.
            lv_step = |<i>{ lv_step } (4)</i>|.
          WHEN OTHERS.
            ASSERT 1 = 2.
        ENDCASE.
        IF lv_list IS INITIAL.
          lv_list = lv_step.
        ELSE.
          lv_list = lv_list && `, ` && lv_step.
        ENDIF.
      ENDLOOP.

      rv_html = rv_html && |<td>{ lv_list }</td>|.

      rv_html = rv_html && |</tr>|.

    ENDLOOP.

    rv_html = rv_html && |</tbody></table>|.
    rv_html = rv_html && |<br>|.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA:
      lv_path     TYPE string,
      lv_filename TYPE string,
      li_fe_serv  TYPE REF TO Lif_abapgit_frontend_services.

    CASE ii_event->mv_action.
      WHEN c_action-save.

        CONCATENATE 'abapGit_Debug_Info_' sy-datlo '_' sy-timlo '.html' INTO lv_filename.

        li_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

        lv_path = li_fe_serv->show_file_save_dialog(
          iv_title            = 'abapGit - Debug Info'
          iv_extension        = 'html'
          iv_default_filename = lv_filename ).

        li_fe_serv->file_download(
          iv_path = lv_path
          iv_xstr = Lcl_abapgit_convert=>string_to_xstring_utf8( mv_html ) ).

        MESSAGE 'abapGit Debug Info successfully saved' TYPE 'S'.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="debug_info" class="debug_container">' ).
    ri_html->add( render_debug_info( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="exit_info" class="debug_container">' ).
    ri_html->add( render_exit_info( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '<div id="supported_objects" class="debug_container">' ).
    ri_html->add( render_supported_object_types( ) ).
    ri_html->add( '</div>' ).

    mv_html = '<!DOCTYPE html><html lang="en"><title>abapGit Debug Info</title></head>'.
    mv_html = |<body>{ ri_html->render( ) }</body></html>|.

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DEBUGINFO implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DIFF <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_diff=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_diff=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_DIFF implementation.
*"* method's implementations
*include methods.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_diff.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key    = iv_key
        is_file   = is_file
        is_object = is_object
        it_files  = it_files.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Diff'
      iv_page_layout        = get_page_layout( )
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DIFF implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_DIFF_BASE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_diff_baseccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_diff_baseccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_DIFF_BASE implementation.
*"* method's implementations
*include methods.
  METHOD add_filter_sub_menu.

    DATA:
      lo_sub_filter TYPE REF TO Lcl_abapgit_html_toolbar,
      lv_user       TYPE string,
      lt_extensions TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY,
      lt_obj_types  TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY,
      lt_users      TYPE SORTED TABLE OF string WITH UNIQUE DEFAULT KEY.

    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff_files,
                   <lv_i>    TYPE string.

    " Get unique filter values
    LOOP AT mt_diff_files ASSIGNING <ls_diff>.
      lv_user = <ls_diff>-changed_by.
      INSERT <ls_diff>-type INTO TABLE lt_extensions.
      INSERT <ls_diff>-obj_type INTO TABLE lt_obj_types.
      INSERT lv_user INTO TABLE lt_users.
    ENDLOOP.

    IF lines( lt_extensions ) > 1 OR lines( lt_obj_types ) > 1 OR lines( lt_users ) > 1.
      CREATE OBJECT lo_sub_filter EXPORTING iv_id = 'diff-filter'.

      IF lines( lt_users ) > 1.
        lo_sub_filter->add( iv_txt = 'Only my changes'
                            iv_typ = Lif_abapgit_html=>c_action_type-onclick
                            iv_aux = |{ sy-uname }|
                            iv_chk = abap_false ).
      ENDIF.

      " File extensions
      IF lines( lt_extensions ) > 1.
        lo_sub_filter->add( iv_txt = 'Extension'
                            iv_typ = Lif_abapgit_html=>c_action_type-separator ).
        LOOP AT lt_extensions ASSIGNING <lv_i>.
          lo_sub_filter->add( iv_txt = <lv_i>
                       iv_typ = Lif_abapgit_html=>c_action_type-onclick
                       iv_aux = 'extension'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      " Object types
      IF lines( lt_obj_types ) > 1.
        lo_sub_filter->add( iv_txt = 'Object Type'
                            iv_typ = Lif_abapgit_html=>c_action_type-separator ).
        LOOP AT lt_obj_types ASSIGNING <lv_i>.
          lo_sub_filter->add( iv_txt = <lv_i>
                       iv_typ = Lif_abapgit_html=>c_action_type-onclick
                       iv_aux = 'object-type'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      " Changed by
      IF lines( lt_users ) > 1.
        lo_sub_filter->add( iv_txt = 'Changed By'
                            iv_typ = Lif_abapgit_html=>c_action_type-separator ).
        LOOP AT lt_users ASSIGNING <lv_i>.
          lo_sub_filter->add( iv_txt = <lv_i>
                       iv_typ = Lif_abapgit_html=>c_action_type-onclick
                       iv_aux = 'changed-by'
                       iv_chk = abap_true ).
        ENDLOOP.
      ENDIF.

      io_menu->add( iv_txt = 'Filter'
                    io_sub = lo_sub_filter ).
    ENDIF.

  ENDMETHOD.
  METHOD add_jump_sub_menu.

    DATA: lo_sub_jump    TYPE REF TO Lcl_abapgit_html_toolbar,
          lv_jump_target TYPE string.
    FIELD-SYMBOLS: <ls_diff> LIKE LINE OF mt_diff_files.

    CREATE OBJECT lo_sub_jump EXPORTING iv_id = 'jump'.

    LOOP AT mt_diff_files ASSIGNING <ls_diff>.

      lv_jump_target = <ls_diff>-path && <ls_diff>-filename.

      lo_sub_jump->add(
          iv_id  = |li_jump_{ sy-tabix }|
          iv_txt = lv_jump_target
          iv_typ = Lif_abapgit_html=>c_action_type-onclick ).

    ENDLOOP.

    io_menu->add( iv_txt = 'Jump'
                  io_sub = lo_sub_jump ).

  ENDMETHOD.
  METHOD add_menu_begin.

    io_menu->add(
        iv_txt   = c_action_texts-refresh_local
        iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
        iv_act   = c_actions-refresh_local
        iv_id    = c_actions-refresh_local
        iv_title = c_action_titles-refresh_local ).

    io_menu->add(
        iv_txt   = c_action_texts-refresh_all
        iv_typ   = Lif_abapgit_html=>c_action_type-sapevent
        iv_act   = c_actions-refresh_all
        iv_id    = c_actions-refresh_all
        iv_title = c_action_titles-refresh_all ).

  ENDMETHOD.
  METHOD add_menu_end.

    io_menu->add( iv_txt = 'Split/Unified'
                  iv_act = c_actions-toggle_unified ).

    add_view_sub_menu( io_menu ).

  ENDMETHOD.
  METHOD add_view_sub_menu.

    DATA lo_sub_view TYPE REF TO Lcl_abapgit_html_toolbar.
    DATA lv_txt TYPE string.

    CREATE OBJECT lo_sub_view EXPORTING iv_id = 'diff-view'.

    IF ms_view-hide_diffs = abap_true.
      lv_txt = 'Expand All Diffs'.
    ELSE.
      lv_txt = 'Collapse All Diffs'.
    ENDIF.

    lo_sub_view->add( iv_txt = lv_txt
                      iv_act = c_actions-toggle_hide_diffs ).

    lo_sub_view->add( iv_txt = 'Show Hidden Characters'
                      iv_act = c_actions-toggle_hidden_chars
                      iv_chk = ms_view-hidden_chars ).

    lo_sub_view->add( iv_txt = 'Ignore Whitespace'
                      iv_act = c_actions-toggle_ignore_indent
                      iv_chk = ms_view-ignore_indent ).

    lo_sub_view->add( iv_txt = 'Ignore Comments'
                      iv_act = c_actions-toggle_ignore_comments
                      iv_chk = ms_view-ignore_comments ).

    lo_sub_view->add( iv_txt = 'Ignore Pretty-Print Case'
                      iv_act = c_actions-toggle_ignore_case
                      iv_chk = ms_view-ignore_case ).

    io_menu->add( iv_txt = 'View'
                  io_sub = lo_sub_view ).

  ENDMETHOD.
  METHOD append_diff.

    DATA:
      lv_offs    TYPE i,
      ls_r_dummy LIKE LINE OF it_remote ##NEEDED,
      ls_l_dummy LIKE LINE OF it_local  ##NEEDED.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF it_remote,
                   <ls_local>  LIKE LINE OF it_local,
                   <ls_diff>   LIKE LINE OF mt_diff_files.


    READ TABLE it_remote ASSIGNING <ls_remote>
      WITH KEY file_path
       COMPONENTS path     = is_status-path
                  filename = is_status-filename.
    IF sy-subrc <> 0.
      ASSIGN ls_r_dummy TO <ls_remote>.
    ENDIF.

    READ TABLE it_local ASSIGNING <ls_local>
      WITH KEY file-filename = is_status-filename
               file-path     = is_status-path.
    IF sy-subrc <> 0.
      ASSIGN ls_l_dummy TO <ls_local>.
    ENDIF.

    IF <ls_local> IS INITIAL AND <ls_remote> IS INITIAL.
      Lcx_abapgit_exception=>raise( |DIFF: file not found { is_status-filename }| ).
    ENDIF.

    APPEND INITIAL LINE TO mt_diff_files ASSIGNING <ls_diff>.
    <ls_diff>-path     = is_status-path.
    <ls_diff>-filename = is_status-filename.
    <ls_diff>-obj_type = is_status-obj_type.
    <ls_diff>-obj_name = is_status-obj_name.
    <ls_diff>-lstate   = is_status-lstate.
    <ls_diff>-rstate   = is_status-rstate.

    IF <ls_diff>-lstate IS NOT INITIAL AND <ls_diff>-rstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-both.
    ELSEIF <ls_diff>-lstate IS NOT INITIAL.
      <ls_diff>-fstate = c_fstate-local.
    ELSE. "rstate IS NOT INITIAL, lstate = empty.
      <ls_diff>-fstate = c_fstate-remote.
    ENDIF.

    " Changed by
    IF <ls_local>-item-obj_type IS NOT INITIAL.
      <ls_diff>-changed_by = Lcl_abapgit_objects=>changed_by(
        is_item     = <ls_local>-item
        iv_filename = is_status-filename ).
    ENDIF.
    IF <ls_diff>-changed_by IS INITIAL.
      <ls_diff>-changed_by = Lcl_abapgit_objects_super=>c_user_unknown.
    ENDIF.

    " Extension
    IF <ls_local>-file-filename IS NOT INITIAL.
      <ls_diff>-type = reverse( <ls_local>-file-filename ).
    ELSE.
      <ls_diff>-type = reverse( <ls_remote>-filename ).
    ENDIF.

    FIND FIRST OCCURRENCE OF '.' IN <ls_diff>-type MATCH OFFSET lv_offs.
    <ls_diff>-type = reverse( substring( val = <ls_diff>-type
                                         len = lv_offs ) ).
    IF <ls_diff>-type <> 'xml' AND <ls_diff>-type <> 'abap'.
      <ls_diff>-type = 'other'.
    ENDIF.

    IF <ls_diff>-type = 'other'
       AND is_binary( iv_d1 = <ls_remote>-data
                      iv_d2 = <ls_local>-file-data ) = abap_true.
      <ls_diff>-type = 'binary'.
    ENDIF.

    " Diff data
    IF <ls_diff>-type <> 'binary'.
      IF <ls_diff>-fstate = c_fstate-remote. " Remote file leading changes
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new                = <ls_remote>-data
            iv_old                = <ls_local>-file-data
            iv_ignore_indentation = ms_view-ignore_indent
            iv_ignore_comments    = ms_view-ignore_comments
            iv_ignore_case        = ms_view-ignore_case.
      ELSE.             " Local leading changes or both were modified
        CREATE OBJECT <ls_diff>-o_diff
          EXPORTING
            iv_new                = <ls_local>-file-data
            iv_old                = <ls_remote>-data
            iv_ignore_indentation = ms_view-ignore_indent
            iv_ignore_comments    = ms_view-ignore_comments
            iv_ignore_case        = ms_view-ignore_case.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD calculate_diff.

    DATA: lt_remote TYPE Lif_abapgit_git_definitions=>ty_files_tt,
          lt_local  TYPE Lif_abapgit_definitions=>ty_files_item_tt,
          lt_status TYPE Lif_abapgit_definitions=>ty_results_tt.

    DATA li_exit TYPE REF TO Lif_abapgit_exit.

    FIELD-SYMBOLS: <ls_status> LIKE LINE OF lt_status.

    CLEAR: mt_diff_files.

    lt_remote = mo_repo->get_files_remote( ).
    lt_local  = mo_repo->get_files_local( ).

    lt_status = Lcl_abapgit_repo_status=>calculate( mo_repo ).

    li_exit = Lcl_abapgit_exit=>get_instance( ).
    li_exit->pre_calculate_repo_status(
      EXPORTING
        is_repo_meta = mo_repo->ms_data
      CHANGING
        ct_local  = lt_local
        ct_remote = lt_remote ).

    IF is_file IS NOT INITIAL.        " Diff for one file

      READ TABLE lt_status ASSIGNING <ls_status>
        WITH KEY path = is_file-path filename = is_file-filename.
      IF sy-subrc <> 0.
        Lcx_abapgit_exception=>raise( |File { is_file-path }{ is_file-filename } not found| ).
      ENDIF.

      append_diff( it_remote = lt_remote
                   it_local  = lt_local
                   is_status = <ls_status> ).

    ELSEIF is_object IS NOT INITIAL.  " Diff for whole object

      LOOP AT lt_status ASSIGNING <ls_status>
          USING KEY sec_key
          WHERE obj_type = is_object-obj_type
          AND obj_name = is_object-obj_name
          AND match IS INITIAL.
        append_diff( it_remote = lt_remote
                     it_local  = lt_local
                     is_status = <ls_status> ).
      ENDLOOP.

    ELSE.                             " Diff for the whole repo

      SORT lt_status BY
        path ASCENDING
        filename ASCENDING.
      LOOP AT lt_status ASSIGNING <ls_status> WHERE match IS INITIAL.

        IF is_file_requested( it_files  = it_files
                              is_status = <ls_status> ) = abap_true.

          append_diff( it_remote = lt_remote
                       it_local  = lt_local
                       is_status = <ls_status> ).

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    DATA: lv_ts TYPE timestamp.

    super->constructor( ).
    mv_unified  = Lcl_abapgit_persistence_user=>get_instance( )->get_diff_unified( ).
    mv_repo_key = iv_key.
    mo_repo    ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

    GET TIME STAMP FIELD lv_ts.
    mv_seed = |diff{ lv_ts }|. " Generate based on time

    ASSERT is_file IS INITIAL OR is_object IS INITIAL. " just one passed

    calculate_diff(
        is_file   = is_file
        is_object = is_object
        it_files  = it_files ).

    IF lines( mt_diff_files ) = 0.
      Lcx_abapgit_exception=>raise(
        'There are no differences to show. The local state completely matches the remote repository.' ).
    ENDIF.

  ENDMETHOD.
  METHOD get_normalized_fname_with_path.

    rv_filename = normalize_path( is_diff-path )
               && `_`
               && normalize_filename( is_diff-filename ).

  ENDMETHOD.
  METHOD get_page_layout.

    TRY.
        IF Lcl_abapgit_persistence_user=>get_instance( )->get_diff_unified( ) = abap_true.
          rv_page_layout = Lcl_abapgit_gui_page=>c_page_layout-centered.
        ELSE.
          rv_page_layout = Lcl_abapgit_gui_page=>c_page_layout-full_width.
        ENDIF.
      CATCH Lcx_abapgit_exception.
        rv_page_layout = Lcl_abapgit_gui_page=>c_page_layout-full_width.
    ENDTRY.

  ENDMETHOD.
  METHOD has_diffs.

    LOOP AT it_diffs TRANSPORTING NO FIELDS WHERE result IS NOT INITIAL.
      rv_has_diffs = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.
  METHOD insert_nav.

  ENDMETHOD.
  METHOD is_binary.

    FIELD-SYMBOLS <lv_data> LIKE iv_d1.

    IF iv_d1 IS NOT INITIAL. " One of them might be new and so empty
      ASSIGN iv_d1 TO <lv_data>.
    ELSE.
      ASSIGN iv_d2 TO <lv_data>.
    ENDIF.

    rv_yes = Lcl_abapgit_utils=>is_binary( <lv_data> ).

  ENDMETHOD.
  METHOD is_file_requested.

    IF lines( it_files ) = 0.
      rv_is_file_requested = abap_true.
      RETURN.
    ENDIF.

    READ TABLE it_files WITH KEY file-path     = is_status-path
                                 file-filename = is_status-filename
                        TRANSPORTING NO FIELDS.
    rv_is_file_requested = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD is_refresh.

    FIND FIRST OCCURRENCE OF REGEX |^{ c_actions-refresh_prefix }| IN iv_action.
    rv_is_refrseh = boolc( sy-subrc = 0 ).

  ENDMETHOD.
  METHOD modify_files_before_diff_calc.

    DATA ls_file LIKE LINE OF rt_files.

    FIELD-SYMBOLS <ls_diff_file_old> TYPE ty_file_diff.

    " We need to supply files again in calculate_diff. Because
    " we only want to refresh the visible files. Otherwise all
    " diff files would appear.
    " Which is not wanted when we previously only selected particular files.
    LOOP AT it_diff_files_old ASSIGNING <ls_diff_file_old>.
      CLEAR ls_file.
      MOVE-CORRESPONDING <ls_diff_file_old> TO ls_file-file.
      INSERT ls_file INTO TABLE rt_files.
    ENDLOOP.

  ENDMETHOD.
  METHOD normalize_filename.

    rv_normalized = replace( val  = iv_filename
                             sub  = '.'
                             occ  = 0
                             with = '_' ).

  ENDMETHOD.
  METHOD normalize_path.

    rv_normalized = replace( val  = iv_path
                             sub  = '/'
                             occ  = 0
                             with = '_' ).

  ENDMETHOD.
  METHOD refresh.

    DATA:
      lt_diff_files_old TYPE ty_file_diffs,
      lt_files          TYPE Lif_abapgit_definitions=>ty_stage_tt.


    lt_diff_files_old = mt_diff_files.

    CASE iv_action.
      WHEN c_actions-refresh_all.
        refresh_full( ).
      WHEN c_actions-refresh_local.
        refresh_local( ).
      WHEN OTHERS.
        refresh_local_object( iv_action ).
    ENDCASE.

    lt_files = modify_files_before_diff_calc( lt_diff_files_old ).

    calculate_diff( it_files = lt_files ).

  ENDMETHOD.
  METHOD refresh_full.
    mo_repo->refresh( abap_true ).
  ENDMETHOD.
  METHOD refresh_local.
    mo_repo->refresh_local_objects( ).
  ENDMETHOD.
  METHOD refresh_local_object.

    DATA:
      lv_regex    TYPE string,
      lv_obj_type TYPE tadir-object,
      lv_obj_name TYPE tadir-obj_name.

    lv_regex = c_actions-refresh_local_object && `_(\w{4})_(.*)`.

    FIND FIRST OCCURRENCE OF REGEX lv_regex
      IN iv_action
      SUBMATCHES lv_obj_type lv_obj_name.

    IF sy-subrc = 0.
      mo_repo->refresh_local_object(
        iv_obj_type = to_upper( lv_obj_type )
        iv_obj_name = to_upper( lv_obj_name ) ).
    ELSE.
      Lcx_abapgit_exception=>raise( |Invalid refresh action { iv_action }| ).
    ENDIF.

  ENDMETHOD.
  METHOD render_beacon.

    DATA: lv_beacon  TYPE string,
          lt_beacons TYPE Lif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF is_diff_line-beacon > 0.
      lt_beacons = is_diff-o_diff->get_beacons( ).
      READ TABLE lt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.

    ri_html->add( '<thead class="nav_line">' ).
    ri_html->add( '<tr>' ).

    render_beacon_begin_of_row(
      ii_html = ri_html
      is_diff = is_diff ).

    IF mv_unified = abap_true.
      ri_html->add( '<th class="num"></th>' ).
      ri_html->add( '<th class="mark"></th>' ).
      ri_html->add( |<th>@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ELSE.
      ri_html->add( |<th colspan="6">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).
    ENDIF.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
  METHOD render_beacon_begin_of_row.

    ii_html->add( '<th class="num"></th>' ).

  ENDMETHOD.
  METHOD render_diff.

    DATA lv_display TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<div class="diff" data-extension="{ is_diff-type
      }" data-object-type="{ is_diff-obj_type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ).
    ri_html->add( render_diff_head( is_diff ) ).

    " Content
    IF ms_view-hide_diffs = abap_true.
      lv_display = ' nodisplay'.
    ENDIF.

    IF is_diff-type <> 'binary'.
      ri_html->add( |<div class="diff_content{ lv_display }">| ).
      ri_html->add( |<table class="diff_tab syntax-hl" id="{ is_diff-filename }">| ).
      ri_html->add( render_table_head( is_diff ) ).
      ri_html->add( render_lines( is_diff ) ).
      ri_html->add( '</table>' ).
    ELSE.
      ri_html->add( '<div class="diff_content paddings center grey">' ).
      ri_html->add( 'The content seems to be binary.' ).
      ri_html->add( 'Cannot display as diff.' ).
    ENDIF.
    ri_html->add( '</div>' ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_diff_head.

    DATA: ls_stats TYPE Lif_abapgit_definitions=>ty_count,
          lv_icon  TYPE string,
          lv_jump  TYPE string,
          lv_link  TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="diff_head">' ).

    IF ms_view-hide_diffs = abap_true.
      lv_icon = 'chevron-right'.
    ELSE.
      lv_icon = 'chevron-down'.
    ENDIF.

    ri_html->add_icon(
      iv_name    = lv_icon
      iv_hint    = 'Collapse/Expand'
      iv_class   = 'cursor-pointer'
      iv_onclick = 'onDiffCollapse(event)' ).

    IF is_diff-type <> 'binary'.
      ls_stats = is_diff-o_diff->stats( ).
      IF is_diff-fstate = c_fstate-both. " Merge stats into 'update' if both were changed
        ls_stats-update = ls_stats-update + ls_stats-insert + ls_stats-delete.
        CLEAR: ls_stats-insert, ls_stats-delete.
      ENDIF.

      ri_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ri_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ri_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    " no links for nonexistent or deleted objects
    IF NOT ( is_diff-lstate = Lif_abapgit_definitions=>c_state-unchanged AND
             is_diff-rstate = Lif_abapgit_definitions=>c_state-added ) AND
         NOT is_diff-lstate = Lif_abapgit_definitions=>c_state-deleted.

      lv_jump = Lcl_abapgit_html_action_utils=>jump_encode(
        iv_obj_type = |{ is_diff-obj_type }|
        iv_obj_name = |{ is_diff-obj_name }|
        iv_filename = is_diff-filename ).

      lv_link = ri_html->a(
        iv_txt = |{ is_diff-path }{ is_diff-filename }|
        iv_act = |{ Lif_abapgit_definitions=>c_action-jump }?{ lv_jump }| ).
    ENDIF.

    IF lv_link IS NOT INITIAL.
      ri_html->add( |<span class="diff_name">{ lv_link }</span>| ).
    ELSE.
      ri_html->add( |<span class="diff_name">{ is_diff-path }{ is_diff-filename }</span>| ).
    ENDIF.

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state(
      iv_lstate = is_diff-lstate
      iv_rstate = is_diff-rstate ) ).

    render_diff_head_after_state(
      ii_html = ri_html
      is_diff = is_diff ).

    ri_html->add( '<span class="diff_changed_by">Last Changed by: ' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_user_name( is_diff-changed_by ) ).
    ri_html->add( '</span>' ).

    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_diff_head_after_state.

    IF is_diff-fstate = c_fstate-both AND mv_unified = abap_true.
      ii_html->add( '<span class="attention pad-sides">Attention: Unified mode'
                 && ' highlighting for MM assumes local file is newer ! </span>' ).
    ENDIF.

    IF is_diff-obj_type IS NOT INITIAL AND is_diff-obj_name IS NOT INITIAL.
      ii_html->add( '<span class="repo_name">' ).
      ii_html->add_a( iv_txt   = ii_html->icon( iv_name  = 'redo-alt-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Local refresh of this object' )
                      iv_act   = |{ c_actions-refresh_local_object }_{ is_diff-obj_type }_{ is_diff-obj_name }|
                      iv_class = |url| ).
      ii_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO Lcl_abapgit_syntax_highlighter,
          lt_diffs       TYPE Lif_abapgit_definitions=>ty_diffs_tt,
          lv_insert_nav  TYPE abap_bool,
          lv_tabix       TYPE syst-tabix.

    FIELD-SYMBOLS <ls_diff> LIKE LINE OF lt_diffs.
    FIELD-SYMBOLS <ls_diff_line> LIKE LINE OF lt_diffs.

    lo_highlighter = Lcl_abapgit_syntax_factory=>create( iv_filename     = is_diff-filename
                                                         iv_hidden_chars = ms_view-hidden_chars ).
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lt_diffs = is_diff-o_diff->get( ).

    IF has_diffs( lt_diffs ) = abap_false.
      ri_html->add( render_line_no_diffs( ) ).
      RETURN.
    ENDIF.

    lv_insert_nav = insert_nav( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.

      lv_tabix = sy-tabix.

      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        " Get line where diff really starts
        READ TABLE lt_diffs ASSIGNING <ls_diff_line> INDEX lv_tabix + 8.
        IF sy-subrc <> 0.
          " Occurs only for small files/diffs with less than 8 lines.
          " Therefore let's use the first line as beacon
          ASSIGN <ls_diff> TO <ls_diff_line>.
          ASSERT <ls_diff_line> IS ASSIGNED.
        ENDIF.
        ri_html->add( render_beacon( is_diff_line = <ls_diff_line>
                                     is_diff      = is_diff ) ).
        lv_insert_nav = abap_false.
      ENDIF.

      IF lo_highlighter IS BOUND.
        <ls_diff>-new = lo_highlighter->process_line( <ls_diff>-new ).
        <ls_diff>-old = lo_highlighter->process_line( <ls_diff>-old ).
      ELSE.
        <ls_diff>-new = escape( val    = <ls_diff>-new
                                format = cl_abap_format=>e_html_attr ).
        <ls_diff>-old = escape( val    = <ls_diff>-old
                                format = cl_abap_format=>e_html_attr ).
      ENDIF.

      CONDENSE <ls_diff>-new_num. "get rid of leading spaces
      CONDENSE <ls_diff>-old_num.

      IF mv_unified = abap_true.
        ri_html->add( render_line_unified( is_diff_line = <ls_diff> ) ).
      ELSE.
        ri_html->add( render_line_split( is_diff_line = <ls_diff>
                                         iv_filename  = get_normalized_fname_with_path( is_diff )
                                         iv_fstate    = is_diff-fstate
                                         iv_index     = lv_tabix ) ).
      ENDIF.

    ENDLOOP.

    IF mv_unified = abap_true.
      ri_html->add( render_line_unified( ) ). " Release delayed lines
    ENDIF.

  ENDMETHOD.
  METHOD render_line_no_diffs.

    DATA ls_diff_line TYPE Lif_abapgit_definitions=>ty_diff.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF mv_unified = abap_true.
      ls_diff_line-old = 'No diffs found'.
      ri_html->add( render_line_unified( is_diff_line = ls_diff_line ) ).
    ELSE.
      ls_diff_line-new = 'No diffs found'.
      ri_html->add( render_line_split( is_diff_line = ls_diff_line
                                       iv_filename  = ''
                                       iv_fstate    = ''
                                       iv_index     = 1 ) ).
    ENDIF.

  ENDMETHOD.
  METHOD render_line_split.

    DATA: lv_new  TYPE string,
          lv_old  TYPE string,
          lv_mark TYPE string,
          lv_bg   TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    " Note: CSS classes "new" and "old" are used to enable column-based copy to clipboard

    " New line
    lv_mark = ` `.
    IF is_diff_line-result IS NOT INITIAL.
      IF iv_fstate = c_fstate-both OR is_diff_line-result = Lif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff_line-result = Lif_abapgit_definitions=>c_diff-insert.
        lv_bg = ' diff_ins'.
        lv_mark = `+`.
      ENDIF.
    ENDIF.
    lv_new = |<td class="num diff_others" line-num="{ is_diff_line-new_num }"></td>|
          && |<td class="mark diff_others">{ lv_mark }</td>|
          && |<td class="code{ lv_bg } diff_left new">{ is_diff_line-new }</td>|.

    " Old line
    CLEAR lv_bg.
    lv_mark = ` `.
    IF is_diff_line-result IS NOT INITIAL.
      IF iv_fstate = c_fstate-both OR is_diff_line-result = Lif_abapgit_definitions=>c_diff-update.
        lv_bg = ' diff_upd'.
        lv_mark = `~`.
      ELSEIF is_diff_line-result = Lif_abapgit_definitions=>c_diff-delete.
        lv_bg = ' diff_del'.
        lv_mark = `-`.
      ENDIF.
    ENDIF.
    lv_old = |<td class="num diff_others" line-num="{ is_diff_line-old_num }"></td>|
          && |<td class="mark diff_others">{ lv_mark }</td>|
          && |<td class="code{ lv_bg } diff_right old">{ is_diff_line-old }</td>|.

    " render line, inverse sides if remote is newer
    ri_html->add( '<tr class="diff_line">' ).

    render_line_split_row(
        ii_html                = ri_html
        iv_filename            = iv_filename
        is_diff_line           = is_diff_line
        iv_index               = iv_index
        iv_fstate              = iv_fstate
        iv_old                 = lv_old
        iv_new                 = lv_new ).

    ri_html->add( '</tr>' ).

  ENDMETHOD.
  METHOD render_line_split_row.

    IF iv_fstate = c_fstate-remote. " Remote file leading changes
      ii_html->add( iv_old ). " local
      ii_html->add( iv_new ). " remote
    ELSE.             " Local leading changes or both were modified
      ii_html->add( iv_new ). " local
      ii_html->add( iv_old ). " remote
    ENDIF.

  ENDMETHOD.
  METHOD render_line_unified.

    FIELD-SYMBOLS <ls_diff_line> LIKE LINE OF mt_delayed_lines.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    " Note: CSS classes "new" and "old" are used to enable column-based copy to clipboard

    " Release delayed subsequent update lines
    IF is_diff_line-result <> Lif_abapgit_definitions=>c_diff-update.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ri_html->add( '<tr class="diff_line">' ).
        ri_html->add( |<td class="num diff_others" line-num="{ <ls_diff_line>-old_num }"></td>|
                   && |<td class="num diff_others" line-num=""></td>|
                   && |<td class="mark diff_others">-</td>|
                   && |<td class="code diff_del diff_unified old">{ <ls_diff_line>-old }</td>| ).
        ri_html->add( '</tr>' ).
      ENDLOOP.
      LOOP AT mt_delayed_lines ASSIGNING <ls_diff_line>.
        ri_html->add( '<tr class="diff_line">' ).
        ri_html->add( |<td class="num diff_others" line-num=""></td>|
                   && |<td class="num diff_others" line-num="{ <ls_diff_line>-new_num }"></td>|
                   && |<td class="mark diff_others">+</td>|
                   && |<td class="code diff_ins diff_unified new">{ <ls_diff_line>-new }</td>| ).
        ri_html->add( '</tr>' ).
      ENDLOOP.
      CLEAR mt_delayed_lines.
    ENDIF.

    ri_html->add( '<tr class="diff_line">' ).
    CASE is_diff_line-result.
      WHEN Lif_abapgit_definitions=>c_diff-update.
        APPEND is_diff_line TO mt_delayed_lines. " Delay output of subsequent updates
      WHEN Lif_abapgit_definitions=>c_diff-insert.
        ri_html->add( |<td class="num diff_others" line-num=""></td>|
                   && |<td class="num diff_others" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="mark diff_others">+</td>|
                   && |<td class="code diff_ins diff_unified new">{ is_diff_line-new }</td>| ).
      WHEN Lif_abapgit_definitions=>c_diff-delete.
        ri_html->add( |<td class="num diff_others" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num diff_others" line-num=""></td>|
                   && |<td class="mark diff_others">-</td>|
                   && |<td class="code diff_del diff_unified old">{ is_diff_line-old }</td>| ).
      WHEN OTHERS. "none
        ri_html->add( |<td class="num diff_others" line-num="{ is_diff_line-old_num }"></td>|
                   && |<td class="num diff_others" line-num="{ is_diff_line-new_num }"></td>|
                   && |<td class="mark diff_others">&nbsp;</td>|
                   && |<td class="code diff_unified">{ is_diff_line-old }</td>| ).
    ENDCASE.
    ri_html->add( '</tr>' ).

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).

    ri_html->add( 'restoreScrollPosition();' ).
    ri_html->add( 'var gHelper = new DiffHelper({' ).
    ri_html->add( |  seed:        "{ mv_seed }",| ).
    ri_html->add( '  ids: {' ).
    ri_html->add( '    jump:        "jump",' ).
    ri_html->add( '    diffList:    "diff-list",' ).
    ri_html->add( '    filterMenu:  "diff-filter",' ).
    ri_html->add( '  }' ).
    ri_html->add( '});' ).

    ri_html->add( 'addMarginBottom();' ).

    ri_html->add( 'var gGoJumpPalette = new CommandPalette(enumerateJumpAllFiles, {' ).
    ri_html->add( '  toggleKey: "F2",' ).
    ri_html->add( '  hotkeyDescription: "Jump to File ..."' ).
    ri_html->add( '});' ).

    " Feature for selecting ABAP code by column and copy to clipboard
    ri_html->add( 'var columnSelection = new DiffColumnSelection();' ).

  ENDMETHOD.
  METHOD render_table_head.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( '<thead class="header">' ).
    ri_html->add( '<tr>' ).

    IF mv_unified = abap_true.

      render_table_head_unified( ri_html ).

    ELSE.

      render_table_head_non_unified(
          ii_html = ri_html
          is_diff = is_diff ).

    ENDIF.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
  METHOD render_table_head_non_unified.

    ii_html->add( '<th class="num"></th>' ).
    ii_html->add( '<th class="mark"></th>' ).
    ii_html->add( '<th>LOCAL</th>' ).
    ii_html->add( '<th class="num"></th>' ).
    ii_html->add( '<th class="mark"></th>' ).
    ii_html->add( '<th>REMOTE</th>' ).

  ENDMETHOD.
  METHOD render_table_head_unified.

    ii_html->add( '<th class="num">old</th>' ).
    ii_html->add( '<th class="num">new</th>' ).
    ii_html->add( '<th class="mark"></th>' ).
    ii_html->add( '<th>code</th>' ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA ls_view LIKE ms_view.

    ls_view = ms_view.

    CASE ii_event->mv_action.
      WHEN c_actions-toggle_unified. " Toggle file diplay

        mv_unified = Lcl_abapgit_persistence_user=>get_instance( )->toggle_diff_unified( ).

        rs_handled-page  = Lcl_abapgit_gui_page_hoc=>create(
          iv_page_title         = 'Diff'
          iv_page_layout        = get_page_layout( )
          ii_page_menu_provider = me
          ii_child_component    = me ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.

      WHEN c_actions-toggle_hide_diffs. " Toggle display of diffs

        ms_view-hide_diffs = boolc( ms_view-hide_diffs = abap_false ).

      WHEN c_actions-toggle_hidden_chars. " Toggle display of hidden characters

        ms_view-hidden_chars = boolc( ms_view-hidden_chars = abap_false ).

      WHEN c_actions-toggle_ignore_indent. " Toggle ignore indentation

        ms_view-ignore_indent = boolc( ms_view-ignore_indent = abap_false ).

      WHEN c_actions-toggle_ignore_comments. " Toggle ignore comments

        ms_view-ignore_comments = boolc( ms_view-ignore_comments = abap_false ).

      WHEN c_actions-toggle_ignore_case. " Toggle case sensitivity

        ms_view-ignore_case = boolc( ms_view-ignore_case = abap_false ).

      WHEN OTHERS.

        IF is_refresh( ii_event->mv_action ) = abap_true.

          refresh( ii_event->mv_action ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

        ENDIF.

    ENDCASE.

    " If view has changed, refresh local files recalculating diff, and update menu
    IF ms_view <> ls_view.
      IF ms_view-hide_diffs = ls_view-hide_diffs.
        refresh( c_actions-refresh_local ).
      ENDIF.
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Diff'.

    ls_hotkey_action-description = |Refresh Local|.
    ls_hotkey_action-action      = c_actions-refresh_local.
    ls_hotkey_action-hotkey      = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Refresh All|.
    ls_hotkey_action-action      = c_actions-refresh_all.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Toogle Split/Unified|.
    ls_hotkey_action-action      = c_actions-toggle_unified.
    ls_hotkey_action-hotkey      = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Toogle Hidden Characters|.
    ls_hotkey_action-action      = c_actions-toggle_hidden_chars.
    ls_hotkey_action-hotkey      = |h|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    add_menu_begin( ro_toolbar ).
    add_jump_sub_menu( ro_toolbar ).
    add_filter_sub_menu( ro_toolbar ).
    add_menu_end( ro_toolbar ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA: ls_diff_file LIKE LINE OF mt_diff_files,
          li_progress  TYPE REF TO Lif_abapgit_progress.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    li_progress = Lcl_abapgit_progress=>get_instance( lines( mt_diff_files ) ).

    ri_html->add( `<div class="repo">` ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top( mo_repo ) ).
    ri_html->add( `</div>` ).

    ri_html->add( |<div id="diff-list" data-repo-key="{ mv_repo_key }">| ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).
    LOOP AT mt_diff_files INTO ls_diff_file.
      li_progress->show(
        iv_current = sy-tabix
        iv_text    = |Render Diff - { ls_diff_file-filename }| ).

      ri_html->add( render_diff( ls_diff_file ) ).
    ENDLOOP.
    IF sy-subrc <> 0.
      ri_html->add( |No more diffs| ).
    ENDIF.
    ri_html->add( '</div>' ).

    register_deferred_script( render_scripts( ) ).

    li_progress->off( ).

    register_handlers( ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_DIFF_BASE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_EX_OBJECT <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_ex_objectccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_ex_objectccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_EX_OBJECT implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.

    CREATE OBJECT mo_form_data.
    mo_form_data->set(
      iv_key = c_id-only_main
      iv_val = abap_true ).

    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.
  METHOD create.
    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_ex_object.
    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Export Objects to Files'
      ii_child_component = lo_component ).
  ENDMETHOD.
  METHOD export_object.
    DATA lv_object_type TYPE trobjtype.
    DATA lt_names TYPE STANDARD TABLE OF sobj_name WITH DEFAULT KEY.
    DATA lv_name LIKE LINE OF lt_names.
    DATA lv_list TYPE string.
    DATA lv_only_main TYPE abap_bool.

    lv_object_type = mo_form_data->get( c_id-object_type ).
    lv_list = mo_form_data->get( c_id-object_name ).
    lv_only_main = mo_form_data->get( c_id-only_main ).

    REPLACE ALL OCCURRENCES OF |\r| IN lv_list WITH ''.
    SPLIT lv_list AT |\n| INTO TABLE lt_names.

    LOOP AT lt_names INTO lv_name.
      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.
      Lcl_abapgit_zip=>export_object(
        iv_main_language_only = lv_only_main
        iv_object_type        = lv_object_type
        iv_object_name        = lv_name ).
    ENDLOOP.
  ENDMETHOD.
  METHOD get_form_schema.
    ro_form = Lcl_abapgit_html_form=>create( iv_form_id = 'export-object-to-files' ).

    ro_form->text(
      iv_label       = 'Object Type'
      iv_name        = c_id-object_type
      iv_required    = abap_true
      iv_upper_case  = abap_true
      iv_side_action = c_event-choose_object_type ).

    ro_form->textarea(
      iv_label       = 'Object Names'
      iv_name        = c_id-object_name
      iv_required    = abap_true
      iv_placeholder = 'One object name per line'
      iv_upper_case  = abap_true ).

    ro_form->checkbox(
      iv_label = 'Only Main Language'
      iv_name  = c_id-only_main ).

    ro_form->command(
      iv_label       = 'Export'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-export
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.
    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-export.

        export_object( ).
        MESSAGE 'Object successfully exported' TYPE 'S'.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.

      WHEN c_event-choose_object_type.

        mo_form_data->set(
          iv_key = c_id-object_type
          iv_val = Lcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TADIR-OBJECT' ) ).

        IF mo_form_data->get( c_id-object_type ) IS NOT INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.
    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="form-container">' ).
    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
    ri_html->add( '</div>' ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_EX_OBJECT implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_EX_PCKAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_ex_pckageccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_ex_pckageccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_EX_PCKAGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.
    super->constructor( ).
    CREATE OBJECT mo_validation_log.
    CREATE OBJECT mo_form_data.
    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).
  ENDMETHOD.
  METHOD create.
    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_ex_pckage.
    CREATE OBJECT lo_component.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Export Package to ZIP'
      ii_child_component = lo_component ).
  ENDMETHOD.
  METHOD export_package.
    DATA lv_package TYPE devclass.
    DATA lv_folder_logic TYPE string.
    DATA lv_main_lang_only TYPE abap_bool.

    lv_package        = mo_form_data->get( c_id-package ).
    lv_folder_logic   = mo_form_data->get( c_id-folder_logic ).
    lv_main_lang_only = mo_form_data->get( c_id-main_lang_only ).

    Lcl_abapgit_zip=>export_package(
        iv_package        = lv_package
        iv_folder_logic   = lv_folder_logic
        iv_main_lang_only = lv_main_lang_only ).
  ENDMETHOD.
  METHOD get_form_schema.
    ro_form = Lcl_abapgit_html_form=>create( iv_form_id = 'export-package-to-files' ).

    ro_form->text(
      iv_name          = c_id-package
      iv_label         = 'Package'
      iv_required      = abap_true
      iv_upper_case    = abap_true
      iv_side_action   = c_event-choose_package
    )->radio(
      iv_name          = c_id-folder_logic
      iv_label         = 'Folder Logic'
      iv_default_value = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
      iv_hint          = 'Define how package folders are named in repository'
    )->option(
      iv_label         = 'Prefix'
      iv_value         = Lif_abapgit_dot_abapgit=>c_folder_logic-prefix
    )->option(
      iv_label         = 'Full'
      iv_value         = Lif_abapgit_dot_abapgit=>c_folder_logic-full
    )->option(
      iv_label         = 'Mixed'
      iv_value         = Lif_abapgit_dot_abapgit=>c_folder_logic-mixed
    )->checkbox(
      iv_name          = c_id-main_lang_only
      iv_label         = 'Serialize Main Language Only'
      iv_hint          = 'Ignore translations, serialize just main language'
    )->command(
      iv_label         = 'Export Package to ZIP'
      iv_action        = c_event-export_package
      iv_cmd_type      = Lif_abapgit_html_form=>c_cmd_type-input_main
    )->command(
      iv_label         = 'Back'
      iv_action        = Lif_abapgit_definitions=>c_action-go_back ).
  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.
    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.
      WHEN c_event-export_package.

        mo_validation_log = mo_form_util->validate( mo_form_data ).
        IF mo_validation_log->is_empty( ) = abap_false.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          export_package( ).
          MESSAGE 'Package successfully exported' TYPE 'S'.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
        ENDIF.

      WHEN c_event-choose_package.

        mo_form_data->set(
          iv_key = c_id-package
          iv_val = Lcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'TDEVC-DEVCLASS' ) ).

        IF mo_form_data->get( c_id-package ) IS NOT INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ENDIF.
    ENDCASE.
  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.
    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( mo_form->render(
      io_values         = mo_form_data
      io_validation_log = mo_validation_log ) ).
  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_EX_PCKAGE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_MERGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_merge====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_merge====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_MERGE implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    io_repo->select_branch( |{ Lif_abapgit_git_definitions=>c_git_branch-heads_prefix }{ iv_target }| ).

    CREATE OBJECT mi_merge TYPE Lcl_abapgit_merge
      EXPORTING
        io_repo          = io_repo
        iv_source_branch = iv_source.

    mi_merge->run( ).

  ENDMETHOD.
  METHOD show_file.

    FIELD-SYMBOLS <ls_show> LIKE LINE OF it_expanded.

    READ TABLE it_expanded ASSIGNING <ls_show>
      WITH KEY path_name
      COMPONENTS path = is_file-path name = is_file-name.
    IF sy-subrc = 0.
      IF <ls_show>-sha1 = is_result-sha1.
        ii_html->add( |<td>{ <ls_show>-path }{ <ls_show>-name }</td><td><b>{ <ls_show>-sha1(7) }</b></td>| ).
      ELSE.
        ii_html->add( |<td>{ <ls_show>-path }{ <ls_show>-name }</td><td>{ <ls_show>-sha1(7) }</td>| ).
      ENDIF.
    ELSE.
      ii_html->add( '<td></td><td></td>' ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_actions-merge.
        IF mi_merge->has_conflicts( ) = abap_true.
          Lcx_abapgit_exception=>raise( 'conflicts exists' ).
        ENDIF.

        IF mi_merge->get_result( )-stage->count( ) = 0.
          Lcx_abapgit_exception=>raise( 'nothing to merge' ).
        ENDIF.

        IF mo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

          rs_handled-page = Lcl_abapgit_gui_page_code_insp=>create(
            io_repo  = mo_repo
            io_stage = mi_merge->get_result( )-stage ).

        ELSE.

          rs_handled-page = Lcl_abapgit_gui_page_commit=>create(
            io_repo  = mo_repo
            io_stage = mi_merge->get_result( )-stage ).

        ENDIF.

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-res_conflicts.

        rs_handled-page = Lcl_abapgit_gui_page_merge_res=>create(
          io_repo       = mo_repo
          io_merge_page = me
          io_merge      = mi_merge ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

    ENDCASE.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_merge.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo   = io_repo
        iv_source = iv_source
        iv_target = iv_target.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Merge'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add( iv_txt = 'Merge'
                     iv_act = c_actions-merge
                     iv_cur = abap_false ).

    IF mi_merge->has_conflicts( ) = abap_true.
      ro_toolbar->add( iv_txt = 'Resolve Conflicts'
                       iv_act = c_actions-res_conflicts ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA: ls_merge  TYPE Lif_abapgit_merge=>ty_merge,
          lt_files  LIKE ls_merge-stree,
          ls_result LIKE LINE OF ls_merge-result.

    FIELD-SYMBOLS <ls_file> LIKE LINE OF lt_files.

    register_handlers( ).

    ls_merge = mi_merge->get_result( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div id="toc">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo         = mo_repo
      iv_show_package = abap_false
      iv_show_branch  = abap_false ) ).

    ri_html->add( '<table>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Source</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-source-name ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Target</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-target-name ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td>Ancestor</td>' ).
    ri_html->add( '<td>' ).
    ri_html->add( ls_merge-common-commit ).
    ri_html->add( '</td></tr>' ).
    ri_html->add( '</table>' ).

    ri_html->add( '<br>' ).

    APPEND LINES OF ls_merge-stree TO lt_files.
    APPEND LINES OF ls_merge-ttree TO lt_files.
    APPEND LINES OF ls_merge-ctree TO lt_files.
    SORT lt_files BY path DESCENDING name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_files COMPARING path name.

    ri_html->add( '<table>' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<td><u>Source</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Target</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Ancestor</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '<td><u>Result</u></td>' ).
    ri_html->add( '<td></td>' ).
    ri_html->add( '</tr>' ).
    LOOP AT lt_files ASSIGNING <ls_file>.
      CLEAR ls_result.
      READ TABLE ls_merge-result INTO ls_result
        WITH KEY path_name
        COMPONENTS path = <ls_file>-path name = <ls_file>-name.

      ri_html->add( '<tr>' ).
      show_file( it_expanded = ls_merge-stree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-ttree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-ctree
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      show_file( it_expanded = ls_merge-result
                 ii_html     = ri_html
                 is_file     = <ls_file>
                 is_result   = ls_result ).
      ri_html->add( '</tr>' ).
    ENDLOOP.
    ri_html->add( '</table>' ).
    ri_html->add( '<br>' ).
    ri_html->add( '<b>' ).
    ri_html->add( ls_merge-conflict ).
    ri_html->add( '</b>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_MERGE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_MERGE_RES <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_merge_resccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_merge_resccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_MERGE_RES implementation.
*"* method's implementations
*include methods.
  METHOD apply_merged_content.

    DATA:
      lv_merge_content    TYPE string,
      lv_new_file_content TYPE xstring.

    FIELD-SYMBOLS:
      <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict.

    lv_merge_content = ii_event->form_data( )->get( 'MERGE_CONTENT' ).

    REPLACE ALL OCCURRENCES
      OF cl_abap_char_utilities=>cr_lf IN lv_merge_content WITH cl_abap_char_utilities=>newline.

    lv_new_file_content = Lcl_abapgit_convert=>string_to_xstring_utf8( lv_merge_content ).

    READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
    <ls_conflict>-result_sha1 = Lcl_abapgit_hash=>sha1_blob( lv_new_file_content ).
    <ls_conflict>-result_data = lv_new_file_content.
    mo_merge->resolve_conflict( <ls_conflict> ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).

    mo_repo = io_repo.

    mo_merge_page = io_merge_page.
    mo_merge = io_merge.
    mv_merge_mode = c_merge_mode-selection.
    mv_current_conflict_index = 1.
    mt_conflicts = io_merge->get_conflicts( ).

  ENDMETHOD.
  METHOD is_binary.

    FIELD-SYMBOLS <lv_data> LIKE iv_d1.

    IF iv_d1 IS NOT INITIAL. " One of them might be new and so empty
      ASSIGN iv_d1 TO <lv_data>.
    ELSE.
      ASSIGN iv_d2 TO <lv_data>.
    ENDIF.

    rv_yes = Lcl_abapgit_utils=>is_binary( <lv_data> ).

  ENDMETHOD.
  METHOD render_beacon.

    DATA: lv_beacon  TYPE string,
          lt_beacons TYPE Lif_abapgit_definitions=>ty_string_tt.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF is_diff_line-beacon > 0.
      lt_beacons = is_diff-o_diff->get_beacons( ).
      READ TABLE lt_beacons INTO lv_beacon INDEX is_diff_line-beacon.
    ELSE.
      lv_beacon = '---'.
    ENDIF.

    ri_html->add( '<thead class="nav_line">' ).
    ri_html->add( '<tr>' ).

    ri_html->add( '<th class="num"></th>' ).
    ri_html->add( |<th colspan="3">@@ { is_diff_line-new_num } @@ { lv_beacon }</th>| ).

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
  METHOD render_diff.

    DATA lv_target_content TYPE string.

    FIELD-SYMBOLS <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<div class="diff" data-type="{ is_diff-type
      }" data-changed-by="{ is_diff-changed_by
      }" data-file="{ is_diff-path && is_diff-filename }">| ).
    ri_html->add( render_diff_head( is_diff ) ).

    " Content
    IF is_diff-type <> 'binary'.

      IF mv_merge_mode = c_merge_mode-selection.
        ri_html->add( '<div class="diff_content">' ).
        ri_html->add( '<table class="diff_tab syntax-hl">' ).
        ri_html->add( render_table_head( ) ).
        ri_html->add( render_lines( is_diff ) ).
        ri_html->add( '</table>' ).
        ri_html->add( '</div>' ).
      ELSE.

        "Table for Div-Table and textarea
        ri_html->add( '<div class="diff_content">' ).
        ri_html->add( '<table class="w100">' ).
        ri_html->add( '<thead class="header">' ).
        ri_html->add( '<tr>' ).
        ri_html->add( '<th>Code</th>' ).
        ri_html->add( '<th>Merge - ' ).
        ri_html->add_a( iv_act = 'submitFormById(''merge_form'');'
                        iv_txt = 'Apply'
                        iv_typ = Lif_abapgit_html=>c_action_type-onclick
                        iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
        ri_html->add( '</th> ' ).
        ri_html->add( '</tr>' ).
        ri_html->add( '</thead>' ).
        ri_html->add( '<td>' ).

        "Diff-Table of source and target file
        ri_html->add( '<table class="diff_tab syntax-hl">' ).
        ri_html->add( render_table_head( ) ).
        ri_html->add( render_lines( is_diff ) ).
        ri_html->add( '</table>' ).

        READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
        IF sy-subrc = 0.
          lv_target_content = Lcl_abapgit_convert=>xstring_to_string_utf8( <ls_conflict>-target_data ).
          lv_target_content = escape( val = lv_target_content
                                      format = cl_abap_format=>e_html_text ).
        ENDIF.

        ri_html->add( '</td>' ).
        ri_html->add( '<td>' ).
        ri_html->add( '<div class="form-container">' ).
        ri_html->add( |<form id="merge_form" class="aligned-form w100" accept-charset="UTF-8"| ).
        ri_html->add( |method="post" action="sapevent:apply_merge">| ).
        ri_html->add( |<textarea id="merge_content" name="merge_content" class="w100" | ).
        ri_html->add( |rows="{ lines( is_diff-o_diff->get( ) ) }">{ lv_target_content }</textarea>| ).
        ri_html->add( '<input type="submit" class="hidden-submit">' ).
        ri_html->add( '</form>' ).
        ri_html->add( '</div>' ).
        ri_html->add( '</td>' ).
        ri_html->add( '</table>' ).
        ri_html->add( '</div>' ).
      ENDIF.
    ELSE.
      ri_html->add( '<div class="diff_content paddings center grey">' ).
      ri_html->add( 'The content seems to be binary.' ).
      ri_html->add( 'Cannot display as diff.' ).
      ri_html->add( '</div>' ).
    ENDIF.

    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_diff_head.

    DATA ls_stats TYPE Lif_abapgit_definitions=>ty_count.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="diff_head">' ).

    IF is_diff-type <> 'binary' AND is_diff-o_diff IS NOT INITIAL.
      ls_stats = is_diff-o_diff->stats( ).
      ri_html->add( |<span class="diff_banner diff_ins">+ { ls_stats-insert }</span>| ).
      ri_html->add( |<span class="diff_banner diff_del">- { ls_stats-delete }</span>| ).
      ri_html->add( |<span class="diff_banner diff_upd">~ { ls_stats-update }</span>| ).
    ENDIF.

    ri_html->add( |<span class="diff_name">{ is_diff-filename }</span>| ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_lines.

    DATA: lo_highlighter TYPE REF TO Lcl_abapgit_syntax_highlighter,
          lt_diffs       TYPE Lif_abapgit_definitions=>ty_diffs_tt,
          lv_insert_nav  TYPE abap_bool.

    FIELD-SYMBOLS <ls_diff>  LIKE LINE OF lt_diffs.

    lo_highlighter = Lcl_abapgit_syntax_factory=>create( is_diff-filename ).
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lt_diffs = is_diff-o_diff->get( ).

    LOOP AT lt_diffs ASSIGNING <ls_diff>.
      IF <ls_diff>-short = abap_false.
        lv_insert_nav = abap_true.
        CONTINUE.
      ENDIF.

      IF lv_insert_nav = abap_true. " Insert separator line with navigation
        ri_html->add( render_beacon( is_diff_line = <ls_diff>
                                     is_diff = is_diff ) ).
        lv_insert_nav = abap_false.
      ENDIF.

      IF lo_highlighter IS BOUND.
        <ls_diff>-new = lo_highlighter->process_line( <ls_diff>-new ).
        <ls_diff>-old = lo_highlighter->process_line( <ls_diff>-old ).
      ELSE.
        <ls_diff>-new = escape( val = <ls_diff>-new
                                format = cl_abap_format=>e_html_attr ).
        <ls_diff>-old = escape( val = <ls_diff>-old
                                format = cl_abap_format=>e_html_attr ).
      ENDIF.

      CONDENSE <ls_diff>-new_num. "get rid of leading spaces
      CONDENSE <ls_diff>-old_num.

      ri_html->add( render_line_split( <ls_diff> ) ).

    ENDLOOP.

  ENDMETHOD.
  METHOD render_line_split.

    DATA: lv_new  TYPE string,
          lv_old  TYPE string,
          lv_mark TYPE string,
          lv_bg   TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    " New line
    lv_mark = ` `.
    IF is_diff_line-result = Lif_abapgit_definitions=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = Lif_abapgit_definitions=>c_diff-insert.
      lv_bg = ' diff_ins'.
      lv_mark = `+`.
    ENDIF.
    lv_new = |<td class="num" line-num="{ is_diff_line-new_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-new }</td>|.

    " Old line
    CLEAR lv_bg.
    lv_mark = ` `.
    IF is_diff_line-result = Lif_abapgit_definitions=>c_diff-update.
      lv_bg = ' diff_upd'.
      lv_mark = `~`.
    ELSEIF is_diff_line-result = Lif_abapgit_definitions=>c_diff-delete.
      lv_bg = ' diff_del'.
      lv_mark = `-`.
    ENDIF.
    lv_old = |<td class="num" line-num="{ is_diff_line-old_num }"></td>|
          && |<td class="code{ lv_bg }">{ lv_mark }{ is_diff_line-old }</td>|.

    " render line, inverse sides if remote is newer
    ri_html->add( '<tr>' ).
    ri_html->add( lv_old ). " Target
    ri_html->add( lv_new ). " Source
    ri_html->add( '</tr>' ).

  ENDMETHOD.
  METHOD render_table_head.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<thead class="header">' ).
    ri_html->add( '<tr>' ).
    ri_html->add( '<th class="num"></th>' ).

    IF mv_merge_mode = c_merge_mode-selection.
      ri_html->add( '<form id="target_form" method="post" action="sapevent:apply_target">' ).
      ri_html->add( '<th>Target - ' && mo_repo->get_selected_branch( ) && ' - ' ).
      ri_html->add_a( iv_act = 'submitFormById(''target_form'');'
                      iv_txt = 'Apply'
                      iv_typ = Lif_abapgit_html=>c_action_type-onclick
                      iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      ri_html->add( '</th> ' ).
      ri_html->add( '</form>' ).
      ri_html->add( '<th class="num"></th>' ).
      ri_html->add( '<form id="source_form" method="post" action="sapevent:apply_source">' ).
      ri_html->add( '<th>Source  - ' && mo_merge->get_source_branch( ) && ' - ' ).
      ri_html->add_a( iv_act = 'submitFormById(''source_form'');'
                      iv_txt = 'Apply'
                      iv_typ = Lif_abapgit_html=>c_action_type-onclick
                      iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      ri_html->add( '</th> ' ).
      ri_html->add( '</form>' ).
    ELSE.
      ri_html->add( '<th>Target - ' && mo_repo->get_selected_branch( ) && '</th> ' ).
      ri_html->add( '<th class="num"></th>' ).
      ri_html->add( '<th>Source - ' && mo_merge->get_source_branch( ) && '</th> ' ).
    ENDIF.

    ri_html->add( '</tr>' ).
    ri_html->add( '</thead>' ).

  ENDMETHOD.
  METHOD resolve_diff.

    DATA lv_offs TYPE i.

    FIELD-SYMBOLS <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict.

    CLEAR ms_diff_file.

    READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ms_diff_file-path     = <ls_conflict>-path.
    ms_diff_file-filename = <ls_conflict>-filename.
    ms_diff_file-type = reverse( <ls_conflict>-filename ).

    FIND FIRST OCCURRENCE OF '.' IN ms_diff_file-type MATCH OFFSET lv_offs.
    ms_diff_file-type = reverse( substring( val = ms_diff_file-type
                                            len = lv_offs ) ).
    IF ms_diff_file-type <> 'xml' AND ms_diff_file-type <> 'abap'.
      ms_diff_file-type = 'other'.
    ENDIF.

    IF ms_diff_file-type = 'other'
    AND is_binary( iv_d1 = <ls_conflict>-source_data
                   iv_d2 = <ls_conflict>-target_data ) = abap_true.
      ms_diff_file-type = 'binary'.
    ENDIF.

    IF ms_diff_file-type <> 'binary'.
      CREATE OBJECT ms_diff_file-o_diff
        EXPORTING
          iv_new = <ls_conflict>-source_data
          iv_old = <ls_conflict>-target_data.
    ENDIF.

  ENDMETHOD.
  METHOD toggle_merge_mode.

    IF mv_merge_mode = c_merge_mode-selection.
      mv_merge_mode = c_merge_mode-merge.
    ELSE.
      mv_merge_mode = c_merge_mode-selection.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    FIELD-SYMBOLS <ls_conflict> TYPE Lif_abapgit_merge=>ty_merge_conflict.

    CASE ii_event->mv_action.
      WHEN c_actions-apply_merge
        OR c_actions-apply_source
        OR c_actions-apply_target
        OR c_actions-cancel.

        CASE ii_event->mv_action.
          WHEN c_actions-apply_merge.
            apply_merged_content( ii_event ).

          WHEN c_actions-apply_source.
            READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
            <ls_conflict>-result_sha1 = <ls_conflict>-source_sha1.
            <ls_conflict>-result_data = <ls_conflict>-source_data.
            mo_merge->resolve_conflict( <ls_conflict> ).

          WHEN c_actions-apply_target.
            READ TABLE mt_conflicts ASSIGNING <ls_conflict> INDEX mv_current_conflict_index.
            <ls_conflict>-result_sha1 = <ls_conflict>-target_sha1.
            <ls_conflict>-result_data = <ls_conflict>-target_data.
            mo_merge->resolve_conflict( <ls_conflict> ).

        ENDCASE.

        mv_current_conflict_index = mv_current_conflict_index + 1.
        IF mv_current_conflict_index > lines( mt_conflicts ).
          CLEAR mv_current_conflict_index.
        ENDIF.

        IF mv_current_conflict_index IS NOT INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ELSE.
          rs_handled-page = mo_merge_page.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
        ENDIF.

      WHEN c_actions-toggle_mode.
        toggle_merge_mode( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_merge_res.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        io_merge_page = io_merge_page
        io_merge      = io_merge.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Resolve Merge Conflicts'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add( iv_txt = 'Toggle merge mode'
                     iv_act = c_actions-toggle_mode ).
    ro_toolbar->add( iv_txt = 'Cancel'
                     iv_act = c_actions-cancel ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    resolve_diff( ).

    IF ms_diff_file IS INITIAL.
      Lcx_abapgit_exception=>raise( 'no conflict found' ).
    ENDIF.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    ri_html->add( |<div id="diff-list" data-repo-key="{ mo_repo->get_key( ) }">| ).
    ri_html->add( render_diff( ms_diff_file ) ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_MERGE_RES implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_MERGE_SEL <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_merge_selccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_merge_selccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_MERGE_SEL implementation.
*"* method's implementations
*include methods.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    mo_repo ?= ii_repo.

    read_branches( ).

    mo_form = get_form_schema( ).
    mo_form_util = Lcl_abapgit_html_form_utils=>create( mo_form ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_merge_sel.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo = ii_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Merge Branches'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    FIELD-SYMBOLS <ls_branch> LIKE LINE OF mt_branches.

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'merge-branches-form'
                iv_help_page = 'https://docs.abapgit.org/' ). " todo, add docs

    ro_form->start_group(
      iv_name  = c_id-branches
      iv_label = 'Branch Selection'
      iv_hint  = 'Select the branches that should be merged'
    )->radio(
      iv_name          = c_id-source
      iv_label         = 'Source Branch'
      iv_default_value = substring(
                           val = mo_repo->get_selected_branch( )
                           off = 11 )
      iv_condense      = abap_true ).

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      ro_form->option(
        iv_label = <ls_branch>-display_name
        iv_value = <ls_branch>-display_name ).
    ENDLOOP.

    ro_form->radio(
      iv_name     = c_id-target
      iv_label    = 'Target Branch'
      iv_condense = abap_true ).

    LOOP AT mt_branches ASSIGNING <ls_branch>.
      ro_form->option(
        iv_label = <ls_branch>-display_name
        iv_value = <ls_branch>-display_name ).
    ENDLOOP.

    ro_form->command(
      iv_label    = 'Preview Merge'
      iv_cmd_type = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action   = c_event-merge
    )->command(
      iv_label    = 'Back'
      iv_action   = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD read_branches.

    DATA lo_branches TYPE REF TO Lcl_abapgit_git_branch_list.

    lo_branches = Lcl_abapgit_git_transport=>branches( mo_repo->get_url( ) ).
    mt_branches = lo_branches->get_branches_only( ).

    DELETE mt_branches WHERE name = Lif_abapgit_git_definitions=>c_head_name.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    mo_form_data = mo_form_util->normalize( ii_event->form_data( ) ).

    CASE ii_event->mv_action.

      WHEN c_event-merge.
        IF mo_form_data->get( c_id-source ) = mo_form_data->get( c_id-target ).
          Lcx_abapgit_exception=>raise( 'Select different branches' ).
        ENDIF.

        rs_handled-page  = Lcl_abapgit_gui_page_merge=>create(
          io_repo   = mo_repo
          iv_source = mo_form_data->get( c_id-source )
          iv_target = mo_form_data->get( c_id-target ) ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.
        ASSERT 1 = 1.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render( io_values = mo_form_data ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_MERGE_SEL implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_PATCH <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_patch====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_patch====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_gui_page_patch====ccau.
*"* use this source file for your ABAP unit test classes




*CLASS zcl_abapgit_gui_page_patch DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KWZ5SFY.





class LCL_ABAPGIT_GUI_PAGE_PATCH implementation.
*"* method's implementations
*include methods.
  METHOD add_menu_begin.

    io_menu->add(
        iv_txt   = c_action_texts-refresh_local
        iv_typ   = Lif_abapgit_html=>c_action_type-dummy
        iv_act   = c_actions-refresh_local
        iv_id    = c_actions-refresh_local
        iv_title = c_action_titles-refresh_local ).

    io_menu->add(
        iv_txt   = c_action_texts-refresh_all
        iv_typ   = Lif_abapgit_html=>c_action_type-dummy
        iv_act   = c_actions-refresh_all
        iv_id    = c_actions-refresh_all
        iv_title = c_action_titles-refresh_all ).

  ENDMETHOD.
  METHOD add_menu_end.

    io_menu->add( iv_txt = 'Stage'
                  iv_act = c_patch_actions-stage
                  iv_id  = 'stage'
                  iv_typ = Lif_abapgit_html=>c_action_type-dummy ).

    add_view_sub_menu( io_menu ).

  ENDMETHOD.
  METHOD add_to_stage.

    DATA: lt_diff              TYPE Lif_abapgit_definitions=>ty_diffs_tt,
          lv_something_patched TYPE abap_bool,
          ls_status            TYPE Lif_abapgit_definitions=>ty_result,
          lv_patch             TYPE xstring,
          lo_git_add_patch     TYPE REF TO Lcl_abapgit_git_add_patch.

    FIELD-SYMBOLS: <ls_diff_file> TYPE ty_file_diff.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.

      IF <ls_diff_file>-o_diff IS NOT BOUND.
        " When we deal with binary files we don't have a diff object.
        " There's nothing to do because they cannot be patched
        CONTINUE.
      ENDIF.

      lt_diff = <ls_diff_file>-o_diff->get( ).

      READ TABLE lt_diff TRANSPORTING NO FIELDS
                         WITH KEY patch_flag = abap_true.
      CHECK sy-subrc = 0.

      lv_something_patched = abap_true.

      CREATE OBJECT lo_git_add_patch
        EXPORTING
          it_diff = <ls_diff_file>-o_diff->get( ).

      lv_patch = lo_git_add_patch->get_patch_binary( ).

      IF <ls_diff_file>-lstate = 'D' AND are_all_lines_patched( lt_diff ) = abap_true.

        ls_status-lstate = Lif_abapgit_definitions=>c_state-deleted.
        mo_stage->rm(
          iv_path     = <ls_diff_file>-path
          is_status   = ls_status
          iv_filename = <ls_diff_file>-filename ).

      ELSE.

        IF <ls_diff_file>-lstate = 'A' AND are_all_lines_patched( lt_diff ) = abap_true.
          ls_status-lstate = Lif_abapgit_definitions=>c_state-added.
        ELSE.
          ls_status-lstate = Lif_abapgit_definitions=>c_state-modified.
        ENDIF.

        mo_stage->add(
          iv_path     = <ls_diff_file>-path
          iv_filename = <ls_diff_file>-filename
          is_status   = ls_status
          iv_data     = lv_patch ).

      ENDIF.

    ENDLOOP.

    IF lv_something_patched = abap_false.
      Lcx_abapgit_exception=>raise( |Nothing added| ).
    ENDIF.

  ENDMETHOD.
  METHOD apply_patch_all.

    DATA: lv_filename   TYPE string,
          lt_patch      TYPE string_table,
          lv_line_index TYPE string.

    FIELD-SYMBOLS: <lv_patch>     TYPE LINE OF string_table.

    SPLIT iv_patch AT ',' INTO TABLE lt_patch.

    LOOP AT lt_patch ASSIGNING <lv_patch>.

      get_patch_data(
        EXPORTING
          iv_patch      = <lv_patch>
        IMPORTING
          ev_filename   = lv_filename
          ev_line_index = lv_line_index ).

      apply_patch_for( iv_filename   = lv_filename
                       iv_line_index = lv_line_index
                       iv_patch_flag = iv_patch_flag ).

    ENDLOOP.

  ENDMETHOD.
  METHOD apply_patch_for.

    DATA: lo_diff      TYPE REF TO Lcl_abapgit_diff,
          ls_diff_line TYPE Lif_abapgit_definitions=>ty_diff,
          lv_line      TYPE i.

    lo_diff = get_diff_object( iv_filename ).

    ls_diff_line = get_diff_line( io_diff       = lo_diff
                                  iv_line_index = iv_line_index ).

    CASE ls_diff_line-result.
      WHEN Lif_abapgit_definitions=>c_diff-update
        OR Lif_abapgit_definitions=>c_diff-insert.

        lv_line = ls_diff_line-new_num.

        lo_diff->set_patch_new( iv_line_new   = lv_line
                                iv_patch_flag = iv_patch_flag ).

      WHEN Lif_abapgit_definitions=>c_diff-delete.

        lv_line = ls_diff_line-old_num.

        lo_diff->set_patch_old( iv_line_old   = lv_line
                                iv_patch_flag = iv_patch_flag ).

    ENDCASE.

  ENDMETHOD.
  METHOD apply_patch_from_form_fields.

    DATA:
      lv_add    TYPE string,
      lv_remove TYPE string.

    lv_add    = ii_event->form_data( )->get( c_patch_action-add ).
    lv_remove = ii_event->form_data( )->get( c_patch_action-remove ).

    apply_patch_all( iv_patch      = lv_add
                     iv_patch_flag = abap_true ).

    apply_patch_all( iv_patch      = lv_remove
                     iv_patch_flag = abap_false ).

  ENDMETHOD.
  METHOD are_all_lines_patched.

    DATA: lv_patch_count TYPE i.

    FIELD-SYMBOLS: <ls_diff> TYPE Lif_abapgit_definitions=>ty_diff.

    LOOP AT it_diff ASSIGNING <ls_diff>
                    WHERE patch_flag = abap_true.
      lv_patch_count = lv_patch_count + 1.
    ENDLOOP.

    rv_are_all_lines_patched = boolc( lv_patch_count = lines( it_diff ) ).

  ENDMETHOD.
  METHOD constructor.

    super->constructor(
      iv_key    = iv_key
      is_file   = is_file
      is_object = is_object
      it_files  = it_files ).

    IF mo_repo->is_offline( ) = abap_true.
      Lcx_abapgit_exception=>raise( |Patching is only possible for online repositories.| ).
    ENDIF.

    mo_repo_online ?= mo_repo.

    " While patching we always want to be in split mode
    CLEAR: mv_unified.
    CREATE OBJECT mo_stage.

  ENDMETHOD.
  METHOD get_diff_line.

    DATA: lt_diff       TYPE Lif_abapgit_definitions=>ty_diffs_tt,
          lv_line_index TYPE sy-tabix.


    lv_line_index = iv_line_index.
    lt_diff = io_diff->get( ).

    READ TABLE lt_diff INTO rs_diff
                       INDEX lv_line_index.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid line index { lv_line_index }| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_diff_object.

    FIELD-SYMBOLS: <ls_diff_file> LIKE LINE OF mt_diff_files.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.
      IF get_normalized_fname_with_path( <ls_diff_file> ) = iv_filename.
        ro_diff = <ls_diff_file>-o_diff.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ro_diff IS NOT BOUND.
      Lcx_abapgit_exception=>raise( |Invalid filename { iv_filename }| ).
    ENDIF.

  ENDMETHOD.
  METHOD get_patch_data.

    DATA: lv_section TYPE string.

    CLEAR: ev_filename, ev_line_index.

    FIND FIRST OCCURRENCE OF REGEX `patch_line` && `_(.*)_(\d)+_(\d+)`
         IN iv_patch
         SUBMATCHES ev_filename lv_section ev_line_index.
    IF sy-subrc <> 0.
      Lcx_abapgit_exception=>raise( |Invalid patch| ).
    ENDIF.

  ENDMETHOD.
  METHOD insert_nav.

    " add beacon at beginning of file
    rv_insert_nav = abap_true.

  ENDMETHOD.
  METHOD is_patch_line_possible.

    IF is_diff_line-result = Lif_abapgit_definitions=>c_diff-update
    OR is_diff_line-result = Lif_abapgit_definitions=>c_diff-insert
    OR is_diff_line-result = Lif_abapgit_definitions=>c_diff-delete.
      rv_is_patch_line_possible = abap_true.
    ENDIF.

  ENDMETHOD.
  METHOD refresh.

    DATA: lt_diff_files_old TYPE ty_file_diffs.

    lt_diff_files_old = mt_diff_files.

    super->refresh( iv_action ).

    restore_patch_flags( lt_diff_files_old ).

  ENDMETHOD.
  METHOD render_beacon_begin_of_row.

    mv_section_count = mv_section_count + 1.

    ii_html->add( |<th class="patch">| ).
    ii_html->add_checkbox( |patch_section_{ get_normalized_fname_with_path( is_diff ) }_{ mv_section_count }| ).
    ii_html->add( '</th>' ).

  ENDMETHOD.
  METHOD render_diff_head_after_state.

    DATA: lv_act_id TYPE string.

    lv_act_id = |{ c_actions-refresh_local_object }_{ is_diff-obj_type }_{ is_diff-obj_name }|.

    IF is_diff-obj_type IS NOT INITIAL AND is_diff-obj_name IS NOT INITIAL.
      " Dummy link is handled in JS (based on ID)
      ii_html->add( '<span class="repo_name">' ).
      ii_html->add_a( iv_txt   = ii_html->icon( iv_name  = 'redo-alt-solid'
                                                iv_class = 'pad-sides'
                                                iv_hint  = 'Local refresh of this object' )
                      iv_id    = lv_act_id
                      iv_act   = lv_act_id
                      iv_typ   = Lif_abapgit_html=>c_action_type-dummy
                      iv_class = |url| ).
      ii_html->add( '</span>' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_line_split_row.

    render_patch( ii_html      = ii_html
                  iv_filename  = iv_filename
                  is_diff_line = is_diff_line
                  iv_index     = iv_index ).

    super->render_line_split_row(
        ii_html      = ii_html
        iv_filename  = iv_filename
        is_diff_line = is_diff_line
        iv_index     = iv_index
        iv_fstate    = iv_fstate
        iv_new       = iv_new
        iv_old       = iv_old ).

  ENDMETHOD.
  METHOD render_patch.

    CONSTANTS:
      BEGIN OF lc_css_class,
        patch TYPE string VALUE `patch`,
      END OF lc_css_class.

    DATA:
      lv_id                TYPE string,
      lv_patched           TYPE abap_bool,
      lv_is_patch_possible TYPE abap_bool.

    lv_patched = get_diff_object( iv_filename )->is_line_patched( iv_index ).

    lv_is_patch_possible = is_patch_line_possible( is_diff_line ).

    IF lv_is_patch_possible = abap_true.

      lv_id = |{ iv_filename }_{ mv_section_count }_{ iv_index }|.

      ii_html->add( |<td class="{ lc_css_class-patch }">| ).
      ii_html->add_checkbox(
          iv_id      = |patch_line_{ lv_id }|
          iv_checked = lv_patched ).
      ii_html->add( |</td>| ).

    ELSE.

      ii_html->add( |<td class="{ lc_css_class-patch }">| ).
      ii_html->add( |</td>| ).

    ENDIF.

  ENDMETHOD.
  METHOD render_patch_head.

    ii_html->add( |<th class="patch">| ).
    ii_html->add_checkbox( |patch_file_{ get_normalized_fname_with_path( is_diff ) }| ).
    ii_html->add( '</th>' ).

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( 'preparePatch();' ).
    ri_html->add( 'registerStagePatch();' ).

  ENDMETHOD.
  METHOD render_table_head_non_unified.

    render_patch_head( ii_html = ii_html
                       is_diff = is_diff ).

    super->render_table_head_non_unified(
        ii_html = ii_html
        is_diff = is_diff ).

  ENDMETHOD.
  METHOD restore_patch_flags.

    DATA:
      lt_diff_old TYPE Lif_abapgit_definitions=>ty_diffs_tt.

    FIELD-SYMBOLS:
      <ls_diff_file>     TYPE ty_file_diff,
      <ls_diff_file_old> TYPE ty_file_diff,
      <ls_diff_old>      TYPE Lif_abapgit_definitions=>ty_diff.

    LOOP AT mt_diff_files ASSIGNING <ls_diff_file>.

      READ TABLE it_diff_files_old ASSIGNING <ls_diff_file_old>
                                   WITH KEY secondary
                                   COMPONENTS path     = <ls_diff_file>-path
                                              filename = <ls_diff_file>-filename.
      IF sy-subrc <> 0.
        CONTINUE. " e.g. new objects
      ENDIF.

      IF <ls_diff_file_old>-o_diff IS NOT BOUND.
        CONTINUE. " e.g. binary files
      ENDIF.

      lt_diff_old = <ls_diff_file_old>-o_diff->get( ).

      LOOP AT lt_diff_old ASSIGNING <ls_diff_old>
                          WHERE patch_flag = abap_true.

        <ls_diff_file>-o_diff->set_patch_by_old_diff(
            is_diff_old   = <ls_diff_old>
            iv_patch_flag = abap_true ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
  METHOD start_staging.

    apply_patch_from_form_fields( ii_event ).
    add_to_stage( ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    CASE ii_event->mv_action.
      WHEN c_patch_actions-stage.

        start_staging( ii_event ).

        rs_handled-page = Lcl_abapgit_gui_page_commit=>create(
          io_repo  = mo_repo_online
          io_stage = mo_stage ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN OTHERS.

        IF is_refresh( ii_event->mv_action ) = abap_true.

          apply_patch_from_form_fields( ii_event ).
          refresh( ii_event->mv_action ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

        ELSE.

          rs_handled = super->Lif_abapgit_gui_event_handler~on_event( ii_event ).

        ENDIF.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Patch'.

    ls_hotkey_action-description = |Stage Changes|.
    ls_hotkey_action-action      = |stagePatch|.
    ls_hotkey_action-hotkey      = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Refresh Local|.
    ls_hotkey_action-action      = |refreshLocal|.
    ls_hotkey_action-hotkey      = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Refresh All|.
    ls_hotkey_action-action      = |refreshAll|.
    ls_hotkey_action-hotkey      = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_patch.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key    = iv_key
        is_file   = is_file
        is_object = is_object
        it_files  = it_files.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Patch'
      iv_page_layout        = Lcl_abapgit_gui_page=>c_page_layout-full_width
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    CLEAR mv_section_count.

    IF mv_pushed = abap_true.
      refresh_full( ).
      calculate_diff( ).
      CLEAR mv_pushed.
    ENDIF.

    register_handlers( ).

    ri_html = super->Lif_abapgit_gui_renderable~render( ).

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_PATCH implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_REPO_VIEW <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_repo_viewccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_repo_viewccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_REPO_VIEW implementation.
*"* method's implementations
*include methods.
  METHOD apply_order_by.

    DATA:
      lt_sort                        TYPE abap_sortorder_tab,
      ls_sort                        LIKE LINE OF lt_sort,
      lt_non_code_and_metadata_items LIKE ct_repo_items,
      lt_code_items                  LIKE ct_repo_items,
      lt_diff_items                  LIKE ct_repo_items.

    FIELD-SYMBOLS:
      <ls_repo_item> TYPE Lif_abapgit_definitions=>ty_repo_item.

    IF mv_order_by IS INITIAL.
      RETURN.
    ENDIF.

    " we want to preserve non-code and metadata files at the top,
    " so we isolate them and and sort only the code artifacts
    LOOP AT ct_repo_items ASSIGNING <ls_repo_item>.

      IF <ls_repo_item>-obj_type IS INITIAL AND <ls_repo_item>-is_dir = abap_false.
        INSERT <ls_repo_item> INTO TABLE lt_non_code_and_metadata_items.
      ELSE.
        INSERT <ls_repo_item> INTO TABLE lt_code_items.
      ENDIF.

    ENDLOOP.

    IF mv_diff_first = abap_true.
      " fix diffs on the top, right after non-code and metadata
      LOOP AT lt_code_items ASSIGNING <ls_repo_item>
                            WHERE changes > 0.
        INSERT <ls_repo_item> INTO TABLE lt_diff_items.
      ENDLOOP.

      DELETE lt_code_items WHERE changes > 0.
    ENDIF.

    CLEAR: ct_repo_items.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = mv_order_by.
    INSERT ls_sort INTO TABLE lt_sort.

    " Combine state fields for order of 'Status' column
    IF mv_order_by = 'LSTATE'.
      ls_sort-name = 'RSTATE'.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    " Use object name as secondary sort criteria
    IF mv_order_by <> 'OBJ_NAME'.
      ls_sort-name = 'OBJ_NAME'.
      INSERT ls_sort INTO TABLE lt_sort.
    ENDIF.

    SORT lt_code_items STABLE BY (lt_sort).
    SORT lt_diff_items STABLE BY (lt_sort).

    INSERT LINES OF lt_non_code_and_metadata_items INTO TABLE ct_repo_items.
    INSERT LINES OF lt_diff_items INTO TABLE ct_repo_items.
    INSERT LINES OF lt_code_items INTO TABLE ct_repo_items.

    " Files are listed under the object names so we always sort them by name
    LOOP AT ct_repo_items ASSIGNING <ls_repo_item>.
      order_files( CHANGING ct_files = <ls_repo_item>-files ).
    ENDLOOP.

  ENDMETHOD.
  METHOD build_advanced_dropdown.

    CREATE OBJECT ro_advanced_dropdown.

    ro_advanced_dropdown->add( iv_txt = 'Activate Objects'
                               iv_act = |{ Lif_abapgit_definitions=>c_action-repo_activate_objects }?key={ mv_key }| ).

    IF mo_repo->is_offline( ) = abap_false. " Online ?
      ro_advanced_dropdown->add(
        iv_txt = 'Transport to Branch'
        iv_act = |{ Lif_abapgit_definitions=>c_action-repo_transport_to_branch }?key={ mv_key }|
        iv_opt = get_crossout( Lif_abapgit_auth=>c_authorization-transport_to_branch ) ).
    ENDIF.

    IF mv_are_changes_recorded_in_tr = abap_true.
      ro_advanced_dropdown->add(
        iv_txt = 'Add All Objects to Transport'
        iv_act = |{ Lif_abapgit_definitions=>c_action-repo_add_all_obj_to_trans_req }?key={ mv_key }| ).
    ENDIF.
    IF mo_repo->is_offline( ) = abap_true.
      ro_advanced_dropdown->add( iv_txt = 'Export by Transport'
                                 iv_act = |{ Lif_abapgit_definitions=>c_action-zip_export_transport }?key={ mv_key }| ).
    ELSE.
      ro_advanced_dropdown->add( iv_txt = 'Stage by Transport'
                                 iv_act = |{ Lif_abapgit_definitions=>c_action-go_stage_transport }?key={ mv_key }| ).
    ENDIF.

    ro_advanced_dropdown->add( iv_txt = 'Quality Assurance'
                               iv_typ = Lif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt = 'Syntax Check'
                               iv_act = |{ Lif_abapgit_definitions=>c_action-repo_syntax_check }?key={ mv_key }| ).
    ro_advanced_dropdown->add( iv_txt = 'Unit Test'
                               iv_act = |{ c_actions-go_unit }| ).
    ro_advanced_dropdown->add( iv_txt = 'Run Code Inspector'
                               iv_act = |{ Lif_abapgit_definitions=>c_action-repo_code_inspector }?key={ mv_key }| ).

    ro_advanced_dropdown->add( iv_txt = 'Very Advanced'
                               iv_typ = Lif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt = 'Update Local Checksums'
                               iv_act = |{ Lif_abapgit_definitions=>c_action-repo_refresh_checksums }?key={ mv_key }|
                               iv_opt = get_crossout( Lif_abapgit_auth=>c_authorization-update_local_checksum ) ).

    ro_advanced_dropdown->add( iv_txt = 'Data Config'
                               iv_act = |{ c_actions-go_data }?key={ mv_key }| ).

    IF is_repo_lang_logon_lang( ) = abap_false AND Lcl_abapgit_services_abapgit=>get_abapgit_tcode( ) IS NOT INITIAL.
      ro_advanced_dropdown->add(
        iv_txt = 'Open in Main Language'
        iv_act = |{ Lif_abapgit_definitions=>c_action-repo_open_in_master_lang }?key={ mv_key }| ).
    ENDIF.

    ro_advanced_dropdown->add( iv_txt = 'Danger'
                               iv_typ = Lif_abapgit_html=>c_action_type-separator ).

    ro_advanced_dropdown->add( iv_txt   = 'Remove Repository'
                               iv_title = `Remove abapGit's records of the repository (the system's `
                                          && `development objects will remain unaffected)`
                               iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_remove }?key={ mv_key }| ).

    ro_advanced_dropdown->add( iv_txt   = 'Remove Objects'
                               iv_title = `Delete all development objects belonging to this package `
                                          && `(and subpackages) from the system, but keep repository in abapGit`
                               iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_delete_objects }?key={ mv_key }| ).

    ro_advanced_dropdown->add( iv_txt   = 'Uninstall'
                               iv_title = `Delete all development objects belonging to this package `
                                          && `(and subpackages) from the system, and remove the repository from abapGit`
                               iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_purge }?key={ mv_key }|
                               iv_opt   = get_crossout(
                                            iv_authorization = Lif_abapgit_auth=>c_authorization-uninstall
                                            iv_protected     = abap_true ) ).

  ENDMETHOD.
  METHOD build_branch_dropdown.

    CREATE OBJECT ro_branch_dropdown.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    ro_branch_dropdown->add( iv_txt = 'Switch'
                             iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_switch }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Create'
                             iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_create }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Delete'
                             iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_delete }?key={ mv_key }| ).
    ro_branch_dropdown->add( iv_txt = 'Merge'
                             iv_act = |{ Lif_abapgit_definitions=>c_action-git_branch_merge }?key={ mv_key }| ).

  ENDMETHOD.
  METHOD build_dir_jump_link.

    DATA lv_path   TYPE string.
    DATA lv_encode TYPE string.
    DATA li_html TYPE REF TO Lif_abapgit_html.

    CREATE OBJECT li_html TYPE Lcl_abapgit_html.

    lv_path = iv_path.
    REPLACE FIRST OCCURRENCE OF mv_cur_dir IN lv_path WITH ''.
    lv_encode = Lcl_abapgit_html_action_utils=>dir_encode( lv_path ).

    " remove leading and trailing / for display
    IF lv_path <> '/'.
      IF lv_path(1) = '/'.
        lv_path = lv_path+1.
      ENDIF.
      IF substring( val = reverse( lv_path )
                    len = 1 ) = '/'.
        lv_path = substring( val = lv_path
                             len = strlen( lv_path ) - 1 ).
      ENDIF.
    ENDIF.

    rv_html = li_html->a(
      iv_txt = lv_path
      iv_act = |{ c_actions-change_dir }?{ lv_encode }| ).

  ENDMETHOD.
  METHOD build_inactive_object_code.

    IF is_item-inactive = abap_true.
      rv_inactive_html_code = Lcl_abapgit_html=>icon(
        iv_name  = 'bolt/orange'
        iv_hint  = 'Object or object part is inactive'
        iv_class = 'inactive' ).
    ENDIF.

  ENDMETHOD.
  METHOD build_main_toolbar.

    DATA:
      li_log TYPE REF TO Lif_abapgit_log.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-repo'.

    IF mo_repo->is_offline( ) = abap_false.
      " online repo

      IF mo_repo_aggregated_state->is_unchanged( ) = abap_false. " Any changes
        ro_toolbar->add( iv_txt = 'Pull'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-git_pull }?key={ mv_key }|
                         iv_opt = get_crossout( iv_protected = abap_true
                                                iv_strong    = abap_true ) ).
        ro_toolbar->add( iv_txt = 'Stage'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-go_stage }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Patch'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-go_patch }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Diff'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-go_repo_diff }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-repo_log }?key={ mv_key }| ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Branch'
                       io_sub = build_branch_dropdown( ) ).
      ro_toolbar->add( iv_txt = 'Tag'
                       io_sub = build_tag_dropdown( ) ).

    ELSE.
      " offline repo

      IF mo_repo->has_remote_source( ) = abap_true AND mo_repo_aggregated_state->is_unchanged( ) = abap_false.
        ro_toolbar->add( iv_txt = 'Pull <sup>zip</sup>'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-git_pull }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
        ro_toolbar->add( iv_txt = 'Diff'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-go_repo_diff }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Import <sup>zip</sup>'
                       iv_act = |{ Lif_abapgit_definitions=>c_action-zip_import }?key={ mv_key }|
                       iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      IF mo_repo->get_local_settings( )-write_protected = abap_true.
        ro_toolbar->add( iv_txt = 'Compare <sup>rfc</sup>'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-rfc_compare }?key={ mv_key }|
                         iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      ENDIF.
      ro_toolbar->add( iv_txt = 'Export <sup>zip</sup>'
                       iv_act = |{ Lif_abapgit_definitions=>c_action-zip_export }?key={ mv_key }|
                       iv_opt = Lif_abapgit_html=>c_html_opt-strong ).
      li_log = mo_repo->get_log( ).
      IF li_log IS BOUND AND li_log->count( ) > 0.
        ro_toolbar->add( iv_txt = 'Log'
                         iv_act = |{ Lif_abapgit_definitions=>c_action-repo_log }?key={ mv_key }| ).
      ENDIF.

    ENDIF.

    ro_toolbar->add( iv_txt = 'Advanced'
                     io_sub = build_advanced_dropdown( ) ).

    ro_toolbar->add( iv_txt = 'View'
                     io_sub = build_view_dropdown( ) ).

    ro_toolbar->add( iv_txt = 'Refresh'
                     iv_act = |{ Lif_abapgit_definitions=>c_action-repo_refresh }?key={ mv_key }|
                     iv_opt = Lif_abapgit_html=>c_html_opt-strong ).

    ro_toolbar->add( iv_txt   = 'Settings'
                     iv_act   = |{ Lif_abapgit_definitions=>c_action-repo_settings }?key={ mv_key }|
                     iv_opt   = Lif_abapgit_html=>c_html_opt-strong
                     iv_title = `Repository Settings` ).

  ENDMETHOD.
  METHOD build_srcsystem_code.

    IF is_item-srcsystem IS NOT INITIAL AND is_item-srcsystem <> sy-sysid.
      rv_srcsystem_html_code = Lcl_abapgit_html=>icon(
        iv_name  = 'server-solid/grey'
        iv_hint  = |Original system: { is_item-srcsystem }|
        iv_class = 'cursor-pointer' ).
    ENDIF.

  ENDMETHOD.
  METHOD build_tag_dropdown.

    CREATE OBJECT ro_tag_dropdown.

    IF mo_repo->is_offline( ) = abap_true.
      RETURN.
    ENDIF.

    ro_tag_dropdown->add( iv_txt = 'Switch'
                          iv_act = |{ Lif_abapgit_definitions=>c_action-git_tag_switch }?key={ mv_key }| ).
    ro_tag_dropdown->add( iv_txt = 'Create'
                          iv_act = |{ Lif_abapgit_definitions=>c_action-git_tag_create }?key={ mv_key }| ).
    ro_tag_dropdown->add( iv_txt = 'Delete'
                          iv_act = |{ Lif_abapgit_definitions=>c_action-git_tag_delete }?key={ mv_key }| ).


  ENDMETHOD.
  METHOD build_view_dropdown.

    CREATE OBJECT ro_toolbar.

    ro_toolbar->add(
      iv_txt = 'Changes First'
      iv_chk = mv_diff_first
      iv_act = c_actions-toggle_diff_first ).

    ro_toolbar->add(
      iv_txt = 'Changes Only'
      iv_chk = mv_changes_only
      iv_act = c_actions-toggle_changes ).

    ro_toolbar->add(
      iv_txt = 'File Paths'
      iv_chk = boolc( NOT mv_hide_files = abap_true )
      iv_act = c_actions-toggle_hide_files ).

    ro_toolbar->add(
      iv_txt = 'Folders'
      iv_chk = mv_show_folders
      iv_act = c_actions-toggle_folders ).

  ENDMETHOD.
  METHOD check_branch.

    DATA lo_repo TYPE REF TO Lif_abapgit_repo_online.

    IF mo_repo->is_offline( ) = abap_false.
      lo_repo ?= mo_repo.
      lo_repo->check_for_valid_branch( ).
    ENDIF.

  ENDMETHOD.
  METHOD constructor.

    DATA: lo_settings         TYPE REF TO Lcl_abapgit_settings,
          lx_error            TYPE REF TO Lcx_abapgit_exception,
          lo_persistence_user TYPE REF TO Lif_abapgit_persist_user.

    super->constructor( ).

    TRY.
        lo_persistence_user = Lcl_abapgit_persistence_user=>get_instance( ).

        mv_key = iv_key.
        mo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
        mv_cur_dir = '/'. " Root

        mv_hide_files = lo_persistence_user->get_hide_files( ).
        mv_changes_only = lo_persistence_user->get_changes_only( ).
        mv_order_by = lo_persistence_user->get_order_by( ).
        mv_order_descending = lo_persistence_user->get_order_descending( ).
        mv_diff_first = lo_persistence_user->get_diff_first( ).
        mv_show_folders = lo_persistence_user->get_show_folders( ).

        " Read global settings to get max # of objects to be listed
        lo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).
        mv_max_lines = lo_settings->get_max_lines( ).
        mv_max_setting = mv_max_lines.

      CATCH Lcx_abapgit_exception INTO lx_error.
        " Reset 'last shown repo' so next start will go to repo overview
        " and allow troubleshooting of issue
        Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).

        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_repo_view.

    CREATE OBJECT lo_component
      EXPORTING
        iv_key = iv_key.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Repository'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD get_crossout.
    IF iv_strong = abap_true.
      rv_crossout = Lif_abapgit_html=>c_html_opt-strong.
    ENDIF.
    IF iv_protected = abap_true AND mo_repo->get_local_settings( )-write_protected = abap_true.
      rv_crossout = Lif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
    IF iv_authorization IS NOT INITIAL AND Lcl_abapgit_auth=>is_allowed( iv_authorization ) = abap_false.
      rv_crossout = Lif_abapgit_html=>c_html_opt-crossout.
    ENDIF.
  ENDMETHOD.
  METHOD get_item_class.

    DATA lt_class TYPE TABLE OF string.

    IF iv_is_object_row = abap_true.
      APPEND 'object_row' TO lt_class.
    ELSE.
      APPEND 'file_row' TO lt_class.
    ENDIF.

    IF is_item-is_dir = abap_true.
      APPEND 'folder' TO lt_class.
    ELSEIF is_item-changes > 0.
      APPEND 'modified' TO lt_class.
    ELSEIF is_item-obj_name IS INITIAL.
      APPEND 'unsupported' TO lt_class.
    ENDIF.

    IF lines( lt_class ) > 0.
      rv_html = | class="{ concat_lines_of( table = lt_class
                                            sep = ` ` ) }"|.
    ENDIF.

  ENDMETHOD.
  METHOD is_repo_lang_logon_lang.
    rv_repo_lang_is_logon_lang = boolc( mo_repo->get_dot_abapgit( )->get_main_language( ) = sy-langu ).
  ENDMETHOD.
  METHOD open_in_main_language.

    DATA:
      lv_main_language TYPE spras,
      ls_item          TYPE Lif_abapgit_definitions=>ty_item,
      lv_tcode         TYPE tcode.

    lv_main_language = mo_repo->get_dot_abapgit( )->get_main_language( ).
    lv_tcode = Lcl_abapgit_services_abapgit=>get_abapgit_tcode( ).
    ASSERT lv_tcode IS NOT INITIAL.

    IF lv_main_language = sy-langu.
      Lcx_abapgit_exception=>raise( |Repo already opened in main language| ).
    ENDIF.

    ls_item-obj_name = lv_tcode.
    ls_item-obj_type = |TRAN|.

    IF Lcl_abapgit_objects=>exists( ls_item ) = abap_false.
      Lcx_abapgit_exception=>raise( |Please install the abapGit repository| ).
    ENDIF.

    Lcl_abapgit_objects_factory=>get_gui_jumper( )->jump_abapgit(
      iv_language = lv_main_language
      iv_key      = mo_repo->get_key( ) ).

  ENDMETHOD.
  METHOD order_files.

    DATA:
      lt_sort TYPE abap_sortorder_tab,
      ls_sort LIKE LINE OF lt_sort.

    IF lines( ct_files ) = 0.
      RETURN.
    ENDIF.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = 'PATH'.
    INSERT ls_sort INTO TABLE lt_sort.

    ls_sort-descending = mv_order_descending.
    ls_sort-astext     = abap_true.
    ls_sort-name       = 'FILENAME'.
    INSERT ls_sort INTO TABLE lt_sort.

    SORT ct_files STABLE BY (lt_sort).

  ENDMETHOD.
  METHOD render_file_command.

    DATA: lv_difflink TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div>' ).
    IF is_file-is_changed = abap_true.
      lv_difflink = Lcl_abapgit_html_action_utils=>file_encode(
        iv_key  = mo_repo->get_key( )
        ig_file = is_file ).
      ri_html->add_a( iv_txt = 'diff'
                      iv_act = |{ Lif_abapgit_definitions=>c_action-go_file_diff }?{ lv_difflink }| ).
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_file-lstate
                                                                  iv_rstate = is_file-rstate ) ).
    ELSE.
      ri_html->add( '&nbsp;' ).
    ENDIF.
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_head_line.

    DATA:
      lo_toolbar      TYPE REF TO Lcl_abapgit_html_toolbar,
      ls_settings     TYPE Lif_abapgit_definitions=>ty_s_user_settings,
      lo_label_colors TYPE REF TO Lcl_abapgit_string_map,
      lt_labels       TYPE string_table.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    lo_toolbar = build_main_toolbar( ).

    ri_html->add( '<div class="paddings">' ).
    ri_html->add( '<table class="w100"><tr>' ).

    IF mv_show_folders = abap_true.
      ri_html->add( '<td class="current_dir">' ).
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_path( mv_cur_dir ) ).
      ri_html->add( '</td>' ).
    ENDIF.

    lt_labels = Lcl_abapgit_repo_labels=>split( mo_repo->ms_data-local_settings-labels ).

    IF lines( lt_labels ) > 0.
      ls_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_user_settings( ).
      lo_label_colors = Lcl_abapgit_repo_labels=>split_colors_into_map( ls_settings-label_colors ).

      ri_html->td(
        iv_content = Lcl_abapgit_gui_chunk_lib=>render_label_list(
                       it_labels = lt_labels
                       io_label_colors = lo_label_colors )
        iv_class   = 'labels' ).
    ENDIF.

    ri_html->add( '<td class="right">' ).
    ri_html->add( lo_toolbar->render( iv_right = abap_true ) ).
    ri_html->add( '</td>' ).
    ri_html->add( '</tr></table>' ).
    ri_html->add( '</div>' ).

  ENDMETHOD.
  METHOD render_item.

    DATA: lv_link    TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( |<tr{ get_item_class( is_item = is_item
                                        iv_is_object_row = abap_true ) }>| ).

    IF is_item-obj_name IS INITIAL AND is_item-is_dir = abap_false.
      ri_html->add( |<td colspan="2"></td>|
                 && '<td class="object">'
                 && '<i class="grey">non-code and meta files</i>'
                 && '</td>' ).
    ELSE.
      ri_html->add( |<td class="icon">{ Lcl_abapgit_gui_chunk_lib=>get_item_icon( is_item ) }</td>| ).

      IF is_item-is_dir = abap_true. " Subdir
        lv_link = build_dir_jump_link( is_item-path ).
        ri_html->add( |<td class="dir" colspan="2">{ lv_link }</td>| ).
      ELSE.
        lv_link = Lcl_abapgit_gui_chunk_lib=>get_item_link( is_item ).
        ri_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ri_html->add( |<td class="object">{ lv_link } { build_inactive_object_code( is_item )
                      } { build_srcsystem_code( is_item ) } { build_origlang_code( is_item ) }</td>| ).
      ENDIF.
    ENDIF.

    " Changed by
    ri_html->add( '<td class="user">' ).
    ri_html->add( render_item_changed_by( is_item ) ).
    ri_html->add( '</td>' ).

    IF iv_render_transports = abap_true.
      ri_html->add( render_item_transport( is_item ) ).
    ENDIF.

    " Command
    ri_html->add( '<td class="cmd">' ).
    IF mo_repo->has_remote_source( ) = abap_true.
      ri_html->add( render_item_command( is_item ) ).
    ENDIF.
    ri_html->add( '</td>' ).

    ri_html->add( '</tr>' ).

    ri_html->add( render_item_files( is_item ) ).

  ENDMETHOD.
  METHOD render_item_changed_by.
    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF is_item-changes = 0 OR is_item-changed_by IS INITIAL.
      ri_html->add( '&nbsp;' ).
    ELSE.
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_user_name( is_item-changed_by ) ).
    ENDIF.

  ENDMETHOD.
  METHOD render_item_command.

    DATA lv_difflink TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF is_item-is_dir = abap_true. " Directory
      ri_html->add( '<div>' ).
      ri_html->add( |<span class="grey">{ is_item-changes } changes</span>| ).
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                                  iv_rstate = is_item-rstate ) ).
      ri_html->add( '</div>' ).

    ELSEIF is_item-changes > 0.
      IF mv_hide_files = abap_true AND is_item-obj_name IS NOT INITIAL.

        lv_difflink = Lcl_abapgit_html_action_utils=>obj_encode(
          iv_key    = mo_repo->get_key( )
          ig_object = is_item ).

        ri_html->add( '<div>' ).
        ri_html->add_a( iv_txt = |diff ({ is_item-changes })|
                        iv_act = |{ Lif_abapgit_definitions=>c_action-go_file_diff }?{ lv_difflink }| ).
        ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state( iv_lstate = is_item-lstate
                                                                    iv_rstate = is_item-rstate ) ).
        ri_html->add( '</div>' ).

      ENDIF.

    ENDIF.

  ENDMETHOD.
  METHOD render_item_files.

    DATA: ls_file LIKE LINE OF is_item-files.
    DATA li_exit TYPE REF TO Lif_abapgit_exit.
    DATA lv_filename TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF mv_hide_files = abap_true AND is_item-obj_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    li_exit = Lcl_abapgit_exit=>get_instance( ).

    LOOP AT is_item-files INTO ls_file.
      ri_html->add( |<tr{ get_item_class( is_item ) }>| ).

      ri_html->add( |<td class="icon"></td>| ).

      ri_html->add( |<td class="type"></td>| ).
      ri_html->add( |<td class="filename darkgrey">| ).

      IF mv_show_folders = abap_true.
        lv_filename = ls_file-filename.
      ELSE.
        lv_filename = ls_file-path && ls_file-filename.
      ENDIF.

      lv_filename = li_exit->adjust_display_filename(
        is_repo_meta = mo_repo->ms_data
        iv_filename  = lv_filename ).

      ri_html->add( |<div>{ lv_filename }</div>| ).

      ri_html->add( |</td>| ).

      " Changed by (not applicable to file)
      ri_html->add( '<td class="user">' ).
      ri_html->add( '</td>' ).

      " Transport (not applicable to file)
      IF mv_are_changes_recorded_in_tr = abap_true.
        ri_html->add( `<td></td>` ).
      ENDIF.

      " Command
      ri_html->add( '<td class="cmd">' ).
      IF mo_repo->has_remote_source( ) = abap_true.
        ri_html->add( render_file_command( ls_file ) ).
      ENDIF.
      ri_html->add( '</td>' ).

      ri_html->add( '</tr>' ).

    ENDLOOP.

  ENDMETHOD.
  METHOD render_item_transport.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<td class="transport">' ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_transport( is_item-transport ) ).

    ri_html->add( '</td>' ).

  ENDMETHOD.
  METHOD render_parent_dir.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<tr class="folder">' ).
    ri_html->add( |<td class="icon">{ ri_html->icon( 'folder' ) }</td>| ).
    ri_html->add( |<td class="dir" colspan="4">{ build_dir_jump_link( '..' ) }</td>| ).
    IF mo_repo->has_remote_source( ) = abap_true.
      ri_html->add( |<td colspan="1"></td>| ). " Dummy for online
    ENDIF.
    ri_html->add( '</tr>' ).

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_palette(
      iv_action = Lif_abapgit_definitions=>c_action-go_repo ) ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA lv_path TYPE string.
    DATA lv_key TYPE Lif_abapgit_persistence=>ty_value.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_repo. " Switch to another repo
        rs_handled-page  = create( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.

      WHEN c_actions-go_data.
        rs_handled-page  = Lcl_abapgit_gui_page_data=>create( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-go_unit.
        rs_handled-page  = Lcl_abapgit_gui_page_runit=>create( mo_repo ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_actions-toggle_hide_files. " Toggle file diplay
        mv_hide_files    = Lcl_abapgit_persistence_user=>get_instance( )->toggle_hide_files( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-change_dir.        " Change dir
        lv_path         = ii_event->query( )->get( 'PATH' ).
        mv_cur_dir = Lcl_abapgit_path=>change_dir(
          iv_cur_dir = mv_cur_dir
          iv_cd      = lv_path ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_folders.    " Toggle folder view
        mv_show_folders = Lcl_abapgit_persistence_user=>get_instance( )->toggle_show_folders( ).
        mv_cur_dir      = '/'. " Root
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_changes.    " Toggle changes only view
        mv_changes_only = Lcl_abapgit_persistence_user=>get_instance( )->toggle_changes_only( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-toggle_diff_first.
        mv_diff_first = Lcl_abapgit_persistence_user=>get_instance( )->set_diff_first(
          boolc( mv_diff_first = abap_false ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN c_actions-display_more.      " Increase MAX lines limit
        mv_max_lines    = mv_max_lines + mv_max_setting.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN Lif_abapgit_definitions=>c_action-change_order_by.
        mv_order_by = Lcl_abapgit_persistence_user=>get_instance( )->set_order_by(
          ii_event->query( )->get( 'ORDERBY' ) ).
        mv_order_descending = Lcl_abapgit_persistence_user=>get_instance( )->set_order_descending( abap_false ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN Lif_abapgit_definitions=>c_action-direction.
        mv_order_descending = Lcl_abapgit_persistence_user=>get_instance( )->set_order_descending(
          boolc( ii_event->query( )->get( 'DIRECTION' ) = 'DESCENDING' ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

      WHEN Lif_abapgit_definitions=>c_action-repo_open_in_master_lang.
        open_in_main_language( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA: ls_hotkey_action LIKE LINE OF rt_hotkey_actions.
    ls_hotkey_action-ui_component = 'Repo'.

    ls_hotkey_action-description   = |Stage|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_stage.
    ls_hotkey_action-hotkey = |s|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Switch Branch|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-git_branch_switch.
    ls_hotkey_action-hotkey = |b|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Repository List|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-abapgit_home.
    ls_hotkey_action-hotkey = |o|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Refresh Repository|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_refresh.
    ls_hotkey_action-hotkey = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Pull|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-git_pull.
    ls_hotkey_action-hotkey = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description = |Patch|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_patch.
    ls_hotkey_action-hotkey = |a|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Diff|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-go_repo_diff.
    ls_hotkey_action-hotkey = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Uninstall Repository|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_purge.
    ls_hotkey_action-hotkey = |u|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run Syntax Check|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_syntax_check.
    ls_hotkey_action-hotkey = |c|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run Unit Tests|.
    ls_hotkey_action-action = c_actions-go_unit.
    ls_hotkey_action-hotkey = |t|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Run Code Inspector|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_code_inspector.
    ls_hotkey_action-hotkey = |i|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description   = |Show Log|.
    ls_hotkey_action-action = Lif_abapgit_definitions=>c_action-repo_log.
    ls_hotkey_action-hotkey = |l|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    ro_toolbar->add(
      iv_txt = Lcl_abapgit_gui_buttons=>repo_list( )
      iv_act = Lif_abapgit_definitions=>c_action-abapgit_home
    )->add(
      iv_txt = Lcl_abapgit_gui_buttons=>help( )
      io_sub = Lcl_abapgit_gui_menus=>help( ) ).

    Lcl_abapgit_gui_menus=>experimental( ro_toolbar ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    DATA: lt_repo_items TYPE Lif_abapgit_definitions=>ty_repo_item_tt,
          lo_browser    TYPE REF TO Lcl_abapgit_repo_content_list,
          lx_error      TYPE REF TO Lcx_abapgit_exception,
          lv_max        TYPE abap_bool,
          lv_max_str    TYPE string,
          lv_add_str    TYPE string,
          li_log        TYPE REF TO Lif_abapgit_log,
          lv_msg        TYPE string,
          lo_news       TYPE REF TO Lcl_abapgit_news.

    FIELD-SYMBOLS <ls_item> LIKE LINE OF lt_repo_items.

    register_handlers( ).

    CREATE OBJECT mo_repo_aggregated_state.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    TRY.
        " Reinit, for the case of type change
        mo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( mo_repo->get_key( ) ).

        check_branch( ).

        mv_are_changes_recorded_in_tr = Lcl_abapgit_factory=>get_sap_package( mo_repo->get_package( )
          )->are_changes_recorded_in_tr_req( ).

        lo_news = Lcl_abapgit_news=>create( mo_repo ).

        ri_html->add( |<div class="repo" id="repo{ mv_key }">| ).
        ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
          io_repo               = mo_repo
          io_news               = lo_news
          iv_show_edit          = abap_true
          iv_interactive_branch = abap_true ) ).

        ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_news( io_news = lo_news ) ).

        Lcl_abapgit_exit=>get_instance( )->wall_message_repo(
          is_repo_meta = mo_repo->ms_data
          ii_html      = ri_html ).

        CREATE OBJECT lo_browser
          EXPORTING
            io_repo = mo_repo.

        lt_repo_items = lo_browser->list( iv_path         = mv_cur_dir
                                          iv_by_folders   = mv_show_folders
                                          iv_changes_only = mv_changes_only
                                          iv_transports   = mv_are_changes_recorded_in_tr ).

        apply_order_by( CHANGING ct_repo_items = lt_repo_items ).

        LOOP AT lt_repo_items ASSIGNING <ls_item>.
          mo_repo_aggregated_state->sum_with_repo_item( <ls_item> ).
        ENDLOOP.

        ri_html->add( render_head_line( ) ).

        li_log = lo_browser->get_log( ).
        IF li_log->count( ) > 0.
          ri_html->add( '<div class="log">' ).
          ri_html->add( Lcl_abapgit_log_viewer=>to_html( li_log ) ). " shows eg. list of unsupported objects
          ri_html->add( '</div>' ).
        ENDIF.

        ri_html->add( '<div class="repo_container">' ).

        CLEAR lv_msg.

        IF lines( lt_repo_items ) = 0.
          IF mv_changes_only = abap_true.
            IF mo_repo->is_offline( ) = abap_true.
              " Offline match banner
              IF mo_repo->has_remote_source( ) = abap_true.
                lv_msg = 'Local state completely <b>matches</b> the ZIP file'.
              ELSE.
                lv_msg = 'Import a ZIP file to see if there are any changes'.
              ENDIF.
            ELSE.
              " Online match banner
              lv_msg = 'Local state completely <b>matches</b> the remote repository'.
            ENDIF.
          ELSE.
            lv_msg = |Package is empty. Show { build_dir_jump_link( 'parent' ) } package|.
          ENDIF.
        ELSE.
          " Repo content table
          ri_html->add( '<table class="repo_tab">' ).

          ri_html->add( render_table_header( ) ).

          IF Lcl_abapgit_path=>is_root( mv_cur_dir ) = abap_false.
            ri_html->add( render_parent_dir( ) ).
          ENDIF.

          LOOP AT lt_repo_items ASSIGNING <ls_item>.
            IF mv_max_lines > 0 AND sy-tabix > mv_max_lines.
              lv_max = abap_true.
              EXIT. " current loop
            ENDIF.
            ri_html->add( render_item( is_item = <ls_item>
                                       iv_render_transports = mv_are_changes_recorded_in_tr ) ).
          ENDLOOP.

          ri_html->add( render_table_footer( ) ).

          ri_html->add( '</table>' ).
        ENDIF.

        IF NOT lv_msg IS INITIAL.
          ri_html->add( |<div class="panel success repo_banner">{ lv_msg }</div>| ).
        ENDIF.

        IF lv_max = abap_true.
          ri_html->add( '<div class = "dummydiv">' ).
          IF mv_max_lines = 1.
            lv_max_str = '1 object'.
          ELSE.
            lv_max_str = |first { mv_max_lines } objects|.
          ENDIF.
          lv_add_str = |+{ mv_max_setting }|.
          ri_html->add( |Only { lv_max_str } objects shown in list. Display {
            ri_html->a( iv_txt = lv_add_str
                        iv_act = c_actions-display_more )
            } more (change in Settings > {
            ri_html->a( iv_txt = 'Personal Settings'
                        iv_act = Lif_abapgit_definitions=>c_action-go_settings_personal )
            })| ).
          ri_html->add( '</div>' ).
        ENDIF.

        ri_html->add( '</div>' ).
        ri_html->add( '</div>' ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        " Reset 'last shown repo' so next start will go to repo overview
        " and allow troubleshooting of issue
        Lcl_abapgit_persistence_user=>get_instance( )->set_repo_show( || ).

        ri_html->add( render_head_line( ) ).

        ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_error(
          iv_extra_style = 'repo_banner'
          ix_error = lx_error ) ).
    ENDTRY.

    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
  METHOD build_origlang_code.

    IF is_item-origlang IS NOT INITIAL AND is_item-origlang <> mo_repo->get_dot_abapgit( )->get_main_language( ).
      rv_html_code = Lcl_abapgit_html=>icon(
        iv_name  = 'language-solid/grey'
        iv_hint  = |Original language: { is_item-origlang }|
        iv_class = 'cursor-pointer' ).
    ENDIF.

  ENDMETHOD.
  METHOD render_table_footer.

    DATA lv_action TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    IF mv_changes_only = abap_true.
      lv_action = ri_html->a(
        iv_txt = 'Show All'
        iv_act = c_actions-toggle_changes ).

      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_table_footer( |(Only changes are shown. { lv_action })| ) ).
    ENDIF.

  ENDMETHOD.
  METHOD render_table_header.

    DATA:
      lt_col_spec TYPE Lif_abapgit_definitions=>ty_col_spec_tt,
      ls_col_spec TYPE Lif_abapgit_definitions=>ty_col_spec.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    " icon
    APPEND INITIAL LINE TO lt_col_spec.

    ls_col_spec-tech_name = 'OBJ_TYPE'.
    ls_col_spec-display_name = 'Type'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    ls_col_spec-tech_name = 'OBJ_NAME'.
    ls_col_spec-display_name = 'Name'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    ls_col_spec-tech_name = 'CHANGED_BY'.
    ls_col_spec-display_name = 'Changed by'.
    ls_col_spec-allow_order_by = abap_true.
    APPEND ls_col_spec TO lt_col_spec.

    IF mv_are_changes_recorded_in_tr = abap_true.
      ls_col_spec-tech_name = 'TRANSPORT'.
      ls_col_spec-display_name = 'Transport'.
      ls_col_spec-allow_order_by = abap_true.
      APPEND ls_col_spec TO lt_col_spec.
    ENDIF.

    ls_col_spec-tech_name = 'LSTATE'.
    ls_col_spec-display_name = 'Status'.
    ls_col_spec-allow_order_by = abap_true.
    ls_col_spec-css_class = 'cmd'.
    APPEND ls_col_spec TO lt_col_spec.

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_table_header(
      it_col_spec         = lt_col_spec
      iv_order_by         = mv_order_by
      iv_order_descending = mv_order_descending ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_REPO_VIEW implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_STAGE <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_stage====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_stage====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_STAGE implementation.
*"* method's implementations
*include methods.
  METHOD check_selected.

    DATA:
      ls_file    TYPE Lif_abapgit_git_definitions=>ty_file,
      lv_pattern TYPE string,
      lv_msg     TYPE string.

    FIELD-SYMBOLS:
      <ls_item>     LIKE LINE OF io_files->mt_entries,
      <ls_item_chk> LIKE LINE OF io_files->mt_entries.

    " Check all added files if the exist in different paths (packages) without being removed
    LOOP AT io_files->mt_entries ASSIGNING <ls_item> WHERE v = Lif_abapgit_definitions=>c_method-add.

      Lcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = to_lower( <ls_item>-k )
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      " Skip packages since they all have identical filenames
      IF ls_file-filename <> 'package.devc.xml'.
        lv_pattern = '*/' && to_upper( ls_file-filename ).
        REPLACE ALL OCCURRENCES OF '#' IN lv_pattern WITH '##'. " for CP

        LOOP AT io_files->mt_entries ASSIGNING <ls_item_chk>
          WHERE k CP lv_pattern AND k <> <ls_item>-k AND v <> Lif_abapgit_definitions=>c_method-rm.

          lv_msg = |In order to add { to_lower( <ls_item>-k ) }, | &&
                   |you have to remove { to_lower( <ls_item_chk>-k ) }|.
          Lcx_abapgit_exception=>raise( lv_msg ).

        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD constructor.

    DATA lv_ts TYPE timestamp.

    super->constructor( ).

    mo_repo               = io_repo.
    mv_seed               = iv_seed.
    mv_sci_result         = iv_sci_result.
    mi_obj_filter         = ii_obj_filter.

    IF mv_seed IS INITIAL. " Generate based on time unless obtained from diff page
      GET TIME STAMP FIELD lv_ts.
      mv_seed = |stage{ lv_ts }|.
    ENDIF.

    init_files( ).

  ENDMETHOD.
  METHOD count_default_files_to_commit.

    FIELD-SYMBOLS <ls_status> LIKE LINE OF ms_files-status.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF ms_files-remote.

    rv_count = lines( ms_files-local ).

    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = <ls_remote>-path
          filename = <ls_remote>-filename.
      ASSERT sy-subrc = 0.

      IF <ls_status>-lstate = Lif_abapgit_definitions=>c_state-deleted
        AND <ls_status>-rstate = Lif_abapgit_definitions=>c_state-unchanged.
        rv_count = rv_count + 1.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD find_changed_by.

    DATA: ls_local             LIKE LINE OF it_files-local,
          ls_remote            LIKE LINE OF it_files-remote,
          ls_changed_by        LIKE LINE OF rt_changed_by,
          lt_changed_by_remote LIKE rt_changed_by,
          ls_item              TYPE Lif_abapgit_definitions=>ty_item,
          lv_transport         LIKE LINE OF it_transports,
          lv_user              TYPE uname.

    FIELD-SYMBOLS <ls_changed_by> LIKE LINE OF lt_changed_by_remote.

    LOOP AT it_files-local INTO ls_local WHERE NOT item IS INITIAL.
      ls_changed_by-item = ls_local-item.
      ls_changed_by-filename = ls_local-file-filename.
      ls_changed_by-name = Lcl_abapgit_objects=>changed_by(
        is_item     = ls_local-item
        iv_filename = ls_local-file-filename ).
      INSERT ls_changed_by INTO TABLE rt_changed_by.
    ENDLOOP.

    LOOP AT it_files-remote INTO ls_remote WHERE filename IS NOT INITIAL.
      TRY.
          Lcl_abapgit_filename_logic=>file_to_object(
            EXPORTING
              iv_filename = ls_remote-filename
              iv_path     = ls_remote-path
              io_dot      = mo_repo->get_dot_abapgit( )
            IMPORTING
              es_item     = ls_item ).
          ls_changed_by-item = ls_item.
          INSERT ls_changed_by INTO TABLE lt_changed_by_remote.
        CATCH Lcx_abapgit_exception.
      ENDTRY.
    ENDLOOP.

    LOOP AT lt_changed_by_remote ASSIGNING <ls_changed_by>.
      " deleted files might still be in a transport
      CLEAR lv_transport.
      READ TABLE it_transports WITH KEY
        obj_type = <ls_changed_by>-item-obj_type
        obj_name = <ls_changed_by>-item-obj_name
        INTO lv_transport.
      IF sy-subrc = 0.
        lv_user = Lcl_abapgit_factory=>get_cts_api( )->read_user( lv_transport-trkorr ).
        IF lv_user IS NOT INITIAL.
          <ls_changed_by>-name = lv_user.
        ENDIF.
      ENDIF.
      IF <ls_changed_by>-name IS INITIAL.
        <ls_changed_by>-name = Lcl_abapgit_objects_super=>c_user_unknown.
      ENDIF.
    ENDLOOP.

    INSERT LINES OF lt_changed_by_remote INTO TABLE rt_changed_by.

  ENDMETHOD.
  METHOD find_transports.

    DATA li_cts_api TYPE REF TO Lif_abapgit_cts_api.
    DATA lt_items TYPE Lif_abapgit_definitions=>ty_items_tt.
    DATA ls_item TYPE Lif_abapgit_definitions=>ty_item.
    DATA lo_dot TYPE REF TO Lcl_abapgit_dot_abapgit.
    FIELD-SYMBOLS <ls_local> LIKE LINE OF it_files-local.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF it_files-remote.


    li_cts_api = Lcl_abapgit_factory=>get_cts_api( ).

    TRY.
        LOOP AT it_files-local ASSIGNING <ls_local> WHERE item IS NOT INITIAL.
          IF li_cts_api->is_chrec_possible_for_package( <ls_local>-item-devclass ) = abap_false.
            RETURN. " Assume all other objects are also in packages without change recording
          ENDIF.
          APPEND <ls_local>-item TO lt_items.
        ENDLOOP.

        lo_dot = mo_repo->get_dot_abapgit( ).
        LOOP AT it_files-remote ASSIGNING <ls_remote> WHERE filename IS NOT INITIAL.
          Lcl_abapgit_filename_logic=>file_to_object(
            EXPORTING
              iv_filename = <ls_remote>-filename
              iv_path     = <ls_remote>-path
              io_dot      = lo_dot
            IMPORTING
              es_item     = ls_item ).
          IF ls_item IS INITIAL.
            CONTINUE.
          ENDIF.
          APPEND ls_item TO lt_items.
        ENDLOOP.

        SORT lt_items BY obj_type obj_name.
        DELETE ADJACENT DUPLICATES FROM lt_items COMPARING obj_type obj_name.

        rt_transports = li_cts_api->get_transports_for_list( lt_items ).

      CATCH Lcx_abapgit_exception.
    ENDTRY.

  ENDMETHOD.
  METHOD get_page_patch.

    DATA: lv_key   TYPE Lif_abapgit_persistence=>ty_repo-key,
          lt_files TYPE Lif_abapgit_definitions=>ty_stage_tt.

    lv_key = mo_repo->get_key( ).
    lt_files = io_stage->get_all( ).

    DELETE lt_files WHERE method <> Lif_abapgit_definitions=>c_method-add
                      AND method <> Lif_abapgit_definitions=>c_method-rm.

    ri_page  = Lcl_abapgit_gui_page_patch=>create(
      iv_key   = lv_key
      it_files = lt_files ).

  ENDMETHOD.
  METHOD init_files.
    ms_files = Lcl_abapgit_factory=>get_stage_logic( )->get( io_repo       = mo_repo
                                                             ii_obj_filter = mi_obj_filter ).

    IF lines( ms_files-local ) = 0 AND lines( ms_files-remote ) = 0.
      mo_repo->refresh( ).
      Lcx_abapgit_exception=>raise( 'There are no changes that could be staged' ).
    ENDIF.
  ENDMETHOD.
  METHOD render_actions.

    DATA: lv_local_count TYPE i,
          lv_add_all_txt TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.
    lv_local_count = count_default_files_to_commit( ).
    IF lv_local_count > 0.
      lv_add_all_txt = |Add All and Commit ({ lv_local_count })|.
      " Otherwise empty, but the element (id) is preserved for JS
    ENDIF.

    ri_html->add( '<table class="w100 margin-v5"><tr>' ).

    " Action buttons
    ri_html->add( '<td class="indent5em">' ).
    ri_html->add_a( iv_act   = 'errorStub(event)' " Will be reinit by JS
                    iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                    iv_id    = 'commitSelectedButton'
                    iv_style = 'display: none'
                    iv_txt   = 'Commit Selected (<span class="counter"></span>)'
                    iv_opt   = Lif_abapgit_html=>c_html_opt-strong ).
    ri_html->add_a( iv_act   = 'errorStub(event)' " Will be reinit by JS
                    iv_typ   = Lif_abapgit_html=>c_action_type-onclick
                    iv_id    = 'commitFilteredButton'
                    iv_style = 'display: none'
                    iv_txt   = 'Add <b>Filtered</b> and Commit (<span class="counter"></span>)' ).
    ri_html->add_a( iv_act = |{ c_action-stage_all }|
                    iv_id  = 'commitAllButton'
                    iv_txt = lv_add_all_txt ).


    ri_html->add( '</td>' ).

    " Filter bar
    ri_html->add( '<td class="right">' ).
    ri_html->add( '<input class="stage-filter" id="objectSearch"' &&
                  ' type="search" placeholder="Filter Objects"' &&
                  | value="{ mv_filter_value }">| ).
    Lcl_abapgit_gui_chunk_lib=>render_sci_result(
      ii_html       = ri_html
      iv_sci_result = mv_sci_result ).
    ri_html->add( '</td>' ).

    ri_html->add( '</tr>' ).
    ri_html->add( '</table>' ).

  ENDMETHOD.
  METHOD render_deferred_hidden_events.

    DATA ls_event TYPE Lcl_abapgit_gui_chunk_lib=>ty_event_signature.

    ls_event-method = 'post'.
    ls_event-name   = 'stage_commit'.
    ri_html = Lcl_abapgit_gui_chunk_lib=>render_event_as_form( ls_event ).
    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).

  ENDMETHOD.
  METHOD render_file.

    DATA: lv_param    TYPE string,
          lv_filename TYPE string.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_filename = is_file-path && is_file-filename.
    " make sure whitespace is preserved in the DOM
    REPLACE ALL OCCURRENCES OF ` ` IN lv_filename WITH '&nbsp;'.

    ri_html->add( |<tr class="{ iv_context }">| ).
    ri_html->add( '<td>' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_item_state(
      iv_lstate = is_status-lstate
      iv_rstate = is_status-rstate ) ).
    ri_html->add( '</td>' ).

    CASE iv_context.
      WHEN 'local'.
        lv_param = Lcl_abapgit_html_action_utils=>file_encode(
          iv_key  = mo_repo->get_key( )
          ig_file = is_file ).

        lv_filename = ri_html->a(
          iv_txt = lv_filename
          iv_act = |{ Lif_abapgit_definitions=>c_action-go_file_diff }?{ lv_param }| ).

        ri_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ri_html->add( |<td class="name">{ lv_filename }</td>| ).
      WHEN 'remote'.
        ri_html->add( |<td class="type">{ is_item-obj_type }</td>| ).
        ri_html->add( |<td class="name">{ lv_filename }</td>| ).
    ENDCASE.

    ri_html->add( '<td class="user">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_user_name( iv_changed_by ) ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="transport">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_transport( iv_transport ) ).
    ri_html->add( '</td>' ).

    ri_html->add( '<td class="status">?</td>' ).
    ri_html->add( '<td class="cmd"></td>' ). " Command added in JS

    ri_html->add( '</tr>' ).

  ENDMETHOD.
  METHOD render_list.

    DATA: lt_changed_by  TYPE ty_changed_by_tt,
          ls_changed_by  LIKE LINE OF lt_changed_by,
          lt_transports  TYPE Lif_abapgit_cts_api=>ty_transport_list,
          ls_transport   LIKE LINE OF lt_transports,
          ls_item_remote TYPE Lif_abapgit_definitions=>ty_item.

    FIELD-SYMBOLS: <ls_remote> LIKE LINE OF ms_files-remote,
                   <ls_status> LIKE LINE OF ms_files-status,
                   <ls_local>  LIKE LINE OF ms_files-local.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<table id="stageTab" class="stage_tab w100">' ).

    lt_transports = find_transports( ms_files ).
    lt_changed_by = find_changed_by(
      it_files = ms_files
      it_transports = lt_transports ).

    " Local changes
    LOOP AT ms_files-local ASSIGNING <ls_local>.
      AT FIRST.
        ri_html->add( '<thead><tr class="local">' ).
        ri_html->add( '<th class="stage-status"></th>' ). " Diff state
        ri_html->add( '<th class="stage-objtype">Type</th>' ).
        ri_html->add( '<th title="Click filename to see diff">File</th>' ).
        ri_html->add( '<th>Changed by</th>' ).
        ri_html->add( '<th>Transport</th>' ).
        ri_html->add( '<th></th>' ). " Status
        ri_html->add( '<th class="cmd">' ).
        ri_html->add( '<a>add</a>&#x2193; <a>reset</a>&#x2193;' ).
        ri_html->add( '</th>' ).
        ri_html->add( '</tr></thead>' ).
        ri_html->add( '<tbody>' ).
      ENDAT.

      READ TABLE lt_changed_by INTO ls_changed_by WITH TABLE KEY
        item     = <ls_local>-item
        filename = <ls_local>-file-filename.
      IF sy-subrc <> 0.
        READ TABLE lt_changed_by INTO ls_changed_by WITH KEY item = <ls_local>-item.
      ENDIF.

      READ TABLE lt_transports INTO ls_transport WITH KEY
        obj_type = <ls_local>-item-obj_type
        obj_name = <ls_local>-item-obj_name.              "#EC CI_SUBRC
      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      ASSERT sy-subrc = 0.

      ri_html->add( render_file(
        iv_context    = 'local'
        is_file       = <ls_local>-file
        is_item       = <ls_local>-item
        is_status     = <ls_status>
        iv_changed_by = ls_changed_by-name
        iv_transport  = ls_transport-trkorr ) ).

      CLEAR ls_transport.

      AT LAST.
        ri_html->add( '</tbody>' ).
      ENDAT.
    ENDLOOP.

    " Remote changes
    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      AT FIRST.
        ri_html->add( '<thead><tr class="remote">' ).
        ri_html->add( '<th></th>' ). " Diff state
        ri_html->add( '<th></th>' ). " Type
        ri_html->add( '<th colspan="3">Files to remove or non-code</th>' ).
        ri_html->add( '<th></th>' ). " Transport
        ri_html->add( '<th class="cmd">' ).
        ri_html->add( '<a>ignore</a>&#x2193; <a>remove</a>&#x2193; <a>reset</a>&#x2193;' ).
        ri_html->add( '</th>' ).
        ri_html->add( '</tr></thead>' ).
        ri_html->add( '<tbody>' ).
      ENDAT.

      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = <ls_remote>-path
          filename = <ls_remote>-filename.
      ASSERT sy-subrc = 0.

      TRY.
          Lcl_abapgit_filename_logic=>file_to_object(
            EXPORTING
              iv_filename = <ls_remote>-filename
              iv_path     = <ls_remote>-path
              io_dot      = mo_repo->get_dot_abapgit( )
            IMPORTING
              es_item     = ls_item_remote ).
          READ TABLE lt_transports INTO ls_transport WITH KEY
            obj_type = ls_item_remote-obj_type
            obj_name = ls_item_remote-obj_name.

          READ TABLE lt_changed_by INTO ls_changed_by WITH TABLE KEY
            item     = ls_item_remote
            filename = <ls_remote>-filename.
          IF sy-subrc <> 0.
            READ TABLE lt_changed_by INTO ls_changed_by WITH KEY item = ls_item_remote.
          ENDIF.
        CATCH Lcx_abapgit_exception.
          CLEAR ls_transport.
      ENDTRY.

      ri_html->add( render_file(
        iv_context    = 'remote'
        is_status     = <ls_status>
        is_file       = <ls_remote>
        is_item       = ls_item_remote
        iv_changed_by = ls_changed_by-name
        iv_transport  = ls_transport-trkorr ) ).

      AT LAST.
        ri_html->add( '</tbody>' ).
      ENDAT.
    ENDLOOP.

    ri_html->add( '</table>' ).

  ENDMETHOD.
  METHOD render_main_language_warning.

    DATA lv_main_language TYPE spras.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    lv_main_language = mo_repo->get_dot_abapgit( )->get_main_language( ).

    IF lv_main_language <> sy-langu.
      ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_warning_banner(
                        |Caution: Main language of the repo is '{ lv_main_language }', |
                     && |but you're logged on in '{ sy-langu }'| ) ).
    ENDIF.

  ENDMETHOD.
  METHOD render_scripts.

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->set_title( cl_abap_typedescr=>describe_by_object_ref( me )->get_relative_name( ) ).

    ri_html->add( 'var gStageParams = {' ).
    ri_html->add( |  seed:            "{ mv_seed }",| ). " Unique page id
    ri_html->add( |  user:            "{ to_lower( sy-uname ) }",| ).
    ri_html->add( '  formAction:      "stage_commit",' ).
    ri_html->add( |  patchAction:     "{ Lif_abapgit_definitions=>c_action-go_patch }",| ).
    ri_html->add( '  focusFilterKey:  "f",' ).

    ri_html->add( '  ids: {' ).
    ri_html->add( '    stageTab:          "stageTab",' ).
    ri_html->add( '    commitAllBtn:      "commitAllButton",' ).
    ri_html->add( '    commitSelectedBtn: "commitSelectedButton",' ).
    ri_html->add( '    commitFilteredBtn: "commitFilteredButton",' ).
    ri_html->add( '    patchBtn:          "patchBtn",' ).
    ri_html->add( '    objectSearch:      "objectSearch",' ).
    ri_html->add( '  }' ).

    ri_html->add( '}' ).
    ri_html->add( 'var gHelper = new StageHelper(gStageParams);' ).

  ENDMETHOD.
  METHOD stage_all.

    FIELD-SYMBOLS <ls_local> LIKE LINE OF ms_files-local.
    FIELD-SYMBOLS <ls_remote> LIKE LINE OF ms_files-remote.
    FIELD-SYMBOLS <ls_status> LIKE LINE OF ms_files-status.

    CREATE OBJECT ro_stage.

    LOOP AT ms_files-local ASSIGNING <ls_local>.
      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = <ls_local>-file-path
          filename = <ls_local>-file-filename.
      ASSERT sy-subrc = 0.

      ro_stage->add(
        iv_path     = <ls_local>-file-path
        iv_filename = <ls_local>-file-filename
        is_status   = <ls_status>
        iv_data     = <ls_local>-file-data ).
    ENDLOOP.

    LOOP AT ms_files-remote ASSIGNING <ls_remote>.
      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = <ls_remote>-path
          filename = <ls_remote>-filename.
      ASSERT sy-subrc = 0.

      IF <ls_status>-lstate = Lif_abapgit_definitions=>c_state-deleted
        AND <ls_status>-rstate = Lif_abapgit_definitions=>c_state-unchanged.

        ro_stage->rm(
          iv_path     = <ls_remote>-path
          iv_filename = <ls_remote>-filename
          is_status   = <ls_status> ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
  METHOD stage_selected.

    DATA ls_file  TYPE Lif_abapgit_git_definitions=>ty_file.
    DATA lo_files TYPE REF TO Lcl_abapgit_string_map.

    FIELD-SYMBOLS:
      <ls_file>   LIKE LINE OF ms_files-local,
      <ls_status> LIKE LINE OF ms_files-status,
      <ls_item>   LIKE LINE OF lo_files->mt_entries.

    lo_files = ii_event->form_data( ).

    IF lo_files->size( ) = 0.
      Lcx_abapgit_exception=>raise( 'process_stage_list: empty list' ).
    ENDIF.

    check_selected( lo_files ).

    CREATE OBJECT ro_stage.

    LOOP AT lo_files->mt_entries ASSIGNING <ls_item>
      "Ignore Files that we don't want to stage, so any errors don't stop the staging process
      WHERE v <> Lif_abapgit_definitions=>c_method-skip.

      Lcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = to_lower( <ls_item>-k ) " filename is lower cased
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      READ TABLE ms_files-status ASSIGNING <ls_status>
        WITH TABLE KEY
          path     = ls_file-path
          filename = ls_file-filename.
      IF sy-subrc <> 0.
* see https://github.com/abapGit/abapGit/issues/3073
        Lcx_abapgit_exception=>raise(
          |Unable to stage { ls_file-filename }. If the filename contains spaces, this is a known issue.| &&
          | Consider ignoring or staging the file at a later time.| ).
      ENDIF.

      CASE <ls_item>-v.
        WHEN Lif_abapgit_definitions=>c_method-add.
          READ TABLE ms_files-local ASSIGNING <ls_file>
            WITH KEY file-path     = ls_file-path
                     file-filename = ls_file-filename.

          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( |process_stage_list: unknown file { ls_file-path }{ ls_file-filename }| ).
          ENDIF.

          ro_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         is_status   = <ls_status>
                         iv_data     = <ls_file>-file-data ).
        WHEN Lif_abapgit_definitions=>c_method-ignore.
          ro_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN Lif_abapgit_definitions=>c_method-rm.
          ro_stage->rm( iv_path     = ls_file-path
                        is_status   = <ls_status>
                        iv_filename = ls_file-filename ).
        WHEN Lif_abapgit_definitions=>c_method-skip.
          " Do nothing
        WHEN OTHERS.
          Lcx_abapgit_exception=>raise( |process_stage_list: unknown method { <ls_item>-v }| ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA: lo_stage  TYPE REF TO Lcl_abapgit_stage.

    CASE ii_event->mv_action.
      WHEN c_action-stage_all.

        lo_stage = stage_all( ).

        rs_handled-page = Lcl_abapgit_gui_page_commit=>create(
          io_repo       = mo_repo
          io_stage      = lo_stage
          iv_sci_result = mv_sci_result ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-stage_commit.

        lo_stage = stage_selected( ii_event ).

        rs_handled-page = Lcl_abapgit_gui_page_commit=>create(
          io_repo       = mo_repo
          io_stage      = lo_stage
          iv_sci_result = mv_sci_result ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-stage_filter.

        mv_filter_value = ii_event->form_data( )->get( 'filterValue' ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-go_patch.                         " Go Patch page

        lo_stage = stage_selected( ii_event ).
        rs_handled-page  = get_page_patch( lo_stage ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.

      WHEN c_action-stage_refresh.
        mo_repo->refresh( abap_true ).
        init_files( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_branch_switch.
        Lcl_abapgit_services_git=>switch_branch( |{ ii_event->query( )->get( 'KEY' ) }| ).
        mo_repo->refresh( abap_true ).
        init_files( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_hotkeys~get_hotkey_actions.

    DATA ls_hotkey_action LIKE LINE OF rt_hotkey_actions.

    ls_hotkey_action-ui_component = 'Stage'.
    ls_hotkey_action-description  = |Patch|.
    ls_hotkey_action-action       = 'submitPatch'. " JS function in StageHelper
    ls_hotkey_action-hotkey       = |p|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description  = |Diff|.
    ls_hotkey_action-action       = Lif_abapgit_definitions=>c_action-go_repo_diff.
    ls_hotkey_action-hotkey       = |d|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    ls_hotkey_action-description  = |Refresh|.
    ls_hotkey_action-action       = c_action-stage_refresh.
    ls_hotkey_action-hotkey       = |r|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

    " registered/handled in js
    ls_hotkey_action-description = |Focus filter|.
    ls_hotkey_action-action = `#`.
    ls_hotkey_action-hotkey = |f|.
    INSERT ls_hotkey_action INTO TABLE rt_hotkey_actions.

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_stage.

    CREATE OBJECT lo_component
      EXPORTING
        io_repo       = io_repo
        iv_seed       = iv_seed
        iv_sci_result = iv_sci_result
        ii_obj_filter = ii_obj_filter.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title         = 'Stage'
      ii_page_menu_provider = lo_component
      ii_child_component    = lo_component ).

  ENDMETHOD.
  METHOD Lif_abapgit_gui_menu_provider~get_menu.

    CREATE OBJECT ro_toolbar EXPORTING iv_id = 'toolbar-main'.

    IF lines( ms_files-local ) > 0
    OR lines( ms_files-remote ) > 0.
      ro_toolbar->add(
        iv_txt = 'Refresh'
        iv_act = |{ c_action-stage_refresh }|
        iv_opt = Lif_abapgit_html=>c_html_opt-strong
      )->add(
        iv_txt = |Diff|
        iv_act = |{ Lif_abapgit_definitions=>c_action-go_repo_diff }?key={ mo_repo->get_key( ) }|
      )->add(
        iv_txt = |Patch|
        iv_typ = Lif_abapgit_html=>c_action_type-onclick
        iv_id  = |patchBtn| ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( '<div class="repo">' ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
      io_repo = mo_repo
      iv_interactive_branch = abap_true ) ).
    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_js_error_banner( ) ).
    ri_html->add( render_main_language_warning( ) ).

    ri_html->add( '<div class="stage-container">' ).
    ri_html->add( render_actions( ) ).
    ri_html->add( render_list( ) ).
    ri_html->add( '</div>' ).

    ri_html->add( '</div>' ).

    gui_services( )->get_html_parts( )->add_part(
      iv_collection = Lcl_abapgit_gui_component=>c_html_parts-hidden_forms
      ii_part       = render_deferred_hidden_events( ) ).
    register_deferred_script( render_scripts( ) ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_STAGE implementation

*>>>>>>> ZCL_ABAPGIT_GUI_PAGE_TAGS <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_page_tags=====ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_page_tags=====ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_PAGE_TAGS implementation.
*"* method's implementations
*include methods.
  METHOD choose_commit.

    DATA li_popups TYPE REF TO Lif_abapgit_popups.

    li_popups = Lcl_abapgit_ui_factory=>get_popups( ).

    rv_commit = li_popups->commit_list_popup(
      iv_repo_url    = mo_repo->get_url( )
      iv_branch_name = mo_repo->get_selected_branch( ) )-sha1.

  ENDMETHOD.
  METHOD constructor.

    super->constructor( ).
    CREATE OBJECT mo_form_data.
    CREATE OBJECT mo_validation_log.
    mo_repo ?= ii_repo.

    " Get settings from DB
    mo_settings = Lcl_abapgit_persist_factory=>get_settings( )->read( ).

    mo_form = get_form_schema( ).

    initialize_form_data( ).

  ENDMETHOD.
  METHOD create.

    DATA lo_component TYPE REF TO Lcl_abapgit_gui_page_tags.

    CREATE OBJECT lo_component
      EXPORTING
        ii_repo = ii_repo.

    ri_page = Lcl_abapgit_gui_page_hoc=>create(
      iv_page_title      = 'Create Tag'
      ii_child_component = lo_component ).

  ENDMETHOD.
  METHOD get_form_schema.

    DATA lv_commitmsg_comment_length TYPE i.

    IF io_form_data IS BOUND AND io_form_data->is_empty( ) = abap_false.
      ms_tag-type = io_form_data->get( c_id-tag_type ).
    ENDIF.

    lv_commitmsg_comment_length = mo_settings->get_commitmsg_comment_length( ).

    ro_form = Lcl_abapgit_html_form=>create(
                iv_form_id   = 'create-tag-form'
                iv_help_page = 'https://docs.abapgit.org/' ). " todo, add docs

    ro_form->start_group(
      iv_name  = c_id-tag_group
      iv_label = 'New Tag'
    )->radio(
      iv_label  = 'Type'
      iv_name   = c_id-tag_type
      iv_action = c_event-change_type
    )->option(
      iv_label = 'Lightweight'
      iv_value = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag
    )->option(
      iv_label = 'Annotated'
      iv_value = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag
    )->text(
      iv_name        = c_id-name
      iv_label       = 'Tag Name'
      iv_required    = abap_true
    )->text(
      iv_name        = c_id-sha1
      iv_label       = 'Commit'
      iv_min         = 40
      iv_max         = 40
      iv_condense    = abap_true
      iv_required    = abap_true
      iv_side_action = c_event-choose_commit ).

    IF ms_tag-type = Lif_abapgit_git_definitions=>c_git_branch_type-annotated_tag.
      ro_form->start_group(
        iv_name        = c_id-anno_group
        iv_label       = 'Annotation'
      )->text(
        iv_name        = c_id-message
        iv_label       = 'Comment'
        iv_max         = lv_commitmsg_comment_length
        iv_placeholder = |Add a mandatory comment with max { lv_commitmsg_comment_length } characters|
      )->textarea(
        iv_name        = c_id-body
        iv_label       = 'Body'
        iv_rows        = 6
        iv_cols        = mo_settings->get_commitmsg_body_size( )
        iv_placeholder = 'Add an optional description...'
      )->text(
        iv_name        = c_id-tagger_name
        iv_label       = 'Tagger Name'
      )->text(
        iv_name        = c_id-tagger_email
        iv_label       = 'Tagger Email' ).
    ELSE.
      ro_form->hidden( c_id-message
      )->hidden( c_id-body
      )->hidden( c_id-tagger_name
      )->hidden( c_id-tagger_email ).
    ENDIF.

    ro_form->command(
      iv_label       = 'Create'
      iv_cmd_type    = Lif_abapgit_html_form=>c_cmd_type-input_main
      iv_action      = c_event-create
    )->command(
      iv_label       = 'Back'
      iv_action      = Lif_abapgit_definitions=>c_action-go_back ).

  ENDMETHOD.
  METHOD get_tagger_email.

    DATA li_user TYPE REF TO Lif_abapgit_persist_user.

    li_user = Lcl_abapgit_persistence_user=>get_instance( ).

    rv_email = li_user->get_repo_git_user_email( mo_repo->get_url( ) ).
    IF rv_email IS INITIAL.
      rv_email = li_user->get_default_git_user_email( ).
    ENDIF.
    IF rv_email IS INITIAL.
      " get default from user record
      rv_email = Lcl_abapgit_user_record=>get_instance( sy-uname )->get_email( ).
    ENDIF.

  ENDMETHOD.
  METHOD get_tagger_name.

    DATA li_user TYPE REF TO Lif_abapgit_persist_user.

    li_user = Lcl_abapgit_persistence_user=>get_instance( ).

    rv_user  = li_user->get_repo_git_user_name( mo_repo->get_url( ) ).
    IF rv_user IS INITIAL.
      rv_user  = li_user->get_default_git_user_name( ).
    ENDIF.
    IF rv_user IS INITIAL.
      " get default from user record
      rv_user = Lcl_abapgit_user_record=>get_instance( sy-uname )->get_name( ).
    ENDIF.

  ENDMETHOD.
  METHOD initialize_form_data.

    ms_tag-type = Lif_abapgit_git_definitions=>c_git_branch_type-lightweight_tag.

    mo_form_data->set(
      iv_key = c_id-tag_type
      iv_val = ms_tag-type ).

    ms_tag-tagger_name  = get_tagger_name( ).
    ms_tag-tagger_email = get_tagger_email( ).

    mo_form_data->set(
      iv_key = c_id-tagger_name
      iv_val = ms_tag-tagger_name ).
    mo_form_data->set(
      iv_key = c_id-tagger_email
      iv_val = ms_tag-tagger_email ).

  ENDMETHOD.
  METHOD validate_form.

    DATA:
      lt_tags         TYPE Lif_abapgit_git_definitions=>ty_git_branch_list_tt,
      lv_new_tag_name TYPE string.

    ro_validation_log = Lcl_abapgit_html_form_utils=>create( mo_form )->validate( io_form_data ).

    IF Lcl_abapgit_utils=>is_valid_email( io_form_data->get( c_id-tagger_email ) ) = abap_false.
      ro_validation_log->set(
        iv_key = c_id-tagger_email
        iv_val = |Invalid email address| ).
    ENDIF.

    lv_new_tag_name = io_form_data->get( c_id-name ).

    IF lv_new_tag_name IS NOT INITIAL.
      " Check if tag already exists
      lt_tags = Lcl_abapgit_git_transport=>branches( mo_repo->get_url( ) )->get_tags_only( ).

      READ TABLE lt_tags TRANSPORTING NO FIELDS WITH TABLE KEY name_key
        COMPONENTS name = Lcl_abapgit_git_tag=>add_tag_prefix( lv_new_tag_name ).
      IF sy-subrc = 0.
        ro_validation_log->set(
          iv_key = c_id-name
          iv_val = |Tag already exists| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    DATA:
      lx_error  TYPE REF TO Lcx_abapgit_exception,
      lv_commit TYPE Lif_abapgit_git_definitions=>ty_sha1,
      lv_text   TYPE string.

    mo_form_data->merge( Lcl_abapgit_html_form_utils=>create( mo_form )->normalize( ii_event->form_data( ) ) ).

    CASE ii_event->mv_action.

      WHEN c_event-choose_commit.
        lv_commit = choose_commit( ).

        IF lv_commit IS INITIAL.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ELSE.
          mo_form_data->set(
            iv_key = c_id-sha1
            iv_val = lv_commit ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

      WHEN c_event-change_type.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        mo_validation_log->clear( ).

      WHEN c_event-create.
        " Validate form entries before creating tag
        mo_validation_log = validate_form( mo_form_data ).

        IF mo_validation_log->is_empty( ) = abap_true.

          mo_form_data->strict( abap_false ).
          mo_form_data->to_abap( CHANGING cs_container = ms_tag ).

          REPLACE ALL OCCURRENCES
            OF cl_abap_char_utilities=>cr_lf
            IN ms_tag-body
            WITH cl_abap_char_utilities=>newline.

          ms_tag-name = Lcl_abapgit_git_tag=>add_tag_prefix( ms_tag-name ).
          ASSERT ms_tag-name CP Lif_abapgit_git_definitions=>c_git_branch-tags.

          TRY.
              Lcl_abapgit_git_porcelain=>create_tag(
                iv_url = mo_repo->get_url( )
                is_tag = ms_tag ).
            CATCH Lcx_abapgit_exception INTO lx_error.
              Lcx_abapgit_exception=>raise( |Cannot create tag { ms_tag-name }: { lx_error->get_text( ) }| ).
          ENDTRY.

          lv_text = |Tag { Lcl_abapgit_git_tag=>remove_tag_prefix( ms_tag-name ) } created|.
          MESSAGE lv_text TYPE 'S'.

          rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
        ELSE.
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
        ENDIF.

    ENDCASE.

    " If staying on form, initialize it with current settings
    IF rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      mo_form = get_form_schema( mo_form_data ).
    ENDIF.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_renderable~render.

    register_handlers( ).

    CREATE OBJECT ri_html TYPE Lcl_abapgit_html.

    ri_html->add( `<div class="repo">` ).

    ri_html->add( Lcl_abapgit_gui_chunk_lib=>render_repo_top(
                    io_repo               = mo_repo
                    iv_show_commit        = abap_false
                    iv_interactive_branch = abap_false ) ).

    ri_html->add( mo_form->render( io_values         = mo_form_data
                                   io_validation_log = mo_validation_log ) ).

    ri_html->add( `</div>` ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_PAGE_TAGS implementation

*>>>>>>> ZCL_ABAPGIT_GUI_ROUTER <<<<<<<*

*"* macro definitions
*include zcl_abapgit_gui_router========ccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_gui_router========ccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


class LCL_ABAPGIT_GUI_ROUTER implementation.
*"* method's implementations
*include methods.
  METHOD abapgit_services_actions.

    IF ii_event->mv_action = Lif_abapgit_definitions=>c_action-abapgit_home.
      rs_handled-page  = Lcl_abapgit_gui_page_repo_over=>create( ).
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
    ENDIF.

  ENDMETHOD.
  METHOD call_browser.

    Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = |{ iv_url }| ).

  ENDMETHOD.
  METHOD db_actions.

    DATA ls_db_key TYPE Lif_abapgit_persistence=>ty_content.
    DATA lo_query TYPE REF TO Lcl_abapgit_string_map.

    lo_query = ii_event->query( ).
    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-db_edit.
        lo_query->to_abap( CHANGING cs_container = ls_db_key ).
        rs_handled-page  = Lcl_abapgit_gui_page_db_entry=>create(
          is_key       = ls_db_key
          iv_edit_mode = abap_true ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-db_display.
        lo_query->to_abap( CHANGING cs_container = ls_db_key ).
        rs_handled-page  = Lcl_abapgit_gui_page_db_entry=>create( ls_db_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.
  METHOD file_download.

    DATA:
      lv_path    TYPE string,
      lv_default TYPE string,
      li_fe_serv TYPE REF TO Lif_abapgit_frontend_services,
      lv_package TYPE devclass.

    lv_package = iv_package.
    TRANSLATE lv_package USING '/#'.
    CONCATENATE lv_package '_' sy-datlo '_' sy-timlo INTO lv_default.

    li_fe_serv = Lcl_abapgit_ui_factory=>get_frontend_services( ).

    lv_path = li_fe_serv->show_file_save_dialog(
      iv_title            = 'Export ZIP'
      iv_extension        = 'zip'
      iv_default_filename = lv_default ).

    li_fe_serv->file_download(
      iv_path = lv_path
      iv_xstr = iv_xstr ).

  ENDMETHOD.
  METHOD general_page_routing.

    DATA: lv_key              TYPE Lif_abapgit_persistence=>ty_repo-key,
          lv_last_repo_key    TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_obj_filter_trans TYPE REF TO Lcl_abapgit_object_filter_tran,
          lo_repo             TYPE REF TO Lcl_abapgit_repo,
          lt_r_trkorr         TYPE Lif_abapgit_definitions=>ty_trrngtrkor_tt.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-go_home.                        " Go Home
        lv_last_repo_key = Lcl_abapgit_persistence_user=>get_instance( )->get_repo_show( ).

        IF lv_last_repo_key IS NOT INITIAL.
          rs_handled-page  = Lcl_abapgit_gui_page_repo_view=>create( lv_last_repo_key ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
        ELSE.
          rs_handled-page = main_page( ).
          rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
        ENDIF.
      WHEN Lif_abapgit_definitions=>c_action-go_back.                        " Go Back
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-go_back.
      WHEN Lif_abapgit_definitions=>c_action-go_db.                          " Go DB util page
        rs_handled-page  = Lcl_abapgit_gui_page_db=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-go_debuginfo.                   " Go debug info
        rs_handled-page  = Lcl_abapgit_gui_page_debuginfo=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-go_settings.                    " Go global settings
        rs_handled-page  = Lcl_abapgit_gui_page_sett_glob=>create( ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-go_settings_personal.           " Go personal settings
        rs_handled-page  = Lcl_abapgit_gui_page_sett_pers=>create( ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-go_background_run.              " Go background run page
        rs_handled-page  = Lcl_abapgit_gui_page_run_bckg=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-go_repo_diff                    " Go Diff page
        OR Lif_abapgit_definitions=>c_action-go_file_diff.
        rs_handled-page  = get_page_diff( ii_event ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
      WHEN Lif_abapgit_definitions=>c_action-go_patch.                       " Go Patch page
        rs_handled-page  = get_page_patch( ii_event ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
      WHEN Lif_abapgit_definitions=>c_action-go_stage.                        " Go Staging page
        rs_handled-page  = get_page_stage( ii_event ).
        rs_handled-state = get_state_diff( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-go_stage_transport.              " Go Staging page by Transport

        lt_r_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_select_wb_tc_tr_and_tsk( ).

        lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        CREATE OBJECT lo_obj_filter_trans.
        lo_obj_filter_trans->set_filter_values( iv_package  = lo_repo->get_package( )
                                                it_r_trkorr = lt_r_trkorr ).

        rs_handled-page = get_page_stage( ii_event      = ii_event
                                          ii_obj_filter = lo_obj_filter_trans ).
        rs_handled-state = get_state_diff( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-go_tutorial.                     " Go to tutorial
        rs_handled-page  = Lcl_abapgit_gui_page_tutorial=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-documentation.                   " abapGit docs
        Lcl_abapgit_services_abapgit=>open_abapgit_wikipage( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-go_explore.                      " dotabap
        Lcl_abapgit_services_abapgit=>open_dotabap_homepage( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-changelog.                       " abapGit full changelog
        Lcl_abapgit_services_abapgit=>open_abapgit_changelog( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-homepage.                        " abapGit homepage
        Lcl_abapgit_services_abapgit=>open_abapgit_homepage( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-sponsor.                         " abapGit sponsor us
        Lcl_abapgit_services_abapgit=>open_abapgit_homepage( 'sponsor.html' ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-show_hotkeys.                    " show hotkeys
        Lcl_abapgit_ui_factory=>get_gui_services(
                             )->get_hotkeys_ctl(
                             )->set_visible( abap_true ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
  METHOD get_page_diff.

    DATA: ls_file   TYPE Lif_abapgit_git_definitions=>ty_file,
          ls_object TYPE Lif_abapgit_definitions=>ty_item,
          lv_key    TYPE Lif_abapgit_persistence=>ty_repo-key.

    lv_key             = ii_event->query( )->get( 'KEY' ).
    ls_file-path       = ii_event->query( )->get( 'PATH' ).
    ls_file-filename   = ii_event->query( )->get( 'FILENAME' ). " unescape ?
    ls_object-obj_type = ii_event->query( )->get( 'OBJ_TYPE' ).
    ls_object-obj_name = ii_event->query( )->get( 'OBJ_NAME' ). " unescape ?

    ri_page = Lcl_abapgit_gui_page_diff=>create(
      iv_key    = lv_key
      is_file   = ls_file
      is_object = ls_object ).

  ENDMETHOD.
  METHOD get_page_stage.

    DATA: lo_repo       TYPE REF TO Lcl_abapgit_repo_online,
          lv_key        TYPE Lif_abapgit_persistence=>ty_repo-key,
          lv_seed       TYPE string,
          lv_sci_result TYPE Lif_abapgit_definitions=>ty_sci_result,
          lx_error      TYPE REF TO cx_sy_move_cast_error.

    lv_key   = ii_event->query( )->get( 'KEY' ).
    lv_seed  = ii_event->query( )->get( 'SEED' ).

    lv_sci_result = Lif_abapgit_definitions=>c_sci_result-no_run.

    TRY.
        lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
      CATCH cx_sy_move_cast_error INTO lx_error.
        Lcx_abapgit_exception=>raise( `Staging is only possible for online repositories.` ).
    ENDTRY.

    IF lo_repo->get_selected_branch( ) CP Lif_abapgit_git_definitions=>c_git_branch-tags.
      Lcx_abapgit_exception=>raise( |You are working on a tag, must be on branch| ).
    ELSEIF lo_repo->get_selected_commit( ) IS NOT INITIAL.
      Lcx_abapgit_exception=>raise( |You are working on a commit, must be on branch| ).
    ENDIF.

    IF lo_repo->get_local_settings( )-code_inspector_check_variant IS NOT INITIAL.

      TRY.
          ri_page = Lcl_abapgit_gui_page_code_insp=>create(
            io_repo                  = lo_repo
            iv_raise_when_no_results = abap_true ).

        CATCH Lcx_abapgit_exception.
          lv_sci_result = Lif_abapgit_definitions=>c_sci_result-passed.
      ENDTRY.

    ENDIF.

    IF ri_page IS INITIAL.
      " force refresh on stage, to make sure the latest local and remote files are used
      lo_repo->refresh( ).

      ri_page = Lcl_abapgit_gui_page_stage=>create(
        io_repo       = lo_repo
        iv_seed       = lv_seed
        iv_sci_result = lv_sci_result
        ii_obj_filter = ii_obj_filter ).
    ENDIF.

  ENDMETHOD.
  METHOD get_state_diff.

    " Bookmark current page before jumping to diff page
    IF ii_event->mv_current_page_name CP 'LCL_ABAPGIT_GUI_PAGE_DIFF'.
      rv_state = Lcl_abapgit_gui=>c_event_state-new_page.
    ELSE.
      rv_state = Lcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
    ENDIF.

  ENDMETHOD.
  METHOD get_state_settings.

    " Bookmark current page before jumping to any settings page
    IF ii_event->mv_current_page_name CP 'LCL_ABAPGIT_GUI_PAGE_SETT_*'.
      rv_state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.
    ELSE.
      rv_state = Lcl_abapgit_gui=>c_event_state-new_page_w_bookmark.
    ENDIF.

  ENDMETHOD.
  METHOD git_services.

    DATA lv_key TYPE Lif_abapgit_persistence=>ty_repo-key.
    DATA li_repo TYPE REF TO Lif_abapgit_repo.

    lv_key = ii_event->query( )->get( 'KEY' ).

    IF lv_key IS NOT INITIAL.
      li_repo = Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    ENDIF.

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-git_pull.                      " GIT Pull
        Lcl_abapgit_services_git=>pull( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_branch_create.             " GIT Create new branch
        Lcl_abapgit_services_git=>create_branch( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_branch_delete.             " GIT Delete remote branch
        Lcl_abapgit_services_git=>delete_branch( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_branch_switch.             " GIT Switch branch
        Lcl_abapgit_services_git=>switch_branch( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_branch_merge.              " GIT Merge branch
        rs_handled-page  = Lcl_abapgit_gui_page_merge_sel=>create( li_repo ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-git_tag_create.                " GIT Tag create
        rs_handled-page  = Lcl_abapgit_gui_page_tags=>create( li_repo ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-git_tag_delete.                " GIT Tag delete
        Lcl_abapgit_services_git=>delete_tag( lv_key ).
        Lcl_abapgit_services_repo=>refresh( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-git_tag_switch.                " GIT Switch Tag
        Lcl_abapgit_services_git=>switch_tag( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
    ENDCASE.

  ENDMETHOD.
  METHOD jump_display_transport.

    DATA:
      lv_adt_link         TYPE string,
      lv_adt_jump_enabled TYPE abap_bool.

    lv_adt_jump_enabled = Lcl_abapgit_persist_factory=>get_settings( )->read( )->get_adt_jump_enabled( ).
    IF lv_adt_jump_enabled = abap_true.
      TRY.
          lv_adt_link = Lcl_abapgit_adt_link=>link_transport( iv_transport ).
          Lcl_abapgit_ui_factory=>get_frontend_services( )->execute( iv_document = lv_adt_link ).
        CATCH Lcx_abapgit_exception.
          " Fallback if ADT link execution failed or was cancelled
          CALL FUNCTION 'TR_DISPLAY_REQUEST'
            EXPORTING
              i_trkorr = iv_transport.
      ENDTRY.
    ELSE.
      CALL FUNCTION 'TR_DISPLAY_REQUEST'
        EXPORTING
          i_trkorr = iv_transport.
    ENDIF.

  ENDMETHOD.
  METHOD jump_display_user.

    " todo, user display in ADT

    CALL FUNCTION 'BAPI_USER_DISPLAY'
      EXPORTING
        username = iv_username.

  ENDMETHOD.
  METHOD main_page.

    DATA lt_repo_all_list TYPE Lif_abapgit_repo_srv=>ty_repo_list.

    " if there are no favorites, check if there are any repositories at all
    " if not, go to tutorial where the user can create the first repository
    lt_repo_all_list = Lcl_abapgit_repo_srv=>get_instance( )->list( ).
    IF lt_repo_all_list IS NOT INITIAL.
      ri_page = Lcl_abapgit_gui_page_repo_over=>create( ).
    ELSE.
      ri_page = Lcl_abapgit_gui_page_tutorial=>create( ).
    ENDIF.

  ENDMETHOD.
  METHOD other_utilities.
    TYPES ty_char600 TYPE c LENGTH 600.
    DATA lv_clip_content TYPE string.
    DATA lt_clipboard TYPE STANDARD TABLE OF ty_char600.

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-ie_devtools.
        Lcl_abapgit_ui_factory=>get_frontend_services( )->open_ie_devtools( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-clipboard.
        lv_clip_content = ii_event->query( )->get( 'CLIPBOARD' ).
        APPEND lv_clip_content TO lt_clipboard.
        Lcl_abapgit_ui_factory=>get_frontend_services( )->clipboard_export( lt_clipboard ).
        MESSAGE 'Successfully exported URL to Clipboard.' TYPE 'S'.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-yank_to_clipboard.
        lv_clip_content = ii_event->form_data( )->get( 'CLIPBOARD' ).
        APPEND lv_clip_content TO lt_clipboard.
        Lcl_abapgit_ui_factory=>get_frontend_services( )->clipboard_export( lt_clipboard ).
        MESSAGE 'Successfully exported to Clipboard.' TYPE 'S'.
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.
  METHOD repository_services.

    DATA:
      lv_key  TYPE Lif_abapgit_persistence=>ty_repo-key,
      lo_repo TYPE REF TO Lcl_abapgit_repo,
      li_log  TYPE REF TO Lif_abapgit_log.

    lv_key = ii_event->query( )->get( 'KEY' ).
    IF lv_key IS NOT INITIAL.
      lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
    ENDIF.

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-repo_newoffline.                 " New offline repo
        rs_handled-page  = Lcl_abapgit_gui_page_addofflin=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-repo_add_all_obj_to_trans_req.   " Add objects to transport
        Lcl_abapgit_transport=>add_all_objects_to_trans_req( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_refresh.                    " Repo refresh
        Lcl_abapgit_services_repo=>refresh( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_syntax_check.               " Syntax check
        rs_handled-page  = Lcl_abapgit_gui_page_syntax=>create( lo_repo ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-repo_code_inspector.             " Code inspector
        rs_handled-page  = Lcl_abapgit_gui_page_code_insp=>create( lo_repo ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-repo_purge.                      " Purge all objects and repo (uninstall)
        Lcl_abapgit_services_repo=>purge( lv_key ).
        rs_handled-page  = Lcl_abapgit_gui_page_repo_over=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.
      WHEN Lif_abapgit_definitions=>c_action-repo_delete_objects.             " Purge all objects (uninstall)
        Lcl_abapgit_services_repo=>purge(
          iv_key       = lv_key
          iv_keep_repo = abap_true ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_remove.                     " Repo remove
        Lcl_abapgit_services_repo=>remove( lv_key ).
        rs_handled-page  = Lcl_abapgit_gui_page_repo_over=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page_replacing.
      WHEN Lif_abapgit_definitions=>c_action-repo_activate_objects.           " Repo activate objects
        Lcl_abapgit_services_repo=>activate_objects( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_newonline.                  " New online repo
        rs_handled-page  = Lcl_abapgit_gui_page_addonline=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-flow.                            " Flow page
        rs_handled-page  = Lcl_abapgit_gui_page_flow=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-repo_refresh_checksums.          " Rebuild local checksums
        Lcl_abapgit_services_repo=>refresh_local_checksums( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_toggle_fav.                 " Toggle repo as favorite
        Lcl_abapgit_services_repo=>toggle_favorite( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_transport_to_branch.        " Transport to branch
        Lcl_abapgit_services_repo=>transport_to_branch( lv_key ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
      WHEN Lif_abapgit_definitions=>c_action-repo_settings.                   " Repo settings
        rs_handled-page  = Lcl_abapgit_gui_page_sett_repo=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-repo_local_settings.             " Local repo settings
        rs_handled-page  = Lcl_abapgit_gui_page_sett_locl=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-repo_remote_settings.            " Remote repo settings
        rs_handled-page  = Lcl_abapgit_gui_page_sett_remo=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-repo_background.                 " Repo background mode
        rs_handled-page  = Lcl_abapgit_gui_page_sett_bckg=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-repo_infos.                      " Repo infos
        rs_handled-page  = Lcl_abapgit_gui_page_sett_info=>create( lo_repo ).
        rs_handled-state = get_state_settings( ii_event ).
      WHEN Lif_abapgit_definitions=>c_action-repo_log.                        " Repo log
        li_log = lo_repo->get_log( ).
        Lcl_abapgit_log_viewer=>show_log( li_log ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
    ENDCASE.

  ENDMETHOD.
  METHOD sap_gui_actions.

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-jump.                          " Open object editor
        jump_object(
          iv_obj_type   = ii_event->query( )->get( 'TYPE' )
          iv_obj_name   = ii_event->query( )->get( 'NAME' )
          iv_filename   = ii_event->query( )->get( 'FILE' )
          iv_sub_type   = ii_event->query( )->get( 'SUBTYPE' )
          iv_sub_name   = ii_event->query( )->get( 'SUBNAME' )
          iv_line       = ii_event->query( )->get( 'LINE' )
          iv_new_window = ii_event->query( )->get( 'NEW_WINDOW' ) ).

        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-jump_transport.
        jump_display_transport( |{ ii_event->query( )->get( 'TRANSPORT' ) }| ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-jump_user.
        jump_display_user( |{ ii_event->query( )->get( 'USER' ) }| ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

      WHEN Lif_abapgit_definitions=>c_action-url.
        call_browser( ii_event->query( )->get( 'URL' ) ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.

    ENDCASE.

  ENDMETHOD.
  METHOD Lif_abapgit_gui_event_handler~on_event.

    IF rs_handled-state IS INITIAL.
      rs_handled = general_page_routing( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = repository_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = git_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = zip_services( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = db_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = abapgit_services_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = sap_gui_actions( ii_event ).
    ENDIF.
    IF rs_handled-state IS INITIAL.
      rs_handled = other_utilities( ii_event ).
    ENDIF.

    IF rs_handled-state IS INITIAL.
      rs_handled-state = Lcl_abapgit_gui=>c_event_state-not_handled.
    ENDIF.

  ENDMETHOD.
  METHOD zip_services.

    DATA: lv_key              TYPE Lif_abapgit_persistence=>ty_repo-key,
          lo_repo             TYPE REF TO Lcl_abapgit_repo,
          lv_path             TYPE string,
          lv_dest             TYPE rfcdest,
          lv_msg              TYPE c LENGTH 200,
          lv_xstr             TYPE xstring,
          lv_package          TYPE Lif_abapgit_persistence=>ty_repo-package,
          lv_folder_logic     TYPE string,
          lv_main_lang_only   TYPE Lif_abapgit_persistence=>ty_local_settings-main_language_only,
          lo_obj_filter_trans TYPE REF TO Lcl_abapgit_object_filter_tran,
          lt_r_trkorr         TYPE Lif_abapgit_definitions=>ty_trrngtrkor_tt.

    CONSTANTS:
      BEGIN OF lc_page,
        main_view TYPE string VALUE 'LCL_ABAPGIT_GUI_PAGE_MAIN',
        repo_view TYPE string VALUE 'LCL_ABAPGIT_GUI_PAGE_REPO_VIEW',
      END OF lc_page.

    lv_key = ii_event->query( )->get( 'KEY' ).

    CASE ii_event->mv_action.
      WHEN Lif_abapgit_definitions=>c_action-zip_import                       " Import repo from ZIP
        OR Lif_abapgit_definitions=>c_action-rfc_compare.                     " Compare repo via RFC

        lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).

        IF ii_event->mv_action = Lif_abapgit_definitions=>c_action-zip_import.
          lv_path = Lcl_abapgit_ui_factory=>get_frontend_services( )->show_file_open_dialog(
            iv_title            = 'Import ZIP'
            iv_extension        = 'zip'
            iv_default_filename = '*.zip' ).
          lv_xstr = Lcl_abapgit_ui_factory=>get_frontend_services( )->file_upload( lv_path ).
        ELSE.
          lv_dest = Lcl_abapgit_ui_factory=>get_popups( )->popup_search_help( 'RFCDES-RFCDEST' ).

          IF lv_dest IS INITIAL.
            rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
            RETURN.
          ENDIF.

          lv_package            = lo_repo->get_package( ).
          lv_folder_logic       = lo_repo->get_dot_abapgit( )->get_folder_logic( ).
          lv_main_lang_only     = lo_repo->get_local_settings( )-main_language_only.

          CALL FUNCTION 'Z_ABAPGIT_SERIALIZE_PACKAGE'
            DESTINATION lv_dest
            EXPORTING
              iv_package            = lv_package
              iv_folder_logic       = lv_folder_logic
              iv_main_lang_only     = lv_main_lang_only
            IMPORTING
              ev_xstring            = lv_xstr
            EXCEPTIONS
              system_failure        = 1 MESSAGE lv_msg
              communication_failure = 2 MESSAGE lv_msg
              OTHERS                = 3.
          IF sy-subrc <> 0.
            Lcx_abapgit_exception=>raise( |RFC import error: { lv_msg }| ).
          ENDIF.
        ENDIF.

        lo_repo->set_files_remote( Lcl_abapgit_zip=>load( lv_xstr ) ).
        Lcl_abapgit_services_repo=>refresh( lv_key ).

        CASE ii_event->mv_current_page_name.
          WHEN lc_page-repo_view.
            rs_handled-state = Lcl_abapgit_gui=>c_event_state-re_render.
          WHEN lc_page-main_view.
            rs_handled-page  = Lcl_abapgit_gui_page_repo_view=>create( lo_repo->get_key( ) ).
            rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
          WHEN OTHERS.
            rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
        ENDCASE.
      WHEN Lif_abapgit_definitions=>c_action-zip_export.                      " Export repo as ZIP
        lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lv_xstr = Lcl_abapgit_zip=>encode_files( lo_repo->get_files_local( ) ).
        file_download( iv_package = lo_repo->get_package( )
                       iv_xstr    = lv_xstr ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-zip_export_transport.                      " Export repo as ZIP

        lt_r_trkorr = Lcl_abapgit_ui_factory=>get_popups( )->popup_select_wb_tc_tr_and_tsk( ).
        lo_repo ?= Lcl_abapgit_repo_srv=>get_instance( )->get( lv_key ).
        lo_repo->refresh( ).
        CREATE OBJECT lo_obj_filter_trans.
        lo_obj_filter_trans->set_filter_values( iv_package  = lo_repo->get_package( )
                                                it_r_trkorr = lt_r_trkorr ).

        lv_xstr = Lcl_abapgit_zip=>encode_files( lo_repo->get_files_local_filtered( lo_obj_filter_trans ) ).
        lo_repo->refresh( ).
        file_download( iv_package = lo_repo->get_package( )
                       iv_xstr    = lv_xstr ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-zip_package.                     " Export package as ZIP
        rs_handled-page  = Lcl_abapgit_gui_page_ex_pckage=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
      WHEN Lif_abapgit_definitions=>c_action-zip_transport.                   " Export transports as ZIP
        Lcl_abapgit_transport_mass=>run( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-no_more_act.
      WHEN Lif_abapgit_definitions=>c_action-zip_object.                      " Export object as ZIP
        rs_handled-page  = Lcl_abapgit_gui_page_ex_object=>create( ).
        rs_handled-state = Lcl_abapgit_gui=>c_event_state-new_page.
    ENDCASE.

  ENDMETHOD.
  METHOD jump_object.

    DATA:
      ls_item        TYPE Lif_abapgit_definitions=>ty_item,
      ls_sub_item    TYPE Lif_abapgit_definitions=>ty_item,
      lx_error       TYPE REF TO Lcx_abapgit_exception,
      lv_line_number TYPE i,
      lv_new_window  TYPE abap_bool,
      li_html_viewer TYPE REF TO Lif_abapgit_html_viewer.

    ls_item-obj_type = cl_http_utility=>unescape_url( |{ iv_obj_type }| ).
    ls_item-obj_name = cl_http_utility=>unescape_url( |{ iv_obj_name }| ).
    ls_sub_item-obj_type = cl_http_utility=>unescape_url( |{ iv_sub_type }| ).
    ls_sub_item-obj_name = cl_http_utility=>unescape_url( |{ iv_sub_name }| ).

    IF iv_line CO '0123456789'.
      lv_line_number = iv_line.
    ENDIF.
    lv_new_window = boolc( iv_new_window IS NOT INITIAL ).

    TRY.
        li_html_viewer = Lcl_abapgit_ui_factory=>get_html_viewer( ).

        " Hide HTML Viewer in dummy screen0 for direct CALL SCREEN to work
        li_html_viewer->set_visiblity( abap_false ).

        IF ls_item-obj_type = Lif_abapgit_data_config=>c_data_type-tabu.
          Lcl_abapgit_data_utils=>jump( ls_item ).
        ELSEIF lv_line_number IS INITIAL OR ls_sub_item IS INITIAL.
          Lcl_abapgit_objects=>jump(
            is_item       = ls_item
            iv_filename   = iv_filename
            iv_new_window = lv_new_window ).
        ELSE.
          Lcl_abapgit_objects=>jump(
            is_item        = ls_item
            is_sub_item    = ls_sub_item
            iv_filename    = iv_filename
            iv_line_number = lv_line_number
            iv_new_window  = lv_new_window ).
        ENDIF.

        li_html_viewer->set_visiblity( abap_true ).
      CATCH Lcx_abapgit_exception INTO lx_error.
        li_html_viewer->set_visiblity( abap_true ).
        RAISE EXCEPTION lx_error.
    ENDTRY.

  ENDMETHOD.
  METHOD get_page_patch.

    DATA: ls_file   TYPE Lif_abapgit_git_definitions=>ty_file,
          ls_object TYPE Lif_abapgit_definitions=>ty_item,
          lv_key    TYPE Lif_abapgit_persistence=>ty_repo-key.

    lv_key             = ii_event->query( )->get( 'KEY' ).
    ls_file-path       = ii_event->query( )->get( 'PATH' ).
    ls_file-filename   = ii_event->query( )->get( 'FILENAME' ). " unescape ?
    ls_object-obj_type = ii_event->query( )->get( 'OBJ_TYPE' ).
    ls_object-obj_name = ii_event->query( )->get( 'OBJ_NAME' ). " unescape ?

    ri_page = Lcl_abapgit_gui_page_patch=>create(
      iv_key    = lv_key
      is_file   = ls_file
      is_object = ls_object ).

  ENDMETHOD.
endclass. "ZCL_ABAPGIT_GUI_ROUTER implementation

*>>>>>>> ZCL_ABAPGIT_OBJECTS_ACTIVATION <<<<<<<*

*"* macro definitions
*include zcl_abapgit_objects_activationccmac.
*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

*"* local class implementation
*include zcl_abapgit_objects_activationccimp.
*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations


*"* test class
*include zcl_abapgit_objects_activationccau.
*CLASS SHRITEFUH64VYIPO5IWUYB3KW35SFY DEFINITION DEFERRED.
*CLASS zcl_abapgit_objects_activation DEFINITION LOCAL FRIENDS SHRITEFUH64VYIPO5IWUYB3KW35SFY.



class LCL_ABAPGIT_OBJECTS_ACTIVATION implementation.
*"* method's implementations
*include methods.
  METHOD activate.

    " Make sure that all changes are committed since any activation error will lead to a rollback
    COMMIT WORK AND WAIT.

    IF use_new_activation_logic( ) = abap_true.
      activate_new(
        iv_ddic = iv_ddic
        ii_log  = ii_log ).
    ELSE.
      activate_old(
        iv_ddic = iv_ddic
        ii_log  = ii_log ).
    ENDIF.

    update_where_used( ii_log ).

  ENDMETHOD.
  METHOD activate_ddic.

    DATA: lt_gentab     TYPE STANDARD TABLE OF dcgentb,
          lv_rc         TYPE sy-subrc,
          ls_gentab     LIKE LINE OF lt_gentab,
          lt_deltab     TYPE STANDARD TABLE OF dcdeltb,
          lt_action_tab TYPE STANDARD TABLE OF dctablres,
          lv_logname    TYPE ddmass-logname.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects.


    LOOP AT gt_objects ASSIGNING <ls_object>.
      " Filter types supported by mass activation
      IF is_ddic_type( <ls_object>-object ) = abap_false.
        CONTINUE.
      ENDIF.
